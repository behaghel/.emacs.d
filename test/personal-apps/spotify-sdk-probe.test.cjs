const test = require('node:test');
const assert = require('node:assert/strict');
const createProbe = require('../../modules/interactive/apps/spotify-sdk-probe.js');

const uri = 'spotify:track:4uLU6hMCjMI75M1A2tKUQC';
function fixture(ending = false) {
  const nodes = Object.fromEntries(['authorize', 'play', 'pause', 'status', 'track', 'duration', 'trace-box', 'trace'].map(id => [id, { disabled: true, textContent: '' }]));
  const requests = [], timers = new Map(), history = [], lifecycle = {}, intervals = [], seeks = [];
  let sdkState = null;
  let serial = 0, authorized = false, device, activated = 0, paused = 0, disconnected = 0;
  class Player {
    constructor(options) { this.options = options; this.events = {}; device = this; }
    addListener(name, callback) { this.events[name] = callback; }
    async connect() { this.events.ready({ device_id: 'synthetic-device' }); return true; }
    async activateElement() { activated++; }
    async pause() { paused++; }
    async seek(position) { seeks.push(position); }
    async getCurrentState() { return sdkState; }
    disconnect() { disconnected++; }
  }
  const env = {
    location: { hash: '#synthetic-capability' },
    history: { replaceState(_a, _b, value) { history.push(value); } },
    document: { getElementById(id) { return nodes[id]; }, createElement() { return {}; },
      head: { appendChild() { env.onSpotifyWebPlaybackSDKReady(); } } },
    Spotify: { Player },
    AbortSignal,
    open() { return { location: '', close() {} }; },
    addEventListener(name, fn) { lifecycle[name] = fn; },
    setInterval(fn, delay) { intervals.push({ fn, delay }); lifecycle.poll = fn; return intervals.length; }, clearInterval() {},
    setTimeout(fn, delay) { const id = ++serial; timers.set(id, { fn, delay }); return id; },
    clearTimeout(id) { timers.delete(id); },
    async fetch(url, options) {
      requests.push({ url, options });
      if (url.startsWith('/')) assert.equal(options.headers['X-Emacs-Spotify-Probe'], 'synthetic-capability');
      const payload = url === '/state' ? { authorized, track_uri: uri, ending_check: ending, error: null }
        : url === '/authorize' ? { authorization_url: 'https://accounts.spotify.com/authorize?synthetic' }
          : url === '/token' ? { access_token: 'SYNTHETIC-ACCESS' } : {};
      return { ok: true, status: url.startsWith('https:') ? 204 : 200, json: async () => payload };
    }
  };
  Object.defineProperty(env, 'localStorage', { get() { throw Error('Persistent storage forbidden'); } });
  Object.defineProperty(env, 'sessionStorage', { get() { throw Error('Persistent storage forbidden'); } });
  return { env, nodes, requests, timers, history, lifecycle, intervals, seeks,
    emit(state) { sdkState = state; device.events.player_state_changed(state); },
    authorize() { authorized = true; }, get device() { return device; },
    get activated() { return activated; }, get paused() { return paused; }, get disconnected() { return disconnected; } };
}

test('explicit authorization readies a dedicated SDK player without persisting tokens', async () => {
  const f = fixture();
  const probe = createProbe(f.env);
  await probe.boot;
  assert.deepEqual(f.history, ['/']);
  assert.equal(f.nodes.play.disabled, true);
  assert.equal(f.nodes.authorize.disabled, false);
  await f.nodes.authorize.onclick();
  f.authorize(); await probe.poll();
  assert.equal(f.nodes.play.disabled, false);
  assert.match(f.nodes.status.textContent, /Ready/);
  assert.equal(f.activated, 0);
  assert.equal(f.requests.filter(r => r.url.startsWith('https:')).length, 0);
});

test('play activates audio in the click and targets one URI on the SDK device', async () => {
  const f = fixture(); f.authorize();
  const probe = createProbe(f.env); await probe.boot;
  const play = f.nodes.play.onclick();
  assert.equal(f.activated, 1);
  await play;
  const request = f.requests.find(r => r.url.startsWith('https:'));
  assert.match(request.url, /device_id=synthetic-device/);
  assert.deepEqual(JSON.parse(request.options.body), { uris: [uri], position_ms: 0 });
  assert.equal(request.options.headers.Authorization, 'Bearer SYNTHETIC-ACCESS');
  f.device.events.player_state_changed({ paused: false, track_window: { current_track: { uri, name: 'Song', artists: [{ name: 'Artist' }] } } });
  assert.match(f.nodes.status.textContent, /Playing selected track/);
  const end = [...f.timers.values()].find(t => t.delay === 15000);
  assert.ok(end); await end.fn();
  assert.equal(f.paused, 1);
  assert.match(f.nodes.status.textContent, /Paused after 15 seconds/);
});

test('SDK failures display event names, never arbitrary messages or credentials', async () => {
  const f = fixture(); f.authorize();
  const probe = createProbe(f.env); await probe.boot;
  f.device.events.account_error({ message: 'SYNTHETIC-PRIVATE-PAYLOAD' });
  assert.equal(f.nodes.status.textContent, 'SDK error: account_error');
  assert.equal(f.nodes.play.disabled, true);
  assert.ok(f.disconnected > 0);
  assert.doesNotMatch(f.nodes.status.textContent, /SYNTHETIC/);
});

test('callbacks from a disconnected SDK instance cannot replace a newer player', async () => {
  const f = fixture(); f.authorize();
  const probe = createProbe(f.env); await probe.boot;
  const old = f.device;
  await f.nodes.authorize.onclick();
  await probe.poll();
  assert.notEqual(f.device, old);
  old.events.account_error({ message: 'SYNTHETIC-PRIVATE-PAYLOAD' });
  assert.equal(f.nodes.play.disabled, false);
  assert.match(f.nodes.status.textContent, /Ready/);
});

test('an unexpected track and closing the page stop the owned SDK connection', async () => {
  const f = fixture(); f.authorize();
  const probe = createProbe(f.env); await probe.boot;
  await f.nodes.play.onclick();
  f.device.events.player_state_changed({ paused: false, track_window: { current_track: { uri: 'spotify:track:1111111111111111111111' } } });
  assert.match(f.nodes.status.textContent, /Unexpected track/);
  f.lifecycle.pagehide();
  assert.ok(f.paused > 0);
  assert.ok(f.disconnected > 0);
});

function playback(position, paused = false, extra = {}) {
  return { position, paused, duration: 194607, repeat_mode: 0, shuffle: false,
    track_window: { current_track: { uri }, previous_tracks: [], next_tracks: [] }, ...extra };
}
const flush = () => new Promise(resolve => setImmediate(resolve));
const trace = f => f.nodes.trace.textContent.split('\n').filter(Boolean).map(JSON.parse);

test('SDK playback errors remain identifiable before or after a user-requested seek', async () => {
  for (const seek of [false, true]) {
    const f = fixture(true); f.authorize();
    const probe = createProbe(f.env); await probe.boot;
    await f.nodes.play.onclick(); f.emit(playback(0));
    if (seek) { await f.nodes.play.onclick(); await flush(); }
    f.device.events.playback_error({ message: 'PRIVATE-TOKEN-OR-SERVICE-DETAILS' });
    assert.match(f.nodes.status.textContent, /SDK error: playback_error/);
    assert.ok(trace(f).some(row => row.source === 'playback_error'));
    assert.equal(trace(f).some(row => row.source === 'seek_requested'), seek);
    assert.ok(f.disconnected > 0);
    assert.doesNotMatch(f.nodes.trace.textContent, /PRIVATE|spotify:track:/);
  }
});

test('an unattended normal start stops without ever seeking', async () => {
  const f = fixture(true); f.authorize();
  const probe = createProbe(f.env); await probe.boot;
  await f.nodes.play.onclick(); f.emit(playback(0)); await flush();
  assert.deepEqual(f.seeks, []);
  const guard = [...f.timers.values()].find(timer => timer.delay === 30000);
  assert.ok(guard); await guard.fn();
  assert.ok(f.paused > 0 && f.disconnected > 0);
  assert.ok(trace(f).some(row => row.source === 'before_seek_timeout'));
});

test('ending check seeks once, samples the tail, and records a safe paused/reset transition', async () => {
  const f = fixture(true); f.authorize();
  const probe = createProbe(f.env); await probe.boot;
  assert.equal(f.nodes.play.textContent, 'Start ending check');
  assert.equal(f.nodes['trace-box'].hidden, false);
  await f.nodes.play.onclick();
  f.emit(playback(0, false, { message: 'PRIVATE-PAYLOAD', access_token: 'PRIVATE-TOKEN' }));
  await flush();
  assert.deepEqual(f.seeks, [], 'metadata alone must not initiate seeking');
  await probe.poll();
  assert.equal(f.nodes.play.textContent, 'Seek final 8 seconds');
  assert.match(f.nodes.status.textContent, /When you hear music/);
  await f.nodes.play.onclick(); await flush();
  assert.deepEqual(f.seeks, [186607]);
  assert.equal(f.requests.filter(r => r.url.startsWith('https:')).length, 1);
  await f.nodes.play.onclick();
  assert.deepEqual(f.seeks, [186607], 'a duplicate seek click is ignored');
  f.emit(playback(186607));
  f.emit(playback(194000));
  f.emit(playback(0, true));
  assert.match(f.nodes.status.textContent, /SDK paused\/reset observed; copy safe trace/);
  assert.ok(trace(f).some(s => s.paused === true && s.position_ms === 0 && s.current_matches === true));
  assert.ok(f.paused > 0 && f.disconnected > 0);
  assert.doesNotMatch(f.nodes.trace.textContent, /PRIVATE|spotify:track:|synthetic|SYNTHETIC/);
});

test('an initial paused reset is not mistaken for an observed ending', async () => {
  const f = fixture(true); f.authorize();
  const probe = createProbe(f.env); await probe.boot;
  await f.nodes.play.onclick();
  f.emit(playback(0, true));
  assert.doesNotMatch(f.nodes.status.textContent, /observed; copy/);
  assert.equal(f.disconnected, 0);
});

test('cancelling a pending ending seek preserves the cancellation trace despite late callbacks', async () => {
  const f = fixture(true); f.authorize();
  const probe = createProbe(f.env); await probe.boot;
  await f.nodes.play.onclick();
  let resolveSeek;
  f.device.seek = () => new Promise(resolve => { resolveSeek = resolve; });
  const old = f.device;
  f.emit(playback(0));
  await f.nodes.play.onclick();
  await f.nodes.pause.onclick();
  const cancelled = f.nodes.trace.textContent;
  resolveSeek(); await flush();
  old.events.player_state_changed(playback(0, true));
  assert.match(f.nodes.status.textContent, /cancelled; copy safe trace/);
  assert.equal(f.nodes.trace.textContent, cancelled);
  assert.ok(trace(f).some(s => s.source === 'manual_pause'));
});

test('repeat resets, unexpected tracks, and a safety timeout stop rather than continue playback', async () => {
  for (const ending of ['repeat', 'different', 'timeout']) {
    const f = fixture(true); f.authorize();
    const probe = createProbe(f.env); await probe.boot;
    await f.nodes.play.onclick(); f.emit(playback(0));
    await f.nodes.play.onclick(); await flush();
    f.emit(playback(194000));
    if (ending === 'repeat') f.emit(playback(0));
    else if (ending === 'different') f.emit(playback(0, false, {
      track_window: { current_track: { uri: 'PRIVATE-OTHER-URI' }, previous_tracks: [{ uri }], next_tracks: [] }
    }));
    else await [...f.timers.values()].find(t => t.delay === 20000).fn();
    assert.ok(f.paused > 0 && f.disconnected > 0, ending);
    assert.match(f.nodes.status.textContent, /copy safe trace/);
    assert.doesNotMatch(f.nodes.trace.textContent, /PRIVATE|spotify:track:/);
  }
});

test('ending samples are bounded and late state polls cannot rewrite a stopped trace', async () => {
  const f = fixture(true); f.authorize();
  const probe = createProbe(f.env); await probe.boot;
  await f.nodes.play.onclick(); f.emit(playback(0));
  await f.nodes.play.onclick(); await flush();
  f.emit(playback(186607));
  for (let i = 0; i < 100; i++) f.emit(playback(186607 + i));
  assert.ok(trace(f).length <= 80);
  const poll = f.intervals.find(i => i.delay === 500);
  assert.ok(poll);
  let resolveState;
  f.device.getCurrentState = () => new Promise(resolve => { resolveState = resolve; });
  const pending = poll.fn();
  await f.nodes.pause.onclick();
  const stopped = f.nodes.trace.textContent;
  resolveState(playback(0, true)); await pending;
  assert.equal(f.nodes.trace.textContent, stopped);
});
