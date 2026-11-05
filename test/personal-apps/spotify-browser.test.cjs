const test = require('node:test');
const assert = require('node:assert/strict');
const { Receiver } = require('../../modules/interactive/apps/spotify-browser.js');
const uri = 'spotify:track:4uLU6hMCjMI75M1A2tKUQC';
const flush = () => new Promise(resolve => setImmediate(resolve));
function fixture() {
  let state = null, clock = 0, paused = 0, destroyed = 0;
  const sent = [], plays = [], seeks = [];
  const player = { async pause() { paused++; if (state) state = { ...state, paused: true }; },
    async resume() {}, async seek(position) { seeks.push(position); }, async getCurrentState() { return state; } };
  const receiver = new Receiver(player, async (track, signal) => { plays.push({ track, signal }); },
    message => sent.push(message), () => { destroyed++; }, () => clock);
  const frame = (position, paused = false, previous = false) => ({ position, paused, duration: 10000, repeat_mode: 0, shuffle: false,
    track_window: { current_track: { uri }, previous_tracks: previous ? [{ uri }] : [], next_tracks: [] } });
  return { receiver, player, sent, plays, seeks, frame,
    async emit(position, paused = false, previous = false) { state = frame(position, paused, previous); receiver.observe(state); await flush(); },
    async tick(position, paused = false, previous = false) { state = frame(position, paused, previous); clock += 500; await receiver.tick(); await flush(); },
    get paused() { return paused; }, get destroyed() { return destroyed; } };
}
async function start(f, run = 1, seq = 1) {
  await f.receiver.command({ kind: 'play', run, seq, uri });
  await f.emit(0); await f.tick(1200);
}
test('one URI starts at zero and a confirmed natural ending is emitted exactly once', async () => {
  const f = fixture(); await start(f);
  assert.equal(f.plays.length, 1); assert.equal(f.seeks.length, 0);
  await f.tick(9700); await f.emit(0, true, true); await flush();
  assert.equal(f.sent.filter(x => x.type === 'ended').length, 1);
  assert.ok(f.paused > 0);
  await f.emit(0, true, true);
  assert.equal(f.sent.filter(x => x.type === 'ended').length, 1);
});
test('an arbitrary pause/reset or missing playback history cannot finish the occurrence', async () => {
  for (const previous of [false, true]) {
    const f = fixture(); await start(f);
    if (!previous) await f.tick(9700);
    await f.emit(0, true, previous);
    assert.equal(f.sent.filter(x => x.type === 'ended').length, 0);
  }
});
test('pause intent invalidates a near-end candidate before SDK acknowledgement', async () => {
  const f = fixture(); await start(f); await f.tick(9700);
  await f.receiver.command({ kind: 'pause', run: 1, seq: 2 });
  await f.emit(0, true, true);
  assert.equal(f.sent.filter(x => x.type === 'ended').length, 0);
  assert.ok(f.sent.some(x => x.type === 'ack' && x.seq === 2));
});
test('seek waits for actual progress, ignores stale pre-seek polls, and never auto-seeks startup', async () => {
  const f = fixture(); await f.receiver.command({ kind: 'play', run: 1, seq: 1, uri });
  const seek = f.receiver.command({ kind: 'seek', run: 1, seq: 2, seconds: 3 });
  await flush(); assert.deepEqual(f.seeks, []);
  await f.emit(0); await f.tick(1200); await flush();
  assert.deepEqual(f.seeks, [4200]);
  await f.tick(1200);
  assert.ok(!f.sent.some(x => x.type === 'ack' && x.seq === 2));
  await f.tick(4200); await seek;
  assert.ok(f.sent.some(x => x.type === 'ack' && x.seq === 2));
});
test('cancelling a pending load destroys the SDK before acknowledging stop', async () => {
  const f = fixture(); let resolve;
  f.receiver.play = (_uri, signal) => new Promise(done => { resolve = done; f.signal = signal; });
  const pending = f.receiver.command({ kind: 'play', run: 1, seq: 1, uri }); await flush();
  await f.receiver.command({ kind: 'stop', run: 1, seq: 2 });
  assert.ok(f.signal.aborted); assert.equal(f.destroyed, 1);
  assert.ok(f.sent.some(x => x.type === 'ack' && x.seq === 2));
  resolve(); await pending;
  assert.ok(!f.sent.some(x => x.type === 'ack' && x.seq === 1));
});
test('a stopped occurrence cannot complete a later occurrence of the same URI', async () => {
  const f = fixture(); await start(f); await f.receiver.command({ kind: 'stop', run: 1, seq: 2 });
  await start(f, 2, 3); await f.emit(0, true, true);
  assert.equal(f.sent.filter(x => x.type === 'ended').length, 0);
  await f.tick(9700); await f.emit(0, true, true);
  assert.equal(f.sent.filter(x => x.type === 'ended' && x.run === 2).length, 1);
});
test('seeking while paused acknowledges the new paused position without requiring resume', { timeout: 1000 }, async () => {
  const f = fixture(); await start(f);
  await f.receiver.command({ kind: 'pause', run: 1, seq: 2 });
  const seek = f.receiver.command({ kind: 'seek', run: 1, seq: 3, seconds: 3 });
  await flush(); assert.deepEqual(f.seeks, [4200]);
  await f.tick(4200, true); await seek;
  assert.ok(f.sent.some(x => x.type === 'ack' && x.seq === 3));
  assert.ok(!f.sent.some(x => x.type === 'ended'));
});

test('a late unsolicited playing state after stopping disconnects instead of overlapping local audio', async () => {
  const f = fixture(); await start(f);
  await f.receiver.command({ kind: 'stop', run: 1, seq: 2 });
  await f.emit(1500);
  assert.equal(f.destroyed, 1);
});

test('a previous paused track is harmless while a different Spotify occurrence loads', async () => {
  const f = fixture(); await start(f); await f.receiver.command({ kind: 'stop', run: 1, seq: 2 });
  await f.receiver.command({ kind: 'play', run: 2, seq: 3, uri: 'spotify:track:1111111111111111111111' });
  await f.emit(0, true, true);
  assert.equal(f.destroyed, 0);
  assert.ok(!f.sent.some(x => x.type === 'error'));
});

test('losing local SDK state after actual progress stops rather than inventing completion', async () => {
  const f = fixture(); await start(f); f.player.getCurrentState = async () => null;
  await f.receiver.tick();
  assert.ok(f.sent.some(x => x.type === 'error' && x.kind === 'state'));
  assert.ok(!f.sent.some(x => x.type === 'ended'));
});

test('every command ACK carries the occurrence identity expected by EMMS', async () => {
  const f = fixture(); await start(f, 7, 1);
  await f.receiver.command({ kind: 'pause', run: 7, seq: 2 });
  await f.receiver.command({ kind: 'resume', run: 7, seq: 3 });
  const seek = f.receiver.command({ kind: 'seek', run: 7, seq: 4, seconds: 3 });
  await flush(); await f.tick(4200); await seek;
  await f.receiver.command({ kind: 'stop', run: 7, seq: 5 });
  assert.deepEqual(f.sent.filter(x => x.type === 'ack').map(x => [x.run, x.seq]), [[7, 1], [7, 2], [7, 3], [7, 4], [7, 5]]);
});

test('stop waits for the SDK paused state instead of failing on a stale playing snapshot', async () => {
  const f = fixture(); await start(f); let reads = 0;
  f.player.pause = async () => {};
  f.player.getCurrentState = async () => f.frame(1200, ++reads >= 2);
  await f.receiver.command({ kind: 'stop', run: 1, seq: 2 });
  assert.ok(f.sent.some(x => x.type === 'ack' && x.seq === 2));
  assert.ok(!f.sent.some(x => x.type === 'error'));
});

test('a failed stop never acknowledges safety and SDK diagnostics discard raw messages', async () => {
  const f = fixture(); await start(f);
  f.player.pause = async () => { throw Error('PRIVATE-DETAILS'); };
  await f.receiver.command({ kind: 'stop', run: 1, seq: 2 });
  assert.ok(!f.sent.some(x => x.type === 'ack' && x.seq === 2));
  assert.ok(f.sent.some(x => x.type === 'error'));
  assert.doesNotMatch(JSON.stringify(f.sent), /PRIVATE-DETAILS/);
});
