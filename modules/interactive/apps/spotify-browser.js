/* One owned SDK device, one current EMMS occurrence; never a Spotify queue. */
(function (root, factory) {
  const api = factory();
  if (typeof module === 'object' && module.exports) module.exports = api;
  else api.boot(root);
})(globalThis, function () {
  'use strict';
  const sdkErrors = ['initialization_error', 'authentication_error', 'account_error', 'playback_error', 'autoplay_failed'];
  const safeKinds = [...sdkErrors, 'play', 'control', 'state', 'transport', 'unexpected_track', 'receiver_queue', 'stop'];
  class Receiver {
    constructor(player, play, send, destroy, now = Date.now) {
      Object.assign(this, { player, play, send, destroy, now, run: null, epoch: 0, closed: false, chain: Promise.resolve(), waiters: [] });
    }
    current(run) { return Boolean(run) && this.run === run && !this.closed; }
    wake() { this.waiters.splice(0).forEach(resolve => resolve()); }
    async until(run, predicate) {
      while (this.current(run) && !predicate()) await new Promise(resolve => this.waiters.push(resolve));
      if (!this.current(run)) throw Error('cancelled');
    }
    fail(kind, run = this.run, status) {
      if (this.closed) return;
      this.closed = true; this.run = null; this.epoch++; this.wake();
      if (run && run.abort) run.abort.abort();
      this.destroy(kind);
      this.send({ type: 'error', run: run ? run.id : 0, seq: run ? run.seq : 0,
        kind: safeKinds.includes(kind) ? kind : 'control',
        status: Number.isInteger(status) && status >= 100 && status <= 599 ? status : null });
    }
    command(command) {
      if (command.kind === 'stop') return this.stop(command);
      const epoch = this.epoch;
      this.chain = this.chain.then(async () => {
        if (this.closed || epoch !== this.epoch) return;
        try { await this.execute(command); }
        catch (error) { if (epoch === this.epoch) this.fail(command.kind === 'play' ? 'play' : 'control', this.run, error.status); }
      });
      return this.chain;
    }
    async execute(command) {
      if (command.kind === 'play') {
        if (this.run || !/^spotify:track:[A-Za-z0-9]{22}$/.test(command.uri)) throw Error('invalid occurrence');
        const run = this.run = { id: command.run, seq: command.seq, uri: command.uri, baseline: false, armed: false,
          paused: false, busy: false, near: null, position: null, duration: null, awaiting: null, abort: new AbortController() };
        await this.play(run.uri, run.abort.signal);
        if (this.current(run)) { run.accepted = true; this.send({ type: 'ack', run: command.run, seq: command.seq }); }
        return;
      }
      const run = this.run;
      if (!run || run.id !== command.run) return;
      run.seq = command.seq; run.near = null; run.finishing = false; run.busy = true;
      // SDK metadata alone is not proof that its decoder is ready for controls.
      await this.until(run, () => run.armed && run.accepted);
      if (command.kind === 'pause') { run.paused = true; await this.player.pause(); }
      else if (command.kind === 'resume') { run.paused = false; await this.player.resume(); }
      else if (command.kind === 'seek') {
        const state = await this.player.getCurrentState();
        if (!this.current(run) || !state || !Number.isFinite(command.seconds)) throw Error('unavailable');
        run.awaiting = Math.max(0, Math.min(state.duration - 1000, state.position + command.seconds * 1000));
        run.position = null;
        await this.player.seek(Math.floor(run.awaiting));
        await this.until(run, () => run.awaiting === null);
      } else throw Error('unknown command');
      if (this.current(run)) {
        run.busy = false; run.near = null;
        this.send({ type: 'ack', run: command.run, seq: command.seq });
      }
    }
    async stop(command) {
      const run = this.run;
      this.run = null; this.epoch++; this.stopping = true; this.wake();
      if (run && run.abort) run.abort.abort();
      try {
        if (!this.closed && (!run || !run.armed)) {
          // pause cannot safely cancel an unstarted asynchronous decoder load.
          this.closed = true; this.destroy();
        } else if (!this.closed) {
          await this.player.pause();
          await this.confirmPaused();
        }
        this.send({ type: 'ack', run: command.run, seq: command.seq });
      } catch (_) { this.fail('stop', run); }
      finally { this.stopping = false; }
    }
    async confirmPaused() {
      // SDK method resolution can precede its cached state transition.
      for (let attempt = 0; attempt < 20; attempt++) {
        const state = await this.player.getCurrentState();
        if (state && state.paused === true) return;
        await new Promise(resolve => setTimeout(resolve, 100));
      }
      throw Error('stop unconfirmed');
    }
    endFrame(run, state) {
      const window = state && state.track_window;
      return state && state.paused === true && state.position === 0 && state.repeat_mode === 0 && state.shuffle === false &&
        window && window.current_track && window.current_track.uri === run.uri &&
        Array.isArray(window.previous_tracks) && window.previous_tracks.some(t => t && t.uri === run.uri) &&
        Array.isArray(window.next_tracks) && window.next_tracks.length === 0;
    }
    observe(state, polled = false) {
      const run = this.run;
      if (!run && !this.closed && !this.stopping && state && state.paused === false) {
        this.fail('unexpected_track'); return;
      }
      if (!this.current(run) || !state || run.ended) return;
      const window = state.track_window, track = window && window.current_track;
      // The SDK can retain the previous paused selection while a new URI loads.
      if (!run.armed && state.paused === true && (!track || track.uri !== run.uri)) return;
      if (!track || track.uri !== run.uri) { this.fail('unexpected_track'); return; }
      if (state.repeat_mode !== 0 || state.shuffle !== false || !Array.isArray(window.next_tracks) || window.next_tracks.length) {
        this.fail('receiver_queue'); return;
      }
      if (run.awaiting !== null) {
        if (!Number.isFinite(state.position) || Math.abs(state.position - run.awaiting) > 1500) return;
        run.awaiting = null; this.wake();
      }
      if (state.paused) {
        if (run.armed && run.accepted && !run.paused && !run.busy && run.awaiting === null && run.near &&
            run.near.seq === run.seq && this.now() - run.near.at < 3000 && this.endFrame(run, state) && !run.finishing) {
          run.finishing = true; this.confirmEnd(run, run.seq);
        }
        return;
      }
      const position = state.position, duration = state.duration;
      if (!Number.isFinite(position) || !Number.isFinite(duration) || duration <= 0 || position < 0) return;
      if (position < 1000) run.baseline = true;
      if (run.baseline && position >= 1000 && (run.position === null || position > run.position)) { run.armed = true; this.wake(); }
      if (polled && run.armed && !run.busy && !run.paused && duration - position <= 1500 && position < duration) {
        run.near = { at: this.now(), seq: run.seq };
      }
      run.position = position; run.duration = duration;
      this.send({ type: 'state', run: run.id, seq: run.seq, position: Math.floor(position), duration: Math.floor(duration) });
    }
    async confirmEnd(run, seq) {
      try {
        const state = await this.player.getCurrentState();
        if (!this.current(run) || run.seq !== seq || run.busy || run.paused || !this.endFrame(run, state)) return;
        await this.player.pause();
        const confirmed = await this.player.getCurrentState();
        if (!this.current(run) || run.seq !== seq || run.busy || run.paused || !this.endFrame(run, confirmed)) return;
        run.ended = true;
        this.send({ type: 'ended', run: run.id, seq });
      } catch (_) { if (this.current(run)) this.fail('state'); }
      finally { if (this.current(run) && run.seq === seq) run.finishing = false; }
    }
    async tick() {
      const run = this.run, seq = run && run.seq;
      if (!this.current(run) || !run) return;
      try {
        const state = await this.player.getCurrentState();
        if (this.current(run) && run.seq === seq) {
          if (!state && run.armed && !run.busy && !run.paused) this.fail('state');
          else this.observe(state, true);
        }
      } catch (_) { if (this.current(run)) this.fail('state'); }
    }
  }
  async function boot(env) {
    const ids = ['authorize', 'activate', 'disconnect', 'status'];
    const el = Object.fromEntries(ids.map(id => [id, env.document.getElementById(id)]));
    const key = env.location.hash.slice(1); env.history.replaceState(null, '', '/');
    let socket, player, receiver, device, failed = false, checking = false, sdkReady;
    const status = text => { el.status.textContent = text; };
    const send = message => {
      if (message.type === 'ended' || (message.type === 'ack' && receiver && !receiver.run && player)) status('Ready — control playback in Emacs');
      if (socket && socket.readyState === 1) socket.send(JSON.stringify(message));
    };
    const destroy = kind => {
      if (player) { try { player.disconnect(); } catch (_) { /* already closed */ } }
      player = null; device = null; el.activate.disabled = true;
      status(safeKinds.includes(kind) ? 'SDK error: ' + kind + '; reconnect from Emacs' : 'Receiver inactive — reconnect from Emacs');
      send({ type: 'ready', ready: false });
    };
    const fail = kind => {
      if (failed) return;
      failed = true;
      if (receiver && !receiver.closed) receiver.fail(kind);
      destroy(); status('SDK error: ' + (safeKinds.includes(kind) ? kind : 'transport') + '; reconnect from Emacs');
      el.authorize.disabled = el.disconnect.disabled = true;
    };
    const api = async (path, method = 'GET') => {
      const response = await env.fetch(path, { method, headers: { 'X-Emacs-Spotify-Probe': key },
        cache: 'no-store', credentials: 'omit', redirect: 'error', referrerPolicy: 'no-referrer', signal: env.AbortSignal.timeout(path === '/token' ? 35000 : 5000) });
      if (!response.ok) throw Error('local session unavailable');
      return response.json();
    };
    const token = async () => (await api('/token')).access_token;
    const connect = async () => {
      if (player || failed) return;
      await sdkReady;
      if (failed) return;
      const instance = new env.Spotify.Player({ name: 'Emacs mixed playlist', volume: 0.5,
        getOAuthToken: callback => { token().then(value => { if (player === instance) callback(value); }).catch(() => { if (player === instance) fail('authentication_error'); }); } });
      player = instance;
      receiver = new Receiver(instance, async (uri, signal) => {
        status('Starting selected track from Emacs');
        const access = await token();
        if (signal.aborted) throw Error('cancelled');
        const response = await env.fetch('https://api.spotify.com/v1/me/player/play?device_id=' + encodeURIComponent(device), {
          method: 'PUT', headers: { Authorization: 'Bearer ' + access, 'Content-Type': 'application/json' },
          body: JSON.stringify({ uris: [uri], position_ms: 0 }), signal: env.AbortSignal.any([signal, env.AbortSignal.timeout(10000)]),
          credentials: 'omit', redirect: 'error', referrerPolicy: 'no-referrer' });
        if (!response.ok) throw Object.assign(Error('play failed'), { status: response.status });
      }, send, destroy);
      for (const name of sdkErrors) instance.addListener(name, () => { if (player === instance) fail(name); });
      instance.addListener('not_ready', () => { if (player === instance) fail('transport'); });
      instance.addListener('ready', ({ device_id }) => {
        if (player !== instance) return;
        device = device_id; el.activate.disabled = !socket || socket.readyState !== 1;
        status('Click Enable audio, then return to Emacs');
      });
      instance.addListener('player_state_changed', state => { if (player === instance) receiver.observe(state); });
      if (!await instance.connect()) fail('transport');
    };
    const pollAuthorization = async () => {
      if (checking || failed || player) return;
      checking = true;
      try {
        const state = await api('/state');
        if (state.error === 'authorization_required') {
          el.authorize.disabled = false; status('Saved authorization expired or was revoked — Authorize Spotify');
        } else if (state.error === 'authorization_storage') {
          el.authorize.disabled = false; status('Cannot save authorization — unlock GPG, then Authorize Spotify');
        } else if (state.error) fail('authentication_error');
        else if (state.authorized) { el.authorize.disabled = true; await connect(); }
      }
      catch (_) { fail('transport'); } finally { checking = false; }
    };
    el.authorize.onclick = async () => {
      const popup = env.open('about:blank', 'spotify-sdk-authorization');
      if (!popup) { status('Allow popups, then authorize'); return; }
      el.authorize.disabled = true;
      try {
        const auth = await api('/authorize', 'POST'), url = new URL(auth.authorization_url);
        if (url.origin !== 'https://accounts.spotify.com' || url.pathname !== '/authorize') throw Error('invalid origin');
        popup.location = url.href; status('Complete authorization in the Spotify tab');
      } catch (_) { popup.close(); fail('authentication_error'); }
    };
    el.activate.onclick = async () => {
      if (!player || !device || failed || !socket || socket.readyState !== 1) return;
      const instance = player;
      try { await instance.activateElement();
        if (failed || player !== instance || socket.readyState !== 1) return;
        send({ type: 'ready', ready: true });
        el.activate.disabled = el.authorize.disabled = true; status('Ready — control playback in Emacs');
      } catch (_) { fail('autoplay_failed'); }
    };
    el.disconnect.onclick = () => { failed = true; destroy(); if (socket) socket.close(); status('Disconnected — reconnect from Emacs'); };
    env.addEventListener('pagehide', el.disconnect.onclick);
    if (!/^[A-Za-z0-9_-]{16,}$/.test(key)) { fail('transport'); return; }
    sdkReady = new Promise((resolve, reject) => {
      env.onSpotifyWebPlaybackSDKReady = resolve;
      const script = env.document.createElement('script'); script.src = 'https://sdk.scdn.co/spotify-player.js';
      script.onerror = reject; env.document.head.appendChild(script);
    });
    sdkReady.catch(() => fail('initialization_error'));
    try {
      const config = await api('/bridge-config');
      if (failed) return;
      if (!/^ws:\/\/127\.0\.0\.1:[0-9]+\/$/.test(config.url)) throw Error('invalid transport');
      socket = new env.WebSocket(config.url);
      socket.onopen = () => {
        if (failed) { socket.close(); return; }
        send({ type: 'hello', key }); el.authorize.disabled = false; el.disconnect.disabled = false;
        if (device && player) el.activate.disabled = false;
      };
      socket.onmessage = event => {
        try {
          const message = JSON.parse(event.data);
          if (message.type === 'ping') send({ type: 'pong' });
          else if (message.type === 'command' && receiver) receiver.command(message);
        } catch (_) { fail('transport'); }
      };
      socket.onclose = () => fail('transport'); socket.onerror = () => fail('transport');
      env.setInterval(pollAuthorization, 1000);
      env.setInterval(() => { if (receiver && !failed) receiver.tick(); }, 500);
      await pollAuthorization();
    } catch (_) { fail('transport'); }
  }
  return { Receiver, boot };
});
