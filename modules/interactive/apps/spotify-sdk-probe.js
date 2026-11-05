/* Official SDK compatibility check. No persistent storage or raw diagnostics. */
(function (root, factory) {
  if (typeof module === 'object' && module.exports) module.exports = factory;
  else factory(root);
})(globalThis, function createProbe(env) {
  'use strict';
  const el = Object.fromEntries(['authorize', 'play', 'pause', 'status', 'track', 'duration', 'trace-box', 'trace']
    .map(id => [id, env.document.getElementById(id)]));
  const capability = env.location.hash.slice(1);
  // Scrub the capability before loading any third-party code.
  env.history.replaceState(null, '', '/');
  let player = null, deviceId = null, uri = null, failed = false;
  let polling = false, connecting = false, trial = false, previewTimer = null, startupTimer = null;
  let ending = false, endingRun = null, endingDuration = null, sampleInterval = null, trialSerial = 0;
  const observations = [];
  const milliseconds = value => Number.isFinite(value) && value >= 0 ? Math.floor(value) : null;
  const record = (source, state) => {
    if (!ending || !trial) return;
    const window = state && state.track_window;
    const row = { source, available: Boolean(state) };
    if (state) Object.assign(row, {
      paused: typeof state.paused === 'boolean' ? state.paused : null,
      position_ms: milliseconds(state.position), duration_ms: milliseconds(state.duration),
      current_matches: Boolean(window && window.current_track && window.current_track.uri === uri),
      previous_matches: window && Array.isArray(window.previous_tracks) ? window.previous_tracks.some(t => t.uri === uri) : null,
      next_count: window && Array.isArray(window.next_tracks) ? window.next_tracks.length : null,
      repeat_mode: [0, 1, 2].includes(state.repeat_mode) ? state.repeat_mode : null,
      shuffle: typeof state.shuffle === 'boolean' ? state.shuffle : null
    });
    observations.push(row);
    if (observations.length > 80) observations.shift();
    el.trace.textContent = observations.map(row => JSON.stringify(row)).join('\n');
    el.trace.scrollTop = el.trace.scrollHeight;
  };
  const clearTimers = () => {
    if (previewTimer !== null) env.clearTimeout(previewTimer);
    if (startupTimer !== null) env.clearTimeout(startupTimer);
    if (sampleInterval !== null) env.clearInterval(sampleInterval);
    previewTimer = startupTimer = sampleInterval = null;
  };
  const status = text => { el.status.textContent = text; };
  const updatePlayLabel = () => {
    el.play.textContent = ending ? (trial ? 'Seek final 8 seconds' : 'Start ending check') : 'Play 15 seconds';
  };
  const halt = () => {
    trial = false; ++trialSerial; endingRun = null; endingDuration = null; clearTimers();
    updatePlayLabel();
    if (player) {
      try { Promise.resolve(player.pause()).catch(() => {}); } catch (_) { /* Best-effort cleanup. */ }
      try { player.disconnect(); } catch (_) { /* The SDK may not have initialized. */ }
    }
    player = null; deviceId = null;
    el.play.disabled = el.pause.disabled = true;
  };
  const fail = (text, source = 'failure') => {
    record(source);
    failed = true; halt(); status(text); el.authorize.disabled = false;
  };
  const safeFailure = (where, error) => {
    const code = error && error.status;
    fail(where + (Number.isInteger(code) && code >= 100 && code <= 599 ? ': HTTP ' + code : ''));
  };
  const api = async (path, method = 'GET') => {
    const response = await env.fetch(path, {
      method, headers: { 'X-Emacs-Spotify-Probe': capability },
      cache: 'no-store', credentials: 'omit', redirect: 'error',
      referrerPolicy: 'no-referrer', signal: env.AbortSignal.timeout(5000)
    });
    if (!response.ok) throw Object.assign(new Error('local request failed'), { status: response.status });
    return response.json();
  };
  const getToken = async () => (await api('/token')).access_token;
  let sdkReady;
  const loadSdk = () => {
    sdkReady = new Promise(resolve => {
      const timeout = env.setTimeout(() => { fail('SDK loading failed'); resolve(false); }, 10000);
      env.onSpotifyWebPlaybackSDKReady = () => {
        env.clearTimeout(timeout); resolve(true);
      };
      const script = env.document.createElement('script');
      script.src = 'https://sdk.scdn.co/spotify-player.js'; script.async = true;
      script.onerror = () => { env.clearTimeout(timeout); fail('SDK loading failed'); resolve(false); };
      env.document.head.appendChild(script);
    });
  };
  const pausePreview = async label => {
    trial = false; clearTimers();
    el.pause.disabled = true; el.play.disabled = !deviceId; el.authorize.disabled = false;
    if (player) {
      try { await player.pause(); status(label); }
      catch (_) { fail('SDK pause failed'); }
    }
  };
  const finishEnding = (label, source) => {
    record(source);
    // One bounded attempt. Do not reconnect automatically or infer completion.
    failed = true; halt(); status(label); el.authorize.disabled = false;
  };
  const currentEnding = (instance, run) => player === instance && trial && !failed && endingRun === run;
  let sampling = false;
  const sampleEnding = async () => {
    if (sampling || !trial || !endingRun || !player) return;
    sampling = true;
    const instance = player, run = endingRun;
    try {
      const state = await instance.getCurrentState();
      if (currentEnding(instance, run)) stateChanged(state, 'poll');
    } catch (_) { if (currentEnding(instance, run)) fail('SDK state sampling failed'); }
    finally { sampling = false; }
  };
  const seekEnding = duration => {
    if (!duration) { fail('SDK duration unavailable for ending check'); return; }
    clearTimers();
    const instance = player;
    const run = endingRun = { pending: true, target: Math.max(0, duration - 8000), duration, tailSeen: false, lastPosition: null };
    record('seek_requested');
    status('Seeking to final 8 seconds');
    previewTimer = env.setTimeout(() => {
      if (currentEnding(instance, run)) finishEnding('Ending check timed out; copy safe trace', 'guard_timeout');
    }, 20000);
    sampleInterval = env.setInterval(sampleEnding, 500);
    try {
      Promise.resolve(instance.seek(run.target)).then(() => {
        if (!currentEnding(instance, run)) return;
        run.pending = false; record('seek_completed');
        sampleEnding();
      }).catch(() => { if (currentEnding(instance, run)) fail('SDK ending seek failed'); });
    } catch (_) { if (currentEnding(instance, run)) fail('SDK ending seek failed'); }
  };
  const stateChanged = (state, source = 'event') => {
    record(source, state);
    if (!state) return;
    if (!ending && state.paused) return;
    // Never permit an unsolicited or late playback transition to keep sounding.
    if (!trial) { if (player) Promise.resolve(player.pause()).catch(() => {}); return; }
    const track = state.track_window && state.track_window.current_track;
    if (!track || track.uri !== uri) {
      if (ending && endingRun) finishEnding('Track changed; playback stopped; copy safe trace', 'unexpected_track');
      else if (!state.paused) fail('Unexpected track; playback stopped');
      return;
    }
    if (ending && state.paused) {
      if (endingRun && endingRun.tailSeen && !endingRun.pending) {
        finishEnding('SDK paused/reset observed; copy safe trace', 'paused_or_reset');
      }
      return;
    }
    const artists = (track.artists || []).map(a => a.name).filter(Boolean).join(', ');
    el.track.textContent = [artists, track.name, 'open in Spotify'].filter(Boolean).join(' — ');
    status('Playing selected track');
    if (startupTimer !== null) env.clearTimeout(startupTimer);
    startupTimer = null;
    if (ending) {
      if (!endingRun) {
        endingDuration = milliseconds(state.duration);
        el.play.disabled = !endingDuration;
        updatePlayLabel();
        status(endingDuration ? 'When you hear music, click Seek final 8 seconds' : 'Waiting for SDK track duration');
        return;
      }
      const position = milliseconds(state.position), run = endingRun;
      if (position === null) return;
      if (run.tailSeen && !run.pending && run.lastPosition >= run.duration - 1500 && position < 1000) {
        finishEnding('SDK playback reset; playback stopped; copy safe trace', 'playback_reset'); return;
      }
      if (position >= run.target && (position > 0 || run.target === 0)) run.tailSeen = true;
      run.lastPosition = position;
      status('Playing final 8 seconds; observing SDK state');
    } else if (previewTimer === null) {
      previewTimer = env.setTimeout(() => pausePreview('Paused after 15 seconds'), 15000);
    }
  };
  const connect = async () => {
    if (player || connecting || failed) return;
    connecting = true;
    try {
      if (!await sdkReady || failed) return;
      status('Connecting SDK player');
      const instance = new env.Spotify.Player({
        name: 'Emacs SDK compatibility check', volume: 0.5,
        getOAuthToken: callback => {
          getToken().then(token => { if (player === instance && !failed) callback(token); })
            .catch(error => { if (player === instance) safeFailure('SDK authorization failed', error); });
        }
      });
      player = instance;
      const listen = (name, callback) => instance.addListener(name, (...args) => {
        if (player === instance && !failed) callback(...args);
      });
      for (const name of ['initialization_error', 'authentication_error', 'account_error', 'playback_error', 'autoplay_failed']) {
        listen(name, () => fail('SDK error: ' + name, name));
      }
      listen('ready', ({ device_id }) => {
        if (failed) return;
        deviceId = device_id; el.play.disabled = false; el.authorize.disabled = false;
        status(ending ? 'Ready — click Start ending check' : 'Ready — click Play 15 seconds');
      });
      listen('not_ready', () => fail('SDK device unavailable'));
      listen('player_state_changed', state => stateChanged(state));
      const connected = await instance.connect();
      if (player === instance && !connected) fail('SDK connection failed');
    } catch (_) { fail('SDK initialization failed'); }
    finally { connecting = false; }
  };
  const poll = async () => {
    if (polling || failed) return;
    polling = true;
    try {
      const state = await api('/state');
      if (!/^spotify:track:[A-Za-z0-9]{22}$/.test(state.track_uri)) throw new Error('invalid track');
      uri = state.track_uri;
      ending = state.ending_check === true;
      updatePlayLabel();
      el['trace-box'].hidden = !ending;
      if (ending) el.duration.textContent = 'Start normally; seek only after you hear music. No EMMS advancement.';
      el.track.href = 'https://open.spotify.com/track/' + uri.split(':')[2];
      if (state.error) {
        // Server error text is never displayed; only this finite enum is allowed.
        fail(state.error === 'authorization_denied' ? 'Authorization denied; authorize again' : 'Authorization failed; authorize again');
      } else if (state.authorized) await connect();
      else if (player) fail('Authorization expired; authorize again');
    } catch (error) { safeFailure('Local SDK session unavailable; run the Emacs command again', error); }
    finally { polling = false; }
  };
  el.authorize.onclick = async () => {
    // Create the popup inside the click, before awaiting network I/O.
    const popup = env.open('about:blank', 'spotify-sdk-authorization');
    if (!popup) { status('Allow popups for this loopback page, then authorize'); return; }
    halt(); failed = false;
    el.authorize.disabled = true; status('Complete authorization in the Spotify tab');
    try {
      const auth = await api('/authorize', 'POST');
      const url = new URL(auth.authorization_url);
      if (url.origin !== 'https://accounts.spotify.com' || url.pathname !== '/authorize') throw new Error('invalid authorization target');
      popup.location = url.href;
    } catch (error) { popup.close(); safeFailure('Cannot start authorization', error); }
  };
  el.play.onclick = async () => {
    if (!player || !deviceId || failed) return;
    if (ending && trial) {
      if (endingRun || !endingDuration) return;
      el.play.disabled = true;
      record('seek_clicked');
      seekEnding(endingDuration);
      return;
    }
    trial = true; clearTimers(); endingRun = null; endingDuration = null;
    const serial = ++trialSerial, instance = player, device = deviceId;
    if (ending) previewTimer = env.setTimeout(() => {
      if (trial && trialSerial === serial && !endingRun) {
        finishEnding('Ending check timed out before seek; copy safe trace', 'before_seek_timeout');
      }
    }, 30000);
    observations.length = 0;
    if (ending) el.trace.textContent = '';
    el.play.disabled = el.authorize.disabled = true; el.pause.disabled = false;
    status('Starting selected track');
    startupTimer = env.setTimeout(() => fail('SDK playback not observed within 10 seconds'), 10000);
    try {
      // Must be invoked in the actual click, before asynchronous token retrieval.
      await instance.activateElement();
      const token = await getToken();
      if (!trial || player !== instance || trialSerial !== serial) return;
      const response = await env.fetch('https://api.spotify.com/v1/me/player/play?device_id=' + encodeURIComponent(device), {
        method: 'PUT', headers: { Authorization: 'Bearer ' + token, 'Content-Type': 'application/json' },
        body: JSON.stringify({ uris: [uri], position_ms: 0 }),
        credentials: 'omit', redirect: 'error', referrerPolicy: 'no-referrer',
        signal: env.AbortSignal.timeout(10000)
      });
      if (!response.ok) throw Object.assign(new Error('playback failed'), { status: response.status });
    } catch (error) { if (trial && player === instance && trialSerial === serial) safeFailure('Playback request failed', error); }
  };
  el.pause.onclick = () => ending
    ? finishEnding('Ending check cancelled; copy safe trace', 'manual_pause')
    : pausePreview('Paused');
  let interval;
  env.addEventListener('pagehide', () => { env.clearInterval(interval); halt(); });
  const boot = (async () => {
    if (!/^[A-Za-z0-9_-]{16,}$/.test(capability)) {
      fail('Session link expired; run the Emacs command again'); return;
    }
    el.authorize.disabled = false;
    loadSdk();
    await poll();
    interval = env.setInterval(poll, 1000);
  })();
  return { boot, poll };
});
