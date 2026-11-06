/* Optional real-Chrome integration. Synthetic SDK/credentials; no service audio. */
const test = require('node:test');
const assert = require('node:assert/strict');
const fs = require('node:fs');
const os = require('node:os');
const path = require('node:path');
const net = require('node:net');
const { spawn } = require('node:child_process');
const chromePath = '/Applications/Google Chrome.app/Contents/MacOS/Google Chrome';
const sleep = ms => new Promise(resolve => setTimeout(resolve, ms));
async function waitFor(fn, ms = 10000) {
  const end = Date.now() + ms;
  while (Date.now() < end) { try { const value = await fn(); if (value) return value; } catch (_) {} await sleep(50); }
  throw Error('Synthetic browser fixture did not reach the expected state');
}
async function freePort() {
  const server = net.createServer(); await new Promise(resolve => server.listen(0, '127.0.0.1', resolve));
  const port = server.address().port; await new Promise(resolve => server.close(resolve)); return port;
}
async function cdp(url) {
  const ws = new WebSocket(url), pending = new Map(); let serial = 0;
  await new Promise((resolve, reject) => { ws.onopen = resolve; ws.onerror = reject; });
  ws.onmessage = event => {
    const message = JSON.parse(event.data), request = pending.get(message.id);
    if (request) { pending.delete(message.id); message.error ? request.reject(Error('CDP request failed')) : request.resolve(message.result); }
  };
  return { close: () => ws.close(), call(method, params = {}) {
    return new Promise((resolve, reject) => { const id = ++serial; pending.set(id, { resolve, reject }); ws.send(JSON.stringify({ id, method, params })); });
  } };
}
async function stop(child) {
  if (!child || child.exitCode !== null || child.signalCode !== null) return;
  const closed = new Promise(resolve => child.once('close', resolve)); child.kill('SIGTERM'); await closed;
}
test('real Chrome activates the receiver, drives a mixed EMMS queue, and renders safe SDK errors', {
  skip: !fs.existsSync(chromePath), timeout: 60000
}, async context => {
  for (const screen of ['ready', 'mixed', 'controls', 'error']) await context.test(screen, async () => {
    const port = await freePort(), profile = fs.mkdtempSync(path.join(os.tmpdir(), 'spotify-browser-ui-'));
    let emacs, chrome, client;
    try {
      emacs = spawn('emacs', ['--batch', '-Q', '-L', 'lisp', '-l', 'test/test-helpers.el',
        '-l', 'test/personal-apps/spotify-browser-ui-fixture.el', '--eval', `(hub/spotify-browser-ui-fixture '${screen} ${port})`], { stdio: 'ignore' });
      const origin = `http://127.0.0.1:${port}`;
      await waitFor(async () => (await fetch(origin + '/')).ok);
      chrome = spawn(chromePath, ['--headless=new', '--disable-gpu', '--no-first-run', '--no-default-browser-check',
        '--disable-background-networking', '--disable-sync', '--remote-debugging-address=127.0.0.1', '--remote-debugging-port=0',
        '--host-resolver-rules=MAP * ~NOTFOUND, EXCLUDE 127.0.0.1', '--user-data-dir=' + profile, 'about:blank'], { stdio: 'ignore' });
      const debugPort = await waitFor(() => fs.readFileSync(path.join(profile, 'DevToolsActivePort'), 'utf8').split('\n')[0]);
      const tabs = await (await fetch(`http://127.0.0.1:${debugPort}/json/list`)).json();
      client = await cdp(tabs.find(tab => tab.type === 'page').webSocketDebuggerUrl);
      await client.call('Page.enable');
      await client.call('Emulation.setDeviceMetricsOverride', { width: 800, height: 440, deviceScaleFactor: 1, mobile: false });
      await client.call('Page.navigate', { url: origin + '/#synthetic-capability' });
      const evaluate = async expression => (await client.call('Runtime.evaluate', { expression, returnByValue: true })).result.value;
      await waitFor(() => evaluate('Boolean(document.getElementById("activate") && !document.getElementById("activate").disabled)'));
      await evaluate('document.getElementById("activate").click(); true');
      if (screen === 'error') {
        await waitFor(() => evaluate('document.getElementById("status").textContent.includes("SDK error: playback_error")'));
        assert.doesNotMatch(await evaluate('document.body.textContent'), /PRIVATE/);
      } else {
        await waitFor(async () => (await (await fetch(origin + '/fixture-state')).json()).ready);
        if (screen === 'mixed') {
          const state = await waitFor(async () => { const value = await (await fetch(origin + '/fixture-state')).json(); return value.done && value; });
          assert.equal(state.local, 1);
          assert.equal(await evaluate('window.fixtureStarts'), 2);
        }
        if (screen === 'controls') {
          await waitFor(async () => (await (await fetch(origin + '/fixture-state')).json()).playing);
          for (const action of ['seek', 'pause', 'seek', 'pause']) {
            assert.ok((await fetch(origin + '/fixture-' + action)).ok);
            await waitFor(async () => { const value = await (await fetch(origin + '/fixture-state')).json(); return !value.pending && value.playing && !value.error; });
          }
          // Stay beyond the old ten-second control deadline to catch latent disconnects.
          await sleep(11000);
          const value = await (await fetch(origin + '/fixture-state')).json();
          assert.equal(value.ready, true); assert.equal(value.playing, true); assert.equal(value.error, false);
          // A real SDK token callback waits for native renewal beyond the old 5s fetch deadline.
          assert.ok((await fetch(origin + '/fixture-renew')).ok);
          await evaluate('window.fixtureRenew(); true');
          await waitFor(() => evaluate('window.fixtureRenewed'));
          const renewed = await (await fetch(origin + '/fixture-state')).json();
          assert.equal(renewed.renewals, 1);
          assert.equal(renewed.ready, true); assert.equal(renewed.playing, true); assert.equal(renewed.error, false);
          assert.equal(await evaluate('window.fixtureStarts'), 1);
        } else assert.equal(await evaluate('document.getElementById("status").textContent'), 'Ready — control playback in Emacs');
      }
      const shot = await client.call('Page.captureScreenshot', { format: 'png' });
      fs.writeFileSync(path.join(os.tmpdir(), `spotify-browser-ui-${screen}.png`), Buffer.from(shot.data, 'base64'));
    } finally {
      if (client) client.close(); await stop(chrome); await stop(emacs); fs.rmSync(profile, { recursive: true, force: true });
    }
  });
});
