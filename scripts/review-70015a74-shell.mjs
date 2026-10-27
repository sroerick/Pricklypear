#!/usr/bin/env node
// Card 70015a74 UI review, part 2: the chat WINDOW is a filmstrip pane, where
// the log is a bounded scroll container. Opens /chat through the same htmx
// app-link path the shell uses (POST /filmstrip/open), then asserts autoscroll
// under the 4s poll, a scrolled-up reader, send snap + composer clear, and two
// lazy-history pages (advance then delete the control).
import { spawn } from 'node:child_process';
import { setTimeout as sleep } from 'node:timers/promises';
import http from 'node:http';
import fs from 'node:fs';
import path from 'node:path';

const BASE = (process.env.PP_URL || 'http://127.0.0.1:18080').replace(/\/$/, '');
const AUTH = process.env.PP_PASS
  ? 'Basic ' + Buffer.from((process.env.PP_USER || 'admin') + ':' + process.env.PP_PASS).toString('base64')
  : 'Bearer ' + (process.env.PP_TOKEN || '');
const CHROME = process.env.CHROME || '/usr/local/bin/chrome';
const OUT = path.resolve(process.env.REVIEW_OUT || 'screenshots/review-70015a74');
const PORTC = 9700 + (process.pid % 200);
const USER_DATA = '/tmp/chrome-review-70015a74s-' + process.pid;

let fails = 0;
const results = [];
function ok(cond, name, detail = '') {
  console.log((cond ? 'OK   ' : 'FAIL ') + name + (detail ? ' : ' + detail : ''));
  results.push({ name, pass: !!cond, detail });
  if (!cond) fails++;
}
let chrome;
const cleanup = () => { try { if (chrome) chrome.kill('SIGKILL'); } catch (_) {} try { fs.rmSync(USER_DATA, { recursive: true, force: true }); } catch (_) {} };
process.on('exit', cleanup);
setTimeout(() => { console.error('FAIL watchdog 180s'); cleanup(); process.exit(1); }, 180000).unref();

function httpReq(url) {
  return new Promise((resolve) => {
    const req = http.request(url, (res) => { let d = ''; res.on('data', (c) => (d += c)); res.on('end', () => resolve({ status: res.statusCode, body: d })); });
    req.on('error', (e) => resolve({ status: 0, body: String(e) }));
    req.end();
  });
}
const wsData = (ev) => { const d = ev?.data ?? ev; return typeof d === 'string' ? d : Buffer.from(d).toString(); };
function cdpSend(ws, id, method, params = {}) {
  return new Promise((resolve, reject) => {
    const timer = setTimeout(() => { clean(); reject(new Error('CDP timeout ' + method)); }, 20000);
    const onMsg = (ev) => { try { const j = JSON.parse(wsData(ev)); if (j.id === id) { clean(); j.error ? reject(new Error(JSON.stringify(j.error))) : resolve(j.result); } } catch (_) {} };
    const onErr = (e) => { clean(); reject(e?.error || e); };
    function clean() { clearTimeout(timer); ws.removeEventListener('message', onMsg); ws.removeEventListener('error', onErr); }
    ws.addEventListener('message', onMsg); ws.addEventListener('error', onErr);
    ws.send(JSON.stringify({ id, method, params }));
  });
}

fs.mkdirSync(OUT, { recursive: true });
chrome = spawn(CHROME, ['--remote-debugging-port=' + PORTC, '--user-data-dir=' + USER_DATA, '--headless=new', '--disable-gpu', '--no-sandbox', '--disable-dev-shm-usage', '--window-size=1200,900', 'about:blank'], { stdio: ['ignore', 'ignore', 'pipe'] });
const chromeErr = [];
chrome.stderr.on('data', (c) => chromeErr.push(String(c)));
let ready = false;
for (let i = 0; i < 120; i++) { const v = await httpReq('http://127.0.0.1:' + PORTC + '/json/version'); if (v.status === 200) { ready = true; break; } await sleep(250); }
if (!ready) { console.error('FAIL chromium CDP not ready'); console.error((chromeErr.join('') || '').split('\n').filter((l) => !/dbus|bus\.cc|object_proxy/.test(l)).slice(-8).join('\n')); process.exit(1); }
const list = JSON.parse((await httpReq('http://127.0.0.1:' + PORTC + '/json/list')).body);
const ws = new globalThis.WebSocket(list.find((t) => t.type === 'page').webSocketDebuggerUrl);
await new Promise((res, rej) => { const t = setTimeout(() => rej(new Error('ws timeout')), 10000); ws.onopen = () => { clearTimeout(t); res(); }; ws.onerror = (e) => { clearTimeout(t); rej(e?.error || e); }; });
let nextId = 1;
const send = (m, p) => cdpSend(ws, nextId++, m, p);
await send('Page.enable');
await send('Runtime.enable');
await send('Network.enable');
await send('Network.setExtraHTTPHeaders', { headers: { Authorization: AUTH } });
await send('Page.navigate', { url: BASE + '/filmstrip' });

const js = async (expr) => {
  const r = await send('Runtime.evaluate', { expression: expr, returnByValue: true, awaitPromise: true });
  if (r.exceptionDetails) throw new Error(JSON.stringify(r.exceptionDetails).slice(0, 300));
  return r.result.value;
};
async function waitFor(desc, fn, timeout = 20000, interval = 250) {
  const t0 = Date.now();
  while (Date.now() - t0 < timeout) { const v = await fn(); if (v) return v; await sleep(interval); }
  throw new Error('timeout waiting for ' + desc);
}
async function snap(name) {
  const r = await send('Page.captureScreenshot', { format: 'png' });
  const p = path.join(OUT, name); fs.writeFileSync(p, Buffer.from(r.data, 'base64')); return p;
}
const metrics = () => js(`(() => {
  const l = document.getElementById('chat-main-log');
  if (!l) return { missing: true };
  const pane = l.closest('.fs-pane');
  const b = document.querySelector('.chat-older-btn');
  const t = document.querySelector('.fs-pane textarea.chat-input');
  return {
    bubbles: l.querySelectorAll('.bubble').length,
    gap: Math.round(l.scrollHeight - l.scrollTop - l.clientHeight),
    scrollHeight: l.scrollHeight, clientHeight: l.clientHeight,
    paneH: pane ? Math.round(pane.getBoundingClientRect().height) : -1,
    older: document.querySelectorAll('#chat-older .bubble').length,
    olderMore: !!document.getElementById('chat-older-more'),
    btnHref: b ? b.getAttribute('hx-get') : null,
    btnText: b ? b.textContent.trim() : null,
    composer: t ? t.value : null,
    docScroll: Math.round(document.scrollingElement.scrollTop),
  };
})()`);

// open the chat pane through the shell's own app-link path
const waitHtmx = await waitFor('htmx', () => js('typeof htmx === "object" || typeof htmx === "function"'), 20000);
ok(!!waitHtmx, 'shell loaded with htmx');
await js(`htmx.ajax('POST','/filmstrip/open',{values:{route:'/chat'},target:'#fs-sink',swap:'innerHTML'})`);
let m = await waitFor('chat pane log', async () => { const x = await metrics(); return x.missing ? null : x; }, 25000);
ok(m.paneH > 0, 'chat renders inside a .fs-pane', 'pane height=' + m.paneH);
const windowSize = 50;
m = await waitFor('windowed log', async () => { const x = await metrics(); return x.bubbles === windowSize ? x : null; }, 25000);
ok(m.bubbles === windowSize, 'pane log renders one bounded window', 'bubbles=' + m.bubbles);

m = await waitFor('pane snap', async () => { const x = await metrics(); return x.gap <= 4 ? x : null; }, 15000);
ok(m.scrollHeight > m.clientHeight, 'pane log is a real scroll container', `scrollHeight=${m.scrollHeight} clientHeight=${m.clientHeight} paneH=${m.paneH}`);
ok(m.gap <= 4, 'pane log lands on the latest message', 'gap=' + m.gap);
const shot1 = await snap('05-pane-chat-bottom.png');

const box = await js(`(() => { const r = document.getElementById('chat-main-log').getBoundingClientRect(); return { x: Math.round(r.left + r.width / 2), y: Math.round(r.top + r.height / 2) }; })()`);
for (let i = 0; i < 4; i++) { await send('Input.dispatchMouseEvent', { type: 'mouseWheel', x: box.x, y: box.y, deltaX: 0, deltaY: -600 }); await sleep(120); }
await sleep(500);
m = await metrics();
ok(m.gap > 160, 'wheel-up scrolls the reader off the bottom in the pane', 'gap=' + m.gap);
await sleep(5000);
m = await metrics();
ok(m.gap > 160, 'pane poll does NOT yank a scrolled-up reader', 'gap=' + m.gap);
const shot2 = await snap('06-pane-reader-scrolled-up.png');
for (let i = 0; i < 6; i++) { await send('Input.dispatchMouseEvent', { type: 'mouseWheel', x: box.x, y: box.y, deltaX: 0, deltaY: 800 }); await sleep(120); }
m = await waitFor('re-arm', async () => { const x = await metrics(); return x.gap <= 4 ? x : null; }, 8000);
ok(m.gap <= 4, 'returning to the bottom re-arms the pane snap');
await sleep(4500);
m = await metrics();
ok(m.gap <= 4, 'pane poll holds a re-armed reader at the latest');

// lazy history: page 1 advances the control, page 2 exhausts and deletes it
const firstHref = m.btnHref;
await js(`document.querySelector('.chat-older-btn').click()`);
m = await waitFor('older page 1', async () => { const x = await metrics(); return x.older > 0 ? x : null; }, 15000);
const after1 = await metrics();
ok(after1.older > 0, 'pane Load older prepends history into #chat-older', 'older=' + after1.older);
ok(after1.olderMore === true && after1.btnHref !== firstHref, 'control advances to the next older cursor', `href=${after1.btnHref}`);
ok(after1.gap <= 4, 'history load leaves the parked pane log alone', 'gap=' + after1.gap);
const shot3 = await snap('07-pane-history-loaded.png');
await js(`document.querySelector('.chat-older-btn').click()`);
await sleep(2500);
const after2 = await metrics();
const noOlder = await js(`document.querySelector('#chat-older') ? document.querySelector('#chat-older').textContent.includes('No older messages') : false`);
ok(after2.olderMore === false, 'second page deletes the load-older control at the oldest row', 'olderMore=' + after2.olderMore);
ok(noOlder, 'exhausted history says "No older messages."');
ok(after2.gap <= 4, 'second history load leaves the pane log alone', 'gap=' + after2.gap);

// send: snap + composer reset
const marker = 'ui review 70015a74 pane send';
await js(`(() => { const f = document.querySelector('.fs-pane form.chat-composer'); const t = f.querySelector('textarea[name=body]'); t.value = ${JSON.stringify(marker)}; t.dispatchEvent(new Event('input',{bubbles:true})); f.requestSubmit(); })()`);
await waitFor('sent bubble', async () => { const seen = await js(`Array.from(document.querySelectorAll('#chat-main-log .bubble-body')).some((n) => n.textContent.includes(${JSON.stringify(marker)}))`); const x = await metrics(); return seen && x.bubbles === windowSize ? x : null; }, 20000);
m = await metrics();
ok(m.gap <= 4, 'send snaps the pane log to the latest', 'gap=' + m.gap);
ok(m.composer === '', 'send clears the composer', JSON.stringify(m.composer));
const shot4 = await snap('08-pane-after-send.png');

fs.writeFileSync(path.join(OUT, 'results-shell.json'), JSON.stringify({ base: BASE, results }, null, 1));
console.log('\nARTIFACTS ' + OUT);
console.log('checks ' + (results.length - fails) + '/' + results.length + ' passed');
process.exit(fails ? 1 : 0);
