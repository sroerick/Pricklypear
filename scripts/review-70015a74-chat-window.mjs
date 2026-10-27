#!/usr/bin/env node
// Card 70015a74 UI review (FACES lane): chat autoscroll + lazy history.
//
// Drives a REAL headless chromium over CDP against a running image and asserts
// the rendered behaviour, not the handler: the live window is bounded, the log
// lands on the latest message, a 4s poll keeps it there, a reader scrolled up
// is NOT yanked back, sending snaps to the latest, and Load older prepends one
// page into #chat-older (advancing then removing the control).
//
// Auth: PP_PASS -> HTTP Basic (local dev image), else PP_TOKEN -> Bearer.
// Env: PP_URL (default http://127.0.0.1:18080), REVIEW_OUT.
// Exit 0 when every check passes.
import { spawn } from 'node:child_process';
import { setTimeout as sleep } from 'node:timers/promises';
import http from 'node:http';
import fs from 'node:fs';
import path from 'node:path';

const BASE = (process.env.PP_URL || 'http://127.0.0.1:18080').replace(/\/$/, '');
const USER = process.env.PP_USER || 'admin';
const PASS = process.env.PP_PASS || '';
const TOKEN = process.env.PP_TOKEN || '';
const AUTH = PASS
  ? 'Basic ' + Buffer.from(USER + ':' + PASS).toString('base64')
  : 'Bearer ' + TOKEN;
const CHROME = process.env.CHROME || '/usr/local/bin/chrome';
const OUT = path.resolve(process.env.REVIEW_OUT || 'screenshots/review-70015a74');
const PORTC = 9400 + (process.pid % 300);
const USER_DATA = '/tmp/chrome-review-70015a74-' + process.pid;

let fails = 0;
const results = [];
function ok(cond, name, detail = '') {
  console.log((cond ? 'OK   ' : 'FAIL ') + name + (detail ? ' : ' + detail : ''));
  results.push({ name, pass: !!cond, detail });
  if (!cond) fails++;
}

let chrome;
const cleanup = () => {
  try { if (chrome) chrome.kill('SIGKILL'); } catch (_) {}
  try { fs.rmSync(USER_DATA, { recursive: true, force: true }); } catch (_) {}
};
process.on('exit', cleanup);
setTimeout(() => { console.error('FAIL watchdog 180s'); cleanup(); process.exit(1); }, 180000).unref();

function httpReq(url) {
  return new Promise((resolve) => {
    const req = http.request(url, { method: 'GET' }, (res) => {
      let d = '';
      res.on('data', (c) => (d += c));
      res.on('end', () => resolve({ status: res.statusCode, body: d }));
    });
    req.on('error', (e) => resolve({ status: 0, body: String(e) }));
    req.end();
  });
}

function apiEval(expr) {
  return new Promise((resolve, reject) => {
    const body = JSON.stringify({ expr });
    const req = http.request(new URL('/api/eval', BASE), {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
        'Content-Length': Buffer.byteLength(body),
        Authorization: AUTH,
      },
    }, (res) => {
      let d = '';
      res.on('data', (c) => (d += c));
      res.on('end', () => {
        try {
          const j = JSON.parse(d);
          if (j.ok === false) reject(new Error(j.error || 'api error'));
          else resolve(j.value);
        } catch (e) {
          reject(new Error('api parse: ' + d.slice(0, 200)));
        }
      });
    });
    req.on('error', reject);
    req.end(body);
  });
}

function wsData(ev) {
  const d = ev?.data ?? ev;
  return typeof d === 'string' ? d : Buffer.from(d).toString();
}

function cdpSend(ws, id, method, params = {}) {
  return new Promise((resolve, reject) => {
    const timer = setTimeout(() => { clean(); reject(new Error('CDP timeout ' + method)); }, 20000);
    const onMsg = (ev) => {
      try {
        const j = JSON.parse(wsData(ev));
        if (j.id === id) { clean(); j.error ? reject(new Error(JSON.stringify(j.error))) : resolve(j.result); }
      } catch (_) {}
    };
    const onErr = (e) => { clean(); reject(e?.error || e); };
    function clean() { clearTimeout(timer); ws.removeEventListener('message', onMsg); ws.removeEventListener('error', onErr); }
    ws.addEventListener('message', onMsg);
    ws.addEventListener('error', onErr);
    ws.send(JSON.stringify({ id, method, params }));
  });
}

fs.mkdirSync(OUT, { recursive: true });

// ---- chromium ----
chrome = spawn(CHROME, [
  '--remote-debugging-port=' + PORTC, '--user-data-dir=' + USER_DATA,
  '--headless=new', '--disable-gpu', '--no-sandbox', '--disable-dev-shm-usage',
  '--window-size=1200,900', 'about:blank',
], { stdio: ['ignore', 'ignore', 'pipe'] });
// Single-vCPU box: give chromium up to 30s. dbus noise is expected here.
const chromeErr = [];
chrome.stderr.on('data', (c) => chromeErr.push(String(c)));
let ready = false;
for (let i = 0; i < 120; i++) {
  const v = await httpReq('http://127.0.0.1:' + PORTC + '/json/version');
  if (v.status === 200) { ready = true; break; }
  await sleep(250);
}
if (!ready) {
  console.error('FAIL chromium CDP not ready after 30s');
  console.error((chromeErr.join('') || '(no stderr)').split('\n').filter((l) => !/dbus|bus\.cc|object_proxy/.test(l)).slice(-10).join('\n'));
  process.exit(1);
}
const list = JSON.parse((await httpReq('http://127.0.0.1:' + PORTC + '/json/list')).body);
const page = list.find((t) => t.type === 'page');
const ws = new globalThis.WebSocket(page.webSocketDebuggerUrl);
await new Promise((res, rej) => {
  const t = setTimeout(() => rej(new Error('ws timeout')), 10000);
  ws.onopen = () => { clearTimeout(t); res(); };
  ws.onerror = (e) => { clearTimeout(t); rej(e?.error || e); };
});
let nextId = 1;
const send = (m, p) => cdpSend(ws, nextId++, m, p);

// ---- network capture (poll + lazy-history fragments) ----
const net = { reqs: new Map(), fragments: [] };
ws.addEventListener('message', (ev) => {
  let j;
  try { j = JSON.parse(wsData(ev)); } catch (_) { return; }
  if (j.method === 'Network.requestWillBeSent') {
    const u = j.params.request.url;
    if (/\/chat(\?|$|\/partial)/.test(u)) net.reqs.set(j.params.requestId, u);
  } else if (j.method === 'Network.loadingFinished') {
    const u = net.reqs.get(j.params.requestId);
    if (u && (/\/chat\/partial/.test(u) || /\/chat\?(before|dm)/.test(u))) {
      send('Network.getResponseBody', { requestId: j.params.requestId })
        .then((r) => net.fragments.push({ url: u, body: r.base64Encoded ? Buffer.from(r.body, 'base64').toString() : r.body }))
        .catch(() => {});
    }
  }
});

await send('Page.enable');
await send('Runtime.enable');
await send('Network.enable');
await send('Network.setCacheDisabled', { cacheDisabled: true });
await send('Network.setExtraHTTPHeaders', { headers: { Authorization: AUTH } });
await send('Accessibility.enable');

await send('Page.navigate', { url: BASE + '/chat' });

const js = async (expr) => {
  const r = await send('Runtime.evaluate', { expression: expr, returnByValue: true, awaitPromise: true });
  if (r.exceptionDetails) throw new Error(JSON.stringify(r.exceptionDetails).slice(0, 300));
  return r.result.value;
};
const metrics = () => js(`(() => {
  const l = document.getElementById('chat-main-log');
  if (!l) return { missing: true };
  const o = document.querySelector('#chat-older');
  const b = document.querySelector('.chat-older-btn');
  return {
    bubbles: l.querySelectorAll('.bubble').length,
    gap: Math.round(l.scrollHeight - l.scrollTop - l.clientHeight),
    scrollHeight: l.scrollHeight, clientHeight: l.clientHeight,
    older: o ? o.querySelectorAll('.bubble').length : -1,
    olderMore: !!document.getElementById('chat-older-more'),
    btnHref: b ? b.getAttribute('hx-get') : null,
    btnText: b ? b.textContent.trim() : null,
    stick: l.dataset.stick ?? null,
    composer: (document.querySelector('form.chat-composer textarea') || {}).value ?? null,
    olderTop: o ? Math.round(o.getBoundingClientRect().top) : null,
    logTop: Math.round(l.getBoundingClientRect().top),
  };
})()`);

async function waitFor(desc, fn, timeout = 20000, interval = 250) {
  const t0 = Date.now();
  let last;
  while (Date.now() - t0 < timeout) {
    last = await fn();
    if (last) return last;
    await sleep(interval);
  }
  throw new Error('timeout waiting for ' + desc);
}

async function snap(name) {
  const r = await send('Page.captureScreenshot', { format: 'png' });
  const p = path.join(OUT, name);
  fs.writeFileSync(p, Buffer.from(r.data, 'base64'));
  return p;
}

// ---- 0. room size from the image (the DOM must be a bounded window) ----
const total = await apiEval('(length (chat/history chat/lobby-id 100000))');
const windowSize = await apiEval('chat/window-size');
ok(total >= 50, 'room has history beyond one page', `total=${total}`);

// ---- 1. initial bounded render ----
let m = await waitFor('chat log', async () => { const x = await metrics(); return x.missing ? null : x; });
m = await waitFor('50-bubble window', async () => {
  const x = await metrics();
  return x.bubbles === windowSize ? x : null;
});
ok(m.bubbles === windowSize, 'live window renders exactly chat/window-size', `bubbles=${m.bubbles} total=${total} window=${windowSize}`);
ok(total > m.bubbles, 'initial DOM is a window, not the whole room', `dom=${m.bubbles} < room=${total}`);
ok(m.older === 0 && m.olderMore && m.btnText === 'Load older messages', 'lazy-history control present', `text=${JSON.stringify(m.btnText)}`);
ok(/^\/chat\?before=/.test(m.btnHref || ''), 'control carries a before= cursor', m.btnHref);

// First htmx poll swap is what snaps the log to the latest message.
m = await waitFor('initial snap to bottom', async () => {
  const x = await metrics();
  return x.gap <= 4 ? x : null;
}, 15000);
ok(m.gap <= 4, 'load snaps the log to the latest message', `gap=${m.gap} scrollHeight=${m.scrollHeight} clientHeight=${m.clientHeight}`);
ok(m.scrollHeight > m.clientHeight, 'log is actually scrollable (not a short list)', `scrollHeight=${m.scrollHeight} clientHeight=${m.clientHeight}`);
const shot1 = await snap('01-chat-window-bottom.png');

// ---- 2. component registration ----
const elements = await apiEval('(lookbook/elements)');
for (const name of ['ui/chat-log', 'ui/chatpage', 'chat/older-region', 'chat/older-bar', 'chat/older-more', 'chat/older-oob', 'chat/older-gone']) {
  ok(elements.includes(name), 'lookbook registers ' + name, 'elements=' + elements.length);
}
const dom = await js(`(() => {
  const l = document.getElementById('chat-main-log');
  const o = document.getElementById('chat-older');
  const more = document.getElementById('chat-older-more');
  return {
    olderAboveLog: o.getBoundingClientRect().top < l.getBoundingClientRect().top,
    btnInCarrier: !!(more && more.querySelector('button.chat-older-btn')),
    logClasses: l.className, olderClasses: o.className,
    beforeSwap: l.getAttribute('hx-on::before-swap') || '',
    afterSwap: l.getAttribute('hx-on::after-swap') || '',
    trigger: l.getAttribute('hx-trigger') || '',
    pageComposer: !!document.querySelector('.chatpage form.chat-composer'),
  };
})()`);
ok(dom.btnInCarrier && dom.olderAboveLog, '#chat-older sits above the polled log, carrier wraps the button', JSON.stringify({ olderAboveLog: dom.olderAboveLog, btnInCarrier: dom.btnInCarrier }));
ok(/dataset\.stick/.test(dom.beforeSwap) && /dataset\.stick/.test(dom.afterSwap), 'log renders the measured-position autoscroll hooks', 'before=' + dom.beforeSwap.slice(0, 60));
ok(dom.trigger === 'load, every 4s', 'log polls load + every 4s', dom.trigger);
ok(dom.pageComposer, 'composer is the declared .chat-composer inside .chatpage');

// a11y snapshot before the control disappears.
const ax = await send('Accessibility.getFullAXTree');
fs.writeFileSync(path.join(OUT, 'ax-initial.json'), JSON.stringify(ax, null, 1));
const btnNode = (ax.nodes || []).find((n) => n.role?.value === 'button' && (n.name?.value || '').includes('Load older messages'));
ok(!!btnNode, 'a11y tree exposes a button named "Load older messages"', btnNode ? 'nodeId=' + btnNode.nodeId : 'not found');

// ---- 3. the 4s poll keeps a parked reader at the bottom, bounded ----
const partialBefore = net.fragments.filter((f) => /\/chat\/partial/.test(f.url)).length;
await sleep(5000);
m = await metrics();
const partialAfter = net.fragments.filter((f) => /\/chat\/partial/.test(f.url)).length;
ok(partialAfter > partialBefore, 'the 4s poll actually issued /chat/partial', `requests=${partialAfter}`);
ok(m.bubbles === windowSize, 'poll re-renders the same bounded window', `bubbles=${m.bubbles}`);
ok(m.gap <= 4, 'poll keeps a parked reader at the latest', `gap=${m.gap}`);
const partialBody = net.fragments.filter((f) => /\/chat\/partial/.test(f.url)).pop();
const partialBubbles = partialBody ? (partialBody.body.match(/class="bubble /g) || []).length : -1;
ok(partialBubbles === windowSize, 'poll fragment carries only the window', `fragment bubbles=${partialBubbles}`);

// ---- 4. a reader scrolled up is not yanked back ----
const box = await js(`(() => { const r = document.getElementById('chat-main-log').getBoundingClientRect(); return { x: Math.round(r.left + r.width / 2), y: Math.round(r.top + r.height / 2) }; })()`);
for (let i = 0; i < 4; i++) {
  await send('Input.dispatchMouseEvent', { type: 'mouseWheel', x: box.x, y: box.y, deltaX: 0, deltaY: -600 });
  await sleep(120);
}
await sleep(600);
m = await metrics();
ok(m.gap > 160, 'wheel-up actually scrolls the reader away from the bottom', `gap=${m.gap}`);
const shot2 = await snap('02-chat-reader-scrolled-up.png');
await sleep(5000);
m = await metrics();
ok(m.gap > 160, '4s poll does NOT yank a scrolled-up reader to the latest', `gap=${m.gap} stick=${JSON.stringify(m.stick)}`);

// ---- 5. returning to the bottom re-arms the snap ----
for (let i = 0; i < 6; i++) {
  await send('Input.dispatchMouseEvent', { type: 'mouseWheel', x: box.x, y: box.y, deltaX: 0, deltaY: 800 });
  await sleep(120);
}
m = await waitFor('bottom re-arm', async () => { const x = await metrics(); return x.gap <= 4 ? x : null; }, 8000);
ok(m.gap <= 4, 'reader back at the bottom re-arms the snap', `gap=${m.gap}`);
await sleep(5000);
m = await metrics();
ok(m.gap <= 4, 'next poll holds the re-armed reader at the latest', `gap=${m.gap}`);

// ---- 6. lazy history: one page, prepended above the log ----
const beforeClick = await metrics();
const olderReqsBefore = net.fragments.filter((f) => /\/chat\?before=/.test(f.url)).length;
await js(`document.querySelector('.chat-older-btn').click()`);
m = await waitFor('older page', async () => { const x = await metrics(); return x.older > 0 ? x : null; }, 15000);
await sleep(500);
m = await metrics();
ok(m.older === total - windowSize, 'Load older prepends one page of history into #chat-older', `older=${m.older} expected=${total - windowSize}`);
ok(m.bubbles === windowSize, 'live log is untouched by the history load', `bubbles=${m.bubbles}`);
ok(m.olderMore === false, 'control is removed at the oldest message (OOB delete)', `olderMore=${m.olderMore}`);
ok(m.gap <= 4, 'history load does not move the parked log', `gap=${m.gap} before=${beforeClick.gap}`);
ok(m.olderTop < m.logTop, 'loaded history renders above the log', `olderTop=${m.olderTop} logTop=${m.logTop}`);
const olderFrag = net.fragments.filter((f) => /\/chat\?before=/.test(f.url)).pop();
const olderBubbles = olderFrag ? (olderFrag.body.match(/class="bubble /g) || []).length : -1;
ok(olderBubbles === total - windowSize, 'history fragment carries exactly one page', `fragment bubbles=${olderBubbles} url=${olderFrag ? olderFrag.url.replace(BASE, '') : 'none'}`);
const domAll = await js(`document.getElementById('chat-main').parentElement.outerHTML`);
fs.writeFileSync(path.join(OUT, 'dom-after-lazy.html'), domAll);
const shot3 = await snap('03-chat-history-loaded.png');

// ---- 7. sending snaps the log to the latest ----
const marker = 'ui review 70015a74: lazy history + autoscroll';
const sentAt = Date.now();
await js(`(() => {
  const f = document.querySelector('.chatpage form.chat-composer');
  const t = f.querySelector('textarea[name=body]');
  t.value = ${JSON.stringify(marker)};
  t.dispatchEvent(new Event('input', { bubbles: true }));
  f.requestSubmit();
})()`);
await waitFor('sent bubble', async () => {
  const seen = await js(`Array.from(document.querySelectorAll('#chat-main-log .bubble-body')).some((n) => n.textContent.includes(${JSON.stringify(marker)}))`);
  const x = await metrics();
  return seen && x.bubbles === windowSize ? x : null;
}, 20000);
m = await metrics();
ok(m.gap <= 4, 'send snaps the log to the latest message', `gap=${m.gap}`);
ok(m.composer === '', 'send clears the composer', JSON.stringify(m.composer));
const shot4 = await snap('04-chat-after-send.png');

// ---- artifacts ----
results.push({ total, windowSize, sentMarker: marker, sentAt, screenshots: [shot1, shot2, shot3, shot4] });
fs.writeFileSync(path.join(OUT, 'results.json'), JSON.stringify({ base: BASE, results }, null, 1));
console.log('\nARTIFACTS ' + OUT);
console.log('checks ' + (results.filter((r) => r.name).length - fails) + '/' + results.filter((r) => r.name).length + ' passed');
process.exit(fails ? 1 : 0);
