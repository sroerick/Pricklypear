#!/usr/bin/env node
// battle_glochid_cdp.mjs - HARNESS battle-test sweep (Board card bcb5a70a).
//
// Headless CDP pass over the native glochid/filmstrip harness surfaces:
//   /  /filmstrip  /chat  kanban (/todos)  /notes  /calendar
// asserting: shell render, htmx swap contracts, error/empty states, a11y.
//
// Round-2 restructure: round 1 found the shell at / and /filmstrip crashing
// on the current build (defect D1, filmstrip/sentinel extra-attrs paren,
// card c7be7174), so DOM/a11y checks run on the /chat face and the shell
// crash is asserted + recorded explicitly.
//
// Usage: CHROME=/usr/local/bin/chrome node scripts/battle_glochid_cdp.mjs
// Exit 0 = every check passed; exit 1 = at least one DEFECT line below.

import { spawn } from 'node:child_process';
import { setTimeout as sleep } from 'node:timers/promises';
import http from 'node:http';
import fs from 'node:fs';
import path from 'node:path';

const NL = String.fromCharCode(10);
const Q = String.fromCharCode(34);
const GLOCHID = process.env.GLOCHID_BIN || path.resolve('_build/default/glochid/bin/main.exe');
const CHROME = process.env.CHROME || '/usr/local/bin/chrome';
const PORTC = 9100 + (process.pid % 600);
const USER_DATA = '/tmp/chrome-battle-' + process.pid;
const DIR = '/tmp/glochid-battle-' + process.pid;
const MOCK = path.join(DIR, 'mock.json');

let fails = 0;
const results = [];
function ok(cond, name, detail = '') {
  console.log((cond ? 'OK   ' : 'FAIL ') + name + (detail ? ' : ' + detail : ''));
  results.push({ name: name, pass: !!cond, detail: detail });
  if (!cond) fails++;
}

function httpReq(url, headers = {}, method = 'GET', body = null) {
  return new Promise((resolve, reject) => {
    const req = http.request(url, { method: method, headers: headers }, (res) => {
      let d = ''; res.on('data', (c) => (d += c));
      res.on('end', () => resolve({ status: res.statusCode, body: d }));
    });
    req.on('error', (e) => resolve({ status: 0, body: 'FETCH-ERR ' + String(e).slice(0, 80) }));
    const to = setTimeout(() => { req.destroy(new Error('http timeout ' + url)); }, 20000);
    req.on('close', () => clearTimeout(to));
    if (body) req.write(body);
    req.end();
  });
}
const httpGet = (url) => httpReq(url, {});
const bytes = (ev) => (Buffer.isBuffer(ev && ev.data) ? ev.data.toString() : String(ev ? ev.data : ev));
function cdpSend(ws, id, method, params = {}) {
  return new Promise((resolve, reject) => {
    const timer = setTimeout(() => { clean(); reject(new Error('CDP timeout ' + method)); }, 15000);
    const onMsg = (ev) => {
      try { const j = JSON.parse(bytes(ev)); if (j.id === id) { clean(); resolve(j.result); } } catch (_) {}
    };
    const onErr = (e) => { clean(); reject((e && e.error) || e); };
    function clean() { clearTimeout(timer); ws.removeEventListener('message', onMsg); ws.removeEventListener('error', onErr); }
    ws.addEventListener('message', onMsg);
    ws.addEventListener('error', onErr);
    ws.send(JSON.stringify({ id: id, method: method, params: params }));
  });
}

let glochid, chrome;
const cleanup = () => {
  try { if (glochid) glochid.kill('SIGTERM'); } catch (_) {}
  try { if (chrome) chrome.kill('SIGKILL'); } catch (_) {}
  try { fs.rmSync(USER_DATA, { recursive: true, force: true }); } catch (_) {}
  try { fs.rmSync(DIR, { recursive: true, force: true }); } catch (_) {}
};
process.on('exit', cleanup);
process.on('SIGTERM', () => { cleanup(); process.exit(1); });
setTimeout(() => { console.error('FAIL global watchdog timeout'); cleanup(); process.exit(1); }, 150000).unref();

if (!fs.existsSync(GLOCHID)) {
  console.error('FAIL ' + GLOCHID + ' missing (dune build glochid/bin/main.exe)');
  process.exit(1);
}

// scratch agent: one live chat pane (the shell crash makes richer seeds moot)
fs.mkdirSync(path.join(DIR, '.glochid'), { recursive: true });
fs.writeFileSync(path.join(DIR, '.glochid', 'workspace.json'), JSON.stringify({
  axis: 'col', desktop: 0, focus: 'content:0',
  content: [
    { width: 'third', route: '/chat', id: 'p1', desktop: 0, state: 'live' },
  ],
  cascade: ['/chat'],
}));
fs.writeFileSync(MOCK, JSON.stringify([{ text: 'battle reply', calls: [] }]) + NL);

glochid = spawn(GLOCHID, ['--serve', '--port', '0', '--mock-script', MOCK], { stdio: ['ignore', 'pipe', 'pipe'], cwd: DIR });
let base = '';
let buf = '';
glochid.stdout.on('data', (c) => {
  buf += c;
  const i = buf.indexOf('glochid serving http://');
  if (i >= 0 && !base) {
    base = buf.slice(i + 'glochid serving'.length).trim().split(' ')[0];
    while (base.endsWith('/')) base = base.slice(0, -1);
  }
});
for (let i = 0; i < 80 && !base; i++) await sleep(250);
if (!base) { console.error('FAIL glochid never printed its URL'); process.exit(1); }
console.log('OK   glochid at ' + base);

// boot chromium CDP
chrome = spawn(CHROME, [
  '--remote-debugging-port=' + PORTC, '--user-data-dir=' + USER_DATA,
  '--headless=new', '--disable-gpu', '--no-sandbox', '--disable-dev-shm-usage',
  '--window-size=1600,1000', 'about:blank',
], { stdio: 'ignore' });
  console.log('OK   chrome spawned (CDP port ' + PORTC + ')');
let ready = false;
for (let i = 0; i < 50; i++) {
  try { const v = await httpGet('http://127.0.0.1:' + PORTC + '/json/version'); if (v.status === 200) { ready = true; break; } } catch (_) {}
  await sleep(150);
}
if (!ready) { console.error('FAIL boot: chromium CDP not ready'); process.exit(1); }
const list = JSON.parse((await httpGet('http://127.0.0.1:' + PORTC + '/json/list')).body);
const page = list.find((t) => t.type === 'page');
const ws = new globalThis.WebSocket(page.webSocketDebuggerUrl);
await new Promise((res, rej) => {
  const t = setTimeout(() => rej(new Error('ws open timeout')), 10000);
  ws.onopen = () => { clearTimeout(t); res(); };
  ws.onerror = (e) => { clearTimeout(t); rej((e && e.error) || e); };
});
console.log('OK   CDP websocket open');
let nextId = 1;
const send = (m, p) => cdpSend(ws, nextId++, m, p);
await send('Page.enable');
await send('Runtime.enable');
async function js(expr) {
  try {
    const r = await send('Runtime.evaluate', { expression: expr, returnByValue: true, awaitPromise: true });
    if (r && r.exceptionDetails) throw new Error('page JS: ' + JSON.stringify(r.exceptionDetails).slice(0, 300));
    return r && r.result ? r.result.value : undefined;
  } catch (e) {
    console.log('JSERR ' + String(e).slice(0, 160));
    return undefined;
  }
}
// ============ PHASE A: shell render at / and /filmstrip (D1) ============
const root = await httpGet(base + '/');
ok(root.status === 200, 'GET / -> 200');
ok(root.body.toLowerCase().indexOf('<!doctype html>') === 0,
  'D1 shell crash: GET / serves a full shell document', root.body.slice(0, 80));
const film = await httpGet(base + '/filmstrip');
ok(film.status === 200 && film.body.toLowerCase().indexOf('<!doctype html>') === 0,
  'D1 shell crash: GET /filmstrip serves a full shell document', film.body.slice(0, 80));

// ============ PHASE B: /chat surface (render + swap + a11y) ============
const chatFull = await httpGet(base + '/chat');
ok(chatFull.status === 200 && chatFull.body.toLowerCase().indexOf('<!doctype html>') === 0 && chatFull.body.includes('<title>'),
  'GET /chat plain -> full document with title');
const chatFrag = await httpReq(base + '/chat', { 'HX-Request': 'true' });
ok(chatFrag.status === 200 && chatFrag.body.indexOf('<!doctype') < 0 && chatFrag.body.indexOf('<html') < 0
  && chatFrag.body.includes('id=' + Q + 'f' + Q) && chatFrag.body.includes('id=' + Q + 'log' + Q),
  'GET /chat HX-Request -> pane fragment (no document chrome)', chatFrag.body.slice(0, 60));

await send('Page.navigate', { url: base + '/chat' });
await sleep(1200);
ok(await js(`!!document.getElementById('f') && !!document.getElementById('msg') && !!document.getElementById('log')`),
  'chat DOM: form #f + input #msg + #log');

await js(`document.getElementById('msg').value = 'battle test'; true;`);
await js(`document.getElementById('f').requestSubmit(); true;`);
let chatOk = false;
for (let i = 0; i < 20 && !chatOk; i++) {
  await sleep(300);
  chatOk = await js(`(() => { const log = document.getElementById('log'); if (!log) return false; const t = log.textContent; return t.indexOf('battle test') >= 0 && t.indexOf('battle reply') >= 0; })()`);
}
ok(chatOk, 'chat submit -> user bubble + mock reply land in #log');
ok(await js(`document.getElementById('msg').value === ''`), 'chat input clears at submit (hx-on::before:request reset)');

// a11y on the glochid-owned chat face
ok(await js(`!!(document.documentElement.getAttribute('lang') || '').trim()`),
  'D3 a11y: html[lang] present on /chat (WCAG 3.1.1)');
ok(await js(`(document.title || '').trim().length > 0`), 'document.title non-empty');
const btnBad = await js(`(() => Array.from(document.querySelectorAll('button')).filter(function (b) { return !(b.getAttribute('aria-label') || (b.textContent || '').trim() || b.getAttribute('title')); }).map(function (b) { return b.outerHTML.slice(0, 80); }))()`);
ok(Array.isArray(btnBad) && btnBad.length === 0, 'a11y: every button has an accessible name', JSON.stringify(btnBad || []));
const inpBad = await js(`(() => Array.from(document.querySelectorAll('input:not([type=hidden]), select, textarea')).filter(function (e) { const id = e.id; const lab = id ? document.querySelector('label[for=' + id + ']') : null; return !(lab || e.getAttribute('aria-label') || e.getAttribute('aria-labelledby') || e.getAttribute('title')); }).map(function (e) { return e.tagName + '#' + (e.id || '?') + ' name=' + (e.name || '?'); }))()`);
ok(Array.isArray(inpBad) && inpBad.length === 0, 'D3 a11y: every form control has a programmatic label', JSON.stringify(inpBad || []));
const imgBad = await js(`Array.from(document.images).filter(function (i) { return !i.hasAttribute('alt'); }).length`);
ok(imgBad === 0, 'a11y: every <img> carries alt', 'missing: ' + imgBad);
ok(await js(`document.querySelectorAll('h1').length === 1`), 'a11y: exactly one h1 on /chat');

// ============ PHASE C: mutation swap contract (POST /filmstrip/open) ============
const openRes = await httpReq(base + '/filmstrip/open',
  { 'HX-Request': 'true', 'Content-Type': 'application/x-www-form-urlencoded' },
  'POST', 'route=/sessions');
ok(openRes.status === 200 && openRes.body.indexOf('<!doctype') < 0 && openRes.body.indexOf('<html') < 0,
  'POST /filmstrip/open -> fragment, not a full document', 'status ' + openRes.status);
ok(openRes.body.includes('hx-swap-oob'), 'POST /filmstrip/open response carries OOB swaps');
ok(openRes.body.includes('fs-tree-windows') || openRes.body.includes('fs-rail'), 'POST /filmstrip/open refreshes the windows rail OOB');

// ============ PHASE D: sessions empty state ============
const sess = await httpGet(base + '/sessions');
ok(sess.status === 200 && sess.body.includes('<table'), 'GET /sessions -> 200 with the table shell');
const sessParts = sess.body.split('<table');
const sessTable = sessParts.length > 1 ? sessParts[1].split('</table>')[0] : '';
ok(sessTable.indexOf('<td') < 0, '/sessions fresh boot renders zero rows (empty state, no crash)');
ok(sess.body.includes('transcripts:'), '/sessions shows the transcripts hint line');

// ============ PHASE E: kanban / notes / calendar ============
const FWDS = ['todos', 'notes', 'calendar'];
for (const app of FWDS) {
  const r = await httpGet(base + '/' + app);
  ok(r.status === 200, app + ' face serves 200 through the harness forward',
    'status ' + r.status + ' head: ' + r.body.slice(0, 80));
}

// ============ PHASE F: unknown route ============
const unknown = await httpGet(base + '/definitely-not-a-route');
const crashed = unknown.body.indexOf('Fatal error') >= 0 || unknown.body.indexOf('Uncaught exception') >= 0 || unknown.body.indexOf('Segfault') >= 0;
ok(unknown.status === 404 && !crashed, 'unknown route -> clean 404, no runtime crash', 'status ' + unknown.status);

// ---- summary ----
console.log('');
console.log('==== BATTLE SWEEP SUMMARY ====');
console.log((results.length - fails) + '/' + results.length + ' checks passed, ' + fails + ' failed');
for (const f of results) if (!f.pass) console.log('DEFECT ' + f.name + ' :: ' + f.detail);
process.exit(fails === 0 ? 0 : 1);
