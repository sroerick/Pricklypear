#!/usr/bin/env node
// battle_glochid_cdp.mjs - HARNESS battle-test sweep (Board card bcb5a70a).
//
// Headless CDP pass over the native glochid/filmstrip harness surfaces:
//   /  /filmstrip  /chat  kanban (/todos)  /notes  /calendar
// asserting: shell render, htmx swap contracts, error/empty states, a11y.
//
// Round 3: the round-1 shell crash (defect D1) is fixed, so the sweep now
// judges RENDERED pages (not bare 200s): every face is navigated in chromium
// and the document is inspected for error chrome, headings, a11y names, and
// the filmstrip component ids declared in libs/filmstrip/lib.pp. The stale
// round-2 assertion "open refreshes the windows rail OOB" was wrong: open
// OOBs the pane sections into #fs-content (beforeend) and never touches the
// rail tree. The swap contract asserted here is the one the shell ships.
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
const SHOTS = path.resolve(process.env.BATTLE_SHOTS || 'screenshots/battle-bcb5a70a');
const FILMSTRIP_SRC = path.resolve('libs/filmstrip/lib.pp');
const SURFACE_SRC = path.resolve('glochid/lib/surface.ml');
const ERR_RE = /Route handler error|no active request|Fatal error|Uncaught exception|Internal Server Error|habitat bridge not configured|habitat-unreachable|Segmentation fault/;

let fails = 0;
const results = [];
function ok(cond, name, detail = '') {
  console.log((cond ? 'OK   ' : 'FAIL ') + name + (detail ? ' : ' + detail : ''));
  results.push({ name: name, pass: !!cond, detail: detail });
  if (!cond) fails++;
}

function httpReq(url, headers = {}, method = 'GET', body = null) {
  return new Promise((resolve) => {
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
setTimeout(() => { console.error('FAIL global watchdog timeout'); cleanup(); process.exit(1); }, 180000).unref();

if (!fs.existsSync(GLOCHID)) {
  console.error('FAIL ' + GLOCHID + ' missing (dune build glochid/bin/main.exe)');
  process.exit(1);
}
fs.mkdirSync(SHOTS, { recursive: true });

// scratch agent: chat + notes + calendar panes (kanban and the rest are opened
// by the sweep itself through the launcher, exercising the swap contract).
fs.mkdirSync(path.join(DIR, '.glochid'), { recursive: true });
fs.writeFileSync(path.join(DIR, '.glochid', 'workspace.json'), JSON.stringify({
  axis: 'col', desktop: 0, focus: 'content:0',
  content: [
    { width: 'half', route: '/chat', id: 'p1', desktop: 0, state: 'live' },
    { width: 'half', route: '/notes', id: 'p2', desktop: 0, state: 'live' }
  ],
  cascade: ['/chat', '/notes']
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
  '--window-size=1400,1000', 'about:blank',
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
await send('Emulation.setDeviceMetricsOverride', { width: 1400, height: 1000, deviceScaleFactor: 1, mobile: false });

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

async function shot(name) {
  try {
    const r = await send('Page.captureScreenshot', { format: 'png' });
    const file = path.join(SHOTS, name + '.png');
    fs.writeFileSync(file, Buffer.from(r.data, 'base64'));
    const size = fs.statSync(file).size;
    ok(size > 5000, 'screenshot ' + name + ' written', file + ' (' + size + ' bytes)');
    return file;
  } catch (e) {
    ok(false, 'screenshot ' + name + ' written', String(e).slice(0, 120));
    return null;
  }
}

async function goto(url, waitMs = 1500) {
  await send('Page.navigate', { url: url });
  await sleep(waitMs);
}

const SNAP = '(function(){' +
  'var b=document.body?document.body.innerText:"";' +
  'var h1=Array.prototype.slice.call(document.querySelectorAll("h1")).map(function(h){return (h.innerText||"").trim()});' +
  'return JSON.stringify({url:location.pathname+location.search,lang:document.documentElement.getAttribute("lang")||"",' +
  'title:document.title||"",h1:h1,textlen:b.trim().length,err:' + ERR_RE.toString() + '.test(b)});' +
  '})()';
const A11Y = '(function(){' +
  'var btns=Array.prototype.slice.call(document.querySelectorAll("button")).filter(function(b){return !(b.getAttribute("aria-label")||(b.textContent||"").trim()||b.getAttribute("title"))});' +
  'var links=Array.prototype.slice.call(document.querySelectorAll("a[href]")).filter(function(a){return !(a.getAttribute("aria-label")||(a.textContent||"").trim()||a.getAttribute("title"))});' +
  'var inputs=Array.prototype.slice.call(document.querySelectorAll("input:not([type=hidden]),select,textarea")).filter(function(e){var id=e.id;var lab=id?document.querySelector("label[for=\""+id+"\"]"):null;return !(lab||e.getAttribute("aria-label")||e.getAttribute("aria-labelledby")||e.getAttribute("title"))});' +
  'var imgs=Array.prototype.slice.call(document.images).filter(function(i){return !i.hasAttribute("alt")});' +
  'return JSON.stringify({lang:document.documentElement.getAttribute("lang")||"",title:document.title||"",badButtons:btns.map(function(b){return b.outerHTML.slice(0,60)}),badLinks:links.length,badInputs:inputs.map(function(e){return e.tagName+"#"+(e.id||"?")}),badImgs:imgs.length});' +
  '})()';

async function snap() { const s = await js(SNAP); try { return JSON.parse(s); } catch (_) { return null; } }
async function a11y() { const s = await js(A11Y); try { return JSON.parse(s); } catch (_) { return null; } }

async function assertSurface(name, url, opts) {
  const s = await snap();
  const a = await a11y();
  ok(!!s, name + ': document snapshot answered');
  if (!s) return;
  ok(s.url.split('?')[0] === opts.path, name + ': browser is on ' + opts.path, s.url);
  ok(!s.err, name + ': no error page in the rendered document', s.err ? 'rendered error chrome found' : '');
  ok(s.title.trim().length > 0, name + ': document title non-empty', s.title);
  ok(s.textlen > opts.minText, name + ': body renders text', 'chars ' + s.textlen);
  ok(s.h1.length >= 1, name + ': page carries a heading', JSON.stringify(s.h1.slice(0, 2)));
  ok(s.lang.trim().length > 0, name + ': html[lang] present (WCAG 3.1.1)', s.lang);
  if (a) {
    ok(a.badButtons.length === 0, name + ': every button has an accessible name', JSON.stringify(a.badButtons));
    ok(a.badLinks === 0, name + ': every link has an accessible name', 'bad: ' + a.badLinks);
    ok(a.badInputs.length === 0, name + ': every form control is labelled', JSON.stringify(a.badInputs));
    ok(a.badImgs === 0, name + ': every img carries alt', 'bad: ' + a.badImgs);
  }
  await shot(opts.shot);
}

// ============ PHASE A: shell render at / and /filmstrip ============
for (const route of ['/', '/filmstrip']) {
  const r = await httpGet(base + route);
  ok(r.status === 200 && r.body.toLowerCase().indexOf('<!doctype html>') === 0,
    'GET ' + route + ' -> 200 full shell document', 'status ' + r.status);
  await goto(base + route);
  const layout = await js('(function(){var q=function(s){return !!document.querySelector(s)};' +
    'return JSON.stringify({shell:q("#fs-shell"),rail:q("#fs-rail"),content:q("#fs-content"),' +
    'panes:document.querySelectorAll(".fs-pane").length,tabs:document.querySelectorAll(".fs-railtab").length,' +
    'trees:document.querySelectorAll("[id^=fs-tree-][id$=-root]").length,' +
    'start:document.querySelectorAll(".fs-start-entry").length,' +
    'heads:document.querySelectorAll(".fs-pane .fs-head").length,' +
    'overflowX:document.documentElement.scrollWidth>document.documentElement.clientWidth+8});})()');
  let L = null; try { L = JSON.parse(layout); } catch (_) {}
  ok(!!L, route + ': shell layout probe answered');
  if (L) {
    ok(L.shell && L.rail && L.content, route + ': shell frame composed of #fs-shell/#fs-rail/#fs-content');
    ok(L.panes >= 2, route + ': seeded panes render', 'panes ' + L.panes);
    ok(L.heads === L.panes, route + ': every pane carries its component head', L.heads + ' heads / ' + L.panes + ' panes');
    ok(L.tabs >= 3, route + ': rail tabs render', 'tabs ' + L.tabs);
    ok(L.trees >= 3, route + ': rail trees render', 'trees ' + L.trees);
    ok(L.start >= 5, route + ': start-menu entries render', 'entries ' + L.start);
    ok(!L.overflowX, route + ': shell does not overflow the viewport horizontally');
  }
  await assertSurface('shell ' + route, base + route, { path: route === '/' ? '/' : '/filmstrip', minText: 40, shot: route === '/' ? 'shell-root' : 'shell-filmstrip' });
}

// ============ PHASE B: /chat surface (render + swap + a11y) ============
const chatFull = await httpGet(base + '/chat');
ok(chatFull.status === 200 && chatFull.body.toLowerCase().indexOf('<!doctype html>') === 0 && chatFull.body.includes('<title>'),
  'GET /chat plain -> full document with title');
const chatFrag = await httpReq(base + '/chat', { 'HX-Request': 'true' });
ok(chatFrag.status === 200 && chatFrag.body.indexOf('<!doctype') < 0 && chatFrag.body.indexOf('<html') < 0
  && chatFrag.body.includes('id=' + Q + 'f' + Q) && chatFrag.body.includes('id=' + Q + 'log' + Q),
  'GET /chat HX-Request -> pane fragment (no document chrome)', chatFrag.body.slice(0, 60));

await goto(base + '/chat');
ok(await js('!!document.getElementById("f") && !!document.getElementById("msg") && !!document.getElementById("log")'),
  'chat DOM: form #f + input #msg + #log');
await js('document.getElementById("msg").value = "battle test"; true;');
await js('document.getElementById("f").requestSubmit(); true;');
let chatOk = false;
for (let i = 0; i < 20 && !chatOk; i++) {
  await sleep(300);
  chatOk = await js('(function(){var l=document.getElementById("log");if(!l)return false;var t=l.textContent;return t.indexOf("battle test")>=0&&t.indexOf("battle reply")>=0;})()');
}
ok(chatOk, 'chat submit -> user bubble + mock reply land in #log');
ok(await js('document.getElementById("msg").value === ""'), 'chat input clears at submit (hx-on::before:request reset)');
await assertSurface('chat', base + '/chat', { path: '/chat', minText: 10, shot: 'face-chat' });

// ============ PHASE C: mutation swap contract (POST /filmstrip/open) ============
const openRes = await httpReq(base + '/filmstrip/open',
  { 'HX-Request': 'true', 'Content-Type': 'application/x-www-form-urlencoded' },
  'POST', 'route=/sessions');
ok(openRes.status === 200 && openRes.body.indexOf('<!doctype') < 0 && openRes.body.indexOf('<html') < 0,
  'POST /filmstrip/open -> fragment, not a full document', 'status ' + openRes.status);
ok(openRes.body.includes('hx-swap-oob=' + Q + 'beforeend:#fs-content' + Q),
  'POST /filmstrip/open appends the pane into #fs-content (OOB beforeend)');
ok(openRes.body.includes('id=' + Q + 'fs-pane-') || openRes.body.includes('id=' + Q + 'fs-body-'),
  'POST /filmstrip/open answers a pane section (fs-pane/fs-body)');
ok(!openRes.body.includes('id=' + Q + 'fs-tree-windows-root' + Q),
  'POST /filmstrip/open leaves the rail tree alone (no stale twin OOB)');

await goto(base + '/');
ok(await js('!!document.getElementById("fs-content")'), 'swap target #fs-content exists in the shell DOM');
const panesBefore = await js('document.querySelectorAll(".fs-pane").length');
const clicked = await js('(function(){var es=document.querySelectorAll(".fs-start-entry");var pick=null;for(var i=0;i<es.length;i++){var v=es[i].getAttribute("hx-vals")||"";if(v.indexOf("/bindings")>=0){pick=es[i];break;}}if(!pick)pick=es[es.length-1];if(!pick)return "no-entry";pick.click();return pick.getAttribute("hx-vals")||"clicked";})()');
ok(clicked !== 'no-entry', 'launcher exposes a clickable start entry', String(clicked));
let panesAfter = panesBefore;
for (let i = 0; i < 20 && panesAfter <= panesBefore; i++) {
  await sleep(250);
  panesAfter = await js('document.querySelectorAll(".fs-pane").length');
}
ok(panesAfter > panesBefore, 'clicking a start entry splices a new pane into #fs-content',
  'panes ' + panesBefore + ' -> ' + panesAfter);
ok(await js('location.pathname === "/"'), 'launcher swap does not navigate the document');
const newPaneText = await js('(function(){var p=document.querySelectorAll(".fs-pane");var last=p[p.length-1];return last?((last.querySelector(".fs-body")||{}).textContent||"").trim().length:0;})()');
ok(newPaneText > 0, 'the spliced pane carries rendered body content', 'chars ' + newPaneText);
await shot('shell-opened');

// ============ PHASE D: sessions empty state ============
const sess = await httpGet(base + '/sessions');
ok(sess.status === 200 && sess.body.includes('<table'), 'GET /sessions -> 200 with the table shell');
const sessParts = sess.body.split('<table');
const sessTable = sessParts.length > 1 ? sessParts[1].split('</table>')[0] : '';
ok(sessTable.indexOf('<td') < 0, '/sessions fresh boot renders zero rows (empty state, no crash)');
ok(sess.body.includes('transcripts:'), '/sessions shows the transcripts hint line');

// ============ PHASE E: kanban / notes / calendar (RENDERED, not bare 200) ============
for (const face of [{ app: 'todos', minText: 200 }, { app: 'notes', minText: 100 }, { app: 'calendar', minText: 100 }]) {
  const r = await httpGet(base + '/' + face.app);
  ok(r.status === 200, face.app + ': face serves 200 through the harness forward', 'status ' + r.status);
  await goto(base + '/' + face.app, 2500);
  await assertSurface(face.app, base + '/' + face.app, { path: '/' + face.app, minText: face.minText, shot: 'face-' + face.app });
}

// ============ PHASE F: unknown route ============
const unknown = await httpGet(base + '/definitely-not-a-route');
const crashed = ERR_RE.test(unknown.body);
ok(unknown.status === 404 && !crashed, 'unknown route -> clean 404, no runtime crash', 'status ' + unknown.status);

// ============ PHASE G: component registration (lookbook inventory) ============
// The shell must be composed of the DECLARED filmstrip elements. Every fs-*
// id/class the rendered shell ships must appear as a literal in the filmstrip
// lookbook source (or glochid's declared surface override for host-only ids).
try {
  const shellHtml = await httpGet(base + '/');
  fs.writeFileSync(path.join(SHOTS, 'shell-root.html'), shellHtml.body);
  const decl = fs.readFileSync(FILMSTRIP_SRC, 'utf8') + fs.readFileSync(SURFACE_SRC, 'utf8');
  const ids = [...new Set((shellHtml.body.match(/id=["'](fs-[a-z0-9-]+)/g) || [])
    .map((m) => m.replace(/^id=["']/, '')))];
    const undeclared = ids.filter((id) => !decl.includes(id) && !/^fs-(pane|body|tree)-/.test(id));
  ok(ids.length > 8, 'registration: shell ships the filmstrip component ids', ids.length + ' fs-* ids');
  ok(undeclared.length === 0, 'registration: every fs-* id is declared in the filmstrip lookbook', JSON.stringify(undeclared));
} catch (e) {
  ok(false, 'registration: lookbook inventory readable', String(e).slice(0, 120));
}

// ---- summary ----
console.log('');
console.log('==== BATTLE SWEEP SUMMARY ====');
console.log((results.length - fails) + '/' + results.length + ' checks passed, ' + fails + ' failed');
console.log('screenshots: ' + SHOTS);
for (const f of results) if (!f.pass) console.log('DEFECT ' + f.name + ' :: ' + f.detail);
process.exit(fails === 0 ? 0 : 1);
