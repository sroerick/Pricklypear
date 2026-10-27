#!/usr/bin/env node
// Card c7be7174 UI review, part 2: the ORDERING claim (keyboard move).
// Seeds TWO panes (the first harness closed one before moving, so the
// transposition had no sibling), injects 1.2s of network latency, presses
// Alt+Shift+ArrowRight, and samples the DOM BEFORE the roundtrip returns.
import { spawn } from 'node:child_process';
import { setTimeout as sleep } from 'node:timers/promises';
import http from 'node:http';
import fs from 'node:fs';
import path from 'node:path';

const GLOCHID = process.env.GLOCHID_BIN || path.resolve('_build/default/glochid/bin/main.exe');
const CHROME = process.env.CHROME || '/usr/local/bin/chrome';
const PORTC = 9200 + (process.pid % 500);
const USER_DATA = '/tmp/chrome-c7be7174m-' + process.pid;
const DIR = '/tmp/glochid-c7be7174m-' + process.pid;
const MOCK = path.join(DIR, 'mock.json');
const SHOTS = path.resolve(process.env.SHOT_OUT || 'screenshots/review-c7be7174');

let fails = 0;
const results = [];
function ok(cond, name, detail = '') {
  console.log((cond ? 'OK   ' : 'FAIL ') + name + (detail ? ' : ' + detail : ''));
  results.push({ name, pass: !!cond, detail });
  if (!cond) fails++;
}
function httpReq(url) {
  return new Promise((resolve) => {
    const req = http.request(url, { method: 'GET' }, (res) => {
      let d = ''; res.on('data', (c) => (d += c));
      res.on('end', () => resolve({ status: res.statusCode, body: d }));
    });
    req.on('error', (e) => resolve({ status: 0, body: String(e) }));
    req.end();
  });
}
function cdpSend(ws, id, method, params = {}) {
  return new Promise((resolve, reject) => {
    const timer = setTimeout(() => { clean(); reject(new Error('CDP timeout ' + method)); }, 20000);
    const onMsg = (ev) => {
      try { const j = JSON.parse(typeof ev.data === 'string' ? ev.data : Buffer.from(ev.data).toString()); if (j.id === id) { clean(); resolve(j.result); } } catch (_) {}
    };
    const onErr = (e) => { clean(); reject((e && e.error) || e); };
    function clean() { clearTimeout(timer); ws.removeEventListener('message', onMsg); ws.removeEventListener('error', onErr); }
    ws.addEventListener('message', onMsg);
    ws.addEventListener('error', onErr);
    ws.send(JSON.stringify({ id, method, params }));
  });
}
let glochid, chrome, stub;
const cleanup = () => {
  try { if (glochid) glochid.kill('SIGTERM'); } catch (_) {}
  try { if (chrome) chrome.kill('SIGKILL'); } catch (_) {}
  try { if (stub) stub.close(); } catch (_) {}
  try { fs.rmSync(USER_DATA, { recursive: true, force: true }); } catch (_) {}
  try { fs.rmSync(DIR, { recursive: true, force: true }); } catch (_) {}
};
process.on('exit', cleanup);
setTimeout(() => { console.error('FAIL watchdog'); cleanup(); process.exit(1); }, 120000).unref();

fs.mkdirSync(SHOTS, { recursive: true });
fs.mkdirSync(path.join(DIR, '.glochid'), { recursive: true });
fs.writeFileSync(path.join(DIR, '.glochid', 'workspace.json'), JSON.stringify({
  axis: 'col', desktop: 0, focus: 'content:0',
  content: [
    { width: 'half', route: '/chat', id: 'p1', desktop: 0, state: 'live' },
    { width: 'half', route: '/notes', id: 'p2', desktop: 0, state: 'live' },
  ],
  cascade: ['/chat', '/notes'],
}));
fs.writeFileSync(MOCK, JSON.stringify([{ text: 'review reply', calls: [] }]) + '\n');

const STUB_PORT = 9300 + (process.pid % 200);
const STUB_URL = 'http://127.0.0.1:' + STUB_PORT;
stub = http.createServer((req, res) => {
  const u = new URL(req.url, 'http://stub');
  const face = u.pathname.replace(/^\//, '');
  if (face === 'notes') { res.writeHead(200, { 'content-type': 'text/html' }); res.end('<div id="stub-notes"><h1>Notes</h1></div>'); return; }
  res.writeHead(404); res.end('stub 404');
});
await new Promise((r) => stub.listen(STUB_PORT, '127.0.0.1', r));

glochid = spawn(GLOCHID, ['--serve', '--port', '0', '--mock-script', MOCK], {
  stdio: ['ignore', 'pipe', 'pipe'], cwd: DIR,
  env: { ...process.env, PP_URL: STUB_URL, PP_TOKEN: 'review-token', PP_USER: 'review', PP_PASS: 'review',
    GLOCHID_HABITAT_URL: '', GLOCHID_HABITAT_TOKEN: '', GLOCHID_HABITAT_USER: '', GLOCHID_HABITAT_PASS: '' },
});
let base = ''; let buf = '';
glochid.stdout.on('data', (c) => {
  buf += c;
  const i = buf.indexOf('glochid serving http://');
  if (i >= 0 && !base) { base = buf.slice(i + 'glochid serving'.length).trim().split(' ')[0]; while (base.endsWith('/')) base = base.slice(0, -1); }
});
for (let i = 0; i < 80 && !base; i++) await sleep(250);
if (!base) { console.error('FAIL glochid never printed its URL'); process.exit(1); }
console.log('OK   glochid at ' + base);

chrome = spawn(CHROME, ['--remote-debugging-port=' + PORTC, '--user-data-dir=' + USER_DATA,
  '--headless=new', '--disable-gpu', '--no-sandbox', '--disable-dev-shm-usage', '--window-size=1400,1000', 'about:blank'], { stdio: 'ignore' });
let ready = false;
for (let i = 0; i < 50; i++) {
  try { const v = await httpReq('http://127.0.0.1:' + PORTC + '/json/version'); if (v.status === 200) { ready = true; break; } } catch (_) {}
  await sleep(150);
}
if (!ready) { console.error('FAIL chromium CDP not ready'); process.exit(1); }
const list = JSON.parse((await httpReq('http://127.0.0.1:' + PORTC + '/json/list')).body);
const page = list.find((t) => t.type === 'page');
const ws = new globalThis.WebSocket(page.webSocketDebuggerUrl);
await new Promise((res, rej) => { const t = setTimeout(() => rej(new Error('ws timeout')), 10000); ws.onopen = () => { clearTimeout(t); res(); }; ws.onerror = (e) => { clearTimeout(t); rej((e && e.error) || e); }; });
let nextId = 1;
const send = (m, p) => cdpSend(ws, nextId++, m, p);
await send('Page.enable'); await send('Runtime.enable'); await send('Network.enable');
await send('Emulation.setDeviceMetricsOverride', { width: 1400, height: 1000, deviceScaleFactor: 1, mobile: false });
async function js(expr) {
  const r = await send('Runtime.evaluate', { expression: expr, returnByValue: true, awaitPromise: true });
  if (r && r.exceptionDetails) throw new Error('page JS: ' + JSON.stringify(r.exceptionDetails).slice(0, 240));
  return r && r.result ? r.result.value : undefined;
}
const J = async (e) => { try { return JSON.parse(await js(e)); } catch (err) { console.log('JSERR ' + String(err).slice(0, 200)); return null; } };
async function shot(name) {
  const r = await send('Page.captureScreenshot', { format: 'png' });
  const file = path.join(SHOTS, name + '.png');
  fs.writeFileSync(file, Buffer.from(r.data, 'base64'));
  ok(fs.statSync(file).size > 5000, 'screenshot ' + name, file);
}

await send('Page.navigate', { url: base + '/filmstrip' });
await sleep(2500);

const decl = await J(`(function(){
  var css=''; var ss=document.querySelectorAll('style');
  for (var i=0;i<ss.length;i++) css += ss[i].textContent;
  var w=document.querySelector('.fs-pane .fs-btn[data-optimistic="width"]');
  var c=document.querySelector('.fs-pane .fs-btn[data-optimistic="close"]');
  var o=document.querySelector('[data-optimistic="open"]');
  var m=document.querySelector('.fs-sentinel[data-optimistic="move"]');
  return JSON.stringify({
    panes: document.querySelectorAll('.fs-pane').length,
    width: !!w, close: !!c, open: !!o, move: !!m,
    widthAttr: w ? w.getAttribute('data-optimistic') : null,
    closeAttr: c ? c.getAttribute('data-optimistic') : null,
    openAttr: o ? o.getAttribute('data-optimistic') : null,
      moveBefore: m ? (m.getAttribute('hx-on::before:request') || '') : '',
      moveAfter: m ? (m.getAttribute('hx-on::after:request') || '') : '',
    cssTrigger: css.indexOf('[data-optimistic].htmx-request') >= 0,
    cssPane: css.indexOf('.fs-pane:has([data-optimistic].htmx-request)') >= 0,
    cssOpt: css.indexOf('.fs-pane.fs-optimistic') >= 0,
    err: /Route handler error|Fatal error|Uncaught/.test(document.body.innerText)
  });
})()`);
ok(decl && decl.panes === 2, 'render: two panes', decl && String(decl.panes));
ok(decl && !decl.err, 'render: no error chrome');
ok(decl && decl.widthAttr === 'width', 'decl: width trigger declared', decl && decl.widthAttr);
ok(decl && decl.closeAttr === 'close', 'decl: close trigger declared', decl && decl.closeAttr);
ok(decl && decl.openAttr === 'open', 'decl: open trigger declared', decl && decl.openAttr);
ok(decl && decl.moveBefore.indexOf('fs-optimistic') >= 0, 'decl: move before-request transposes', decl && decl.moveBefore.slice(0, 90));
ok(decl && decl.moveAfter.indexOf('fs-optimistic') >= 0, 'decl: move after-request clears');
ok(decl && decl.cssTrigger, 'css: trigger fade rule');
ok(decl && decl.cssPane, 'css: pane :has fade rule');
ok(decl && decl.cssOpt, 'css: fs-optimistic rule');

const order0 = await js(`(function(){return [].slice.call(document.querySelectorAll('.fs-pane')).map(function(p){return p.getAttribute('data-pane')}).join(',')})()`);
ok(order0 === 'p1,p2', 'move: starting order p1,p2', order0);

await send('Network.emulateNetworkConditions', { offline: false, latency: 1500, downloadThroughput: -1, uploadThroughput: -1 });
await send('Input.dispatchKeyEvent', { type: 'rawKeyDown', modifiers: 9, key: 'ArrowRight', code: 'ArrowRight', windowsVirtualKeyCode: 39, nativeVirtualKeyCode: 39, autoRepeat: false });
await send('Input.dispatchKeyEvent', { type: 'keyUp', modifiers: 9, key: 'ArrowRight', code: 'ArrowRight', windowsVirtualKeyCode: 39, nativeVirtualKeyCode: 39 });
await sleep(250);
const mid = await J(`(function(){
  var panes=[].slice.call(document.querySelectorAll('.fs-pane'));
  var opt=document.querySelector('.fs-pane.fs-optimistic');
  return JSON.stringify({order: panes.map(function(p){return p.getAttribute('data-pane')}).join(','),
    optCount: document.querySelectorAll('.fs-pane.fs-optimistic').length,
    optOp: opt ? getComputedStyle(opt).opacity : null,
    req: document.querySelectorAll('[data-optimistic].htmx-request').length});
})()`);
ok(mid && mid.order === 'p2,p1', 'move: order transposes before the roundtrip returns', (order0 || '') + ' -> ' + (mid && mid.order));
ok(mid && mid.optCount === 1, 'move: transposed pane carries fs-optimistic', mid && String(mid.optCount));
ok(mid && parseFloat(mid.optOp) < 0.9, 'move: transposed pane is semi-faded while pending', mid && 'opacity ' + mid.optOp);
await shot('07-move-pending-fade');
await sleep(3500);
const done = await J(`(function(){
  return JSON.stringify({order:[].slice.call(document.querySelectorAll('.fs-pane')).map(function(p){return p.getAttribute('data-pane')}).join(','),
    opt: document.querySelectorAll('.fs-optimistic').length,
    req: document.querySelectorAll('[data-optimistic].htmx-request').length,
    op: getComputedStyle(document.querySelector('.fs-pane')).opacity});
})()`);
ok(done && done.order === 'p2,p1', 'move: server truth keeps the transposed order', done && done.order);
ok(done && done.opt === 0 && done.req === 0, 'move: unfaded after settle');
ok(done && done.op === '1', 'move: settled pane fully opaque', done && done.op);
await shot('08-move-settled');

console.log('\n' + (fails === 0 ? 'PASS' : 'FAIL') + ' ' + (results.length - fails) + '/' + results.length);
fs.writeFileSync(path.join(SHOTS, 'results-move.json'), JSON.stringify(results, null, 2));
process.exit(fails === 0 ? 0 : 1);
