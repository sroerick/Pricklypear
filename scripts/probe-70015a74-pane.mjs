#!/usr/bin/env node
// Card 70015a74 probe: does the chat PANE log actually autoscroll? Also records
// which htmx swap/request event names fire under the bundled htmx.
import { spawn } from 'node:child_process';
import { setTimeout as sleep } from 'node:timers/promises';
import http from 'node:http';
import fs from 'node:fs';

const BASE = (process.env.PP_URL || 'http://127.0.0.1:18080').replace(/\/$/, '');
const AUTH = 'Basic ' + Buffer.from('admin:admin').toString('base64');
const OUT = '/home/al/pricklypear/screenshots/review-70015a74';
const PORT = 9900 + (process.pid % 90);
const UD = '/tmp/chrome-probe-70015a74-' + process.pid;
function get(url) { return new Promise((r) => { const q = http.request(url, (s) => { let d = ''; s.on('data', (c) => (d += c)); s.on('end', () => r({ status: s.statusCode, body: d })); }); q.on('error', (e) => r({ status: 0, body: String(e) })); q.end(); }); }
const wsData = (ev) => { const d = ev?.data ?? ev; return typeof d === 'string' ? d : Buffer.from(d).toString(); };
function cdpSend(ws, id, method, params = {}) { return new Promise((resolve, reject) => { const t = setTimeout(() => { c(); reject(new Error('timeout ' + method)); }, 20000); const om = (ev) => { try { const j = JSON.parse(wsData(ev)); if (j.id === id) { c(); j.error ? reject(new Error(JSON.stringify(j.error))) : resolve(j.result); } } catch (_) {} }; const oe = (e) => { c(); reject(e?.error || e); }; function c() { clearTimeout(t); ws.removeEventListener('message', om); ws.removeEventListener('error', oe); } ws.addEventListener('message', om); ws.addEventListener('error', oe); ws.send(JSON.stringify({ id, method, params })); }); }
const chrome = spawn('/usr/local/bin/chrome', ['--remote-debugging-port=' + PORT, '--user-data-dir=' + UD, '--headless=new', '--disable-gpu', '--no-sandbox', '--disable-dev-shm-usage', '--window-size=1200,900', 'about:blank'], { stdio: ['ignore', 'ignore', 'ignore'] });
process.on('exit', () => { try { chrome.kill('SIGKILL'); } catch (_) {} try { fs.rmSync(UD, { recursive: true, force: true }); } catch (_) {} });
setTimeout(() => process.exit(2), 120000).unref();
let ready = false;
for (let i = 0; i < 120; i++) { if ((await get('http://127.0.0.1:' + PORT + '/json/version')).status === 200) { ready = true; break; } await sleep(250); }
if (!ready) { console.error('FAIL chrome'); process.exit(1); }
const list = JSON.parse((await get('http://127.0.0.1:' + PORT + '/json/list')).body);
const ws = new globalThis.WebSocket(list.find((t) => t.type === 'page').webSocketDebuggerUrl);
await new Promise((res, rej) => { ws.onopen = res; ws.onerror = rej; });
let n = 1; const send = (m, p) => cdpSend(ws, n++, m, p);
await send('Page.enable'); await send('Runtime.enable'); await send('Network.enable');
await send('Network.setExtraHTTPHeaders', { headers: { Authorization: AUTH } });
await send('Page.navigate', { url: BASE + '/filmstrip' });
const js = async (e) => { const r = await send('Runtime.evaluate', { expression: e, returnByValue: true, awaitPromise: true }); if (r.exceptionDetails) throw new Error(JSON.stringify(r.exceptionDetails).slice(0, 200)); return r.result.value; };
for (let i = 0; i < 60; i++) { if (await js('typeof htmx === "object" || typeof htmx === "function"')) break; await sleep(250); }
console.log('htmx version =', await js('htmx.version || "?"'));
await js(`window.__ev=[];['htmx:before:swap','htmx:after:swap','htmx:beforeSwap','htmx:afterSwap','htmx:after:request','htmx:afterRequest'].forEach(function(k){document.body.addEventListener(k,function(){window.__ev.push(k)})}); true`);
await js(`htmx.ajax('POST','/filmstrip/open',{values:{route:'/chat'},target:'#fs-sink',swap:'innerHTML'})`);
for (let i = 0; i < 80; i++) { if (await js('!!document.getElementById("chat-main-log")')) break; await sleep(250); }
await sleep(7000);
const m = await js(`(() => {
  const l = document.getElementById('chat-main-log');
  const older = document.getElementById('chat-older');
  const first = document.querySelector('#chat-main-log .bubble-meta');
  return {
    bubbleCount: l.querySelectorAll('.bubble').length,
    scrollTop: Math.round(l.scrollTop), scrollHeight: l.scrollHeight, clientHeight: l.clientHeight,
    gap: Math.round(l.scrollHeight - l.scrollTop - l.clientHeight),
    paneHeight: Math.round(l.closest('.fs-pane').getBoundingClientRect().height),
    firstVisible: first ? first.textContent.trim() : null,
    events: window.__ev,
  };
})()`);
console.log(JSON.stringify(m, null, 1));
const s = await send('Page.captureScreenshot', { format: 'png' });
fs.writeFileSync(OUT + '/05-pane-chat-no-snap.png', Buffer.from(s.data, 'base64'));
console.log('shot ' + OUT + '/05-pane-chat-no-snap.png');
process.exit(0);
