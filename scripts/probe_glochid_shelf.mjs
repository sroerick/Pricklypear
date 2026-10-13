#!/usr/bin/env node
// probe_glochid_shelf.mjs — CDP probe: do the SHELF row verbs (restore /
// destroy) actually work in a real browser? Boots its own glochid --serve
// (mock replies), opens a pane, shelves it (close), exposes the shelf
// subtree in the windows rail tree, then CLICKS the restore verb and the
// destroy verb like an operator would, checking the DOM after each.
//
// Read-mostly against its OWN throwaway instance (port 0, temp profile);
// safe to run against prod-looking checkouts.
import { spawn } from "node:child_process";
import { setTimeout as sleep } from "node:timers/promises";
import http from "node:http";
import fs from "node:fs";

const REPO = process.env.PP_REPO || "/home/roerick/dev/wyo.tech/pricklypear";
const AGENT = process.env.AGENT_BIN || `${REPO}/_build/default/glochid/bin/main.exe`;
const MOCK = `/tmp/glochid-shelf-probe-${process.pid}.json`;
const CHROME = process.env.CHROME || "/usr/bin/chromium";
const USER_DATA = `/tmp/chrome-glochid-shelf-${process.pid}`;

function httpGet(url) {
  return new Promise((resolve, reject) => {
    http.get(url, (res) => { let d = ""; res.on("data", (c) => (d += c)); res.on("end", () => resolve({ status: res.statusCode, body: d })); }).on("error", reject);
  });
}
const wsData = (ev) => { const d = ev?.data ?? ev; return Buffer.isBuffer?.(d) ? d.toString() : String(d); };

let agent, chrome;
function cleanup() { try { agent?.kill(); } catch (_) {} try { chrome?.kill(); } catch (_) {} fs.rmSync(USER_DATA, { recursive: true, force: true }); fs.rmSync(MOCK, { force: true }); try { fs.rmSync(TMP, { recursive: true, force: true }); } catch (_) {} }
process.on("exit", cleanup);

fs.writeFileSync(MOCK, JSON.stringify([{ text: "shelf-probe reply", calls: [] }]) + "\n");
const TMP = fs.mkdtempSync("/tmp/glochid-shelf-cwd-");
agent = spawn(AGENT, ["--serve", "--port", "0", "--mock-script", MOCK], { stdio: ["ignore", "pipe", "pipe"], cwd: TMP });
let agentUrl = "", buf = "";
agent.stdout.on("data", (c) => { buf += c; const m = buf.match(/glochid serving (http:\/\/\S+)/); if (m) agentUrl = m[1].replace(/\/$/, ""); });
agent.stderr.on("data", (c) => { buf += c; });
for (let i = 0; i < 60 && !agentUrl; i++) await sleep(250);
if (!agentUrl) { console.error("glochid never printed URL; out:\n" + buf.slice(-2000)); process.exit(1); }
console.log("glochid at", agentUrl);

chrome = spawn(CHROME, [`--remote-debugging-port=9047`, `--user-data-dir=${USER_DATA}`, "--headless=new", "--disable-gpu", "--no-sandbox", "--disable-dev-shm-usage", "--window-size=1880,1000", "about:blank"], { stdio: "ignore" });
let ready = false;
for (let i = 0; i < 40; i++) { try { if ((await httpGet(`http://127.0.0.1:9047/json/version`)).status === 200) { ready = true; break; } } catch (_) {} await sleep(150); }
if (!ready) { console.error("CDP not ready"); process.exit(1); }
const list = JSON.parse((await httpGet(`http://127.0.0.1:9047/json/list`)).body);
const page = list.find((t) => t.type === "page");
const ws = new globalThis.WebSocket(page.webSocketDebuggerUrl);
await new Promise((res, rej) => { ws.onopen = res; ws.onerror = rej; });

let nextId = 1;
const pending = new Map();
ws.onmessage = (ev) => { const m = JSON.parse(wsData(ev)); if (m.id && pending.has(m.id)) { pending.get(m.id)(m); pending.delete(m.id); } };
function send(method, params = {}) { const id = nextId++; ws.send(JSON.stringify({ id, method, params })); return new Promise((res) => pending.set(id, res)); }
async function evalJs(expression) {
  const r = await send("Runtime.evaluate", { expression, returnByValue: true, awaitPromise: true });
  if (r.result?.exceptionDetails) throw new Error(JSON.stringify(r.result.exceptionDetails));
  return r.result?.result?.value ?? r.result?.value;
}

await send("Page.enable"); await send("Runtime.enable");
await send("Network.enable");
const netLog = [];
ws.onmessage = wrapHandler(ws.onmessage);
function wrapHandler(prev) { return (ev) => { try { const m = JSON.parse(wsData(ev));
  if (m.method === "Network.requestWillBeSent") netLog.push({ req: m.params.request.url, method: m.params.request.method, postData: m.params.request.postData });
  if (m.method === "Network.responseReceived") { const l = netLog[netLog.length-1]; if (l) l.status = m.params.response.status; }
  if (m.method === "Runtime.consoleAPICalled") console.log("[console]", m.params.type, m.params.args?.map(a => a.value ?? a.description ?? "").join(" ")); } catch (_) {} prev(ev); }; }
await send("Page.navigate", { url: agentUrl });
await sleep(1800);

// collect htmx client events (AFTER navigation; re-arm whenever we nav)
const armHtmx = `window.__htmx = [];
  ['htmx:before:request','htmx:after:request','htmx:before-swap','htmx:after:swap','htmx:response:error','htmx:send:error'].forEach(n =>
    document.body.addEventListener(n, (e) => window.__htmx.push({ ev: n, path: e.detail?.pathInfo?.requestPath, elt: e.detail?.elt?.outerHTML?.slice(0,120) })));
  "armed"`;
await evalJs(armHtmx);

// 1. Open a real pane through the launcher POST (open-or-focus).
await evalJs(`htmx.ajax('POST', '/filmstrip/open', { values: { route: '/settings' }, target: '#fs-sink', swap: 'innerHTML' })`);
await sleep(2000);

const paneProbe = await evalJs(`(function(){
  const p = document.querySelector('#fs-content .fs-pane');
  return p ? { id: p.dataset.pane, cls: p.className } : null; })()`);
console.log("live pane:", JSON.stringify(paneProbe));
if (!paneProbe) { console.error("no pane opened; abort"); process.exit(1); }
const PID = paneProbe.id.replace(/"/g, "");

// 2. Shelf it the way the operator does: POST /filmstrip/close via htmx.
await evalJs(`htmx.ajax('POST', '/filmstrip/close', { values: { id: "${PID}" }, target: '#fs-sink', swap: 'innerHTML' })`);
await sleep(1500);

const shelfState = await evalJs(`(function(){
  const p = document.getElementById('fs-pane-${PID}');
  const tree = document.getElementById('fs-tree-windows-root');
  return { paneCls: p ? p.className : null,
           shelfLis: tree ? [...tree.querySelectorAll('li[data-node="shelf"]')].length : -1 }; })()`);
console.log("after close:", JSON.stringify(shelfState));

// 3. Expand the shelf subtree (toggle once pull).
await evalJs(`(function(){
  const d = document.querySelector('#fs-tree-windows-root li[data-node="shelf"] details');
  if (!d) return "no shelf details";
  d.open = true; d.dispatchEvent(new Event('toggle'));
  return "expanded"; })()`);
await sleep(1800);

const shelfRow = await evalJs(`(function(){
  const shelf = document.querySelector('#fs-tree-windows-root li[data-node="shelf"]');
  const row = shelf && shelf.querySelector('li[data-node="pane"] , li[data-node^="pane/"]');
  const btns = row ? [...row.querySelectorAll('button[data-verb]')].map(b => ({v: b.dataset.verb, hx: b.getAttribute('hx-post'), trig: b.getAttribute('hx-trigger')})).filter(b => b.hx.indexOf('restore') >= 0 || b.hx.indexOf('destroy') >= 0) : null;
  return { found: !!row, btns }; })()`);
console.log("shelf row buttons:", JSON.stringify(shelfRow));
netLog.length = 0;

// 4. CLICK restore like an operator.
await evalJs(`(function(){
  const b = document.querySelector('#fs-tree-windows-root li[data-node="shelf"] button[data-verb="restore"]');
  if (!b) return "no restore button";
  b.click(); return "clicked"; })()`);
await sleep(2000);

const afterRestore = await evalJs(`(function(){
  const p = document.getElementById('fs-pane-${PID}');
  const tree = document.getElementById('fs-tree-windows-root');
  const shelfLi = tree && tree.querySelector('li[data-node="shelf"]');
  return { paneExists: !!p, paneCls: p ? p.className : null,
           shelfStillHasRow: !!(shelfLi && shelfLi.querySelector('li[data-node^="pane/"]')),
           desk0Rows: [...(tree?.querySelectorAll('li[data-node="desk/0"] li[data-node^="pane/"]') || [])].length,
           events: (window.__htmx || []).slice(-6) }; })()`);
console.log("after restore CLICK:", JSON.stringify(afterRestore, null, 1));
console.log("network during restore:", JSON.stringify(netLog.map(n => ({ method: n.method, url: n.req, post: n.postData, status: n.status })), null, 1));

// 5. Re-shelve, then CLICK destroy.
await evalJs(`htmx.ajax('POST', '/filmstrip/close', { values: { id: "${PID}" }, target: '#fs-sink', swap: 'innerHTML' })`);
await sleep(1200);
await evalJs(`(function(){
  const d = document.querySelector('#fs-tree-windows-root li[data-node="shelf"] details');
  if (d) { d.open = true; d.dispatchEvent(new Event('toggle')); }
  return "expanded"; })()`);
await sleep(1500);
await evalJs(`(function(){
  const b = document.querySelector('#fs-tree-windows-root li[data-node="shelf"] button[data-verb="destroy"]');
  if (!b) return "no destroy button";
  b.click(); return "clicked"; })()`);
await sleep(2000);

const afterDestroy = await evalJs(`(function(){
  const p = document.getElementById('fs-pane-${PID}');
  const tree = document.getElementById('fs-tree-windows-root');
  const shelfLi = tree && tree.querySelector('li[data-node="shelf"]');
  return { paneExists: !!p,
           shelfStillHasRow: !!(shelfLi && shelfLi.querySelector('li[data-node^="pane/"]')),
           events: (window.__htmx || []).slice(-6) }; })()`);
console.log("after destroy CLICK:", JSON.stringify(afterDestroy, null, 1));
console.log("network during destroy:", JSON.stringify(netLog.map(n => ({ method: n.method, url: n.req, post: n.postData, status: n.status }))));

console.log("done");
process.exit(0);
