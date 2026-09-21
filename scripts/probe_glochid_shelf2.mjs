#!/usr/bin/env node
// probe_glochid_shelf2.mjs — focused instrument of ONE restore click:
// who hears the click, who fires the request, with what path/verb.
import { spawn } from "node:child_process";
import { setTimeout as sleep } from "node:timers/promises";
import http from "node:http";
import fs from "node:fs";

const REPO = "/home/roerick/dev/wyo.tech/pricklypear";
const AGENT = `${REPO}/_build/default/agent/bin/main.exe`;
const MOCK = `/tmp/glochid-shelf2-${process.pid}.json`;
const TMP = fs.mkdtempSync("/tmp/glochid-shelf2-cwd-");
const USER_DATA = `/tmp/chrome-glochid-shelf2-${process.pid}`;

function httpGet(url) {
  return new Promise((resolve, reject) => {
    http.get(url, (res) => { let d = ""; res.on("data", (c) => (d += c)); res.on("end", () => resolve({ status: res.statusCode, body: d })); }).on("error", reject);
  });
}
const wsData = (ev) => { const d = ev?.data ?? ev; return Buffer.isBuffer?.(d) ? d.toString() : String(d); };

let agent, chrome;
function cleanup() { try { agent?.kill(); } catch (_) {} try { chrome?.kill(); } catch (_) {} fs.rmSync(USER_DATA, { recursive: true, force: true }); fs.rmSync(MOCK, { force: true }); try { fs.rmSync(TMP, { recursive: true, force: true }); } catch (_) {} }
process.on("exit", cleanup);

fs.writeFileSync(MOCK, JSON.stringify([{ text: "shelf2 reply", calls: [] }]) + "\n");
agent = spawn(AGENT, ["--serve", "--port", "0", "--mock-script", MOCK], { stdio: ["ignore", "pipe", "ignore"], cwd: TMP });
let agentUrl = "", buf = "";
agent.on("exit", (c) => { buf += "agent exited " + c; });
// port 0 prints URL to stdout
let out = "";
agent.stdout?.on("data", (c) => { out += c; const m = out.match(/agent serving (http:\/\/\S+)/); if (m) agentUrl = m[1].replace(/\/$/, ""); });
for (let i = 0; i < 60 && !agentUrl; i++) await sleep(250);
if (!agentUrl) { console.error("no url", buf); process.exit(1); }
console.log("agent at", agentUrl);

chrome = spawn("/usr/bin/chromium", ["--remote-debugging-port=9048", `--user-data-dir=${USER_DATA}`, "--headless=new", "--disable-gpu", "--no-sandbox", "--disable-dev-shm-usage", "--window-size=1880,1000", "about:blank"], { stdio: "ignore" });
let ready = false;
for (let i = 0; i < 40; i++) { try { if ((await httpGet(`http://127.0.0.1:9048/json/version`)).status === 200) { ready = true; break; } } catch (_) {} await sleep(150); }
if (!ready) { console.error("CDP not ready"); process.exit(1); }
const list = JSON.parse((await httpGet(`http://127.0.0.1:9048/json/list`)).body);
const page = list.find((t) => t.type === "page");
const ws = new globalThis.WebSocket(page.webSocketDebuggerUrl);
await new Promise((res, rej) => { ws.onopen = res; ws.onerror = rej; });
let nextId = 1;
const pending = new Map();
const netLog = [];
ws.onmessage = (ev) => {
  const m = JSON.parse(wsData(ev));
  if (m.id && pending.has(m.id)) { pending.get(m.id)(m); pending.delete(m.id); return; }
  if (m.method === "Network.requestWillBeSent") netLog.push({ req: m.params.request.url, method: m.params.request.method, post: m.params.request.postData });
  if (m.method === "Network.responseReceived") { const l = netLog[netLog.length - 1]; if (l) l.status = m.params.response.status; }
  if (m.method === "Runtime.consoleAPICalled") console.log("[console]", m.params.type, m.params.args?.map(a => a.value ?? a.description ?? "").join(" ").slice(0, 300));
};
function send(method, params = {}) { const id = nextId++; ws.send(JSON.stringify({ id, method, params })); return new Promise((res) => pending.set(id, res)); }
async function evalJs(expression) {
  const r = await send("Runtime.evaluate", { expression, returnByValue: true, awaitPromise: true });
  if (r.result?.exceptionDetails) throw new Error(JSON.stringify(r.result.exceptionDetails));
  return r.result?.result?.value ?? r.result?.value;
}

await send("Page.enable"); await send("Runtime.enable"); await send("Network.enable");
await send("Page.navigate", { url: agentUrl });
await sleep(2000);

const open = await evalJs(`htmx.ajax('POST', '/filmstrip/open', { values: { route: '/settings' }, target: '#fs-sink', swap: 'innerHTML' })`);
await sleep(2000);

// in-page listener: log every click's composed path + any htmx detail
await evalJs(`
  window.__clicks = [];
  document.addEventListener('click', (e) => window.__clicks.push({ target: e.target.tagName + '.' + e.target.className, path: e.composedPath().map(n => n.tagName || n.nodeName).slice(0,8).join('>') }), true);
  window.__req = [];
  document.body.addEventListener('htmx:before:request', (e) => window.__req.push({ path: e.detail?.pathInfo?.requestPath, verb: e.detail?.requestConfig?.verb, elt: e.detail?.elt?.outerHTML?.slice(0,150) }));
  'armed'`);

await evalJs(`htmx.ajax('POST', '/filmstrip/close', { values: { id: (document.querySelector('#fs-content .fs-pane')||{}).dataset?.pane }, target: '#fs-sink', swap: 'innerHTML' })`);
await sleep(1500);

const exp1 = await evalJs(`(function(){
  const d = document.querySelector('#fs-tree-windows-root li[data-node="shelf"] details');
  if (!d) return "no shelf details";
  d.open = true; d.dispatchEvent(new Event('toggle'));
  return "expanded"; })()`);
await sleep(1800);
console.log("expand:", exp1, "net after expand:", JSON.stringify(netLog.slice()));

netLog.length = 0;
const click = await evalJs(`(function(){
  const b = document.querySelector('#fs-tree-windows-root li[data-node="shelf"] button[data-verb="restore"]');
  if (!b) return "no restore button";
  window.__restoreBtn = b;
  b.click();
  return JSON.stringify({ isConnected: b.isConnected, cls: b.className, trig: b.getAttribute('hx-trigger'), post: b.getAttribute('hx-post'), detail: b.getAttribute('hx-post') + ' ' + b.getAttribute('hx-vals') }); })()`);
await sleep(2000);
console.log("click result:", click);
console.log("clicks seen:", JSON.stringify(evalJs ? ` ` : ``));
const clicks = await evalJs(`JSON.stringify(window.__clicks.slice(-3))`);
console.log("capture-phase clicks:", clicks);
const req = await evalJs(`JSON.stringify(window.__req.slice(-4))`);
console.log("htmx requests:", req);
console.log("net during click:", JSON.stringify(netLog.map(n => `${n.method} ${n.req} ${n.post || ""} ${n.status || ""}`)));
const finalState = await evalJs(`(function(){
  const b = window.__restoreBtn;
  const p = document.querySelector('#fs-content .fs-pane, .fs-pane.shelved');
  return { btnStillConnected: !!b, btnParent: b ? (b.closest('details')?.getAttribute('hx-get') || 'no hx-get on details') : null }; })()`);
console.log("post-click:", JSON.stringify(finalState));
process.exit(0);
