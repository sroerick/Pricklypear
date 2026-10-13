#!/usr/bin/env node
// CDP probe: does the chat input CLEAR at submit (htmx:before:request) while
// the turn still reaches the DOM? Boots its own glochid --serve --mock-script.
import { spawn } from "node:child_process";
import { setTimeout as sleep } from "node:timers/promises";
import http from "node:http";
import fs from "node:fs";

const GLOCHID = process.env.GLOCHID_BIN || process.env.AGENT_BIN || "/home/roerick/dev/wyo.tech/pricklypear-sec/_build/default/glochid/bin/main.exe";
const MOCK = `/tmp/glochid-clear-probe-${process.pid}.json`;
const PORTC = 9033;
const CHROME = process.env.CHROME || "/usr/bin/chromium";
const USER_DATA = `/tmp/chrome-glochid-clear-${process.pid}`;

function httpGet(url) {
  return new Promise((resolve, reject) => {
    http.get(url, (res) => { let d = ""; res.on("data", (c) => (d += c)); res.on("end", () => resolve({ status: res.statusCode, body: d })); }).on("error", reject);
  });
}
const wsData = (ev) => { const d = ev?.data ?? ev; return Buffer.isBuffer?.(d) ? d.toString() : String(d); };

let glochid, chrome;
function cleanup() { try { glochid?.kill(); } catch (_) {} try { chrome?.kill(); } catch (_) {} fs.rmSync(USER_DATA, { recursive: true, force: true }); fs.rmSync(MOCK, { force: true }); }
process.on("exit", cleanup);

fs.writeFileSync(MOCK, JSON.stringify([{ text: "clear-probe reply", calls: [] }]) + "\n");
glochid = spawn(GLOCHID, ["--serve", "--port", "0", "--mock-script", MOCK], { stdio: ["ignore", "pipe", "pipe"], cwd: "/home/roerick/dev/wyo.tech/pricklypear" });
let glochidUrl = "", buf = "";
glochid.stdout.on("data", (c) => { buf += c; const m = buf.match(/glochid serving (http:\/\/\S+)/); if (m) glochidUrl = m[1].replace(/\/$/, ""); });
for (let i = 0; i < 60 && !glochidUrl; i++) await sleep(250);
if (!glochidUrl) { console.error("glochid never printed URL"); process.exit(1); }
console.log("glochid at", glochidUrl);

chrome = spawn(CHROME, [`--remote-debugging-port=${PORTC}`, `--user-data-dir=${USER_DATA}`, "--headless=new", "--disable-gpu", "--no-sandbox", "--disable-dev-shm-usage", "--window-size=1280,800", "about:blank"], { stdio: "ignore" });
let ready = false;
for (let i = 0; i < 40; i++) { try { if ((await httpGet(`http://127.0.0.1:${PORTC}/json/version`)).status === 200) { ready = true; break; } } catch (_) {} await sleep(150); }
if (!ready) { console.error("CDP not ready"); process.exit(1); }
const list = JSON.parse((await httpGet(`http://127.0.0.1:${PORTC}/json/list`)).body);
const page = list.find((t) => t.type === "page");
const ws = new globalThis.WebSocket(page.webSocketDebuggerUrl);
await new Promise((res, rej) => { ws.onopen = res; ws.onerror = rej; });
let nextId = 1;
const send = (method, params = {}) => new Promise((resolve) => {
  const id = nextId++;
  const onMsg = (ev) => { const j = JSON.parse(wsData(ev)); if (j.id === id) { ws.removeEventListener("message", onMsg); resolve(j.result); } };
  ws.addEventListener("message", onMsg);
  ws.send(JSON.stringify({ id, method, params }));
});
await send("Runtime.enable"); await send("Page.enable");
await send("Page.navigate", { url: glochidUrl });
await sleep(1200);

const ev = await send("Runtime.evaluate", { expression: `(function(){
  window.__events = [];
  ['htmx:before:request','htmx:after:request','htmx:before-request','htmx:after-request'].forEach(n =>
    window.addEventListener(n, () => window.__events.push(n)));
  var m = document.getElementById('msg');
  m.value = 'clear me';
  document.getElementById('f').requestSubmit(document.querySelector('#f button'));
  return m.value; })()`, returnByValue: true });
const immediately = ev.result.value;
await sleep(300);
const r2 = await send("Runtime.evaluate", { expression: `(function(){
  return JSON.stringify({ value300ms: document.getElementById('msg')?.value ?? null,
    events: window.__events,
    bubbles: document.querySelectorAll('#log .bubble').length,
    reply: document.querySelector('#log .bubble.llm')?.textContent || null }); })()`, returnByValue: true });
console.log("value right after requestSubmit:", JSON.stringify(immediately));
console.log(r2.result.value);

// ---- REPL mode: a leading ( evaluates locally, no model turn ----
await send("Runtime.evaluate", { expression: `(function(){
  var m = document.getElementById('msg');
  m.value = '(+ 2 2)';
  document.getElementById('f').requestSubmit(document.querySelector('#f button'));
  return true; })()`, returnByValue: true });
await sleep(500);
const r3 = await send("Runtime.evaluate", { expression: `(function(){
  var repl = document.querySelectorAll('#log .bubble.repl');
  return JSON.stringify({ replBubbles: repl.length,
    last: repl.length ? repl[repl.length-1].textContent : null }); })()`, returnByValue: true });
console.log("repl:", r3.result.value);

// ---- error path: unbound var must render READABLY, not Interp_error(_) ----
await send("Runtime.evaluate", { expression: `(function(){
  var m = document.getElementById('msg');
  m.value = '(no-such-fn 1)';
  document.getElementById('f').requestSubmit(document.querySelector('#f button'));
  return true; })()`, returnByValue: true });
await sleep(500);
const r4 = await send("Runtime.evaluate", { expression: `(function(){
  var traces = document.querySelectorAll('#log .trace');
  var last = traces.length ? traces[traces.length-1].textContent : '';
  return JSON.stringify({ opaque: last.indexOf('Interp_error') >= 0,
    readable: last.indexOf('unbound') >= 0, last }); })()`, returnByValue: true });
console.log("err:", r4.result.value);
process.exit(0);
