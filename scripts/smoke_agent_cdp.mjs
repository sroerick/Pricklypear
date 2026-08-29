#!/usr/bin/env node
/**
 * Agent chat-surface CDP smoke: drives a REAL headless chromium against
 * `agent --serve --mock-script`, submits the hx-post /chat form, and
 * asserts the multipart stream landed in the DOM (user bubble, llm bubble,
 * assistant token span) and that the htmx request completed
 * (htmx:multipart:after:part fired per part per the upstream hx-multipart
 * extension semantics).
 *
 * This is the htmx-4 stack regression net: htmx.min.js upgrade, extension
 * re-vendor, or /chat framing regressions all fail here.
 *
 * Prereq: dune build agent/bin/main.exe; /usr/bin/chromium present.
 * Exit 0 on success.
 */
import { spawn } from "node:child_process";
import { setTimeout as sleep } from "node:timers/promises";
import http from "node:http";
import fs from "node:fs";

const AGENT = process.env.AGENT_BIN || "_build/default/agent/bin/main.exe";
const PORTC = 9025;
const CHROME = process.env.CHROME || "/usr/bin/chromium";
const USER_DATA = `/tmp/chrome-agent-cdp-${process.pid}`;
const MOCK = `/tmp/agent-cdp-mock-${process.pid}.json`;

function ok(m) { console.log("OK  ", m); }
function fail(m) { console.error("FAIL", m); cleanup(); process.exit(1); }

function httpGet(url) {
  return new Promise((resolve, reject) => {
    http.get(url, (res) => {
      let d = "";
      res.on("data", (c) => (d += c));
      res.on("end", () => resolve({ status: res.statusCode, body: d }));
    }).on("error", reject);
  });
}
const wsData = (ev) => {
  const d = ev?.data ?? ev;
  return Buffer.isBuffer?.(d) ? d.toString() : String(d);
};
async function cdpSend(ws, id, method, params = {}) {
  return new Promise((resolve, reject) => {
    const timer = setTimeout(() => { cleanup2(); reject(new Error(`CDP timeout ${method}`)); }, 10000);
    const onMsg = (ev) => {
      try { const j = JSON.parse(wsData(ev)); if (j.id === id) { cleanup2(); resolve(j.result); } } catch (_) {}
    };
    const onErr = (e) => { cleanup2(); reject(e?.error || e); };
    function cleanup2() { clearTimeout(timer); ws.removeEventListener("message", onMsg); ws.removeEventListener("error", onErr); }
    ws.addEventListener("message", onMsg);
    ws.addEventListener("error", onErr);
    ws.send(JSON.stringify({ id, method, params }));
  });
}

let agent, chrome;
function cleanup() {
  try { agent?.kill(); } catch (_) {}
  try { chrome?.kill(); } catch (_) {}
  try { fs.rmSync(MOCK, { force: true }); } catch (_) {}
}
process.on("exit", cleanup);

if (!fs.existsSync(AGENT)) fail(`${AGENT} missing (run: dune build agent/bin/main.exe)`);
fs.writeFileSync(MOCK, JSON.stringify([{ text: "cdp smoke reply", calls: [] }]) + "\n");

// --- boot agent surface ---
agent = spawn(AGENT, ["--serve", "--mock-script", MOCK], { stdio: ["ignore", "pipe", "pipe"] });
let agentUrl = "";
let buf = "";
agent.stdout.on("data", (c) => {
  buf += c;
  const m = buf.match(/agent serving (http:\/\/\S+)/);
  if (m) agentUrl = m[1].replace(/\/$/, "");
});
for (let i = 0; i < 60 && !agentUrl; i++) await sleep(250);
if (!agentUrl) fail("agent never printed its URL");
ok(`agent at ${agentUrl}`);

// --- boot chromium CDP ---
chrome = spawn(CHROME, [
  `--remote-debugging-port=${PORTC}`,
  `--user-data-dir=${USER_DATA}`,
  "--headless=new", "--disable-gpu", "--no-sandbox",
  "--disable-dev-shm-usage", "--window-size=1200,800",
  "about:blank",
], { stdio: "ignore" });

let ready = false;
for (let i = 0; i < 40; i++) {
  try { if ((await httpGet(`http://127.0.0.1:${PORTC}/json/version`)).status === 200) { ready = true; break; } } catch (_) {}
  await sleep(150);
}
if (!ready) fail("chromium CDP not ready");

const list = JSON.parse((await httpGet(`http://127.0.0.1:${PORTC}/json/list`)).body);
const page = list.find((t) => t.type === "page");
if (!page?.webSocketDebuggerUrl) fail("no page target");

const ws = new globalThis.WebSocket(page.webSocketDebuggerUrl);
await new Promise((res, rej) => {
  const t = setTimeout(() => rej(new Error("ws open timeout")), 10000);
  ws.onopen = () => { clearTimeout(t); res(); };
  ws.onerror = (e) => { clearTimeout(t); rej(e.error || e); };
});
ok("CDP websocket open");

let nextId = 1;
const send = (m, p) => cdpSend(ws, nextId++, m, p);
await send("Page.enable");
await send("Runtime.enable");

// Count multipart part events in-page so we can assert the streaming path.
await send("Page.navigate", { url: agentUrl });
await sleep(1200);

const arm = await send("Runtime.evaluate", {
  expression: `(function(){
    window.__partCount = 0; window.__done = false;
    window.addEventListener('htmx:multipart:after:part', function(){ window.__partCount++; });
    window.addEventListener('htmx:after:request', function(){ window.__done = true; });
    var f = document.getElementById('f');
    var m = document.getElementById('msg');
    var mr = m ? m.getBoundingClientRect() : null;
    window.__msgOk = !!(m && mr && mr.width > 10 && mr.height > 5);
    return !!f;
  })()`,
  returnByValue: true,
});
if (!arm?.result?.value) fail("chat form #f not found on page");
// The input must EXIST and be VISIBLE: a quoted-attribute parse bug (e.g.
// hx-on::after-request="this.reset()'>) makes the browser swallow the input
// into an attribute value — the form still works programmatically, but a
// human has no text box. Regression-guarded here since 2026-08-29.
await send("Runtime.evaluate", { expression: "window.__msgOk", returnByValue: true })
  .then((r) => { if (!r?.result?.value) fail("chat input #msg missing or zero-size (parse bug in chat/page markup?)"); });

// Submit through the DOM (same path a user's click takes).
const sub = await send("Runtime.evaluate", {
  expression: `(function(){
    document.getElementById('msg').value = 'hello smoke';
    document.getElementById('f').requestSubmit(document.querySelector('#f button'));
    return true;
  })()`,
  returnByValue: true,
});
if (!sub?.result?.value) fail("submit failed");

// Wait for the turn to complete.
let st = null;
for (let i = 0; i < 50; i++) {
  const r = await send("Runtime.evaluate", {
    expression: `(function(){
      var log = document.getElementById('log');
      return JSON.stringify({
        html: log ? log.innerHTML : '',
        parts: window.__partCount,
        done: window.__done
      });
    })()`,
    returnByValue: true,
  });
  st = JSON.parse(r.result.value);
  if (st.done) break;
  await sleep(200);
}

if (!st.done) fail("htmx request never completed (stream never terminated — framing regression?)");
if (st.parts < 3) fail(`expected >=3 multipart parts (echo, llm open, token), saw ${st.parts}`);
if (!st.html.includes('bubble user')) fail("user bubble missing from #log");
if (!st.html.includes('bubble llm')) fail("llm bubble missing from #log");
if (!st.html.includes('cdp smoke reply')) fail("assistant token span missing from #log");

ok(`turn streamed: ${st.parts} parts, DOM has user+llm bubbles + assistant text`);
cleanup();
process.exit(0);
