#!/usr/bin/env node
// One-off CDP probe: why is #msg (the chat input) not visible on the glochid
// chat surface? Boots headless chromium, navigates to the running agent,
// dumps layout/computed style + post-htmx DOM + console errors.
import { spawn } from "node:child_process";
import { setTimeout as sleep } from "node:timers/promises";
import http from "node:http";
import fs from "node:fs";

const URL_TARGET = process.env.PROBE_URL || "http://127.0.0.1:37723/";
const PORTC = 9031;
const CHROME = process.env.CHROME || "/usr/bin/chromium";
const USER_DATA = `/tmp/chrome-glochid-probe-${process.pid}`;

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

let chrome;
process.on("exit", () => { try { chrome?.kill(); } catch (_) {} fs.rmSync(USER_DATA, { recursive: true, force: true }); });

chrome = spawn(CHROME, [
  `--remote-debugging-port=${PORTC}`, `--user-data-dir=${USER_DATA}`,
  "--headless=new", "--disable-gpu", "--no-sandbox", "--disable-dev-shm-usage",
  "--window-size=1280,800", "about:blank",
], { stdio: "ignore" });

let ready = false;
for (let i = 0; i < 40; i++) {
  try { if ((await httpGet(`http://127.0.0.1:${PORTC}/json/version`)).status === 200) { ready = true; break; } } catch (_) {}
  await sleep(150);
}
if (!ready) { console.error("chromium CDP not ready"); process.exit(1); }

const list = JSON.parse((await httpGet(`http://127.0.0.1:${PORTC}/json/list`)).body);
const page = list.find((t) => t.type === "page");
const ws = new globalThis.WebSocket(page.webSocketDebuggerUrl);
await new Promise((res, rej) => { ws.onopen = res; ws.onerror = rej; });

let nextId = 1;
const logs = [];
ws.addEventListener("message", (ev) => {
  const j = JSON.parse(wsData(ev));
  if (j.id) return;
  if (j.method === "Runtime.consoleAPICalled") logs.push(j.params.args.map((a) => a.value ?? a.description ?? "").join(" "));
  if (j.method === "Runtime.exceptionThrown") logs.push("EXC " + (j.params.exceptionDetails?.exception?.description || j.params.exceptionDetails?.text));
});
const send = (method, params = {}) => new Promise((resolve) => {
  const id = nextId++;
  const onMsg = (ev) => { const j = JSON.parse(wsData(ev)); if (j.id === id) { ws.removeEventListener("message", onMsg); resolve(j.result); } };
  ws.addEventListener("message", onMsg);
  ws.send(JSON.stringify({ id, method, params }));
});

await send("Runtime.enable");
await send("Page.enable");
await send("Page.navigate", { url: URL_TARGET });
await sleep(1500);

const r = await send("Runtime.evaluate", { expression: `(function(){
  const m = document.getElementById('msg');
  const f = document.getElementById('f');
  const cs = m ? getComputedStyle(m) : null;
  const fs = f ? getComputedStyle(f) : null;
  return JSON.stringify({
    inputPresent: !!m,
    inputInForm: !!(m && f && m.closest('#f') === f),
    formChildNodes: f ? [...f.childNodes].map(n => n.nodeType + ':' + (n.nodeName||'')) : null,
    rect: m ? m.getBoundingClientRect().toJSON() : null,
    display: cs?.display, width: cs?.width, height: cs?.height,
    position: cs?.position, visibility: cs?.visibility, opacity: cs?.opacity,
    formDisplay: fs?.display,
    bodyLastHtml: document.body.innerHTML.slice(-900),
  }, null, 1);
})()`, returnByValue: true });
console.log(r.result.value);
console.log("---- console ----");
logs.forEach((l) => console.log(l));
process.exit(0);
