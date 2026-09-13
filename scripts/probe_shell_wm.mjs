#!/usr/bin/env node
// probe_shell_wm.mjs — read-only geometry probe for the glochid/filmstrip
// shell (window-controlbox-v2 make harness). Boots headless chromium,
// loads a running agent, measures shell/strip/pane geometry, dumps JSON
// and screenshots to /tmp.
//
//   PROBE_URL=http://127.0.0.1:18091/ node scripts/probe_shell_wm.mjs
//   FLIP=1 ...   # also client-flip the axis sentinel both ways (in-DOM
//                # only, restored after; never persists anything)
//   SHOT=1 ...   # write /tmp/wm-col.png /tmp/wm-row.png (col as loaded)
//
// Read-only by default: NO posts, NO mutations against the target.
import { spawn } from "node:child_process";
import { setTimeout as sleep } from "node:timers/promises";
import http from "node:http";
import fs from "node:fs";

const URL_TARGET = process.env.PROBE_URL || "http://127.0.0.1:18091/";
const PORTC = 9044;
const CHROME = process.env.CHROME || "/usr/bin/chromium";
const USER_DATA = `/tmp/chrome-wmprobe-${process.pid}`;

function httpGet(url) {
  return new Promise((resolve, reject) => {
    http.get(url, (res) => {
      let d = ""; res.on("data", (c) => (d += c));
      res.on("end", () => resolve({ status: res.statusCode, body: d }));
    }).on("error", reject);
  });
}
const wsData = (ev) => { const d = ev?.data ?? ev; return Buffer.isBuffer?.(d) ? d.toString() : String(d); };

let chrome;
process.on("exit", () => { try { chrome?.kill(); } catch (_) {} fs.rmSync(USER_DATA, { recursive: true, force: true }); });

chrome = spawn(CHROME, [
  `--remote-debugging-port=${PORTC}`, `--user-data-dir=${USER_DATA}`,
  "--headless=new", "--disable-gpu", "--no-sandbox", "--disable-dev-shm-usage",
  "--window-size=1880,1000", "--hide-scrollbars", "about:blank",
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
const pending = new Map();
ws.onmessage = (ev) => { const m = JSON.parse(wsData(ev)); if (m.id && pending.has(m.id)) { pending.get(m.id)(m); pending.delete(m.id); } };
function send(method, params = {}) { const id = nextId++; ws.send(JSON.stringify({ id, method, params })); return new Promise((res) => pending.set(id, res)); }
async function evalJs(expression) {
  const r = await send("Runtime.evaluate", { expression, returnByValue: true, awaitPromise: true });
  if (r.result?.exceptionDetails) throw new Error(JSON.stringify(r.result.exceptionDetails));
  return r.result?.result?.value ?? r.result?.value;
}
async function shot(name) {
  const r = await send("Page.captureScreenshot", { format: "png" });
  fs.writeFileSync(name, Buffer.from(r.result.data, "base64"));
  console.error("shot:", name);
}

await send("Page.enable"); await send("Runtime.enable");
await send("Page.navigate", { url: URL_TARGET });
await sleep(1500);

const MEASURE = `(function(){
  function rect(e){ if(!e) return null; const r=e.getBoundingClientRect();
    return {x:Math.round(r.x),y:Math.round(r.y),w:Math.round(r.width),h:Math.round(r.height)}; }
  const q = (s)=>document.querySelector(s);
  const shell = q('#fs-shell');
  const panes = [...document.querySelectorAll('.fs-pane')];
  const cboxes = [...document.querySelectorAll('.fs-cbox')];
  return {
    url: location.href,
    axisSentinel: (q('#fs-axis')||{}).className || null,
    deskSentinel: (q('#ws-desk')||{}).className || null,
    shell: rect(shell),
    rail: rect(q('.fs-rail')),
    cascade: rect(q('#fs-cascade') || q('.fs-cascade')),
    content: rect(q('#fs-content')),
    panes: panes.map(p => ({ cls: p.className, desk: p.dataset.desktop || null,
                             rect: rect(p),
                             bodyShown: (()=>{ const b=p.querySelector('.fs-body');
                               return b ? getComputedStyle(b).display !== 'none' : false; })() })),
    controlboxes: cboxes.map(c => ({ label: c.dataset.cbox || c.textContent.trim().slice(0,40), rect: rect(c) })),
    errors: window.__ppErrors || []
  };
})()`;

const out = {};
out.asLoaded = await evalJs(MEASURE);
if (process.env.SHOT) await shot("/tmp/wm-col.png");

if (process.env.FLIP && out.asLoaded.axisSentinel) {
  const cur = out.asLoaded.axisSentinel.includes("col") ? "col" : "row";
  const other = cur === "col" ? "row" : "col";
  await evalJs(`(()=>{const s=document.getElementById('fs-axis');s.classList.replace('${cur}','${other}');return s.className})()`);
  await sleep(400);
  out["flippedTo-" + other] = await evalJs(MEASURE);
  if (process.env.SHOT) await shot("/tmp/wm-" + other + ".png");
  await evalJs(`(()=>{const s=document.getElementById('fs-axis');s.classList.replace('${other}','${cur}');return s.className})()`);
  await sleep(200);
  out.restored = await evalJs(MEASURE);
}

console.log(JSON.stringify(out, null, 2));
process.exit(0);
