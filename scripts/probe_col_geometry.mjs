#!/usr/bin/env node
/**
 * One-off geometry probe: col-axis layout after the axis-button retirement.
 * Boots a scratch glochid with the OPERATOR's workspace (col, full chat pane
 * + cascade) and measures where cascade / #fs-rail / main pane actually land,
 * plus which :has(#fs-axis.col) rules matched. Reports side-by-side geometry.
 */
import { spawn } from "node:child_process";
import { setTimeout as sleep } from "node:timers/promises";
import http from "node:http";
import fs from "node:fs";
import path from "node:path";

const AGENT = process.env.AGENT_BIN || "_build/default/agent/bin/main.exe";
const PORTC = 9035;
const CHROME = process.env.CHROME || "/usr/bin/chromium";
const USER_DATA = `/tmp/chrome-geoprobe-${process.pid}`;
const AGENT_DIR = `/tmp/agent-geoprobe-${process.pid}`;

function cleanup() { try { agent?.kill(); } catch (_) {} try { chrome?.kill(); } catch (_) {} try { fs.rmSync(USER_DATA, { recursive: true, force: true }); } catch (_) {} try { fs.rmSync(AGENT_DIR, { recursive: true, force: true }); } catch (_) {} }
process.on("exit", cleanup);
process.on("SIGINT", () => { cleanup(); process.exit(1); });
function httpGet(url) {
  return new Promise((resolve, reject) => {
    http.get(url, (res) => { let d = ""; res.on("data", (c) => (d += c)); res.on("end", () => resolve({ status: res.statusCode, body: d })); }).on("error", reject);
  });
}
const bytes = (evv) => Buffer.isBuffer(evv.data) ? evv.data.toString() : String(evv.data);

let agent, chrome;
fs.mkdirSync(path.join(AGENT_DIR, ".agent"), { recursive: true });
fs.copyFileSync(path.join(".agent", "workspace.json"), path.join(AGENT_DIR, ".agent", "workspace.json"));

agent = spawn(AGENT, ["--serve", "--port", "0"], { stdio: ["ignore", "pipe", "pipe"], cwd: AGENT_DIR });
agent.stderr.on("data", () => {});
let base = await new Promise((resolve, reject) => {
  const to = setTimeout(() => reject(new Error("agent never announced")), 8000);
  agent.stdout.on("data", (c) => {
    const m = String(c).match(/http:\/\/127\.0\.0\.1:(\d+)\//);
    if (m) { clearTimeout(to); resolve(`http://127.0.0.1:${m[1]}`); }
  });
  agent.on("exit", (code) => { clearTimeout(to); reject(new Error(`agent exited ${code}`)); });
});

chrome = spawn(CHROME, [
  `--remote-debugging-port=${PORTC}`, `--user-data-dir=${USER_DATA}`,
  "--headless=new", "--disable-gpu", "--no-sandbox", "--disable-dev-shm-usage",
  "--window-size=1366,1010", "about:blank",
], { stdio: "ignore" });
let ready = false;
for (let i = 0; i < 40; i++) {
  try { if ((await httpGet(`http://127.0.0.1:${PORTC}/json/version`)).status === 200) { ready = true; break; } } catch (_) {}
  await sleep(150);
}
const list = JSON.parse((await httpGet(`http://127.0.0.1:${PORTC}/json/list`)).body);
const ws = new WebSocket(list.find((t) => t.type === "page").webSocketDebuggerUrl);
await new Promise((res, rej) => { ws.onopen = res; ws.onerror = rej; });
let nextId = 1; const pending = new Map();
ws.onmessage = (evv) => { const m = JSON.parse(bytes(evv)); if (m.id && pending.has(m.id)) { pending.get(m.id)(m); pending.delete(m.id); } };
const send = (method, params = {}) => { const id = nextId++; ws.send(JSON.stringify({ id, method, params })); return new Promise((r) => pending.set(id, r)); };
async function ev(expression) {
  const r = await send("Runtime.evaluate", { expression, returnByValue: true, awaitPromise: true });
  if (r.result?.exceptionDetails) throw new Error(JSON.stringify(r.result.exceptionDetails));
  return r.result?.result?.value;
}
await send("Page.enable"); await send("Runtime.enable");
await send("Page.navigate", { url: base + "/" });
await sleep(1200);

const OUT = await ev(`(()=>{
  const box=(e)=>{if(!e)return null;const r=e.getBoundingClientRect();return {x:Math.round(r.x),y:Math.round(r.y),w:Math.round(r.width),h:Math.round(r.height)};};
  const cs=(e,p)=>e?getComputedStyle(e)[p]:null;
  const cascade=document.querySelector('#fs-cascade');
  const surface=document.querySelector('#fs-surface')||document.querySelector('.fs-shell');
  const rail=document.querySelector('#fs-rail');
  const content=document.querySelector('#fs-content');
  const pane=document.querySelector('.fs-pane.focused')||document.querySelector('.fs-pane');
  const axis=document.querySelector('#fs-axis');
  const shell=document.querySelector('.fs-shell');
  const railbox=rail?rail.closest('[id]'):null;
  // find the box wrapping the cascade+rail (the col sidebar candidates)
  return {
    axisClass: axis?axis.className:null,
    shellDir: cs(shell,'flexDirection'),
    boxes: {
      surface: box(surface),
      cascade: box(cascade),
      rail: box(rail),
      content: box(content),
      pane: box(pane),
    },
    styles: {
      cascadeOrder: cascade?cs(cascade,'order')+' basis:'+cs(cascade,'flexBasis'):null,
      railOrder: rail?cs(rail,'order')+' basis:'+cs(rail,'flexBasis')+' dir:'+cs(rail,'flexDirection'):null,
      contentOrder: content?cs(content,'order')+' basis:'+cs(content,'flexBasis'):null,
      parentOfCascade: cascade?cascade.parentElement.id||cascade.parentElement.className:null,
      parentOfRail: rail?rail.parentElement.id||rail.parentElement.className:null,
      railMatch2: rail?cs(rail,'maxWidth')+'/'+cs(rail,'minWidth'):null,
    },
    railHTMLHead: rail?rail.outerHTML.slice(0,200):null,
  };
})()`);
console.log(JSON.stringify(OUT, null, 1));
// also screenshot
const resp = await send("Page.captureScreenshot", { format: "png" });
fs.writeFileSync("/tmp/glochid-geom-shot.png", Buffer.from(resp.result?.data ?? resp.data ?? "", "base64"));
console.log("shot written /tmp/glochid-geom-shot.png");
await sleep(200);
process.exit(0);
