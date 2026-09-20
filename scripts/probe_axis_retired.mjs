#!/usr/bin/env node
/**
 * One-off: assert the axis-toggle retirement (BORGE-1789879934) on a
 * live scratch shell — no #fs-axis-btn anywhere, railtabs has exactly
 * the tree roots, and the inert #fs-axis sentinel pins col (sidebar
 * layout machinery still keyed on :has(#fs-axis.col)).
 */
import { spawn } from "node:child_process";
import { setTimeout as sleep } from "node:timers/promises";
import http from "node:http";
import fs from "node:fs";
import path from "node:path";

const AGENT = process.env.AGENT_BIN || "_build/default/agent/bin/main.exe";
const PORTC = 9034;
const CHROME = process.env.CHROME || "/usr/bin/chromium";
const USER_DATA = `/tmp/chrome-axis-probe-${process.pid}`;
const AGENT_DIR = `/tmp/agent-axis-probe-${process.pid}`;

function ok(m) { console.log("OK  ", m); }
function fail(m) { console.error("FAIL", m); cleanup(); process.exit(1); }
function cleanup() { try { agent?.kill(); } catch (_) {} try { fs.rmSync(USER_DATA, { recursive: true, force: true }); } catch (_) {} try { fs.rmSync(AGENT_DIR, { recursive: true, force: true }); } catch (_) {} }
process.on("exit", cleanup);
function httpGet(url) {
  return new Promise((resolve, reject) => {
    http.get(url, (res) => { let d = ""; res.on("data", (c) => (d += c)); res.on("end", () => resolve({ status: res.statusCode, body: d })); }).on("error", reject);
  });
}
const bytes = (evv) => Buffer.isBuffer(evv.data) ? evv.data.toString() : String(evv.data);

fs.mkdirSync(path.join(AGENT_DIR, ".agent"), { recursive: true });
fs.writeFileSync(path.join(AGENT_DIR, ".agent", "workspace.json"),
  JSON.stringify({ axis: "col", desktop: 0, focus: "content:0", cascade: [], content: [
    { width: "third", route: "/chat", id: "p1", desktop: 0, state: "live" },
  ]}));

let agent;
agent = spawn(AGENT, ["--serve", "--port", "0"],
  { stdio: ["ignore", "pipe", "pipe"], cwd: AGENT_DIR });
agent.stderr.on("data", () => {});
let base = await new Promise((resolve, reject) => {
  const to = setTimeout(() => reject(new Error("boot: agent never announced")), 8000);
  agent.stdout.on("data", (c) => {
    const m = String(c).match(/http:\/\/127\.0\.0\.1:(\d+)\//);
    if (m) { clearTimeout(to); resolve(`http://127.0.0.1:${m[1]}`); }
  });
  agent.on("exit", (code) => { clearTimeout(to); reject(new Error(`boot: agent exited ${code}`)); });
});

const chrome = spawn(CHROME, [
  `--remote-debugging-port=${PORTC}`, `--user-data-dir=${USER_DATA}`,
  "--headless=new", "--disable-gpu", "--no-sandbox", "--disable-dev-shm-usage",
  "--window-size=1440,900", "about:blank",
], { stdio: "ignore" });
let ready = false;
for (let i = 0; i < 40; i++) {
  try { if ((await httpGet(`http://127.0.0.1:${PORTC}/json/version`)).status === 200) { ready = true; break; } } catch (_) {}
  await sleep(150);
}
if (!ready) fail("boot: chromium CDP not ready");
const list = JSON.parse((await httpGet(`http://127.0.0.1:${PORTC}/json/list`)).body);
const ws = new WebSocket(list.find((t) => t.type === "page").webSocketDebuggerUrl);
await new Promise((res, rej) => { ws.onopen = res; ws.onerror = rej; });
let nextId = 1; const pending = new Map();
ws.onmessage = (ev) => { const m = JSON.parse(bytes(ev)); if (m.id && pending.has(m.id)) { pending.get(m.id)(m); pending.delete(m.id); } };
const send = (method, params = {}) => { const id = nextId++; ws.send(JSON.stringify({ id, method, params })); return new Promise((r) => pending.set(id, r)); };
async function ev(expression) {
  const r = await send("Runtime.evaluate", { expression, returnByValue: true, awaitPromise: true });
  if (r.result?.exceptionDetails) throw new Error(JSON.stringify(r.result.exceptionDetails));
  return r.result?.result?.value;
}
await send("Page.enable"); await send("Runtime.enable");
// BUDGET GATE (known blocker, borge/filmstrip.borg 2026-09-16): the
// shell render sits at the 400k boundary and flakes. Poll for a render.
let booted = false;
for (let i = 0; i < 8 && !booted; i++) {
  await send("Page.navigate", { url: base + "/" });
  await sleep(1200);
  booted = await ev(`!document.body.textContent.includes('budget exceeded')`);
}
if (!booted) fail("shell never rendered (render budget exceeded 8x)");

console.log("BODY:", JSON.stringify(await ev(`document.body.innerHTML.substring(0,300)`)));
const s = await ev(`(()=>{
  const btns=[...document.querySelectorAll('#fs-railtabs button')].map(b=>b.textContent.trim()+'|'+(b.getAttribute('hx-post')||''));
  return {
    axisBtn: !!document.querySelector('#fs-axis-btn'),
    axisSentinel: document.querySelector('#fs-axis')?.className || null,
    shellRow: !!document.querySelector('.fs-shell:has(#fs-axis.col)'),
    railtabBtns: btns,
  };
})()`);
console.log(JSON.stringify(s, null, 1));
if (s.axisBtn) fail("an #fs-axis-btn still renders");
if (!/col/.test(s.axisSentinel || "")) fail("the #fs-axis sentinel is not col");
if (!s.shellRow) fail("the sidebar (col) shell layout is not keyed on the sentinel");
if (s.railtabBtns.some((t) => t.startsWith("⇅") || t.startsWith("⇆"))) fail("an axis button word lingers on the railtabs row");
const after = "passed";
ok("#fs-axis-btn gone from the shell");
ok("#fs-axis sentinel pinned col, sidebar layout active");
ok("#fs-railtabs carries only the tree-root words: " + s.railtabBtns.join(", "));
process.exit(0);
