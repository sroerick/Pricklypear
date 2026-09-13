#!/usr/bin/env node
/**
 * probe_shell_sem.mjs — SEMANTIC shell probe for the glochid/filmstrip
 * shell (companion to the read-only geometry probe_shell_wm.mjs and the
 * chat-surface smoke_agent_cdp.mjs; UI review 2026-09, batch B).
 *
 * Boots its own scratch glochid (own agent cwd, mock LLM, random port,
 * seeded workspace) and asserts the shell's behavioral INVARIANTS under
 * real mutations — geometry probes measure rects, this one measures
 * truths. These are the falsifiable versions of the window-controlbox
 * spec smokes:
 *
 *   I1  a visible focused (main) pane always exists while panes do
 *   I2  clicking a shelved app's name UNSHELVES it (no 0x0 focused pane)
 *   I3  open/cascade mutations refresh the windows rail (winbox rows ==
 *       live panes of the current desktop after a reload)
 *   I4  the launcher names only routes that exist on this host
 *   I5  every <a> inside a fragment pane body is pane-local
 *       (hx-target="closest .fs-body"); zero plain shell-escaping anchors
 *   I6  a /code dir walk happens INSIDE the pane (same window throughout)
 *   I7  fresh panes stamp the operator's current desktop
 *
 *   AGENT_BIN=... node scripts/probe_shell_sem.mjs     (exit 0 = all OK)
 */
import { spawn } from "node:child_process";
import { setTimeout as sleep } from "node:timers/promises";
import http from "node:http";
import fs from "node:fs";
import path from "node:path";

process.on("SIGINT", () => process.exit(1));
const AGENT =
  process.env.AGENT_BIN ||
  (fs.existsSync("_build/default/agent/bin/main.exe")
    ? path.resolve("_build/default/agent/bin/main.exe")
    : path.resolve("_build/default/agent/bin/main.exe"));
const PORTC = 9046;
const CHROME = process.env.CHROME || "/usr/bin/chromium";
const USER_DATA = `/tmp/chrome-semprobe-${process.pid}`;
const AGENT_DIR = `/tmp/agent-semprobe-${process.pid}`;
const MOCK = path.join(AGENT_DIR, "mock.json");

let fails = 0;
function ok(cond, name, detail = "") {
  console.log(`${cond ? "OK  " : "FAIL"} ${name}${detail ? " : " + detail : ""}`);
  if (!cond) fails++;
}

function httpGet(url) {
  return new Promise((resolve, reject) => {
    http.get(url, (res) => {
      let d = ""; res.on("data", (c) => (d += c));
      res.on("end", () => resolve({ status: res.statusCode, body: d }));
    }).on("error", reject);
  });
}
function httpPost(url, data, hx) {
  return new Promise((resolve, reject) => {
    const r = http.request(url, { method: "POST", headers: {
      "content-type": "application/x-www-form-urlencoded",
      ...(hx ? { "HX-Request": "true" } : {}),
    }}, (res) => {
      let d = ""; res.on("data", (c) => (d += c));
      res.on("end", () => resolve({ status: res.statusCode, body: d }));
    });
    r.on("error", reject);
    r.end(new URLSearchParams(data).toString());
  });
}
const bytes = (ev) => Buffer.isBuffer(ev.data) ? ev.data.toString() : String(ev.data);

const chrome = spawn(CHROME, [
  `--remote-debugging-port=${PORTC}`, `--user-data-dir=${USER_DATA}`,
  "--headless=new", "--disable-gpu", "--no-sandbox", "--disable-dev-shm-usage",
  "--window-size=1880,1000", "--hide-scrollbars", "about:blank",
], { stdio: "ignore" });
let agent;
const cleanup = () => {
  try { agent?.kill("SIGTERM"); } catch (_) {}
  try { chrome.kill("SIGKILL"); } catch (_) {}
  try { fs.rmSync(USER_DATA, { recursive: true, force: true }); } catch (_) {}
  try { fs.rmSync(AGENT_DIR, { recursive: true, force: true }); } catch (_) {}
};
process.on("exit", cleanup);
process.on("SIGTERM", () => { cleanup(); process.exit(1); });

let ready = false;
for (let i = 0; i < 40; i++) {
  try { if ((await httpGet(`http://127.0.0.1:${PORTC}/json/version`)).status === 200) { ready = true; break; } } catch (_) {}
  await sleep(150);
}
if (!ready) { console.error("FAIL boot: chromium CDP not ready"); process.exit(1); }

// ---- scratch agent (own cwd; never touch repo .agent or live glochid) ----
fs.mkdirSync(path.join(AGENT_DIR, ".agent"), { recursive: true });
fs.writeFileSync(path.join(AGENT_DIR, ".agent", "workspace.json"),
  JSON.stringify({ axis: "col", desktop: 0, focus: "content:0", cascade: [], content: [
    { width: "third", route: "/chat", id: "p1", desktop: 0, state: "live" },
    { width: "half", route: "/settings", id: "p2", desktop: 0, state: "shelved" },
  ]}));
fs.writeFileSync(MOCK, JSON.stringify([{ text: "probe reply", calls: [] }]));

let base = null;
agent = spawn(AGENT, ["--serve", "--port", "0", "--mock-script", path.basename(MOCK)],
  { stdio: ["ignore", "pipe", "pipe"], cwd: AGENT_DIR });
agent.stderr.on("data", () => {});
const url = await new Promise((resolve, reject) => {
  const to = setTimeout(() => reject(new Error("FAIL boot: agent never announced")), 8000);
  agent.stdout.on("data", (c) => {
    const m = String(c).match(/http:\/\/127\.0\.0\.1:(\d+)\//);
    if (m) { clearTimeout(to); resolve(`http://127.0.0.1:${m[1]}`); }
  });
  agent.on("exit", (code) => { clearTimeout(to); reject(new Error("FAIL boot: agent exited code " + code)); });
});
base = url;
await sleep(400);

// ---- CDP plumbing ----
const list = JSON.parse((await httpGet(`http://127.0.0.1:${PORTC}/json/list`)).body);
const ws = new WebSocket(list.find((t) => t.type === "page").webSocketDebuggerUrl);
await new Promise((res, rej) => { ws.onopen = res; ws.onerror = rej; });
let nextId = 1;
const pending = new Map();
ws.onmessage = (ev) => { const m = JSON.parse(bytes(ev)); if (m.id && pending.has(m.id)) { pending.get(m.id)(m); pending.delete(m.id); } };
const send = (method, params = {}) => { const id = nextId++; ws.send(JSON.stringify({ id, method, params })); return new Promise((r) => pending.set(id, r)); };
async function ev(expression) {
  const r = await send("Runtime.evaluate", { expression, returnByValue: true, awaitPromise: true });
  if (r.result?.exceptionDetails) throw new Error(JSON.stringify(r.result.exceptionDetails));
  return r.result?.result?.value;
}
await send("Page.enable"); await send("Runtime.enable");

const SNAPSHOT = `(()=>{
  const vis=(e)=>{const r=e.getBoundingClientRect();return r.width>1&&r.height>1;};
  const panes=[...document.querySelectorAll('.fs-pane')];
  const focused=panes.find(p=>p.classList.contains('focused'));
  const live=panes.filter(p=>!p.classList.contains('shelved'));
  return {
    focusedRect: focused?(function(){const r=focused.getBoundingClientRect();return {w:Math.round(r.width),h:Math.round(r.height),q:vis(focused)};})():null,
    focusedShelved: focused?focused.classList.contains('shelved'):false,
    liveCount: live.length,
    shelvedCount: panes.length-live.length,
    winRows: document.querySelectorAll('.fs-win-row').length,
    shelfRows: document.querySelectorAll('.fs-shelf-row').length,
    bareAnchors: [...document.querySelectorAll('.fs-pane .fs-body a')].filter(a=>!a.getAttribute('hx-target')).length,
  };
})()`;

await send("Page.navigate", { url: base + "/" });
await sleep(1500);

console.log(`# probe_shell_sem @ ${base} (agent cwd ${AGENT_DIR})`);

// ---- I1/I3/I5 as loaded ----
const s0 = await ev(SNAPSHOT);
ok(s0.focusedRect && s0.focusedRect.q, "I1 visible focused main pane on load", JSON.stringify(s0.focusedRect));
ok(s0.shelfRows === 1 && s0.shelvedCount === 1, "shelf row == shelved count on load", `shelf=${s0.shelfRows} shelved=${s0.shelvedCount}`);
ok(s0.liveCount === 1 && s0.winRows === 1, "winbox rows == live panes on load", `win=${s0.winRows} live=${s0.liveCount}`);
ok(s0.bareAnchors === 0, "I5 zero shell-escaping anchors in pane bodies", `bare=${s0.bareAnchors}`);

// ---- I2 + focus discipline: click the shelved app's cascade name ----
const html = (await ev(`(()=>{window.__sem='pre'; const b=[...document.querySelectorAll('#fs-cascade .fs-entry')].find(e=>e.textContent.trim()==='settings'); b.click(); return 'clicked'})()`));
await sleep(900);
const s1 = await ev(SNAPSHOT);
ok(html === "clicked", "cascade click dispatched (settings, shelved)");
ok(s1.focusedRect && s1.focusedRect.q && !s1.focusedShelved, "I2 focused pane visible+live after unshelving click", JSON.stringify(s1.focusedRect));
ok(s1.sameWindow !== false && (await ev(`window.__sem==='pre'`)), "I2 same-window (no navigation)");
ok(s1.shelfRows === 0 && s1.shelvedCount === 0, "I2 shelf row removed in same response", `shelf=${s1.shelfRows}`);
const wsp = JSON.parse(fs.readFileSync(path.join(AGENT_DIR, ".agent", "workspace.json")));
ok(wsp.content.find((p) => p.id === "p2").state === "live", "I2 unshelve persisted to workspace row");

// ---- I3: open a new route via launcher POST, winbox refreshes ----
await send("Page.navigate", { url: base + "/" });
await sleep(1200);
const openResp = await httpPost(base + "/filmstrip/open", { route: "/sessions" }, true);
ok(/id="fs-winbox"/.test(openResp.body), "I3 /filmstrip/open response carries winbox OOB");
const s2 = await ev(SNAPSHOT);
ok(s2.winRows === 2 && s2.liveCount === 2, "I3 winbox rows == live panes after open", `win=${s2.winRows} live=${s2.liveCount}`);

// ---- I4: launcher honesty ----
const boot = await httpGet(base + "/");
ok(/placeholder="\/chat"/.test(boot.body), "I4 placeholder names a real host route (== /chat)");
const bad = await httpPost(base + "/filmstrip/open", { route: "/todos" }, true);
ok(/routes on this host:/.test(bad.body), "I4 no-route message lists real routes", bad.body.slice(0, 140));
ok(!/try \/todos/.test(bad.body), "I4 no dead /todos hint in the no-route message");

// ---- I7: fresh pane stamps current desktop ----
await ev(`(()=>{ fetchNothing = 1 })()`);
await httpPost(base + "/filmstrip/desktop", { d: "1" });
await httpPost(base + "/filmstrip/open", { route: "/code" });
const wsp2 = JSON.parse(fs.readFileSync(path.join(AGENT_DIR, ".agent", "workspace.json")));
const codePane = wsp2.content.find((p) => p.route === "/code");
ok(codePane && String(codePane.desktop) === "1", "I7 fresh pane stamps the current desktop", codePane ? String(codePane.desktop) : "missing");

// ---- I6 + I5: real click-through, dir walk inside the pane ----
fs.mkdirSync(path.join(AGENT_DIR, "deep"), { recursive: true });
fs.writeFileSync(path.join(AGENT_DIR, "deep", "leaf.txt"), "leaf\n");
await send("Page.navigate", { url: base + "/" });
await sleep(1200);
console.log(await ev(`(()=>{window.__sem2='s'; const pane=document.querySelector('.fs-pane.focused'); const row=[...pane.querySelectorAll('.fs-body a')].find(a=>a.textContent.trim()==='deep'); if(!row) return 'rows: '+[...pane.querySelectorAll('.fs-body a')].map(a=>a.textContent.trim()).join(','); if(row.getAttribute('hx-target')!=='closest .fs-body') return 'ROW NOT PANE-LOCAL'; row.click(); return 'walked'})()`));
await sleep(900);
console.log(await ev(`(()=>{const pane=document.querySelector('.fs-pane.focused'); const row=[...pane.querySelectorAll('.fs-body a')].find(a=>a.textContent.trim()==='leaf.txt'); if(!row) return 'no leaf'; row.click(); return 'leaf clicked'})()`));
await sleep(900);
const s3 = await ev(SNAPSHOT);
ok(await ev(`window.__sem2==='s'`), "I6 dir walk stayed in the same window");
ok(s3.focusedRect && s3.focusedRect.q, "I1 visible focused pane after pane-local walk", JSON.stringify(s3.focusedRect));
ok((await ev(`document.querySelector('.fs-pane.focused .fs-body').textContent.includes('leaf')`)), "I6 file content rendered inside the pane body");

console.log(fails === 0 ? "\nSEMANTIC PROBE: ALL GREEN" : `\nSEMANTIC PROBE: ${fails} FAILING`);
process.exit(fails === 0 ? 0 : 1);
