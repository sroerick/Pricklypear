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
 *   I1  a visible FOCUSED pane always exists while panes do — in the MAIN
 *       window (the lift, region=main) or in the rail (region=rail); with
 *       zero rail panes this is exactly the v2 "focused main pane" rule
 *   I2  clicking a shelved app's name UNSHELVES it (no 0x0 focused pane)
 *   I3  open/cascade mutations refresh the windows rail (winbox rows ==
 *       live panes of the current desktop after a reload)
 *   I4  the launcher names only routes that exist on this host
 *   I5  every <a> inside a fragment pane body is pane-local
 *       (hx-target="closest .fs-body"); zero plain shell-escaping anchors
 *   I6  a /code dir walk happens INSIDE the pane (same window throughout)
 *   I7  fresh panes stamp the operator's current desktop
 *   I8  (region, window-controlbox-v3) pinning a pane to the rail leaves
 *       its BODY visible in the rail while the focused main pane stays
 *       visible. RECORDED from v3 task 0 (snapshot carries pinnedCount +
 *       per-pane region + rail-body visibility), ASSERTED only from
 *       v3 task 2 onward — before the pin verb exists the assertions
 *       would be vacuous, so they are gated on I8_ARMED=1 and printed
 *       as SKIP.
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
  // Per-pane REGION readout (window-controlbox-v3): data-region attribute;
  // a MISSING attribute defaults to "main" (pre-v3 panes carry none).
  const region=(p)=>p.getAttribute('data-region')||'main';
  const panes=[...document.querySelectorAll('.fs-pane')];
  const focused=panes.find(p=>p.classList.contains('focused'));
  const live=panes.filter(p=>!p.classList.contains('shelved'));
  const rail=panes.filter(p=>region(p)==='rail'&&!p.classList.contains('shelved'));
  return {
    panes: panes.map(p=>({pane:p.getAttribute('data-pane')||'', route:((p.querySelector('.fs-title')||{}).textContent||'').trim(), region:region(p), state:p.classList.contains('shelved')?'shelved':'live', focused:p.classList.contains('focused')})),
    focusedRegion: focused?region(focused):null,
    focusedRect: focused?(function(){const r=focused.getBoundingClientRect();return {w:Math.round(r.width),h:Math.round(r.height),q:vis(focused)};})():null,
    focusedShelved: focused?focused.classList.contains('shelved'):false,
    pinnedCount: rail.length,
    railBodies: rail.map(p=>({pane:p.getAttribute('data-pane')||'', bodyVisible:vis(p.querySelector(':scope > .fs-body'))})),
    liveCount: live.length,
    shelvedCount: panes.length-live.length,
    winRows: document.querySelectorAll('.fs-win-row').length,
    shelfRows: document.querySelectorAll('.fs-shelf-row').length,
    bareAnchors: [...document.querySelectorAll('.fs-pane .fs-body a')].filter(a=>!a.getAttribute('hx-target')).length,
  };
})()`;

// ---- I8 (region invariants) — recorded from v3 task 0, ASSERTED from v3 task 2 ----
// "pinning a pane to the rail leaves its BODY visible in the rail while the
// focused main pane stays visible." Arming: task 2 shipped the pin verb, so I8
// is ASSERTED BY DEFAULT from v3 task 3 onward; export I8_ARMED=0 to record
// only (e.g. bisecting an older shell).
const I8_ARMED = process.env.I8_ARMED !== "0";
// expectPinned=false: nothing pinned YET (pre-pin load/records tags) — print
// the observation without asserting, so the armed gate still pins through the
// real verb before any I8 assertion is made.
async function checkI8(tag, expectPinned = true) {
  const s = await ev(SNAPSHOT);
  if (!I8_ARMED) {
    console.log(`SKIP I8 ${tag} (asserted from v3 task 2; I8_ARMED=1 to arm)`);
    console.log(`     I8 record pinned=${s.pinnedCount} railBodies=${JSON.stringify(s.railBodies)}`);
    return;
  }
  if (!expectPinned) {
    console.log(`I8 record ${tag} pinned=${s.pinnedCount} railBodies=${JSON.stringify(s.railBodies)} (nothing pinned yet)`);
    return;
  }
  ok(s.railBodies.length > 0 && s.railBodies.every((r) => r.bodyVisible), `I8 pinned rail pane body visible in the rail (${tag})`, JSON.stringify(s.railBodies));
  ok(s.focusedRect && s.focusedRect.q, `I8 focused pane stays visible alongside pinned rail body (${tag})`, JSON.stringify(s.focusedRect));
}

await send("Page.navigate", { url: base + "/" });
await sleep(1500);

console.log(`# probe_shell_sem @ ${base} (agent cwd ${AGENT_DIR})`);

// ---- I1/I3/I5 as loaded ----
const s0 = await ev(SNAPSHOT);
// I1 widened (v3): a FOCUSED pane exists, is visible, live, and its region
// is main (the lift) or rail (visible in the rail). With zero pinned panes
// this must reduce to exactly the v2 rule — assert that too.
ok(s0.focusedRect && s0.focusedRect.q && !s0.focusedShelved && (s0.focusedRegion === "main" || s0.focusedRegion === "rail"), "I1 visible focused pane on load (main lift or rail)", JSON.stringify({rect: s0.focusedRect, region: s0.focusedRegion}));
ok(s0.pinnedCount > 0 || s0.focusedRegion === "main", "I1 backward-compat: with zero pinned panes the focused pane is the main lift", `pinned=${s0.pinnedCount} region=${s0.focusedRegion}`);
await checkI8("on load", false);
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
ok(s3.focusedRect && s3.focusedRect.q && (s3.focusedRegion === "main" || s3.focusedRegion === "rail"), "I1 visible focused pane after pane-local walk (main lift or rail)", JSON.stringify({rect: s3.focusedRect, region: s3.focusedRegion}));
ok((await ev(`document.querySelector('.fs-pane.focused .fs-body').textContent.includes('leaf')`)), "I6 file content rendered inside the pane body");

// ---- I8 ARMED (v3 task 2): pin/unpin through the real verbs ----
// Desktop 1 holds the focused /code pane (main lift). Flip to desktop 0
// via the real deskbox button (htmx applies the OOB swap DOM-side; focus
// heals to the first live desk-0 pane, a MAIN lift), then pin p2
// (/settings, live desk 0) — an UNFOCUSED pane — with htmx.ajax (the real
// request machinery: response OOB swaps land in the DOM) so the focused
// main pane stays visible BESIDE the pinned rail body: I8's full shape.
await ev(`(()=>{const b=[...document.querySelectorAll('#fs-deskbox button')].find(x=>x.getAttribute('hx-vals')==='{\\"d\\":\\"0\\"}'); b.click(); return 'flipped'})()`);
await sleep(900);
ok(await ev(`document.querySelector('#ws-desk').classList.contains('dsk-0')`), "I8 setup: desktop 0 active after the deskbox click");
await ev(`htmx.ajax('POST','/filmstrip/pin',{values:{id:'p2'},target:'#fs-sink',swap:'innerHTML'})`);
await sleep(600);
const s4 = await ev(SNAPSHOT);
ok(s4.pinnedCount === 1 && s4.focusedRegion === "main", "I8 pin leaves the focused pane on the main lift", JSON.stringify({pinned: s4.pinnedCount, region: s4.focusedRegion}));
ok(s4.railBodies[0] && s4.railBodies[0].pane === "p2", "I8 the pinned pane is p2", JSON.stringify(s4.railBodies));
ok((await ev(`document.querySelector('.fs-pane[data-pane="p2"]').getAttribute('data-region')`)) === "rail", "I8 pinned section carries data-region=rail in the DOM");
ok((await ev(`[...document.querySelectorAll('.fs-win-row')].some(r=>r.textContent.includes('unpin'))`)), "I8 winbox row re-labeled to unpin (winbox OOB rode the pin response)");
ok((await ev(`[...document.querySelectorAll('.fs-pane[data-pane="p2"] header button')].some(b=>b.textContent.trim()==='unpin')`)), "I8 pane-head pin word re-labeled to unpin");
await checkI8("after pin");
const wsp3 = JSON.parse(fs.readFileSync(path.join(AGENT_DIR, ".agent", "workspace.json")));
ok(wsp3.content.find((p) => p.id === "p2").region === "rail", "I8 pin persisted to workspace row (region=rail)");

// unpin returns the pane to the strip
await ev(`htmx.ajax('POST','/filmstrip/pin',{values:{id:'p2'},target:'#fs-sink',swap:'innerHTML'})`);
await sleep(600);
const s5 = await ev(SNAPSHOT);
ok(s5.pinnedCount === 0 && s5.focusedRegion === "main", "unpin returns the pane to the strip (zero pinned, focused main lift restored)", JSON.stringify({pinned: s5.pinnedCount, region: s5.focusedRegion}));
ok((await ev(`document.querySelector('.fs-pane[data-pane="p2"]').getAttribute('data-region')`)) === "main", "I8 unpinned section carries data-region=main in the DOM");
ok((await ev(`[...document.querySelectorAll('.fs-win-row')].some(r=>r.textContent.includes('unpin'))`)) === false, "I8 winbox row re-labeled back to pin (unpin OOB rode the response)");
ok(s5.focusedRect && s5.focusedRect.q, "I1 still visible-focused after pin/unpin churn", JSON.stringify(s5.focusedRect));
const wsp4 = JSON.parse(fs.readFileSync(path.join(AGENT_DIR, ".agent", "workspace.json")));
ok(wsp4.content.find((p) => p.id === "p2").region === "main", "unpin persisted to workspace row (region=main)");

// ---- v3 task 3: focus/open discipline in both regions ----
// Re-pin p2 and click ITS winbox focus row: the rail pane must take the ring
// IN PLACE (body visible in the rail) while the focused main pane (p1) keeps
// its lift — main pane unaffected, persisted focus stays on the main pane.
await ev(`htmx.ajax('POST','/filmstrip/pin',{values:{id:'p2'},target:'#fs-sink',swap:'innerHTML'})`);
await sleep(600);
console.log(await ev(`(()=>{const row=[...document.querySelectorAll('.fs-win-row')].find(r=>/settings/.test(r.textContent||'')); const b=[...row.querySelectorAll('button')].find(b=>b.textContent.trim()==='focus'); b.click(); return 'focus clicked'})()`));
await sleep(600);
const s6 = await ev(SNAPSHOT);
ok((await ev(`document.querySelector('.fs-pane[data-pane="p1"]').classList.contains('focused')`)), "rail focus: p1 keeps .focused (main lift intact)");
ok((await ev(`document.querySelector('.fs-pane[data-pane="p2"]').classList.contains('focused')`)), "rail focus: winbox focus gives p2 the ring in place");
ok(s6.focusedRegion === "main", "rail focus: the lift region is still main", JSON.stringify({region: s6.focusedRegion}));
ok(s6.railBodies[0] && s6.railBodies[0].pane === "p2" && s6.railBodies[0].bodyVisible, "rail focus: pinned p2 body still visible in the rail", JSON.stringify(s6.railBodies));
const wsp5 = JSON.parse(fs.readFileSync(path.join(AGENT_DIR, ".agent", "workspace.json")));
ok(wsp5.focus === "content:0", "rail focus: persisted focus stays on the lifted main pane (no rail steal)", JSON.stringify(wsp5.focus));
ok(s6.focusedRect && s6.focusedRect.q, "I1 visible focused pane after rail focus", JSON.stringify(s6.focusedRect));

// open-or-focus of the pinned route via a real cascade leaf click: highlight
// in place, main pane unaffected (no lift steal; workspace focus unmoved).
after_reload_tag: {
  await send("Page.navigate", { url: base + "/" });
  await sleep(1200);
  // the rail ring is live-DOM state: after reload only the PERSISTED focus
  // (p1) wears the ring, so the cascade open must apply it afresh.
  ok(!(await ev(`document.querySelector('.fs-pane[data-pane="p2"]').classList.contains('focused')`)), "cascade setup: p2 ring is live-DOM state, gone after reload");
  console.log(await ev(`(()=>{const b=[...document.querySelectorAll('#fs-cascade .fs-entry')].find(e=>e.textContent.trim()==='settings'); b.click(); return 'clicked'})()`));
  await sleep(900);
  const s7 = await ev(SNAPSHOT);
  ok((await ev(`document.querySelector('.fs-pane[data-pane="p2"]').classList.contains('focused')`)), "open-or-focus of a pinned route highlights the rail pane in place");
  ok((await ev(`document.querySelector('.fs-pane[data-pane="p1"]').classList.contains('focused')`)), "open-or-focus of a pinned route leaves the main lift untouched");
  ok(s7.focusedRegion === "main" && s7.focusedRect && s7.focusedRect.q, "I1 main lift still valid after pinned-route open", JSON.stringify({region: s7.focusedRegion, rect: s7.focusedRect}));
  ok(s7.railBodies[0] && s7.railBodies[0].bodyVisible, "pinned p2 body still visible after open-or-focus", JSON.stringify(s7.railBodies));
  const wsp6 = JSON.parse(fs.readFileSync(path.join(AGENT_DIR, ".agent", "workspace.json")));
  ok(wsp6.focus === "content:0", "pinned-route open did not move the persisted focus", JSON.stringify(wsp6.focus));
}

// Healing considers rail panes: shelve every MAIN pane (real winbox close
// clicks — the second close heals focus onto the pinned pane), reload: the
// focused RAIL pane satisfies I1 (no forced main lift), body visible.
after_reload_tag2: {
  const closePane = (route) => `(()=>{const row=[...document.querySelectorAll('.fs-win-row')].find(r=>r.textContent&&r.textContent.includes('${route}')); if(!row) return 'no row'; const b=[...row.querySelectorAll('button')].find(b=>b.textContent.trim()==='close'); b.click(); return 'closed'})()`;
  console.log(await ev(closePane("/sessions")));
  await sleep(600);
  console.log(await ev(closePane("/chat")));
  await sleep(600);
  await send("Page.navigate", { url: base + "/" });
  await sleep(1200);
  const s8 = await ev(SNAPSHOT);
  ok(s8.focusedRegion === "rail" && s8.focusedRect && s8.focusedRect.q && !s8.focusedShelved, "I1 rail-only heal: the focused rail pane satisfies I1 (no forced main lift)", JSON.stringify({region: s8.focusedRegion, rect: s8.focusedRect, shelved: s8.focusedShelved}));
  ok(s8.railBodies[0] && s8.railBodies[0].pane === "p2" && s8.railBodies[0].bodyVisible, "rail-only heal: pinned p2 body still visible", JSON.stringify(s8.railBodies));
  // restore the shelved mains so the world stays sane for later runs
  console.log(await ev(`(()=>{const row=[...document.querySelectorAll('.fs-shelf-row')].find(r=>r.textContent&&r.textContent.includes('/chat')); const b=[...row.querySelectorAll('button')].find(b=>b.textContent.trim()==='restore'); b.click(); return 'restored'})()`));
  await sleep(600);
  const s9 = await ev(SNAPSHOT);
  ok(s9.focusedRect && s9.focusedRect.q && !s9.focusedShelved, "I1 still green after restoring a main pane beside a focused rail pane", JSON.stringify(s9.focusedRect));
}

console.log(fails === 0 ? "\nSEMANTIC PROBE: ALL GREEN" : `\nSEMANTIC PROBE: ${fails} FAILING`);
process.exit(fails === 0 ? 0 : 1);
