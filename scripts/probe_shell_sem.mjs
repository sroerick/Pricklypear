#!/usr/bin/env node
/**
 * probe_shell_sem.mjs — SEMANTIC shell probe for the glochid/filmstrip
 * shell (companion to the read-only geometry probe_shell_wm.mjs and the
 * chat-surface smoke_agent_cdp.mjs; UI review 2026-09, batch B + C).
 *
 * Boots its own scratch glochid (own agent cwd, mock LLM, random port,
 * seeded workspace) and asserts the shell's behavioral INVARIANTS under
 * real mutations — geometry probes measure rects, this one measures
 * truths. Falsifiable versions of the window-controlbox spec smokes:
 *
 *   I1  a visible FOCUSED pane always exists while panes do — a strip
 *       column on stage (region=main) or in the rail (region=rail)
 *   I2  clicking a shelved app's name UNSHELVES it (no 0x0 focused pane)
 *   I3  open/cascade mutations refresh the windows rail (the windows-tree
 *       OOB rides every mutation response)
 *   D   sidebar/main dichotomy (filmstrip.borg sidebar-main-dichotomy):
 *       cascade column 0 is all route leaves; no launcher, no desktop
 *       controlbox, no pane-tag strip in the col rail; the axis toggle
 *       lives on the rail tabs row
 *  S1..S4 (main-strip-v5): the main region IS the niri strip — live
 *       main panes of the current desktop render SIMULTANEOUSLY as
 *       columns of one overflow row; focus is on-stage with neighbors
 *       peeking (no lift, no position:fixed); the one-up verb stages a
 *       pane alone (workspace field, round-trips); a region flip MOVES
 *       the section between #fs-content and #fs-rail via the surface
 *       OOB; close heals to the adjacent column; zero JS.
 *   I5  every <a> inside a fragment pane body is pane-local
 *   I6  a /code dir walk happens INSIDE the pane (same window throughout)
 *   I7  fresh panes stamp the operator's current desktop
 *   I8  (region, v3) pinning a pane to the rail leaves its body visible
 *       in the rail beside the focused main pane
 *  T1..T6 (v4 rail-trees): ONE tree renderer, N roots — mode tabs swap
 *       visible root only, expansion state survives flips (DOM-pure),
 *       lazy levels fetch exactly once (re-fold never refetches), label =
 *       open-or-focus (acme Get), verbs POST without navigation,
 *       collapse-all restores head state with ONE POST, shelve moves the
 *       row under the shelf subtree on OOB, zero inline styles.
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
  path.resolve("_build/default/agent/bin/main.exe");
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
const bytes = (evv) => Buffer.isBuffer(evv.data) ? evv.data.toString() : String(evv.data);

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
ws.onmessage = (evv) => { const m = JSON.parse(bytes(evv)); if (m.id && pending.has(m.id)) { pending.get(m.id)(m); pending.delete(m.id); } };
const send = (method, params = {}) => { const id = nextId++; ws.send(JSON.stringify({ id, method, params })); return new Promise((r) => pending.set(id, r)); };
async function ev(expression) {
  const r = await send("Runtime.evaluate", { expression, returnByValue: true, awaitPromise: true });
  if (r.result?.exceptionDetails) throw new Error(JSON.stringify(r.result.exceptionDetails));
  return r.result?.result?.value;
}
await send("Page.enable"); await send("Runtime.enable");

const SNAPSHOT = `(()=>{
  const vis=(e)=>{const r=e.getBoundingClientRect();return r.width>1&&r.height>1;};
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
    trees: document.querySelectorAll('.fs-rmode').length,
    railtabs: document.querySelectorAll('#fs-railtabs button').length,
    railmode: (document.querySelector('#fs-railmode')||{className:''}).className,
    shelfDir: !!document.querySelector('#fs-tree-windows li[data-node="shelf"] > details'),
    bareAnchors: [...document.querySelectorAll('.fs-pane .fs-body a')].filter(a=>!a.getAttribute('hx-target')).length,
  };
})()`;


// Wait for the shell DOM after a navigate (boot renders are lazy-budgeted;
// a fixed 1200/1500ms raced under heavier boot renders).
async function waitShell(ms = 10000) {
  const t0 = Date.now();
  while (Date.now() - t0 < ms) {
    try {
      if (await ev(`document.querySelectorAll('.fs-pane').length > 0 && !!document.querySelector('#fs-railtabs')`)) return true;
    } catch (_) {}
    await sleep(300);
  }
  console.log("     DEBUG waitShell: location=" + await ev(`location.href`) + " title=" + await ev(`document.title`) + " bodyLen=" + await ev(`document.body?document.body.innerHTML.length:-1`) + " body=" + await ev(`document.body?document.body.innerHTML.slice(0,120):""`) + " panes=" + await ev(`document.querySelectorAll('.fs-pane').length`));
  return false;
}

// ---- v4 TREE HELPERS (rail-trees-v4) ----
// The winbox/shelf cboxes are gone: the windows TREE (targeting) carries
// the rows lazily under desk/<n> and shelf dirs. Expand via a real
// summary click (htmx toggle fires the one-time level GET); find rows by
// their open-word text, preferring VISIBLE rows (stale collapsed levels
// persist in the DOM by design — stale is fine, hidden is the tell).
const treeExpand = (node) => `(()=>{const d=document.querySelector('#fs-tree-windows li[data-node="${node}"] > details'); if(!d) return 'no node ${node}'; if(d.open) return 'open already'; d.querySelector('summary').click(); return 'opening ${node}'})()`;
const treeRowExpr = (route) => `[...document.querySelectorAll('#fs-tree-windows li.fs-tr-item')].find(li=>{const b=li.querySelector(':scope > details > summary .fs-tr-open'); return b && b.textContent.trim()==='${route}' && li.getClientRects().length>0;})`;
const treeOpenClick = (route) => `(()=>{const row=${treeRowExpr(route)}; if(!row) return 'no row ${route}'; const b=row.querySelector(':scope > details > summary .fs-tr-open'); if(!b) return 'no open word on ${route}'; const vals=b.getAttribute('hx-vals')||'{}'; htmx.ajax('POST','/filmstrip/open',{values:JSON.parse(vals),target:'#fs-sink',swap:'innerHTML'}); return 'opened ${route}'})()`;
const treeVerbClick = (route, verb) => `(()=>{const row=${treeRowExpr(route)}; if(!row) return 'no row ${route}'; const b=[...row.querySelectorAll(':scope > details > summary .fs-tr-verb')].find(b=>b.getAttribute('data-verb')==='${verb}'); if(!b) return 'no ${verb} word on ${route}'; htmx.ajax('POST', b.getAttribute('hx-post'), {values: JSON.parse(b.getAttribute('hx-vals')), target:'#fs-sink', swap:'innerHTML'}); return '${verb} posted on ${route} via ' + b.getAttribute('hx-post')})()`;
// action buttons are GLYPHS now: select by data-verb, never by text.
const treeVerbPresent = (verb, route) => `[...document.querySelectorAll('#fs-tree-windows li.fs-tr-item .fs-tr-verb')].some(b=>b.getAttribute('data-verb')==='${verb}' && b.getAttribute('hx-post')==='${route}')`;
const paneHeadVerb = (pane, verb) => `[...document.querySelectorAll('.fs-pane[data-pane="${pane}"] header button')].some(b=>b.getAttribute('data-verb')==='${verb}')`;
// expand + wait for the spliced level
async function ensureNode(node) {
  const r = await ev(treeExpand(node));
  if (!String(r).includes("opening") && !String(r).includes("already")) console.log(`     tree expand ${node}: ${r}`);
  await sleep(700);
  return r;
}
// close a pane by ROUTE through its tree row's close verb; the box resets
// to head+level0 on the OOB, so stale levels may hide the row — re-expand
// both desk levels then re-find.
async function treeClose(route) {
  let r = await ev(treeVerbClick(route, "close"));
  if (String(r).includes("no row")) {
    await ensureNode("desk/0"); await ensureNode("desk/1");
    r = await ev(treeVerbClick(route, "close"));
  }
  return r;
}

const I8_ARMED = process.env.I8_ARMED !== "0";
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
ok(await waitShell(), "shell appeared after load");
await sleep(500);

console.log(`# probe_shell_sem @ ${base} (agent cwd ${AGENT_DIR})`);

// ---- I1/I3/I5 as loaded ----
const s0 = await ev(SNAPSHOT);
ok(s0.focusedRect && s0.focusedRect.q && !s0.focusedShelved && (s0.focusedRegion === "main" || s0.focusedRegion === "rail"), "I1 visible focused pane on load (main lift or rail)", JSON.stringify({rect: s0.focusedRect, region: s0.focusedRegion}));
ok(s0.pinnedCount > 0 || s0.focusedRegion === "main", "I1 backward-compat: with zero pinned panes the focused pane is the main lift", `pinned=${s0.pinnedCount} region=${s0.focusedRegion}`);
await checkI8("on load", false);
ok(s0.shelvedCount === 1 && s0.shelfDir, "windows tree: shelf subtree present with the shelved pane counted", `shelved=${s0.shelvedCount} shelfDir=${s0.shelfDir}`);
ok(s0.liveCount === 1 && s0.trees === 4 && s0.railtabs === 13, "v4+ habitat-panes: FOUR tree boxes (windows/code/prims/ui) + tabs + NINE start-menu entries (six glochid surfaces + three habitat faces)", `trees=${s0.trees} tabs=${s0.railtabs} live=${s0.liveCount}`);
ok(s0.bareAnchors === 0, "I5 zero shell-escaping anchors in pane bodies", `bare=${s0.bareAnchors}`);
ok((await ev(`[...document.querySelectorAll('#fs-shell [style]')].length`)) === 0, "T6 zero inline styles in the shell at rest (classes only, before any htmx settling)");
const __styleEls = await ev("[...document.querySelectorAll('#fs-shell [style]')].map(function(e){return e.tagName}).join(',')");
console.log("     DEBUG style els:", __styleEls);

// ---- I2 + focus discipline: click the shelved app's cascade name ----
const clicked = await ev(`(()=>{window.__sem='pre'; const b=[...document.querySelectorAll('#fs-start .fs-start-entry')].find(e=>e.textContent.trim()==='settings'); b.click(); return 'clicked'})()`);
await sleep(900);
const s1 = await ev(SNAPSHOT);
ok(clicked === "clicked", "start-menu entry dispatched (settings, shelved)");
ok(s1.focusedRect && s1.focusedRect.q && !s1.focusedShelved, "I2 focused pane visible+live after unshelving click", JSON.stringify(s1.focusedRect));
ok((await ev(`window.__sem==='pre'`)), "I2 same-window (no navigation)");
ok(s1.shelvedCount === 0, "I2 shelved count zero after the unshelve click", `shelved=${s1.shelvedCount}`);
const wsp = JSON.parse(fs.readFileSync(path.join(AGENT_DIR, ".agent", "workspace.json")));
ok(wsp.content.find((p) => p.id === "p2").state === "live", "I2 unshelve persisted to workspace row");

// ---- I3: open a new route via launcher POST, windows rail refreshes ----
await send("Page.navigate", { url: base + "/" });
ok(await waitShell(), "shell appeared after load");
await sleep(500);
const openResp = await httpPost(base + "/filmstrip/open", { route: "/sessions" }, true);
ok(/id="fs-tree-windows-root"/.test(openResp.body), "I3 /filmstrip/open response carries the windows-tree rows OOB (ul innerHTML seam)");
// drive the SAME open through the page's htmx so the OOB actually lands
// (an eager desk carries its kid rows in the boot render -- it never
// re-fetches its level, so the side-channel POST would never show up)
await ev(`htmx.ajax('POST','/filmstrip/open',{values:{route:'/sessions'},target:'#fs-sink',swap:'innerHTML'})`);
await sleep(700);
const s2 = await ev(SNAPSHOT);
ok(s2.liveCount === 2, "I3 live panes after open", `live=${s2.liveCount}`);
await ensureNode("desk/0");
ok((await ev(`[...document.querySelectorAll('#fs-tree-windows li.fs-tr-item .fs-tr-open')].filter(b=>['/chat','/sessions'].includes(b.textContent.trim())).length`)) === 2, "I3 windows tree lists the live panes under desk/0 after the open OOB");

// ---- M: cross-desktop move (window-controlbox desktop int, move verb) ----
// The desk dirs render DEFAULT-OPEN (expand seed rides eager kids: a
// parsed-open details never fires its toggle-once pull), so the pane rows
// under desk/0 are already in the DOM without any expand step.
ok(await ev(`(function(){const ds=[...document.querySelectorAll('#fs-tree-windows li[data-node^="desk"] > details')]; return ds.length===3 && ds.every(d=>d.open);})()`), "M0 the three desk dirs render DEFAULT-OPEN (expand seed)");
ok(await ev(`[...document.querySelectorAll('#fs-tree-windows li[data-node^="desk"] > details')].every(d=>d.getAttribute('hx-get')==null)`), "M0b default-open dirs carry eager kids (no lazy pull attrs)");
// move /sessions to desktop 1 through its tree row's explicit to1 word
// (the row also carries to2; the current desktop has no self-move word)
let mv = await ev(treeVerbClick('/sessions','to1'));
ok(String(mv).includes('to1 posted'), "M1 the tree row carries explicit to-verbs per other desktop", mv);
await sleep(300);
// the OOB swap (tree rows + whole strip) must settle before the DOM reads
let __m2b = false;
for (let t = 0; t < 15 && !__m2b; t++) { await sleep(300); __m2b = await ev(`[...document.querySelectorAll('#fs-content .fs-pane')].some(p=>String(p.getAttribute('data-desktop'))==='1' && (p.querySelector('.fs-title')||{}).textContent.trim()==='/sessions')`); }
ok(__m2b, "M2b the moved pane's section re-rendered with data-desktop=1");
const wspM = JSON.parse(fs.readFileSync(path.join(AGENT_DIR, ".agent", "workspace.json")));
ok(wspM.content.find((p) => p.route === "/sessions" && p.state !== "shelved").desktop === 1, "M2 move-to persisted desktop=1 on the pane record");
ok((await ev(`[...document.querySelectorAll('#fs-content .fs-pane')].some(p=>String(p.getAttribute('data-desktop'))==='1' && (p.querySelector('.fs-title')||{}).textContent.trim()==='/sessions')`)), "M2b the moved pane's section re-rendered with data-desktop=1");
const sm1 = await ev(SNAPSHOT);
ok(!(sm1.panes.find((p)=>p.route==='/sessions')||{focused:false}).focused, "M3 the moved-away pane never keeps the ring", JSON.stringify(sm1.panes));
ok(sm1.focusedRect && sm1.focusedRect.q && !sm1.focusedShelved, "I1 focus healed onto a visible pane after the leave");
ok(await ev(`[...document.querySelectorAll('#fs-content .fs-pane:not(.shelved) header button')].some(b=>b.getAttribute('data-verb')==='move' && b.getAttribute('hx-post')==='/filmstrip/move-to')`), "M4 the pane head carries the move glyph POSTing move-to");

// ---- S: the main region IS the niri strip (main-strip-v5) ----
ok((await ev(`[...document.querySelectorAll('#fs-content .fs-pane')].filter(p=>(p.getAttribute('data-region')||'main')==='main'&&!p.classList.contains('shelved')&&String(p.getAttribute('data-desktop'))==='0').every(p=>p.getClientRects().length>0)`)), "S1 live main panes of the current desktop render as strip columns", ``);
ok((await ev(`[...document.querySelectorAll('#fs-content .fs-pane')].filter(p=>(p.getAttribute('data-region')||'main')==='main'&&!p.classList.contains('shelved')&&String(p.getAttribute('data-desktop'))==='0').length`)) >= 2, "S2 more than one reading view on stage at once (the 2026-09-14 review's ask)");
ok((await ev(`getComputedStyle(document.querySelector('.fs-pane[data-region="main"]')).position`)) !== "fixed", "S3 no lifted pane: no position:fixed main pane (geometry from classes)");
ok((await ev(`document.querySelectorAll('#fs-surface').length===1 && !!document.querySelector('#fs-surface #fs-content') && !!document.querySelector('#fs-surface #fs-rail')`)), "S4 surface wrapper hosts both containers (the region-move OOB target)");

// ---- v5 one-up: stage a pane alone, verify field + class, unstage ----
await ev(`htmx.ajax('POST','/filmstrip/single',{values:{id:'p1'},target:'#fs-sink',swap:'innerHTML'})`);
await sleep(700);
ok((await ev(`document.querySelector('.fs-pane[data-pane="p1"]').classList.contains('fs-single')`)), "v5 one-up: the staged pane wears fs-single");
ok((await ev(`[...document.querySelectorAll('#fs-content .fs-pane:not(.fs-single)')].filter(p=>(p.getAttribute('data-region')||'main')==='main'&&!p.classList.contains('shelved')&&String(p.getAttribute('data-desktop'))==='0'&&p.getClientRects().length>0).length`)) === 0, "v5 one-up: staged alone (strip siblings step aside)");
ok((await ev(`document.querySelector('.fs-pane[data-pane="p1"] header button[data-verb="single"]')`)) !== null, "v5 one-up: the pane head carries the single glyph");
const wsSingle = JSON.parse(fs.readFileSync(path.join(AGENT_DIR, ".agent", "workspace.json")));
ok(wsSingle.content.find((p) => p.id === "p1").single === true, "v5 one-up persisted to workspace row (single)");
await ev(`htmx.ajax('POST','/filmstrip/single',{values:{id:'p1'},target:'#fs-sink',swap:'innerHTML'})`);
await sleep(700);
ok((await ev(`!document.querySelector('.fs-pane[data-pane="p1"]').classList.contains('fs-single')`)), "v5 one-off: unstaging returns the pane to the strip");

// ---- D: sidebar/main dichotomy ----
const boot = await httpGet(base + "/");
ok(!/id="fs-launch"/.test(boot.body), "D no launcher in the shell");
ok(!/fs-deskbox/.test(boot.body), "D no desktop controlbox in the shell");
ok((await ev(`[...document.querySelectorAll('#fs-start .fs-start-entry')].every(e=>JSON.parse(e.getAttribute('hx-vals')).route.startsWith('/'))`)), "D start-menu entries are all route leaves");
ok((await ev(`!document.querySelector('#fs-cascade')`)), "D the launchbar/cascade column is retired (start menu lives on the rail)");
ok((await ev(`!document.querySelector('#fs-railtabs #fs-axis-btn')`)), "D the axis toggle is retired (sidebar-only shell, BORGE-1789879934)");
ok(/id="fs-railtabs"/.test(boot.body) && !/fs-pane .fs-title.*settings.*pin/.test(boot.body.split('#fs-content')[1] || ""), "D no pane-tag strip inventory in the rail (tree is the pane inventory)");

// ---- I7: fresh pane stamps current desktop ----
await httpPost(base + "/filmstrip/desktop", { d: "1" });
await httpPost(base + "/filmstrip/open", { route: "/code" });
const wsp2 = JSON.parse(fs.readFileSync(path.join(AGENT_DIR, ".agent", "workspace.json")));
const codePane = wsp2.content.find((p) => p.route === "/code");
ok(codePane && String(codePane.desktop) === "1", "I7 fresh pane stamps the current desktop", codePane ? String(codePane.desktop) : "missing");

// ---- I6 + I5: real click-through, the SAME outline template as the
// rail (rail-trees-v4 amendment): dir labels fold in place (eager small
// dirs pull NOTHING), leaf labels open-or-focus via acme Get. ----
fs.mkdirSync(path.join(AGENT_DIR, "deep"), { recursive: true });
fs.writeFileSync(path.join(AGENT_DIR, "deep", "leaf.txt"), "leaf\n");
await send("Page.navigate", { url: base + "/" });
ok(await waitShell(), "shell appeared after load");
await sleep(500);
console.log(await ev(`(()=>{window.__sem2='s'; const pane=document.querySelector('.fs-pane.focused'); const d=pane.querySelector('.fs-body li[data-node="deep"] > details'); if(!d) return 'no deep row: '+[...pane.querySelectorAll('.fs-body li.fs-tr-item')].map(li=>li.getAttribute('data-node')).join(','); if(!d.open) d.querySelector('summary').click(); return 'deep folded open'})()`));
await sleep(600);
ok((await ev(`document.querySelector('.fs-pane.focused .fs-body li[data-node="deep"] > details').getAttribute('hx-get')`)) === null, "I6 the small deep dir rendered EAGER (no toggle-once pull -- zero XHRs below the threshold)");
console.log(await ev(`(()=>{const b=[...document.querySelectorAll('.fs-pane.focused .fs-body li[data-node="deep/leaf.txt"] > details > summary .fs-tr-open')].find(x=>x.getClientRects().length>0); if(!b) return 'no leaf label'; b.click(); return 'leaf opened-or-focused (acme Get)'})()`));
await sleep(900);
const s3 = await ev(SNAPSHOT);
ok(await ev(`window.__sem2==='s'`), "I6 tree walk stayed in the same window");
ok(s3.focusedRect && s3.focusedRect.q && (s3.focusedRegion === "main" || s3.focusedRegion === "rail"), "I1 visible focused pane after the tree walk (main lift or rail)", JSON.stringify({rect: s3.focusedRect, region: s3.focusedRegion}));
ok((await ev(`document.querySelector('.fs-pane.focused .fs-body').textContent.includes('leaf')`)), "I6 the leaf label's acme Get focused the file pane (content in the body)");

// ---- I8 ARMED (v3): pin/unpin through the real verbs ----
after_reload_tag: {
  await ev(`htmx.ajax('POST','/filmstrip/desktop',{values:{d:'0'},target:'#fs-sink',swap:'innerHTML'})`);
  await sleep(900);
  ok(await ev(`document.querySelector('#ws-desk').classList.contains('dsk-0')`), "I8 setup: desktop 0 active after the desktop POST");
  await ev(`htmx.ajax('POST','/filmstrip/pin',{values:{id:'p2'},target:'#fs-sink',swap:'innerHTML'})`);
  await sleep(600);
  const s4 = await ev(SNAPSHOT);
  ok(s4.pinnedCount === 1 && s4.focusedRegion === "main", "I8 pin leaves the focused pane on the strip's focused column", JSON.stringify({pinned: s4.pinnedCount, region: s4.focusedRegion}));
  ok(s4.railBodies[0] && s4.railBodies[0].pane === "p2", "I8 the pinned pane is p2", JSON.stringify(s4.railBodies));
  ok((await ev(`document.querySelector('.fs-pane[data-pane="p2"]').getAttribute('data-region')`)) === "rail", "I8 pinned section carries data-region=rail in the DOM");
  // v5: a region flip MOVES the section between containers — the pane
  // section must be inside #fs-rail now (the surface OOB did its job).
  ok((await ev(`!!document.querySelector('.fs-pane[data-pane="p2"]').closest('#fs-rail')`)), "v5 region move: the pinned section physically lives inside #fs-rail");
  ok((await ev(`!!document.querySelector('.fs-pane[data-pane="p1"]').closest('#fs-content')`)), "v5 region move: main panes still in #fs-content after the flip");
  // the pin OOB re-rendered the box back to head+level0 -> re-expand, then
  // the /settings row should carry the UNPIN verb (region=rail).
  await ensureNode("desk/0");
  ok((await ev(treeVerbPresent('unpin', '/filmstrip/pin'))), "I8 tree row re-labeled to unpin (windows-tree rows OOB rode the pin response)");
  ok((await ev(paneHeadVerb('p2', 'unpin'))), "I8 pane-head pin glyph re-labeled to unpin");
  await checkI8("after pin");
  ok((await ev(`Math.round(document.querySelector('.fs-pane[data-pane="p2"]').getBoundingClientRect().height)`)) > 256, "cascade cap released: the single pinned pane fills the rail band (> 16rem)");
  const wsp3 = JSON.parse(fs.readFileSync(path.join(AGENT_DIR, ".agent", "workspace.json")));
  ok(wsp3.content.find((p) => p.id === "p2").region === "rail", "I8 pin persisted to workspace row (region=rail)");

  // unpin returns the pane to the strip
  await ev(`htmx.ajax('POST','/filmstrip/pin',{values:{id:'p2'},target:'#fs-sink',swap:'innerHTML'})`);
  await sleep(600);
  const s5 = await ev(SNAPSHOT);
  ok(s5.pinnedCount === 0 && s5.focusedRegion === "main", "unpin returns the pane to the strip (zero pinned, focused strip column unchanged)", JSON.stringify({pinned: s5.pinnedCount, region: s5.focusedRegion}));
  ok((await ev(`document.querySelector('.fs-pane[data-pane="p2"]').getAttribute('data-region')`)) === "main", "I8 unpinned section carries data-region=main in the DOM");
  ok((await ev(`!!document.querySelector('.fs-pane[data-pane="p2"]').closest('#fs-content')`)), "v5 region move back: the unpinned section physically lives inside #fs-content");
  await ensureNode("desk/0");
  ok((await ev(`[...document.querySelectorAll('#fs-tree-windows li.fs-tr-item .fs-tr-verb')].some(b=>b.getAttribute('data-verb')==='unpin')`)) === false, "I8 tree row re-labeled back to pin (unpin OOB rode the response)");
  ok(s5.focusedRect && s5.focusedRect.q, "I1 still visible-focused after pin/unpin churn", JSON.stringify(s5.focusedRect));
  const wsp4 = JSON.parse(fs.readFileSync(path.join(AGENT_DIR, ".agent", "workspace.json")));
  ok(wsp4.content.find((p) => p.id === "p2").region === "main", "unpin persisted to workspace row (region=main)");

  // rail focus: click the tree row's focus verb on the RE-PINNED pane
  await ev(`htmx.ajax('POST','/filmstrip/pin',{values:{id:'p2'},target:'#fs-sink',swap:'innerHTML'})`);
  await sleep(600);
  await ensureNode("desk/0");
  console.log(await ev(treeVerbClick("/settings", "focus")));
  await sleep(600);
  const s6 = await ev(SNAPSHOT);
  ok((await ev(`document.querySelector('.fs-pane[data-pane="p1"]').classList.contains('focused')`)), "rail focus: p1 keeps .focused (main lift intact)");
  ok((await ev(`document.querySelector('.fs-pane[data-pane="p2"]').classList.contains('focused')`)), "rail focus: tree focus verb gives p2 the ring in place");
  ok(s6.focusedRegion === "main", "rail focus: the lift region is still main", JSON.stringify({region: s6.focusedRegion}));
  ok(s6.railBodies[0] && s6.railBodies[0].pane === "p2" && s6.railBodies[0].bodyVisible, "rail focus: pinned p2 body still visible in the rail", JSON.stringify(s6.railBodies));
  const wsp5 = JSON.parse(fs.readFileSync(path.join(AGENT_DIR, ".agent", "workspace.json")));
  ok(wsp5.focus === "content:0", "rail focus: persisted focus stays on the lifted main pane (no rail steal)", JSON.stringify(wsp5.focus));
  ok(s6.focusedRect && s6.focusedRect.q, "I1 visible focused pane after rail focus", JSON.stringify(s6.focusedRect));

  // open-or-focus of the pinned route via a real cascade leaf click
  await send("Page.navigate", { url: base + "/" });
  ok(await waitShell(), "shell appeared after load");
  await sleep(500);
  ok(!(await ev(`document.querySelector('.fs-pane[data-pane="p2"]').classList.contains('focused')`)), "cascade setup: p2 ring is live-DOM state, gone after reload");
  console.log(await ev(`(()=>{const b=[...document.querySelectorAll('#fs-start .fs-start-entry')].find(e=>e.textContent.trim()==='settings'); b.click(); return 'clicked'})()`));
  await sleep(900);
  const s7 = await ev(SNAPSHOT);
  ok((await ev(`document.querySelector('.fs-pane[data-pane="p2"]').classList.contains('focused')`)), "open-or-focus of a pinned route highlights the rail pane in place");
  ok((await ev(`document.querySelector('.fs-pane[data-pane="p1"]').classList.contains('focused')`)), "open-or-focus of a pinned route leaves the main lift untouched");
  ok(s7.focusedRegion === "main" && s7.focusedRect && s7.focusedRect.q, "I1 main lift still valid after pinned-route open", JSON.stringify({region: s7.focusedRegion, rect: s7.focusedRect}));
  ok(s7.railBodies[0] && s7.railBodies[0].bodyVisible, "pinned p2 body still visible after open-or-focus", JSON.stringify(s7.railBodies));
  const wsp6 = JSON.parse(fs.readFileSync(path.join(AGENT_DIR, ".agent", "workspace.json")));
  ok(wsp6.focus === "content:0", "pinned-route open did not move the persisted focus", JSON.stringify(wsp6.focus));
}

// ---- rail-only heal: shelve the mains, reload ----
after_reload_tag2: {
  console.log(await treeClose("/sessions"));
  console.log("     DEBUG closes:", await ev("performance.getEntriesByType('resource').filter(e=>e.name.includes('/filmstrip/close')).length"));
  console.log("     DEBUG dupbox:", await ev("document.querySelectorAll('#fs-tree-windows').length"));
  await sleep(600);
  console.log(await treeClose("/chat"));
  await sleep(600);
  await send("Page.navigate", { url: base + "/" });
  ok(await waitShell(), "shell appeared after load");
  await sleep(500);
  const s8 = await ev(SNAPSHOT);
  ok(s8.focusedRegion === "rail" && s8.focusedRect && s8.focusedRect.q && !s8.focusedShelved, "I1 rail-only heal: the focused rail pane satisfies I1 (no forced main lift)", JSON.stringify({region: s8.focusedRegion, rect: s8.focusedRect, shelved: s8.focusedShelved}));
  ok(s8.railBodies[0] && s8.railBodies[0].pane === "p2" && s8.railBodies[0].bodyVisible, "rail-only heal: pinned p2 body still visible", JSON.stringify(s8.railBodies));
  // restore the shelved mains so the world stays sane for later runs
  await ensureNode("shelf");
  console.log(await ev(treeVerbClick("/chat", "restore")));
  await sleep(600);
  const s9 = await ev(SNAPSHOT);
  ok(s9.focusedRect && s9.focusedRect.q && !s9.focusedShelved, "I1 still green after restoring a main pane beside a focused rail pane", JSON.stringify(s9.focusedRect));
}

// ---- v3 task 4: FILES AS A RAIL PANE (/code walk at rail height) ----
v3_rail_files: {
  await send("Page.navigate", { url: base + "/" });
  ok(await waitShell(), "shell appeared after load");
  await sleep(500);
  console.log(await ev(`htmx.ajax('POST','/filmstrip/desktop',{values:{d:'1'},target:'#fs-sink',swap:'innerHTML'})`));
  await sleep(900);
  ok(await ev(`document.querySelector('#ws-desk').classList.contains('dsk-1')`), "files setup: desktop 1 active after the desktop POST");
  const wsj = JSON.parse(fs.readFileSync(path.join(AGENT_DIR, ".agent", "workspace.json")));
  const codeRec = wsj.content.find((p) => p.route === "/code" && p.state === "live");
  ok(codeRec && codeRec.region !== "rail", "files setup: live /code pane present (still on the strip)", codeRec ? codeRec.id : "missing");
  if (codeRec) {
    const idSel = `.fs-pane[data-pane="${codeRec.id}"]`;
    await ev(`htmx.ajax('POST','/filmstrip/pin',{values:{id:'${codeRec.id}'},target:'#fs-sink',swap:'innerHTML'})`);
    await sleep(600);
    ok((await ev(`document.querySelector('${idSel}').getAttribute('data-region')`)) === "rail", "files: /code pane pinned to the rail (data-region=rail)");
    ok((await ev(`document.querySelector('${idSel}').closest('#fs-cascade') === null`)), "files: pinned /code section lives in the content strip, not the cascade");
    const sF = await ev(SNAPSHOT);
    ok(sF.railBodies.length === 2 && sF.railBodies.every((r) => r.bodyVisible), "files: both pinned rail bodies visible (desk-0 p2 rides along — dock semantics)", JSON.stringify(sF.railBodies));
    ok(sF.focusedRect && sF.focusedRect.q && (sF.focusedRegion === "main" || sF.focusedRegion === "rail"), "I1 visible focused pane with /code pinned", JSON.stringify({region: sF.focusedRegion, rect: sF.focusedRect}));
    const bodyH = await ev(`Math.round(document.querySelector('${idSel} > .fs-body').getBoundingClientRect().height)`);
    ok((await ev(`getComputedStyle(document.querySelector('${idSel}')).maxHeight`)) === "none", "files: pinned section carries no max-height (cascade cap released)");
    console.log(`     rail geometry: bodyH=${bodyH} contentH=${await ev(`Math.round(document.querySelector('.fs-content').getBoundingClientRect().height)`)}`);
    console.log(await ev(`(()=>{window.__sem3='pre'; const pane=document.querySelector('${idSel}'); const d=pane.querySelector('.fs-body li[data-node="deep"] > details'); if(!d) return 'no deep row in rail body: '+[...pane.querySelectorAll('.fs-body li.fs-tr-item')].map(li=>li.getAttribute('data-node')).join(','); if(!d.open) d.querySelector('summary').click(); return 'deep folded open in the rail'})()`));
    await sleep(600);
    console.log(await ev(`(()=>{const b=[...document.querySelectorAll('${idSel} .fs-body li[data-node="deep/leaf.txt"] > details > summary .fs-tr-open')].find(x=>x.getClientRects().length>0); if(!b) return 'no leaf label in rail body'; b.click(); return 'leaf opened-or-focused via acme Get'})()`));
    await sleep(900);
    ok(await ev(`window.__sem3==='pre'`), "files: the rail tree stayed in the same window (no navigation)");
    ok((await ev(`document.querySelector('.fs-pane.focused .fs-body').textContent.includes('leaf')`)), "files: the leaf label's acme Get focused the file pane (I6 at rail height)");
    const sF2 = await ev(SNAPSHOT);
    ok(sF2.focusedRect && sF2.focusedRect.q, "I1 visible focused pane after the rail files walk", JSON.stringify(sF2.focusedRect));
  }
}

// ---- v3 task 5: FINAL — round-trip + desktop flips + shelf interplay ----
v3_final: {
  const flipDesk = (d) => `htmx.ajax('POST','/filmstrip/desktop',{values:{d:'${d}'},target:'#fs-sink',swap:'innerHTML'})`;
  await send("Page.navigate", { url: base + "/" });
  ok(await waitShell(), "shell appeared after load");
  await sleep(500);
  const wsj5 = JSON.parse(fs.readFileSync(path.join(AGENT_DIR, ".agent", "workspace.json")));
  const pinnedIds = wsj5.content.filter((p) => p.region === "rail").map((p) => p.id);
  ok(pinnedIds.length === 2, "final: both pinned regions survive in workspace.json across the session", JSON.stringify(pinnedIds));
  const sR = await ev(SNAPSHOT);
  ok(sR.pinnedCount === 2 && sR.railBodies.length === 2 && sR.railBodies.every((r) => r.bodyVisible), "final: reload re-renders both pinned panes as rail sections with visible bodies", JSON.stringify(sR.railBodies));
  ok(sR.focusedRect && sR.focusedRect.q, "I1 visible focused pane after the final reload", JSON.stringify(sR.focusedRect));
  for (const d of ["0", "1"]) {
    console.log(await ev(flipDesk(d)));
    await sleep(900);
    ok(await ev(`document.querySelector('#ws-desk').classList.contains('dsk-${d}')`), `final: desktop ${d} active after the desktop POST`);
    const sD = await ev(SNAPSHOT);
    ok(sD.railBodies.length === 2 && sD.railBodies.every((r) => r.bodyVisible), `final: both pinned rail bodies visible on desktop ${d} (dock semantics)`, JSON.stringify(sD.railBodies));
    ok(sD.focusedRect && sD.focusedRect.q, `I1 visible focused pane on desktop ${d} with pinned panes`, JSON.stringify(sD.focusedRect));
  }
  const wsj6 = JSON.parse(fs.readFileSync(path.join(AGENT_DIR, ".agent", "workspace.json")));
  const codeRec6 = wsj6.content.find((p) => p.route === "/code");
  ok(codeRec6 && codeRec6.region === "rail", "final setup: the live /code pane is currently pinned", JSON.stringify(codeRec6 && codeRec6.region));
  console.log(await treeClose("/code"));
  await sleep(900);
  const wsj7 = JSON.parse(fs.readFileSync(path.join(AGENT_DIR, ".agent", "workspace.json")));
  ok(codeRec6 && wsj7.content.find((p) => p.id === codeRec6.id).state === "shelved", "final: shelve persisted to workspace row (state=shelved)");
  ok(codeRec6 && wsj7.content.find((p) => p.id === codeRec6.id).region === "rail", "final: the shelved row KEEPS region=rail (restore will re-pin)");
  const sSh = await ev(SNAPSHOT);
  await ensureNode("shelf");
  ok((await ev(`[...document.querySelectorAll('#fs-tree-windows li.fs-tr-item .fs-tr-open')].some(b=>b.textContent.trim()==='/code')`)), "final: /code appears in the shelf subtree after the close (shelved like any pane)");
  ok(sSh.pinnedCount === 1, "final: only the desk-0 pane remains pinned+live in the rail", `pinned=${sSh.pinnedCount}`);
  ok(sSh.focusedRect && sSh.focusedRect.q && !sSh.focusedShelved, "I1 after shelving a pinned pane: visible live focused pane (heal never lands on the shelved pane)", JSON.stringify({rect: sSh.focusedRect, shelved: sSh.focusedShelved}));
  console.log(await ev(treeVerbClick("/code", "restore")));
  await sleep(900);
  const wsj8 = JSON.parse(fs.readFileSync(path.join(AGENT_DIR, ".agent", "workspace.json")));
  ok(codeRec6 && wsj8.content.find((p) => p.id === codeRec6.id).region === "rail", "final: restore brings /code back WITH its region (rail)");
  const sSt = await ev(SNAPSHOT);
  ok(sSt.pinnedCount === 2 && sSt.railBodies.every((r) => r.bodyVisible), "final: both pinned panes live again with visible rail bodies after restore", JSON.stringify(sSt.railBodies));
  ok(sSt.focusedRect && sSt.focusedRect.q, "I1 visible focused pane after the rail shelve/restore round-trip", JSON.stringify(sSt.focusedRect));
  await checkI8("after the final shelve/restore round-trip");
  if (codeRec6) {
    await ev(`htmx.ajax('POST','/filmstrip/pin',{values:{id:'${codeRec6.id}'},target:'#fs-sink',swap:'innerHTML'})`);
    await sleep(600);
    const wsj9 = JSON.parse(fs.readFileSync(path.join(AGENT_DIR, ".agent", "workspace.json")));
    ok(wsj9.content.find((p) => p.id === codeRec6.id).region === "main", "final: unpin round-trip persists region=main on /code");
    const sU = await ev(SNAPSHOT);
    ok(sU.pinnedCount === 1 && sU.railBodies.every((r) => r.bodyVisible), "final: only p2 remains pinned after the /code unpin", JSON.stringify(sU.railBodies));
    ok(sU.focusedRect && sU.focusedRect.q, "I1 visible focused pane after the final unpin", JSON.stringify(sU.focusedRect));
  }
}

// ---- I2b CROSS-DESKTOP RESTORE (the "restore did nothing" bug class)
// The shelf subtree is DESKTOP-GLOBAL: shelving a pane on desktop N then
// restoring it from desktop M must re-stamp desktop=M (pane-stamp, like
// open-apply's acme-Get unshelve). state=live alone keeps the OLD
// data-desktop on the record, and :has(#ws-desk.dsk-N) keeps the pane
// display:none -- the operator sees nothing happen.
cross_desk_restore: {
  const flipDesk = (d) => `htmx.ajax('POST','/filmstrip/desktop',{values:{d:'${d}'},target:'#fs-sink',swap:'innerHTML'})`;
  await send("Page.navigate", { url: base + "/" });
  ok(await waitShell(), "shell appeared after load");
  await sleep(500);
  await ev(flipDesk("0"));
  await sleep(900);
  console.log(await treeClose("/code"));
  await sleep(600);
  const wsA = JSON.parse(fs.readFileSync(path.join(AGENT_DIR, ".agent", "workspace.json")));
  const shelved = wsA.content.find((p) => p.route === "/code" && p.state === "shelved");
  ok(!!shelved, "I2b setup: /code is shelved (state=shelved)");
  // restore from the OTHER desktop than the one the pane carries (the
  // pane's desktop is whatever it was last stamped with -- run order
  // dependent -- so derive the target instead of assuming 0 vs 1).
  const from = String(shelved ? shelved.desktop : 0);
  const to = from === "0" ? "1" : "0";
  await ev(flipDesk(to));
  await sleep(900);
  await ensureNode("shelf");
  console.log(await ev(treeVerbClick("/code", "restore")));
  await sleep(900);
  const wsB = JSON.parse(fs.readFileSync(path.join(AGENT_DIR, ".agent", "workspace.json")));
  const rec = wsB.content.find((p) => p.route === "/code" && p.state === "live");
  ok(rec && String(rec.desktop) === to, `I2b restore re-stamps the pane to the CURRENT desktop (${to}, not the shelving desktop ${from})`, JSON.stringify(rec && rec.desktop));
  if (rec) {
    const sel = `.fs-pane[data-pane="${rec.id}"]`;
    ok((await ev(`!!document.querySelector('${sel}')`)) && !(await ev(`document.querySelector('${sel}').classList.contains('shelved')`)), "I2b restored section live in the DOM (no .shelved class)");
    ok((await ev(`document.querySelector('${sel}').getAttribute('data-desktop')`)) === to, `I2b restored section carries data-desktop=${to} in the DOM`);
    const r = JSON.parse(await ev(`(()=>{const s=document.querySelector('${sel}'); const b=s.getBoundingClientRect(); return JSON.stringify({q:b.width>0&&b.height>0,w:b.width,h:b.height})})()`));
    ok(r.q, `I2b I1: the restored pane is actually VISIBLE on desktop ${to} (nonzero rect)`, JSON.stringify(r));
  }
  // leave a sane world: back to desktop 0 as the run started
  await ev(flipDesk("0"));
  await sleep(600);
}

// ---- v4 BATCH C (rail-trees-v4): ONE tree renderer, N roots ----
// T0 mode tabs swap visible root only (sentinel :has, zero JS)
// T1 expansion state survives mode flips (DOM kept, never re-rendered)
// T2 lazy levels: expand fetches exactly once; re-fold never refetches
// T3 label = open-or-focus (acme Get unshelves), verbs POST, same window
// T4 collapse-all restores head state with ONE POST
// T5 shelve via a tree verb OOB-moves the row under the shelf subtree
// T6 zero inline styles in the shell (geometry from classes only)
// T7 action buttons are GLYPHS with word metadata (data-verb / aria-label
//    / tooltip); the narrow-only actions fold is NOT visible at rail
//    width -- never an inline row AND a duplicate fold at once
v4_batch_c: {
  await send("Page.navigate", { url: base + "/" });
  ok(await waitShell(), "shell appeared after load");
  await sleep(500);
  ok((await ev(`document.querySelector('#fs-railmode').className`)).includes("rm-windows"), "T0 railmode sentinel defaults to windows");
  ok((await ev(`[...document.querySelectorAll('.fs-rmode')].filter(box=>box.getClientRects().length>0).length`)) === 1, "T0 exactly one tree box visible (sentinel :has)");
  await ensureNode("desk/0");
  const kidsBefore = await ev(`document.querySelector('#fs-tree-windows li[data-node="desk/0"] > details > .fs-tr-kids').children.length`);
  ok(kidsBefore > 0, "T1 setup: desk/0 expanded with pane rows", `kids=${kidsBefore}`);
  await ev(`(()=>{const b=[...document.querySelectorAll('#fs-railtabs button')].find(x=>x.getAttribute('hx-vals')==='{"root":"code"}'); b.click(); return 'flipped-to-code'})()`);
  await sleep(700);
  ok((await ev(`document.querySelector('#fs-railmode').className`)).includes("rm-code"), "T1 mode flip applied (sentinel rm-code)");
  ok((await ev(`[...document.querySelectorAll('.fs-rmode')].filter(box=>box.getClientRects().length>0).length`)) === 1, "T1 only the code tree visible after the flip");
  const codeNodes = await ev(`document.querySelectorAll('#fs-tree-code .fs-tr-item').length`);
  ok(codeNodes > 0, "T1 the code tree renders its level-0 fs listing via the shared renderer", `codeNodes=${codeNodes}`);
  await ev(`(()=>{const b=[...document.querySelectorAll('#fs-railtabs button')].find(x=>x.getAttribute('hx-vals')==='{"root":"windows"}'); b.click(); return 'flipped-back'})()`);
  await sleep(700);
  const kidsAfter = await ev(`((d)=>d?d.children.length:-1)(document.querySelector('#fs-tree-windows li[data-node="desk/0"] > details > .fs-tr-kids'))`);
  ok(kidsAfter === kidsBefore && kidsAfter > 0, "T1 expansion state survived the mode flip (DOM kept, never re-rendered)", `before=${kidsBefore} after=${kidsAfter}`);
  // T2: lazy level fetches exactly once
  const f0 = await ev(`performance.getEntriesByType('resource').filter(e=>e.name.includes('/filmstrip/tree?')).length`);
  const opened = await ev(`(()=>{const d=document.querySelector('#fs-tree-windows li[data-node="shelf"] > details'); const was = d.open; d.querySelector('summary').click(); return !was})()`);
  await sleep(800);
  const f1 = await ev(`performance.getEntriesByType('resource').filter(e=>e.name.includes('/filmstrip/tree?')).length`);
  ok(opened && f1 === f0 + 1, "T2 expanding a fresh dir fetches its level exactly once", `f0=${f0} f1=${f1} opened=${opened}`);
  const shelfRows = await ev(`[...document.querySelectorAll('#fs-tree-windows li.fs-tr-item .fs-tr-open')].filter(b=>b.textContent.trim()==='/settings').length`);
  ok(shelfRows === 1, "T2 the spliced level carries the shelved pane row", `rows=${shelfRows}`);
  await ev(`(()=>{const d=document.querySelector('#fs-tree-windows li[data-node="shelf"] > details'); d.open=false; return 'folded'})()`);
  await sleep(200);
  await ev(`(()=>{const d=document.querySelector('#fs-tree-windows li[data-node="shelf"] > details'); d.open=true; return 'unfolded'})()`);
  await sleep(500);
  const f2 = await ev(`performance.getEntriesByType('resource').filter(e=>e.name.includes('/filmstrip/tree?')).length`);
  ok(f2 === f1, "T2 re-fold never refetches (toggle once)", `f1=${f1} f2=${f2}`);
  // T3: label = open-or-focus (acme Get on a shelved row) + verbs POST + same window
  await ev(`(()=>{window.__semv4='pre'})()`);
  const __shelvedRoute = await ev(`(()=>{const row=[...document.querySelectorAll('#fs-tree-windows li[data-node="shelf"] .fs-tr-kids li.fs-tr-item')].find(li=>li.getClientRects().length>0); if(!row) return null; const b=row.querySelector(':scope > details > summary .fs-tr-open'); return b?b.textContent.trim():null})()`);
  ok(!!__shelvedRoute, "T3 setup: the shelf subtree holds a shelved pane row", String(__shelvedRoute)); 
  console.log(await ev(treeOpenClick(__shelvedRoute || "/settings")));
  await sleep(900);
  ok((await ev(`window.__semv4==='pre'`)), "T3 tree label click stayed in the same window");
  const sT3 = await ev(SNAPSHOT);
  const stateAfter = JSON.parse(fs.readFileSync(path.join(AGENT_DIR, ".agent", "workspace.json"))).content.find((p) => p.route === __shelvedRoute);
  ok(stateAfter && stateAfter.state === "live", "T3 acme Get: the shelved row (" + __shelvedRoute + ") UNSHELVES on label click", JSON.stringify(stateAfter));
  await sleep(800);
  const sT3b = await ev(SNAPSHOT);
  ok(sT3b.focusedRect && sT3b.focusedRect.q && !sT3b.focusedShelved, "I1 visible focused pane after the tree open", JSON.stringify(sT3b.focusedRect));
  console.log("     DEBUG 0x0 focus:", await ev("[(()=>{const p=document.querySelector('.fs-pane.focused'); return p?p.getAttribute('data-desktop')+' '+p.getAttribute('data-region')+' '+p.className:'none'})()]"), " desk:", await ev("(document.querySelector('#ws-desk')||{}).className"));
  await ensureNode("desk/0");
  console.log(await ev(treeVerbClick("/chat", "focus")));
  await sleep(700);
  ok((await ev(`window.__semv4==='pre'`)), "T3 verb click stayed in the same window");
  ok((await ev(`document.querySelector('.fs-pane[data-pane="p1"]').classList.contains('focused')`)), "T3 tree focus verb moves the ring (mutation POST landed)");
  // T4: collapse-all restores head state with ONE POST
  await ensureNode("desk/0"); await ensureNode("shelf");
  const preCollapse = await ev(`[...document.querySelectorAll('#fs-tree-windows details[open]')].length`);
  ok(preCollapse >= 2, "T4 setup: two dirs expanded before collapse-all", `open=${preCollapse}`);
  await ev(`htmx.ajax('POST','/filmstrip/tree/collapse',{values:{root:'windows'},target:'#fs-sink',swap:'innerHTML'})`);
  await sleep(800);
  ok((await ev(`[...document.querySelectorAll('#fs-tree-windows details[open]')].length`)) === 0, "T4 collapse-all folds everything with one POST");
  // T5: shelve via a tree close verb -> row under the shelf subtree (OOB)
  await ensureNode("desk/0");
  console.log(await ev(treeVerbClick("/chat", "close")));
  await sleep(900);
  await ensureNode("shelf");
  ok((await ev(`[...document.querySelectorAll('#fs-tree-windows li.fs-tr-item .fs-tr-open')].some(b=>b.textContent.trim()==='/chat')`)), "T5 shelve: the row moved under the shelf subtree on OOB");
  const wspT5 = JSON.parse(fs.readFileSync(path.join(AGENT_DIR, ".agent", "workspace.json")));
  ok(wspT5.content.find((p) => p.route === "/chat").state === "shelved", "T5 shelve persisted to the workspace row");
  // leave the world sane for later runs: reopen /chat
  await ev(`htmx.ajax('POST','/filmstrip/open',{values:{route:'/chat'},target:'#fs-sink',swap:'innerHTML'})`);
  await sleep(700);

  // T7 glyph vocabulary: every action button is a GLYPH carrying its word
  // as data-verb + aria-label + a human title; and the row's actions fold
  // is NOT also visible at rail width (the duplication bug: inline words
  // plus an eager fold listing the same verbs).
  await ensureNode("desk/0");
  const acts = await ev(`[...document.querySelectorAll('#fs-tree-windows li.fs-tr-item .fs-tr-verb')].filter(b=>b.getClientRects().length>0).map(b=>({g:(b.textContent||'').trim(),v:b.getAttribute('data-verb'),a:b.getAttribute('aria-label'),t:(b.getAttribute('title')||'').length}))`);
  ok(acts.length > 0, "T7 the windows tree renders visible action glyphs", JSON.stringify(acts.slice(0, 4)));
  ok(acts.every((a) => a.v && a.v.length > 0), "T7 every action glyph carries data-verb (the queryable identity)", JSON.stringify(acts.filter((a) => !a.v)));
  ok(acts.every((a) => a.a === a.v), "T7 aria-label mirrors the verb word on every glyph", JSON.stringify(acts.filter((a) => a.a !== a.v)));
  ok(acts.every((a) => a.t > 0), "T7 every glyph carries a human tooltip (title)", JSON.stringify(acts.filter((a) => a.t === 0)));
  const dupRows = await ev(`[...document.querySelectorAll('#fs-tree-windows li[data-node="desk/0"] > details > .fs-tr-acts-ul')].filter(u=>u.getClientRects().length>0).length`);
  ok(dupRows === 0, "T7 the actions fold is NARROW-ONLY: no visible duplicate verb list at rail width", `visibleFolds=${dupRows}`);
  const ghostRows = await ev(`[...document.querySelectorAll('#fs-tree-windows .fs-tr-actrow')].filter(li=>li.getClientRects().length>0).length`);
  ok(ghostRows === 0, "T7 no stray visible action rows outside a collapsed fold", `visibleActionRows=${ghostRows}`);
  const paneHead = await ev(`[...document.querySelectorAll('.fs-pane header button')].map(b=>({v:b.getAttribute('data-verb'),t:(b.getAttribute('title')||'').length,g:(b.textContent||'').trim()}))`);
  ok(paneHead.length > 0 && paneHead.every((b) => b.v && b.t > 0), "T7 the lifted pane head renders the same glyph + data-verb + tooltip contract", JSON.stringify(paneHead.slice(0, 3)));
}

// ---- BATCH D (lookbook): declared ui elements are INSPECTABLE — the
// ui root inventories the <name>/examples cards, a label opens the
// /lookbook pane, the stage renders a LIVE variant call with BOTH
// projections over the same value, a param POST re-renders ONLY the
// stage, and a missing card degrades to a message (declaration is
// self-declared, never sniffed). --
lookbook_batch_d: {
  await send("Page.navigate", { url: base + "/" });
  ok(await waitShell(), "D0 shell appeared for the lookbook batch");
  await sleep(500);
  // D0 the rail carries a ui tab next to windows/code/prims
  ok(await ev(`[...document.querySelectorAll('.fs-railtab')].map(b=>b.textContent.trim()).includes('ui')`), "D0 the rail tabs include the ui root");
  ok((await ev(`document.querySelectorAll('.fs-cbox[data-rmode="ui"]').length`)) === 1, "D0 exactly one ui tree box in the rail");
  // D1 flip the rail mode to ui (same sentinel :has as every flip)
  await ev(`htmx.ajax('POST','/filmstrip/railmode',{values:{root:'ui'},target:'#fs-sink',swap:'innerHTML'})`);
  await sleep(700);
  ok(String(await ev(`document.querySelector('#fs-railmode').className`)).includes('rm-ui'), "D1 rail mode flipped to ui (sentinel :has, zero JS)");
  // D2 the ui root lists the three seed elements, level-0, no fetch
  const uiRows = await ev(`[...document.querySelectorAll('#fs-tree-ui li.fs-tr-item')].map(li=>li.querySelector('.fs-tr-open').textContent.trim())`);
  ok(uiRows.length === 3 && uiRows.includes("ui/button") && uiRows.includes("ui/badge") && uiRows.includes("ui/card"), "D2 ui root lists the declared elements (examples ARE the declaration)", JSON.stringify(uiRows));
  // D3 the label open-or-focuses /lookbook (acme Get) — a real shell pane
  await ev(`(() => { const li=[...document.querySelectorAll('#fs-tree-ui li.fs-tr-item')].find(x=>x.querySelector('.fs-tr-open').textContent.trim()==='ui/button'); li.querySelector('.fs-tr-open').click(); return 'clicked' })()`);
  await sleep(1100);
  const stage1 = await ev(`!!document.querySelector('.fs-pane .lb-stage [data-lb-projection="html"] .lb-btn-primary')`);
  ok(stage1, "D3 the pane stage renders the live variant via the html projection", String(stage1));
  ok(await ev(`!!document.querySelector('.fs-pane .lb-folds') || !!document.querySelector('.fs-pane details.lb-fold')`), "D3 the declared value + text projection + source are folded beside the stage");
  ok(await ev(`[...document.querySelectorAll('.fs-pane .lb-fold')].length >= 3`), "D3 three folds: declared value / text projection / source");
  // D4 variant tab flips the SAME pane to the danger variant (pane-local)
  await ev(`(() => { const b=[...document.querySelectorAll('.fs-pane .lb-vtab')][1]; if(!b) return 'no vtab'; b.click(); return 'v1 clicked' })()`);
  await sleep(1100);
  ok(await ev(`!!document.querySelector('.fs-pane .lb-stage .lb-btn-danger')`), "D4 variant 1 re-renders the pane with the danger preset (pane-local POST)");
  // D5 a param tweak re-renders ONLY the stage (the pane body survives:
  // the h2 + form are still there, and the stage still shows the value)
  const beforeBody = await ev(`document.querySelector('.fs-pane .fs-body').children.length`);
  await ev(`htmx.ajax('POST','/lookbook/run',{values:{name:'ui/button',v:'1',p_label:'Ship',p_kind:'danger'},target:'#lb-stage-ui-button',swap:'outerHTML'})`);
  await sleep(1100);
  ok(await ev(`document.querySelector('.fs-pane .lb-stage').textContent.includes('Ship')`), "D5 the params form re-renders the stage with the new values");
  ok((await ev(`document.querySelector('.fs-pane .fs-body').children.length`)) === beforeBody, "D5 the pane body around the stage never swapped (stage-only, I5)");
  // D6 a missing card degrades to the declaration message (never an error)
  await ev(`htmx.ajax('POST','/filmstrip/open',{values:{route:'/lookbook?name=ui/nonesuch'},target:'#fs-sink',swap:'innerHTML'})`);
  await sleep(1100);
  ok(await ev(`[...document.querySelectorAll('.fs-pane.fs-body, .fs-pane > .fs-body')].some(b=>b.textContent.includes('no examples card for ui/nonesuch'))`), "D6 a cardless element answers the no-card declaration message (fail-soft)");
}

// ---- BATCH E (habitat-panes): the shell wears the habitat's app
// faces as panes. The probe's scratch agent has NO habitat creds, so
// the honest assertions are the FAIL-SOFT ones: the pane opens, the
// boost shell is present, the muted setup card renders (never a 500,
// never a document crash). A live-creds run additionally pipes real
// fragments (dogfood; the OCaml gate plane is module-tested).
habitat_panes_batch_e: {
  await send("Page.navigate", { url: base + "/" });
  ok(await waitShell(), "E0 shell appeared for the habitat-panes batch");
  await sleep(500);
  ok(await ev(`[...document.querySelectorAll('.fs-railtab')].map(b=>b.textContent.trim()).length === 4`), "E0 four rail trees (windows/code/prims/ui)");
  await ev(`htmx.ajax('POST','/filmstrip/open',{values:{route:'/todos'},target:'#fs-sink',swap:'innerHTML'})`);
  await sleep(1400);
  ok(await ev(`!!document.querySelector('.fs-pane .fs-body [hx-boost="true"][id="fwd-todo"]')`), "E1 the /todos pane carries the boost shell (anchors swap into the pane, never out)");
  // the file-fallback means creds may exist even without env vars: the
  // invariant is FAIL-SOFT EITHER WAY — real habitat content OR the
  // muted setup card, never a nested <!DOCTYPE document and never a 500
  const paneTxt = await ev(`((document.querySelector('[id="fwd-todo"]')||{}).textContent||'')`);
  ok(paneTxt.includes('To-dos') || paneTxt.includes('habitat bridge'), "E2 the pane shows habitat content or the setup card (fail-soft)", paneTxt.slice(0, 120));
  ok(!(await ev(`((document.querySelector('[id="fwd-todo"]'))?.outerHTML||'').toLowerCase().includes('<!doctype')`)), "E2b the pane body is fragment-shaped (no nested document)");
  await ev(`htmx.ajax('POST','/filmstrip/open',{values:{route:'/notes'},target:'#fs-sink',swap:'innerHTML'})`);
  await sleep(1200);
  ok(await ev(`!!document.querySelector('[id="fwd-notes"][hx-boost="true"]')`), "E3 /notes pane wears the same shell");
}

console.log(fails === 0 ? "\nSEMANTIC PROBE: ALL GREEN" : `\nSEMANTIC PROBE: ${fails} FAILING`);
process.exit(fails === 0 ? 0 : 1);
