#!/usr/bin/env node
/**
 * One-off geometry probe for the /sessions table fix (table.sess):
 * loads a data: URL harness carrying the REAL shared chrome CSS +
 * filmstrip pane CSS (copied from the production stylesheets) and
 * measures, at each width preset (third/half/full), whether the pane
 * body or the main strip scrolls horizontally. PASS = no horizontal
 * overflow at any preset, cells ellipsize instead.
 */
import { spawn } from "node:child_process";
import { setTimeout as sleep } from "node:timers/promises";
import http from "node:http";
import fs from "node:fs";

const PORTC = 9033;
const CHROME = process.env.CHROME || "/usr/bin/chromium";
const USER_DATA = `/tmp/chrome-sesscss-probe-${process.pid}`;

function ok(m) { console.log("OK  ", m); }
function fail(m) { console.error("FAIL", m); cleanup(); process.exit(1); }
function cleanup() { try { chrome?.kill(); } catch (_) {} try { fs.rmSync(USER_DATA, { recursive: true, force: true }); } catch (_) {} }
process.on("exit", cleanup);
process.on("SIGINT", () => { cleanup(); process.exit(1); });
function httpGet(url) {
  return new Promise((resolve, reject) => {
    http.get(url, (res) => {
      let d = "";
      res.on("data", (c) => (d += c));
      res.on("end", () => resolve({ status: res.statusCode, body: d }));
    }).on("error", reject);
  });
}
const bytes = (evv) => Buffer.isBuffer(evv.data) ? evv.data.toString() : String(evv.data);

// ---- REAL CSS, copied from production ----
// shared chrome (agent/lib/surface.ml) filmstrip shell (libs/filmstrip/lib.pp):
const PAGE_CSS = `:root{color-scheme:dark;--border:#2a2e3a;--panel:#1b1f2a;--ink:#d8dee9;--muted:#6b7080;--chrome:#12141c;--sel-bg:#2a2e3a;--sel-accent:#7aa2f7}
html,body{height:100%}body{background:#0f1117;color:#d8dee9;font-family:ui-monospace,Menlo,monospace;margin:0;padding:1rem;box-sizing:border-box}
h1{margin:.25rem 0 .75rem 0;font-size:1.1rem}
table{border-collapse:collapse;width:100%;max-width:60rem}td,th{padding:.35rem .6rem;border-bottom:1px solid #222;text-align:left;white-space:nowrap}
.muted{color:#6b7080}
.fs-shell{display:flex;flex-direction:column;gap:.4rem;height:calc(100vh - 6rem);min-height:28rem}
.fs-content{display:flex;flex-direction:row;gap:.5rem;flex:1 1 auto;overflow-x:auto;overflow-y:hidden;align-items:stretch}
.fs-pane{display:flex;flex-direction:column;flex:0 0 33.333%;min-width:15rem;border:1px solid var(--border);background:var(--panel);overflow:hidden;scroll-margin-inline:3rem}
.fs-pane.fs-server-third{flex-basis:33.333%}
.fs-pane.fs-half{flex-basis:50%}
.fs-pane.fs-full{flex-basis:100%}
.fs-body{flex:1 1 auto;overflow:auto;padding:.4rem;min-height:0}
button{background:#1b1f2a;color:#d8dee9;border:1px solid #333;border-radius:6px;padding:.2rem .5rem;font:inherit}
/* THE FIX */
table.sess{table-layout:fixed}
table.sess td,table.sess th{overflow:hidden;text-overflow:ellipsis}
table.sess th:nth-child(1){width:10.5rem}
table.sess th:nth-child(3){width:3rem}
table.sess th:nth-child(5){width:6.5rem}`;

// long prompts like the real transcripts (the ones from the screenshot)
const ROWS = [
  ["2026-09-13 19:55", "hf:zai-org/GLM-4.7-Flash", "2", "hi can you describe your harness?", "<button>resume</button> · <a href=\"#\">view</a>"],
  ["2026-09-11 20:59", "hf:zai-org/GLM-4.7-Flash", "14", "Remember this durable fact: glochid now shares gregors markdown doc conventions with the habitat and the issue register lives at /issues — plus a very long tail that would previously push the whole table wide", "<button>resume</button> · <a href=\"#\">view</a>"],
  ["2026-09-11 20:55", "hf:zai-org/GLM-4.7-Flash", "4", "Remember this durable fact for me: the glochid desktop theater has exactly three fixed width presets and every pane body must fit inside them without scrolling", "<button>resume</button> · <a href=\"#\">view</a>"],
  ["2026-09-11 20:07", "hf:zai-org/GLM-4.7-Flash", "2", "Reply with exactly: persistence works", "<button>resume</button> · <a href=\"#\">view</a>"],
];
const ROWS_HTML = ROWS.map(([t, m, c, p, a]) =>
  `<tr><td class="muted">${t}</td><td>${m}</td><td>${c}</td><td class="muted">${p}</td><td>${a}</td></tr>`).join("\n");

const harness = (cls) => `<!doctype html><html><head><style>${PAGE_CSS}</style></head><body>
<div class="fs-shell"><div class="fs-content" id="strip">
<section class="fs-pane ${cls}"><div class="fs-body">
<h1><span class="muted">/sessions</span></h1>
<p><button>+ new session</button></p>
<table class="sess"><tr><th>time</th><th>model</th><th>msgs</th><th>first prompt</th><th></th></tr>
${ROWS_HTML}
</table>
<p class="muted">transcripts: .agent/sessions/*.jsonl (append-only, local store)</p>
</div></section>
</div></div></body></html>`;

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
let nextId = 1;
const pending = new Map();
ws.onmessage = (ev) => { const m = JSON.parse(bytes(ev)); if (m.id && pending.has(m.id)) { pending.get(m.id)(m); pending.delete(m.id); } };
const send = (method, params = {}) => { const id = nextId++; ws.send(JSON.stringify({ id, method, params })); return new Promise((r) => pending.set(id, r)); };
async function ev(expression) {
  const r = await send("Runtime.evaluate", { expression, returnByValue: true, awaitPromise: true });
  if (r.result?.exceptionDetails) throw new Error(JSON.stringify(r.result.exceptionDetails));
  return r.result?.result?.value;
}
const MEASURE = `(()=>{
  const body=document.querySelector('.fs-body');
  const strip=document.querySelector('#strip');
  const td=document.querySelector('table.sess td:nth-child(4)');
  const trunc=td.scrollWidth>td.clientWidth;
  return {
    bodyOver: body.scrollWidth>body.clientWidth+1,
    stripOver: strip.scrollWidth>strip.clientWidth+3, stripSW: strip.scrollWidth, stripCW: strip.clientWidth,
    previewTruncated: trunc,
    previewStyle: getComputedStyle(td).textOverflow,
  };
})()`;

for (const [cls, label] of [["fs-server-third", "third"], ["fs-half", "half"], ["fs-full", "full"]]) {
  const url = "data:text/html;charset=utf-8," + encodeURIComponent(harness(cls));
  await send("Page.enable");
  await send("Page.navigate", { url });
  await sleep(600);
  const m = await ev(MEASURE);
  console.log(label + ":", JSON.stringify(m));
  if (m.bodyOver) fail(`${label}: pane BODY still scrolls horizontally`);
  if (label !== "third" && !m.previewTruncated) fail(`${label}: preview not truncating (still fully laid out)`);
}
ok("sessions table fits first/half/full presets with ellipsized prompts");
process.exit(0);
