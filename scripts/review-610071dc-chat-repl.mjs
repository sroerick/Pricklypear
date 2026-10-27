#!/usr/bin/env node
/**
 * UI review (card 610071dc): chat bodies over ~500 chars carrying quote
 * characters must reach the model, not bounce through the Nopales REPL gate
 * as a bogus dotted-pair / unbound-word error.
 *
 * Drives a REAL headless chromium over CDP against `glochid --serve
 * --mock-script`. Fills the real #msg input in the filmstrip chat pane,
 * submits through requestSubmit (the path a send click takes), then asserts
 * the streamed DOM: user bubble + llm bubble + the mock reply for both
 * >500-char prose bodies, no .bubble.repl, no reader/interp error text.
 * Finally submits (+ 1 2) and asserts the REPL bubble still renders 3, so
 * the gate did not simply stop evaluating code.
 *
 * Artifacts: screenshots/ui-review-610071dc/ (PNGs + dom.html + ax.json).
 * Prereq: dune build glochid/bin/main.exe. Exit 0 on success.
 */
import { spawn } from "node:child_process";
import { setTimeout as sleep } from "node:timers/promises";
import http from "node:http";
import fs from "node:fs";
import path from "node:path";

const GLOCHID = path.resolve(process.env.GLOCHID_BIN || "_build/default/glochid/bin/main.exe");
const CHROME = process.env.CHROME || "/usr/local/bin/chrome";
const PORTC = 9341;
const OUT = process.env.REVIEW_OUT || "/home/al/pricklypear/screenshots/ui-review-610071dc";
const USER_DATA = `/tmp/chrome-review-610071dc-${process.pid}`;
const MOCK = `/tmp/glochid-review-610071dc-${process.pid}.json`;
const DIR = `/tmp/glochid-review-610071dc-cwd-${process.pid}`;

// The two >500-char bodies from hostkit/test/chat_stream_test.ml: one whose
// ordinary double quotes + period-after-quote used to raise the dotted-pair
// reader error, one whose apostrophes lex as ONE list headed by `I`.
const LONG_QUOTE_PROSE =
  '(Quick note on the "chat" bug. The body below is long enough to cross ' +
  'five hundred characters, which is exactly when this used to bounce. He ' +
  'typed "hello". Then he kept writing, and the reader walked the prose as ' +
  'code. It hit a sentence period after a quoted word and reported a ' +
  'dotted-pair parse error that had nothing to do with what was typed. It ' +
  'should reach the model as chat, not be rejected as broken Lisp. Nothing ' +
  'in this paragraph is an expression; it is one person writing to another, ' +
  'and the paragraph exists only to be long enough to reproduce the report.)';
const LONG_APOSTROPHE_PROSE =
  "(I can't reproduce this. It's odd and long enough that the body crosses " +
  "five hundred characters, which is exactly when this used to bounce. He " +
  "said it wasn't happening, then it was. The reader walked the prose as " +
  "code and reported something bogus instead of reaching the model. Nothing " +
  "here is an expression; it is one person writing to another, and it exists " +
  "only to be long enough to reproduce the report. I don't think it's broken. " +
  "We can't see the bug yet, but it's there, and I'm sure it'll show up again " +
  "the next time someone pastes a paragraph into the box.)";
const REPL_EXPR = "(+ 1 2)";
const REPLIES = [
  "ui review: long quote prose reached the model",
  "ui review: long apostrophe prose reached the model",
];

function ok(m) { console.log("OK  ", m); }
let glochid, chrome;
function killTree(p) {
  if (!p) return;
  try { process.kill(-p.pid, "SIGKILL"); } catch (_) {}
  try { p.kill("SIGKILL"); } catch (_) {}
}
function cleanup() {
  killTree(glochid);
  killTree(chrome);
  try { fs.rmSync(MOCK, { force: true }); } catch (_) {}
  try { fs.rmSync(DIR, { recursive: true, force: true }); } catch (_) {}
}
function fail(m) { console.error("FAIL", m); cleanup(); process.exit(1); }
process.on("exit", cleanup);
setTimeout(() => fail("watchdog: review exceeded 150s"), 150000);

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
    const timer = setTimeout(() => { cleanup2(); reject(new Error(`CDP timeout ${method}`)); }, 15000);
    const onMsg = (ev) => {
      try { const j = JSON.parse(wsData(ev)); if (j.id === id) { cleanup2(); j.error ? reject(new Error(JSON.stringify(j.error))) : resolve(j.result); } } catch (_) {}
    };
    const onErr = (e) => { cleanup2(); reject(e?.error || e); };
    function cleanup2() { clearTimeout(timer); ws.removeEventListener("message", onMsg); ws.removeEventListener("error", onErr); }
    ws.addEventListener("message", onMsg);
    ws.addEventListener("error", onErr);
    ws.send(JSON.stringify({ id, method, params }));
  });
}

if (!fs.existsSync(GLOCHID)) fail(`${GLOCHID} missing (run: dune build glochid/bin/main.exe)`);
if (LONG_QUOTE_PROSE.length <= 500 || LONG_APOSTROPHE_PROSE.length <= 500)
  fail(`prose bodies are not over 500 chars (${LONG_QUOTE_PROSE.length}, ${LONG_APOSTROPHE_PROSE.length})`);

fs.rmSync(OUT, { recursive: true, force: true });
fs.mkdirSync(OUT, { recursive: true });
fs.mkdirSync(USER_DATA, { recursive: true });
fs.writeFileSync(MOCK, JSON.stringify(REPLIES.map((text) => ({ text, calls: [] }))) + "\n");
fs.mkdirSync(`${DIR}/.glochid`, { recursive: true });
fs.writeFileSync(`${DIR}/.glochid/workspace.json`, JSON.stringify({
  axis: "col", desktop: 0, focus: "content:0",
  content: [{ width: "full", route: "/chat", id: "p1", desktop: 0, state: "live" }],
  cascade: ["/chat"],
}));

// --- surfaces ---
glochid = spawn(GLOCHID, ["--serve", "--port", "0", "--mock-script", MOCK], {
  stdio: ["ignore", "pipe", "pipe"], cwd: DIR, detached: true,
});
let glochidUrl = "", buf = "";
glochid.stdout.on("data", (c) => {
  buf += c;
  const m = buf.match(/glochid serving (http:\/\/\S+)/);
  if (m) glochidUrl = m[1].replace(/\/$/, "");
});
for (let i = 0; i < 80 && !glochidUrl; i++) await sleep(250);
if (!glochidUrl) fail("glochid never printed its URL");
ok(`glochid at ${glochidUrl}`);

chrome = spawn(CHROME, [
  "--remote-debugging-port=0",
  `--user-data-dir=${USER_DATA}`,
  "--headless=new", "--disable-gpu", "--no-sandbox",
  "--disable-dev-shm-usage", "--window-size=1200,900",
  "about:blank",
], { stdio: "ignore", detached: true });

let cdpPort = 0;
for (let i = 0; i < 100 && !cdpPort; i++) {
  try { cdpPort = parseInt(fs.readFileSync(`${USER_DATA}/DevToolsActivePort`, "utf8").split("\n")[0], 10); } catch (_) {}
  if (!cdpPort) await sleep(100);
}
if (!cdpPort) fail("chromium never wrote DevToolsActivePort");
const CDP = `http://127.0.0.1:${cdpPort}`;

let ready = false;
for (let i = 0; i < 60; i++) {
  try { if ((await httpGet(`${CDP}/json/version`)).status === 200) { ready = true; break; } } catch (_) {}
  await sleep(200);
}
if (!ready) fail("chromium CDP not ready");
ok(`chromium CDP ready on ${cdpPort}`);

const list = JSON.parse((await httpGet(`${CDP}/json/list`)).body);
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
const evalIn = async (expression) =>
  (await send("Runtime.evaluate", { expression, returnByValue: true, awaitPromise: true }))?.result?.value;

await send("Page.enable");
await send("Runtime.enable");
await send("Accessibility.enable");
await send("Page.navigate", { url: glochidUrl });
await sleep(1500);

await evalIn(`(function(){
  window.__done = false; window.__parts = 0;
  window.addEventListener('htmx:multipart:after:part', function(){ window.__parts++; });
  window.addEventListener('htmx:after:request', function(){ window.__done = true; });
  return true;
})()`);

// --- the shell + the real control ---
const shell = JSON.parse(await evalIn(`(function(){
  var ids = ['fs-content','fs-railtabs','fs-sink','log','f','msg'];
  var out = {};
  ids.forEach(function(id){ var el = document.getElementById(id); out[id] = !!el; });
  var m = document.getElementById('msg');
  var r = m ? m.getBoundingClientRect() : null;
  var b = document.querySelector('#f button');
  return JSON.stringify({
    ids: out,
    msgVisible: !!(m && r && r.width > 10 && r.height > 5),
    msgLabel: m ? m.getAttribute('aria-label') : null,
    msgType: m ? m.tagName + ':' + m.type : null,
    buttonText: b ? b.textContent.trim() : null,
    formInsideContent: !!(document.getElementById('fs-content')?.contains(document.getElementById('f')))
  });
})()`));
for (const id of ["fs-content", "fs-railtabs", "fs-sink", "log", "f", "msg"])
  if (!shell.ids[id]) fail(`#${id} missing from the rendered shell`);
if (!shell.msgVisible) fail("#msg input missing or zero-size (markup parse bug?)");
if (shell.formInsideContent !== true) fail("chat form is not inside the registered #fs-content strip");
if (shell.msgLabel !== "prompt" || shell.msgType !== "INPUT:text" || shell.buttonText !== "send")
  fail(`control shape changed: ${JSON.stringify(shell)}`);
ok(`shell composed: fs-content/fs-railtabs/fs-sink present; control #msg ${shell.msgType} aria-label=${shell.msgLabel} button=${shell.buttonText}`);

async function submit(text) {
  await evalIn(`(function(){
    window.__done = false; window.__parts = 0;
    var m = document.getElementById('msg');
    m.value = ${JSON.stringify(text)};
    document.getElementById('f').requestSubmit(document.querySelector('#f button'));
    return m.value.length;
  })()`);
  for (let i = 0; i < 100; i++) {
    if (await evalIn("window.__done") === true) return true;
    await sleep(100);
  }
  fail(`turn never completed for prompt of ${text.length} chars`);
}
async function logState() {
  return await evalIn(`(function(){
    var log = document.getElementById('log');
    var n = function(s){ return log ? log.querySelectorAll(s).length : -1; };
    var t = function(s){ return log ? Array.from(log.querySelectorAll(s)).map(function(e){return e.innerText;}).join(' | ') : ''; };
    return {
      html: log ? log.innerHTML : '',
      text: log ? log.innerText : '',
      user: n('.bubble.user'), llm: n('.bubble.llm'), repl: n('.bubble.repl'),
      llmText: t('.bubble.llm'), replText: t('.bubble.repl'),
      parts: window.__parts,
      msgValue: document.getElementById('msg') ? document.getElementById('msg').value : null
    };
  })()`);
}
async function shot(name) {
  const rect = JSON.parse(await evalIn(`(function(){
    var el = document.getElementById('fs-content') || document.getElementById('log');
    var r = el.getBoundingClientRect();
    return JSON.stringify({x: r.x + window.scrollX, y: r.y + window.scrollY,
      width: r.width, height: Math.min(r.height, 1400)});
  })()`));
  const png = await send("Page.captureScreenshot", {
    format: "png", captureBeyondViewport: true,
    clip: { x: rect.x, y: rect.y, width: rect.width, height: rect.height, scale: 1 },
  });
  const p = `${OUT}/${name}.png`;
  fs.writeFileSync(p, Buffer.from(png.data, "base64"));
  if (!fs.statSync(p).size) fail(`empty screenshot ${p}`);
  ok(`screenshot ${p} (${fs.statSync(p).size} bytes)`);
  return p;
}
const ERROR_RE = /dotted-pair|unbound|read error|parse error|invalid (syntax|input)/i;

// --- turn 1: >500-char prose with double quotes + period after a quote ---
await submit(LONG_QUOTE_PROSE);
let st = await logState();
ok(`turn 1 (${LONG_QUOTE_PROSE.length} chars, quote prose): parts=${st.parts} user=${st.user} llm=${st.llm} repl=${st.repl}`);
if (st.user !== 1 || st.llm !== 1) fail(`turn 1 bubbles wrong: ${JSON.stringify({user: st.user, llm: st.llm})}`);
if (st.repl !== 0) fail(`turn 1 rendered a REPL bubble (bounced instead of reaching the model): ${st.text.slice(0, 300)}`);
if (!st.text.includes(REPLIES[0])) fail("turn 1 mock reply missing from #log");
if (ERROR_RE.test(st.llmText + "\n" + st.replText)) fail(`turn 1 model output carries a reader/interp error: ${st.llmText.slice(0, 300)}`);
if (st.msgValue !== "") fail("input #msg not cleared after submit");
const shot1 = await shot("01-long-quote-prose");

// --- turn 2: >500-char apostrophe prose (lexes as one list headed by I) ---
await submit(LONG_APOSTROPHE_PROSE);
st = await logState();
ok(`turn 2 (${LONG_APOSTROPHE_PROSE.length} chars, apostrophe prose): user=${st.user} llm=${st.llm} repl=${st.repl}`);
if (st.user !== 2 || st.llm !== 2) fail(`turn 2 bubbles wrong: ${JSON.stringify({user: st.user, llm: st.llm})}`);
if (st.repl !== 0) fail(`turn 2 rendered a REPL bubble: ${st.text.slice(0, 300)}`);
if (!st.text.includes(REPLIES[1])) fail("turn 2 mock reply missing from #log");
if (ERROR_RE.test(st.llmText + "\n" + st.replText)) fail(`turn 2 model output carries a reader/interp error: ${st.llmText.slice(0, 300)}`);
fs.writeFileSync(`${OUT}/dom.html`, st.html);
const shot2 = await shot("02-long-apostrophe-prose");

// --- turn 3: code must still evaluate locally ---
await submit(REPL_EXPR);
st = await logState();
ok(`turn 3 (${REPL_EXPR}): user=${st.user} llm=${st.llm} repl=${st.repl}`);
if (st.repl !== 1) fail(`REPL branch regressed: ${REPL_EXPR} rendered ${st.repl} repl bubbles`);
if (st.llm !== 2) fail(`REPL turn called the model (llm bubbles ${st.llm})`);
if (!/\b3\b/.test(st.text)) fail(`REPL bubble does not carry the value 3: ${st.text.slice(-200)}`);
const shot3 = await shot("03-repl-still-works");

// --- a11y assertion: the control roles/names are stable ---
const ax = await send("Accessibility.getFullAXTree");
const named = (ax.nodes || [])
  .map((n) => ({ role: n.role?.value, name: n.name?.value }))
  .filter((n) => n.name);
fs.writeFileSync(`${OUT}/ax.json`, JSON.stringify(named, null, 2));
const hasPrompt = named.some((n) => n.role === "textbox" && n.name === "prompt");
const hasSend = named.some((n) => n.role === "button" && n.name === "send");
if (!hasPrompt || !hasSend) fail(`a11y tree missing control roles: prompt=${hasPrompt} send=${hasSend}`);
ok("a11y: textbox \"prompt\" and button \"send\" present in the rendered tree");

ok(`DOM + a11y artifacts: ${OUT}/dom.html, ${OUT}/ax.json`);
ok(`screenshots: ${shot1} ${shot2} ${shot3}`);
console.log("PASS card 610071dc: two >500-char quote-bearing bodies streamed model replies, (+ 1 2) still REPL");
cleanup();
process.exit(0);
