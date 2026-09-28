#!/usr/bin/env node
/**
 * /chat lobby CDP review: real headless chromium against the running image.
 * Asserts the declared chat vocabulary renders as one chat box: the composer
 * row sits INSIDE the chat container under a growing transcript, the input is
 * fit-height (not the generic 16rem shell textarea), and a POST swaps a
 * transcript FRAGMENT into #chat-main-log (no whole-page redirect).
 *
 * Env: PP_URL (default http://127.0.0.1:18080), PP_USER, PP_PASS, CHROME.
 * Exit 0 on success. Screenshot -> $CHAT_UI_OUT.
 */
import { spawn } from "node:child_process";
import { setTimeout as sleep } from "node:timers/promises";
import http from "node:http";
import fs from "node:fs";

const PP_URL = (process.env.PP_URL || "http://127.0.0.1:18080").replace(/\/$/, "");
const PP_USER = process.env.PP_USER || "admin";
const PP_PASS = process.env.PP_PASS || "admin";
const PORT = Number(process.env.CDP_PORT || 9344);
const CHROME = process.env.CHROME || "/usr/bin/chromium";
const OUT = process.env.CHAT_UI_OUT || "/tmp/pp-chat-review";
const USER_DATA = `/tmp/chrome-chat-cdp-${process.pid}`;
const MSG = `cdp-lobby-${Date.now()}`;

let chrome;
function cleanup() { try { chrome?.kill(); } catch (_) {} }
process.on("exit", cleanup);
function ok(m) { console.log("OK  ", m); }
function fail(m) { console.error("FAIL", m); cleanup(); process.exit(1); }

function httpGet(url) {
  return new Promise((resolve, reject) => {
    http.get(url, (res) => { let d = ""; res.on("data", (c) => (d += c));
      res.on("end", () => resolve({ status: res.statusCode, body: d })); }).on("error", reject);
  });
}
const wsData = (ev) => { const d = ev?.data ?? ev; return Buffer.isBuffer?.(d) ? d.toString() : String(d); };
async function cdpSend(ws, id, method, params = {}) {
  return new Promise((resolve, reject) => {
    const timer = setTimeout(() => { cleanup2(); reject(new Error(`CDP timeout ${method}`)); }, 20000);
    const onMsg = (ev) => { try { const j = JSON.parse(wsData(ev)); if (j.id === id) { cleanup2();
      if (j.error) reject(new Error(JSON.stringify(j.error))); else resolve(j.result); } } catch (_) {} };
    const onErr = (e) => { cleanup2(); reject(e?.error || e); };
    function cleanup2() { clearTimeout(timer); ws.removeEventListener("message", onMsg); ws.removeEventListener("error", onErr); }
    ws.addEventListener("message", onMsg); ws.addEventListener("error", onErr);
    ws.send(JSON.stringify({ id, method, params }));
  });
}

async function main() {
  fs.mkdirSync(OUT, { recursive: true });
  fs.mkdirSync(USER_DATA, { recursive: true });

  chrome = spawn(CHROME, [
    `--remote-debugging-port=${PORT}`, `--user-data-dir=${USER_DATA}`,
    "--headless=new", "--disable-gpu", "--no-sandbox", "--disable-dev-shm-usage",
    "--window-size=1400,900", "about:blank",
  ], { stdio: "ignore" });
  let ready = false;
  for (let i = 0; i < 40; i++) {
    try { const v = await httpGet(`http://127.0.0.1:${PORT}/json/version`); if (v.status === 200) { ready = true; break; } } catch (_) {}
    await sleep(200);
  }
  if (!ready) fail("chromium CDP not ready");
  ok("chromium CDP ready");

  const list = JSON.parse((await httpGet(`http://127.0.0.1:${PORT}/json/list`)).body);
  const page = list.find((t) => t.type === "page") || list[0];
  if (!page?.webSocketDebuggerUrl) fail("no page target");
  const ws = new globalThis.WebSocket(page.webSocketDebuggerUrl);
  await new Promise((res, rej) => {
    const t = setTimeout(() => rej(new Error("ws open timeout")), 10000);
    ws.onopen = () => { clearTimeout(t); res(); }; ws.onerror = (e) => { clearTimeout(t); rej(e.error || e); };
  });
  ok("CDP websocket open");
  let nextId = 1;
  const send = (m, p) => cdpSend(ws, nextId++, m, p);
  const evalJs = async (expression, awaitPromise = false) => {
    const r = await send("Runtime.evaluate", { expression, awaitPromise, returnByValue: true });
    if (r.exceptionDetails) throw new Error(r.exceptionDetails.text || "eval exception");
    return r.result?.value;
  };
  await send("Page.enable"); await send("Runtime.enable");

  // ---- login ----
  await send("Page.navigate", { url: `${PP_URL}/login` });
  await sleep(900);
  await evalJs(`(function(){ var u=document.querySelector('input[name=username]'); var p=document.querySelector('input[name=password]');
    var f=document.querySelector('form'); if(u)u.value=${JSON.stringify(PP_USER)}; if(p)p.value=${JSON.stringify(PP_PASS)}; if(f)f.submit(); return !!f; })()`);
  await sleep(1500);

  // ---- GET /chat ----
  await send("Page.navigate", { url: `${PP_URL}/chat` });
  await sleep(1600);
  const urlAfterGet = await evalJs("location.pathname");
  if (urlAfterGet !== "/chat") fail(`GET /chat landed on ${urlAfterGet}`);
  ok("GET /chat renders (pathname /chat)");

  // ---- structure + containment ----
  const structure = JSON.parse(await evalJs(`(function(){
    function q(s){return document.querySelector(s);}
    var page=q('#chat-main'), log=q('#chat-main-log'), form=q('#chat-main form.chat-composer'),
        ta=form?form.querySelector('textarea[name=body]'):null, btn=form?form.querySelector('button[type=submit]'):null;
    if(!page||!log||!form||!ta||!btn){return JSON.stringify({err:'missing node',page:!!page,log:!!log,form:!!form,ta:!!ta,btn:!!btn});}
    var pr=page.getBoundingClientRect(), lr=log.getBoundingClientRect(), fr=form.getBoundingClientRect(), tr=ta.getBoundingClientRect();
    var cs=getComputedStyle(ta), ls=getComputedStyle(log), fs=getComputedStyle(form);
    return JSON.stringify({
      pageClass: page.className, logClass: log.className, formClass: form.className,
      logInsidePage: page.contains(log), formInsidePage: page.contains(form),
      taInsideForm: form.contains(ta), btnInsideForm: form.contains(btn),
      logAboveForm: Math.round(lr.bottom) <= Math.round(fr.top)+1,
      formBottom: Math.round(fr.bottom), pageBottom: Math.round(pr.bottom), pageTop: Math.round(pr.top),
      formInsideBox: (fr.top >= pr.top-1) && (fr.bottom <= pr.bottom+1),
      taHeight: Math.round(tr.height), taMinHeight: cs.minHeight, taFlexGrow: cs.flexGrow,
      logFlexGrow: ls.flexGrow, formFlexDirection: fs.flexDirection, formFlexGrow: fs.flexGrow,
      taRows: ta.getAttribute('rows'), taName: ta.name, taPlaceholder: ta.placeholder,
      ids: { page: page.id, log: log.id }, logPoll: log.getAttribute('hx-get'), logTrigger: log.getAttribute('hx-trigger'),
      formHxPost: form.getAttribute('hx-post'), formHxTarget: form.getAttribute('hx-target')
    });
  })()`));
  if (structure.err) fail(`structure: ${structure.err} ${JSON.stringify(structure)}`);
  const asserts = [
    ["#chat-main is .chatpage", structure.pageClass.includes("chatpage")],
    ["transcript #chat-main-log inside #chat-main", structure.logInsidePage === true],
    ["composer form inside #chat-main", structure.formInsidePage === true],
    ["textarea inside composer form", structure.taInsideForm === true],
    ["submit button inside composer form", structure.btnInsideForm === true],
    ["composer form is a row", structure.formFlexDirection === "row"],
    ["transcript grows (flex-grow>=1)", Number(structure.logFlexGrow) >= 1],
    ["composer does not grow (flex 0)", Number(structure.formFlexGrow) === 0],
    ["transcript above composer", structure.logAboveForm === true],
    ["composer bottom inside chat box", structure.formInsideBox === true],
    ["input is fit-height (<120px)", structure.taHeight > 0 && structure.taHeight < 120],
    ["input min-height not the 16rem shell rule", structure.taMinHeight !== "256px" && structure.taMinHeight !== "16rem"],
    ["input grows to fill the row", Number(structure.taFlexGrow) === 1],
    ["log polls /chat/partial", structure.logPoll === "/chat/partial"],
    ["form posts to /chat targeting #chat-main-log", structure.formHxPost === "/chat" && structure.formHxTarget === "#chat-main-log"],
  ];
  let bad = 0;
  for (const [name, condn] of asserts) { if (condn) ok(name); else { console.error("FAIL", name); bad++; } }
  console.log("STRUCTURE", JSON.stringify(structure));
  if (bad) fail(`${bad} structural assertion(s) failed`);

  // ---- a11y tree assertion (roles, not CSS chains) ----
  const ax = await send("Accessibility.getFullAXTree", {});
  const nodes = ax?.nodes || [];
  const findRole = (role) => nodes.filter((n) => n.role?.value === role);
  const textboxes = findRole("textbox"), buttons = findRole("button");
  const composerTa = textboxes.find((n) => (n.name?.value || "").includes("message") || (n.name?.value || "").length === 0);
  const sendBtn = buttons.find((n) => (n.name?.value || "").trim() === "Send");
  if (!textboxes.length) fail("a11y: no textbox role found");
  if (!sendBtn) fail("a11y: no 'Send' button role found");
  ok(`a11y: textbox role present (${textboxes.length}), Send button present`);
  console.log("A11Y", JSON.stringify({ textboxNames: textboxes.map((n) => n.name?.value), buttons: buttons.map((n) => n.name?.value) }));

  // ---- screenshot of the chat region (vision judges layout) ----
  const box = JSON.parse(await evalJs(`(function(){var r=document.querySelector('#chat-main').getBoundingClientRect();
    return JSON.stringify({x:r.x,y:r.y,width:r.width,height:r.height});})()`));
  const clip = { x: Math.max(0, box.x - 8), y: Math.max(0, box.y - 8), width: box.width + 16, height: box.height + 16, scale: 1 };
  const shot = await send("Page.captureScreenshot", { format: "png", clip });
  const shotPath = `${OUT}/chat-composer.png`;
  fs.writeFileSync(shotPath, Buffer.from(shot.data, "base64"));
  ok(`screenshot ${shotPath}`);
  const full = await send("Page.captureScreenshot", { format: "png" });
  fs.writeFileSync(`${OUT}/chat-full.png`, Buffer.from(full.data, "base64"));

  // ---- ACT: fill + submit, assert fragment swap ----
  const scrollBefore = await evalJs("document.querySelector('#chat-main-log').scrollHeight");
  const beforeText = await evalJs("document.querySelector('#chat-main-log').innerText");
  await evalJs(`window.__chatMarker = 'alive';`);
  await evalJs(`(function(){var ta=document.querySelector('#chat-main form.chat-composer textarea[name=body]');
    ta.value=${JSON.stringify(MSG)}; ta.dispatchEvent(new Event('input',{bubbles:true})); return ta.value;})()`);
  await evalJs("document.querySelector('#chat-main form.chat-composer button[type=submit]').click()");
  // wait for htmx swap + poll
  let landed = false, after = "";
  for (let i = 0; i < 25; i++) {
    await sleep(300);
    after = await evalJs("document.querySelector('#chat-main-log').innerText");
    if (after.includes(MSG)) { landed = true; break; }
  }
  const marker = await evalJs("window.__chatMarker === 'alive'");
  const shellAfter = await evalJs("!!document.querySelector('#chat-main') && location.pathname");
  const taAfter = await evalJs(`(function(){var ta=document.querySelector('#chat-main form.chat-composer textarea[name=body]');return ta?ta.value:'MISSING';})()`);
  ok(`posted "${MSG}" landed in transcript: ${landed}`);
  ok(`no full-page navigation (JS marker survived): ${marker}`);
  ok(`chat shell intact after swap: ${shellAfter}`);
  if (!landed) fail(`posted message did not appear in #chat-main-log; log tail: ${after.slice(-300)}`);
  if (!marker) fail("page reloaded on submit (whole-page response, not a fragment)");
  if (!String(shellAfter).startsWith("/chat")) fail(`pathname changed: ${shellAfter}`);
  if (taAfter !== "") console.warn(`WARN composer textarea not reset after submit (pre-existing app-wide htmx4 detail.successful gap): ${JSON.stringify(taAfter)}`);
  else ok("composer reset after successful submit");
  console.log("SWAP", JSON.stringify({ marker, pathname: shellAfter, landed, scrollBefore }));

  // screenshot after submit (message visible in transcript)
  const shot2 = await send("Page.captureScreenshot", { format: "png", clip });
  const shot2Path = `${OUT}/chat-after-post.png`;
  fs.writeFileSync(shot2Path, Buffer.from(shot2.data, "base64"));
  ok(`screenshot ${shot2Path}`);

  cleanup();
  console.log("ALL CHECKS PASSED");
  process.exit(0);
}
main().catch((e) => fail(e?.stack || String(e)));
