#!/usr/bin/env node
// Card 02002b2b UI review: ACT rung over real headless chromium (CDP).
// Assigns a scratch card through the declared assignee select, asserts the board
// badge, clears it, deletes the scratch card, and reads the live lookbook inventory.
// Env: PP_URL, PP_TOKEN, CHROME, UI_OUT.
import { spawn } from "node:child_process";
import { setTimeout as sleep } from "node:timers/promises";
import http from "node:http";
import fs from "node:fs";
import path from "node:path";

const PP_URL = (process.env.PP_URL || "https://pricklypear.rocks").replace(/\/$/, "");
const TOKEN = process.env.PP_TOKEN || "";
const CHROME = process.env.CHROME || "/usr/local/bin/chrome";
const OUT = process.env.UI_OUT || "/home/al/pricklypear/screenshots/ui-review-02002b2b";
const PORT = 9557;
const CARD = "02002b2b-8fd1-4c32-81e4-3ab6605404bf";
const SCRATCH = "31b55bbd-cb6c-4762-a2a5-b7b00b1d153c";
const MARK = "UI-REVIEW 02002b2b scratch 1793063718544";
const UD = `/tmp/chrome-ui-02002b2b-act2-${process.pid}`;
const WAIT = 120000;

fs.mkdirSync(OUT, { recursive: true });
const results = [];
const log = (...a) => console.log(new Date().toISOString().slice(11, 19), ...a);
const json = (u) => new Promise((res, rej) => {
  http.get(u, (r) => { let d = ""; r.on("data", (c) => (d += c)); r.on("end", () => { try { res(JSON.parse(d)); } catch (e) { rej(e); } }); }).on("error", rej);
});
async function waitFor(fn, ms = WAIT, step = 750, label = "") {
  const t0 = Date.now();
  let last = 0;
  for (;;) {
    try { const v = await fn(); if (v) return v; } catch {}
    if (Date.now() - t0 > ms) throw new Error("timeout after " + ms + "ms " + label);
    if (Date.now() - last > 15000) { last = Date.now(); try { log("waiting", label, await evaluate("location.href + ' ready=' + document.readyState")); } catch {} }
    await sleep(step);
  }
}

const chrome = spawn(CHROME, ["--headless", "--disable-gpu", "--no-sandbox", "--remote-allow-origins=*",
  "--disable-dev-shm-usage", "--no-first-run", "--disable-background-networking",
  `--remote-debugging-port=${PORT}`, `--user-data-dir=${UD}`, "--window-size=1200,900",
  "--hide-scrollbars", "about:blank"], { stdio: ["ignore", "ignore", "pipe"] });
chrome.stderr.on("data", () => {});

let ws, nextId = 1;
const pending = new Map();
function send(method, params = {}) {
  const id = nextId++;
  return new Promise((resolve, reject) => {
    pending.set(id, { resolve, reject });
    ws.send(JSON.stringify({ id, method, params }));
    setTimeout(() => { if (pending.has(id)) { pending.delete(id); reject(new Error("CDP timeout " + method)); } }, WAIT);
  });
}
async function evaluate(expression) {
  const r = await send("Runtime.evaluate", { expression, returnByValue: true, awaitPromise: true });
  if (r.exceptionDetails) throw new Error("eval: " + JSON.stringify(r.exceptionDetails).slice(0, 400));
  return r.result?.value;
}
async function shotClip(name, selector, pad = 12, maxH = 900) {
  const box = await evaluate(`(() => { const el = document.querySelector(${JSON.stringify(selector)});
    if (!el) return null; const r = el.getBoundingClientRect();
    return { x: r.left + window.scrollX, y: r.top + window.scrollY, w: r.width, h: r.height }; })()`);
  if (!box) throw new Error("shotClip: no element " + selector);
  const clip = { x: Math.max(0, Math.round(box.x - pad)), y: Math.max(0, Math.round(box.y - pad)),
    width: Math.max(1, Math.round(Math.min(box.w + pad * 2, 1180))),
    height: Math.max(1, Math.round(Math.min(box.h + pad * 2, maxH))), scale: 1 };
  const r = await send("Page.captureScreenshot", { format: "png", captureBeyondViewport: true, clip });
  const p = path.join(OUT, name + ".png");
  fs.writeFileSync(p, Buffer.from(r.data, "base64"));
  return p;
}
function record(name, ok, detail) {
  results.push({ name, ok: !!ok, detail });
  log((ok ? "OK   " : "FAIL ") + name + " :: " + JSON.stringify(detail));
}
async function apiEval(expr) {
  const r = await fetch(`${PP_URL}/api/eval`, { method: "POST",
    headers: { Authorization: `Bearer ${TOKEN}`, "Content-Type": "application/json" },
    body: JSON.stringify({ expr }) });
  return { status: r.status, body: await r.json().catch(async () => ({ raw: await r.text() })) };
}
const plain = (v) => (typeof v === "string" ? v.replace(/^"|"$/g, "") : v);
const arr = (v) => { if (Array.isArray(v)) return v; if (typeof v === "string") { try { const p = JSON.parse(v); return Array.isArray(p) ? p : null; } catch { return null; } } return null; };

async function nav(url, selector) {
  for (let attempt = 1; attempt <= 3; attempt++) {
    try {
      log("navigate", attempt, url);
      await send("Page.navigate", { url });
      await waitFor(() => evaluate(`!!document.querySelector(${JSON.stringify(selector)})`), 90000, 750, selector);
      return true;
    } catch (e) {
      log("nav attempt failed", attempt, e.message);
      await sleep(2000);
    }
  }
  throw new Error("navigation failed " + url);
}

try {
  const ver = await waitFor(() => json(`http://127.0.0.1:${PORT}/json/version`), 30000, 300, "chrome");
  log("chrome", ver.Browser);
  const target = await waitFor(async () => (await json(`http://127.0.0.1:${PORT}/json/list`)).find((t) => t.type === "page"), 30000, 300, "target");
  ws = new WebSocket(target.webSocketDebuggerUrl);
  await Promise.race([new Promise((res, rej) => { ws.onopen = res; ws.onerror = () => rej(new Error("ws error")); ws.onclose = () => rej(new Error("ws closed")); }), sleep(10000).then(() => { throw new Error("ws open timeout"); })]);
  ws.onmessage = (ev) => {
    const m = JSON.parse(ev.data);
    if (m.id && pending.has(m.id)) { const p = pending.get(m.id); pending.delete(m.id); m.error ? p.reject(new Error(JSON.stringify(m.error))) : p.resolve(m.result); }
    else if (m.method === "Page.javascriptDialogOpening") send("Page.handleJavaScriptDialog", { accept: true }).catch(() => {});
  };
  await send("Page.enable");
  await send("Network.enable");
  await send("Network.setExtraHTTPHeaders", { headers: { Authorization: `Bearer ${TOKEN}` } });

  const leaseResp = await apiEval(`(todo/item/assignee (todo/item/get "${CARD}"))`);
  const lease = plain(leaseResp.body && leaseResp.body.value);
  log("card-lease", JSON.stringify(lease));

  await nav(`${PP_URL}/todos?id=${CARD}`, "select#assignee");
  const cardField = await evaluate(`(() => { const s = document.querySelector("select#assignee");
    return { lease: ${JSON.stringify(lease)}, value: s.value, hasLeaseOption: [...s.options].some(o => o.value === ${JSON.stringify(lease)}),
      selectedText: s.options[s.selectedIndex] && s.options[s.selectedIndex].textContent.trim(),
      options: [...s.options].map(o => o.value) }; })()`);
  record("current-lease-claim-represented-in-field", !!cardField.lease && cardField.hasLeaseOption && cardField.value === cardField.lease && cardField.selectedText === cardField.lease, cardField);
  log("screenshot", await shotClip("07-detail-lease-field", ".todo-field.todo-assignee-field", 14, 220));

  await nav(`${PP_URL}/todos?id=${SCRATCH}`, "select#assignee");
  const before = await evaluate(`({ value: document.querySelector("select#assignee").value })`);
  const who = before.value === "gregor" ? "hollis" : "gregor";
  const submitted = await evaluate(`(() => { const f = document.querySelector("form.todo-detail-form");
    const sel = f.querySelector("select#assignee"); sel.value = ${JSON.stringify("TARGET")}; f.requestSubmit(); return true; })()`.replace("TARGET", who));
  // POST /todos action=save answers the list view, so the board row is the first DOM proof.
  await nav(`${PP_URL}/todos`, "li.todo-item");
  await waitFor(() => evaluate(`document.body.innerText.includes(${JSON.stringify(MARK)})`), WAIT, 750, "marker-row");
  const row = await evaluate(`(() => { const li = [...document.querySelectorAll("li.todo-item")].find(li => li.innerText.includes(${JSON.stringify(MARK)}));
    if (!li) return null; const b = li.querySelector(".todo-assignee-badge");
    const lr = li.getBoundingClientRect(), br = b && b.getBoundingClientRect();
    return { badge: b && b.textContent, title: b && b.getAttribute("title"),
      inRow: !!br && br.width > 0 && br.left >= lr.left - 1 && br.right <= lr.right + 1 }; })()`);
  record("act-assignment-renders-badge-in-row", !!row && row.badge === "@" + who && row.title === "assigned to " + who && row.inRow, { who, row });
  log("screenshot", await shotClip("09-board-assigned-row", "li.todo-item:has(.todo-assignee-badge)", 10, 120));

  await nav(`${PP_URL}/todos?id=${SCRATCH}`, "select#assignee");
  const after = await evaluate(`(() => { const s = document.querySelector("select#assignee"); const o = s.options[s.selectedIndex];
    return { value: s.value, selectedText: o.textContent.trim(), selectedAttr: o.hasAttribute("selected") }; })()`);
  record("act-assign-saves-with-selected-flag", submitted && after.value === who && after.selectedText === who && after.selectedAttr, { before, who, after });
  log("screenshot", await shotClip("08-detail-assigned-gregor", ".todo-field.todo-assignee-field", 14, 220));

  await nav(`${PP_URL}/todos?id=${SCRATCH}`, "select#assignee");
  await evaluate(`(() => { const f = document.querySelector("form.todo-detail-form"); f.querySelector("select#assignee").value = ""; f.requestSubmit(); return true; })()`);
  await nav(`${PP_URL}/todos`, "li.todo-item");
  await waitFor(() => evaluate(`document.body.innerText.includes(${JSON.stringify(MARK)})`), WAIT, 750, "marker-row-2");
  const cleared = await evaluate(`(() => { const li = [...document.querySelectorAll("li.todo-item")].find(li => li.innerText.includes(${JSON.stringify(MARK)}));
    return { found: !!li, badge: !!(li && li.querySelector(".todo-assignee-badge")) }; })()`);
  record("act-clear-assignee-removes-badge", cleared.found && cleared.badge === false, cleared);
  const deleted = await evaluate(`(() => { const li = [...document.querySelectorAll("li.todo-item")].find(li => li.innerText.includes(${JSON.stringify(MARK)}));
    const f = li && li.querySelector("form.todo-del"); if (!f) return false; f.requestSubmit(); return true; })()`);
  await waitFor(() => evaluate(`!document.body.innerText.includes(${JSON.stringify(MARK)})`), WAIT, 750, "deleted");
  record("act-delete-scratch-card", deleted === true, { deleted });

  const wanted = ["todo/ui/assignee-field", "todo/ui/assignee-select", "todo/ui/assignee-option", "todo/ui/assignee-badge"];
  const inv = await apiEval("(lookbook/elements)");
  const seam = await apiEval('(lookbook/add-elements (list \"probe-a\") (list \"probe-b\"))');
  const elems = arr(inv.body && inv.body.value);
  const missing = elems ? wanted.filter(w => !elems.includes(w)) : wanted;
  record("lookbook-inventory-registers-assignee-vocabulary", !!elems && missing.length === 0, { apiStatus: inv.status, elements: elems, missing });
  const seamVal = arr(seam.body && seam.body.value);
  record("registration-seam-appends-absent-names", !!seamVal && seamVal.join(",") === "probe-a,probe-b", { apiStatus: seam.status, value: seamVal });

  const failed = results.filter(r => !r.ok);
  log("SUMMARY", JSON.stringify({ total: results.length, failed: failed.length, failedNames: failed.map(f => f.name), screenshots: OUT }));
  log("RESULTS", JSON.stringify(results));
  process.exitCode = failed.length ? 1 : 0;
} catch (e) {
  log("HARNESS ERROR", e && e.stack || e);
  process.exitCode = 2;
} finally {
  try { chrome.kill("SIGKILL"); } catch {}
  try { fs.rmSync(UD, { recursive: true, force: true }); } catch {}
}
