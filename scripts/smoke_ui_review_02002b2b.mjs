#!/usr/bin/env node
// UI review harness for card 02002b2b (todo assignee field / card assignment).
// Real headless chromium over CDP against the running instance.
// Env: PP_URL, PP_TOKEN, CHROME (default /usr/local/bin/chrome), UI_OUT.
import { spawn } from "node:child_process";
import { setTimeout as sleep } from "node:timers/promises";
import http from "node:http";
import fs from "node:fs";
import path from "node:path";

const PP_URL = (process.env.PP_URL || "https://pricklypear.rocks").replace(/\/$/, "");
const TOKEN = process.env.PP_TOKEN || "";
const CHROME = process.env.CHROME || "/usr/local/bin/chrome";
const OUT = process.env.UI_OUT || "/home/al/pricklypear/screenshots/ui-review-02002b2b";
const PORT = 9555;
const CARD = "02002b2b-8fd1-4c32-81e4-3ab6605404bf";
const UD = `/tmp/chrome-ui-02002b2b-${process.pid}`;

fs.mkdirSync(OUT, { recursive: true });

const results = [];
const log = (...a) => console.log(...a);
const json = (u) => new Promise((res, rej) => {
  http.get(u, (r) => { let d = ""; r.on("data", (c) => (d += c)); r.on("end", () => { try { res(JSON.parse(d)); } catch (e) { rej(e); } }); }).on("error", rej);
});
async function waitFor(fn, ms = 90000, step = 500) {
  const t0 = Date.now();
  for (;;) {
    try { const v = await fn(); if (v) return v; } catch {}
    if (Date.now() - t0 > ms) throw new Error("timeout after " + ms + "ms");
    await sleep(step);
  }
}

const chrome = spawn(CHROME, ["--headless", "--disable-gpu", "--no-sandbox", "--remote-allow-origins=*",
  `--remote-debugging-port=${PORT}`, `--user-data-dir=${UD}`, "--window-size=1200,900",
  "--hide-scrollbars", "about:blank"], { stdio: ["ignore", "ignore", "pipe"] });
chrome.stderr.on("data", () => {}); // dbus noise expected on this host

let ws, nextId = 1;
const pending = new Map();
function send(method, params = {}) {
  const id = nextId++;
  return new Promise((resolve, reject) => {
    pending.set(id, { resolve, reject });
    ws.send(JSON.stringify({ id, method, params }));
    setTimeout(() => { if (pending.has(id)) { pending.delete(id); reject(new Error("CDP timeout " + method)); } }, 120000);
  });
}
async function evaluate(expression) {
  const r = await send("Runtime.evaluate", { expression, returnByValue: true, awaitPromise: true });
  if (r.exceptionDetails) throw new Error("eval: " + JSON.stringify(r.exceptionDetails).slice(0, 400));
  return r.result?.value;
}
async function shot(name) {
  const r = await send("Page.captureScreenshot", { format: "png", fromSurface: true, captureBeyondViewport: true });
  const p = path.join(OUT, name + ".png");
  fs.writeFileSync(p, Buffer.from(r.data, "base64"));
  return p;
}
async function shotClip(name, selector, pad = 12, maxH = 1600) {
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
async function goto(url) {
  await send("Page.navigate", { url });
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

try {
  const ver = await waitFor(() => json(`http://127.0.0.1:${PORT}/json/version`));
  log("chrome", ver.Browser);
  const target = await waitFor(async () => (await json(`http://127.0.0.1:${PORT}/json/list`)).find((t) => t.type === "page"));
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

  // ---- 1. BOARD FACE: rows render the declared assignee badge ----
  await goto(`${PP_URL}/todos`);
  await waitFor(() => evaluate(`document.querySelectorAll("li.todo-item").length > 0`), 120000);
  const board = await evaluate(`(() => {
    const items = [...document.querySelectorAll("li.todo-item")];
    const badges = items.flatMap(li => [...li.querySelectorAll(".todo-assignee-badge")].map(b => ({ b, li })));
    const sample = badges.slice(0, 4).map(({ b, li }) => {
      const br = b.getBoundingClientRect(), lr = li.getBoundingClientRect();
      const cs = getComputedStyle(b);
      return { text: b.textContent, title: b.getAttribute("title"),
        inRow: br.width > 0 && br.height > 0 && br.left >= lr.left - 1 && br.right <= lr.right + 1 && br.top >= lr.top - 1 && br.bottom <= lr.bottom + 1,
        visible: cs.display !== "none" && cs.visibility !== "hidden" && cs.opacity !== "0",
        wrap: br.height <= lr.height + 1 };
    });
    return { items: items.length, badges: badges.length, sample,
      docOverflowX: document.documentElement.scrollWidth - document.documentElement.clientWidth }; })()`);
  record("board-rows-render", board.items > 0, { items: board.items });
  record("declared-assignee-badge-present", board.badges > 0 && board.sample.every(s => /^@\S+$/.test(s.text) && /^assigned to \S+/.test(s.title)), { badges: board.badges, sample: board.sample });
  record("badge-contains-and-does-not-overflow-row", board.sample.length > 0 && board.sample.every(s => s.inRow && s.visible && s.wrap), board.sample);
  record("board-no-horizontal-overflow", board.docOverflowX <= 1, { docOverflowX: board.docOverflowX });
  let p1 = await shot("01-board-full");
  log("screenshot", p1);
  try { p1 = await shotClip("02-board-badge-row", "li.todo-item:has(.todo-assignee-badge)", 10, 120); log("screenshot", p1); } catch (e) { log("clip skipped", e.message); }

  // ---- 2. DETAIL FACE: declared assignee field + select ----
  await goto(`${PP_URL}/todos?id=${CARD}`);
  await waitFor(() => evaluate(`!!document.querySelector("form.todo-detail-form")`), 150000);
  const detail = await evaluate(`(() => {
    const form = document.querySelector("form.todo-detail-form");
    const field = form && form.querySelector(".todo-field.todo-assignee-field");
    const label = field && field.querySelector("label[for=assignee]");
    const sel = field && field.querySelector("select#assignee[name=assignee]");
    const opts = sel ? [...sel.options] : [];
    const fr = field && field.getBoundingClientRect(), sr = sel && sel.getBoundingClientRect(),
          lr = label && label.getBoundingClientRect(), br = form.getBoundingClientRect();
    return { hasForm: !!form, hasField: !!field, fieldInsideForm: !!(field && form.contains(field)),
      labelText: label && label.textContent.trim(), selectInsideField: !!(sel && field.contains(sel)),
      selectName: sel && sel.name, selectId: sel && sel.id, selectClass: sel && sel.className,
      optionCount: opts.length, firstOption: opts[0] && { v: opts[0].value, t: opts[0].textContent.trim() },
      selectedValue: sel && sel.value, userOptions: opts.slice(1).filter(o => o.value !== "").length,
        fieldHeight: fr && fr.height,
      selectInsideFormBox: !!(sr && br && sr.left >= br.left - 1 && sr.right <= br.right + 1),
      fieldInsideFormBox: !!(fr && br && fr.left >= br.left - 1 && fr.right <= br.right + 1),
      docOverflowX: document.documentElement.scrollWidth - document.documentElement.clientWidth }; })()`);
  record("detail-form-renders", detail.hasForm, { hasForm: detail.hasForm });
  record("declared-assignee-field-projects", detail.hasField && detail.fieldInsideForm && detail.labelText === "Assignee", { hasField: detail.hasField, inside: detail.fieldInsideForm, label: detail.labelText });
  record("declared-assignee-select-projects", detail.selectInsideField && detail.selectName === "assignee" && detail.selectId === "assignee" && /todo-assignee/.test(detail.selectClass || ""), { name: detail.selectName, id: detail.selectId, cls: detail.selectClass });
  record("assignee-options-one-per-user-with-clear", detail.optionCount > 2 && detail.firstOption?.v === "" && /unassigned/i.test(detail.firstOption?.t || ""), { optionCount: detail.optionCount, first: detail.firstOption, users: detail.userOptions });
    record("assignee-field-renders-in-box", detail.selectInsideFormBox && detail.fieldInsideFormBox && detail.fieldHeight > 0 && detail.docOverflowX <= 1, { selectInForm: detail.selectInsideFormBox, fieldInForm: detail.fieldInsideFormBox, fieldHeight: detail.fieldHeight, docOverflowX: detail.docOverflowX });
  log("detail-selected", JSON.stringify({ selectedValue: detail.selectedValue, users: detail.userOptions }));
  let p2 = await shotClip("03-detail-assignee-field", ".todo-field.todo-assignee-field", 14, 220);
  log("screenshot", p2);
  let p3 = await shotClip("04-detail-form", "form.todo-detail-form", 10, 1100);
  log("screenshot", p3);

  // ---- 5. COMPONENT REGISTRATION: lookbook inventory is the reference ----
  const wanted = ["todo/ui/assignee-field", "todo/ui/assignee-select", "todo/ui/assignee-option", "todo/ui/assignee-badge"];
  let inv = null, seam = null;
  try {
    inv = await apiEval("(lookbook/elements)");
    seam = await apiEval('(lookbook/add-elements (list \"probe-a\") (list \"probe-b\"))');
  } catch (e) { log("api eval error", e.message); }
  const elems = inv && inv.body && (() => { try { const v = inv.body.value; return Array.isArray(v) ? v : (typeof v === "string" ? JSON.parse(v.replace(/^\[/, "[")) : null); } catch { return null; } })();
  const missing = elems ? wanted.filter(w => !elems.includes(w)) : wanted;
  record("lookbook-inventory-registers-assignee-vocabulary", !!elems && missing.length === 0, { apiStatus: inv && inv.status, elements: elems, missing });
  const seamVal = seam && seam.body && seam.body.value;
  record("registration-seam-appends-absent-names", Array.isArray(seamVal) ? seamVal.join(",") === "probe-a,probe-b" : false, { apiStatus: seam && seam.status, value: seamVal });

  const failed = results.filter(r => !r.ok);
  log("SUMMARY", JSON.stringify({ total: results.length, failed: failed.length, failedNames: failed.map(f => f.name) }));
  log("RESULTS", JSON.stringify(results));
  process.exitCode = failed.length ? 1 : 0;
} catch (e) {
  log("HARNESS ERROR", e && e.stack || e);
  process.exitCode = 2;
} finally {
  try { chrome.kill("SIGKILL"); } catch {}
  try { fs.rmSync(UD, { recursive: true, force: true }); } catch {}
}
