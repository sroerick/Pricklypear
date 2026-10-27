#!/usr/bin/env node
// Card 02002b2b UI review ACT rung: real headless chromium over CDP against the
// running image. Proves the detail form names the current claim lease and that a
// no-op save of the form preserves it (the bug this card fixes).
// Env: PP_URL, PP_TOKEN, CHROME, UI_OUT.
import { spawn } from "node:child_process";
import { setTimeout as sleep } from "node:timers/promises";
import http from "node:http";
import fs from "node:fs";
import path from "node:path";

const PP_URL = (process.env.PP_URL || "https://pricklypear.rocks").replace(/\/$/, "");
const TOKEN = process.env.PP_TOKEN || "";
const CHROME = process.env.CHROME || "/usr/local/bin/chrome";
const OUT = process.env.UI_OUT || "/home/al/pricklypear/screenshots/review-02002b2b";
const PORT = 9559;
const CARD = "02002b2b-8fd1-4c32-81e4-3ab6605404bf";
const UD = `/tmp/chrome-ui-02002b2b-act-${process.pid}`;

fs.mkdirSync(OUT, { recursive: true });
const results = [];
const log = (...a) => console.log(...a);
const record = (name, ok, detail) => { results.push({ name, ok: !!ok, detail }); log((ok ? "OK   " : "FAIL ") + name + " :: " + JSON.stringify(detail)); };
const json = (u) => new Promise((res, rej) => { http.get(u, (r) => { let d = ""; r.on("data", (c) => (d += c)); r.on("end", () => { try { res(JSON.parse(d)); } catch (e) { rej(e); } }); }).on("error", rej); });
async function waitFor(fn, ms = 90000, step = 500, label = "") {
  const t0 = Date.now();
  for (;;) { try { const v = await fn(); if (v) return v; } catch {}
    if (Date.now() - t0 > ms) throw new Error("timeout " + label); await sleep(step); }
}
const chrome = spawn(CHROME, ["--headless", "--disable-gpu", "--no-sandbox", "--remote-allow-origins=*",
  `--remote-debugging-port=${PORT}`, `--user-data-dir=${UD}`, "--window-size=1200,900", "--hide-scrollbars", "about:blank"],
  { stdio: ["ignore", "ignore", "pipe"] });
chrome.stderr.on("data", () => {});

let ws, nextId = 1;
const pending = new Map();
function send(method, params = {}) {
  const id = nextId++;
  return new Promise((resolve, reject) => {
    pending.set(id, { resolve, reject }); ws.send(JSON.stringify({ id, method, params }));
    setTimeout(() => { if (pending.has(id)) { pending.delete(id); reject(new Error("CDP timeout " + method)); } }, 120000);
  });
}
async function evaluate(expression) {
  const r = await send("Runtime.evaluate", { expression, returnByValue: true, awaitPromise: true });
  if (r.exceptionDetails) throw new Error("eval: " + JSON.stringify(r.exceptionDetails).slice(0, 400));
  return r.result?.value;
}
async function shotClip(name, selector, pad = 14) {
  const box = await evaluate(`(() => { const el = document.querySelector(${JSON.stringify(selector)});
    if (!el) return null; const r = el.getBoundingClientRect();
    return { x: r.left + window.scrollX, y: r.top + window.scrollY, w: r.width, h: r.height }; })()`);
  if (!box) throw new Error("shotClip: no element " + selector);
  const clip = { x: Math.max(0, Math.round(box.x - pad)), y: Math.max(0, Math.round(box.y - pad)),
    width: Math.max(1, Math.round(Math.min(box.w + pad * 2, 1180))), height: Math.max(1, Math.round(box.h + pad * 2)), scale: 1 };
  const r = await send("Page.captureScreenshot", { format: "png", captureBeyondViewport: true, clip });
  const p = path.join(OUT, name + ".png"); fs.writeFileSync(p, Buffer.from(r.data, "base64")); return p;
}
async function apiEval(expr) {
  const r = await fetch(`${PP_URL}/api/eval`, { method: "POST", headers: { Authorization: `Bearer ${TOKEN}`, "Content-Type": "application/json" }, body: JSON.stringify({ expr }) });
  return { status: r.status, body: await r.json().catch(async () => ({ raw: await r.text() })) };
}
const detailUrl = `${PP_URL}/todos?id=${CARD}`;
const fieldSel = ".todo-field.todo-assignee-field";
async function openDetail() {
  await send("Page.navigate", { url: detailUrl });
  await waitFor(() => evaluate(`!!document.querySelector("form.todo-detail-form select#assignee")`), 150000, 500, "detail-form");
}
async function readField() {
  return evaluate(`(() => {
    const form = document.querySelector("form.todo-detail-form");
    const field = form && form.querySelector(".todo-field.todo-assignee-field");
    const sel = field && field.querySelector("select#assignee");
    if (!sel) return null;
    const o = sel.options[sel.selectedIndex];
    const sr = sel.getBoundingClientRect(), fr = form.getBoundingClientRect(), br = field.getBoundingClientRect();
    const cs = getComputedStyle(sel);
    return { value: sel.value, optionCount: sel.options.length,
      options: [...sel.options].map(x => x.value),
      selectedIndex: sel.selectedIndex, selectedText: o && o.textContent.trim(), selectedAttr: !!(o && o.hasAttribute("selected")),
      inForm: sr.width > 0 && sr.height > 0 && sr.left >= fr.left - 1 && sr.right <= fr.right + 1,
      visible: cs.display !== "none" && cs.visibility !== "hidden" && cs.opacity !== "0",
      label: field.querySelector("label[for=assignee]")?.textContent.trim(),
      overflowX: document.documentElement.scrollWidth - document.documentElement.clientWidth }; })()`);
}

try {
  const ver = await waitFor(() => json(`http://127.0.0.1:${PORT}/json/version`), 30000, 300, "chrome");
  log("chrome", ver.Browser);
  const target = await waitFor(async () => (await json(`http://127.0.0.1:${PORT}/json/list`)).find((t) => t.type === "page"), 30000, 300, "target");
  ws = new WebSocket(target.webSocketDebuggerUrl);
  await new Promise((res, rej) => { ws.onopen = res; ws.onerror = () => rej(new Error("ws error")); });
  ws.onmessage = (ev) => { const m = JSON.parse(ev.data);
    if (m.id && pending.has(m.id)) { const p = pending.get(m.id); pending.delete(m.id); m.error ? p.reject(new Error(JSON.stringify(m.error))) : p.resolve(m.result); } };
  await send("Page.enable");
  await send("Network.enable");
  await send("Network.setExtraHTTPHeaders", { headers: { Authorization: `Bearer ${TOKEN}` } });

  const leaseResp = await apiEval(`(todo/item/assignee (todo/item/get "${CARD}"))`);
  const lease = String(leaseResp.body && leaseResp.body.value || "");
  log("lease", lease, "apiStatus", leaseResp.status);
  record("card-carries-a-claim-lease", /^(run|glochid):/.test(lease), { lease, apiStatus: leaseResp.status });

  await openDetail();
  const before = await readField();
  log("before", JSON.stringify(before));
  record("lease-option-present-and-selected", !!before && before.options.includes(lease) && before.value === lease && before.selectedText === lease && before.selectedAttr, before);
  record("assignee-field-in-box-labelled", !!before && before.label === "Assignee" && before.inForm && before.visible && before.overflowX <= 1, before && { label: before.label, inForm: before.inForm, visible: before.visible, overflowX: before.overflowX });
  const shot1 = await shotClip("10-detail-lease-selected", fieldSel);
  log("screenshot", shot1);

  await evaluate(`(() => { document.querySelector("form.todo-detail-form").requestSubmit(); return true; })()`);
  await waitFor(() => evaluate(`location.pathname === "/todos" && !document.querySelector("form.todo-detail-form")`), 120000, 500, "save-submit");
  log("saved, back on", await evaluate("location.href"));

  await openDetail();
  const after = await readField();
  log("after", JSON.stringify(after));
  record("no-op-save-preserves-lease", !!after && after.value === lease && after.selectedText === lease, { lease, after });
  const apiAfter = await apiEval(`(todo/item/assignee (todo/item/get "${CARD}"))`);
  const leaseAfter = String(apiAfter.body && apiAfter.body.value || "");
  record("stored-assignee-unchanged-after-save", leaseAfter === lease, { lease, leaseAfter, apiStatus: apiAfter.status });
  const shot2 = await shotClip("11-detail-lease-after-save", fieldSel);
  log("screenshot", shot2);

  const inv = await apiEval("(lookbook/elements)");
  const elems = Array.isArray(inv.body && inv.body.value) ? inv.body.value : null;
  const wanted = ["todo/ui/assignee-field", "todo/ui/assignee-select", "todo/ui/assignee-option", "todo/ui/assignee-badge"];
  const missing = elems ? wanted.filter(w => !elems.includes(w)) : wanted;
  record("lookbook-registers-assignee-vocabulary", !!elems && missing.length === 0, { apiStatus: inv.status, missing });

  const failed = results.filter(r => !r.ok);
  log("SUMMARY", JSON.stringify({ total: results.length, failed: failed.length, failedNames: failed.map(f => f.name), screenshots: OUT }));
  process.exitCode = failed.length ? 1 : 0;
} catch (e) {
  log("HARNESS ERROR", (e && e.stack) || e); process.exitCode = 2;
} finally {
  try { chrome.kill("SIGKILL"); } catch {}
  try { fs.rmSync(UD, { recursive: true, force: true }); } catch {}
}
