#!/usr/bin/env node
/**
 * Fidelity smoke: prove the headless hypermedia client (hyper/*) produces
 * the SAME DOM as a real browser (CDP + htmx 4) at each step of a flow.
 *
 * Spec: borge/hypermedia-harness.borg (fidelity-gate).
 *
 * v1 scope: GET /notes?slug=ui-fixture visit fidelity (the foundation).
 *   - CDP: navigate, extract #outline outerHTML (normalized) + .ol-node count + h1 text
 *   - hyper: hyper/visit, hyper/html #outline (normalized) + count + h1 text
 *   - assert byte-identical (modulo whitespace)
 *
 * Multi-step swap fidelity (add-kid, set-title) is DEFERRED: those flows are
 * JS-driven (data-ol-op + inline postOp), not htmx swaps, so hyper/click
 * correctly returns unsupported-feature (borge issue BORGE-1786011829). When
 * a real hx-get/hx-post multi-step flow is identified, extend this script.
 *
 * Multipart /repl-v2 fidelity is DEFERRED: the repl form uses hx-ext="hx-multipart",
 * which is the unsupported-feature boundary (the fixi-five contract excludes hx-ext).
 *
 * Env: PP_URL (default http://127.0.0.1:18080), PP_USER, PP_PASS, CHROME
 * Exit 0 on success, 1 on mismatch.
 */
import { spawn } from "node:child_process";
import { setTimeout as sleep } from "node:timers/promises";
import http from "node:http";
import fs from "node:fs";

const PP_URL = (process.env.PP_URL || "http://127.0.0.1:18080").replace(/\/$/, "");
const PP_USER = process.env.PP_USER || "admin";
const PP_PASS = process.env.PP_PASS || "admin";
const CHROME = process.env.CHROME || "/usr/bin/chromium";
const PORT = 9340;
const USER_DATA = `/tmp/chrome-hyper-fidelity-${process.pid}`;

function log(...a) { console.log(...a); }
function fail(msg) { console.error("FAIL", msg); process.exit(1); }
function ok(msg) { console.log("OK ", msg); }

function httpGet(url) {
  return new Promise((resolve, reject) => {
    http.get(url, (res) => {
      let d = "";
      res.on("data", (c) => (d += c));
      res.on("end", () => resolve({ status: res.statusCode, body: d }));
    }).on("error", reject);
  });
}

function wsData(ev) {
  const d = ev?.data ?? ev;
  if (typeof d === "string") return d;
  if (Buffer.isBuffer?.(d)) return d.toString();
  return String(d);
}

async function cdpSend(ws, id, method, params = {}) {
  const msg = JSON.stringify({ id, method, params });
  return new Promise((resolve, reject) => {
    const timer = setTimeout(() => {
      cleanup();
      reject(new Error(`CDP timeout ${method}`));
    }, 15000);
    function onMsg(ev) {
      try {
        const m = JSON.parse(wsData(ev));
        if (m.id === id) {
          cleanup();
          if (m.error) reject(new Error(JSON.stringify(m.error)));
          else resolve(m.result);
        }
      } catch (e) { /* ignore non-matching */ }
    }
    function cleanup() {
      clearTimeout(timer);
      if (ws.removeEventListener) ws.removeEventListener("message", onMsg);
      else if (ws.onmessage === onMsg) ws.onmessage = null;
    }
    if (ws.addEventListener) ws.addEventListener("message", onMsg);
    else ws.onmessage = onMsg;
    ws.send(msg);
  });
}

// Normalize HTML for byte-comparison: collapse whitespace, strip
// script/style, and remove client-JS class mutations (e.g. is-selected
// added by inline JS on DOMContentLoaded — the hyper simulator doesn't
// run client JS, so these mutations are the moxi.js boundary).
function normalize(s) {
  return s
    .replace(/<script[\s\S]*?<\/script>/gi, "")
    .replace(/<style[\s\S]*?<\/style>/gi, "")
    .replace(/\s+is-selected/g, "")
    .replace(/is-selected\s+/g, "")
    .replace(/is-selected/g, "")
    .replace(/\s+/g, " ")
    .replace(/> </g, "><")
    .trim();
}

// --- CDP side: extract DOM text from a live browser running htmx 4 ---
async function cdpVisit(slug) {
  const chrome = spawn(CHROME, [
    `--remote-debugging-port=${PORT}`,
    `--user-data-dir=${USER_DATA}`,
    "--headless=new",
    "--disable-gpu",
    "--no-sandbox",
    "--disable-dev-shm-usage",
    "--window-size=1400,900",
    "about:blank",
  ], { stdio: "ignore" });

  let ready = false;
  for (let i = 0; i < 30; i++) {
    try {
      if ((await httpGet(`http://127.0.0.1:${PORT}/json/version`)).status === 200) {
        ready = true; break;
      }
    } catch (_) {}
    await sleep(200);
  }
  if (!ready) { chrome.kill(); fail("chromium CDP not ready"); }

  try {
    const list = JSON.parse((await httpGet(`http://127.0.0.1:${PORT}/json/list`)).body);
    const page = list.find((t) => t.type === "page") || list[0];
    if (!page?.webSocketDebuggerUrl) fail("no page target");
    const ws = new WebSocket(page.webSocketDebuggerUrl);
    await new Promise((res, rej) => {
      const t = setTimeout(() => rej(new Error("ws open timeout")), 10000);
      ws.onopen = () => { clearTimeout(t); res(); };
      ws.onerror = (e) => { clearTimeout(t); rej(e.error || e); };
    });

    let id = 1;
    const send = (m, p) => cdpSend(ws, id++, m, p);
    await send("Page.enable");
    await send("Network.enable");

    // Inject session cookie (avoids the login-form.submit() timing flake)
    const CK = await getSessionCookie();
    await send("Network.setCookie", {
      url: PP_URL + "/", name: "pp_session", value: CK,
      httpOnly: true, sameSite: "Lax",
    });

    await send("Page.navigate", { url: `${PP_URL}/notes?slug=${slug}` });
    await sleep(2000);

    const result = await send("Runtime.evaluate", {
      expression: `JSON.stringify({
        outline: (document.querySelector('#outline')||{}).outerHTML || '',
        olNodeCount: document.querySelectorAll('.ol-node').length,
        olTitle: (document.querySelector('.ol-title')||{}).textContent || ''
      })`,
      returnByValue: true,
    });

    chrome.kill();
    return JSON.parse(result.result.value);
  } catch (e) {
    chrome.kill();
    throw e;
  }
}

// --- hyper side: drive the same flow via /api/eval (no browser) ---
async function hyperVisit(slug) {
  const auth = "Basic " + Buffer.from(`${PP_USER}:${PP_PASS}`).toString("base64");
  async function evalApi(expr) {
    const res = await fetch(`${PP_URL}/api/eval`, {
      method: "POST",
      headers: { Authorization: auth, "Content-Type": "application/json" },
      body: JSON.stringify({ expr }),
    });
    const j = await res.json();
    if (!j.ok && j.error) throw new Error(j.error);
    return j.value;
  }
  await evalApi('(load-library "hyper")');
  // Visit once; page handle persists in the module-global soup table across calls.
  const pageHandle = await evalApi(
    `(begin (load-library "hyper") (hyper/visit "/notes?slug=${slug}"))`
  );
  // Strip the JSON string quotes wrapping the handle.
  const pid = pageHandle.replace(/^"|"$/g, "");
  const olCount = await evalApi(`(hyper/count "${pid}" ".ol-node")`);
  const olTitle = await evalApi(`(hyper/text "${pid}" ".ol-title")`);
  const olOutline = await evalApi(`(begin (define ns (hyper/query "${pid}" "#outline")) (if (null? ns) "" (hyper/html (car ns))))`);
  return {
    outline: olOutline.replace(/^"|"$/g, "").replace(/\\n/g, "\n").replace(/\\"/g, '"'),
    olNodeCount: parseInt(olCount, 10),
    olTitle: olTitle.replace(/^"|"$/g, ""),
  };
}

async function getSessionCookie() {
  const res = await fetch(`${PP_URL}/login`, {
    method: "POST",
    headers: { "Content-Type": "application/x-www-form-urlencoded" },
    body: `username=${PP_USER}&password=${PP_PASS}`,
    redirect: "manual",
  });
  const sc = res.headers.get("set-cookie") || "";
  const m = /pp_session=([^;]+)/.exec(sc);
  if (!m) fail("could not get session cookie");
  return m[1];
}

async function main() {
  fs.mkdirSync(USER_DATA, { recursive: true });
  const SLUG = "ui-fixture";

  log("== CDP (real browser + htmx 4) ==");
  const cdp = await cdpVisit(SLUG);
  ok(`ol-node count: ${cdp.olNodeCount}`);
  ok(`ol-title: "${cdp.olTitle}"`);
  ok(`#outline outerHTML: ${cdp.outline.length} chars`);

  log("== hyper (headless, no browser) ==");
  const hyp = await hyperVisit(SLUG);
  ok(`ol-node count: ${hyp.olNodeCount}`);
  ok(`ol-title: "${hyp.olTitle}"`);
  ok(`#outline outerHTML: ${hyp.outline.length} chars`);

  log("== fidelity assertions ==");
  if (cdp.olNodeCount !== hyp.olNodeCount)
    fail(`ol-node count mismatch: CDP=${cdp.olNodeCount} hyper=${hyp.olNodeCount}`);
  ok(`ol-node count matches: ${cdp.olNodeCount}`);

  if (cdp.olTitle !== hyp.olTitle)
    fail(`ol-title mismatch: CDP="${cdp.olTitle}" hyper="${hyp.olTitle}"`);
  ok(`ol-title matches: "${cdp.olTitle}"`);

  const cdpNorm = normalize(cdp.outline);
  const hypNorm = normalize(hyp.outline);
  if (cdpNorm !== hypNorm) {
    console.error("CDP  (first 300):", cdpNorm.slice(0, 300));
    console.error("hyper(first 300):", hypNorm.slice(0, 300));
    fail("#outline outerHTML mismatch (after whitespace normalization)");
  }
  ok(`#outline outerHTML byte-identical (${cdpNorm.length} chars normalized)`);

  log("\nFIDELITY SMOKE OK — hyper matches the real browser at the visit step.");
  log("Multi-step swap fidelity (add-kid, set-title, multipart repl) deferred:");
  log("  those flows are JS-driven (data-ol-op) or hx-ext (unsupported-feature boundary).");
}

main().catch((e) => { console.error("ERROR", e.message || e); process.exit(1); });
