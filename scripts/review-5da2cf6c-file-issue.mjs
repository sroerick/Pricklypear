#!/usr/bin/env node
/**
 * UI review harness for card 5da2cf6c: the filmstrip rail "file an issue"
 * control must open a dialog swapped into #fs-layers, not navigate to
 * /issues.
 *
 * Drives real headless Chromium over CDP against PP_URL. Two passes:
 *   1. load the filmstrip shell and report whether the rail sink exists
 *   2. load a real authed page, inject #fs-layers, POST the dialog route the
 *      button uses, let htmx reveal the issues body, assert the modal
 *
 * Env: PP_URL, PP_USER, PP_TOKEN (or the env file), PP_UI_OUT, CHROME,
 * CDP_PORT.
 */
import { spawn } from "node:child_process";
import { setTimeout as sleep } from "node:timers/promises";
import http from "node:http";
import fs from "node:fs";
import path from "node:path";

const URL_ = (process.env.PP_URL || "https://pricklypear.rocks").replace(/\/$/, "");
const OUT = process.env.PP_UI_OUT || "screenshots";
const PORT = Number(process.env.CDP_PORT || 9346);
const CHROME = process.env.CHROME || "/usr/local/bin/chrome";
const TAG = "5da2cf6c";
const TOKEN = (process.env.PP_TOKEN || "").trim();

function fail(msg) { console.error("FAIL ", msg); process.exitCode = 1; }
function ok(msg) { console.log("OK   ", msg); }
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
function cdpSend(ws, id, method, params = {}) {
  return new Promise((resolve, reject) => {
    const timer = setTimeout(() => { cleanup(); reject(new Error(`CDP timeout ${method}`)); }, 30000);
    function onMsg(ev) {
      try {
        const m = JSON.parse(wsData(ev));
        if (m.id === id) { cleanup(); if (m.error) reject(new Error(JSON.stringify(m.error))); else resolve(m.result); }
      } catch (_) {}
    }
    function cleanup() {
      clearTimeout(timer);
      if (ws.removeEventListener) ws.removeEventListener("message", onMsg);
      else if (ws.onmessage === onMsg) ws.onmessage = null;
    }
    if (ws.addEventListener) ws.addEventListener("message", onMsg);
    else ws.onmessage = onMsg;
    ws.send(JSON.stringify({ id, method, params }));
  });
}
const sleep2 = (ms) => sleep(ms);

const LOOK = `(() => ({
  url: location.pathname,
  title: document.title,
  h1: [...document.querySelectorAll('h1')].map(h => h.textContent.trim()),
  pre: [...document.querySelectorAll('pre')].map(p => p.textContent.trim()).join(' | '),
  hasLayers: !!document.querySelector('#fs-layers'),
  hasRailBtn: !!document.querySelector('#fs-file-issue'),
  bodyText: (document.body?.innerText || '').slice(0, 200)
}))()`;

const INJECT = `(async () => {
  const out = { fetched: null, error: null };
  try {
    document.body.insertAdjacentHTML('beforeend', '<div id="fs-layers" style="position:relative;z-index:5"></div>');
    const r = await fetch('/filmstrip/dialog', {
      method: 'POST',
      headers: { 'hx-request': 'true', 'content-type': 'application/x-www-form-urlencoded;charset=UTF-8' },
      body: 'route=%2Fissues&title=file+an+issue'
    });
    out.fetched = { status: r.status, type: r.headers.get('content-type') };
    const html = await r.text();
    out.html = html.slice(0, 300);
    const layer = document.getElementById('fs-layers');
    layer.innerHTML = html;
      if (window.htmx && htmx.process) htmx.process(layer);
      layer.scrollIntoView({ block: 'center' });
    const deadline = Date.now() + 8000;
    while (Date.now() < deadline) {
      const b = layer.querySelector('.fs-dialog-body');
      if (b && b.textContent.trim().length > 20) break;
      await new Promise(res => setTimeout(res, 150));
    }
    const dlg = layer.querySelector('.fs-dialog, [role=dialog]');
    const body = layer.querySelector('.fs-dialog-body');
    const close = layer.querySelector('.fs-dialog-close');
    const rect = dlg ? dlg.getBoundingClientRect() : null;
    out.dialog = {
      present: !!dlg,
      role: dlg?.getAttribute('role') || null,
      ariaModal: dlg?.getAttribute('aria-modal') || null,
      title: layer.querySelector('.fs-dialog-title')?.textContent.trim() || null,
      closeLabel: close?.textContent.trim() || null,
      closeTarget: close?.getAttribute('hx-target') || null,
      bodyHxGet: body?.getAttribute('hx-get') || null,
      bodyTrigger: body?.getAttribute('hx-trigger') || null,
      bodyTarget: body?.getAttribute('hx-target') || null,
      bodyChars: body ? body.textContent.trim().length : 0,
      bodyHead: body ? body.textContent.trim().slice(0, 160) : null,
      insideLayers: !!layer.querySelector('.fs-dialog'),
      rect: rect ? { x: Math.round(rect.x), y: Math.round(rect.y), w: Math.round(rect.width), h: Math.round(rect.height) } : null,
      viewport: { w: innerWidth, h: innerHeight },
      overflowX: dlg ? getComputedStyle(dlg).overflowX : null
    };
    out.urlAfter = location.pathname + location.search;
  } catch (e) { out.error = String(e); }
  window.__revOut = out;
  return out;
})()`;

async function main() {
  if (!TOKEN) { fail("no PP_TOKEN"); return; }
  fs.mkdirSync(OUT, { recursive: true });
  const userData = `/tmp/chrome-${TAG}-${process.pid}`;
  const chrome = spawn(CHROME, [
    "--headless", "--disable-gpu", "--no-sandbox", `--remote-debugging-port=${PORT}`,
    `--user-data-dir=${userData}`, "--window-size=1200,900", "about:blank",
  ], { stdio: "ignore" });
  process.on("exit", () => { try { chrome.kill(); } catch (_) {} });

  let ready = false;
  for (let i = 0; i < 60; i++) {
    try { const v = await httpGet(`http://127.0.0.1:${PORT}/json/version`); if (v.status === 200) { ready = true; break; } } catch (_) {}
    await sleep2(200);
  }
  if (!ready) { fail("chromium CDP did not come up"); return; }
  ok(`chromium CDP ready on ${PORT}`);

  try {
    const list = JSON.parse((await httpGet(`http://127.0.0.1:${PORT}/json/list`)).body);
    const page = list.find((t) => t.type === "page") || list[0];
    if (!page?.webSocketDebuggerUrl) { fail("no page target"); return; }
    const ws = new WebSocket(page.webSocketDebuggerUrl);
    await new Promise((res, rej) => {
      const t = setTimeout(() => rej(new Error("ws open timeout")), 10000);
      ws.onopen = () => { clearTimeout(t); res(); };
      ws.onerror = (e) => { clearTimeout(t); rej(e.error || e); };
    });
    let nextId = 1;
    const send = (method, params) => cdpSend(ws, nextId++, method, params);
    await send("Page.enable");
    await send("Runtime.enable");
    await send("Network.enable");
    await send("Network.setExtraHTTPHeaders", { headers: { Authorization: `Bearer ${TOKEN}` } });
    await send("Emulation.setDeviceMetricsOverride", { width: 1200, height: 900, deviceScaleFactor: 1, mobile: false });

    // Pass 1: the shell page the rail button lives on.
    const shellShot = path.join(OUT, `${TAG}-shell.png`);
    await send("Page.navigate", { url: `${URL_}/filmstrip` });
    for (let i = 0; i < 80; i++) {
      const st = await send("Runtime.evaluate", { expression: "document.readyState", returnByValue: true });
      if (st.result?.value === "complete") break;
      await sleep2(100);
    }
    await sleep2(500);
    const shell = (await send("Runtime.evaluate", { expression: LOOK, returnByValue: true })).result?.value;
    let shot = await send("Page.captureScreenshot", { format: "png", captureBeyondViewport: false });
    fs.writeFileSync(shellShot, Buffer.from(shot.data, "base64"));
    console.log("SHELL " + JSON.stringify(shell));
    console.log("SHELL_SHOT " + shellShot);
    const shellBroken = !shell?.hasLayers || !shell?.hasRailBtn;
    if (shellBroken) fail(`filmstrip shell did not render the rail sink: ${JSON.stringify(shell)}`);
    else ok("filmstrip shell has #fs-layers and #fs-file-issue");

    // Pass 2: the dialog fragment the button posts, rendered in a real page.
    await send("Page.navigate", { url: `${URL_}/issues` });
    for (let i = 0; i < 80; i++) {
      const st = await send("Runtime.evaluate", { expression: "document.readyState", returnByValue: true });
      if (st.result?.value === "complete") break;
      await sleep2(100);
    }
    await sleep2(300);
    const pre = (await send("Runtime.evaluate", { expression: LOOK, returnByValue: true })).result?.value;
    console.log("HOST " + JSON.stringify({ url: pre?.url, hasHtmx: !!(await send("Runtime.evaluate", { expression: "!!window.htmx", returnByValue: true })).result?.value }));

    const res = await send("Runtime.evaluate", { expression: INJECT, awaitPromise: true, returnByValue: true });
    const v = res.result?.value;
    console.log("DIALOG " + JSON.stringify(v));
    const modalShot = path.join(OUT, `${TAG}-modal.png`);
    shot = await send("Page.captureScreenshot", { format: "png", captureBeyondViewport: false });
    fs.writeFileSync(modalShot, Buffer.from(shot.data, "base64"));
    console.log("MODAL_SHOT " + modalShot);

    if (v?.error) fail(`dialog injection error: ${v.error}`);
    if (v?.fetched?.status !== 200) fail(`dialog fetch ${v?.fetched?.status}`);
    if (!v?.dialog?.present) fail("no .fs-dialog in the swapped fragment");
    if (v?.dialog?.role !== "dialog" || v?.dialog?.ariaModal !== "true") fail("dialog lacks role=dialog/aria-modal");
    if (v?.dialog?.title !== "file an issue") fail(`dialog title is ${v?.dialog?.title}`);
    if (v?.dialog?.bodyHxGet !== "/issues") fail(`dialog body hx-get is ${v?.dialog?.bodyHxGet}`);
    if (!v?.dialog?.insideLayers) fail("dialog not inside #fs-layers");
    if (!(v?.dialog?.bodyChars > 20)) fail("issues body never revealed inside the dialog");
    if (v?.urlAfter !== "/issues") fail(`page navigated to ${v?.urlAfter}`);
    if (v?.dialog?.rect && (v.dialog.rect.x < 0 || v.dialog.rect.y < 0)) fail("dialog positioned off-viewport");
    ok(`modal inside #fs-layers, title=file an issue, body=${v.dialog.bodyChars} chars, page stayed ${v.urlAfter}`);
    ws.close();
  } finally {
    chrome.kill();
  }
}

main().catch((e) => fail(e.stack || String(e)));
