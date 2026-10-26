#!/usr/bin/env node
/**
 * UI review harness for card e3de4aa0: the zero-JS start menu must never
 * extend below the viewport; cap ~2/3 viewport height with its own scroll.
 *
 * Drives real headless Chromium over CDP against a rendered filmstrip shell,
 * opens #fs-start, measures the menu box, and writes screenshots.
 *
 * Env: PP_UI_URL (default http://127.0.0.1:18091), PP_UI_OUT (default
 * screenshots), CHROME (default /usr/local/bin/chrome), CDP_PORT (default 9334).
 */
import { spawn } from "node:child_process";
import { setTimeout as sleep } from "node:timers/promises";
import http from "node:http";
import fs from "node:fs";
import path from "node:path";

const URL_ = (process.env.PP_UI_URL || "http://127.0.0.1:18091").replace(/\/$/, "");
const OUT = process.env.PP_UI_OUT || "screenshots";
const PORT = Number(process.env.CDP_PORT || 9334);
const CHROME = process.env.CHROME || "/usr/local/bin/chrome";
const TAG = "e3de4aa0";

function fail(msg) {
  console.error("FAIL ", msg);
  process.exit(1);
}
function ok(msg) {
  console.log("OK   ", msg);
}
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
    const timer = setTimeout(() => { cleanup(); reject(new Error(`CDP timeout ${method}`)); }, 20000);
    function onMsg(ev) {
      try {
        const m = JSON.parse(wsData(ev));
        if (m.id === id) {
          cleanup();
          if (m.error) reject(new Error(JSON.stringify(m.error)));
          else resolve(m.result);
        }
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

const MEASURE = `(() => {
  const d = document.querySelector('#fs-start');
  const menu = document.querySelector('.fs-start-menu');
  if (!d || !menu) return { error: 'no #fs-start or .fs-start-menu' };
  if (!d.open) { const s = d.querySelector('summary'); if (s) s.click(); }
  const r = menu.getBoundingClientRect();
  const cs = getComputedStyle(menu);
  const entries = [...menu.querySelectorAll('.fs-start-entry')];
  return {
    open: !!d.open,
    viewport: { w: window.innerWidth, h: window.innerHeight },
    rect: { top: Math.round(r.top), bottom: Math.round(r.bottom), left: Math.round(r.left), width: Math.round(r.width), height: Math.round(r.height) },
    maxHeight: cs.maxHeight,
    overflowY: cs.overflowY,
    position: cs.position,
    scrollHeight: menu.scrollHeight,
    clientHeight: menu.clientHeight,
    contentOverflows: menu.scrollHeight > menu.clientHeight + 1,
    entries: entries.length,
    firstEntry: entries[0]?.textContent?.trim() || null,
    lastEntry: entries.at(-1)?.textContent?.trim() || null,
    summaryText: d.querySelector('summary')?.textContent?.trim() || null,
    summaryLabel: d.querySelector('summary')?.getAttribute('aria-label') || null
  };
})()`;

async function main() {
  fs.mkdirSync(OUT, { recursive: true });
  fs.mkdirSync(`/tmp/chrome-${TAG}`, { recursive: true });

  const chrome = spawn(CHROME, [
    `--remote-debugging-port=${PORT}`,
    `--user-data-dir=/tmp/chrome-${TAG}`,
    "--headless=new",
    "--disable-gpu",
    "--no-sandbox",
    "--disable-dev-shm-usage",
    "--window-size=1200,900",
    "about:blank",
  ], { stdio: "ignore" });

  let ready = false;
  for (let i = 0; i < 40; i++) {
    try {
      const v = await httpGet(`http://127.0.0.1:${PORT}/json/version`);
      if (v.status === 200) { ready = true; break; }
    } catch (_) {}
    await sleep(250);
  }
  if (!ready) { chrome.kill(); fail("chromium CDP not ready"); }
  ok(`chromium CDP ready on ${PORT}`);

  const failed = [];
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
    let nextId = 1;
    const send = (method, params) => cdpSend(ws, nextId++, method, params);
    await send("Page.enable");
    await send("Runtime.enable");

    const viewports = [
      { name: "1200x900", w: 1200, h: 900, expectOverflow: false },
      { name: "1200x420", w: 1200, h: 420, expectOverflow: true },
      { name: "700x480", w: 700, h: 480, expectOverflow: true },
    ];

    const results = [];
    for (const vp of viewports) {
      await send("Emulation.setDeviceMetricsOverride", {
        width: vp.w, height: vp.h, deviceScaleFactor: 1, mobile: false,
      });
      await send("Page.navigate", { url: `${URL_}/` });
      for (let i = 0; i < 80; i++) {
        const st = await send("Runtime.evaluate", { expression: "document.readyState", returnByValue: true });
        if (st.result?.value === "complete") break;
        await sleep(100);
      }
      await sleep(300);
      await send("Runtime.evaluate", { expression: `document.querySelector('#fs-start summary')?.click()` });
      await sleep(150);
      const m = await send("Runtime.evaluate", { expression: MEASURE, returnByValue: true });
      const v = m.result?.value;
      results.push({ viewport: vp.name, ...v });
      if (v.error) { failed.push(`${vp.name}: ${v.error}`); continue; }

      if (!v.open) failed.push(`${vp.name}: start menu did not open on summary click`);
      if (v.rect.bottom > v.viewport.h + 1)
        failed.push(`${vp.name}: menu bottom ${v.rect.bottom} exceeds viewport ${v.viewport.h}`);
      if (v.rect.top < -1)
        failed.push(`${vp.name}: menu top ${v.rect.top} is above the viewport`);
      if (v.rect.height > Math.ceil(v.viewport.h * 0.7))
        failed.push(`${vp.name}: menu height ${v.rect.height} > 0.7 * ${v.viewport.h}`);
      if (!/auto|scroll/.test(v.overflowY))
        failed.push(`${vp.name}: overflow-y is ${v.overflowY}, not auto/scroll`);
      if (vp.expectOverflow && !v.contentOverflows)
        failed.push(`${vp.name}: content (${v.scrollHeight}px) did not overflow the ${v.clientHeight}px box, cap unproven`);
      if (v.entries < 5) failed.push(`${vp.name}: only ${v.entries} start entries`);

      const shot = await send("Page.captureScreenshot", { format: "png", captureBeyondViewport: false });
      const file = path.join(OUT, `${TAG}-start-menu-${vp.name}.png`);
      fs.writeFileSync(file, Buffer.from(shot.data, "base64"));
      ok(`${vp.name}: menu top=${v.rect.top} bottom=${v.rect.bottom} h=${v.rect.height} max=${v.maxHeight} overflow=${v.overflowY} scroll=${v.scrollHeight}/${v.clientHeight} entries=${v.entries} shot=${file}`);
    }

    console.log("MEASUREMENTS " + JSON.stringify(results));
    if (failed.length) {
      for (const f of failed) console.error("FAIL ", f);
      ws.close(); chrome.kill(); process.exit(1);
    }
    ok("start menu capped and internally scrolling in every viewport");
    ws.close();
  } finally {
    chrome.kill();
  }
}

main().catch((e) => fail(e.stack || String(e)));
