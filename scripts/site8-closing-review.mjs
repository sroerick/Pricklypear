#!/usr/bin/env node
/**
 * Closing-field review (SITE 8, card e0320e4f).
 *
 * Serves the wyo.town site root, drives headless chromium over CDP, scrolls to
 * the bottom of each page at desktop and phone widths, and reports whether the
 * closing field fills the viewport there: section height, whether it still
 * reaches the document tail, whether its sign/placard/contact stay inside it,
 * and whether the fixed starfield canvas sits behind the content. Writes a
 * viewport PNG taken at the bottom of each page per (page, width).
 *
 * Env:
 *   SITE_ROOT    default /home/al/dev/wyo.town
 *   SITE_PORT    default 18088 (static server on 127.0.0.1)
 *   SITE_PAGES   default "index.html tuna.html pricklypear.html"
 *   SITE_WIDTHS  default "1440 390"
 *   SITE_OUT     default screenshots/site-review/site8
 *   CHROME       default /usr/local/bin/chrome
 *
 * Exit 0 when every asserted page/width passes, 1 otherwise. stdout is JSON
 * lines: one render record per (page, width), then a summary line.
 */
import { spawn } from "node:child_process";
import { setTimeout as sleep } from "node:timers/promises";
import http from "node:http";
import fs from "node:fs";
import path from "node:path";

const ROOT = path.resolve(process.env.SITE_ROOT || "/home/al/dev/wyo.town");
const HOST = "127.0.0.1";
const PORT = Number(process.env.SITE_PORT || 18088);
const PAGES = (process.env.SITE_PAGES || "index.html tuna.html pricklypear.html").trim().split(/\s+/);
const WIDTHS = (process.env.SITE_WIDTHS || "1440 390").trim().split(/\s+/).map(Number);
const OUT = process.env.SITE_OUT || "screenshots/site-review/site8";
const CHROME = process.env.CHROME || "/usr/local/bin/chrome";
const CDP_PORT = Number(process.env.CDP_PORT || 9391);
const USER_DATA = `/tmp/chrome-site8-${process.pid}`;
const BASE = `http://${HOST}:${PORT}`;
// pricklypear.html keeps its own grove closing (#closing, no .closing class);
// it is reported as a control, not asserted.
const CLOSING_PAGES = new Set(["index.html", "tuna.html"]);

const MIME = {
  ".html": "text/html; charset=utf-8",
  ".css": "text/css; charset=utf-8",
  ".js": "text/javascript; charset=utf-8",
  ".json": "application/json",
  ".svg": "image/svg+xml",
  ".png": "image/png",
  ".jpg": "image/jpeg",
  ".gif": "image/gif",
  ".webp": "image/webp",
  ".woff2": "font/woff2",
  ".woff": "font/woff",
  ".txt": "text/plain; charset=utf-8",
};

function serveStatic() {
  const server = http.createServer((req, res) => {
    let pathname;
    try { pathname = decodeURIComponent(new URL(req.url, BASE).pathname); }
    catch (_) { res.writeHead(400).end("bad request"); return; }
    if (pathname.endsWith("/")) pathname += "index.html";
    const file = path.resolve(ROOT, "." + pathname);
    if (file !== ROOT && !file.startsWith(ROOT + path.sep)) {
      res.writeHead(403).end("forbidden"); return;
    }
    fs.readFile(file, (err, data) => {
      if (err) { res.writeHead(404, { "content-type": "text/plain" }).end("not found"); return; }
      res.writeHead(200, {
        "content-type": MIME[path.extname(file).toLowerCase()] || "application/octet-stream",
        "cache-control": "no-store",
      });
      res.end(data);
    });
  });
  return new Promise((resolve, reject) => {
    server.once("error", reject);
    server.listen(PORT, HOST, () => resolve(server));
  });
}

let chrome;
const cleanup = () => { try { chrome?.kill(); } catch (_) {} };
process.on("exit", cleanup);

function httpGet(url) {
  return new Promise((resolve, reject) => {
    http.get(url, (res) => {
      let d = "";
      res.on("data", (c) => (d += c));
      res.on("end", () => resolve({ status: res.statusCode, body: d }));
    }).on("error", reject);
  });
}

const wsData = (ev) => { const d = ev?.data ?? ev; return Buffer.isBuffer(d) ? d.toString() : String(d); };
function cdpSend(ws, id, method, params = {}) {
  return new Promise((resolve, reject) => {
    const timer = setTimeout(() => done(new Error(`CDP timeout ${method}`)), 30000);
    const onMsg = (ev) => {
      try {
        const j = JSON.parse(wsData(ev));
        if (j.id === id) { done(j.error ? new Error(JSON.stringify(j.error)) : null, j.result); }
      } catch (_) {}
    };
    const onErr = (e) => done(e?.error || e);
    function done(err, result) {
      clearTimeout(timer);
      ws.removeEventListener("message", onMsg);
      ws.removeEventListener("error", onErr);
      err ? reject(err) : resolve(result);
    }
    ws.addEventListener("message", onMsg);
    ws.addEventListener("error", onErr);
    ws.send(JSON.stringify({ id, method, params }));
  });
}

// Everything a screenshot cannot settle: exact box, computed min-height, the
// children's containment, and the starfield's stacking context.
const PROBE = `(() => {
  const doc = document.documentElement;
  const vh = window.innerHeight, vw = window.innerWidth;
  const vv = window.visualViewport;
  const el = document.querySelector('.closing') || document.getElementById('closing');
  if (!el) return { found: false, viewport: vh, viewportW: vw, docHeight: doc.scrollHeight };
  const cs = getComputedStyle(el);
  const r = el.getBoundingClientRect();
  const kids = ['.sign', '.placard', '.closing-contact', '.back', '.brand'].map(sel => {
    const k = el.querySelector(sel);
    if (!k) return { sel, present: false };
    const kr = k.getBoundingClientRect();
    return { sel, present: true, text: (k.textContent || '').trim().slice(0, 60),
             top: Math.round(kr.top), bottom: Math.round(kr.bottom),
             left: Math.round(kr.left), right: Math.round(kr.right),
             h: Math.round(kr.height) };
  });
  const canvas = document.querySelector('canvas.starfield');
  const ccs = canvas ? getComputedStyle(canvas) : null;
  const cr = canvas ? canvas.getBoundingClientRect() : null;
  return {
    found: true, viewport: vh, viewportW: vw,
    visual: vv ? { w: Math.round(vv.width), h: Math.round(vv.height), scale: Math.round(vv.scale * 1000) / 1000 } : null,
    docWidth: doc.scrollWidth, dpr: window.devicePixelRatio,
    docHeight: Math.max(doc.scrollHeight, document.body.scrollHeight),
    scrollY: Math.round(window.scrollY),
    classes: String(el.className), id: el.id, title: document.title,
    minHeight: cs.minHeight, padding: cs.padding, maxWidth: cs.maxWidth,
    top: Math.round(r.top), height: Math.round(r.height), bottom: Math.round(r.bottom),
    bodyPaddingBottom: getComputedStyle(document.body).paddingBottom,
    kids,
    starfield: canvas ? { position: ccs.position, zIndex: ccs.zIndex,
                           ariaHidden: canvas.getAttribute('aria-hidden'),
                           w: Math.round(cr.width), h: Math.round(cr.height) } : null,
  };
})()`;

const scrollToBottom = `(() => { window.scrollTo(0, document.documentElement.scrollHeight); return Math.round(window.scrollY); })()`;

const SETTLED = `(() => ({ ready: document.readyState,
  h: document.documentElement.scrollHeight, w: document.documentElement.scrollWidth }))()`;

// Fonts, images and the starfield resize can move the document after load, so
// wait for 'complete' plus three consecutive equal (w,h) samples before
// measuring or scrolling. A single fixed sleep turned the first run's tuna.html
// measurement into a mid-layout snapshot.
async function settle(send) {
  await send("Runtime.evaluate", {
    expression: `new Promise((r) => {
      const done = () => Promise.all([...document.images].map((i) => i.complete ? 0 :
        new Promise((res) => { i.addEventListener('load', res, { once: true }); i.addEventListener('error', res, { once: true }); })))
        .then(() => r(true));
      if (document.readyState === 'complete') done();
      else window.addEventListener('load', done, { once: true });
    })`,
    awaitPromise: true, returnByValue: true,
  });
  let last = null, stable = 0;
  for (let i = 0; i < 40; i++) {
    const v = (await send("Runtime.evaluate", { expression: SETTLED, returnByValue: true })).result.value;
    if (last && v.h === last.h && v.w === last.w) stable++; else stable = 0;
    last = v;
    if (stable >= 3) return v;
    await sleep(100);
  }
  return last;
}

function judge(rec) {
  const assert = CLOSING_PAGES.has(rec.page);
  if (!rec.found) return { assert, failures: assert ? ["closing section not found"] : [] };
  const vh = rec.visual ? rec.visual.h : rec.viewport;
  const layoutVh = rec.viewport;
  const tailPad = parseFloat(rec.bodyPaddingBottom) || 0;
  const failures = [];
  if (assert) {
    const bottomInDoc = rec.bottom + rec.scrollY;
    const maxScroll = Math.max(0, rec.docHeight - layoutVh);
    if (Math.abs(rec.scrollY - maxScroll) > 1) failures.push(`scrollY ${rec.scrollY} != maxScroll ${maxScroll}`);
    if (rec.height < vh - 1) failures.push(`height ${rec.height} < viewport ${vh}`);
    if (Math.abs(parseFloat(rec.minHeight) - vh) > 1) failures.push(`computed min-height ${rec.minHeight} != ${vh}px`);
    if (rec.top > 1) failures.push(`top ${rec.top} > 1 at scroll bottom (section not up to viewport top)`);
    if (rec.bottom < vh - tailPad - 1) failures.push(`bottom ${rec.bottom} < viewport ${vh} minus body padding ${tailPad}`);
    const tailGap = rec.docHeight - bottomInDoc;
    if (tailGap > tailPad + 1) failures.push(`section ends ${tailGap}px above document tail (body padding ${tailPad})`);
    for (const sel of [".sign", ".placard"]) {
      if (!rec.kids.find((k) => k.sel === sel && k.present)) failures.push(`${sel} missing from closing`);
    }
    for (const k of rec.kids) {
      if (!k.present) continue;
      if (k.top < rec.top - 1 || k.bottom > rec.bottom + 1) failures.push(`${k.sel} outside the closing box`);
      if (k.right > rec.viewportW + 1) failures.push(`${k.sel} overflows horizontally (right ${k.right})`);
    }
    if (!rec.starfield) failures.push("starfield canvas missing");
    else {
      if (rec.starfield.position !== "fixed") failures.push(`starfield position=${rec.starfield.position}`);
      if (rec.starfield.ariaHidden !== "true") failures.push("starfield not aria-hidden");
      if (rec.starfield.h < vh - 1) failures.push(`starfield height ${rec.starfield.h} < viewport ${vh}`);
    }
  }
  return { assert, failures };
}

async function main() {
  fs.mkdirSync(OUT, { recursive: true });
  fs.mkdirSync(USER_DATA, { recursive: true });
  const server = await serveStatic();
  const records = [];
  try {
    chrome = spawn(CHROME, [
      `--remote-debugging-port=${CDP_PORT}`, `--user-data-dir=${USER_DATA}`,
      "--headless=new", "--disable-gpu", "--no-sandbox", "--disable-dev-shm-usage",
      "--hide-scrollbars", "--window-size=1440,900", "about:blank",
    ], { stdio: "ignore" });

    let page;
    for (let i = 0; i < 50; i++) {
      try {
        const v = await httpGet(`http://127.0.0.1:${CDP_PORT}/json/list`);
        if (v.status === 200) page = JSON.parse(v.body).find((t) => t.type === "page");
        if (page?.webSocketDebuggerUrl) break;
      } catch (_) {}
      await sleep(200);
    }
    if (!page?.webSocketDebuggerUrl) throw new Error("chromium CDP not reachable");

    const ws = new globalThis.WebSocket(page.webSocketDebuggerUrl);
    await new Promise((res, rej) => {
      const t = setTimeout(() => rej(new Error("ws open timeout")), 10000);
      ws.onopen = () => { clearTimeout(t); res(); };
      ws.onerror = (e) => { clearTimeout(t); rej(e.error || e); };
    });

    let id = 1;
    const send = (method, params) => cdpSend(ws, id++, method, params);
    await send("Page.enable");
    await send("Runtime.enable");

    for (const width of WIDTHS) {
      const height = width >= 800 ? 900 : 844;
      await send("Emulation.setDeviceMetricsOverride", {
        width, height, deviceScaleFactor: 1, mobile: width < 800,
        screenWidth: width, screenHeight: height,
      });
      for (const pageName of PAGES) {
        await send("Page.navigate", { url: `${BASE}/${pageName}` });
        await settle(send);
        const before = (await send("Runtime.evaluate", { expression: PROBE, returnByValue: true })).result.value;
        await send("Runtime.evaluate", { expression: scrollToBottom, returnByValue: true });
        await sleep(250);
        await send("Runtime.evaluate", { expression: scrollToBottom, returnByValue: true });
        await sleep(200);
        const after = (await send("Runtime.evaluate", { expression: PROBE, returnByValue: true })).result.value;
        const shot = await send("Page.captureScreenshot", { format: "png", captureBeyondViewport: false });
        const png = path.join(OUT, `${pageName.replace(/\.html$/, "")}-${width}-bottom.png`);
        fs.writeFileSync(png, Buffer.from(shot.data, "base64"));
        const rec = { page: pageName, width, ...after, before: {
          top: before.top, height: before.height, bottom: before.bottom,
          minHeight: before.minHeight, docHeight: before.docHeight } };
        const verdict = judge(rec);
        rec.assert = verdict.assert;
        rec.failures = verdict.failures;
        rec.pass = verdict.assert ? verdict.failures.length === 0 : null;
        rec.screenshot = png;
        records.push(rec);
        console.log(JSON.stringify(rec));
      }
    }

    const asserted = records.filter((r) => r.assert);
    const failed = asserted.filter((r) => !r.pass);
    console.log(JSON.stringify({ summary: true, asserted: asserted.length,
      passed: asserted.length - failed.length, failed: failed.length,
      pages: PAGES, widths: WIDTHS, out: OUT }));
    fs.writeFileSync(path.join(OUT, "summary.json"), JSON.stringify(records, null, 2));
    ws.close();
    return failed.length ? 1 : 0;
  } finally {
    try { server.close(); } catch (_) {}
  }
}

main().then((code) => process.exit(code || 0)).catch((e) => { console.error(String((e && e.stack) || e)); process.exit(2); });
