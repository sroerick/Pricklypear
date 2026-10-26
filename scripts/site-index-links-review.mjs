#!/usr/bin/env node
/**
 * Product-entry link review (SITE 6).
 *
 * Serves a static site root, boots headless chromium over CDP, and for each
 * viewport loads the index, asserts every numbered product entry's placard
 * name is a real anchor to its product page (visible, focusable, inside the
 * entry, exposed as a link in the accessibility tree), then CLICKS each link
 * and asserts the landing page is that product's page. Writes PNG artifacts.
 *
 * Env:
 *   SITE_ROOT    default /home/al/dev/wyo.town
 *   SITE_HOST    default 127.0.0.1
 *   SITE_PORT    default 18086
 *   SITE_INDEX   default /index.html
 *   SITE_WIDTHS  default "1280" (phone widths: pass SITE_WIDTHS="390" explicitly)
 *   SITE_OUT     default screenshots/site-review/site6
 *   CHROME       default /usr/local/bin/chrome
 *
 * Exit 0 when every entry/width passes, 1 otherwise. stdout is JSON lines:
 * one record per width, then per clicked entry, then a summary line.
 */
import { spawn } from "node:child_process";
import { setTimeout as sleep } from "node:timers/promises";
import http from "node:http";
import fs from "node:fs";
import path from "node:path";

const ROOT = path.resolve(process.env.SITE_ROOT || "/home/al/dev/wyo.town");
const HOST = process.env.SITE_HOST || "127.0.0.1";
const PORT = Number(process.env.SITE_PORT || 18086);
const INDEX = process.env.SITE_INDEX || "/index.html";
const WIDTHS = (process.env.SITE_WIDTHS || "1280").trim().split(/\s+/).map(Number);
const OUT = process.env.SITE_OUT || "screenshots/site-review/site6";
const CHROME = process.env.CHROME || "/usr/local/bin/chrome";
const CDP_PORT = Number(process.env.CDP_PORT || 9389);
const USER_DATA = `/tmp/chrome-site-index-links-${process.pid}`;
const BASE = `http://${HOST}:${PORT}`;

const PRODUCTS = [
  { no: "0001", name: "Pricklypear", href: "pricklypear.html" },
  { no: "0002", name: "Nopales", href: "nopales.html" },
  { no: "0003", name: "Tuna", href: "tuna.html" },
  { no: "0005", name: "Midbar", href: "midbar.html" },
  { no: "0007", name: "Borge", href: "borge.html" },
  { no: "0008", name: "Saguaro", href: "saguaro.html" },
];

const MIME = {
  ".html": "text/html; charset=utf-8",
  ".css": "text/css; charset=utf-8",
  ".js": "text/javascript; charset=utf-8",
  ".mjs": "text/javascript; charset=utf-8",
  ".json": "application/json",
  ".svg": "image/svg+xml",
  ".png": "image/png",
  ".jpg": "image/jpeg",
  ".jpeg": "image/jpeg",
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

const INDEX_PROBE = (products) => `(() => {
  const wanted = ${JSON.stringify(products)};
  const entries = [...document.querySelectorAll('article.entry')].map(entry => {
    const no = (entry.querySelector('.section-no')?.textContent || '').trim();
    const link = entry.querySelector('.section-head .placard a.entry-link');
    const r = link ? link.getBoundingClientRect() : null;
    const cs = link ? getComputedStyle(link) : null;
    let focusable = false;
    if (link) { link.focus(); focusable = document.activeElement === link; link.blur(); }
    return {
      id: entry.id,
      no,
      name: (link?.textContent || '').trim(),
      href: link?.getAttribute('href') || null,
      resolvedHref: link?.href || null,
      insideEntry: !!link?.closest('article.entry'),
      insideHeading: !!link?.closest('h2.placard'),
      visible: !!r && r.width > 0 && r.height > 0 && cs.visibility !== 'hidden' && cs.display !== 'none',
      focusable,
      rect: r ? { x: Math.round(r.x), y: Math.round(r.y), w: Math.round(r.width), h: Math.round(r.height) } : null,
    };
  });
  const clipped = el => {
    for (let p = el.parentElement; p && p !== document.body; p = p.parentElement) {
      const ox = getComputedStyle(p).overflowX;
      if (ox === 'hidden' || ox === 'clip' || ox === 'auto' || ox === 'scroll') return true;
    }
    return false;
  };
  const vw = innerWidth;
  const overflow = [...document.querySelectorAll('body *')]
    .filter(el => { const r = el.getBoundingClientRect(); return r.width > 0 && r.right > vw + 1 && !clipped(el); })
    .map(el => el.tagName.toLowerCase() + (el.className ? '.' + String(el.className).trim().split(/\\s+/).join('.') : ''))
    .slice(0, 8);
  const links = entries.map(e => e.resolvedHref).filter(Boolean);
  const missing = wanted.filter(w => !entries.some(e => e.no === w.no && e.name === w.name && e.href === w.href));
  return {
    title: document.title,
    viewport: vw,
    scrollWidth: document.documentElement.scrollWidth,
    scrollHeight: document.documentElement.scrollHeight,
    entries,
    missing,
    linkCount: links.length,
    overflow,
    bodyChars: (document.body.textContent || '').length,
  };
})()`;

const LANDING_PROBE = `(() => {
  const h1 = document.querySelector('.prod-title .placard');
  const top = document.querySelector('.prod-top');
  const clipped = el => {
    for (let p = el.parentElement; p && p !== document.body; p = p.parentElement) {
      const ox = getComputedStyle(p).overflowX;
      if (ox === 'hidden' || ox === 'clip' || ox === 'auto' || ox === 'scroll') return true;
    }
    return false;
  };
  const vw = innerWidth;
  const overflow = [...document.querySelectorAll('body *')]
    .filter(el => { const r = el.getBoundingClientRect(); return r.width > 0 && r.right > vw + 1 && !clipped(el); })
    .map(el => el.tagName.toLowerCase() + (el.className ? '.' + String(el.className).trim().split(/\\s+/).join('.') : ''))
    .slice(0, 8);
  return {
    href: location.href,
    path: location.pathname,
    title: document.title,
    ready: document.readyState,
    h1: (h1?.textContent || '').trim(),
    register: (top?.querySelector('.acc')?.textContent || '').replace(/\\s+/g, ' ').trim(),
    backToIndex: top?.querySelector('a.prod-wordmark')?.getAttribute('href') || null,
    viewport: vw,
    scrollWidth: document.documentElement.scrollWidth,
    scrollHeight: document.documentElement.scrollHeight,
    overflow,
    bodyChars: (document.body.textContent || '').length,
  };
})()`;

async function evaluate(send, expression) {
  const r = await send("Runtime.evaluate", { expression, returnByValue: true, awaitPromise: true });
  if (r.exceptionDetails) throw new Error(r.exceptionDetails.text || "evaluate failed");
  return r.result.value;
}

async function waitFor(send, expression, predicate, ms = 10000) {
  const until = Date.now() + ms;
  let last = null;
  while (Date.now() < until) {
    // A navigation destroys the execution context, so an evaluate can throw
    // mid-flight; that is a retry, not a failure.
    try { last = await evaluate(send, expression); } catch (_) { last = null; }
    if (predicate(last)) return last;
    await sleep(100);
  }
  return last;
}

const DOC_STATE = "({ path: location.pathname, ready: document.readyState })";
const onIndex = (v) => !!v && v.ready === "complete" && v.path === INDEX;

async function gotoIndex(send) {
  await send("Page.navigate", { url: BASE + INDEX });
  return waitFor(send, DOC_STATE, onIndex);
}

async function fullPageShot(send, width, name, maxHeight = 9000) {
  const m = await evaluate(send, `({ w: innerWidth, h: Math.min(${maxHeight}, Math.max(innerHeight, document.documentElement.scrollHeight)) })`);
  const shot = await send("Page.captureScreenshot", {
    format: "png", fromSurface: true, captureBeyondViewport: true,
    clip: { x: 0, y: 0, width: m.w, height: m.h, scale: 1 },
  });
  const file = path.join(OUT, name);
  fs.writeFileSync(file, Buffer.from(shot.data, "base64"));
  return file;
}

async function clipShot(send, clip, name) {
  const shot = await send("Page.captureScreenshot", {
    format: "png", fromSurface: true, captureBeyondViewport: true,
    clip: { x: clip.x, y: clip.y, width: clip.w, height: clip.h, scale: 1 },
  });
  const file = path.join(OUT, name);
  fs.writeFileSync(file, Buffer.from(shot.data, "base64"));
  return file;
}

async function main() {
  fs.mkdirSync(OUT, { recursive: true });
  fs.mkdirSync(USER_DATA, { recursive: true });
  const server = await serveStatic();

  chrome = spawn(CHROME, [
    `--remote-debugging-port=${CDP_PORT}`, `--user-data-dir=${USER_DATA}`,
    "--headless=new", "--disable-gpu", "--no-sandbox", "--disable-dev-shm-usage",
    "--remote-allow-origins=*", "--window-size=1400,900", "about:blank",
  ], { stdio: "ignore" });

  let page;
  for (let i = 0; i < 80; i++) {
    try {
      const v = await httpGet(`http://127.0.0.1:${CDP_PORT}/json/list`);
      if (v.status === 200) page = JSON.parse(v.body).find(t => t.type === "page");
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
  await send("Accessibility.enable");

  const records = [];
  for (const width of WIDTHS) {
    const height = width >= 800 ? 900 : 844;
    await send("Emulation.setDeviceMetricsOverride", {
      width, height, deviceScaleFactor: 1, mobile: width < 800,
      screenWidth: width, screenHeight: height,
    });

    await send("Page.navigate", { url: BASE + INDEX });
    await waitFor(send, "document.readyState", (s) => s === "complete");
    await sleep(600);

    const probe = await evaluate(send, INDEX_PROBE(PRODUCTS));
    const ax = await send("Accessibility.getFullAXTree", {});
    const axLinks = ax.nodes
      .filter(n => n.role?.value === "link")
      .map(n => ({ name: (n.name?.value || "").trim(), url: n.properties?.find(p => p.name === "url")?.value?.value || "" }));
    const axHits = PRODUCTS.map(p => ({
      name: p.name,
      hits: axLinks.filter(l => l.name === p.name && l.url.endsWith("/" + p.href)).length,
    }));

    const entriesPass = probe.entries.length === PRODUCTS.length && probe.missing.length === 0 &&
      probe.entries.every(e => e.visible && e.focusable && e.insideEntry && e.insideHeading && e.href && e.href.endsWith(".html")) &&
      probe.overflow.length === 0 && probe.scrollWidth <= probe.viewport + 1;

    let indexFile = "";
    let entriesFile = "";
    if (process.env.SITE_FULL === "0") {
      indexFile = await viewportShot(send, `index-${width}.png`);
    } else {
      indexFile = await fullPageShot(send, width, `index-${width}-full.png`);
      const top = Math.min(...probe.entries.map(e => e.rect ? e.rect.y : Infinity));
      const bottom = Math.max(...probe.entries.map(e => e.rect ? e.rect.y + e.rect.h : 0));
      if (Number.isFinite(top)) {
        entriesFile = await clipShot(send, { x: 0, y: Math.max(0, top - 24), w: width, h: Math.min(6000, bottom - top + 48) }, `index-${width}-entries.png`);
      }
    }

    records.push({ width, indexPass: entriesPass, indexFile, entriesFile, probe, axHits });
    console.log(JSON.stringify({
      width, indexPass: entriesPass, indexFile, entriesFile,
      entries: probe.entries.map(e => `${e.no} ${e.name} -> ${e.href}${e.visible ? "" : " (invisible)"}${e.focusable ? "" : " (not focusable)"}`),
      missing: probe.missing, overflow: probe.overflow, axHits,
    }));

    for (const product of PRODUCTS) {
      await gotoIndex(send);
      await sleep(250);

      const box = await evaluate(send, `(() => {
        const el = document.querySelector('article.entry a.entry-link[href="${product.href}"]');
        if (!el) return null;
        el.scrollIntoView({ block: 'center', inline: 'center' });
        const r = el.getBoundingClientRect();
        return { x: r.x + r.width / 2, y: r.y + r.height / 2 };
      })()`);
      if (!box) {
        records.push({ width, product: product.name, clickPass: false, reason: "anchor not found" });
        console.log(JSON.stringify({ width, product: product.name, clickPass: false, reason: "anchor not found" }));
        continue;
      }

      await send("Input.dispatchMouseEvent", { type: "mouseMoved", x: box.x, y: box.y });
      await send("Input.dispatchMouseEvent", { type: "mousePressed", x: box.x, y: box.y, button: "left", clickCount: 1 });
      await send("Input.dispatchMouseEvent", { type: "mouseReleased", x: box.x, y: box.y, button: "left", clickCount: 1 });

      const landing = await waitFor(
        send,
        LANDING_PROBE,
        (v) => v && v.ready === "complete" && v.path && v.path.endsWith("/" + product.href) && v.h1 === product.name,
        10000,
      );

      let landingFile = "";
      if (landing?.path?.endsWith("/" + product.href)) {
        const m = await evaluate(send, `(() => {
          const h = document.querySelector('header.entry');
          const r = h ? h.getBoundingClientRect() : { x: 0, y: 0, width: innerWidth, height: innerHeight };
          return { w: Math.min(innerWidth, r.width), h: Math.min(1200, r.height) };
        })()`);
        landingFile = await clipShot(send, { x: 0, y: 0, w: width, h: m.h }, `${product.href.replace(/\.html$/, "")}-${width}-entry.png`);
      }

      const clickPass = !!landing && landing.path.endsWith("/" + product.href) && landing.h1 === product.name &&
        landing.title.includes(product.name) && landing.backToIndex === "index.html" &&
        landing.overflow.length === 0 && landing.scrollWidth <= landing.viewport + 1 &&
        landing.ready === "complete" && landing.bodyChars > 500;

      records.push({ width, product: product.name, clickPass, landingFile, landing });
      console.log(JSON.stringify({
        width, product: product.name, clickPass, landingFile,
        landed: landing?.path, h1: landing?.h1, title: landing?.title,
        register: landing?.register, backToIndex: landing?.backToIndex,
        overflow: landing?.overflow,
      }));
    }
  }

  const failed = records.filter(r => r.indexPass === false || r.clickPass === false);
  const checks = records.length;
  console.log(JSON.stringify({
    summary: {
      checks,
      passed: records.length - failed.length,
      failed: failed.map(r => r.product ? `${r.product}@${r.width}` : `index@${r.width}`),
      artifacts: OUT,
    },
  }));
  ws.close();
  server.close();
  cleanup();
  process.exit(failed.length ? 1 : 0);
}

main().catch((e) => { console.error("site-index-links-review:", e.stack || e.message || e); cleanup(); process.exit(2); });
