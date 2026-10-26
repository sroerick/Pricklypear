#!/usr/bin/env node
/**
 * Public-site contact-email review (SITE 7).
 *
 * Boots headless chromium over CDP, visits each public route at desktop and
 * phone widths, and asserts the contact email is a real mailto anchor inside
 * the page footer, exposed as a link in the accessibility tree, and laid out
 * without overflow. Writes a clipped footer PNG per (route, width).
 *
 * Env:
 *   SITE_BASE    default https://pricklypear.rocks
 *   SITE_ROUTES  default "/welcome /background /roadmap /guestbook"
 *   SITE_EMAIL   default roerick@wyo.town
 *   SITE_WIDTHS  default "1280 390"
 *   SITE_OUT     default screenshots/site-review/site7
 *   CHROME       default /usr/local/bin/chrome
 *
 * Exit 0 when every route/width passes, 1 otherwise. stdout is JSON lines:
 * one {route,width,...} record per render, then a {summary} line.
 */
import { spawn } from "node:child_process";
import { setTimeout as sleep } from "node:timers/promises";
import http from "node:http";
import fs from "node:fs";
import path from "node:path";

const BASE = (process.env.SITE_BASE || "https://pricklypear.rocks").replace(/\/$/, "");
const ROUTES = (process.env.SITE_ROUTES || "/welcome /background /roadmap /guestbook")
  .trim().split(/\s+/);
const EMAIL = process.env.SITE_EMAIL || "roerick@wyo.town";
const WIDTHS = (process.env.SITE_WIDTHS || "1280 390").trim().split(/\s+/).map(Number);
const OUT = process.env.SITE_OUT || "screenshots/site-review/site7";
const CHROME = process.env.CHROME || "/usr/local/bin/chrome";
const PORT = Number(process.env.CDP_PORT || 9371);
const USER_DATA = `/tmp/chrome-site-mailto-${process.pid}`;

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

// Real-anchor facts plus the layout facts a screenshot would not settle alone:
// is the anchor inside the footer, is it visible, does it take focus.
const PROBE_JS = (email) => `(() => {
  const want = ${JSON.stringify("mailto:" + email)};
  const anchors = [...document.querySelectorAll('a[href^="mailto:"]')].map(el => {
    const r = el.getBoundingClientRect();
    const cs = getComputedStyle(el);
    return {
      tag: el.tagName.toLowerCase(),
      href: el.getAttribute("href"),
      resolvedHref: el.href,
      text: (el.textContent || "").trim(),
      inFooter: !!el.closest(".layout-foot"),
      footerClass: el.parentElement ? el.parentElement.className : "",
      rect: { x: Math.round(r.x), y: Math.round(r.y), w: Math.round(r.width), h: Math.round(r.height) },
      visible: r.width > 0 && r.height > 0 && cs.visibility !== "hidden" && cs.display !== "none",
      cursor: cs.cursor,
      textDecoration: cs.textDecorationLine,
      color: cs.color,
    };
  });
  const target = anchors.find(a => a.resolvedHref === want) || null;
  let focusable = false;
  if (target) {
    const el = document.querySelector('a[href="' + want + '"]');
    if (el) { el.focus(); focusable = document.activeElement === el; }
  }
  const vw = window.innerWidth;
  const doc = document.documentElement;
    const clippedBy = (el) => {
      for (let p = el.parentElement; p && p !== document.body; p = p.parentElement) {
        const ox = getComputedStyle(p).overflowX;
        if (ox === "hidden" || ox === "clip" || ox === "auto" || ox === "scroll") return true;
      }
      return false;
    };
    const overflow = [...document.querySelectorAll("body *")]
      .filter(el => { const r = el.getBoundingClientRect(); return r.width > 0 && r.right > vw + 1 && !clippedBy(el); })
      .map(el => el.tagName.toLowerCase() + (el.className ? "." + String(el.className).trim().split(/\\s+/).join(".") : ""))
      .slice(0, 8);
  return {
    emailAnchors: anchors.length,
    target,
    focusable,
    footerText: (document.querySelector(".layout-foot")?.textContent || "").trim().slice(0, 240),
    viewport: vw,
    scrollWidth: doc.scrollWidth,
    overflow,
    title: document.title,
    };
  })()`;

async function axLinkNames(send, wantName) {
  const tree = await send("Accessibility.getFullAXTree", {});
  const links = tree.nodes.filter(n => n.role?.value === "link");
  const hits = links.filter(n => (n.name?.value || "").trim() === wantName);
  return { linkCount: links.length, hits: hits.map(n => ({ name: n.name.value, url: n.properties?.find(p => p.name === "url")?.value?.value || "" })) };
}

async function main() {
  fs.mkdirSync(OUT, { recursive: true });
  fs.mkdirSync(USER_DATA, { recursive: true });
  chrome = spawn(CHROME, [
    `--remote-debugging-port=${PORT}`, `--user-data-dir=${USER_DATA}`,
      "--headless=new", "--disable-gpu", "--no-sandbox", "--disable-dev-shm-usage",
      "--remote-allow-origins=*", "--window-size=1400,900", "about:blank",
  ], { stdio: "ignore" });

  let page;
  for (let i = 0; i < 60; i++) {
    try {
      const v = await httpGet(`http://127.0.0.1:${PORT}/json/list`);
      if (v.status === 200) { page = JSON.parse(v.body).find(t => t.type === "page"); }
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
    for (const route of ROUTES) {
      await send("Page.navigate", { url: BASE + route });
      await sleep(900);
      const r = await send("Runtime.evaluate", { expression: PROBE_JS(EMAIL), returnByValue: true });
      const probe = r.result.value;
      const ax = await axLinkNames(send, EMAIL);
      const slug = route.replace(/^\//, "").replace(/\//g, "-") || "index";

      let file = "";
      if (probe.target) {
        const rect = await send("Runtime.evaluate", {
          expression: `(() => { const el = document.querySelector('a[href=${JSON.stringify("mailto:" + EMAIL)}]');
            el.scrollIntoView({block: "center"}); const r = el.closest(".layout-foot").getBoundingClientRect();
            return { x: Math.max(0, r.x - 12), y: Math.max(0, r.y - 12), w: Math.min(window.innerWidth, r.width + 24), h: r.height + 24 }; })()`,
          returnByValue: true,
        });
        const clip = rect.result.value;
        const shot = await send("Page.captureScreenshot", {
          format: "png", fromSurface: true,
          clip: { x: clip.x, y: clip.y, width: clip.w, height: clip.h, scale: 1 },
        });
        file = path.join(OUT, `${slug}-${width}-foot.png`);
        fs.writeFileSync(file, Buffer.from(shot.data, "base64"));

        const full = await send("Page.captureScreenshot", {
          format: "png", fromSurface: true, captureBeyondViewport: true,
          clip: { x: 0, y: 0, width, height: Math.min(6000, Math.max(height, probe.scrollHeight || height)), scale: 1 },
        });
        fs.writeFileSync(path.join(OUT, `${slug}-${width}-full.png`), Buffer.from(full.data, "base64"));
      }

      const pass = !!probe.target && probe.target.tag === "a" && probe.target.text === EMAIL &&
        probe.target.inFooter && probe.target.visible && probe.focusable &&
        ax.hits.length > 0 && probe.overflow.length === 0 && probe.scrollWidth <= probe.viewport + 1;
      records.push({ route, width, pass, file, ...probe, axHits: ax.hits, axLinkCount: ax.linkCount });
      console.log(JSON.stringify({ route, width, pass, file, ...probe, axHits: ax.hits }));
    }
  }
  const failed = records.filter(r => !r.pass);
  console.log(JSON.stringify({ summary: { checked: records.length, passed: records.length - failed.length, failed: failed.map(r => `${r.route}@${r.width}`) } }));
  ws.close();
  cleanup();
  process.exit(failed.length ? 1 : 0);
}

main().catch((e) => { console.error("site-mailto-review:", e.message || e); cleanup(); process.exit(2); });
