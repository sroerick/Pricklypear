#!/usr/bin/env node
/**
 * Public-site screenshot + overflow review.
 *
 * Boots headless chromium (CDP), loads each public route at each width, and
 * reports the real layout width vs the viewport width plus the elements that
 * stick out past it. Writes a full-page PNG per (route, width) so a human or
 * agent can look at the result.
 *
 * Env:
 *   SITE_BASE    default https://pricklypear.rocks
 *   SITE_ROUTES  default "/welcome /background /roadmap /guestbook /signup"
 *   SITE_WIDTHS  default "1280 390"
 *   SITE_OUT     default screenshots/site-review
 *   CHROME       default /usr/local/bin/chrome
 *
 * Exit 0 always: it is a review, not a gate. The summary on stdout is
 * JSON-lines: one {route,width,viewport,scrollWidth,overflowing} record.
 */
import { spawn } from "node:child_process";
import { setTimeout as sleep } from "node:timers/promises";
import http from "node:http";
import fs from "node:fs";
import path from "node:path";

const BASE = (process.env.SITE_BASE || "https://pricklypear.rocks").replace(/\/$/, "");
const ROUTES = (process.env.SITE_ROUTES || "/welcome /background /roadmap /guestbook /signup")
  .trim().split(/\s+/);
const WIDTHS = (process.env.SITE_WIDTHS || "1280 390").trim().split(/\s+/).map(Number);
const OUT = process.env.SITE_OUT || "screenshots/site-review";
const CHROME = process.env.CHROME || "/usr/local/bin/chrome";
// Extra stylesheet appended after load: a review can verify a theme fix
// against the deployed page before the fix itself is promoted.
const EXTRA_CSS = process.env.SITE_EXTRA_CSS || "";
const PORT = Number(process.env.CDP_PORT || 9367);
const USER_DATA = `/tmp/chrome-site-review-${process.pid}`;

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

const evaluate = (expr) => `(() => { ${expr} })()`;

// Does this element sit inside an ancestor that clips horizontally? If so,
// sticking out is deliberate (the neocities marquee track, scrollable pre).
const METRICS_JS = `
  const vw = window.innerWidth;
  const doc = document.documentElement;
  const clipped = (el) => {
    for (let p = el.parentElement; p && p !== document.body; p = p.parentElement) {
      const ox = getComputedStyle(p).overflowX;
      if (ox === "hidden" || ox === "clip" || ox === "auto" || ox === "scroll") return true;
    }
    return false;
  };
  const bad = [];
  for (const el of document.body.querySelectorAll("*")) {
    const r = el.getBoundingClientRect();
    if (r.width <= 0) continue;
    if (r.right > vw + 1 && !clipped(el)) {
      const cls = typeof el.className === "string" && el.className.trim()
        ? "." + el.className.trim().split(/\\s+/).join(".") : "";
      bad.push({ sel: el.tagName.toLowerCase() + cls, right: Math.round(r.right), width: Math.round(r.width) });
    }
  }
  return { viewport: vw, scrollWidth: doc.scrollWidth,
           scrollHeight: Math.max(doc.scrollHeight, document.body.scrollHeight),
           overflowing: bad.slice(0, 10) };
`;

async function main() {
  fs.mkdirSync(OUT, { recursive: true });
  fs.mkdirSync(USER_DATA, { recursive: true });
  chrome = spawn(CHROME, [
    `--remote-debugging-port=${PORT}`, `--user-data-dir=${USER_DATA}`,
    "--headless=new", "--disable-gpu", "--no-sandbox", "--disable-dev-shm-usage",
    "--window-size=1400,900", "about:blank",
  ], { stdio: "ignore" });

  let page;
  for (let i = 0; i < 50; i++) {
    try {
      const v = await httpGet(`http://127.0.0.1:${PORT}/json/list`);
      if (v.status === 200) { page = JSON.parse(v.body).find((t) => t.type === "page"); }
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

  for (const width of WIDTHS) {
    const height = width >= 800 ? 900 : 844;
    await send("Emulation.setDeviceMetricsOverride", {
      width, height, deviceScaleFactor: 1, mobile: width < 800,
      screenWidth: width, screenHeight: height,
    });
    for (const route of ROUTES) {
        await send("Page.navigate", { url: BASE + route });
        await sleep(900);
        if (EXTRA_CSS) {
          await send("Runtime.evaluate", {
            expression: `(() => { const s = document.createElement("style");
              s.textContent = ${JSON.stringify(EXTRA_CSS)};
              document.head.appendChild(s); return true; })()`,
            returnByValue: true,
          });
          await sleep(150);
        }
      const m = await send("Runtime.evaluate", {
        expression: evaluate(METRICS_JS), returnByValue: true,
      });
      const metrics = m.result.value;
      const slug = route.replace(/^\//, "").replace(/\//g, "-") || "index";
      const shot = await send("Page.captureScreenshot", {
        format: "png", fromSurface: true, captureBeyondViewport: true,
        clip: { x: 0, y: 0, width, height: Math.min(6000, Math.max(height, metrics.scrollHeight || height)), scale: 1 },
      });
      const file = path.join(OUT, `${slug}-${width}.png`);
      fs.writeFileSync(file, Buffer.from(shot.data, "base64"));
      console.log(JSON.stringify({ route, width, ...metrics, file }));
    }
  }
  ws.close();
  cleanup();
}

main().catch((e) => { console.error("site-review:", e.message || e); cleanup(); process.exit(1); });
