#!/usr/bin/env node
/**
 * SITE 4 review: balanced text blocks at desktop and phone widths
 * (card 5e7327aa, libs/layout 059cdc1 + mobile fixes 54c2e46).
 *
 * Loads the public routes in headless chromium over CDP at 1280 and 390 and
 * asserts, per (route, width):
 *   - document.scrollWidth fits the viewport and no element sticks out
 *     (text stays inside the component boxes),
 *   - the balanced-text contract is live: hero/card/section titles compute
 *     text-wrap:balance, tagline/card-body/section-body compute
 *     text-wrap:pretty,
 *   - the page is composed of the registered layout components (the class
 *     hooks emitted by layout/hero, layout/cards, layout/card,
 *     layout/section, layout/cta, layout/foot), with semantic headings,
 *   - ACT: posting a bad email to /signup re-renders the page with the error
 *     inside it, layout hooks and balanced-text styles intact, no overflow.
 *
 * Writes element-clipped PNGs (readable at natural scale) plus one full-page
 * PNG per (route, width).
 *
 * Env:
 *   SITE_BASE  default https://pricklypear.rocks
 *   SITE_ROUTES default "/welcome /background /roadmap /guestbook /signup"
 *   SITE_WIDTHS default "1280 390"
 *   SITE_OUT   default screenshots/site4-review
 *   CHROME     default /usr/local/bin/chrome
 *   CDP_PORT   default 9371
 *
 * Exit 0 when every assertion passes, 1 otherwise. stdout is JSON lines:
 * one record per (route, width) plus a summary line.
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
const OUT = process.env.SITE_OUT || "screenshots/site4-review";
const CHROME = process.env.CHROME || "/usr/local/bin/chrome";
const PORT = Number(process.env.CDP_PORT || 9371);
const USER_DATA = `/tmp/chrome-site4-${process.pid}`;

const TEXT_CLASSES = [
  ".layout-hero-title", ".layout-hero-tagline",
  ".layout-card-title", ".layout-card-body",
  ".layout-section-title", ".layout-section-body",
];
const BALANCED = [
  [".layout-hero-title", "balance"],
  [".layout-card-title", "balance"],
  [".layout-section-title", "balance"],
  [".layout-hero-tagline", "pretty"],
  [".layout-card-body", "pretty"],
  [".layout-section-body", "pretty"],
];
const COMPONENT_HOOKS = [
  ".layout-page", ".layout-hero", ".layout-hero-title", ".layout-hero-tagline",
  ".layout-cards", ".layout-card", ".layout-card-title", ".layout-card-body",
  ".layout-section", ".layout-section-title", ".layout-section-body",
  ".layout-cta", ".layout-foot",
];

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
        if (j.id === id) done(j.error ? new Error(JSON.stringify(j.error)) : null, j.result);
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

// Layout + composition, evaluated in the page.
const INSPECT_JS = `
  const vw = window.innerWidth;
  const doc = document.documentElement;
  const cs = (el, prop) => getComputedStyle(el).getPropertyValue(prop).trim();
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
    const styles = ${JSON.stringify(BALANCED)}.map(([sel, want]) => {
      const el = document.querySelector(sel);
      const got = el ? cs(el, "text-wrap") : null;
      return { sel, want, got, present: !!el, ok: !el || got === want };
    });
  const hooks = {};
  for (const sel of ${JSON.stringify(COMPONENT_HOOKS)}) hooks[sel] = document.querySelectorAll(sel).length;
  const headings = {};
  for (const [sel, tag] of [[".layout-hero-title", "H1"], [".layout-section-title", "H2"], [".layout-card-title", "H3"]]) {
    const el = document.querySelector(sel);
    headings[sel] = el ? { tag: el.tagName, want: tag, text: (el.textContent || "").trim(), ok: el.tagName === tag && (el.textContent || "").trim().length > 0 } : null;
  }
  const contained = [];
  for (const sel of ${JSON.stringify(TEXT_CLASSES)}) {
    for (const el of document.querySelectorAll(sel)) {
      const r = el.getBoundingClientRect();
      const p = el.parentElement ? el.parentElement.getBoundingClientRect() : null;
      contained.push({
        sel,
        text: (el.textContent || "").trim().slice(0, 48),
        insideViewport: r.right <= vw + 1 && r.left >= -1,
        insideParent: !p || (r.right <= p.right + 1 && r.left >= p.left - 1),
        width: Math.round(r.width), height: Math.round(r.height)
      });
    }
  }
  return {
    url: location.pathname,
    viewport: vw,
    scrollWidth: doc.scrollWidth,
    scrollHeight: Math.max(doc.scrollHeight, document.body.scrollHeight),
    overflowing: bad.slice(0, 10),
    styles, hooks, headings, contained,
    err: (document.querySelector(".err") || {}).textContent || null,
    title: document.title,
  };
`;

const evaluate = (expr) => `(() => { ${expr} })()`;

async function shotRect(send, rect, file) {
  const clip = {
    x: Math.max(0, rect.x), y: Math.max(0, rect.y),
    width: Math.max(1, Math.min(rect.width, 4000)),
    height: Math.max(1, Math.min(rect.height, 4000)), scale: 1,
  };
  const shot = await send("Page.captureScreenshot", {
    format: "png", fromSurface: true, captureBeyondViewport: true, clip,
  });
  fs.writeFileSync(file, Buffer.from(shot.data, "base64"));
  return file;
}

async function rectOf(send, sel, index = 0) {
  const r = await send("Runtime.evaluate", {
    expression: `(() => { const el = document.querySelectorAll(${JSON.stringify(sel)})[${index}];
      if (!el) return null; const b = el.getBoundingClientRect();
      return { x: b.x + window.scrollX, y: b.y + window.scrollY, width: b.width, height: b.height }; })()`,
    returnByValue: true,
  });
  return r.result.value;
}

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

  const failures = [];

  const inspect = async () => {
    const m = await send("Runtime.evaluate", { expression: evaluate(INSPECT_JS), returnByValue: true });
    return m.result.value;
  };

  const assertPage = async (route, width, label) => {
    const m = await inspect();
    const problems = [];
    if (m.scrollWidth > m.viewport + 1) problems.push(`scrollWidth ${m.scrollWidth} > viewport ${m.viewport}`);
    if (m.overflowing.length) problems.push(`overflowing: ${JSON.stringify(m.overflowing)}`);
    for (const s of m.styles) if (!s.ok) problems.push(`text-wrap ${s.sel}: want ${s.want}, got ${s.got}`);
    for (const c of m.contained) {
      if (!c.insideViewport) problems.push(`${c.sel} outside viewport: "${c.text}"`);
      if (!c.insideParent) problems.push(`${c.sel} outside parent: "${c.text}"`);
    }
    for (const [sel, h] of Object.entries(m.headings)) {
      if (h && (!h.ok || h.tag !== h.want)) problems.push(`heading ${sel}: tag ${h.tag} want ${h.want}`);
    }
    for (const p of problems) failures.push(`${label} ${route}@${width}: ${p}`);
    return { m, problems };
  };

  for (const width of WIDTHS) {
    const height = width >= 800 ? 900 : 844;
    await send("Emulation.setDeviceMetricsOverride", {
      width, height, deviceScaleFactor: 1, mobile: width < 800,
      screenWidth: width, screenHeight: height,
    });

    for (const route of ROUTES) {
      await send("Page.navigate", { url: BASE + route });
      await sleep(900);
      const slug = route.replace(/^\//, "").replace(/\//g, "-") || "index";

      const full = await send("Page.captureScreenshot", {
        format: "png", fromSurface: true, captureBeyondViewport: true,
        clip: { x: 0, y: 0, width, height: Math.min(6000, 900), scale: 1 },
      });
      const fullFile = path.join(OUT, `${slug}-${width}.png`);
      const docH = (await inspect()).scrollHeight;
      const fullShot = await send("Page.captureScreenshot", {
        format: "png", fromSurface: true, captureBeyondViewport: true,
        clip: { x: 0, y: 0, width, height: Math.min(6000, Math.max(height, docH || height)), scale: 1 },
      });
      fs.writeFileSync(fullFile, Buffer.from(fullShot.data, "base64"));
      void full;

      const { m, problems } = await assertPage(route, width, "layout");

      // Component composition: the registered layout defs emit these hooks.
      const required = route === "/welcome"
        ? { ".layout-page": 1, ".layout-hero": 1, ".layout-hero-title": 1, ".layout-hero-tagline": 1,
            ".layout-cards": 1, ".layout-card": 2, ".layout-card-title": 2, ".layout-card-body": 2,
            ".layout-section": 2, ".layout-section-title": 2, ".layout-section-body": 2,
            ".layout-cta": 1, ".layout-foot": 1 }
        : { ".layout-page": 1, ".layout-hero": 1, ".layout-hero-title": 1, ".layout-foot": 1 };
      for (const [sel, min] of Object.entries(required)) {
        if ((m.hooks[sel] || 0) < min) {
          const p = `component hook ${sel}: ${m.hooks[sel] || 0} < ${min}`;
          problems.push(p); failures.push(`${route}@${width}: ${p}`);
        }
      }

      // Natural-scale crops of the changed region, one per component kind.
      const crops = [];
      const want = route === "/welcome"
        ? [[".layout-hero", 0, "hero"], [".layout-card", 0, "card"], [".layout-section", 0, "section"]]
        : [[".layout-hero", 0, "hero"]];
      for (const [sel, idx, name] of want) {
        const r = await rectOf(send, sel, idx);
        if (r && r.width > 0) crops.push(await shotRect(send, r, path.join(OUT, `${slug}-${name}-${width}.png`)));
      }

      const rec = {
        route, width, viewport: m.viewport, scrollWidth: m.scrollWidth, scrollHeight: m.scrollHeight,
        overflowing: m.overflowing.length, textWrap: m.styles.map((s) => `${s.sel}=${s.got}`),
        hooks: m.hooks, title: m.title, errors: m.err, crops, file: fullFile, ok: problems.length === 0, problems,
      };
      console.log(JSON.stringify(rec));
    }
  }

  // ACT: submit a bad email to /signup; the handler must re-render the page
  // with the error inside it and the layout/text contract intact.
  for (const width of WIDTHS) {
    const height = width >= 800 ? 900 : 844;
    await send("Emulation.setDeviceMetricsOverride", {
      width, height, deviceScaleFactor: 1, mobile: width < 800, screenWidth: width, screenHeight: height,
    });
    await send("Page.navigate", { url: BASE + "/signup" });
    await sleep(900);
    const act = await send("Runtime.evaluate", {
      expression: `(() => {
        const input = document.querySelector('input[name="email"]');
        const form = document.querySelector('form');
        if (!input || !form) return { ok: false, why: 'signup form not found' };
        const setter = Object.getOwnPropertyDescriptor(HTMLInputElement.prototype, 'value').set;
        setter.call(input, 'not-an-email');
        input.dispatchEvent(new Event('input', { bubbles: true }));
        input.dispatchEvent(new Event('change', { bubbles: true }));
        form.requestSubmit();
        return { ok: true, value: input.value };
      })()`,
      returnByValue: true,
    });
    const submitted = act.result.value;
    let after = null;
    for (let i = 0; i < 40; i++) {
      await sleep(250);
      const r = await send("Runtime.evaluate", {
        expression: `(() => ({ ready: document.readyState, err: (document.querySelector('.err')||{}).textContent || null }))()`,
        returnByValue: true,
      });
      after = r.result.value;
      if (after.ready === "complete" && after.err) break;
    }
    const { m, problems } = await assertPage("/signup", width, "act");
    if (!submitted.ok) { problems.push(submitted.why); failures.push(`act /signup@${width}: ${submitted.why}`); }
    if (!m.err || !/real-looking email/.test(m.err)) {
      const p = `post-submit .err missing: ${JSON.stringify(m.err)}`;
      problems.push(p); failures.push(`act /signup@${width}: ${p}`);
    }
    if ((m.hooks[".layout-page"] || 0) < 1 || (m.hooks[".layout-hero"] || 0) < 1) {
      const p = "post-submit layout hooks lost";
      problems.push(p); failures.push(`act /signup@${width}: ${p}`);
    }
    const cropR = await rectOf(send, ".layout-hero", 0);
    const crop = cropR && cropR.width > 0
      ? await shotRect(send, cropR, path.join(OUT, `signup-after-submit-hero-${width}.png`)) : null;
    console.log(JSON.stringify({
      route: "/signup(POST bad email)", width, submitted,
      err: m.err, hooks: m.hooks, scrollWidth: m.scrollWidth, viewport: m.viewport,
      textWrap: m.styles.map((s) => `${s.sel}=${s.got}`), crop, ok: problems.length === 0, problems,
    }));
  }

  const summary = { checked: ROUTES.length * WIDTHS.length + WIDTHS.length, failures: failures.length, failed: failures };
  console.log(JSON.stringify(summary));
  ws.close();
  cleanup();
  process.exit(failures.length ? 1 : 0);
}

main().catch((e) => { console.error("site4-review:", e.message || e); cleanup(); process.exit(1); });
