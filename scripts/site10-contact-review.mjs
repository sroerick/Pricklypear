#!/usr/bin/env node
/**
 * Contact page review (SITE 10, card eaaf9117-2475-48bb-824d-84d06ff19e81).
 *
 * Serves the wyo.town site root, drives headless chromium over CDP, and checks
 * contact.html the way a reader meets it: the entry header sheet, the two
 * numbered sheets, the closing field, the mailto in both places it appears, the
 * link back to the catalog, and the brand form shared with a product page.
 *
 * Asserts, per page and width:
 *   - the entry opening is the product-page header sheet (sheet f-terr entry,
 *     prod-top wordmark, catalog crumb, placard title, prod-meta register)
 *   - section 1 carries the mailto in a spec register, visible and focusable
 *   - section 2 carries the four-step likeness ledger
 *   - the closing field carries the mailto and a link back to index.html
 *   - no horizontal overflow, no text spilling out of its own box
 *   - every class token on the page resolves to a rule in the three sheets
 *     (registered components, not ad-hoc markup)
 *   - the shared component computed styles match tuna.html (same brand form)
 *   - the accessibility tree exposes the mailto and the catalog link as links
 *
 * Env:
 *   SITE_ROOT    default /home/al/dev/wyo.town
 *   SITE_PORT    default 18090 (static server on 127.0.0.1)
 *   SITE_WIDTHS  default "1440 390"
 *   SITE_OUT     default screenshots/site-review/site10
 *   CHROME       default /usr/local/bin/chrome
 *
 * Exit 0 when every assertion passes, 1 otherwise. stdout is JSON lines: one
 * record per (page, width), then a summary line.
 */
import { spawn } from "node:child_process";
import { setTimeout as sleep } from "node:timers/promises";
import http from "node:http";
import fs from "node:fs";
import path from "node:path";

const ROOT = path.resolve(process.env.SITE_ROOT || "/home/al/dev/wyo.town");
const HOST = "127.0.0.1";
const PORT = Number(process.env.SITE_PORT || 18090);
const WIDTHS = (process.env.SITE_WIDTHS || "1440 390").trim().split(/\s+/).map(Number);
const OUT = process.env.SITE_OUT || "screenshots/site-review/site10";
const CHROME = process.env.CHROME || "/usr/local/bin/chrome";
const CDP_PORT = Number(process.env.CDP_PORT || 9401);
const USER_DATA = `/tmp/chrome-site10-${process.pid}`;
const BASE = `http://${HOST}:${PORT}`;
const MAILTO = "mailto:roerick@wyo.town";

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

// Facts a screenshot cannot settle alone: the box each part occupies, whether a
// link is actually inside the region the card names, whether text spills, which
// class tokens the markup carries, and the computed brand-form signature.
const PROBE = `(() => {
  const vw = window.innerWidth, vh = window.innerHeight, doc = document.documentElement;
  const norm = (s) => (s || '').replace(/\\s+/g, ' ').trim();
  const txt = (el) => el ? norm(el.textContent) : null;
  const box = (el) => { const r = el.getBoundingClientRect();
    return { top: Math.round(r.top), bottom: Math.round(r.bottom), left: Math.round(r.left),
             right: Math.round(r.right), w: Math.round(r.width), h: Math.round(r.height) }; };
  const vis = (el) => { if (!el) return null; const c = getComputedStyle(el); const r = el.getBoundingClientRect();
    return { display: c.display, visibility: c.visibility, opacity: c.opacity,
             w: Math.round(r.width), h: Math.round(r.height),
             insideViewportX: r.left >= -1 && r.right <= vw + 1 }; };
  const styleOf = (sel) => { const el = document.querySelector(sel); if (!el) return null;
    const c = getComputedStyle(el);
    return { fontSize: c.fontSize, fontFamily: c.fontFamily, fontWeight: c.fontWeight,
             letterSpacing: c.letterSpacing, lineHeight: c.lineHeight, textTransform: c.textTransform,
             padding: c.padding, margin: c.margin, border: c.border, borderBottom: c.borderBottom,
             borderBottomWidth: c.borderBottomWidth, borderBottomStyle: c.borderBottomStyle,
             display: c.display, gap: c.gap, alignItems: c.alignItems, justifyContent: c.justifyContent,
             flexDirection: c.flexDirection, gridTemplateColumns: c.gridTemplateColumns, opacity: c.opacity,
             color: c.color, backgroundColor: c.backgroundColor, minHeight: c.minHeight, textAlign: c.textAlign,
             textDecoration: c.textDecorationLine, maxWidth: c.maxWidth }; };
  const sheet = (el) => el ? { id: el.id, tag: el.tagName.toLowerCase(), classes: String(el.className),
    box: box(el), bg: getComputedStyle(el).backgroundColor, color: getComputedStyle(el).color,
    accent: getComputedStyle(el).getPropertyValue('--accent').trim() } : null;
  const header = document.querySelector('header#contact');
  const write = document.getElementById('write');
  const engaging = document.getElementById('engaging');
  const closing = document.getElementById('closing');
  const email = document.querySelector('a[href="mailto:roerick@wyo.town"]');
  const emails = [...document.querySelectorAll('a[href^="mailto:"]')].map(a => ({
    text: txt(a), href: a.getAttribute('href'), box: box(a), vis: vis(a),
    inRegister: !!a.closest('.prod-register'), inClosing: !!a.closest('.closing'),
    focusable: a.tabIndex >= 0 && !a.hasAttribute('disabled') }));
  const catalogLinks = [...document.querySelectorAll('a[href="index.html"]')].map(a => ({
    text: txt(a), box: box(a), inClosing: !!a.closest('.closing') }));
  const specRows = [...document.querySelectorAll('#write .prod-register .spec')].map(r => ({
    label: txt(r.querySelector('.label')), value: txt(r.querySelector('span:last-child')), box: box(r) }));
  const likenesses = [...document.querySelectorAll('#engaging .likeness')].map(l => ({
    no: txt(l.querySelector('.likeness-no')), body: txt(l.querySelector('.likeness-body')),
    box: box(l), insideSheet: !!l.closest('#engaging .likenesses') }));
  const closingKids = ['.brand', '.sign', '.placard', '.closing-contact', '.back'].map(sel => {
    const el = closing ? closing.querySelector(sel) : null;
    return el ? { sel, text: txt(el), box: box(el), vis: vis(el) } : { sel, missing: true }; });
  const overflow = [];
  // Text must stay inside its own box. Registration marks and rules sit outside
  // the trim by design, so measure the text-bearing parts, not every child.
  const textSel = '.sheet p, .sheet h1, .sheet h2, .sheet .lede, .sheet .spec, .sheet .acc,' +
    ' .sheet .mono, .sheet .likeness-body, .sheet .likeness-no, .sheet .section-no,' +
    ' .closing p, .closing a, .closing .sign, .closing .placard, .closing .closing-contact';
  for (const el of document.querySelectorAll(textSel)) {
    if (!el.getClientRects().length) continue;
    if (el.scrollWidth > el.clientWidth + 1)
      overflow.push({ tag: el.tagName.toLowerCase(), cls: String(el.className),
                      scrollW: el.scrollWidth, clientW: el.clientWidth, text: norm(el.textContent).slice(0, 50) });
  }
  const classes = new Set();
  for (const el of document.querySelectorAll('*')) for (const c of (el.classList || [])) classes.add(c);
  return {
    url: location.href, title: document.title, vw, vh, dpr: window.devicePixelRatio,
    docW: doc.scrollWidth, docH: Math.max(doc.scrollHeight, document.body.scrollHeight),
    hScroll: doc.scrollWidth > vw + 1,
    header: sheet(header),
    headerParts: header ? {
      wordmarkHref: header.querySelector('.prod-wordmark')?.getAttribute('href'),
      wordmarkText: txt(header.querySelector('.prod-wordmark .sign')),
      acc: txt(header.querySelector('.acc')),
      h1: txt(header.querySelector('h1')),
      h1Class: header.querySelector('h1')?.className,
      meta: [...header.querySelectorAll('.prod-meta .mono')].map(txt),
      regs: header.querySelectorAll('.reg').length,
    } : null,
    write: sheet(write),
    writeParts: write ? {
      no: txt(write.querySelector('.section-head .section-no')),
      h2: txt(write.querySelector('h2')),
      acc: txt(write.querySelector('.section-head .acc')),
      specRows,
      emailInside: !!(email && write.contains(email)),
      emailInRegister: write.querySelectorAll('.prod-register a[href="mailto:roerick@wyo.town"]').length,
      lede: txt(write.querySelector('.lede')),
      paragraphs: write.querySelectorAll('p').length,
    } : null,
    engaging: sheet(engaging),
    engagingParts: engaging ? {
      no: txt(engaging.querySelector('.section-head .section-no')),
      h2: txt(engaging.querySelector('h2')),
      acc: txt(engaging.querySelector('.section-head .acc')),
      lede: txt(engaging.querySelector('.lede')),
      likenesses,
    } : null,
    closing: sheet(closing),
    closingParts: closing ? {
      kids: closingKids,
      mailto: closing.querySelectorAll('a[href="mailto:roerick@wyo.town"]').length,
      back: (() => { const a = closing.querySelector('a.back');
        return a ? { href: a.getAttribute('href'), text: txt(a), box: box(a), vis: vis(a) } : null; })(),
    } : null,
    emails, catalogLinks: catalogLinks.map(l => ({ text: l.text, box: l.box, inClosing: l.inClosing })),
    overflow, classes: [...classes].sort(),
    styles: {
      headerSheet: styleOf('header.sheet'), h1: styleOf('h1.placard'), prodTop: styleOf('.prod-top'),
      prodMeta: styleOf('.prod-meta'), spec: styleOf('.prod-register .spec'),
      specLabel: styleOf('.prod-register .label'), sectionHead: styleOf('.section-head'),
      sectionNo: styleOf('.section-no'), lede: styleOf('.lede'), likeness: styleOf('.likeness'),
      likenessNo: styleOf('.likeness-no'), likenessBody: styleOf('.likeness-body'),
      closing: styleOf('.closing'), closingSign: styleOf('.closing .sign'), back: styleOf('.back'),
    },
  };
})()`;

const SETTLED = `(() => ({ ready: document.readyState,
  h: document.documentElement.scrollHeight, w: document.documentElement.scrollWidth }))()`;

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

function cssClassRegistry() {
  const sheets = ["brand/brand.css", "styles/main.css", "styles/product.css"];
  const set = new Set();
  for (const f of sheets) {
    const text = fs.readFileSync(path.join(ROOT, f), "utf8");
    for (const m of text.matchAll(/\.([A-Za-z][A-Za-z0-9_-]*)/g)) set.add(m[1]);
  }
  // A component class may also be a field modifier the sheets resolve by
  // inheritance (f-snow is the .sheet default) or a semantic hook the product
  // pages already carry (entry). Those count as registered when a product page
  // uses them; no page may invent a token the catalog does not already speak.
  for (const f of ["tuna.html", "pricklypear.html", "index.html"]) {
    const text = fs.readFileSync(path.join(ROOT, f), "utf8");
    for (const m of text.matchAll(/class='([^']*)'/g))
      for (const c of m[1].split(/\s+/)) if (c) set.add(c);
  }
  return set;
}

function judgeContact(rec, registry) {
  const f = [];
  const need = (cond, msg) => { if (!cond) f.push(msg); };
  const has = (s, c) => new RegExp("(^|\\s)" + c + "(\\s|$)").test(s || "");
  const h = rec.headerParts, w = rec.writeParts, e = rec.engagingParts, c = rec.closingParts;

  need(rec.header && rec.header.tag === "header" && has(rec.header.classes, "sheet") &&
       has(rec.header.classes, "f-terr") && has(rec.header.classes, "entry"),
       "entry header is not header.sheet.f-terr.entry");
  need(h && h.wordmarkHref === "index.html" && h.wordmarkText === "wyo.town",
       "entry wordmark missing or not linking to index.html");
  need(h && h.acc === "Catalog / Contact", `entry crumb is ${JSON.stringify(h && h.acc)}`);
  need(h && h.h1 === "Contact" && has(h.h1Class, "placard") && has(h.h1Class, "placard--xl"),
       "entry title is not a placard placard--xl reading Contact");
  need(h && h.meta.length >= 3, `entry prod-meta has ${h && h.meta.length} mono slots`);
  need(h && h.regs === 4, `entry sheet carries ${h && h.regs} registration marks`);

  need(rec.write && has(rec.write.classes, "sheet"), "section 1 is not a sheet");
  need(w && w.no === "§1" && w.h2 === "Write to us", "section 1 head is not §1 Write to us");
  need(w && w.specRows.length >= 3, `section 1 register has ${w && w.specRows.length} spec rows`);
  need(w && w.specRows.some(r => r.label === "Email" && r.value === "roerick@wyo.town"),
       "section 1 register has no Email spec row with the address");
  need(w && w.emailInRegister === 1 && w.emailInside, "mailto is not inside the section 1 register");
  need(w && w.paragraphs >= 3, `section 1 carries ${w && w.paragraphs} paragraphs`);

  need(rec.engaging && has(rec.engaging.classes, "sheet"), "section 2 is not a sheet");
  need(e && e.no === "§2" && e.h2 === "What engaging looks like",
       "section 2 head is not §2 What engaging looks like");
  need(e && e.likenesses.length === 4, `likeness ledger has ${e && e.likenesses.length} entries`);
  need(e && e.likenesses.every((l, i) => l.no === String(i + 1) && (l.body || "").length >= 40 && l.insideSheet),
       "likeness entries are not four numbered bodies inside the ledger");

  need(rec.closing && has(rec.closing.classes, "closing"), "closing field is not section.closing");
  need(c && c.mailto === 1, `closing field has ${c && c.mailto} mailto links`);
  need(c && c.back && c.back.href === "index.html" && c.back.text === "Back to the catalog",
       "closing field has no Back to the catalog link to index.html");
  need(c && c.kids.every(k => !k.missing), "closing field is missing one of its parts");

  // links are real, visible, and in the region the card names
  const mailtos = rec.emails.filter(x => x.href === MAILTO);
  need(mailtos.length >= 2, `page carries ${mailtos.length} mailto anchors (want 2: register + closing)`);
  need(mailtos.every(x => x.text === "roerick@wyo.town"), "a mailto anchor does not read roerick@wyo.town");
  need(mailtos.every(x => x.vis && x.vis.display !== "none" && x.vis.visibility !== "hidden" &&
                         Number(x.vis.opacity) > 0.5 && x.vis.w > 0 && x.vis.h > 0),
       "a mailto anchor is not visible");
  need(mailtos.every(x => x.focusable), "a mailto anchor is not focusable");
  need(mailtos.some(x => x.inRegister) && mailtos.some(x => x.inClosing),
       "mailto missing from register or closing field");
  need(rec.catalogLinks.length >= 2 && rec.catalogLinks.some(l => l.inClosing),
       "catalog links missing (wordmark + closing back link)");

  // layout: no horizontal scroll, no text out of its own box, sheets inside viewport
  need(!rec.hScroll, `document overflows horizontally (${rec.docW} > ${rec.vw})`);
  need(rec.overflow.length === 0, `text spills out of its box: ${JSON.stringify(rec.overflow).slice(0, 300)}`);
  for (const s of [rec.header, rec.write, rec.engaging, rec.closing].filter(Boolean)) {
    need(s.box.left >= -1 && s.box.right <= rec.vw + 1, `${s.id || s.tag} spans off-screen at ${rec.vw}px`);
    need(s.box.w > 100 && s.box.h > 100, `${s.id || s.tag} collapsed (${s.box.w}x${s.box.h})`);
  }
  if (c) for (const k of c.kids) {
    need(!k.missing && k.box.left >= -1 && k.box.right <= rec.vw + 1 && k.box.w > 0,
         `closing part ${k.sel} out of the box or viewport`);
  }

  // component registration: every class token is either a stylesheet rule or a
  // class the product pages already carry
  const unregistered = rec.classes.filter(cl => !registry.has(cl));
  need(unregistered.length === 0, `unregistered classes: ${unregistered.join(", ")}`);

  return f;
}

// Brand form: the shared components must compute to the same metrics on
// contact.html and the product pages. Per-sheet field/accent colours differ by
// design (one field per sheet, one accent per sheet), so compare typography and
// box metrics, not ink.
const PARITY = {
  h1: ["fontFamily", "fontWeight", "letterSpacing", "textTransform", "lineHeight"],
  prodTop: ["display", "alignItems", "gap", "padding", "borderBottomWidth", "borderBottomStyle"],
  prodMeta: ["display", "gap", "fontSize", "fontFamily"],
  spec: ["fontSize", "fontFamily", "padding", "borderBottomWidth", "borderBottomStyle"],
  specLabel: ["fontFamily", "fontSize", "opacity"],
  sectionHead: ["display", "alignItems", "gap", "padding", "borderBottomWidth", "borderBottomStyle"],
  sectionNo: ["fontFamily", "fontSize", "fontWeight"],
  lede: ["fontFamily", "fontWeight"],
  likeness: ["display", "padding", "borderBottomWidth", "borderBottomStyle"],
  likenessNo: ["fontFamily", "fontSize", "fontWeight"],
  likenessBody: ["fontFamily", "fontSize", "lineHeight"],
  closing: ["display", "flexDirection", "alignItems", "justifyContent", "gap", "padding", "maxWidth", "minHeight"],
  closingSign: ["fontFamily", "fontSize", "fontWeight", "letterSpacing"],
  back: ["display", "fontFamily", "fontSize", "textDecorationLine", "padding"],
};
// likenesses appear on pricklypear.html, not tuna.html
const PARITY_CONTROLS = { likeness: "pricklypear", likenessNo: "pricklypear", likenessBody: "pricklypear" };

function judgeParity(contact, product, other) {
  const f = [];
  for (const [sel, props] of Object.entries(PARITY)) {
    const controlName = PARITY_CONTROLS[sel];
    const control = controlName === "pricklypear" ? other : product;
    const a = contact.styles[sel], b = control.styles[sel];
    if (!a || !b) { f.push(`parity: ${sel} missing on ${!a ? "contact" : controlName || "product"} page`); continue; }
    for (const p of props) {
      if (a[p] !== b[p]) f.push(`parity ${sel}.${p}: contact=${JSON.stringify(a[p])} product=${JSON.stringify(b[p])}`);
    }
  }
  return f;
}

function axLinks(nodes) {
  return nodes.filter(n => n.role?.value === "link" && !n.ignored)
    .map(n => n.name?.value || "");
}

async function runPage(send, pageName, width) {
  const height = width >= 800 ? 900 : 844;
  await send("Emulation.setDeviceMetricsOverride", {
    width, height, deviceScaleFactor: 1, mobile: width < 800,
    screenWidth: width, screenHeight: height,
  });
  await send("Page.navigate", { url: `${BASE}/${pageName}` });
  await settle(send);
  const rec = (await send("Runtime.evaluate", { expression: PROBE, returnByValue: true })).result.value;
  await send("Accessibility.enable");
  const ax = await send("Accessibility.getFullAXTree");
  rec.axLinks = axLinks(ax.nodes || []);
  const metrics = await send("Page.getLayoutMetrics");
  const size = metrics.cssContentSize;
  const shot = await send("Page.captureScreenshot", {
    format: "png", captureBeyondViewport: true,
    clip: { x: 0, y: 0, width: Math.ceil(size.width), height: Math.ceil(size.height), scale: 1 },
  });
  const png = path.join(OUT, `${pageName.replace(/\.html$/, "")}-${width}-full.png`);
  fs.writeFileSync(png, Buffer.from(shot.data, "base64"));
  rec.screenshot = png;
  rec.page = pageName;
  rec.width = width;
  if (pageName === "contact.html") {
    rec.focus = (await send("Runtime.evaluate", { returnByValue: true,
      expression: `(() => { const a = document.querySelector('#write .prod-register a[href="mailto:roerick@wyo.town"]');
        if (!a) return { active: false, why: 'mailto anchor missing' };
        a.focus();
        return { active: document.activeElement === a, href: a.getAttribute('href') }; })()` })).result.value;
  }
  return rec;
}

async function captureRegion(send, sel, file) {
  const r = (await send("Runtime.evaluate", {
    expression: `(() => { const el = document.querySelector(${JSON.stringify(sel)}); if (!el) return null;
      const b = el.getBoundingClientRect();
      return { x: b.left + window.scrollX, y: b.top + window.scrollY, width: b.width, height: b.height }; })()`,
    returnByValue: true,
  })).result.value;
  if (!r) return null;
  const shot = await send("Page.captureScreenshot", {
    format: "png", captureBeyondViewport: true,
    clip: { x: Math.max(0, r.x - 8), y: Math.max(0, r.y - 8),
            width: Math.ceil(r.width + 16), height: Math.ceil(r.height + 16), scale: 1 },
  });
  fs.writeFileSync(file, Buffer.from(shot.data, "base64"));
  return file;
}

async function main() {
  fs.mkdirSync(OUT, { recursive: true });
  fs.mkdirSync(USER_DATA, { recursive: true });
  const registry = cssClassRegistry();
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
      const contact = await runPage(send, "contact.html", width);
      const regions = [];
      if (width === WIDTHS[0]) {
          await send("Page.navigate", { url: `${BASE}/contact.html` });
          await settle(send);
        for (const [sel, name] of [["#write", "write"], ["#engaging", "engaging"], ["#closing", "closing"]]) {
          const p = await captureRegion(send, sel, path.join(OUT, `contact-${width}-${name}.png`));
          if (p) regions.push(p);
        }
      }
        await send("Runtime.evaluate", { returnByValue: true,
          expression: `(() => { const a = document.querySelector('.closing a.back'); if (!a) return false;
            a.scrollIntoView({ block: 'center' }); a.click(); return true; })()` });
        await sleep(600);
        contact.afterBack = (await send("Runtime.evaluate", { returnByValue: true,
          expression: `({ href: location.href, title: document.title })` })).result.value;
        const product = await runPage(send, "tuna.html", width);
        const other = await runPage(send, "pricklypear.html", width);
        const failures = judgeContact(contact, registry);
        const parity = judgeParity(contact, product, other);
      const axFailures = [];
      const axNames = contact.axLinks.map(n => n.toLowerCase());
      if (!axNames.some(n => n.includes("roerick@wyo.town"))) axFailures.push("a11y: no link named roerick@wyo.town");
      if (!axNames.some(n => n.includes("back to the catalog"))) axFailures.push("a11y: no link named Back to the catalog");
      if (!axNames.some(n => /^wyo\.town\s*$/.test(n))) axFailures.push("a11y: no wordmark link named wyo.town");
      if (!contact.focus?.active) axFailures.push("interaction: register mailto does not take focus");
      if (!contact.afterBack?.href?.endsWith("/index.html")) axFailures.push(`interaction: Back to the catalog did not navigate to index.html (${contact.afterBack?.href})`);
      contact.failures = [...failures, ...parity, ...axFailures];
        contact.parity = { compared: Object.keys(PARITY), failures: parity };
      contact.regions = regions;
      contact.pass = contact.failures.length === 0;
      records.push(contact);
      const line = { page: "contact.html", width, pass: contact.pass, failures: contact.failures,
        doc: `${contact.docW}x${contact.docH}`, sheets: [contact.header?.classes, contact.write?.classes,
        contact.engaging?.classes, contact.closing?.classes], screenshot: contact.screenshot, regions };
      console.log(JSON.stringify(line));
    }

    const failed = records.filter(r => !r.pass);
    const summary = { summary: true, widths: WIDTHS, passed: records.length - failed.length,
      failed: failed.length, out: OUT, screenshots: records.map(r => r.screenshot) };
    console.log(JSON.stringify(summary));
    fs.writeFileSync(path.join(OUT, "summary.json"), JSON.stringify(records, null, 2));
    ws.close();
    return failed.length ? 1 : 0;
  } finally {
    try { server.close(); } catch (_) {}
  }
}

main().then((code) => process.exit(code || 0)).catch((e) => { console.error(String((e && e.stack) || e)); process.exit(2); });
