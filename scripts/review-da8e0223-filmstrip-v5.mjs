#!/usr/bin/env node
/**
 * UI review harness for card da8e0223: filmstrip main-strip-v5.
 *
 * Drives real headless Chromium over CDP against a rendered habitat shell
 * and asserts the niri-strip invariants that no in-process simulator can
 * witness: main panes are side-by-side columns of one overflow row, the
 * focused column is on stage with its neighbours peeking, focus is scroll
 * (never a geometry lift), one-up stages a pane single and releases, width
 * presets resize the column, and the render adds no v5 JS or inline styles.
 * Writes screenshots and a JSON report.
 *
 * Env: PP_UI_URL (default http://127.0.0.1:18080), PP_UI_USER/PP_UI_PASS
 *      (default admin:admin), PP_UI_OUT (default screenshots),
 *      CHROME (default /usr/local/bin/chrome), CDP_PORT (default 9348).
 */
import { spawn } from "node:child_process";
import { setTimeout as sleep } from "node:timers/promises";
import http from "node:http";
import fs from "node:fs";
import path from "node:path";

const URL_ = (process.env.PP_UI_URL || "http://127.0.0.1:18080").replace(/\/$/, "");
const OUT = process.env.PP_UI_OUT || "screenshots";
const PORT = Number(process.env.CDP_PORT || 9348);
const CHROME = process.env.CHROME || "/usr/local/bin/chrome";
const TAG = "da8e0223-v5";
const USER = process.env.PP_UI_USER || "admin";
const PASS = process.env.PP_UI_PASS || "admin";
const BASIC = "Basic " + Buffer.from(`${USER}:${PASS}`).toString("base64");

const failed = [];
const ok = (m) => console.log("OK  ", m);
const bad = (m) => {
  failed.push(m);
  console.log("FAIL", m);
};

function httpGet(url) {
  return new Promise((resolve, reject) => {
    http
      .get(url, (res) => {
        let d = "";
        res.on("data", (c) => (d += c));
        res.on("end", () => resolve({ status: res.statusCode, body: d }));
      })
      .on("error", reject);
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
    const timer = setTimeout(() => {
      cleanup();
      reject(new Error(`CDP timeout ${method}`));
    }, 20000);
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
  const rect = (el) => {
    if (!el) return null;
    const r = el.getBoundingClientRect();
    return { l: Math.round(r.left), r: Math.round(r.right), t: Math.round(r.top), b: Math.round(r.bottom), w: Math.round(r.width), h: Math.round(r.height) };
  };
  const content = document.querySelector('#fs-content');
  const panes = [...document.querySelectorAll('#fs-content .fs-pane')];
  const cs = (el) => (el ? getComputedStyle(el) : null);
  return {
    count: panes.length,
    regions: panes.map((p) => p.dataset.region || null),
    classes: panes.map((p) => p.className),
    ids: panes.map((p) => p.dataset.pane || null),
    focused: panes.findIndex((p) => p.classList.contains('focused')),
    single: panes.map((p) => p.classList.contains('fs-single')),
    display: panes.map((p) => cs(p).display),
    position: panes.map((p) => cs(p).position),
    inlineStyle: panes.map((p) => p.getAttribute('style')),
    scrollMargin: panes.map((p) => cs(p).scrollMarginInlineStart || cs(p).scrollMarginLeft),
    scrollPadding: cs(content) ? (cs(content).scrollPaddingInlineStart || cs(content).scrollPaddingLeft) : null,
    overflowX: cs(content) ? cs(content).overflowX : null,
    content: rect(content),
    paneRects: panes.map(rect),
    offsetLeft: panes.map((p) => p.offsetLeft),
    scrollLeft: content ? content.scrollLeft : null,
    scrollWidth: content ? content.scrollWidth : null,
    clientWidth: content ? content.clientWidth : null,
    components: {
      shell: !!document.querySelector('#fs-shell'),
      surface: !!document.querySelector('#fs-surface'),
      content: !!content,
      rail: !!document.querySelector('#fs-rail'),
      railtabs: document.querySelectorAll('#fs-railtabs').length,
      coltabs: document.querySelectorAll('#fs-coltabs').length,
      start: document.querySelectorAll('#fs-start').length,
      sink: document.querySelectorAll('#fs-sink').length,
      panes: panes.length,
      heads: document.querySelectorAll('#fs-content .fs-head').length,
      optimisticAttrs: document.querySelectorAll('#fs-content [data-optimistic]').length,
    },
    scripts: [...document.scripts].map((s) => ({ src: s.src || '', inline: (s.textContent || '').slice(0, 80) })),
    inlineHandlers: document.querySelectorAll('[onclick],[onscroll],[onwheel]').length,
  };
})()`;

async function main() {
  fs.mkdirSync(path.join(OUT, TAG), { recursive: true });
  fs.mkdirSync(`/tmp/chrome-${TAG}`, { recursive: true });

  const chrome = spawn(
    CHROME,
    [
      `--remote-debugging-port=${PORT}`,
      `--user-data-dir=/tmp/chrome-${TAG}`,
      "--headless=new",
      "--disable-gpu",
      "--no-sandbox",
      "--disable-dev-shm-usage",
      "--window-size=1200,900",
      "about:blank",
    ],
    { stdio: "ignore" }
  );

  let ready = false;
  for (let i = 0; i < 60; i++) {
    try {
      const v = await httpGet(`http://127.0.0.1:${PORT}/json/version`);
      if (v.status === 200) {
        ready = true;
        break;
      }
    } catch (_) {}
    await sleep(250);
  }
  if (!ready) {
    chrome.kill();
    console.log("FAIL chromium CDP not ready");
    process.exit(1);
  }
  ok(`chromium CDP ready on ${PORT}`);

  let report = {};
  try {
    const list = JSON.parse((await httpGet(`http://127.0.0.1:${PORT}/json/list`)).body);
    const page = list.find((t) => t.type === "page") || list[0];
    if (!page?.webSocketDebuggerUrl) throw new Error("no page target");
    const ws = new WebSocket(page.webSocketDebuggerUrl);
    await new Promise((res, rej) => {
      const t = setTimeout(() => rej(new Error("ws open timeout")), 10000);
      ws.onopen = () => {
        clearTimeout(t);
        res();
      };
      ws.onerror = (e) => {
        clearTimeout(t);
        rej(e.error || e);
      };
    });
    let nextId = 1;
    const send = (method, params) => cdpSend(ws, nextId++, method, params);
    const ev = async (expression) =>
      (await send("Runtime.evaluate", { expression, returnByValue: true, awaitPromise: true })).result?.value;

    await send("Page.enable");
    await send("Runtime.enable");
    await send("Network.enable");
    await send("Network.setExtraHTTPHeaders", { headers: { Authorization: BASIC } });
    await send("Emulation.setDeviceMetricsOverride", { width: 1200, height: 900, deviceScaleFactor: 1, mobile: false });

    const shoot = async (name) => {
      const shot = await send("Page.captureScreenshot", { format: "png", captureBeyondViewport: false });
      const file = path.join(OUT, TAG, name);
      fs.writeFileSync(file, Buffer.from(shot.data, "base64"));
      ok(`screenshot ${file}`);
      return file;
    };

    await send("Page.navigate", { url: `${URL_}/filmstrip` });
    for (let i = 0; i < 120; i++) {
      const st = await ev("document.readyState");
      const has = await ev("!!document.querySelector('#fs-shell') && document.querySelectorAll('#fs-content .fs-pane').length >= 2");
      if (st === "complete" && has) break;
      await sleep(100);
    }
    await sleep(400);

    const m0 = await ev(MEASURE);
    report.before = m0;
    ok(`shell: panes=${m0.count} regions=${JSON.stringify(m0.regions)} focused=${m0.focused} components=${JSON.stringify(m0.components)}`);

    if (m0.count < 2) bad(`expected at least 2 strip columns, got ${m0.count} (open two main panes first)`);
    if (!m0.regions.every((r) => r === "main")) bad(`#fs-content carries non-main regions: ${JSON.stringify(m0.regions)}`);
    if (!/auto|scroll/.test(m0.overflowX || "")) bad(`#fs-content is not a horizontal overflow container (overflow-x=${m0.overflowX})`);
    if (m0.position.some((p) => p === "fixed")) bad(`a main pane is position:fixed (lift model, not the strip): ${JSON.stringify(m0.position)}`);
    if (m0.inlineStyle.some((s) => s)) bad(`inline style attribute on a pane: ${JSON.stringify(m0.inlineStyle)}`);
    if (m0.inlineHandlers > 0) bad(`${m0.inlineHandlers} inline event handler attrs on the shell`);

    const scrollJs = m0.scripts.filter((s) => /scroll\.js|filmstrip.*\.js/.test(s.src + s.inline));
    if (scrollJs.length) bad(`v5 introduced a script: ${JSON.stringify(scrollJs)}`);
    else ok(`no v5 JS: ${m0.scripts.length} script(s), none about the strip`);

    if (m0.scrollMargin.some((v) => !v || v === "0px")) bad(`pane scroll-margin missing: ${JSON.stringify(m0.scrollMargin)}`);
    else ok(`pane scroll-margin-inline=${JSON.stringify(m0.scrollMargin)}; content scroll-padding=${m0.scrollPadding}`);

    if (m0.focused < 0) bad("no focused column");
    else {
      const c = m0.content;
      const fr = m0.paneRects[m0.focused];
      if (fr.l < c.l - 1 || fr.r > c.r + 1) bad(`focused column not on stage: pane ${fr.l}..${fr.r} vs content ${c.l}..${c.r}`);
      else ok(`focused column on stage: pane ${fr.l}..${fr.r} inside content ${c.l}..${c.r}`);
      const others = m0.paneRects.filter((_, i) => i !== m0.focused);
      const peeking = others.filter((r) => r.r > c.l + 1 && r.l < c.r - 1);
      if (!peeking.length) bad(`no neighbouring column peeks into the stage: ${JSON.stringify(others)}`);
      else ok(`${peeking.length} neighbouring column(s) peek into the stage`);
    }

    for (const k of ["shell", "surface", "content", "rail", "start", "sink", "heads", "optimisticAttrs"]) {
      if (!m0.components[k]) bad(`registered component missing from the render: ${k}`);
    }
    if (m0.components.coltabs !== 1) bad(`#fs-coltabs missing (got ${m0.components.coltabs})`);
    if (m0.components.railtabs !== 1) bad(`#fs-railtabs missing (got ${m0.components.railtabs})`);
    if (m0.components.heads < m0.count) bad(`fewer pane heads than panes: ${m0.components.heads}/${m0.count}`);
    if (m0.components.optimisticAttrs < 1) bad("pane chrome carries no data-optimistic verb attrs (filmstrip/optimistic-attrs not composed)");

    const shotBefore = await shoot("strip-before-1200x900.png");
    report.screenshotBefore = shotBefore;

    // ACT 1: focus moves by scroll, not geometry. Click the other column's
    // chip in the designed #fs-coltabs control.
    if (m0.count >= 2 && m0.focused >= 0) {
      const other = m0.focused === 0 ? 1 : 0;
      const otherId = m0.ids[other];
      const clicked = await ev(`(() => {
        const chips = [...document.querySelectorAll('#fs-coltabs .fs-btn')];
        const chip = chips.find((b) => (b.getAttribute('hx-vals') || '').includes(${JSON.stringify(otherId)}));
        if (!chip) return false;
        chip.click();
        return true;
      })()`);
      if (!clicked) bad(`no #fs-coltabs chip targets pane ${other}`);
      await sleep(900);
      const m1 = await ev(MEASURE);
      report.afterFocus = m1;
      ok(`focus via coltabs: focused ${m0.focused} -> ${m1.focused}; scrollLeft ${m0.scrollLeft} -> ${m1.scrollLeft}; offsets ${JSON.stringify(m0.offsetLeft)} -> ${JSON.stringify(m1.offsetLeft)}`);
      if (m1.focused !== other) bad(`coltab click did not focus pane ${other} (focused=${m1.focused})`);
      if (JSON.stringify(m1.offsetLeft) !== JSON.stringify(m0.offsetLeft))
        bad(`focus moved pane geometry (offsetLeft ${JSON.stringify(m0.offsetLeft)} -> ${JSON.stringify(m1.offsetLeft)}) -- focus must be scroll`);
      const c = m1.content;
      const fr = m1.paneRects[m1.focused];
      if (fr.l < c.l - 1 || fr.r > c.r + 1) bad(`focused column not on stage after focus: ${fr.l}..${fr.r} vs ${c.l}..${c.r}`);
    }

    // ACT 2: one-up (alt+s) stages the focused pane single and toggles back.
    await ev("document.querySelector('#fs-shell').focus?.(); document.body.focus();");
    await send("Input.dispatchKeyEvent", { type: "keyDown", key: "s", code: "KeyS", windowsVirtualKeyCode: 83, modifiers: 1 });
    await send("Input.dispatchKeyEvent", { type: "keyUp", key: "s", code: "KeyS", windowsVirtualKeyCode: 83, modifiers: 1 });
    await sleep(900);
    const m2 = await ev(MEASURE);
    report.afterSingle = m2;
    const singleCount = m2.single.filter(Boolean).length;
    if (singleCount !== 1) bad(`one-up expected exactly 1 fs-single column, got ${singleCount}`);
    if (singleCount === 1) {
      const i = m2.single.indexOf(true);
      if (m2.display.some((d, j) => j !== i && d !== "none")) bad(`one-up left neighbours visible: ${JSON.stringify(m2.display)}`);
      else ok(`one-up: column ${i} full width (${m2.paneRects[i]?.w}px), neighbours display:none`);
    }
    report.screenshotSingle = await shoot("strip-one-up-1200x900.png");

    await send("Input.dispatchKeyEvent", { type: "keyDown", key: "s", code: "KeyS", windowsVirtualKeyCode: 83, modifiers: 1 });
    await send("Input.dispatchKeyEvent", { type: "keyUp", key: "s", code: "KeyS", windowsVirtualKeyCode: 83, modifiers: 1 });
    await sleep(900);
    const m3 = await ev(MEASURE);
    report.afterSingleOff = m3;
    if (m3.single.some(Boolean)) bad(`one-up did not toggle off: ${JSON.stringify(m3.classes)}`);
    else ok("one-up toggled back to the strip");

    // ACT 3: width presets resize the column (not the stage).
    const w0 = await ev(MEASURE);
    const clickWidth = await ev(`(() => {
      const p = document.querySelectorAll('#fs-content .fs-pane')[${w0.focused}];
      const b = p && [...p.querySelectorAll('.fs-head .fs-btn')].find((x) => (x.getAttribute('title') || '').includes('width'));
      if (!b) return false;
      b.click();
      return true;
    })()`);
    if (!clickWidth) bad("no width control on the focused pane head");
    await sleep(900);
    const w1 = await ev(MEASURE);
    report.afterWidth = w1;
    const cls0 = w0.classes[w0.focused] || "";
    const cls1 = w1.classes[w1.focused] || "";
    if (cls0 === cls1) bad(`width cycle did not change the focused column class (${cls1})`);
    else ok(`width cycle ${cls0} -> ${cls1}; pane width ${w0.paneRects[w0.focused]?.w} -> ${w1.paneRects[w1.focused]?.w}`);
    if (JSON.stringify(w1.content) !== JSON.stringify(w0.content)) bad(`width cycle resized the stage: ${JSON.stringify(w0.content)} -> ${JSON.stringify(w1.content)}`);

    report.screenshotAfter = await shoot("strip-after-1200x900.png");

    // Responsive peek: narrow viewport keeps the strip usable.
    await send("Emulation.setDeviceMetricsOverride", { width: 700, height: 480, deviceScaleFactor: 1, mobile: false });
    await sleep(400);
    const m4 = await ev(MEASURE);
    report.narrow = m4;
    if (!m4.components.shell) bad("shell missing at 700x480");
    if (m4.scrollWidth < m4.clientWidth - 1) bad(`strip is not scrollable when columns overflow at 700x480 (scrollWidth ${m4.scrollWidth} clientWidth ${m4.clientWidth})`);
    report.screenshotNarrow = await shoot("strip-700x480.png");

    fs.writeFileSync(path.join(OUT, TAG, "report.json"), JSON.stringify(report, null, 2));
    ok(`report ${path.join(OUT, TAG, "report.json")}`);
    ws.close();
  } catch (e) {
    bad(`harness error: ${e?.message || e}`);
  } finally {
    chrome.kill();
  }

  if (failed.length) {
    console.log(`\nFAILED ${failed.length} check(s):`);
    for (const f of failed) console.log("  -", f);
    process.exit(1);
  }
  console.log("\nALL GREEN: strip holds (columns, focus=scroll, one-up, widths, no v5 JS)");
}

main().catch((e) => {
  console.error("FAIL", e?.message || e);
  process.exit(1);
});
