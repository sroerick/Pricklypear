#!/usr/bin/env node
/**
 * UI review harness for card a5bdd8ce: the filmstrip shell is a window
 * manager surface -- the page itself never scrolls; panes, rails and menus
 * scroll internally.
 *
 * Drives real headless Chromium over CDP against a rendered filmstrip
 * shell, attempts page scrolling three ways (script scroll, wheel, End
 * key), asserts the document does not move, measures the shell box against
 * the viewport, confirms the registered component vocabulary is on the
 * page, and writes screenshots.
 *
 * Env: PP_UI_URL (default http://127.0.0.1:18091), PP_UI_OUT (default
 * screenshots), CHROME (default /usr/local/bin/chrome), CDP_PORT (9337).
 */
import { spawn } from "node:child_process";
import { setTimeout as sleep } from "node:timers/promises";
import http from "node:http";
import fs from "node:fs";
import path from "node:path";

const URL_ = (process.env.PP_UI_URL || "http://127.0.0.1:18091").replace(/\/$/, "");
const OUT = process.env.PP_UI_OUT || "screenshots";
const PORT = Number(process.env.CDP_PORT || 9337);
const CHROME = process.env.CHROME || "/usr/local/bin/chrome";
const TAG = "a5bdd8ce";

function fail(msg) {
  console.error("FAIL", msg);
  process.exit(1);
}
function ok(msg) {
  console.log("OK  ", msg);
}
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
  const de = document.documentElement;
  const body = document.body;
  const shell = document.querySelector('.fs-shell');
  const main = document.querySelector('.fs-pane[data-region="main"]');
  const mainBody = main && main.querySelector('.fs-body');
  const rail = document.querySelector('#fs-rail');
  const rect = (el) => {
    if (!el) return null;
    const r = el.getBoundingClientRect();
    return { top: Math.round(r.top), bottom: Math.round(r.bottom), left: Math.round(r.left), right: Math.round(r.right), width: Math.round(r.width), height: Math.round(r.height) };
  };
  const cs = (el) => (el ? getComputedStyle(el) : null);
  return {
    viewport: { w: innerWidth, h: innerHeight },
    doc: { scrollHeight: de.scrollHeight, clientHeight: de.clientHeight },
    body: { scrollHeight: body.scrollHeight, clientHeight: body.clientHeight, overflowY: cs(body).overflowY },
    htmlOverflowY: cs(de).overflowY,
    scrollY: window.scrollY,
    shell: rect(shell),
    shellHeight: shell ? cs(shell).height : null,
    main: rect(main),
    mainBody: { rect: rect(mainBody), overflowY: mainBody ? cs(mainBody).overflowY : null, scrollHeight: mainBody?.scrollHeight ?? null, clientHeight: mainBody?.clientHeight ?? null },
    rail: rect(rail),
    panes: [...document.querySelectorAll('.fs-pane')].map((p) => ({ region: p.dataset.region || null, rect: rect(p) })),
    components: {
      shell: !!shell,
      axis: !!document.querySelector('#fs-axis'),
      content: !!document.querySelector('.fs-content'),
      surface: !!document.querySelector('#fs-surface'),
      rail: !!rail,
        railTabs: document.querySelectorAll('.fs-railtab').length,
        heads: document.querySelectorAll('.fs-head').length,
        namedButtons: [...document.querySelectorAll('button')].filter((b) => (b.textContent || '').trim() || b.getAttribute('aria-label')).length,
      panes: document.querySelectorAll('.fs-pane').length,
    },
    landmarks: [...document.querySelectorAll('main,[role=main],nav,[role=navigation],header,footer')].map((el) => ({ tag: el.tagName, id: el.id || null, role: el.getAttribute('role'), label: el.getAttribute('aria-label') })),
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
  for (let i = 0; i < 40; i++) {
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
    fail("chromium CDP not ready");
  }
  ok(`chromium CDP ready on ${PORT}`);

  const failed = [];
  const results = [];
  try {
    const list = JSON.parse((await httpGet(`http://127.0.0.1:${PORT}/json/list`)).body);
    const page = list.find((t) => t.type === "page") || list[0];
    if (!page?.webSocketDebuggerUrl) fail("no page target");
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
    await send("Page.enable");
    await send("Runtime.enable");

    const viewports = [
      { name: "1200x900", w: 1200, h: 900 },
      { name: "1440x800", w: 1440, h: 800 },
      { name: "700x480", w: 700, h: 480 },
    ];

    for (const vp of viewports) {
      await send("Emulation.setDeviceMetricsOverride", {
        width: vp.w,
        height: vp.h,
        deviceScaleFactor: 1,
        mobile: false,
      });
      await send("Page.navigate", { url: `${URL_}/` });
      for (let i = 0; i < 80; i++) {
        const st = await send("Runtime.evaluate", { expression: "document.readyState", returnByValue: true });
        const has = await send("Runtime.evaluate", { expression: "!!document.querySelector('.fs-shell')", returnByValue: true });
        if (st.result?.value === "complete" && has.result?.value) break;
        await sleep(100);
      }
      await sleep(250);

      const m0 = (await send("Runtime.evaluate", { expression: MEASURE, returnByValue: true })).result?.value;
      if (m0?.error) {
        failed.push(`${vp.name}: ${m0.error}`);
        continue;
      }

      // Page scroll attempt 1: script scroll.
      await send("Runtime.evaluate", { expression: "window.scrollTo(0, 500)" });
      await sleep(120);
      const afterScript = await send("Runtime.evaluate", {
        expression: "JSON.stringify({y: window.scrollY, top: document.scrollingElement.scrollTop})",
        returnByValue: true,
      });

      // Page scroll attempt 2: wheel over the main window.
      await send("Input.dispatchMouseEvent", {
        type: "mouseWheel",
        x: Math.round(vp.w / 2),
        y: Math.round(vp.h / 2),
        deltaX: 0,
        deltaY: 800,
      });
      await sleep(200);
      const afterWheel = await send("Runtime.evaluate", {
        expression: "JSON.stringify({y: window.scrollY, top: document.scrollingElement.scrollTop})",
        returnByValue: true,
      });

      // Page scroll attempt 3: End key.
      await send("Input.dispatchKeyEvent", { type: "keyDown", key: "End", code: "End", windowsVirtualKeyCode: 35 });
      await send("Input.dispatchKeyEvent", { type: "keyUp", key: "End", code: "End", windowsVirtualKeyCode: 35 });
      await sleep(200);
      const afterKey = await send("Runtime.evaluate", {
        expression: "JSON.stringify({y: window.scrollY, top: document.scrollingElement.scrollTop})",
        returnByValue: true,
      });

      const m = (await send("Runtime.evaluate", { expression: MEASURE, returnByValue: true })).result?.value;
      const payload = {
        viewport: vp.name,
        ...m,
        attempts: {
          script: JSON.parse(afterScript.result.value),
          wheel: JSON.parse(afterWheel.result.value),
          endKey: JSON.parse(afterKey.result.value),
        },
      };
      results.push(payload);

      if (m.doc.scrollHeight > vp.h + 1)
        failed.push(`${vp.name}: page can scroll -- scrollHeight ${m.doc.scrollHeight} > viewport ${vp.h}`);
      if (m.doc.clientHeight !== vp.h) failed.push(`${vp.name}: document clientHeight ${m.doc.clientHeight} != ${vp.h}`);
      if (m.body.overflowY !== "hidden") failed.push(`${vp.name}: body overflow-y is ${m.body.overflowY}, not hidden`);
      for (const [how, a] of Object.entries(payload.attempts)) {
        if (a.y !== 0 || a.top !== 0) failed.push(`${vp.name}: ${how} scrolled the page to ${a.y}/${a.top}`);
      }
      if (!m.shell) failed.push(`${vp.name}: no .fs-shell`);
      else {
        if (m.shell.top < -1) failed.push(`${vp.name}: shell top ${m.shell.top} above viewport`);
        if (m.shell.bottom > vp.h + 1) failed.push(`${vp.name}: shell bottom ${m.shell.bottom} beyond viewport ${vp.h}`);
      }
      if (!m.components.axis) failed.push(`${vp.name}: #fs-axis missing (shell not composed)`);
      if (!m.components.content) failed.push(`${vp.name}: .fs-content missing`);
      if (!m.components.rail) failed.push(`${vp.name}: #fs-rail missing`);
        if (m.components.railTabs < 1) failed.push(`${vp.name}: no .fs-railtab entries in the rail`);
        if (!m.components.panes) failed.push(`${vp.name}: no .fs-pane components`);
        if (m.components.heads < 1) failed.push(`${vp.name}: no .fs-head pane headers`);
        if (m.components.namedButtons < 3) failed.push(`${vp.name}: only ${m.components.namedButtons} accessibly named buttons on the shell`);
        if (!m.mainBody || !/auto|scroll/.test(m.mainBody.overflowY || ""))
          failed.push(`${vp.name}: main pane body is not an internal scroller (overflow-y ${m.mainBody?.overflowY})`);

      const shot = await send("Page.captureScreenshot", { format: "png", captureBeyondViewport: false });
      const file = path.join(OUT, TAG, `filmstrip-${vp.name}.png`);
      fs.writeFileSync(file, Buffer.from(shot.data, "base64"));
      ok(
          `${vp.name}: doc ${m.doc.scrollHeight}/${m.doc.clientHeight} body-overflow=${m.body.overflowY} scrollY=${payload.attempts.endKey.y} wheelY=${payload.attempts.wheel.y} shell=${m.shell?.top}..${m.shell?.bottom} panes=${m.components.panes} railTabs=${m.components.railTabs} shot=${file}`
      );
    }

    console.log("MEASUREMENTS " + JSON.stringify(results));
    if (failed.length) {
      for (const f of failed) console.error("FAIL", f);
      ws.close();
      chrome.kill();
      process.exit(1);
    }
    ok("filmstrip page is scroll-free and the shell fills the viewport at every size; scroll attempts left scrollY at 0");
    ws.close();
  } finally {
    chrome.kill();
  }
}

main().catch((e) => fail(e.stack || String(e)));
