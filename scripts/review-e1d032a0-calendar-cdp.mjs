#!/usr/bin/env node
/**
 * Card e1d032a0 UI review: the calendar day/event click is PANE-LOCAL.
 *
 * Drives real headless Chromium over CDP against PP_URL (Bearer auth
 * from ~/.config/pricklypear/env), loads /calendar, asserts the month
 * grid day cells carry hx-get + hx-target="#cal-main" + hx-swap, clicks
 * a day cell, and asserts the day agenda replaced #cal-main IN PLACE
 * (URL unchanged, one #cal-main, Events + Due todos inside the box).
 *
 * Artifacts: $OUT/*.png plus JSON on stdout. Exit 0 = all assertions
 * hold.
 */
import { spawn } from "node:child_process";
import { setTimeout as sleep } from "node:timers/promises";
import http from "node:http";
import fs from "node:fs";
import os from "node:os";
import path from "node:path";

function readEnvFile(file) {
  const out = {};
  if (!fs.existsSync(file)) return out;
  for (const line of fs.readFileSync(file, "utf8").split("\n")) {
    const m = line.match(/^([A-Za-z_][A-Za-z0-9_]*)=(.*)$/);
    if (!m) continue;
    let v = m[2].trim();
    if (
      (v.startsWith('"') && v.endsWith('"')) ||
      (v.startsWith("'") && v.endsWith("'"))
    )
      v = v.slice(1, -1);
    out[m[1]] = v;
  }
  return out;
}

const env = {
  ...process.env,
  ...readEnvFile(path.join(os.homedir(), ".config/pricklypear/env")),
};
const PP_URL = (env.PP_URL || "http://127.0.0.1:18080").replace(/\/$/, "");
const PP_TOKEN = env.PP_TOKEN || "";
const CHROME = process.env.CHROME || "/usr/local/bin/chrome";
const PORT = Number(process.env.CDP_PORT || 9450);
const OUT = process.env.OUT || "/home/al/pricklypear/screenshots/e1d032a0";
const USER_DATA = `/tmp/chrome-e1d032a0-${process.pid}`;
const failures = [];
const results = {};

function check(name, ok, detail) {
  results[name] = { ok: !!ok, detail };
  if (!ok) failures.push(`${name}: ${JSON.stringify(detail)}`);
  console.log(`${ok ? "OK  " : "FAIL"} ${name}`, JSON.stringify(detail));
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

function cdpSend(ws, id, method, params = {}) {
  return new Promise((resolve, reject) => {
    const timer = setTimeout(() => {
      cleanup();
      reject(new Error(`CDP timeout ${method}`));
    }, 20000);
    function onMsg(ev) {
      let m;
      try {
        m = JSON.parse(typeof ev.data === "string" ? ev.data : String(ev.data));
      } catch {
        return;
      }
      if (m.id !== id) return;
      cleanup();
      if (m.error) reject(new Error(JSON.stringify(m.error)));
      else resolve(m.result);
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

async function main() {
  if (!PP_TOKEN) {
    console.error("missing PP_TOKEN in ~/.config/pricklypear/env");
    process.exit(2);
  }
  fs.mkdirSync(OUT, { recursive: true });
  fs.mkdirSync(USER_DATA, { recursive: true });
  console.log(`PP_URL=${PP_URL} OUT=${OUT}`);

  const chrome = spawn(
    CHROME,
    [
      `--remote-debugging-port=${PORT}`,
      `--user-data-dir=${USER_DATA}`,
      "--headless=new",
      "--disable-gpu",
      "--no-sandbox",
      "--disable-dev-shm-usage",
      "--window-size=1200,900",
      "about:blank",
    ],
    { stdio: "ignore" }
  );
  let ws = null;
  try {
    let ready = false;
    for (let i = 0; i < 40; i++) {
      try {
        const v = await httpGet(`http://127.0.0.1:${PORT}/json/version`);
        if (v.status === 200) {
          ready = true;
          break;
        }
      } catch {}
      await sleep(250);
    }
    if (!ready) throw new Error("chromium CDP not ready");

    const list = JSON.parse(
      (await httpGet(`http://127.0.0.1:${PORT}/json/list`)).body
    );
    const page = list.find((t) => t.type === "page") || list[0];
    if (!page?.webSocketDebuggerUrl) throw new Error("no page target");

    ws = new WebSocket(page.webSocketDebuggerUrl);
    await new Promise((res, rej) => {
      const t = setTimeout(() => rej(new Error("ws open timeout")), 10000);
      ws.onopen = () => {
        clearTimeout(t);
        res();
      };
      ws.onerror = (e) => {
        clearTimeout(t);
        rej(e?.error || new Error("ws error"));
      };
    });

    let nextId = 1;
    const send = (method, params) => cdpSend(ws, nextId++, method, params);
    await send("Page.enable");
    await send("Runtime.enable");
    await send("Network.enable");
    await send("Network.setExtraHTTPHeaders", {
      headers: { Authorization: `Bearer ${PP_TOKEN}` },
    });
    await send("Emulation.setDeviceMetricsOverride", {
      width: 1200,
      height: 900,
      deviceScaleFactor: 1,
      mobile: false,
    });

    const evalJs = async (expression) => {
      const r = await send("Runtime.evaluate", {
        expression,
        returnByValue: true,
        awaitPromise: true,
      });
      if (r.exceptionDetails)
        throw new Error(String(JSON.stringify(r.exceptionDetails)));
      return r.result.value;
    };
    const shot = async (name) => {
      const r = await send("Page.captureScreenshot", { format: "png" });
      const p = path.join(OUT, name);
      fs.writeFileSync(p, Buffer.from(r.data, "base64"));
      return p;
    };

    console.log("== /calendar month grid ==");
    await send("Page.navigate", { url: `${PP_URL}/calendar` });
    await sleep(2500);

    const before = await evalJs(`(() => {
      const links = [...document.querySelectorAll('#cal-main a[hx-get^="/calendar?day="]')];
      return {
        url: location.href,
        title: document.title,
        calMain: document.querySelectorAll('#cal-main').length,
        dayLinks: links.length,
        paneTarget: links.filter(a => a.getAttribute('hx-target') === '#cal-main').length,
        outerSwap: links.filter(a => a.getAttribute('hx-swap') === 'outerHTML').length,
        htmx: typeof window.htmx !== 'undefined',
        sample: links.slice(0, 2).map(a => ({
          href: a.getAttribute('href'),
          get: a.getAttribute('hx-get'),
          target: a.getAttribute('hx-target'),
          swap: a.getAttribute('hx-swap')
        }))
      };
    })()`);
    const beforePng = await shot("1-month-grid.png");

    check("one #cal-main box", before.calMain === 1, before.calMain);
    check("day cells exist", before.dayLinks > 0, before.dayLinks);
    check(
      "every day cell swaps #cal-main",
      before.dayLinks > 0 && before.paneTarget === before.dayLinks,
      `${before.paneTarget}/${before.dayLinks}`
    );
    check(
      "every day cell uses outerHTML swap",
      before.dayLinks > 0 && before.outerSwap === before.dayLinks,
      `${before.outerSwap}/${before.dayLinks}`
    );
    check("htmx loaded on the page", before.htmx === true, before.htmx);

    console.log("== click a day cell ==");
    const clicked = await evalJs(`(() => {
      const links = [...document.querySelectorAll('#cal-main a[hx-get^="/calendar?day="]')];
      const a = links.find(x => x.getAttribute('hx-get').endsWith('-18')) || links[14] || links[0];
      const day = a.getAttribute('hx-get');
      a.click();
      return day;
    })()`);
    await sleep(2000);

    const after = await evalJs(`(() => {
      const cal = document.querySelector('#cal-main');
      return {
        url: location.href,
        calMain: document.querySelectorAll('#cal-main').length,
        dayPanel: !!document.querySelector('#cal-main .cal-day-panel'),
        dayPanelAnywhere: document.querySelectorAll('.cal-day-panel').length,
        header: (document.querySelector('#cal-main h2') || {}).textContent || null,
        headings: [...document.querySelectorAll('#cal-main h3')].map(h => h.textContent),
        eventRows: document.querySelectorAll('#cal-main li.cal-event').length,
        addEventForm: !!document.querySelector('#cal-main form input[name=title]'),
        monthCellsLeft: document.querySelectorAll('#cal-main .cal-cell').length,
        insideBox: cal ? cal.querySelectorAll('.cal-day-panel').length : 0
      };
    })()`);
    const afterPng = await shot("2-day-agenda.png");
    const wantDay = (clicked || "").split("day=")[1] || "";

    check("click did not navigate", after.url === before.url, {
      before: before.url,
      after: after.url,
    });
    check("still exactly one #cal-main", after.calMain === 1, after.calMain);
    check("day agenda rendered inside the box", after.insideBox === 1, after.insideBox);
    check("day header is the clicked day", after.header === wantDay, {
      header: after.header,
      want: wantDay,
    });
    check("Events + Due todos sections",
      after.headings.includes("Events") && after.headings.includes("Due todos"),
      after.headings);
    check("month grid replaced", after.monthCellsLeft === 0, after.monthCellsLeft);
    check("add-event form in the panel", after.addEventForm === true, after.addEventForm);

    console.log("== /filmstrip shell ==");
    const fsStatus = await fetch(`${PP_URL}/filmstrip`, {
      headers: { Authorization: `Bearer ${PP_TOKEN}` },
      redirect: "manual",
    });
    await send("Page.navigate", { url: `${PP_URL}/filmstrip` });
    await sleep(2000);
    const shell = await evalJs(`(() => ({
      url: location.href,
      h1: (document.querySelector('h1') || {}).textContent || null,
      hasShell: !!document.querySelector('#fs-rail, #fs-layers, .fs-rail'),
      hasCalMain: document.querySelectorAll('#cal-main').length
    }))()`);
    const shellPng = await shot("3-filmstrip.png");

    console.log(JSON.stringify({
      verdict: failures.length ? "FAIL" : "PASS",
      before, after, clicked, shell,
      filmstripStatus: fsStatus.status,
      artifacts: { beforePng, afterPng, shellPng },
      failures
    }, null, 2));
    process.exitCode = failures.length ? 1 : 0;
  } finally {
    try { ws?.close(); } catch {}
    chrome.kill();
  }
}

main().catch((e) => {
  console.error("HARNESS ERROR", e?.stack || e);
  process.exit(2);
});
