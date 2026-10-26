#!/usr/bin/env node
/**
 * Todo pool dedupe CDP smoke (card a4fb7706).
 *
 * todo/item/all used to double-count: deferred mirrors file the SAME (type,id)
 * under the garden and the shared data space, so the naive union rendered the
 * same card twice. The fix dedupes at the source (todo/list/merge,
 * workstreams/pulse-nodes). This drives a REAL headless chromium over CDP with
 * a Bearer token and asserts, on the faces that read that merged pool, that
 * every rendered card row carries a DISTINCT id -- plus a screenshot per face.
 *
 * Env: PP_URL, PP_TOKEN (falls back to PP_USER/PP_PASS Basic), CHROME, OUT,
 *      CDP_PORT. Exit 0 on success.
 */
import { spawn } from "node:child_process";
import { setTimeout as sleep } from "node:timers/promises";
import http from "node:http";
import fs from "node:fs";
import path from "node:path";

const PP_URL = (process.env.PP_URL || "http://127.0.0.1:18080").replace(/\/$/, "");
const PP_TOKEN = process.env.PP_TOKEN || "";
const PP_USER = process.env.PP_USER || "admin";
const PP_PASS = process.env.PP_PASS || "admin";
const CHROME = process.env.CHROME || "/usr/local/bin/chrome";
const OUT = process.env.OUT || "screenshots/a4fb7706";
const PORT = Number(process.env.CDP_PORT || 9333);
const USER_DATA = `/tmp/chrome-dedupe-cdp-${process.pid}`;
const AUTH = PP_TOKEN
  ? `Bearer ${PP_TOKEN}`
  : `Basic ${Buffer.from(`${PP_USER}:${PP_PASS}`).toString("base64")}`;

function log(...a) {
  console.log(...a);
}
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
      } catch (_) {
        /* ignore non-matching parse noise */
      }
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

// In-page probes: return the rendered card rows and their ids.
const PROBES = {
  // /todos: one li.todo-item per card, id on the cycle form's hidden input.
  todos: `(() => {
    const rows = [...document.querySelectorAll('li.todo-item')];
    const idOf = r => {
      const i = r.querySelector('form.todo-check input[name="id"]');
      if (i && i.value) return i.value;
      const a = r.querySelector('a[href*="/todos?id="]');
      const m = a && a.getAttribute('href').match(/id=([0-9a-f-]{36})/);
      return m ? m[1] : '';
    };
    const ids = rows.map(idOf).filter(Boolean);
    return JSON.stringify({
      rows: rows.length,
      withId: ids.length,
      uniq: new Set(ids).size,
      fromNote: rows.filter(r => r.classList.contains('todo-from-note')).length,
      titled: rows.filter(r => r.querySelector('.todo-title')).length,
      sample: ids.slice(0, 3),
    });
  })()`,
  // /kanban: one .kb-card per card; id from a hidden input or a card link.
  kanban: `(() => {
    const rows = [...document.querySelectorAll('.kb-card')];
    const idOf = r => {
      const i = r.querySelector('input[name="id"]');
      if (i && i.value) return i.value;
      const a = r.querySelector('a[href*="id="]');
      const m = a && a.getAttribute('href').match(/id=([0-9a-f-]{36})/);
      return m ? m[1] : '';
    };
    const ids = rows.map(idOf);
    return JSON.stringify({
      rows: rows.length,
      withId: ids.filter(Boolean).length,
      uniq: new Set(ids.filter(Boolean)).size,
      titled: rows.filter(r => r.querySelector('.kb-card-title')).length,
      sample: ids.slice(0, 3),
    });
  })()`,
  // /pulse: cards are links into the card page.
  pulse: `(() => {
    const ids = [...document.querySelectorAll('a[href*="/todos?id="]')]
      .map(a => (a.getAttribute('href').match(/id=([0-9a-f-]{36})/) || [])[1])
      .filter(Boolean);
    return JSON.stringify({
      rows: ids.length,
      withId: ids.length,
      uniq: new Set(ids).size,
      sample: ids.slice(0, 3),
    });
  })()`,
};

const FACES = [
  { path: "/todos", probe: PROBES.todos, shot: "todos.png" },
  { path: "/kanban", probe: PROBES.kanban, shot: "kanban.png" },
  { path: "/pulse", probe: PROBES.pulse, shot: "pulse.png" },
];

log(`== target ${PP_URL} (auth ${PP_TOKEN ? "bearer" : "basic"})`);
log("== start chromium");
const chrome = spawn(
  CHROME,
  [
    `--remote-debugging-port=${PORT}`,
    `--user-data-dir=${USER_DATA}`,
    "--headless=new",
    "--disable-gpu",
    "--no-sandbox",
    "--disable-dev-shm-usage",
    "--window-size=1400,900",
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
ok("chromium CDP ready");

fs.mkdirSync(OUT, { recursive: true });

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
  ok("CDP websocket open");

  let nextId = 1;
  const send = (method, params) => cdpSend(ws, nextId++, method, params);

  await send("Page.enable");
  await send("Runtime.enable");
  await send("Network.enable");
  await send("Network.setExtraHTTPHeaders", {
    headers: { Authorization: AUTH },
  });

  const results = [];
    for (const face of FACES) {
      // location assignment, not Page.navigate: navigate can hang while the
      // renderer is busy and the probe loop below is the real readiness gate.
      await send("Runtime.evaluate", {
        expression: `location.href = ${JSON.stringify(PP_URL + face.path)}`,
        returnByValue: true,
      });
      let probed = null;
      for (let i = 0; i < 40; i++) {
        await sleep(300);
        let ev;
        try {
          ev = await send("Runtime.evaluate", {
            expression: face.probe,
            returnByValue: true,
          });
        } catch (_) {
          continue; // context torn down mid-navigation; retry
        }
        const v = ev?.result?.value;
        if (typeof v === "string") {
          try {
            const p = JSON.parse(v);
            if (p.rows > 0) {
              probed = p;
              break;
            }
          } catch (_) {}
        }
      }
    if (!probed) fail(`${face.path}: no card rows rendered`);
    const title = await send("Runtime.evaluate", {
      expression: "document.title || '(untitled)'",
      returnByValue: true,
    });
    const bodyClass = await send("Runtime.evaluate", {
      expression: "document.body ? String(document.body.className) : ''",
      returnByValue: true,
    });
    log(
      `${face.path} title=${JSON.stringify(title?.result?.value)} ` +
        `rows=${probed.rows} withId=${probed.withId} uniq=${probed.uniq} ` +
        (probed.titled !== undefined ? `titled=${probed.titled} ` : "") +
        `sample=${JSON.stringify(probed.sample)}`
    );
    if (String(bodyClass?.result?.value).includes("login"))
      fail(`${face.path}: rendered the login page, not the face`);
    if (probed.titled !== undefined && probed.titled !== probed.rows)
      fail(`${face.path}: ${probed.rows - probed.titled} row(s) render no title`);
    if (probed.uniq !== probed.withId)
      fail(
        `${face.path}: ${probed.withId} id-bearing rows / ${probed.uniq} unique ids`
      );
    if (probed.fromNote !== undefined && probed.withId + probed.fromNote !== probed.rows)
      fail(
        `${face.path}: ${probed.rows} rows but only ${probed.withId} cards + ${probed.fromNote} note previews`
      );
    ok(
      `${face.path}: ${probed.rows} rows, ${probed.withId} cards, ${probed.uniq} distinct ids` +
        (probed.fromNote !== undefined ? `, ${probed.fromNote} note previews` : "")
    );

    const shot = await send("Page.captureScreenshot", { format: "png" });
    const file = path.join(OUT, face.shot);
    fs.writeFileSync(file, Buffer.from(shot.data, "base64"));
    const size = fs.statSync(file).size;
    if (size < 5000) fail(`${face.path}: screenshot too small (${size}B)`);
    ok(`${face.path}: screenshot ${file} (${size}B)`);
    results.push({ face: face.path, rows: probed.rows, uniq: probed.uniq, shot: file });
  }

  log("== summary", JSON.stringify(results));
  ws.close();
} catch (e) {
  chrome.kill();
  fail(e?.stack || String(e));
}

chrome.kill();
ok("done");
process.exit(0);
