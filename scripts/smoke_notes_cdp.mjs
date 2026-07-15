#!/usr/bin/env node
/**
 * Notes desktop toolbar CDP smoke (JS selection path).
 *
 * Starts Chromium with remote debugging, logs in, opens ui-fixture,
 * selects a node, clicks toolbar +kid, asserts DOM + optional model.
 *
 * Env: PP_URL (default http://127.0.0.1:18080), PP_USER, PP_PASS
 * Exit 0 on success.
 */
import { spawn } from "node:child_process";
import { setTimeout as sleep } from "node:timers/promises";
import http from "node:http";
import fs from "node:fs";

const PP_URL = (process.env.PP_URL || "http://127.0.0.1:18080").replace(/\/$/, "");
const PP_USER = process.env.PP_USER || "admin";
const PP_PASS = process.env.PP_PASS || "admin";
const SLUG = "ui-fixture";
const PORT = 9333;
const CHROME = process.env.CHROME || "/usr/bin/chromium";
const OUT = process.env.NOTES_UI_OUT || "/tmp/pp-notes-ui";
const USER_DATA = `/tmp/chrome-notes-cdp-${process.pid}`;

function log(...a) {
  console.log(...a);
}
function fail(msg) {
  console.error("FAIL", msg);
  process.exit(1);
}
function ok(msg) {
  console.log("OK ", msg);
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

async function cdpSend(ws, id, method, params = {}) {
  const msg = JSON.stringify({ id, method, params });
  return new Promise((resolve, reject) => {
    const timer = setTimeout(() => {
      cleanup();
      reject(new Error(`CDP timeout ${method}`));
    }, 15000);
    function onMsg(ev) {
      try {
        const m = JSON.parse(wsData(ev));
        if (m.id === id) {
          cleanup();
          if (m.error) reject(new Error(JSON.stringify(m.error)));
          else resolve(m.result);
        }
      } catch (e) {
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
    ws.send(msg);
  });
}

async function main() {
  fs.mkdirSync(OUT, { recursive: true });
  fs.mkdirSync(USER_DATA, { recursive: true });

  // Prep fixture via eval API (basic auth)
  const auth =
    "Basic " + Buffer.from(`${PP_USER}:${PP_PASS}`).toString("base64");
  async function evalApi(expr) {
    const res = await fetch(`${PP_URL}/api/eval`, {
      method: "POST",
      headers: {
        Authorization: auth,
        "Content-Type": "application/json",
      },
      body: JSON.stringify({ expr }),
    });
    const j = await res.json();
    if (!j.ok && j.error) throw new Error(j.error || JSON.stringify(j));
    return j.value;
  }

  log("== prep fixture ==");
  await evalApi('(load-library "outline")');
  await evalApi('(load-library "notes")');
  await evalApi(
    `(notes/doc/upsert "${SLUG}" "UI Fixture" (outline/node "root" "UI Fixture") "" "note" "")`
  );
  // one child to select
  await evalApi(
    `(notes/doc/upsert "${SLUG}" "UI Fixture" (notes/outline/apply-op "${SLUG}" (notes/doc/outline (notes/doc/get "${SLUG}")) "add-child" "root") "" "note" "")`
  );
  const kids0 = await evalApi(
    `(str (count (outline/kids (notes/doc/outline (notes/doc/get "${SLUG}")))))`
  );
  ok(`fixture kids=${kids0}`);

  log("== start chromium ==");
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
  for (let i = 0; i < 30; i++) {
    try {
      const v = await httpGet(`http://127.0.0.1:${PORT}/json/version`);
      if (v.status === 200) {
        ready = true;
        break;
      }
    } catch (_) {}
    await sleep(200);
  }
  if (!ready) {
    chrome.kill();
    fail("chromium CDP not ready");
  }
  ok("chromium CDP ready");

  try {
    const list = JSON.parse(
      (await httpGet(`http://127.0.0.1:${PORT}/json/list`)).body
    );
    const page = list.find((t) => t.type === "page") || list[0];
    if (!page?.webSocketDebuggerUrl) fail("no page target");

    const WS = globalThis.WebSocket;
    if (!WS) fail("global WebSocket missing (need Node 22+)");
    const ws = new WS(page.webSocketDebuggerUrl);
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

    // Login via form
    await send("Page.navigate", { url: `${PP_URL}/login` });
    await sleep(800);
    await send("Runtime.evaluate", {
      expression: `
        (function(){
          var u=document.querySelector('input[name=username]');
          var p=document.querySelector('input[name=password]');
          var f=document.querySelector('form');
          if(u) u.value=${JSON.stringify(PP_USER)};
          if(p) p.value=${JSON.stringify(PP_PASS)};
          if(f) f.submit();
          return !!f;
        })()
      `,
      awaitPromise: false,
    });
    await sleep(1200);

    await send("Page.navigate", {
      url: `${PP_URL}/notes?slug=${SLUG}`,
    });
    await sleep(1500);

    // Count nodes, select first non-root kid, click +kid
    const before = await send("Runtime.evaluate", {
      expression: `document.querySelectorAll('.ol-node').length`,
      returnByValue: true,
    });
    const beforeN = before.result?.value ?? 0;
    ok(`nodes before=${beforeN}`);

    // Select first non-root node by driving the same setSel path (force hidden id + class)
    const sel = await send("Runtime.evaluate", {
      expression: `
        (function(){
          var nodes=[...document.querySelectorAll('.ol-node[data-id]')];
          var child=nodes.find(n=>n.dataset.id && n.dataset.id!=='root');
          if(!child) return 'no-child';
          var id=child.dataset.id;
          var hid=document.getElementById('ol-selected-id');
          if(hid) hid.value=id;
          var lab=document.getElementById('ol-selected-label');
          var t=child.querySelector('.ol-title');
          if(lab) lab.textContent=(t&&t.innerText)?t.innerText.trim():id;
          var rf=document.getElementById('ol-refile-nid'); if(rf) rf.value=id;
          document.querySelectorAll('.ol-node.is-selected').forEach(function(n){n.classList.remove('is-selected');});
          child.classList.add('is-selected');
          // also click title row to exercise real handler
          var sum=child.querySelector('.ol-sum');
          if(sum) sum.dispatchEvent(new MouseEvent('click',{bubbles:true,cancelable:true}));
          return JSON.stringify({id: hid&&hid.value, lab: lab&&lab.textContent, cls: child.className});
        })()
      `,
      returnByValue: true,
    });
    {
      const v = sel.result?.value;
      ok(`selected ${v}`);
      if (v === "no-child") fail("no child to select");
    }

    // Click toolbar +kid (desktop JS path)
    const click = await send("Runtime.evaluate", {
      expression: `
        (function(){
          var btn=document.querySelector('.ol-toolbar [data-ol-op="add-child"]')
            || document.querySelector('[data-ol-op="add-child"]');
          if(!btn) return 'no-btn';
          btn.dispatchEvent(new MouseEvent('click',{bubbles:true,cancelable:true}));
          return 'clicked:'+(document.getElementById('ol-selected-id')||{}).value;
        })()
      `,
      returnByValue: true,
    });
    const cv = click.result?.value || "";
    if (!String(cv).startsWith("clicked")) fail(`toolbar click ${cv}`);
    ok(`toolbar +kid ${cv}`);
    await sleep(2000);

    const after = await send("Runtime.evaluate", {
      expression: `document.querySelectorAll('.ol-node').length`,
      returnByValue: true,
    });
    const afterN = after.result?.value ?? 0;
    ok(`nodes after=${afterN}`);
    if (afterN <= beforeN) fail(`expected more nodes after +kid (${beforeN}->${afterN})`);

    // model side: total nodes grew (new node may be nested under selection)
    const enc = await evalApi(
      `(outline/encode (notes/doc/outline (notes/doc/get "${SLUG}")))`
    );
    const encStr = String(enc);
    // count "n$ or "root" id occurrences roughly via New node titles / n$
    const nMarks = (encStr.match(/n\$/g) || []).length;
    if (nMarks < 2) fail(`model expected >=2 generated nodes, got ${nMarks} in ${encStr.slice(0, 200)}`);
    ok(`model gen-nodes=${nMarks}`);

    // Screenshot
    const shot = await send("Page.captureScreenshot", { format: "png" });
    if (shot.data) {
      fs.writeFileSync(`${OUT}/cdp-toolbar.png`, Buffer.from(shot.data, "base64"));
      ok(`screenshot ${OUT}/cdp-toolbar.png`);
    }

    // Sticky CSS check
    const sticky = await send("Runtime.evaluate", {
      expression: `
        (function(){
          var el=document.querySelector('.ol-toolbar');
          if(!el) return 'missing';
          var cs=getComputedStyle(el);
          return cs.position+':'+cs.top+':'+cs.display;
        })()
      `,
      returnByValue: true,
    });
    ok(`toolbar computed ${sticky.result?.value}`);
    if (!String(sticky.result?.value || "").includes("sticky")) {
      // mobile hide? desktop should be sticky
      console.warn("WARN toolbar not sticky (maybe narrow viewport)");
    }

    ws.close();
    console.log("SMOKE_NOTES_CDP OK");
  } finally {
    chrome.kill("SIGKILL");
    try {
      fs.rmSync(USER_DATA, { recursive: true, force: true });
    } catch (_) {}
  }
}

main().catch((e) => {
  console.error(e);
  process.exit(1);
});
