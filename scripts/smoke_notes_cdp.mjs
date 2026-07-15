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

    // Regression: root selected → click second title → must STAY on second (not snap to root).
    await send("Runtime.evaluate", {
      expression: `
        (function(){
          var root=document.querySelector('.ol-node[data-id="root"]');
          if(root){var t=root.querySelector('.ol-title'); if(t) t.click();}
          document.getElementById('ol-selected-id').value='root';
        })()
      `,
      returnByValue: true,
    });
    await sleep(200);
    const switchSel = await send("Runtime.evaluate", {
      expression: `
        (function(){
          var kids=[...document.querySelectorAll('.ol-node')].filter(n=>n.dataset.id&&n.dataset.id!=='root');
          if(kids.length<1) return JSON.stringify({err:'no-child'});
          var child=kids[0];
          var title=child.querySelector('.ol-title');
          title.dispatchEvent(new MouseEvent('mousedown',{bubbles:true,cancelable:true,button:0}));
          title.dispatchEvent(new MouseEvent('click',{bubbles:true,cancelable:true}));
          title.focus();
          title.dispatchEvent(new FocusEvent('focusin',{bubbles:true}));
          var id1=document.getElementById('ol-selected-id').value;
          return new Promise(function(res){
            setTimeout(function(){
              var id2=document.getElementById('ol-selected-id').value;
              var lab=document.getElementById('ol-selected-label').textContent;
              res(JSON.stringify({want:child.dataset.id,id1:id1,id2:id2,lab:lab}));
            },300);
          });
        })()
      `,
      awaitPromise: true,
      returnByValue: true,
    });
    {
      const v = switchSel.result?.value;
      ok(`switch-from-root ${v}`);
      let p = {};
      try { p = JSON.parse(v); } catch {}
      if (p.err) fail(p.err);
      if (p.id2 !== p.want)
        fail(`selection snapped back (id1=${p.id1} id2=${p.id2} want ${p.want})`);
    }

    // Selection via TITLE click (contenteditable) — real user path.
    const sel = await send("Runtime.evaluate", {
      expression: `
        (function(){
          var nodes=[...document.querySelectorAll('.ol-node[data-id]')];
          var child=nodes.find(n=>n.dataset.id && n.dataset.id!=='root');
          if(!child) return 'no-child';
          var title=child.querySelector(':scope > .ol-sum .ol-title') || child.querySelector('.ol-title');
          if(!title) return 'no-title';
          title.dispatchEvent(new MouseEvent('click',{bubbles:true,cancelable:true,view:window}));
          title.dispatchEvent(new FocusEvent('focusin',{bubbles:true}));
          var hid=document.getElementById('ol-selected-id');
          var lab=document.getElementById('ol-selected-label');
          return JSON.stringify({id: hid&&hid.value, lab: lab&&lab.textContent, want: child.dataset.id});
        })()
      `,
      returnByValue: true,
    });
    {
      const v = sel.result?.value;
      ok(`selected via title ${v}`);
      if (v === "no-child" || v === "no-title") fail(`select failed ${v}`);
      let parsed;
      try { parsed = JSON.parse(v); } catch { parsed = {}; }
      if (parsed.id !== parsed.want)
        fail(`title click did not select child (got ${parsed.id} want ${parsed.want})`);
    }

    // Top-level +sib with root selected must grow root's children
    await send("Runtime.evaluate", {
      expression: `
        (function(){
          var root=document.querySelector('.ol-node[data-id=root]');
          if(root){var t=root.querySelector(':scope > .ol-sum .ol-title'); if(t) t.click();}
          document.getElementById('ol-selected-id').value='root';
        })()
      `,
      returnByValue: true,
    });
    const kidsBeforeRootSib = parseInt(
      String(
        await evalApi(
          `(str (count (outline/kids (notes/doc/outline (notes/doc/get "${SLUG}")))))`
        )
      ).replace(/"/g, ""),
      10
    );
    await send("Runtime.evaluate", {
      expression: `
        (function(){
          var btn=document.querySelector('.ol-toolbar [data-ol-op="add-sibling"]');
          if(!btn) return 'no-btn';
          btn.dispatchEvent(new MouseEvent('click',{bubbles:true,cancelable:true}));
          return 'ok';
        })()
      `,
      returnByValue: true,
    });
    await sleep(1500);
    const kidsAfterRootSib = parseInt(
      String(
        await evalApi(
          `(str (count (outline/kids (notes/doc/outline (notes/doc/get "${SLUG}")))))`
        )
      ).replace(/"/g, ""),
      10
    );
    ok(`root +sib kids ${kidsBeforeRootSib}->${kidsAfterRootSib}`);
    if (kidsAfterRootSib !== kidsBeforeRootSib + 1)
      fail(`top-level sibling broken (${kidsBeforeRootSib}->${kidsAfterRootSib})`);

    // Re-select first non-root for remaining ops
    await send("Runtime.evaluate", {
      expression: `
        (function(){
          var child=[...document.querySelectorAll('.ol-node')].find(n=>n.dataset.id&&n.dataset.id!=='root');
          if(!child) return;
          var t=child.querySelector('.ol-title');
          if(t) t.click();
          document.getElementById('ol-selected-id').value=child.dataset.id;
        })()
      `,
      returnByValue: true,
    });

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
    const nMarks = (encStr.match(/n\$/g) || []).length;
    if (nMarks < 2) fail(`model expected >=2 generated nodes, got ${nMarks} in ${encStr.slice(0, 200)}`);
    ok(`model gen-nodes=${nMarks}`);

    // --- more toolbar ops on selection ---
    async function toolbar(op) {
      const r = await send("Runtime.evaluate", {
        expression: `
          (function(){
            var btn=document.querySelector('.ol-toolbar [data-ol-op="${op}"]');
            if(!btn) return 'no-btn:${op}';
            var sel=(document.getElementById('ol-selected-id')||{}).value;
            btn.dispatchEvent(new MouseEvent('click',{bubbles:true,cancelable:true}));
            return 'ok:'+sel;
          })()
        `,
        returnByValue: true,
      });
      const v = r.result?.value || "";
      if (!String(v).startsWith("ok:")) fail(`toolbar ${op} ${v}`);
      await sleep(1200);
      return v;
    }

    async function evalDom(expr) {
      const r = await send("Runtime.evaluate", {
        expression: expr,
        returnByValue: true,
      });
      return r.result?.value;
    }

    // selection restored after swap?
    const selAfterKid = await evalDom(
      `(function(){var s=document.querySelector('.ol-node.is-selected');return s?s.dataset.id:'none';})()`
    );
    ok(`selection after +kid swap=${selAfterKid}`);
    if (selAfterKid === "none") fail("selection lost after outline swap");

    // cycle-todo
    await toolbar("cycle-todo");
    const todoCls = await evalDom(
      `(function(){var s=document.querySelector('.ol-node.is-selected');return s?s.className:'none';})()`
    );
    ok(`after cycle-todo class=${todoCls}`);
    if (!String(todoCls).includes("st-TODO") && !String(todoCls).includes("st-DONE"))
      fail(`cycle-todo did not set state class (${todoCls})`);

    // archive (default view hides ARCHIVED kids — assert model, not DOM class)
    await toolbar("archive");
    await sleep(500);
    const encArch = String(
      await evalApi(
        `(outline/encode (notes/doc/outline (notes/doc/get "${SLUG}")))`
      )
    );
    if (!encArch.includes("ARCHIVED"))
      fail(`archive not in model: ${encArch.slice(0, 240)}`);
    ok("archive reflected in model");
    // Archived kids stay visible (dimmed); ensure DOM still has selection or node
    const archDom = await evalDom(
      `(function(){return document.querySelector('.st-ARCHIVED')?'yes':'no';})()`
    );
    ok(`archive visible in DOM=${archDom}`);

    // unarchive (best-effort)
    await toolbar("unarchive").catch(() => {});
    await sleep(800);

    // +sib then move-down
    const nBeforeSib = await evalDom(`document.querySelectorAll('.ol-node').length`);
    await toolbar("add-sibling");
    const nAfterSib = await evalDom(`document.querySelectorAll('.ol-node').length`);
    ok(`+sib nodes ${nBeforeSib}->${nAfterSib}`);
    if (nAfterSib <= nBeforeSib) fail("+sib did not add node");

    await toolbar("move-down");
    await toolbar("move-up");
    ok("move-down/up clicked");

    // delete selection (not root)
    const nBeforeDel = await evalDom(`document.querySelectorAll('.ol-node').length`);
    // ensure selection is not root
    await send("Runtime.evaluate", {
      expression: `
        (function(){
          var s=document.querySelector('.ol-node.is-selected');
          if(s && s.dataset.id==='root'){
            var c=[...document.querySelectorAll('.ol-node')].find(n=>n.dataset.id&&n.dataset.id!=='root');
            if(c){document.getElementById('ol-selected-id').value=c.dataset.id;c.classList.add('is-selected');}
          }
          // auto-confirm delete
          window.confirm=function(){return true;};
          return document.getElementById('ol-selected-id').value;
        })()
      `,
      returnByValue: true,
    });
    await toolbar("delete");
    const nAfterDel = await evalDom(`document.querySelectorAll('.ol-node').length`);
    ok(`delete nodes ${nBeforeDel}->${nAfterDel}`);
    if (nAfterDel >= nBeforeDel) fail("delete did not remove a node");

    // selection still present after delete swap
    const selAfterDel = await evalDom(
      `(function(){return document.getElementById('ol-selected-id')?document.getElementById('ol-selected-id').value:'none';})()`
    );
    ok(`selection id after delete=${selAfterDel}`);

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
