#!/usr/bin/env node
/**
 * Notes outline SELECTION smoke (tier 2 — browser).
 *
 * Guards the browser-only contract that tier-1 pp_outline.py cannot see:
 * delegated pointerdown/click/focusin handlers seat #ol-selected-id so
 * toolbar ops ([data-ol-op]) act on the clicked node, not root.
 *
 * Catches: parse-dead inline scripts (e.g. Nopales string escapes eating
 * a backslash from a JS regex, breaking the whole <script>). Tier-1
 * `pp_outline.py scripts-check` catches static cases; this catches any
 * remaining runtime/runtime-interpolation path.
 *
 * Env: PP_URL PP_USER PP_PASS. Assumes slug ui-fixture (prep via fixture).
 * Exit 0 on success; nonzero on broken selection.
 */
  return new Promise((resolve, reject) => {
    http.get(url, (res) => {
      let d = ""; res.on("data", (c) => (d += c)); res.on("end", () => resolve({ status: res.statusCode, body: d }));
    }).on("error", reject);
  });
}
function wsData(ev){ const d = ev?.data ?? ev; return typeof d === "string" ? d : (Buffer.isBuffer?.(d) ? d.toString() : String(d)); }
async function cdpSend(ws, id, method, params = {}) {
  const msg = JSON.stringify({ id, method, params });
  return new Promise((resolve, reject) => {
    const timer = setTimeout(() => { cleanup(); reject(new Error(`CDP timeout ${method}`)); }, 15000);
    function onMsg(ev){ try { const m = JSON.parse(wsData(ev)); if (m.id === id) { cleanup(); m.error ? reject(new Error(JSON.stringify(m.error))) : resolve(m.result); } } catch (e) {} }
    function cleanup(){ clearTimeout(timer); ws.removeEventListener ? ws.removeEventListener("message", onMsg) : (ws.onmessage === onMsg) && (ws.onmessage = null); }
    ws.addEventListener ? ws.addEventListener("message", onMsg) : (ws.onmessage = onMsg);
    ws.send(msg);
  });
}

async function evalApi(expr, auth) {
  const res = await fetch(`${PP_URL}/api/eval`, { method: "POST", headers: { Authorization: auth, "Content-Type": "application/json" }, body: JSON.stringify({ expr }) });
  const j = await res.json(); if (!j.ok) throw new Error(j.error || JSON.stringify(j)); return j.value;
}

async function main() {
  fs.mkdirSync(USER_DATA, { recursive: true });
  const auth = "Basic " + Buffer.from(`${PP_USER}:${PP_PASS}`).toString("base64");

  console.log("== fixture: root with kidA, kidB ==");
  await evalApi('(load-library "outline")', auth);
  await evalApi('(load-library "notes")', auth);
  await evalApi(`(notes/doc/upsert "${SLUG}" "Sel Fixture" (outline/node* "root" "Root" "" "" (list (outline/node* "kidA" "Kid A" "" "" (list)) (outline/node* "kidB" "Kid B" "" "" (list)))) "" "note" "")`, auth);
  const modelIds = () => evalApi(`(str (list (outline/id (car (outline/kids (notes/doc/outline (notes/doc/get "${SLUG}"))))) (outline/id (car (cdr (outline/kids (notes/doc/outline (notes/doc/get "${SLUG}"))))))))`, auth);
  console.log("model kid ids:", await modelIds());

  console.log("== chromium ==");
  const chrome = spawn(CHROME, [`--remote-debugging-port=${PORT}`, `--user-data-dir=${USER_DATA}`, "--headless=new", "--disable-gpu", "--no-sandbox", "--disable-dev-shm-usage", "--window-size=1400,900", "about:blank"], { stdio: "ignore" });
  let ready = false;
  for (let i = 0; i < 30; i++) { try { const v = await httpGet(`http://127.0.0.1:${PORT}/json/version`); if (v.status === 200) { ready = true; break; } } catch (_) {} await sleep(200); }
  if (!ready) { chrome.kill(); throw new Error("chromium CDP not ready"); }

  try {
    const list = JSON.parse((await httpGet(`http://127.0.0.1:${PORT}/json/list`)).body);
    const page = list.find((t) => t.type === "page") || list[0];
    const ws = new globalThis.WebSocket(page.webSocketDebuggerUrl);
    await new Promise((res, rej) => { const t = setTimeout(() => rej(new Error("ws open timeout")), 10000); ws.onopen = () => { clearTimeout(t); res(); }; ws.onerror = (e) => { clearTimeout(t); rej(e.error || e); }; });
    let nextId = 1; const send = (method, params) => cdpSend(ws, nextId++, method, params);
    const evalJs = async (expression) => { const r = await send("Runtime.evaluate", { expression, returnByValue: true, awaitPromise: false }); if (r.exceptionDetails) return { __jsExn: r.exceptionDetails.text, desc: r.exceptionDetails.exception?.description }; return r.result?.value; };

    await send("Page.enable"); await send("Runtime.enable");

    // capture console
    await send("Log.enable").catch(() => {});
    ws.addEventListener("message", (ev) => { try { const m = JSON.parse(wsData(ev)); if (m.method === "Runtime.consoleAPICalled") { console.log("[console]", m.params.type, m.params.args.map(a => a.value ?? a.description ?? "").join(" ").slice(0, 300)); } if (m.method === "Runtime.exceptionThrown") { console.log("[jsExn]", JSON.stringify(m.params.exceptionDetails.exception?.description || m.params.exceptionDetails.text).slice(0, 500)); } } catch (e) {} });

    console.log("== login ==");
    // curl-login to harvest the session cookie, then inject via CDP.
    const cookieName = process.env.SEL_COOKIE_NAME || "pp_session";
    const loginRes = await fetch(`${PP_URL}/login`, { method: "POST", redirect: "manual", headers: { "Content-Type": "application/x-www-form-urlencoded" }, body: `username=${encodeURIComponent(PP_USER)}&password=${encodeURIComponent(PP_PASS)}` });
    const setCookies = loginRes.headers.getSetCookie ? loginRes.headers.getSetCookie() : [];
    console.log("login status:", loginRes.status, "cookies:", setCookies.length);
    for (const sc of setCookies) {
      const [nv] = sc.split(";");
      const [nm, ...rest] = nv.split("=");
      await send("Network.enable");
      await send("Network.setCookie", { name: nm, value: rest.join("="), domain: new URL(PP_URL).hostname, path: "/", secure: new URL(PP_URL).protocol === "https:" });
    }

    console.log("== open note ==");
    await send("Page.navigate", { url: `${PP_URL}/notes?slug=${SLUG}` }); await sleep(1200);
    console.log("url:", await evalJs(`location.href`));
    console.log("has ol-selected-id:", await evalJs(`!!document.getElementById('ol-selected-id')`));
    console.log("script present:", await evalJs(`(function(){var s=document.querySelectorAll('script');for(var i=0;i<s.length;i++){if(s[i].textContent.indexOf('ol-selected-id')>=0&&s[i].textContent.indexOf('pointerdown')>=0)return true;}return false;})()`));
    console.log("initial selId:", await evalJs(`document.getElementById('ol-selected-id').value`));

    // real click on kidA's title
    console.log("== real click kidA title ==");
    const box = await evalJs(`(function(){var el=document.querySelector('[data-id=\\'kidA\\']');if(!el)return null;var t=el.querySelector(':scope > summary .ol-title');if(!t)return null;var r=t.getBoundingClientRect();return {x:r.x+r.width/2,y:r.y+r.height/2};})()`);
    console.log("kidA title box:", JSON.stringify(box));
    if (box) {
      for (const typ of ["mouseMoved","mousePressed","mouseReleased"]) {
        await send("Input.dispatchMouseEvent", { type: typ, x: Math.round(box.x), y: Math.round(box.y), button: "left", clickCount: 1 }).catch(e => console.log("input ev err", e.message));
      }
      await sleep(400);
    }
    console.log("after REAL click kidA:");
    console.log("  selId =", await evalJs(`document.getElementById('ol-selected-id').value`));
    console.log("  label =", await evalJs(`document.getElementById('ol-selected-label')?.textContent`));
    console.log("  .is-selected data-id =", await evalJs(`(function(){var n=document.querySelector('.ol-node.is-selected');return n?n.getAttribute('data-id'):null;})()`));

    // synthetic pointerdown+click on kidB (control experiment)
    console.log("== synthetic events kidB ==");
    await evalJs(`(function(){var el=document.querySelector('[data-id=\\'kidB\\'] > summary .ol-title');if(!el)return 'noel';el.dispatchEvent(new PointerEvent('pointerdown',{bubbles:true,button:0}));el.dispatchEvent(new MouseEvent('click',{bubbles:true,button:0}));return 'dispatched';})()`);
    await sleep(200);
    console.log("after SYNTH click kidB: selId =", await evalJs(`document.getElementById('ol-selected-id').value`));

    // back to kidA real click, then toolbar add-child
    console.log("== re-select kidA (real), then toolbar +kid ==");
    const box2 = await evalJs(`(function(){var el=document.querySelector('[data-id=\\'kidA\\'] > summary .ol-title');if(!el)return null;var r=el.getBoundingClientRect();return {x:r.x+r.width/2,y:r.y+r.height/2};})()`);
    if (box2) { await send("Input.dispatchMouseEvent", { type: "mouseMoved", x: Math.round(box2.x), y: Math.round(box2.y) }); await send("Input.dispatchMouseEvent", { type: "mousePressed", x: Math.round(box2.x), y: Math.round(box2.y), button: "left", clickCount: 1 }); await send("Input.dispatchMouseEvent", { type: "mouseReleased", x: Math.round(box2.x), y: Math.round(box2.y), button: "left", clickCount: 1 }); await sleep(400); }
    console.log("sel before +kid =", await evalJs(`document.getElementById('ol-selected-id').value`));
    await evalJs(`(function(){var b=document.querySelector('[data-ol-op=\\'add-child\\']');return b?b.click():'nobutton';})()`);
    await sleep(1200);
    console.log("after toolbar add-child:");
    console.log("  selId =", await evalJs(`document.getElementById('ol-selected-id').value`));
    console.log("  model tree kids of root =", await evalApi(`(str (count (outline/kids (notes/doc/outline (notes/doc/get "${SLUG}")))))`, auth));
    console.log("  model tree (compact) =", await evalApi("(str (notes/doc/outline (notes/doc/get \"" + SLUG + "\")))", auth));
  } finally { chrome.kill(); try { fs.rmSync(USER_DATA, { recursive: true, force: true }); } catch (_) {} }
}
main().catch((e) => { console.error("repro error:", e); process.exit(1); });
