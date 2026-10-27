#!/usr/bin/env node
/**
 * Card 508335a4 UI review: dabar menu must not re-load recursively.
 *
 * Drives real headless chromium over CDP (Bearer auth via
 * Network.setExtraHTTPHeaders) against PP_URL, opens /dabar, then:
 *   - asserts exactly one #dabar-panel, inside a filmstrip pane;
 *   - changes the chapter select (htmx change trigger) and submits the
 *     go form, three times, asserting the panel never nests inside itself;
 *   - clicks the interlinear toggle and asserts the same.
 * Screenshot: screenshots/508335a4/dabar-menu.png
 *
 * Env: PP_URL, PP_TOKEN, CHROME (default /usr/local/bin/chrome), SHOT_OUT.
 */
import { spawn } from 'node:child_process';
import fs from 'node:fs';
import http from 'node:http';

const BASE = (process.env.PP_URL || 'http://127.0.0.1:18080').replace(/\/+$/, '');
const TOKEN = process.env.PP_TOKEN || fs.readFileSync('/home/al/.config/pricklypear/pp.token', 'utf8').trim();
const CHROME = process.env.CHROME || '/usr/local/bin/chrome';
const PORT = Number(process.env.CDP_PORT || 9361);
const OUT = process.env.SHOT_OUT || '/home/al/pricklypear/screenshots/508335a4';
const UD = '/tmp/chrome-508335a4-' + process.pid;
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

function httpGet(u) {
  return new Promise((res, rej) => {
    http.get(u, (r) => { let d = ''; r.on('data', (c) => (d += c)); r.on('end', () => res(d)); }).on('error', rej);
  });
}

async function main() {
  fs.mkdirSync(OUT, { recursive: true });
  fs.mkdirSync(UD, { recursive: true });
  spawn(CHROME, ['--headless=new', '--disable-gpu', '--no-sandbox',
    '--remote-debugging-port=' + PORT, '--user-data-dir=' + UD,
    '--window-size=1200,900', 'about:blank'], { stdio: 'ignore' });

  let wsUrl = null;
  for (let i = 0; i < 80 && !wsUrl; i++) {
    try { wsUrl = JSON.parse(await httpGet('http://127.0.0.1:' + PORT + '/json/version')).webSocketDebuggerUrl; }
    catch (e) { await sleep(250); }
  }
  if (!wsUrl) throw new Error('devtools endpoint did not come up');

  const tgt = await (await fetch('http://127.0.0.1:' + PORT + '/json/new?about:blank', { method: 'PUT' })).json();
  const ws = new WebSocket(tgt.webSocketDebuggerUrl);
  await new Promise((res, rej) => { ws.onopen = res; ws.onerror = () => rej(new Error('ws error')); });

  let seq = 0; const pend = new Map();
  const events = [];
  ws.onmessage = (ev) => {
    const m = JSON.parse(ev.data);
    if (m.method) events.push(m);
    if (m.id && pend.has(m.id)) {
      const p = pend.get(m.id); pend.delete(m.id);
      if (m.error) p.rej(new Error(JSON.stringify(m.error))); else p.res(m.result);
    }
  };
  const send = (method, params = {}) => new Promise((res, rej) => {
    const i = ++seq; pend.set(i, { res, rej }); ws.send(JSON.stringify({ id: i, method, params }));
  });

  await send('Page.enable');
  await send('Runtime.enable');
  await send('Network.enable');
  await send('Accessibility.enable');
  await send('Network.setExtraHTTPHeaders', { headers: { Authorization: 'Bearer ' + TOKEN } });

  const ev = async (expr) => {
    const r = await send('Runtime.evaluate', { expression: expr, returnByValue: true, awaitPromise: true });
    if (r.exceptionDetails) throw new Error('eval failed: ' + JSON.stringify(r.exceptionDetails));
    return r.result.value;
  };
  const shot = async (name) => {
    const m = await send('Page.getLayoutMetrics');
    const c = m.cssContentSize || m.contentSize;
    const h = Math.min(Math.ceil(c.height), 2400);
    const r = await send('Page.captureScreenshot', { format: 'png', captureBeyondViewport: true,
      clip: { x: 0, y: 0, width: Math.ceil(c.width), height: h, scale: 1 } });
    fs.writeFileSync(OUT + '/' + name, Buffer.from(r.data, 'base64'));
    return OUT + '/' + name;
  };

  const results = [];
  const check = (label, cond, extra) => {
    results.push({ label, ok: !!cond });
    console.log((cond ? 'PASS ' : 'FAIL ') + label + (extra === undefined ? '' : ' :: ' + JSON.stringify(extra)));
  };

  await send('Page.navigate', { url: BASE + '/dabar' });
  await sleep(2500);
  for (let i = 0; i < 40; i++) {
    if (await ev('document.readyState') === 'complete') break;
    await sleep(500);
  }
  await sleep(1500);

  const docResp = events.filter((e) => e.method === 'Network.responseReceived' && e.params.type === 'Document').pop();
  const status = docResp ? docResp.params.response.status : null;
  const state = await ev(`(function(){
    var panels=[].slice.call(document.querySelectorAll('#dabar-panel'));
    var nested=[].slice.call(document.querySelectorAll('#dabar-panel #dabar-panel'));
    var pane=panels[0]?panels[0].closest('.fs-pane'):null;
    var main=document.querySelector('main');
    return {
      title:document.title, status:null, panels:panels.length, nested:nested.length,
      inPane:!!pane, paneRoute:pane?pane.getAttribute('data-route')||pane.id:null,
      menuForms:document.querySelectorAll('#dabar-panel form.bible-go, #dabar-panel form.bible-search').length,
      interToggle:document.querySelectorAll('#dabar-panel .dabar-inter-toggle').length,
      mainText:(main?main.innerText:document.body.innerText||'').slice(0,300),
      h1:(document.querySelector('#dabar-panel h1')||{}).innerText||null
    };
  })()`);
  console.log('DOC ' + JSON.stringify({ status, ...state }));
  const shot1 = await shot('dabar-menu.png');
  console.log('SHOT ' + shot1);

  check('document status 200', status === 200, status);
  check('page is not a route-handler error', !/Route handler error/.test(state.mainText), state.mainText.slice(0, 140));
  check('exactly one #dabar-panel', state.panels === 1, state.panels);
  check('#dabar-panel inside a filmstrip pane', state.inPane, state.paneRoute);
  check('menu go + search forms present', state.menuForms === 2, state.menuForms);

  if (status === 200 && state.panels === 1) {
    // ACT: change chapter via the htmx change trigger, three times.
    for (const chapter of ['4', '5', '6']) {
      const before = await ev(`(document.querySelector('#dabar-panel h1')||{}).innerText||''`);
      await ev(`(function(){
        var s=document.querySelector('#dabar-panel select[name=chapter]');
        s.value=${JSON.stringify(chapter)};
        s.dispatchEvent(new Event('change',{bubbles:true}));
      })()`);
      let after = before;
      for (let i = 0; i < 30; i++) {
        await sleep(400);
        after = await ev(`(document.querySelector('#dabar-panel h1')||{}).innerText||''`);
        if (after && after !== before) break;
      }
      const s2 = await ev(`(function(){
        var panels=[].slice.call(document.querySelectorAll('#dabar-panel'));
        return {panels:panels.length, nested:document.querySelectorAll('#dabar-panel #dabar-panel').length,
          inPane:!!(panels[0]&&panels[0].closest('.fs-pane')),
          menu:document.querySelectorAll('#dabar-panel form.bible-go').length,
          forms:document.querySelectorAll('#dabar-panel form.bible-go, #dabar-panel form.bible-search, #dabar-panel .dabar-inter-toggle').length};
      })()`);
      check('chapter ' + chapter + ' swap: one panel, no nesting', s2.panels === 1 && s2.nested === 0 && s2.inPane, s2);
      check('chapter ' + chapter + ' swap: full menu still present', s2.forms === 3, s2.forms);
    }
    // ACT: interlinear toggle (verse study view).
    await ev(`(function(){
      var a=document.querySelector('#dabar-panel .dabar-inter-toggle');
      if(a) a.click();
    })()`);
    await sleep(3000);
    const s3 = await ev(`(function(){
      var panels=[].slice.call(document.querySelectorAll('#dabar-panel'));
      return {panels:panels.length, nested:document.querySelectorAll('#dabar-panel #dabar-panel').length,
        inPane:!!(panels[0]&&panels[0].closest('.fs-pane')),
        menu:document.querySelectorAll('#dabar-panel form.bible-go').length};
    })()`);
    check('interlinear toggle: one panel, no nesting, menu intact', s3.panels === 1 && s3.nested === 0 && s3.inPane && s3.menu === 1, s3);
  }

  // Live fragment contract from the browser (real server, real headers).
  const frag = await ev(`fetch('/dabar?book=John&chapter=4',{headers:{'HX-Request':'true'}}).then(r=>r.text()).then(t=>({head:t.slice(0,80), hasPanel:/^\s*<div id="dabar-panel">/.test(t), outer:/hx-swap="outerHTML"/.test(t), inner:/hx-target="#dabar-panel"/.test(t)}))`);
  check('HX fragment carries #dabar-panel + outerHTML swap', frag.hasPanel && frag.outer && frag.inner, frag);

  const axt = await send('Accessibility.getFullAXTree');
  const names = (axt.nodes || []).map((n) => (n.name && n.name.value) || '').join('\n');
  check('a11y names include Book/Chapter/Go', /Book/.test(names) && /Chapter/.test(names) && /Go/.test(names));

  await shot('dabar-menu-after.png');
  const failed = results.filter((r) => !r.ok);
  console.log('SUMMARY ' + JSON.stringify({ pass: results.length - failed.length, fail: failed.length, failed: failed.map((f) => f.label) }));
  ws.close();
  process.exit(failed.length ? 1 : 0);
}

main().catch((e) => { console.error('ERROR ' + e.message); process.exit(2); });
