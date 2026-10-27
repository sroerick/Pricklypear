#!/usr/bin/env node
/**
 * Card 465ce6a9 UI review: identity-hygiene guidance on the bootstrap surfaces.
 *
 * Drives real headless chromium over CDP (Bearer auth via
 * Network.setExtraHTTPHeaders) against PP_URL and asserts the identity-hygiene
 * block renders INSIDE the agents note body and on /agent.txt +
 * /.well-known/agent.json. Also checks the note body is produced by the
 * registered outline component (.ol-body / .ol-node) rather than bare markup,
 * and that nothing overflows horizontally at 1200x900.
 *
 * Screenshots land in screenshots/465ce6a9/.
 *
 * Env: PP_URL, PP_TOKEN, CHROME (default /usr/local/bin/chrome), SHOT_OUT.
 * Exit 0 when every check passes.
 */
import { spawn } from 'node:child_process';
import fs from 'node:fs';
import http from 'node:http';

const BASE = (process.env.PP_URL || 'http://127.0.0.1:18080').replace(/\/+$/, '');
const TOKEN = process.env.PP_TOKEN || '';
const CHROME = process.env.CHROME || '/usr/local/bin/chrome';
const PORT = Number(process.env.CDP_PORT || 9357);
const OUT = process.env.SHOT_OUT || '/home/al/pricklypear/screenshots/465ce6a9';
const UD = '/tmp/chrome-465-' + process.pid;
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

function httpGet(u) {
  return new Promise((res, rej) => {
    http.get(u, (r) => {
      let d = '';
      r.on('data', (c) => (d += c));
      r.on('end', () => res(d));
    }).on('error', rej);
  });
}

async function main() {
  fs.mkdirSync(OUT, { recursive: true });
  fs.mkdirSync(UD, { recursive: true });
  spawn(CHROME, [
    '--headless=new',
    '--disable-gpu',
    '--no-sandbox',
    '--remote-debugging-port=' + PORT,
    '--user-data-dir=' + UD,
    '--window-size=1200,900',
    'about:blank',
  ], { stdio: 'ignore' });

  let wsUrl = null;
  for (let i = 0; i < 80 && !wsUrl; i++) {
    try {
      const j = JSON.parse(await httpGet('http://127.0.0.1:' + PORT + '/json/version'));
      wsUrl = j.webSocketDebuggerUrl;
    } catch (e) {
      await sleep(250);
    }
  }
  if (!wsUrl) throw new Error('devtools endpoint did not come up');

  const tgt = await (await fetch('http://127.0.0.1:' + PORT + '/json/new?about:blank', { method: 'PUT' })).json();
  const ws = new WebSocket(tgt.webSocketDebuggerUrl);
  await new Promise((res, rej) => {
    ws.onopen = res;
    ws.onerror = () => rej(new Error('ws error'));
  });

  let seq = 0;
  const pend = new Map();
  ws.onmessage = (ev) => {
    const m = JSON.parse(ev.data);
    if (m.id && pend.has(m.id)) {
      const p = pend.get(m.id);
      pend.delete(m.id);
      if (m.error) p.rej(new Error(JSON.stringify(m.error)));
      else p.res(m.result);
    }
  };
  const send = (method, params = {}) =>
    new Promise((res, rej) => {
      const i = ++seq;
      pend.set(i, { res, rej });
      ws.send(JSON.stringify({ id: i, method, params }));
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
  const nav = async (p) => {
    const resp = await send('Page.navigate', { url: BASE + p });
    await sleep(1500);
    return resp;
  };
  const waitFor = async (needle, ms = 25000) => {
    const end = Date.now() + ms;
    while (Date.now() < end) {
      const t = await ev('document.body ? document.body.innerText : ""');
      if (t && t.toLowerCase().includes(needle.toLowerCase())) return true;
      await sleep(500);
    }
    return false;
  };
  const status = async () => ev('document.readyState');
  const shot = async (name) => {
    const m = await send('Page.getLayoutMetrics');
    const c = m.cssContentSize || m.contentSize;
    const h = Math.min(Math.ceil(c.height), 3600);
    const r = await send('Page.captureScreenshot', {
      format: 'png',
      captureBeyondViewport: true,
      clip: { x: 0, y: 0, width: Math.ceil(c.width), height: h, scale: 1 },
    });
    fs.writeFileSync(OUT + '/' + name, Buffer.from(r.data, 'base64'));
    return OUT + '/' + name;
  };
  const axHas = async (needle) => {
    const t = await send('Accessibility.getFullAXTree');
    const names = (t.nodes || []).map((n) => (n.name && n.name.value) || '').join('\n');
    return names.includes(needle);
  };
  const overflow = async () =>
    ev('({scrollW: document.documentElement.scrollWidth, innerW: window.innerWidth, over: document.documentElement.scrollWidth > window.innerWidth + 1})');

  const results = [];
  const check = (label, cond, extra) => {
    results.push({ label, ok: !!cond });
    console.log((cond ? 'PASS ' : 'FAIL ') + label + (extra === undefined ? '' : ' :: ' + JSON.stringify(extra)));
  };

  // 1. live agents note, rendered through the notes/outline components.
  await nav('/notes?slug=agents');
  await waitFor('Identity hygiene');
  const note = await ev(`(function(){
    var b=document.querySelector('.ol-body');
    var btxt=b?b.innerText:'';
    return {title:document.title, ready:document.readyState,
      hasBody:!!b, bodyLen:btxt.length,
      olNodes:document.querySelectorAll('.ol-node').length,
      inBody:btxt.indexOf('Identity hygiene: notes write to YOUR garden')>=0,
      ownOK:btxt.indexOf('under my OWN user')>=0 || btxt.indexOf('under your OWN user')>=0,
      exceptionOK:btxt.indexOf('Cross-user note writes are the exception')>=0,
      boardOK:btxt.indexOf('Filing to the shared Board')>=0,
      routeErr:document.body.innerText.indexOf('Route handler error')>=0};
  })()`);
  check('live agents note: outline body component rendered', note.hasBody, { bodyLen: note.bodyLen, title: note.title });
  check('live agents note: no route-handler error', !note.routeErr);
  check('live agents note: guidance inside .ol-body', note.inBody, { bodyLen: note.bodyLen });
  check('live agents note: default to OWN user', note.ownOK);
  check('live agents note: cross-user writes are the exception', note.exceptionOK);
  check('live agents note: Board filing is not cross-user', note.boardOK);
  check('live agents note: a11y tree exposes the section', await axHas('Identity hygiene'));
  const ov1 = await overflow();
  check('live agents note: no horizontal overflow at 1200x900', !ov1.over, ov1);
  console.log('SHOT ' + (await shot('agents-note.png')));

  // 2. /agent.txt
  await nav('/agent.txt');
  await waitFor('Identity hygiene');
  const at = await ev(`(function(){
    var t=document.body.innerText;
    var l=t.toLowerCase();
    return {len:t.length, pre:!!document.querySelector('pre'),
      head:l.indexOf('identity hygiene'),
      own:l.indexOf('your own user'),
      exc:l.indexOf('cross-user note writes are the exception'),
      board:l.indexOf('shared board'),
      routeErr:l.indexOf('route handler error')>=0};
  })()`);
  check('/agent.txt: identity-hygiene section', at.head >= 0, { len: at.len, pre: at.pre });
  check('/agent.txt: own-user default + exception + Board carve-out', at.own >= 0 && at.exc >= 0 && at.board >= 0);
  check('/agent.txt: no route-handler error', !at.routeErr);
  console.log('SHOT ' + (await shot('agent-txt.png')));

  // 3. /.well-known/agent.json
  await nav('/.well-known/agent.json');
  await waitFor('identity hygiene');
  const aj = await ev(`(function(){
    var t=document.body.innerText;
    var l=t.toLowerCase();
    return {len:t.length, head:l.indexOf('identity hygiene'), own:l.indexOf('your own'),
      exc:l.indexOf('cross-user note writes are the exception'), board:l.indexOf('shared board')};
  })()`);
  check('/.well-known/agent.json: identity-hygiene rule', aj.head >= 0, { len: aj.len });
  check('/.well-known/agent.json: own-user default + exception + Board carve-out', aj.own >= 0 && aj.exc >= 0 && aj.board >= 0);
  console.log('SHOT ' + (await shot('agent-json.png')));

  const bad = results.filter((r) => !r.ok).length;
  console.log('SUMMARY ' + (results.length - bad) + '/' + results.length + ' checks passed');
  ws.close();
  process.exit(bad ? 1 : 0);
}

main().catch((e) => {
  console.error('ERROR ' + e.message);
  process.exit(2);
});
