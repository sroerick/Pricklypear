# HANDOFF: window-controlbox-v2 (glochid shell)

You are a fresh agent looping through the make-phase of a designed
feature. The spec is DONE and committed — read it first, it is the
authority:

    borge/filmstrip.borg -> section filmstrip-shell ->
    subsection WINDOW-CONTROLBOX-V2   (status planned)

Supporting background (skim, don't re-litigate):
- borge/agent.borg -> section filmstrip-shell (what ALREADY landed on
  the glochid host: left-rail col mode, focused-pane lift, heads-only
  rail rows, chatpage height fix; commits 8689bd7, 6468add, da7c3c8)
- docs/plans/filmstrip-v1.md (the v1 handoff whose shape this file copies)

## What you are building (one sentence)

The glochid shell becomes a two-strip window manager: a vertical RAIL
strip of control boxes (apps / files / windows / shelf) beside a
horizontal MAIN niri strip of windows third/half/full wide, with
virtual desktops done by CSS hiding and close-defaults-to-shelf —
all state in the workspace row (Nopales data), zero JS, all geometry
in the `glochid/base-css` CSS block.

## Loop protocol (how you work)

- ONE task per loop (the checklists below, in order). Do the next
  unchecked item, verify it, tick it in this file, commit, then end
  your turn. When ALL items are `[x]`, write a short REPORT section at
  the bottom of this file and end your turn.
- Work on the laptop repo. Do NOT push. Do NOT deploy. Do NOT touch:
  - the live glochid on 127.0.0.1:18090, its chromium window,
    `systemctl --user ... glochid`, or repo-root `.agent/` (that IS the
    live user workspace — the running glochid serves with cwd = repo
    root)
  - the untracked junk in repo root: bootstrap-picoclaw-repo.ksh,
    libs/qmlsmith/, scripts/glochid-served.sh, spikesizon — never
    `git add` them
  - any `.borg` file. If the spec must change, run
    `borge issue new "..."` and keep moving; note it in REPORT.
- Commit after every item that builds and probes green. Stage files by
  name only. Commit style: short first line + a WHY sentence.
- If blocked >2 attempts on one item, write the attempts tried into a
  `## BLOCKERS` section here and go on to the next item (or, if
  everything left depends on it, still end the turn — the operator
  will read BLOCKERS).

## Environment facts

- OCaml switch: run `eval $(opam env --switch=poohstack --set-switch)`
  before dune. Build target: `dune build agent/bin/main.exe`.
- The glochid UI source is a Nopales program embedded in an OCaml
  string: `agent/lib/surface.ml`, variable `surface_source` inside
  `{pp| ... |pp}`. Most of your edits live inside that string, in
  Nopales (a Scheme-family Lisp-1); dune compiles the wrapper.
- Nopales dialect gotchas (from borge/agent.borg filmstrip-shell):
  `let*` is SINGLE-BODY — multi-step handlers wrap the tail in
  `(seq ...)`; `(length ...)` uses cons-pair counting; routes are
  exact-match strings; `dict-get/dict-set*` over row dicts;
  `render-html` for fragments; htmx via html/attr strings.
- The lib (shared with the habitat) is `libs/filmstrip/lib.pp`. Only
  touch it when the spec says a prim is lib-level (the app-link prim
  is; desktop/shelf state is glochid row fields).

## The make loop (harness) — memorize this

1. Build: `eval $(opam env --switch=poohstack --set-switch) && dune
   build agent/bin/main.exe && echo BUILD_OK`
2. Boot a SCRATCH instance (own workspace, own cwd, mock LLM):

       mkdir -p /tmp/wm-test/.agent
       cat > /tmp/wm-test/.agent/workspace.json <<'EOF'
       {"axis":"col","desktop":0,"focus":"content:0","content":[
         {"width":"third","route":"/chat","id":"p1","desktop":0},
         {"width":"half","route":"/settings","id":"p2","desktop":0},
         {"width":"full","route":"/sessions","id":"p3","desktop":1}],"cascade":["/chat"]}
       EOF
       echo '[{"text":"probe reply","calls":[]}]' > /tmp/wm-test/mock.json
       cd /tmp/wm-test && nohup /home/roerick/dev/wyo.tech/pricklypear/_build/default/agent/bin/main.exe \
         --serve --port 18091 --mock-script mock.json > boot.log 2>&1 &
       sleep 3; curl -s -o /dev/null -w '%{http_code}\n' http://127.0.0.1:18091/

   Kill between rebuilds with a NARROW pattern, e.g.
   `pkill -f 'main.exe --serve --port 18091'` — NEVER a bare `pkill -f
   <port>` from a shell whose own command line contains the pattern
   (it will kill itself; learned the hard way).
3. Probe (read-only; screenshots to /tmp, geometry JSON to stdout):

       cd /home/roerick/dev/wyo.tech/pricklypear
       PROBE_URL=http://127.0.0.1:18091/ SHOT=1 node scripts/probe_shell_wm.mjs

   Expected CURRENT baseline (col axis): shell rail 320x825 at (16,16);
   focused pane lifted to (352,16) 1512x825; non-focused rail rows
   322x27 with bodyShown false; deskSentinel null (until task 2).
   After every UI item, re-probe and diff against expectations you
   write down BEFORE running.
4. Interaction tests (when needed): extend NO product code with test
   hooks; drive real htmx via CDP Runtime.evaluate clicking the
   rendered buttons (document.querySelector('.fs-cbox ...').click())
   in the probe if you need click-through assertions, or post forms
   directly with curl to the SCRATCH instance only:
   `curl -s -X POST http://127.0.0.1:18091/filmstrip/orient`.
5. Patterns to copy (exist, don't reinvent):
   - new POST route + OOB swaps: `glochid/orient` in agent/lib/surface.ml
     (reads param, mutates ws row, answers OOB swaps)
   - OOB pane class swap: `filmstrip/oob-section` in libs/filmstrip/lib.pp
   - shell assembly + CSS: `glochid/shell-inner` and `glochid/base-css`
     in agent/lib/surface.ml (the CSS is a Nopales string-append of
     rule strings; keep every rule on its own string)
   - workspace persistence: `glochid/ws-load / ws-save` +
     `glochid/save-all` (already carries `axis` — extend for
     `desktop`; note save-all currently REBUILDS the dict: preserve any
     fields you add through every write path)

## Tasks (do in order; tick when green+committed)

- [x] 0. BASELINE (no product code). Build, boot scratch, run probe,
      record the numbers in a `## MEASURED` section here. Confirm the
      workspace seed above loads all three panes (p3 lives on desktop 1
      — it will RENDER anyway until task 3 exists; note the observed
      rects).

- [x] 1. SHELF = default close. Pane records gain `"state":"live"`.
      POST /filmstrip/close becomes: set state=shelved (NOT delete),
      OOB class-swap the pane section to add class `shelved`, and
      persist. New route POST /filmstrip/destroy {id}= the OLD delete
      (row removal + filmstrip/close-focus hand-off). CSS:
      `.fs-pane.shelved{display:none}`. Shelf UI: a controlbox row in
      the rail (below the strip) listing shelved pane routes, each row
      words: `[route] restore destroy`. Clicking restore POSTs
      /filmstrip/restore {id} (state=live, OOB class swap back).
      Render path: pane sections in glochid/shell-inner (via
      filmstrip/panes-html) must emit the shelved class from row data.
      Probe: seed p2 with state shelved; assert p2 rect is zero/hidden
      and bodyShown irrelevant; curl POST restore then re-probe shows
      it back. `width`/`close` button on a pane head still works —
      close now shelves.

- [x] 2. DESKTOPS. Workspace gains `"desktop":N` (int, default 0); pane
      records carry `"desktop"`. New route POST /filmstrip/desktop
      {d:int} — sets ws desktop, persists, answers with OOB re-render
      of the content strip (swap #fs-content innerHTML — pane DOM of
      OTHER desktops may be dropped per swap; the workspace row keeps
      them; acceptable for v2, note it in REPORT). Render: pane
      sections get `data-desktop="N"` attribute; a sentinel
      `<div id="ws-desk" class="fs-sh-axis dsk-N">` (inert, beside
      #fs-axis) carries the current desktop. CSS (generate the ten
      pairs in the base-css string):
      `.fs-shell:has(#ws-desk.dsk-2) .fs-pane[data-desktop]:not([data-desktop="2"]){display:none}`.
      Controlbox row in the rail: `[d0] [d1] [d2]` words POSTing
      desktop=0/1/2. Probe: seed p3 on desktop 1; as-loaded
      deskSentinel `fs-sh-axis dsk-0`, p3 hidden; curl POST desktop=1;
      re-probe shows p3 visible and p1/p2 hidden. Save-all must carry
      desktop through (same caution as axis).

- [x] 3. WINDOWS rail + focus-by-id. In the lib, add
      `filmstrip/focus-id` (POST id param -> set focus to that pane's
      index, same OOB two-section class swap as filmstrip/focus) and
      register route pattern in the GLOCHID layer (see
      `(route "/filmstrip/focus" ...)` block in surface.ml: add
      `(route "/filmstrip/focus-id" "filmstrip/focus-id")`; keep the
      lib's route table untouched if the lib has its own — glochid
      routes are the shell's). Windows controlbox in the rail: one row
      per LIVE pane of the CURRENT desktop: `[/chat focus] [width]
      [close]` — focus word = POST focus-id. Extend focus healing
      (glochid/shell, exists today: heals unset focus to pane 0) to
      heal to `first live pane on the current desktop`. Probe: with 3
      panes, click-through via curl POST focus-id {id:p3}; re-probe
      shows p3 carrying .focused (lifted at 352,16).

- [x] 4. APP-LINK prim + chat nav repair (the bug that started this:
      clicking `code` in the chat pane's nav did a full-window GET and
      escaped the shell). In libs/filmstrip/lib.pp add
      `filmstrip/app-link` (lambda (route label)) emitting a button
      styled as a link (class fs-applink; CSS: bare link look, padding
      0) with hx-post /filmstrip/open — check filmstrip/open's
      parameter shape (route param, open-or-focus semantics) in
      lib.pp. In agent/lib/surface.ml, the chat surface's
      "agent — /code /sessions /settings" nav
      (chat/body fragment chrome) re-authors to app-links. Probe:
      seed workspace with only the chat pane focused; via CDP
      evaluate a click on the rendered app-link for /code; re-probe:
      the shell is STILL present (dot .fs-shell exists, rail rows
      exist) and a /code pane was open-or-focused. None of this may
      require a page reload.

- [x] 5. FINAL. Seed the 8-pane 3-desktop workspace from the spec's
      smoke line (invent routes from the glochid surface list:
      /chat /code /sessions /settings; extra routes may 404 inside
      panes — acceptable for geometry probing, but prefer existing
      ones). Probe all states; save 3 screenshots to /tmp
      (wm-f0.png..). Run the chat-surface regression:
      `dune runtest` (lisp/ + common/ need no services) and
      `node scripts/smoke_agent_cdp.mjs` if it passes preconditions —
      if it needs the mock-agent boot it should work (it boots its
      own). Write REPORT.

## MEASURED (task 4, 2026-09-13; scratch 18091, build + `dune runtest lisp/ common/` green)

Seed: ONE chat pane, full width, live, focused, desktop 0 (the bug's
minimal state: nav anchors in a lifted pane navigate the whole window
out of the shell).

- lib prim `filmstrip/app-link` (route label): link-look BUTTON, class
  `fs-applink`, `hx-post /filmstrip/open` + `hx-vals {"route":...}` +
  `hx-target #fs-sink` (same shape as the key sentinels — open-or-focus,
  OOB pane swaps answer). Returns a PLAIN string: render-html inserts
  the `__html__:` sentinel which is only unwrapped at the route
  boundary — embedding it in chat/body's string-append would leak the
  sentinel into the page. html/button is string-append based, so the
  prim is safe to embed mid-string.
- CSS in glochid/base-css (covers pane + any non-shell render):
  `.fs-applink{background:none;border:0;color:#7aa2f7;padding:0;margin:0;
  font:inherit;cursor:pointer;text-decoration:none}` + hover underline —
  bare link look over the generic blue button.
- chat/body h1 nav re-authored: "agent — /code /sessions /settings —
  model:" — the three nav words are app-links now; the `agent` root
  anchor is left as an anchor (untouched per scope; it reloads the
  shell at `/` in place). NOTE for later: the resumed-session line's
  `+ new` and `/sessions` links and the /code page's own h1 anchors
  still navigate out — REPORTed as leftover teeth, not fixed here.
- REAL htmx click-through (CDP Runtime.evaluate btn.click() on the
  rendered /code app-link, window marker set before the click):
  sameWindow true (marker + href intact — NO navigation), `#fs-shell`
  and `#fs-sink` still present, pane count 1→2, the new /code pane is
  `.fs-pane.fs-third.focused` lifted 1512x825 @ (352,16) with the code
  page dispatched in its body, chat pane dropped back to a 322x27 rail
  head, 0 JS errors (screenshot /tmp/wm-applink-code.png).
- p2 audit via curl (HX-Request): POST /filmstrip/open route=/settings
  with /code already present → appended a new pane, focus content:2,
  workspace.json carried the new record (state live, desktop absent →
  defaults to desk 0). Re-POST /settings → open-OR-FOCUS: single OOB
  section re-swap focused, no duplicate row (find-route-index works).
- reload persistence: fresh GET renders all three panes, /settings
  focused + lifted, no errors.
- KNOWN v2 gap (compile into REPORT): filmstrip/open-apply stamps new
  pane records WITHOUT "desktop" — a pane opened by app-link on a desk>0
  lands on desk 0 and is hidden there until a desktop switch re-heals
  focus. Acceptable for v2 (desktop state is glochid row fields, the
  lib must not read them); noted, not fixed.
- `dune runtest lisp/ common/` green; zero JS; nothing live touched.

## MEASURED (task 3, 2026-09-13; scratch 18091, build + `dune runtest lisp/ common/` green)

Seed: plan seed (p1 /chat third live focused desk 0, p2 /settings half
live desk 0, p3 /sessions full live desk 1; ws desktop 0, focus content:0,
axis col).

- windows controlbox exists: `#fs-winbox` (class `fs-cbox fs-winbox`,
  data-cbox windows) between #fs-content and #fs-deskbox — 320x79 (label
  + one row per live pane of the CURRENT desktop: p1+p2; p3 excluded on
  desk 1). Each row: `[route] focus width close` — focus POSTs
  /filmstrip/focus-id, width/close ride the pane-head verbs. Content
  strip narrowed 583→425 to make room (winbox above deskbox above shelf).
- curl POST /filmstrip/focus-id {id:p3}: persisted + same OOB two-section
  class swap as focus (p1 section unfocused, p3 section focused); first
  cut had NO save-all — caught by the probe (workspace.json focus stayed
  content:0), fixed. Shelved/unknown id answers "no such live pane";
  already-focused id answers empty (mirrors filmstrip/focus). Re-probe:
  p3 `.fs-pane.fs-full.focused` at rect 0x0 — CORRECT (desk 0 hides it;
  .focused landed without lifting, no JS).
- POST desktop d=1: p3 already focused → lifts 1512x825 @ (352,16);
  focus content:2 carried through (screenshot /tmp/wm-f1-desk1-p3-focused.png)
- REAL htmx click-through (CDP Runtime.evaluate on the rendered win-row
  focus button, no reload): click /settings focus → .focused lands on p2,
  focus persists content:1
- winbox freshness: POST close {id:p2} answers ONE #fs-winbox OOB (1 row —
  p2's row dropped) + shelf gains /settings row; POST restore {id:p2} →
  winbox 2 rows again, state live re-persisted. Done via a second lib
  hook, `filmstrip/winbox-oob` (default "", like shelf-oob) called from
  close/restore/destroy responses; glochid redefines it over the row.
- healing is DESKTOP-AWARE now (glochid/heal-focus →
  glochid/first-on-desk panes (glochid/deskt) 0, fallback
  first-live-index when the current desktop has none): distinguishing
  seed w1 desk-1 live + w2 desk-0 live, ws desktop 0, focus unset → GET /
  lifts w2 (352,16), not w1; winbox shows only w2. Still render-time
  only (not persisted), per the existing contract.
- `dune runtest lisp/ common/` green; zero JS; no JS errors in any probe;
  nothing live touched

Checkpoint note: the pkill footgun has a SECOND tooth — the bracket
trick does NOT save you when the same invocation also SPAWNS the server
(the wrapper `bash -c` cmdline then contains the literal unbracketed
pattern from the spawn text and pkill kills the worker shell). Always
split pkill and boot into separate tool calls.

## MEASURED (task 2, 2026-09-13; scratch 18091, build + `dune runtest lisp/ common/` green)

Seed: p1 /chat third live desk 0 FOCUSED, p2 /settings half live desk 0,
p3 /sessions full live desk 1; ws desktop 0, axis col.

- as loaded: deskSentinel `fs-sh-axis dsk-0` (#ws-desk exists, inert,
  beside #fs-axis); every pane section carries `data-desktop`; p3 (desk 1)
  HIDDEN — rect 0x0 via the `:has(#ws-desk.dsk-0)` CSS pair, no JS;
  p1 lifted 1512x825 @ (352,16); p2 rail row 322x27 @ (16,258)
- desktop controlbox exists: `#fs-deskbox` (class `fs-cbox fs-deskbox`,
  data-cbox desktop) between #fs-content and #fs-shelf — deskbox 320x30,
  shelf now 320x30 below it
- curl POST /filmstrip/desktop d=1: response = OOB msg + `#ws-desk`
  dsk-1 outerHTML swap + `#fs-content` re-render (all panes keep their
  data/desktop records; bodies via hx-preserve); workspace.json now has
  `"desktop":1` AND `"focus":"content:2"` — focus healed onto the first
  live pane of the target desktop (rail discipline: a main pane ALWAYS
  exists; not required by the task text, noted as an addition)
- re-probe after d=1: deskSentinel dsk-1; p1/p2 rect 0x0 (hidden);
  p3 `.fs-pane.fs-full.focused` lifted 1512x825 @ (352,16), body dispatched
- reload persistence: GET / re-renders dsk-1; POST d=0 restores
  dsk-0 + focus content:0 (heals onto p1); round-trip verified both ways
- CDP click-through (real htmx, no reload — window marker survived):
  d1 click → sentinel dsk-1 + p3 display none→flex + .focused lands on
  p3; d0 click → back. Booking the deskbox buttons POSTs exactly
  /filmstrip/desktop {d:'N'}
- `dune runtest lisp/ common/` green; zero JS added; no JS errors in
  any probe; nothing live touched

v2 DOM caveat (per plan, acceptable, REPORTed): the #fs-content OOB
re-render re-dispatches EVERY pane's fragment server-side; panes of the
desktops you are LEAVING keep their slot in the row but their body DOM
is re-created (hx-preserve only survives for panes present in the new
HTML, which is all of them here — count is unchanged). Also: switching
desktop re-renders in-pane chat scroll state of the desktop you left.
Workspace rows are untouched — only the swapped DOM shrinks back on
return via fresh dispatch.

Process note for future loops: pkill/nohup within one bash invocation
must use the `[.]` bracket trick in the pattern (the plan's own
caveat); a bare quoted pattern killed the worker shell mid-command and
produced phantom boot failures.

## MEASURED (task 1, 2026-09-13; scratch 18091, build green)

All done on the laptop repo, scratch instance only (seed: p1 /chat third
live focused, p2 /settings half SHELVED, p3 /sessions full live desktop 1;
axis col):

- as loaded: p2 renders `fs-pane fs-half shelved` at rect 0x0 (display:none)
  — shelved class comes from row data through filmstrip/pane-html; p1 lifted
  1512x825 @ (352,16) focused; p3 heads-only rail row 322x27 @ (16,258)
  (one row HIGHER than baseline — the shelved pane no longer takes a rail row;
  content strip narrowed 583→522 to make room for the shelf controlbox)
- shelf controlbox exists: `#fs-shelf` (class `fs-cbox fs-shelf`, data-cbox
  shelf) below #fs-content, rect 320x54 @ (16,787) with one row; only the
  label remains when nothing is shelved (320x30)
- curl POST /filmstrip/restore {id:p2}: OOB shelf swap (empty again) + OOB
  section swap WITHOUT shelved + state=live persisted; re-probe: p2 back as a
  322x27 rail row @ (16,258)
- curl POST /filmstrip/close {id:p1} on the FOCUSED pane: state=shelved
  persisted, focus healed content:0 -> content:1 (first live = p2), OOB
  swap adds shelved class + shelf gains a /chat row; re-probe: p2 lifted
  1512x825 @ (352,16), p1 0x0
- curl POST /filmstrip/destroy {id:p2}: OOB delete:#fs-pane-p2, row removed,
  focus hand-off content:1 -> content:0 (the old close semantics verbatim)
- width cycle on a live pane still works (p3 full -> third)
- `dune runtest lisp/ common/` green; no JS errors in any probe

Note: the probe seed now stamps `state` on each pane record; old records
without `state` render live (pane-state defaults empty to live).

## MEASURED (task 0 baseline, 2026-09-13, build green, scratch 18091)

Probe `scripts/probe_shell_wm.mjs` with the plan's seed workspace
(p1 /chat third focused, p2 /settings half desktop 0, p3 /sessions full
desktop 1; axis col):

- axisSentinel: `fs-sh-axis col` · deskSentinel: null (no `#ws-desk` yet,
  as expected pre-task-2)
- shell `#fs-shell`: 320x825 @ (16,16) — matches design expectation
- rail strip: `.fs-rail` selector matches nothing (null) — the rail is
  currently built from `#fs-cascade` (320x126 @ (16,16)) + `#fs-content`
  (320x583 @ (16,258)) inside the shell, not a `.fs-rail` element
- focused pane p1 lifted into main window: 1512x825 @ (352,16), cls
  `fs-pane fs-third focused`, bodyShown true
- non-focused panes are heads-only rail rows: p2 322x27 @ (16,258),
  p3 322x27 @ (16,294), both bodyShown false
- p3 (desktop 1 in the seed) RENDERS as a rail row anyway — confirmed;
  no desktop hiding exists pre-task-2, no `data-desktop` attributes yet
- controlboxes: none with `.fs-cbox` class (pane-head tag rows exist but
  unclassified) · JS errors: none
- screenshot: /tmp/wm-col.png

Implication for later tasks: CSS written against `.fs-rail` (if any)
should target the existing shell structure, or the rail wrapper needs
introducing with the controlbox work.

## Done means

- build green, probe green on every claimed behavior, all tasks ticked
- REPORT section below lists: commits (short hashes), measured
  geometry changes, any BLOCKERS, any borge issues filed
- nothing in repo root .agent/, nothing live touched, tasks list fully
  checked

## REPORT (window-controlbox-v2 — done 2026-09-13, loops 0-6)

All five tasks built, probed green, and committed per-loop. Nothing
live touched: no 18090, no chromium, no `systemctl --user glochid`, no
repo-root `.agent/` writes (scratch copies live under /tmp only).

### Commits
- 25f2805 probe harness · c04712d spec stanza · f74ae27 plan handoff
- b796648 task 0 baseline · 30a142f task 1 shelf-close · 5009433
  task 2 desktops · 3c1e3a2 task 3 windows rail · 8aae00f task 4
  app-link · (this loop) task 5 final smoke + self-contained CDP smoke

### MEASURED (task 5, final 8-pane 3-desktop smoke; scratch 18091)
Seed: 8 panes over 3 desktops, axis col — desk0 p1 /chat third (focused)
p2 /settings half, p3 /code third; desk1 p4 /sessions half p5 /chat half
p6 /settings full; desk2 p7 /code half, p8 /sessions half SHELVED.

- desktop 0: deskSentinel dsk-0; p1 lifted 1512x825 @ (352,16) focused,
  p2/p3 heads-only rail rows 322x27; desks 1+2 hidden (5 panes rect 0x0);
  winbox 320x104 (3 rows), deskbox 320x30, shelf 320x54 (p8 row); 0 JS errors
- POST desktop=1: sentinel dsk-1; p4 healed-focused lifted 1512x825 @
  (352,16), p5/p6 rail rows; 5 hidden; workspace.json persisted
  desktop:1 + focus content:3 (screenshot /tmp/wm-f1.png)
- POST desktop=2: sentinel dsk-2; p7 healed-focused lifted 1512x825 @
  (352,16); 7 hidden (incl. shelved p8); winbox shrunk to 320x54 (1
  live row), shelf keeps p8 (screenshot /tmp/wm-f2.png, /tmp/wm-f0.png
  for desk 0)
- SPEC SMOKE "renders from state classes only (no inline styles)": every
  pane is `<section class="fs-pane …" data-desktop=…>` with ZERO style=
  attributes — geometry comes entirely from .fs-third/.fs-half/.fs-full/
  .focused/.shelved classes + the :has(#ws-desk.dsk-N) hiding rules.
  DOM for ALL desktops (incl. hidden) stays in the page (bodies
  hx-preserve'd), so desktop flips chat scroll state (task-2 caveat).

### Regression net
- `dune build agent/bin/main.exe` green; `dune runtest lisp/ common/`
  green (exit 0).
- `scripts/smoke_agent_cdp.mjs` was REPORTING-ready but its bare-`/`
  precondition broke when the glochid shell landed: a FRESH glochid
  boot now renders an EMPTY strip (no default chat pane, no #f form).
  Fixed the harness (script, not product): it now seeds its own
  /tmp agent cwd with a minimal workspace (one focused /chat pane) and
  spawns `--serve` with that cwd — no longer inherits the caller's
  .agent (which also protects repo-root .agent/). Green end-to-end:
  "turn streamed: 3 parts, DOM has user+llm bubbles + assistant text".

### Known gaps / leftovers (not fixed, by scope)
- filmstrip/open-apply stamps new panes without "desktop" — an
  app-link open on desk>0 lands on desk 0 hidden until a desktop
  switch re-heals focus. Lib must not read glochid row fields;
  documented in task 4.
- Nav teeth that still navigate out of the shell as plain anchors:
  resumed-session `+ new` / `/sessions` links, /code page h1 + rows
  anchors, and the chat h1 `agent` root anchor. Each is a candidate
  app-link conversion in a follow-up pass.
- The desktop POST re-renders #fs-content server-side: bodies of
  panes on the desktop you LEFT are re-dispatched (scroll state reset,
  hx-preserve survives only same-count swaps) — per-frame DOM cost of
  the v2 CSS-hiding model, row data untouched.
- Fresh glochid boot = empty workspace (no default pane). Pre-existing
  glochid behavior, now covered by the self-contained smoke.

### Harness lore (learned this loop)
- pkill kills your own shell when the same bash invocation also SPAWNS
  the server (the wrapper cmdline contains the pattern) — always split
  pkill and boot into separate tool calls (loop 3 note, still true).
- smoke_agent_cdp needs a seeded chat workspace ("OK agent at…" then
  `FAIL chat form #f not found` otherwise) — now built in.
- SMOKE/EXIT codes: the smoke script calls process.exit(1) only on the
  fail() path through its own exit handler — always read the OK/FAIL
  lines, not the shell exit status.

### Spec smokes (borge/filmstrip.borg window-controlbox-v2.verify)
- desktop flip via sentinel class: VERIFIED (task 2 + task 5)
- shelve/restore with DOM intact to desktop+position: VERIFIED (task 1,
  restore returns the pane to its row position)
- app-link open-or-focus, plain <a> never escapes: app-link VERIFIED by
  real CDP click (task 4); plain-<a>-never-escapes PARTIAL — existing
  teeth enumerated above are bugs of that class, left open
- geometry probe of a seeded 8-pane 3-desktop workspace: VERIFIED (task 5)
