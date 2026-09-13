# HANDOFF: window-controlbox-v3 (glochid shell — REGION ships)

You are a fresh agent looping through the make-phase of a designed
feature. The spec is DONE and committed — read it first, it is the
authority:

    borge/filmstrip.borg -> section filmstrip-shell ->
    subsection WINDOW-CONTROLBOX-V3   (status planned)

Supporting background (skim, don't re-litigate):
- borge/filmstrip.borg §window-controlbox-v2 (status implemented) —
  the two-strip model THIS builds on; its agent notes record the
  2026-09-14 review cycle (docs/reviews/glochid-shell-review-2026-09-14.md).
- libs/filmstrip/lib.pp (the shared lib) + agent/lib/surface.ml
  (glochid layer + pages).
- docs/plans/window-controlbox-v2.md (the v2 handoff whose shape this
  copies) — especially its REPORT's harness lore.

## What you are building (one sentence)

Region ships: a pane-head PIN verb flips a pane between region=main
(v2 model: strip + single lifted focused pane) and region=rail (live
BODY rendered inside the rail — desktop-global, dock semantics —
filling the dead middle between cascade and controlboxes), so the
operator can hold reading views (file tree!) beside the main window;
all geometry stays in CSS classes, zero JS, zero inline styles.

## Loop protocol (how you work)

- ONE task per loop (the checklist below, in order). Do the next
  unchecked item, verify it (build + probe green), tick it in this
  file, commit, then end with TASK DONE.
- Work on the laptop repo. Do NOT push. Do NOT deploy. Do NOT touch:
  - the live glochid, its chromium window, systemctl --user glochid,
    or repo-root `.agent/` (scratch copies live under /tmp only)
  - the untracked junk in repo root: bootstrap-picoclaw-repo.ksh,
    libs/qmlsmith/, scripts/glochid-served.sh, spikesizon — never add
  - any `.borg` file (the v3 stanza is FROZEN; if the spec must
    change, run `borge issue new "..."`, keep moving, note it in REPORT)
- Commit after every task that builds and probes green. Stage files by
  name only. Commit style: short first line + a WHY sentence.
- If blocked >2 attempts, write attempts into `## BLOCKERS` here and go
  on; if everything left depends on it, still end the turn.

## Environment facts

- OCaml switch: `eval $(opam env --switch=poohstack --set-switch)`.
  Build: `dune build agent/bin/main.exe`. Tests: `dune runtest lisp/ common/`.
- The glochid UI source is a Nopales program embedded in an OCaml
  string: `agent/lib/surface.ml` (surface_source = agent pages,
  glochid_source = the shell layer). Most edits are inside those
  strings. The shared lib is `libs/filmstrip/lib.pp` — touch it only
  where the spec says lib-level (region state IS lib-level: panes,
  pane-html, open/cascade/pin mutations; the RAIL LAYOUT CSS is
  glochid's glochid/base-css; the pin BOTTON lives in filmstrip's
  pane-head like the v2 verbs).
- Nopales dialect gotchas (agent.borg filmstrip-shell): `let*` is
  SINGLE-BODY (wrap multi-step tails in `(seq ...)`); routes are
  exact-match strings; `render-html` for fragments; html/attr attrs;
  filmstrip/pane-stamp + save-all are the seams that carry extra pane
  fields through write paths.
- THE GATE is scripts/probe_shell_sem.mjs (batch B harness): extend it
  with region invariants FIRST (task 0), keep it ALL GREEN after every
  later task. Decode of its world: I1 visible focused pane; I2
  shelved-click unshelves; I3 winbox freshness; I4 launcher honesty;
  I5 every fragment anchor pane-local; I6 in-pane /code walk; I7 fresh
  panes stamp current desktop. It boots its own scratch agent (own
  /tmp cwd, random port) — never point it at 18090 or repo .agent.
- Scratch boots: `--serve --port 0 --mock-script mock.json` under a
  /tmp agent dir you seed yourself (copy the workspace seed shapes from
  docs/plans/window-controlbox-v2.md). pkill footgun lore (v2 report):
  NEVER run pkill in the same bash invocation that spawns the server;
  use `pkill -f 'main.exe --serve --port <N>'` in its own call, and
  remember pkill's pattern matches YOUR OWN shell's command line
  (split spawn and kill across tool calls).

## Tasks (do in order; tick when green+committed)

- [x] 0. PROBE FIRST (harness only, no product code). Extend
      probe_shell_sem.mjs: (a) snapshot gains pinnedCount + per-pane
      region readout (`data-region`/row fields; region undefined =>
      "main"); (b) I1 widens to "a FOCUSED pane exists and is visible —
      in the MAIN window (lift) or in the rail (region=rail)" — with
      zero region panes this must behave exactly as today; (c) new
      invariant I8 recorded but asserted only from task 2 onward:
      "pinning a pane to the rail leaves its BODY visible in the rail
      while the focused main pane stays visible" (skip until then).
      Show the probe ALL GREEN on the unmodified shell (this proves the
      widened invariants are backward-compatible), commit.

- [x] 1. REGION STATE (lib): pane records carry "region"
      (filmstrip/pane-region accessor, default "main" — like
      pane-state's empty=live). Carry it through EVERY write path:
      panes-emit JSON, and guard save-all-style rebuilds
      (agent/lib/surface.ml glochid/save-all + the v2 lesson: every
      field added must ride all writers). pane-html/oob-section emit
      `data-region` on the section. Workspace round-trip probe: hand-
      seed a record with "region":"rail", GET /, confirm the attribute;
      confirm no visual change yet (rail-region renders like today's
      heads-only row). probe I1-I7 green. Commit.

- [x] 2. PIN VERB + RAIL BODIES: new POST /filmstrip/pin {id} (lib)
      -> flips region, persists, answers the OOB two-section class
      swap (same grammar as filmstrip/width) + winbox/shelf OOBs.
      Register the route in the glochid layer (surface.ml routes
      block — copy the filmstrip/focus-id line's pattern). Pane-head
      gains the pin word next to width/close in filmstrip/win-row
      (agent/lib/surface.ml glochid/win-btn's win-row!) AND in the
      lifted pane's head (filmstrip/pane-head): label "pin"/"unpin"
      by current region. CSS (glochid/base-css): RAIL-REGION panes
      keep their .fs-body VISIBLE inside the rail (overriding the
      heads-only rule with a :has on data-region="rail"), body height
      = available rail share (flex), the cascade cap (max-height 16rem)
      does NOT bound them; the position:fixed lift applies ONLY to
      region=main focused panes (scope the existing `.fs-pane.focused`
      fixed rule to `[data-region="main"]` — check glochid's rail-mode
      selectors + the ROW (row axis) mode too; row mode geometry must
      not break). probe I1 widened + I8 green; unpin returns to the
      strip. Commit.

- [x] 3. FOCUS/OPEN DISCIPLINE IN BOTH REGIONS (lib): focus-id and
      open-apply/cascade/open-or-focus never lift a rail-region pane
      into the fixed main slot — instead the rail pane gets .focused
      (highlight, CSS only) and a MAIN pane stays lifted if one exists;
      if none exists, a focused rail pane visible in the rail satisfies
      I1 (no forced main pane). Healing (glochid/heal-focus,
      first-on-desk) must consider rail-region live panes on the
      current desktop as valid focus targets, and must NEVER heal onto
      a shelved pane (I2 already green — keep it). probe: pin a pane,
      click its winbox focus row -> highlight in place, main pane
      unaffected; focus-id onto a pinned route via cascade click -> same.
      Commit.

- [ ] 4. FILES AS A RAIL PANE: the /code surface is the files view.
      Verify a pinned /code pane with the pane-local tree walk (I6)
      works at rail height (click sub/leaf dirs in the RAIL; same
      window, in-pane updates); the cascade keeps only the app picker
      (column 0) — the code cascade walk may stay available via
      cascade but the REVIEW's ask is satisfied by the rail pane, so
      no cascade work is required beyond ensuring the 16rem cap no
      longer bounds the pinned view (task 2 did; probe here). Seed a
      demo workspace (/chat focused main + /code pinned rail) as the
      probe's default state if useful. Screenshots to /tmp. Commit.

- [ ] 5. FINAL: round-trip + shelf interplay + full gate. Region must
      survive reload (workspace.json -> GET /) and desktop flips
      (rail panes visible on every desktop); shelving a rail-region
      pane shelves it like any pane (shelf row; restore restores
      region); pin/unpin round-trips. Run probe_shell_sem.mjs (must be
      ALL GREEN), the geometry probe (probe_shell_wm.mjs, diffs OK),
      smoke_agent_cdp.mjs (chat regression), dune runtest. Write the
      REPORT section (commits, measured geometry, gaps, harness lore).
      End with COMPLETE.

## Done means

- build green, probe green (probe_shell_sem ALL GREEN with I8 active),
  all tasks ticked, REPORT written
- nothing in repo root .agent/, nothing live touched, no .borg edits

## SPEC SMOKE MAP (borge/filmstrip.borg window-controlbox-v3.verify)

- pin /code -> rail body visible + /chat lifted + zero inline styles:
  tasks 2/4 (probe + screenshot)
- desktop flips + unpin position restore: task 5
- open-or-focus highlights in place: task 3
- probe gate after every mutation: every task (probe GREEN is the tick
  condition)
- shelve/restore of a rail pane: task 5
- pinned files view fills rail height (cascade cap released): task 2/4
- region persists through save-all + reload: tasks 1/5

## PROGRESS (loop-log)

### Task 0 DONE (loop 1, 2026-09-15) — probe FIRST, harness only

- scripts/probe_shell_sem.mjs extended, no product code touched:
  - SNAPSHOT gains per-pane region readout (`panes: {pane, route, region,
    state, focused}` — `data-region` attribute, MISSING attr => "main"),
    plus `focusedRegion`, `pinnedCount` (non-shelved rail panes) and
    `railBodies` ({pane, bodyVisible} per pinned pane).
  - I1 widened: focused pane exists + visible + live + region is "main"
    (lift) or "rail" (visible in rail), asserted on load AND after the
    I6 pane-local walk. Extra backward-compat assertion: with
    `pinnedCount === 0` the focused pane's region must be "main" — so on
    the unmodified shell the widened I1 reduces to exactly the v2 rule.
  - I8 recorded-not-asserted: `checkI8(tag)` prints the observation
    (SKIP + record line) unless `I8_ARMED=1` is exported; when armed it
    asserts (a) at least one pinned rail pane with its `.fs-body`
    visible inside the rail, (b) the focused pane stays visible beside
    it. Arming is the job of the task-2 loop.
- Verified on the UNMODIFIED shell (dune build green first): probe
  ALL GREEN (exit 0) — `SKIP I8 on load ... record pinned=0
  railBodies=[]`, region readout all "main", both I1 lines OK.
  `dune runtest lisp/ common/` green.
- Harness lore: `:scope > .fs-body` selects the pane's own body div
  (needed once panes can carry sibling bodies via OOB swaps).

Still open: tasks 1-5 (region state lib layer, pin verb + rail bodies,
focus/open discipline, files-as-rail-pane, final round-trip + report).

### Task 1 DONE (loop 2, 2026-09-15) — region state in the lib

- libs/filmstrip/lib.pp, no product OCaml touched:
  - NEW `filmstrip/pane-region` accessor (default "main" when the field
    is missing/empty — the ACCESSOR defaults, so selectors + JSON see
    "main" on pre-v3 records, never `data-region=""`; an empty attribute
    would have broken task 2's `[data-region="main"]` scoping of the
    focused-lift rule on legacy records).
  - `filmstrip/pane-html` + `filmstrip/oob-section` now emit
    `data-region` on every pane section.
  - `filmstrip/native-pane-json` (soil-strip policy state) carries
    `region` too — every writer rides.
- Write-path check: `panes-emit` is a pass-through of whole records, so
  region rides save-all automatically; glochid/save-all rebuilds only WS
  fields and needed no change. VERIFIED live: seed p3/p2 with
  region:"rail" -> POST /filmstrip/width {p3} + /filmstrip/restore {p2}
  -> region still "rail" in workspace.json after each; reload keeps it.
- No-visual-change VERIFIED via CDP on the seeded scratch: rail-region
  panes render exactly like today's heads-only rows (322x27, body
  display:none), focused main lift untouched; legacy p1 renders
  data-region="main".
- Verified: probe ALL GREEN (standard region-less seed; I8 still SKIP /
  pinned=0), `dune build agent/bin/main.exe` green, `dune runtest lisp/
  common/` green.
- Harness lore: none new (throwaway CDP check lived in /tmp; the pattern
  above is enough for lib-only state tasks).

Still open: tasks 2-5 (pin verb + rail CSS, focus/open discipline,
files-as-rail-pane, final round-trip + REPORT; I8 arming is task 2).

### Task 2 DONE (loop 3, 2026-09-15) — pin verb + rail bodies, I8 ARMED

- libs/filmstrip/lib.pp:
  - NEW `filmstrip/pane-set-region` (setter beside pane-set-width) and
    `filmstrip/pin` (POST /filmstrip/pin {id}) — flips region main<->rail,
    persists via save-all, answers the SAME grammar as filmstrip/width:
    oob-msg + shelf-oob + winbox-oob + one oob-section (body DOM rides
    through hx-preserve). Unknown id -> status line only.
  - `filmstrip/pane-head` gains the pin word BEFORE width, label "pin"/
    "unpin" by the pane's CURRENT region (rail -> "unpin").
- agent/lib/surface.ml (glochid layer):
  - route registered: `(route "/filmstrip/pin" "filmstrip/pin")` after
    the width line.
  - `glochid/win-row` gains a pin word (win-btn, region-aware label
    pin/unpin) between focus and width.
  - base-css: the fixed LIFT is scoped to
    `.fs-pane.focused[data-region="main"]`; NEW
    `.fs-pane[data-region="rail"]{flex:1 1 auto;...}` +
    `.fs-pane[data-region="rail"] .fs-body{display:block;flex:1 1 auto;
    min-height:0;overflow:auto}` — pinned panes share the rail height by
    flex, the cascade 16rem cap never binds (pinned panes live in
    .fs-content, not the cascade). ROW mode untouched (all rules scoped
    to :has(#fs-axis.col); row has no heads-only rule, bodies already
    visible there — geometry unchanged).
- scripts/probe_shell_sem.mjs: I8 ARMED by default during the pin step
  (I8_ARMED=1 not needed anymore for the assertions; the env var still
  forces the old gate). `checkI8(tag, expectPinned=true)` — the load
  tag passes expectPinned=false (nothing pinned yet: record line, no
  vacuous assertion). NEW pin/unpin step after I6: deskbox BUTTON click
  to desktop 0 (raw httpPost does NOT apply OOB — htmx only applies
  swaps on its own requests; use real clicks or htmx.ajax), then pin
  p2 via `htmx.ajax(...)` (real request machinery — OOB swaps LAND),
  assertions: pinned=1 + focused region main; railBodies=[p2 visible];
  data-region=rail in DOM; win row + pane-head re-labeled "unpin";
  workspace region=rail; then unpin -> all back to main. I8 on load
  still prints the record. Probe ALL GREEN both unsecured and with
  I8_ARMED=1 (exit 0 both ways).
- MEASURED (CDP, /tmp/v3-pin-geom.mjs + /tmp/v3-pin-geom.png, throwaway):
  before pin p2 322x27 (head) body 0x0 hidden, focused pane position:fixed
  1512x825 @ (352,16); after pin p2 322x433 with body 320x406
  overflow:auto INSIDE the rail, focused main pane UNCHANGED (fixed lift),
  cascade cap 118px untouched. Zero NEW inline styles (the one [style] in
  panes is chat/head's model select, pre-existing v2).
- Verified: `dune build agent/bin/main.exe` green, `dune runtest lisp/
  common/` green, probe ALL GREEN (both runs above).
- Harness lore: (a) httpPost-raw responses NEVER mutate the DOM — after a
  raw POST only workspace.json changes; DOM-visible behavior needs an
  htmx-driven request (element click or htmx.ajax with
  target/swap/values). (b) htmx.ajax IS the CDP harness twin of a real
  button: values:{id}, target:#fs-sink, swap:innerHTML — OOB sections in
  the response are processed. (c) chromium reuse footgun: a probe that
  dies before cleanup leaves an orphan chrome holding the CDP port —
  pkill 'remote-debugging-port=<N>' in its OWN tool call, rm -rf the
  /tmp dirs, retry.

Still open: tasks 3-5 (focus/open discipline in both regions, files as
a rail pane + demo seed, final round-trip + geometry probe + chat smoke
+ REPORT).

### Task 3 DONE (loop 4, 2026-09-15) — focus/open discipline in both regions

- libs/filmstrip/lib.pp:
  - NEW `filmstrip/lift-holds-live-main?` (focus points at a LIVE
    main-region pane — that pane owns the fixed lift) + NEW
    `filmstrip/rail-focus-onto panes old new` — THE region-focus rule:
    a rail-region target never takes the persisted focus when a live
    main pane holds the lift (only the OOB ring moves onto it); when no
    live main pane is lifted, the rail pane BECOMES the persisted focus
    (a focused rail pane in the rail satisfies I1). Main targets behave
    exactly as before.
  - `filmstrip/focus-id` rewritten over rail-focus-onto; `filmstrip/open`,
    `filmstrip/cascade` (leaf clicks) and (surface.ml)
    `glochid/resume` compute focus2 the same way. open-response
    signature grew `target` (the pane the ring lands on) distinct from
    `focus2` (the index persisted) — callers updated everywhere.
  - Removal of one reader error I introduced (first-on-desk rewrite
    below missing a close paren — caught by scratch boot, NOT by dune:
    .pp/.pp-embedded strings are read at BOOT). Probe lore: after ANY
    surface.ml-string edit, boot the binary once before trusting dune.
- agent/lib/surface.ml:
  - `glochid/first-on-desk` now two passes: first LIVE MAIN pane on the
    desktop, else first live RAIL pane (rail panes are valid focus
    targets — I1 holds when the desk has no live main); helper
    `glochid/first-region-on-desk`. Never a shelved pane (pane-live?
    gates both passes — I2 kept). heal-focus + desktop-set inherit.
  - win-row focus button title is region-aware ("highlight the rail
    pane in place (no lift)" vs "lift into the main window").
- SEMANTIC MODEL (for task 5): the rail ring is live-DOM state, NOT
  persisted — after reload only the persisted focus (a live main pane
  when one was lifted) wears .focused. Shimmed deliberately; flagged
  for the REPORT as a minor gap (ring does not survive reload; spec
  text does not require it).
- scripts/probe_shell_sem.mjs: I8 now ARMED BY DEFAULT
  (I8_ARMED=0 record-only — task 2's "armed by default" note was
  aspirational; the env gate remained and the default run printed SKIP).
  New v3-task-3 steps after the pin/unpin churn: (a) re-pin p2, real
  winbox focus click -> p2 ringed in place + p1 keeps .focused + lift
  1512x825 + ws focus still content:0 (no rail steal); (b) reload (ring
  is live-DOM state, gone), real cascade leaf click on 'settings'
  (pinned route) -> ring lands fresh on p2, main lift untouched, ws
  focus unmoved; (c) shelve both MAIN panes via real winbox closes
  (second close heals focus onto the pinned pane), reload ->
  focusedRegion=rail, body visible, not shelved (I1 satisfied with no
  forced main lift); restore /chat -> I1 stays green.
- Verified: probe ALL GREEN (default armed + I8_ARMED=0 both exit 0),
  dune build agent/bin/main.exe + dune runtest lisp/ common/ green,
  `borge drift` 0 items.
- Harness lore: (a) JS labels on blocks (`tag: {…}`) are valid; the
  probe uses them for scoping reload steps. (b) The reader error line
  number refers to the boot-loaded source (embedded glochid string),
  not the OCaml file line — grep for the flagged form instead.

Still open: tasks 4-5 (files as a rail pane + demo seed, final
round-trip + geometry probe + chat smoke + REPORT).
