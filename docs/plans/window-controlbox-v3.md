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

- [ ] 2. PIN VERB + RAIL BODIES: new POST /filmstrip/pin {id} (lib)
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

- [ ] 3. FOCUS/OPEN DISCIPLINE IN BOTH REGIONS (lib): focus-id and
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
