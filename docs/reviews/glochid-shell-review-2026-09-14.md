# UI review: glochid shell (shelf / windows rail / desktops)

> **RESOLVED (2026-09-14, same day):** B1+B2+launcher in
> 22906c9, B4 + /filmstrip new/resume twins in 2db1008, settings h1
> + semantic probe in 9a7bd3b, DESKTOP-STAMP gap closed via the
> filmstrip/pane-stamp hook (22906c9). Spec: v2 status → implemented
> (review smokes added, e993fb1); v3 REGION stanza written + executed
> via the loop → pin/pinned-rail-panes shipped
> (476f4db..55619a5), probe gate 82/82 OK. See batch C below — the
> "multiple side views" ask is now the pinned /code rail pane.

2026-09-14 · reviewer: pi agent · subject: `agent/lib/surface.ml` +
`libs/filmstrip/lib.pp` vs `borge/agent.borg` §filmstrip-shell and
`borge/filmstrip.borg` §window-controlbox-v2.

Method: read both spec stanzas and the implementation, then booted a
scratch glochid (port 18091, seeded workspace, mock LLM), drove real
htmx through headless-chromium CDP (`scripts/probe_shell_wm.mjs`
pattern), measured geometry, and reproduced each reported symptom
end-to-end. Screenshots in `docs/reviews/shots/`. Scratch instance
killed; nothing live touched.

## Verdict, one paragraph

The v2 window machinery (shelf-close, desktops, windows rail) is
implemented faithfully to what `window-controlbox-v2` declares, and it
works. What "feels off" is three real **bugs** (one severe), one
**host-seam miss** (habitat vocabulary leaking into glochid), and one
**design limit** that is in the spec itself: the rail-modes model
(`region: rail | main`, exactly one body on stage) rules out the thing
you most want — more than one reading view visible at once. That last
one is a spec conversation, not a fix.

## 1. Spec assessment (code vs. spec)

- **`window-controlbox-v2` status is `planned` but the work shipped.**
  Tasks 0–5 are ticked with measured geometry in
  `docs/plans/window-controlbox-v2.md` (REPORT section) and commits
  30a142f..7d3f4e1 exist. By the plan's own REPORT two verify smokes
  are PARTIAL (plain-`<a>` escapes enumerated but unfixed;
  open-apply doesn't stamp `desktop`). Honest status: **`partial`**.
  Process note: the make loop updated the plan file but never flipped
  the spec status — status flips belong at the end of a make loop.
- `borge/agent.borg` §filmstrip-shell (status `implemented`) describes
  the pre-v2 col-axis model accurately and remains true; v2 additions
  (shelf/deskbox/winbox in `glochid/shell-inner`) are declared only in
  filmstrip.borg v2 — the cross-reference is in the right place and
  the "host seams" discipline held.
- `borge drift` output matches the pre-existing baseline; no new
  module-level drift introduced by the v2 work.

So: the interface **is** declared in the spec, correctly, in two
layers (mechanism in filmstrip.borg, host seams in agent.borg). The
drift is administrative (stale status), plus the design gap below.

## 2. Confirmed bugs (reproduced, root-caused)

### B1 — clicking a shelved app steals focus and blanks the main window (severe)
Repro: seed `/settings` shelved (`state:"shelved"`), click "settings"
in the cascade. `filmstrip/cascade` → `filmstrip/open-apply` finds the
existing pane **by route, ignoring state**, moves focus onto it, and
the OOB swap emits `<section class="fs-pane fs-half focused shelved">`.
`.fs-pane.shelved{display:none}` wins → **0x0 focused pane, previous
main pane unfocused, main window entirely black**
(`shots/glochid-shelved-click-blank.png`). The only recovery is
realizing the shelf box (bottom of the rail, tiny) has a restore
button. This is your "close settings → shelf → top toolbar does
nothing".
Root cause: open-or-focus has no shelve awareness.
Fix shapes: (a) open-or-focus un-shelves on hit (spectacle-minimal:
shelved + requested ⇒ treat as restore); (b) refuse with an oob-msg
"shelved — restore from shelf". Both are one-pointer diffs in
`filmstrip/cascade`/`open-apply`; (a) matches acme ("get" reopens).

### B2 — windows rail goes stale on open (moderate)
`/filmstrip/open` and `/filmstrip/cascade` responses contain
`oob-msg` + pane swaps but **no `filmstrip/winbox-oob`**. Opening
`/settings` via cascade left the windows box listing only `/chat`
(`shots/glochid-code-cascade.png` bottom-left). It self-heals only on
close/restore/destroy/reload. Fix: call `winbox-oob` (and
`shelf-oob` for symmetry) in the open code paths — the hook already
exists and defaults to `""` for hosts without the rail.

### B3 — the launcher is a dead end dressed as a feature (UX bug)
`filmstrip/launcher-html` ships `placeholder="/todos"` and the empty
input answers "routes start with / **(try /todos)**" — habitat
vocabulary on a host where `/todos` does not exist. You typed
`todos`, got "routes start with /", typed `/todos` (as the hint
suggests), got "filmstrip: no route /todos". The launcher also
duplicates cascade column 0 (which lists exactly the four real
routes) and gives no way to discover what *does* exist. Fix:
host-appropriate hint + at minimum make the error list valid routes
(or hide the launcher on glochid until it earns its place).

### B4 — `/code` pane bodies escape the shell (bug class, cataloged not fixed)
Inside a `/code` pane, every row is a plain `<a href="/code?path=…">`;
htmx never sees it → the **whole window** navigates out of the shell
to a bare page. The plan REPORT enumerates these "leftover teeth"
(chat `agent` anchor, sessions resume/view links, code rows) as the
known open half of the app-link smoke. Fix per spec: pane-local links
must become app-links or `hx-get` targeting `closest .fs-body`.

Also carries over from the v2 REPORT (not re-verified, still true in
code): `filmstrip/open-apply` stamps new panes with no `desktop` field
— app-link opens on desk>0 land hidden on desk 0 until a desktop
switch re-heals.

## 3. Design findings (the "feels off" inventory)

1. **One-body-on-stage is too tight.** Rail mode lifts exactly one
   focused pane into the main window; rail rows are heads-only by
   rule. Your top ask — several live views in the sidebar (file tree +
   chat + sessions) — is *unrepresentable* in the current model. Note
   though: the spec's own vocabulary already names the escape hatch —
   panes gain **`region: rail | main`** in window-controlbox-v2, and
   the code never implements it. A "pin to rail" verb (region=rail ⇒
   body renders in the rail at a flex height; main stays lifted) is a
   *spec-alignment* fix, not new invention.
2. **The cascade can't use the space it has.** In col mode the
   cascade is capped `max-height:16rem`; a `/code` dir walk shows ~8
   entries in a 184px strip while the rail middle sits empty (probe:
   cascade 320x184, dead column-space below). "Files should take over
   the sidebar" = cascade wants to flex to fill (or the file view
   wants to be a rail-region *pane*, not a cascade).
3. **Dead vertical middle.** `#fs-content` is `flex:1` and holds only
   heads-only rows now; with one focused pane the rail is cascade +
   launcher + 400px of nothing + three small controlboxes at the
   bottom (`shots/glochid-baseline.png`). Composition reads
   accidental.
4. **Shelve/unshelve discoverability.** Shelf is at the *bottom*, far
   from the gesture that created it (close button top-right). No
   in-place hint, and B1 makes the naïve recovery path actively
   harmful.
5. **No new-pane story for an empty workspace.** Fresh boot = empty
   shell (REPORT's known pre-existing gap): cascade + dead launcher.
   The shell could open-or-focus `/chat` when the row has zero panes.

## 4. Recommendations

P0 bugs (direct fixes, no spec change needed): **B1** (open-or-focus ⇒
restore), **B2** (winbox-oob in open/cascade responses), **B3**
(launcher hint/visibility), **B4** (code-page links through app-link).

Design (needs a spec/plan pass, candidate `window-controlbox-v3`
stanza): implement **`region`** (rail-region panes with real bodies —
file tree becomes a resident sidebar view; verifies your "multiple
side views" ask in the spec's own words); cascade fills available rail
height; empty-workspace boots `/chat`.

## 5. The process point (how to run UI design formally)

You said you can't do UI review "in your head" like backend. You don't
need taste in your head — you need a checklist the machine can verify.
What worked here and should become the standing protocol:

1. **Design stanza first** (you have this: window-controlbox-v2's doc
   stanza is good spec text — two primitives, invariants named).
2. **Invariants → verify smokes, in the same stanza**, each worded so
   a probe can falsify it ("clicking a shelved app's name never leaves
   a 0x0 focused pane" would have caught B1 in-loop).
3. **Measure, don't look.** The CDP probe pattern
   (`probe_shell_wm.mjs`) already records geometry JSON per mutation —
   extend it with *semantic* assertions (visible focused pane exists;
   controlbox row count == live pane count; zero plain `<a>` inside
   `.fs-body`).
4. **Dogfood log.** One dated paragraph in the habitat `agents` note
   per session of real use — you felt five things off today and three
   were reproducible bugs; that's a good signal-to-effort ratio.
5. **Close the loop in the spec.** A make loop ends with status
   flips; `window-controlbox-v2` sitting at `planned` a day after its
   REPORT is the drift that made today's review start from archaeology.
