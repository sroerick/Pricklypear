# Handoff: ship filmstrip main-strip-v5 (the niri strip) on the habitat

Date: 2026-10-01 · Author: pi session with roerick · Board card: on `Board`
(title "filmstrip main-strip-v5: verify gate, ship, flip status")

## What this is

`borge/filmstrip.borg` subsection `main-strip-v5` (status **planned**): the
main region of the filmstrip shell becomes the niri-style horizontal strip
again — live main panes render simultaneously as columns of one overflow
row; focus is on-stage with neighbor peeks via scroll-margin (NO JS, NO
inline styles); the `single` pane field + one-up verb stages a pane alone,
persisted in the workspace row; close-heal targets the adjacent strip
column; pinned rail panes are not strip columns.

**The code already shipped** (`c136875` "filmstrip main-strip-v5: the main
region is the niri strip again", plus follow-ups `ea74182`, `3b347a2`,
`c656a07`, `21303d3`, `750b494`). What did NOT ship is the **gate**: the
2026-09-16 make note in the stanza requires `scripts/probe_shell_sem.mjs`
(re-written S1..S4 strip invariants) ALL GREEN **on the main tree**, and
that re-run was blocked by habitat issue
`i-1789591788-8d507ea1-4455-4421-97bd-ae8e9a3705b2` — the scratch-agent boot
shell render blew the 400k eval budget because of same-day uncommitted
libs edits (measured: tree-boxes ~94k clean vs ~224k working tree). Those
edits are long since committed; whether the budget is healthy again is
UNVERIFIED. That is task 1 of this handoff.

## Why now

2026-10-01 nomenclature lock (borge agent note in filmstrip.borg):
filmstrip = the window-manager interface; glochid = the local agent
harness (its screen is a filmstrip). Same session, roerick's product
decision: filmstrip becomes the default way to operate pricklypear.
Landing wiring already done (git `7ca1f53`: `/` redirects authed users to
`/filmstrip`; post-login default `/filmstrip`; vault route footer so fresh
habitats boot the shell). main-strip-v5 is the flagship window-management
upgrade that makes the default view worth landing on — without it the
shell still shows exactly one on-stage main pane at a time (lift model),
which the 2026-09-14 review already flagged as the design limit.

## Work plan

1. **Budget re-probe** (kernel-lab, no prod). Boot a scratch glochid off
   the CURRENT main tree the way the 2026-09-16 make note did:
   - `eval $(opam env --switch=poohstack --set-switch); dune build @all`
   - `AGENT_BIN=... node scripts/probe_shell_sem.mjs` — it boots its own
     scratch instance (own cwd, mock LLM, random port, seeded workspace).
     If it runs to completion, the budget blocker is dead; record
     `PP_PROFILE_RENDER=1` numbers for the boot render if easy.
   - Also read live issue `i-1789591788-8d507ea1-4455-4421-97bd-ae8e9a3705b2`
     (notes/issues or `/issues`) and resolve it if green.
2. **If the blocker is real**, this becomes a perf card: the boot render
   path is `glochid/tree-boxes` + tree-box stubs (the lazy flip from
   `docs/reviews/glochid-lazy-analysis-2026-09-20.md` is the prior art —
   BORGE-1789950734). Fix = keep the boot render under budget; do not
   weaken the 400k route budget, and do not smuggle JS (zero-JS
   discipline holds through v5).
3. **Prove the v5 smokes** from the stanza (all seven): strip columns side
   by side with peeks; open-or-focus on a strip-visible pane flips
   `.focused` without geometry moves; width presets are column widths +
   one-up round-trips reload; close-heal to adjacent column / empty strip
   valid; shelve + acme-Get unshelve INTO the strip; desktop flips hide
   foreign columns; Alt+arrows walk the strip; rail panes not columns —
   `probe_shell_sem.mjs` ALL GREEN including the rewritten I1 strip block,
   plus v3/v4 assertions. Geometry probe
   (`scripts/probe_shell_wm.mjs`) after; live hosts last.
4. **Habitat verify on prod (or dev):** GET `/filmstrip` renders strip
   columns (this also exercises the `7ca1f53` default-view landing).
5. **Flip the stanza** `main-strip-v5` planned → implemented in
   borge/filmstrip.borg, then `borge balance pricklypear.borg`,
   `borge parse pricklypear.borg --quiet`, `borge drift` (record result in
   the commit).

## Guardrails

- Zero-JS / zero-inline-styles discipline is a spec clause, not a style
  preference. If exact centering can't scroll natively, the stanza already
  says: it becomes a borge issue, NOT a JS permit.
- The 400k route budget is load-bearing (perf batch lore); optimize the
  render path, never raise the budget to pass the gate.
- `filmstrip/pane-single` MUST read through (accessor checks
  `as-str v = "true"` vs key absence) — the make note documents unstaging
  breaking otherwise.
- Region moves (rail⇄main) answer `filmstrip/oob-content-strip` —
  in-place swaps cannot cross containers (glochid splits inventory into
  #fs-content + #fs-rail under #fs-surface).
- Per the workstreams pipeline, this card must pass UI-REVIEW before DONE
  (it touches routes/rendering/interaction).

## Repo context

- Working dir: `/home/roerick/dev/wyo.tech/pricklypear`
- Build: `eval $(opam env --switch=poohstack --set-switch); dune build`
- Tests: `dune runtest lisp; dune runtest common; dune runtest agent`
  (1 pre-existing rail-trees failure at HEAD as of 2026-10-01 —
  "tree box: desk dirs carry no lazy pull" — verify whether the v5 work
  fixes it or file it as its own card; do not silently absorb)
- Spec: borge book is source of truth; spec questions belong in the
  stanza or a borge issue, never editorialized in code comments.
- Live habitat: `set -a; . ~/.config/pricklypear/env; set +a` then
  `scripts/pp-eval.sh -q '(expr)'` (PP_URL + Basic auth).

## Prior art to read first

- borge/filmstrip.borg stanza `main-strip-v5` (spec + make note + smokes)
- docs/reviews/glochid-lazy-analysis-2026-09-20.md (render-cost analysis)
- archive/reviews/glochid-shell-review-2026-09-14.md (the UI review that
  flagged the lift-model limit v5 fixes)
- scripts/probe_shell_sem.mjs header (invariant list)
