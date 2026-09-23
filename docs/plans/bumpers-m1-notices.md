# Bumpers M1–M6 implementation loop — HANDOFF PLAN

**Goal:** implement the bumpers chapter (`borge/bumpers.borg`, status
planned, committed 2026-09-23 as 28069dc) milestone by milestone.
This file is the shared memory between loops: pick the next unchecked
task, do only it, verify, update this file, commit, end.

**Spec authority (read first):**
- `borge/bumpers.borg` — the whole design (notices envelope, notice
  budget, verify-route, fragment-by-default, discovery surfaces,
  sequencing M1–M6, chapter-boundaries).
- `borge/habitat-workflow.borg` §§ gen-drift, temporal-log — the
  staleness-check semantics define-notices consume (M2+).
- `borge/prim-seam.borg` — owns the wire codec; the notices FIELD is
  additive-only there (cite, never re-spec).
- Discussion record: this plan's commit + `docs/agent-ui-harness.md`
  for the htmx tier ladder route-notices point to.

**Invariants (violating one = stop and ask, do not improvise):**
1. `define`'s return value NEVER changes. Notices are out-of-band only.
   Any consumer parsing `"value"` must be untouched; a client without
   notices support must work unchanged.
2. The notices field is ADDITIVE on the existing eval envelope
   (`{"ok","value"[, "error"]}` + `"notices":[...]`). Never rename or
   re-shape existing fields.
3. Notice budget is law (bumpers.notice-budget): ≤2 notices/response,
   each tip-kind once per session, fire only when actionable. When in
   doubt, emit fewer.
4. route_gate stays legality-only. Fragment classification is
   advisory text, never a publish block.
5. Do NOT touch: `storage.borg`, auth surfaces, migrations unless a
   milestone explicitly says so (none do — all milestones are code
   only, no schema changes).
6. Nothing that touches the notes-tree eval path may run synchronously
   before `/health` serves (2026-09-14 promote incident; AGENTS.md).
7. Telemetry is not content; notices are generated, never stored as
   tree datums.

**Code seams (verified 2026-09-23):**
- Shared codec: `pure/wire_codec.ml` (error<->JSON for /api/eval),
  `pure/prims_json.ml` (value codecs), composition root
  `pure/prims_pure.ml`. ALL rims link these — lens included.
- Envelope assembly: `image/lib/http_pages.ml` `api_eval` (~L1561):
  builds `("ok","value")` / error JSON. Notices attach HERE.
- Session plumbing: `image/lib/session.ml` (`Session.eval_one`).
  Notice buffer belongs per-session/per-eval — drained into the
  envelope, not threaded through Nopales values.
- Effect inference: `lisp/lib/effect.ml` (`infer`,
  `known_fn_of_bindings`, `EffSet.short` badge string) — pure, usable
  for the lisp/lib notice generation.
- Define parsing: `lisp/lib/eval.ml` `parse_top_define` (gives name,
  doc presence).
- CLI: `cli/bin/pp_eval.ml` (cmdliner; add `-q/--quiet` flag that
  sends `"quiet": true` in the POST body and suppresses notice
  printing).
- Route publish gate: `lisp/lib/route_gate.ml` + image route prim.
- Lens local eval: `remote/lib/` (`proxy_rim.ml`, `remote.ml`).
- Tests: `dune runtest` (lisp/common/pure need no services; image
  DB-backed suites skip without PG; `scripts/dev.sh start` for the
  full stack on ports 5433/18080).

**Working rules:**
- On the poohstack switch: `eval $(opam env --switch=poohstack --set-switch)`.
- After ANY change touching a .borg-declared module: `borge drift`.
- `borge check` must stay at 42 passed / 1 failed (pre-existing
  `storage.borg:521` failure — NOT yours to fix).
- There are unrelated in-flight changes in the working tree
  (`borge/workout.borg`, `libs/workout/lib.pp`, `libs/filmstrip/lib.pp`,
  `docs/plans/workout-gating.md`). NEVER stage them. `git add` only
  files you changed.
- One git commit per task, message cites the bumpers section.
- Per AGENTS.md: a spec change discovered mid-make becomes
  `borge issue new "..."` — do NOT edit bumpers.borg from this loop.
  Status flips in a section after its milestone ships are fine.
- Update the checkbox below as each task lands. When ALL of a
  milestone is green and committed, flip its (status planned) in
  borge/bumpers.borg to (status partial)/implemented as appropriate,
  in the same commit as the last task of the milestone.

## M1 — notices envelope + pp_eval -q

- [ ] Add a per-eval notice accumulator: an OCaml ref/buffer scoped to
  one `Session.eval_one` call (list of dict-shaped notices:
  `{kind,name?,effects?,text}`). No notices are generated yet — this
  milestone is PLUMBING ONLY.
- [ ] `api_eval` in `image/lib/http_pages.ml`: include `"notices": []`
  or the drained list in the success envelope; REQUEST flag
  `"quiet": true` suppresses the field entirely.
- [ ] Shared shape lives in `pure/wire_codec.ml` (or adjacent pure
  module) so remote proxy rims serialize identically. Add codec tests
  in `pure/test/` or `lisp/test/codec_test.ml` as fits.
- [ ] `cli/bin/pp_eval.ml`: `-q`/`--quiet` flag → sends the quiet flag;
  when notices are present and not quiet, print them after the value
  (stderr, prefixed `;; notice: `).
- [ ] Verify: `dune build @all`; `dune runtest`; manual:
  `pp_eval -j '(+ 1 2)'` unchanged output; a test that an old-style
  request (no quiet flag) gets a well-formed response.
- [ ] `borge drift` clean of new code drift.

## M2 — define-notices (effects + doc + bench/staleness)

- [ ] Pure notice generation in `lisp/lib` (new small module, e.g.
  `lisp/lib/notices.ml`): given the parsed top-level define
  (name, doc?, body) + effect inference, emit
  `{kind:define,name,effects}` + optionally `{kind:tip}` for missing
  docstring with text "no docstring — (define name ... \"doc\") makes
  (function-doc name) work" and bench pointer 'bench with
  (bench "(name ...)" 100); watch /timing after deploy'.
- [ ] Session taper: per-session record of which tip-kinds fired;
  once per kind per session; ≤2 total notices per eval response.
- [ ] Wire: image define path drains notices into the M1 buffer;
  lens local-eval path (remote/lib) uses the SAME lisp/lib generator.
- [ ] Staleness flag (consumes habitat-workflow.gen-drift; may be
  image-side only): when a spec/<pkg> doc owns the defined name and
  the binding's gen exceeds the section's gen, add
  `{kind:tip,text:"<pkg> spec section last spoke N gens ago"}`.
  If tree_versions gen plumbing is not yet exposed to prims, defer
  THIS BULLET ONLY with a `borge issue new` and continue.
- [ ] Tests: alcotest on the pure generator (effects badge text,
  once-per-session, budget ≤2); the "quiet define" (well-formed,
  doc'd, pure) emits no notice beyond the define kind.

## M3 — route-notices at route_gate

- [ ] On route publish: notice with path, handler, effect badge,
  htmx-harness pointer (docs/agent-ui-harness.md tier ladder as a
  short text: "verify tiered: wget content → scripted flow → CDP
  snapshot+vision; graduate green flows into scripts/smoke_<pkg>_ui.sh").
- [ ] Fragment classification: STATIC check over handler AST — if the
  handler body calls a full-shell constructor (layout/page, html/html,
  or a head tag), classify full-shell; remark "renders a full shell;
  apps fit glochid/habitat panes as fragments built from libs/ui +
  libs/layout; standalone page? declare it". Pure; in lisp/lib so
  lens agrees.
- [ ] Declared-standalone silences the fragment family: accept a
  route annotation or per-package datum (pick the minimal mechanism;
  cite your choice in the commit + update bumpers.borg fragment-by-
  default details via a borge issue if the mechanism diverges from
  what the section says).
- [ ] route_gate semantics UNCHANGED: no new denial cases. Test: a
  full-shell handler publishes exactly as before, only with a notice.

## M4 — verify-route dry-run

- [ ] `(verify-route "/path")` prim (image/lib): resolve route on the
  session's chain, invoke handler once under the standard budget,
  return a dict: `{cost: {ms, prim-calls, eval-cost}, effects:
  "<badge>", shape: fragment|shell|not-html, warnings: [...]}`.
- [ ] Budget overflow must surface as `{kind: "budget-exceeded"}` in
  the report, NOT an exception/500.
- [ ] First named warning pattern (from prod 2026-12-30 layout
  incident): handler rebuilds a large constant per invocation → warn
  "consider (delay ...)". Static AST heuristic is fine; false
  negatives acceptable, false positives not.
- [ ] Effect register: classify the prim in lisp/lib/effect.ml
  (read-db is the honest floor).
- [ ] Smoke per bumpers.verify-route: notes face returns the dict;
  over-budget handler reports, not hangs.

## M5 — discovery surfaces

- [ ] Effect badges in describe / apropos-docs text output
  (`image/lib/agent_tools.ml`): `name (args) [read-db]`.
- [ ] `(docs-missing [pkg])` prim: visible bindings without
  docstrings (function-doc () means missing), filtered to package
  prefix when given.
- [ ] describe package footer: "documented X/Y — (docs-missing
  \"pkg\")" when Y > X.
- [ ] `(app-check "pkg")`: per-route fragment/shell/not-html
  classification report (reuse M3 classifier). Advisory only.
- [ ] This implements habitat-workflow.spec-health's "gap follows the
  agent around" line — note the cross-ref in the commit.

## M6 — error hints + durable quiet

- [ ] `pure/wire_codec.ml` error codec: optional `"hint"` field for
  known error classes — Unbound_var → '(apropos "<stem>")';
  prim bad-args → '(function-doc "<prim>")'. Unknown classes: none.
- [ ] `(agent-profile 'quiet)` prim: store per-identity datum on
  <user>/personal; session consults it as the third quiet tier
  (request flag > session taper > durable datum). Narrowest wins.
- [ ] Tests for both; bumpers.borg M6 status flip + `borge drift`.

## Done-when (loop completion condition)

All M1–M6 checked, `dune build @all` + `dune runtest` green,
`borge drift` shows no new code drift, `borge check` 42/43, every
bumpers section status flipped appropriately, one commit per task on
this working tree. End with the loop's completion marker.
