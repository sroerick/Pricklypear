# Bumpers M1–M6 implementation loop — HANDOFF PLAN

**Goal:** implement the bumpers chapter (`borge/bumpers.borg`, status
planned, committed 2026-09-23 as 28069dc) milestone by milestone.
This file is the shared memory between loops: pick the next unchecked
task, do only it, verify, update this file, commit, end.

**Loop status:** M5 DONE (see M5 section · discovery surfaces ship:
`(docs-missing [pkg])` + `(app-check "pkg")` prims, effect badges + arity
(`name (args) [read-db]`) across describe single-binding/package/apropos
fallback, describe doc-coverage footer, all smoke + build/drift/check
gates green; bumpers.borg discovery-surfaces flipped to partial
(badges/footer/docs-missing half), fragment-by-default flipped to
implemented (its last open item was app-check). Next unchecked
milestone: **M6 — error hints + durable quiet**.

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

## M1 — notices envelope + pp_eval -q  (LOOP #1: DONE, committed)

- [x] Add a per-eval notice accumulator: an OCaml ref/buffer scoped to
  one `Session.eval_one` call (list of dict-shaped notices:
  `{kind,name?,effects?,text}`). No notices are generated yet — this
  milestone is PLUMBING ONLY.
- [x] `api_eval` in `image/lib/http_pages.ml`: include `"notices": []`
  or the drained list in the success envelope; REQUEST flag
  `"quiet": true` suppresses the field entirely.
- [x] Shared shape lives in `pure/wire_codec.ml` (or adjacent pure
  module) so remote proxy rims serialize identically. Add codec tests
  in `pure/test/` or `lisp/test/codec_test.ml` as fits.
- [x] `cli/bin/pp_eval.ml`: `-q`/`--quiet` flag → sends the quiet flag;
  when notices are present and not quiet, print them after the value
  (stderr, prefixed `;; notice: `).
- [x] Verify: `dune build @all`; `dune runtest`; manual:
  `pp_eval -j '(+ 1 2)'` unchanged output; a test that an old-style
  request (no quiet flag) gets a well-formed response.
- [x] `borge drift` clean of new code drift.

**M1 progress note (loop #1, committed 55a57cc):** shipped. Design choices worth
recording for later loops:
- The shared notice shape + codec went IN `pure/wire_codec.ml`
  (`Wire_codec.notice` / `notice_to_json` / `notices_to_json` /
  `notice_of_json` / `notice_to_line`) — the plan's primary suggestion,
  and deliberately NOT a new `pure/notices.ml` file: every pure module
  is declared in `lisp-interpreter.borg pure-prim-modules`, and a new
  file would have added a drift item that `borge` cannot auto-resolve
  without a spec edit. `wire_codec.ml` is already declared there, so
  zero new drift. (The wire-field home is the same file prim-seam's
  shared-codec / chapter-boundaries already points at.) M2's *generator*
  still goes to `lisp/lib/notices.ml` (its own declaration decision
  then; lisp/lib cannot see pure/, so generation will emit the shape
  directly or via a lisp-side type — next loop's call, potentially a
  `borge issue new` if a declaration is needed).
- Session buffer: `pending_notices : Wire_codec.notice list` field on
  `session_types.t`; RESET at the top of every `Session.eval_one` (so a
  non-api_eval caller — REPL, jobs — can't leak a prior eval's notices
  into the next envelope), drained by `api_eval` via
  `Session.take_notices` (read-and-clear). `pp_eval` links
  `pricklypear-pure-prims` now (for the shared rendering).
- Quiet is a request flag parsed in `api_eval` (`Body_fields
  "quiet": true`); quiet responses omit the notices field entirely, so
  they are wire-identical to an old-style client.
- Manual verify (local dev image, fresh throwaway PG on 5435/18081):
  old-style `(+ 1 2)` → `{"ok":true,"value":3,"notices":[]}`; quiet →
  `{"ok":true,"value":3}` (no notices key); unbound error shape
  unchanged; bare-JSON body path also carries `notices:[]`. `pp_eval -j
  '(+ 1 2)'` output unchanged. End-to-end notice printing proven with a
  fake peer: `-j` keeps stdout JSON clean, notices on stderr as `;;
  notice: define add [pure]: defined add`; `-q` suppresses entirely.
- New tests: 7 notices cases added to `pure/test/wire_codec_test.ml`
  (full/minimal JSON shape, additive no-null rule, empty + 2-list
  assembly, of_json round-trip, malformed rejection, to_line) — all
  green; wire-codec suite now 17 tests.
- **PRE-EXISTING, NOT OURS (verified):** `image/test/seed_sources_test`
  fails 5/9 on master independent of this work — a strict linter over
  `libs/*.pp` (kernel-form defines / parse errors / bare top-level
  forms / define-time sibling calls). Proven by running the built test
  against `git archive HEAD libs` in a clean dir: identical 5 failures
  with zero bumpers code involved. Leave them alone; not a bumpers
  concern. `borge check` stays 42/43 (storage.borg:521 pre-existing);
  `borge drift` stays at the 23-item pre-existing baseline (zero new).
- Not in M1 scope (still open, per spec verify): browser /repl muted
  notice rendering (only meaningful once M2 generates notices); the
  lens local-eval `notices` wire field is M2's seam (uses the same
  generator).

## M2 — define-notices (effects + doc + bench/staleness)

> This block DUPLICATES the M1 plumbing tasks verbatim (leftover from
> the handoff draft). ALL SIX shipped in M1 (55a57cc/e627a3c): per-eval
> buffer, api_eval drain + quiet flag, shared codec in
> pure/wire_codec.ml, pp_eval -q + notice printing, verify, drift
> baseline. Kept checked for traceability.

- [x] Add a per-eval notice accumulator: an OCaml ref/buffer scoped to
  one `Session.eval_one` call (list of dict-shaped notices:
  `{kind,name?,effects?,text}`). No notices are generated yet — this
  milestone is PLUMBING ONLY.
- [x] `api_eval` in `image/lib/http_pages.ml`: include `"notices": []`
  or the drained list in the success envelope; REQUEST flag
  `"quiet": true` suppresses the field entirely.
- [x] Shared shape lives in `pure/wire_codec.ml` (or adjacent pure
  module) so remote proxy rims serialize identically. Add codec tests
  in `pure/test/` or `lisp/test/codec_test.ml` as fits.
- [x] `cli/bin/pp_eval.ml`: `-q`/`--quiet` flag → sends the quiet flag;
  when notices are present and not quiet, print them after the value
  (stderr, prefixed `;; notice: `).
- [x] Verify: `dune build @all`; `dune runtest`; manual:
  `pp_eval -j '(+ 1 2)'` unchanged output; a test that an old-style
  request (no quiet flag) gets a well-formed response.
- [x] `borge drift` clean of new code drift.

## M2 — define-notices (effects + doc + bench/staleness)

- [x] Pure notice generation: the generator went into `lisp/lib/effect.ml`
  (`Effect.define_notices` / `define_notice` / `taper_tips` /
  `notice_budget` = 2 / `cap_notices`) rather than a new
  `lisp/lib/notices.ml` — every lisp/lib module is spec-declared and a
  new file would add a drift item borge cannot auto-resolve (the M1
  progress note left this exact call open; see note below). Given the
  parsed top-level define (name, doc?, body) + effect inference, emit
  `{kind:define,name,effects}` (+ bench pointer text "bench:
  (bench "(name ...)" 100); watch /timing after deploy" when
  non-pure) + optionally `{kind:tip}` for missing docstring with text
  "no docstring — (define name ... \"doc\") makes (function-doc name)
  work".
- [x] Session taper: per-session record of which tip-kinds fired
  (`fired_tip_kinds` on session_types.t; lens `local_fired_tips`);
  each tip kind fires at most once per identity-session
  (`Effect.taper_tips`, once-per-session tested); ≤2 total notices per
  eval response enforced at the drain (`Session.take_notices` caps at
  `Effect.notice_budget`; begin-wrapped batches held to 2).
- [x] Wire: image define path drains notices into the M1 buffer
  (`Session.notice_define`, called from BOTH `persist_define` bodies in
  `eval_one` and `eval_form`; gated OFF during bulk load/seed via the
  existing `suppress_refresh_on_define` flag so library loads and the
  boot seed do not nag). Lens local-eval path (`remote/lib/remote.ml`
  `notice_local` + `take_local_notices`) uses the SAME
  `Effect.define_notices`/`taper_tips` generator; pp-remote `-e` and
  the /eval surface render them (`;; notice: ` on stderr / muted under
  the result).
- [ ] Staleness flag — **DEFERRED, not done**: consumed
  habitat-workflow.gen-drift, whose spec-section→binding OWNERSHIP
  edge is still status-planned (no /spec row, no spec/drift prim, no
  lookup to consume at define time). Gen COORDINATES are prim-exposed
  (gen-read/gen-diff) but the ownership edge is not; implementing it
  now is spec work, not a bumpers make. Filed `borge issue new` →
  **BORGE-1790178960** (open, .borge-bugs/, gitignored working doc).
  Wire the tip in `Session.notice_define` when gen-drift lands.
- [x] Tests: alcotest on the pure generator in `lisp/test/effect_test.ml`
  (34 tests; +4 cases: effects badge text, quiet define = 1 notice,
  missing-docstring tip within ≤2 budget, once-per-session taper)
  and `remote/test/remote_test.ml` (new `local define notices` case:
  lens local define emits the same generator, taper once per lens
  process, zero prim calls — out of band).

**M2 progress note (loop #2, committed d961b0e):** define-notices shipped
end to end. Design choices worth recording for later loops:
- **Generator home = effect.ml, NOT a new lisp/lib/notices.ml.** The
  plan suggested `lisp/lib/notices.ml` but every lisp/lib module is
  spec-declared; an undeclared file shows as `unspecified-module` in
  `borge drift`, and done-when requires ZERO new drift. effect.ml
  already hosts advisory AST lint (`txn_lint`) on the same `infer`, so
  the generator sits beside its input. If a future loop wants the file
  split, that is a spec/declaration change → `borge issue new` first.
- **Bulk gate**: `Session.notice_define` is skipped while
  `suppress_refresh_on_define` is set (the whole load_string /
  load_library / boot-seed window), so interactive defines alone get
  notices. Budget+taper then handle the interactive case.
- **Budget enforced at the DRAIN** (`Session.take_notices` caps at
  `Effect.notice_budget` = 2, first N win) so a begin-wrapped batch
  cannot burst the envelope; the pure generator can never exceed 2 per
  define by construction.
- **Taper is per identity-session** (session_types.fired_tip_kinds;
  lens keeps its own local_fired_tips). Because sessions are
  per-identity (Session_registry), the docstring tip effectively fires
  once per identity (proven live: admin's tip fired on the first bare
  define then stayed silent; a fresh identity m2tester got it once and
  tapered).
- **Known_fn default = prim-only** for effect inference at define time
  (user-callee resolution would need a per-define env/branch scan; the
  badge is primarily about prim effects — read-db vs write-db — which
  is the strong-effects signal the notice is for). Follow-up if
  stronger callee badges are wanted.
- **Live verify (local dev image 5435/18081)**: bare define →
  `[{kind:define,effects:pure},{kind:tip,...doc...}]`; quiet define
  (doc'd+pure) → exactly `[{kind:define,...}]`; non-pure →
  `{kind:define,effects:db-write,text:bench...}`; replay → tip
  tapered; begin-wrapped 2 bare defines → exactly 2 define notices
  (budget); `quiet:true` request → NO notices key; `pp_eval -j` prints
  `;; notice: define ...` on stderr with stdout JSON clean; `-q`
  suppresses. pre-existing image/test/seed_sources 5/9 failures still
  baseline (not ours).
- **Still open** (per plan): the browser /repl muted notice rendering
  (only meaningful once notices fire — they do now; a follow-up
  milestone/loop can render them), and the staleness flag
  (BORGE-1790178960).

## M3 — route-notices at route_gate

- [x] On route publish: notice with path, handler, effect badge,
  htmx-harness pointer (docs/agent-ui-harness.md tier ladder as a
  short text: "verify tiered: wget content → scripted flow → CDP
  snapshot+vision; graduate green flows into scripts/smoke_<pkg>_ui.sh").
- [x] Fragment classification: STATIC check over handler AST — if the
  handler body calls a full-shell constructor (layout/page, html/html,
  or a head tag), classify full-shell; remark "renders a full shell;
  apps fit glochid/habitat panes as fragments built from libs/ui +
  libs/layout; standalone page? declare it". Pure; in lisp/lib so
  lens agrees.
- [x] Declared-standalone silences the fragment family: accept a
  route annotation or per-package datum (pick the minimal mechanism;
  cite your choice in the commit + update bumpers.borg fragment-by-
  default details via a borge issue if the mechanism diverges from
  what the section says).
- [x] route_gate semantics UNCHANGED: no new denial cases. Test: a
  full-shell handler publishes exactly as before, only with a notice.

**M3 progress note (loops #3+#4, committed …):** route-notices shipped end to end.
The previous loop left the whole implementation in the working tree
uncommitted and unverified; this loop verified it build/test/live and
landed it. Design choices worth recording:
- **Generator + classifier home = lisp/lib/effect.ml** (same reasoning
  as M2's define-notices: every lisp/lib module is spec-declared, so
  `Effect.classify_route_shape` / `route_shape_string` /
  `route_notice` / `route_facts` / `expr_uses_head` sit beside
  define_notices on the same `infer` — zero new drift).
  `Route_gate.handler_body` (image/lisp seam) hoists the gate's own
  raw lookup so the classifier reads the handler body EXACTLY as the
  gate does (no VThunk force → no request-context requirement, pure).
- **Mechanism for bullet 3 = route annotation, not package datum**: a
  trailing `'standalone` arg on `(route path handler ['standalone])` /
  `(route path handler vis 'standalone)` / `(public-route path
  handler ['standalone])`, accepted as string or quoted symbol. The
  spec's fragment-by-default says "route annotation or package datum,
  e.g. standalone: true" — the route annotation branch matches the
  letter of the spec, so NO borge issue for divergence; the choice is
  cited here + in the commit. **Session-scoped** (in-memory
  `session_types.standalone_routes`, monotone: once declared, later
  re-publishes of the path stay silent even without the annotation)
  because route-notices fire only at publish time. The cross-session
  durability gap (a fresh session re-publishing the path without the
  annotation draws the full-shell remark again) needs a route-meta
  column = schema change, which M3 forbids (invariant 5) → filed
  **BORGE-1790180098** (open, .borge-bugs/).
- **route-notice text varies by shape**: Shell → full-shell remark +
  htmx-harness pointer; Fragment → harness pointer; NotHtml →
  "published"; unbound handler (define-after-route still allowed on
  non-public routes, unchanged from before) → "published (handler not
  yet defined…)". kind "route" is NOT a tip, so the session taper
  doesn't apply; the ≤2 budget is still enforced at the api_eval drain
  (begin-wrapped publish batches held to 2).
- **Gated off during seed/load** via the same
  `suppress_refresh_on_define` flag as define-notices — library loads
  and boot seed never nag.
- **route_gate untouched**: `register_route_checked` keeps its
existing gate; M3 only added `declare_standalone` + `notice_route`
  calls on the SUCCESS path. Verified live: unbound public route still
  denied ("…is not a defined function", no notice); bad visibility
  arg still rejected; 32 existing 2-arg `(route path handler)` calls
  across libs/ are unaffected.
- **Existing arg shapes unchanged**: 2-arg, 3-arg visibility, and the
  `public-route` forms all still parse; 4-arg adds the standalone
  tail. The 3-arg "visibility must be …" error message now mentions
  standalone (cosmetic).
- **Verify**: `dune build @all` + `dune runtest` green (only
  pre-existing image/test/seed_sources 5/9 baseline failures); borge
  drift at the 23-item baseline (zero new); borge check 42/43.
  Live dev image (throwaway PG 5435 / 18081): full-shell page publishes
  with remark+harness, fragment carries harness pointer, not-html says
  published, `'standalone` silences monotone (incl. re-publish without
  the annotation), `quiet:true` drops the notices field, budget caps a
  begin-wrapped route+2 defines at 2 notices, M2 define-notices still
  fire. borge/bumpers.borg fragment-by-default flipped (status partial)
  — the M5 app-check consumer is what keeps it short of implemented.
- **Still open** (per plan): browser /repl muted notice rendering;
  staleness flag BORGE-1790178960 (M2 bullet 4); cross-session
  standalone durability BORGE-1790180098 (needs a migration); and
  M4–M6.

## M4 — verify-route dry-run  (LOOP #5: DONE, committed …)

- [x] `(verify-route "/path")` prim (image/lib): resolve route on the
  session's chain, invoke handler once under the standard budget,
  return a dict: `{cost: {ms, prim-calls, eval-cost}, effects:
  "<badge>", shape: fragment|shell|not-html, warnings: [...]}`.
- [x] Budget overflow must surface as `{kind: "budget-exceeded"}` in
  the report, NOT an exception/500.
- [x] First named warning pattern (from prod 2026-12-30 layout
  incident): handler rebuilds a large constant per invocation → warn
  "consider (delay ...)". Static AST heuristic is fine; false
  negatives acceptable, false positives not.
- [x] Effect register: classify the prim in lisp/lib/effect.ml —
  classified **WriteDb** (see progress note: executor semantics, same
  as route/dispatch), not the plan's literal "read-db floor".
- [x] Smoke per bumpers.verify-route: live /api/eval verify on dev
  image 5435/18081 — /hello → fragment + effects html; /dabar → shell
  + a real find-rows render error surfaced as JSON; over-budget
  recursion → `kind:"budget-exceeded"` with eval-cost 400001, no hang.

**M4 progress note (loop #5, committed …):** design choices to record:
- **Prim home = image/lib/session.ml, inline next to route/lookup**
  (registered into the live prim table so it auto-binds), driven by a
  late-bound `verify_route_fn` ref bound right after call_handler —
  the exact route/dispatch pattern (the prim needs call_handler-era
  machinery, so a forward ref avoids a dependency cycle). Invocation
  reuses `eval_string_rim_budgeted` with a FRESH `Eval.make_budget`
  (default 400k standard budget) plus a default GET request bound via
  `current_request` so request-* prims inside the handler resolve.
  No /timing ring sample and no X-Timing-Ms header: verify is a probe,
  not a real request.
- **Budget-exceeded detection**: because call_handler swallows
  `Interp_error Budget_exceeded` into `Handler_error "budget
  exceeded"` (and emits an incident — wrong for a dry-run probe),
  verify_route does its OWN invocation catching `Budget_exceeded`
  explicitly before `pp_interp_exn`, and reports `{kind:
  "budget-exceeded"}` alongside the cost/effects/shape fields.
  Verified live: an infinite 100M-count recursion reports
  eval-cost 400001 / prim-calls 72726 / ms 280 in-port and returns.
- **Warning pattern home = lisp/lib/effect.ml** (`verify_route_warnings`
  / `has_large_const_on_path` / `is_large_literal` / `const_payload`)
  beside txn_lint + the other advisory lints — zero new drift. Only
  AST LITERALS count (a `Lit` string >=1024 chars, or a `(list ...)`
  whose args are ALL literals with >=24 elements or >=1024 chars
  payload — the actual layout stylesheet shape is a ~30-element CSS
  string list), and the walk recurses into args so
  `(html/join (list <30 str>))` fires. Request-dependent builders
  have no literal payload → can never be flagged (false negatives
  accepted, false positives not). Does NOT chase calls into sibling
  functions (layout/neocities-theme's own big list is a false-miss by
  design).
- **Classification = WriteDb, NOT the plan's literal "read-db
  floor"**: verify-route EXECUTES a named handler — the same
  capability as route/dispatch, which is conservatively WriteDb so
  public routes can never call it and fire another route's writes. A
  read-db classify would let a public handler probe/trigger writes
  through it; the honest floor is just "not pure". If a purely-static
  variant is wanted later it can be read-db. boot classification
  assertion passed (image starts), so it IS in exactly one bucket.
- **Tests**: 5 new cases in lisp/test/effect_test.ml (effects suite
  now 52): big css string → warn; 30-element literal list (layout
  shape) → warn; small constants → none; request-dependent build →
  none; 10-item list under thresholds → none.
- **Verify gates**: `dune build @all` + `dune runtest` green (only the
  pre-existing image/test/seed_sources 5/9 baseline failures); borge
  drift 23-item baseline (zero new — effect.ml + session.ml are
  spec-declared); borge check 42/43 (storage.borg:521 pre-existing).
- Also live-verified: unknown route → `{error:"no such route"}`
  JSON; unbound handler → "unbound…" error field; a handler whose own
  body errors (bad html/p arity, bad find-rows sort) surfaces the
  render error readably in `error` — endpoints reach prod broken
  becomes a REPL-visible JSON report (the section's value prop).
- **Still open** (per plan): browser /repl muted notice rendering;
  staleness flag BORGE-1790178960 (M2 bullet 4); cross-session
  standalone durability BORGE-1790180098 (needs a migration); and M5
  (discovery surfaces) + M6 (error hints + durable quiet).

## M5 — discovery surfaces

- [x] Effect badges in describe / apropos-docs text output
  (`image/lib/agent_tools.ml`): `name (args) [read-db]`.
- [x] `(docs-missing [pkg])` prim: visible bindings without
  docstrings (function-doc () means missing), filtered to package
  prefix when given.
- [x] describe package footer: "documented X/Y — (docs-missing
  \"pkg\")" when Y > X.
- [x] `(app-check "pkg")`: per-route fragment/shell/not-html
  classification report (reuse M3 classifier). Advisory only.
- [x] This implements habitat-workflow.spec-health's "gap follows the
  agent around" line — note the cross-ref in the commit.

**M5 progress note (loops #6+#7, committed …):** design choices to record:
- **Generator home = image/lib/agent_tools.ml, beside the other agent
  tools** (describe/apropos_docs/function-doc already live there) —
  pure formatters (`annotate_name`, `doc_coverage_footer`,
  `app_check_line`, `lambda_params`, `binding_has_doc`) declared so the
  no-DB agent_tools_test suite can pin their shape. Prims registered in
  `image/lib/prims_code.ml` (`docs-missing`, `app-check`);
  `session.ml` re-exports; `lisp/lib/effect.ml` classifies both
  prims ReadDb (discovery never writes).
- **The last loop (#6) left M5 uncommitted + broken**: helper was named
  `mm5_annotate` in the test but `annotate_name` in the code → the test
  binary would not compile. Fixed the test to `annotate_name`; also
  corrected a stray `*)` comment.
- **Badge + arity in describe**: single binding
  `name (args) [badge]` (args from the body's lambda params), package
  listing, AND the apropos-fallback listing all carry
  `binding_annot ~known_fn` (store functions are effect-inferred over
  their body with the precomputed branch known_fn so user-callee
  capabilities are credited; prims/special forms resolve from the
  classified catalog; unknown → "[?]" never a false "pure"). The
  standalone `(apropos-docs pat)` prim stays structured `(name, doc)`
  pairs (consumers exact-match names); badges are in the describe TEXT
  output, matching the "text output" reading of the bullet.
- **Arity decode gap found this loop**: seeded/load-library bodies
  persist as the reader form `App (Var "lambda", params :: _)` (flat
  encoding: first param = App head, rest = args array, `Lit VNil` for
  ()), while interactive define-sugar persists a `Lambda` constructor.
  `binding_annot` only matched `Lambda` → arity never rendered for
  seeded funcs. Added `lambda_params : expr -> string list option`
  handling BOTH shapes (+ 4 tests pinning the encodings; m5 suite now
  11 tests). Live: `(describe "todo/add")` →
  `todo/add (list-id title) [db-write]`; `todo/page` (thunk) →
  `todo/page [db-write]`.
- **Footer** `documented X/Y — (docs-missing "pkg")` only when Y > X
  (pinned as a pure formatter). Live: `(describe "todo")` ends with
  `documented 22/149 — (docs-missing "todo")`.
- **docs-missing**: filter is package prefix (same match as
  package-bindings). Live: `(count (docs-missing "todo"))` = 127;
  `(docs-missing)` = full cross-package list, no auth/DB cost surprises.
- **app-check**: per-route classification over `Route_gate.handler_body`
  + `Effect.classify_route_shape` (exactly the M3 classifier); `unbound`
  when the handler isn't defined yet; empty pkg = all routes; matches
  handler == pkg OR pkg/ prefix (= package-bindings semantics, same as
  docs-missing). Live: `(app-check "welcome")` → 4 routes with
  shell/fragment/not-html badges; `(app-check "todo")` → "no routes
  whose handler is in this package" (todo has no routes on the dev
  seed — /notes etc aren't registered there either; not a bug, the
  report is what it reports).
- **Spec flips**: discovery-surfaces planned→partial (badges/footer/
  docs-missing shipped; the error-codec hint field + (help) prim stay
  for M6), fragment-by-default partial→implemented (its last open item
  — the app-check report — now ships). Commit cites
  habitat-workflow.spec-health.
- **Verify**: `dune build @all` + `dune runtest` green (only the
  pre-existing image/test/seed_sources 5/9 baseline failures); borge
  drift 23-item baseline (zero new); borge check 42/43. Live dev
  image (throwaway PG 5433/18080) confirmed all four surfaces.
- **Still open** (per plan): browser /repl muted notice rendering;
  staleness flag BORGE-1790178960 (M2 bullet 4); cross-session
  standalone durability BORGE-1790180098 (needs a migration); and M6
  (error hints + durable quiet).

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
