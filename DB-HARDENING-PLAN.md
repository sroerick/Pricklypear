# DB-Hardening Plan — handoff for executing agent

**Branch/worktree:** `db-hardening`, worktree at `../pricklypear-db-hardening`
(relative to the main checkout). Base: master @ c351578.
**Goal:** firm up the branching/storage reliability gaps identified in the
2026-08-xx storage assessment (promote atomicity, merge semantics, hash
domain, provable core, stale doc). Scope is kernel OCaml + spec edits +
tests. No schema shape changes expected; if one becomes necessary, follow
the additive-only migration rule.

## Ground rules (from AGENTS.md — read it first)

- `eval $(opam env --switch=poohstack --set-switch)` before any dune command.
- Build `dune build @all`, test `dune runtest` from the **worktree root**,
  not the main checkout. Use `dune build --root <worktree>` if the shared
  `_build` gets confusing; the worktree has its own `_build`.
- Tiered verify for FE, but this plan is backend: alcotest + borge
  pipelines are the gates.
- Spec workflow: there are two kinds of spec touch here.
  (a) T1/T4 *document existing behavior more honestly* → edit the relevant
    `.borg` chapter in a plan-mode spirit, note it in the commit message.
  (b) T3 (hash-domain unification) changes storage semantics that
    `borge/one-tree.borg` declares → discuss of record: update the chapter
    as part of the same commit that lands the code change, per the
    normal plan-then-make flow. Do NOT `borge issue new` for work that is
    in-scope here; issues are for out-of-scope discoveries only.
- After every code change touching a `.borg`-declared module: `borge drift`
  must be clean (or resolved in the same commit).
- Tests that need Postgres: use `scripts/dev.sh start` in the WORKTREE
  (ports 5433/18080/17878 are shared with the main checkout — do NOT run
  both dev stacks at once; coordinate/stop the other one first). Kernel
  work validates against the local dev PG, not prod.
- Commit per task or per logical unit, conventional messages. Keep master
  mergeable at every commit.

## Task 1 — Atomic promote (reliability, small)

`Store.promote_functions` (`image/lib/store.ml` ~L659) runs N function
upserts + route upserts + `Tree_store.recompute_head_hash` as bare `exec`
calls outside any transaction. A crash mid-promote leaves a partial
promote; `merge_branch` already models the right pattern
(`with_transaction`, default `Serializable`, see `store_db.ml:143`).

Do:
1. Wrap the whole body in `Store_db.with_transaction` (or the `store.ml`
   local helper — match `merge_branch`'s usage). Careful: keep the "no DB →
   return 0" graceful-degradation path the REPL tests rely on.
2. Failure semantics: any exception → rollback + return 0 with stderr log
   (current behavior) — now actually atomic.
3. Tests (`image/test/merge_promote_test.ml`):
   - promote of K names is all-or-nothing: inject a failure on the last
     name (e.g. promote a name list where one name provokes the error
     path) and assert zero rows landed on target.
   - happy path unchanged: existing promote tests still green, including
     route-travels-with-handler and head-hash recompute.

Acceptance: `dune runtest image/test` green; new atomicity test present;
`borge drift` notes no change (this is an implementation-honesty fix, no
spec semantics change).

## Task 2 — Merge semantics on diverged: decide, document, test

Today `merge_branch` (store.ml ~L1515, parent-target path) upserts
`ahead-new | ahead | diverged` names alike — diverged names are copied
source-wins while ALSO being reported as "conflicts". Non-parent merges
are 2-way (`NOT DISTINCT FROM` diff, no base, silent source-wins).

Decision required (make it with the repo owner, or default to the
conservative option and record the decision in the spec):
- **Recommended:** diverged names are NOT upserted; merge applies only
  ahead/ahead-new and returns the diverged set as conflicts requiring
  human resolution. This matches the workflow in
  `borge/habitat-workflow.borg` (obituary rule: no silent reversals).
- Fallback if owner prefers status quo: keep source-wins but declare it
  explicitly in `borge/branching.borg` ("merge on diverged = source-wins
  by design, conflict list is advisory").

Do:
1. Implement the decision in `merge_branch`.
2. Update `borge/branching.borg` branch-status/merge S3 prose to state the
   final rule in one plain sentence.
3. Tests: extend `image/test/merge_promote_test.ml` /
   `branch_status_test.ml` — diverged + merge = no overwrite on target,
   name present in conflict report (recommended option), and a post-merge
   `branch_status` still flags the name diverged.

## Task 3 — Unify the hash domain (unblocks content-addressed promote)

Known debt, recorded in `borge/branching.borg` 2026-08-28 agent note:
stored `tree_versions.hash` / `tree_nodes.hash` are NOT comparable across
write paths — tree-first `Tree_store.hash_body` (compact JSON) digest vs
the 009-mirror `digest(jsonb::text)` (spaced-canonical) digest produce
different hashes for identical bodies. Workaround today: re-hash at query
time. This is why `head_hash` is a "consistency artifact, not content".

Do:
1. Pick ONE canonicalization. Recommendation: PG's `body->'body'::text`
   (jsonb spaced-canonical) as the single domain — it's what the query-time
   workaround already uses, so the status/merge code changes the least.
   `Tree_store.hash_body` must switch to emitting exactly that
   canonicalization (jsonb via PG, not OCaml-side compact JSON), or the
   hash must be computed DB-side at write time (`payload->'body'::text`
   digest in the upsert) — DB-side is more robust to writer diversity.
2. One-time backfill migration (new additive migration `027_*`): recompute
   `tree_nodes.hash` and `tree_versions.hash` in-domain for all rows.
   Downtime-free; update-in-place on a shadow column is fine, then flip.
3. Remove the query-time re-hash workaround once stored hashes are in one
   domain; status code compares stored columns directly again.
4. Update `borge/one-tree.borg` (and the branching chapter note) to declare
   the single hash domain rule: "one canonicalization, DB-side, all
   writers".
5. Tests: write the same body through BOTH paths (tree-first upsert and
   scaffolding-table upsert) and assert equal stored hashes. Extend
   `tree_store_test.ml` / `tree_version_shadow_test.ml`. Live smoke:
   `ksh scripts/verify_one_tree.sh` green.

Acceptance: single-domain hash test green; `tree_verify()` in sync; drift
clean.

## Task 4 — Extract the branch algebra as pure OCaml + QCheck (provable core)

`borge/formal-verification.borg` (status planned) wants property-based
verification islands; the branch algebra is the cheapest real island. The
semantics are extractable without I/O because they're set/map logic:

- override resolution: closest-own-wins along a parent chain
- pin hygiene: pin ⟺ body ≠ nearest inherited visible body
- branch status classify: ahead-new / ahead / behind / diverged / synced
  from (base set, own set, nearest-inherited set)

Do:
1. New pure module (e.g. `image/lib/branch_algebra.ml`, no `store`
   dependency, operates on abstract `branch_tree * (name -> body) maps`).
   Reimplement the crazy-small pure rules; this is the SPEC of the
   semantics, executable.
2. Refactor the SQL-driven `classify_status` / pin-hygiene paths to consult
   the pure module where the data is already fetched (or keep SQL and
   cross-check — but the goal is the pure module is the semantic
   reference).
3. QCheck tests (`image/test/branch_algebra_test.ml`): random branch
   forests + random name/body assignments; properties:
   - override: resolve(b, n) equals body's of nearest ancestor-with-pin,
     else main's, else absent. Model-checked against a naive reference
     implementation.
   - pin hygiene round-trip: after `prune-synced`, no synced pins remain;
     after define-identical, no pin exists.
   - classify determinism + totality: every (base, own, inherited) shape
     maps to exactly one status; ahead-new/ahead/behind/diverged/synced
     partition the space.
   - merge partial application (post-T2 semantics): applied names ⊆ own
     pins; diverged ∩ applied = ∅ (recommended T2 option).
4. Note in `borge/formal-verification.borg` that the first island shipped
   (flip that chapter's status toward partial with an agent note).

Scope guard: do NOT attempt to machine-check SQL triggers or the GUC-guard
mirror — explicitly out of the provable fragment. Say so in the spec note.

## Task 5 — Doc consistency pass (cheap, do anytime)

- `docs/one-tree.md` says tree reads/writes are ON by default; `AGENTS.md`
  One-tree section says default OFF (`PP_TREE_READS`/`PP_TREE_WRITES` "set
  =1 to exercise"). Determine current kernel default from `store.ml`
  `fn_rel/ty_rel/rt_rel` flag code and fix the stale document (and the
  project AGENTS.md notes if the pillar flip shipped).
- Final gate: `borge report` and `borge drift` both run; status flips in
  the report match what this branch landed.

## Definition of done (whole plan)

- [ ] T1: promote is one transaction; atomicity regression test green.
- [ ] T2: merge-on-diverged semantics decided + implemented + spec says the
  rule in one sentence; test covers conflict-not-overwritten.
- [ ] T3: single hash domain; cross-writer hash equality test; migration
  applied on dev stack; verify_one_tree green.
- [ ] T4: pure branch-algebra module + QCheck properties green;
  formal-verification chapter status updated.
- [ ] T5: docs consistent; `borge drift` clean.
- [ ] All existing alcotest suites green on this branch.
- [ ] Commits are individually sane; branch ready for merge to master.
