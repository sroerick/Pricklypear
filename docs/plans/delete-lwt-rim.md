# Plan — Remove the Lwt rim (eio-floor Phase 5)

**Status:** plan for a follow-up agent. Not started.
**Read first:** `docs/eio-floor-checklist.md` § Phase 5 (the authoritative
checklist) and `borge/single-image.borg` § runtime (the declarative spec).

## Declarative end state

One runtime. The OCaml rim is **Eio**, period. The process runs under
`Eio_main`; the HTTP accept loop is `Httpun_eio`; the Postgres rim is
`Io_eio` (pure pgx over `Eio.Net`). There is no second concurrency monad,
no `PP_IO` flag, no `Io_lwt`, no `Lwt_eio` bridge. The Lisp evaluator is
already direct-style (`lisp/lib/eval.ml` performs I/O via
`Control.perform_prim`, OCaml 5 effects); the rim handler answers synchronously
on the Eio loop.

This is the final eio-floor step. It is **mechanical at the surface but has
three non-mechanical traps** (below) that must be designed, not improvised.

## What Lwt actually is today (verified 2026-07-26)

The runtime is **already Eio at the floor** (`image/bin/main.ml`:
`Eio_main.run` → `Io_eio.Db`). Lwt survives only as these residues, in
descending order of how load-bearing they are:

1. **The `Lwt_eio` bridge** in `main.ml` / `boot.ml`.
   `Lwt_eio.with_event_loop` wraps the Eio loop so legacy `Lwt` code can
   `Lwt_eio.run_lwt` into the Eio runtime. Everything below exists only
   because this bridge exists.
2. **TCP REPL endpoint** — `image/lib/repl_endpoint.ml` (35 Lwt refs).
   Uses Lwt socket primitives, driven by `Lwt_eio.run_lwt` from `boot.ml:58`.
   This is the ONLY reason `main.ml` keeps the `Lwt_eio` bridge.
3. **`Io_lwt`** — `image/lib/io_lwt.ml` (facade) + the `Lwt_conn` arm of
   `store_db.ml` + `PP_IO` flag in `io_eio.ml`/`boot.ml`. The DB call sites in
   `crud.ml` (41 refs), `auth_store.ml` (34), `tree_store.ml` (26),
   `ai_store.ml`, `media.ml`, `ops_incidents.ml`, `session.ml` call
   `Io_lwt.Db.{current_conn,execute,with_dynamic_transaction}`. **These are
   mechanical**: `Io_eio.Db` exposes the identical surface and is the live
   backend; swapping the module name and deleting `Io_lwt` should just work.
4. **Subprocess + sleep prims** — `image/lib/llm_client.ml` (5 refs) and
   `image/lib/prims_net.ml` (6 refs) shell out via `Lwt_process` /
   `Lwt_unix.sleep` under `Lwt_eio.run_lwt`. Note: `http-get`/`http-post` in
   `prims_net.ml` shell out to **curl**, not an OCaml HTTP client — so there
   is NO OCaml HTTP client to port (the checklist's "Eio HTTP client story"
   worry is narrower than it reads: it's curl subprocess + a curl-based
   `llm_client`, both `Lwt_process`).
5. **Prim ABI coloring** — `prim_fn = value list -> value Lwt.t` (35 `value
   Lwt.t` mentions across the image). **Already synchronous under the hood**;
   the `Lwt.t` is a vestigial monadic wrapper from the pre-effects ABI.
   Decoloring is the "decolor pass" from `docs/runtime-effects-transition.md`
   Phase 3. Low risk but wide ripple: every `register_sync` wrapper, every
   prim signature, the `Control.perform_prim` rim handler, and ~half the
   image's type ascriptions.
6. **`control_rim.run_lwt`** — already `let run_lwt = run` (an alias). Delete
   the alias + the name.
7. **Stale docstrings** in `lisp/lib/control.ml`, `eval.ml`, `effect.ml` that
   say "Lwt" where the code is already direct-style. Cleanup, not migration.

## The three traps (design, don't improvise)

These are from the checklist watch-list and are the reason this is a
migration, not a delete-and-rebuild:

### Trap 1 — the REPL (`repl_endpoint.ml`) is the only reason `Lwt_eio` exists
`boot.ml:58` runs the REPL via `Lwt_eio.run_lwt`. To delete the bridge, the
REPL must serve on Eio. Options:
- **(a) Eio socket server**: rewrite `Repl_endpoint.serve` with
  `Eio.Net.listen` + `Eio.Buf_read` for the line protocol. Cleanest; makes the
  REPL a native Eio fiber. The line protocol is simple (read line, eval,
  print), so this is contained. `lineedit.ml` (3 Lwt refs) needs the same
  treatment for any interactive line editing it owns.
- **(b) Retire the TCP REPL**: if `/repl` (browser) and `POST /api/eval` are
  the primary surfaces (they are, per `AGENTS.md`), the optional TCP REPL may
  be deletable outright. **Confirm with the user before choosing this.**

Decide (a) vs (b) first; it gates whether the bridge can go at all.

### Trap 2 — subprocess calls under `Lwt_process` must not block the Eio loop
`llm_client.ml` and `prims_net.ml` call curl / external procs via
`Lwt_process.pread` / `open_process_in`. Under a single Eio loop, a blocking
`Unix.system` would freeze every fiber. Options:
- **(a) `Eio.Process`** — spawn via `Eio.Process.run` / `crunch`, which
  integrates with the Eio scheduler. Preferred; keeps concurrency honest.
- **(b) blocking-call on a domain** — `Eio.Domain_manager` /
  `Eio_unix.run_in_systhread` to park the shell-out off the main loop.
The checklist says *"Don't improvise streaming semantics"* — the llm path
streams; preserve the read-line-until-EOF behavior exactly. Pick (a) unless
(a) can't express the timeout/`pread` contract, then (b).

### Trap 3 — `media.ml` multipart streaming parity
`media.ml` (10 Lwt refs) streams multipart upload bodies. Confirm
`Httpun_eio`'s body reader gives the same chunked semantics the old
httpaf-lwt path had. This is a *verification* task more than a rewrite: the
accept loop is already `Httpun_eio`, so mostly confirm + fix edge cases.

## Deletion inventory (the checklist, concretized)

Order matters — do the DB facade and subprocess prims before the bridge:

1. Swap `Io_lwt.Db` → `Io_eio.Db` in the 7 DB modules. Delete `io_lwt.ml`,
   the `Lwt_conn` arm of `store_db.ml`, the `Io_unix` shim, and the `PP_IO`
   env flag (`io_eio.ml` `io_mode_from_env`, `boot.ml` match). After this the
   DB is pure Eio; the only remaining Lwt is REPL/subprocess/ABI.
2. Resolve Trap 1 (REPL → Eio, or retire).
3. Resolve Trap 2 (subprocess → `Eio.Process`).
4. Resolve Trap 3 (media multipart parity).
5. Decolor the prim ABI: `value list -> value Lwt.t` → `value list -> value`.
   Update `register_sync`, the `Control.perform_prim` rim handler, and prim
   signatures. Delete `control_rim.run_lwt`.
6. Drop dune deps: `lwt`, `lwt.unix`, `lwt_eio`, `pgx_lwt`,
   `httpun-lwt-unix` from `image/lib/dune`; `lwt.unix` from `image/bin/dune`.
   Remove `io_lwt` from the `image/lib/dune` modules list (line 5).
7. `rg -n "Lwt" image/lib lisp/lib` must be empty except intentional
   docstring words (which should be rewritten to say Eio). Update
   `scripts/check_io_rim.sh` to assert it.
8. Clean stale docstrings (control.ml, eval.ml, effect.ml, boot.ml comments).

## Verification

- `dune build @all` and `dune runtest` green at each step (commit per step —
  the DB-facade swap, the REPL port, the subprocess port, and the ABI
  decolor are four independently revertable commits).
- `scripts/dev.sh start`; `curl /health`; a CRUD round-trip via
  `scripts/pp-eval.sh`; exercise a `(sleep 1)` and an `(http-get …)` prim to
  prove Trap 2; exercise the TCP REPL (if kept) to prove Trap 1.
- `ksh scripts/verify_one_tree.sh` as a broad smoke (it does CRUD + eval +
  isolation end to end).
- Concurrency sanity: hit `/health` and a data route in parallel (e.g. 50
  concurrent curls) to prove subprocess calls don't freeze the loop.

## Spec hygiene (do this in a borge plan phase, before/after make)

The `.borg` book is **declarative** — it says what IS, not what was removed
or when. Do NOT add a "we deleted Lwt" note. Instead edit the passages that
still describe Lwt as the live rim to state the Eio end state plainly:

- `borge/single-image.borg`: § runtime still says *"Lwt today; Eio is the
  floor we are moving to"* (line ~138) and *"Residual Lwt is rim-only"* (~181)
  and lists `image/lib/io_lwt.ml` (~110). Rewrite these to describe Eio as the
  (only) rim. Flip § runtime status `planned` → `implemented`.
- `borge/one-tree.borg`: the `details` blocks at lines ~58–71 and ~543 call
  `Io_lwt` "the v1 rim backend" / "vestigial rim plumbing… to be replaced" —
  that phrasing is already correct about intent; tighten to past-tense-free
  declarative ("the rim is Eio").
- `borge/image.borg`: historical references to the retired `httpaf-lwt-unix`
  launch path and `pgx_lwt` serialization (lines ~23, 28, 285, 298, 403,
  432–455, 553–573). Where these describe the *retired* path, prune or
  mark clearly; where they describe *current* state, update to Eio.
- Keep BORGE-1785397493 / BORGE-1785211159 / BORGE-1785481932 referenced in
  the checklist; close them with evidence when done (per checklist).

Spec edits and code deletions are different borge phases: update the spec in
a `borg_plan` phase first (this chapter is the guide), then `borg_make` for
the code, then `borg_review`.

## Suggested execution shape

One agent, four commits, gated by build+test+smoke between each:

1. **DB facade** (step 1) — biggest line count, lowest risk, verifies the
   whole DB layer works without `Io_lwt`.
2. **Subprocess prims** (Trap 2) — small, needs the Eio.Process decision.
3. **REPL** (Trap 1) — needs the (a)/(b) decision first; if (b), tiny.
4. **Prim-ABI decolor** (step 5) — widest ripple, do last, well-tested.

Do NOT try to do all four in one commit. Each is independently revertable.

## What this does NOT cover

- `request_context.ml` — the checklist mentions `Lwt.key` → `Eio.Fiber` key,
  but grep shows no live `Lwt.key` there now (0 refs); confirm and drop the
  checklist item if it's already done (it may have been Phase 3 work).
- This plan does not remove the **idea** of a transaction scheduler — the
  pgx connection's internal sequencer (which serializes ops on a connection)
  is Eio-side already and stays.
