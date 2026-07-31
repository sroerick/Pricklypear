# Eio floor — implementation checklist

**Authority:** `borge/single-image.borg` → subsection `runtime` (the law).
**Runbook:** `docs/runtime-effects-transition.md` (H1/H2 done; this is H3 + decolor).
**Goal:** ONE effects-native runtime. Eio is the floor; Lwt leaves the dependency tree.

Closes borge issues: BORGE-1785397493 (crud Lwt), BORGE-1785211159 (session peel),
BORGE-1785481932 (spec promotion).

## Ground rules

- Bottom-up, commit-sized steps. Each step leaves `dune build && dune runtest` green.
- `ksh scripts/check_io_rim.sh` stays green at every step.
- Do not edit `.borg` files during build; file `borge issue new "..."` for spec gaps.
- Pure `lisp/` tests stay service-free. No guest-visible change: Nopales syntax,
  capability badges, `(with-transaction ...)` surface all unchanged.
- Hard invariants (from spec): composition yields plain values; dynamic tx scope
  per-fiber, never ambient-global; nested `with-transaction` fails; budget ticks
  at eval step AND rim dispatch.

## Phase 0 — Baseline (no code change)

- [ ] `dune build && dune runtest` green on master.
- [ ] Boot dev stack (`scripts/dev.sh start`) on default `PP_IO=lwt`; smoke
      `/health`, login, one CRUD roundtrip via `/api/eval` (get/create/rows),
      `(with-transaction (lambda () (create ...)))` + `select-for-update`.
- [ ] Record current `rg -c "Lwt" image/lib/*.ml` as the before-count.

## Phase 1 — Eio soak (flag stays, still opt-in)

Make `PP_IO=eio` a first-class citizen before flipping anything.

- [ ] Boot with `PP_IO=eio`; fix whatever breaks. Known surface from Appendix G:
      `Io_eio.Pg` (pure pgx over `Eio.Net`), native `Httpun_eio` accept loop,
      REPL via `Lwt_eio` from `Boot.prepare`.
- [ ] Full smoke matrix under eio: `/health`, login/session cookie, CRUD
      roundtrip, `with-transaction` + `select-for-update`, `fork`/`checkout`,
      `load-library`, `/repl`, `/api/eval`, `sleep`, `http-get`, media upload.
- [ ] `image/test/io_eio_test.ml` green with `PP_REQUIRE_EIO_DB=1`.
- [ ] Run the whole `dune runtest` suite under `PP_IO=eio` (add CI/dune alias if cheap).
- [ ] Verify `eio_enter_depth` guard still prevents nested `run_eio` in tx paths.
- [ ] Fix gaps found; if something is architecturally blocking (e.g. a prim that
      genuinely needs Lwt semantics), STOP and file a borge issue.

## Phase 2 — Flip default

- [ ] `store_db.ml`: default backend becomes Eio when `PP_IO` unset; `PP_IO=lwt`
      remains as explicit rollback.
- [ ] `http.ml` / `boot.ml`: Eio accept loop is the default; `Lwt_main` path only
      under explicit `PP_IO=lwt`.
- [ ] Deploy notes: `/etc/pricklypear/pricklypear.env` unchanged gets Eio;
      document rollback (`PP_IO=lwt` + `rcctl restart`).
- [ ] Soak on prod. This is the point of no cheap return — do not proceed to
      Phase 3 until quiet.

## Phase 3 — Decolor domain (mechanical; closes 1785397493)

Only after Eio is the floor. DB calls now block the calling fiber.

- [ ] `io.mli`: DB port signatures become plain `'a` (drop `Lwt.t`).
- [ ] `io_eio.ml`: already direct-style; conform to new port.
- [ ] `io_lwt.ml` / `store_db.ml`: Lwt backend keeps working via existing
      `Lwt_eio` bridging (it runs under the Eio main loop now), but expose
      direct-style signatures at the port.
- [ ] `store.ml`, `crud.ml`, `tree_store.ml`, `auth_store.ml`, `ai_store.ml`,
      `ops_incidents.ml`: delete `open Lwt.Syntax`, convert `let*` → `let`,
      `Lwt.return x` → `x`, `Lwt.fail e` → `raise e`.
- [ ] `loader.ml`, `agent_tools.ml`, `session.ml`: same decolor. Session's
      prim closures become plain `value list -> value`.
- [ ] `Primitives.prim_fn = value list -> value`; `register` and
      `register_sync` collapse (keep both names if it minimizes churn).
- [ ] `control_rim.ml`: add direct-style `run_eio`; effect case calls prim fn
      and `continue`/`discontinue` directly — no promise binding.
- [ ] `http*.ml`, `repl_endpoint.ml`: adapt handlers to direct-style eval at
      the Eio rim (fiber per request already exists via `accept_fork`).
- [ ] Tests: drop `Lwt_main.run` wrappers around prim/eval calls.

**with-transaction invariant:** `with_dynamic_transaction` binds `current_tx`
around the thunk; the guest lambda re-enters `Eval` under the existing rim;
restore on exit AND exception (`Fun.protect`). Per-fiber binding — verify two
concurrent requests' transactions don't see each other's `current_tx`.

## Phase 4 — Session peel (closes 1785211159)

With all prims uniform `register_sync` tables, extraction is mechanical. One
module per commit:

- [ ] `prims_auth.ml` — create-user, tokens, is-admin?, create-agent, promote
- [ ] `prims_agent.ml` — agent-* tools, llm_* prims
- [ ] `prims_net.ml` — http-get/post/put/fetch, sleep
- [ ] `prims_db.ml` — finish the crud.ml partition (registration glue)
- [ ] `prims_request.ml` — request-scoped prims
- [ ] `boot.ml` — seed_stdlib, discover_seed_packages, agents-note seeding
- [ ] `session.ml` keeps: `t`, rehydrate, checkout, eval_one/eval_form,
      run_handler.

## Phase 5 — Delete Lwt

Only after Eio path is default and quiet.

- [ ] Remove `io_lwt.ml`, `store_db.ml` Lwt_conn arm + `Io_unix` shim,
      `control_rim.run_lwt`, `PP_IO` flag.
- [ ] Drop deps from `image/lib/dune`: `lwt`, `lwt.unix`, `lwt_eio`, `pgx_lwt`,
      `httpaf-lwt-unix` / `httpun-lwt-unix`.
- [ ] `rg -n "Lwt" image/lib lisp/lib` → empty. Update `scripts/check_io_rim.sh`
      to assert it.
- [ ] Flip spec subsection `runtime` status planned → implemented on next
      borge plan pass; update `thesis`/`topology` subsections (they still say
      httpaf-lwt-unix / pgx_lwt).
- [ ] Close BORGE-1785397493, BORGE-1785211159, BORGE-1785481932 with evidence.

## Watch-list (known traps)

- `llm_client.ml` — network I/O; needs an Eio HTTP client story or stays on a
  fiber-blocking unix call. Don't improvise streaming semantics.
- `media.ml` upload — multipart streaming; confirm Httpun_eio body handling
  parity with the httpaf path.
- `request_context.ml` uses `Lwt.key` for per-request binding — under Eio this
  becomes a fiber-local (`Eio.Fiber` key or dynamic binding). Part of Phase 3.
- Fire-and-forget `Lwt.async` sites — all known ones were already converted to
  awaited calls (BORGE-1785115219/1785211092); if you find a new one, make it
  a fiber spawn (`Eio.Fiber.fork`) with explicit error handling, never silent.
