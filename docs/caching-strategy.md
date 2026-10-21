# PricklyPear caching strategy

Status: design v1, 2026-10-21 (gregor). Companion cards: PERF-1..9 on the Board list 0b21ffaa. This doc is the reference for the perf loop; each round re-benches and cuts cards from the profiler output.

## Measured baseline (evidence)

- tree/get ~1.5s, tree/list ~0.5-1s per call, flat vs selectivity (prod, 10-05, warm, in-image deltas). Cause: Postgres RLS per-row policy tax + no fast ltree path.
- /pulse, /workstreams, /kanban re-scan the whole todo-item subtree ~3x per stream per render: ~70 tree/list calls x 0.5-1s = 30-90s pages (P0 board card).
- Board reads 10-30x over the 100k-us eval budget (/timing pp/4 wire-cost ledger).
- Trivial eval (+ 1 2) via /api/eval: 0.31s -> 0.84s -> 1.10s over three consecutive calls (vultr probes, 10-21). Per-eval overhead dominates and degrades under consecutive load.
- No HTTP caching today: every repeat request re-pays full render and bandwidth.

## Principle

One image process owns every write. Caching is safe wherever we can version-stamp: a synchronous invalidation at the write path is complete by construction - no cross-node protocol. Cache aggressively at every layer, invalidate exactly where writes commit, measure every round.

## Layers (edge -> kernel)

### L0 - nginx edge (deploy config)

- proxy_cache micro-cache (30-60s) for anonymous GETs on the public surface (.rocks + .club).
- Static assets: far-future Cache-Control + immutable.
- gzip at the edge; ETag passthrough.

### L1 - render memo + ETag (OCaml; PERF-2, PERF-3)

- In-image memo table keyed (path, sorted params, anon-only) with a short TTL: identical anonymous GETs return the same HTML. Authed content must never enter the anon memo.
- ETag = hash of rendered body; If-None-Match -> 304. Repeat visitors stop re-paying render and bandwidth.

### L2 - in-image store cache (OCaml; PERF-9)

- Tree nodes cached by path, stamped with a per-garden version counter; write prims bump the touched garden's version inside the same tx.
- Subtree reads materialize once per render from the cache: one scan instead of ~70 tree/list calls (this is the P0 fix).
- Board JSON serialized once per (list, version) for the kanban page.

### L3 - Postgres fast path (OCaml + SQL; PERF-5)

- Reads run through a security-definer function with the authz predicate applied once per query (the app already computes the garden/user filter), instead of per row via RLS.
- ltree/path-prefix index on the tree table; path_id lookups O(1).
- Prepared statements cached on the pool; pool sized to domain count.
- Target: tree/get 1.5s flat -> tens of ms, scaling with selectivity.

### L4 - kernel/runtime caches (OCaml; PERF-7)

- lib.pp parsed once per package, ASTs shared across sessions; load-library becomes an O(1) bind (load_skips stops fingerprint-scanning store rows; be1b4d6 lineage).
- Session env LRU: hot sessions stay materialized in memory; rehydrate on miss only. Fixes the degrading trivial-eval curve.
- JSON codec: encode hot rows once per version.

### L5 - concurrency (OCaml; PERF-6)

- httpaf request handling on multiple Eio domains; PG I/O, JSON, and rendering run off the main domain.
- The Nopales interpreter stays serialized behind an eval lock - image semantics preserved; everything around it parallel.
- This is the under-load answer: today one long eval starves the single domain; after, evals queue but pages don't.

## Bench harness + round protocol (PERF-8)

- Fixed probe set: /health, /, /kanban, /notes, trivial eval, board read, todo/item/active.
- Concurrency ladder 1/4/8/16; record p50/p95 per round into a PP ledger note.
- Prod is a valid bench target (roerick green-lit 10-21).
- Round protocol: bench -> profile (pp/4 ledger) -> cut cards -> runners patch -> re-bench. Repeat.

## Targets (round exit criteria)

- Trivial eval p95 < 50ms warm (now 0.3-1.1s, degrading).
- tree/get p95 < 100ms (now ~1.5s flat).
- Board pages < 1s (now 30-90s).
- p95 degradation under 8 concurrent users < 2x idle (now: unbounded, single domain).

## Order of attack (biggest lever first)

1. One-scan subtree reads (P0 card + PERF-9): 30-90s pages -> seconds.
2. L3 PG fast path (PERF-5): the kernel flat-cost card.
3. L2 store cache + L1 render memo: multiply 1+2 across requests.
4. L5 concurrency (PERF-6): load behavior.
5. L4 kernel caches (PERF-7): steady-state overhead.

## Risks / notes

- The eval lock keeps Nopales throughput bounded by one core; caching and PG fixes move work out of the locked section. Long evals remain the thing to watch (64 KiB eval cap stays).
- L1 memo keys on anon-only; authed content never enters the anon memo.
- be619942 (stale todo package gen 500s) is a correctness bug, not perf; track on its card.
- Builds on vultr: opam switch poohstack, not /usr/local/bin/dune (see AUTOLITH.org). The wyo hook builds + deploys on master push.
