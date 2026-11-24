# Pricklypear agent history

Archived from the historical hand-authored `pp/AGENTS.md` when the durable
agent instructions moved to `pp/AGENTS.fragment.md`. This file preserves
status snapshots and incident narratives; it is not the current quick-start.

## Status (2026-08-31 reconcile)

**Orchestration (2026-09-25):** the work-triage board became a real delivery
pipeline — spec `borge/workstreams.borg` (registered in the book after
work-triage). Workstream = a todo-list (glochid `fa93a1af`, Board `0b21ffaa`,
…), same column taxonomy everywhere; the habitat is the **record + queue**, the
laptop is the **executor** (scripts/pp-flow.sh + pi ralph/subagent claim cards
via the brief protocol; no agents in the prod image). List deletion is allowed
(`todo/delete-list`; default-Inbox guarded) — the old 'never delete' rule was
retired 2026-09-25 after a 365→9 list cleanup (snapshot
`backups/pre-cleanup-20260925-005600.json`).

**Unattended executor loop (2026-09-27).** `scripts/pp-flow.sh --drain
[list-id] [N]` picks its own work (queue order, TODO/CODE, unleased) and runs
each card as a separate `--run` subprocess — that separation is what keeps an
AUTO-lane LOGIC-REVIEW reviewer independent of the implementer. The runner for
headless runs is the Autolith adapter, pinned to Synthetic:

```sh
set -a; . ~/.config/pricklypear/env; set +a
export PP_FLOW_PI="$PWD/scripts/pp-flow-autolith.sh"
export AUTOLITH_MODEL=syn:large:text        # default; the adapter assumes it
scripts/pp-flow.sh --drain 6f625d8c-fb5f-485c-b5c7-f2f4c41772cc --states TODO --max 1
```

Knobs: `--states "TODO,CODE"`, `--exclude-kinds BUG`, `--max N`,
`PP_FLOW_DRAIN_CONTINUE=1` (don't stop on the first failure), `--force`
(reclaim a live lease), `PP_FLOW_AUTOLITH_PERM=full` (cards that must touch
the repo). The adapter refuses, in <1s with the accepted values named, a model
that rejects the executor role's `:reasoning-effort :medium` (the Synthetic
glm-flash family accepts only low|high|max) — it dies as a cryptic
`:PROCESS-FAILURE` otherwise. `syn:large:text` is the tested default. The
failure line reports the provider category + message verbatim. NOTE
(2026-09-27): one run hit a transient Synthetic HTTP 402 (“Subscription access
is currently blocked…”); immediate retries of the same model+key succeeded and
direct curl never failed, so that was upstream flakiness — do NOT read a lone
402 as an exhausted account. The adapter now RETRIES exactly that class of
fault (HTTP 402/408/409/425/429/5xx, plus a run-job crash with no result):
2 attempts by default, 10s backoff doubling to a 300s cap
(`PP_FLOW_AUTOLITH_RETRIES`, `PP_FLOW_AUTOLITH_RETRY_BASE`). Deterministic
failures — `:PROCESS-FAILURE`, `:INVALID-*`, `:TIMED-OUT`, a 401/403/404 — run
exactly ONCE, so a retry can never burn a card's lease repeatedly on the same
guaranteed failure. An agent-named `DESTINATION:` is validated
against the card's own column set before it is obeyed; an unknown column falls
back to the one-column advance. Per-run ledger: `.ralph/runs.jsonl` (+ `RUN
SUMMARY` lines).

**Single-image v1 habitat is the running system.** Spec is a multi-file
literate book (chapter `.borg` files + root). `borge report` after the
habitat-book reconcile is roughly **268 implemented / 53 planned /
3 deprecated (~83%)** — planned items are real gaps (roam, promote UX,
type-system, review-merge, full FTS, habitat chapter editing, live
OCaml eval), not stale foundation work. The spec-as-book pipeline
(habitat-book chapter) is implemented end to end: borge book
print/export over the inline walk, `scripts/load-book.sh` seeding,
`/book` + `/book/chapter/<slug>` + `/book/course/<slug>` habitat faces
(libs/book), restricted `example-eval` prim + `book/verify`
(drift-as-rendering), first course `course/cs-interpreters.borg`, and
release artifacts (book.pdf workshop + reader flavor + export JSON).
Packages on poohstack; build/tests on lisp + common + image. Product
libs (notes/todo/outline/ui/bible/dabar/ai/…) seed to `system/main`.
Supervisor/authd multi-process topology is retired (see
`borge/single-image.borg`). Open borge issues that still matter live in
`.borge-bugs/` (quote compounds, AST nodes design, schema-authority,
optional SSH). Monitor command: bottom of `README.md`.

### 2026-10-18 page-latency incident (forced-RLS def views + factory load)

**Symptom.** Every page was seconds slow, `/welcome` (a pure static
compose) worst at 2-8s; `/health`, `/agent.txt` and
`/.well-known/agent.json` stayed fast. Load on the 1-vCPU box sat at
1.3-1.8 with one Postgres backend pinned at 70-85% CPU for hours; the
image log showed ~2.5 statements/sec over 20ms.

**Two independent causes, both fixed.**

1. **The def views were not indexable under FORCED RLS** (code bug,
   migration `044_tree_def_name_index.sql`). `tree_nodes` has
   `relforcerowsecurity = true`, and the views derived `name` in the
   SELECT list as `tree_dec_label((string_to_array(path,'.'))[4])`.
   Because `tree_dec_label` is IMMUTABLE but **not LEAKPROOF**, the
   planner may not push the derived-name equality below the RLS
   barrier: it drove from `tree_nodes_kind_type_idx` on `kind='fn'` and
   filtered after the policy, scanning all 4293 fn rows (259MB table)
   to return 1. Measured as the app role on prod: view lookup **48.7ms**
   with "Rows Removed by Filter: 4292"; the same shape against
   `function_defs` (which has a plain `(branch_id, name)` btree) **1.1ms**.
   An expression index on `tree_dec_label(...)` does NOT fix it — also
   unusable across the RLS barrier (verified). The fix is a STORED
   GENERATED column `tree_nodes.def_name` (a plain column reference
   raises no leak-proofing question) + `(def_name, kind)` partial index
   + the three views selecting the column. Result on prod: 48.7ms →
   **2.4ms**, lockstep intact (0 drift). Table rewrite ~14-18s.
   **Durable lesson: a non-LEAKPROOF expression anywhere in a predicate
   on an RLS-forced table silently disables its indexes. Prefer stored
   columns for derived join keys.**

2. **The software factory ran ~92% of all HTTP traffic with 70x
   waste** (glochid/pp-flow, commit cddd0a9). The supervisor and
   pp-flow wrapped *every* todo call in
   `(begin (load-library "todo") ...)`, re-evaluating the whole
   package onto the runner's branch per call. Package names bind
   LAZILY (image.lazy-env-load) and sessions are per-identity and never
   expired, so the wrapper was pure waste: measured on prod against a
   fresh identity, `(count (todo/item/all))` is **23ms bare vs 1600ms
   wrapped (~70x)**, and it spammed the log with `[load-library] todo
   onto non-main branch ...` pin warnings. `pp_todo_val` also prefixed
   its own wrapper, double-wrapping 5 call sites.

**Operational follow-ups applied.** Unrotated `image.log` (137MB of
mostly `PP_TRACE_DB` lines) is now rotated by an idempotent
`install_log_rotation` in `pricklypear-promote.ksh` (OpenBSD newsyslog
reads ONLY `/etc/newsyslog.conf` — there is no `.conf.d` drop-in on the
runtime host); prod's log was rotated live. **`PP_TRACE_DB=1` is set in
prod's `/etc/pricklypear/pricklypear.env`** — it traces every query
over 20ms to stderr and should be removed once the remaining slow
statements (`tree_data_rows` scans, RLS-filtering 869 rows in ~84ms)
are dealt with.

**Debug recipes that worked** (reusable): `/timing` is the per-route
server-side truth; `PP_EVAL_TRACE=1` gives per-phase attach timings;
`PP_TRACE_DB=1` logs any query over 20ms; `activity_ledger` plus
`pg_stat_activity` identify who is generating load; `nginx-access.log`
is the only access log (nginx logs there, not `/var/log/nginx`).
Compare app-role timings against superuser timings — **RLS only applies
to the app role**, so a superuser EXPLAIN will happily show an index
scan that the image can never get.

### 2026-09-14 promote incident (gate vs boot floor)

Two promotes (20347ee, ca2bc21) rolled back despite healthy binaries:
gregor's town-p1 moved `seed_agents_note` into `boot_personal`, and its
existence probe — even the "cheap" `notes/doc/row` form (40s live) —
pays prod's notes tree floor before `/health` answers; boot-personal
hit 78-90s against a 30s gate. Fix (bede450): the agents-note seed is
**async** — a Boot.start_eio fiber with its own store+session (jobs-
runner pattern, same identity/branch); boot_personal no longer calls
it. Health gate raised 30s→120s (`scripts/openbsd/pricklypear-promote.ksh`).
Two standing constraints: **nothing that touches the notes tree eval
path may run synchronously before /health serves**, and the promote
hook self-updates the installed promote script only AFTER a success —
so a gate fix can never rescue its own first deploy. Rollback restarts
can also race EADDRINUSE with a slow old image (rc_stop's port-free
poll is bounded at 10s+2s).

### 2026-08-21 session model (perf)

Shared-session flip stall is fixed on prod (commits bf2e808, 74699ba):
per-identity `Session_registry` (one `Session.t` per identity, own pgx
conn; anonymous parked on `system/main`), env loading is VThunk-lazy
(`load_defs` binds recipes; bodies force once on first touch,
memoized incl. failures — defs are recipes, never load-time hooks),
staleness stamp at attach, `function_stat` ring-flushed (30s). Specs:
`borge/image.borg` lazy-env-load / per-identity-sessions /
function-stat-ring; `borge/lisp-interpreter.borg` lazy-env-thunks
(user surface: `(delay)` / `(force)`). Runtime now needs the OpenBSD
`argon2` package (first argon2id deploy). Known remaining floors,
filed in `/issues`: `notes/doc/get` ~80s on big docs; argon2id verify
per Basic-auth request (~0.5-1s/request). Live details: notes doc
`agents` → "Sessions & lazy env (2026-08-21)".

### 2026-09-08 perf batch (four log/issues, deploys 81c0606 + fe78dbe)

`function_stat` telemetry LEFT the object tree (migration 031):
dedicated table, plain INSERT flush, set-based retention (newest 1000).
TELEMETRY IS NOT CONTENT — never store it as tree datums, and never
bulk-delete tree datum rows per-row on prod (~1s/row: mirrors + RLS +
versions; a 54k-row delete was a 25-minute transaction that took prod
down — the rc.d health probe now has a 5s timeout so the watchdog
cannot hang). Also in the batch: `tree/note-index-full` (real outlines
for scrapes; the plain index stays light for the /notes UI), a
content-keyed JSON parse memo under the dict prims, one grouped COUNT
for `/data`, `branch_status` from live tree rows + a def_gen-SUM stamp
cache (`/branches` warm ~1.3s), and the load-library skip map
fingerprinting chain-effective bodies (+ begin-wrapped top-level
defines persist). Details: notes doc `agents` → "Perf batch + incident
(2026-09-08)".
