# PP perf 1104 — phase split + statement cut

Branch `factory/perf-phase-1104` (worktree `.worktrees/perf-1104`) from master
`64f70b9`. No master push, no prod deploy, no reseed.

## What this branch changes

1. `X-Timing-Ms` carries the whole budget split, always, no env var
   (`image/lib/timing.ml`): `<total_ms> db=<db_ms> hits=<db_hits> steps=<eval_steps>`.
   Every prod hit is self-diagnosing: a 3s page reporting `db=2900.0` indicts the
   DB, high `steps` with low `db` indicts interpreted Lisp.
2. `/todos` reads the todo-list pool once per GET render instead of twice
   (`libs/todo/lib.pp`: `todo/page` threads the pool into a new `todo/ui/main*`;
   `todo/ui/main` stays 1-arity as a wrapper). Removes 2 of the 6 `tree/list`
   statements per render.

## Baseline (prod, 2026-11-04, read-only)

`GET /debug/stats` -> `timing` aggregate for route `todo/page`, 21 samples:

| field | value |
|---|---|
| avg wall | 4044 ms (max 6903) |
| db_ms | 3807 ms (94% of wall) |
| db_hits | 21 db-touching prim calls per render |
| eval steps | 235757 per render |

The ring's per-prim breakdown divided by the 21 samples is the whole statement
budget of one /todos render:

| prim | calls/render | ms/render | ms/call |
|---|---|---|---|
| tree/list | 6 | 2032 | 339 |
| tree/get | 1 | 1059 | 1059 |
| tree/find-by | 5 | 610 | 122 |
| node-path | 3 | 103 | 34 |
| space-exists? | 1 | 3.6 | 3.6 |
| total | 16 | 3807 | |

`db_ms` equals the sum of those prims exactly, so 100% of the DB time is those
16 statements and ~237 ms (6%) is everything else (eval steps + HTML build).

## Per-statement cost, measured on prod (`POST /api/eval`, read-only)

| expr | wall |
|---|---|
| `(list 1 2 3)` (no DB) | 35.5 ms |
| 1x `(tree/get <node>)` | 1070 ms |
| 2x `(tree/get <node>)` | 3278 ms |
| 4x `(tree/get <node>)` | 4555 ms |
| `(tree/list <type-prefix> 3 5)` returning 0 rows | 1385 ms |
| `(tree/list <type-prefix> 3 2 "todo-item")` returning 2 rows | 1139 ms |
| 2x the same `tree/list` in one eval | 2346 ms |
| `(todo/item/all)` (2 tree/list) | 2970 ms |
| `(todo/list/all)` (2 tree/list) | 1257 ms |
| 4x `(space-exists? "todo")` | 44 ms (11 ms/statement) |

### Verdict on agenda d7a84496 ("about 1s per tree_nodes statement whatever it returns")

CONFIRMED and narrowed. The fixed cost is ~1.0-1.6 s per **tree_nodes read**
statement, independent of rows returned (0 rows 1385 ms, 2 rows 1139 ms, 198
rows 1160 ms measured 11-02). It is **not** per-statement overhead in general:
`space-exists?` against `data_spaces` answers 4 statements in 44 ms on the same
single pgx connection. So the pool/round-trip theory and the per-statement
fixed-overhead theory are both refuted; the cost belongs to queries against
`tree_nodes` (plan and/or RLS predicate evaluation), not to the wire.

`048_tree_nodes_path_btree.sql` already added a btree on `path` + `ANALYZE` and
the second is still there, so the next step is `EXPLAIN (ANALYZE, BUFFERS)` on
the two read shapes, not another index guess.

## Attribution of the /todos seconds

For a 4044 ms render:

- 2032 ms `tree/list` x6 (50%) — four of the six are two prefix scans per
  register (garden `data.u.*` + shared `data.s.*`) for items and lists.
- 1059 ms `tree/get` x1 (26%) — a single point read costing the same ~1 s.
- 610 ms `tree/find-by` x5 (15%) — the from-notes scrape, one indexed read per
  marked keyword (`todo/node/marked-keywords`, 5 keywords).
- 103 ms `node-path` x3 (3%) — space admission lookups.
- ~237 ms eval steps + HTML (6%).

Statement count x per-statement cost is the whole story. There is no cache-miss
mystery and nothing in the HTML builder: per-row rendering is pure Lisp and the
prod `fn_steps` confirm it.

## What was cut, and what is proposed

Cut on this branch: the duplicate list-pool read in `todo/page` (2 fewer
`tree/list`).

Proposed, not done here (each needs a kernel prim or a decision, so they are
named rather than guessed at):

- **One indexed read for the from-notes scrape.** 5 `tree/find-by` calls, one per
  keyword. A multi-value variant (`body.todo = ANY($values)`) collapses them to
  one statement (~490 ms of the 610 ms).
- **One statement for the two-register scans.** `todo/item/all` and
  `todo/list/all` each issue a garden scan and a shared scan. A `tree/list`
  taking several prefixes in one SQL (`path <@ ANY(...)`) makes each one
  statement, taking 6 `tree/list` down to 2.
- **The ~1 s per tree_nodes statement itself.** This is the dominant term and it
  is not a code change: it needs `EXPLAIN (ANALYZE, BUFFERS)` on /todos' two read
  shapes, a look at the `tree_nodes` RLS predicate (`datum_user_isolation` ->
  `tree_path_in_member_space` / `tree_is_seed_type` / `tree_user_is_admin`), and
  a `VACUUM (ANALYZE) tree_nodes` (bloat on a 259 MB table would explain a
  constant second per read; 048 deliberately left VACUUM to an operator step).

With those, /todos goes from 16 statements to ~4: ~3807 ms of DB time scales to
~1 s at today's per-statement cost, or to ~200 ms if the per-statement cost is
fixed too.

## Before / after

- **Before**: measured above, on prod, 2026-11-04.
- **After**: NOT measured. There is no Postgres on this box (no psql, nothing
  listening on 5432/5433), so a local render benchmark is impossible, and this
  branch is deliberately not deployed. Both halves of the after-number need a
  deploy: the statement cut shows up as `hits=` dropping by 2 in `X-Timing-Ms`,
  and the per-statement cost only moves with an index/policy change on prod.
- The flatness check the card asks for (render 5 / 50 / 500 items, assert
  `db_hits` and `steps` do not grow) is **not written here**. It needs a live
  store to seed rows and to drive `Session.call_handler`, i.e. the same Postgres
  this box does not have; it belongs with the deploy, not ahead of it.

## Exact ask for roerick/hollis

One line: run `EXPLAIN (ANALYZE, BUFFERS)` on
`SELECT ... FROM tree_nodes WHERE path = $1::ltree` and on
`Tree_store.list_children`'s containment query on prod, plus
`VACUUM (ANALYZE) tree_nodes`, and paste the plans back — that is the one
measurement that closes the ~1 s per tree_nodes statement.
