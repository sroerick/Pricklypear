# R1 static perf lint: real findings across libs/

Date: 2026-11-05. Branch `factory/guard-r1-1105` (worktree
`/home/al/.worktrees/guard-r1-1105`). Tool: `perflint/` (library
`pp_perf_lint`, binary `pp-perf-lint`).

Rule: R1 from `perf/PERF-GUARD-SPEC.md`: a DB prim call lexically inside a
lambda passed to a row loop is one statement per row. RED = the prim is in the
loop lambda. AMBER = the prim is in a helper that loop lambda calls (a lambda
bound in the same define, or a define in the same `lib.pp`).

DB prim set: `Db_cost.known_prims` (the tier table, reads and writes) plus
`tree/list-kind` and `datum/rows-for-item`.

Row loops: `list/foldl`, `list/foldr`, `list/map`, `list/filter`, `for-each`,
`list/for-each`, `each`, plus package-local HOFs recognised by name tail
(`map`, `map-indexed`, `filter`, `foldl`, `foldr`, `for-each`, `each`) when a
lambda is passed.

```
$ dune build perflint/bin/perf_lint_main.exe
$ _build/default/perflint/bin/perf_lint_main.exe libs
... 34 finding lines ...
perf-lint R1: 76 lib.pp files, 34 findings (18 RED, 16 AMBER)
$ echo $?
1
```

## Counts

- 76 `libs/*/lib.pp` parsed, 0 parse errors, 0 position-reader disagreements.
- 34 findings in 11 files: **18 RED, 16 AMBER**.
- By file: todo 9, notes 9, edit 7, workout 2; wiki, roam, music, memory, mem,
  kanban, food 1 each.
- RED by prim: `delete` 8, `tree/get` 2, `tree/delete` 2, `tree/move` 1,
  `tree/list-kind` 1, `tree/get-uuid` 1, `tree/find-by` 1, `space-admit` 1,
  `get` 1.
- AMBER by prim: `get` 3, `tree/upsert` 2, `tree/get-uuid` 2, `tree/get` 2,
  `node-path` 2, `create` 2, `update-field` 1, `rows` 1, `delete` 1.

## RED (gates CI)

| file:line:col | define | prim | loop |
|---|---|---|---|
| libs/edit/lib.pp:668:23 | edit/edl-rebuild-from-shots | delete | list/map |
| libs/edit/lib.pp:1871:9 | edit/shot-cast-clear | delete | list/map |
| libs/mem/lib.pp:177:28 | mem/forget | delete | map |
| libs/memory/lib.pp:96:28 | mem/forget | delete | map |
| libs/music/lib.pp:311:27 | music/pl-tracks | get | list/map |
| libs/notes/lib.pp:2802:20 | notes/migrate/legacy-delete-all | tree/get-uuid | list/map |
| libs/notes/lib.pp:2805:15 | notes/migrate/legacy-delete-all | delete | list/map |
| libs/notes/lib.pp:4269:17 | notes/tree/clear-headings | tree/delete | list/foldl |
| libs/notes/lib.pp:4293:22 | notes/tree/clear-stale-headings | tree/delete | list/foldl |
| libs/roam/lib.pp:107:14 | roam/delete-from-slug | delete | list/foldl |
| libs/todo/lib.pp:289:41 | todo/adopt-stranded | tree/get | list/foldl |
| libs/todo/lib.pp:293:35 | todo/adopt-stranded | tree/move | list/foldl |
| libs/todo/lib.pp:298:19 | todo/adopt-stranded | tree/list-kind | list/foldl |
| libs/todo/lib.pp:1347:19 | todo/ensure-space | space-admit | list/foldl |
| libs/todo/lib.pp:3506:21 | todo/node/marked-rows | tree/find-by | list/foldl |
| libs/todo/lib.pp:3742:19 | todo/node/slug-map | tree/get | list/foldl |
| libs/wiki/lib.pp:275:22 | wiki/prune-events | delete | list/foldl |
| libs/workout/lib.pp:84:20 | workout/cal-delete-ksk1-from | delete | list/foldl |

## AMBER (advisory)

| file:line:col | define | prim | loop | via |
|---|---|---|---|---|
| libs/edit/lib.pp:62:16 | edit/shot-row | get | list/map | edit/asset-get |
| libs/edit/lib.pp:610:5 | edit/edl-rebuild-from-shots | create | list/map | edit/edl-add |
| libs/edit/lib.pp:2302:16 | edit/export-cue-sheet | get | list/map | edit/take-get |
| libs/edit/lib.pp:2339:5 | edit/choose | update-field | list/map | edit/take-set-state |
| libs/edit/lib.pp:2354:11 | edit/spend-cents | rows | list/foldl | edit/takes |
| libs/food/lib.pp:826:5 | food/apply-templates | create | food/foldl | food/log |
| libs/kanban/lib.pp:1061:16 | kanban/waiting-page | get | list/foldl | kanban/waiting-li |
| libs/notes/lib.pp:2829:24 | notes/migrate/legacy-force-all | tree/get-uuid | list/map | notes/migrate/legacy-force-one |
| libs/notes/lib.pp:2858:24 | notes/migrate/legacy-all | tree/get-uuid | list/map | notes/migrate/legacy-one |
| libs/notes/lib.pp:2886:21 | notes/migrate/legacy-all | tree/upsert | list/map | notes/migrate/legacy-one |
| libs/notes/lib.pp:5007:18 | notes/tree/save-outline | tree/get | list/foldl | notes/tree/write-heading |
| libs/notes/lib.pp:5017:9 | notes/tree/save-outline | tree/upsert | list/foldl | notes/tree/write-heading |
| libs/todo/lib.pp:4426:5 | todo/adopt-stranded | node-path | list/foldl | todo/space-path |
| libs/todo/lib.pp:4894:13 | todo/dep/blocked-ids | tree/get | list/foldl | todo/tree/get |
| libs/todo/lib.pp:4894:23 | todo/dep/blocked-ids | node-path | list/foldl | todo/tree/get |
| libs/workout/lib.pp:493:5 | workout/session-delete | delete | list/map | workout/exercise-delete |

## What the counts mean

`todo/adopt-stranded` is three of the eighteen REDs, not the only one: `tree/get`
and `tree/move` sit in the inner 4000-row `list/foldl`, and `tree/list-kind` sits
in the outer `list/foldl` over `todo/admitted-types`, so the 4000-row scan runs
once per admitted type.

Eleven of the eighteen REDs are writes (`delete`, `tree/delete`, `tree/move`,
`space-admit`). The tier table is reads plus writes, per the spec's "DB prim set =
db_cost.static_cost tiers"; a per-row write is the same N-statements defect as a
per-row read. If the first gate should be reads-only, filter `Db_cost.tiers` by
tier and the RED count drops to 7.

Some REDs are one-shot or admin paths rather than render paths
(`notes/migrate/*`, `edit/edl-rebuild-from-shots`, `edit/shot-cast-clear`,
`wiki/prune-events`, `workout/cal-delete-ksk1-from`, `mem`/`memory` `forget`).
R1 has no route knowledge, so it reports them the same as page paths. Treat this
table as a triage list, not a fix list: the N+1 reads on page paths are
`music/pl-tracks` (one `get` per track id), `todo/node/slug-map` (`tree/get` per
node), `kanban/waiting-page` (`get` per waiting card) and, at AMBER,
`todo/dep/blocked-ids` and `edit/export-cue-sheet`.

Dedupe is one finding per prim call site: a helper called from two loops is
reported once, against the first loop seen.

## How the AST was reached

`libs/*/lib.pp` are parsed with the repo's own reader
(`Pricklypear_lisp.Reader`), the same reader the loader uses, so the analysis
runs on `Pricklypear_lisp.Ast.expr`, not on text patterns. The reader exposes
positions only for top-level forms (`Reader.read_all_pos`), so per-node
line/column come from a thin wrapper in `perflint/lib/perf_lint.ml` that reuses
the reader's own token functions (`peek`/`advance`/`skip_ws_and_comments`/
`read_atom`/`read_string`). Every file is then cross-checked against
`Reader.read_all`: a structural mismatch is a hard error, so a drifting wrapper
cannot quietly report wrong lines. 76/76 files matched.

## Not covered

R2-R7 from the spec are not implemented here (R2 recursive helper reachability,
R3 distinct-prim budget, R4, R5-R7 dynamic). R1 also has no idea whether a
define is on a render route.
