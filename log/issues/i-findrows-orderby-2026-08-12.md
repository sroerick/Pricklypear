# i-findrows-orderby-2026-08-12

## SMELL: bible-specific ORDER BY baked into generic find-rows

find_rows in crud.ml hardcodes ORDER BY COALESCE((fields->>verse)::int, 0) ASC, created_at ASC. The verse sort is domain knowledge about bible-verse data in the generic CRUD layer. Works for other types (degrades to created_at) but couples CRUD to bible schema. Consider optional sort parameter or moving verse-ordering to bible/chapter. Found 2026-08-12.

## Resolution (2026-08-12, Iteration 4)

Removed the hardcoded verse ORDER BY from generic `find_rows`.

**What changed**:
- `image/lib/crud.ml`: `find_rows` now takes an optional `?(sort : string option = None)` param. When `Some s`, the SQL becomes `ORDER BY <s> LIMIT $n` (sort appended verbatim); when `None`, falls back to the generic default `ORDER BY created_at ASC`. The bible-verse expression is no longer baked into crud.ml.
- `image/lib/prims_db.ml`: `find-rows` parser now accepts an optional trailing `[sort]` VStr positional (after optional `[limit]`): `(find-rows 'type "f" "v" … [limit] [sort])`. Returns `(filters, limit, sort)`.
- `libs/bible/lib.pp`: `bible/chapter` now passes the verse sort explicitly: `"COALESCE((fields->>'verse')::int, 0) ASC, created_at ASC"`.

**Verification**:
- `dune build` green.
- `dune runtest` green (full suite).
- `borge drift`: only pre-existing unrelated items (18 before and after), no new drift.
- Acceptance grep: `crud.ml` no longer contains `fields->>'verse'` ORDER BY SQL (only unrelated pre-existing mentions of `'bible-verse'` in the seed-type list at line 42 and a tree-mirror comment at 197, plus the docstring). Verse ordering now lives solely in the bible caller.

Files touched: `image/lib/crud.ml`, `image/lib/prims_db.ml`, `libs/bible/lib.pp`.
