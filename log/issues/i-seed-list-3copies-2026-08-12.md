# i-seed-list-3copies-2026-08-12

## SMELL: seed type list maintained in 3 places, no single source of truth

Seed types hardcoded in: (1) crud.ml visible_sql SQL string, (2) tree_store.ml seed_types OCaml list, (3) SQL tree_is_seed_type function. All agree today (5 types) but migration 014 already showed the drift risk -- guestbook_entry was missing from the SQL function after 013 shipped. Adding a shared type requires updating all three. Consider deriving visible_sql from tree_is_seed_type or a single OCaml constant. Found 2026-08-12.

## Resolution (2026-08-12)

Collapsed the CRUD read copy and centralized the SQL authority:

- **crud.ml `visible_sql`** — replaced the hardcoded
  `type_name IN ('bible-verse','bible-book','bible-interlinear','strongs-entry','guestbook_entry')`
  with `(author = $N OR tree_is_seed_type(type_name))`. The read filter now derives
  directly from the authoritative SQL predicate, so CRUD reads can never drift from
  the RLS floor. Removed crud.ml's array entirely.
- **migrations/016_seed_single_source.sql** — added `tree_seed_types()` as the ONE
  server-side array that lists the type names, and re-defined `tree_is_seed_type(t)`
  as `t = ANY(tree_seed_types())`. The SQL side now has a single source of truth.
- **tree_store.ml** — the OCaml `seed_types` array remains as the PURE mirror
  required by the DB-free `node-path` prim (tree_store_test.ml asserts `data.shared.*`
  routing without a DB). Added `db_seed_types` + `verify_seed_types` which read
  `tree_seed_types()` from the DB and warn (non-fatal, at boot) if the OCaml constant
  diverges — so the two domains can't silently drift. **boot.ml** calls
  `verify_seed_types` after `Store.connect`.
- **scripts/dev.sh** — added `016_seed_single_source` to the migration list.

Tradeoff (documented): a strict single literal occurrence is impossible without
making `node-path` DB-backed, which would change its "pure path constructor"
contract — deferred as a larger design change (Issue #4 territory). This fix makes
crud.ml derive from the SQL authority, gives SQL one array, and guards the remaining
OCaml mirror against drift at boot.

Verified: `dune build` green; `dune runtest` green; `borge drift` 18 items
(pre-existing baseline, no new items); migration applied to local dev PG
(socket /tmp:5433) — `tree_seed_types()` returns 5 rows, `tree_is_seed_type` and the
`tree_data_rows` WHERE fragment both execute correctly.
