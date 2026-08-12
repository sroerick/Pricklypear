# i-rls-nontx-2026-08-12

## Resolution

Fixed 2026-08-12. All six CRUD read prims (`get`, `list`, `find-row`, `find_rows`, `find_random`, `find_text`) in `image/lib/crud.ml` now route through `Io_lwt.Db.with_transaction` instead of calling `Io_lwt.Db.execute` on the raw pool conn.

- `with_transaction` invokes `set_local_pp_user` (store_db.ml) at tx open, so `SET LOCAL pp.user` now runs before every read and the RLS floor (`tree_data_rows` view + `tree_nodes` policy, migration 013) is active.
- The callback receives the tx conn `Some (tx, mu)` and the read bodies use that conn directly (not `current_conn`, which still returns the pool conn inside a plain `with_transaction` in the Eio path).
- `find_rows` (filters=[]) delegates to `list`; both now independently open a tx — fine since they run at top level. (`update_field`, issue 2, composes a single tx separately.)

Files touched: `image/lib/crud.ml`.

Verification: `dune build` green, `dune runtest` green, `borge drift` shows no new items (the listed prims_pure.ml/unspecified items are pre-existing).

Acceptance: RLS confinement now applies on non-tx reads (pp.user set on every read connection).
