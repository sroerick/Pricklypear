# i-updatefield-race-2026-08-12

## BUG: update-field has lost-update race (non-atomic read-modify-write)

update_field in crud.ml does get then dict-set then update as two separate SQL statements with no transaction. Two concurrent update-field calls on the same row race: both read the same base, both write, second write clobbers first. Comment acknowledges this. Fix: wrap in with-transaction plus select-for-update. Found 2026-08-12.
## Resolution (2026-08-12, fix-loop iter 2)

Rewrote `update_field` in `image/lib/crud.ml` to run in a single
`Io_lwt.Db.with_transaction`:

- Opens ONE transaction (sets `pp.user` LOCAL via `set_local_pp_user`,
  keeping the RLS floor active — composes with Issue 1's tx routing).
- Reads the row with `SELECT ... FOR UPDATE` (same WHERE/visible_sql as
  `get`), acquiring the row lock so concurrent update-field calls on the
  same row serialize and both land (no lost-update race).
- Merges the field in OCaml (same Yojson numeric/value-coercion rules as
  before / dict-set) and issues the UPDATE via `Tree_store.update_datum`
  on the SAME tx conn — not `current_conn` (which still returns the pool
  conn inside a plain with_transaction on the Eio path).
- Preserved the original `check_type_fields` rejection path (sample
  single-field JSON) so undeclared-type validation behavior is unchanged.

Return shape unchanged: `VStr id` on success, `VNil` if no row/not
visible, `VStr error-msg` on type validation reject.

Files touched: `image/lib/crud.ml` (one function + docstring).
Verification: `dune build` green; `dune runtest` green; `borge drift`
shows only pre-existing unrelated drift (no crud.ml structural change).
Committed with `(i-updatefield-race-2026-08-12)`.
