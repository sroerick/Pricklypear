# i-stale-comments-2026-08-12

## CLEANUP: stale comments in crud.ml data_rel and module header

data_rel comment says reads go through data_rows but returns tree_data_rows. References PP_TREE_READS=1 as conditional but code unconditionally returns the view. Says writes always hit data_rows but writes go through Tree_store to tree_nodes (reverse trigger mirrors to data_rows -- opposite direction). Module header has similar stale text. Mid-refactor leftovers. Found 2026-08-12.

## Resolution (2026-08-12)

Pure comment fix in image/lib/crud.ml. No behavior change.

- **Module header**: dropped "CRUD primitives over the data_rows table" and the
  storage.types/data_rows framing; now describes one-tree datum namespace —
  reads project through tree_data_rows (the tree_nodes datum view), the
  data_rows scaffolding table is the reverse-mirror kept warm for residual
  readers / fork-copy; data not branch-versioned; RLS on tree_nodes is the
  floor (migration 013). Removed stale "Uses [Io_lwt.Db.current_conn]" note
  in favor of noting read prims run inside with_transaction (RLS floor active;
  cf. i-rls-nontx).
- **data_rel comment**: removed the PP_TREE_READS=1 conditional language —
  tree_data_rows is the UNCONDITIONAL read source. Corrected the write
  direction: writes go through Tree_store into tree_nodes directly; the
  REVERSE trigger mirrors tree_nodes back into data_rows (opposite of the old
  "writes always hit data_rows" claim).
- **deftype docstring**: "actual data lives in data_rows (migration 003)"
  corrected to datum nodes (tree_nodes / tree_data_rows), migration 013.
- Line 204 already correct ("tree is truth; reverse trigger mirrors to
  data_rows") — left as-is.

Verified: `dune build` green; `dune runtest` green. borge drift: 19 items =
18 pre-existing baseline + 1 for the new migrations/016_seed_single_source.sql
being unlisted in borge/migrations.borg (same accepted pattern as
015_jobs.sql); Issue 7 itself added no structural drift.
