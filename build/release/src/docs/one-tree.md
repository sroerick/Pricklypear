# One-tree storage

Authoritative design: [`borge/one-tree.borg`](../borge/one-tree.borg).

## What shipped (P0–P6)

| Piece | Where |
|-------|--------|
| Physical tree | `migrations/009_tree.sql` — `tree_nodes`, `tree_branches`, mirrors, views, `tree_verify()` |
| OCaml API | `image/lib/tree_store.ml` |
| Read flip | `store.ml` `fn_rel`/`ty_rel`/`rt_rel`, `crud.ml` `data_rel` — unconditional since 2026-09 |
| Write flip | `tree_store` upserts + reverse trigger |
| Empty-lens fork | `Store.fork_branch` |
| Typed boundary | `validate_fields` on create/update |
| Prims | `type-fields`, `node-path`, `backlinks` |
| ONE hash domain | `migrations/027_hash_domain.sql` + DB-side hashing in every writer (2026-09) |
| Branch algebra | `image/lib/branch_algebra.ml` + QCheck properties (formal-verification island 1) |
| Diverged = conflict-only merge | `Store.merge_branch` S3 (2026-09): diverged never auto-applied |
| Atomic promote | `Store.promote_functions` in one SERIALIZABLE tx (2026-09) |

Defaults: **tree is truth** (`PP_TREE_READS` / `PP_TREE_WRITES` ON).

## Residual (not blocking)

- Promote/merge still multi-row upsert on scaffolding tables (forward-mirrored).
- Full 3-way AST diff / content-addressed pointer promote deferred.
- Scaffolding tables not dropped (additive-only).
- Sync WIRE hash (the (path,hash) manifest with the offline lens client)
  is still the client-side content_hash domain — re-pointing it at the
  unified storage domain (or defining a canonical-JSON wire domain) is a
  local-first follow-up; see the hash-domain note in borge/one-tree.borg.
- Known pre-existing DB-suite failures on master (identical before the
  2026-09 hardening): tree_datum 1/9, tree_edges 4/5, tree_sibling_order
  5/6, sync_db 5/6, branch_status prune-synced, branch_generation/1
  (child write bumps the PARENT's generation; test has no Random.
  self_init so branch names collide across runs).

## Verify

```sh
# from repo root, with scripts/dev.sh start already up
eval $(opam env --switch=poohstack --set-switch)
ksh scripts/verify_one_tree.sh
```

Backup before P4 cutover: `backups/pg-pre-one-tree-*.dump`.
