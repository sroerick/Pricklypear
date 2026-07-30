# One-tree storage

Authoritative design: [`borge/one-tree.borg`](../borge/one-tree.borg).

## What shipped (P0–P6)

| Piece | Where |
|-------|--------|
| Physical tree | `migrations/009_tree.sql` — `tree_nodes`, `tree_branches`, mirrors, views, `tree_verify()` |
| OCaml API | `image/lib/tree_store.ml` |
| Read flip | `store.ml` `fn_rel`/`ty_rel`/`rt_rel`, `crud.ml` `data_rel` |
| Write flip | `tree_store` upserts + reverse trigger |
| Empty-lens fork | `Store.fork_branch` |
| Typed boundary | `validate_fields` on create/update |
| Prims | `type-fields`, `node-path`, `backlinks` |

Defaults: **tree is truth** (`PP_TREE_READS` / `PP_TREE_WRITES` ON).

## Residual (not blocking)

- Promote/merge still multi-row upsert on scaffolding tables (forward-mirrored).
- Full 3-way AST diff / content-addressed pointer promote deferred.
- Scaffolding tables not dropped (additive-only).

## Verify

```sh
# from repo root, with scripts/dev.sh start already up
eval $(opam env --switch=poohstack --set-switch)
ksh scripts/verify_one_tree.sh
```

Backup before P4 cutover: `backups/pg-pre-one-tree-*.dump`.
