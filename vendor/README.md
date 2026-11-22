# Vendored Nopales packages

This directory is the second package root beside `libs/`.

- `libs/` is the core + vault: shipped with the repo, mirrored by
  `pp-sync`.
- `vendor/` holds external Nopales projects pinned by
  `packages.lock`.

`scripts/vendor.sh` fetches each locked revision into `.sources/` and
materializes its repo root at `vendor/<package>/`. The package file is
`vendor/<package>/lib.pp`, so the loader sees the same shape as
`libs/<package>/lib.pp`.

Run:

```sh
scripts/vendor.sh          # fetch/materialize every locked package
scripts/vendor.sh status   # report missing packages
scripts/vendor.sh clean    # remove .sources/ and materialized dirs
```

The generated package directories are gitignored. Commit the lock, not
the copies. A release build stages `vendor/` beside `libs/`; the loader
searches the primary `libs` root first and then the sibling `vendor`
root. Set `PP_LIBS_PATH=path1:path2` to override the search roots
explicitly.

This slice deliberately keeps live `system/main` as the runtime source
of truth. Vendored packages are a pinned read projection; `pp-sync`
still owns `libs/`. Making an external repo the write authority is a
separate decision.

Monorepo sibling projects (for example `../pp-borge/`) are package
roots too, but they are not vendor entries: `scripts/dev.sh` adds them
to `PP_LIBS_PATH`, and the release stages them under `vendor/` beside
`libs/`.
