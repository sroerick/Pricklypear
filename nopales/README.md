# nopales — the standalone script runner

`nopales` runs Nopales as a plain programming language: the pure
interpreter (`lisp/`) plus the pure prim catalog (`pure/`) and a small
process host. No image, no Postgres, no network, no auth. With
`--lens DIR` it also reads the data rows a local lens clone already
holds.

```sh
dune build ./nopales/bin/nopales.exe

nopales -e '(+ 1 2)'                 # 3
nopales chords.pp arg1 arg2          # script; (argv) => ("arg1" "arg2")
nopales --lens ../pricklypear/lens -e '(count (rows (quote todo) 100))'
nopales                              # REPL on a tty
cat test.pp | nopales                # script from piped stdin
nopales - < test.pp
```

A script:

```lisp
(load "theory.pp")                   ; resolved relative to this file

(define (triad root)
  "Major triad."
  (list root (+ root 4) (+ root 7)))

(println (triad 60))                 ; (60 64 67)
(println (count (argv)))
(exit 0)
```

## What is installed

| Surface | Prims |
|---|---|
| Pure catalog | arithmetic, math, strings, lists, JSON/dicts, HTML, UUID, RNG, date parsing (`pure/prims_pure.ml`) |
| Process | `print`, `println`, `fs/read`, `fs/write`, `fs/exists?`, `argv`, `exit`, `load` |
| Lens (with `--lens`) | `get`, `tree/get`, `rows` over the cloned local data |
| Stdlib seed | `libs/std/lib.pp` — core Nopales has no `let*`/`when`/`unless`; they are `defmacro`s there |

`print`/`println` join arguments with a space and print strings raw
(no quotes). Both return `()`. `-e` prints the last value unless it is
`()`, a function, or a macro; `--quiet` suppresses it entirely.

`load` resolves relative paths against the loading file's directory,
detects include cycles, and shares the caller's step budget. Top-level
`(package …)` metadata in a package file is skipped rather than
evaluated; every other form runs.

Without `--lens`, DB, identity, route, and branch prims (`get`, `rows`,
`whoami`, `route`, `with-transaction`, …) are intentionally unbound: a
function that calls them fails at call time with `unbound: …`. With
`--lens`, the three path/index reads below are bound to the local lens;
writes and the remaining habitat prims stay unbound. The image remains
the full host.

## Local data (lens)

`--lens DIR` (or `NOPALES_LENS` / `PP_LENS`) points at a lens
materialized by `pp-local pull-data user` + `sync`. The host reads it
directly, offline:

| Form | Behavior |
|---|---|
| `(get 'type id)` | body JSON of `data.u.<owner>.<type>.<id>`, or `()` |
| `(tree/get path)` | full envelope JSON at `path`, or `()` |
| `(rows 'type [limit] [sort])` | scan of `data/u/<owner>/<type>/*.json`, default limit 100 |

`rows` returns the image's shape — a list of `(id fields-json)` pairs —
with the **row** id taken from the datum path label (dashed uuid). The
envelope's `id` field is a tree-node uuid and is ignored. `sort` accepts
`created_at`, `updated_at`, or `id`, optionally followed by `ASC` or
`DESC`; the default is `created_at ASC`. Sort is lexical on the
stored timestamp string; server envelopes use a fixed-width format, so
that matches chronological order.

The lens format itself lives in `lenslib/` (shared with pp-remote).
The owner is inferred from the lens's single `data/u/<label>` directory;
`--lens-owner NAME` overrides it when the lens has no data rows or the
directory is ambiguous. Reads are working-set/path-addressed only:
there is no local index beyond the type directory scan, no write path,
and no network. `data.s.<space>` rows are not exposed by `get`/`rows`
(same as pp-remote's lens-first `get`).

## Stdlib discovery

Unless `--no-std` is passed, the CLI loads the stdlib seed from:

1. `--std PATH`
2. `NOPALES_STD`
3. `<exe-dir>/../share/pricklypear-nopales/std.pp` (installed)
4. `libs/std/lib.pp` walking up from the cwd / exe dir (source tree)

If no seed is found, `let*`/`when`/`unless` are unavailable and the CLI
says so on stderr.

## Errors and exit codes

Load-time failures are reported against the source:

```
chords.pp:12:3: unbound: songs/parse-chord in (define chart …)
```

Exit status: `0` success, `1` eval/read/file failure, `2` CLI misuse,
and `(exit N)` → `N`.

## Tests

```sh
dune runtest ./nopales
```

The suite covers form ordering, deploy-form skipping, stdlib macros,
argv, print capture, fs prims, relative `load`, cycle detection, exit,
error positions, and lens reads (get/tree-get/rows, sort, limit, owner
inference).
