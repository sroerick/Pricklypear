# nopales — the standalone script runner

`nopales` runs Nopales as a plain programming language: the pure
interpreter (`lisp/`) plus the pure prim catalog (`pure/`) and a small
process host. No image, no Postgres, no network, no auth.

```sh
dune build ./nopales/bin/nopales.exe

nopales -e '(+ 1 2)'                 # 3
nopales chords.pp arg1 arg2          # script; (argv) => ("arg1" "arg2")
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
| Stdlib seed | `libs/std/lib.pp` — core Nopales has no `let*`/`when`/`unless`; they are `defmacro`s there |

`print`/`println` join arguments with a space and print strings raw
(no quotes). Both return `()`. `-e` prints the last value unless it is
`()`, a function, or a macro; `--quiet` suppresses it entirely.

`load` resolves relative paths against the loading file's directory,
detects include cycles, and shares the caller's step budget. Top-level
`(package …)` metadata in a package file is skipped rather than
evaluated; every other form runs.

DB, identity, route, and branch prims (`get`, `rows`, `whoami`,
`route`, `with-transaction`, …) are intentionally unbound: a function
that calls them fails at call time with `unbound: …`. The habitat image
remains the full host.

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
and error positions.
