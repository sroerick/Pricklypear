# nopales-check

Static checking for a language with no compile phase.

Nopales definitions are read, stored, and hot-edited. Nothing checks a
definition against its callers until a call happens to run, so arity, unbound
names and unknown callables surface as runtime failures. This tool walks the
corpus with the interpreter's own reader, **expands the corpus's macros**, and
reports those classes statically — plus perflint's R1 rule (DB prim inside a
per-row loop), folded in as one severity class, and a capability pass that
classifies every define the way the change-set gates need (`--effects`).

```
eval $(opam env --switch=poohstack --set-switch)
dune build ./nopales-check/bin/nopales_check.exe
./_build/default/nopales-check/bin/nopales_check.exe            # libs/, recursive
./_build/default/nopales-check/bin/nopales_check.exe --json
./_build/default/nopales-check/bin/nopales_check.exe --strict   # warnings fail too
./_build/default/nopales-check/bin/nopales_check.exe --no-perf  # without R1
./_build/default/nopales-check/bin/nopales_check.exe --no-unbound-bare
./_build/default/nopales-check/bin/nopales_check.exe --effects  # capability pass
./_build/default/nopales-check/bin/nopales_check.exe --txn      # advisory txn hints
./_build/default/nopales-check/bin/nopales_check.exe --save-baseline .nopales-check.json
./_build/default/nopales-check/bin/nopales_check.exe --baseline .nopales-check.json
```

Exit: `0` clean, `1` any ERROR (or any WARN with `--strict`), `2` nothing
readable.

## What it checks

| severity | code | meaning |
|---|---|---|
| ERROR | `reader` | the file does not parse |
| ERROR | `arity` | a call to a known define with the wrong number of arguments |
| ERROR | `unbound` | a package-qualified name (`a/b`) that no definition binds |
| ERROR | `perf-r1` | perflint RED: a DB prim inside a per-row loop lambda |
| WARN | `unknown-callable` | a qualified call that is not a define, special form or known prim |
| WARN | `unbound` | a bare name the expanded body never binds |
| WARN | `macro-expand` | a form's macro expansion failed; the unexpanded form was walked |
| WARN | `perf-r1` | perflint AMBER: the prim is in a helper the loop calls |

Calls point at the exact source line. A bare name reports its enclosing
**define's** line instead, because the printed form `x` is not unique in a file
and a wrong line is worse than a coarse one; the message names the define.

## What it found on the corpus (2026-11-22)

77 files, 3,442 defines: **9 arity errors, 21 R1 RED, 8 unbound names**, 14
genuinely unknown host names, 39 warnings total (22 with `--no-perf`).

### Arity (9 at 8 defects)

| call site | the call |
|---|---|
| `libs/book/lib.pp:210` | `book/collect` with 2; takes 1 |
| `libs/calendar/lib.pp:524` | `calendar/day-cell-str` with 6; takes 7 |
| `libs/filmstrip/lib.pp:1721` | `filmstrip/close-heal` with 2; takes 1 |
| `libs/food/lib.pp:761` | `html/p` with 2; takes 1 (`html/p*` is the attrs form) |
| `libs/songs/lib.pp:1185` | `songs/perform-section` with 3; takes 2 |
| `libs/standup/lib.pp:105` | `chat/history` with 1; takes 2 |
| `libs/suno/lib.pp:77`, `:686` | `dict-set*` with 3; takes 2 (`json`, flat `kvs`) |
| `libs/todo/lib.pp:5405` | `todo/ui/assignee-option` with 2; takes 3 |

### Unbound names (8, two classes)

The interesting half. Macro expansion is what makes these visible: without it,
`let*` and `when` hide every binding they introduce.

**`let` used as if it were `let*`** — a later binding's value reads an earlier
name, but Nopales `let` evaluates every value in the *outer* scope:

| define | name |
|---|---|
| `chat/dm-messages-html` | `rid`, read by the `xs` binding |
| `doasq/path-for` | `plen`, read by the `hit` binding |
| `notes/doc/upsert-in` | `row`, read by the `fields` binding |
| `wiki/apply-entity` | `s`, read by the `t` binding |

**Genuinely unbound**: `todo/item/save` and `todo/save` both call
`todo/state/refuse` with `cur`, but their bindings are `prev`/`next`/`lid` (the
name was renamed and two call sites were missed); `todo/done?` and
`workout/ksk1-replan` use the symbols `false`/`true`, which the reader does not
turn into booleans (`#f`/`#t`) and nothing defines.

The 14 remaining boundary warnings are names no layer classifies: `glochid/tg`,
`plot/*` (14 symbols) and `sexp/parse`/`sexp/extract`. The interpreter's own
effect table (`lisp/lib/effect.ml`) already resolves `tree/*`, `soup/*`,
`route/dispatch` and the rest of the image catalogue, and the checker now
consumes that table instead of keeping a second, smaller prim list.

## Macro expansion

`macroexpand_expr` is the interpreter's own expander, so the checker does not
reimplement macro semantics:

1. every corpus `defmacro` is evaluated into one `env` (the pure prim catalogue
   is installed and bound as `VPrim`, and evaluation runs under
   `Control.run_with_rim`);
2. each top-level form is expanded with `Eval.macroexpand_expr` before the walk;
3. positions come from the **unexpanded** tree, matched by printed form, so
   user-written calls keep exact lines and macro-introduced forms take the
   enclosing form's line;
4. an expansion that fails records `macro-expand` and the unexpanded form is
   walked, so one bad macro cannot hide the rest of a file.

## How the walk works, and its limits

- The walker models `let`, `let*`, `letrec`, `define`, `lambda`, `defmacro`,
  `cond`, `guard` and quote structurally; everything else in
  `Eval.special_form_names` is walked as expressions.
- A list is not always a call: an unknown head may be a host prim or a macro
  whose arguments are syntax, so it is not reported and its arguments are not
  walked. Only qualified names are claims about the corpus.
- Not yet: the *budget* half of the effect/budget pass (needs
  `nopales-budget`), a per-file severity policy, and exact positions for
  macro-introduced code.

## Adopting it on a corpus that already fails

The checker's own corpus already has 30 findings, so a hard gate would have
been impossible to turn on. `--save-baseline FILE` records the current finding
multiset, and `--baseline FILE` suppresses exactly those occurrences and exits
on the rest:

```
nopales-check --save-baseline .nopales-check.json   # once, at adoption
nopales-check --baseline .nopales-check.json        # in CI / before promote
```

The key is `(file, code, name, message)`, not the line, so moving a definition
does not resurrect it; it is a *multiset*, so a second identical call site is a
new finding and a changed arity message never matches. A missing or malformed
baseline exits 2 rather than suppressing silently. `--save-baseline` is
maintenance, not a gate: it records and exits 0 even though the recorded
findings remain. Run both from the repo root: the baseline stores the path
spelling it was given.

## Effects

`--effects` runs the interpreter's own inference (`Effect.infer` over the
macro-expanded bodies, with `Effect.known_fn_of_bindings` resolving corpus
calls) and emits one row per define:

| class | meaning |
|---|---|
| `pure` | no capability; deterministic, no live target needed |
| `db` | reads or writes the store (and branch operations) |
| `tx` | opens `with-transaction` (joins `db` when it also writes) |
| `egress` | `net` or `ai`: leaves the process |
| `route` | a publish/route registration |
| `unknown` | a qualified call neither the corpus nor `Effect` classifies |

The union is transitive: a wrapper that calls a DB reader is `db`. `Html` is
not a gate class (it only builds a string), so a renderer stays `pure` for gate
selection. `unknown` is the fail-closed class the factory reads as db-grade; the
row also names the unresolved symbols. `route` comes from top-level
`(route …)` forms, which are not defines, so the change-set layer adds it from
the semantic diff.

A row marked `ambiguous` means the same name is defined in more than one file
(163 such names in this corpus); union the rows before gating.

`--txn` adds the interpreter's advisory transactional hints (two or more write
sites, or `select-for-update`, outside `with-transaction`). They are opt-in:
the inference is "soft", and a writer may open the transaction in its caller,
which a per-define static pass cannot see. 41 hints on this corpus.

JSON gains an `effects` array only when `--effects` is passed:
`{name, file, line, classes, effects, unknown, ambiguous}`.

## Tests

```
dune runtest --force ./nopales-check
```

`test/fixtures` carries one case per class: arity, qualified unbound, unresolved
call, bare unbound, a clean file that exercises `let*`, `cond`, `guard` and a
`defmacro`, and `effects.pp` (pure, db, transitive db, tx, and the txn hint).
The baseline test writes a baseline, suppresses a finding, proves a second
identical call site is not suppressed, and proves a changed arity is new.
`macro.pp` proves expansion: its macro introduces a `let` binding, and the
walker must not report the bound name. The arity case pins the call line; the
bare case pins the define line.
