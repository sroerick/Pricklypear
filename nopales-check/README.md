# nopales-check

Static checking for a language with no compile phase.

Nopales definitions are read, stored, and hot-edited. Nothing checks a
definition against its callers until a call happens to run, so arity, unbound
names and unknown callables surface as runtime failures. This tool walks the
corpus with the interpreter's own reader, **expands the corpus's macros**, and
reports those classes statically — plus perflint's R1 rule (DB prim inside a
per-row loop), folded in as one severity class.

```
eval $(opam env --switch=poohstack --set-switch)
dune build ./nopales-check/bin/nopales_check.exe
./_build/default/nopales-check/bin/nopales_check.exe            # libs/, recursive
./_build/default/nopales-check/bin/nopales_check.exe --json
./_build/default/nopales-check/bin/nopales_check.exe --strict   # warnings fail too
./_build/default/nopales-check/bin/nopales_check.exe --no-perf  # without R1
./_build/default/nopales-check/bin/nopales_check.exe --no-unbound-bare
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

77 files, 3,442 defines: **9 arity errors, 21 R1 RED, 8 unbound names**, 65
host-boundary warnings.

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

The 65 warnings are the host boundary: qualified host prims (`tree/subtree`,
`soup/parse`, `route/dispatch`, `glochid/tg`, `plot/*`) with no static
catalogue, deduped one per file.

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
- Not yet: the effect / budget pass (needs `nopales-budget`), a per-file
  severity policy, and exact positions for macro-introduced code.

## Tests

```
dune runtest --force ./nopales-check
```

`test/fixtures` carries one case per class: arity, qualified unbound, unresolved
call, bare unbound, and a clean file that exercises `let*`, `cond`, `guard` and
a `defmacro`. `macro.pp` proves expansion: its macro introduces a `let` binding,
and the walker must not report the bound name. The arity case pins the call
line; the bare case pins the define line.
