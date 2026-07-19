# Formal Verification — Working Plan

This is a **plan artifact**, not book content. The declarative chapter
(goals, status rungs, property registry) is `borge/formal-verification.borg`.
Everything here is sequencing, tooling, and done-criteria for the make side.

## Verification islands (priority order)

- **Island A — Pure Lisp kernel (first).** `lisp/`: effect.ml, ast.ml,
  reader.ml, eval.ml, env.ml. Already pure, no Postgres/HTTP, small, high
  leverage under every product path. Starter properties: `effect.*`,
  `reader.*`, `budget.*`.
- **Island B — Auth password/session core (second).**
  `image/lib/auth_password.ml` (+ pure helpers). Keep HTTP handlers out of
  the proof boundary.
- **Island C — Branch visibility model (third).** Extract a pure model of
  ancestry + binding visibility + merge conflict detection; store/Postgres
  tests use the model as an oracle. Do not attempt live SQL proofs.
- **Island D — Deferred.** http routing tables, transaction dynamic binding,
  package seed purity, Solo5/Mirage hardening. Not v1 scope.

Boundary rule: prefer pure functions with explicit results/errors. I/O
wrappers call pure decision functions. Generators and proofs attach to the
pure side only.

## Tooling

v1 (normative for the foothold):
- QCheck for property generation; qcheck-alcotest to wire into the existing
  alcotest runners.
- `dune runtest` in `lisp/` remains service-free.
- Wall-clock timeouts around budget suites so a missed tick cannot hang CI.

Later (not required to start):
- Gospel + Cameleer, or Why3 + Alt-Ergo/Z3, for Island A contracts once
  QCheck is boring.
- Coq/Rocq only for tiny kernels after properties stabilize.

Suggested layout:
```
lisp/test/gen.ml                shared generators (eff, expr, pure programs)
lisp/test/properties_effect.ml  Island A effect lattice properties
lisp/test/properties_reader.ml  AST JSON + surface round-trips
lisp/test/properties_budget.ml  budget termination / monotonicity
```

Deps: add `qcheck` and `qcheck-alcotest` on the poohstack switch for lisp
tests only. Do not pull proof tools into the image runtime closure.

Equality oracles (locked defaults):
- `EffSet.t` → `EffSet.to_list` structural equality
- `expr` → `Reader.pp_expr` string equality until a dedicated `expr_equal`
- values → `Reader.pp_value` for pure literals; never structural equality
  on closures
- budget → outcome tag only: `Value | Budget | Error`

## Passes

**Pass A — QCheck laws for EffSet / policy** (`lisp/lib/effect.ml`)
- Generators: `gen_eff` (oneof all eff tags incl. Pure), `gen_eff_list`
  (list_size 0..6), `gen_set` (map `EffSet.normalize`).
- Implement the eight `effect.*` lattice properties.
- Wire via qcheck-alcotest; keep existing `effect_test.ml` examples.
- Any shrunk counterexample becomes a named alcotest case.
- Done when: `dune runtest` in lisp/ is green and the eight properties are
  named in test output.

**Pass B/C — AST JSON then surface round-trips** (`ast.ml`, `reader.ml`,
`eval.ml`)
- Generate ASTs, not arbitrary strings. Depth-bound. Start without macros,
  quasiquote, gensym, floats, with-transaction; add constructors
  incrementally when green.
- Order: `reader.ast-json-roundtrip` → `reader.surface-roundtrip` →
  `reader.quote-reify-roundtrip`.
- Equality: `Reader.pp_expr` unless `expr_equal` is added. Custom shrinkers
  that delete subexprs are encouraged once failures get noisy.

**Pass D — budget termination and monotonicity** (`lisp/lib/eval.ml`)
- Harness: pure prim table only; run eval under `Lwt_main.run`; catch
  `Interp_error` and classify Budget vs other; suite timeout.
- Properties: `budget.finite-outcome`, `budget.monotonic-success`,
  `budget.divergence-trips`.
- Do not start until effect + JSON round-trip generators are trustworthy.

**Pass E — infer Pure implies no non-pure prim dispatch**
- Prim table where each prim records its name into a trace cell, then
  delegates. Generate terms `Effect.infer` marks Pure; eval; assert
  trace ⊆ pure_prims ∪ special forms.
- First deep semantic property; best candidate to promote toward
  Gospel/Why3 later.

**Proofs (later).** Only after Passes A–E are boring: Gospel specs adjacent
to effect.ml / pure helpers; Cameleer or Why3 discharge lattice laws and
pure-sound fragments; Coq/Rocq only if a kernel is both critical and tiny.
Proof work must not block product Lisp packages; the runtime image must not
depend on proof tooling.

**Auth + branch islands (second/third waves).** Auth: properties on
`auth_password` and pure session helpers only; HTTP login handlers stay
example/smoke tested. Branch: extract pure visibility/merge model, generate
small DAGs, oracle-compare model vs store.

## Borge workflow integration

- Plan: name properties in the registry before writing tests when possible.
- Make: implement generators + QCheck suites. If a property needs
  rewording, file `borge issue new` rather than editing the chapter
  mid-make.
- Review: `dune runtest` green for touched packages; counterexamples
  promoted to named regressions.
- Commit: one property cluster per PR when practical; do not mark
  `verified` until the target rung is honestly green.
- Cross-chapter: `lisp-interpreter.borg` owns language semantics; the FV
  chapter owns how those semantics are checked.

## Success criteria (v1 foothold)

Complete when:
1. qcheck wired for lisp tests
2. effect lattice properties green (Pass A)
3. AST JSON round-trip property green (Pass B)
4. budget finite-outcome + divergence-trips green (Pass D)
5. chapter subsections move planned → implemented (verified where rung-1
   holds)
6. docs/agent-index mention how to run the property suite

Explicitly not required: Gospel/Why3 proofs; auth/branch islands; surface
syntax perfection for all printers; zero flaky random seeds (pin counts,
re-seed on fail).
