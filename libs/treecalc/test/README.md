# treecalc test vectors

Acceptance vectors for the `treecalc` package (PP tree-calculus interpreter,
spec `~/swarm-demo/treecalc/PLAN.md`). Every reduction vector was
cross-checked against Johannes Bader's reference TS evaluator (vendored
read-only under tuna-fix/reference/tree-calculus); the oracle harness and
its outputs are recorded in the claim receipts.

## Run

Needs a PP instance with the lib loaded:

    export PP_URL=http://127.0.0.1:18081 PP_USER=admin PP_PASS=admin
    # boot: PP_LIBS_DIR=<repo>/libs; then (load-library "treecalc")
    ksh libs/treecalc/test/vectors.ksh

Exit 0 = all green.

## Coverage

- S1: representation, rule (1) incl. extra-argument spines, ternary
  round-trips, identity self-eval (0 steps).
- S2: rule (2), triage rules (3a/3b/3c) with distinct markers, SKK = I,
  K a b -> a, is-zero via triage{true, K(K I), K(K (K I))}.
- S3: tc/step rule field, tc/trace single-step cap, omega
  (S I I)(S I I) out-of-steps EXACTLY at tc/eval's limit (default 4000,
  explicit 300), minbin strong round-trips (reference vectors
  '1' / '001010111' / '00111').
- S4: lambda terms compiled by the reference abstraction-elimination
  (bracket_ski) evaluated under tc/eval: compiled K behaves as K,
  compiled I self-application terminates, compiled (λx. x x)
  self-application diverges exactly at the budget.

Budget note: on the stock 400k-step PP build the canonical size program
(25096 TC steps, oracle count) exceeds ANY tc/eval limit (measured ~84
PP steps per TC step on rule-2-heavy work); it completes only on a build
with >= ~2.2M budget. Its oracle output is in the S3 claim receipt.
