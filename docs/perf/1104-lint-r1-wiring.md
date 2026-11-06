# Wiring R1 into the gate (note, not the wiring)

The checker exists and is green on `factory/guard-r1-1105`; nothing calls it
yet. This is the shape the call should take.

## The command

```
dune build perflint/bin/perf_lint_main.exe
_build/default/perflint/bin/perf_lint_main.exe libs          # humans
_build/default/perflint/bin/perf_lint_main.exe --json libs   # machines
```

Exit codes: `0` no RED, `1` at least one RED, `2` unreadable or unparsable
input. Gate on non-zero; treat `2` as a tool failure, not a finding. `--json`
emits `{"findings":[...],"summary":{"files","findings","red","amber"}}` with
`file`, `line`, `col`, `severity`, `define`, `prim`, `loop`, `via` per finding.

## Merge gate

One CI step after build, before merge: run the binary on `libs`, fail the job on
exit 1, keep the log. RED blocks, AMBER prints. Do this only after the 18
current REDs are triaged (docs/perf/1104-lint-r1-findings.md); today the gate
would be red on day one, which is why this is a note and not the wiring.

## erde lint role

`/home/al/erde-ocaml/loop-lint.sh` runs the lint lane. Add one lane step:

1. build `perf_lint_main.exe` with the repo's poohstack build line;
2. run it `--json` over `libs`;
3. for every finding with `"severity":"RED"`, open one PP card, kind BUG, the
   same per-finding shape the arity scan used (cards `d9f7d2b` / `551d5fc`):
   title `perf-r1: <prim> in <loop> at <define>`, body containing
   `file:line:col`, the prim, the loop, `via` when present, and the R1 line;
   put a stable dedupe marker in the body, e.g. `perf-r1:<file>:<line>:<prim>`,
   so a re-run updates rather than duplicates;
4. AMBER findings stay advisory: print them in the lane log, no card;
5. alerting rides the existing loop card path (ntfy), no new channel.

Stale cards should leave the queue the way the arity scan's did (stale +
exhausted), so a fixed finding stops nagging.
