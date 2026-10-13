# Spec: workout-ksf-cjc — the first program-as-sub-module

**Goal:** prove the "each workout program is its own software program" vision
with one complete instance. The agenda page answers roerick's login question:
*what's my current program, what's next, how am I doing?*

## Shape

- **Package:** `workout-ksf-cjc` (separate lib file, `libs/workout-ksf-cjc/lib.pp`)
- **Defines:** `agenda` (the page), plus small private helpers
- **Names read nested:** called as `workout-ksf-cjc/agenda`
- **Composes with core:** calls `workout/sessions-sorted`, `workout/session-*`,
  `workout/chart-*` from the parent lib — no new schema, no data duplication
- **Route:** lib route form, top-level: `(route "/workout/cjc" "workout-ksf-cjc/agenda" "private")`
  (top-level lib routes register via the seed — verified 2026-10-07)

## The agenda page

1. **Current program:** KSF — CJC block, 20kg (from an active-program datum;
   hardcode-then-extract is acceptable for phase 1)
2. **What's next:** today's scheduled session or "next session" from history
3. **Recent progress:** last ~8 sessions as a compact list + the chart
   (reuse `workout/chart-data-js` / `chart-page` pieces)
4. **Log entry point:** link to the run page / quick log

## Tests — the first render-assert in the house

UIs here are s-expressions; test the data. In the repl (lab first, prod after):

- `(workout-ksf-cjc/agenda)` returns a struct containing the program name,
  a 60/60 reference (or current-target line), and a chart node
- The page contains a link to the log flow
- Lab boot: seed loads the package, route resolves, page returns 200

## Loop

spec (this file) → build in lab → render-assert in repl → commit → push →
hook deploys → fresh-session verify → handback.

## Non-goals (this item)

Session tracking UI (the run page already exists), the extraction of the
monolith's KSF internals (they stay; the sub-module composes), the
active-program framework (emerges after the second program).

## Seed crystal

This item is the template: the second program (whenever roerick starts one)
should be creatable by an agent following this spec's shape without
inventing structure. If the second program needs decisions the first didn't
anticipate, the pattern grows then — not before.
