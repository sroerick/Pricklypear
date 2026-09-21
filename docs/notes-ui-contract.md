# Notes UI contract

Acceptance scenarios for the outline notes editor. Agents implement until
`scripts/smoke_notes_model.py`, `scripts/smoke_notes_ui.sh`, and
`scripts/pp_outline.py check` pass. `pp_outline.py scripts-check` guards that
every `<script>` the editor emits still parses (`node --check`) - the selection
toolbar goes inert the moment one dies at parse time.

## Tiered loop

| Tier | Tool | Question answered |
|------|------|-------------------|
| 0 model | `pp_outline.py fixture/op/tree` | does the op transform the tree correctly? (no HTTP) |
| 1 fragment | `pp_outline.py check` | partial HTML round-trips and every emitted script parses? |
| 2 browser | `smoke_notes_selection_cdp.mjs`, plus `smoke_notes_*.py/sh` | do delegated click/focus handlers seat #ol-selected-id on the clicked node so toolbar acts on it, not root? |

Default to tier 0/1 (sub-second, no browser). Escalate to tier 2 only for the
genuinely client-only path - delegated handlers and toolbar dispatch. Never
drive clicks from pixels.

## Fixture

- Slug: **`ui-fixture`**
- Title: `UI Fixture`
- Seeded by smokes via `(notes/doc/upsert …)` on **system/main** (or after
  `(load-library "outline")` + `(load-library "notes")` so pins match git).

**Personal-branch warning:** if `username/personal` pins old `outline/*` or
`notes/*`, UI will not match git. Smokes checkout `system/main` or reload
libs before asserting.

## Scenarios

| ID | Scenario | Pass criteria |
|----|----------|----------------|
| N1 | Index lists notes | GET `/notes` 200; create form present |
| N2 | Open fixture | GET `/notes?slug=ui-fixture` 200; `#outline` present; root `data-id=root` |
| N3 | add-child (server) | POST node-op `add-child` on root → kid appears; model kids ≥ 1 |
| N4 | add-sibling | POST `add-sibling` on a kid → sibling exists |
| N5 | cycle-todo | POST `cycle-todo` → node todo non-empty / class `st-TODO` etc. |
| N6 | move-up / move-down | order of two kids changes in model |
| N7 | archive / unarchive | todo becomes ARCHIVED then cleared path works |
| N8 | delete | POST `delete` removes node from model |
| N9 | Toolbar present (desktop) | `.ol-toolbar` in DOM |
| N10 | Mobile actions present | `.ol-actions` / `···` in DOM |
| N11 | Capture | POST capture creates/updates inbox (model or status) |
| N12 | Visual (optional) | desktop + mobile screenshots under `/tmp/pp-notes-ui/` |

## Broken-control inventory

Filled by `archive/scripts/inventory_notes_ui.py` (and human notes); the
inventory page itself is a generated snapshot, not tracked.

## Agent rules

1. Fix **behavior** until model + UI smokes green before CSS polish.
2. One control / scenario per Ralph iteration when repairing.
3. Prefer git `libs/notes` + `libs/outline`; reseed or load-library on main.
4. Screenshots only for layout; clicks/ops for correctness.
5. **Scripts must parse.** The editor's interactive toolbar dead-snaps the
   instant any emitted `<script>` throws at parse time (handlers never
   attach, `#ol-selected-id` stays `"root"`, every `data-ol-op` button is
   inert). After editing any `*-script` define, run `pp_outline.py
   scripts-check`; if it reports BAD, the page is broken even if the model
   smoke passes.

General agent FE verify research (a11y snapshots, tiered checks, htmx
gotchas): [`agent-ui-harness.md`](agent-ui-harness.md).
