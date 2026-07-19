# Notes UI contract

Acceptance scenarios for the outline notes editor. Agents implement until
`scripts/smoke_notes_model.py` and `scripts/smoke_notes_ui.sh` pass.

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

Filled by `scripts/inventory_notes_ui.py` (and human notes). See
`docs/notes-ui-inventory.md` (generated/updated by inventory run).

## Agent rules

1. Fix **behavior** until model + UI smokes green before CSS polish.
2. One control / scenario per Ralph iteration when repairing.
3. Prefer git `libs/notes` + `libs/outline`; reseed or load-library on main.
4. Screenshots only for layout; clicks/ops for correctness.

General agent FE verify research (a11y snapshots, tiered checks, htmx
gotchas): [`agent-ui-harness.md`](agent-ui-harness.md).
