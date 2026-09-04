# filmstrip v1 — the habitat shell (handoff)

Status: **make-phase handoff** for an implementation agent. Spec authority:
`borge/filmstrip.borg` (the shell chapter). Vocabulary naming follows
`borge/pads.borg`, but **pads is NOT consumed this pass** — filmstrip v1 is
built stringly, exactly like every existing package. **If code and spec
disagree, the code is wrong**; if the spec must change mid-make, file a borge
issue and stop that item.

Victory: the habitat becomes a windowed desktop. `GET /filmstrip` shows the
user's sketch — a Miller-column **cascade strip** over a **content strip** of
panes (todo, notes, calendar), niri-style, keyboard-navigable, and the whole
arrangement persists because it is a row in Postgres.

---

## 0. Orientation (no prior context assumed)

Pricklypear is a multi-user Lisp OS: **one OCaml image** (`image/`) + **Postgres**;
the language is **Nopales** (Scheme-family Lisp-1, interpreter in `lisp/` —
`defmacro`/quasiquote/`&rest` available). Apps are Nopales packages in
`libs/<pkg>/lib.pp` seeded to `system/main`; routes live in `view_bindings`
rows (route → function name; handlers return HTML strings via `libs/html`).
Key existing machinery:

- **Route dispatch**: `route/lookup` + `route/dispatch` prims. With NO
  `HX-Request` header a handler returns the full page; WITH it, the fragment.
  The in-process dispatch pattern to copy is `libs/hyper/lib.pp`
  (`hyper/visit`).
- **Auth/session**: HTTP gate in the image; identity via the session
  registry (each package resolves its user — copy their pattern). Route
  visibility: `migrations/006_route_visibility.sql` — shell routes are
  **private**.
- **Data storage**: `data_rows` via the data prims — study `libs/todo/state`
  and `libs/notes` for the established row idioms before inventing any.

House conventions:

- OCaml 5.4.x on poohstack: `eval $(opam env --switch=poohstack --set-switch)`;
  `dune build @all`, `dune runtest --force` (alcotest).
- Dev image: `scripts/dev.sh start` (PG :5433, image :18080). Bootstrap admin
  `admin`/`admin`. Shell eval: `scripts/pp-eval.sh` (needs `PP_PASS=admin`).
- Package sync: `scripts/pp_sync.py push <pkg>`.
- Nopales style: `(define f (lambda (x) "docstring" body))` — docstring after
  params, before body. Read `libs/html/lib.pp` before writing markup.
- Smokes: `scripts/smoke_*.py` (see `scripts/smoke_pads_hello.py` for the
  newest pattern, and `scripts/smoke_data_browser.py`).
- Book lint: `borge balance <file>`, `borge check .` — pre-existing failures
  in `borge/music.borg` and `borge/formal-verification.borg` are NOT yours.

## 1. Victory condition

**V1 — shell.** Logged-in `GET /filmstrip` renders the two-strip layout:
cascade strip (short, ~30vh, overflow-x) over content strip (grow,
overflow-x). Empty state is valid (no panes yet, launcher visible).

**V2 — cascade.** The top strip renders the crumb path as Miller columns
using `pkg/browse`. Implemented for **todo, notes, calendar**. Clicking an
entry at depth d truncates the path to d, appends the choice, and swaps the
tail columns; a **leaf** opens-or-focuses its content pane below.

**V3 — interaction.** Launcher opens a pane (route + width preset) → appended
to the strip via `hx-swap="beforeend"`. Close removes it. Alt+arrows move
`.focused` between panes via htmx key filters POSTing to
`/filmstrip/focus/<dir>` — server owns focus, answers with OOB class swaps.
Width cycles through presets `third`/`half`/`full` (`/filmstrip/width/<id>`).
Interactions INSIDE a pane (todo add, notes edit) swap in-place — panes don't
know they're windowed, no full-page reloads.

**V4 — persistence.** Reload `/filmstrip`: same panes, order, widths, focus,
cascade path. The workspace row round-trips.

## 2. Architecture

```
browser ── GET /filmstrip ──▶ shell handler
                                │ reads workspace row (data_rows)
                                │ renders cascade columns (browse path)
                                │ renders pane sections (in-process partial
                                │   dispatch of each pane's route)
                                └─ htmx on body: key filters → /filmstrip/*
POST /filmstrip/open|close|focus|width ──▶ mutate row ──▶ OOB/fragment swaps
```

One package: `libs/filmstrip/lib.pp`. No changes under `image/`, `lisp/`,
`pure/`. The workspace row is data; the shell is a route; panes are existing
routes dispatched in partial mode.

## 3. Work items

**W1 — workspace row.** Per-user row: `{cascade: [path entries],
content: [{id, route, width}], focus: [strip, index]}`. Widths are the three
presets only — never pixels (niri set-column-width discipline). Stable pane
ids (follow the id discipline used by todo/notes rows). Ephemeral state
(scroll position, hover) is NEVER in the row and never rendered from.

**W2 — shell route + CSS.** `/filmstrip` handler + `filmstrip/css` following
the `css/sheet`/`theme` idiom (~50 lines): flex strips, pane sections,
`.focused` ring, preset width classes. Launcher = a simple inline form row at
the head of the content strip (text input for a route + open button). NO
floats, NO modals — dialogs are a pads-float concern for a later pass.

**W3 — pane dispatch.** Open = append record, then in-process dispatch of the
pane's route with the `HX-Request` header on (the `hyper/visit` seam — reuse
`route/lookup`/`route/dispatch`), fragment goes `beforeend` into the strip.
Close/width = row mutation + minimal OOB swaps. Focus = row mutation + OOB
class swap on exactly two panes (old, new).

**W4 — cascade protocol.** `(pkg/browse path) -> (columns . leaf?)` where
columns is a list of column values (each a list of `(label . target)`
entries) and leaf? says whether the deepest target opens a pane. Implement
`browse` for todo, notes, calendar using their existing data (lists → items;
outline headings; year → month → day — match whatever granularity their
routes actually serve; do not invent routes). Entry click semantics per V2.
Global cascade v1 — one crumb path per user, NOT per-pane (recorded decision).

**W5 — hotkeys + smoke.** htmx key filters on the shell body: alt+left/right
focus, alt+shift+left/right move pane order (optional), a key opens the
launcher input focus. Prefer ZERO javascript files; if scroll-into-view
smoothness genuinely needs ~15 lines, quarantine in one static file and
justify in the runbook. `scripts/smoke_filmstrip.py`: boot check → open pane →
assert pane fragment present → focus POST → assert `.focused` moved (OOB) →
cascade walk → truncation → reload round-trip (fresh GET shows same row
state). Follow `smoke_pads_hello.py` structure.

**W6 — book hygiene + runbook.** Flip the IMPLEMENTED subsection statuses in
`borge/filmstrip.borg` (workspace-model, cascade-protocol, pane-protocol,
shell-events → `implemented`; leave roadmap-tiers `planned`), add a dated
agent note, `borge balance` + `borge check .` (no NEW failures), and append a
runbook section to THIS file (seeding steps: pp_sync push, image restart,
view_bindings rows; how to demo).

## 4. Decisions already made (do not relitigate)

- **D1** Global cascade v1 (one crumb path per user). Per-pane cascades are a
  recorded later option.
- **D2** Widths are presets (`third`/`half`/`full`), never pixels.
- **D3** Zero-JS. htmx key filters + OOB swaps; optional quarantined
  scroll.js only with runbook justification.
- **D4** Panes are partial-mode dispatches of existing routes; no pane-local
  state beyond the workspace row.
- **D5** No floats/modals/notifications in v1 (pads float band later).
- **D6** Stringly rendering (libs/html), NOT pads — pads integration is the
  next pass, this pass is the product.

## 5. Out of scope (will look adjacent; do not chase)

Native shim / Wayland (stage 2 consumes this workspace value), pads
consumption or twin-pane verify, ANSI renderer, notifications as cascade
cards, per-pane cascades, mobile CSS, theming beyond `theme/vars` defaults,
multi-user seats (each user's row is theirs; nothing cross-user).

## 6. Acceptance checklist

- [ ] `dune build @all` clean; `dune runtest --force` no NEW failures
- [ ] `scripts/dev.sh start` → logged-in `/filmstrip` renders two strips (empty state ok)
- [ ] Launcher opens todo → pane appears with the todo fragment; in-pane interactions swap in-place
- [ ] Cascade: walk todo → list → leaf opens/focuses pane; mid-path click truncates + re-walks
- [ ] Focus POST moves `.focused` via OOB; alt+arrows wired via key filters
- [ ] Width preset cycles on a pane
- [ ] Reload round-trips panes/order/widths/focus/cascade
- [ ] `scripts/smoke_filmstrip.py` green
- [ ] `borge/filmstrip.borg` shipped subsections flipped; `borge check .` no NEW failures
- [ ] All work committed on `filmstrip-v1`
