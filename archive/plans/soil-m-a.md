# soil M-A — filmstrip kiosk (handoff)

Status: **make-phase handoff** for an implementation agent. Spec authority:
`borge/soil.borg` (milestones M-A) + `borge/pads.borg` (scene dialect,
code-normative docstring on `pads/render-scene`). `borge/filmstrip.borg` is
the shell spec. **If code and spec disagree, the code is wrong**; a mid-make
spec gap becomes a `borge issue` and stops that item.

Victory: the filmstrip shell renders on a real Wayland toplevel —
`soil-hello --eval "(filmstrip/scene)"` shows the cascade strip over the
content strip fullscreen, focused pane ringed, widths honored. No river, no
input, no WM policy (those are M-B).

---

## 0. Orientation

`native/soil_hello.ml` (proven on the pads-hello spike) consumes
scene-dialect v0 JSON — `{"version":0,"root":<node>}`, kinds `row|column|
text`, attrs `size` (`fit|grow:N|fixed:N`), `gap`, `padding`, `level` —
through owned two-pass layout + software raster (rect fill + font8x8), one
commit per tick, `--frames N` smoke mode. Scene sources: `--scene FILE` or
loopback eval via `scripts/pp-eval.sh` (thin shim, soil/binding-path (a) —
decision already made and proven).

`libs/filmstrip/lib.pp` owns the workspace row (`filmstrip/row` → cascade /
panes / focus) and the Miller walk (`filmstrip/browse` → columns . leaf?).
It renders HTML stringly today; the native tier needs the same state as a
pad tree.

Dialect fact worth internalizing: `pads/scene-attrs` serializes ANY attrs
alist — display-hint attrs ride the generic channel and backends decide the
paint. That is the pads.borg renderer-dialects doctrine ("pure paint per
backend").

## 1. Victory condition

`python3 scripts/smoke_soil_ma.py` green: scene validation (v0 shape, stable
path ids, size grammar, sel placement follows live workspace mutations) +
`soil_hello --frames 12 --eval "(filmstrip/scene)"` exits 0 with the shell
visible on the running compositor.

## 2. Work items

**W1 — `filmstrip/scene` (Nopales, libs/filmstrip/lib.pp).** Workspace →
pad tree → `pads/render-scene`. Root column (gap 4, padding 8):
cascade row (Miller columns from `filmstrip/browse`, entries as text,
selected entry wears `sel`, capped 12/column with a dim `+N more` tail) →
dim launcher hint line → content row (`grow:1`) of pane columns: title
(level 1; focused also wears `sel`), body text summaries per route prefix
(todo lists with item counts / notes doc titles / calendar today+events /
generic hint), capped like the cascade.

**W2 — niri width mapping.** Presets as grow weights over denominator 6:
third=2, half=3, full=6; a trailing empty-text spacer with the remainder
weight keeps fractions exact (third+half → 2:3 + 1/6 empty). full+others
shrink proportionally (niri would scroll; documented approximation — no
scroll in the shim v1).

**W3 — shim paint (native/soil_hello.ml).** `sel` attr: text → accent
color, box → 2px accent border (4 fill rects). `dim` attr: muted text
color. No layout changes, no input, no new scene kinds.

**W4 — smoke + book.** `scripts/smoke_soil_ma.py` (scene validation over
live workspace mutations + shim run when `WAYLAND_DISPLAY` is set, SKIP
otherwise). Flip `borge/soil.borg` binding-path + native-rendering →
implemented (river-split + milestones stay planned: river is M-B), dated
agent notes in soil.borg + filmstrip.borg, `sel`/`dim` documented in
pads.borg, runbook section appended here, checklist ticked, all committed.

## 3. Decisions already made (do not relitigate)

- **D1** Thin shim (loopback eval) — proven; embedded interpreter stays
  deferred (soil/binding-path).
- **D2** Scene dialect stays v0: no new kinds; `sel`/`dim` are pass-through
  display hints, not dialect version bumps.
- **D3** Kiosk is display-only (soil M-A smoke says "shows", not
  "navigates"); input/policy is M-B.
- **D4** Pane bodies are text summaries from existing prims — the shell is
  the M-A subject, not live pane interiors.
- **D5** `filmstrip/scene` lives in libs/filmstrip/lib.pp and is exported;
  it is a pure data render (no route, eval-only like `hello/scene`).

## 4. Acceptance checklist

- [x] `dune build @all` clean (native/ included); no NEW test failures
- [x] `(filmstrip/scene)` evals to valid v0 scene JSON on the dev image
- [x] Scene follows workspace mutations: open/focus/width/cascade reflected
- [x] `soil_hello --frames 12 --eval "(filmstrip/scene)"` exits 0, shell visible
- [x] `scripts/smoke_soil_ma.py` green (or SKIP on the wayland leg off-desktop)
- [x] `borge check .` no NEW failures; touched `.borg` files balance
- [x] All work committed

## 5. Runbook (seeding + demo)

Build + seed (from the `filmstrip-v1` worktree):

```sh
eval $(opam env --switch=poohstack --set-switch)
dune build @all                    # includes native/soil_hello.exe
scripts/dev.sh start               # PG :5433, image :18080 (if not up)
export PP_URL=http://127.0.0.1:18080 PP_USER=admin PP_PASS=admin
python3 scripts/pp_sync.py push filmstrip
python3 scripts/smoke_soil_ma.py   # scene checks + shim leg (needs desktop)
```

Demo (the kiosk):

```sh
./_build/default/native/soil_hello.exe --eval "(filmstrip/scene)"
```

A fullscreen toplevel opens showing the workspace: cascade strip (app
chooser + walked columns, selected crumb in amber) over the content
strip (pane frames at niri width fractions, focused pane ringed, todo
lists with item counts / notes titles / calendar today). Now edit the
workspace in the browser at `/filmstrip` — open panes, move focus
(alt+arrows), cycle widths — and re-run the shim: the kiosk is a
projection of the same Postgres workspace row.

Imageless variant (no image needed): `--scene native/fixture/hello-scene.json`
(the pads-hello fixture; the filmstrip scene has no file fixture because
it is workspace-shaped by definition).

## 6. What M-B adds (next pass, river)

river-window-management-v1 policy client: each pane a real window,
foreign apps (foot, browser) routed through the same workspace row,
alt+arrows focus REAL windows, xdg-popup/modal per pads/layers-modality,
plus the p95 manage-event -> layout-reply latency test on the thin shim.
Run river nested inside Hyprland for the dev loop (soil/river-split).
