# soil M-B — river window policy (handoff)

Status: **make-phase handoff**. Spec authority: `borge/soil.borg` (river-split,
milestones M-B). M-A (kiosk) is shipped. **If code and spec disagree, the code
is wrong**; a mid-make spec gap becomes a `borge issue` and stops that item.

Victory: under a real river compositor, every mapped window becomes a
filmstrip pane — foot maps into the strip, alt+arrows focus real windows,
close removes the pane record — and the workspace row stays the single
policy authority shared with the browser shell.

---

## 0. Orientation

**river 0.4.x is the target** (decision recorded 2026-09-04; see soil.borg
agent note): the compositor ships NO window manager — a separate client
speaking the stable `river-window-management-v1` protocol owns all policy.
Arch ships it as `pacman -S river` (0.4.8); protocol XMLs at
`/usr/share/river-protocols/stable/`. river-classic (0.3 fork, maintenance
mode, riverctl-script keybindings) is explicitly NOT the target: the 0.4
protocol is the one the soil chapter names, is version-stable, and makes
keybindings WM policy instead of riverctl shell glue.

Programming model (the manage/render loop): river batches state events
(window/output/seat + app_id/title/dimensions/closed) then sends
`manage_start`; inside the sequence the WM makes policy requests —
`propose_dimensions`/`set_position` (layout), `focus_window` (seat),
`close`, `set_borders` — then `manage_finish` applies atomically.
`manage_dirty` requests a fresh sequence when the WM's own state changes
(= our policy evals changed the row). Keybindings: `river-xkb-bindings-v1`
globals; bindings enabled during a manage sequence; `pressed` events carry
the policy trigger. river owns rendering/frames; the WM only paints
decorations via `set_borders` (used here for the focus ring).

Dev loop: river NESTED inside Hyprland (`river -nested`); the shim attaches
to the nested socket (`WAYLAND_DISPLAY` of the nested instance).

## 1. Victory condition

`python3 scripts/smoke_soil_mb.py` green: nested river up, shim attached,
foot maps → workspace row gains `app:foot` pane, browser focus POST →
shim focuses the real window, foot dies → row record removed, p95
manage-event → layout-applied under budget. Alt+arrows: binding
registration verified at startup; the physical-key press is the same code
path as the browser focus leg (manual key test documented in the runbook).

## 2. Work items

**W1 — protocol bindings.** Vendor the two XMLs (`native/protocol/`,
pinned to river 0.4.8), dune rule running `wayland-scanner-ocaml`, pin the
client to global v1 (app_id/title/closed/dimensions/propose_dimensions/
set_position/close/set_borders/focus_window all exist at v1).

**W2 — Nopales policy surface (libs/filmstrip/lib.pp).** Foreign panes:
`{id route width view}` where `view` is the river window key (`w<proxy-id>`)
— habitat panes have no `view` field. New evals: `filmstrip/native-state`
(panes+focus+cascade JSON — the layout source), `filmstrip/native-open`
(app-id, view-key → open-or-focus, returns state), `native-close`
(view-key), `native-focus`/`native-move` (dir — same row mutations as the
HTTP handlers). All return native-state so one round trip drives a sequence.

**W3 — `native/soil_strip.ml`.** The policy client: bind manager +
xkb-bindings; track windows (app_id, title, key); on first app_id →
`native-open` eval + `manage_dirty`; on `closed` → `native-close` +
`manage_dirty`; on `manage_start` → eval `native-state`, lay out view-carrying
panes across the output strip (same 2/3/6 weights as the scene), focus +
accent-border the focused pane, 0-border the rest, `manage_finish`.
Bindings alt+Left/Right (focus), alt+shift+Left/Right (move) → eval +
`manage_dirty`. A 500ms poller re-syncs browser-driven row changes. Zero
Nopales eval on any frame path — evals happen on river events / poll ticks
(soil contract).

**W4 — latency + smoke + book.** Instrument event→applied and eval
durations; `--until N` smoke mode exits after N open+close cycles with a
p50/p95 report. `scripts/smoke_soil_mb.py` drives nested river + foot.
Book: river-split + milestones → implemented, dated agent notes, runbook,
checklist, commit.

## 3. Decisions already made (do not relitigate)

- **D1** river 0.4.x (stable protocol), not river-classic; global bound at
  v1 (v2+ features unused this pass).
- **D2** The workspace row is the ONLY policy authority; the shim is a dumb
  actuator + reporter. Browser and native mutations converge through it.
- **D3** Thin shim stays (loopback eval per policy event); embedded
  interpreter still deferred.
- **D4** Only view-carrying panes are window-managed; habitat panes stay
  browser-only geometry (the row is shared, layouts are per-tier).
- **D5** No shell-surface rendering this pass (cascade stays browser/kiosk);
  `set_borders` is the only paint (focus ring).
- **D6** Physical alt+arrow presses are manual-test + code-path-shared with
  the browser leg in the smoke (no virtual keyboard client this pass).

## 4. Acceptance checklist

- [x] `dune build @all` clean; no NEW test failures
- [x] nested river runs; shim attaches and completes manage sequences
- [x] foot maps → row gains `app:foot` pane (river → policy leg)
- [x] browser focus POST → shim focuses the real window (policy → river leg)
- [x] foot exits → row record removed
- [x] p95 event→applied printed and under budget
- [x] `scripts/smoke_soil_mb.py` green; alt+arrow manual test documented
- [x] `borge check .` no NEW failures; touched `.borg` files balance
- [x] All work committed

## 5. Runbook (dev loop + demo)

One-time: `sudo pacman -S river foot` (river 0.4.x ships the stable
river-window-management-v1; protocol XMLs vendored at
`native/protocol/`). Build + seed:

```sh
eval $(opam env --switch=poohstack --set-switch)
dune build @all
scripts/dev.sh start
export PP_URL=http://127.0.0.1:18080 PP_USER=admin PP_PASS=admin
python3 scripts/pp_sync.py push filmstrip
python3 scripts/smoke_soil_mb.py
```

Manual demo (nested in Hyprland — river nests automatically when
`WAYLAND_DISPLAY` is set):

```sh
river                                                          # nested
WAYLAND_DISPLAY=wayland-2 ./_build/default/native/soil_strip.exe
WAYLAND_DISPLAY=wayland-2 foot
```

foot maps into the strip (third-width, amber focus ring). Open more
windows, cycle widths from the browser (`/filmstrip` width button),
reorder with alt+shift+arrows, and reload — the workspace row is the
one authority; river is just its projection. Kill a window: its pane
record disappears with focus hand-off.

Latency: the shim prints p50/p95 event→applied + policy-eval at exit
(`--until 1` smoke mode). Measured: **740ms p95 with per-eval Basic
auth** (argon2id per request), **63–330ms with the session-cookie
transport** (the smoke's budget is a 500ms regression guard — the floor
is the image's per-eval cost: ~22ms idle, queuing under concurrent load
because all of admin's evals funnel through the identity session's one
store connection). Making policy evals genuinely ~1-5ms is an
IMAGE-side optimization (a fast eval path that skips per-request
session attach), not a shim one — flagged for the next image pass.

## 6. Known v1 edges (next-pass material)

- Only the first output is laid out; multi-output strip spreading is a
  later pass.
- No shell surface: the cascade stays browser/kiosk-only (M-A owns the
  native shell view; M-B owns window policy).
- The physical alt+arrow press is a manual test (D6) — no virtual
  keyboard client this pass.
- `exit 0` after the report: the wayland dispatch thread outlives
  switch teardown; a graceful shutdown path is a v2 nicety.
