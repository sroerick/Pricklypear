# qmlsmith — UI prims -> desktop QML (spike)

Two halves:

- `lib.pp` (this dir) — the Nopales package: `qml/compile`, `qmlsmith/*` prims.
  A Nopales expression returns a UI prim tree
  (`icon/rect/circle/text/button/group`); `qml/compile` deterministically
  emits QtQuick text (or HTML — `qmlsmith/hesc` backend). The model never
  writes QML; the compiler owns the Qt.
- `desktop/` — the Quickshell/omarchy-shell plugin *example* that renders it.
  Polls `POST /api/eval` for a compiled widget, instantiates the QML with
  `Qt.createQmlObject` on a bottom-layer desktop window, and wires
  `act|(eval ...)` objectNames back to image evals. The repo copy is the
  canonical example; the live install is
  `~/.config/omarchy/plugins/<user>.qmlsmith/` (same files,
  `qmlsmith/widgets.json` registration — see `widgets.example.json`).

Not prod ready (2026-09 spike): poll + full re-render per cycle, no diffing,
no true server-side rasterization — the image compiles the tree to QML text
and the desktop renders it.
