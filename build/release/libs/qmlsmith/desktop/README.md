# QML Smith (roerick.qmlsmith)

Desktop icons forged by Pricklypear. This is the repo mirror of the live
plugin at `~/.config/omarchy/plugins/<user>.qmlsmith/` (see `../README.md`
for the package overview). Pipeline:

    description -> (qmlsmith/icon "...")          POST /api/eval on the image
                -> ai/ask authors a UI PRIM TREE (not QML!)
                -> qml/compile (deterministic, in libs/qmlsmith/lib.pp)
                -> QtQuick file -> ~/.config/omarchy/qmlsmith/<slug>.qml
                -> desktop layer (behind your windows, over the wallpaper)

The model NEVER writes QML. It writes a tiny s-expr UI tree
(`(icon (name ...) (launch ...) ... (rect ...) (circle ...) (text ...) (group ...))`),
and `qml/compile` owns the Qt. Safer on every axis, and it always parses.

## Use

- Forge: SUPER + ALT + Q (type a description, Enter) or
  `omarchy-shell shell summon roerick.qmlsmith '{"desc":"a rocket icon, labeled launch"}'`
- Click an icon on the desktop to run its `launch` command (if the tree set one).
- Hover shows an X to delete the icon.
- All icons live in `~/.config/omarchy/qmlsmith/`; edit those QML files freely.

## Image-side prims

`libs/qmlsmith/lib.pp` (this repo, pushed via `pp-sync push qmlsmith`):
`qmlsmith/prompt`, `qml/compile tree`, `qmlsmith/icon desc` -> JSON
`{name, qml, launch}`.

Status: spike, not prod ready. Model occasionally emits an unparseable tree;
forging again usually works (the failure surfaces verbatim in the bubble).
