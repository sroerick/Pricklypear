# native/ — soil-hello (pads hello dual-render spike, V2)

`soil-hello` is the OCaml native shim for the pads spike
(`docs/plans/pads-hello-dualrender.md`, W5; spec: `borge/soil.borg`
native-rendering). It renders a scene-dialect-v0 document (output of
Nopales `pads/render-scene`) to a real Wayland toplevel: owned two-pass
layout (D1), software raster (rect fill + bitmap font, D4), `wl_shm`
ARGB32 buffer, xdg-shell toplevel on the running compositor. Full-surface
repaint per commit (dumb v1). No Nopales eval on any frame path; no input
beyond window-close (D5).

## Build

```
cd <worktree root>
eval $(opam env --switch=poohstack --set-switch)
dune build native/
# binary: _build/default/native/soil_hello.exe
```

Dependencies: `wayland` (ocaml-wayland, pinned from
`https://github.com/talex5/ocaml-wayland.git`), `wayland.protocols`, `eio`,
`eio_main`, `yojson`, `logs`. Already on the poohstack switch; pin only if
rebuilding a fresh switch:

```
opam pin add -y wayland https://github.com/talex5/ocaml-wayland.git
```

## Run — scene source 1: file (imageless)

```
./_build/default/native/soil_hello.exe --scene native/fixture/hello-scene.json
```

Renders with no image running (the local-first seed). Smoke mode:
`--frames N` exits 0 after N committed frames (vsync-paced when the window
is visible; a 100 ms timer fallback keeps progress when the compositor
suspends the window, e.g. parked on an inactive workspace — frame callbacks
never fire there, so without the fallback `--frames` would hang).

```
./_build/default/native/soil_hello.exe --frames 60 --scene native/fixture/hello-scene.json
```

## Run — scene source 2: live eval (loopback)

```
scripts/dev.sh start              # if the dev image is not up
export PP_USER=admin PP_PASS=admin
./_build/default/native/soil_hello.exe --frames 60
```

Default expression is `(hello/scene)`; `--eval "EXPR"` fetches any expr via
`scripts/pp-eval.sh` (HTTP Basic loopback eval, `POST /api/eval`). The
response `{"ok":true,"value":<scene>}` is unwrapped; `value` may be the
scene JSON as a string or as an embedded object — both accepted. Set
`SOIL_PP_EVAL=/abs/path/scripts/pp-eval.sh` to run from outside the repo
root. Needs the Wayland socket of the running Hyprland session
(`WAYLAND_DISPLAY`), which is the normal desktop environment here.

## Protocol notes (why the code looks like that)

- One `wl_shm` pool + buffer per commit; the pool is destroyed right after
  `create_buffer` (buffers outlive their pool per wayland.xml). The pool's
  fd is closed by `with_memory_fd`'s `finally` — do NOT also close it in
  the buffer setup closure (double close → EBADF).
- Hyprland sends `wl_buffer.release` twice per buffer; the `on_release`
  handler destroys exactly once (guarded by a flag). Double destroy aborts
  message dispatch and wedges the frame loop.
- Frame callbacks do not fire while the toplevel is suspended; pacing is
  `Fiber.first (frame callback) (sleep 0.1)`.

## Font provenance

`font8x8.ml` embeds `font8x8_basic` (U+0020..U+007E) from
<https://github.com/dhepper/font8x8> — Daniel Hepper, **Public Domain**,
derived from public-domain VGA fonts by Marcel Sondaar / IBM. 8×8 source
glyphs are drawn into an 8×16 cell (each source row doubled); headings
(level ≥ 1) render at 2× scale (16×32). LSB of each byte = leftmost pixel.

## Scene dialect v0 (summary)

`{"version":0,"root":{kind,id,attrs,kids}}`; kinds `row | column | text`
(text nodes carry `text`); sizes `fit | grow:N | fixed:N` in the `size`
attr; `gap`/`padding` in px; `level` on headings. Normative docstring lives
on `pads/render-scene` in `libs/pads/lib.pp`.
