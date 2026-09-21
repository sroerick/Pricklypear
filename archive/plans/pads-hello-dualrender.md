# pads hello — dual-render victory spike (handoff)

Status: **implementation handoff** for a make-phase agent. Spec authority is
the borge book: `borge/pads.borg` (UI tree), `borge/soil.borg` (native tier),
`borge/filmstrip.borg` (shell — NOT built in this spike). **If code and spec
disagree, the code is wrong**; if the spec must change mid-make, file a borge
issue and stop there.

Victory: **one Nopales view, two renders** — the same pad tree renders (1) to
HTML through the image's normal web stack, verified byte-identical against the
string pipeline, and (2) to a native Wayland window on this Hyprland machine
through an OCaml shim. After this spike the production loop exists: author in
Nopales, render web or native, local-first raster with sync-up to pricklypear
as the follow-on pass.

---

## 0. You are here (orientation, no prior context assumed)

Pricklypear is a multi-user Lisp OS: **one OCaml image** (`image/`) + **Postgres**
object store; the language is **Nopales** (Scheme-family Lisp-1, interpreter in
`lisp/`, has `defmacro`/quasiquote/`&rest`/TCO). Apps are Nopales packages in
`libs/<pkg>/lib.pp`, seeded to the `system/main` branch; routes map via
`view_bindings` rows (route → function name), handlers return HTML strings
(string-spliced via `libs/html`). `libs/hyper` + the `soup/*` prims
(`pure/prims_soup.ml`) are a headless htmx simulator: parse → dispatch
route in-process → immutable page tree → `hyper/expect` assertions.

House conventions:

- OCaml 5.4.x on the poohstack opam switch:
  `eval $(opam env --switch=poohstack --set-switch)`; then `dune build @all`,
  `dune runtest --force` (alcotest).
- Dev image: `scripts/dev.sh start` (PG :5433, image :18080). Boostrap admin is
  `admin`/`admin` unless overridden.
- Eval from shell: `scripts/pp-eval.sh '<expr>'` (HTTP Basic + `POST /api/eval`).
- Sync packages to a running image: `scripts/pp_sync.py push <pkg>` (see
  `scripts/pp_sync.py --help`; `libs/*/lib.pp` headers document the vault flow).
- Nopales style: `(define f (lambda (x) "docstring" body))` — docstring LAST
  arg-position before body; look at `libs/html/lib.pp` before writing.
- Smokes live in `scripts/smoke_*.py`; habitat-visible checks go through
  `libs/verify`/`hyper`.
- Lint the book when you touch it: `borge balance <file>`, `borge check .`
  (pre-existing failure in `borge/music.borg` is NOT yours).

Relevant design facts already established (from the chapters, do not redigest):

- `soup/*` proves the image holds real tree values; `structed` proves
  structure-parametric tree surgery (`get-kids`/`set-kids`).
- The habitat CSS inventory is ~18 flex / 1 grid / 0 floats: the tier-2 pads
  vocabulary (row/column/flow + fit/grow/fixed) covers it.
- No Nopales eval on any frame path, ever (pads/renderer-dialects,
  soil/soil-contract). Policy events only.

## 1. Victory condition (exactly these)

**V1 — web.** `GET /hello` on the dev image returns HTML produced by
`pads/render-html`, and for a fixed fixture tree the output is **byte-identical**
to the same tree rendered through the legacy `libs/html` pipeline. Proven by
`hyper/expect`-style assertions in a smoke script (W3).

**V2 — native.** A native binary `soil-hello` opens an ordinary Wayland
toplevel **under the user's running Hyprland session** (no river, no
compositor code — river arrives at soil M-B, not here) and renders the *same
fixture pad tree*: `pads/render-scene` (Nopales) → scene JSON → OCaml layout
→ software raster → `wl_shm` buffer. Two scene sources, both must work:

- `--scene path/to/scene.json` — imageless file input (the local-first seed:
  render with no image running), and
- live pull: fetch `(hello/scene)` via loopback `scripts/pp-eval.sh` from a
  running dev image.

Runs until window closed (or a `--frames N` smoke mode exits 0 after N
committed frames — use this in CI-ish checks).

**V3 — one-authoring-surface proof.** Edit the greeting text in
`libs/hello/lib.pp`, push, and both renders change with **zero** edits to the
web route plumbing or the shim. This is the test that the pipeline, not the
demo, is what got built.

## 2. Architecture

```
libs/hello (Nopales)
   (hello/view) ── pad tree value (data)
        │
        ├─ pads/render-html ── HTML string ── route /hello ── hyper/expect (V1)
        │
        └─ pads/render-scene ── scene JSON (v0 dialect)
                                   │
                       ┌───────────┴────────────┐
                  --scene file              loopback /api/eval
                       │                        │
                       ▼                        ▼
                 SOIL-HELLO (OCaml, native/)
                   ocaml-wayland client (xdg-shell toplevel, wl_shm)
                   soil layout (OCaml, owned — see D1)
                   raster: rect fill + bitmap font glyphs
                       │
                       └─ present on Hyprland (V2)
```

Nothing in `image/`, `lisp/`, or `pure/` is touched by V1–V3. Pads is pure
Nopales in `libs/`; the shim is a new top-level `native/` dune exe.

## 3. Work items

**W1 — `libs/pads/lib.pp` (pure Nopales; the heart).**
- Constructor layers 1+2 only: positional base, then variadic
  `(pad tag attrs &rest kids)`; **no macro sugar** (quote-compounds is an open
  edge-case area — spec says desugar-only, so skip the macro entirely v1).
- Minimal vocabulary (pads/vocabulary-tiers): text-run strings in kid position;
  `row`, `column`; `heading` with `(level . n)`; sizes `(fit)`, `(grow n)`,
  `(fixed n)`; attrs as string-key alists. Skip float/flow/z-bands this spike.
- `pads/render-html` — MUST replicate `libs/html` escaping/attr semantics
  byte-for-byte; read `libs/html/lib.pp` first and treat it as the oracle.
- `pads/render-scene` — emits the v0 scene dialect (below) as JSON via the
  existing json prims (`apropos "json"` in the REPL).
- Ship the four ops (`replace`/`insert-at`/`remove`/`move`) as DATA
  DEFINITIONS + docstrings only (pads/ops-contract); nothing consumes them yet.

**W2 — `libs/hello/lib.pp`.**
- `hello/page` (legacy string pipeline, the oracle) and `hello/view` (pad tree)
  describing the SAME hello card: heading level 1 + para + one row of two
  text boxes, fixed/grow sized, padding/gap set.
- `hello/scene` = `(pads/render-scene (hello/view))`.
- Route: `view_bindings` row `/hello` → `hello/page`. Document the seeding
  steps in this file's runbook (pp-sync push + binding via REPL on dev image).

**W3 — verify.**
- `scripts/smoke_pads_hello.py` (pattern after `scripts/smoke_data_browser.py`):
  boot check → fetch `/hello` → in-image eval rendering the fixture tree both
  ways → assert byte equality of fragments (`soup/serialize` both trees and
  compare, or `hyper/expect` fixed strings). Exit non-zero on mismatch.
- Keep OCaml test surface untouched; pads tests are habitat-side by design.

**W4 — scene dialect v0** (one stable page; put the normative text as a
docstring on `pads/render-scene` and mirror here):

```json
{ "version": 0,
  "root": { "kind": "column", "id": "root", "attrs": {"gap": "12", "padding": "16"},
            "kids": [ { "kind": "text", "id": "t1", "text": "hello",
                        "attrs": {"level": "1", "size": "fit"} } ] } }
```

Kinds v0: `row` | `column` | `text`. Size attr grammar: `fit` | `grow:N` |
`fixed:N` in main/cross attr slots. Ids are stable strings (future structed
handshake; synthesize deterministically, e.g. path ids `0/2/1`).

**W5 — `native/soil-hello` (OCaml exe; the shim).**
- Wayland client: pin `ocaml-wayland` (Eio; aligns with
  `docs/runtime-effects-transition.md`):
  `opam pin add -y ocaml-wayland https://github.com/talex5/ocaml-wayland.git`
  — xdg-shell toplevel, `wl_shm` ARGB buffers, frame callback loop. Reference
  its examples/ for the minimal toplevel skeleton.
- **Layout (owned, D1):** two passes per axis — bottom-up fit/measure
  (text measured with the font's advance table), then top-down distribution of
  grow/fixed with gap/padding and cross-axis align. Direction &gt;200 LOC for
  v0 kinds means you're gold-plating; consult `~/dev/clay_test/clay.h` ONLY as
  reference reading for the algorithm shape.
- **Raster:** ARGB32 rect fill; glyphs via a vendored permissively-licensed
  8×16 bitmap font embedded as an OCaml table (e.g. a public-domain classic
  X11 fixed font; record provenance in `native/README.md`). No shaping, no
  AA, pixel-perfect is a non-goal.
- Damage: full-surface repaint per commit (soil chapter: dumb v1, stated so
  nobody optimizes it).
- Flags: `--scene FILE` | `--eval "EXPR"` (shells out to
  `scripts/pp-eval.sh`) | `--frames N`. Default expression `(hello/scene)`.

**W6 — runbook + book hygiene.** `native/README.md` (build/run both scene
sources); if any spec text needed adjusting, edit the chapter (plan phase
rules don't apply — you're in make: mid-make spec changes go through a borge
issue per AGENTS.md). `borge balance` any touched `.borg`.

## 4. Decisions already made (do not relitigate)

- **D1** Layout is owned OCaml, not C-Clay FFI (recorded in
  `borge/soil.borg` native-rendering; C-Clay is the documented fallback only
  if wrap/float/scroll complexity outgrows the owned engine).
- **D2** Thin shim: loopback eval + `--scene` file fallback. The embedded
  interpreter topology is deferred (soil/binding-path).
- **D3** Pads is pure Nopales; zero changes under `image/`, `lisp/`, `pure/`.
- **D4** Bitmap font before pango/freetype. Boring text beats blocked text.
- **D5** No river, no WM policy, no input handling beyond window-close.
- **D6** No pads macro sugar; constructors stay variadic-callable.

## 5. Out of scope (will look adjacent; do not chase)

filmstrip shell, two-sequence workspace, cascade/browser; floats, z-bands,
modal routing; ANSI renderer; structed editing of pad trees; multi-user seats;
local-first lens sync-up to prod (that's `agent/` + `borge/local-first.borg`,
the NEXT pass after this loop exists); CSS/theme port.

## 6. Acceptance checklist

- [x] `dune build @all` clean; `dune runtest --force` as before (no new failures)
- [x] `scripts/dev.sh start` → `curl -u admin:admin localhost:18080/hello` serves the card
- [x] `scripts/smoke_pads_hello.py` green (byte-identity, legacy vs pads)
- [x] `soil-hello --frames 60 --scene fixture.json` exits 0, window visible on Hyprland
- [x] `soil-hello --frames 60` (live eval path) renders identically to V1 content
- [x] V3: greeting text edit reflects in both renders with no shim/route edits
- [x] `borge check .` shows no NEW failures; touched `.borg` files balance
