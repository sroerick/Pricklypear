# filmstrip cut-over, phase 2 -- working notes

Authority: docs/filmstrip-glochid-seam-map.md (do not modify).

## Landed
- e962da3 framework: desktops family back-migrated into libs/filmstrip/lib.pp (one source, seam-map section 4); 1b ws-init! boot guard carried.
- 4c91b0d glochid: app copy sheds desktops/railmode/save-all/key redefs; calls renamed glochid/* -> filmstrip/*.
- 2be140f boot-load repair (gregor; the p2 child died mid-verify). Three splice bugs:
  1. (filmstrip/register-store! ...) read/write pair never closed -> the form swallowed the blob to EOF (the "unterminated list" boot crash).
  2. glochid/tree-boxes: stray ) from the rename splice orphaned the visited binding in the let*.
  3. (filmstrip/register-keys! (list/map ...)) lost its rows list ("list/map needs (f xs)"); rows 5+ of glochid/shortcuts feed it now, rows 1-4 ride the framework registry defaults.

## Verify + deploy
- Ground truth = the repo's own reader: lisp/bin/repl.exe over the extracted blobs (a paren scanner can disagree at the margins; the PP reader settles it).
- dune runtest glochid: 9/27 (documented baseline 10 - one better).
- master e7631d6 (merge wyo docs 51ed9ca); github + wyo pushed; hook: release e7631d66d642 @ 2026-10-14T12:57Z, migration 28 no-op, service ok.
- Post-deploy: health 200; (now-iso) ok; /filmstrip 303 (login gate); (filmstrip/ws-desktop) -> 0 via /api/eval (desktops defs live); save-all carry-forward (railmode/boxrows/axis/desktop) verified in the framework body.
- Deploy climb: ~5 min post-promote, /api/eval + /filmstrip time out, then clear (both fe6df36 and e7631d66d642). Probe /health first; give it a buffer.

## Phase 3 (per seam map)
- register-app! (apps/browse registry verb) remains.

## Addendum 10-14 ~13:4xZ: css render-bomb
- Post-deploy probe caught (filmstrip/css) dying with 'unbound: ##Q##:root##Q##' - the p2 child's ksh quote-token leaked into lib.pp as a bare symbol: the :root selector was never a string. Every shell render died; boot gates missed it (boot only defines; the glochid suite does not exercise the framework css).
- Fix c758acc: token -> ":root", doubled palette comment deduped. runtest 9 -> 4/27 (5 surfaces tests that render the shell flipped green - they were crashing on the same bomb).
- Release c758acc56ae4 LIVE 13:40Z (hook, migration 28 no-op). Prod-verified via /api/eval: css renders (5KB, :root + 0f1117), shell fragment renders 118KB (ws-desk x11, fs-railmode x5, hx-post x218, dark palette).

## Addendum 10-14 ~14:1xZ: admin-seat verification
- /filmstrip is login-gated (303 -> /login?next=%2Ffilmstrip); with the corridor Bearer it returns the FULL page (135KB) on BOTH hosts (pricklypear.rocks + admin.pricklypear.rocks): chrome, htmx.min.js + htmx-2-compat.min.js, ws-desk x11, fs-railmode x5, hx-post x219, dark palette (0f1117). No unbound/error text in the page.
- Corridor Bearer identity = gregor (whoami), NOT admin; delegated-token writes still attribute to admin (09-14 note). Admin's ws row on prod is EMPTY (never saved; pane persistence silently failed until 2c9e65d 10-13; ws-type unchanged 'filmstrip-ws', nothing orphaned).
- Rendered admin's actual path (empty row -> ws-default -> shell-html): 6.5KB, all machinery markers, clean. So admin's seat = new dark shell + rail + desktops over a default workspace; no old panes exist to restore.

## Addendum 10-14 ~22:3xZ: harness homepage render fixed (3deabd0, rel c125e7ecb944)
- Root causes (both p2 leftovers, harness-only: the habitat tree-roots default is EMPTY, so the habitat never rendered the app's rail trees):
  1. tree-roots windows root still called the retired glochid/windows-tree after the desktops back-migration renamed the family filmstrip/*: every harness GET / + /filmstrip answered 'route/dispatch: unbound: glochid/windows-tree' (45-byte bodies).
  2. json-emit is polymorphic over alist | legacy VStr JSON: a vals string that already looks like JSON ({"d":"0"}) is emitted RAW as an object, so the desk-dir verbs round-trip handed verb-btn an alist and as-str crashed (57-byte bodies, windows tree only).
- Fixes: surface.ml tree-roots calls filmstrip/windows-tree (+ one stale keep-nodes docstring); lib.pp verb-btn hx-vals shape-tolerant (string? rides as-is, parsed dicts re-emit via json-emit).
- Verified: harness GET / 29.7KB full shell (ws-desk x11), /filmstrip/tree?root=windows renders, /chat unchanged; runtest glochid holds 4/27-failure baseline. Deployed c125e7ecb944 (merged wyo 8b0f2b4 auth-spec; hook 22:29Z, glochid harness staged): habitat health 200, /filmstrip 135KB (ws-desk x11, hx-post x216), / -> 303 /filmstrip.
- Known minor: /filmstrip/cascade 45-byte error on the harness (route contract; the cascade column was retired by the start menu - not homepage-critical).
