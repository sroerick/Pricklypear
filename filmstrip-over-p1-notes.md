# filmstrip cut-over, phase 1 -- working notes (branch filmstrip-over)

Authority: docs/filmstrip-glochid-seam-map.md at master 974321c (do not modify).

## Environment facts

- shell.run tool is BROKEN in this child session (cl-exec-sandbox-process-group:
  could not create process group). All shell work goes through lisp.eval +
  uiop:run-program instead. Works fine.
- PP dialect has NO set!. Lambda bodies eval in Env.child of the closure env,
  so define inside a called lambda cannot rebind globals. Therefore the knob
  setter and the keys append are DEFMACROs: the expansion lands as top-level
  defines in the caller's env -- the same env-late-binding surface the old
  wholesale redefinitions rode.
- Plan: framework seam edits first (lib.pp), then blob splices in surface.ml
  one fn at a time; dune build + dune runtest glochid after every chunk;
  baseline 10 pre-existing failures, never more.

## Per-fn verdicts

- filmstrip/ws-node: framework @lib.pp (tree/get via node-path) vs blob
  @surface.ml (fs-backed read of glochid/ws-file). DIVERGED by HOST (fs vs
  tree) -- seam absorbs it: blob copy deleted, fs pair registered at boot
  via filmstrip/register-store! (bodies preserved inside the registration).
- filmstrip/ws-save: same treatment as ws-node.
- filmstrip/save-all: blob copy carries railmode/boxrows/axis/desktop
  carry-forward + glochid/axis + glochid/deskt; framework save-all rebuilds
  from {} with cascade/content/focus only. DIVerged beyond storage. STOP for
  this fn this phase: copy left in place, recorded here (phase 2 candidate).
- filmstrip/key-sentinels: wholesale redefinition replaced with a boot-time
  append of only rows 5+ of glochid/shortcuts through filmstrip/register-keys!;
  rows 1-4 are the framework registry defaults (identical alt-arrow rows).
  Guard splicing preserved verbatim in the boot loader.

## Build/test record

- framework seam chunk (lib.pp): dune build image+glochid EXIT 0; runtest glochid = 10 failures (baseline: surfaces 0-8 + libs-hyper-on-glochid visit-click-stream).
- blob chunk 1 (ws-node/ws-save -> register-store!): build EXIT 0; runtest 10 (baseline).
- Deploy prep: push wyo master rejected (fetch first) -- wyo/master had moved 974321c..5497b76 with two docs-only commits (f372130 seam-map re-cuts, 5497b76 1a ledger + 1b boot-coupling guard; both touch docs/filmstrip-glochid-seam-map.md only, 121+/23-). Re-cut map matches this implementation (storage seam = read+write pair). 1b guard compatible: my lib.pp additions are defines/defmacros only, no new top-level calls; blob boot calls only define lambdas + eval the pure glochid/shortcuts table. Merged wyo/master into master = fe6df36 (docs-only merge, ort clean); final build+runtest gated on fe6df36 before a single-session github+wyo push.
- Deployed merge fe6df36 (wyo hook): build release green, scp + promote fe6df363ac99 as current, migration 28 applied (028_branch_def_gen NOTICES -- def_gen col already exists), service restart pricklypear(ok), pruned 1 old release, deploy OK. Hook NOTE for later phases: "kernel deployed; libs/*.pp edits to existing names are NOT live (additive seed) -- pp-sync status/pkg / borge/branching.borg vault-authority (card 8dff7c9e)".
- Probes (one shot, recorded; no prod iteration): (now-iso) via /api/eval -> {ok:true,value:2026-10-14T06:23:49Z}; probe ran while the promote restart was completing. /filmstrip HTTP code -> 000 (no HTTP status within 20s; hop right after restart climb). Both recorded as-is.
- Master state: fe6df36 = merge of 4a0e0d5 (this p1) + hollis docs (f372130, 5497b76). Branch filmstrip-over stays at 4a0e0d5. Untracked: this notes file only.
- Sometimes-surprising: PP has no set!; the setter/append seams are defmacros
  expanding to top-level defines in the caller env. The blob's key trigger
  strings were reused verbatim from the pre-splice file (sed line splice), so
  the emitted sentinel DOM construction is byte-identical.

## Phase 2 remains

- Per seam map section 5:
  - save-all: still diverged (railmode/boxrows/axis/desktop carry-forward); either grow the framework save-all to the shared read-modify-write shape or keep a glochid-local variant behind a seams-aware footgun.
  - shelf-oob, winbox-oob, pane-stamp, oob-content-strip: still wholesale
    redefinitions in the blob; phase 2 = OOB hook table with app-supplied content
    (seam-map seam 4), then delete the copies.
  - Desktops family (deskt, desk-sentinel, first-on-desk, first-region-on-desk,
    desktop-set, heal-focus, windows-tree, windows-pane-node, keep-nodes):
    migrate BACK into libs/filmstrip (they were born in the app; a desktop
    framework feature per the locked model). Watch the year-entry shape the
    habitat fixed four times (seam-map section 4) - it must ride one source.
  - App registry verb (filmstrip/register-app!) replacing the apps/browse
    wholesale copies (seam-map seam 2; out of p1 scope, still wholesale today).
  - Local-prim purge: glochid dict-set*/request-param shims -> shared floor
    (seam-map seam 5).
  - Known context, do not chase in p2 either unless goals change: harness bin
    boot crash boot-load: unbound: type-fields.
