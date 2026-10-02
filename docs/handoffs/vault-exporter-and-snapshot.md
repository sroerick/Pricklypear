# Handoff: vault exporter fix + comment-preserving snapshot (card 8dff7c9e)

Date: 2026-10-02 · Author: pi session with roerick · Board card: `8dff7c9e`
(title "BUG: promote does not deploy Nopales .pp; vault has drifted 435
live-only defines"); filed track-BUG `8201f0f4` ("pp-sync snapshot strips
interior comments from the vault").

## What this is

Option 1 of card `8dff7c9e` was chosen (roerick, 2026-10-02): **Postgres
`system/main` stays authoritative for package Nopales; `libs/` is a
generated MIRROR (vault), not an authoring source.** The work this session:
make the vault tooling actually able to refresh itself without data loss,
then stop — do NOT run the real snapshot without the review below.

Five commits on `master` (all local; **nothing pushed**, no deploy):

| commit | what |
|---|---|
| `628416f` | `delete-user` prim + admin route + button (card `0d20dc58`); drop regressed `libs/list` |
| `a16414a` | per-identity inflight fair-share cap (card `1bdeb856`) |
| `5f6a9fa` | `pp-issue-reconcile.sh` also matches the CARD id (card `a80bef58`) |
| `b8cff99` | `pp-sync status` body-level drift view + spec `vault-authority` |
| `4a50d34` | exporter membership from PROVENANCE + snapshot guards |
| `794309a` | comment-preserving vault merge (BUG `8201f0f4`) |

(`e4b99d1` `set-password` is roerick's, same day, not this session.)

## The bug chain (why the snapshot was unsafe)

1. **Exporter membership was name-prefix based.** `(export-package pkg)`
   matched bindings by `pkg/…`. Wrong for any package whose defines do not
   share its name: `std` defines `list/…`, `html/…`, `css/…`; each
   `theme-asNNN` defines `theme/…`; `memory` defines `mem/…`. For those it
   returned an export-list-only body (**zero defines**), and a snapshot
   wrote that over the vault: `libs/std` 69 defines → 0, every theme skin
   35 → 0. Fixed: membership now comes from `function_defs.package`
   **provenance** (the `(package …)` form's stamp, set at load), prefix
   only as fallback. `Store.package_names_by_provenance`.

2. **`export-package` emits NO interior comments.** The vault is literate
   source; comments exist only in git. Measured: `filmstrip` 146 comment
   lines in git vs **2** in the live export; `todo` 110 vs ~0. A plain
   generator write deleted that documentation. Fixed: snapshot now MERGES
   (`cli/lib/pp_sync_merge.ml`) — unchanged forms keep the OLD text
   verbatim (comments + formatting), only changed forms take new text; a
   changed form that had interior comments is reported by name.

3. **Guards.** Snapshot refuses (a) to overwrite a file without the
   `exported from live branch` header ("hand-authored"), and (b) to write
   fewer defines than the file it replaces. `--force` overrides both.

## Verified (this session)

- `dune runtest cli`: `pp_sync_merge` 5/5, `pp_sync_diff` 6/6,
  `sync_vault` 3/3. `image/test/package_provenance_test` (new, PG-backed)
  2/2. `borge check` 45/45; `borge drift` spec/structural none.
- Live previews (wrote to `/tmp` only, since cleaned): after the fix
  `std` exports 74 defines (was 0), `theme` 50 (was 0). With the merge,
  forms kept verbatim: `notes` 294/294, `music` 31/31, `theme` 50/50,
  `html` 36/37, `todo` 196, `filmstrip` 115/138.
- **The vault is unchanged.** `git status --short libs/` is empty.

## What is NOT done — read before running anything

### A. "Hand-authored" is a 3-way distinction, not 2

The header test is a blunt proxy. There are really three classes:

1. **Pure generator output** — header + ~0 comments (`rb`, `book`, `edit`,
   `workout`, most). Safe to merge.
2. **Generator output later repo-edited** — header + comments (`todo` 110,
   `filmstrip` 146, `music` 38, `layout` 25). The merge handles these:
   repo comments survive for unchanged forms.
3. **Repo-authored, never pulled** — custom header (`std`, the six
   `theme-*`, `workstreams`, `lookbook`, `memory`, `hyper`, `suno`, …).
   Currently REFUSED wholesale.

Class 3 is the flaw to fix next. `std` **is seeded and has live content**
(74 defines) and should be **merged** (preserving its custom header), while
`theme-*` have **zero** live content and must be refused. The header can't
tell them apart; the **define count** can. Also: `pp_sync_merge` currently
takes the generator's prefix for the FIRST form, which would clobber a
custom header — so class 3 can't move from "refuse" to "merge" without
preserving the old first-prefix.

Suggested refinement:
- no live content for the package → refuse;
- live content exists → merge, and **preserve the OLD first-form prefix**
  (keep the custom header), not the generator's.

### B. Deferred decisions

- `filmstrip` has **17 changed forms** whose interior comments the merge
  reports as lost; they need re-attaching by hand (or accept new bodies).
- Two prod anomalies surfaced while diffing (`only-live`, not vault bugs):
  `theme` exports 5 stray names (`hi-page`, `zz-dot-test`, `zz-list`,
  `zz-raw`, `zz-unicode-handler`); `rb` 26 / `chat` 9 / `std` 69 /
  `lookbook` 19 local-only names. Decide keep-live vs delete.
- `libs/memory` (hand-authored, defines `mem/…`) vs `libs/mem` (generated,
  24 defines) — double bookkeeping; reconcile.
- `pp-sync status -b system/main` is the honest post-deploy check:
  `body-stale: 0` means the `.pp` half actually went live.

### C. Deploy is separate and unchanged

Promote only auto-deploys the **kernel**; `.pp` changes reach `system/main`
only via the (additive) boot seed, which never overwrites an existing
name. The exporter fix lives in the kernel, so prod still runs the OLD
`export-package` (name-prefix) until a release is promoted. Any real
snapshot run against prod BEFORE that deploy will still see `std`/`theme`
export as zero defines — the guard catches it, but the merge gets no real
content for those until the kernel ships.

## Next step (recommended order)

1. Refine the guard + merge per (A): define-count-based refusal,
   first-prefix preservation. Add a unit test for "custom header survives".
2. `dune build @all && dune runtest cli image/test/package_provenance_test`.
3. Run `pp-sync snapshot --branch system/main` for real (generator files
   only, comments preserved). Review the diff; hand-fix the `filmstrip`
   report; snapshot the class-3 packages once (A) lands.
4. `borge drift`; commit the vault refresh separately from the tooling.
5. Deploy the kernel normally (roerick), then `pp-sync status` to confirm.

## Refs

- Spec: `borge/branching.borg` § `vault-authority` (new, 2026-10-02) and
  § `seed-policy`. `AGENTS.md` § Branches updated.
- Code: `cli/lib/pp_sync_diff.ml`, `cli/lib/pp_sync_merge.ml`,
  `cli/bin/pp_sync.ml` (`cmd_status`, `cmd_snapshot`),
  `image/lib/agent_tools.ml` (`package_binding_names`),
  `image/lib/store_packages.ml` (`package_names_by_provenance`).
- Filed issue: `8201f0f4` (BUG, comment stripping). Card `8dff7c9e` open.
