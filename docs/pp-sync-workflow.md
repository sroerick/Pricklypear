# pp-sync: the two-homes workflow

The one-page authority for moving package Nopales between the two
homes. Spec: `borge/branching.borg` § `vault-authority` (and § `seed-policy`).
Tools: `scripts/pp-sync.sh` (OCaml `cli/bin/pp_sync.ml`). Living agent
lore: habitat notes doc `agents` (section *pp-sync working rules*).

## The two homes

| home | what lives there | authoritative for |
|---|---|---|
| **LIVE habitat** (Postgres branches) | product Nopales on `system/main` + your garden (`<user>/personal`, `<user>/dev`) | all package Nopales |
| **GIT repo** | kernel (OCaml), `libs/<pkg>/lib.pp` = the **vault**: a generated mirror of main; migrations, docs | kernel + everything-not-Nopales |

One rule makes everything else follow: **live main is the source of
truth for package code; the vault is its mirror.** The vault exists for
backup, bootstrap, cloning, and code review — never as the "real"
editor of shipped behavior. The trap that makes this hard: the boot
seed is *additive* (only fills missing names), so a fix hand-edited
into `libs/` for a name that already exists on main is committed but
**never deployed**. You cannot fix live by editing the mirror.

## Change-type decision table

Every change has exactly one first-class path. Don't improvise; use the row.

| you changed… | author it | deploy | refresh mirror |
|---|---|---|---|
| a package define / route / helper (the normal case) | **in the habitat**: REPL/browser on `personal`, or `pp-sync push <pkg>` loads your local file onto the *garden* branch for evaluation | **promote** the touched names: `pp-sync land <pkg> [names…]` (promote to main + vault refresh + commit), or `/branches` → Promote, or `(promote-to-main …)` | included in `land`; otherwise `pp-sync pull <pkg>` (guarded merge into the vault) |
| many forms of one package, already written in the habitat | in the habitat | promote from the habitat | `pp-sync pull <pkg>` |
| several packages at once, or just want true numbers first | — | — | `pp-sync snapshot --dry-run` → review report → `pp-sync snapshot` (defaults to `system/main`) → `git add libs/ && git commit` **separate** from tooling commits |
| a **kernel** prim / route shell / migration (OCaml, SQL) | in git | normal release **promote** (rebuilds + restarts the image) | n/a (`.pp` names in the release only flow additively via seed) |
| a **new package** never seen on main | author `libs/<pkg>/lib.pp` in git (hand-authored is fine) | the boot seed adds it to main at deploy (additive), or `(reseed-package "<pkg>")` for right-now | it's already in the mirror; after main has it, merges keep it honest |
| a fix you wrote in git for an **existing** live name (the trap row) | the file in git is not the deploy path — re-apply **in the habitat**, or `pp-sync push` (load-string onto a garden branch) then promote the affected names by hand | promote | `pp-sync pull <pkg>` afterwards |
| whole-vault recovery ("main is wrong, git is right") | — | `pp-sync reseed <pkg>` (admin) — refuses if main has live-only names OR same-name body drift; resolve those first (`pull` merges main's version in) | reseed itself makes git → live; then status to confirm |
| branch state dump (personal/dev lenses, never main) | — | — | `pp-sync snapshot --branch admin/personal --out backups/<ref>/` (gitignored; never lands in `libs/`) |

## Command semantics (what each verb may and may not do)

- `status [pkg] [--names-only] [-v]` — compares vault ↔ **system/main**
  (default, checks out and restores). Report: name drift (`only-local` /
  `only-live`) and **`body-stale`** — names present both sides whose live
  body differs from the vault body, i.e. *git has the fix main doesn't*
  (or the reverse). `--names-only` is the fast blind pass. Exit 1 on
  drift, 2 on refusal. **Post-deploy check: `body-stale: 0`.**
- `pull <pkg>` — live main → vault. **Guarded merge**, not an overwrite:
  unchanged forms keep their text verbatim (comments survive), changed
  forms take live text and the report names them. Refuses to drop a
  name that live main binds NOWHERE (main is append-only — a dropped
  name is an anomaly, not an edit); a re-homed name (bound under
  another package, e.g. ui/bubble moving chat→ui) drops with a note. `--force` does a raw overwrite and warns about the comment
  lines it destroys. `--dry-run` prints the report, writes nothing.
- `push <pkg>` — vault file → CURRENT session branch (garden) via
  `(load-string …)`. That's for evaluation/demos on personal — it is
  *not* a deploy; land/promote is.
- `land <pkg> [names…] [--from <ref>]` — promote named bindings from a
  source branch to `system/main`, then refresh the vault through the
  same guarded merge, then `git commit` the vault file. Never pushes.
- `snapshot [--branch R] [--out DIR] [--dry-run]` — refresh every
  discovered package. Default branch `system/main` → `libs/`; any
  other ref → `backups/<ref>/`. Same guards and merge per `pull`;
  hand-authored files WITH live content merge keeping their custom
  header (std), skins and dead sources (zero live defines) refuse.
  Review the report, then commit the vault refresh as its own commit.
- `reseed <pkg>` — admin: upsert the git file onto main (the only
  intentional git→live clobber). Refuses while main has live-only
  names or same-name **body drift** (`pull` first to converge, or
  `--force` for the deliberate case). Read-only while refusing.
- `pull-all` — `pull` over every discovered package. Same guards.

Guards live in ONE place (`Pp_sync_merge.plan_write`) so pull, snapshot
and land cannot disagree; `--force` is the reviewed override for all of
them. Guard semantics (in order): hand-authored file + zero live
defines → refuse (skins/dead source); ANY name the file would drop that
live main binds nowhere → refuse by name (main is append-only);
otherwise merge — unlimited accounted shrink is a legitimate mirror
edit. Banned families (live rows that must never re-materialize a
vault file, e.g. `list` = std's old shape) are skipped by discovery
and refused explicitly.

## The loops (verification is part of the workflow, not optional)

**Product change loop.** Edit on personal → `pp-sync status <pkg>`
(garden drift view) → behave → `pp-sync land <pkg>` → `status` exits 0
for it → commit vault. No kernel deploy involved.

**Deploy loop (kernel release).** `dune build @all && dune runtest` →
release promote (kernel only) → **`pp-sync status`** → `body-stale: 0`
for the packages the release's `.pp` content should have updated — a
body a release claims to ship is not live until this reads 0. (Names
new in a release flow via the additive seed; that part works.)

**Mirror refresh loop.** `snapshot --dry-run` → read every REFUSED and
`changed-with-comments` line → run for real → `git diff libs/` review →
commit ("vault: snapshot …") separate from tooling/code commits →
`pp-sync status` exits 0.

**Sanity invariants.** Hand-authored vault files keep their custom
header + interior comments forever under merges; a guard refusal is a
question, not an error (read it; then `--force`, or fix the cause);
never `reseed` a drifted package to "land" a fix (that's the silent
live-only clobber the guards exist for).

## Current cleanup backlog (known decisions, 2026-10-02)

Decision cases parked in the review, with their intended moves — none
block the loops above:

- **std** (hand-authored, live content): snapshots/pulls now MERGE it
  (header kept, unchanged forms verbatim). First real merge needs a
  human read of the changed forms.
- **theme-\* skins** (no live content), **memory vs mem** double
  bookkeeping: stay hand-curated / untouched until decided; merge
  refuses, which is correct. When decided, retire `libs/memory` by
  re-attaching its CONTRACT comments inside `libs/mem` (comments survive
  merges) and deleting the dead file.
- **filmstrip's 19 live-only browse fns** (`todo/browse*`,
  `calendar/browse*`, `notes/browse*`): a plain snapshot merges them in
  — read the diff.
- **strays on main** (`theme`'s hi-page/zz-* probes; the 138 root
  junk names): **cannot be deleted yet** — no primitive deletes a define
  row from `system/main` (`unbind` refuses on the inheritance root).
  Filed work: a guarded admin delete-on-main primitive; ban-list holds
  the fort (`vault_banned`) until it exists.
- **`list` package** (std's old shape, live duplicates): banned from
  the vault so pull-all/snapshot stop resurrecting `libs/list`.
