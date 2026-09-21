# Archive — historical receipts, kept out of the book and out of docs/

This directory holds **point-in-time artifacts**: executed make-phase
handoffs, plan-of-record snapshots, deploy write-ups superseded by the
`scripts/openbsd/` hooks, UI-review evidence, and one-off probe scripts.

The rules that put them here:

- **Spec authority is the borge book** (`borge/*.borg` inlined at
  `pricklypear.borg`). When a doc duplicates chapter content, the chapter
  wins and the doc is at best evidence. Nothing here is normative.
- **`docs/` is for living documentation only** (overviews and contracts a
  human or agent reads this week). Once work lands, its handoff moves here.
- Files keep their original names so old commit messages, borge
  references, and chat links stay greppable. Borge chapters point at these
  paths as evidence citations; that is intentional.

Current layout:
- `plans/` — executed handoffs and plan snapshots (DB-HARDENING-PLAN,
  eio-floor, Lwt-rim deletion, one-tree cutover, soil/filmstrip/controlbox
  handoffs, interlinear, …).
- `deploy/` — OpenBSD prod deploy + SSH-REPL bridge write-ups.
- `proposals/` — proposals and audits that were discussed but did not
  become chapters (component UI, cs-curriculum, theme hex audit).
- `reviews/` — UI review records + screenshot evidence.
- `scripts/` — one-off probes and one-time backfill verifiers, retired
  from `scripts/` once their make-phase ended.
