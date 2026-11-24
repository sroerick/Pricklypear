<!-- nomia:generated release=0.1.0 digest=sha256:97b23def203ea449d99f69987812c1408c8843af182eab3c52a69be8fbbd7f85 inherit-root=true -->
<!-- Edit the reviewed nomia project source, not this generated file. -->

# Project instructions (pp/AGENTS.fragment.md)

# Pricklypear project instructions

Pricklypear is a multi-user Lisp OS: the language is **Nopales** (a
Scheme-family Lisp-1), with one unix image process, Postgres as the object
store, and htmx as the interface layer.

**The authoritative design is the multi-file Borge book.** Read root
`pricklypear.borg` and its chapter files in inline order. If code and spec
disagree, code is wrong. Use Borge to plan, make, and review changes. In
`plan` or spec discussion, edit the spec directly. File `borge issue new` only
when you are mid-`make` and discover the locked spec must change, or when you
find a genuine problem outside the current instructions.

**Naming:** **filmstrip** is the window-manager interface and shell — the
habitat UI (`libs/filmstrip`, `/filmstrip`) and the embedded harness view.
**glochid** is strictly the local agent harness in `glochid/`.

Status snapshots and the performance/promote incident record moved to
[`docs/AGENTS-history.md`](docs/AGENTS-history.md). Read that history when
working on the affected performance or promotion paths; it is not current
quick-start guidance.

## Live image work

The public bootstrap documents on the running host are the entry point for
agents:

1. Fetch `GET /agent.txt` and `GET /.well-known/agent.json` without auth.
   Production is `https://pricklypear.rocks/agent.txt`; local dev is
   `http://127.0.0.1:18080/agent.txt`.
2. Those documents explain how to build a REPL client: HTTP Basic plus
   `POST /api/eval` with `{"expr":"…"}`.
3. After auth, read the living playbook with `(notes/doc/get "agents")`.
4. Explore selectively with `(list-packages)`, `(describe "todo")`,
   `(apropos-docs "todo/")`, and `(function-doc "…")`.

The durable agent guide is the habitat notes document with slug `agents`, not
only this git file. Agents may and should update it when they learn something
durable about workflows, promote/seed policy, package gotchas, or UI
contracts. Browser: `/notes?slug=agents`. Keep this reviewed fragment roughly
aligned when authority or workflow rules change.

`load-library` is a deploy tool, not a session step. Package functions bind
lazily from the branch chain. Calling `(load-library "pkg")` re-evaluates the
release file onto the current branch and can clobber newer pins; never use it
in route handlers or routine repair scripts.

Notes writes attach to the current identity. Each agent's session logs, issue
notes, package specs, and personal docs belong in its own `<user>/personal`
garden. Never check out another user's branch to edit their notes. Filing to
the shared Board is not a cross-user notes write.

For local Nopales development, prefer the local lens over per-call HTTP:
`scripts/pp-local.sh` wires the repo lens directory, and `pull-data` /
`pull-tree` seed it. Definitions and reads resolve locally; rows, writes, and
transactions still cross to the server, which is index and write authority.
Use `sync` for one three-way round, `sync-loop` in the background, and `serve`
for the local chat/eval surface.

## Session ritual

Live authority is the agents note section **Session ritual
(habitat-workflow)**, plus `log/issues`, `log/sessions`, and per-package
`spec/<pkg>` notes. Spec chapter: `borge/habitat-workflow.borg`.

1. Work on your branch — define and route on `<user>/personal` or a dev branch,
   not directly on `system/main`; promote is the gate.
2. Obituary rule — no silent reversals. Before replacing an implementation,
   write what and why in the package's `spec/<pkg>` DECISIONS.
3. File, don't fix — out-of-scope work becomes a kind-tagged shared Board card.
   The `log/issues` ledger is a frozen archive. Use `/issues` or the
   `notes/issues/*` prims. This habitat product track is separate from the
   git/spec `borge issue new` track.
4. Handoff — end a session with roughly three lines in `log/sessions`.

From a configured laptop:

```bash
set -a; . ~/.config/pricklypear/env; set +a
export PP_EVAL="${PP_EVAL:-$HOME/dev/wyo.tech/pricklypear/_build/default/cli/bin/pp_eval.exe}"
curl -sS "$PP_URL/agent.txt" | head
"$PP_EVAL" -j '(whoami)'
"$PP_EVAL" -j '(list-packages)'
"$PP_EVAL" -j '(notes/doc/get "agents")'
```

- Client: OCaml `pp-eval` to `POST /api/eval`; wrapper `scripts/pp-eval.sh`.
- Secrets live only in `~/.config/pricklypear/env` with mode 600.
- Prefer eval over inventing REST for todos, notes, bible, UI, and similar
  product surfaces.
- Do not dump `(list-bindings)` into model context; use `describe`,
  `apropos-docs`, and `function-doc`.

## Branches and the vault

- Kernel OCaml and migrations: git is source of truth.
- Nopales packages: Postgres branches are live source; git `libs/` is a vault.
- Boot seed is additive into `system/main` only. It fills missing names from
  `libs/*.pp` and never overwrites existing main rows after promote.
- `(reseed-package "notes")` / `pp-sync reseed notes` is the only intentional
  git-to-main clobber.
- Daily work: personal/dev branch -> promote to main -> optional
  `pp-sync snapshot` into the repo.
- Local `scripts/dev.sh` Postgres is kernel-lab only; agents default to the
  configured production URL.

Branches:

- `system/main` — package defaults and inheritance root.
- `<user>/personal` — your garden; image boots here after seed.
- `<user>/dev` — optional workshop for product spikes intended to promote.

The vault is a generated mirror, not an authoring source. Do not hand-edit
`libs/*.pp` to ship a product change; the additive seed will not deploy edits
to existing names. Work on the habitat, promote, then refresh the vault with
`pp-sync snapshot`.

`pp-sync status [pkg]` compares the vault against `system/main` by canonical
AST body. `body-stale` means git has a fix main does not. Never reseed a
drifted package to land a fix; converge first, then reseed. `pull`, `snapshot`,
and `land` share one guarded write path. `--dry-run` previews honestly;
`--force` is the deliberate clobber. Full workflow: `docs/pp-sync-workflow.md`.

## One-tree storage

Postgres holds bytes, but the logical store is one tree (`tree_nodes` plus
`tree_branches`). Spec: `borge/one-tree.borg`; overview: `docs/one-tree.md`.

- Paths: `b.<branch>.{fn,type,route}.<label>` and `data.<type>.<id>`.
- The tree is the read and write source of truth; scaffolding tables are
  mirrored projections.
- Node hashes are sha256 over PG canonical jsonb text of the body, computed
  database-side in every writer.
- Fork is an empty lens with a parent pointer; inheritance uses a CTE.
- Typed records require all declared fields on create and update.
- Verify: `ksh scripts/verify_one_tree.sh` after `scripts/dev.sh start`.
- Module: `image/lib/tree_store.ml`.

## Stack

- OCaml 5.4.1 on the **`poohstack`** switch. Run
  `eval $(opam env --switch=poohstack --set-switch)` before building.
- The I/O rim is Eio: `Httpun_eio`, `Io_eio.Pg`, and pure pgx over Eio.Net.
  The Lwt rim and `PP_IO` flag are gone; rollback is git revert.
- Single unix binary in `image/`; Solo5 remains an optional later hardening
  axis, not the product path.
- Auth is in-process. Password hashes are argon2id PHC; legacy
  `sha256$salt$digest` rows verify transparently and rewrite on successful
  login.
- `pure-html` for type-safe HTML and `HTML.Hx.*` for htmx. Use
  `txt ~raw:true "%s" s` for deliberate raw HTML injection.
- `alcotest` for tests and `yojson` for JSON.
- Interactive surfaces are browser `/repl` and `POST /api/eval`; the optional
  line-protocol REPL uses `PP_REPL_PORT`.

## Layout

Build dependency order is `lisp/` -> `common/` -> `migrations/` -> `image/`.
The supervisor/authd packages were retired by the single-image pivot.

- `lisp/` — pure Nopales interpreter, no I/O.
- `common/` — pure shared types, including auth types.
- `migrations/` — additive SQL applied by dev and deploy hooks.
- `libs/` — core Nopales packages and the vault.
- `vendor/` — pinned external packages materialized by `scripts/vendor.sh`.
- `plugins/<pkg>/lib.pp` in the monorepo — package roots staged beside
  `libs/` and `vendor/`; boot seed topologically sorts `requires`.
- `image/` — the single unix binary: Nopales, REPL, htmx, Postgres, and auth.
- `scripts/` — dev orchestration. `scripts/dev.sh start` brings up Postgres and
  the image; `stop`, `restart`, `status`, and `clean` manage them.

## Auth

- Browser login: `/login`, `/logout`, HttpOnly `pp_session` cookie.
- Hard-public paths: `/login`, `/health`, `/logout`, `/welcome`, `/book.pdf`,
  `/src.tgz`, `/code`, `/code/*`, `/assets/*`, `/.well-known/agent.json`,
  `/agent.txt`. Everything else requires a session.
- `/agent` is the private HTML agent portal; `/book` is private.
- Admin user management: `/admin/users`.
- Bootstrap uses `PP_BOOTSTRAP_USER` and `PP_BOOTSTRAP_PASSWORD` only when the
  user set is empty or admin has no credential.
- Agents and `pp_eval` use HTTP Basic against the credentials table.
- `PP_AUTH_DISABLED=1` is a dev-only escape hatch.
- Verify auth with `ksh scripts/verify_auth.sh`.

## Workflow

- Build: `dune build` at the project root.
- Test: `dune runtest`; `lisp/` and `common/` need no services, image tests use
  the switch, and auth smoke needs Postgres plus the image.
- Dev stack: `scripts/dev.sh start`; ports are 5433 for Postgres, 18080 for
  HTTP, and 17878 for the REPL. Logs use the runtime temp directory configured
  by the script.
- Nopales routes use `(route "/x" "fn")` (private), `'public`, or
  `(public-route …)`. Kernel paths such as `/data`, `/admin`, and `/api/eval`
  stay hard-private.
- The `.borg` file is the source of truth. Do not edit it directly in
  make/task mode; file a spec gap and let the next plan pass resolve it.
- After changing a `.borg`-declared module, run `borge drift`.

## Conventions

- No `(load-library "pkg")` in route handlers. The session environment binds
  package definitions lazily from the branch chain.
- `pure-html` is tree-only; use `HTML.Hx.*`, not raw htmx strings. Use
  `txt ~raw:true` only for deliberate raw injection.
- Agent UI verification is tiered: wget, workflow/smoke, then screenshot and
  vision checks; graduate green flows into smokes. See
  [`docs/agent-ui-harness.md`](docs/agent-ui-harness.md).
- HTTP uses `Httpun_eio`; Postgres uses pure pgx over Eio.Net, no ORM, plain
  SQL. Transactions are SERIALIZABLE by default.
- Migrations are additive only and applied by `psql`, never by the image.
  Each migration self-registers its version; expensive backfills guard on it.
- Status flips, drift observations, and verification results belong in commit
  messages and reports, not in the issue tracker.

## Two queues: `jobs` versus `run`

They are deliberate and must not be merged:

- **`jobs`** — in-image Eio fiber and `jobs`/`job_steps`/`schedules` tables.
  Use for app async and cron work; claim is `SKIP LOCKED`.
- **`run`** — `data.s.run.*` plus `libs/workstreams` and the external glochid
  supervisor. Use for factory work: one run advances one card one gate. The
  LLM loop never runs inside the Pricklypear runtime.

Both use the same lease semantics: heartbeat plus deadline, reclaim on deadline
passed, and terminal writes fenced on ownership. They share the protocol by
reference, not by code; changing one requires checking the other.
