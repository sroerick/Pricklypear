# Pricklypear

A multi-user Lisp operating system with Postgres as the shared object
store and htmx as the interface layer. Humans and agents share one
live image: the running process **is** the API.

The design is a multi-file borge book: root
[`pricklypear.borg`](pricklypear.borg) inlines chapter files
(`philosophy.borg`, `single-image.borg`, `lisp-interpreter.borg`,
`ai-authoring.borg`, …) in literate reading order.
If the code disagrees, the code is wrong.

## Status

**v1 single-image habitat** — end-to-end built, dogfooded, and
smoke-tested.

| Tracker | Value (2026-07-12 reconcile) |
|---|---|
| `borge report` | **55 implemented / 0 verified / 10 planned / 9 deprecated** |
| Completion | **~74%** of tracked sections |
| Architecture | One unix image process + Postgres + nginx |
| Auth | In-process web login (`pp_session`) + HTTP Basic for agents |
| Product shell | notes, todos, outline, ui/theme (Lisp CSS), bible/dabar, agent sessions |

Only **real** planned gaps remain (not stale statuses). Highlights:

- `object-model.type-system` — design only (v1.5+)
- `branching.ast-nodes`, `branching.promote` — design / promote UX
- `notes.links-and-roam` — wikilinks + backlinks
- `dabar` study polish (full interlinear corpus, real FTS, work-packages)
- `ai-authoring.review-merge` — human review UI for AI freezes
- `product-experience.horizon` — design-only futures

Historical multi-process chapters (`supervisor.*`, `auth.rpc-from-supervisor`,
`image.ssh-interface`) are **deprecated** in favor of `single-image.borg`.

## Architecture (current)

```
Browser / pp_eval / browser REPL
        │
        ▼
nginx (TLS / reverse proxy; loopback backends)
        │
        ▼
image  (one OCaml unix binary)
   ├── HTTP  :18080   htmx UI + /api/eval + /api/agent
   │     /login, /logout, /health, /admin/users
   │     /notes, /todos, /dabar, /bindings, /branches, /repl, …
   │     user routes via (route "/x" "handler")
   ├── REPL  :17878   optional line-protocol (loopback / tunnel)
   └── Auth in-process (password verify, sessions, bootstrap admin)
        │
        ▼
Postgres (shared object store)
   branches, function_defs, type_defs, view_bindings,
   data_rows, users, credentials, auth_sessions, auth_log,
   system_keys, ai_sessions, ai_messages, …
```

**Not the current system:** supervisor fleet, authd sidecar, per-user
posix_spawn images, default awa-ssh front door. Those exist only as
historical notes / deprecated spec chapters / empty stubs.

**Multi-user still means:** many humans, each with credentials, a
browser/API session, and `<username>/personal` garden branch inheriting
`system/main`. Isolation is session + branch scoped, not multi-process.

## Components

- **`lisp/`** — pure Scheme-flavored Lisp-1 interpreter (no I/O).
  `Eval.eval : env -> expr -> value Lwt.t`. Unit tests need no services.
- **`common/`** — shared types (manifest, branch, auth_types, …).
- **`migrations/`** — additive SQL applied by `scripts/dev.sh` (and
  deploy hooks) before the image serves traffic:
  `001_branches`, `002_auth`, `003_data`, `004_bible_ref_index`,
  `005_ai_sessions`.
- **`image/`** — the sole runtime binary: Lisp, REPL, htmx, Postgres,
  auth, AI session loop. Modules include `http.ml`, `session.ml`,
  `crud.ml`, `auth_*.ml`, `ai_store.ml`, `llm_client.ml`, …
- **`libs/`** — Lisp packages seeded to `system/main` at boot:
  `std`, `ui`, `ai`, `outline`, `notes`, `todo`, `calendar`, `bible`,
  `dabar`, `weather`, …
- **`scripts/`** — `dev.sh` (PG + image), `pp_eval.py`, `pp_sync.py`,
  seed scripts, OpenBSD deploy helpers.
- **`spikes/`** — historical feasibility probes (awa-ssh, mirage_solo5).
  Solo5 remains a possible hardening axis, not the v1 path.

`supervisor/` is a retired stub. There is no separate `auth/` package.

## Stack

- OCaml 5.4.1 on the **`poohstack`** opam switch
- Lwt for image I/O (`httpaf-lwt-unix`, `pgx_lwt` + custom `Io_unix`)
- `pure-html` for type-safe HTML + `HTML.Hx.*` htmx attributes
- Password hashes: interim `sha256$salt$digest` (argon2 deferred —
  BORGE-1783831698)
- AI: Synthetic-compatible chat client + durable sessions in Postgres
- Eio only for pure-unix dev tooling if any — not inside the image

## Build

```bash
eval $(opam env --switch=poohstack --set-switch)
dune build @all
dune runtest --force
```

## Dev stack

```bash
scripts/dev.sh start    # PG :5433 + image :18080 / :17878
scripts/dev.sh status
scripts/dev.sh stop
```

Logs under `/tmp/pp-dev/`. Env knobs are documented in the `dev.sh`
header.

## Laptop / agent eval (the image is the API)

```bash
set -a; . ~/.config/pricklypear/env; set +a   # PP_URL, PP_USER, PP_PASS, PP_EVAL
python3 "$PP_EVAL" -j '(whoami)'
python3 "$PP_EVAL" -j '(load-library "todo")'
python3 scripts/pp_eval.py -j '(bible/verse "John" 3 16)'
```

`POST /api/eval` (HTTP Basic or session cookie). Prefer eval over
inventing new REST for notes/todos/bible/ui.

## Branches: defaults vs personal garden

- **`system/main`** — package defaults. Boot `seed_stdlib` always writes
  `libs/*` here from git. Treat as read-mostly.
- **`<user>/personal`** — garden. Empty child of main (inherits via
  recursive CTE). Your `(define …)` and `(route …)` land here and
  survive reseed/deploy.
- Switch: `(checkout "system/main")` / `(checkout "user/personal")`.

## Spec reconciliation (borge)

Statuses are maintained with the borge workflow (`plan` / `make` /
`review`). The 2026-07-12 reconcile pass closed the big
README-vs-code gap after the single-image pivot and product dogfood:

- Marked `single-image.*` implemented
- Marked auth web-login / identity / admin users implemented
- Marked notes core + unified todos implemented (roam still planned)
- Marked dabar packaging/reader/nav/strongs implemented (FTS/corpus partial)
- Marked image packages + product-theme implemented
- Deprecated supervisor + SSH-front-door sections
- Resolved historical borge issues that already shipped

Open issues that still matter:

| ID | Topic |
|---|---|
| BORGE-1783383905 | Quote of compound s-exprs |
| BORGE-1783398327 | Addressable AST nodes (design) |
| BORGE-1783439434 | awa-ssh identity (if SSH returns) |
| BORGE-1783491378 | data_rows vs schema-authority DDL |
| BORGE-1783831698 | argon2id password hashing |

## Deferred / roadmap

- Real Postgres FTS for bible/notes search
- Notes roam (`[[slug]]` + backlinks)
- Branch promote UX + unoverride prim
- AI review-merge UI
- Typed fields (`object-model.type-system`)
- argon2id; optional hardened SSH front door
- Solo5 / process-isolation multi-tenant (explicit later product)

## Monitor command

```bash
cd /home/roerick/dev/wyo.tech/pricklypear && \
  eval $(opam env --switch=poohstack --set-switch) && \
  dune build @all && \
  dune runtest --force && \
  borge lint && \
  borge drift && \
  borge report | tail -5
```

Expected after this reconcile: build/tests green, lint clean,
report near **55 implemented / 10 planned / 9 deprecated / ~74%**.
Verified counts stay 0 until smoke stanzas are re-run under review.
