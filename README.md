# Pricklypear

A multi-user Lisp OS: one live image process, Postgres as the object
store, htmx as the interface. Humans and agents share the running
system — the process **is** the API.

Design is a multi-file borge book: root [`pricklypear.borg`](pricklypear.borg)
inlines chapters (`borge/philosophy.borg`, `borge/single-image.borg`,
`borge/lisp-interpreter.borg`, …) in literate order. If code and spec
disagree, the code is wrong.

## Status

**v1 single-image habitat** — built, dogfooded, publicly hosted.

| | |
|---|---|
| Architecture | One OCaml image + Postgres + nginx |
| Auth | In-process web login (`pp_session`) + HTTP Basic for agents |
| Product | notes, todos, outline, ui/theme, bible/dabar, agent sessions |
| Public face | `/welcome`, `/code`, `/book.pdf`, `/src.tgz` |

Real planned gaps (not stale foundation): type-system design, branch
promote UX, notes roam, dabar FTS/corpus polish, AI review-merge UI,
argon2. Deprecated multi-process chapters remain in the book as
history (`supervisor.*`, old SSH-front-door notes).

## Architecture

```
Browser / pp_eval / browser REPL
        │
        ▼
nginx (TLS / reverse proxy)
        │
        ▼
image  (one OCaml unix binary)
   ├── HTTP   htmx UI + /api/eval + /api/agent
   │     public: /welcome /code /book.pdf /login /health …
   │     private: / /bindings /edit /repl /data /admin …
   ├── REPL   optional line-protocol (loopback / tunnel)
   └── Auth   in-process (sessions, passwords, bootstrap admin)
        │
        ▼
Postgres  (branches, function_defs, type_defs, view_bindings,
           data_rows, users, credentials, auth_sessions, …)
```

Multi-user = credentials + sessions + `<user>/personal` garden branches
inheriting `system/main`. Not a process fleet.

## Layout

| Path | Role |
|---|---|
| `lisp/` | Pure Lisp-1 interpreter (`Eval.eval : env -> expr -> value Lwt.t`) |
| `common/` | Shared types |
| `migrations/` | Additive SQL |
| `image/` | Sole runtime binary |
| `libs/` | Lisp packages seeded to `system/main` |
| `scripts/` | `dev.sh`, `pp_eval.py`, OpenBSD deploy, `public_artifacts.sh` |
| `docs/` | Deploy notes + vendored `pricklypear-book.pdf` |
| `spikes/` | Historical probes only (awa-ssh, mirage_solo5) |

## Stack

- OCaml 5.4.1 on **`poohstack`**
- Lwt + `httpaf-lwt-unix` + `pgx_lwt`
- `pure-html` / `HTML.Hx.*`
- Interim password hash `sha256$salt$digest` (argon2 deferred)

## Build & dev

```bash
eval $(opam env --switch=poohstack --set-switch)
dune build @all && dune runtest --force

scripts/dev.sh start    # PG :5433 + image :18080 / :17878
```

## Public release (hosted instance)

| Path | What |
|---|---|
| `/welcome` | Landing (`layout/*` + `welcome/page`) |
| `/code` | Read-only source browser |
| `/book.pdf` | Literate book (vendored or `borge book print`) |
| `/src.tgz` | Pruned source tarball |
| `/login` | Workspace |

```bash
ksh scripts/public_artifacts.sh   # refresh PDF/tarball when tools allow
# deploy ships docs/pricklypear-book.pdf if borge is missing on the build host
```

## Agent eval

```bash
set -a; . ~/.config/pricklypear/env; set +a
python3 "$PP_EVAL" -j '(whoami)'
python3 "$PP_EVAL" -j '(load-library "todo")'
```

## Branches

- **`system/main`** — package defaults from `libs/*` at seed
- **`<user>/personal`** — garden overrides; survive reseed

## Spec workflow

Use borge (`plan` / `make` / `review`). Open issues that still matter
live under `.borge-bugs/` (quote compounds, AST nodes design, argon2,
schema-authority, optional SSH notes).

## Monitor

```bash
eval $(opam env --switch=poohstack --set-switch)
dune build @all && dune runtest --force
borge lint && borge drift && borge report | tail -5
```
