# Pricklypear

A multi-user Lisp OS habitat — the language is **Nopales** (a Scheme-family
Lisp-1): **one** OCaml image process, Postgres as the object store, htmx as
the interface. Humans and agents share the running system — the process **is** the API.

This is a dogfooded personal system that happens to be multi-user, not a
polished product suite. Design lives in a multi-file borge book
([`pricklypear.borg`](pricklypear.borg) + `borge/*.borg`). If code and
spec disagree, the code is wrong.

Copyright (c) 2026 Roerick Sweeney. All rights reserved.

## What you get

| | |
|---|---|
| Runtime | One unix binary (`image/`) + Postgres |
| Auth | In-process web login (`pp_session`) + HTTP Basic for agents |
| Code store | Branched `function_defs` with parent override (not PG inheritance) |
| Data store | Shared `data_rows` (JSON fields) across branches |
| UI | Server-rendered htmx; Nopales packages in `libs/` |
| Public face | `/welcome`, `/code`, `/book.pdf`, `/src.tgz`, `/.well-known/agent.json`, `/agent.txt` |

Dogfood packages seeded on boot include todo, notes/outline, calendar,
contacts, bookmarks, flashcards, bible/dabar, weather, AI helpers, and
the welcome/layout shell. Treat them as a living garden, not a frozen
SDK.

## Honest limitations (read before deploying)

- Password hashing is interim **`sha256$salt$digest`** (argon2 planned).
- First boot defaults bootstrap admin to **`admin` / `admin`** unless
  `PP_BOOTSTRAP_USER` / `PP_BOOTSTRAP_PASSWORD` are set. Change these
  before any network exposure.
- Optional TCP REPL (`PP_REPL_PORT`) has **no application auth** — bind
  loopback only (or tunnel). Do not expose it on a public interface.
- Session cookies are `HttpOnly; SameSite=Lax` (no `Secure` on plain
  HTTP). Put TLS on a reverse proxy for real use.
- CSRF tokens are not implemented yet; same-site cookie policy is the
  current mitigation.
- `PP_AUTH_DISABLED=1` skips the HTTP auth gate — **dev only**.

## Architecture

```
Browser / pp_eval / browser REPL
        │
        ▼
reverse proxy (TLS optional)
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

Multi-user means credentials + sessions + `<user>/personal` garden
branches inheriting `system/main`. Not a process fleet. (Older
multi-process / supervisor chapters remain in the design book as
history only.)

## Layout

| Path | Role |
|---|---|
| `lisp/` | Pure interpreter for **Nopales** (Lisp-1; `Eval.eval : env -> expr -> value Lwt.t`) |
| `common/` | Shared types |
| `migrations/` | Additive SQL applied on boot / deploy |
| `image/` | Sole runtime binary |
| `libs/` | Nopales packages seeded to `system/main` |
| `cli/` | OCaml laptop clients: `pp-eval`, `pp-sync` (no Python) |
| `nopales/` | Standalone Nopales script runner + REPL: pure interpreter, pure prims, file/argv/exit/load, optional offline lens reads — no image/DB/network ([`nopales/README.md`](nopales/README.md)) |
| `glochid/` | **glochid** — local-first agent harness: Nopales-native coding agent; Nopales htmx fragments + multipart/mixed streamed turns ([`borge/glochid.borg`](borge/glochid.borg)) |
| `scripts/` | `dev.sh`, `pp-eval.sh` wrappers, `verify_habitat.sh` (on-image `libs/verify`), seed ETL |
| `docs/` | **Living docs only**: [`one-tree.md`](docs/one-tree.md) (storage overview), [`one-substrate-tree.md`](docs/one-substrate-tree.md) (sequence: shared LLM lib -> turn contract -> monorepo), [`notes-ui-contract.md`](docs/notes-ui-contract.md), [`agent-ui-harness.md`](docs/agent-ui-harness.md) (agent FE verify research) |
| `archive/` | Executed handoffs / superseded plans / UI reviews / retired probes — receipts, not spec ([`archive/README.md`](archive/README.md)) |
| `borge/` | Design chapters |

## Stack

- OCaml 5.4.x on the **`poohstack`** opam switch
- Lwt + `httpaf-lwt-unix` + `pgx_lwt` (today; effects/direct-style transition: `archive/plans/runtime-effects-transition.md`)
- `pure-html` / `HTML.Hx.*` for type-safe markup
- Guest capability effects: `lisp/lib/effect.ml` (separate from host I/O runtime)

## Build & dev

```bash
eval $(opam env --switch=poohstack --set-switch)
dune build @all && dune runtest --force

scripts/dev.sh start    # PG :5433 + image :18080 / :17878
scripts/dev.sh status
scripts/dev.sh stop
```

### From a clean Linux box

Four blockers, in the order a fresh switch hits them:

1. **httpun drifts to 0.2.0.** `image/lib/http_agent.ml` matches the body
   writer's unit literals (`Closed` / `Written`), which 0.2.0 renamed, so the
   image stops compiling. Pin the pair first — see `scripts/opam-pins.txt`:
   `opam pin add httpun 0.1.0 -y` and `opam pin add httpun-eio 0.1.0 -y`.
2. **argon2 is dlsym'd out of the main program.** The binding resolves
   `argon2_type2string` at runtime; with only the shared library installed,
   boot dies `DL_error /pp/image undefined symbol argon2_type2string`. Run the
   image under `LD_PRELOAD=libargon2.so.1` (or link the main program with
   `-largon2`).
3. **pgx has no SCRAM.** The image's PG role authenticates over SASL and the
   connect fails. Set `password_encryption = 'md5'` in `postgresql.conf`,
   re-set the role password so it is stored as `md5`, and use `md5` (not
   `scram-sha-256`) for that role in `pg_hba.conf`.
4. **todo seeds before chat.** On a fresh DB every boot logs
   `BOOT SEED FAILURES (1): todo form 5 failed: unbound chat/lookbook-add`.
   `libs/todo` registers its ui vocabulary through the chat lookbook seam, so
   the seed must reach it by name (`chat/lookbook-add`) instead of forcing it
   while loading.

`dune runtest` pins all four (`tests/pp_clean_build_pins.ml`).

Useful checks against a running image:

```bash
ksh scripts/verify_auth.sh
ksh scripts/verify_habitat.sh   # product/budget suites on-image (libs/verify)
python3 scripts/auth_product_smoke.py   # auth HTTP only; prefer not to grow new .py smokes
```

## Agent eval (against a live image)

Local eval means the `pp-eval` client runs on your laptop and talks to
`PP_URL` (prod by default, from `~/.config/pricklypear/env`). It does **not**
mean local Postgres — `scripts/dev.sh` (local PG + image) is the kernel-lab
for OCaml work, not the default eval path.

OCaml CLI (no Python). Build once: `dune build cli/bin/pp_eval.exe`.

```bash
set -a; . ~/.config/pricklypear/env; set +a   # local secrets, mode 600
export PP_EVAL=$PWD/_build/default/cli/bin/pp_eval.exe
"$PP_EVAL" -j '(whoami)'
scripts/pp-eval.sh -j '(load-library "todo")'
```

## Branches

- **`system/main`** — package defaults from `libs/*` at seed (read-mostly)
- **`<user>/personal`** — garden overrides; survive reseed/deploy

## Public artifacts

```bash
ksh scripts/public_artifacts.sh   # refresh book.pdf + src.tgz when tools allow
```

| Path | What |
|---|---|
| `/welcome` | Landing |
| `/code` | Read-only source browser |
| `/book.pdf` | Literate design book |
| `/src.tgz` | Pruned source tarball |
| `/login` | Workspace login |

## Spec workflow

Use borge (`plan` / `make` / `review`) when changing design-backed
behavior. Planned product gaps include type-system work, branch promote
UX, notes roam, dabar FTS polish, AI review-merge UI, and argon2.

## Monitor

```bash
eval $(opam env --switch=poohstack --set-switch)
dune build @all && dune runtest --force
borge lint && borge drift && borge report | tail -5
```
