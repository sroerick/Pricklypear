# Pricklypear — Agent Notes

Pricklypear is a multi-user Lisp OS: one unix image process, Postgres as
the object store, htmx as the interface layer. **The authoritative design is the multi-file borge book:** root [`pricklypear.borg`](pricklypear.borg) inlines chapter files (`philosophy.borg`, `single-image.borg`, `lisp-interpreter.borg`, `ai-authoring.borg`, …). Read them in root inline order (literate book order). If code and spec
disagree, code is wrong. Use borge to plan/make/review changes — don't
edit the spec freely during a `make` phase, file a `borge issue new`
instead.

## Status (2026-07-12 reconcile)

**Single-image v1 habitat is the running system.** Spec is a multi-file
literate book (chapter `.borg` files + root). `borge report` after the
status-reconcile pass is roughly **55 implemented / 10 planned /
9 deprecated (~74%)** — planned items are real gaps (roam, promote UX,
type-system, review-merge, argon2, full FTS), not stale foundation work.
Packages on poohstack; build/tests on lisp + common + image. Product
libs (notes/todo/outline/ui/bible/dabar/ai/…) seed to `system/main`.
Supervisor/authd multi-process topology is retired (see
`single-image.borg`). Open borge issues that still matter live in
`.borge-bugs/` (quote compounds, AST nodes design, schema-authority,
argon2, optional SSH). Monitor command: bottom of `README.md`.

## Live image eval (laptop / pi agent)

The running image **is** the API. From this machine:

```bash
set -a; . ~/.config/pricklypear/env; set +a
python3 "$PP_EVAL" -j '(whoami)'
python3 "$PP_EVAL" -j '(load-library "todo")'
```

- Client: `scripts/pp_eval.py` → `POST /api/eval` (HTTP basic auth)
- Secrets: `~/.config/pricklypear/env` (local only, mode 600 — never commit)
- Pi skill: `pricklypear-eval` (`~/.pi/agent/skills/pricklypear-eval/SKILL.md`)
- Prefer eval over inventing REST for todos/bible/ui/etc.

## Branches: defaults vs personal garden

- **`system/main`** — package defaults. Boot `seed_stdlib` always writes
  `libs/{std,weather,ui,layout,welcome,ai,outline,notes,todo,contacts,bookmarks,calendar,bible,dabar}` here from git. Treat as read-mostly.
- **`<user>/personal`** — your garden. Image boots here after seed. Empty
  child of main (inherits via recursive CTE). Your `(define …)` and
  `(route …)` land here and **survive reseed/deploy**.
- Switch: `(checkout "system/main")` / `(checkout "dev/personal")`
  (owner is `PP_USERNAME`). Header shows current branch.
- Explicit `(load-library "ui")` writes the **current** branch — on
  personal that pins full package overrides (main updates won't show for
  those names until you delete the override or merge).
- `pp_sync pull` exports the active branch. Prefer editing on personal;
  pull when you want your garden in git.

## Stack

- OCaml 5.4.1 on the **`poohstack`** opam switch (NOT a dedicated
  `pricklypear` switch). `eval $(opam env --switch=poohstack --set-switch)`
  before building any package.
- Lwt for image I/O (`httpaf-lwt-unix`, `pgx_lwt` + custom `Io_unix` shim).
- Single unix binary (`image/`) — not a Solo5 unikernel in v1. Solo5 remains
  an optional later hardening axis (`spikes/mirage_solo5/`), not the product path.
- Auth is in-process (`image/lib/auth_*.ml`). Password hashes are interim
  `sha256$salt$digest` until argon2 (BORGE-1783831698).
- `pure-html` for type-safe HTML + `HTML.Hx.*` htmx attributes. Use
  `txt ~raw:true "%s" s` to bypass escaping for raw-HTML injection from
  Lisp route handlers.
- `alcotest` for tests, `yojson` for JSON (serialized Lisp ASTs are
  JSONB; dict prims cover field-level access on JSON strings).
- Eio only for pure-unix dev tooling if any — NOT inside the image.
- Optional line-protocol REPL on `PP_REPL_PORT`; primary interactive
  surfaces are browser `/repl` and `POST /api/eval`.

The pure libraries (`lisp/`, `common/`) have no service deps and can be
  tested without Postgres. Everyone uses the poohstack switch in practice.

## Layout

Per spec `architecture.package-layout`. The order of build dependency is:
`lisp/` -> `common/` -> `migrations/` -> `image/` (single-image v1).
Supervisor/authd packages were retired by the single-image pivot;
auth is **in-process** in the image (Option B / auth.web-login).

- `lisp/` — pure interpreter (no I/O). Testable with no services.
- `common/` — pure shared types (incl. `auth_types.ml`).
- `migrations/` — SQL applied by `scripts/dev.sh` (and deploy hooks).
- `image/` — the single unix binary: Lisp, REPL, htmx, Postgres, **auth**.
  Auth modules: `image/lib/auth_password.ml`, `image/lib/auth_store.ml`.
- `scripts/` — dev orchestration. `scripts/dev.sh start` brings up PG
  (initdb if needed) + the image; `stop`/`restart`/`status`/`clean`
  manage them. See `scripts/dev.sh` header for env-var knobs.

## Auth (single-image)

- Browser: `GET/POST /login`, `POST /logout`, cookie `pp_session` (HttpOnly).
- Middleware hard-public: `/login`, `/health`, `/logout`, plus public
  release face `/welcome`, `/book`, `/book.pdf`, `/src.tgz`, `/code`,
  `/code/*`, `/assets/*`. Else session required. Anon `/` → `/welcome`.
- Admin: `GET/POST /admin/users` (create user, set password, revoke sessions).
- Bootstrap: `PP_BOOTSTRAP_USER` / `PP_BOOTSTRAP_PASSWORD` (default admin/admin)
  when users empty, or when admin has no password credential yet.
- Agents/`pp_eval`: HTTP Basic (username/password against credentials table)
  or session cookie. nginx htpasswd is emergency edge only, not the user DB.
- Escape hatch: `PP_AUTH_DISABLED=1` skips HTTP auth gate (dev only).
- Password hash interim: `sha256$salt_hex$digest_hex` (argon2 deferred —
  BORGE-1783831698). Verify: `ksh scripts/verify_auth.sh`.

## Workflow

- Build: `dune build` at the root (each package also builds standalone)
- Test: `dune runtest` (lisp/ and common/ tests need no services;
  image tests need the switch; auth smoke needs PG + image)
- Dev stack: `scripts/dev.sh start` brings up PG + image on ports
  5433 (PG) / 18080 (HTTP) / 17878 (REPL). Logs in `/tmp/pp-dev/`.
- Public release face (no login): `/welcome` (layout/* landing),
  `/code` (read-only CodeMirror tree over `PP_SOURCE_ROOT`),
  `/book` + `/book.pdf` + `/src.tgz` (from `PP_PUBLIC_DIR`; build with
  `scripts/public_artifacts.sh`), `/assets/cm/*` (vendored CM5).
  Packages: `libs/layout`, `libs/welcome`.
- The image's HTTP UI exposes system pages: `/`, `/login`, `/health`,
  `/admin/users`, `/branches` (list all branches), `/bindings` (list
  visible function_defs on the current branch), `/binding/<name>`
  (pretty-print one function_def's AST), `/types` (list visible
  type_defs on the current branch — fields shown comma-separated),
  `/data` (generic data_rows browser: list types + counts, browse
  `/data/<type>`, create/edit/delete rows at `/data/<type>/…`).
  `/inspect/row/<type>/<id>` redirects to `/data/<type>/<id>`.
  User routes via `(route "/x" "fn")` (private by default) or
  `(route "/x" "fn" 'public)` / `(public-route …)` for anonymous GET.
  Kernel paths (`/data`, `/admin`, `/api/eval`, …) stay hard-private.
  Anonymous identity uses `system/main` (package public routes inherit).
- Lisp primitives: pure (`+ - * = < > <= >= not list car cdr cons
  null? pair? count string-append str type-of whoami current-author
  list-bindings dict-get dict-set dict-keys render-html`), DB-
  touching control (`fork merge route checkout`), type declaration
  (`deftype`), and CRUD over `data_rows` (`get create update delete
  rows`). DB-touching prims are NORMAL prim_fns returning `value
  Lwt.t` (post-iteration-12 refactor); they compose — e.g.
  `(define u (get 'user id))` then `(str u)` returns the bound DB
  row. The list-rows prim is named `rows`, NOT `list` (the latter
  is the pure VList-constructor). Dict prims operate on the VStr
  JSON returned by `(get 'type id)` so Lisp code can do field-level
  access (`(dict-get u "name")`) WITHOUT a new value type — v1
  implementation choice, accept pre-formalization. `(deftype 'name
  'field1 'field2 ...)` registers a type on the current branch
  (all args are quoted symbols → VStr); data rows go in the shared
  `data_rows` table keyed by (type_name, id). `(render-html "<b>...")`
  tags the returned VStr so http.ml renders it RAW (unescaped) in
  the route handler output; plain VStrs render as bare text in `<p>`.
- Any spec change during `make`: `borge issue new "..."`, don't edit
  `.borg` directly. The next `plan` pass resolves issues.
- After every code change touching a `.borg`-declared module: `borge
  drift` to verify spec/code alignment.

## Conventions (mirrors the spec's `workflow.conventions`)

- OCaml / dune, `mirage/unix` processes for v1 (Solo5 target deferred to
  v1.5 per the Phase 1 probe at `spikes/mirage_solo5/`)
- Lwt for all unikernel I/O (the it12 refactor made `Eval.eval` itself
  `value Lwt.t` — pure prims wrap results in `Lwt.return` via a
  `register_sync` helper at registration time)
- `pure-html` for HTML generation; use `HTML.Hx.*` not raw htmx attribute
  strings (poohstack rule, preserved here via pure-html). Use
  `txt ~raw:true "%s" s` for raw-HTML injection (escape-bypass path);
  there is NO `Pure_html.of_string` — pure-html is tree-only.
- `httpaf-lwt-unix` direct for HTTP (NOT `httpaf-mirage` — that package
  doesn't exist on opam; the spec's transport-choices text is inaccurate,
  see BORGE-1783439434. The future Mirage path is `gluten-mirage` +
  `httpaf`, not pre-installed on poohstack)
- `pgx_lwt` + custom `Io_unix` shim for Postgres in v1 (NOT
  `pgx_lwt_mirage`; v1 uses POSIX-flavored libs. `pgx_lwt_mirage` IS
  installed v2.2 but needs a real `Tcpip.Stack.V4V6` device). No ORM,
  no inheritance, plain SQL. SERIALIZABLE transactions by default for
  mutations — `Store.with_transaction` (it16) wraps pgx's native
  functor; the `(with-transaction (lambda () body))` prim (it29) gives
  Lisp code a session-level transaction scope so `(select-for-update ...)`
  row locks persist across subsequent prims in the body.
- Additive-only schema migrations; the supervisor's migration runner uses
  `split_sql_statements` (handles dollar-quoted `$$...$$` blocks,
  single-quoted strings, `--` line comments, multi-statement splits —
  the pgx prepared-statement protocol can't do multi-statement natively).
- The `.borg` file is the source of truth. During a `make` phase, file a
  `borge issue new "..."` for any spec gap surfaced — the next `plan`
  pass resolves issues. Use borge to plan/make/review changes — never
  edit `.borg` directly in make/task mode.
