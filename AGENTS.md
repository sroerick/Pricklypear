# Pricklypear — Agent Notes

Pricklypear is a multi-user Lisp OS — the language is **Nopales** (a
Scheme-family Lisp-1): one unix image process, Postgres as the object
store, htmx as the interface layer. **The authoritative design is the multi-file borge book:** root [`pricklypear.borg`](pricklypear.borg) inlines chapter files (`borge/philosophy.borg`, `borge/single-image.borg`, `borge/lisp-interpreter.borg`, `borge/ai-authoring.borg`, …). Read them in root inline order (literate book order). If code and spec
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
`borge/single-image.borg`). Open borge issues that still matter live in
`.borge-bugs/` (quote compounds, AST nodes design, schema-authority,
argon2, optional SSH). Monitor command: bottom of `README.md`.

## Live image eval (laptop / pi agent)

**Entry point for agents:** the public bootstrap docs on the running host — not this file as the full playbook.

1. **Fetch** `GET /agent.txt` and `GET /.well-known/agent.json` (no auth).
   Prod: `https://pricklypear.rocks/agent.txt`
   Local: `http://127.0.0.1:18080/agent.txt`
2. Those documents explain **how to build a REPL client**: HTTP Basic + `POST /api/eval` with JSON `{"expr":"…"}`, plus curl/Python examples and discovery prims.
3. After auth, read the **living playbook** on the habitat:
   `(load-library "notes")` then `(notes/doc/get "agents")`.
4. Explore selectively: `(list-packages)` → `(describe "todo")` → `(apropos-docs "todo/")` → `(function-doc "…")`.

### Live `agents` note (edit this — it is the habitat AGENTS)

The durable agent guide is the **notes doc** with slug `agents`, not only
this git file. Agents **should read and update it** when they learn
something durable about how to work the habitat (workflows, promote/
seed policy, package gotchas, UI contracts).

```lisp
(load-library "notes")
(notes/doc/get "agents")   ; (id slug title outline tags kind day)
;; Upsert signature: (notes/doc/upsert slug title outline tags kind day)
;; Prefer edit via browser /notes?slug=agents or read-modify-upsert:
;;   keep title/outline from get; change outline nodes; upsert back.
(notes/doc/upsert "agents" title outline tags kind day)
```

Browser: `/notes?slug=agents`. Same note is advertised in `/agent.txt`
and `/.well-known/agent.json`. **Agents may and should edit this note**
when they learn durable habitat conventions — that is the intended
feedback loop (not a human-only doc). Prefer writing agent lore **there**
(Postgres on the habitat) over only editing git `AGENTS.md`. Git
`AGENTS.md` is the laptop/repo mirror for humans and offline agents;
keep the two roughly aligned when you change authority/workflow rules.

Also: `(notes/doc/get "wiki-schema")` for the notes/wiki shape.

From this machine (after bootstrap + secrets):

```bash
set -a; . ~/.config/pricklypear/env; set +a
export PP_EVAL="${PP_EVAL:-$HOME/dev/wyo.tech/pricklypear/_build/default/cli/bin/pp_eval.exe}"
# Or: curl the public docs first
curl -sS "$PP_URL/agent.txt" | head
"$PP_EVAL" -j '(whoami)'
"$PP_EVAL" -j '(list-packages)'
"$PP_EVAL" -j '(load-library "todo")'
"$PP_EVAL" -j '(describe "todo")'
"$PP_EVAL" -j '(load-library "notes")'
"$PP_EVAL" -j '(notes/doc/get "agents")'
```

- Client: OCaml **`pp-eval`** → `POST /api/eval` (HTTP basic auth). Wrapper: `scripts/pp-eval.sh`
- Secrets: `~/.config/pricklypear/env` (local only, mode 600 — never commit)
- Pi skill: `pricklypear-eval` (`~/.pi/agent/skills/pricklypear-eval/SKILL.md`)
- Prefer eval over inventing REST for todos/bible/ui/etc.
- Do **not** dump `(list-bindings)` into model context; use describe / apropos-docs.
- **Do** edit the live `agents` note when agent guidance should stick.

## Branches: defaults vs personal garden

**Authority (branching.package-updates / seed-policy):**
- Kernel (OCaml/migrations) → git is source of truth.
- Package Nopales → Postgres branches are live source; git is a **vault**.
- Boot seed is **additive** into `system/main` only (fills missing names
  from `libs/*.pp`; never overwrites existing main rows after promote).
- Explicit `(reseed-package "notes")` / `pp-sync reseed notes` (admin)
  is the only intentional git→main clobber.
- Daily product Nopales: work on personal/dev on the habitat → **promote**
  to main → optional `pp-sync snapshot` into the repo.
- Local `scripts/dev.sh` PG is kernel-lab only; agents default to prod
  via `~/.config/pricklypear/env`.

- **`system/main`** — package defaults (inheritance root). Boot seed
  additively loads `libs/*` here. Promote target for shared defaults.
- **`<user>/personal`** — garden. Image boots here after seed. Empty
  child of main. Your `(define …)` / `(route …)` pins survive deploy.
- **`<user>/dev`** — optional workshop (empty child); product spikes
  you intend to promote (keep long-lived custom pins on personal).
- Switch: `(checkout "system/main")` / `(checkout "admin/personal")`.
- Explicit `(load-library "ui")` **upserts the current branch** — on
  personal that pins the package. Prefer small defines or promote.
- Vault: `pp-sync pull <pkg>`, `pp-sync pull-all`,
  `pp-sync snapshot [--branch system/main] [--out backups/…]`.
  `libs/` = main vault (seed input); `backups/<ref>/` = non-main dumps.

## One-tree storage (default)

Postgres still holds bytes, but the **logical store is ONE TREE**
(`tree_nodes` + `tree_branches`, migration `009_tree.sql`). Spec:
`borge/one-tree.borg`. Overview: `docs/one-tree.md`.

- **Paths:** `b.<branch>.{fn,type,route}.<label>` · `data.<type>.<id>`
- **Default OFF (opt-in):** `PP_TREE_READS` / `PP_TREE_WRITES` (set `=1` to exercise tree path; tables + forward mirror remain source of truth for prims until verified)
- **Mirrors:** deferred triggers keep scaffolding tables
  (`function_defs` / `type_defs` / `view_bindings` / `data_rows`) in
  lockstep with the tree (bidirectional, recursion-guarded).
- **Fork:** empty lens (parent pointer only); inheritance via CTE.
- **Typed boundary:** declared types require all fields on create/update.
  Prims: `(type-fields 't)`, `(node-path 't id)`, `(backlinks needle)`.
- **Verify:** `ksh scripts/verify_one_tree.sh` (needs `scripts/dev.sh start`).
- **Module:** `image/lib/tree_store.ml`

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
  Nopales route handlers.
- `alcotest` for tests, `yojson` for JSON (serialized Nopales ASTs are
  JSONB; dict prims cover field-level access on JSON strings).
- Eio only for pure-unix dev tooling if any — NOT inside the image (H3 deferred; shipping rim is Io_lwt — see docs/runtime-effects-transition.md).
- Optional line-protocol REPL on `PP_REPL_PORT`; primary interactive
  surfaces are browser `/repl` and `POST /api/eval`.

The pure libraries (`lisp/`, `common/`) have no service deps and can be
  tested without Postgres. Everyone uses the poohstack switch in practice.

## Layout

Per spec `single-image.topology`. The order of build dependency is:
`lisp/` -> `common/` -> `migrations/` -> `image/` (single-image v1).
Supervisor/authd packages were retired by the single-image pivot;
auth is **in-process** in the image.

- `lisp/` — pure interpreter for Nopales (no I/O). Testable with no services.
- `common/` — pure shared types (incl. `auth_types.ml`).
- `migrations/` — SQL applied by `scripts/dev.sh` (and deploy hooks).
- `image/` — the single unix binary: Nopales, REPL, htmx, Postgres, **auth**.
  Auth modules: `image/lib/auth_password.ml`, `image/lib/auth_store.ml`.
- `scripts/` — dev orchestration. `scripts/dev.sh start` brings up PG
  (initdb if needed) + the image; `stop`/`restart`/`status`/`clean`
  manage them. See `scripts/dev.sh` header for env-var knobs.

## Auth (single-image)

- Browser: `GET/POST /login`, `POST /logout`, cookie `pp_session` (HttpOnly).
- Middleware hard-public: `/login`, `/health`, `/logout`, plus public
  release face `/welcome`, `/book.pdf`, `/src.tgz`, `/code`,
  `/code/*`, `/assets/*`, `/.well-known/agent.json`, `/agent.txt`.
  Else session required. Anon `/` → `/welcome`.
  (`/agent` is the private HTML agent portal — not the public doc.)
  (`/book` HTML explainer is private — not hard-public.)
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
  `/book.pdf` + `/src.tgz` (from `PP_PUBLIC_DIR`; build with
  `scripts/public_artifacts.sh`), `/assets/cm/*` (vendored CM5),
  `/.well-known/agent.json` + `/agent.txt` (agent bootstrap: how to
  auth + call `POST /api/eval`; no secrets). Kernel module:
  `image/lib/agent_discovery.ml`.
  (`/book` is not part of the public release face; `/agent` is private.)
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
- Nopales primitives: pure (`+ - * = < > <= >= not list car cdr cons
  null? pair? count string-append str type-of whoami current-author
  list-bindings dict-get dict-set dict-keys render-html`), DB-
  touching control (`fork merge route checkout`), admin auth
  (`is-admin?`; `create-user` username password [is-admin] — admin
  only, mirrors `/admin/users`), type declaration (`deftype`), and
  CRUD over `data_rows` (`get create update delete rows`). DB-touching prims are NORMAL prim_fns returning `value
  Lwt.t` (post-iteration-12 refactor); they compose — e.g.
  `(define u (get 'user id))` then `(str u)` returns the bound DB
  row. The list-rows prim is named `rows`, NOT `list` (the latter
  is the pure VList-constructor). Dict prims operate on the VStr
  JSON returned by `(get 'type id)` so Nopales code can do field-level
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

## Conventions

- OCaml / dune, single unix image process for v1 (Solo5 optional later —
  `spikes/mirage_solo5/`, not the product path)
- Lwt for image I/O (the it12 refactor made `Eval.eval` itself
  `value Lwt.t` — pure prims wrap results in `Lwt.return` via a
  `register_sync` helper at registration time). **Long-term:** direct-style
  core via OCaml 5 effects, Lwt only at the rim — see
  [`docs/runtime-effects-transition.md`](docs/runtime-effects-transition.md)
  (slow, opportunistic path; not a freeze on product work).
- `pure-html` for HTML generation; use `HTML.Hx.*` not raw htmx attribute
  strings (poohstack rule, preserved here via pure-html). Use
  `txt ~raw:true "%s" s` for raw-HTML injection (escape-bypass path);
  there is NO `Pure_html.of_string` — pure-html is tree-only.
- Agent FE verify: tiered checks (wget → workflow/smoke → screenshot+vision),
  a11y snapshots over CSS selectors, graduate green flows into smokes.
  Research wiki: [`docs/agent-ui-harness.md`](docs/agent-ui-harness.md).
  Example contract: [`docs/notes-ui-contract.md`](docs/notes-ui-contract.md).
- `httpaf-lwt-unix` for HTTP; `pgx_lwt` + custom Io_unix for Postgres (no ORM)
- `pgx_lwt` + custom `Io_unix` shim for Postgres in v1 (NOT
  `pgx_lwt_mirage`; v1 uses POSIX-flavored libs. `pgx_lwt_mirage` IS
  installed v2.2 but needs a real `Tcpip.Stack.V4V6` device). No ORM,
  no inheritance, plain SQL. SERIALIZABLE transactions by default for
  mutations — `Store.with_transaction` (it16) wraps pgx's native
  functor; the `(with-transaction (lambda () body))` prim (it29) gives
  Nopales code a session-level transaction scope so `(select-for-update ...)`
  row locks persist across subsequent prims in the body.
- Additive-only schema migrations; the migration runner uses
  `split_sql_statements` (handles dollar-quoted `$$...$$` blocks,
  single-quoted strings, `--` line comments, multi-statement splits —
  the pgx prepared-statement protocol can't do multi-statement natively).
- The `.borg` file is the source of truth. During a `make` phase, file a
  `borge issue new "..."` for any spec gap surfaced — the next `plan`
  pass resolves issues. Use borge to plan/make/review changes — never
  edit `.borg` directly in make/task mode.
