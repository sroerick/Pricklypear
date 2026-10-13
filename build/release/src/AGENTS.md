# Pricklypear — Agent Notes

Pricklypear is a multi-user Lisp OS — the language is **Nopales** (a
Scheme-family Lisp-1): one unix image process, Postgres as the object
store, htmx as the interface layer. **The authoritative design is the multi-file borge book:** root [`pricklypear.borg`](pricklypear.borg) inlines chapter files (`borge/philosophy.borg`, `borge/single-image.borg`, `borge/lisp-interpreter.borg`, `borge/ai-authoring.borg`, …). Read them in root inline order (literate book order). If code and spec
disagree, code is wrong. Use borge to plan/make/review changes. File a
`borge issue new` ONLY when (a) you're mid-`make` and discover the spec
must change (spec is locked then), or (b) you find a genuine problem
outside the scope of your current instructions. In `plan` phase or spec
discussions, edit the spec directly or just talk — do NOT file issues.

**Naming (locked 2026-10-01, borge/filmstrip.borg):** **filmstrip** = the
window-manager interface / shell (cascade + pane strips, workspace state) —
habitat (`libs/filmstrip`, `/filmstrip`) and embedded in the harness.
**glochid** = strictly the local agent harness (laptop binary, `glochid/`).
The harness's screen IS a filmstrip; say "filmstrip" for window management.

## Status (2026-08-31 reconcile)

**Orchestration (2026-09-25):** the work-triage board became a real delivery
pipeline — spec `borge/workstreams.borg` (registered in the book after
work-triage). Workstream = a todo-list (glochid `fa93a1af`, Board `0b21ffaa`,
…), same column taxonomy everywhere; the habitat is the **record + queue**, the
laptop is the **executor** (scripts/pp-flow.sh + pi ralph/subagent claim cards
via the brief protocol; no agents in the prod image). List deletion is allowed
(`todo/delete-list`; default-Inbox guarded) — the old 'never delete' rule was
retired 2026-09-25 after a 365→9 list cleanup (snapshot
`backups/pre-cleanup-20260925-005600.json`).

**Unattended executor loop (2026-09-27).** `scripts/pp-flow.sh --drain
[list-id] [N]` picks its own work (queue order, TODO/CODE, unleased) and runs
each card as a separate `--run` subprocess — that separation is what keeps an
AUTO-lane LOGIC-REVIEW reviewer independent of the implementer. The runner for
headless runs is the Autolith adapter, pinned to Synthetic:

```sh
set -a; . ~/.config/pricklypear/env; set +a
export PP_FLOW_PI="$PWD/scripts/pp-flow-autolith.sh"
export AUTOLITH_MODEL=syn:large:text        # default; the adapter assumes it
scripts/pp-flow.sh --drain 6f625d8c-fb5f-485c-b5c7-f2f4c41772cc --states TODO --max 1
```

Knobs: `--states "TODO,CODE"`, `--exclude-kinds BUG`, `--max N`,
`PP_FLOW_DRAIN_CONTINUE=1` (don't stop on the first failure), `--force`
(reclaim a live lease), `PP_FLOW_AUTOLITH_PERM=full` (cards that must touch
the repo). The adapter refuses, in <1s with the accepted values named, a model
that rejects the executor role's `:reasoning-effort :medium` (the Synthetic
glm-flash family accepts only low|high|max) — it dies as a cryptic
`:PROCESS-FAILURE` otherwise. `syn:large:text` is the tested default. The
failure line reports the provider category + message verbatim. NOTE
(2026-09-27): one run hit a transient Synthetic HTTP 402 (“Subscription access
is currently blocked…”); immediate retries of the same model+key succeeded and
direct curl never failed, so that was upstream flakiness — do NOT read a lone
402 as an exhausted account. The adapter now RETRIES exactly that class of
fault (HTTP 402/408/409/425/429/5xx, plus a run-job crash with no result):
2 attempts by default, 10s backoff doubling to a 300s cap
(`PP_FLOW_AUTOLITH_RETRIES`, `PP_FLOW_AUTOLITH_RETRY_BASE`). Deterministic
failures — `:PROCESS-FAILURE`, `:INVALID-*`, `:TIMED-OUT`, a 401/403/404 — run
exactly ONCE, so a retry can never burn a card's lease repeatedly on the same
guaranteed failure. An agent-named `DESTINATION:` is validated
against the card's own column set before it is obeyed; an unknown column falls
back to the one-column advance. Per-run ledger: `.ralph/runs.jsonl` (+ `RUN
SUMMARY` lines).

**Single-image v1 habitat is the running system.** Spec is a multi-file
literate book (chapter `.borg` files + root). `borge report` after the
habitat-book reconcile is roughly **268 implemented / 53 planned /
3 deprecated (~83%)** — planned items are real gaps (roam, promote UX,
type-system, review-merge, full FTS, habitat chapter editing, live
OCaml eval), not stale foundation work. The spec-as-book pipeline
(habitat-book chapter) is implemented end to end: borge book
print/export over the inline walk, `scripts/load-book.sh` seeding,
`/book` + `/book/chapter/<slug>` + `/book/course/<slug>` habitat faces
(libs/book), restricted `example-eval` prim + `book/verify`
(drift-as-rendering), first course `course/cs-interpreters.borg`, and
release artifacts (book.pdf workshop + reader flavor + export JSON).
Packages on poohstack; build/tests on lisp + common + image. Product
libs (notes/todo/outline/ui/bible/dabar/ai/…) seed to `system/main`.
Supervisor/authd multi-process topology is retired (see
`borge/single-image.borg`). Open borge issues that still matter live in
`.borge-bugs/` (quote compounds, AST nodes design, schema-authority,
optional SSH). Monitor command: bottom of `README.md`.

### 2026-09-14 promote incident (gate vs boot floor)

Two promotes (20347ee, ca2bc21) rolled back despite healthy binaries:
gregor's town-p1 moved `seed_agents_note` into `boot_personal`, and its
existence probe — even the "cheap" `notes/doc/row` form (40s live) —
pays prod's notes tree floor before `/health` answers; boot-personal
hit 78-90s against a 30s gate. Fix (bede450): the agents-note seed is
**async** — a Boot.start_eio fiber with its own store+session (jobs-
runner pattern, same identity/branch); boot_personal no longer calls
it. Health gate raised 30s→120s (`scripts/openbsd/pricklypear-promote.ksh`).
Two standing constraints: **nothing that touches the notes tree eval
path may run synchronously before /health serves**, and the promote
hook self-updates the installed promote script only AFTER a success —
so a gate fix can never rescue its own first deploy. Rollback restarts
can also race EADDRINUSE with a slow old image (rc_stop's port-free
poll is bounded at 10s+2s).

### 2026-08-21 session model (perf)

Shared-session flip stall is fixed on prod (commits bf2e808, 74699ba):
per-identity `Session_registry` (one `Session.t` per identity, own pgx
conn; anonymous parked on `system/main`), env loading is VThunk-lazy
(`load_defs` binds recipes; bodies force once on first touch,
memoized incl. failures — defs are recipes, never load-time hooks),
staleness stamp at attach, `function_stat` ring-flushed (30s). Specs:
`borge/image.borg` lazy-env-load / per-identity-sessions /
function-stat-ring; `borge/lisp-interpreter.borg` lazy-env-thunks
(user surface: `(delay)` / `(force)`). Runtime now needs the OpenBSD
`argon2` package (first argon2id deploy). Known remaining floors,
filed in `/issues`: `notes/doc/get` ~80s on big docs; argon2id verify
per Basic-auth request (~0.5-1s/request). Live details: notes doc
`agents` → "Sessions & lazy env (2026-08-21)".

### 2026-09-08 perf batch (four log/issues, deploys 81c0606 + fe78dbe)

`function_stat` telemetry LEFT the object tree (migration 031):
dedicated table, plain INSERT flush, set-based retention (newest 1000).
TELEMETRY IS NOT CONTENT — never store it as tree datums, and never
bulk-delete tree datum rows per-row on prod (~1s/row: mirrors + RLS +
versions; a 54k-row delete was a 25-minute transaction that took prod
down — the rc.d health probe now has a 5s timeout so the watchdog
cannot hang). Also in the batch: `tree/note-index-full` (real outlines
for scrapes; the plain index stays light for the /notes UI), a
content-keyed JSON parse memo under the dict prims, one grouped COUNT
for `/data`, `branch_status` from live tree rows + a def_gen-SUM stamp
cache (`/branches` warm ~1.3s), and the load-library skip map
fingerprinting chain-effective bodies (+ begin-wrapped top-level
defines persist). Details: notes doc `agents` → "Perf batch + incident
(2026-09-08)".

## Live image eval (laptop / pi agent)

**Entry point for agents:** the public bootstrap docs on the running host — not this file as the full playbook.

1. **Fetch** `GET /agent.txt` and `GET /.well-known/agent.json` (no auth).
   Prod: `https://pricklypear.rocks/agent.txt`
   Local: `http://127.0.0.1:18080/agent.txt`
2. Those documents explain **how to build a REPL client**: HTTP Basic + `POST /api/eval` with JSON `{"expr":"…"}`, plus curl/Python examples and discovery prims.
3. After auth, read the **living playbook** on the habitat:
   `(notes/doc/get "agents")` (package names bind lazily — no load step).
4. Explore selectively: `(list-packages)` → `(describe "todo")` → `(apropos-docs "todo/")` → `(function-doc "…")`.

**`load-library` is a deploy tool, not a session step.** Package
functions bind LAZILY from the branch chain — call them directly.
`(load-library "pkg")` re-evals the RELEASE file onto the current
branch (warns + pins on non-main — i-1790493758); never in repair
scripts or route handlers (rb dec-2).

### Live `agents` note (edit this — it is the habitat AGENTS)

The durable agent guide is the **notes doc** with slug `agents`, not only
this git file. Agents **should read and update it** when they learn
something durable about how to work the habitat (workflows, promote/
seed policy, package gotchas, UI contracts).

```lisp
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

### Local-first dev loop (laptop lens, no per-call HTTP)

For Nopales development on this device, prefer the local lens over
`pp-eval` round trips: `scripts/pp-local.sh` (committed 2026-09-11)
wraps `pp-remote` with the repo `lens/` directory (gitignored working
copy) always wired. Seed once (`pull-data user`, `pull-tree <entry>`),
then defs + `(get …)`/`(tree/get …)` reads and pure compute resolve
locally (~20-30ms, works offline); `rows`/writes/transactions still
cross the wire (server is index + write authority). `sync` runs one
three-way data round; `sync-loop` runs it in the background; `serve`
adds the local chat/eval surface. Full lore: habitat `agents` note,
section **Local-first dev loop on a laptop (pp-local)**; sync 413
round-trip fix + open gaps: borge issue `i-1789178161-87d0742f` in
`log/issues`.

### Session ritual (habitat-workflow)

Live authority: agents note section **Session ritual (habitat-workflow)**
plus `log/issues`, `log/sessions`, and per-package `spec/<pkg>` notes
docs (seeded: `spec/todo`). Spec chapter: `borge/habitat-workflow.borg`.

1. **Work on your branch** — define/route on `<user>/personal` or dev, not
   directly on `system/main`. Promote is the gate.
2. **Obituary rule** — no silent reversals; before replacing an
   implementation, write what/why in `spec/<pkg>` DECISIONS.
3. **File, don't fix** — out-of-scope items → the `log/issues` register
   (notes doc slug `log/issues`, kind `issue`). Three ways in: browser
   `/issues` (dedicated face: Kind BUG/DESIGN/SMELL/CLEANUP + title +
   body, per-row resolve/reopen/delete), shell
   `scripts/file-issue.sh "BUG: title" "body"`, or REPL
   `(notes/issues/add KIND TITLE BODY)` (+ `notes/issues/resolve` |
   `reopen` | `delete`). Sparse one-liners still fine. This is the
   **habitat product track**; `borge issue new` is the separate
   git/spec track for make-mode spec drift — don't conflate them.
4. **Handoff** — end of session ~3 lines → `log/sessions`.

From this machine (after bootstrap + secrets):

```bash
set -a; . ~/.config/pricklypear/env; set +a
export PP_EVAL="${PP_EVAL:-$HOME/dev/wyo.tech/pricklypear/_build/default/cli/bin/pp_eval.exe}"
# Or: curl the public docs first
curl -sS "$PP_URL/agent.txt" | head
"$PP_EVAL" -j '(whoami)'
"$PP_EVAL" -j '(list-packages)'
"$PP_EVAL" -j '(describe "todo")'
"$PP_EVAL" -j '(notes/doc/get "agents")'
```

- Client: OCaml **`pp-eval`** → `POST /api/eval` (HTTP basic auth). Wrapper: `scripts/pp-eval.sh`
- Secrets: `~/.config/pricklypear/env` (local only, mode 600 — never commit)
- `pp-eval` runs on the laptop and talks to `PP_URL` (prod by default); local `scripts/dev.sh` PG is kernel-lab only, not the eval path
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
- Explicit `(load-library "ui")` upserts the current branch, but
  **pin-hygiene** (branching.pin-hygiene, 2026-12-30) means a
  contentless pin no longer sticks: rows identical to the inherited
  (parent-chain) body are never written, and an identical re-define
  unpins. Pins exist only where the branch genuinely differs. Sweep
  historical sludge with `(prune-synced)` (returns
  `{"pruned":N,"names":[...]}`); `synced` pins in `/branches` are
  prune candidates.
- Vault: `pp-sync pull <pkg>`, `pp-sync pull-all`,
  `pp-sync snapshot [--branch system/main] [--out backups/…]`.
  `libs/` = main vault (seed input); `backups/<ref>/` = non-main dumps.
- **Vault is a generated MIRROR, not an authoring source**
  (branching.vault-authority, 2026-10-02). Don't hand-edit `libs/*.pp`
  to ship a product change: the additive boot seed only ADDS names absent
  on main, so editing an EXISTING define is committed but never deployed.
  Work the habitat (personal/dev) → promote;  refresh the vault with
  `pp-sync snapshot`. Kernel changes still ship through the release.
- **Drift view:** `pp-sync status [pkg]` compares the vault against
  **system/main by default** (checkout + restore) — canonical AST
  **bodies**, not just names. `body-stale` names are the
  git-has-the-fix / main-does-not signal; `only-local`/`only-live` are
  name-set drift. `--names-only` is the fast blind pass. Run it before
  trusting that a `.pp` change deployed (`body-stale: 0` = honestly
  live).
  Never `reseed` a drifted package to "land" a fix — the reseed guard
  refuses on live-only names AND same-name body drift (both mean
  "converge first": `pull` the package, review, then reseed; `--force`
  is the deliberate clobber).
- **ONE guarded write path (2026-10-02 review):** `pull`, `snapshot`
  and `land` all write the vault through the same gate
  (`Pp_sync_merge.plan_write`): refuse hand-authored files with zero
  live content, MERGE hand-authored files WITH live content keeping
  the custom header (std merges, theme-* skins refuse), refuse to
  drop a name live main binds nowhere (dropped-name accounting — main
  is append-only so a vault-only name is an anomaly, refused by name),
  otherwise comment-preserving merge. `--force` overrides loudly; `--dry-run` previews honestly
  (`--out /tmp` previews bypass guards — don't use them to approve).
  `land <pkg>` = promote to main + vault refresh + commit in one.
  Full workflow doc: `docs/pp-sync-workflow.md`.
- **Snapshot is comment-preserving** (branching.vault-authority,
  2026-10-02). The vault is literate source; `export-package` emits no
  interior comments, so `pp-sync snapshot` MERGES: unchanged forms keep
  their old text verbatim, only changed forms take new text, and a lost
  comment on a changed form is reported by name. It also refuses to
  overwrite a hand-authored file (no `exported from live branch` header)
  or to shrink a file's define count; `--force` overrides both.

## One-tree storage (default)

Postgres still holds bytes, but the **logical store is ONE TREE**
(`tree_nodes` + `tree_branches`, migration `009_tree.sql`). Spec:
`borge/one-tree.borg`. Overview: `docs/one-tree.md`.

- **Paths:** `b.<branch>.{fn,type,route}.<label>` · `data.<type>.<id>`
- **Default ON: the tree is the read AND write source of truth** (reads
  go through the `tree_*` views; writes through `Tree_store`). The old
  `PP_TREE_READS` / `PP_TREE_WRITES` opt-in flags are retired — the
  scaffolding tables (`function_defs` / `type_defs` / `view_bindings` /
  `data_rows`) are forward/backward-mirrored projections, not sources.
- **Hash domain (migration 027, 2026-09):** node hashes are sha256 over
  PG canonical jsonb text of the body, computed DB-side in every writer
  (`Tree_store` upserts AND the 009 mirror) — one domain, comparable
  across write paths; `branch_status` reads the stored column directly.
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
- **The I/O rim is Eio, period** (`Httpun_eio` + `Io_eio.Pg` / pure pgx
  over Eio.Net). The Lwt rim, the `PP_IO` flag, and the `Lwt_eio` bridge
  were deleted at eio-floor Phase 5 (2026-09-15): there is no rollback
  flag, rollback is git revert. See `borge/single-image.borg` § runtime
  and `archive/plans/eio-floor-checklist.md` / `archive/plans/runtime-effects-transition.md`.
- Single unix binary (`image/`) — not a Solo5 unikernel in v1. Solo5 remains
  an optional later hardening axis (`spikes/mirage_solo5/`), not the product path.
- Auth is in-process (`image/lib/auth_*.ml`). Password hashes are argon2id as of
  2026-12 (`$argon2id$` PHC); legacy
  `sha256$salt$digest` rows verify transparently and are
  rewritten on successful login (was BORGE-1783831698 /
  1785545259, closed).
- `pure-html` for type-safe HTML + `HTML.Hx.*` htmx attributes. Use
  `txt ~raw:true "%s" s` to bypass escaping for raw-HTML injection from
  Nopales route handlers.
- `alcotest` for tests, `yojson` for JSON (serialized Nopales ASTs are
  JSONB; dict prims cover field-level access on JSON strings).
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
- Password hash: argon2id PHC champion + transparent legacy
  `sha256$salt_hex$digest_hex` verify-and-rewrite (BORGE-1785545259,
  closed; supersedes stale 1783831698). Verify: `ksh scripts/verify_auth.sh`.

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
  `/timing` (per-route server-side page render cost: total/db ms,
  db hits, slowest-first; borge/image.borg page-timing),
  `/data` (generic data_rows browser: list types + counts, browse
  `/data/<type>`, create/edit/delete rows at `/data/<type>/…`).
  `/inspect/row/<type>/<id>` redirects to `/data/<type>/<id>`.
  User routes via `(route "/x" "fn")` (private by default) or
  `(route "/x" "fn" 'public)` / `(public-route …)` for anonymous GET.
  Kernel paths (`/data`, `/admin`, `/api/eval`, …) stay hard-private.
  Package-provided routes live in `libs/*` — e.g. the `notes` library
  serves `/issues` (the `log/issues` register face; see § File, don't fix).
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
- A spec change discovered during `make`: `borge issue new "..."`, don't
  edit `.borg` directly. The next `plan` pass resolves issues. Do NOT
  file issues during `plan` or open discussion — plan mode exists to
  change the spec directly; discussion mode exists to talk.
- After every code change touching a `.borg`-declared module: `borge
  drift` to verify spec/code alignment.

## Conventions

- OCaml / dune, single unix image process for v1 (Solo5 optional later —
  `spikes/mirage_solo5/`, not the product path)
- Eval core is direct-style (`lisp/` Lwt-free; `Control.Prim_call` +
  rim). Image domain still has monadic coloring at the prim ABI until the
  Eio-floor decolor pass (`archive/plans/eio-floor-checklist.md` Phase 3). Default
  process rim is Eio; Phase 5 (2026-09-15) deleted the Lwt rim and the
  `PP_IO` flag.
- No `(load-library "pkg")` in Nopales route handlers (spec/rb dec-2,
  2026-09-07): the session env binds package defs from the branch chain
  lazily, and load-library re-evals the DEPLOYED release file onto the
  current branch — on a garden branch that clobbers newer pins of changed
  handlers on their own next page load. notes/todo never call it; rb was
  stripped (3c405cd).
- `pure-html` for HTML generation; use `HTML.Hx.*` not raw htmx attribute
  strings (poohstack rule, preserved here via pure-html). Use
  `txt ~raw:true "%s" s` for raw-HTML injection (escape-bypass path);
  there is NO `Pure_html.of_string` — pure-html is tree-only.
- Agent FE verify: tiered checks (wget → workflow/smoke → screenshot+vision),
  a11y snapshots over CSS selectors, graduate green flows into smokes.
  Research wiki: [`docs/agent-ui-harness.md`](docs/agent-ui-harness.md).
  Example contract: [`docs/notes-ui-contract.md`](docs/notes-ui-contract.md).
- HTTP: `Httpun_eio` accept loop (`httpun-lwt-unix` deleted at Phase 5).
  Postgres: pure pgx over Eio.Net (`Io_eio`); `pgx_lwt` deleted at Phase 5.
  No ORM, plain SQL. SERIALIZABLE transactions by default —
  `Store.with_transaction` / `(with-transaction (lambda () body))` keep
  `(select-for-update ...)` locks across prims in the body. Under Eio the
  store holds a reentrant per-connection mutex for whole-tx wire sections.
- Additive-only schema migrations, applied by psql — never by the image
  (image/lib/boot.ml does not read `schema_migrations`): `scripts/dev.sh`
  (dev) and `scripts/openbsd/pricklypear-promote.ksh` (prod), both
  version-gated on `schema_migrations`. Migration files self-register
  their version row; expensive backfills additionally DO-guard on it
  (027's hash-domain rehash).
- The `.borg` file is the source of truth. Never edit `.borg` directly
  in make/task mode — a spec gap surfaced there becomes a `borge issue
  new "..."`, resolved by the next `plan` pass. Issues are the make-mode
  escape hatch, not a note-taking habit: status flips, drift
  observations, and verification results belong in commit messages and
  reports, not in the issue tracker.
