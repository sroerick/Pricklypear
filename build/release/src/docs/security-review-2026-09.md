# Security review — 2026-09 (agent hardening pass)

Scope: sandboxing of Nopales eval, the prim capability system, `/api/eval`
+ `/api/prim`, public routes, auth/session, data & code tenancy, HTTP
surface. Method: read of kernel + image sources (lisp/lib, image/lib,
migrations). This file is a point-in-time review; issues cited by
file:line as of this branch.

**Verdict in one line:** the auth layer, the `example_eval` sandbox, and
the RLS datum floor are genuinely good; the **prim capability layer is
fail-open and publish-time-only**, and there are **two unauthenticated /
any-user write-and-read primitives** (`sort` SQL injection, `code-restore`
cross-branch write) that break the tenancy model the promote gate is
supposed to provide.

---

## P0 — fix before anything else

### 1. SQL injection via `sort` in `(rows …)` / `(find-rows …)` — read any table

`Crud.list` (`image/lib/crud.ml` ~L110):

```ocaml
let order_by = match sort with
  | Some s -> " ORDER BY " ^ s          (* raw concat, no validation *)
  | None -> " ORDER BY created_at ASC"
```

Same in `Crud.find_rows` (~L428: `" ORDER BY " ^ s ^ " LIMIT "`). The prim
surface passes the Lisp string straight through
(`image/lib/prims_db.ml` L202, L243: `(rows 'type "<sort>")`,
`(find-rows 'type … "<sort>")`).

pgx executes this as a single prepared statement, so no multi-statement
smuggling — but PostgreSQL accepts arbitrary **expressions** in ORDER BY.
That is a full blind/error-based oracle over every table the image role
can read (`GRANT SELECT … ON users, credentials, auth_sessions …`,
migrations/002_auth.sql L74):

- `(rows 'x "(CASE WHEN (SELECT 1 FROM credentials WHERE secret_data->>'api_key' LIKE 'sk-a%') THEN 1 ELSE 1/0 END)")`
  → error-based exfiltration, one char per request.
- Password hashes (`credentials.secret_data`), **LLM API keys (stored
  plaintext, see §6)**, media bytes — all readable by any authenticated
  session. RLS does not apply (it floors only `tree_nodes` datums).

Reachable from any authenticated eval (`/api/eval`, browser REPL), from
agents, and — once smuggled through a public route via §2 — from a page
visit running as the visitor.

**Fix:** make `sort` an enum: accept only `created_at | updated_at | id`
+ optional ` ASC|DESC`, build the ORDER BY from the parsed token, never
from the raw string. Grep the whole repo for other `" ORDER BY " ^`
patterns while at it.

### 2. Public-route effect gate is bypassable (fail-open unknowns + TOCTOU)

The gate (`lisp/lib/route_gate.ml`, `image/lib/session.ml:624`) is the
right idea but has two independent holes:

**(a) `Effect.effects_for_prim` fails OPEN** (`lisp/lib/effect.ml`):
any prim name not in the hand-maintained lists infers as `EffSet.empty`
(pure). The lists have drifted from the prim catalog — unclassified
prims include `eval-string`, `tree/upsert`, `tree/delete`, `link`,
`unlink`, `sync/push`, `sync/plan`, `lens/fork`, `fork-data`,
`merge-data`, `abandon-data`, `type-fields`, `node-path`, `multipart`,
`part`, `json-part`, … (verified against the registered catalog). The
killer is **`eval-string`** (registered `image/lib/session.ml:1897`, in
NO effect list):

- publish-time inference sees `(eval-string "(route …)(create …)")` as
  **pure** → passes the public gate and the AI install gate
  (`ai_default_allow`) alike;
- at runtime `eval-string` evaluates in the live session env with the
  **full prim table** and a **fresh budget** (`make_budget ()`) —
  capability + resource-limit reset in one prim.

**(b) The gate runs only at publish.** `call_handler`
(`session.ml` ~L2315) never re-checks; `(route "/x" "h" 'public)` then
`(define h <body with writes>)` re-points the route with zero re-gate,
and boot rehydrate loads stored routes unchecked. Also `Effect.infer` is
macroexpanded at publish but the *runtime* body can be swapped later.

Effect: a public page runs as the **visiting identity** (`deny_anon`
only protects `anonymous`), so an authenticated visitor triggering such
a handler performs WriteDb/Ai effects **as themselves** (stored-CSRF:
edit their rows, rewrite their LLM key, define code on their branch,
`submit-job`).

**Fix (defense in depth, all three):**
1. Fail **closed**: unknown prim ⇒ infer the strongest effect set (or a
   dedicated `Unknown` denied by every allowlist). Classify the
   unclassified prims explicitly (the tree/* read split is already done
   correctly in `example_eval.ml`'s `read_extra_prims` — reuse it).
2. Re-run `check_public_handler` at dispatch for `visibility = "public"`
   routes (cache the verdict per handler name, invalidate in
   `record_define_side_effects`).
3. At minimum, deny `eval-string` (and `load-string`/`load-library`)
   inside public-route handler dispatch — they are the meta-eval
   escape hatch.

Note `example_eval.ml` proves the team knows the right pattern
(whitelisted name-filter table, fail-closed, wall-clock cap) — the main
route gate just doesn't use it.

### 3. `code-restore` writes code to ANY branch, no ownership check, no anon gate

`image/lib/prims_db.ml` ~L1268: takes `(code-restore name [n]
["owner/branch"])`; `resolve_branch_id` looks up **any** branch by
owner/name with no `branch_owner_authorized` check, then
`Store.code_restore` (`image/lib/store.ml` L2537) does `upsert_fn /
upsert_type / upsert_route` on that branch. No `deny_anon`. RLS
deliberately exempts non-datum kinds (`migrations/013…sql`: "Non-datum
kinds unrestricted here"). So **any identity — including anonymous via a
public handler — can rewrite fn/type/route rows on any branch, including
`system/main`**, bypassing the promote gate: e.g. revert any promoted
handler to an older version (restore-by-write). `undo`
(`prims_db.ml` L1245) also lacks the anon gate (RLS does floor its
datum-path writes, so it's the lesser sibling).

**Fix:** `branch_owner_authorized` (or admin-only for `system/*`) on
both prims, keyed off the request manifest; add them to the WriteDb
effect list (they're missing there too, which is how they'd sail through
the publish gate as "pure" — see §2a).

---

## P1 — high

### 4. No wall-clock cap on eval/handlers; `sleep` + `with-transaction` = cross-tenant DoS

`sleep` is capped at 60 s/call (`prims_net.ml` L310) but is classified
**Pure** (`effect.ml` pure_prims), and `with-transaction` wraps any
lambda. `(with-transaction (lambda () (sleep 60) (sleep 60) …))` costs
~2 steps per sleep inside a 100k-step budget → one request can hold one
pgx connection + SERIALIZABLE txn open for **days**. The per-connection
Eio mutex (`store_db.ml` L30) then serializes everything behind it — and
the **anonymous session is shared by all anonymous visitors**, so a
public page can freeze the entire public surface. `example_eval` blocks
`sleep` explicitly; `/api/eval` and route handlers have a step budget
but **no wall-clock deadline at all** (a single expensive prim —
`find-text` ILIKE over the corpus — is one step; known `notes/doc/get`
~80 s floor).

**Fix:** (a) deny `sleep` (and `http-*`, llm prims) inside
`__with_transaction__`; (b) wrap request eval in an Eio timeout/cancellation
that aborts after N seconds (prim boundaries are natural cancellation
points); (c) cap txn wall-time in `with_dynamic_transaction`; (d) cap
accumulated `db_ms` per budget.

### 5. XSS by design with no CSP → one-click account impersonation

User routes return raw HTML (`render-html`, raw VStr path in
`respond_handler_value` / `serve_public_body`) — that IS the product,
fine. But: no `Content-Security-Policy` header anywhere, no CSRF tokens,
and `/api/eval` accepts the **session cookie**. HttpOnly+SameSite=Lax
blocks cookie theft and cross-site POSTs, but **same-site** injected
script (any user's public page) can `fetch('/api/eval', …)` with the
visitor's cookie and run Nopales as them (their data, their branch).
Subdomain cookie scoping (`PP_BASE_DOMAIN`) widens the blast radius to
all gardens.

**Fix:** send `Content-Security-Policy: default-src 'self'` (+ what
htmx/CM need, ideally nonce'd), `X-Content-Type-Options: nosniff`,
`X-Frame-Options: DENY`. Consider `SameSite=Strict` on `pp_session` and
a CSRF token (or custom-header requirement) on browser-driven
state-changing kernel endpoints (`/api/eval`, `/admin/users`, `/data`
writes).

### 6. LLM API keys stored plaintext in `credentials.secret_data`

`auth_store.set_llm_key` (~L917): `secret_data = {"api_key": …}` raw.
Combines with §1 into full key theft; also any future read-primitive bug
leaks them.

**Fix:** envelope-encrypt at rest (libsodium secretbox with a key from
env/`PP_SECRET`, or at minimum pgcrypto), decrypt only in
`llm_client`. Session tokens are already stored hashed — bring keys to
the same standard.

### 7. `load-library` name is a path + reader-error reflection ⇒ probable arbitrary file read

`loader.ml`: `library_path` does `Filename.concat root name` with no
validation of `name` (`../` travels), `read_file` opens anything, and
`load_string`'s failure paths **echo source snippets / unbound-atom
names** back as the error string (the `unbound: <atom>` message includes
the raw token). `(load-library "../../etc/passwd")` → each line parses
as atoms → eval fails → error text contains the line → returned as VStr
to the caller. Any authenticated user; via §2 also public-route
smugglers.

**Fix:** validate `name` against `^[a-z0-9][a-z0-9_-]*$`, and never
include source text in load error strings (position only).

---

## P2 — medium / hardening

8. **Basic-auth argon2 verify per request + throttle table eviction.**
   Every Basic-auth request burns ~0.5–1 s argon2id
   (`http_core.resolve_identity` → `verify_password`); unknown usernames
   burn the dummy hash (by design for timing). Pre-auth CPU amplification
   with rotating usernames; the bounded throttle table (4096, LRU-evict)
   lets a username flood evict a real user's failure window. Add a
   per-IP limiter and consider caching `(user, pass)` → verified for a
   short TTL, or verifying once and issuing a scoped token.

9. **DNS-rebinding TOCTOU in egress block** (`prims_net.ml`):
   `egress_blocked` resolves, then curl re-resolves. Pin the checked IP
   (`curl --resolve host:port:ip`) or proxy egress.

10. **Shared anonymous session env is global mutable state.** Anonymous
    `load-string`/`eval-string` defines persist in the *shared* anon env
    until restart (persistence is correctly skipped: `persist_define`
    checks `username <> "anonymous"`, `session.ml` ~L1100), so public
    pages can pollute each other in-memory. Consider a fresh child env
    per public request.

11. **Handler error text is rendered to clients** (`Handler_error msg` →
    page text; `pp_interp_error` includes OCaml exception strings) —
    internal paths/SQL shapes leak. Keep 500s generic (already done) and
    trim prim/interp error text of internals for non-admin callers.

12. **`effect.ml` hygiene:** duplicate `read_db_prims` definition (first
    list dead/shadowed) — symptom of the drift that causes §2a. Add a
    boot-time assertion: every registered prim name must appear in
    exactly one capability list (fail boot on violation). This single
    check would have caught the whole class.

13. **Cookie `Secure` conditional** (`http_core.ml` ~L510): on bare-IP/
    http deployments the session cookie goes without `Secure`. Fine
    behind nginx+TLS; consider defaulting `PP_COOKIE_SECURE=1` and
    requiring opt-out.

14. **`find-text` needle** is `%`-wrapped into ILIKE — user `%`/`_`
    wildcards are unescaped (minor; full-corpus scans also cheap-DoS —
    see §4c).

---

## What's already good (keep, don't regress)

- `example_eval.ml`: whitelisted rim, fail-closed absent prims, static
  gate, wall-clock cap, child env, `Budget_exceeded` re-raised through
  `guard`. This is the model for the main route gate.
- Auth: argon2id (OWASP params), constant-time compare, dummy-hash
  timing flatten, per-username throttle w/ backoff, session tokens
  stored SHA-256-hashed, transparent legacy rehash.
- RLS floor on datum rows (`pp.user` GUC + view predicate + policy),
  author-scoped CRUD SQL, seed-type carve-out via one SQL predicate.
- Request gate: identity attach before classify, subdomain garden
  confinement, kernel hard-private list, generic 500s (no exception
  echo), 413 on oversized bodies (1 MiB default, media 5 MiB with
  content-type allowlist, SVG forced to attachment).
- SSRF blocklist + O_EXCL 0600 temp files + proper POSIX quoting in
  net/llm prims; `/code` traversal guard (reject `..`, realpath root);
  line REPL binds 127.0.0.1; `deny_anon` layer on CRUD/fork/merge/route/
  checkout/admin prims; `assert_bindable_name` stops prim shadowing
  (BORGE-1785213238).

## Suggested order of execution

1. §1 sort enum (small, surgical) + repo-wide `ORDER BY ^` sweep.
2. §3 `code-restore`/`undo` authorization (+ effect-list entries).
3. §2 fail-closed `effects_for_prim` + boot-time prim↔list assertion +
   dispatch-time re-gate for public routes.
4. §4 sleep-in-txn denial + request wall-clock deadline.
5. §6 LLM key encryption; §7 loader path validation.
6. §5 CSP + cookie/CSRF hardening.
7. P2 batch.

Each fix is small; 1–4 are the ones that actually break tenancy or leak
credentials today. Suggest filing `log/issues` entries per item (habitat
track) before landing fixes, per the session ritual.
