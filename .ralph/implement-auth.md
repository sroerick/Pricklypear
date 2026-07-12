# Implement multi-user auth (single-image)

Close the product auth gap from `auth.borg` for the **current single-image topology**
(one image binary; no authd/supervisor required for web login).

Authoritative product spec:
- `auth.multi-user-product` (complete user stories)
- `auth.web-login` (GET/POST /login, cookie, middleware, logout)
- `auth.login-experience` (land on username/personal)
- `auth.user-management` (admin creates users + passwords)
- `auth.identity-and-branch` (whoami / personal garden / author)
- `single-image.auth` Option B: password/session verify **in the image process**

## Goals
- Human can open the browser, log in with username/password, and land in their garden
- Unauthenticated product routes redirect to `/login` (HTML) or 401 (API)
- Session cookie `pp_session` is HttpOnly; sessions are revocable and expire
- Admin can create users + set passwords; non-admin cannot
- Alice and Bob keep distinct personal branches and authorship
- Monitor-rerunnable verification exists for build + smoke

## Non-goals (defer)
- Separate authd unikernel / mTLS RPC (fleet topology)
- argon2id (keep interim salted SHA256; filed BORGE-1783831698)
- OAuth / magic links
- Process-per-user fleet isolation (US-ISOLATION) — cotenant identity is correct for v1 single-image
- Rebuilding retired supervisor/auth packages
- Marking auth.borg subsections `implemented` (separate borge plan/review pass)

## Checklist

### Spec / phase discipline
- [x] Confirm borge status; entered `borg_task` for multi-section auth work
- [x] Read current `image/lib/{http,session,store,boot}.ml` auth touchpoints and `common/lib/auth_types.ml`
- [x] Filed BORGE-1783831698 (argon2 deferred)

### Core auth library (in-image)
- [x] `image/lib/auth_store.ml` — users, credentials, auth_sessions, auth_log helpers
- [x] `image/lib/auth_password.ml` — interim `sha256$salt$digest` + secure_equal
- [x] Session token mint: opaque random id; store only SHA-256 hash in `auth_sessions`
- [x] Session verify / revoke / revoke_all
- [x] auth_log writes for password success/fail, mint, logout, admin create/revoke, bootstrap
- [x] Bootstrap admin when users empty; also set password if admin exists without password cred

### HTTP surface
- [x] `GET /login` — public form
- [x] `POST /login` — verify → mint → Set-Cookie `pp_session` → redirect `/`
- [x] Invalid login: generic error, no user enumeration
- [x] `POST /logout` — revoke session, clear cookie, redirect `/login`
- [x] Auth middleware: public `/login`, `/health`, `/logout`; else require session
- [x] Attach identity (user_id, username, is_admin) per request; request mutex for cotenant
- [x] Ensure personal + checkout on login; re-attach on subsequent requests
- [x] API path: 401 JSON when unauthenticated for `/api/*`
- [x] Basic auth fallback for agents/`pp_eval` (Authorization: Basic)
- [x] `launch` takes thunk (Lwt eager-promise fix — avoids double httpaf respond)

### User management
- [x] Admin create user + set password via `POST /admin/users`
- [x] `GET /admin/users` list UI — admin only; 403 else
- [x] Non-admin cannot create users (smoke verified)
- [x] Admin revoke all sessions for a user (UI button + smoke)

### Identity wiring
- [x] Dynamic `(whoami)` / `(current-author)` via username_fn on session
- [x] define author = session username; personal branch per user
- [x] Alice pin not visible as Bob/admin personal value (smoke)
- [x] is_admin flag on session; merge/promote checks is_admin
- [x] Dev: `PP_AUTH_DISABLED=1` bypass; REPL still env identity; HTTP uses cookies/basic

### Tests & smoke
- [x] Unit/alcotest `image/test/auth_password_test.ml` (5 cases)
- [x] `dune build @all` green
- [x] `dune runtest` green (includes auth_password)
- [x] `scripts/auth_product_smoke.py` ALL CHECKS PASSED (incl. revoke_sessions)
- [x] Final monitor command: `ksh scripts/verify_auth.sh` ALL GREEN

### Docs / agent notes
- [x] `scripts/dev.sh` passes PP_BOOTSTRAP_* and PP_AUTH_DISABLED
- [x] AGENTS.md updated for single-image auth + ports + verify command
- [ ] Mark auth.borg subsections implemented via plan/review pass (deferred — not blocking product)

## Verification

### Unit / build
```bash
eval $(opam env --switch=poohstack --set-switch)
dune build @all
dune runtest --force
```

### Product smoke
```bash
ksh scripts/verify_auth.sh
# or with stack already up:
python3 scripts/auth_product_smoke.py
```

## Final Verification
- Exact monitor-rerunnable command: `ksh scripts/verify_auth.sh`
- Working directory: `/home/roerick/dev/wyo.tech/pricklypear`
- Required env: opam switch `poohstack`; optional `PP_BOOTSTRAP_USER`/`PASSWORD` (default admin/admin)
- Required preserved artifacts:
  - dune build tree (`_build/`)
  - `scripts/verify_auth.sh`, `scripts/auth_product_smoke.py`, `scripts/dev.sh`
  - `image/lib/auth_password.ml`, `image/lib/auth_store.ml`, auth wiring in http/session/boot
  - opam switch `poohstack` with deps (digestif, mirage-crypto-rng, base64, pgx, …)
- Result (2026-07-12 iteration 1): **ALL GREEN**
  - dune build @all exit 0
  - dune runtest green (auth_password 5 + image primitives 48 + packages 5 + lisp/common)
  - auth product smoke ALL CHECKS PASSED including admin revoke_sessions

## Notes
- Topology: **in-image auth (Option B)**. No authd.
- Critical fix: `launch` must take a **thunk** (`unit -> unit Lwt.t`) because Lwt promises are eager.
- Cookie: HttpOnly; SameSite=Lax; Path=/; no Secure on plain HTTP dev.
- Password format: `sha256$salt_hex$digest_hex` with digest = sha256(salt_hex + ":" + password).
- Cotenant: single Session.t + `Lwt_mutex` serializes identity attach across concurrent users.
- Deferred polish: argon2 (BORGE-1783831698), fleet isolation, auth.borg status flip via plan pass.
