# Pricklypear

A multi-user Lisp operating system built on MirageOS unikernels, with
Postgres as the shared object store and htmx as the user interface layer.

The design is a multi-file borge book: root [`pricklypear.borg`](pricklypear.borg) inlines chapter files (`philosophy.borg`, `lisp-interpreter.borg`, `ai-authoring.borg`, …) in literate reading order.
If the code disagrees, the code is wrong.

## Status

v1 — end-to-end built and smoke-tested across all 5 packages.
Spec completion: **95.5%** (21 implemented / 30 partial / 1 planned).
Only `object-model.type-system` remains at `planned` (v1 has design
only, zero typed-field implementation — by design; deferred to v1.5+).

The v1 architecture is proven end-to-end:

```
SSH client (Ed25519 pubkey auth)
   │
   ▼
supervisor (awa-ssh port :12223 OPT-IN, or non-crypto line-protocol port :17822 default)
   │
   ▼
authd RPC (:17879) → verify_ssh_key against Postgres (role-isolated)
   │
   ▼
supervisor Fleet.boot → per-user image process (posix_spawn for v1)
   │
   ▼
image (mirage/unix unikernel)
   ├── HTTP server (httpaf-lwt-unix) on :18080+   — htmx UI + route pages
   │     /health, /fleet, /branches, /bindings, /binding/<name>, /types
   │     user routes via (route "/x" 'handler)   — (render-html ...) for raw HTML
   └── REPL endpoint (:17878+)                   — banner + prompt + eval loop
   │
   ▼
Postgres (shared object store — branches, function_defs, type_defs,
view_bindings, sessions, data_rows, users, credentials, auth_sessions,
auth_log, system_keys)
```

## Components

- **`lisp/`** — the pure Scheme-flavored Lisp-1 interpreter (no I/O).
  Lwt-typed evaluator: `Eval.eval : env -> expr -> value Lwt.t`
  (post-iteration-12 refactor — the door to read-side DB composition).
  30 alcotest tests pass with no services running.
- **`common/`** — shared OCaml types between supervisor, auth, and image
  (manifest, branch, protocol, auth_types). 10 alcotest tests pass.
- **`migrations/`** — additive-only SQL applied by the supervisor before
  any image boots. 3 migrations: `001_branches.sql` (branches + function_defs
  + type_defs + view_bindings + sessions + schema_migrations),
  `002_auth.sql` (users + credentials + auth_sessions + auth_log +
  system_keys + `pricklypear_auth` role grants), `003_data.sql`
  (shared generic `data_rows` table for all user-declared types).
- **`auth/`** — the auth unikernel (`authd`): verify_ssh_key against PG,
  mint+verify HMAC-SHA256 tokens, bootstrap-admin from env var,
  audit_log, role-isolated PG access. 4 alcotest tests pass.
- **`supervisor/`** — the root unikernel: fleet management (per-user
  image instances + port allocation with probe-before-allocate),
  SSH front door (non-crypto line protocol default + OPT-IN awa-ssh
  side port with real Ed25519 pubkey auth + chacha20-poly1305 cipher),
  admin HTTP (/, /fleet JSON, /health JSON), migration runner
  (with `split_sql_statements` handling dollar-quoted blocks +
  single-quoted strings + `--` comments + multi-statement splitting
  the pgx prepared-statement protocol can't do natively).
- **`image/`** — the per-user unikernel: runs the Lisp interpreter,
  exposes a REPL endpoint + htmx HTTP endpoint. ~48 alcotest tests
  pass covering pure primitives.
- **`scripts/dev.sh`** (ksh) — dev orchestration: `start` brings up
  PG (initdb if first run) + authd + supervisor on dev ports
  5433/17879/17880/17822; `stop`/`restart`/`status`/`clean` manage them.
- **`spikes/`** — feasibility probes (`awa_ssh/` — three functor-unit
  shims that satisfy `Awa_mirage.Make`; `mirage_solo5/` — Phase 1
  probe of the Mirage 4.x `mirage configure` toolchain, deferred
  to v1.5 pending 6 missing runtime packages requiring user approval).

## Stack

- OCaml 5.4.1 on the **`poohstack`** opam switch (NOT a dedicated
  `pricklypear` switch — that earlier note was corrected at it3;
  the dedicated switch never got the Mirage stack, poohstack has it).
- Lwt for all unikernel I/O (Mirage's runtime is Lwt-based).
- Mirage unikernels: `mirage/unix` for v1 development; Solo5 unikernel
  target is a v1.5 hardening axis (Phase 1 probe at
  `spikes/mirage_solo5/` confirmed `mirage configure` round-trips but
  `dune build` fails with 6 missing Mirage 4.x runtime packages).
- `pgx_lwt` with a custom `Io_unix` shim (NOT `pgx_lwt_mirage` — the
  image/lib library uses POSIX-flavored libs in v1, not the
  Mirage-native stack; the spec's transport-choices text promised
  `pgx_lwt_mirage` but the v1 reality uses `pgx_lwt` + the shim —
  pgx_lwt_mirage IS installed v2.2 but requires a real
  `Tcpip.Stack.V4V6` device, satisfiable only inside a Mirage unikernel).
- `httpaf-lwt-unix` for HTTP (NOT `httpaf-mirage` — that package
  doesn't exist on opam; the spec inaccuracy is documented in
  BORGE-1783439434. The future mirage path is `gluten-mirage` +
  `httpaf`, both not pre-installed on poohstack).
- `awa-mirage` 0.3.1 for real SSH (OPT-IN side port on `PP_AWA_SSH_PORT`;
  the awa 0.5.x upgrade path would expose the authenticated username
  in the auth-event callback, enabling per-user identity propagation
  + the line-protocol replacement + the non-interactive `ssh user@host
  'cmd'` Channel EOF fix — deferred per BORGE-1783439434).
- `pure-html` for type-safe HTML + `HTML.Hx.*` htmx attribute support
  (the dream_html core, usable without Dream). Use `txt ~raw:true "%s" s`
  to bypass escaping for raw-HTML injection from Lisp route handlers.
- `alcotest` for tests, `yojson` for JSON (serialized Lisp ASTs are
  JSONB; Lisp fields-pass-in/out as JSON strings — no VDict value type
  needed in v1, dict prims cover field-level access).
- `mirage-crypto-rng` (Fortuna) initialized at supervisor + authd boot
  for awa hostkey generation + HMAC token minting.

## Build

```bash
# The poohstack opam switch (NOT a dedicated pricklypear switch).
eval $(opam env --switch=poohstack --set-switch)

# Build all 5 packages + spikes:
dune build @all                  # exit 0

# Run all unit tests (no services needed):
dune runtest --force             # 92 alcotest tests pass
                                 #   (lisp 30 + common 10 + auth 4 + image 48)
```

## Dev stack (for end-to-end smoke tests)

```bash
# Bring up PG + authd + supervisor (uses temp PG cluster at
# /tmp/pp-pgsup on port 5433 + awa-ssh OPT-IN if PP_AWA_SSH_PORT set):
scripts/dev.sh start
sleep 2

# Smoke tests preserved at /tmp/pp_*_it*.py — each footnotes which ports
# they need + which cleanup is required. Example end-to-end:
python3 /tmp/pp_e2e_it6.py      # SSH→auth→image→REPL→define→call

scripts/dev.sh stop
```

## Laptop / agent eval (the image is the API)

From your machine, call live Lisp without building per-app REST:

```bash
export PP_URL=http://155.138.208.45   # or local image URL
export PP_USER=pricklypear
export PP_PASS='…'                    # nginx basic auth

python3 scripts/pp_eval.py '(+ 1 2)'
python3 scripts/pp_eval.py -e '(load-library "todo")' -e '(todo/list)'
python3 scripts/pp_eval.py -j '(bible/verse "John" 3 16)'   # JSON for agents
python3 scripts/pp_eval.py -i                                 # interactive
```

This hits `POST /api/eval` on the running image. Same env as the browser
REPL — libraries, data_rows, routes. Real multi-turn SSH/line-protocol
REPL remains local/loopback until the proxy path is hardened.

## Spec reconciliation (borge)

The spec is kept faithful via the borge workflow (`plan`/`make`/`review`
phases). 6 plan-passes have moved status from 0% → 95.5% completed:

- it17 plan-pass #1 (3 sections): fork + merge + transactions
- it18 plan-pass #2 (11 sections): branching + storage + lisp + workflow
- it19 plan-pass #3 (10 sections): multi-user + auth
- it20 plan-pass #4 (9 sections): migrations + supervisor
- it23 plan-pass #5 (9 sections): philosophy + architecture + image + object-model
- it25 module-enum pass: 12 `(implements ...)` declarations → 0 drift items
- it30 plan-pass #6 (3 sections): storage + transactions (finalized) + with-transaction (new subsection)

11 borge issues filed across the project, each tracking a real spec gap
surfaced by actual builds (Lwt/Mirage stack adoption, quote-of-non-atoms,
pgx multi-stmt splitter, Lwt-typed evaluator ABI, reader escape bug,
deftype+dict+render-html, comprehensive plan-pass + module-enum,
storage.transactions plan-pass, select-for-update prim + caveat,
with-transaction prim design, awa 0.3.1 static-auth-db constraint + side-port).

## v1.x roadmap (deferred work)

The only `planned` section is `object-model.type-system` — legitimately
v1.5+ (v1 has zero typed-field implementation, only design). All other
deferred items are documented in borge issues + agent notes:

- **Solo5 mirage target Phase 2+** (v1.5 hardening axis): refactor
  image/lib to use `pgx_lwt_mirage` + `gluten-mirage`+`httpaf` behind
  the same `Store.t` / `Http.t` interface; requires installing 6 missing Mirage
  4.x runtime packages + Solo5 stack packages (user approval needed
  per AGENTS.md).
- **awa 0.5.x upgrade** (v1.5+): would expose the authenticated username
  in the auth-event callback — enabling per-user identity propagation
  through the awa-ssh path, the line-protocol replacement as the default
  SSH front door, and the non-interactive `ssh user@host 'cmd'` Channel
  EOF fix. Documented in BORGE-1783439434; deferred (21 upgrades +
  3 removals + 4 installs — too risky without explicit user direction).
- **Opt-in weaker-isolation `(query ... isolation: 'read-committed)`
  passthrough** (v1.5 ergonomics): the with-transaction prim already
  supports the isolation arg shape but always uses SERIALIZABLE in v1.
- **Multi-image `find_by_user` REPL endpoint stale-state bug** (test
  hygiene): surfaces only when running smoke tests back-to-back on
  the same dev stack (the prior test leaves an image process alive
  with a drained REPL banner; the next SSH connect reuses the stale
  instance). Real-world production use (one image per long-running
  user session) doesn't trigger it. Deferred to a future hardening
  axis (would require a health-check probe on `find_by_user` matches
  before reuse, or per-instance REPL session-mutex).

## Monitor command (rerunnable verification of v1 final state)

```bash
cd /home/roerick/dev/wyo.tech/pricklypear && \
  eval $(opam env --switch=poohstack --set-switch) && \
  dune build @all && \
  dune runtest --force && \
  borge lint && \
  borge drift && \
  borge report | tail -3
```

Expected: build exit 0 / runtest exit 0 (92 tests pass) /
lint 0 errors 0 warnings / drift 0 items total /
report "21 implemented | 0 verified | 0 in-progress | 1 planned |
 22 total tracked | 95.5%".
