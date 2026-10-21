# One permission model for the OCaml layer

Status: APPROVED by roerick 10-21 (judgment delegated on the open
questions: gaps first, then the migration). Decisions locked:
no UI; policy stays in code, enforced at the OCaml layer. The doasq lane
on wyo is out of scope (documented in ~/wyo-ops/ only).

## 1. Problem

The authorization logic exists but is scattered across session.ml,
session_control_prims.ml, prims_code.ml, prims_data.ml, crud.ml,
prims_auth.ml and http_core.ml, with several slightly different idioms:

- THREE admin idioms: `t.is_admin` flag, literal `username = "admin" /
  "system"` comparisons, and `Prims_db_common.is_verified_admin` (flag
  plus live users re-check).
- TWO owner-check idioms: `branch_owner_authorized` (checkout/fork/merge
  family) and the fresh owner = manifest.username rule I wrote for
  reseed-package (909f295).
- Drift already bit: branch_owner_authorized has a textual `u = "admin"`
  fallback. Reusing it for the reseed namespace gate would have let
  admin-delegated tokens through everywhere. Same concept, three
  implementations, already diverging.

## 2. Identity classes

One class per session, computed ONCE (currently each site recomputes its
own idea of it):

| class | definition |
|---|---|
| anonymous | no auth; public manifest on system/main |
| self-human | own token/cookie, user_type=human |
| self-agent | own token, user_type=agent (e.g. gregor on .rocks) |
| delegated | on-behalf-of token (spec_delegation): manifest.username = OWNER, actor = carrying agent; agent_branch_lock set; data-only |
| tenant | PP_PROFILE set AND not admin (spec_town_profile). Mainframe runs without PP_PROFILE, so there the class is empty; town/club flip every non-admin to tenant |
| admin | users.is_admin |

`restricted t` = delegated OR tenant. On the mainframe it is exactly the
delegated class.

## 3. Current enforcement inventory

Source of truth as of master 909f295 (2026-10-21). Full prim-by-prim
gates below; the interesting rows are the exceptions.

### 3.1 HTTP edge (http_core.ml)

- Hard-public: /login /logout /health /welcome /background /roadmap
  /signup /guestbook /book.pdf /src.tgz /code/* /assets/*
  /.well-known/agent.json /agent.txt; /tree + /partials/tree (confined to
  public prefixes by Http_tree.path_visible); /notes/<user>/<slug> is
  hard-public at the route layer but renders only rows that pass the vis
  floor inside tree/find-by.
- Hard-private (Lisp cannot open): /admin /analytics /data + more prefix
  families in is_system_hard_private.
- /api/eval: identity attach per request (Basic first login, then Bearer
  or cookie).
- /api/prim and fs.write: privileged master-key surface, known open items
  (swarm Phase A list).
- PP_AUTH_DISABLED: guarded with a loud fallback (auth_disabled_guard_test).

### 3.2 Branch operations (session_control_prims.ml)

Helpers: branch_owner_authorized (write side), branch_readable (read
side: system/* public + owner rule), agent_branch_lock (blocks
fork/merge/checkout for locked agents), public-dispatch effect lattice
(fail-closed), restricted (routes are memory-only).

- fork, merge, checkout: anon deny; lock; owner rules (merge: system/*
  admin-only; source readable).
- branch/pull, branch/push, branch/pick, branch/ingest: owner/promote
  rules (agent -> parent requires allow_agent_promote; humans own;
  admin valve when no users row) but deliberately NOT lock-gated.
- branch/status, branch/log, branch/defs, branch/cp-list:
  branch_readable. cp/cp-diff/cp-restore: current-branch only, system/main
  refused.
- Ungated by design: route-visibility (in-memory), bench (evals arbitrary
  expressions - see gaps), __step__ (private job checkpoints).

### 3.3 Code and packages (session.ml, prims_code.ml)

- reseed-package: system/main restricted+admin (132acce); other branches
  open to self sessions, owner-namespace-gated for delegated/tenant
  (909f295), denial before the lookup.
- load-library: delegated = seeded packages only; non-main interactive
  loads pin-warn.
- define/route persist: skipped for delegated (data, never for code);
  seed_additive insert-if-absent during boot seed.
- deftype: anon + delegated denied (schema is code-adjacent); branch
  ownership enforced downstream in Store.persist_type.
- code-restore, undo: owner rules; package-enable/disable: protected
  package + anon deny; function-source/ast/find-code/code-diff:
  branch_read_scope (branch_readable).
- Gap: code-history and diff resolve ARBITRARY owner/branch refs with no
  branch_readable check (cross-garden code metadata leak).
- Gap: propose-function / install-function write code; their gating lives
  inside Agent_tools, not at the prim layer.

### 3.4 Data (prims_data.ml, crud.ml)

- RLS floor: set_local_pp_user inside every datum tx (m013); visible_sql
  predicate (author OR seed OR member space OR admin, m016/029/033) on
  every read.
- Writes: create/update/delete guarded (guard_space_write twice on
  create, closing the anonymous open-space body-routing hole);
  check_type_fields declared-type gate.
- Spaces: space_* membership; space_owner_ok for member/admit ops.
- Notes: gate_note_vis (cross-garden reads need vis public/users);
  gate_note_prefix on the index/search prims (own garden or data.shared
  or verified admin).
- Gaps: tree/garden-root "user" reveals any user's garden root;
  tree/children-html takes a caller-chosen prefix that never passes
  gate_note_prefix; RLS-only prims (get/rows/find-*/tree-list/...) open
  their floor when pp.user is unset.

### 3.5 Auth and identity prims (prims_auth.ml)

- require_admin: create-user, delete-user (plus self/last-admin guards),
  create-agent, set-user-type, set-password, retention-* (4 prims).
- Owner-or-admin: issue/list/revoke/rotate personal tokens, mint/list/
  revoke delegated tokens (owner self-service; admin for third parties).
- Self-only: add/list/revoke ssh keys, set-allow-agent-promote.
- promote: agent -> parent personal branch, requires parent consent;
  promote-to-main: hybrid admin check (flag OR literal username).
- Gap: revoke-token has no prim-layer gate (comment claims owner-or-admin
  and delegates to Auth_store; verify and close).

### 3.6 Agents (prims_agent.ml)

Zero inline gates; enforcement lives inside Agent_tools. Needs its own
inventory pass before migration step 6.

## 4. Consolidation and gaps (each becomes a work item)

1. Unify the three admin idioms on is_verified_admin.
2. Unify the two owner idioms on one authorize_owner (subsumes
   branch_owner_authorized and the reseed namespace rule; agent
   tenant-strip stays).
3. code-history / diff: add branch_readable. LANDED 1ac2412 (10-21;
   deployed; prod-verified as gregor).
4. tree/garden-root: restrict to own garden or admin. LANDED 1ac2412.
5. tree/children-html: pass the prefix through gate_note_prefix.
   LANDED 1ac2412.
6. revoke-token: prim-layer owner-or-admin gate. LANDED 1ac2412.
   Follow-up: rotate-token still lets a DELEGATED session rotate the
   owner's tokens (same shape) - decide during migration step 5.
7. bench: decide - deny for anonymous/delegated at minimum.
8. /api/prim + fs.write: close (swarm Phase A, feeds the matrix).
9. RLS-only reads: confirm the floor default is correct for every entry
   point that can leave pp.user unset.
10. Agent_tools surface: inventory pass (prims_agent has none inline).

## 5. Proposal: image/lib/authz.ml

- `type identity_class` + `identity_class_of_session t` - the single
  computation every site calls.
- A declarative capability table: each gated surface names its capability
  and the per-class rule. The matrix in section 3 becomes DATA.
- `authorize t ~capability ?owner` returning (unit, string) result; all
  denial strings centralized so they stop drifting apart.
- Boot assertion (sibling of the existing prim-classification check):
  every prim touching a privileged surface must appear in the capability
  table; an unregistered prim fails boot instead of quietly being open.
- Tests: one per matrix cell, plus the existing delegation/branch suites.
- Migration order, behavior-preserving, each step deployed green:
  1. authz.ml skeleton + identity_class + table + tests (no call sites)
  2. branch-op family (checkout/fork/merge/branch/*)
  3. reseed / load-library / deftype family
  4. data family (space guards, note prefixes)
  5. auth-prim family (consolidate require_admin, fix revoke-token)
  6. HTTP edge + /api/prim + fs.write + Agent_tools inventory

Gap fixes land with the family that owns them; steps 2-5 are mostly
rewiring existing checks through the table, not changing policy.

## 6. Non-goals

No UI, no runtime-editable policy (every hole found came from code drift,
not missing configuration). The doasq lane stays as-is; the matrix simply
documents which PP capabilities its ppdesignate token carries.

## 7. Review asks

1. Matrix shape and classes complete? Anything missing (jobs? media?)
2. Migration order ok, or different priority (e.g. jump straight to the
   section 4 gap fixes before the rewiring)?
