#!/bin/ksh
# verify_prims_db_split.sh - pin the DB-prim surface after the prims_db split
# (i-1788671035-8). The old single-closure prims_db.register registered 137
# prims across unrelated domains; the families now live in prims_data /
# prims_sync / prims_jobs / prims_auth / prims_code / prims_agent /
# prims_llm / prims_ops, with prims_db as composition root over shared
# helpers in prims_db_common.
#
# This script pins, against the expected list below (extracted from the
# pre-split prims_db.ml):
#   1. prims_db.ml registers NOTHING (composition root only).
#   2. The family modules register exactly the expected 137 prim names,
#      each exactly once (no drops, no renames, no duplicates).
#   3. The server-only set (register_server_only*) is exactly the
#      expected 7 names (remote-eval classification is registry-derived).
#   4. Every family module exposes `let register`.
#
# Negative-test: temporarily rename/delete a prim registration and re-run -
# this script must FAIL.

set -eu
cd "$(dirname "$0")/.."

FAMILIES="prims_data prims_sync prims_jobs prims_auth prims_code prims_agent prims_llm prims_ops"
LIBDIR=image/lib

EXPECTED_NAMES='[
  "abandon-data", "add-ssh-key", "agent", "agent-history", "agent-say", "agent-session-delete",
  "agent-session-new", "agent-sessions", "agent-task", "apropos-docs", "backlinks",
  "cancel-job", "clear-llm-key", "code-diff", "code-history", "code-restore",
  "create", "create-agent", "create-user", "current-branch", "data-history",
  "deftype", "delete", "describe", "diff", "export-package", "find-code", "find-random",
  "find-row", "find-rows", "find-text", "forge-effects", "fork-data", "function-ast",
  "function-doc", "function-source", "gen-diff", "gen-now", "gen-read", "get",
  "install-function", "is-admin?", "issue-token", "job-status", "jobs", "lens/fork",
  "link", "list-branches", "list-data-lenses", "list-packages", "list-ssh-keys",
  "list-tokens", "list-usernames", "llm-chat", "llm-inner", "llm-key-status",
  "llm-outer", "llm-rewrite", "llm-seed", "log-tail", "merge-data", "node-path",
  "ops/incidents", "ops/summary", "out-links", "package-bindings", "package-disable",
  "package-enable", "package-exports", "packages", "promote", "promote-to-main",
  "propose-function", "replication/unit", "retention-delete-policy", "retention-policies",
  "retention-policy-set", "retention-run", "revoke-ssh-key", "revoke-token",
  "rotate-token", "rows", "schedule", "schedules", "select-for-update", "set-allow-agent-promote",
  "set-llm-key", "set-user-type", "space-add-member", "space-admit", "space-create",
  "space-exists?", "space-list", "space-members", "space-remove-member",
  "submit-job", "submit-job-at", "sync/plan", "sync/push", "tree/ancestors",
  "tree/child", "tree/children-html", "tree/children-of", "tree/delete",
  "tree/find-by", "tree/folder-index", "tree/garden-root", "tree/get", "tree/get-uuid",
  "tree/insert-child", "tree/leaf", "tree/lens-children", "tree/lens-get",
  "tree/lens-subtree", "tree/list", "tree/list-kind", "tree/move", "tree/move-after",
  "tree/move-before", "tree/next", "tree/note-index", "tree/parent", "tree/path-label",
  "tree/prev", "tree/reorder", "tree/search-text", "tree/segments", "tree/siblings",
  "tree/subtree", "tree/upsert", "type-fields", "undo", "unlink", "unschedule",
  "update", "update-field", "verify-ssh-key"
]'

EXPECTED_SERVER_ONLY='["create-user","is-admin?","retention-delete-policy","retention-policies","retention-policy-set","retention-run","set-user-type"]'

fail() { print -u2 "FAIL: $*"; exit 1; }

# 1. composition root registers nothing, but calls every family
if grep -qE 'register_crud "|Primitives\.register( |_sync|_server_only)' "$LIBDIR/prims_db.ml"; then
  fail "prims_db.ml must be a pure composition root (found a registration call)"
fi
for f in $FAMILIES; do
  grep -q "Prims_${f#prims_}.register" "$LIBDIR/prims_db.ml" \
    || fail "prims_db.ml composition root does not call Prims_${f#prims_}.register"
done

# 4. every family exposes register (before scanning names)
for f in $FAMILIES; do
  [ -f "$LIBDIR/$f.ml" ] || fail "missing family module $LIBDIR/$f.ml"
  grep -q '^let register' "$LIBDIR/$f.ml" || fail "$f.ml does not define let register"
done

# 2 + 3. scan registrations across families, compare via python3 json
python3 - "$@" <<PYEOF
import json, re, sys

families = """$FAMILIES""".split()
pat = (r'register_crud "([^"]+)"|register_server_only tbl "([^"]+)"'
       r'|register_server_only_sync tbl "([^"]+)"'
       r'|Primitives\.register tbl "([^"]+)"'
       r'|Primitives\.register_sync tbl "([^"]+)"')
so_pat = r'register_server_only(?:_sync)? tbl "([^"]+)"'

names, so = [], []
for fam in families:
    src = open(f"image/lib/{fam}.ml").read()
    for tup in re.findall(pat, src):
        names.append((next(s for s in tup if s), fam))
    so += re.findall(so_pat, src)

expected = json.loads('''$EXPECTED_NAMES''')
expected_so = json.loads('''$EXPECTED_SERVER_ONLY''')

got = sorted(n for n, _ in names)
if len(names) != len(set(names)):
    from collections import Counter
    dups = [n for n, c in Counter(names).items() if c > 1]
    sys.exit(f"FAIL: duplicate prim registrations: {dups}")
if got != expected:
    missing = sorted(set(expected) - set(got))
    extra = sorted(set(got) - set(expected))
    sys.exit(f"FAIL: prim surface drift. missing={missing} extra={extra}")
if sorted(set(so)) != expected_so:
    sys.exit(f"FAIL: server-only set drift: got {sorted(set(so))} expected {expected_so}")
print(f"prims_db split OK: {len(names)} prims, {len(set(so))} server-only, {len(families)} families")
PYEOF
