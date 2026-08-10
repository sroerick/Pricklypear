#!/bin/ksh
# scripts/smoke_forge.sh — end-to-end forge loop smoke.
# Verifies: graph CRUD, frontier (requirements excluded), LLM draft via llm-chat,
# forge-effects inference, mechanical flags (capability + victory), level gate.
#
# Prereqs: scripts/dev.sh start (PG + image on 18080), LLM_MODEL set to a working
# model (syn:large:text recommended), admin/admin credentials or PP_AUTH_DISABLED.
#
# Usage: ksh scripts/smoke_forge.sh
# Exit: 0 = pass, 1 = fail.

set -e
PP_URL="${PP_URL:-http://127.0.0.1:18080}"
PP_USER="${PP_USER:-admin}"
PP_PASS="${PP_PASS:-admin}"
SCRIPT_DIR="$(CDPATH= cd -- "$(dirname "$0")" && pwd)"
ROOT="$(CDPATH= cd -- "$SCRIPT_DIR/.." && pwd)"
PEVAL="$ROOT/scripts/pp-eval.sh"

export PP_URL PP_USER PP_PASS

# Resolve pp-eval binary
if [ -x "$ROOT/_build/default/cli/bin/pp_eval.exe" ]; then
  export PP_EVAL="$ROOT/_build/default/cli/bin/pp_eval.exe"
fi

fails=0
step() { echo "── $* ──"; }
check() { if [ "$1" = "$2" ]; then echo "  ✓ $3"; else echo "  ✗ $3 (got: $1, want: $2)"; fails=$((fails+1)); fi; }
check_contains() { case "$1" in *"$2"*) echo "  ✓ $3";; *) echo "  ✗ $3 (missing: $2)"; fails=$((fails+1));; esac; }

ev() { "$PEVAL" -j "$1" 2>&1 | tail -1; }

step "0. health"
curl -sf -o /dev/null "$PP_URL/health" || { echo "FATAL: image not up at $PP_URL"; exit 1; }
echo "  ✓ image reachable"

step "1. load forge"
r=$(ev '(load-library "forge")')
check_contains "$r" "loaded" "forge loaded"

# unique graph name per run (idempotent across reruns)
G="smoke-$(date +%s)"

step "2. create graph + nodes"
ev "(forge/new-graph \"$G\" \"forge smoke\" 4)" >/dev/null
ev "(forge/add-node \"$G\" \"$G\" \"requirement\" \"a todo app\" \"\")" >/dev/null
ev "(forge/add-node \"$G\" \"$G/add\" \"function\" \"add a todo\" \"$G\")" >/dev/null
ev "(forge/add-node \"$G\" \"$G/list\" \"function\" \"list todos\" \"$G\")" >/dev/null
echo "  ✓ graph $G + 3 nodes created"

step "3. frontier (requirements excluded, both functions ready)"
r=$(ev "(length (forge/frontier \"$G\"))")
check_contains "$r" '"value":"2"' "frontier has 2 ready nodes (not the requirement)"

step "4. step — LLM drafts the first frontier node"
r=$(ev "(forge/step \"$G\")")
check_contains "$r" '"ok":true' "step succeeded"

step "5. body was drafted (non-empty)"
r=$(ev "(forge/field-str (cdr (forge/node-by-path \"$G\" \"$G/add\")) \"body\")")
check_contains "$r" "lambda" "body contains a lambda form"

step "6. effects inferred"
r=$(ev "(forge/field-str (cdr (forge/node-by-path \"$G\" \"$G/add\")) \"effects\")")
check_contains "$r" "db-write" "effects = db-write (create call inferred)"

step "7. status = drafted"
r=$(ev "(forge/field-str (cdr (forge/node-by-path \"$G\" \"$G/add\")) \"status\")")
check_contains "$r" "drafted" "status advanced planned → drafted"

step "8. mechanical flags raised"
r=$(ev "(length (forge/open-flags \"$G\"))")
# capability + victory + size = 3 (LLM body calls create → db-write; no verify stanza; body >200 chars)
check_contains "$r" '"value":"3"' "3 open flags (capability + victory + size)"

step "9. flag details"
r=$(ev "(forge/open-flags \"$G\")")
check_contains "$r" "capability" "capability flag present"
check_contains "$r" "victory" "victory flag present"

step "10. level gate blocks (unapproved drafted node)"
r=$(ev "(forge/step \"$G\")")
check_contains "$r" "pending" "step blocked on level approval"

step "11. approve + descent"
ev "(forge/set-status \"$G\" \"$G/add\" \"approved\")" >/dev/null
# second function still planned-empty → frontier ready, step drafts it
r=$(ev "(forge/step \"$G\")")
check_contains "$r" '"ok":true' "step drafts the next frontier node after approval"

step "result"
if [ "$fails" -eq 0 ]; then
  echo "PASS: forge loop end-to-end (0 failures)"
  exit 0
else
  echo "FAIL: $fails check(s) failed"
  exit 1
fi
