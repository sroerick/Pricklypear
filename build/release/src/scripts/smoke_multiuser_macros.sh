#!/bin/ksh
# Smoke: plan, chat, llm keys, ssh keys, macros.
# Requires: scripts/dev.sh stack on :18080, admin/admin.
set -eu
export PP_URL="${PP_URL:-http://127.0.0.1:18080}"
export PP_USER="${PP_USER:-admin}"
export PP_PASS="${PP_PASS:-admin}"
EVAL="${PP_EVAL:-python3 scripts/pp_eval.py}"
cd "$(dirname "$0")/.."

j() { $EVAL -j "$1"; }

echo "== whoami =="
j '(whoami)'

echo "== plan =="
j '(plan "smoke plan")'
j '(plan-of "admin")'

echo "== chat =="
j '(chat/say "smoke lobby")'
j '(null? (chat/lobby))'

echo "== llm key =="
j '(set-llm-key "sk-smoke-key-xyz9")'
j '(llm-key-status)'
j '(clear-llm-key)'

echo "== ssh key =="
# optional if key file present
if [ -f /tmp/pp-test-key.pub ]; then
  KEY=$(cat /tmp/pp-test-key.pub)
  j "(add-ssh-key \"$KEY\" \"smoke\")"
  j "(verify-ssh-key \"$KEY\")"
  FP=$(j '(car (list-ssh-keys))' | sed -n 's/.*"\(SHA256:[^\"]*\)".*/\1/p')
  j "(revoke-ssh-key \"$FP\")"
fi

echo "== macros =="
j '(defmacro when (t b) `(if ,t ,b ()))'
j '(when #t 42)'
j '`(a ,@'"'"'(b c) d)'

echo "== HTTP unauth =="
code=$(curl -sS -o /dev/null -w '%{http_code}' "$PP_URL/plan" || true)
echo "GET /plan unauth -> $code (expect 303)"

echo "SMOKE OK"
