#!/bin/ksh
# Verify agent discovery spine P0–P3 (local image).
set -e
cd /home/roerick/dev/wyo.tech/pricklypear
export PP_EVAL="${PP_EVAL:-$HOME/dev/wyo.tech/pricklypear/_build/default/cli/bin/pp_eval.exe}"
export PP_URL="${PP_URL:-http://127.0.0.1:18080}"
export PP_USER="${PP_USER:-admin}"
export PP_PASS="${PP_PASS:-admin}"

curl -sf -o /dev/null "$PP_URL/health" || { echo "health fail"; exit 1; }
curl -sf -o /dev/null "$PP_URL/agent.txt" || { echo "agent.txt fail"; exit 1; }
curl -sf "$PP_URL/agent.txt" | grep -q 'How to build a REPL client' || { echo "agent.txt missing REPL section"; exit 1; }
curl -sf "$PP_URL/agent.txt" | grep -q 'list-packages' || { echo "agent.txt missing discovery"; exit 1; }
curl -sf "$PP_URL/.well-known/agent.json" | grep -q 'list_packages' || { echo "agent.json missing discovery"; exit 1; }

fail=0
check() {
  label=$1
  expr=$2
  want=$3
  r=$("$PP_EVAL" -j "$expr") || true
  echo "$r" | grep -q "$want" || { echo "FAIL $label: $r"; fail=1; return; }
  echo "OK $label"
}

"$PP_EVAL" -j '(load-library "todo")' >/dev/null
"$PP_EVAL" -j '(load-library "notes")' >/dev/null
"$PP_EVAL" -j '(load-library "ai")' >/dev/null
"$PP_EVAL" -j '(load-library "ref")' >/dev/null

check package-bindings-count '(count (package-bindings "todo"))' '50'
check package-exports-shallow '(count (package-exports "todo"))' '2'
check list-packages '(list-packages)' 'todo'
check describe-pkg '(describe "todo")' 'package todo'
check describe-fn '(describe "todo/item/add")' 'Create TODO item'
check apropos-docs '(apropos-docs "todo/item/add")' 'todo/item/add'
check agents-note '(if (null? (notes/doc/get "agents")) "MISSING" "OK")' 'OK'
check ai-context '(string-contains? (ai/context) "bindings-count=")' '#t'

code=$(curl -s -o /dev/null -w '%{http_code}' -u "$PP_USER:$PP_PASS" "$PP_URL/ref")
[ "$code" = "200" ] || { echo "FAIL /ref $code"; fail=1; }
code=$(curl -s -o /dev/null -w '%{http_code}' -u "$PP_USER:$PP_PASS" "$PP_URL/ref?q=html/join")
[ "$code" = "200" ] || { echo "FAIL /ref?q= $code"; fail=1; }

if [ "$fail" -ne 0 ]; then echo FAIL; exit 1; fi
echo PASS
