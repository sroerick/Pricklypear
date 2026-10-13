#!/bin/ksh
# Smoke function-observability: recursion CSS, big-O badge, analyze/explain,
# core endpoints. Local image default.
set -eu
BASE="${PP_LOCAL_URL:-http://127.0.0.1:18080}"
USER="${PP_BOOTSTRAP_USER:-admin}"
PASS="${PP_BOOTSTRAP_PASSWORD:-admin}"
COOKIE=$(mktemp)
trap 'rm -f "$COOKIE"' EXIT

fail() { echo "FAIL: $*" >&2; exit 1; }
ok() { echo "OK: $*"; }

. "$(dirname "$0")/eval_json.sh"

# Login
code=$(curl -sS -c "$COOKIE" -o /tmp/pp_login.html -w '%{http_code}' \
  -X POST -d "username=$USER&password=$PASS" "$BASE/login" || echo 000)
# Accept 200 or 303/302
case "$code" in
  200|302|303) ok "login $code" ;;
  *) fail "login http $code" ;;
esac

# Health
h=$(curl -sS -o /tmp/pp_health.txt -w '%{http_code}' "$BASE/health" || echo 000)
[ "$h" = "200" ] || fail "health $h"
ok "health"

# Define a recursive fact for observability (via api/eval basic auth)
eval_json() {
  curl -sS -u "$USER:$PASS" -H 'Content-Type: application/json' \
    -d "{\"expr\":$(python3 -c 'import json,sys; print(json.dumps(sys.argv[1]))' "$1")}" \
    "$BASE/api/eval"
}

r=$(eval_json '(define obs/fact (lambda (n) (if (= n 0) 1 (* n (obs/fact (- n 1))))))')
printf '%s' "$r" | pp_json_ok || fail "define obs/fact: $r"
ok "define obs/fact"

r=$(eval_json '(obs/fact 5)')
printf '%s' "$r" | pp_json_ok || fail "call obs/fact: $r"
ok "call obs/fact (steps + function_stat)"

# Binding page
page=$(curl -sS -b "$COOKIE" "$BASE/binding/obs%2Ffact" || true)
echo "$page" | grep -q 'ast-walk\|ast-self-ref\|badge complexity' \
  || fail "binding page missing observability markers"
echo "$page" | grep -q 'complexity\|O(' || fail "binding page missing big-O badge"
echo "$page" | grep -q 'ast-self-ref\|recursive' || fail "binding page missing recursion mark"
ok "GET /binding/obs/fact observability"

# Bindings list still works
b=$(curl -sS -b "$COOKIE" -o /tmp/pp_bindings.html -w '%{http_code}' "$BASE/bindings")
[ "$b" = "200" ] || fail "bindings $b"
ok "GET /bindings"

# Inspect
i=$(curl -sS -b "$COOKIE" -o /tmp/pp_inspect.html -w '%{http_code}' \
  "$BASE/inspect/binding/obs%2Ffact")
[ "$i" = "200" ] || fail "inspect $i"
grep -q 'ast-walk\|badge complexity\|pretty' /tmp/pp_inspect.html \
  || fail "inspect missing content"
ok "GET /inspect/binding/obs/fact"

# Analyze (heuristic)
a=$(curl -sS -b "$COOKIE" -X POST -o /tmp/pp_analyze.html -w '%{http_code}' \
  "$BASE/api/analyze/obs%2Ffact")
[ "$a" = "200" ] || fail "analyze http $a"
grep -qi 'complexity\|O(\|heuristic\|LLM\|analyze' /tmp/pp_analyze.html \
  || fail "analyze body unexpected: $(head -c 200 /tmp/pp_analyze.html)"
ok "POST /api/analyze/obs/fact"

# Explain
e=$(curl -sS -b "$COOKIE" -X POST -o /tmp/pp_explain.html -w '%{http_code}' \
  "$BASE/api/explain/obs%2Ffact")
[ "$e" = "200" ] || fail "explain http $e"
grep -q 'explain\|call:\|if\|obs/fact\|recursive' /tmp/pp_explain.html \
  || fail "explain body unexpected"
ok "POST /api/explain/obs/fact"

# Prim paths
r=$(eval_json '(explain "obs/fact")')
printf '%s' "$r" | pp_json_ok || fail "explain prim: $r"
ok "(explain \"obs/fact\")"

r=$(eval_json '(analyze "obs/fact")')
# may fail without LLM key — accept ok or clean error string
echo "$r" | grep -Eq '"ok":(true|false)' || fail "analyze prim bad response: $r"
ok "(analyze \"obs/fact\") responded"

# Core endpoints still healthy
for path in /login /welcome /notes /pages /types /data /bindings /repl; do
  c=$(curl -sS -b "$COOKIE" -o /dev/null -w '%{http_code}' "$BASE$path" || echo 000)
  case "$c" in
    200|302|303) ok "GET $path -> $c" ;;
    *) echo "WARN: GET $path -> $c" ;;
  esac
done

echo "SMOKE_FUNCTION_OBSERVABILITY OK"
