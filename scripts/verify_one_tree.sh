#!/bin/ksh
# one-tree final verification (monitor-rerunnable).
#
# Prerequisites:
#   - Postgres + image up via: scripts/dev.sh start
#     (or already running on PP_HTTP_PORT / PP_DB_PORT)
#   - Built tree: eval $(opam env --switch=poohstack --set-switch)
#                 dune build @all
#
# Env (defaults match scripts/dev.sh):
#   PP_HTTP_PORT=18080  PP_DB_PORT=5433  PP_SOCKET_DIR=/tmp
#   PP_DB_USER=postgres  PP_DB_NAME=pricklypear
#   PP_BASIC_USER=admin  PP_BASIC_PASS=admin
#
# Exit 0 = all checks pass.

set -e
PP_ROOT="${PP_ROOT:-$(CDPATH= cd -- "$(dirname "$0")/.." && pwd)}"
PP_HTTP_PORT="${PP_HTTP_PORT:-18080}"
PP_DB_PORT="${PP_DB_PORT:-5433}"
PP_SOCKET_DIR="${PP_SOCKET_DIR:-/tmp}"
PP_DB_USER="${PP_DB_USER:-postgres}"
PP_DB_NAME="${PP_DB_NAME:-pricklypear}"
PP_BASIC_USER="${PP_BASIC_USER:-admin}"
PP_BASIC_PASS="${PP_BASIC_PASS:-admin}"
BASE="http://127.0.0.1:${PP_HTTP_PORT}"

fail() { echo "FAIL: $*" >&2; exit 1; }
ok() { echo "OK: $*"; }

echo "=== one-tree verify (root=$PP_ROOT) ==="

# 1. build + unit tests
if command -v opam >/dev/null 2>&1; then
  eval "$(opam env --switch=poohstack --set-switch)" 2>/dev/null || true
fi
cd "$PP_ROOT" || fail "cd $PP_ROOT"
dune build @all || fail "dune build @all"
dune runtest image/test/tree_store_test.exe || fail "tree_store_test"
ok "dune build + tree_store_test"

# 2. PG tree_verify lockstep
psql -h "$PP_SOCKET_DIR" -p "$PP_DB_PORT" -U "$PP_DB_USER" -d "$PP_DB_NAME" -v ON_ERROR_STOP=1 -t -A <<'SQL' >/tmp/pp-tree-verify.out
SELECT kind || '=' || in_sync::text FROM tree_verify() ORDER BY kind;
SELECT bool_and(in_sync)::text FROM tree_verify();
SQL
grep -qE 'fn=t(rue)?' /tmp/pp-tree-verify.out || fail "fn not in_sync"
grep -qE 'type=t(rue)?' /tmp/pp-tree-verify.out || fail "type not in_sync"
grep -qE 'route=t(rue)?' /tmp/pp-tree-verify.out || fail "route not in_sync"
grep -qE 'datum=t(rue)?' /tmp/pp-tree-verify.out || fail "datum not in_sync"
tail -1 /tmp/pp-tree-verify.out | grep -qxE 't(rue)?' || fail "bool_and in_sync"
ok "tree_verify all kinds in_sync"

# 3. HTTP health
code=$(curl -s -o /dev/null -w '%{http_code}' "$BASE/health")
[ "$code" = "200" ] || fail "/health got $code"
ok "/health 200"

# 4. eval smokes (tree reads/writes default ON)
eval_json() {
  curl -s -u "${PP_BASIC_USER}:${PP_BASIC_PASS}" \
    -X POST "$BASE/api/eval" \
    -H 'Content-Type: application/json' \
    --data-binary "$1"
}

r=$(eval_json '{"expr":"(list (whoami) (count (list-bindings)))"}')
echo "$r" | grep -q '"ok":true' || fail "whoami/bindings: $r"
ok "eval whoami + list-bindings"

r=$(eval_json '{"expr":"(deftype (quote __otv) (quote a) (quote b))"}')
echo "$r" | grep -q 'defined type' || fail "deftype: $r"
r=$(eval_json '{"expr":"(type-fields (quote __otv))"}')
echo "$r" | grep -q 'a' || fail "type-fields: $r"
r=$(eval_json '{"expr":"(create (quote __otv) \"{\\\"a\\\":1}\")"}')
echo "$r" | grep -q 'missing fields' || fail "validation should reject: $r"
ok "typed-node validation rejects missing fields"

r=$(eval_json '{"expr":"(create (quote __otv) \"{\\\"a\\\":1,\\\"b\\\":2}\")"}')
echo "$r" | grep -q '"ok":true' || fail "create full: $r"
id=$(echo "$r" | sed -n 's/.*\([0-9a-f]\{8\}-[0-9a-f]\{4\}-[0-9a-f]\{4\}-[0-9a-f]\{4\}-[0-9a-f]\{12\}\).*/\1/p' | head -1)
[ -n "$id" ] || fail "no uuid from create: $r"
r=$(eval_json "{\"expr\":\"(node-path (quote __otv) \\\"$id\\\")\"}")
echo "$r" | grep -q 'data\.' || fail "node-path: $r"
r=$(eval_json "{\"expr\":\"(get (quote __otv) \\\"$id\\\")\"}")
echo "$r" | grep -q '"ok":true' || fail "get: $r"
r=$(eval_json "{\"expr\":\"(delete (quote __otv) \\\"$id\\\")\"}")
echo "$r" | grep -q "$id" || fail "delete: $r"
ok "CRUD + node-path round-trip"

# cleanup type def (best-effort)
psql -h "$PP_SOCKET_DIR" -p "$PP_DB_PORT" -U "$PP_DB_USER" -d "$PP_DB_NAME" -q \
  -c "DELETE FROM type_defs WHERE name='__otv'; DELETE FROM data_rows WHERE type_name='__otv';" \
  >/dev/null 2>&1 || true

psql -h "$PP_SOCKET_DIR" -p "$PP_DB_PORT" -U "$PP_DB_USER" -d "$PP_DB_NAME" -t -A \
  -c "SELECT bool_and(in_sync) FROM tree_verify();" | grep -qxE 't(rue)?' \
  || fail "post-cleanup tree_verify"
ok "post-cleanup tree_verify"

echo "=== ALL CHECKS PASSED ==="
exit 0
