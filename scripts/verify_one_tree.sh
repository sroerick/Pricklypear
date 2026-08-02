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
echo "$r" | grep -qE 'data\.(u\.|shared\.)' || fail "node-path (want data.u.* or data.shared.*): $r"
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


# 5. cross-user isolation (one-tree /data DECISION / BORGE-1785693032)
r=$(eval_json '{"expr":"(deftype (quote __otv_iso) (quote a))"}')
echo "$r" | grep -q 'defined type' || fail "iso deftype: $r"
r=$(eval_json '{"expr":"(create (quote __otv_iso) \"{\\\"a\\\":\\\"secret-alice\\\"}\")"}')
echo "$r" | grep -q '"ok":true' || fail "iso create: $r"
iso_id=$(echo "$r" | sed -n 's/.*\([0-9a-f]\{8\}-[0-9a-f]\{4\}-[0-9a-f]\{4\}-[0-9a-f]\{4\}-[0-9a-f]\{12\}\).*/\1/p' | head -1)
[ -n "$iso_id" ] || fail "iso no uuid: $r"

r=$(eval_json '{"expr":"(rows (quote __otv_iso))"}')
echo "$r" | grep -q 'secret-alice' || fail "admin should see own row: $r"
ok "admin sees own __otv_iso row"

# Prim path as a second user.
eval_json '{"expr":"(create-user \"bob\" \"bobpass\")"}' >/dev/null 2>&1 || true

bob_eval() {
  curl -s -u "bob:bobpass" \
    -X POST "$BASE/api/eval" \
    -H 'Content-Type: application/json' \
    --data-binary "$1"
}

bob_r=$(bob_eval '{"expr":"(whoami)"}' 2>/dev/null || true)
if echo "$bob_r" | grep -q '"ok":true' && echo "$bob_r" | grep -q bob; then
  bob_rows=$(bob_eval '{"expr":"(rows (quote __otv_iso))"}')
  echo "$bob_rows" | grep -q 'secret-alice' && fail "bob prim leaked admin row: $bob_rows"
  ok "prim confinement: bob (rows '__otv_iso) does not see admin secret"
else
  echo "WARN: bob basic-auth not available ($bob_r); skipping prim cross-user (RLS floor still checked)"
fi

# RLS floor: superusers bypass RLS even with FORCE. Test as a non-bypass role.
psql -h "$PP_SOCKET_DIR" -p "$PP_DB_PORT" -U "$PP_DB_USER" -d "$PP_DB_NAME" -v ON_ERROR_STOP=1 <<'SQL' >/dev/null
DO $$
BEGIN
  IF NOT EXISTS (SELECT 1 FROM pg_roles WHERE rolname = 'pp_rls_probe') THEN
    CREATE ROLE pp_rls_probe LOGIN PASSWORD 'probe';
  END IF;
END $$;
GRANT USAGE ON SCHEMA public TO pp_rls_probe;
GRANT SELECT ON tree_nodes, tree_data_rows TO pp_rls_probe;
-- table owner / superuser still bypass; probe must NOT be superuser
SQL

# set_config(..., true) is LOCAL and only sticks inside a real transaction.
# Emit ONLY digit lines so BEGIN/bob/ROLLBACK noise cannot poison the count.
bob_tree=$(PGPASSWORD=probe psql -h "$PP_SOCKET_DIR" -p "$PP_DB_PORT" -U pp_rls_probe -d "$PP_DB_NAME" -v ON_ERROR_STOP=1 -t -A <<SQL | grep -E '^[0-9]+$' | tail -1
BEGIN;
SELECT set_config('pp.user', 'bob', true);
SELECT count(*)::text FROM tree_nodes
 WHERE kind='datum' AND type_name='__otv_iso'
   AND tree_datum_id(path) = '$iso_id'::uuid;
ROLLBACK;
SQL
)
bob_tree=$(printf '%s' "$bob_tree" | tr -d '[:space:]')

bob_view=$(PGPASSWORD=probe psql -h "$PP_SOCKET_DIR" -p "$PP_DB_PORT" -U pp_rls_probe -d "$PP_DB_NAME" -v ON_ERROR_STOP=1 -t -A <<SQL | grep -E '^[0-9]+$' | tail -1
BEGIN;
SELECT set_config('pp.user', 'bob', true);
SELECT count(*)::text FROM tree_data_rows
 WHERE type_name='__otv_iso' AND id = '$iso_id'::uuid;
ROLLBACK;
SQL
)
bob_view=$(printf '%s' "$bob_view" | tr -d '[:space:]')

[ "$bob_tree" = "0" ] || fail "RLS leaked admin datum to bob (tree_nodes count=$bob_tree)"
[ "$bob_view" = "0" ] || fail "view leaked admin datum to bob (count=$bob_view)"
ok "RLS+view: non-superuser bob cannot see admin __otv_iso row"

psql -h "$PP_SOCKET_DIR" -p "$PP_DB_PORT" -U "$PP_DB_USER" -d "$PP_DB_NAME" -t -A \
  -c "SELECT path::text FROM tree_nodes WHERE kind='datum' AND tree_datum_id(path)='$iso_id'::uuid LIMIT 1;" \
  >/tmp/pp-tree-iso-path.out
grep -qE '^data\.u\.' /tmp/pp-tree-iso-path.out || fail "datum path not user-scoped: $(cat /tmp/pp-tree-iso-path.out)"
ok "datum path is data.u.* for non-seed type"

psql -h "$PP_SOCKET_DIR" -p "$PP_DB_PORT" -U "$PP_DB_USER" -d "$PP_DB_NAME" -q \
  -c "DELETE FROM type_defs WHERE name IN ('__otv','__otv_iso'); DELETE FROM data_rows WHERE type_name IN ('__otv','__otv_iso'); DELETE FROM tree_nodes WHERE kind='datum' AND type_name IN ('__otv','__otv_iso');" \
  >/dev/null 2>&1 || true

echo "=== ALL CHECKS PASSED ==="
exit 0
