#!/bin/ksh
# tests/sql/run-rls-equivalence.sh — one command for the perf-1104 floor.
#
# Builds a scratch database, applies every migration in order, provisions the
# two roles the wrappers need, and asserts wrapper output == RLS-enforced
# output per user. Drops the scratch DB afterwards (keep it with KEEP=1).
#
#   ksh tests/sql/run-rls-equivalence.sh
#
# Connection comes from the usual libpq vars (PGHOST/PGPORT/PGUSER). It needs a
# role that can create databases and roles: postgres on mainframe.
set -e

: "${PGHOST:=/tmp}"
: "${PGPORT:=5432}"
: "${PGUSER:=postgres}"
export PGHOST PGPORT PGUSER

here=$(cd "$(dirname "$0")/../.." && pwd)
db="pp_rls_equiv_$$"

cleanup() {
  if [ "${KEEP:-0}" = "1" ]; then
    echo "[rls-equiv] keeping scratch db $db"
  else
    dropdb --if-exists "$db" >/dev/null 2>&1 || true
  fi
}
trap cleanup EXIT

echo "[rls-equiv] scratch db $db on $PGHOST:$PGPORT as $PGUSER"
createdb "$db"
export PGDATABASE="$db"

for m in "$here"/migrations/[0-9][0-9][0-9]_*.sql; do
  psql -X -q -v ON_ERROR_STOP=1 -f "$m" >/dev/null
done
echo "[rls-equiv] migrations applied"

psql -X -q -v ON_ERROR_STOP=1 -f "$here/tests/sql/rls_definer_role.sql"
psql -X -v ON_ERROR_STOP=1 -f "$here/tests/sql/rls_equivalence.sql"

psql -X -t -A -c 'SELECT pp_rls_wrappers_mode()' | sed 's/^/[rls-equiv] /'
