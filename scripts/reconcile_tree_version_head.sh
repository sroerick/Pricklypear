#!/bin/ksh
# tree-time head reconciliation runner (monitor-rerunnable).
#
# Reconciles tree_versions with the LIVE tree_nodes authority for the one
# non-head-equivalent class the Phase-1 verify gate reports as a hard block:
# a shadowed LIVE tree_nodes row whose newest tree_versions row is a
# TOMBSTONE (delete-then-recreate whose re-create did not re-shadow). It runs
# the additive, idempotent tree_versions_reconcile_heads() (migration 021),
# which appends a fresh non-tombstone version row from the live content so
# versioned-head reads == today's reads, then re-runs the Phase-1 verify gate
# (scripts/verify_tree_version_shadow.sh) to confirm the corpus is now
# head-equivalent (or reports whatever divergence remains).
#
# NOTE: this only mutates the tree_versions history table (append-only),
# never tree_nodes and never the scaffolding mirrors. It is safe to run
# while the image serves; healed paths are simply re-versioned from the live
# row. It is NOT a substitute for finding WHY a path stopped re-shadowing —
# if a live bug caused the divergence it may reappear; see the plan log.
#
# Prerequisites: PG up via scripts/dev.sh start. Exits 0 iff the Phase-1
# verify gate passes after reconciliation.
# Env (defaults match scripts/dev.sh):
#   PP_DB_PORT=5433 PP_SOCKET_DIR=/tmp PP_DB_USER=postgres PP_DB_NAME=pricklypear

set -e
PP_ROOT="${PP_ROOT:-$(CDPATH= cd -- "$(dirname "$0")/.." && pwd)}"
PP_DB_PORT="${PP_DB_PORT:-5433}"
PP_SOCKET_DIR="${PP_SOCKET_DIR:-/tmp}"
PP_DB_USER="${PP_DB_USER:-postgres}"
PP_DB_NAME="${PP_DB_NAME:-pricklypear}"

echo "=== tree-time head reconcile (root=$PP_ROOT) ==="

PSQL="psql -h $PP_SOCKET_DIR -p $PP_DB_PORT -U $PP_DB_USER -d $PP_DB_NAME -t -A"

PSQL_PLAIN="psql -h $PP_SOCKET_DIR -p $PP_DB_PORT -U $PP_DB_USER -d $PP_DB_NAME"

ver=$($PSQL -c "SELECT version FROM schema_migrations WHERE version=21 LIMIT 1" 2>&1)
if [ "$ver" != "21" ]; then
  echo "migration 21 not applied — applying migrations/021_tree_versions_reconcile.sql"
  $PSQL_PLAIN -f "$PP_ROOT/migrations/021_tree_versions_reconcile.sql" >/dev/null
  ver=$($PSQL -c "SELECT version FROM schema_migrations WHERE version=21 LIMIT 1" 2>&1)
fi
[ "$ver" = "21" ] || { echo "FAIL: migration 021 could not be applied ($ver)" >&2; exit 1; }

healed=$($PSQL -c "SELECT tree_versions_reconcile_heads()" 2>&1)
echo "healed $healed path(s) (tombstone-head → live content re-versioned)"

# Re-run the Phase-1 verify gate; it must now pass.
ksh "$PP_ROOT/scripts/verify_tree_version_shadow.sh"
