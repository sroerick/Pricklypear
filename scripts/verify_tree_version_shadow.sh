#!/bin/ksh
# tree-time Phase 1 dual-write shadow verification (monitor-rerunnable).
#
# Verifies the 5.1 versioned-leaves write path: every tree_nodes write is
# shadowed into tree_versions (migration 018 tree_version_append trigger).
# Per tree-time.coexistence-plan PHASE 1, this checks the SHADOWED corpus
# (rows written since the trigger was attached) — rows that predate the
# trigger have no history, which is expected and out of scope (no backfill).
# Concretely:
#   1. migration 018 is applied and the tree_version_append trigger is
#      attached to tree_nodes;
#   2. every shadowed LIVE tree_nodes row has a matching newest non-tombstone
#      tree_versions row whose body/hash equal the live payload/hash
#      (versioned head read == today's read);
#   3. no shadowed LIVE tree_nodes row has a TOMBSTONE as its newest version
#      row (a tombstone head means the tree node should be absent);
#   4. the data head counter (tree_generations) and per-branch code heads
#      (branch_generations) are monotonic and never behind the max gen in
#      tree_versions for their path class (head never moves backwards).
#
# Prerequisites: PG up via scripts/dev.sh start (dev PG, not prod). Exits 0
# if all checks pass. Run: ksh scripts/verify_tree_version_shadow.sh
#
# Env (defaults match scripts/dev.sh):
#   PP_DB_PORT=5433 PP_SOCKET_DIR=/tmp PP_DB_USER=postgres PP_DB_NAME=pricklypear

set -e
PP_ROOT="${PP_ROOT:-$(CDPATH= cd -- "$(dirname "$0")/.." && pwd)}"
PP_DB_PORT="${PP_DB_PORT:-5433}"
PP_SOCKET_DIR="${PP_SOCKET_DIR:-/tmp}"
PP_DB_USER="${PP_DB_USER:-postgres}"
PP_DB_NAME="${PP_DB_NAME:-pricklypear}"

fail() { echo "FAIL: $*" >&2; exit 1; }
ok() { echo "OK: $*"; }

echo "=== tree-time Phase 1 shadow verify (root=$PP_ROOT) ==="

PSQL="psql -h $PP_SOCKET_DIR -p $PP_DB_PORT -U $PP_DB_USER -d $PP_DB_NAME -t -A"

# 1. migration 018 applied + trigger attached
ver=$($PSQL -c "SELECT version FROM schema_migrations WHERE version=18 LIMIT 1" 2>&1)
[ "$ver" = "18" ] || fail "migration 018 not applied (got '$ver')"
trg=$($PSQL -c "SELECT count(*) FROM pg_trigger WHERE tgname='tree_version_append_trg' AND tgrelid='tree_nodes'::regclass AND NOT tgisinternal" 2>&1)
[ "$trg" = "1" ] || fail "tree_version_append_trg not attached (count=$trg)"
ok "migration 18 applied; tree_version_append_trg attached to tree_nodes"

# 2. shadowed live tree_nodes rows (those WITH version history) must have a
#    matching newest version row (hash/body). Rows without history predate the
#    trigger — expected for a freshly-started Phase 1 shadow, out of scope.
nv=$($PSQL -c "
SELECT count(*) FROM tree_nodes tn
WHERE EXISTS (SELECT 1 FROM tree_versions v0 WHERE v0.path = tn.path)
  AND NOT EXISTS (
    SELECT 1 FROM tree_versions v
    WHERE v.path = tn.path
      AND v.body::text = tn.payload::text
      AND v.hash   = tn.hash
      AND v.gen = (SELECT max(x.gen) FROM tree_versions x WHERE x.path = tn.path)
  )" 2>&1)
[ "$nv" = "0" ] || fail "$nv shadowed live tree_nodes row(s) lack a matching newest version row"
ok "every shadowed live tree_nodes row's newest version row matches head (hash+body)"

# 3. no shadowed live tree_nodes row has a TOMBSTONE as newest version row.
tomb=$($PSQL -c "
SELECT count(*) FROM tree_nodes tn
WHERE EXISTS (SELECT 1 FROM tree_versions v0 WHERE v0.path = tn.path)
  AND (SELECT (x.body #>> '{_tombstone}')::text FROM tree_versions x
       WHERE x.path = tn.path ORDER BY x.gen DESC LIMIT 1) = 'true'" 2>&1)
[ "$tomb" = "0" ] || fail "$tomb shadowed live tree_nodes row(s) have TOMBSTONE as newest version"
ok "no TOMBSTONE at head for any shadowed live tree_nodes row"

# 4. head counters never move backwards relative to version rows.
#    data: tree_generations.generation >= max(gen) over data.* version rows.
#    code: branch_generations.generation >= max(gen) over that branch's b.* rows.
dg=$($PSQL -c "
SELECT CASE WHEN (SELECT generation FROM tree_generations LIMIT 1) >=
                 COALESCE((SELECT max(gen) FROM tree_versions WHERE branch_id IS NULL), 0)
            THEN 'ok' ELSE 'BAD' END" 2>&1)
[ "$dg" = "ok" ] || fail "tree_generations head behind data version rows ($dg)"
cg=$($PSQL -c "
SELECT CASE WHEN count(*) = 0 THEN 'ok' ELSE 'BAD' END FROM (
  SELECT bg.branch_id FROM branch_generations bg
  WHERE bg.generation < COALESCE(
    (SELECT max(v.gen) FROM tree_versions v WHERE v.branch_id = bg.branch_id), 0)
) t" 2>&1)
[ "$cg" = "ok" ] || fail "branch_generations head behind a branch's code version rows ($cg)"
ok "tree_generations and branch_generations heads stay ahead of version rows"

# 5. aggregate info for the operator / monitor.
r=$($PSQL -c "SELECT count(*) FROM tree_versions")
t=$($PSQL -c "SELECT count(DISTINCT path) FROM tree_versions")
n=$($PSQL -c "SELECT count(*) FROM tree_nodes")
echo "INFO: tree_versions rows=$r distinct-paths-with-history=$t live-tree-nodes=$n"

echo "PASS: tree-time Phase 1 dual-write shadow verified ($r version rows, $t shadowed paths)"
