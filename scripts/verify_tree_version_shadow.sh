#!/bin/ksh
# tree-time Phase 1 dual-write shadow verification (monitor-rerunnable).
#
# Verifies the 5.1 versioned-leaves write path: every tree_nodes write is
# shadowed into tree_versions (migration 018 tree_version_append trigger).
# Per tree-time.coexistence-plan PHASE 1, this checks the SHADOWED corpus
# (rows written since the trigger was attached) — rows that predate the
# trigger have no history, which is expected and out of scope (no backfill).
#
# This script is the PHASE 2 EQUIVALENCE GATE (tree-time.projections): the
# condition that must hold before the read path may switch to tree_versions.
# It therefore distinguishes AUTHENTIC divergence from the known deferred
# hash-normalization artifact:
#
#   * CONTENT equivalence (HARD gate): for every shadowed LIVE tree_nodes
#     row whose newest version row is NOT a tombstone, the live payload and
#     the newest version body must be CONTENT-identical. Comparison removes
#     the derived `hash` SUBFIELD from both envelopes, because the live
#     tree_nodes.hash is recomputed by the mirror over a differently-
#     normalized body (BORGE-1786931598, deferred single-hash-authority);
#     a hash-subfield-only delta is NOT a content divergence. This is the
#     "versioned head read == today's read" invariant.
#
#   * TOMBSTONE HEAD (HARD gate, the real block): no shadowed LIVE
#     tree_nodes row may have a TOMBSTONE as its newest version row. A
#     tombstone head means the node SHOULD be absent; a live row beneath a
#     tombstone head means a delete-then-recreate cycle wrote a tombstone
#     but the re-create did not append a new (non-tombstone) version row —
#     the corpus is not head-equivalent and the read switch must NOT run.
#
#   * HASH-ARTIFACT (REPORT ONLY): rows whose newest version differs from
#     live ONLY in the derived hash subfield are content-identical; they are
#     counted and reported, not failed (deferred BORGE-1786931598).
#
#   * HEAD COUNTERS (HARD gate): tree_generations_seq (the data.* head
#     sequence, 038) / branch_generations never move backwards relative to
#     the version rows they own.
#
# Exit 0 iff the HARD gates pass. Prerequisites: PG up via
# scripts/dev.sh start (dev PG, not prod).
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
warn() { echo "NOTE: $*"; }

echo "=== tree-time Phase 1 shadow verify (root=$PP_ROOT) ==="

PSQL="psql -h $PP_SOCKET_DIR -p $PP_DB_PORT -U $PP_DB_USER -d $PP_DB_NAME -t -A"

# Newest version row's '_tombstone' flag for a path (or '' when no history).
newest_tombstone='(SELECT COALESCE((x.body #>> '\''{_tombstone}'\'')::text,'\'''\'') FROM tree_versions x WHERE x.path = tn.path ORDER BY x.gen DESC LIMIT 1)'

# 1. migration 018 applied + trigger attached
ver=$($PSQL -c "SELECT version FROM schema_migrations WHERE version=18 LIMIT 1" 2>&1)
[ "$ver" = "18" ] || fail "migration 018 not applied (got '$ver')"
trg=$($PSQL -c "SELECT count(*) FROM pg_trigger WHERE tgname='tree_version_append_trg' AND tgrelid='tree_nodes'::regclass AND NOT tgisinternal" 2>&1)
[ "$trg" = "1" ] || fail "tree_version_append_trg not attached (count=$trg)"
ok "migration 18 applied; tree_version_append_trg attached to tree_nodes"

# 2. CONTENT-EQUIVALENCE gate: non-tombstone-newest shadowed live rows must be
#    content-identical to their newest version body (envelope `hash` subfield
#    excluded — deferred BORGE-1786931598). Removing the hash subfield means a
#    live mirror-recomputed hash alone never trips this gate.
content_diff=$($PSQL -c "
SELECT count(*) FROM tree_nodes tn
WHERE EXISTS (SELECT 1 FROM tree_versions v0 WHERE v0.path = tn.path)
  AND $newest_tombstone <> 'true'
  AND NOT EXISTS (
    SELECT 1 FROM tree_versions v
    WHERE v.path = tn.path
      AND v.gen = (SELECT max(x.gen) FROM tree_versions x WHERE x.path = tn.path)
      AND (v.body - 'hash') = (tn.payload - 'hash')
  )" 2>&1)
[ "$content_diff" = "0" ] || fail "$content_diff shadowed live tree_nodes row(s) differ in CONTENT from newest version (read-switch blocked)"
ok "every non-tombstone shadowed live tree_nodes row's newest version is content-identical to head"

# 3. TOMBSTONE-HEAD gate: a tombstone as the newest version of a LIVE row means
#    the delete/recreate history is not head-equivalent — hard block for the
#    read switch. Reported by kind for triage.
tomb=$($PSQL -c "
SELECT count(*) FROM tree_nodes tn
WHERE EXISTS (SELECT 1 FROM tree_versions v0 WHERE v0.path = tn.path)
  AND $newest_tombstone = 'true'" 2>&1)
if [ "$tomb" != "0" ]; then
  echo "--- tombstone-head divergences by kind (read-switch blocker) ---"
  $PSQL -c "
SELECT tn.kind, count(*) FROM tree_nodes tn
WHERE EXISTS (SELECT 1 FROM tree_versions v0 WHERE v0.path = tn.path)
  AND $newest_tombstone = 'true'
GROUP BY tn.kind ORDER BY 2 DESC" 2>&1
  fail "$tomb shadowed live tree_nodes row(s) have TOMBSTONE as newest version — delete-then-recreate did not re-shadow; reconcile before the Phase-2 read switch"
fi
ok "no TOMBSTONE at head for any shadowed live tree_nodes row"

# 4. HASH-ARTIFACT report (deferred BORGE-1786931598): content-identical rows
#    whose whole payload (incl. derived hash subfield) differs from the
#    version body. Informational; not a hard gate.
hash_art=$($PSQL -c "
SELECT count(*) FROM tree_nodes tn
WHERE EXISTS (SELECT 1 FROM tree_versions v0 WHERE v0.path = tn.path)
  AND $newest_tombstone <> 'true'
  AND EXISTS (SELECT 1 FROM tree_versions v2 WHERE v2.path = tn.path
              AND v2.gen = (SELECT max(x2.gen) FROM tree_versions x2 WHERE x2.path = tn.path)
              AND (v2.body - 'hash') = (tn.payload - 'hash'))
  AND NOT EXISTS (SELECT 1 FROM tree_versions v3 WHERE v3.path = tn.path
              AND v3.gen = (SELECT max(x3.gen) FROM tree_versions x3 WHERE x3.path = tn.path)
              AND v3.body = tn.payload)" 2>&1)
[ -n "$hash_art" ] && [ "$hash_art" != "0" ] && \
  warn "$hash_art content-identical row(s) differ only in the derived hash subfield (deferred BORGE-1786931598)"

# 5. head counters never move backwards relative to version rows.
#    data: tree_generations_seq last_value >= max(gen) over data.* version rows
#      (the data.* head is the SEQUENCE since migration 038 — BORGE-1790987438;
#      the old tree_generations singleton row is frozen and no longer written,
#      so the sequence is the authority here).
#    code: branch_generations.generation >= max(gen) over that branch's b.* rows.
dg=$($PSQL -c "
SELECT CASE WHEN (SELECT last_value FROM tree_generations_seq) >=
                 COALESCE((SELECT max(gen) FROM tree_versions WHERE branch_id IS NULL), 0)
            THEN 'ok' ELSE 'BAD' END" 2>&1)
[ "$dg" = "ok" ] || fail "tree_generations head behind data version rows ($dg)"
cg=$($PSQL -c "
SELECT CASE WHEN count(*) = 0 THEN 'ok' ELSE 'BAD' END FROM (
  SELECT bg.branch_id FROM branch_generations bg
  WHERE bg.generation < COALESCE(
    (SELECT max(v.gen) FROM tree_versions v
     WHERE v.branch_id = bg.branch_id
       AND EXISTS (SELECT 1 FROM tree_nodes tn WHERE tn.path = v.path)), 0)
) t" 2>&1)
[ "$cg" = "ok" ] || fail "branch_generations head behind a branch's code version rows ($cg)"
ok "tree_generations_seq and branch_generations heads stay ahead of version rows"

# 6. aggregate info for the operator / monitor.
r=$($PSQL -c "SELECT count(*) FROM tree_versions")
t=$($PSQL -c "SELECT count(DISTINCT path) FROM tree_versions")
n=$($PSQL -c "SELECT count(*) FROM tree_nodes")
echo "INFO: tree_versions rows=$r distinct-paths-with-history=$t live-tree-nodes=$n"

echo "PASS: tree-time Phase 1 dual-write shadow verified ($r version rows, $t shadowed paths)"
