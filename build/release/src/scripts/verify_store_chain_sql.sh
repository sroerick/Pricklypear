#!/bin/ksh
# verify_store_chain_sql.sh — the branch-chain recursive CTE lives in
# exactly ONE place (Store_common.chain_sql), and every SQL walk over the
# branch parent chain in the store modules goes through it.
#
# Why: the closest-ancestor-wins walk is the semantic heart of the
# system; it was copy-pasted 12x in slightly divergent spellings
# (i-1788671262-4), so any fix to the walk (cycle guard, depth cap)
# had to be applied 12 times — and a missed copy silently diverged.
# This is a plain text scan of the module sources, so it catches a
# re-introduced inline WITH RECURSIVE even before any DB is up.
#
# i-1788671083-c: store.ml was split into store_{common,defs,branch,
# packages,history,retention}.ml — the walk lives in store_common.ml,
# the one sanctioned second chain (diff_branches' chain_to leg) in
# store_branch.ml.
#
# Needs: nothing beyond POSIX tools (no services).
# Run from anywhere; cd's to the repo root.
set -e
cd "$(dirname "$0")/.."

common=image/lib/store_common.ml
branch=image/lib/store_branch.ml
for src in "$common" "$branch"; do
  [ -f "$src" ] || { echo "FAIL: $src not found"; exit 1; }
done

fail=0

# 1. Exactly one WITH RECURSIVE literal across the whole store family:
#    the chain_sql helper's own.
#    (Any second literal is a copy-paste of the walk — the exact drift
#    the helper exists to kill.)
with_recursive=$(cat image/lib/store_common.ml image/lib/store_defs.ml \
                   image/lib/store_branch.ml image/lib/store_packages.ml \
                   image/lib/store_history.ml image/lib/store_retention.ml \
                   image/lib/store.ml | grep -c '"WITH RECURSIVE' || true)
if [ "$with_recursive" != "1" ]; then
  echo "FAIL: store family has $with_recursive '\"WITH RECURSIVE' literals (want 1, the chain_sql helper)"
  grep -n '"WITH RECURSIVE' image/lib/store_common.ml image/lib/store_defs.ml \
    image/lib/store_branch.ml image/lib/store_packages.ml \
    image/lib/store_history.ml image/lib/store_retention.ml image/lib/store.ml || true
  fail=1
fi

# 2. The helper is defined once (in store_common.ml) and takes the walk
#    shape (anchor, depth column, parent-join) — the canonical spelling.
for frag in 'let chain_sql ?(cte = "chain") ?(anchor = "$1") body' \
            'SELECT id, parent_id, 0 AS depth FROM branches WHERE id = ' \
            'SELECT b.id, b.parent_id, c.depth + 1'; do
  n=$(grep -cF "$frag" "$common" || true)
  if [ "$n" -lt 1 ]; then
    echo "FAIL: chain_sql fragment missing ($n x) in $common: $frag"
    fail=1
  fi
done

# 3. Every caller goes through the helper: the recursive parent-join
#    spelling appears exactly twice across the family — once in the
#    helper, once for the ONE sanctioned body-embedded second chain
#    (diff_branches' chain_to leg, inside a chain_sql body). A third
#    occurrence is a new copy-paste of the walk.
n=$(cat "$common" "$branch" | grep -c 'b.id = c.parent_id' || true)
if [ "$n" != "2" ]; then
  echo "FAIL: store family has $n recursive parent-joins (want 2: chain_sql + diff_branches' chain_to leg)"
  grep -n 'b.id = c.parent_id' "$common" "$branch" || true
  fail=1
fi

if [ "$fail" = 0 ]; then
  echo "OK: branch-chain CTE defined once (Store_common.chain_sql); store family has no inline copies"
fi
exit $fail
