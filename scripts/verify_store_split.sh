#!/bin/ksh
# verify_store_split.sh — the store split invariants (i-1788671083-c).
#
# image/lib/store.ml was a ~2,700-line monolith mixing five domains AND
# the conn/tx substrate (include Store_db), with a Lisp-AST-typed API.
# It is now:
#   store_common    rel names + the ONE branch-chain CTE + staleness stamp
#   store_defs      def lookups / counts / fingerprints (opaque JSON bodies)
#   store_branch    branch status / persist / promote / merge / diff / fork
#   store_packages  package registry
#   store_history   code history / diff / restore
#   store_retention tree-time.gc retention worker
#   store           composition facade
# This script pins the two structural decisions the split made:
#   1. the facade does NOT re-export the substrate wholesale
#      (explicit accessors instead of `include Store_db`);
#   2. the store family stores OPAQUE jsonb bodies — no store_*.ml
#      module references the Lisp interpreter (AST codec lives at the
#      eval boundary: Pricklypear_lisp.Ast.expr_of_body_json).
#
# Needs: nothing beyond POSIX tools (no services).
# Run from anywhere; cd's to the repo root.
set -e
cd "$(dirname "$0")/.."

fail=0
facade=image/lib/store.ml
families="image/lib/store_common.ml image/lib/store_defs.ml image/lib/store_branch.ml image/lib/store_packages.ml image/lib/store_history.ml image/lib/store_retention.ml"

for f in $facade $families; do
  [ -f "$f" ] || { echo "FAIL: $f not found"; exit 1; }
done

# Strip comment lines (leading '(' or continuation '*') so doc mentions
# don't trip the code scans below.
code() { grep -vE '^[[:space:]]*[(*]' "$1" || true; }

# 1. Facade: explicit accessors, not a substrate include.
n=$(code "$facade" | grep -c 'include Store_db' || true)
if [ "$n" != "0" ]; then
  echo "FAIL: $facade still has $n 'include Store_db' (want 0: explicit accessors only)"
  grep -n 'include Store_db' "$facade"
  fail=1
fi
for acc in 'let current_conn = Store_db.current_conn' \
           'let exec = Store_db.exec' \
           'let with_transaction = Store_db.with_transaction'; do
  n=$(code "$facade" | grep -cF "$acc" || true)
  if [ "$n" != "1" ]; then
    echo "FAIL: facade missing explicit accessor ($n x): $acc"
    fail=1
  fi
done

# 2. Opaque bodies: no store family module (facade included) links the
#    interpreter — no AST types, no AST codec, no pretty-printer.
for f in $facade $families; do
  n=$(code "$f" | grep -cE 'Pricklypear_lisp\.(Ast|Reader|Eval)' || true)
  if [ "$n" != "0" ]; then
    echo "FAIL: $f references the Lisp interpreter ($n x) — the store must store opaque jsonb bodies"
    grep -nE 'Pricklypear_lisp\.(Ast|Reader|Eval)' "$f"
    fail=1
  fi
done

# 3. Facade composes every domain family exactly once; the substrate
#    helper module is referenced explicitly (visible_stamp), not included.
for fam in Store_defs Store_branch Store_packages Store_history Store_retention; do
  n=$(code "$facade" | grep -c "include $fam" || true)
  if [ "$n" != "1" ]; then
    echo "FAIL: facade includes $fam $n x (want 1)"
    fail=1
  fi
done
n=$(code "$facade" | grep -c 'let visible_stamp = Store_common.visible_stamp' || true)
if [ "$n" != "1" ]; then
  echo "FAIL: facade visible_stamp re-export missing ($n x)"
  fail=1
fi

# 4. chain_sql is defined once, in store_common (guarded in detail by
#    verify_store_chain_sql.sh; here just the one-definition check).
n=$(cat $families | grep -c 'let chain_sql' || true)
if [ "$n" != "1" ]; then
  echo "FAIL: chain_sql defined $n x across the family (want 1, in store_common)"
  fail=1
fi

if [ "$fail" = 0 ]; then
  echo "OK: store split invariants hold (facade w/ explicit accessors, opaque jsonb bodies, 6-family composition)"
fi
exit $fail
