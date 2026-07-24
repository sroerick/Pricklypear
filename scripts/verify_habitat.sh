#!/bin/ksh
# Habitat product verification — Lisp-on-image (libs/verify), not Python.
#
# Usage (local):
#   scripts/dev.sh start
#   export PP_URL=http://127.0.0.1:18080 PP_USER=admin PP_PASS=admin
#   scripts/verify_habitat.sh
#
# Habitat:
#   set -a; . ~/.config/pricklypear/env; set +a
#   scripts/verify_habitat.sh
#
# Exit 0 on ALL PASSED.

set -e
PP_ROOT="${PP_ROOT:-$(CDPATH= cd -- "$(dirname "$0")/.." && pwd)}"
cd "$PP_ROOT"

if [ -z "${PP_URL:-}" ]; then
  PP_URL="http://127.0.0.1:18080"
fi
export PP_URL
export PP_USER="${PP_USER:-admin}"
export PP_PASS="${PP_PASS:-admin}"

# Prefer OCaml pp-eval (no Python).
PP_EVAL_BIN="${PP_EVAL_BIN:-}"
if [ -z "$PP_EVAL_BIN" ]; then
  if [ -n "${PP_EVAL:-}" ] && [ -x "$PP_EVAL" ] && ! echo "$PP_EVAL" | grep -q '\.py$'; then
    PP_EVAL_BIN="$PP_EVAL"
  elif [ -x "$PP_ROOT/_build/default/cli/bin/pp_eval.exe" ]; then
    PP_EVAL_BIN="$PP_ROOT/_build/default/cli/bin/pp_eval.exe"
  elif command -v pp-eval >/dev/null 2>&1; then
    PP_EVAL_BIN="pp-eval"
  else
    PP_EVAL_BIN="$PP_ROOT/scripts/pp-eval.sh"
  fi
fi
export PP_EVAL_BIN

eval_j() {
  "$PP_EVAL_BIN" -j "$1"
}

echo "[verify_habitat] PP_URL=$PP_URL"
echo "[verify_habitat] load verify lib"
eval_j '(load-library "verify")' >/dev/null
eval_j '(load-library "outline")' >/dev/null
eval_j '(load-library "notes")' >/dev/null

fails=0
# One suite per eval — preserves step budget (notes.budget-scale).
for suite in extract-links list-hof budget-parse outline-render \
             upsert-reindex extra-actions-contract
do
  echo "[verify_habitat] suite $suite"
  out=$(eval_j "(verify/summary-of (verify/run-suite \"$suite\"))" 2>&1) || {
    echo "$out"
    echo "[verify_habitat] FAIL: eval error in $suite"
    fails=$((fails + 1))
    continue
  }
  echo "  $out"
  echo "$out" | grep -q 'ALL PASSED' || {
    echo "[verify_habitat] FAIL: suite $suite"
    fails=$((fails + 1))
  }
done

if [ "$fails" -ne 0 ]; then
  echo "[verify_habitat] $fails suite(s) failed"
  exit 1
fi

# HTTP smoke (ksh+curl only — no domain logic)
if command -v curl >/dev/null 2>&1; then
  code=$(curl -sS -o /dev/null -w "%{http_code}" -u "$PP_USER:$PP_PASS" \
    "$PP_URL/notes?slug=inbox" || echo "000")
  echo "[verify_habitat] GET /notes?slug=inbox -> $code"
  if [ "$code" != "200" ]; then
    echo "[verify_habitat] FAIL: inbox not 200"
    exit 1
  fi
  sfu=$(eval_j \
    '(select-for-update (quote note) "00000000-0000-0000-0000-000000000001")' 2>&1 || true)
  echo "$sfu" | grep -qi 'with-transaction' || {
    echo "[verify_habitat] FAIL: expected select-for-update outside-tx error"
    echo "$sfu"
    exit 1
  }
  echo "[verify_habitat] select-for-update outside tx -> clean error"
else
  echo "[verify_habitat] curl missing; skip HTTP checks"
fi

echo "[verify_habitat] ALL GREEN"
exit 0
