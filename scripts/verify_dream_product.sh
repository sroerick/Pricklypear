#!/bin/ksh
# scripts/verify_dream_product.sh — monitor-rerunnable final verification
# for the dream product cut (login, base fns, editor, AI propose).
#
# Usage (from repo root):
#   scripts/verify_dream_product.sh
#
# Optional env (for AI check):
#   LLM_API_BASE, LLM_API_KEY, LLM_MODEL
#   (defaults loaded from ../TREE/.env if present)

set -e

PP_ROOT="${PP_ROOT:-$(cd "$(dirname "$0")/.." && pwd)}"
cd "$PP_ROOT"

if [ -f "../TREE/.env" ]; then
  # shellcheck disable=SC1091
  set -a
  . "../TREE/.env"
  set +a
fi

eval "$(opam env --switch=poohstack --set-switch)"

echo "[verify] dune build @all"
dune build @all

echo "[verify] dune runtest --force"
dune runtest --force

echo "[verify] restart dev stack"
scripts/dev.sh restart
sleep 3

# Drop stale image ports if any
fuser -k 18080/tcp >/dev/null 2>&1 || true
fuser -k 17878/tcp >/dev/null 2>&1 || true
sleep 1

echo "[verify] dream product smoke"
python3 scripts/dream_product_smoke.py

echo "[verify] ALL GREEN"
