#!/bin/ksh
# Monitor-rerunnable auth verification.
# Working directory: repo root.
set -e
PP_ROOT="${PP_ROOT:-$(cd "$(dirname "$0")/.." && pwd)}"
cd "$PP_ROOT"

eval "$(opam env --switch=poohstack --set-switch)"

echo "[verify_auth] dune build @all"
dune build @all

echo "[verify_auth] dune runtest"
dune runtest --force

echo "[verify_auth] ensuring dev stack"
scripts/dev.sh start

# Give image a moment after start
sleep 1

export PP_HTTP_PORT="${PP_HTTP_PORT:-18080}"
export PP_BOOTSTRAP_USER="${PP_BOOTSTRAP_USER:-admin}"
export PP_BOOTSTRAP_PASSWORD="${PP_BOOTSTRAP_PASSWORD:-admin}"

echo "[verify_auth] auth product smoke"
python3 scripts/auth_product_smoke.py

echo "[verify_auth] ALL GREEN"
