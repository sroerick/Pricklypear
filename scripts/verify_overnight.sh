#!/bin/ksh
# scripts/verify_overnight.sh — monitor-rerunnable gate for overnight-foundation
#
# Priority coverage:
#   1) multi-file borge book balance/parse/check/lint
#   2) effects unit tests
#   3) full product smoke (includes AI propose)
#   4) llm-inner / llm-seed eval smokes (needs LLM_API_*)
#
# Usage (repo root):
#   scripts/verify_overnight.sh

set -e
PP_ROOT="${PP_ROOT:-$(cd "$(dirname "$0")/.." && pwd)}"
cd "$PP_ROOT"

if [ -f "../TREE/.env" ]; then
  set -a
  # shellcheck disable=SC1091
  . "../TREE/.env"
  set +a
fi

eval "$(opam env --switch=poohstack --set-switch)"

echo "[verify] borge multi-file book"
borge balance pricklypear.borg
borge parse pricklypear.borg >/dev/null
borge check
borge lint

echo "[verify] dune build + runtest"
dune build @all
dune runtest --force

echo "[verify] restart stack + product smoke"
scripts/dev.sh restart
sleep 3
fuser -k 18080/tcp >/dev/null 2>&1 || true
fuser -k 17878/tcp >/dev/null 2>&1 || true
sleep 1
python3 scripts/dream_product_smoke.py

echo "[verify] effects badges on /bindings"
printf ':quit\n' | scripts/repl.sh >/dev/null 2>&1 || true
sleep 1
curl -sS "http://127.0.0.1:18080/bindings" | grep -E -q 'pure|db-read|effects'

if [ -n "${LLM_API_KEY:-}" ]; then
  echo "[verify] llm-inner freeze"
  curl -sS -X POST "http://127.0.0.1:18080/api/eval" \
    -H 'content-type: application/json' \
    -d '{"expr":"(llm-inner \"ov-sq\" \"lambda of x returning (* x x)\")"}' \
    --max-time 180 | grep -q froze
  curl -sS -X POST "http://127.0.0.1:18080/api/eval" \
    -H 'content-type: application/json' \
    -d '{"expr":"(ov-sq 7)"}' | grep -q 49

  echo "[verify] llm-seed growth"
  curl -sS -X POST "http://127.0.0.1:18080/api/eval" \
    -H 'content-type: application/json' \
    -d '{"expr":"(llm-seed \"pure function that triples an integer\" 2)"}' \
    --max-time 300 | grep -E -q 'seed|DONE|froze'
  curl -sS -X POST "http://127.0.0.1:18080/api/eval" \
    -H 'content-type: application/json' \
    -d '{"expr":"(seed 5)"}' | grep -q 15
else
  echo "[verify] SKIP live llm-inner/seed (LLM_API_KEY unset)"
fi

echo "[verify] ALL GREEN"
