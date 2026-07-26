#!/bin/ksh
# Monitor-rerunnable final verification for docstring-backfill
# Requires: local image on PP_URL with this worktree as PP_LIBS_DIR
set -e
cd /home/roerick/dev/wyo.tech/pricklypear
export PP_EVAL="${PP_EVAL:-$HOME/dev/wyo.tech/pricklypear/_build/default/cli/bin/pp_eval.exe}"
export PP_URL="${PP_URL:-http://127.0.0.1:18080}"
export PP_USER="${PP_USER:-admin}"
export PP_PASS="${PP_PASS:-admin}"

curl -sf -o /dev/null "$PP_URL/health" || { echo "health failed at $PP_URL"; exit 1; }

fail=0
for pkg in std layout welcome signup ai verify contacts ui bookmarks dabar \
           plan bi bible chat calendar outline notes flashcards todo hello weather ref; do
  r=$("$PP_EVAL" -j "(load-library \"$pkg\")") || true
  echo "$r" | grep -q '"ok":true' || { echo "load fail $pkg: $r"; fail=1; }
done

for n in html/join theme/bg todo/page notes/page flash/due chat/lobby-id \
         calendar/today outline/promote bible/ref ref/page weather/widget \
         hi-page signup/page dabar/page bookmarks/page contacts/page \
         ai/context verify/run-suite ui/css plan/mine bi/metric/profit; do
  r=$("$PP_EVAL" -j "(function-doc \"$n\")")
  echo "$r" | grep -q '"ok":true' || { echo "function-doc fail $n: $r"; fail=1; continue; }
  echo "$r" | grep -q '"value":()' && { echo "nodoc $n"; fail=1; }
done

# Routes (bare /ref walks all bindings and can budget-exceed — use filtered)
for path in /health /welcome /todos /notes /flashcards /chat /calendar; do
  if [ "$path" = /health ] || [ "$path" = /welcome ]; then
    code=$(curl -s -o /dev/null -w '%{http_code}' "$PP_URL$path")
  else
    code=$(curl -s -o /dev/null -w '%{http_code}' -u "$PP_USER:$PP_PASS" "$PP_URL$path")
  fi
  [ "$code" = "200" ] || { echo "http $path $code"; fail=1; }
done

code=$(curl -s -o /dev/null -w '%{http_code}' -u "$PP_USER:$PP_PASS" "$PP_URL/ref?q=html/join")
[ "$code" = "200" ] || { echo "http /ref?q=html/join $code"; fail=1; }
curl -s -u "$PP_USER:$PP_PASS" "$PP_URL/ref?q=html/join" \
  | grep -q 'Concatenate list of values' \
  || { echo "ref missing html/join docstring text"; fail=1; }

# String constants still values
r=$("$PP_EVAL" -j 'theme/bg')
echo "$r" | grep -q '#d4d0c8' || { echo "theme/bg corrupted: $r"; fail=1; }
r=$("$PP_EVAL" -j 'chat/lobby-id')
echo "$r" | grep -q 'lobby' || { echo "chat/lobby-id corrupted: $r"; fail=1; }

if [ "$fail" -ne 0 ]; then
  echo FAIL
  exit 1
fi
echo PASS
