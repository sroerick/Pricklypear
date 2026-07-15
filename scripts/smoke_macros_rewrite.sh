#!/bin/sh
# Smoke: std macros + rewritten packages.
set -eu
export PP_URL="${PP_URL:-http://127.0.0.1:18080}"
export PP_USER="${PP_USER:-admin}"
export PP_PASS="${PP_PASS:-admin}"
EVAL="${PP_EVAL:-python3 scripts/pp_eval.py}"
cd "$(dirname "$0")/.."
j() { $EVAL -j "$1"; }

echo "== kernel =="
j '(cond (#f 1) (else 2))'
j '(and #t 3)'
j '(or #f 4)'

echo "== std macros =="
j '(when #t 1)'
j '(unless #f 2)'
j '(blank? "")'
j '(blank? "x")'

echo "== packages bound =="
j '(type-of todo/ui/handle-post)'
j '(type-of contacts/page)'
j '(type-of notes/ui/handle-post)'
j '(type-of bookmarks/handle-post)'
j '(type-of calendar/handle-post)'

echo "== HTTP pages =="
rm -f /tmp/pp-macro-cookie
curl -sS -c /tmp/pp-macro-cookie -b /tmp/pp-macro-cookie -X POST "$PP_URL/login" \
  --data-urlencode "username=$PP_USER" --data-urlencode "password=$PP_PASS" -o /dev/null
for p in /contacts /bookmarks /todos /notes /plan /chat /calendar /flashcards; do
  code=$(curl -sS -o /tmp/pp-macro-page.html -w '%{http_code}' -b /tmp/pp-macro-cookie "$PP_URL$p")
  if [ "$code" != "200" ]; then
    echo "FAIL $p -> $code"
    exit 1
  fi
  if grep -q 'Interp_error\|unbound: class' /tmp/pp-macro-page.html 2>/dev/null; then
    # allow todo titles that mention unbound from ops tests
    if [ "$p" != "/todos" ]; then
      echo "FAIL $p has error text"
      exit 1
    fi
  fi
  echo "OK $p"
done

echo "SMOKE_MACROS_REWRITE OK"
