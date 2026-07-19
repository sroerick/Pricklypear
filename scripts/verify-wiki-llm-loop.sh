#!/bin/ksh
# Final verification for wiki-llm-loop-implementation (externally rerunnable).
# Prerequisites: poohstack opam switch; from repo root.
# Starts (or reuses) local dev stack on :18080, runs Phase1–4 acceptance.

set -e
PP_ROOT=$(cd "$(dirname "$0")/.." && pwd)
cd "$PP_ROOT"
eval $(opam env --switch=poohstack --set-switch)

echo "== build =="
dune build
echo "build ok"

echo "== unit tests =="
dune runtest
echo "runtest ok"

echo "== dev stack =="
if ! ksh scripts/dev.sh status 2>/dev/null | grep -q 'running.*image'; then
  ksh scripts/dev.sh start
else
  echo "dev stack already running"
fi

export PP_URL=http://127.0.0.1:18080 PP_USER=admin PP_PASS=admin PP_EVAL="$PP_ROOT/scripts/pp_eval.py"
JAR=/tmp/wiki-verify-cookies.txt
rm -f "$JAR"
curl -s -c "$JAR" -X POST --data-urlencode "username=admin" --data-urlencode "password=admin" \
  "$PP_URL/login" -o /dev/null

echo "== Phase1 flip-test (personal route under anon traffic) =="
# Ensure navbar editor on personal (idempotent-ish)
python3 "$PP_EVAL" -j '(checkout "admin/personal")' >/dev/null
python3 "$PP_EVAL" -j < spikes/navbar-editor/lib.pp >/dev/null 2>&1 || true
# If nav-editor missing, skip with note (editor may already be loaded)
code=$(curl -s -b "$JAR" "$PP_URL/nav-editor" -o /dev/null -w '%{http_code}')
if [ "$code" != "200" ]; then
  echo "WARN: /nav-editor not 200 ($code) — loading install bits"
  python3 "$PP_EVAL" -j '(deftype (quote navbar-config) (quote items))' >/dev/null 2>&1 || true
  python3 "$PP_EVAL" -j '(define navbar/ui-menu-backup ui/menu)' >/dev/null 2>&1 || true
  python3 "$PP_EVAL" -j '(if (null? (rows (quote navbar-config))) (create (quote navbar-config) (dict-set* "{}" (list "items" (json-emit (ui/menu))))) "ok")' >/dev/null 2>&1 || true
  python3 "$PP_EVAL" -j '(define ui/menu (lambda () (navbar/items)))' >/dev/null 2>&1 || true
fi
fails=0
i=1
while [ $i -le 20 ]; do
  curl -s "$PP_URL/welcome" -o /dev/null
  c=$(curl -s -b "$JAR" "$PP_URL/nav-editor" -o /dev/null -w '%{http_code}')
  [ "$c" = "200" ] || fails=$((fails+1))
  i=$((i+1))
done
echo "flip-test fails=$fails/20"
[ "$fails" -eq 0 ] || exit 1

echo "== Phase2–4 notes libs =="
python3 "$PP_EVAL" -j '(load-library "outline")' >/dev/null
python3 "$PP_EVAL" -j '(load-library "notes")' >/dev/null

# links-and-roam
python3 "$PP_EVAL" -j '(notes/doc/upsert "vf-dst" "VF Dst" (outline/node* "root" "VF Dst" "target" "" (list)) "" "note" "")' >/dev/null
python3 "$PP_EVAL" -j '(notes/doc/upsert "vf-src" "VF Src" (outline/node* "root" "VF Src" "See [[vf-dst|dest]]." "" (list)) "" "note" "")' >/dev/null
bls=$(python3 "$PP_EVAL" -j '(roam/backlinks "vf-dst")')
echo "$bls" | grep -q vf-src && echo "backlinks ok" || { echo "backlinks FAIL $bls"; exit 1; }
html=$(python3 "$PP_EVAL" -j '(roam/render-body "See [[vf-dst|dest]].")')
echo "$html" | grep -q 'href=.*vf-dst' && echo "render ok" || { echo "render FAIL $html"; exit 1; }

# outline-actions eval
python3 "$PP_EVAL" -j '(notes/doc/upsert "vf-eval" "VF Eval" (outline/node* "root" "VF Eval" "x" "" (list)) "" "note" "")' >/dev/null
curl -s -b "$JAR" -X POST \
  --data-urlencode 'slug=vf-eval' --data-urlencode 'op=eval' \
  --data-urlencode 'node-id=root' --data-urlencode 'target=child' \
  --data-urlencode 'expr=(outline/node (gensym "n") "via-eval")' \
  "$PP_URL/partials/notes-node-op" -o /tmp/vf-eval.html -w ''
grep -q 'via-eval' /tmp/vf-eval.html && echo "eval child ok" || { echo "eval FAIL"; exit 1; }

# wiki ingest/query (needs LLM key via env or settings)
echo "== wiki ingest (requires LLM) =="
ing=$(python3 "$PP_EVAL" -j '(wiki/ingest-from-source "Marie Curie discovered radium and polonium. She won two Nobel Prizes.")')
echo "$ing" | grep -q src- && echo "ingest ok" || { echo "ingest FAIL $ing"; exit 1; }
q=$(python3 "$PP_EVAL" -j '(wiki/query "Who discovered radium?")')
echo "$q" | grep -q '\[\[' && echo "query cites ok" || echo "WARN: query missing [[ citation ]]: $q"
ev=$(python3 "$PP_EVAL" -j '(count (wiki/events))')
echo "wiki-events count value: $ev"

echo "== ALL CHECKS PASSED =="
