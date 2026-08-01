#!/bin/ksh
# Notes UI smoke: HTTP node-ops + DOM checks + screenshots.
# Requires: local/remote image, PP_* or defaults admin/admin on :18080,
#           chromium for screenshots (optional if CHROME unset).
#
#   export PP_URL=http://127.0.0.1:18080 PP_USER=admin PP_PASS=admin
#   ./scripts/smoke_notes_ui.sh

set -eu
cd "$(dirname "$0")/.."
PP_URL="${PP_URL:-http://127.0.0.1:18080}"
PP_USER="${PP_USER:-admin}"
PP_PASS="${PP_PASS:-admin}"
EVAL="${PP_EVAL:-python3 scripts/pp_eval.py}"
COOKIE="${COOKIE_JAR:-/tmp/pp-notes-ui.cookie}"
OUT="${NOTES_UI_OUT:-/tmp/pp-notes-ui}"
SLUG=ui-fixture
CHROME="${CHROME:-/usr/bin/chromium}"

j() { $EVAL -j "$1"; }
# Extract JSON string/number value from pp_eval -j output
jval() {
  $EVAL -j "$1" | python3 -c 'import sys,json; d=json.load(sys.stdin); v=d.get("value","");
v=v if isinstance(v,str) else str(v);
# pp_value wraps strings in double quotes
if len(v)>=2 and v[0]=="\"" and v[-1]=="\"": v=v[1:-1]
print(v)'
}
fail() { echo "FAIL $*" >&2; exit 1; }
ok() { echo "OK  $*"; }

mkdir -p "$OUT"
rm -f "$COOKIE"

echo "== login =="
export PP_URL PP_USER PP_PASS
code=$(curl -sS -c "$COOKIE" -b "$COOKIE" -o /dev/null -w '%{http_code}' \
  -X POST "$PP_URL/login" \
  --data-urlencode "username=$PP_USER" \
  --data-urlencode "password=$PP_PASS")
[ "$code" = "303" ] || [ "$code" = "302" ] || [ "$code" = "200" ] || fail "login HTTP $code"
ok "login $code"

echo "== prep: reload libs on user branch + fixture =="
# HTTP uses username/personal; pin git libs so personal overrides do not hide fixes.
j '(load-library "outline")' >/dev/null
j '(load-library "notes")' >/dev/null
j "(notes/doc/upsert \"$SLUG\" \"UI Fixture\" (outline/node \"root\" \"UI Fixture\") \"\" \"note\" \"\")" >/dev/null
ok "fixture seeded"

echo "== N1 index =="
code=$(curl -sS -b "$COOKIE" -o "$OUT/index.html" -w '%{http_code}' "$PP_URL/notes")
[ "$code" = "200" ] || fail "index $code"
grep -q 'action="create"\|name="slug"\|New note\|notes' "$OUT/index.html" || fail "index missing create UI"
ok "index 200"

echo "== N2 open fixture =="
code=$(curl -sS -b "$COOKIE" -o "$OUT/show.html" -w '%{http_code}' \
  "$PP_URL/notes?slug=$SLUG")
[ "$code" = "200" ] || fail "show $code"
grep -q 'id="outline"' "$OUT/show.html" || fail "missing #outline"
grep -q 'data-id="root"' "$OUT/show.html" || fail "missing root data-id"
grep -q 'ol-toolbar' "$OUT/show.html" || fail "missing desktop toolbar"
grep -q 'ol-actions' "$OUT/show.html" || fail "missing mobile actions"
ok "show fixture + chrome"

post_op() {
  op="$1"
  nid="$2"
  curl -sS -b "$COOKIE" -o "$OUT/op-$op.html" -w '%{http_code}' \
    -X POST "$PP_URL/partials/notes-node-op?slug=$SLUG&op=$op&node-id=$nid"
}

echo "== N3 add-child (HTTP partial) =="
code=$(post_op add-child root)
[ "$code" = "200" ] || fail "add-child HTTP $code (body=$(head -c 80 "$OUT/op-add-child.html"))"
grep -q 'id="outline"' "$OUT/op-add-child.html" || fail "add-child response not outline"
kids=$(jval "(str (count (outline/kids (notes/doc/outline (notes/doc/get \"$SLUG\")))))")
[ "${kids:-0}" -ge 1 ] || fail "kids after add-child=$kids"
KID=$(jval "(outline/id (car (outline/kids (notes/doc/outline (notes/doc/get \"$SLUG\")))))")
[ -n "$KID" ] || fail "empty kid id"
ok "add-child -> kid=$KID kids=$kids"

echo "== N4 add-sibling =="
code=$(post_op add-sibling "$KID")
[ "$code" = "200" ] || fail "add-sibling HTTP $code"
kids=$(jval "(str (count (outline/kids (notes/doc/outline (notes/doc/get \"$SLUG\")))))")
[ "${kids:-0}" -ge 2 ] || fail "kids after sibling=$kids"
ok "add-sibling kids=$kids"

echo "== N5 cycle-todo =="
code=$(post_op cycle-todo "$KID")
[ "$code" = "200" ] || fail "cycle-todo HTTP $code"
todo=$(jval "(outline/todo (outline/find (notes/doc/outline (notes/doc/get \"$SLUG\")) \"$KID\"))")
ok "cycle-todo response 200 todo=$todo"

echo "== N8 delete =="
before=$kids
code=$(post_op delete "$KID")
[ "$code" = "200" ] || fail "delete HTTP $code"
after=$(jval "(str (count (outline/kids (notes/doc/outline (notes/doc/get \"$SLUG\")))))")
[ "${after:-0}" -lt "${before:-0}" ] || fail "delete did not shrink kids $before->$after"
ok "delete $before->$after"

echo "== N9 notes-search partial (search-modal) =="
code=$(curl -sS -b "$COOKIE" -o "$OUT/search-empty.html" -w '%{http_code}' \
  "$PP_URL/partials/notes-search?q=")
[ "$code" = "200" ] || fail "search empty HTTP $code"
grep -q 'ol-find-results' "$OUT/search-empty.html" || fail "search empty missing results container"
ok "search empty 200"

# Ensure fixture title is findable via Postgres tree search (not garden enum).
j "(notes/doc/upsert \"$SLUG\" \"UI Fixture\" (outline/node \"root\" \"UI Fixture\") \"\" \"note\" \"\")" >/dev/null
code=$(curl -sS -b "$COOKIE" -o "$OUT/search-hit.html" -w '%{http_code}' \
  "$PP_URL/partials/notes-search?q=Fixture")
[ "$code" = "200" ] || fail "search hit HTTP $code"
grep -q "slug=$SLUG" "$OUT/search-hit.html" || fail "search hit missing slug link"
grep -q 'ol-find-hit' "$OUT/search-hit.html" || fail "search hit missing hit row"
ok "search hit Fixture"

code=$(curl -sS -b "$COOKIE" -o "$OUT/search-none.html" -w '%{http_code}' \
  "$PP_URL/partials/notes-search?q=zzznomatch999")
[ "$code" = "200" ] || fail "search none HTTP $code"
grep -q 'No matches' "$OUT/search-none.html" || fail "search none missing empty state"
ok "search none"

grep -q 'ol-find-modal' "$OUT/show.html" || fail "show page missing find modal"
grep -q 'partials/notes-search' "$OUT/show.html" || fail "show page missing search hx-get"
ok "show page find chrome"

echo "== screenshots (optional) =="
if [ -x "$CHROME" ]; then
  # cookie for chromium: convert netscape jar is hard; use --dump-dom after
  # loading login via data URL is messy — screenshot public-ish via cookie file not native.
  # Use headless with stored HTML file as file:// is wrong for relative assets.
  # Screenshot live URL without auth will redirect; use dump of saved HTML instead.
  "$CHROME" --headless --disable-gpu --no-sandbox --window-size=1400,900 \
    --screenshot="$OUT/desktop-show.png" \
    "data:text/html;base64,$(base64 -w0 "$OUT/show.html" 2>/dev/null || base64 "$OUT/show.html")" \
    2>/dev/null || true
  if [ -f "$OUT/desktop-show.png" ]; then
    ok "screenshot $OUT/desktop-show.png"
  else
    ok "screenshot skipped/failed (non-fatal)"
  fi
else
  ok "no chromium; skip screenshots"
fi

echo "SMOKE_NOTES_UI OK"
echo "Artifacts: $OUT/"
