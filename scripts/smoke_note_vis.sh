#!/bin/ksh
# Note-visibility smoke (note-vis-p1): the kernel vis floor + the
# cross-garden route. Owner seeds three fixture notes (private / users /
# public); a second signed-in identity and an anonymous visitor probe
# them; cleanup deletes the fixtures.
#
#   export PP_URL=http://127.0.0.1:18080 PP_USER=admin PP_PASS=admin
#   export PP_USER2=john PP_PASS2=...   # second signed-in identity
#   ./scripts/smoke_note_vis.sh
# Second identity optional: those checks are skipped when unset.

set -eu
cd "$(dirname "$0")/.."
PP_URL="${PP_URL:-http://127.0.0.1:18080}"
PP_USER="${PP_USER:-admin}"
PP_PASS="${PP_PASS:-admin}"
EVAL="${PP_EVAL:-scripts/pp-eval.sh}"
S=note-vis-fixture

ev() { PP_URL="$PP_URL" PP_USER="$1" PP_PASS="$2" $EVAL "$3"; }
fail() { echo "FAIL $*" >&2; exit 1; }
ok() { echo "OK  $*"; }

# truthy: raw pp-eval prints pp_value booleans as true/false
truthy() { [ "$1" = true ]; }

echo "== seed (owner $PP_USER) =="
ev "$PP_USER" "$PP_PASS" '(load-library "notes")' >/dev/null
ev "$PP_USER" "$PP_PASS" "(notes/doc/upsert \"$S-p\" \"Vis Private\" (outline/node \"root\" \"Vis Private\") \"\" \"note\" \"\")" >/dev/null
ev "$PP_USER" "$PP_PASS" "(notes/doc/upsert \"$S-u\" \"Vis Users\" (outline/node \"root\" \"Vis Users\") \"\" \"note\" \"\")" >/dev/null
ev "$PP_USER" "$PP_PASS" "(notes/doc/upsert \"$S-g\" \"Vis Public\" (outline/node \"root\" \"Vis Public\") \"\" \"note\" \"\")" >/dev/null
ev "$PP_USER" "$PP_PASS" "(notes/doc/set-vis \"$S-u\" \"users\")" >/dev/null
ev "$PP_USER" "$PP_PASS" "(notes/doc/set-vis \"$S-g\" \"public\")" >/dev/null
ok "fixtures seeded: $S-p private, $S-u users, $S-g public"

echo "== upsert stamps + preserves vis =="
truthy "$(ev "$PP_USER" "$PP_PASS" \
  "(string-eq (notes/doc/row-vis (notes/doc/row \"$S-p\")) \"private\")")" \
  || fail "create did not stamp private"
ev "$PP_USER" "$PP_PASS" \
  "(notes/doc/upsert \"$S-u\" \"Vis Users\" (outline/node \"root\" \"Vis Users updated\") \"\" \"note\" \"\")" >/dev/null
truthy "$(ev "$PP_USER" "$PP_PASS" \
  "(string-eq (notes/doc/row-vis (notes/doc/row \"$S-u\")) \"users\")")" \
  || fail "update did not preserve vis=users"
ok "stamps private on create, preserves on update"

echo "== owner sees all three in own garden =="
for x in p u g; do
  truthy "$(ev "$PP_USER" "$PP_PASS" \
    "(if (notes/doc/cross-row \"$PP_USER\" \"$S-$x\") #t #f)")" \
    || fail "owner cannot see own $S-$x"
done
ok "owner matrix"

if [ -n "${PP_USER2:-}" ]; then
  echo "== second user $PP_USER2 =="
  truthy "$(ev "$PP_USER2" "${PP_PASS2:-}" \
    "(if (notes/doc/cross-row \"$PP_USER\" \"$S-p\") #t #f)")" \
    && fail "private note leaked to $PP_USER2"
  truthy "$(ev "$PP_USER2" "${PP_PASS2:-}" \
    "(if (notes/doc/cross-row \"$PP_USER\" \"$S-u\") #t #f)")" \
    || fail "users note not readable by $PP_USER2"
  truthy "$(ev "$PP_USER2" "${PP_PASS2:-}" \
    "(if (notes/doc/cross-row \"$PP_USER\" \"$S-g\") #t #f)")" \
    || fail "public note not readable by $PP_USER2"
  ev "$PP_USER2" "${PP_PASS2:-}" \
    "(tree/get (string-append (tree/garden-root \"$PP_USER\") \".garden.$S-p\"))" \
    >/tmp/pp-vis-denial.out 2>&1 \
    && fail "tree/get into owner garden was NOT denied for $PP_USER2"
  grep -q 'denied' /tmp/pp-vis-denial.out || fail "tree/get failure not a vis denial: $(cat /tmp/pp-vis-denial.out)"
  ok "users/public readable, private nil + tree/get denied"
fi

echo "== anonymous HTTP =="
code=$(curl -sS -o /tmp/pp-vis-anon.html -w '%{http_code}' "$PP_URL/notes/$PP_USER/$S-g")
[ "$code" = "200" ] || fail "public cross-garden HTTP $code"
grep -q 'Vis Public' /tmp/pp-vis-anon.html || fail "public page missing title"
grep -q "garden" /tmp/pp-vis-anon.html || fail "public page missing owner breadcrumb"
for x in p u; do
  code=$(curl -sS -o /tmp/pp-vis-anon2.html -w '%{http_code}' "$PP_URL/notes/$PP_USER/$S-$x")
  [ "$code" = "200" ] || fail "$S-$x cross-garden HTTP $code"
  grep -q 'Missing' /tmp/pp-vis-anon2.html || fail "$S-$x rendered for anonymous (leak)"
done
code=$(curl -sS -o /dev/null -w '%{http_code}' "$PP_URL/notes/$PP_USER")
[ "$code" = "400" ] || fail "bad path shape should 400, got $code"
ok "public renders; users/private missing; bad shape 400"

echo "== cleanup (owner) =="
for x in p u g; do
  ev "$PP_USER" "$PP_PASS" "(notes/delete \"$S-$x\")" >/dev/null
done
ok "fixtures deleted"
echo "ALL OK"
