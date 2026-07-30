#!/bin/ksh
# Monitor-rerunnable smoke for notes-as-tree (K0–R0 + M0 helpers).
# Requires: running image (scripts/dev.sh start) and built pp_eval.
#
#   eval $(opam env --switch=poohstack --set-switch)
#   export PP_URL=http://127.0.0.1:18080 PP_USER=admin PP_PASS=admin
#   ksh scripts/verify_notes_tree.sh

set -e
ROOT="$(CDPATH= cd -- "$(dirname "$0")/.." && pwd)"
cd "$ROOT"

PP_EVAL="${PP_EVAL:-$ROOT/_build/default/cli/bin/pp_eval.exe}"
PP_URL="${PP_URL:-http://127.0.0.1:18080}"
export PP_URL PP_USER="${PP_USER:-admin}" PP_PASS="${PP_PASS:-admin}" PP_EVAL

if [ ! -x "$PP_EVAL" ]; then
  eval $(opam env --switch=poohstack --set-switch)
  dune build cli/bin/pp_eval.exe image
  PP_EVAL="$ROOT/_build/default/cli/bin/pp_eval.exe"
fi

echo "[verify_notes_tree] health $PP_URL"
curl -sf -o /dev/null "$PP_URL/health"

echo "[verify_notes_tree] unit tests"
eval $(opam env --switch=poohstack --set-switch)
dune exec image/test/tree_store_test.exe
dune exec image/test/primitives_test.exe >/tmp/pp-prims-test.out
tail -3 /tmp/pp-prims-test.out

echo "[verify_notes_tree] tree prims + notes façade"
"$PP_EVAL" -e '(load-library "std")' -e '(load-library "outline")' -e '(load-library "roam")' -e '(load-library "notes")' -e '(load-library "bible")' -e '
(seq
  (notes/tree/ensure-folder (notes/tree/root) (whoami))
  (notes/doc/upsert-in "verify-tree" "Verify Tree"
    (outline/node* "root" "Verify Tree" "" ""
      (list (outline/node* "c1" "H1" "see (bible John 3 16) and [[inbox]]" "" (list))))
    "verify" "note" "" "verify/folder")
  (let ((n (notes/doc/get "verify-tree")))
    (if (null? n) "MISSING"
      (list
        (car (cdr n))
        (notes/doc/folder n)
        (count (outline/kids (notes/doc/outline n)))
        (notes/doc/row-path (notes/doc/row "verify-tree"))))))
' | tee /tmp/pp-notes-tree-verify.out
grep -q verify-tree /tmp/pp-notes-tree-verify.out || {
  echo "FAIL: verify-tree note missing" >&2
  exit 1
}

echo "[verify_notes_tree] render-links bible"
"$PP_EVAL" -j '(render-links "x (bible Gen 1 1) y")' | tee /tmp/pp-bible-render.out
echo "$("$PP_EVAL" -j '(render-links "x (bible Gen 1 1) y")')" | grep -q dabar || {
  echo "FAIL: bible render missing dabar" >&2
  exit 1
}

echo "[verify_notes_tree] legacy migrate helper present"
"$PP_EVAL" -e '(load-library "notes")' -e '(notes/migrate/legacy-all)' | tee /tmp/pp-m0.out

echo "[verify_notes_tree] OK"
