#!/bin/ksh
# Final verification for notes-inbox-issues Ralph loop (local + optional live).
# Rerun from repo root with poohstack opam switch.
set -e
ROOT=$(CDPATH= cd -- "$(dirname "$0")/.." && pwd)
cd "$ROOT"
eval $(opam env --switch=poohstack --set-switch)

echo "== lisp unit tests (incl. multi-arg seq) =="
dune build lisp/
dune runtest lisp/

echo "== static presence checks =="
grep -q 'seq.*_head :: bodies\|"_head :: bodies"' lisp/lib/eval.ml \
  || grep -q '"seq", _head :: bodies' lisp/lib/eval.ml
grep -q 'outline/archive' libs/outline/lib.pp
grep -q 'notes/refile' libs/notes/lib.pp
grep -q 'notes/hidden-from-index?' libs/notes/lib.pp
grep -q 'capture-drop' libs/ui/lib.pp
grep -q 'hx-swap" "outerHTML"' libs/outline/lib.pp

echo "OK: local verification passed"
