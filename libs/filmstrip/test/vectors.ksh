#!/bin/ksh
# filmstrip welcome-pane acceptance vectors (card 2c440c53).
#
# Criterion 2: with GARDEN_DIR pointing at a fixture directory whose
# welcome.md holds known text, the rendered pane body contains that text
# verbatim.
# Criterion 3: with GARDEN_DIR unset/empty, the pane body contains
# "Welcome to Pricklypear".
#
# The pane body text is read at RENDER time from $GARDEN_DIR/welcome.md by
# the (garden-welcome) prim (image/lib/garden_note.ml); libs/filmstrip/lib.pp
# filmstrip/welcome-pane-html is the first pane of filmstrip/main-strip-html
# and renders that prim's answer. This script asserts both legs: the prim's
# answer (via the probe binary, which calls the same function the prim
# registers) and the source-level wiring in lib.pp -- so the pane cannot
# drift back to a hardcoded note.
set -u
ROOT=$(cd "$(dirname "$0")/../../.." && pwd)
PROBE=${PROBE:-$ROOT/_build/default/test/garden_note_probe.exe}
pass=0; fail=0

chk() { # chk NAME WANT GOT
  name=$1; want=$2; got=$3
  if [ "$got" = "$want" ]; then
    pass=$((pass+1)); echo "PASS $name"
  else
    fail=$((fail+1)); echo "FAIL $name: want [$want] got [$got]"
  fi
}

chk_has() { # chk_has NAME NEEDLE HAYSTACK
  name=$1; needle=$2; hay=$3
  case "$hay" in
    *"$needle"*) pass=$((pass+1)); echo "PASS $name" ;;
    *) fail=$((fail+1)); echo "FAIL $name: missing [$needle]" ;;
  esac
}

# Under `dune runtest` the script runs inside the sandbox: the probe dep lands
# at $ROOT/test/garden_note_probe.exe instead of the source tree's _build path.
if [ ! -x "$PROBE" ] && [ -x "$ROOT/test/garden_note_probe.exe" ]; then
  PROBE="$ROOT/test/garden_note_probe.exe"
fi

if [ ! -x "$PROBE" ]; then
  ( cd "$ROOT" && dune build test/garden_note_probe.exe ) || {
    echo "FAIL probe: dune build test/garden_note_probe.exe"; exit 1; }
fi

FIX=$(mktemp -d)
trap 'rm -rf "$FIX"' EXIT
NOTE='Hello from the garden: roerick edits this file.'
printf '%s\n' "$NOTE" > "$FIX/welcome.md"

# Criterion 2: known fixture text renders verbatim.
got=$(GARDEN_DIR=$FIX "$PROBE")
chk fixture_text_verbatim "$NOTE" "$got"

# Criterion 3: unset GARDEN_DIR -> the one permitted fallback.
got=$(env -u GARDEN_DIR "$PROBE")
chk unset_fallback "Welcome to Pricklypear" "$got"

# Criterion 3: empty GARDEN_DIR -> same fallback.
got=$(GARDEN_DIR= "$PROBE")
chk empty_fallback "Welcome to Pricklypear" "$got"

# The pane is the FIRST pane of the main strip and reads the prim (no
# hardcoded welcome text in lib.pp outside the guard fallback).
LIB="$ROOT/libs/filmstrip/lib.pp"
chk_has pane_reads_prim "(garden-welcome)" "$(cat "$LIB")"
chk_has pane_is_first "$(printf '(list (filmstrip/welcome-pane-html))')" \
  "$(tr -d '\n' < "$LIB" | sed 's/  */ /g')"

# Banana button: the page carries the button and its exact alert text.
chk_has banana_button "alert('Yes! we have no bananas')" \
  "$(cat "$ROOT/libs/filmstrip/page.ml")"
chk_has banana_on_page "banana () ^ strip ()" \
  "$(cat "$ROOT/libs/filmstrip/page.ml")"

echo "----"
echo "pass=$pass fail=$fail"
[ "$fail" = 0 ]
