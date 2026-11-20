#!/bin/ksh
# filmstrip pane acceptance vectors.
#
# Welcome pane (card 2c440c53):
#   Criterion 2: with GARDEN_DIR pointing at a fixture directory whose
#   welcome.md holds known text, the rendered pane body contains that text
#   verbatim.
#   Criterion 3: with GARDEN_DIR unset/empty, the pane body contains
#   "Welcome to Pricklypear".
# The pane body text is read at RENDER time from $GARDEN_DIR/welcome.md by
# the (garden-welcome) prim (image/lib/garden_note.ml); libs/filmstrip/lib.pp
# filmstrip/welcome-pane-html is the first pane of filmstrip/main-strip-html
# and renders that prim's answer. This script asserts both legs: the prim's
# answer (via the probe binary, which calls the same function the prim
# registers) and the source-level wiring in lib.pp -- so the pane cannot
# drift back to a hardcoded note.
#
# Public non-interactive board pane (card a6ca7850):
#   one tile per board card carrying title + state only, grouped by state,
#   notes redacted, and NOTHING interactive -- no form element, no href into
#   /admin, no POST target. The board probe prints the same
#   Public_board.render_body the (public-board-tiles) prim serves, so these
#   vectors assert the real projection rather than a test-local mirror.
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

chk_lacks() { # chk_lacks NAME NEEDLE HAYSTACK
  name=$1; needle=$2; hay=$3
  case "$hay" in
    *"$needle"*) fail=$((fail+1)); echo "FAIL $name: found [$needle]" ;;
    *) pass=$((pass+1)); echo "PASS $name" ;;
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

# ---- public non-interactive board pane (card a6ca7850) ----
BPROBE=${BPROBE:-$ROOT/_build/default/image/test/public_board_probe.exe}
if [ ! -x "$BPROBE" ] && [ -x "$ROOT/image/test/public_board_probe.exe" ]; then
  BPROBE="$ROOT/image/test/public_board_probe.exe"
fi
if [ ! -x "$BPROBE" ]; then
  ( cd "$ROOT" && dune build image/test/public_board_probe.exe ) || {
    echo "FAIL board probe: dune build image/test/public_board_probe.exe"; exit 1; }
  BPROBE="$ROOT/_build/default/image/test/public_board_probe.exe"
fi

# Three seeded cards in three states, one carrying internal notes text.
BODY=$("$BPROBE")

# Criterion 2: all three titles and all three states appear.
chk_has board_title_todo "fix the leak" "$BODY"
chk_has board_title_code "ship the pane" "$BODY"
chk_has board_title_done "review the diff" "$BODY"
chk_has board_state_todo ">TODO<" "$BODY"
chk_has board_state_code ">CODE<" "$BODY"
chk_has board_state_done ">DONE<" "$BODY"
# AC1: grouped by state -- one state block per state.
chk_has board_grouped 'kb-board-state">TODO<' "$BODY"
chk_has board_grouped_code 'kb-board-state">CODE<' "$BODY"
chk_has board_grouped_done 'kb-board-state">DONE<' "$BODY"

# Criterion 2: a seeded card's notes text never reaches the body.
chk_lacks board_notes_absent "internal: roerick line" "$BODY"
chk_lacks board_notes_absent_all "do not publish" "$BODY"

# Criterion 3: no interactive surface rides the pane.
chk_lacks board_no_form "<form" "$BODY"
chk_lacks board_no_admin_href 'href="/admin' "$BODY"
chk_lacks board_no_any_href "href=" "$BODY"
chk_lacks board_no_post_target "POST " "$BODY"
chk_lacks board_no_hx "hx-post" "$BODY"

# The pane is wired to the projection prim (no card markup pinned in lib.pp).
chk_has board_reads_prim "(public-board-tiles (filmstrip/board-rows))" \
  "$(tr -d '\n' < "$LIB" | sed 's/  */ /g')"
chk_has board_pane_public "filmstrip/board-pane-html" "$(cat "$LIB")"

# Banana button: the page carries the button and its exact alert text.
# Under `dune runtest` the sandboxed tree may hold page.ml at either the
# source-relative path or the sandbox root (deps are copied in).
PAGE="$ROOT/libs/filmstrip/page.ml"
[ -f "$PAGE" ] || PAGE="$ROOT/page.ml"
chk_has banana_button "alert('Yes! we have no bananas')" "$(cat "$PAGE")"
chk_has banana_on_page "banana () ^ strip ()" "$(cat "$PAGE")"

echo "----"
echo "pass=$pass fail=$fail"
[ "$fail" = 0 ]
