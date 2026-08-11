#!/bin/ksh
## lint_prim_error_returns.sh — flag VStr error-return sites in image/lib prim source.
##
## Per design note error-shape-design §5: new prims that return VStr error
## strings are design violations. This scan flags VStr returns whose content
## matches an error-prefix keyword (need|bad|fail|denied|cannot|invalid|unknown|
## no such|not found) in image/lib/*.ml. It exits non-zero if any are found.
##
## Usage:
##   scripts/lint_prim_error_returns.sh             # scan, exit 1 on hits
##   scripts/lint_prim_error_returns.sh --fixture   # plant a fixture, verify detection
##
## Wire into promote/forge gate is a follow-up; this is the scan only.
## Exempt: doc comments (lines starting with (* or containing -> VStr ...).

set -u

LIB_DIR="${1:-image/lib}"
FIXTURE=0
[ "${1:-}" = "--fixture" ] && { FIXTURE=1; LIB_DIR="image/lib"; }

# Pattern: VStr "..." where content contains an error keyword.
# Skip: comment lines (start with *, (, or contain -> VStr as doc).
PATTERN='VStr "[^"]*(need |bad |fail|denied|cannot|invalid|unknown|no such|not found)'

echo "scanning $LIB_DIR for VStr error-return sites..."

if [ "$FIXTURE" = "1" ]; then
  # Plant a fixture file and verify detection
  fixture="$LIB_DIR/lint_fixture_test.ml"
  cat > "$fixture" <<'EOF'
(* fixture for lint_prim_error_returns.sh — should be flagged *)
let _ = VStr "get: need type, id"
EOF
  echo "planted fixture: $fixture"
fi

hits=0
for f in "$LIB_DIR"/*.ml; do
  [ -f "$f" ] || continue
  # grep -n gives line numbers; skip comment lines (start with * or () and
  # doc lines (contain -> VStr as a contract description).
  matches=$(grep -nE -- "$PATTERN" "$f" 2>/dev/null \
    | grep -vE '^[0-9]+:[[:space:]]*\(\*' \
    | grep -vE -- '-> VStr' || true)
  if [ -n "$matches" ]; then
    echo "--- $f ---"
    echo "$matches"
    count=$(echo "$matches" | wc -l | tr -d ' ')
    hits=$((hits + count))
  fi
done

if [ "$FIXTURE" = "1" ]; then
  rm -f "$LIB_DIR/lint_fixture_test.ml"
fi

echo ""
if [ "$hits" -gt 0 ]; then
  echo "FAIL: $hits VStr error-return site(s) found."
  echo "Per error-shape-design §5, prims should RAISE Interp_error with a kind,"
  echo "not return VStr error strings. Convert these to raises."
  exit 1
fi

echo "OK: no VStr error-return sites found."
exit 0
