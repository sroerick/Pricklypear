#!/bin/ksh
## lint_prim_error_returns.sh — flag VStr error-return sites in image/lib prim source.
##
## Per design note error-shape-design §5: new prims that return VStr error
## strings are design violations. This scan flags VStr returns whose content
## matches an error-prefix keyword (need|bad|fail|denied|cannot|invalid|unknown|
## no such|not found) OR whose literal is a bare "<prim>: " prefix-concat
## (e.g. ("llm-chat: " ^ e)). It exits non-zero if any are found.
##
## Usage:
##   scripts/lint_prim_error_returns.sh             # scan, exit 1 on hits
##   scripts/lint_prim_error_returns.sh --fixture   # plant a fixture, verify detection
##
## Exempt: doc comments, and lines carrying an inline `lint-exempt-prim-error`
## marker (use it when a caller contract depends on the returned string —
## cite the tracking issue in the marker comment).
## Wire into promote/forge gate is a follow-up; this is the scan only.

set -u

LIB_DIR="${1:-image/lib}"
FIXTURE=0
[ "${1:-}" = "--fixture" ] && { FIXTURE=1; LIB_DIR="image/lib"; }

echo "scanning $LIB_DIR for VStr error-return sites..."

if [ "$FIXTURE" = "1" ]; then
  # Plant a fixture file and verify detection
  fixture="$LIB_DIR/lint_fixture_test.ml"
  printf 'let f x = match x with _ -> (Ast.VStr "f: need args")\n' > "$fixture"
  if ./scripts/lint_prim_error_returns.sh >/dev/null 2>&1; then
    echo "FIXTURE FAIL: detector missed the planted violation" >&2
    rm -f "$fixture"
    exit 1
  fi
  rm -f "$fixture"
  echo "FIXTURE OK: detector caught the planted violation"
  exit 0
fi

hits=$(perl -0777 -ne '
  my $n = 0;
  while (/VStr\s*(?:\(\s*)?"((?:[^"\\]|\\.)*)"/g) {
    my $lit = $1;
    my $pre = substr($_, 0, $+[0]);
    my $line = ($pre =~ tr/\n//) + 1;
    if ($lit =~ /(need |bad |fail|denied|cannot|invalid|unknown|no such|not found)/
        || $lit =~ /^[a-z][a-z0-9_\/-]*: $/) {
      my @lines = split /\n/;
      my $window = join "\n", grep { defined }
        @lines[$line - 3 .. $line + 1];
      next if $window =~ /lint-exempt-prim-error/;
      printf "--- %s (line %d): VStr \"%s\"\n", $ARGV, $line,
             (length $lit > 60 ? substr($lit, 0, 60) . "…" : $lit);
      $n++;
    }
  }
  ' "$LIB_DIR"/*.ml)

if [ -n "$hits" ]; then
  echo "$hits"
  echo "FAIL: VStr error-return site(s) found."
  echo "Per error-shape-design §5, prims should RAISE Interp_error with a kind,"
  echo "not return VStr error strings. Convert these to raises."
  exit 1
fi
echo "OK: no VStr error-return sites."
exit 0
