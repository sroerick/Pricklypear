#!/bin/ksh
# verify_book_inline.sh — every borge/*.borg chapter is inlined into the
# canonical root book (pricklypear.borg) exactly once.
#
# Why: every projection built on the root walk (borge book print/export,
# scripts/load-book.sh, the /book faces, book.pdf) silently omits
# chapters the root never inlines — and per borge/habitat-workflow an
# out-of-book chapter is functionally not spec. Found via
# i-1788670834-f: borge/music.borg shipped + promoted but was never
# inlined; borge lint's orphan check missed it because a parse error in
# the file made the walker skip it silently. This check is a plain text
# scan of inline directives, so it catches unreferenced chapters even
# when they fail to parse.
#
# Needs: nothing beyond POSIX tools (no borge, no services).
# Run from anywhere; cd's to the repo root.
set -e
cd "$(dirname "$0")/.."

fail=0

[ -f pricklypear.borg ] || { echo "FAIL: pricklypear.borg not found"; exit 1; }

# The canonical walk is root-flat today: only pricklypear.borg carries
# (inline ...) directives. Course roots (course/*.borg, ../ paths) are
# excluded from the canonical book walk by the borge composition-root
# rule. If a chapter ever gains its own (inline ...) directive the walk
# is no longer flat — fail loudly so the exactly-once counting below
# stays honest.
nested=$(grep -l '(inline' borge/*.borg 2>/dev/null || true)
if [ -n "$nested" ]; then
  echo "FAIL: nested (inline ...) directives inside borge/ — the canonical walk is no longer root-flat; extend this check to follow the inline tree:"
  echo "$nested"
  exit 1
fi

targets=$(grep -o '([[:space:]]*inline[[:space:]]*[^)]*)' pricklypear.borg \
  | sed -e 's/^([[:space:]]*inline[[:space:]]*//' -e 's/[[:space:]]*)$//' -e 's#^\./##' \
  | sort)

# Every directive must name a file that exists.
for t in $(printf '%s\n' "$targets" | sort -u); do
  [ -f "$t" ] || { echo "FAIL: inline target '$t' does not exist"; fail=1; }
done

# Every chapter exactly once.
for f in borge/*.borg; do
  n=$(printf '%s\n' "$targets" | grep -cx -- "$f" || true)
  case "$n" in
    1) ;;
    0) echo "FAIL: $f is never inlined into the root book (out-of-book chapter is functionally not spec)"; fail=1 ;;
    *) echo "FAIL: $f is inlined $n times (expected exactly once)"; fail=1 ;;
  esac
done

if [ "$fail" -eq 0 ]; then
  echo "ok: all $(printf '%s\n' "$targets" | grep -c . || true) borge/*.borg chapters are inlined into pricklypear.borg exactly once"
fi
exit $fail
