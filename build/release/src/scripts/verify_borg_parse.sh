#!/bin/ksh
# verify_borg_parse.sh — every spec file parses; no chapter walks as a
# raw drift hole.
#
# Why: the root book walk and borge lint both swallow per-file parse
# errors (the walker prints "inline tree error ... printing it as a
# lone chapter" and moves on), so one unbalanced comment turns a
# chapter into an unstructured hole in every root-walk projection
# (borge book print/export, scripts/load-book.sh, the /book faces,
# book.pdf). Found via i-1788680784: borge/formal-verification.borg
# line 4 opened a (* agent note ... comment that was never closed, so
# the chapter exported as a lone node while every `borge book` run
# stayed exit 0. This check is the complement of
# verify_book_inline.sh (which stays borge-free on purpose and catches
# never-inlined chapters even when they parse).
#
# Needs: borge on PATH. Run from anywhere; cd's to the repo root.
set -e
cd "$(dirname "$0")/.."

command -v borge >/dev/null 2>&1 || { echo "FAIL: borge not found on PATH"; exit 1; }

fail=0
files=$(ls pricklypear.borg borge/*.borg 2>/dev/null)
[ -n "$files" ] || { echo "FAIL: no spec files found (pricklypear.borg, borge/*.borg)"; exit 1; }

for f in $files; do
  out=$(borge parse "$f" 2>&1) || { echo "FAIL: $f does not parse:"; printf '%s\n' "$out"; fail=1; }
done

if [ "$fail" -eq 0 ]; then
  echo "ok: $(printf '%s\n' "$files" | grep -c .) spec files parse clean"
fi
exit $fail
