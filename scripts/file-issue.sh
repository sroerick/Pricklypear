#!/bin/bash
# file-issue.sh — quick-submit an issue to the habitat `log/issues` register.
#
# Opens /issues in a browser for the dedicated face, or append from the
# terminal/REPL with this one-liner. The issue lands as a node on the
# `log/issues` notes doc (kind "issue") — the live source of truth, mirrored
# into log/issues/*.md + FIX-LOOP.md by the fold step (see habitat-workflow).
#
# Usage:
#   scripts/file-issue.sh "<KIND>: <title>"          # KIND inferred from prefix
#   scripts/file-issue.sh BUG "RLS floor inactive" "set_local_pp_user only in tx"
#   scripts/file-issue.sh DESIGN "fields as VStr"    # body optional
#
# KIND: BUG | DESIGN | SMELL | CLEANUP (defaults to BUG).
# Requires the `notes` library loaded on the habitat (it seeds at boot).
#
# Env (from ~/.config/pricklypear/env or exported): PP_URL PP_USER PP_PASS PP_EVAL.
set -euo pipefail

SCRIPT_DIR="$(CDPATH= cd -- "$(dirname "$0")" && pwd)"
ROOT="$(CDPATH= cd -- "$SCRIPT_DIR/.." && pwd)"

case "${1:-}" in
  -h|--help)
    sed -n '2,18p' "$0"; exit 0 ;;
esac

# Determine kind / title / body from args.
if [ "$#" -ge 2 ] && [[ "$1" == @(BUG|DESIGN|SMELL|CLEANUP) ]]; then
  kind="$1"; title="$2"; body="${3:-}"
elif [ "$#" -ge 1 ]; then
  # Infer kind from a "KIND: title" prefix; default BUG.
  case "$1" in
    BUG:*|DESIGN:*|SMELL:*|CLEANUP:*) kind="${1%%:*}"; title="${1#*: }" ;;
    *) kind="BUG"; title="$1" ;;
  esac
  body="${2:-}"
else
  echo "usage: $0 <KIND|\"KIND: title\"> <title> [body]" >&2; exit 64
fi

# Load habitat env if present (local-only, mode 600, never committed).
if [ -z "${PP_USER:-}" ] || [ -z "${PP_URL:-}" ]; then
  set -a; . "${HOME}/.config/pricklypear/env" 2>/dev/null || true; set +a
fi
: "${PP_URL:?PP_URL not set — source ~/.config/pricklypear/env or export it}"
: "${PP_USER:?PP_USER not set}"
: "${PP_PASS:?PP_PASS not set}"
export PP_EVAL="${PP_EVAL:-$ROOT/_build/default/cli/bin/pp_eval.exe}"
if [ ! -x "$PP_EVAL" ]; then PP_EVAL="$ROOT/scripts/pp-eval.sh"; fi

# Escape double-quotes/backslashes for a Lisp string literal.
esc () { printf '%s' "$1" | sed -e 's/\\/\\\\/g' -e 's/"/\\"/g'; }

expr="(seq (load-library \"notes\") (notes/issues/add \"$kind\" \"$(esc "$title")\" \"$(esc "$body")\"))"

if "$PP_EVAL" -j "$expr" >/tmp/pp-file-issue.out 2>&1; then
  if grep -q '"ok":true' /tmp/pp-file-issue.out; then
    echo "filed [$kind] $title → /issues"
  else
    echo "filed but habitat reported:" >&2; cat /tmp/pp-file-issue.out >&2; exit 1
  fi
else
  echo "failed to submit issue" >&2; cat /tmp/pp-file-issue.out >&2; exit 1
fi
