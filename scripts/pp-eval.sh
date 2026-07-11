#!/bin/ksh
# Thin wrapper around scripts/pp_eval.py for agents and humans.
#
#   export PP_URL=http://155.138.208.45
#   export PP_USER=pricklypear
#   export PP_PASS=...
#   scripts/pp-eval.sh '(+ 1 2)'
#   scripts/pp-eval.sh -i
#   scripts/pp-eval.sh -j '(list-bindings)'

set -e
SCRIPT_DIR="$(CDPATH= cd -- "$(dirname "$0")" && pwd)"
if ! command -v python3 >/dev/null 2>&1; then
  echo "pp-eval: python3 required" >&2
  exit 1
fi
exec python3 "$SCRIPT_DIR/pp_eval.py" "$@"
