#!/bin/ksh
# Launch OCaml pp-sync (no Python).
set -e
SCRIPT_DIR="$(CDPATH= cd -- "$(dirname "$0")" && pwd)"
ROOT="$(CDPATH= cd -- "$SCRIPT_DIR/.." && pwd)"
BUILD="$ROOT/_build/default/cli/bin/pp_sync.exe"

if command -v pp-sync >/dev/null 2>&1; then
  exec pp-sync "$@"
fi
if [ -x "$BUILD" ]; then
  exec "$BUILD" "$@"
fi
if command -v dune >/dev/null 2>&1; then
  cd "$ROOT" || exit 1
  exec dune exec -- cli/bin/pp_sync.exe "$@"
fi
echo "pp-sync: build with: dune build cli/bin/pp_sync.exe" >&2
exit 2
