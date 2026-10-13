#!/bin/ksh
# Launch OCaml pp-eval (no Python).
#
#   export PP_URL=… PP_USER=… PP_PASS=…
#   scripts/pp-eval.sh -j '(whoami)'
#
# Resolution order:
#   1. $PP_EVAL if it is an executable binary (not *.py)
#   2. pp-eval on PATH
#   3. dune _build/default/cli/bin/pp_eval.exe from repo root
#   4. dune exec (builds if needed)

set -e
SCRIPT_DIR="$(CDPATH= cd -- "$(dirname "$0")" && pwd)"
ROOT="$(CDPATH= cd -- "$SCRIPT_DIR/.." && pwd)"
BUILD="$ROOT/_build/default/cli/bin/pp_eval.exe"

is_python_script() {
  case "$1" in
    *.py) return 0 ;;
  esac
  return 1
}

if [ -n "${PP_EVAL:-}" ] && [ -x "$PP_EVAL" ] && ! is_python_script "$PP_EVAL"; then
  exec "$PP_EVAL" "$@"
fi

if command -v pp-eval >/dev/null 2>&1; then
  exec pp-eval "$@"
fi

if [ -x "$BUILD" ]; then
  exec "$BUILD" "$@"
fi

if command -v dune >/dev/null 2>&1; then
  cd "$ROOT" || exit 1
  # shellcheck disable=SC2086
  if [ -n "${OPAM_SWITCH_PREFIX:-}" ] || command -v opam >/dev/null 2>&1; then
    :
  fi
  exec dune exec -- cli/bin/pp_eval.exe "$@"
fi

echo "pp-eval: no binary found. Build with:" >&2
echo "  eval \$(opam env --switch=poohstack --set-switch)" >&2
echo "  dune build cli/bin/pp_eval.exe" >&2
echo "  export PP_EVAL=$BUILD" >&2
exit 2
