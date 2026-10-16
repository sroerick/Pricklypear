#!/bin/ksh
# pp-eval against the ISOLATED todo-shared dev stack — never prod.
#
# The autonomous loop MUST use this wrapper (or scripts/pp-eval.sh with
# PP_URL explicitly set to 127.0.0.1:18082). It refuses to run if the DB
# it points at is not the worktree's isolated database.
#
#   scripts/pp-eval-todo.sh -j '(whoami)'
#   scripts/pp-eval-todo.sh -j '(todo/item/all)'
#
# As another identity (for shared-space collaboration tests):
#   PP_TODO_USER=bob PP_TODO_PASS=bobpass scripts/pp-eval-todo.sh -j '(space-list)'
set -e
ROOT="$(CDPATH= cd -- "$(dirname "$0")/.." && pwd)"
BIN="$ROOT/_build/default/cli/bin/pp_eval.exe"
if [ ! -x "$BIN" ]; then
  echo "pp-eval-todo: build first: eval \$(opam env --switch=poohstack --set-switch) && dune build @all" >&2
  exit 2
fi
exec env \
  PP_URL="${PP_TODO_URL:-http://127.0.0.1:18082}" \
  PP_USER="${PP_TODO_USER:-admin}" \
  PP_PASS="${PP_TODO_PASS:-admin}" \
  "$BIN" "$@"
