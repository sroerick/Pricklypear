#!/bin/ksh
# Isolated dev stack for the `todo-shared-spaces` worktree.
#
# WHY: this worktree is worked by an autonomous loop while another agent
# (issue triage) owns the main checkout's stack. Distinct data dir, ports,
# and log dir so the two never collide.
#
# Ports (main checkout uses 5433/18080/17878; tuna uses 5434):
#   PG    5435   (data dir /tmp/pp-pgsup-todo-shared)
#   HTTP  18082  (image)
#   REPL  17880  (image line protocol)
#
# Usage:  scripts/dev-todo.sh start|stop|restart|status|clean
set -e
ROOT="$(CDPATH= cd -- "$(dirname "$0")/.." && pwd)"
exec env \
  PP_DATA_DIR="${PP_TODO_DATA_DIR:-/tmp/pp-pgsup-todo-shared}" \
  PP_DB_PORT="${PP_TODO_DB_PORT:-5435}" \
  PP_DB_NAME="${PP_TODO_DB_NAME:-pricklypear_todo}" \
  PP_HTTP_PORT="${PP_TODO_HTTP_PORT:-18082}" \
  PP_REPL_PORT="${PP_TODO_REPL_PORT:-17880}" \
  PP_DEV_DIR="${PP_TODO_DEV_DIR:-/tmp/pp-dev-todo-shared}" \
  PP_ROOT="$ROOT" \
  "$ROOT/scripts/dev.sh" "$@"
