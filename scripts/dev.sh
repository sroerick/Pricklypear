#!/bin/ksh
# scripts/dev.sh — start the full Pricklypear dev stack.
#
# Brings up: temp Postgres (if not already running), authd, supervisor.
# The supervisor will spawn image unikernels on SSH connect.
#
# Usage:
#   scripts/dev.sh start    # boot everything
#   scripts/dev.sh stop     # kill everything (leaves PG running)
#   scripts/dev.sh restart  # stop + start
#   scripts/dev.sh status   # show what's running
#   scripts/dev.sh clean    # stop + DROP the pricklypear DB
#
# All PIDs/logs go to /tmp/pp-dev/. Configure via env vars below.
#
# Per AGENTS.md: scripts use ksh (no bash-isms). All paths assume
# the script is run from the pricklypear repo root.

set -e

# --- Config (override via env) ---
PP_ROOT="${PP_ROOT:-$(cd "$(dirname "$0")/.." && pwd)}"
PP_DATA_DIR="${PP_DATA_DIR:-/tmp/pp-pgsup}"
PP_SOCKET_DIR="${PP_SOCKET_DIR:-/tmp}"
PP_DB_PORT="${PP_DB_PORT:-5433}"
PP_DB_NAME="${PP_DB_NAME:-pricklypear}"
PP_DB_USER="${PP_DB_USER:-postgres}"
PP_AUTH_PORT="${PP_AUTH_PORT:-17879}"
PP_SSH_PORT="${PP_SSH_PORT:-17822}"
PP_ADMIN_PORT="${PP_ADMIN_PORT:-17880}"
PP_IMAGE_HTTP_PORT="${PP_IMAGE_HTTP_PORT:-18080}"
PP_IMAGE_REPL_PORT="${PP_IMAGE_REPL_PORT:-17878}"
PP_DEV_DIR="${PP_DEV_DIR:-/tmp/pp-dev}"

# Build artifacts (poohstack opam switch required)
OPAMSwitch="poohstack"

mkdir -p "$PP_DEV_DIR"

# --- Helpers ---

have_opam_env=0
opam_env() {
  if [ "$have_opam_env" -eq 0 ]; then
    eval "$(opam env --switch=$OPAMSwitch --set-switch)"
    have_opam_env=1
  fi
}

build_all() {
  opam_env
  echo "[dev] building all packages (poohstack)..."
  (cd "$PP_ROOT" && dune build @all)
}

pg_pidfile="$PP_DEV_DIR/pg.pid"
auth_pidfile="$PP_DEV_DIR/authd.pid"
sup_pidfile="$PP_DEV_DIR/supervisor.pid"

start_pg() {
  if [ -f "$pg_pidfile" ] && kill -0 "$(cat "$pg_pidfile")" 2>/dev/null; then
    echo "[dev] pg already running (pid $(cat "$pg_pidfile"))"
    return
  fi
  if [ -d "$PP_DATA_DIR" ]; then
    if pg_ctl -D "$PP_DATA_DIR" status >/dev/null 2>&1; then
      echo "[dev] pg already running (cluster_exists)"
      pg_ctl -D "$PP_DATA_DIR" status | head -1
      return
    fi
  else
    echo "[dev] initializing PG data dir at $PP_DATA_DIR"
    initdb -D "$PP_DATA_DIR" -U "$PP_DB_USER" --auth=trust >/dev/null
  fi
  echo "[dev] starting PG on port $PP_DB_PORT (socket $PP_SOCKET_DIR)"
  pg_ctl -D "$PP_DATA_DIR" \
    -o "-k $PP_SOCKET_DIR -p $PP_DB_PORT" \
    -l "$PP_DEV_DIR/pg.log" start
  sleep 1
  if ! psql -h "$PP_SOCKET_DIR" -p "$PP_DB_PORT" -U "$PP_DB_USER" \
       -d postgres -tAc "SELECT 1" >/dev/null 2>&1; then
    echo "[dev] FATAL: PG didn't come up; check $PP_DEV_DIR/pg.log" >&2
    exit 1
  fi
  if ! psql -h "$PP_SOCKET_DIR" -p "$PP_DB_PORT" -U "$PP_DB_USER" \
       -d postgres -lqt 2>/dev/null | cut -d'|' -f1 | grep -qw "$PP_DB_NAME"; then
    echo "[dev] creating database $PP_DB_NAME"
    createdb -h "$PP_SOCKET_DIR" -p "$PP_DB_PORT" -U "$PP_DB_USER" "$PP_DB_NAME"
  fi
  echo "[dev] applying migrations 001 + 002"
  psql -h "$PP_SOCKET_DIR" -p "$PP_DB_PORT" -U "$PP_DB_USER" -d "$PP_DB_NAME" \
    -f "$PP_ROOT/migrations/001_branches.sql" -q
  psql -h "$PP_SOCKET_DIR" -p "$PP_DB_PORT" -U "$PP_DB_USER" -d "$PP_DB_NAME" \
    -f "$PP_ROOT/migrations/002_auth.sql" -q
  psql -h "$PP_SOCKET_DIR" -p "$PP_DB_PORT" -U "$PP_DB_USER" -d "$PP_DB_NAME" \
    -f "$PP_ROOT/migrations/003_data.sql" -q
  # Seed system/main branch if not present (001 should do this; idempotent)
  psql -h "$PP_SOCKET_DIR" -p "$PP_DB_PORT" -U "$PP_DB_USER" -d "$PP_DB_NAME" -q <<EOF
INSERT INTO branches (id, parent_id, owner, name)
  SELECT '00000000-0000-0000-0000-000000000001', NULL, 'system', 'main'
  WHERE NOT EXISTS (
    SELECT 1 FROM branches WHERE owner='system' AND name='main'
  );
EOF
  echo "[dev] pg ready (db=$PP_DB_NAME on :$PP_DB_PORT)"
}

start_authd() {
  if [ -f "$auth_pidfile" ] && kill -0 "$(cat "$auth_pidfile")" 2>/dev/null; then
    echo "[dev] authd already running (pid $(cat "$auth_pidfile"))"
    return
  fi
  echo "[dev] starting authd on port $PP_AUTH_PORT"
  PP_DB_HOST=127.0.0.1 \
  PP_DB_PORT="$PP_DB_PORT" \
  PP_DB_NAME="$PP_DB_NAME" \
  PP_DB_USER=pricklypear_auth \
  PP_DB_PASS=change-me-on-prod \
  PP_AUTH_PORT="$PP_AUTH_PORT" \
  PP_BOOTSTRAP_ADMIN_KEY="${PP_BOOTSTRAP_ADMIN_KEY:-ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIVtestkeyforpricklypearadmin}" \
    "$PP_ROOT/_build/default/auth/bin/authd.exe" \
      >"$PP_DEV_DIR/authd.log" 2>&1 &
  echo $! > "$auth_pidfile"
  sleep 1
  if ! kill -0 "$(cat "$auth_pidfile")" 2>/dev/null; then
    echo "[dev] FATAL: authd failed to start; check $PP_DEV_DIR/authd.log" >&2
    exit 1
  fi
  echo "[dev] authd ready (pid $(cat "$auth_pidfile"))"
}

start_supervisor() {
  if [ -f "$sup_pidfile" ] && kill -0 "$(cat "$sup_pidfile")" 2>/dev/null; then
    echo "[dev] supervisor already running (pid $(cat "$sup_pidfile"))"
    return
  fi
  echo "[dev] starting supervisor: ssh=:$PP_SSH_PORT admin=:$PP_ADMIN_PORT"
  PP_DB_HOST=127.0.0.1 \
  PP_DB_PORT="$PP_DB_PORT" \
  PP_DB_NAME="$PP_DB_NAME" \
  PP_DB_USER="$PP_DB_USER" \
  PP_ADMIN_PORT="$PP_ADMIN_PORT" \
  PP_SSH_PORT="$PP_SSH_PORT" \
  PP_AUTH_HOST=127.0.0.1 \
  PP_AUTH_PORT="$PP_AUTH_PORT" \
  PP_IMAGE_BIN="$PP_ROOT/_build/default/image/bin/unikernel.exe" \
    "$PP_ROOT/_build/default/supervisor/bin/supervisor.exe" \
      >"$PP_DEV_DIR/supervisor.log" 2>&1 &
  echo $! > "$sup_pidfile"
  sleep 1
  if ! kill -0 "$(cat "$sup_pidfile")" 2>/dev/null; then
    echo "[dev] FATAL: supervisor failed; check $PP_DEV_DIR/supervisor.log" >&2
    exit 1
  fi
  echo "[dev] supervisor ready (pid $(cat "$sup_pidfile"))"
}

stop_pidfile() {
  pf="$1"; name="$2"
  if [ -f "$pf" ]; then
    pid="$(cat "$pf")"
    if kill -0 "$pid" 2>/dev/null; then
      echo "[dev] stopping $name (pid $pid)"
      kill "$pid" 2>/dev/null || true
      sleep 0.5
      kill -9 "$pid" 2>/dev/null || true
    fi
    rm -f "$pf"
  fi
}

do_start() {
  build_all
  start_pg
  start_authd
  start_supervisor
  echo
  echo "[dev] all services up. Endpoints:"
  echo "  authd RPC:    http://127.0.0.1:$PP_AUTH_PORT/health"
  echo "  supervisor:   http://127.0.0.1:$PP_ADMIN_PORT/  (admin)"
  echo "  SSH:          ssh-like line proto on 127.0.0.1:$PP_SSH_PORT"
  echo "  PostgreSQL:   -h /tmp -p $PP_DB_PORT -U $PP_DB_USER -d $PP_DB_NAME"
  echo "  logs:         $PP_DEV_DIR/{authd,supervisor,pg}.log"
}

do_stop() {
  stop_pidfile "$sup_pidfile" "supervisor"
  stop_pidfile "$auth_pidfile" "authd"
  # also kill any orphaned image unikernels spawned by the supervisor
  pkill -9 -f "_build/default/image/bin/unikernel.exe" 2>/dev/null || true
  echo "[dev] services stopped (PG left running — use 'clean' to drop DB)"
}

do_status() {
  for pf in "$auth_pidfile" "$sup_pidfile"; do
    if [ -f "$pf" ] && kill -0 "$(cat "$pf")" 2>/dev/null; then
      echo "  running  pid $(cat "$pf")  ($(basename "$pf" .pid))"
    else
      echo "  stopped  ($(basename "$pf" .pid))"
    fi
  done
  if pg_ctl -D "$PP_DATA_DIR" status >/dev/null 2>&1; then
    echo "  running  pg on :$PP_DB_PORT"
  else
    echo "  stopped  pg"
  fi
}

do_clean() {
  do_stop
  if psql -h "$PP_SOCKET_DIR" -p "$PP_DB_PORT" -U "$PP_DB_USER" \
       -d postgres -tAc "SELECT 1" >/dev/null 2>&1; then
    echo "[dev] dropping database $PP_DB_NAME"
    psql -h "$PP_SOCKET_DIR" -p "$PP_DB_PORT" -U "$PP_DB_USER" \
      -d postgres -c "DROP DATABASE IF EXISTS $PP_DB_NAME"
  fi
}

case "${1:-}" in
  start) do_start ;;
  stop) do_stop ;;
  restart) do_stop; do_start ;;
  status) do_status ;;
  clean) do_clean ;;
  *)
    echo "Usage: $0 {start|stop|restart|status|clean}" >&2
    exit 2
    ;;
esac
