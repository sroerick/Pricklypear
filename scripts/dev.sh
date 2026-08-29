#!/bin/ksh
# scripts/dev.sh — start the Pricklypear dev stack.
#
# Brings up: temp Postgres (if not already running) + the image binary.
# One process. No supervisor, no authd (retired by the single-image pivot).
#
# Note: agent eval (pp-eval) targets PP_URL and normally bypasses this
# stack — this is the kernel-lab for OCaml work, not the default eval path.
#
# Usage:
#   scripts/dev.sh start    # boot PG + image
#   scripts/dev.sh stop     # kill the image (leaves PG running)
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
PP_HTTP_PORT="${PP_HTTP_PORT:-18080}"
PP_REPL_PORT="${PP_REPL_PORT:-17878}"
PP_LIBS_DIR="${PP_LIBS_DIR:-$PP_ROOT/libs}"
PP_SOURCE_ROOT="${PP_SOURCE_ROOT:-$PP_ROOT}"
PP_PUBLIC_DIR="${PP_PUBLIC_DIR:-$PP_ROOT/public-artifacts}"
PP_STATIC_DIR="${PP_STATIC_DIR:-$PP_ROOT/image/static}"
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
image_pidfile="$PP_DEV_DIR/image.pid"

apply_migrations() {
  # Idempotent SQL files — safe when PG already running (new 006 etc.).
  if ! psql -h "$PP_SOCKET_DIR" -p "$PP_DB_PORT" -U "$PP_DB_USER" \
       -d postgres -lqt 2>/dev/null | cut -d'|' -f1 | grep -qw "$PP_DB_NAME"; then
    echo "[dev] creating database $PP_DB_NAME"
    createdb -h "$PP_SOCKET_DIR" -p "$PP_DB_PORT" -U "$PP_DB_USER" "$PP_DB_NAME"
  fi
  echo "[dev] applying migrations"
  for m in "$PP_ROOT/migrations"/[0-9][0-9][0-9]_*.sql; do
    psql -h "$PP_SOCKET_DIR" -p "$PP_DB_PORT" -U "$PP_DB_USER" -d "$PP_DB_NAME" \
      -f "$m" -q
  done
  # Seed system/main branch if not present (001 should do this; idempotent)
  psql -h "$PP_SOCKET_DIR" -p "$PP_DB_PORT" -U "$PP_DB_USER" -d "$PP_DB_NAME" -q <<EOF
INSERT INTO branches (id, parent_id, owner, name)
  SELECT '00000000-0000-0000-0000-000000000001', NULL, 'system', 'main'
  WHERE NOT EXISTS (
    SELECT 1 FROM branches WHERE owner='system' AND name='main'
  );
EOF
}

start_pg() {
  if [ -f "$pg_pidfile" ] && kill -0 "$(cat "$pg_pidfile")" 2>/dev/null; then
    echo "[dev] pg already running (pid $(cat "$pg_pidfile"))"
    apply_migrations
    return
  fi
  if [ -d "$PP_DATA_DIR" ]; then
    if pg_ctl -D "$PP_DATA_DIR" status >/dev/null 2>&1; then
      echo "[dev] pg already running (cluster_exists)"
      pg_ctl -D "$PP_DATA_DIR" status | head -1
      apply_migrations
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
  echo $! > "$pg_pidfile"
  sleep 1
  if ! psql -h "$PP_SOCKET_DIR" -p "$PP_DB_PORT" -U "$PP_DB_USER" \
       -d postgres -tAc "SELECT 1" >/dev/null 2>&1; then
    echo "[dev] FATAL: PG didn't come up; check $PP_DEV_DIR/pg.log" >&2
    exit 1
  fi
  apply_migrations
  echo "[dev] pg ready (db=$PP_DB_NAME on :$PP_DB_PORT)"
}

start_image() {
  if [ -f "$image_pidfile" ] && kill -0 "$(cat "$image_pidfile")" 2>/dev/null; then
    echo "[dev] image already running (pid $(cat "$image_pidfile"))"
    return
  fi
  echo "[dev] starting image: http=:$PP_HTTP_PORT repl=:$PP_REPL_PORT"
  # Forward LLM_* if present (optional; agent-say needs LLM_API_KEY).
  # Also load ~/.config/pricklypear/llm.env when it exists (mode 600, never commit).
  if [ -f "${HOME}/.config/pricklypear/llm.env" ]; then
    # shellcheck disable=SC1090
    set -a
    . "${HOME}/.config/pricklypear/llm.env"
    set +a
  fi
  PP_DB_HOST=127.0.0.1 \
  PP_DB_PORT="$PP_DB_PORT" \
  PP_DB_NAME="$PP_DB_NAME" \
  PP_DB_USER="$PP_DB_USER" \
  PP_LIBS_DIR="$PP_LIBS_DIR" \
  PP_SOURCE_ROOT="$PP_SOURCE_ROOT" \
  PP_PUBLIC_DIR="$PP_PUBLIC_DIR" \
  PP_STATIC_DIR="$PP_STATIC_DIR" \
  PP_ROOT="$PP_ROOT" \
  PP_USERNAME="${PP_USERNAME:-admin}" \
  PP_BRANCH_NAME="${PP_BRANCH_NAME:-main}" \
  PP_BRANCH_ID="${PP_BRANCH_ID:-00000000-0000-0000-0000-000000000001}" \
  PP_BOOTSTRAP_USER="${PP_BOOTSTRAP_USER:-admin}" \
  PP_BOOTSTRAP_PASSWORD="${PP_BOOTSTRAP_PASSWORD:-admin}" \
  PP_ALLOW_INSECURE_BOOTSTRAP="${PP_ALLOW_INSECURE_BOOTSTRAP:-1}" \
  PP_AUTH_DISABLED="${PP_AUTH_DISABLED:-}" \
  PP_PUBLIC_ALLOW_WRITE="${PP_PUBLIC_ALLOW_WRITE:-1}" \
  PP_EVAL_CONTROL="${PP_EVAL_CONTROL:-eff}" \
  PP_IO="${PP_IO:-eio}" \
  PP_HTTP_PORT="$PP_HTTP_PORT" \
  PP_REPL_PORT="$PP_REPL_PORT" \
  LLM_API_BASE="${LLM_API_BASE:-}" \
  LLM_API_KEY="${LLM_API_KEY:-}" \
  LLM_MODEL="${LLM_MODEL:-}" \
    "$PP_ROOT/_build/default/image/bin/main.exe" \
      >"$PP_DEV_DIR/image.log" 2>&1 &
  echo $! > "$image_pidfile"
  sleep 1
  if ! kill -0 "$(cat "$image_pidfile")" 2>/dev/null; then
    echo "[dev] FATAL: image failed to start; check $PP_DEV_DIR/image.log" >&2
    exit 1
  fi
  echo "[dev] image ready (pid $(cat "$image_pidfile"))"
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
  start_image
  echo
  echo "[dev] all services up. Endpoints:"
  echo "  image HTTP:   http://127.0.0.1:$PP_HTTP_PORT/  (htmx)"
  echo "  image REPL:    127.0.0.1:$PP_REPL_PORT  (line protocol)"
  echo "  PostgreSQL:   -h /tmp -p $PP_DB_PORT -U $PP_DB_USER -d $PP_DB_NAME"
  echo "  logs:         $PP_DEV_DIR/{image,pg}.log"
}

do_stop() {
  stop_pidfile "$image_pidfile" "image"
  echo "[dev] services stopped (PG left running — use 'clean' to drop DB)"
}

do_status() {
  if [ -f "$image_pidfile" ] && kill -0 "$(cat "$image_pidfile")" 2>/dev/null; then
    echo "  running  pid $(cat "$image_pidfile")  (image)"
  else
    echo "  stopped  (image)"
  fi
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
