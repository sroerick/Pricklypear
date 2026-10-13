#!/bin/ksh
# scripts/measure-boot.sh — time image exec -> /health ok:true.
#
# Baseline harness for the promote-window work
# (.ralph/promote-optimization.md task 1). Launches the image exactly like
# scripts/dev.sh start_image does (env block below — keep in sync there),
# then polls /health every 100ms until ok:true and prints elapsed ms.
#
# This script does NOT build. Run `dune build` or `scripts/dev.sh start`
# first, and leave PG up (dev.sh leaves PG running on `stop`).
#
# Usage:
#   scripts/measure-boot.sh                    # 1 warm-DB boot measurement
#   PP_MEASURE_RUNS=3 scripts/measure-boot.sh  # 3 runs + summary
#   PP_MEASURE_CLEAN=1 scripts/measure-boot.sh # drop + recreate DB first
#                                              # (cold-boot measurement)
#
# No new deps: curl + base perl (Time::HiRes) + psql, all present on the
# dev laptop and OpenBSD base.

set -u

# --- Config (same defaults as scripts/dev.sh — keep in sync) ---
PP_ROOT="${PP_ROOT:-$(cd "$(dirname "$0")/.." && pwd)}"
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
PP_MEASURE_CLEAN="${PP_MEASURE_CLEAN:-0}"
PP_MEASURE_RUNS="${PP_MEASURE_RUNS:-1}"

image_pidfile="$PP_DEV_DIR/image.pid"
health="http://127.0.0.1:${PP_HTTP_PORT}/health"
poll_interval="${PP_MEASURE_POLL:-0.1}"   # seconds
cap_ms="${PP_MEASURE_CAP_MS:-120000}"     # give up after 120s

mkdir -p "$PP_DEV_DIR"

log() { print -ru2 -- "[measure] $*"; }
now_ms() { perl -MTime::HiRes=time -e 'printf "%d\n", time * 1000'; }

health_ok() {
  curl -fsS --max-time 2 "$health" 2>/dev/null \
    | grep -q '"ok"[[:space:]]*:[[:space:]]*true'
}

pg_up() {
  psql -h "$PP_SOCKET_DIR" -p "$PP_DB_PORT" -U "$PP_DB_USER" \
    -d postgres -tAc "SELECT 1" >/dev/null 2>&1
}

drop_db() {
  psql -h "$PP_SOCKET_DIR" -p "$PP_DB_PORT" -U "$PP_DB_USER" \
    -d postgres -c "DROP DATABASE IF EXISTS $PP_DB_NAME" >/dev/null 2>&1 || true
}

# Recreate empty schema (same as dev.sh apply_migrations) so a CLEAN run
# measures real first-boot work (bootstrap + seed), not a migration error.
create_db() {
  createdb -h "$PP_SOCKET_DIR" -p "$PP_DB_PORT" -U "$PP_DB_USER" \
    "$PP_DB_NAME" >/dev/null 2>&1 || true
  for m in "$PP_ROOT/migrations"/[0-9][0-9][0-9]_*.sql; do
    psql -h "$PP_SOCKET_DIR" -p "$PP_DB_PORT" -U "$PP_DB_USER" \
      -d "$PP_DB_NAME" -f "$m" -q || { log "migration failed: $m"; return 1; }
  done
  psql -h "$PP_SOCKET_DIR" -p "$PP_DB_PORT" -U "$PP_DB_USER" -d "$PP_DB_NAME" -q <<EOF
INSERT INTO branches (id, parent_id, owner, name)
  SELECT '00000000-0000-0000-0000-000000000001', NULL, 'system', 'main'
  WHERE NOT EXISTS (SELECT 1 FROM branches WHERE owner='system' AND name='main');
EOF
}

stop_image() {
  if [ -f "$image_pidfile" ]; then
    pid="$(cat "$image_pidfile" 2>/dev/null || true)"
    if [ -n "$pid" ]; then
      kill "$pid" 2>/dev/null || true
      sleep 0.3
      kill -9 "$pid" 2>/dev/null || true
    fi
    rm -f "$image_pidfile"
  fi
}

start_and_time() {
  stop_image
  if [ "$PP_MEASURE_CLEAN" = "1" ]; then
    log "clean mode: dropping + recreating $PP_DB_NAME"
    drop_db
    create_db || return 1
  fi

  # Optional LLM env, same as dev.sh start_image.
  if [ -f "${HOME}/.config/pricklypear/llm.env" ]; then
    # shellcheck disable=SC1090
    set -a
    . "${HOME}/.config/pricklypear/llm.env"
    set +a
  fi

  t0=$(now_ms)
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

  ok=0
  while :; do
    if health_ok; then ok=1; break; fi
    if ! kill -0 "$(cat "$image_pidfile")" 2>/dev/null; then
      log "image died before health-ok; last log lines:"
      tail -5 "$PP_DEV_DIR/image.log" >&2
      break
    fi
    now=$(now_ms)
    if [ $((now - t0)) -gt "$cap_ms" ]; then
      log "timeout: no ok:true within ${cap_ms}ms"
      break
    fi
    sleep "$poll_interval"
  done
  t1=$(now_ms)
  ms=$((t1 - t0))

  if [ "$ok" = "1" ]; then
    print -- "boot_ok_ms=$ms"
  else
    print -- "boot_FAILED_ms=$ms"
    return 1
  fi
}

# --- Precondition: PG up ---
if ! pg_up; then
  log "PG not reachable on :$PP_DB_PORT — run 'scripts/dev.sh start' once first"
  exit 1
fi

runs=0
total=0
fail=0
i=0
while [ "$i" -lt "$PP_MEASURE_RUNS" ]; do
  i=$((i + 1))
  line="$(start_and_time)" || { fail=$((fail + 1)); runs=$((runs + 1)); continue; }
  runs=$((runs + 1))
  ms="${line##*=}"
  total=$((total + ms))
  log "run $i/$PP_MEASURE_RUNS: $line"
done

if [ "$PP_MEASURE_RUNS" -gt 1 ] && [ "$fail" -eq 0 ] && [ "$total" -gt 0 ]; then
  log "summary: runs=$runs avg_ms=$((total / runs)) min/max not tracked (use individual lines)"
fi
[ "$fail" -eq 0 ] || exit 1
exit 0
