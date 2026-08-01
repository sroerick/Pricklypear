#!/bin/ksh
# smoke_dabar_interlinear.sh — rerunnable monitor for dabar interlinear WP3.
#
# Verifies:
#   1) seeder --help / dry-run John 1
#   2) (optional) local PG row count when PP_SMOKE_DB=1
#   3) live image: dabar/inter + HTTP plain/inter toggle + Strong's link
#
# Usage (local dev):
#   scripts/dev.sh start   # wait until /health 200
#   export PP_URL=http://127.0.0.1:18080 PP_USER=admin PP_PASS=admin
#   ksh scripts/smoke_dabar_interlinear.sh
#
# Usage (prod):
#   set -a; . ~/.config/pricklypear/env; set +a
#   ksh scripts/smoke_dabar_interlinear.sh
#
# Env:
#   PP_URL PP_USER PP_PASS   — image eval/HTTP (required for live checks)
#   PP_EVAL                  — optional path to pp_eval.exe
#   PP_SMOKE_DB=1            — also check local PG counts (dev defaults)
#   PP_DB_HOST PP_DB_PORT PP_DB_USER PP_DB_NAME PP_DB_PASS / PGPASSWORD

set -e
ROOT=$(CDPATH= cd -- "$(dirname "$0")/.." && pwd)
cd "$ROOT"

fail=0
pass() { printf 'OK  %s\n' "$1"; }
bad()  { printf 'FAIL %s\n' "$1"; fail=1; }

# --- 1. seeder CLI ---
if python3 scripts/seed_interlinear.py --help >/dev/null 2>&1; then
  pass "seed_interlinear.py --help"
else
  bad "seed_interlinear.py --help"
fi

dry=$(python3 scripts/seed_interlinear.py --book John --limit 1 --dry-run 2>&1 || true)
case "$dry" in
  *'BSB:John:1:1'*|*'G1722'*) pass "dry-run John 1:1 has tokens" ;;
  *) bad "dry-run John 1:1"; printf '%s\n' "$dry" | head -5 ;;
esac

# --- 2. optional local DB ---
if [ "${PP_SMOKE_DB:-0}" = "1" ]; then
  host=${PP_DB_HOST:-/tmp}
  port=${PP_DB_PORT:-5433}
  user=${PP_DB_USER:-postgres}
  db=${PP_DB_NAME:-pricklypear}
  export PGPASSWORD="${PGPASSWORD:-${PP_DB_PASS:-}}"
  n=$(psql -h "$host" -p "$port" -U "$user" -d "$db" -t -A \
    -c "SELECT count(*) FROM data_rows WHERE type_name='bible-interlinear'" 2>/dev/null || echo 0)
  if [ "$n" -ge 878 ] 2>/dev/null; then
    pass "bible-interlinear rows=$n (>=878)"
  else
    bad "bible-interlinear rows=$n (want >=878; run seed_interlinear.py)"
  fi
fi

# --- 3. live image ---
if [ -z "${PP_URL:-}" ]; then
  bad "PP_URL unset — skip live checks"
  exit 1
fi

if [ -n "${PP_EVAL:-}" ] && [ -x "$PP_EVAL" ]; then
  EVAL="$PP_EVAL"
elif [ -x "$ROOT/_build/default/cli/bin/pp_eval.exe" ]; then
  EVAL="$ROOT/_build/default/cli/bin/pp_eval.exe"
else
  EVAL="$ROOT/scripts/pp-eval.sh"
fi

# Ensure live handlers match the vault. Prefer load-string of local libs/
# (prod deploy tree can lag until git push). Do NOT bare load-library — that
# reloads stale /usr/local/libexec/.../libs on the habitat and undoes push.
if [ -f libs/dabar/lib.pp ]; then
  # pp-sync push dabar/strongs when available
  if [ -x "$ROOT/_build/default/cli/bin/pp_sync.exe" ]; then
    "$ROOT/_build/default/cli/bin/pp_sync.exe" push dabar >/dev/null 2>&1 || true
    "$ROOT/_build/default/cli/bin/pp_sync.exe" push strongs >/dev/null 2>&1 || true
  fi
fi
"$EVAL" -j '(route "/dabar" "dabar/page")' >/dev/null 2>&1 || true
"$EVAL" -j '(route "/strongs" "strongs/page")' >/dev/null 2>&1 || true

v=$("$EVAL" -j '(dabar/inter "John" 1 1)' 2>&1 || true)
case "$v" in
  *G1722*|*dabar-token*|*dabar-orig*) pass "(dabar/inter John 1 1)" ;;
  *) bad "(dabar/inter John 1 1)"; printf '%s\n' "$v" | head -3 ;;
esac

v=$("$EVAL" -j '(dabar/inter "Gen" 1 1)' 2>&1 || true)
case "$v" in
  *H7225*|*H430*) pass "(dabar/inter Gen 1 1)" ;;
  *) bad "(dabar/inter Gen 1 1)"; printf '%s\n' "$v" | head -3 ;;
esac

auth=()
if [ -n "${PP_USER:-}" ] && [ -n "${PP_PASS:-}" ]; then
  auth=(-u "${PP_USER}:${PP_PASS}")
fi

body=$(curl -sS "${auth[@]}" "${PP_URL}/dabar?book=John&chapter=3&verse=16&inter=0" || true)
case "$body" in
  *'Plain view'*) pass "HTTP plain toggle (inter=0)" ;;
  *) bad "HTTP plain toggle (inter=0)" ;;
esac
case "$body" in
  *dabar-token*) bad "HTTP plain should not embed tokens" ;;
  *) pass "HTTP plain has no dabar-token" ;;
esac

body=$(curl -sS "${auth[@]}" "${PP_URL}/dabar?book=John&chapter=3&verse=16&inter=1" || true)
case "$body" in
  *dabar-token*G2316*|*G2316*dabar-token*|*G2316*) pass "HTTP inter=1 tokens+G2316" ;;
  *)
    # accept either order
    echo "$body" | grep -q 'dabar-token' && echo "$body" | grep -q 'G2316' \
      && pass "HTTP inter=1 tokens+G2316" \
      || bad "HTTP inter=1 tokens+G2316"
    ;;
esac

body=$(curl -sS "${auth[@]}" "${PP_URL}/strongs?n=G2316" || true)
case "$body" in
  *not\ found*) bad "Strong's G2316" ;;
  *G2316*) pass "Strong's G2316" ;;
  *) bad "Strong's G2316" ;;
esac

hx=$(curl -sS "${auth[@]}" -H 'HX-Request: true' \
  "${PP_URL}/dabar?book=John&chapter=1&verse=1&inter=1" || true)
case "$hx" in
  *'<html'*) bad "HX fragment should not be full page" ;;
  *G1722*) pass "HX fragment John 1:1" ;;
  *) bad "HX fragment John 1:1" ;;
esac

if [ "$fail" -eq 0 ]; then
  printf 'SMOKE_DABAR_INTERLINEAR OK\n'
  exit 0
fi
printf 'SMOKE_DABAR_INTERLINEAR FAIL\n'
exit 1
