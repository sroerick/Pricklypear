#!/bin/sh
# glochid-served.sh — under systemd, --open is IMPROVEMENTAL but often
# WRONG for headless-first login (no DISPLAY yet, wrong $DISPLAY later).
# Left unset unless explicitly wanted: GLOCHID_SERVED_OPEN=1.
set -eu

PORT="${GLOCHID_PORT:-18090}"

case "${GLOCHID_SERVED_OPEN:-0}" in
  1) OPEN_FLAG="--open" ;;
  *) OPEN_FLAG="" ;;
esac

exec /home/roerick/dev/wyo.tech/pricklypear/_build/default/agent/bin/main.exe \
  --serve --port "$PORT" $OPEN_FLAG
