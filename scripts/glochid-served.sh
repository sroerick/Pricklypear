#!/bin/sh
# glochid-served.sh — under systemd, --open is IMPROVEMENTAL but often
# WRONG for headless-first login (no DISPLAY yet, wrong $DISPLAY later).
# Left unset unless explicitly wanted: GLOCHID_SERVED_OPEN=1.
set -eu

PORT="${GLOCHID_PORT:-18090}"

# GLOCHID_BIN overrides the exec target (default: the repo _build, the
# development truth). A stable install at ~/.local/share/glochid/bin
# lets the local service run a pinned, budget-verified build while the
# working tree is mid-make (see habitat issue i-1789591788-8d507ea1).
BIN="${GLOCHID_BIN:-/home/roerick/dev/wyo.tech/pricklypear/_build/default/agent/bin/main.exe}"

case "${GLOCHID_SERVED_OPEN:-0}" in
  1) OPEN_FLAG="--open" ;;
  *) OPEN_FLAG="" ;;
esac

exec "$BIN" \
  --serve --port "$PORT" $OPEN_FLAG
