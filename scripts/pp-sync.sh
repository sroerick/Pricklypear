#!/bin/ksh
# Sync libs/*.pp with live Pricklypear language state.
#   set -a; . ~/.config/pricklypear/env; set +a
#   scripts/pp-sync.sh status ui
#   scripts/pp-sync.sh pull ui
#   scripts/pp-sync.sh push todo

set -e
SCRIPT_DIR="$(CDPATH= cd -- "$(dirname "$0")" && pwd)"
exec python3 "$SCRIPT_DIR/pp_sync.py" "$@"
