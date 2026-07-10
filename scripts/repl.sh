#!/bin/ksh
# scripts/repl.sh — interactive login to the Pricklypear image REPL.
#
# Connects to the supervisor SSH front door (line protocol on PP_SSH_PORT),
# authenticates with an SSH public key, and drops you into the live Lisp REPL.
# No netcat required — uses python3 (scripts/repl_client.py).
#
# Usage:
#   scripts/repl.sh
#   scripts/repl.sh --key "ssh-ed25519 AAAA... comment"
#   scripts/repl.sh --key-file ~/.ssh/id_ed25519.pub
#   PP_SSH_KEY_FILE=~/.ssh/id_ed25519.pub scripts/repl.sh
#
# Env:
#   PP_SSH_HOST          default 127.0.0.1
#   PP_SSH_PORT          default 17822
#   PP_SSH_KEY           full OpenSSH public key line
#   PP_SSH_KEY_FILE      path to .pub file
#   PP_IMAGE_HTTP_PORT   default 18080 (printed as UI hint)
#
# Stack must be running first: scripts/dev.sh start

set -e

SCRIPT_DIR="$(CDPATH= cd -- "$(dirname "$0")" && pwd)"
HOST="${PP_SSH_HOST:-127.0.0.1}"
PORT="${PP_SSH_PORT:-17822}"
HTTP_PORT="${PP_IMAGE_HTTP_PORT:-18080}"
KEY="${PP_SSH_KEY:-ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIVtestkeyforpricklypearadmin}"
KEY_FILE="${PP_SSH_KEY_FILE:-}"

usage() {
  cat <<EOF
Usage: scripts/repl.sh [options]

  --host HOST          supervisor host (default: $HOST)
  --port PORT          SSH front-door port (default: $PORT)
  --key "KEY LINE"     OpenSSH public key line
  --key-file PATH      read public key from file
  -h, --help           show this help

Examples:
  scripts/dev.sh start
  scripts/repl.sh
  scripts/repl.sh --key-file ~/.ssh/id_ed25519.pub
EOF
}

while [ $# -gt 0 ]; do
  case "$1" in
    --host)
      HOST="$2"; shift 2 ;;
    --port)
      PORT="$2"; shift 2 ;;
    --key)
      KEY="$2"; shift 2 ;;
    --key-file)
      KEY_FILE="$2"; shift 2 ;;
    -h|--help)
      usage; exit 0 ;;
    *)
      echo "repl: unknown arg: $1" >&2
      usage >&2
      exit 2
      ;;
  esac
done

if [ -n "$KEY_FILE" ]; then
  if [ ! -f "$KEY_FILE" ]; then
    echo "repl: key file not found: $KEY_FILE" >&2
    exit 1
  fi
  KEY="$(sed -n '1p' "$KEY_FILE")"
fi

if [ -z "$KEY" ]; then
  echo "repl: empty SSH public key" >&2
  exit 1
fi

if ! command -v python3 >/dev/null 2>&1; then
  echo "repl: python3 is required" >&2
  exit 1
fi

exec python3 "$SCRIPT_DIR/repl_client.py" \
  --host "$HOST" \
  --port "$PORT" \
  --http-port "$HTTP_PORT" \
  --key "$KEY"
