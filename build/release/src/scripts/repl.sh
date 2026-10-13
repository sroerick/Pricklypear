#!/bin/sh
# scripts/repl.sh — line-protocol REPL client (borge/product-experience.borg
# product-experience + access-paths smokes).
#
# Streams stdin, one expression per line, to the image's OPTIONAL line-protocol
# REPL over TCP. PP_REPL_PORT is a dev-stack env knob (17878 under
# scripts/dev.sh); when unset the image does not listen, and the primary
# interactive surfaces are the browser /repl face and POST /api/eval
# (scripts/pp-eval.sh / scripts/pp_eval.py).
#
# Usage:  scripts/repl.sh [host:port] < exprs.txt
#         echo '(+ 1 2)' | scripts/repl.sh 127.0.0.1:17878

TARGET="${1:-127.0.0.1:${PP_REPL_PORT:-17878}}"
HOST="${TARGET%%:*}"
PORT="${TARGET##*:}"

if [ "$HOST" = "$PORT" ]; then
	echo "repl.sh: expected host:port (got '$TARGET')" >&2
	exit 2
fi

if ! command -v nc >/dev/null 2>&1; then
	echo "repl.sh: nc not found (OpenBSD netcat or equivalent required)" >&2
	exit 3
fi

exec nc "$HOST" "$PORT"
