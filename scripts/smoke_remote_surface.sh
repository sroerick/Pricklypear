#!/bin/ksh
# pp-remote HTML surface smoke — the shared agent loop running on the
# CLIENT host (borge/remote-eval.borg agent-loop): chat page, multipart
# /chat stream over a mock provider, eval box at /eval, lens browser at
# /code. Mirrors smoke_agent_surface.sh; no image required (the mock
# provider makes no tool calls, so nothing crosses the wire).
#
# Boots `_build/default/remote/bin/main.exe --serve --mock-script`, then:
#   1. GET /            => chat page has the hx-post /chat form + assets
#   2. POST /chat       => multipart/mixed, chunked TE, all parts present,
#                          close boundary, curl EXIT 0 promptly
#   3. GET /eval        => eval box moved off /
#   4. GET /code        => lens browser (no lens wired notice)
#
# Usage:  scripts/smoke_remote_surface.sh
# Prereq: dune build remote/bin/main.exe
set -u
cd "$(dirname "$0")/.."

BIN=_build/default/remote/bin/main.exe
[ -x "$BIN" ] || { echo "FAIL: $BIN missing (run: dune build remote/bin/main.exe)"; exit 1; }

MOCK=$(mktemp /tmp/ppr-smoke-XXXX.json)
printf '[{"text": "three parts make a turn", "calls": []}]\n' > "$MOCK"
LOG=$(mktemp /tmp/ppr-smoke-XXXX.log)

"$BIN" --serve --mock-script "$MOCK" > "$LOG" 2>&1 &
PID=$!
cleanup() { kill "$PID" 2>/dev/null; rm -f "$MOCK" "$LOG"; }
trap cleanup EXIT

URL=""
for _ in $(seq 1 40); do
  URL=$(sed -n 's/^pp-remote serving //p' "$LOG" | tr -d '[:space:]')
  [ -n "$URL" ] && break
  sleep 0.25
done
[ -n "$URL" ] || { echo "FAIL: pp-remote never printed its URL"; cat "$LOG"; exit 1; }
URL=${URL%/}
echo "-- pp-remote at $URL"

fails=0
check() { # name, condition(0/1)
  if [ "$2" = 0 ]; then echo "OK   $1"; else echo "FAIL $1"; fails=$((fails+1)); fi
}

# 1. chat page
PAGE=$(curl -sS -m 5 "$URL/")
echo "$PAGE" | grep -q 'hx-post="/chat"'                       ; check "chat page has hx-post /chat form" $?
echo "$PAGE" | grep -q '/assets/hx-multipart.js'              ; check "chat page loads hx-multipart asset" $?
echo "$PAGE" | grep -q 'id="log"'                             ; check "chat page has #log target" $?

# 2. /chat round trip on keep-alive — must terminate promptly
H=$(mktemp /tmp/ppr-smoke-hdr-XXXX); B=$(mktemp /tmp/ppr-smoke-body-XXXX)
START=$(date +%s)
RC=0
curl -sS -m 10 -X POST "$URL/chat" -d 'msg=hello' -D "$H" -o "$B" || RC=$?
ELAPSED=$(( $(date +%s) - START ))
grep -qi '^content-type: multipart/mixed;' "$H"               ; check "/chat is multipart/mixed" $?
grep -qi '^transfer-encoding: chunked' "$H"                   ; check "/chat is chunked-framed" $?
[ "$RC" = 0 ]                                                 ; check "/chat response TERMINATES (curl rc=$RC)" $?
[ "$ELAPSED" -lt 10 ]                                         ; check "/chat completed fast (${ELAPSED}s)" $?
grep -q 'class="bubble user"' "$B"                            ; check "/chat part: user echo bubble" $?
grep -q 'class="bubble llm"' "$B"                             ; check "/chat part: llm bubble opener" $?
grep -q 'three parts make a turn' "$B"                        ; check "/chat part: assistant token" $?
tail -c 120 "$B" | grep -q -- "--.*--"                        ; check "/chat close boundary present" $?
rm -f "$H" "$B"

# 3. eval box moved to /eval
EVAL=$(curl -sS -m 5 "$URL/eval")
echo "$EVAL" | grep -q 'hx-post="/eval"'                      ; check "/eval renders the eval box" $?

# 4. lens browser renders (no lens wired notice without --lens)
CODE=$(curl -sS -m 5 "$URL/code")
echo "$CODE" | grep -q 'no lens wired'                        ; check "/code renders lens browser" $?

if [ "$fails" = 0 ]; then echo "PASS pp-remote surface smoke"; exit 0
else echo "FAIL ($fails checks)"; exit 1; fi
