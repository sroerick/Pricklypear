#!/bin/ksh
# Agent HTML surface smoke — framing + parts + termination over HTTP/1.1
# keep-alive. Catches the close-delimited-body regression (respond_with_
# streaming without transfer-encoding => response never completes on the
# wire, htmx fetch never resolves).
#
# Boots `_build/default/agent/bin/main.exe --serve --mock-script`, then:
#   1. GET /            => chat page has the htmx form + asset script tags
#   2. GET /assets/hx-multipart.js => 200 + upstream marker
#   3. POST /chat (keep-alive default) => multipart/mixed, chunked TE,
#      all parts present, close boundary, and curl EXIT CODE 0 promptly
#      (pre-fix: curl rc=28 'Operation timed out' — body never terminated)
#   4. GET /code        => code browser lists files
#   5. GET /settings    => llm-config surface renders with the model
#                          <select> (borge/agent.borg llm-config)
#
# Usage:  scripts/smoke_agent_surface.sh
# Prereq: dune build agent/bin/main.exe
set -u
cd "$(dirname "$0")/.."

BIN=_build/default/agent/bin/main.exe
[ -x "$BIN" ] || { echo "FAIL: $BIN missing (run: dune build agent/bin/main.exe)"; exit 1; }

MOCK=$(mktemp /tmp/agent-smoke-XXXX.json)
printf '[{"text": "three parts make a turn", "calls": []}]\n' > "$MOCK"
LOG=$(mktemp /tmp/agent-smoke-XXXX.log)

"$BIN" --serve --mock-script "$MOCK" > "$LOG" 2>&1 &
PID=$!
cleanup() { kill "$PID" 2>/dev/null; rm -f "$MOCK" "$LOG"; }
trap cleanup EXIT

URL=""
for _ in $(seq 1 40); do
  URL=$(sed -n 's/^agent serving //p' "$LOG" | tr -d '[:space:]')
  [ -n "$URL" ] && break
  sleep 0.25
done
[ -n "$URL" ] || { echo "FAIL: agent never printed its URL"; cat "$LOG"; exit 1; }
URL=${URL%/}
echo "-- agent at $URL"

fails=0
check() { # name, condition(0/1)
  if [ "$2" = 0 ]; then echo "OK   $1"; else echo "FAIL $1"; fails=$((fails+1)); fi
}

# 1. chat page
PAGE=$(curl -sS -m 5 "$URL/")
echo "$PAGE" | grep -q 'hx-post="/chat"'                       ; check "page has hx-post /chat form" $?
echo "$PAGE" | grep -q '/assets/hx-multipart.js'              ; check "page loads hx-multipart asset" $?
echo "$PAGE" | grep -q 'id="log"'                             ; check "page has #log target" $?

# 2. extension asset
ASSET=$(curl -sS -m 5 -o /dev/null -w '%{http_code}' "$URL/assets/hx-multipart.js")
[ "$ASSET" = 200 ]                                            ; check "hx-multipart.js 200" $?
curl -sS -m 5 "$URL/assets/hx-multipart.js" | grep -q 'Response.prototype.parts' \
                                                              ; check "asset is upstream artifact (fetch-multipart prollyfill)" $?

# 3. /chat round trip on keep-alive — must terminate promptly
H=$(mktemp /tmp/agent-smoke-hdr-XXXX); B=$(mktemp /tmp/agent-smoke-body-XXXX)
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

# 4. code browser renders a file tree
CODE=$(curl -sS -m 5 "$URL/code")
echo "$CODE" | grep -q 'name</th>'                            ; check "/code renders file tree" $?

# 5. llm-config surface: settings page renders with a model <select>,
#    and the chat header carries the same dropdown (hx-post /settings).
#    With no API key in the smoke env the catalog is current-model-only
#    (the provider /models fetch is skipped) — shape, not contents.
SETTINGS=$(curl -sS -m 5 "$URL/settings")
echo "$SETTINGS" | grep -q '<select name="model"'             ; check "/settings has model select" $?
echo "$PAGE" | grep -q 'hx-post="/settings"'                  ; check "chat header has model dropdown" $?

if [ "$fails" = 0 ]; then echo "PASS agent surface smoke"; exit 0
else echo "FAIL ($fails checks)"; exit 1; fi
