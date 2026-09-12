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

# 1. filmstrip shell + chat page
SHELL=$(curl -sS -m 5 "$URL/")
echo "$SHELL" | grep -q 'id="fs-cascade"'                     ; check "shell has cascade strip" $?
echo "$SHELL" | grep -q 'id="fs-content"'                     ; check "shell has content strip" $?
echo "$SHELL" | grep -q 'id="fs-sink"'                        ; check "shell has OOB sink" $?
echo "$SHELL" | grep -q 'id="fs-launch"'                      ; check "shell has launcher" $?
echo "$SHELL" | grep -q '/assets/htmx.min.js'                 ; check "shell loads htmx" $?
echo "$SHELL" | grep -q 'fs-entry'                            ; check "cascade lists the agent surfaces" $?
PAGE=$(curl -sS -m 5 "$URL/chat")
echo "$PAGE" | grep -q 'hx-post="/chat"'                      ; check "chat page has hx-post /chat form" $?
echo "$PAGE" | grep -q '/assets/hx-multipart.js'              ; check "chat page loads hx-multipart asset" $?
echo "$PAGE" | grep -q 'id="log"'                             ; check "chat page has #log target" $?

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

# 6. filmstrip protocol (borge/agent.borg filmstrip-shell verify).
#    Runs in the repo cwd (the smoke cds to the repo root); clean below.
WS=.agent/workspace.json
rm -f "$WS"
OPEN=$(curl -sS -m 5 -X POST "$URL/filmstrip/open" -d 'route=/code')
echo "$OPEN" | grep -q 'hx-swap-oob="beforeend:#fs-content"'  ; check "launcher open answers OOB pane append" $?
echo "$OPEN" | grep -q 'fs-pane fs-third focused'             ; check "opened pane wears .focused at third width" $?
echo "$OPEN" | grep -q 'fs-title">/code'                      ; check "pane carries the dispatched /code fragment" $?
FRAG=$(curl -sS -m 5 -H 'HX-Request: true' "$URL/")
echo "$FRAG" | grep -q '<!doctype'
[ "$?" != 0 ]                                                 ; check "HX-Request / renders the shell FRAGMENT (no doctype)" $?
echo "$FRAG" | grep -q 'fs-body'                              ; check "shell fragment mounts pane bodies" $?
[ -f "$WS" ]                                                  ; check "workspace row persisted (.agent/workspace.json)" $?
PANE_ID=$(sed -n 's/.*"id":"\([^"]*\)".*/\1/p' "$WS" | head -1)
FOCUS=$(curl -sS -m 5 -X POST "$URL/filmstrip/focus" -d 'dir=right')
echo "$FOCUS" | grep -q 'hx-swap-oob="true"'                  ; check "focus POST answers OOB section swaps" $?
CASCADE=$(curl -sS -m 5 -X POST "$URL/filmstrip/cascade" -d 'd=0&t=code')
echo "$CASCADE" | grep -q 'fs-col'                            ; check "cascade crumb walk appends Miller columns" $?
RELOAD=$(curl -sS -m 5 "$URL/")
echo "$RELOAD" | grep -q 'fs-pane'                            ; check "workspace round-trips on reload" $?
CLOSE=$(curl -sS -m 5 -X POST "$URL/filmstrip/close" -d "id=$PANE_ID")
echo "$CLOSE" | grep -q "delete:#fs-pane-$PANE_ID"            ; check "close answers OOB pane delete" $?
rm -f "$WS"

if [ "$fails" = 0 ]; then echo "PASS agent surface smoke"; exit 0
else echo "FAIL ($fails checks)"; exit 1; fi
