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

# 1. filmstrip shell + chat page. GET-page curls ride -m 30: shape
# checks must not race the CPU (a cold first render after a heavy
# dune build + runtest run measured >5s once; the /chat POST below
# keeps its tight timing — THAT one asserts the streaming contract).
DOC=$(curl -sS -m 30 "$URL/")
echo "${DOC}" | grep -q 'id="fs-start"'                       ; check "shell has the start menu on the rail" $?
echo "${DOC}" | grep -q 'id="fs-content"'                     ; check "shell has content strip" $?
echo "${DOC}" | grep -q 'id="fs-sink"'                        ; check "shell has OOB sink" $?
echo "${DOC}" | grep -q 'id="fs-railtabs"'                    ; check "shell has rail tabs (trees)" $?
! echo "${DOC}" | grep -q 'id="fs-launch"'                   ; check "no launcher in the shell" $?
! echo "${DOC}" | grep -q 'fs-deskbox'                        ; check "no desktop controlbox in the shell" $?
echo "${DOC}" | grep -q '/assets/htmx.min.js'                 ; check "shell loads htmx" $?
echo "${DOC}" | grep -q 'fs-start-entry'                      ; check "start menu lists the agent surfaces" $?
PAGE=$(curl -sS -m 30 "$URL/chat")
echo "$PAGE" | grep -q 'hx-post="/chat"'                      ; check "chat page has hx-post /chat form" $?
echo "$PAGE" | grep -q '/assets/hx-multipart.js'              ; check "chat page loads hx-multipart asset" $?
echo "$PAGE" | grep -q 'id="log"'                             ; check "chat page has #log target" $?

# 2. extension asset
ASSET=$(curl -sS -m 5 -o /dev/null -w '%{http_code}' "$URL/assets/hx-multipart.js")
[ "$ASSET" = 200 ]                                            ; check "hx-multipart.js 200" $?
curl -sS -m 30 "$URL/assets/hx-multipart.js" | grep -q 'Response.prototype.parts' \
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

# 4. code browser renders the file tree (the SAME outline rows the rail rides)
CODE=$(curl -sS -m 30 "$URL/code")
echo "$CODE" | grep -q 'fs-tr-item'                            ; check "/code renders the file tree" $?

# 4b. code file arm boots CodeMirror (BORGE-1789581611): the file <pre>
#     carries the pp-cm boot target attributes, the CM assets serve, and
#     the shell page bakes the CM boot scripts.
CMFILE=$(curl -sS -m 30 "$URL/code?path=AGENTS.md")
echo "$CMFILE" | grep -q 'data-pp-cm'                          ; check "/code file arm is a pp-cm boot target" $?
echo "$CMFILE" | grep -q 'data-path="AGENTS.md"'               ; check "/code file arm carries data-path" $?
curl -sS -m 5 -o /dev/null -w '%{http_code}' "$URL/assets/codemirror.min.js" | grep -q 200 ; check "CM core asset serves" $?
curl -sS -m 5 -o /dev/null -w '%{http_code}' "$URL/assets/codemirror.min.css" | grep -q 200 ; check "CM css asset serves" $?
curl -sS -m 5 -o /dev/null -w '%{http_code}' "$URL/assets/cm/scheme.min.js" | grep -q 200 ; check "CM scheme mode asset serves" $?
curl -sS -m 5 -o /dev/null -w '%{http_code}' "$URL/assets/pp-cm.js" | grep -q 200            ; check "CM boot script serves" $?
echo "${DOC}" | grep -q '/assets/codemirror.min.js'            ; check "shell loads CodeMirror" $?
echo "${DOC}" | grep -q '/assets/pp-cm.js'                     ; check "shell loads pp-cm boot" $?

# 4c. editor write-back: POST /code/save (fs/write sandbox guard is the
#     prim's) round-trips, and a traversal attempt is rejected (fs write
#     error -> non-2xx/task error text).
SAVE=$(curl -sS -m 10 -X POST "$URL/code/save" -d 'path=.agent/smoke-save.txt&content=hello cm save')
echo "$SAVE" | grep -q '^saved$'                              ; check "/code/save writes" $?
RDBACK=$(curl -sS -m 10 "$URL/code?path=.agent/smoke-save.txt")
echo "$RDBACK" | grep -q 'hello cm save'                      ; check "/code/save round-trips in the view" $?
# the escape POST returns an error body; the guard verdict is: the target
# must NOT exist on disk
curl -sS -m 10 -X POST "$URL/code/save" --data-urlencode 'path=../../etc/smoke-escape' --data-urlencode 'content=nope' > /dev/null 2>&1 || true
[ ! -e /etc/smoke-escape ]                                     ; check "traversal escape did not write" $?
rm -f .agent/smoke-save.txt

# 5. llm-config surface: settings page renders with a model <select>,
#    and the chat header carries the same dropdown (hx-post /settings).
#    With no API key in the smoke env the catalog is current-model-only
#    (the provider /models fetch is skipped) — shape, not contents.
SETTINGS=$(curl -sS -m 30 "$URL/settings")
echo "$SETTINGS" | grep -q '<select name="model"'             ; check "/settings has model select" $?
echo "$PAGE" | grep -q 'hx-post="/settings"'                  ; check "chat header has model dropdown" $?
echo "$PAGE" | grep -q '/bindings'                              ; check "chat header links the bindings browser" $?

# 6. bindings browser renders (borge/agent.borg filmstrip-shell; the
#    bind/* habitat-bridge prims fail soft at CALL time — with or
#    without habitat creds the page itself must render). Shape-only:
#    the package TREE (the rail prims box pane-face twin) when a
#    habitat answers, the muted bridge-down notice otherwise. -m 30:
#    list-packages rides the identity's env load the first time.
BINDINGS=$(curl -sS -m 30 "$URL/bindings")
echo "$BINDINGS" | grep -qF 'glochid — bindings'                ; check "/bindings renders" $?
BIND_FRAG=$(curl -sS -m 30 -H 'HX-Request: true' "$URL/bindings")
echo "$BIND_FRAG" | grep -q '<!doctype'
[ "$?" != 0 ]                                                   ; check "/bindings fragment mode (no doctype)" $?

# 7. filmstrip protocol (borge/agent.borg filmstrip-shell verify). (borge/agent.borg filmstrip-shell verify).
#    Runs in the repo cwd (the smoke cds to the repo root); clean below.
WS=.agent/workspace.json
rm -f "$WS"
OPEN=$(curl -sS -m 5 -X POST "$URL/filmstrip/open" -d 'route=/code')
echo "$OPEN" | grep -q 'hx-swap-oob="beforeend:#fs-content"'  ; check "open POST answers OOB pane append" $?
echo "$OPEN" | grep -q 'fs-pane fs-third focused'             ; check "opened pane wears .focused at third width" $?
echo "$OPEN" | grep -q 'fs-title">/code'                      ; check "pane carries the dispatched /code fragment" $?
FRAG=$(curl -sS -m 30 -H 'HX-Request: true' "$URL/")
echo "$FRAG" | grep -q '<!doctype'
[ "$?" != 0 ]                                                 ; check "HX-Request / renders the shell FRAGMENT (no doctype)" $?
echo "$FRAG" | grep -q 'fs-body'                              ; check "shell fragment mounts pane bodies" $?
[ -f "$WS" ]                                                  ; check "workspace row persisted (.agent/workspace.json)" $?
PANE_ID=$(sed -n 's/.*"id":"\([^"]*\)".*/\1/p' "$WS" | head -1)
FOCUS=$(curl -sS -m 5 -X POST "$URL/filmstrip/focus" -d 'dir=right')
echo "$FOCUS" | grep -q 'hx-swap-oob="true"'                  ; check "focus POST answers OOB section swaps" $?
CASCADE=$(curl -sS -m 5 -X POST "$URL/filmstrip/cascade" -d 'd=0&t=/settings')
echo "$CASCADE" | grep -q 'hx-swap-oob="beforeend:#fs-content"' ; check "cascade leaf click open-or-focuses the pane" $?
ZERO=$(curl -sS -m 5 -X POST "$URL/filmstrip/cascade" -d 'd=0&t=code')
echo "$ZERO" | grep -q 'fs-col'                                ; check "bare-name target gets exactly one column (no walk)" $?
RELOAD=$(curl -sS -m 30 "$URL/")
echo "$RELOAD" | grep -q 'fs-pane'                            ; check "workspace round-trips on reload" $?
CLOSE=$(curl -sS -m 5 -X POST "$URL/filmstrip/close" -d "id=$PANE_ID")
echo "$CLOSE" | grep -q 'state="shelved"\|fs-pane.*shelved'        ; check "close answers OOB shelve (record survives, v2 default close)" $?
echo "$CLOSE" | grep -q 'fs-tree-windows-root'                 ; check "close answers windows-tree rows OOB (v4)" $?
PAGE2=$(curl -sS -m 30 "$URL/")
echo "$PAGE2" | grep -q 'id="fs-axis" class="fs-sh-axis col"' ; check "axis sentinel pins col on the shell (sidebar-only, BORGE-1789879934)" $?
echo "$PAGE2" | grep -q 'id="fs-axis-btn"'                       ; [ $? -ne 0 ] ; check "no axis toggle button on the railtabs row (retired)" $?
echo "$PAGE2" | grep -q 'fs-sh-axis row'                       ; [ $? -ne 0 ] ; check "shell never renders the row axis" $?
ORIENT=$(curl -sS -m 5 -X POST "$URL/filmstrip/orient")
echo "$ORIENT" | grep -q 'fs-sh-axis\|fs-axis-btn\|route/dispatch: col' ; [ $? -ne 0 ] ; check "POST /filmstrip/orient can no longer flip the axis (retired route)" $?
grep -q '"axis":"col"' "$WS"                                  ; check "axis persisted col to the workspace row" $?
rm -f "$WS"

if [ "$fails" = 0 ]; then echo "PASS agent surface smoke"; exit 0
else echo "FAIL ($fails checks)"; exit 1; fi
