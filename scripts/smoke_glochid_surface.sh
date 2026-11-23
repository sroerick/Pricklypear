#!/bin/ksh
# Agent HTML surface smoke — framing + parts + termination over HTTP/1.1
# keep-alive. Catches the close-delimited-body regression (respond_with_
# streaming without transfer-encoding => response never completes on the
# wire, htmx fetch never resolves).
#
# Boots `_build/default/glochid/bin/main.exe --serve --pp --mock-script`,
# then:
#   1. GET /            => chat page has the htmx form + asset script tags
#   2. GET /assets/hx-multipart.js => 200 + upstream marker
#   3. POST /chat (keep-alive default) => multipart/mixed, chunked TE,
#      all parts present, close boundary, and curl EXIT CODE 0 promptly
#      (pre-fix: curl rc=28 'Operation timed out' — body never terminated)
#   4. GET /code        => code browser lists files
#   5. GET /settings    => llm-config surface renders with the model
#                          <select> (borge/agent.borg llm-config)
#
# HERMETIC (T0): the binary runs with cwd, HOME, XDG_*, and TMPDIR all
# pointing inside a fresh mktemp sandbox, with a scrubbed environment
# (`env -i`, so no ambient PP_*/GLOCHID_*/AGENT_* provider or habitat
# credentials are visible). `--pp` is passed explicitly so this legacy smoke
# still exercises the full surface (bindings face, adapter app entries) with
# the adapter in its bridge-down/no-credential state; the base-mode negative
# control lives in scripts/verify-glochid-base-mode.sh (L2). Fixture files
# live in the sandbox work dir; every write this script inspects
# (.glochid/workspace.json, code/save) lands there. The developer's repo,
# ~/.config/pricklypear/env, and ~/.config/glochid are never read or written.
# The sandbox is removed on exit after the server is terminated and reaped.
#
# Usage:  scripts/smoke_glochid_surface.sh
# Prereq: dune build glochid/bin/main.exe
set -u

REPO=$(cd "$(dirname "$0")/.." && pwd)
BIN="${GLOCHID_SMOKE_BIN:-$REPO/_build/default/glochid/bin/main.exe}"
[ -x "$BIN" ] || { echo "FAIL: $BIN missing (run: dune build glochid/bin/main.exe)"; exit 1; }

# Snapshot repo-local generated state so the run can prove it wrote none.
# Covers the checkout root as well as the dune root, and lists directories
# as well as files (an empty `.agent/` is still a write).
state_snapshot() {
  for d in "$REPO/.agent" "$REPO/.glochid" "$REPO/../.agent" "$REPO/../.glochid"; do
    if [ -e "$d" ]; then
      echo "== $d"
      find "$d" -mindepth 1 2>/dev/null | sort
    else
      echo "absent $d"
    fi
  done
}
STATE_BEFORE=$(state_snapshot)

SANDBOX=$(mktemp -d "${TMPDIR:-/tmp}/glochid-smoke-XXXXXX") \
  || { echo "FAIL: cannot create sandbox"; exit 1; }

PID=""
cleanup() {
  if [ -n "$PID" ]; then
    kill -TERM "$PID" 2>/dev/null
    for _ in 1 2 3 4 5 6 7 8 9 10; do
      kill -0 "$PID" 2>/dev/null || break
      sleep 0.2
    done
    kill -0 "$PID" 2>/dev/null && kill -KILL "$PID" 2>/dev/null
    wait "$PID" 2>/dev/null
  fi
  rm -rf "$SANDBOX"
}
trap cleanup EXIT

mkdir -p "$SANDBOX/home" "$SANDBOX/work/src" "$SANDBOX/tmp" \
         "$SANDBOX/xdg/config" "$SANDBOX/xdg/cache" \
         "$SANDBOX/xdg/data" "$SANDBOX/xdg/state"

MOCK="$SANDBOX/mock.json"
printf '[{"text": "three parts make a turn", "calls": []}]\n' > "$MOCK"
LOG="$SANDBOX/glochid.log"

# Fixtures: the code browser and the /code file arm read the process cwd,
# so the sandbox work dir must carry the files the checks look for.
cat > "$SANDBOX/work/AGENTS.md" <<'FIXTURE'
# Smoke fixture

This file exists only inside the hermetic smoke sandbox.
FIXTURE
printf 'let hello = "world"\n' > "$SANDBOX/work/src/hello.ml"

# Scrubbed child env: only sandbox-local homes, cwd, and TMPDIR. The
# binary's cwd is the sandbox work dir (set by cd), never the checkout.
(
  cd "$SANDBOX/work" || exit 1
  exec env -i \
    HOME="$SANDBOX/home" \
    XDG_CONFIG_HOME="$SANDBOX/xdg/config" \
    XDG_CACHE_HOME="$SANDBOX/xdg/cache" \
    XDG_DATA_HOME="$SANDBOX/xdg/data" \
    XDG_STATE_HOME="$SANDBOX/xdg/state" \
    TMPDIR="$SANDBOX/tmp" \
    PATH="${PATH:-/usr/bin:/bin}" \
    USER="${USER:-smoke}" \
    "$BIN" --serve --pp --mock-script "$MOCK"
) > "$LOG" 2>&1 &
PID=$!

URL=""
for _ in $(seq 1 40); do
  URL=$(sed -n 's/^glochid serving //p' "$LOG" | tr -d '[:space:]')
  [ -n "$URL" ] && break
  sleep 0.25
done
[ -n "$URL" ] || { echo "FAIL: glochid never printed its URL"; cat "$LOG"; exit 1; }
URL=${URL%/}
echo "-- glochid at $URL (bin: $BIN; sandbox: $SANDBOX)"

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
echo "${DOC}" | grep -q 'fs-start-entry'                      ; check "start menu lists the glochid surfaces" $?
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
H="$SANDBOX/hdr"; B="$SANDBOX/body"
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
echo "$CODE" | grep -q 'hello.ml'                              ; check "/code tree lists the sandbox fixture" $?

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
#     error -> non-2xx/task error text). Paths are the runtime's own
#     `.glochid/` store, inside the sandbox work dir.
SAVE=$(curl -sS -m 10 -X POST "$URL/code/save" -d 'path=.glochid/smoke-save.txt&content=hello cm save')
echo "$SAVE" | grep -q '^saved$'                              ; check "/code/save writes" $?
RDBACK=$(curl -sS -m 10 "$URL/code?path=.glochid/smoke-save.txt")
echo "$RDBACK" | grep -q 'hello cm save'                      ; check "/code/save round-trips in the view" $?
[ -f "$SANDBOX/work/.glochid/smoke-save.txt" ]                ; check "/code/save wrote inside the sandbox" $?
# the escape POST returns an error body; the guard verdict is: the target
# (one level above the work dir, still inside the sandbox) must NOT exist
curl -sS -m 10 -X POST "$URL/code/save" --data-urlencode 'path=../escape-sentinel' --data-urlencode 'content=nope' > /dev/null 2>&1 || true
[ ! -e "$SANDBOX/escape-sentinel" ]                           ; check "traversal escape did not write" $?
# KNOWN GAP (out of T0 scope, recorded for U1): resolve_path permits
# ABSOLUTE paths (hostkit/host_seam.ml:133-143 only refuses ".."), so
# /code/save can write outside the cwd. The old check only proved
# ../../etc did not resolve to /etc. U1 must decide containment or
# document it; this test does not assert either way.
rm -f "$SANDBOX/work/.glochid/smoke-save.txt"

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

# 7. filmstrip protocol (borge/agent.borg filmstrip-shell verify).
#    The workspace row is the harness's local store, .glochid/ under the
#    process cwd (the sandbox work dir) — never the developer's tree.
WS="$SANDBOX/work/.glochid/workspace.json"
rm -f "$WS"
OPEN=$(curl -sS -m 5 -X POST "$URL/filmstrip/open" -d 'route=/code')
echo "$OPEN" | grep -q 'hx-swap-oob="beforeend:#fs-content"'  ; check "open POST answers OOB pane append" $?
echo "$OPEN" | grep -q 'fs-pane fs-third focused'             ; check "opened pane wears .focused at third width" $?
echo "$OPEN" | grep -q 'fs-title">/code'                      ; check "pane carries the dispatched /code fragment" $?
FRAG=$(curl -sS -m 30 -H 'HX-Request: true' "$URL/")
echo "$FRAG" | grep -q '<!doctype'
[ "$?" != 0 ]                                                 ; check "HX-Request / renders the shell FRAGMENT (no doctype)" $?
echo "$FRAG" | grep -q 'fs-body'                              ; check "shell fragment mounts pane bodies" $?
[ -f "$WS" ]                                                  ; check "workspace row persisted (.glochid/workspace.json)" $?
PANE_ID=$(sed -n 's/.*"id":"\([^"]*\)".*/\1/p' "$WS" | head -1)
# Seed a non-empty axis, then let the next save-all rebuild the row: the
# carried-forward field must survive. (The axis flip is retired; the
# render-side pin is asserted separately below.)
sed 's/"axis":""/"axis":"sentinel-axis"/' "$WS" > "$WS.seed" && mv "$WS.seed" "$WS"
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
# The axis flip is retired (BORGE-1789879934): the shell pins col via
# the #fs-axis sentinel/CSS; the ws row only CARRIES the field forward
# (libs/filmstrip/lib.pp save-all, "the stored axis field only carries
# forward here instead of being re-asserted to col"). The seed above is
# non-empty so this proves carry-forward, not merely that the key exists.
grep -q '"axis":"sentinel-axis"' "$WS"                        ; check "save-all carries a seeded axis forward" $?
! grep -q '"axis":"row"' "$WS"                                ; check "retired axis flip cannot persist row to the workspace row" $?
rm -f "$WS"

# 8. hermeticity: the run must not have created or changed any
#    repo-local generated state (the defect this card exists to fix).
STATE_AFTER=$(state_snapshot)
[ "$STATE_BEFORE" = "$STATE_AFTER" ]                          ; check "repo has no .agent/.glochid state written by the smoke" $?
[ ! -e "$SANDBOX/home/.config/pricklypear" ]                   ; check "smoke never created a PP config home" $?

if [ "$fails" = 0 ]; then echo "PASS glochid surface smoke"; exit 0
else echo "FAIL ($fails checks)"; exit 1; fi
