(** Agent Nopales surface (borge/agent.borg surfaces).

    The chat + code browser pages are NOPALES-AUTHORED htmx fragments on
    the agent route table, rendered by Nopales via [libs/html] — NOT
    compiled OCaml pages and NOT vanilla-JS SSE. The surface is
    redefinable live (same env as plugins/REPL): re-evaluating a
    `(define chat/page …)` at the REPL changes the next render without a
    restart.

    The multipart STREAM for a chat/agent turn stays OCaml (http_ui.ml):
    provider SSE → delta assembly → per-part (html, HX-Target, HX-Swap)
    frame + flush. Nopales owns the page skeleton + the fragment
    templates (chat/user-bubble, chat/turn-open, chat/think-open,
    chat/think-token, chat/tool-open, chat/tool-line, chat/tool-done,
    chat/token, chat/error); OCaml owns the effects-coloured stream and
    calls these templates per part.

    [load] boot-loads the embedded Nopales packages (libs/html, libs/hyper)
    then this surface source into the shared env, and registers the
    route-table routes (/, /code). *)

open Pricklypear_lisp

(** The surface source. Defines chat/page, chat/user-bubble,
    chat/llm-bubble-open, chat/token, code/page and registers
    the / and /code routes via the agent route table. Uses libs/html +
    the agent's request-shape prims (query-param). *)
let surface_source = {pp|
;; ---- shared page chrome (borge/agent.borg filmstrip-shell) ----
;; Pages are PANE-AWARE: under HX-Request (filmstrip partial mode) a
;; route renders the BODY fragment only; a full GET gets the doc
;; skeleton + htmx scripts. One css block shared by every page + the
;; shell (incl. the theme custom props filmstrip/css consumes). Redefine
;; glochid/base-css live to restyle everything.
(define glochid/base-css
  (string-append
    ":root{color-scheme:dark;--border:#2a2e3a;--panel:#1b1f2a;--ink:#d8dee9;--muted:#6b7080;--chrome:#12141c;--sel-bg:#2a2e3a;--sel-accent:#7aa2f7}"
    "html,body{height:100%}"
    "body{background:#0f1117;color:#d8dee9;font-family:ui-monospace,Menlo,monospace;margin:0;padding:1rem;box-sizing:border-box}"
    "a{color:#7aa2f7;text-decoration:none}a:hover{text-decoration:underline}"
    ".muted{color:#6b7080}"
    "h1{margin:.25rem 0 .75rem 0;font-size:1.1rem}"
    "table{border-collapse:collapse;width:100%;max-width:60rem}td,th{padding:.35rem .6rem;border-bottom:1px solid #222;text-align:left;white-space:nowrap}"
    ;; sessions table: FIXED layout so the three width presets always
    ;; fit the pane — long first-prompt/model cells ellipsize instead of
    ;; forcing the pane body (and the strip) to scroll horizontally.
    "table.sess{table-layout:fixed}table.sess td,table.sess th{overflow:hidden;text-overflow:ellipsis}"
    "table.sess th:nth-child(1){width:10.5rem}table.sess th:nth-child(3){width:3rem}table.sess th:nth-child(5){width:6.5rem}"
    "pre{background:#12141c;padding:1rem;border-radius:6px;overflow:auto}"
    "input{background:#1b1f2a;color:#d8dee9;border:1px solid #333;border-radius:6px;padding:.5rem}"
    "select{background:#1b1f2a;color:#d8dee9;border:1px solid #333;border-radius:4px}"
    "label{color:#8a8f9e}"
    ".card{background:#1b1f2a;border:1px solid #2a2e3a;border-radius:6px;padding:1rem;max-width:38rem}"
    ".card p{margin:.5rem 0}"
    ".card input,.card select{width:100%;padding:.5rem;margin-top:.25rem}"
    "button{background:#2a6fdb;color:#fff;border:0;border-radius:6px;padding:.5rem 1rem;cursor:pointer}"
    ".fs-shell{height:calc(100vh - 2rem)}"
    ".fs-sh-axis{display:none}"
    ;; v5 col axis is a ROW, not a collapsed rail: cascade app menu
    ;; (20rem) | #fs-rail box (20rem: docked pane bodies + railtabs +
    ;; trees) | #fs-content = THE NIRI STRIP of main panes (full
    ;; remaining width, overflow-x). Focus is a SCROLL into stage (the
    ;; OOB autofocus + scroll-margin peeks), NOT a lift: no
    ;; position:fixed pane, no heads-only rail, no one-body-on-stage
    ;; model. Left ordering is CSS `order` so the DOM keeps the row-axis
    ;; sequence (content, then rail furniture) for the flex column.
    ".fs-shell:has(#fs-axis.col){flex-direction:row}"
    ".fs-shell:has(#fs-axis.col) .fs-cascade{order:-2;flex:0 0 20rem;flex-direction:column;max-height:none;height:100%;min-height:0;overflow-y:auto;overflow-x:hidden}"
    ".fs-shell:has(#fs-axis.col) .fs-content{order:0;flex:1 1 auto;min-height:0;min-width:0}"
    "#fs-surface{display:contents}"
    "#fs-rail{display:contents}"
    ".fs-shell:has(#fs-axis.col) #fs-rail{order:-1;display:flex;flex-direction:column;flex:0 0 20rem;max-width:20rem;min-width:0;min-height:0;overflow-y:auto;overflow-x:hidden;gap:.25rem;padding:.25rem .25rem .25rem 0}"
    ;; row axis: the rail furniture is a bottom band — rail panes dock
    ;; there as horizontal boxes (bodies live; dock semantics in both
    ;; axes).
    ".fs-shell:not(:has(#fs-axis.col)) .fs-pane[data-region=\"rail\"]{flex:0 0 auto;max-height:20rem;min-width:20rem;min-height:0;overflow:hidden}"
    ".fs-shell:not(:has(#fs-axis.col)) .fs-pane[data-region=\"rail\"] .fs-body{display:block;flex:1 1 auto;min-height:0;overflow:auto}"
    ;; col axis: docked panes stack down the rail box, each live body at
    ;; its flex share (the cascade's 16rem cap never applies — pinned
    ;; panes live in #fs-rail, not the cascade).
    ".fs-shell:has(#fs-axis.col) .fs-pane[data-region=\"rail\"]{flex:1 1 auto;width:100%;min-height:0}"
    ".fs-shell:has(#fs-axis.col) .fs-pane[data-region=\"rail\"] .fs-body{display:block;flex:1 1 auto;min-height:0;overflow:auto}"
    ;; chatpage sizes against the VIEWPORT (calc(100vh - 6rem)); that
    ;; always overflows a pane box. In rail mode fit it to the pane.
    ".fs-shell:has(#fs-axis.col) .chatpage{height:100%}"
    ".bubble{background:#1b1f2a;border-radius:6px;padding:.5rem .75rem;margin:.5rem 0;white-space:pre-wrap}"
    ".bubble.user{color:#9ece6a}"
    ".bubble.llm{color:#c0caf5}.bubble.repl{color:#bb9af7}"
    ".trace{color:#6b7080;font-size:.85em;white-space:pre-wrap}"
    ".think{color:#6b7080;font-size:.9em;margin:.4rem 0}"
    ".think summary{cursor:pointer}"
    ".think>div{white-space:pre-wrap}"
    ".reason{color:#8a8f9e;font-style:italic}"
    ".think .tk,.think .tool{margin:.25rem 0 .25rem .25rem}"
    ".think .tk>summary,.think .tool>summary{color:#8a8f9e}"
    ".think .tk>div,.think .tool>div{margin-left:.9rem;padding-left:.5rem;border-left:1px solid #2a2e3a}"
    ".think:not(:has(.reason,.trace)){display:none}"
    ".chatpage{display:flex;flex-direction:column;height:calc(100vh - 6rem)}"
    ".chatpage #log{flex:1;min-height:0;overflow-y:auto;border:1px solid #333;border-radius:6px;padding:.75rem;margin-bottom:.75rem}"
    ".chatpage form{display:flex;gap:.5rem;flex-shrink:0}"
    ".chatpage #msg{flex:1}"
    ".chatpage form button{flex-shrink:0}"
    "*{scrollbar-width:thin;scrollbar-color:#2a2e3a transparent}"
    "::-webkit-scrollbar{width:10px;height:10px}"
    "::-webkit-scrollbar-track{background:transparent}"
    "::-webkit-scrollbar-thumb{background:#2a2e3a;border-radius:6px}"
    "::-webkit-scrollbar-thumb:hover{background:#4a4f5c}"
    "::-webkit-scrollbar-corner{background:transparent}"
    ;; v2 desktops: the #ws-desk sentinel carries the current desktop to
    ;; CSS — one :has pair per desktop (0..9) hides panes of the others
    ;; (data-desktop is on every pane section; no JS, no re-render).
    ".fs-shell:has(#ws-desk.dsk-0) .fs-pane[data-desktop]:not([data-desktop=\"0\"]):not([data-region=\"rail\"]){display:none}"
    ".fs-shell:has(#ws-desk.dsk-1) .fs-pane[data-desktop]:not([data-desktop=\"1\"]):not([data-region=\"rail\"]){display:none}"
    ".fs-shell:has(#ws-desk.dsk-2) .fs-pane[data-desktop]:not([data-desktop=\"2\"]):not([data-region=\"rail\"]){display:none}"
    ".fs-shell:has(#ws-desk.dsk-3) .fs-pane[data-desktop]:not([data-desktop=\"3\"]):not([data-region=\"rail\"]){display:none}"
    ".fs-shell:has(#ws-desk.dsk-4) .fs-pane[data-desktop]:not([data-desktop=\"4\"]):not([data-region=\"rail\"]){display:none}"
    ".fs-shell:has(#ws-desk.dsk-5) .fs-pane[data-desktop]:not([data-desktop=\"5\"]):not([data-region=\"rail\"]){display:none}"
    ".fs-shell:has(#ws-desk.dsk-6) .fs-pane[data-desktop]:not([data-desktop=\"6\"]):not([data-region=\"rail\"]){display:none}"
    ".fs-shell:has(#ws-desk.dsk-7) .fs-pane[data-desktop]:not([data-desktop=\"7\"]):not([data-region=\"rail\"]){display:none}"
    ".fs-shell:has(#ws-desk.dsk-8) .fs-pane[data-desktop]:not([data-desktop=\"8\"]):not([data-region=\"rail\"]){display:none}"
    ".fs-shell:has(#ws-desk.dsk-9) .fs-pane[data-desktop]:not([data-desktop=\"9\"]):not([data-region=\"rail\"]){display:none}"
    ;; v2 app-links: a button styled as a bare link (chat/sessions/settings
    ;; nav in pane chrome; the anchor version navigated the whole window
    ;; out of the shell with a full GET). Overrides the generic button look.
    ".fs-applink{background:none;border:0;color:#7aa2f7;padding:0;margin:0;font:inherit;cursor:pointer;text-decoration:none}"
    ".fs-applink:hover{text-decoration:underline}"
    ".fs-modelsel{font-size:.6em;padding:.1rem .3rem}"
    ;; v4 rail trees: mode tabs controlbox + one :has pair per root id
    ;; (windows / code / prims) shows only the active tree box off the
    ;; #fs-railmode sentinel -- the SAME hiding trick desktops use, so a
    ;; mode flip never re-renders (expansion state is DOM).
    ".fs-railtabs{display:flex;gap:.3rem;align-items:center;border:1px solid var(--border);background:var(--panel);padding:.3rem .35rem}"
    ".fs-railtab.fs-btn{background:none;border:1px solid transparent;color:#7aa2f7}"
    ".fs-shell:has(#fs-railmode.rm-windows) .fs-rmode:not([data-rmode=\"windows\"]){display:none}"
    ".fs-shell:has(#fs-railmode.rm-code) .fs-rmode:not([data-rmode=\"code\"]){display:none}"
    ".fs-shell:has(#fs-railmode.rm-prims) .fs-rmode:not([data-rmode=\"prims\"]){display:none}"
    ;; /code file arm: CodeMirror boots into .pp-cm-host (replaces the
    ;; tagged <pre>; theme rides the shell dark palette).
    ".pp-cm-host .CodeMirror{height:80vh;font-size:13px;line-height:1.4;background:#12141c;color:#c0caf5}"
    ".pp-cm-host .CodeMirror-gutters{background:#12141c;border-right:1px solid #2a2e3a}"
    ".pp-cm-host .CodeMirror-linenumber{color:#4a4f5c}"
    ".pp-cm-host .CodeMirror-cursor{border-left:1px solid #7aa2f7}"
    ;; editor chrome: save row (button + status) the boot script injects
    ;; above each CM host
    ".pp-cm-bar{display:flex;gap:.6rem;align-items:center;margin:.35rem 0}"
    ".pp-cm-save{cursor:pointer}"
    ".pp-cm-status::after{content:none}"))

(define (glochid/layout title body)
  (if (hx-request?)
      (render-html body)
      (render-html
        (string-append
          "<!doctype html><html><head><meta charset=utf-8><title>" title "</title>"
          ;; CodeMirror for the /code file arm (BORGE-1789581611): CSS in
          ;; head, lib + modes + boot before </body> — boot-all benches
          ;; page pres; observer boots swapped-in pane pres later.
          "<link rel=stylesheet href=\"/assets/codemirror.min.css\">"
          "<style>" glochid/base-css "</style></head><body>"
          body
          "<script src=\"/assets/htmx.min.js\"></script>"
          "<script src=\"/assets/htmx-2-compat.min.js\"></script>"
          "<script src=\"/assets/hx-multipart.js\"></script>"
          "<script src=\"/assets/codemirror.min.js\"></script>"
          "<script src=\"/assets/cm/mllike.min.js\"></script>"
          "<script src=\"/assets/cm/markdown.min.js\"></script>"
          "<script src=\"/assets/cm/scheme.min.js\"></script>"
          "<script src=\"/assets/pp-cm.js\"></script>"
          "</body></html>"))))

;; ---- llm-config helpers (borge/agent.borg llm-config) ----
;; The model dropdown is fed by the provider ABI (agent/models = GET
;; {base}/models, cached host-side); the choice POSTs to /settings and
;; persists per-surface (model is NEVER replicated user data).
(define (settings/model-options current ms)
  (if (null? ms)
      ""
      (string-append
        (if (string=? (car ms) current)
            (string-append "<option value=\"" (html-escape (car ms)) "\" selected>" (html-escape (car ms)) "</option>")
            (string-append "<option value=\"" (html-escape (car ms)) "\">" (html-escape (car ms)) "</option>"))
        (settings/model-options current (cdr ms)))))

;; ---- chat surface (GET /) ----
;; The chat page skeleton: log target + an hx-post=/chat form with the
;; upstream htmx-4 hx-multipart extension so a multipart/mixed reply
;; streams each part live into the bubble. htmx + hx-multipart are
;; vendored assets served at /assets/*.js (baked into the agent binary).
;; hx-config '{"timeout":0}' disables htmx's 60s defaultTimeout for a
;; request whose lawful lifetime is "a whole agent turn".
;; hx-config '{"'timeout'":0}' disables htmx's 60s defaultTimeout for a
;; request whose lawful lifetime is "a whole agent turn".
(define (chat/head frag)
  ;; The 'agent' root anchor is shell-level (it reloads / in place) — a
  ;; plain <a> inside a fragment is the escape bug class, so fragments
  ;; get a muted label instead; nav words stay app-links in both modes.
  (string-append
    "<h1>"
    (if frag
        "<span class=\"muted\">agent</span>"
        "<a href=\"/\">agent</a>")
    " <span class=\"muted\">— "
    (filmstrip/app-link "/code" "/code")
    " · "
    (filmstrip/app-link "/bindings" "/bindings")
    " · "
    (filmstrip/app-link "/sessions" "/sessions")
    " · "
    (filmstrip/app-link "/settings" "/settings")
    " · model: "
    "<select name=\"model\" class=\"fs-modelsel\" hx-post=\"/settings\" hx-trigger=\"change\" hx-swap=\"none\">"
    (settings/model-options (agent/model) (agent/models))
    "</select></span></h1>"))

(define (chat/body)
  "Chat surface body — one structure for page AND pane, so the stream's
#log target works in both (borge/agent.borg filmstrip-shell #4).
Fragment-mode (hx-request?) differences: the h1 'agent' anchor and the
resumed-line +new //sessions anchors are shell-level escapes — a plain
<a> inside a fragment is the filmstrip bug class, so fragments get
app-link/button twins (POST /filmstrip/new, open-or-focus /sessions)."
  (let ((frag (hx-request?))
        (sid (query-param "session")))
    (string-append
      "<div class=\"chatpage\">"
      (chat/head frag)
      "<div id=\"log\">"
      (if (null? sid)
          "<div class=\"muted\">agent ready.</div>"
          (string-append
            "<div class=\"muted\">resumed session " (html-escape sid) " · "
            (if frag
                "<button class=\"fs-applink\" hx-post=\"/filmstrip/new\" hx-target=\"#fs-sink\" hx-swap=\"innerHTML\">+ new</button>"
                "<a href=\"/new\">+ new</a>")
            " · "
            (if frag
                (filmstrip/app-link "/sessions" "/sessions")
                "<a href=\"/sessions\">/sessions</a>")
            "</div>"
            (chat/bubbles (session/data sid))))
      "</div>"
      "<form id=\"f\" hx-post=\"/chat\" hx-target=\"#log\" hx-swap=\"beforeend\" hx-ext=\"hx-multipart\""
      " hx-config='{\"timeout\":0}' hx-on::before:request=\"this.reset()\">"
      "<input id=\"msg\" name=\"msg\" type=\"text\" placeholder=\"prompt…\" autocomplete=\"off\" autofocus>"
      "<button>send</button>"
      "</form></div>")))

(define (chat/page)
  (glochid/layout "glochid — agent" (chat/body)))
(route "/chat" "chat/page")

;; ---- sessions surface (GET /sessions; borge/agent.borg sessions-persist) ----
;; Appended-only JSONL transcripts under .agent/sessions/. Resume:
;; GET /resume?id=<id> re-points the live history + current session,
;; then lands back on the chat page with the transcript backfilled.
(define (redirect loc)
  ;; unquoted attribute values (url may not contain spaces/quotes here —
  ;; ids are hex-safe) keep the Nopales string free of escape layers.
  (render-html
    (string-append "<!doctype html><meta http-equiv=refresh content=0;url=" (html-escape loc) ">")))

(define (query-str n)
  (let ((v (query-param n)))
    (if (null? v) "" v)))

(define (chat/bubble m)
  (let ((role (dict-get m "role")) (text (dict-get m "text")))
    (if (string-eq role "user")
        (string-append "<div class=\"bubble user\">" (html-escape text) "</div>")
        (if (string-eq role "tool")
            (string-append "<div class=\"trace\">" (html-escape text) "</div>")
            (string-append "<div class=\"bubble llm\">" (html-escape text) "</div>")))))

(define (chat/bubbles data)
  (list/foldl (lambda (acc m) (string-append acc (chat/bubble m)))
               "" (json-parse data)))

(define (sessions/rows data)
  (list/foldl
    (lambda (acc s)
      (let ((id (dict-get s "id"))
            (utc (dict-get s "utc"))
            (count (dict-get s "count"))
            (model (dict-get s "model"))
            (preview (dict-get s "preview")))
        ;; Fragment mode (pane body): view = open-or-focus its chat pane
        ;; via app-link; resume = POST /filmstrip/resume (act + OOB swaps
        ;; — the anchor twins navigate the whole window out of the shell).
        ;; Standalone page keeps the anchor twins (meta-refresh redirect).
        (string-append acc
          "<tr><td class=\"muted\">" (html-escape utc)
          "</td><td>" (html-escape model)
          "</td><td>" (html-escape count)
          "</td><td class=\"muted\">" (html-escape preview)
          "</td><td>"
          (if (hx-request?)
              (string-append
                "<button class=\"fs-applink\" hx-post=\"/filmstrip/resume\" "
                "hx-vals='{\"id\":\"" (html-escape id) "\"}' "
                "hx-target=\"#fs-sink\" hx-swap=\"innerHTML\">resume</button>"
                " \183 "
                (filmstrip/app-link (string-append "/chat?session=" id) "view"))
              (string-append "<a href=\"/resume?id=" (html-escape id) "\">resume</a>"
                " \183 <a href=\"/?session=" (html-escape id) "\">view</a>"))
          "</td></tr>")))
    "" (json-parse data)))

(define (sessions/page)
  (glochid/layout "glochid — sessions"
    (string-append
      (if (hx-request?)
          "<h1><span class=\"muted\">/sessions</span></h1>"
          "<h1><a href=\"/\">agent</a> <span class=\"muted\">— /sessions</span></h1>")
      (if (hx-request?)
          "<p><button class=\"fs-applink\" hx-post=\"/filmstrip/new\" hx-target=\"#fs-sink\" hx-swap=\"innerHTML\">+ new session</button></p>"
          "<p><a href=\"/new\">+ new session</a></p>")
      "<table class=\"sess\"><tr><th>time</th><th>model</th><th>msgs</th><th>first prompt</th><th></th></tr>"
      (sessions/rows (session/list))
      "</table>"
      "<p class=\"muted\">transcripts: .agent/sessions/*.jsonl (append-only, local store)</p>")))
(route "/sessions" "sessions/page")

(define (sessions/new)
  (session/new)
  (redirect "/"))
(route "/new" "sessions/new")

(define (sessions/resume)
  (let ((id (query-str "id")))
    (begin
      (session/resume id)
      (redirect (string-append "/?session=" (html-escape id))))))
(route "/resume" "sessions/resume");; ---- settings surface (GET /settings; borge/agent.borg llm-config) ----
;; Blank fields KEEP their values (password input round-trips blank).
;; Key + base normally arrive invisibly: pp-remote --sync writes them
;; to <lens>/__config.json from the habitat's sync round metadata
;; (auth.user-llm-keys 5b). Model is a per-surface dial from the
;; provider's own /models catalog.
(define (settings/page)
  (glochid/layout "glochid — settings"
    (string-append
      (if (hx-request?)
          "<h1><span class=\"muted\">/settings</span></h1>"
          "<h1><a href=\"/\">agent</a> <span class=\"muted\">— /settings</span></h1>")
      "<div class=\"card\"><form method=\"post\" action=\"/settings\">"
      "<p><label>API key — " (agent/key-status) " · blank keeps current</label>"
      "<input type=\"password\" name=\"api_key\" autocomplete=\"off\"></p>"
      "<p><label>Base URL</label>"
      "<input type=\"text\" name=\"base_url\" value=\"" (html-escape (agent/base-url)) "\"></p>"
      "<p><label>Model</label>"
      "<select name=\"model\">" (settings/model-options (agent/model) (agent/models)) "</select></p>"
      "<button>save</button></form>"
      "<p class=\"muted\">Key + base come down invisibly via pp-remote --sync "
      "(lens __config.json). Model is chosen per surface from the provider's /models.</p>"
      "</div>")))
(route "/settings" "settings/page")

;; ---- chat fragment templates (called per-part by the OCaml stream) ----
;; Each returns a raw-HTML fragment the multipart emitter writes as one
;; part with per-part HX-Target/HX-Swap. Redefine live to restyle.
(define (chat/user-bubble text)
  (render-html (string-append "<div class=\"bubble user\">" (html-escape text) "</div>")))

(define (chat/llm-bubble-open id)
  (render-html (string-append "<div class=\"bubble llm\" id=\"" id "\"></div>")))

;; REPL result bubble: a chat prompt starting with ( evaluates LOCALLY in
;; the shared env (same rule as the stdin REPL — no model turn) and the
;; last value lands here, REPL-style via the host printer.
(define (chat/repl-bubble text)
  (render-html (string-append "<div class=\"bubble repl\">" (html-escape text) "</div>")))

;; Per-turn skeleton: collapsible thinking zone (reasoning + tool traces)
;; followed by the streamed response bubble. Render order in one part
;; because the targets must exist before any delta parts arrive. #<id>-think
;; is the zone container: the stream appends one NESTED <details> block per
;; reasoning run (chat/think-open) and per tool call (chat/tool-open), so a
;; finished turn scans as a list of collapsible items instead of one flat
;; span/trace wall.
(define (chat/turn-open id)
  (render-html
    (string-append
      "<div class=\"turn\"><details class=\"think\"><summary>thinking + tools</summary>"
      "<div id=\"" id "-think\"></div></details>"
      "<div class=\"bubble llm\" id=\"" id "\"></div></div>")))

;; Nested thinking block: one contiguous reasoning run gets its own
;; <details>/<summary> inside the zone; think-token deltas stream into #id.
;; Starts CLOSED so a finished turn stays scannable.
(define (chat/think-open id)
  (render-html
    (string-append
      "<details class=\"tk\"><summary>thinking</summary>"
      "<div id=\"" id "\"></div></details>")))

;; Nested tool-call block: <details>/<summary> inside the zone; input/output
;; lines (chat/tool-line) stream into #id, and chat/tool-done marks the
;; summary via innerHTML on #<id>-sum when the call returns.
(define (chat/tool-open id name)
  (render-html
    (string-append
      "<details class=\"tool\"><summary id=\"" id "-sum\">tool: "
      (html-escape name) "</summary><div id=\"" id "\"></div></details>")))

(define (chat/tool-line label text)
  (render-html
    (string-append
      "<div class=\"trace\">" (html-escape label) " "
      (html-escape text) "</div>")))

(define (chat/tool-done name)
  (render-html (string-append "tool: " (html-escape name) " &#10003;")))

;; Provider chain-of-thought delta, into the current thinking block.
(define (chat/think-token text)
  (render-html (string-append "<span class=\"reason\">" (html-escape text) "</span>")))

(define (chat/token text)
  (render-html (string-append "<span>" (html-escape text) "</span>")))

(define (chat/error text)
  (render-html (string-append "<div class=\"trace\">[turn error: " (html-escape text) "]</div>")))

;; ---- code browser (GET /code) ----
;; Read-only file tree + file content arm, rendered by Nopales via fs/ls
;; + fs/read + fs/dir? (agent host prims). query-param "path" selects
;; the entry (relative to the agent's cwd); the agent's fs prims refuse
;; ".." escapes (sandbox posture).
;; ONE TEMPLATE, TWO FACES (rail-trees-v4 amendment): the dir arm
;; renders the SAME outline rows the rail code box renders --
;; filmstrip/tree-node-li over the glochid/code-tree levelfn, eager
;; kids under the threshold, native <details> folds. The retired table
;; walk (code/rows + code/link + the pane-local <a> discipline)
;; wanted a second template and a nav trick the tree makes moot: a dir
;; label folds IN PLACE (no re-render, no URL change), a leaf label
;; open-or-focuses /code?path=<file> via POST /filmstrip/open.
;; The file arm is the pane's own: <pre> content, no children.
(define (code/head)
  (if (hx-request?)
      "<h1><span class=\"muted\">/code</span></h1>"
      "<h1><a href=\"/\">agent</a> <span class=\"muted\">/</span> <a href=\"/code\">code</a></h1>"))

(define (code/tree rel)
  "The /code dir face, both modes: the same node rows the rail tree
box rides (root id 'code' -- lazily folded dirs pull their level via
the ONE /filmstrip/tree endpoint; neither face gets box chrome here)."
  (string-append
    "<p class=\"muted\">/code"
    (if (string-eq rel "") "" (string-append " " (html-escape rel)))
    "</p>"
    "<ul class=\"fs-tr-kids fs-tr-root\">"
    (filmstrip/tree-level "code" (glochid/code-tree rel))
    "</ul>"))

(define (code/page)
  (let* ((p (query-param "path"))
         (rel (if (null? p) "" p)))
    (if (fs/dir? rel)
        (glochid/layout "glochid — code"
          (string-append (code/head) (code/tree rel)))
        (let* ((content (fs/read rel)))
          (glochid/layout "glochid — code"
            (string-append
              (code/head)
              "<p class=\"muted\">" (html-escape rel) "</p>"
              ;; file arm (BORGE-1789581611): still a server-escaped
              ;; <pre>, tagged as a CM boot target — pp-cm.js swaps in
              ;; the read-only CodeMirror; spec stanza amended.
              "<pre data-pp-cm data-path=\"" (html-escape rel) "\">"
              (html-escape content) "</pre>"))))))

;; POST /code/save — the file arm's write-back (edit half of
;; BORGE-1789581611): pp-cm.js posts the CM buffer as path+content and
;; reads the response body as a status string. The sandbox is the PRIM
;; layer's job: fs/write resolves through resolve_path, which refuses
;; ".." — no Nopales-side traversal guard to maintain.
(define (code/save)
  (let ((rel (request-param "path"))
        (content (request-param "content")))
    (if (or (null? rel) (null? content))
        "error: path and content required"
        (begin
          (fs/write rel content)
          "saved"))))
(route "/code/save" "code/save")

(route "/code" "code/page")

;; ---- bindings browser (GET /bindings; borge/agent.borg filmstrip-shell cascade) ----
;; The habitat's FUNCTION GRAMMAR (the image's /bindings face) as a
;; glochid app: package -> function -> doc+source, read-only over the
;; bind/* habitat-bridge prims (agent/lib/habitat.ml — the one
;; POST /api/eval client; creds AGENT_HABITAT_(URL|USER|PASS) > PP_*).
;; FAIL-SOFT at CALL time like the mem/* bridge, so every page/cascade
;; touch wraps its call in bind/safe (a guard) and renders a muted
;; notice / empty column when the bridge is unconfigured or down — the
;; shell itself never errors on the browser's behalf.
;; PANE-LOCAL NAV: same discipline as the code browser — pane-body
;; links hx-get into closest .fs-body; the standalone page keeps plain
;; anchors. The deep pages ride the pane-dispatchable URLs
(define bind/safe
  (lambda (thunk)
    (guard (lambda (e) (list "err" (nth e 1)))
           (list "ok" (thunk))))
  "Run one bridge call; (ok json) or (err msg). The bridge raises
habitat-unreachable at CALL time, so the surface renders instead of
500ing.")

(define bind/list-result
  (lambda (r)
    (if (string-eq (car r) "ok") (json-parse (nth r 1)) (list)))
  "Parsed entry list of a bind/safe (ok json), else the empty list.")

(define bind/bridge-down
  (lambda (msg)
    (string-append
      "<p class=\"muted\">habitat bridge: " (html-escape (as-str msg)) "</p>"
      "<p class=\"muted\">set AGENT_HABITAT_URL / AGENT_HABITAT_USER / AGENT_HABITAT_PASS "
      "(or PP_URL / PP_USER / PP_PASS) so glochid can read the habitat's function grammar.</p>"))
  "The page's bridge-down block: the bridge's own error text + the
credential pointer (same env trio the mem/* bridge uses).")

(define bind/head
  (lambda ()
    (if (hx-request?)
        "<h1><span class=\"muted\">/bindings</span></h1>"
        "<h1><a href=\"/\">agent</a> <span class=\"muted\">/</span> <a href=\"/bindings\">bindings</a></h1>"))
  "Pane-mode head is a muted label (a plain <a> in a fragment is the
shell-escape bug class).")

(define bind/link
  (lambda (url label)
    (if (hx-request?)
        (string-append
          "<a href=\"" url "\" hx-get=\"" url
          "\" hx-target=\"closest .fs-body\" hx-swap=\"innerHTML\">"
          (html-escape label) "</a>")
        (string-append "<a href=\"" url "\">" (html-escape label) "</a>")))
  "A pane-aware link: fragment mode walks INSIDE the pane (closest
.fs-body); standalone GET carries a plain href.")

(define bind/trail
  (lambda (pkg name)
    (if (string-eq pkg "")
        ""
        (string-append
          "<p class=\"muted\">"
          (bind/link "/bindings" "packages")
          " · " (bind/link (string-append "/bindings?pkg=" pkg) pkg)
          (if (string-eq name "") "" (string-append " · " (html-escape name)))
          "</p>")))
  "Breadcrumb line: packages · <pkg> [ · <name>]. Nothing at the root
(the tree folds DOWN; the trail is the only way up, so its pane-local
anchors stay — the one legal <a> use in a fragment).")

(define bind/tree
  (lambda (pkg)
    (string-append
      "<p class=\"muted\">/bindings"
      (if (string-eq pkg "") "" (string-append " " (html-escape pkg)))
      "</p>"
      "<ul class=\"fs-tr-kids fs-tr-root\">"
      (filmstrip/tree-level
        "prims"
        (glochid/prims-tree
          (if (string-eq pkg "") "" (string-append "prims/" pkg))))
      "</ul>"))
  "The /bindings tree face, both modes: the SAME node rows the rail
prims box rides (root id 'prims' -- folded packages pull their level
via the ONE /filmstrip/tree endpoint; neither face gets box chrome).")

(define bind/root-html
  (lambda ()
    (let ((r (bind/safe bind/packages)))
      (if (string-eq (car r) "ok")
          (string-append
            (bind/tree "")
            "<p class=\"muted\">the habitat's function grammar (list-packages over POST /api/eval) — read-only in glochid</p>")
          (bind/bridge-down (nth r 1)))))
  "The /bindings root: the package tree when the bridge answers; the
bridge-down block otherwise (ONE bridge call, checked before render
-- the levelfn re-calls fail-soft).")

(define bind/field
  (lambda (d k)
    (let ((v (dict-get d k)))
      (if (null? v) "" (as-str v))))
  "Text at key k of a detail row; doc/source null (no doc / no body on
this branch) renders as the empty string.")

(define bind/detail-html
  (lambda (pkg name)
    (let ((r (bind/safe (lambda () (bind/get name)))))
      (if (string-eq (car r) "ok")
          (let ((d (nth r 1)))
            (string-append
              "<h2>" (html-escape name) "</h2>"
              "<p class=\"muted\">" (html-escape (bind/field d "doc")) "</p>"
              "<pre>" (html-escape (bind/field d "source")) "</pre>"))
          (bind/bridge-down (nth r 1)))))
  "/bindings?pkg=<pkg>&name=<name>: docstring + pretty source.")

(define bind/page
  (lambda ()
    (let ((pkg (query-str "pkg")) (name (query-str "name")))
      (glochid/layout "glochid — bindings"
        (string-append
          (bind/head)
          (bind/trail pkg name)
          (if (string-eq name "")
              (if (string-eq pkg "")
                  (bind/root-html)
                  (bind/tree pkg))
              (bind/detail-html pkg name))))))
  "GET /bindings: the SAME tree template as the rail prims box --
root = package dirs, ?pkg=<pkg> = that package's function level,
?pkg=&name= = the detail arm (docstring + pretty source, the pane's
file-arm twin). The retired table walk (bind/rows + bind/packages-html
+ bind/pkg-html) wanted a second template the shared outline makes
moot: a package label folds in place, a function label open-or-focuses
its detail pane, the trail is the only way up.")
(route "/bindings" "bind/page")
|pp}

(** The glochid layer (borge/agent.borg filmstrip-shell). Load order:
    libs/html, libs/css, libs/filmstrip, THIS, then [surface_source].
    The filmstrip lib boots as glochid's shell with two HOST seams
    swapped (the harness's request-runner seam makes this a load, not a
    port — the allowed variation is recorded in borge/filmstrip.borg):

      1. WORKSPACE BACKING: filmstrip/ws-node / ws-save redefined over
         fs prims into .agent/workspace.json (project-local, beside
         .agent/sessions). The habitat's tree/upsert row stays the
         image's; a lens-backed row is the deferred unification.
      2. THE CASCADE IS THE AGENT'S: filmstrip/apps + filmstrip/browse
         redefined — chat, code (a filesystem cascade), sessions,
         settings. request-param is a shim over query-param (the POST
         gate merges the form body into params) — NOT a new host prim.

    Filmstrip make-note dialect gotchas apply: let* is SINGLE-BODY
    (wrap multi-step tails in (seq ...)); length accepts cons pairs;
    routes are exact-match strings. *)
let glochid_source = {pp|
;; ---- addressing shim: filmstrip reads request-param; the gate merges
;; the form body into params, so query-param answers both channels.
(define (request-param n) (query-param n))

;; ---- flat multi-key dict update (pure catalog has dict-set only; the
;; habitat gets this from libs/std).
(define dict-set*
  (lambda (json kvs)
    (if (null? kvs)
        json
        (if (null? (cdr kvs))
            json
            (dict-set*
              (dict-set json (car kvs) (car (cdr kvs)))
              (cdr (cdr kvs))))))
  "Apply flat key/value pairs from kvs onto json via dict-set.")

;; ---- 1. WORKSPACE BACKING: .agent/workspace.json ----
(define glochid/ws-file ".agent/workspace.json")
(define filmstrip/ws-node
  (lambda ()
    (if (fs/exists? glochid/ws-file)
        (dict-set "{}" "body" (fs/read glochid/ws-file))
        ()))
  "This workspace's fields holder, or () when never saved. The habitat
uses tree/get + node-path; glochid rides the local fs (HOST SEAM 1).")
(define filmstrip/ws-save
  (lambda (fields)
    (seq (fs/write glochid/ws-file fields) fields))
  "Persist workspace fields to .agent/workspace.json (HOST SEAM 1).")

;; ---- 2b. AXIS (RETIRED — sidebar-only shell) ----
;; OBITUARY (BORGE-1789879934, 2026-09-19 operator decision): the axis
;; toggle (POST /filmstrip/orient + the rail-tabs button) is GONE; glochid
;; is a sidebar-only shell. glochid/axis pins "col" unconditionally — a
;; stale ws row that still says "row" cannot strand the operator in the
;; bottom-band layout with no way back. The #fs-axis inert sentinel stays
;; (CSS :has(#fs-axis.col) keys the sidebar layout off it; the lib's
;; row-axis rules + ws field remain as dormant shared defaults).
(define glochid/axis
  (lambda () "col")
  "Pinned shell axis: COL (left rail) always — the axis toggle was
retired, see the obituary above.")

(define filmstrip/save-all
  (lambda (cascade panes focus)
    (filmstrip/ws-save
      (dict-set*
        "{}"
        (list
          "cascade"
          (json-emit (list/map (lambda (c) c) cascade))
          "content"
          (filmstrip/panes-emit panes)
          "focus"
          (string-append "content:" (as-str focus))
          "axis"
          (glochid/axis)
          "desktop"
          (as-str (glochid/deskt))))))
  "Write all workspace fields (cascade/content/focus + axis + desktop)
in one row upsert (glochid: the lib's save-all would drop the shell
fields). save-all REBUILDS the dict — every field added to the ws row
must be carried HERE through every write path.")

;; ---- 2. THE CASCADE IS THE AGENT'S ----
(define glochid/apps
  (lambda ()
    (list
      (list "chat" "/chat")
      (list "code" "/code")
      (list "bindings" "/bindings")
      (list "sessions" "/sessions")
      (list "settings" "/settings")))
  "Column 0: the agent's interfaces, ALL route leaves
(borge/filmstrip.borg sidebar-main-dichotomy): every entry
open-or-focuses a main-window pane. Structure browsing is the rail
trees' job (code -> glochid/code-tree fs walk; prims ->
glochid/prims-tree over the habitat's grammar), never cascade crumbs.

The lib's filmstrip/apps is redefined TOO (not just browse): the app
list IS the cascade's column 0 AND the launcher's hint source
(filmstrip/launcher-hint) — if it stayed habitat vocabulary, the
launcher advertised /todos on a host with no /todos route.")

(define filmstrip/apps
  (lambda () (glochid/apps))
  "glochid's shell app list (HOST SEAM 2): the agent's interfaces, all
leaves. Redefined so any lib read of the app list (hint-routes) sees
host vocabulary, never habitat apps.")

(define filmstrip/browse
  (lambda (path)
    (cons (list (glochid/apps)) #f))
  "glochid's shell cascade (HOST SEAM 2): column 0 is the agent's
interfaces, all leaves — no deeper columns, no crumbs. The stored path
only ever names the picked route (entry-html wears it as fs-sel).")

;; ---- 2d. DESKTOPS ----
;; The ws row carries "desktop":N (0 default) and each pane record a
;; "desktop" field. An inert #ws-desk sentinel beside #fs-axis carries
;; the current desktop to CSS (:has hides the other desktops' panes —
;; ten pairs in glochid/base-css, no JS); the windows tree's desktop
;; rows carry the switch verb (no separate desktop controlbox —
;; sidebar-main-dichotomy). Focus heals onto the first live pane of the target
;; desktop when the focused pane lives elsewhere (a live MAIN pane is
;; preferred — the lift; a rail pane satisfies I1 when no main exists).
(define glochid/deskt
  (lambda ()
    (let
      ((v (dict-get (filmstrip/ws-fields) "desktop")))
      (if (null? v) 0 (parse-int (as-str v)))))
  "Current shell desktop from the ws row (0 when unset).")

(define glochid/first-on-desk
  (lambda (panes d i)
    (let
      ((m (glochid/first-region-on-desk panes d i "main")))
      (if (< m 0) (glochid/first-region-on-desk panes d i "rail") m)))
  "Index of the first LIVE pane on desktop d, else -1 (v3 heal
discipline: a live MAIN pane is preferred — it takes the lift; rail
panes are valid focus targets too and satisfy I1 when the desktop has
no live main pane. Never a shelved pane: pane-live? gates both passes.")

(define glochid/first-region-on-desk
  (lambda (panes d i region)
    (if
      (null? panes)
      -1
      (if
        (and (= (filmstrip/pane-desk (car panes)) d)
             (string-eq (filmstrip/pane-region (car panes)) region)
             (filmstrip/pane-live? (car panes)))
        i
        (glochid/first-region-on-desk (cdr panes) d (+ i 1) region))))
  "Index of the first LIVE pane of one REGION on desktop d, else -1.")

(define glochid/desk-sentinel
  (lambda (d ?oob)
    (html/tag
      "div"
      (html/attrs
        (append
          (list
            (html/attr "id" "ws-desk")
            (html/attr "class" (string-append "fs-sh-axis dsk-" (as-str d))))
          (if ?oob
              (list (html/attr "hx-swap-oob" "outerHTML:#ws-desk"))
              (list))))
      ""))
  "Inert desktop sentinel beside the axis one; CSS :has(#ws-desk.dsk-N)
hides the other desktops' panes. ?oob renders it as an OOB outerHTML
swap (the desktop response).")

(define glochid/desktop-set
  (lambda ()
    (let*
      ((dv (request-param "d"))
        (d (if (null? dv) 0 (parse-int (as-str dv))))
        (row (filmstrip/row))
        (cascade (nth row 0))
        (panes (nth row 1))
        (focus (nth row 2))
        (focus2
          (if
            (and (>= focus 0)
                 (= (filmstrip/pane-desk (filmstrip/pane-at panes focus)) d))
            focus
            (let
              ((f (glochid/first-on-desk panes d 0)))
              (if (< f 0) focus f)))))
      (seq
        (filmstrip/ws-save
          (dict-set* (filmstrip/ws-fields) (list "desktop" (as-str d))))
        (filmstrip/save-all cascade panes focus2)
        (render-html
          (html/join
            (list
              (filmstrip/oob-msg "")
              (glochid/desk-sentinel d #t)
              (filmstrip/oob-content-strip panes focus2)))))))
  "POST /filmstrip/desktop: d param -> set + persist the shell desktop;
heal focus onto the first live pane of that desktop when the focused
pane lives elsewhere; answer with OOB swaps of the desk sentinel + a
re-render of the content strip (panes of the new desktop keep their
live DOM via hx-preserve; panes of the others re-render inert — the
workspace row keeps every record, only the swapped DOM shrinks).")

;; focus healing at render: an unset focus OR one pointing at a shelved
;; pane heals to the first LIVE pane ON THE CURRENT DESKTOP (v2 task 3;
;; v3: main panes first, rail panes as the valid fallback; falls back to
;; the first live pane anywhere when the desktop has none).
(define glochid/heal-focus
  (lambda (panes focus)
    (if
      (if (< focus 0)
          #t
          (string-eq
            (filmstrip/pane-state (filmstrip/pane-at panes focus))
            "shelved"))
      (let
        ((f (glochid/first-on-desk panes (glochid/deskt) 0)))
        (if (< f 0) (filmstrip/first-live-index panes) f))
      focus))
  "Focus that always has a live pane behind it: unset (-1) and
shelved-pointing both heal to the first live pane on the CURRENT
desktop — v3: a live MAIN pane first (the lift), the first live RAIL
pane as the fallback (a focused rail pane in the rail satisfies I1
when the desktop has no live main pane); -1 only when the desktop has
no live pane at all (falls back to the first live pane anywhere).
Never a shelved pane (I2). Not persisted — alt+right heals it for real
on the next focus step.")

;; ---- 2b2. RAIL TREES (v4: the rail inventory = N trees, ONE renderer) ----
;; borge/filmstrip.borg rail-trees-v4. Three roots:
;;   windows -- TARGETING tree from DECLARED ws state (desktops 0..2 +
;;              shelf subtree; labels POST the verb the word names;
;;              pane rows carry focus/pin/width/close or restore/destroy)
;;   code    -- the fs walk as a tree (leaf labels open /code?path panes)
;;   prims   -- the habitat's function grammar via the bind/* bridge
;;              (packages -> functions; leaf labels open /bindings panes)
;; The windows tree REPLACES the v2 winbox/shelf cboxes (shelf = subtree):
;; the lib's mutation hooks re-render #fs-tree-windows OOB (these
;; redefinitions live BELOW the v2 defines so load order wins).
;; Expansion state is <details> DOM state -- splice never swap; rail
;; mode flips are a sentinel :has hide exactly like desktops.
(define glochid/pane-verb
  (lambda (word route vals)
    (list word route
      (string-append "{" (list/foldl
        (lambda (acc kv)
          (string-append acc
            (if (string-eq acc "") "" ",")
            "\"" (car kv) "\":\"" (filmstrip/url-enc (car (cdr kv))) "\""))
        "" vals)
        "}")
      ;; 4th element = the GLYPH. Rows carry it explicitly (the renderer
      ;; falls back to the word when it is empty), so a host edit or a
      ;; lib table change cannot silently re-label the rail.
      (filmstrip/act-glyph word)))
  "One tree verb row spec: (word route vals-json glyph) from (key val)
pairs; values ride filmstrip/url-enc (route/path vals carry ?&). The
glyph is filmstrip/act-glyph's answer for the word (host: pin/pin
label by region, so the row and the pane head agree).")

(define glochid/windows-pane-node
  (lambda (p shelved?)
    (let*
      ((id (filmstrip/pane-id p))
        (route (filmstrip/pane-route p))
        (v-verbs
          (lambda (pairs)
            (list/map
              (lambda (pr)
                (glochid/pane-verb (car pr) (car (cdr pr))
                  (list (list "id" id))))
              pairs)))
        (live-verbs
          (v-verbs
            (list
              (list "focus" "/filmstrip/focus-id")
              (if (string-eq (filmstrip/pane-region p) "rail")
                  (list "unpin" "/filmstrip/pin")
                  (list "pin" "/filmstrip/pin"))
              (list "width" "/filmstrip/width")
              (list "single" "/filmstrip/single")
              (list "close" "/filmstrip/close"))))
        (shelved-verbs
          (v-verbs
            (list (list "restore" "/filmstrip/restore")
                  (list "destroy" "/filmstrip/destroy")))))
      (dict-set*
        "{}"
        (list
          "label" route
          "kind" "leaf"
          "node" (string-append "pane/" id)
          "open" route
          "verbs" (json-emit (if shelved? shelved-verbs live-verbs))))))
  "One pane row: label = route (open-or-focus, acme Get on a shelved
row), verbs by CURRENT state (live: focus/pin/width/close; shelved:
restore/destroy). Declared state only -- no DOM scraping.")

(define glochid/keep-nodes
  (lambda (nodes)
    (if (null? nodes)
        (list)
        (if (string-eq (car nodes) "")
            (glochid/keep-nodes (cdr nodes))
            (cons (car nodes) (glochid/keep-nodes (cdr nodes))))))
  "Drop the empty-string markers map-indexed leaves in place (the tree
renderer takes only real node dicts).")

(define glochid/windows-tree
  (lambda (path)
    (let*
      ((panes (nth (filmstrip/row) 1))
        (live-on-desk
          (lambda (d)
            (glochid/keep-nodes
              (filmstrip/map-indexed
                (lambda (p i)
                  (if
                    (if (string-eq (filmstrip/pane-state p) "shelved") #f
                        (string-eq (as-str (filmstrip/pane-desk p)) d))
                    (glochid/windows-pane-node p #f)
                    ""))
                panes 0))))
        (desk-dir
          (lambda (d)
            (dict-set*
              "{}"
              (list
                "label" (string-append "desktop " d)
                "kind" "dir"
                "node" (string-append "desk/" d)
                "open" ""
                "verbs"
                (json-emit
                  (list
                    (list "switch" "/filmstrip/desktop"
                      (string-append "{\"d\":\"" d "\"}"))))))))
        (shelf-dir
          (dict-set*
            "{}"
            (list
              "label" "shelf"
              "kind" "dir"
              "node" "shelf"
              "open" ""
              "verbs" ""))))
      (if
        (string-eq path "")
        (list (desk-dir "0") (desk-dir "1") (desk-dir "2") shelf-dir)
        (if
          (string-eq path "shelf")
          (glochid/keep-nodes
            (filmstrip/map-indexed
              (lambda (p i)
                (if (string-eq (filmstrip/pane-state p) "shelved")
                    (glochid/windows-pane-node p #t)
                    ""))
              panes 0))
          (live-on-desk (substring path 5 (string-length path)))))))
  "levelfn for the windows tree (node 'desk/<n>' -> that desktop's
panes; 'shelf' -> the shelved panes): DESKTOP-GLOBAL data from the
one workspace row, never from live DOM.")

;; ---- 2b2b. THE SHARED FILE TREE (rail + pane ARE the same rows) ----
;; rail-trees-v4 amendment: a node dict MAY carry "kids" -- a JSON
;; string of its walked child level; the lib renders eager kids INLINE
;; (no toggle-once GET) and keeps the lazy level pull for big/deep
;; dirs. The threshold is HOST policy (fs knowledge lives here, not in
;; the lib): a dir pre-walks its children only while the child level
;; holds at most glochid/tree-kids-max visible entries and the eager
;; depth is below glochid/tree-kids-depth. The defaults (12 entries, 1
;; level) are calibrated to the interpreter's 400k-step request budget:
;; the whole-root walk costs ~60k steps (vs 16k lazy) and fits beside
;; the shell render; bigger budgets can push both constants up.
;; node_modules-scale subtrees degrade to the ordinary one-GET lazy
;; level. ONE walk feeds BOTH faces: the rail code box and the /code
;; pane are the same rows.
(define glochid/tree-kids-max 12)
(define glochid/tree-kids-depth 1)

(define glochid/json-arr-tail
  (lambda (xs)
    (if (null? (cdr xs))
        (car xs)
        (string-append (car xs) "," (glochid/json-arr-tail (cdr xs)))))
  "Comma join (glochid/json-arr's inner walk).")

(define glochid/json-arr
  (lambda (xs)
    (if (null? xs)
        "[]"
        (string-append "[" (glochid/json-arr-tail xs) "]")))
  "Join already-encoded JSON object strings into one array literal.")

(define glochid/fs-node
  (lambda (name is-dir path kids-json)
    (if (string-eq kids-json "")
        (dict-set*
          "{}"
          (list
            "label" name
            "kind" (if is-dir "dir" "leaf")
            "node" path
            "open" (if is-dir "" (string-append "/code?path=" path))
            "verbs" ""))
        (dict-set*
          "{}"
          (list
            "label" name
            "kind" (if is-dir "dir" "leaf")
            "node" path
            "open" (if is-dir "" (string-append "/code?path=" path))
            "verbs" ""
            "kids" kids-json))))
  "One fs node dict (a JSON string, like every tree row): absent/empty
kids = ordinary lazy dir; a non-empty kids array string = eager dir.")

(define glochid/kids-arr
  (lambda (dir depth)
    (if (>= depth glochid/tree-kids-depth)
        ""
        (let
          ((entries (json-parse (fs/ls dir))))
          (if (> (count entries) glochid/tree-kids-max)
              ""
              (glochid/json-arr
                (glochid/fs-rows-of dir (+ depth 1) entries))))))
  "The eager threshold in one place: one readdir to COUNT the child
level, inline it only under the cap and below the depth bound; else
lazy (empty string, no kids field).")

(define glochid/fs-rows-of
  (lambda (path depth entries)
    (glochid/keep-nodes
      (filmstrip/map-indexed
        (lambda (e i)
          (let*
            ((name (as-str (dict-get e "name")))
              (is-dir (string-eq (dict-get e "dir") "true"))
              (child
                (if (string-eq path "")
                    name
                    (string-append path "/" name))))
            (if (string-starts-with? name ".")
                ""
                (glochid/fs-node
                  name
                  is-dir
                  child
                  (if is-dir (glochid/kids-arr child depth) "")))))
        entries
        0)))
  "One fs level as node-dict rows (JSON strings): the same hidden-dot
rule as the old cascade walk; dir children self-report their eager
threshold. glochid/keep-nodes drops the hidden markers in place.")

(define glochid/code-tree
  (lambda (path)
    (let ((entries (json-parse (fs/ls (if (string-eq path "") "." path)))))
      (glochid/fs-rows-of path 0 entries)))
  "levelfn for the files tree (rail code box AND /code pane face): one
node contract, one walk -- small subtrees render eagerly, big ones
fetch their level exactly once on first unfold.")

(define glochid/prim-leaf
  (lambda (pkg fn)
    (dict-set*
      "{}"
      (list
        "label" (as-str fn)
        "kind" "leaf"
        "node" (string-append "prims/" pkg)
        "open" (string-append "/bindings?pkg=" pkg "&name=" (as-str fn))
        "verbs" "")))
  "One function leaf row (a JSON string, like every tree row): label
open-or-focuses the /bindings?pkg=&name= detail pane via acme Get.")

(define glochid/prims-tree
  (lambda (path)
    (if
      (string-eq path "")
      (glochid/keep-nodes
        (filmstrip/map-indexed
          (lambda (p i)
            (dict-set*
              "{}"
              (list
                "label" (as-str p)
                "kind" "dir"
                "node" (string-append "prims/" (as-str p))
                "open" ""
                "verbs" "")))
          (bind/list-result (bind/safe bind/packages))
          0))
      (list/map
        (lambda (n)
          (glochid/prim-leaf (substring path 6 (string-length path)) n))
        (bind/list-result
          (bind/safe
            (lambda () (bind/list (substring path 6 (string-length path)))))))))
  "levelfn for the prims tree (package -> function) ON THE SHARED NODE
CONTRACT (rail box and pane face both ride it). Bridge fail-soft: a
down bridge answers an empty level -- the shell never errors on the
browser's behalf. NO eager kids here, deliberately: unlike fs/ls a
count costs one habitat BRIDGE round-trip per package, so folds pull
their level exactly once -- the template is shared, only the cache
locality differs.")

(define filmstrip/tree-roots
  (lambda ()
    (list
      (list "windows" "windows" glochid/windows-tree)
      (list "code" "code" glochid/code-tree)
      (list "prims" "prims" glochid/prims-tree)))
  "glochid's rail tree roots (windows / files / prims). Redefining the
lib's default host seam; the renderer + endpoints stay shared.")

(define glochid/railtabs
  (lambda ()
    (list/map
      (lambda (r)
        (let ((id (as-str (car r))))
          (html/tag
            "button"
            (html/attrs
              (list
                (html/attr "class" "fs-btn fs-railtab")
                (html/attr "title" (string-append "show the " id " tree"))
                (html/attr "hx-post" "/filmstrip/railmode")
                (html/attr "hx-vals" (string-append "{\"root\":\"" id "\"}"))
                (html/attr "hx-target" "#fs-sink")
                (html/attr "hx-swap" "innerHTML")))
            (html/text id))))
      (filmstrip/tree-roots)))
  "The rail mode tabs: one word per tree root (mode names are CONTENT
labels, not verbs -- they stay words, like the cascade's app names).
POST /filmstrip/railmode flips the #fs-railmode sentinel -- the SAME
:has hiding desktops use, so mode flips keep every tree's expansion
DOM alive.")

(define glochid/railtabs-html
  (lambda ()
    (html/tag
      "div"
      (html/attrs
        (list
          (html/attr "id" "fs-railtabs")
          (html/attr "class" "fs-cbox fs-railtabs")
          (html/attr "data-cbox" "railtabs")))
      (html/join
        (append
          (list (html/span (html/attr "class" "fs-title") (html/text "rail")))
          (glochid/railtabs)))))
  "The rail mode controlbox (one word per tree root, per
filmstrip.borg sidebar-main-dichotomy — the axis toggle word was
retired, BORGE-1789879934: sidebar-only shell).")

(define glochid/railmode
  (lambda ()
    (let
      ((m (as-str (dict-get (filmstrip/ws-fields) "railmode"))))
      (if (string-eq m "") "windows" m)))
  "Rail mode read (ws fields; empty = the first root, never an error).")

(define glochid/railmode-set
  (lambda ()
    (let
      ((root (as-str (request-param "root"))))
      (seq
        (filmstrip/ws-save
          (dict-set* (filmstrip/ws-fields) (list "railmode" root)))
        (render-html
          (html/join
            (list
              (filmstrip/oob-msg "")
              (glochid/railmode-sentinel root #t)))))))
  "POST /filmstrip/railmode: root param -> persist + OOB the sentinel
(class rm-<root>; CSS shows only that tree); no tree re-render --
expansion state is DOM.")

(define glochid/railmode-sentinel
  (lambda (m ?oob)
    (html/tag
      "div"
      (html/attrs
        (append
          (list
            (html/attr "id" "fs-railmode")
            (html/attr "class" (string-append "fs-sh-axis rm-" m)))
          (if ?oob
              (list (html/attr "hx-swap-oob" "outerHTML:#fs-railmode"))
              (list))))
      ""))
  "Inert rail-mode sentinel (like the axis/desk ones); CSS
:has(#fs-railmode.rm-<id>) hides every other tree box.")

(define glochid/tree-boxes
  (lambda ()
    (list/map filmstrip/tree-box (filmstrip/tree-roots)))
  "All host tree boxes (shell render: each in its own .fs-rmode cbox;
the sentinel :has shows exactly one).")

(define filmstrip/shelf-oob
  (lambda () "")
  "v4: the shelf is a windows-tree SUBTREE; the windows OOB re-renders
#fs-tree-windows once -- a second identical OOB swap of the same id in
one response would make htmx run its OOB pass twice over the same
node, so the shelf hook answers empty.")

(define filmstrip/winbox-oob
  (lambda () (filmstrip/tree-oob (car (filmstrip/tree-roots))))
  "v4: same -- the windows tree REPLACES the winbox (close/restore/
destroy/refresh call this on every mutation response).")

;; ---- 2f. HOST STAMP + fragment twins of the /new /resume redirects ----
;; open-apply stamps fresh panes through filmstrip/pane-stamp (lib hook,
;; default identity); glochid's override rides the CURRENT desktop, so an
;; app-link open on desk>0 is VISIBLE there (closes the window-controlbox-v2
;; known gap: panes used to land on desk 0, hidden mid-flight). The lib
;; must not read glochid row fields — same seam discipline as save-all.
(define filmstrip/pane-stamp
  (lambda (rec)
    (dict-set rec "desktop" (as-str (glochid/deskt))))
  "A fresh pane record with the operator's current desktop.")

;; The sessions table's resume/+new links are shell-level actions wearing
;; redirect responses (GET /new /resume answer meta-refresh). In fragment
;; mode those escapes are the plain-<a> bug class; the twins below act and
;; answer OOB swaps instead. The chat pane picks up the new/resumed
;; session on its next render (or shows it via open-or-focus).
(define glochid/new
  (lambda ()
    (seq
      (session/new)
      (render-html
        (html/join
          (list
            (filmstrip/oob-msg "glochid: new session started")
            (filmstrip/winbox-oob))))))
  "POST /filmstrip/new: rotate the transcript; fragment twin of GET /new.")

(define glochid/resume
  (lambda ()
    (let*
      ((sid (request-param "id"))
        (row (filmstrip/row))
        (cascade (nth row 0))
        (panes (nth row 1))
        (focus (nth row 2))
        (r (string-append "/chat?session=" (as-str sid)))
        (res (filmstrip/open-apply panes r))
        (panes2 (nth res 0))
        (target (nth res 1))
        (appended? (nth res 2))
        (unshelved? (nth res 3))
        (focus2 (filmstrip/rail-focus-onto panes2 focus target)))
      (seq
        (session/resume (as-str sid))
        (filmstrip/save-all cascade panes2 focus2)
        (render-html
          (html/join
            (list
              (filmstrip/oob-msg "")
              (if unshelved? (filmstrip/shelf-oob) "")
              (filmstrip/winbox-oob)
              (filmstrip/open-response panes2 focus focus2 target appended?)))))))
  "POST /filmstrip/resume: re-point the live session AND open-or-focus\nits /chat?session= pane (fragment twin of GET /resume).")

;; ---- 3. THE SHELL PAGE (GET / and GET /filmstrip) ----
;; ?session=ID resumes as a chat fragment pane (/chat?session=ID — the
;; sessions table's view link and /resume land HERE); ?open=ROUTE is the
;; GET twin of the launcher POST (open-or-focus through the same defs).
;; The shell body is glochid's own assemble of the filmstrip parts, which
;; includes the inert #fs-axis sentinel. The AXIS TOGGLE was retired
;; (BORGE-1789879934): no button, no /filmstrip/orient — sidebar only.
(define glochid/axis-sentinel
  (lambda (axis ?oob)
    (html/tag
      "div"
      (html/attrs
        (append
          (list
            (html/attr "id" "fs-axis")
            (html/attr "class" (string-append "fs-sh-axis " axis)))
          (if ?oob
              (list (html/attr "hx-swap-oob" "outerHTML:#fs-axis"))
              (list))))
      ""))
  "Inert axis sentinel (axis PINNED col — BORGE-1789879934); CSS
:has(#fs-axis.col) lays the shell out as the left-rail sidebar; no OOB
mutator remains for it.")


(define glochid/shell-requested
  (lambda ()
    (let
      ((sess (query-param "session")))
      (if
        (null? sess)
        (let
          ((o (query-param "open")))
          (if (null? o) "" (as-str o)))
        (string-append "/chat?session=" sess))))
  "Route text the shell should open-or-focus this render, or \"\".")

(define glochid/shell-row
  (lambda ()
    (let*
      ((r (string-trim (glochid/shell-requested)))
        (row (filmstrip/row)))
      (if
        (string-eq r "")
        row
        (if
          (string-starts-with? r "/")
          (if
            (null? (route/lookup (car (filmstrip/split-query r))))
            row
            (let*
              ((res (filmstrip/open-apply (nth row 1) r))
                (panes2 (nth res 0))
                (focus2 (nth res 1)))
              (filmstrip/save-all (nth row 0) panes2 focus2)
              (list (nth row 0) panes2 focus2)))
          row))))
  "Workspace row after honoring the open/session param (persisted — a
reload keeps the pane). Bad routes fall back to the plain row.")

(define glochid/shell-doc
  (lambda (frag shell)
    (if
      frag
      (render-html shell)
      (render-html
        (string-append
          "<!doctype html><html><head><meta charset=utf-8><title>glochid shell</title>"
          ;; CM ride (BORGE-1789581611) — code panes boot wherever they
          ;; end up (boot-all + observer in pp-cm.js).
          "<link rel=stylesheet href=\"/assets/codemirror.min.css\">"
          "<style>" glochid/base-css "</style></head><body>"
          shell
          "<script src=\"/assets/htmx.min.js\"></script>"
          "<script src=\"/assets/htmx-2-compat.min.js\"></script>"
          "<script src=\"/assets/hx-multipart.js\"></script>"
          "<script src=\"/assets/codemirror.min.js\"></script>"
          "<script src=\"/assets/cm/mllike.min.js\"></script>"
          "<script src=\"/assets/cm/markdown.min.js\"></script>"
          "<script src=\"/assets/cm/scheme.min.js\"></script>"
          "<script src=\"/assets/pp-cm.js\"></script>"
          "</body></html>"))))
  "The shell doc: glochid chrome + filmstrip/shell-html (which carries
its own strip css + theme props from glochid/base-css). HX-Request
renders the fragment only — the shell itself can be windowed. frag is
captured by the CALLER before pane dispatch runs: nested route/dispatch
rebinds the request context, so hx-request? must be read first.")

(define glochid/panes-region
  (lambda (panes focus region)
    (filmstrip/map-indexed
      (lambda (p i)
        (if (string-eq (filmstrip/pane-region p) region)
            (filmstrip/pane-html p (= i focus)
              (filmstrip/pane-content (filmstrip/pane-route p)))
            ""))
      panes 0))
  "Pane sections of ONE region only (v5 split): glochid renders main
panes into the #fs-content strip and rail (pinned) panes into #fs-rail
— the strip is the reading surface, the rail box the dock. Empty
strings for the other region keep map-indexed's shape (html/join skips
them).")

(define glochid/rail-box
  (lambda (panes focus)
    (html/tag
      "div"
      (html/attrs
        (list
          (html/attr "id" "fs-rail")
          (html/attr "class" "fs-rail")))
      (html/join
        (append
          (glochid/panes-region panes focus "rail")
          (list
            (glochid/railtabs-html)
            (html/join (glochid/tree-boxes)))))))
  "The rail BOX (v5): docked panes + railtabs + tree boxes in one
flex-column side panel. Row-axis CSS turns #fs-rail into
display:contents (the furniture flows into the shell column as
before); col-axis CSS makes it a real 20rem left panel.")

(define glochid/content-area
  (lambda (panes focus)
    (html/tag
      "div"
      (html/attrs
        (list
          (html/attr "id" "fs-surface")
          (html/attr "class" "fs-surface")))
      (html/join
        (list
          (html/tag
            "div"
            (html/attrs
              (list
                (html/attr "id" "fs-content")
                (html/attr "class" "fs-content")))
            (html/join (glochid/panes-region panes focus "main")))
          (glochid/rail-box panes focus)))))
  "The surface area (v5): the main strip FIRST (plain strip on wide
screens, the niri row on the col axis's right), then the rail box.
Wrapping both under #fs-surface gives the region-move OOB ONE swap
target (filmstrip/oob-content-strip's glochid override re-renders
this wrapper; every pane body is an hx-preserve placeholder, so live
DOM survives the move between strip and rail).")

;; v5 region-move OOB override: the lib's default re-renders
;; #fs-content with ALL panes; glochid's split inventory needs the
;; wrapper (strip + rail box) so a pane MOVES containers on pin/unpin.
;; Load order wins: this redefinition lands after libs/filmstrip.
(define filmstrip/oob-content-strip
  (lambda (panes focus)
    (html/tag
      "div"
      (html/attrs
        (list
          (html/attr "id" "fs-surface")
          (html/attr "class" "fs-surface")
          (html/attr "hx-swap-oob" "true")))
      (html/join
        (list
          (html/tag
            "div"
            (html/attrs
              (list
                (html/attr "id" "fs-content")
                (html/attr "class" "fs-content")))
            (html/join (glochid/panes-region panes focus "main")))
          (glochid/rail-box panes focus)))))
  "OOB re-render of glochid's whole surface area (both containers, pane
bodies preserved). The pin/move responses land here.")

(define glochid/shell-inner
  (lambda (cascade panes focus)
    (html/join
      (list
        (html/tag "style" "" (css/render (filmstrip/css)))
        (html/tag
          "div"
          (html/attrs
            (list
              (html/attr "class" "fs-shell")
              (html/attr "id" "fs-shell")))
          (html/join
            (list
              (filmstrip/cascade-html cascade)
              (glochid/content-area panes focus)
              (glochid/railmode-sentinel (glochid/railmode) #f)
              (filmstrip/key-sentinels)
              (html/tag
                "div"
                (html/attrs
                  (list
                    (html/attr "id" "fs-sink")
                    (html/attr "class" "fs-sink")))
                "")
              ;; inert anchor for the lib's OOB status line: a shell with
              ;; no launcher still has somewhere for oob-msg to land, so
              ;; htmx never warns on a missing OOB target.
              (html/span
                (html/attrs
                  (list
                    (html/attr "id" "fs-launch-msg")
                    (html/attr "class" "fs-msg")
                    (html/attr "hidden" "hidden")))
                "")
              (glochid/axis-sentinel (glochid/axis) #f)
              (glochid/desk-sentinel (glochid/deskt) #f)))))))
  "The shell inner (filmstrip/shell-html's parts minus the launcher and
the desktop controlbox, plus rail tabs + rail trees + sentinels —
filmstrip.borg sidebar-main-dichotomy, v5-ized: content-area wrapper
holds the main strip + rail box): style + cascade over [strip | rail],
key sentinels, OOB sink, status-line anchor, axis and desktop
sentinels. Zero scripts (CSS :has flips the layout from the axis
sentinel's class and hides off-desktop panes from the desk one).")

(define glochid/shell
  (lambda ()
    (let*
      ((frag (hx-request?))
        (row (glochid/shell-row))
        ;; rail discipline: a main pane ALWAYS exists while live panes
        ;; do. Heal an unset focus (or one sitting on a shelved pane) to
        ;; the first live pane at RENDER (not persisted — alt+right
        ;; heals it for real on the next focus step).
        (focus (glochid/heal-focus (nth row 1) (nth row 2))))
      (glochid/shell-doc
        frag
        (glochid/shell-inner (nth row 0) (nth row 1) focus))))
  "GET / and /filmstrip: the glochid shell. filmstrip's parts under
glochid chrome; zero JS beyond the vendored htmx assets. Focus heals
to the first visible pane at render when unset — v5: the strip shows
every live main pane of the desktop, so the heal picks the ring only
(the panes are visible regardless).")

;; ---- shell routes ----
(route "/" "glochid/shell")
(route "/filmstrip" "glochid/shell")
(route "/filmstrip/open" "filmstrip/open")
(route "/filmstrip/cascade" "filmstrip/cascade")
(route "/filmstrip/close" "filmstrip/close")
(route "/filmstrip/restore" "filmstrip/restore")
(route "/filmstrip/destroy" "filmstrip/destroy")
(route "/filmstrip/focus" "filmstrip/focus")
(route "/filmstrip/focus-id" "filmstrip/focus-id")
(route "/filmstrip/move" "filmstrip/move")
(route "/filmstrip/width" "filmstrip/width")
(route "/filmstrip/pin" "filmstrip/pin")
(route "/filmstrip/single" "filmstrip/single")
(route "/filmstrip/tree" "filmstrip/tree")
(route "/filmstrip/tree/collapse" "filmstrip/tree-collapse")
(route "/filmstrip/railmode" "glochid/railmode-set")
(route "/filmstrip/desktop" "glochid/desktop-set")
(route "/filmstrip/new" "glochid/new")
(route "/filmstrip/resume" "glochid/resume")
|pp}

(** Boot-load the embedded Nopales packages + this surface into the
    shared env, registering the route-table routes. Idempotent-ish:
    re-evaluating defines redefines bindings live. *)
(** Inert defaults for the llm-config host prims (agent/models,
    agent/model, agent/base-url, agent/key-status) so the surface
    renders in non-serve contexts (tests, -p pipe evals).
    [Http_ui.serve] RE-REGISTERS them with live cfg backing — prim
    registration is last-write-wins on the same name. *)
let register_config_default_prims ~(rt : Runtime.t) : unit =
  Runtime.add_prim rt "agent/models" (fun _ -> Ast.VList []);
  Runtime.add_prim rt "agent/model" (fun _ -> Ast.VStr "");
  Runtime.add_prim rt "agent/base-url" (fun _ -> Ast.VStr "");
  Runtime.add_prim rt "agent/key-status" (fun _ -> Ast.VStr "not set");
  (* inert session prims (sessions-persist) so the pages render in
     non-serve contexts; Http_ui.serve / the REPL re-register with live
     store backing — prim registration is last-write-wins. *)
  Runtime.add_prim rt "session/name" (fun _ -> Ast.VStr "");
  Runtime.add_prim rt "session/list" (fun _ -> Ast.VStr "[]");
  Runtime.add_prim rt "session/data" (fun _ -> Ast.VStr "[]");
  Runtime.add_prim rt "session/new" (fun _ -> Ast.VStr "");
  Runtime.add_prim rt "session/resume" (fun _ -> Ast.VStr "")

let load ~(rt : Runtime.t) : unit =
  register_config_default_prims ~rt;
  (* Boot order (glochid_source header): libs, then the glochid layer
     (whose ws/browse/shell redefinitions must win over the filmstrip
     defaults via env late binding), then the agent surface. *)
  Runtime.load_source ~rt Libs_source.html_lib;
  Runtime.load_source ~rt Libs_source.css_lib;
  Runtime.load_source ~rt Libs_source.hyper_lib;
  Runtime.load_source ~rt Libs_source.filmstrip_lib;
  Runtime.load_source ~rt glochid_source;
  Runtime.load_source ~rt surface_source
