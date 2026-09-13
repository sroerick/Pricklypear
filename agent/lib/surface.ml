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
    chat/llm-bubble-open, chat/token, code/page, code/rows and registers
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
    "pre{background:#12141c;padding:1rem;border-radius:6px;overflow:auto}"
    "input{background:#1b1f2a;color:#d8dee9;border:1px solid #333;border-radius:6px;padding:.5rem}"
    "select{background:#1b1f2a;color:#d8dee9;border:1px solid #333;border-radius:4px}"
    "label{color:#8a8f9e}"
    ".card{background:#1b1f2a;border:1px solid #2a2e3a;border-radius:6px;padding:1rem;max-width:38rem}"
    ".card p{margin:.5rem 0}"
    ".card input,.card select{width:100%;padding:.5rem;margin-top:.25rem}"
    "button{background:#2a6fdb;color:#fff;border:0;border-radius:6px;padding:.5rem 1rem;cursor:pointer}"
    ".fs-shell{height:calc(100vh - 2rem)}"
    ".fs-topline{display:flex;gap:.75rem;align-items:center;flex:0 0 auto;margin:0}"
    ".fs-sh-axis{display:none}"
    ;; left-rail axis: the WHOLE SHELL collapses to a screen-edge toolbar
    ;; column (~20rem). Cascade stacks its Miller columns as a vertical
    ;; app menu; panes stack BELOW it as the vertical filmstrip (the same
    ;; flex-basis width presets re-read as heights); the desktop stays
    ;; free to the right. Same three override toggles, no JS.
    ".fs-shell:has(#fs-axis.col){width:20rem;max-width:20rem;height:calc(100vh - 2rem);min-height:0}"
    ".fs-shell:has(#fs-axis.col) .fs-cascade{flex:0 0 auto;flex-direction:column;max-height:16rem;min-height:0;overflow-y:auto;overflow-x:hidden}"
    ".fs-shell:has(#fs-axis.col) .fs-launch,.fs-shell:has(#fs-axis.col) .fs-topline{flex-wrap:wrap}"
    ".fs-shell:has(#fs-axis.col) .fs-launch input[type=text]{width:100%}"
    ".fs-shell:has(#fs-axis.col) .fs-content{flex:1 1 auto;min-height:0;flex-direction:column;overflow-y:auto;overflow-x:hidden}"
    ;; rail rows are pane HEADS only — bodies belong to the focused pane
    ;; (lifted into the main window below). No presets-as-heights in the
    ;; rail; a row collapses to its title bar.
    ".fs-shell:has(#fs-axis.col) .fs-pane{flex:0 0 auto;min-width:0;width:100%;min-height:0}"
    ".fs-shell:has(#fs-axis.col) .fs-pane:not(.focused) .fs-body{display:none}"
    ;; the rail is the strip of panes; the FOCUSED pane is lifted out of it
    ;; into the main window (position:fixed fills the space right of the
    ;; rail). Open-or-focus a pane and it jumps to the main window; the
    ;; focus ring drops (the big window IS the focus signal).
    ".fs-shell:has(#fs-axis.col) .fs-pane.focused{position:fixed;left:22rem;top:1rem;right:1rem;bottom:1rem;width:auto;min-width:0;min-height:0;z-index:1000;outline:none}"
    ;; chatpage sizes against the VIEWPORT (calc(100vh - 6rem)); that always
    ;; overflows a pane box. In rail mode fit it to the pane instead.
    ".fs-shell:has(#fs-axis.col) .chatpage{height:100%}"
    ;; v2 shelf: the rail's shelved-pane controlbox (rows: [route] restore destroy);
    ;; hidden panes are .fs-pane.shelved (display:none, lib css).
    ".fs-shelf{display:flex;flex-direction:column;gap:.15rem;border:1px solid var(--border);background:var(--panel);padding:.3rem .35rem}"
    ".fs-shelf-row{display:flex;gap:.3rem;align-items:center;min-height:1.4rem}"
    ;; v2 windows rail: the per-live-pane controlbox (rows: [route] focus
    ;; width close); same chrome as the shelf box.
    ".fs-winbox{display:flex;flex-direction:column;gap:.15rem;border:1px solid var(--border);background:var(--panel);padding:.3rem .35rem}"
    ".fs-win-row{display:flex;gap:.3rem;align-items:center;min-height:1.4rem}"
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
    ".fs-shell:has(#ws-desk.dsk-0) .fs-pane[data-desktop]:not([data-desktop=\"0\"]){display:none}"
    ".fs-shell:has(#ws-desk.dsk-1) .fs-pane[data-desktop]:not([data-desktop=\"1\"]){display:none}"
    ".fs-shell:has(#ws-desk.dsk-2) .fs-pane[data-desktop]:not([data-desktop=\"2\"]){display:none}"
    ".fs-shell:has(#ws-desk.dsk-3) .fs-pane[data-desktop]:not([data-desktop=\"3\"]){display:none}"
    ".fs-shell:has(#ws-desk.dsk-4) .fs-pane[data-desktop]:not([data-desktop=\"4\"]){display:none}"
    ".fs-shell:has(#ws-desk.dsk-5) .fs-pane[data-desktop]:not([data-desktop=\"5\"]){display:none}"
    ".fs-shell:has(#ws-desk.dsk-6) .fs-pane[data-desktop]:not([data-desktop=\"6\"]){display:none}"
    ".fs-shell:has(#ws-desk.dsk-7) .fs-pane[data-desktop]:not([data-desktop=\"7\"]){display:none}"
    ".fs-shell:has(#ws-desk.dsk-8) .fs-pane[data-desktop]:not([data-desktop=\"8\"]){display:none}"
    ".fs-shell:has(#ws-desk.dsk-9) .fs-pane[data-desktop]:not([data-desktop=\"9\"]){display:none}"
    ;; desktop controlbox chrome (same shelf look)
    ".fs-deskbox{display:flex;gap:.3rem;align-items:center;border:1px solid var(--border);background:var(--panel);padding:.3rem .35rem}"
    ;; v2 app-links: a button styled as a bare link (chat/sessions/settings
    ;; nav in pane chrome; the anchor version navigated the whole window
    ;; out of the shell with a full GET). Overrides the generic button look.
    ".fs-applink{background:none;border:0;color:#7aa2f7;padding:0;margin:0;font:inherit;cursor:pointer;text-decoration:none}"
    ".fs-applink:hover{text-decoration:underline}"))

(define (glochid/layout title body)
  (if (hx-request?)
      (render-html body)
      (render-html
        (string-append
          "<!doctype html><html><head><meta charset=utf-8><title>" title "</title>"
          "<style>" glochid/base-css "</style></head><body>"
          body
          "<script src=\"/assets/htmx.min.js\"></script>"
          "<script src=\"/assets/htmx-2-compat.min.js\"></script>"
          "<script src=\"/assets/hx-multipart.js\"></script>"
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
    (filmstrip/app-link "/sessions" "/sessions")
    " · "
    (filmstrip/app-link "/settings" "/settings")
    " · model: "
    "<select name=\"model\" hx-post=\"/settings\" hx-trigger=\"change\" hx-swap=\"none\" "
    "style=\"font-size:.6em\">"
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
      "<table><tr><th>time</th><th>model</th><th>msgs</th><th>first prompt</th><th></th></tr>"
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
;; Read-only file tree + file view, rendered by Nopales via fs/ls +
;; fs/read + fs/dir? (agent host prims). query-param "path" selects the
;; entry (relative to the agent's cwd); the agent's fs prims refuse ".."
;; escapes (sandbox posture).
;; PANE-LOCAL NAV (filmstrip nav discipline): in fragment mode (pane
;; body) the rows/issues carry hx-get targeting closest .fs-body, so a
;; dir walk happens INSIDE the pane instead of navigating the whole
;; window out of the shell (the app-link twin would open-or-focus a NEW
;; pane per path — wrong for a tree walk). The standalone page keeps
;; plain anchors.
(define (code/link child label)
  (if (hx-request?)
      (string-append "<a href=\"/code?path=" (html-escape child)
        "\" hx-get=\"/code?path=" (html-escape child)
        "\" hx-target=\"closest .fs-body\" hx-swap=\"innerHTML\">"
        (html-escape label) "</a>")
      (string-append "<a href=\"/code?path=" (html-escape child)
        "\">" (html-escape label) "</a>")))

(define (code/head)
  (if (hx-request?)
      "<h1><span class=\"muted\">/code</span></h1>"
      "<h1><a href=\"/\">agent</a> <span class=\"muted\">/</span> <a href=\"/code\">code</a></h1>"))

(define (code/page)
  (let* ((p (query-param "path"))
         (rel (if (null? p) "" p)))
    (if (fs/dir? rel)
        (let* ((entries (fs/ls rel))
               (items (json-parse entries)))
          (glochid/layout "glochid — code"
            (string-append
              (code/head)
              "<p class=\"muted\">" (html-escape rel) "</p>"
              "<table><tr><th></th><th>name</th></tr>"
              (code/rows items rel)
              "</table>")))
        (let* ((content (fs/read rel)))
          (glochid/layout "glochid — code"
            (string-append
              (code/head)
              "<p class=\"muted\">" (html-escape rel) "</p>"
              "<pre>" (html-escape content) "</pre>"))))))

(define (code/rows items rel)
  (list/foldl
    (lambda (acc entry)
      (let* ((name (dict-get entry "name"))
             (dir? (string-eq (dict-get entry "dir") "true"))
             (kind (if dir? "dir" "file"))
             (child (if (string-eq rel "") name (string-append rel "/" name))))
        (string-append acc
          "<tr><td>" kind
          "</td><td>" (code/link child name)
          "</td></tr>")))
    "" items))

(route "/code" "code/page")
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

;; ---- 2b. AXIS TOGGLE (row = horizontal film, col = left rail) ----
;; The lib's save-all writes only cascade/content/focus; the glochid
;; version carries the axis field through (the pane width presets stay
;; the same three values — rail mode just re-reads them as heights
;; via the CSS flex-direction flip: .fs-shell:has(#fs-axis.col)).
(define glochid/axis
  (lambda ()
    (let
      ((v (dict-get (filmstrip/ws-fields) "axis")))
      (if (null? v) "row" (as-str v))))
  "Persisted shell axis from the ws row; \"row\" when unset.")

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
      (list "code" "code")
      (list "sessions" "/sessions")
      (list "settings" "/settings")))
  "Column 0: the agent's own surfaces. Entries are (label . target);
a target starting / is a route (leaf), a bare name is a crumb that
names a deeper cascade — only \"code\" is a cascade here: dir -> dir
-> file over the operator's cwd (fs/ls).")

;; The lib's filmstrip/apps is redefined TOO (not just browse): the app
;; list IS the cascade's column 0 AND the launcher's hint source
;; (filmstrip/launcher-hint) — if it stayed habitat vocabulary, the
;; launcher advertised /todos on a host with no /todos route.
(define filmstrip/apps
  (lambda () (glochid/apps))
  "glochid's shell app list (HOST SEAM 2): chat / code / sessions /
settings; code is the filesystem cascade crumb.")

(define glochid/dir-of
  (lambda (prefix)
    (if (string-eq prefix "code")
        "."
        (substring prefix 5 (string-length prefix))))
  "Cascade path element -> fs dir (\"code\" is the root, \"code/lib\" ->
\"lib\").")

(define glochid/ls-entries
  (lambda (prefix items)
    (if
      (null? items)
      (list)
      (let*
        ((e (car items))
          (name (as-str (dict-get e "name")))
          (dir (glochid/dir-of prefix))
          (child (if (string-eq dir ".") name (string-append dir "/" name))))
        (append
          (if
            (string-starts-with? name ".")
            (list)
            (if
              (string-eq (dict-get e "dir") "true")
              (list (list name (string-append "code/" child)))
              (list (list name (string-append "/code?path=" child)))))
          (glochid/ls-entries prefix (cdr items))))))
  "One fs/ls listing as cascade entries: dirs are crumbs (code/<child>),
files are leaves (routes /code?path=<child>); dot entries hidden.")

(define glochid/code-entries
  (lambda (prefix)
    (glochid/ls-entries prefix (json-parse (fs/ls (glochid/dir-of prefix)))))
  "childfn for the code cascade: list one directory.")

(define glochid/pkg-browse
  (lambda (pkg pkg-path)
    (if
      (string-eq pkg "code")
      (filmstrip/browse-walk
        glochid/code-entries
        (list (glochid/code-entries "code"))
        pkg-path)
      (cons (list (list)) #f)))
  "Dispatch the package browse; code walks the filesystem, unknown
packages get one empty column.")

(define filmstrip/browse
  (lambda
    (path)
    (let
      ((apps (glochid/apps)))
      (if
        (null? path)
        (cons (list apps) #f)
        (let
          ((r
            (glochid/pkg-browse (as-str (car path)) (cdr path))))
          (cons (cons apps (car r)) (cdr r))))))
  "glochid's shell cascade (HOST SEAM 2): column 0 is the agent's own
surfaces; deeper columns come from glochid/pkg-browse.")

;; ---- 2c. SHELF RAIL (v2: close defaults to shelf) ----
;; The rail gains a shelf controlbox (#fs-shelf): one row per SHELVED
;; pane -- [route] restore destroy. Restore POSTs /filmstrip/restore
;; (state=live); destroy POSTs /filmstrip/destroy (record removal).
;; The close/restore/destroy handlers answer a shelf OOB re-render via
;; filmstrip/shelf-oob (default "" in the lib; redefined here).
(define glochid/shelf-row
  (lambda (p)
    (let
      ((id (filmstrip/pane-id p))
        (route (filmstrip/pane-route p)))
      (html/tag
        "div"
        (html/attr "class" "fs-shelf-row")
        (html/join
          (list
            (html/span (html/attr "class" "fs-title") (html/text route))
            (html/tag
              "button"
              (html/attrs
                (list
                  (html/attr "class" "fs-btn")
                  (html/attr "title" "restore to its position")
                  (html/attr "hx-post" "/filmstrip/restore")
                  (html/attr "hx-vals" (string-append "{\"id\":\"" id "\"}"))
                  (html/attr "hx-target" "#fs-sink")
                  (html/attr "hx-swap" "innerHTML")))
              "restore")
            (html/tag
              "button"
              (html/attrs
                (list
                  (html/attr "class" "fs-btn")
                  (html/attr "title" "destroy the pane record")
                  (html/attr "hx-post" "/filmstrip/destroy")
                  (html/attr "hx-vals" (string-append "{\"id\":\"" id "\"}"))
                  (html/attr "hx-target" "#fs-sink")
                  (html/attr "hx-swap" "innerHTML")))
              "destroy"))))))
  "One shelf controlbox row: route label + restore + destroy.")
(define glochid/shelf-rows
  (lambda (panes)
    (filmstrip/map-indexed
      (lambda (p i)
        (if (string-eq (filmstrip/pane-state p) "shelved")
            (glochid/shelf-row p)
            ""))
      panes 0))
  "One row per shelved pane (live panes render nothing here).")
(define glochid/shelf-body
  (lambda (panes)
    (html/join
      (append
        (list (html/span (html/attr "class" "fs-title") (html/text "shelf")))
        (glochid/shelf-rows panes))))
  "Shell label + one row per shelved pane.")
(define glochid/shelf-html
  (lambda (panes)
    (html/tag
      "div"
      (html/attrs
        (list
          (html/attr "id" "fs-shelf")
          (html/attr "class" "fs-cbox fs-shelf")
          (html/attr "data-cbox" "shelf")))
      (glochid/shelf-body panes)))
  "The shelf controlbox in the rail (below the content strip).")
(define filmstrip/shelf-oob
  (lambda ()
    (html/tag
      "div"
      (html/attrs
        (list
          (html/attr "id" "fs-shelf")
          (html/attr "class" "fs-cbox fs-shelf")
          (html/attr "data-cbox" "shelf")
          (html/attr "hx-swap-oob" "outerHTML:#fs-shelf")))
      (glochid/shelf-body (nth (filmstrip/row) 1))))
  "OOB re-render of #fs-shelf (the lib's default is empty; glochid owns
the shelf rail). Close/restore/destroy call it in every response.")

;; ---- 2d. DESKTOPS (v2) ----
;; The ws row carries "desktop":N (0 default) and each pane record a
;; "desktop" field. An inert #ws-desk sentinel beside #fs-axis carries
;; the current desktop to CSS (:has hides the other desktops' panes —
;; ten pairs in glochid/base-css, no JS); d0/d1/d2 buttons in the rail
;; POST the switch. Focus heals onto the first live pane of the target
;; desktop when the focused pane lives elsewhere (a main pane ALWAYS
;; exists).
(define glochid/deskt
  (lambda ()
    (let
      ((v (dict-get (filmstrip/ws-fields) "desktop")))
      (if (null? v) 0 (parse-int (as-str v)))))
  "Current shell desktop from the ws row (0 when unset).")

(define glochid/first-on-desk
  (lambda (panes d i)
    (if
      (null? panes)
      -1
      (if
        (and (= (filmstrip/pane-desk (car panes)) d)
             (filmstrip/pane-live? (car panes)))
        i
        (glochid/first-on-desk (cdr panes) d (+ i 1)))))
  "Index of the first LIVE pane on desktop d, else -1.")

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

(define glochid/desk-btn
  (lambda (d)
    (html/tag
      "button"
      (html/attrs
        (list
          (html/attr "class" "fs-btn")
          (html/attr "title" (string-append "switch to desktop " d))
          (html/attr "hx-post" "/filmstrip/desktop")
          (html/attr "hx-vals" (string-append "{\"d\":\"" d "\"}"))
          (html/attr "hx-target" "#fs-sink")
          (html/attr "hx-swap" "innerHTML")))
      (html/text (string-append "d" d))))
  "One desktop switcher button: POSTs /filmstrip/desktop {d:N}.")

(define glochid/deskbox-html
  (lambda ()
    (html/tag
      "div"
      (html/attrs
        (list
          (html/attr "id" "fs-deskbox")
          (html/attr "class" "fs-cbox fs-deskbox")
          (html/attr "data-cbox" "desktop")))
      (html/join
        (append
          (list (html/span (html/attr "class" "fs-title") (html/text "desktop")))
          (list/map glochid/desk-btn (list "0" "1" "2"))))))
  "The desktop controlbox in the rail: [d0] [d1] [d2].")

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

;; ---- 2e. WINDOWS rail (v2) ----
;; The rail gains a windows controlbox (#fs-winbox): one row per LIVE
;; pane of the CURRENT desktop — [route] focus width close. focus POSTs
;; /filmstrip/focus-id (lib, above the deskbox work: the OOB two-section
;; class swap); width/close ride the pane-head verbs (/filmstrip/width,
;; /filmstrip/close). close/restore/destroy answer a winbox OOB
;; re-render via filmstrip/winbox-oob (default "" in the lib;
;; redefined here), so a pane shelved from ANY surface drops its row.
(define glochid/win-btn
  (lambda (id verb title route)
    (html/tag
      "button"
      (html/attrs
        (list
          (html/attr "class" "fs-btn")
          (html/attr "title" title)
          (html/attr "hx-post" route)
          (html/attr "hx-vals" (string-append "{\"id\":\"" id "\"}"))
          (html/attr "hx-target" "#fs-sink")
          (html/attr "hx-swap" "innerHTML")))
      verb))
  "One windows-row button: POSTs {id} at the verb's route.")

(define glochid/win-row
  (lambda (p)
    (let
      ((id (filmstrip/pane-id p))
        (route (filmstrip/pane-route p)))
      (html/tag
        "div"
        (html/attr "class" "fs-win-row")
        (html/join
          (list
            (html/span (html/attr "class" "fs-title") (html/text route))
            (glochid/win-btn id "focus" "lift into the main window" "/filmstrip/focus-id")
            (glochid/win-btn id "width" "cycle width (third/half/full)" "/filmstrip/width")
            (glochid/win-btn id "close" "close pane (to the shelf)" "/filmstrip/close"))))))
  "One windows controlbox row: route label + focus + width + close.")

(define glochid/win-rows
  (lambda (panes)
    (filmstrip/map-indexed
      (lambda (p i)
        (if
          (and (= (filmstrip/pane-desk p) (glochid/deskt))
               (filmstrip/pane-live? p))
          (glochid/win-row p)
          ""))
      panes 0))
  "One row per LIVE pane of the CURRENT desktop (others render nothing).")

(define glochid/winbox-body
  (lambda (panes)
    (html/join
      (append
        (list (html/span (html/attr "class" "fs-title") (html/text "windows")))
        (glochid/win-rows panes))))
  "Windows label + one row per live pane of the current desktop.")

(define glochid/winbox-html
  (lambda (panes)
    (html/tag
      "div"
      (html/attrs
        (list
          (html/attr "id" "fs-winbox")
          (html/attr "class" "fs-cbox fs-winbox")
          (html/attr "data-cbox" "windows")))
      (glochid/winbox-body panes)))
  "The windows controlbox in the rail (between content strip and deskbox).")

(define filmstrip/winbox-oob
  (lambda ()
    (html/tag
      "div"
      (html/attrs
        (list
          (html/attr "id" "fs-winbox")
          (html/attr "class" "fs-cbox fs-winbox")
          (html/attr "data-cbox" "windows")
          (html/attr "hx-swap-oob" "outerHTML:#fs-winbox")))
      (glochid/winbox-body (nth (filmstrip/row) 1))))
  "OOB re-render of #fs-winbox (the lib's default is empty; glochid owns
the windows rail). Close/restore/destroy call it in every response.")

;; focus healing at render: an unset focus OR one pointing at a shelved
;; pane heals to the first LIVE pane ON THE CURRENT DESKTOP (v2 task 3;
;; falls back to the first live pane anywhere when the desktop has
;; none).
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
  "Focus that always has a live main pane behind it: unset (-1) and
shelved-pointing both heal to the first live pane on the CURRENT
desktop (-1 when the desktop has none — falls back to the first live
pane anywhere). Not persisted — alt+right heals it for real on the
next focus step.")

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
        (focus2 (nth res 1))
        (appended? (nth res 2))
        (unshelved? (nth res 3)))
      (seq
        (session/resume (as-str sid))
        (filmstrip/save-all cascade panes2 focus2)
        (render-html
          (html/join
            (list
              (filmstrip/oob-msg "")
              (if unshelved? (filmstrip/shelf-oob) "")
              (filmstrip/winbox-oob)
              (filmstrip/open-response panes2 focus focus2 appended?)))))))
  "POST /filmstrip/resume: re-point the live session AND open-or-focus\nits /chat?session= pane (fragment twin of GET /resume).")

;; ---- 3. THE SHELL PAGE (GET / and GET /filmstrip) ----
;; ?session=ID resumes as a chat fragment pane (/chat?session=ID — the
;; sessions table's view link and /resume land HERE); ?open=ROUTE is the
;; GET twin of the launcher POST (open-or-focus through the same defs).
;; The shell body is glochid's own assemble of the filmstrip parts
;; (instead of filmstrip/shell-html) so the AXIS toggle rides along: an
;; inert #fs-axis sentinel + a toggle button beside the launcher.
(define glochid/axis-btn
  (lambda (axis)
    (html/tag
      "button"
      (html/attrs
        (list
          (html/attr "id" "fs-axis-btn")
          (html/attr "class" "fs-btn")
          (html/attr "title" "toggle horizontal / vertical pane strip (persisted)")
          (html/attr "hx-post" "/filmstrip/orient")
          (html/attr "hx-target" "#fs-sink")
          (html/attr "hx-swap" "innerHTML")))
      (html/text (if (string-eq axis "col") "⇆ strip" "⇅ stack"))))
  "Axis toggle beside the launcher: stack = vertical full-width panes,
strip = the horizontal film row. OOB-swappable (the id carries it).")

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
  "Inert axis sentinel; CSS :has(#fs-axis.col) flips the shell to the
left-rail strip layout. ?oob true renders it as an OOB outerHTML swap
(the orient response).")

(define glochid/orient
  (lambda ()
    (let*
      ((next (if (string-eq (glochid/axis) "col") "row" "col")))
      (seq
        (filmstrip/ws-save
          (dict-set* (filmstrip/ws-fields) (list "axis" next)))
        (render-html
          (html/join
            (list
              (filmstrip/oob-msg "")
              (glochid/axis-sentinel next #t)
              (html/tag
                "button"
                (html/attrs
                  (list
                    (html/attr "id" "fs-axis-btn")
                    (html/attr "class" "fs-btn")
                    (html/attr "title" "toggle horizontal / vertical pane strip (persisted)")
                    (html/attr "hx-post" "/filmstrip/orient")
                    (html/attr "hx-target" "#fs-sink")
                    (html/attr "hx-swap" "innerHTML")
                    (html/attr "hx-swap-oob" "outerHTML:#fs-axis-btn")))
                (html/text (if (string-eq next "col") "⇆ strip" "⇅ stack")))))))))
  "POST /filmstrip/orient: flip + persist the shell axis; two OOB swaps
(sentinel + button) keep every live pane DOM untouched — the layout
flip is CSS-only on the client.")

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
          "<style>" glochid/base-css "</style></head><body>"
          shell
          "<script src=\"/assets/htmx.min.js\"></script>"
          "<script src=\"/assets/htmx-2-compat.min.js\"></script>"
          "<script src=\"/assets/hx-multipart.js\"></script>"
          "</body></html>"))))
  "The shell doc: glochid chrome + filmstrip/shell-html (which carries
its own strip css + theme props from glochid/base-css). HX-Request
renders the fragment only — the shell itself can be windowed. frag is
captured by the CALLER before pane dispatch runs: nested route/dispatch
rebinds the request context, so hx-request? must be read first.")

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
              (html/tag
                "div"
                (html/attr "class" "fs-topline")
                (html/join
                  (list
                    (filmstrip/launcher-html)
                    (glochid/axis-btn (glochid/axis)))))
              (html/tag
                "div"
                (html/attrs
                  (list
                    (html/attr "id" "fs-content")
                    (html/attr "class" "fs-content")))
                (html/join (filmstrip/panes-html panes focus)))
              (glochid/winbox-html panes)
              (glochid/deskbox-html)
              (glochid/shelf-html panes)
              (filmstrip/key-sentinels)
              (html/tag
                "div"
                (html/attrs
                  (list
                    (html/attr "id" "fs-sink")
                    (html/attr "class" "fs-sink")))
                "")
              (glochid/axis-sentinel (glochid/axis) #f)
              (glochid/desk-sentinel (glochid/deskt) #f)))))))
  "The shell inner (filmstrip/shell-html's parts + the axis toggle + the
windows desktop controlboxes + sentinels): style + cascade strip over
launcher+toggle + content strip, winbox, deskbox, key sentinels, OOB
sink, axis desktop sentinels. Zero scripts (CSS :has flips the layout
from the axis sentinel's class and hides off-desktop panes from the
desk one).")

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
to pane 0 at render when unset — rail mode always has a main pane.")

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
(route "/filmstrip/orient" "glochid/orient")
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
