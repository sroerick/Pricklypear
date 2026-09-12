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
    "::-webkit-scrollbar-corner{background:transparent}"))

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
(define (chat/body)
  "Chat surface body — one structure for page AND pane, so the stream's
#log target works in both (borge/agent.borg filmstrip-shell #4)."
  (let ((sid (query-param "session")))
    (string-append
      "<div class=\"chatpage\">"
      "<h1><a href=\"/\">agent</a> <span class=\"muted\">— <a href=\"/code\">/code</a> · <a href=\"/sessions\">/sessions</a> · <a href=\"/settings\">/settings</a> · model: "
      "<select name=\"model\" hx-post=\"/settings\" hx-trigger=\"change\" hx-swap=\"none\" "
      "style=\"font-size:.6em\">"
      (settings/model-options (agent/model) (agent/models))
      "</select></span></h1>"
      "<div id=\"log\">"
      (if (null? sid)
          "<div class=\"muted\">agent ready.</div>"
          (string-append "<div class=\"muted\">resumed session " (html-escape sid) " · <a href=\"/new\">+ new</a> · <a href=\"/sessions\">/sessions</a></div>" (chat/bubbles (session/data sid))))
      "</div>"
      "<form id=\"f\" hx-post=\"/chat\" hx-target=\"#log\" hx-swap=\"beforeend\" hx-ext=\"hx-multipart\""
      " hx-config='{\"timeout\":0}' hx-on::before:request=\"this.reset()\">"
      "<input id=\"msg\" name=\"msg\" type=\"text\" placeholder=\"prompt…\" autocomplete=\"off\" autofocus>"
      "<button>send</button>"
      "</form></div>")))

(define (chat/page)
  (glochid/layout "agent" (chat/body)))
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
        (string-append acc
          "<tr><td class=\"muted\">" (html-escape utc)
          "</td><td>" (html-escape model)
          "</td><td>" (html-escape count)
          "</td><td class=\"muted\">" (html-escape preview)
          "</td><td><a href=\"/resume?id=" (html-escape id) "\">resume</a>"
          " \183 <a href=\"/?session=" (html-escape id) "\">view</a></td></tr>")))
    "" (json-parse data)))

(define (sessions/page)
  (glochid/layout "agent sessions"
    (string-append
      "<h1><a href=\"/\">agent</a> <span class=\"muted\">— /sessions</span></h1>"
      "<p><a href=\"/new\">+ new session</a></p>"
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
  (glochid/layout "agent settings"
    (string-append
      "<h1><a href=\"/\">agent</a> <span class=\"muted\">— /settings</span></h1>"
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
(define (code/page)
  (let* ((p (query-param "path"))
         (rel (if (null? p) "" p)))
    (if (fs/dir? rel)
        (let* ((entries (fs/ls rel))
               (items (json-parse entries)))
          (glochid/layout "agent code"
            (string-append
              "<h1><a href=\"/\">agent</a> <span class=\"muted\">/</span> <a href=\"/code\">code</a></h1>"
              "<p class=\"muted\">" (html-escape rel) "</p>"
              "<table><tr><th></th><th>name</th></tr>"
              (code/rows items rel)
              "</table>")))
        (let* ((content (fs/read rel)))
          (glochid/layout "agent code"
            (string-append
              "<h1><a href=\"/\">agent</a> <span class=\"muted\">/</span> <a href=\"/code\">code</a></h1>"
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
          "</td><td><a href=\"/code?path=" child "\">"
          (html-escape name) "</a></td></tr>")))
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

;; ---- 3. THE SHELL PAGE (GET / and GET /filmstrip) ----
;; ?session=ID resumes as a chat fragment pane (/chat?session=ID — the
;; sessions table's view link and /resume land HERE); ?open=ROUTE is the
;; GET twin of the launcher POST (open-or-focus through the same defs). 
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
          "<!doctype html><html><head><meta charset=utf-8><title>glochid</title>"
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

(define glochid/shell
  (lambda ()
    (let*
      ((frag (hx-request?))
        (row (glochid/shell-row)))
      (glochid/shell-doc
        frag
        (filmstrip/shell-html (nth row 0) (nth row 1) (nth row 2)))))
  "GET / and /filmstrip: the glochid shell. filmstrip/page's body with
glochid chrome; zero JS beyond the vendored htmx assets.")

;; ---- shell routes ----
(route "/" "glochid/shell")
(route "/filmstrip" "glochid/shell")
(route "/filmstrip/open" "filmstrip/open")
(route "/filmstrip/cascade" "filmstrip/cascade")
(route "/filmstrip/close" "filmstrip/close")
(route "/filmstrip/focus" "filmstrip/focus")
(route "/filmstrip/move" "filmstrip/move")
(route "/filmstrip/width" "filmstrip/width")
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
