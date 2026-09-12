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
(define (chat/page)
  ;; ?session=<id> resumes/backfills: the page renders the stored
  ;; transcript into #log (sessions-persist resume path). The live
  ;; resume side effect happens on /resume (GET) before the redirect here.
  (let ((sid (query-param "session")))
    (render-html
    (string-append
      "<!doctype html><html><head><meta charset=utf-8><title>agent</title>"
      "<style>"
      "html,body{height:100%}"
      "body{background:#0f1117;color:#d8dee9;font-family:ui-monospace,Menlo,monospace;margin:0;padding:1rem;box-sizing:border-box;display:flex;flex-direction:column}"
      "a{color:#7aa2f7;text-decoration:none}a:hover{text-decoration:underline}"
      ".muted{color:#6b7080}"
      "#log{flex:1;min-height:0;overflow-y:auto;border:1px solid #333;border-radius:6px;padding:.75rem;margin-bottom:.75rem}"
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
      "form{display:flex;gap:.5rem;flex-shrink:0}"
      "h1{margin:.25rem 0 .75rem 0;font-size:1.1rem;flex-shrink:0}"
      "input{flex:1;background:#1b1f2a;color:#d8dee9;border:1px solid #333;border-radius:6px;padding:.5rem}"
      "button{background:#2a6fdb;color:#fff;border:0;border-radius:6px;padding:.5rem 1rem;cursor:pointer}"
      ":root{color-scheme:dark}"
      "*{scrollbar-width:thin;scrollbar-color:#2a2e3a transparent}"
      "::-webkit-scrollbar{width:10px;height:10px}"
      "::-webkit-scrollbar-track{background:transparent}"
      "::-webkit-scrollbar-thumb{background:#2a2e3a;border-radius:6px}"
      "::-webkit-scrollbar-thumb:hover{background:#4a4f5c}"
      "::-webkit-scrollbar-corner{background:transparent}"
      "</style></head><body>"
      "<h1><a href=\"/\">agent</a> <span class=\"muted\">— <a href=\"/code\">/code</a> · <a href=\"/sessions\">/sessions</a> · <a href=\"/settings\">/settings</a> · model: "
      "<select name=\"model\" hx-post=\"/settings\" hx-trigger=\"change\" hx-swap=\"none\" "
      "style=\"background:#1b1f2a;color:#d8dee9;border:1px solid #333;border-radius:4px;font-size:.6em\">"
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
      "</form>"
      "<script src=\"/assets/htmx.min.js\"></script>"
      "<script src=\"/assets/htmx-2-compat.min.js\"></script>"
      "<script src=\"/assets/hx-multipart.js\"></script>"
      "</body></html>"))))
(route "/" "chat/page")

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
  (render-html
    (string-append
      "<!doctype html><html><head><meta charset=utf-8><title>agent sessions</title>"
      "<style>"
      "body{background:#0f1117;color:#d8dee9;font-family:ui-monospace,Menlo,monospace;margin:0;padding:1.5rem}"
      "a{color:#7aa2f7;text-decoration:none}a:hover{text-decoration:underline}"
      ".muted{color:#6b7080}"
      "table{border-collapse:collapse;width:100%;max-width:60rem}td,th{padding:.35rem .6rem;border-bottom:1px solid #222;text-align:left;white-space:nowrap}"
      ":root{color-scheme:dark}"
      "</style></head><body>"
      "<h1><a href=\"/\">agent</a> <span class=\"muted\">— /sessions</span></h1>"
      "<p><a href=\"/new\">+ new session</a></p>"
      "<table><tr><th>time</th><th>model</th><th>msgs</th><th>first prompt</th><th></th></tr>"
      (sessions/rows (session/list))
      "</table>"
      "<p class=\"muted\">transcripts: .agent/sessions/*.jsonl (append-only, local store)</p>"
      "</body></html>")))
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
  (render-html
    (string-append
      "<!doctype html><html><head><meta charset=utf-8><title>agent settings</title>"
      "<style>"
      "body{background:#0f1117;color:#d8dee9;font-family:ui-monospace,Menlo,monospace;margin:0;padding:1.5rem}"
      "a{color:#7aa2f7;text-decoration:none}a:hover{text-decoration:underline}"
      ".muted{color:#6b7080}"
      ".card{background:#1b1f2a;border:1px solid #2a2e3a;border-radius:6px;padding:1rem;max-width:38rem}"
      "label{color:#8a8f9e}"
      "input,select{width:100%;background:#0f1117;color:#d8dee9;border:1px solid #333;border-radius:6px;padding:.5rem;margin-top:.25rem}"
      "button{background:#2a6fdb;color:#fff;border:0;border-radius:6px;padding:.5rem 1rem;cursor:pointer;margin-top:.5rem}"
      "</style></head><body>"
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
      "</div>"
      "<script src=\"/assets/htmx.min.js\"></script>"
      "</body></html>")))
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
          (render-html
            (string-append
              "<!doctype html><html><head><meta charset=utf-8><title>code</title>"
              "<style>"
              "body{background:#0f1117;color:#d8dee9;font-family:ui-monospace,Menlo,monospace;margin:0;padding:1.5rem}"
              "a{color:#7aa2f7;text-decoration:none}"
              ".muted{color:#6b7080}"
              "table{border-collapse:collapse}td,th{padding:.25rem .75rem;border-bottom:1px solid #222}"
              "pre{background:#12141c;padding:1rem;border-radius:6px;overflow:auto}"
              ":root{color-scheme:dark}"
              "*{scrollbar-width:thin;scrollbar-color:#2a2e3a transparent}"
              "::-webkit-scrollbar{width:10px;height:10px}"
              "::-webkit-scrollbar-track{background:transparent}"
              "::-webkit-scrollbar-thumb{background:#2a2e3a;border-radius:6px}"
              "::-webkit-scrollbar-thumb:hover{background:#4a4f5c}"
              "::-webkit-scrollbar-corner{background:transparent}"
              "</style></head><body>"
              "<h1><a href=\"/\">agent</a> <span class=\"muted\">/</span> <a href=\"/code\">code</a></h1>"
              "<p class=\"muted\">" (html-escape rel) "</p>"
              "<table><tr><th></th><th>name</th></tr>"
              (code/rows items rel)
              "</table></body></html>")))
        (let* ((content (fs/read rel)))
          (render-html
            (string-append
              "<!doctype html><html><head><meta charset=utf-8><title>code</title>"
              "<style>"
              "body{background:#0f1117;color:#d8dee9;font-family:ui-monospace,Menlo,monospace;margin:0;padding:1.5rem}"
              "a{color:#7aa2f7;text-decoration:none}"
              ".muted{color:#6b7080}"
              "pre{background:#12141c;padding:1rem;border-radius:6px;overflow:auto}"
              ":root{color-scheme:dark}"
              "*{scrollbar-width:thin;scrollbar-color:#2a2e3a transparent}"
              "::-webkit-scrollbar{width:10px;height:10px}"
              "::-webkit-scrollbar-track{background:transparent}"
              "::-webkit-scrollbar-thumb{background:#2a2e3a;border-radius:6px}"
              "::-webkit-scrollbar-thumb:hover{background:#4a4f5c}"
              "::-webkit-scrollbar-corner{background:transparent}"
              "</style></head><body>"
              "<h1><a href=\"/\">agent</a> <span class=\"muted\">/</span> <a href=\"/code\">code</a></h1>"
              "<p class=\"muted\">" (html-escape rel) "</p>"
              "<pre>" (html-escape content) "</pre></body></html>"))))))

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
  Runtime.load_source ~rt Libs_source.html_lib;
  Runtime.load_source ~rt Libs_source.hyper_lib;
  Runtime.load_source ~rt surface_source
