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
;; ---- chat surface (GET /) ----
;; The chat page skeleton: log target + an hx-post=/chat form with the
;; upstream htmx-4 hx-multipart extension so a multipart/mixed reply
;; streams each part live into the bubble. htmx + hx-multipart are
;; vendored assets served at /assets/*.js (baked into the agent binary).
;; hx-config '{"timeout":0}' disables htmx's 60s defaultTimeout for a
;; request whose lawful lifetime is "a whole agent turn".
(define (chat/page)
  (render-html
    (string-append
      "<!doctype html><html><head><meta charset=utf-8><title>agent</title>"
      "<style>"
      "body{background:#0f1117;color:#d8dee9;font-family:ui-monospace,Menlo,monospace;margin:0;padding:1.5rem}"
      "a{color:#7aa2f7;text-decoration:none}a:hover{text-decoration:underline}"
      ".muted{color:#6b7080}"
      "#log{height:70vh;overflow-y:scroll;border:1px solid #333;border-radius:6px;padding:.75rem;margin-bottom:.75rem}"
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
      "form{display:flex;gap:.5rem}"
      "input{flex:1;background:#1b1f2a;color:#d8dee9;border:1px solid #333;border-radius:6px;padding:.5rem}"
      "button{background:#2a6fdb;color:#fff;border:0;border-radius:6px;padding:.5rem 1rem;cursor:pointer}"
      "</style></head><body>"
      "<h1><a href=\"/\">agent</a> <span class=\"muted\">— <a href=\"/code\">/code</a></span></h1>"
      "<div id=\"log\"><div class=\"muted\">agent ready.</div></div>"
      "<form id=\"f\" hx-post=\"/chat\" hx-target=\"#log\" hx-swap=\"beforeend\" hx-ext=\"hx-multipart\""
      " hx-config='{\"timeout\":0}' hx-on::before:request=\"this.reset()\">"
      "<input id=\"msg\" name=\"msg\" type=\"text\" placeholder=\"prompt…\" autocomplete=\"off\" autofocus>"
      "<button>send</button>"
      "</form>"
      "<script src=\"/assets/htmx.min.js\"></script>"
      "<script src=\"/assets/htmx-2-compat.min.js\"></script>"
      "<script src=\"/assets/hx-multipart.js\"></script>"
      "</body></html>")))
(route "/" "chat/page")

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
let load ~(rt : Runtime.t) : unit =
  Runtime.load_source ~rt Libs_source.html_lib;
  Runtime.load_source ~rt Libs_source.hyper_lib;
  Runtime.load_source ~rt surface_source
