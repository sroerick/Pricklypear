(** pp-remote Nopales surface (borge/remote-eval.borg surfaces +
    agent-loop).

    The browser surface served by [Http_ui.serve] is NOPALES-AUTHORED
    htmx fragments on the client route table, rendered by Nopales via
    [libs/html] — NOT compiled OCaml pages. The surface is redefinable
    live (same env as the REPL): re-evaluating a `(define chat/page …)`
    at the REPL changes the next render without a restart. This is the
    faithful version of the agent's surface thesis, ported to the
    client: the route table + template dispatch are env-only and run
    unchanged under the proxy rim.

    Routes:
      /        — chat page. POST /chat (OCaml-handled multipart stream)
                 runs the SHARED agent loop locally: provider call on
                 this machine, tool bodies under the proxy rim (DB prims
                 cross to the image). Fragment templates
                 (chat/user-bubble, chat/turn-open, chat/token, ...) are
                 Nopales, same names as the agent's so they transfer.
      /eval    — eval box. POST /eval (OCaml-handled) calls Remote.eval
                 and renders the result via the Nopales eval/result
                 template.
      /code    — lens browser. Lists lens nodes (lens/list) and reads
                 one (lens/read). Shows "no lens" when --lens is absent.

    The chat stream emitter + eval POST handler and the HTTP accept
    loop stay OCaml (per the surfaces scope: bootstrap/transport
    plumbing is OCaml); the PAGES and fragment templates are Nopales. *)

open Pricklypear_lisp

(** The surface source. Defines chat/page + the chat/* fragment
    templates, eval/page, eval/result, code/page, code/rows and registers
    the /, /eval and /code routes via the client route table. Uses
    libs/html + the client's request-shape prims (query-param) + the
    lens-browsing prims (lens/list, lens/read). The chat fragment
    templates carry the same names/shapes as the agent's
    (agent/lib/surface.ml) so a restyling transfers between hosts. *)
let surface_source = {pp|
;; ---- chat surface (GET /) ----
;; The chat page skeleton: log target + an hx-post=/chat form with the
;; upstream htmx-4 hx-multipart extension so a multipart/mixed reply
;; streams each part live into the bubble. htmx + hx-multipart are
;; vendored assets served at /assets/*.js (baked into the binary).
;; hx-config '{"timeout":0}' disables htmx's 60s defaultTimeout for a
;; request whose lawful lifetime is "a whole agent turn".
(define (pp/nav)
  (string-append
    "<h1><a href=\"/\">pp-remote</a> <span class=\"muted\">\u2014 "
    "<a href=\"/eval\">/eval</a> \u2014 <a href=\"/code\">/code</a></span></h1>"))

(define (pp/styles)
  (string-append
    "body{background:#0f1117;color:#d8dee9;font-family:ui-monospace,Menlo,monospace;margin:0;padding:1.5rem}"
    "a{color:#7aa2f7;text-decoration:none}a:hover{text-decoration:underline}"
    ".muted{color:#6b7080}"
    "h1 a{color:#d8dee9}"
    "#log{height:60vh;overflow-y:scroll;border:1px solid #333;border-radius:6px;padding:.75rem;margin-bottom:.75rem}"
    ".bubble{background:#1b1f2a;border-radius:6px;padding:.5rem .75rem;margin:.5rem 0;white-space:pre-wrap}"
    ".bubble.user{color:#9ece6a}"
    ".bubble.llm{color:#c0caf5}"
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
    "textarea{background:#1b1f2a;color:#d8dee9;border:1px solid #333;border-radius:6px;padding:.5rem;font-family:inherit;font-size:1rem;min-height:6rem}"
    "pre{background:#12141c;padding:1rem;border-radius:6px;overflow:auto;white-space:pre-wrap;word-break:break-word}"
    ".expr{color:#9ece6a;margin:.5rem 0}"
    "table{border-collapse:collapse}td,th{padding:.25rem .75rem;border-bottom:1px solid #222;text-align:left}"))

(define (chat/page)
  (render-html
    (string-append
      "<!doctype html><html><head><meta charset=utf-8><title>pp-remote</title>"
      "<style>" (pp/styles) "</style></head><body>"
      (pp/nav)
      "<nav class=\"muted\">chat: the agent turn runs here \u2014 provider local, DB prims proxy to the image.</nav>"
      "<div id=\"log\"><div class=\"muted\">pp-remote ready.</div></div>"
      "<form id=\"f\" hx-post=\"/chat\" hx-target=\"#log\" hx-swap=\"beforeend\" hx-ext=\"hx-multipart\""
      " hx-config='{\"timeout\":0}' hx-on::after-request=\"this.reset()\'>"
      "<input id=\"msg\" name=\"msg\" type=\"text\" placeholder=\"prompt\u2026\" autofocus>"
      "<button>send</button>"
      "</form>"
      "<script src=\"/assets/htmx.min.js\"></script>"
      "<script src=\"/assets/htmx-2-compat.min.js\"></script>"
      "<script src=\"/assets/hx-multipart.js\"></script>"
      "</body></html>")))
(route "/" "chat/page")

;; ---- chat fragment templates (called per-part by the OCaml stream) ----
;; Same names/shapes as the agent's surface (agent/lib/surface.ml):
;; redefine live to restyle; a restyle transfers between hosts.
(define (chat/user-bubble text)
  (render-html (string-append "<div class=\"bubble user\">" (html-escape text) "</div>")))

(define (chat/turn-open id)
  (render-html
    (string-append
      "<div class=\"turn\"><details class=\"think\"><summary>thinking + tools</summary>"
      "<div id=\"" id "-think\"></div></details>"
      "<div class=\"bubble llm\" id=\"" id "\"></div></div>")))

(define (chat/think-open id)
  (render-html
    (string-append
      "<details class=\"tk\"><summary>thinking</summary>"
      "<div id=\"" id "\"></div></details>")))

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

(define (chat/think-token text)
  (render-html (string-append "<span class=\"reason\">" (html-escape text) "</span>")))

(define (chat/token text)
  (render-html (string-append "<span>" (html-escape text) "</span>")))

(define (chat/error text)
  (render-html (string-append "<div class=\"trace\">[turn error: " (html-escape text) "]</div>")))

;; ---- eval surface (GET /eval) ----
;; An eval box: textarea + button that hx-post=/eval, target #out.
;; htmx is a vendored asset served at /assets/*.js (baked into the
;; pp-remote binary). The POST handler (OCaml) calls Remote.eval and
;; renders the result via eval/result, so the result fragment is
;; Nopales-authored and redefinable live.
(define (eval/page)
  (render-html
    (string-append
      "<!doctype html><html><head><meta charset=utf-8><title>pp-remote eval</title>"
      "<style>" (pp/styles) "</style></head><body>"
      (pp/nav)
      "<nav class=\"muted\">eval against the lens + proxy rim. POST /eval runs Remote.eval.</nav>"
      "<form hx-post=\"/eval\" hx-target=\"#out\" hx-swap=\"innerHTML\">"
      "<textarea name=\"expr\" placeholder=\"(+ 1 2)\\n(list-packages)\\n(whoami)\" autofocus></textarea>"
      "<button>eval</button></form>"
      "<pre id=\"out\" class=\"muted\">—</pre>"
      "<script src=\"/assets/htmx.min.js\"></script>"
      "</body></html>")))
(route "/eval" "eval/page")

;; ---- eval result fragment (rendered by the OCaml POST /eval handler) ----
;; (eval/result expr result) -> raw-HTML fragment swapped into #out.
;; [result] is the pp_value string the handler produced; [expr] is the
;; source the user submitted. Redefine live to restyle.
(define (eval/result expr result)
  (render-html
    (string-append
      "<div class=\"expr\">$ " (html-escape expr) "</div>"
      "<pre>" (html-escape result) "</pre>")))

;; ---- lens browser (GET /code) ----
;; Read-only list of lens nodes + a node view, rendered by Nopales via
;; lens/list + lens/read (client host prims backed by Lens_store). With
;; no --lens wired, lens/list returns "[]" and the page says so.
;; query-param "path" selects a node to view.
(define (code/page)
  (let* ((p (query-param "path"))
         (rel (if (null? p) "" p)))
    (if (string-eq rel "")
        (let* ((nodes-json (lens/list))
               (nodes (json-parse nodes-json)))
          (render-html
            (string-append
              "<!doctype html><html><head><meta charset=utf-8><title>code</title>"
              "<style>" (pp/styles) "</style></head><body>"
              (pp/nav)
              (if (null? nodes)
                  "<p class=\"muted\">no lens wired (start pp-remote with --lens DIR).</p>"
                  (string-append
                    "<p class=\"muted\">" (html-escape (as-str (count nodes))) " node(s)</p>"
                    "<table><tr><th>kind</th><th>name</th><th>path</th></tr>"
                    (code/rows nodes)
                    "</table>"))
              "</body></html>")))
        (let* ((src (lens/read rel)))
          (render-html
            (string-append
              "<!doctype html><html><head><meta charset=utf-8><title>code</title>"
              "<style>" (pp/styles) "</style></head><body>"
              (pp/nav)
              "<p class=\"muted\">" (html-escape rel) "</p>"
              (if (null? src)
                  "<p class=\"muted\">not found.</p>"
                  (string-append "<pre>" (html-escape src) "</pre>"))
              "</body></html>"))))))

(define (code/rows nodes)
  (list/foldl
    (lambda (acc entry)
      (let* ((name (dict-get entry "name"))
             (kind (dict-get entry "kind"))
             (path (dict-get entry "path")))
        (string-append acc
          "<tr><td>" (html-escape kind)
          "</td><td><a href=\"/code?path=" (html-escape path) "\">"
          (html-escape name) "</a></td><td class=\"muted\">"
          (html-escape path) "</td></tr>")))
    "" nodes))

(route "/code" "code/page")
|pp}

(** Boot-load the embedded Nopales packages + this surface into the
    shared env, registering the route-table routes. Idempotent-ish:
    re-evaluating defines redefines bindings live. Call AFTER
    [Remote.make] (which registers the route + soup + pure prims and
    the let* macro). *)
let load ~(rt : Remote.t) : unit =
  Remote.load_source ~rt Libs_source.html_lib;
  Remote.load_source ~rt Libs_source.hyper_lib;
  Remote.load_source ~rt surface_source
