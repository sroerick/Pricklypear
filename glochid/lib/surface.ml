(** Agent Nopales surface (borge/glochid.borg surfaces).

    The chat + code browser pages are NOPALES-AUTHORED htmx fragments on
    the glochid route table, rendered by Nopales via [libs/html] — NOT
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
    the / and /code routes via the glochid route table. Uses libs/html +
    the glochid request-shape prims (query-param). *)
let surface_source = {pp|
;; ---- shared page chrome (borge/glochid.borg filmstrip-shell) ----
;; Pages are PANE-AWARE: under HX-Request (filmstrip partial mode) a
;; route renders the BODY fragment only; a full GET gets the doc
;; skeleton + htmx scripts. One css block shared by every page + the
;; shell (incl. the theme custom props filmstrip/css consumes). Redefine
;; glochid/base-css live to restyle everything.
(define glochid/base-css
  (string-append
    ;; THE PALETTE IS THE THEME: every token resolves from the active
    ;; omarchy theme's colors.toml (defaults = the pre-theme palette;
    ;; filmstrip/css consumes these SAME custom props).
    ":root{color-scheme:"
    (if (string-eq (glochid/tg "mode" "dark") "light") "light" "dark")
    ";--border:" (glochid/tg "selection" "#2a2e3a")
    ";--panel:" (glochid/tg "lighter_background" "#1b1f2a")
    ";--ink:" (glochid/tg "foreground" "#d8dee9")
    ";--muted:" (glochid/tg "muted" "#6b7080")
    ";--chrome:" (glochid/tg "dark_background" "#12141c")
    ";--sel-bg:" (glochid/tg "selection" "#2a2e3a")
    ";--sel-accent:" (glochid/tg "accent" "#7aa2f7")
    ";--pp-bg:" (glochid/tg "darker_background" "#0f1117")
    ";--pp-repl:" (glochid/tg "magenta" "#bb9af7")
    ";--pp-ok:" (glochid/tg "green" "#2f9e44")
    ";--pp-warn:"
    (glochid/tg "orange" (glochid/tg "yellow" "#e8590c"))
    ";--pp-danger:" (glochid/tg "bright_red" "#c92a2a")
    ";--pp-btn:" (glochid/tg "blue" "#2a6fdb")
    ";--pp-btn-hi:" (glochid/tg "accent" "#3b5bdb")
    "}"
    "html,body{height:100%}"
    "body{background:var(--pp-bg);color:var(--ink);font-family:ui-monospace,Menlo,monospace;margin:0;padding:1rem;box-sizing:border-box}"
    "a{color:var(--sel-accent);text-decoration:none}a:hover{text-decoration:underline}"
    ".muted{color:var(--muted)}"
    "h1{margin:.25rem 0 .75rem 0;font-size:1.1rem}"
    "table{border-collapse:collapse;width:100%;max-width:60rem}td,th{padding:.35rem .6rem;border-bottom:1px solid var(--border);text-align:left;white-space:nowrap}"
    ;; sessions table: FIXED layout so the three width presets always
    ;; fit the pane — long first-prompt/model cells ellipsize instead of
    ;; forcing the pane body (and the strip) to scroll horizontally.
    "table.sess{table-layout:fixed}table.sess td,table.sess th{overflow:hidden;text-overflow:ellipsis}"
    "table.sess th:nth-child(1){width:10.5rem}table.sess th:nth-child(3){width:3rem}table.sess th:nth-child(5){width:6.5rem}"
    "pre{background:var(--chrome);padding:1rem;border-radius:6px;overflow:auto}"
    "input{background:var(--panel);color:var(--ink);border:1px solid var(--border);border-radius:6px;padding:.5rem}"
    "select{background:var(--panel);color:var(--ink);border:1px solid var(--border);border-radius:4px}"
    "label{color:var(--muted)}"
    ".card{background:var(--panel);border:1px solid var(--border);border-radius:6px;padding:1rem;max-width:38rem}"
    ".card p{margin:.5rem 0}"
    ".card input,.card select{width:100%;padding:.5rem;margin-top:.25rem}"
    "button{background:var(--pp-btn);color:#fff;border:0;border-radius:6px;padding:.5rem 1rem;cursor:pointer}"
    ".fs-shell{height:calc(100vh - 2rem)}"
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
    ".fs-shell:has(#fs-axis.col) #fs-rail{order:-3;display:flex;flex-direction:column;flex:0 0 20rem;max-width:20rem;min-width:0;min-height:0;overflow-y:auto;overflow-x:hidden;gap:.25rem;padding:.25rem .25rem .25rem 0}"
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
    ".bubble{background:var(--panel);border-radius:6px;padding:.5rem .75rem;margin:.5rem 0;white-space:pre-wrap}"
    ".bubble.user{color:var(--pp-ok)}"
    ".bubble.llm{color:var(--ink)}.bubble.repl{color:var(--pp-repl)}"
    ".trace{color:var(--muted);font-size:.85em;white-space:pre-wrap}"
    ".think{color:var(--muted);font-size:.9em;margin:.4rem 0}"
    ".think summary{cursor:pointer}"
    ".think>div{white-space:pre-wrap}"
    ".reason{color:var(--muted);font-style:italic}"
    ".think .tk,.think .tool{margin:.25rem 0 .25rem .25rem}"
    ".think .tk>summary,.think .tool>summary{color:var(--muted)}"
    ".think .tk>div,.think .tool>div{margin-left:.9rem;padding-left:.5rem;border-left:1px solid var(--border)}"
    ".think:not(:has(.reason,.trace)){display:none}"
    ".chatpage{display:flex;flex-direction:column;height:calc(100vh - 6rem)}"
    ".chatpage #log{flex:1;min-height:0;overflow-y:auto;border:1px solid var(--border);border-radius:6px;padding:.75rem;margin-bottom:.75rem}"
    ".chatpage form{display:flex;gap:.5rem;flex-shrink:0}"
    ".chatpage #msg{flex:1}"
    ".chatpage form button{flex-shrink:0}"
    "*{scrollbar-width:thin;scrollbar-color:var(--sel-bg) transparent}"
    "::-webkit-scrollbar{width:10px;height:10px}"
    "::-webkit-scrollbar-track{background:transparent}"
    "::-webkit-scrollbar-thumb{background:var(--sel-bg);border-radius:6px}"
    "::-webkit-scrollbar-thumb:hover{background:var(--muted)}"
    "::-webkit-scrollbar-corner{background:transparent}"
    ;; v2 app-links: a button styled as a bare link (chat/sessions/settings
    ;; nav in pane chrome; the anchor version navigated the whole window
    ;; out of the shell with a full GET). Overrides the generic button look.
    ".fs-applink{background:none;border:0;color:var(--sel-accent);padding:0;margin:0;font:inherit;cursor:pointer;text-decoration:none}"
    ".fs-applink:hover{text-decoration:underline}"
    ".fs-modelsel{font-size:.6em;padding:.1rem .3rem}"
    ;; lookbook seed elements (libs/lookbook): the elements declare
    ;; classes; the HOST stylesheet styles them (same seam discipline
    ;; as every host variation -- the habitat pass-two will theme its
    ;; own). Kinds ride the class suffix from the variant param.
    ".lb-btn{font:inherit;padding:.25rem .7rem;border:1px solid var(--border);border-radius:4px;background:var(--panel);color:inherit;cursor:pointer}"
    ".lb-btn-primary{background:var(--pp-btn-hi);border-color:var(--pp-btn-hi);color:#fff}"
    ".lb-btn-danger{background:var(--pp-danger);border-color:var(--pp-danger);color:#fff}"
    ".lb-badge{display:inline-block;font-size:.72em;padding:.05rem .45rem;border-radius:999px;border:1px solid var(--border);color:var(--muted)}"
    ".lb-badge-ok{color:var(--pp-ok);border-color:var(--pp-ok)}"
    ".lb-badge-warn{color:var(--pp-warn);border-color:var(--pp-warn)}"
    ".lb-card{border:1px solid var(--border);border-radius:6px;padding:.75rem;background:var(--panel)}"
    ".lb-card h3{margin:0 0 .5rem;display:flex;gap:.5rem;align-items:center}"
    ".lb-card p{margin:.25rem 0 0}"
    ;; lookbook pane chrome: the stage + the forms + the folds.
    ".lb-stage{border:1px dashed var(--border);padding:.9rem;border-radius:6px;display:flex;gap:.6rem;align-items:center;flex-wrap:wrap}"
    ".lb-form{display:flex;gap:.4rem;flex-wrap:wrap;align-items:center;margin:.6rem 0}"
    ".lb-form input{font:inherit;padding:.2rem .4rem;background:var(--panel);color:inherit;border:1px solid var(--border);border-radius:4px}"
    ".lb-form label{color:var(--muted,#9aa3b2);font-size:.85em}"
    ".lb-folds{margin-top:.6rem}"
    ".lb-folds details{margin:.3rem 0}"
    ".lb-folds summary{cursor:pointer;color:var(--muted,#9aa3b2)}"
    ".lb-folds pre{overflow:auto;font-size:.85em;background:var(--panel);padding:.5rem;border-radius:4px;border:1px solid var(--border)}"
    ".lb-vtabs{display:flex;gap:.3rem;flex-wrap:wrap;margin:.4rem 0}
    .lb-vtab{cursor:pointer}
    .lb-vtab.fs-sel{outline:1px solid var(--sel-accent)}"
    ;; theme swatches: the lookbook face of the PALETTE ITSELF
    ;; (ui/swatch + ui/palette -- the ui rail root renders the theme).
    ".lb-swatch{display:inline-flex;align-items:center;gap:.4rem;padding:.15rem .55rem;border:1px solid var(--border);border-radius:4px;font-size:.85em}"
    ".lb-swatch b{width:.95em;height:.95em;display:inline-block;border-radius:3px;border:1px solid var(--border)}"
    ".lb-palette{display:flex;gap:.35rem;flex-wrap:wrap}"
    ".lb-palette .lb-swatch{border:none;padding-left:0}"
    ".lb-palette .lb-swatch b{width:1.6em;height:1.6em}"
    ".lb-palette .lb-swatch span{color:var(--muted)}"
    ;; v4 rail trees: mode tabs controlbox + one :has pair per root id
    ;; (windows / code / prims / ui) shows only the active tree box off the
    ;; #fs-railmode sentinel -- the SAME hiding trick desktops use, so a
    ;; mode flip never re-renders (expansion state is DOM).
    ".fs-railtabs{display:flex;gap:.3rem;align-items:center;border:1px solid var(--border);background:var(--panel);padding:.3rem .35rem}"
    ;; start menu: zero-JS <details> fold in the railtabs row (B: the
    ;; cascade/launchbar column's replacement). Closed = one word; open =
    ;; an absolutely-positioned menu floating OVER the tree below —
    ;; position:fixed pane discipline doesn't apply to menus (pads/layers-
    ;; modality: genuinely transient), and no JS is involved.
    ".fs-start{position:relative;flex:0 0 auto}"
    ".fs-start-word{cursor:pointer;list-style:none;font-weight:700}"
    ".fs-start-word::-webkit-details-marker{display:none}"
    ".fs-start-menu{position:absolute;left:0;top:100%;z-index:30;display:flex;flex-direction:column;min-width:10rem;background:var(--panel);border:1px solid var(--border);box-shadow:0 4px 14px rgba(0,0,0,.5);padding:.25rem}"
    ;; card e3de4aa0: the menu must never extend below the screen -- cap it
    ;; at about two-thirds of the viewport height with its own scroll.
    ".fs-start-menu{max-height:66vh;overflow-y:auto}"
    ".fs-start-entry{width:100%;text-align:left;padding:.3rem .5rem;white-space:nowrap}"
    ".fs-start-entry:hover{background:var(--sel-bg)}"
    ".fs-railtab.fs-btn{background:none;border:1px solid transparent;color:var(--sel-accent)}"
    ;; /code file arm: CodeMirror boots into .pp-cm-host (replaces the
    ;; tagged <pre>; theme rides the shell dark palette).
    ".pp-cm-host .CodeMirror{height:80vh;font-size:13px;line-height:1.4;background:var(--chrome);color:var(--ink)}"
    ".pp-cm-host .CodeMirror-gutters{background:var(--chrome);border-right:1px solid var(--border)}"
    ".pp-cm-host .CodeMirror-linenumber{color:var(--muted)}"
    ".pp-cm-host .CodeMirror-cursor{border-left:1px solid var(--sel-accent)}"
    ;; editor chrome: save row (button + status) the boot script injects
    ;; above each CM host
    ".pp-cm-bar{display:flex;gap:.6rem;align-items:center;margin:.35rem 0}"
    ".pp-cm-save{cursor:pointer}"
    ".pp-cm-status::after{content:none}"
    ;; keyboard hint glyph in the rail tabs row: the map lives in the
    ;; title tooltip (zero-JS discoverability).
    ".fs-keys{margin-left:auto;color:var(--muted,#6b7080);cursor:help;font-size:1.1em;user-select:none}"))

(define (glochid/layout title body)
  (if (hx-request?)
      (render-html body)
      (render-html
        (string-append
            "<!doctype html><html lang=en><head><meta charset=utf-8><title>" title "</title>"
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

;; ---- llm-config helpers (borge/glochid.borg llm-config) ----
;; The model dropdown is fed by the provider ABI (glochid/models = GET
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
;; vendored assets served at /assets/*.js (baked into the glochid binary).
;; hx-config '{"timeout":0}' disables htmx's 60s defaultTimeout for a
;; request whose lawful lifetime is "a whole glochid turn".
;; hx-config '{"'timeout'":0}' disables htmx's 60s defaultTimeout for a
;; request whose lawful lifetime is "a whole glochid turn".
(define (chat/head frag)
  ;; The 'glochid' root anchor is shell-level (it reloads / in place) — a
  ;; plain <a> inside a fragment is the escape bug class, so fragments
  ;; get a muted label instead; nav words stay app-links in both modes.
  (string-append
    "<h1>"
    (if frag
        "<span class=\"muted\">glochid</span>"
        "<a href=\"/\">glochid</a>")
    " <span class=\"muted\">— "
    (filmstrip/app-link "/code" "/code")
    " · "
    (filmstrip/app-link "/bindings" "/bindings")
    " · "
    (filmstrip/app-link "/sessions" "/sessions")
    " · "
    (filmstrip/app-link "/settings" "/settings")
    " · model: "
      "<select name=\"model\" aria-label=\"model\" class=\"fs-modelsel\" hx-post=\"/settings\" hx-trigger=\"change\" hx-swap=\"none\">"
    (settings/model-options (glochid/model) (glochid/models))
    "</select></span></h1>"))

(define (chat/body)
  "Chat surface body — one structure for page AND pane, so the stream's
#log target works in both (borge/glochid.borg filmstrip-shell #4).
Fragment-mode (hx-request?) differences: the h1 'glochid' anchor and the
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
          "<div class=\"muted\">glochid ready.</div>"
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
        "<input id=\"msg\" name=\"msg\" type=\"text\" placeholder=\"prompt…\" autocomplete=\"off\" autofocus aria-label=\"prompt\">"
      "<button>send</button>"
      "</form></div>")))

(define (chat/page)
  (glochid/layout "glochid — chat" (chat/body)))
(route "/chat" "chat/page")

;; ---- sessions surface (GET /sessions; borge/glochid.borg sessions-persist) ----
;; Appended-only JSONL transcripts under .glochid/sessions/ (legacy .agent/sessions trees keep working). Resume:
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
          "<h1><a href=\"/\">glochid</a> <span class=\"muted\">— /sessions</span></h1>")
      (if (hx-request?)
          "<p><button class=\"fs-applink\" hx-post=\"/filmstrip/new\" hx-target=\"#fs-sink\" hx-swap=\"innerHTML\">+ new session</button></p>"
          "<p><a href=\"/new\">+ new session</a></p>")
      "<table class=\"sess\"><tr><th>time</th><th>model</th><th>msgs</th><th>first prompt</th><th></th></tr>"
      (sessions/rows (session/list))
      "</table>"
      "<p class=\"muted\">transcripts: .glochid/sessions/*.jsonl (append-only, local store)</p>")))
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
(route "/resume" "sessions/resume");; ---- settings surface (GET /settings; borge/glochid.borg llm-config) ----
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
          "<h1><a href=\"/\">glochid</a> <span class=\"muted\">— /settings</span></h1>")
      "<div class=\"card\"><form method=\"post\" action=\"/settings\">"
      "<p><label>API key — " (glochid/key-status) " · blank keeps current</label>"
      "<input type=\"password\" name=\"api_key\" autocomplete=\"off\"></p>"
      "<p><label>Base URL</label>"
      "<input type=\"text\" name=\"base_url\" value=\"" (html-escape (glochid/base-url)) "\"></p>"
      "<p><label>Model</label>"
      "<select name=\"model\">" (settings/model-options (glochid/model) (glochid/models)) "</select></p>"
      "<button>save</button></form>"
      "<p class=\"muted\">Key + base come down invisibly via pp-remote --sync "
      "(lens __config.json). Model is chosen per surface from the provider's /models.</p>"
      "</div>")))
(route "/settings" "settings/page")

;; ---- chat fragment templates (called per-part by the OCaml stream) ----
;; Each returns a raw-HTML fragment the multipart emitter writes as one
;; part with per-part HX-Target/HX-Swap. Redefine live to restyle. The
;; bubble/trace bodies are DECLARED ELEMENTS (ui/bubble, ui/tool-line in
;; the glochid layer) projected through lookbook/html -- the stream
;; contract (raw HTML out) is unchanged; the markup site is the one
;; projection.
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
  (render-html (lookbook/html (ui/turn-open id))))

;; Nested thinking block: one contiguous reasoning run gets its own
;; <details>/<summary> inside the zone; think-token deltas stream into #id.
;; Starts CLOSED so a finished turn stays scannable.
(define (chat/think-open id)
  (render-html (lookbook/html (ui/think-open id))))

;; Nested tool-call block: <details>/<summary> inside the zone; input/output
;; lines (chat/tool-line) stream into #id, and chat/tool-done marks the
;; summary via innerHTML on #<id>-sum when the call returns.
(define (chat/tool-open id name)
  (render-html (lookbook/html (ui/tool-open id name))))

(define (chat/tool-line label text)
  (render-html (lookbook/html (ui/tool-line label text))))

(define (chat/tool-done name)
  (render-html (string-append "tool: " (html-escape name) " &#10003;")))

;; Provider chain-of-thought delta, into the current thinking block.
(define (chat/think-token text)
  (render-html (string-append "<span class=\"reason\">" (html-escape text) "</span>")))

(define (chat/token text)
  (render-html (string-append "<span>" (html-escape text) "</span>")))

(define (chat/error text)
  (render-html (lookbook/html (ui/turn-error text))))

;; ---- code browser (GET /code) ----
;; Read-only file tree + file content arm, rendered by Nopales via fs/ls
;; + fs/read + fs/dir? (glochid host prims). query-param "path" selects
;; the entry (relative to the glochid cwd); the glochid fs prims refuse
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
      "<h1><a href=\"/\">glochid</a> <span class=\"muted\">/</span> <a href=\"/code\">code</a></h1>"))

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

;; ---- bindings browser (GET /bindings; borge/glochid.borg filmstrip-shell cascade) ----
;; The habitat's FUNCTION GRAMMAR (the image's /bindings face) as a
;; glochid app: package -> function -> doc+source, read-only over the
;; bind/* habitat-bridge prims (glochid/lib/habitat.ml — the one
;; POST /api/eval client; creds GLOCHID_HABITAT_(URL|TOKEN|USER|PASS) (legacy AGENT_*) > PP_*,
;; token wins when set).
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
      "<p class=\"muted\">set GLOCHID_HABITAT_URL / GLOCHID_HABITAT_USER / GLOCHID_HABITAT_PASS "
      "(or PP_URL / PP_USER / PP_PASS) so glochid can read the habitat's function grammar.</p>"))
  "The page's bridge-down block: the bridge's own error text + the
credential pointer (same env trio the mem/* bridge uses).")

(define bind/head
  (lambda ()
    (if (hx-request?)
        "<h1><span class=\"muted\">/bindings</span></h1>"
        "<h1><a href=\"/\">glochid</a> <span class=\"muted\">/</span> <a href=\"/bindings\">bindings</a></h1>"))
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

(** The glochid layer (borge/glochid.borg filmstrip-shell). Load order:
    libs/html, libs/css, libs/filmstrip, THIS, then [surface_source].
    The filmstrip lib boots as glochid's shell with two HOST seams
    swapped (the harness's request-runner seam makes this a load, not a
    port — the allowed variation is recorded in borge/filmstrip.borg):

      1. WORKSPACE BACKING: filmstrip/ws-node / ws-save redefined over
         fs prims into the resolved glochid/ws-file (project-local, beside
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

;; ---- 1a. THE DESKTOP PALETTE (omarchy theme; HOST SEAM 3) ----
;; The shell was hardcoded to one dark palette; now every token in
;; base-css resolves from the ACTIVE omarchy theme via the omarchy/
;; theme host prim (the stable-keyed colors.toml -- see the OCaml
;; comment). The resolve is LOAD-TIME: (glochid/tg k default) answers
;; the toml value or the pre-theme fallback, and no omarchy on a box
;; means exactly the old colors. `omarchy theme set X` + a session
;; reload (HUP) re-resolves everything.

(define glochid/theme-raw (omarchy/theme))

(define glochid/tg
  (lambda (k d)
    (let ((v (dict-get glochid/theme-raw k)))
      (if (null? v) d (as-str v))))
  "One palette token: toml value or the pre-theme default (fail-soft).")

;; ---- 1a0. GLOCHID UI PRIMS AS DECLARED ELEMENTS (libs/lookbook;
;; borge/filmstrip.borg lookbook stanza, first batch) ----
;; OBITUARY: the rail tabs, start-menu entries, chat bubbles and tool
;; trace lines used to be markup strings hand-built INLINE in their
;; consumers (html/tag in glochid/railtabs + glochid/start-menu,
;; string-append in the chat/* templates). They are now DECLARED UI
;; VALUES (ui/elm) here, and each consumer projects the value through
;; lookbook/html -- the libs/lookbook discipline: declare, never
;; render; one markup site (the projection). The lookbook pane then
;; inventories + stages them like any other element: examples ARE the
;; declaration (<name>/examples cards below). The OCaml stream contract
;; is untouched: chat//templates still answer raw-HTML strings -- the
;; projection call sits INSIDE the template.

;; capture the lib seeds BEFORE redefining the inventory seam, so lib
;; additions keep showing without this list restating them.
(define lb/seed-elements (lookbook/elements))

(define ui/railtab
  (lambda (label)
    (ui/elm
      "button"
      (list
        "class" "fs-btn fs-railtab"
        "title" (string-append "show the " label " tree")
        "hx-post" "/filmstrip/railmode"
        "hx-vals" (string-append "{\"root\":\"" label "\"}")
        "hx-target" "#fs-sink"
        "hx-swap" "innerHTML")
      (list (ui/text label))))
  "One rail mode tab: POSTs /filmstrip/railmode with this root (the
sentinel :has flip -- expansion DOM stays alive). Declared behavior
data: the hx-* verbs ride attrs verbatim through the projection.")

(define ui/start-entry
  (lambda (label route)
    (ui/elm
      "button"
      (list
        "class" "fs-btn fs-start-entry"
        "title" (string-append "open " label)
        "hx-post" "/filmstrip/open"
        "hx-vals" (string-append "{\"route\":\"" route "\"}")
        "hx-target" "#fs-sink"
        "hx-swap" "innerHTML")
      (list (ui/text label))))
  "One start-menu entry: opens `route` as a pane via /filmstrip/open.")

(define ui/bubble
  (lambda (kind text id)
    (ui/elm
      "div"
      (if (string-eq id "")
          (list "class" (string-append "bubble " kind))
          (list "class" (string-append "bubble " kind) "id" (as-str id)))
      (list (ui/text text))))
  "One chat bubble of `kind` (user / llm / repl); `id` pins the swap
target for streamed turns -- empty id renders without one. The
streamed LLM bubble opens EMPTY: deltas swap innerHTML into #id.")

(define ui/tool-line
  (lambda (label text)
    (ui/elm
      "div"
      (list "class" "trace")
      (list (ui/text label) (ui/text " ") (ui/text text))))
  "One tool trace line inside a tool-call fold: label + text, both
escaped through the projection (never trust the model's strings).")

(define ui/turn-open
  (lambda (id)
    (ui/elm
      "div"
      (list "class" "turn")
      (list
        (ui/elm "details" (list "class" "think")
          (list
            (ui/elm "summary" (list)
              (list (ui/text "thinking + tools")))
            (ui/elm "div"
              (list "id" (string-append (as-str id) "-think"))
              (list))))
        (ui/elm "div" (list "class" "bubble llm" "id" (as-str id))
          (list)))))
  "Per-turn skeleton: the thinking+tools fold (the empty zone div the
stream APPENDS reasoning/tool folds into) + the empty streamed LLM
bubble #id. Rendered as ONE part so the targets exist before any
delta parts arrive.")

(define ui/think-open
  (lambda (id)
    (ui/elm "details" (list "class" "tk")
      (list
        (ui/elm "summary" (list)
          (list (ui/text "thinking")))
        (ui/elm "div" (list "id" (as-str id)) (list)))))
  "One contiguous reasoning run: starts CLOSED so a finished turn
stays scannable; think-token deltas stream into #id.")

(define ui/tool-open
  (lambda (id name)
    (ui/elm "details" (list "class" "tool")
      (list
        (ui/elm "summary"
          (list "id" (string-append (as-str id) "-sum"))
          (list (ui/text (string-append "tool: " name))))
        (ui/elm "div" (list "id" (as-str id)) (list)))))
  "One tool call fold: trace lines (ui/tool-line) stream into #id;
completion marks the summary via innerHTML on #<id>-sum.")

(define ui/turn-error
  (lambda (text)
    (ui/elm "div" (list "class" "trace")
      (list (ui/text (string-append "[turn error: " text "]")))))
  "One turn error line: trace-styled bracketed message, escaped
through the projection like everything else.")

(define ui/swatch
  (lambda (label color)
    (ui/elm "span"
      (list "class" "lb-swatch" "title" label)
      (list
        (ui/elm "b" (list "style" (string-append "background:" color))
          (list))
        (ui/text (string-append label " " color)))))
  "One palette swatch chip: a colored dot + the token name. The
lookbook face of the DESKTOP THEME itself -- the ui rail root's
palette row element (ui/palette composes one chip per token).")

(define ui/palette
  (lambda ()
    (ui/elm "div" (list "class" "lb-palette")
      (list
        (ui/swatch "sel-accent" (glochid/tg "accent" "#7aa2f7"))
        (ui/swatch "bg" (glochid/tg "darker_background" "#0f1117"))
        (ui/swatch "panel" (glochid/tg "lighter_background" "#1b1f2a"))
        (ui/swatch "border" (glochid/tg "selection" "#2a2e3a"))
        (ui/swatch "ink" (glochid/tg "foreground" "#d8dee9"))
        (ui/swatch "muted" (glochid/tg "muted" "#6b7080"))
        (ui/swatch "ok" (glochid/tg "green" "#2f9e44"))
        (ui/swatch "warn"
          (glochid/tg "orange" (glochid/tg "yellow" "#e8590c")))
        (ui/swatch "danger" (glochid/tg "bright_red" "#c92a2a")))))
  "The ACTIVE THEME rendered as UI PRIMS: one ui/swatch per palette
token, each value resolved live from the active omarchy theme's
colors.toml (load-time resolve; HUP re-resolves after omarchy theme
set). Staged via the usual live variant call -- what you see in the
lookbook IS the theme the shell is wearing.")

;; ---- examples cards: the declaration (doc + formals + variants +
;; source; the card shape mirrors bind/get's doc+source row). The file
;; field names the GLOCHID layer, not a lib: these are host elements. ----

(define ui/railtab/examples
  (lambda ()
    (lb/obj
      (list
        "name" "ui/railtab"
        "doc" "One rail mode tab (glochid shell chrome): flips the railmode sentinel."
        "file" "glochid/lib/surface.ml (glochid layer)"
        "formals" (json-emit (list "label"))
        "variants" (json-emit
                     (list
                       (lb/obj (list "label" "windows"))
                       (lb/obj (list "label" "ui"))))
        "source"
        "(ui/railtab label) ->\n  (ui/elm \"button\" (list \"class\" \"fs-btn fs-railtab\"\n          \"hx-post\" \"/filmstrip/railmode\"\n          \"hx-vals\" (string-append \"{\\\"root\\\":\\\"\" label \"\\\"}\")\n          ...)\n    (list (ui/text label)))")))
  "The ui/railtab card.")

(define ui/start-entry/examples
  (lambda ()
    (lb/obj
      (list
        "name" "ui/start-entry"
        "doc" "One start-menu entry (glochid shell chrome): opens route as a pane."
        "file" "glochid/lib/surface.ml (glochid layer)"
        "formals" (json-emit (list "label" "route"))
        "variants" (json-emit
                     (list
                       (lb/obj (list "label" "chat" "route" "/"))
                       (lb/obj (list "label" "sessions" "route" "/sessions"))))
        "source"
        "(ui/start-entry label route) ->\n  (ui/elm \"button\" (list \"class\" \"fs-btn fs-start-entry\"\n          \"hx-post\" \"/filmstrip/open\"\n          \"hx-vals\" (string-append \"{\\\"route\\\":\\\"\" route \"\\\"}\")\n          ...)\n    (list (ui/text label)))")))
  "The ui/start-entry card.")

(define ui/bubble/examples
  (lambda ()
    (lb/obj
      (list
        "name" "ui/bubble"
        "doc" "One chat bubble (id pins the streamed swap target; empty id = static)."
        "file" "glochid/lib/surface.ml (glochid layer)"
        "formals" (json-emit (list "kind" "text" "id"))
        "variants" (json-emit
                     (list
                       (lb/obj (list "kind" "user" "text" "hello shell" "id" ""))
                       (lb/obj (list "kind" "llm" "text" "" "id" "t-3"))))
        "source"
        "(ui/bubble kind text id) ->\n  (ui/elm \"div\"\n    (if (string-eq id \"\")\n        (list \"class\" (string-append \"bubble \" kind))\n        (list \"class\" ... \"id\" id))\n    (list (ui/text text)))")))
  "The ui/bubble card -- the conditional-attr pattern: the def computes
attrs (declaration), never markup.")

(define ui/tool-line/examples
  (lambda ()
    (lb/obj
      (list
        "name" "ui/tool-line"
        "doc" "One tool trace line (label + text, both escaped)."
        "file" "glochid/lib/surface.ml (glochid layer)"
        "formals" (json-emit (list "label" "text"))
        "variants" (json-emit
                     (list
                       (lb/obj (list "label" "tool:" "text" "fs/ls docs"))))
        "source"
        "(ui/tool-line label text) ->\n  (ui/elm \"div\" (list \"class\" \"trace\")\n    (list (ui/text label) (ui/text \" \") (ui/text text)))")))
  "The ui/tool-line card.")

(define ui/turn-open/examples
  (lambda ()
    (lb/obj
      (list
        "name" "ui/turn-open"
        "doc" "Per-turn skeleton: thinking+tools fold + the streamed LLM bubble target."
        "file" "glochid/lib/surface.ml (glochid layer)"
        "formals" (json-emit (list "id"))
        "variants" (json-emit
                     (list
                       (lb/obj (list "id" "t-1"))))
        "source"
        "(ui/turn-open id) ->\n  (ui/elm \"div\" (list \"class\" \"turn\")\n    (list (ui/elm \"details\" (list \"class\" \"think\")\n            (list (ui/elm \"summary\" (list) ...)\n                  (ui/elm \"div\" (list \"id\" (string-append id \"-think\")) (list))))\n          (ui/elm \"div\" (list \"class\" \"bubble llm\" \"id\" id) (list))))")))
  "The ui/turn-open card -- the composite seed: three defs composed by
plain calls, the nesting visible in BOTH projections.")

(define ui/think-open/examples
  (lambda ()
    (lb/obj
      (list
        "name" "ui/think-open"
        "doc" "One reasoning-run fold (starts closed; deltas stream into #id)."
        "file" "glochid/lib/surface.ml (glochid layer)"
        "formals" (json-emit (list "id"))
        "variants" (json-emit
                     (list
                       (lb/obj (list "id" "t-1"))))
        "source"
        "(ui/think-open id) ->\n  (ui/elm \"details\" (list \"class\" \"tk\")\n    (list (ui/elm \"summary\" (list) (list (ui/text \"thinking\")))\n          (ui/elm \"div\" (list \"id\" id) (list))))")))
  "The ui/think-open card.")

(define ui/tool-open/examples
  (lambda ()
    (lb/obj
      (list
        "name" "ui/tool-open"
        "doc" "One tool-call fold (summary #id-sum; trace lines stream into #id)."
        "file" "glochid/lib/surface.ml (glochid layer)"
        "formals" (json-emit (list "id" "name"))
        "variants" (json-emit
                     (list
                       (lb/obj (list "id" "t-1" "name" "fs/ls"))))
        "source"
        "(ui/tool-open id name) ->\n  (ui/elm \"details\" (list \"class\" \"tool\")\n    (list (ui/elm \"summary\" (list \"id\" (string-append id \"-sum\"))\n            (list (ui/text (string-append \"tool: \" name))))\n          (ui/elm \"div\" (list \"id\" id) (list))))")))
  "The ui/tool-open card.")

(define ui/turn-error/examples
  (lambda ()
    (lb/obj
      (list
        "name" "ui/turn-error"
        "doc" "One turn error line (trace-styled, bracketed, escaped)."
        "file" "glochid/lib/surface.ml (glochid layer)"
        "formals" (json-emit (list "text"))
        "variants" (json-emit
                     (list
                       (lb/obj (list "text" "model request failed"))))
        "source"
        "(ui/turn-error text) ->\n  (ui/elm \"div\" (list \"class\" \"trace\")\n    (list (ui/text (string-append \"[turn error: \" text \"]\"))))")))
  "The ui/turn-error card.")

(define ui/swatch/examples
  (lambda ()
    (lb/obj
      (list
        "name" "ui/swatch"
        "doc" "One palette swatch chip (dot + token name) -- the theme's lookbook row."
        "file" "glochid/lib/surface.ml (glochid layer)"
        "formals" (json-emit (list "label" "color"))
        "variants" (json-emit
                     (list
                       (lb/obj (list "label" "ink" "color" (glochid/tg "foreground" "#d8dee9")))
                       (lb/obj (list "label" "accent" "color" (glochid/tg "accent" "#7aa2f7")))))
        "source"
        "(ui/swatch label color) ->\n  (ui/elm \"span\" (list \"class\" \"lb-swatch\" \"title\" label)\n    (list (ui/elm \"b\" (list \"style\" (string-append \"background:\" color)) (list))\n          (ui/text label)))")))
  "The ui/swatch card.")

(define ui/palette/examples
  (lambda ()
    (lb/obj
      (list
        "name" "ui/palette"
        "doc"
        "The active omarchy theme as UI prims: one ui/swatch per palette token (resolved at load; HUP re-resolves after omarchy theme set)."
        "file" "glochid/lib/surface.ml (glochid layer)"
        "formals" (json-emit (list))
        "variants" (json-emit (list))
        "source"
        "(ui/palette) ->\n  (ui/elm \"div\" (list \"class\" \"lb-palette\")\n    (list (ui/swatch \"sel-accent\" (glochid/tg \"accent\" ...))\n          (ui/swatch \"bg\" ...) ...))")))
  "The ui/palette card -- the composition seed for pickers: zero
formals, tokens resolved through glochid/tg.")

;; ---- inventory seam (HOST SEAM: libs/lookbook seeds + the glochid
;; elements above; redefining is exactly the seam's intended use) ----
(define lookbook/elements
  (lambda ()
    (append
      lb/seed-elements
      (list
        "ui/railtab" "ui/start-entry" "ui/bubble" "ui/tool-line"
        "ui/turn-open" "ui/think-open" "ui/tool-open" "ui/turn-error"
          "ui/swatch" "ui/palette" "ui/file-issue")))
  "Glochid's lookbook inventory: the lib seed elements + glochid's own
shell chrome. Every entry answers a <name>/examples card; the ui rail
root + /lookbook face read this one def.")

;; ---- 1. WORKSPACE BACKING: .glochid/workspace.json ----
;; Legacy .agent trees keep their store while .agent exists (existing
;; data stays readable and writable there); glochid trees + fresh
;; projects use .glochid (fs/write creates the parent). Resolved once
;; at boot: cwd is fixed for the process.
(define glochid/ws-file
  (if (fs/exists? ".glochid")
      ".glochid/workspace.json"
      (if (fs/exists? ".agent")
          ".agent/workspace.json"
          ".glochid/workspace.json")))
;; HOST SEAM 1 ride: the workspace row lives in the local fs. Legacy
;; .agent trees keep their store while .agent exists; glochid trees
;; and fresh projects use .glochid. The read/write pair registers
;; HERE at boot through the framework store-backend knob; the
;; framework ws-node/ws-save/save-all verbs route through the pair
;; and no verb body is copied anymore.
(filmstrip/register-store!
  (lambda ()
    (if (fs/exists? glochid/ws-file)
        (dict-set "{}" "body" (fs/read glochid/ws-file))
        ()))
  (lambda (fields)
    (seq (fs/write glochid/ws-file fields) fields)))

;; ---- 2a. THE SHEET IS A LAZY MEMO (lisp-interpreter.lazy-env-thunks) ---
;; The filmstrip sheet is static lib vocabulary, yet filmstrip/shell-html
;; re-built it as data AND css/render-ed it to text on EVERY shell render
;; (css/render-decls showed ~5k steps/render — small next to the trees,
;; but the habitat hit budget-death on exactly this pattern in
;; libs/layout 2026-12-30: layout/style-text was the fix; this is its
;; glochid twin). (delay ...) binds without evaluating; first touch
;; renders the sheet ONCE and memoizes for the session.
(define glochid/css-text
  (delay (css/render (filmstrip/css))))
"Rendered filmstrip stylesheet, memoized: (force glochid/css-text).
CAVEAT same as layout/style-text: redefining filmstrip/css live leaves
the memo stale until the session rehydrates."

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

;; ---- 2. THE CASCADE IS THE AGENT'S ----
(define glochid/apps
  (lambda ()
    (append
      (list
        (list "chat" "/chat")
        (list "code" "/code")
        (list "bindings" "/bindings")
        (list "lookbook" "/lookbook")
        (list "sessions" "/sessions")
        (list "settings" "/settings"))
      (glochid/fwd-apps)))
  "Column 0: glochid's interfaces + the habitat's app faces the
panes wear (glochid/fwd-apps), ALL route leaves
(borge/filmstrip.borg sidebar-main-dichotomy): every entry
open-or-focuses a main-window pane. Structure browsing is the rail
trees' job (code -> glochid/code-tree fs walk; prims ->
glochid/prims-tree over the habitat's grammar; ui -> glochid/ui-tree
over the lookbook's declared elements), never cascade crumbs.

The lib's filmstrip/apps is redefined TOO (not just browse): the app
list IS the cascade's column 0 AND the launcher's hint source
(filmstrip/launcher-hint) — if it stayed habitat vocabulary, the
launcher advertised /todos on a host with no /todos route.")

(define filmstrip/apps
  (lambda () (glochid/apps))
  "glochid's shell app list (HOST SEAM 2): glochid's interfaces, all
leaves. Redefined so any lib read of the app list (hint-routes) sees
host vocabulary, never habitat apps.")

(define filmstrip/browse
  (lambda (path)
    (cons (list (glochid/apps)) #f))
  "glochid's shell cascade (HOST SEAM 2): column 0 is glochid's
interfaces, all leaves — no deeper columns, no crumbs. The stored path
only ever names the picked route (entry-html wears it as fs-sel).")

;; ---- 2c. HABITAT PANES (glochid.borg habitat-panes, 2026-09-20) ----
;; The habitat's app faces are ordinary htmx fragment routes already;
;; glochid panes WEAR them: the gate forwards these prefixes to the
;; habitat with the operator's session (habitat/fwd-prefix at boot;
;; kernel paths can never register — the prim refuses). In-pane nav on
;; the habitat faces is anchor-shaped, so the pane shell carries
;; hx-boost: anchors become ajax swaps INTO the pane (one wrapper div
;; per app, id stable across swaps; hx-target keeps the wrapper's inner
;; being the pane). In-process pane OPEN (filmstrip/open pane-content)
;; rides the Nopales fwd faces over the habitat/fwd prim; browser
;; interactions ride the OCaml gate plane. Auth: ONE POST /login per
;; process (one argon2id verify); the pp_session cookie rides every
;; forward.
;; PREFIX REGISTRY: registered OCaml-side at boot (bin/main.ml, after
;; Surface.load) so this layer has no boot prim dependency; kernel paths
;; refuse by construction in Habitat.fwd_register_prefix. The routes
;; below are the habitat's vocabulary. DIALECT: no trailing docstrings
;; on sugar defines (they RETURN the docstring) -- docs in ;; here.

;; (cascade-label home-path) rows: the PATHS are the habitat's route
;; vocabulary (/todos -- the habitat built for full-page anchors),
;; never a glochid-side renaming.
(define glochid/fwd-apps
  (lambda ()
    (list
      (list "todo" "/todos")
      (list "notes" "/notes")
      (list "calendar" "/calendar"))))

;; one stable id per app so boosted anchors swap INTO the wrapper's
;; inner (the wrapper survives every swap); hx-boost catches anchors
;; AND un-attributed forms; explicit hx-* elements inside (todo's
;; check forms) keep their own targets.
(define (glochid/fwd-wrap app inner)
  (string-append
    "<div id=\"fwd-" app
    "\" hx-boost=\"true\" hx-target=\"#fwd-" app
    "\" hx-swap=\"innerHTML\">"
    inner
    "</div>"))

;; the merged param alist (query ++ body) as one query string -- the
;; forward pipes the browser's params through 1:1.
(define (glochid/fwd-qs)
  (list/foldl
    (lambda
      (acc kv)
      (string-append
        acc
        (if (string-eq acc "") "" "&")
        (filmstrip/url-enc (car kv))
        "="
        (filmstrip/url-enc (car (cdr kv)))))
    ""
    (request-params)))

;; the in-process face for one app: the habitat's fragment inside the
;; boost shell, loaded with the pane's own query params. Fail-soft --
;; a down bridge renders the muted setup card, the pane never 500s.
(define (glochid/fwd-face app)
  (render-html
    (guard
      (lambda (e)
        (string-append
          "<p class=\"muted\">habitat bridge: "
          (html-escape (as-str (nth e 1)))
          " -- set GLOCHID_HABITAT_URL/USER/PASS (or PP_URL/PP_USER/PP_PASS)."
          "</p>"))
      (glochid/fwd-wrap
        app
        (habitat/fwd
          (as-str (request-method))
          (car (cdr (car (list/filter
                           (lambda (a) (string-eq app (car a)))
                           (glochid/fwd-apps)))))
          (glochid/fwd-qs)
          "")))))

(define (glochid/fwd-todo) (glochid/fwd-face "todo"))
(define (glochid/fwd-notes) (glochid/fwd-face "notes"))
(define (glochid/fwd-calendar) (glochid/fwd-face "calendar"))

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
              (filmstrip/json-arr
                (glochid/fs-rows-of dir (+ depth 1) entries))))))
  "The eager threshold in one place: one readdir to COUNT the child
level, inline it only under the cap and below the depth bound; else
lazy (empty string, no kids field).")

(define glochid/fs-rows-of
  (lambda (path depth entries)
    (filmstrip/keep-nodes
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
threshold. filmstrip/keep-nodes drops the hidden markers in place.")

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
      (filmstrip/keep-nodes
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

;; ---- 2b2d. THE UI TREE (lookbook inventory; borge/filmstrip.borg
;; lookbook) ----
;; The DECLARED ui elements as tree rows: one leaf per element, label =
;; the def name, label word open-or-focuses the /lookbook pane (acme
;; Get). Variants live IN the pane (variant tabs), not as tree levels --
;; the tree is PICKING, the pane is the inspecting. No eager kids, no
;; deeper levels: the inventory is the <name>/examples cards, one
;; string list per level, fail-soft like every root (a bad card means a
;; no-card pane, never a broken tree).
(define glochid/ui-tree
  (lambda (path)
    (if (string-eq path "")
        (list/map
          (lambda (name)
            (dict-set*
              "{}"
              (list
                "label" name
                "kind" "leaf"
                "node" (string-append "elt/" name)
                "open"
                (string-append
                  "/lookbook?name=" (filmstrip/url-enc name))
                "verbs" "")))
          (lookbook/elements))
        (list)))
  "levelfn for the ui tree (root = declared lookbook elements), ON THE
SHARED NODE CONTRACT. lookbook/elements is the host inventory seam
(libs/lookbook default: the lib seeds; glochid may redefine); an
entry without a card still opens -- the pane renders the no-card
message (falsifiable inventory, never source sniffing).")

(define filmstrip/tree-roots
  (lambda ()
    (list
        (list "windows" "windows" filmstrip/windows-tree)
      (list "code" "code" glochid/code-tree)
      (list "prims" "prims" glochid/prims-tree)
      (list "ui" "ui" glochid/ui-tree)))
  "glochid's rail tree roots (windows / files / prims / ui). Redefining
the lib's default host seam; the renderer + endpoints stay shared.
The ui root inventories the DECLARED lookbook elements (the
<name>/examples cards -- examples ARE the declaration), never guesses
from source sniffing.")

(define glochid/railtabs
  (lambda ()
    (list/map
      (lambda (r)
        (lookbook/html (ui/railtab (as-str (car r)))))
      (filmstrip/tree-roots)))
  "The rail mode tabs: one word per tree root (mode names are CONTENT
labels, not verbs -- they stay words, like the cascade's app names).
DECLARED ELEMENTS: each tab is a ui/railtab value projected through
lookbook/html -- no inline markup here anymore. POST
/filmstrip/railmode flips the #fs-railmode sentinel -- the SAME :has
hiding desktops use, so mode flips keep every tree's expansion DOM
alive.")

(define glochid/start-menu
  (lambda ()
    (html/tag
      "details"
      (html/attrs
        (list
          (html/attr "id" "fs-start")
          (html/attr "class" "fs-start")))
      (html/join
        (list
          (html/tag
            "summary"
            (html/attrs
              (list
                (html/attr "class" "fs-btn fs-start-word")
                (html/attr "title" "start: open one of the glochid surfaces")
                (html/attr "aria-label" "start menu")))
            (html/text "start"))
          (html/tag
            "div"
            (html/attrs
              (list (html/attr "class" "fs-start-menu")))
            (html/join
              (list/map
                (lambda (app)
                  (lookbook/html
                    (ui/start-entry (car app) (car (cdr app)))))
                (glochid/apps))))))))
  "The start menu lives on the rail (B: replaces the cascade/launchbar
column, 2026-09-20 operator decision). A zero-JS <details> fold; the
cascade column is gone from glochid's shell; /filmstrip/cascade stays
routed for the lib contract, filmstrip/apps stays HOST SEAM 2.")

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
          (list (glochid/start-menu))
          (list (html/span (html/attr "class" "fs-title") (html/text "rail")))
          (glochid/railtabs)
          (list (glochid/keys-hint))))))
  "The rail mode controlbox (one word per tree root, per
filmstrip.borg sidebar-main-dichotomy -- the axis toggle word was
retired, BORGE-1789879934: sidebar-only shell) + the keyboard-hint
glyph (the shortcut map is the glyph's title tooltip).")

;; ---- 2e. KEYBOARD SHORTCUTS (borge/glochid.borg keyboard-shortcuts:
;; the filmstrip zero-JS hotkeys mechanism, glochid's chord set) ----
;; THE KEYBOARD MAP IS ONE TABLE restated as body sentinels: rows 1-4
;; restate the lib's alt+arrows (focus / row move) verbatim -- a capture
;; of filmstrip/key-sentinels would not be reload-safe (each load would
;; nest the join deeper), and one table reads whole. The glochid rows:
;; alt+1/2/3 jump desktops, alt+n starts a new
;; session, alt+w/p/s drive the FOCUSED pane (close / pin / single),
;; each POSTing the same route the pointing UI uses. The guard clause
;; keeps a chord typed into the chat prompt (or any input) inert.
(define glochid/keyguard
  (string-append
    "document.activeElement.tagName!='INPUT'"
    "&&document.activeElement.tagName!='TEXTAREA'"
    "&&document.activeElement.tagName!='SELECT'")
  "Guard clause spliced into the trigger conditions of the glochid
shortcuts: a chord typed into the chat prompt (or any input/textarea/
select) never fires the shell. The lib's alt+arrow rows keep their
original conditions (restated verbatim, no guard applied).")

(define glochid/shortcuts
  (lambda ()
    (list
      ;; the lib's four alt+arrows (restated; condition / url / vals / guard)
      (list "altKey&&!shiftKey&&key=='ArrowLeft'" "/filmstrip/focus" "{\"dir\":\"left\"}" "")
      (list "altKey&&!shiftKey&&key=='ArrowRight'" "/filmstrip/focus" "{\"dir\":\"right\"}" "")
      (list "altKey&&shiftKey&&key=='ArrowLeft'" "/filmstrip/move" "{\"dir\":\"left\"}" "")
      (list "altKey&&shiftKey&&key=='ArrowRight'" "/filmstrip/move" "{\"dir\":\"right\"}" "")
      ;; the glochid layer: desktops / session / focused pane
      (list "altKey&&key=='1'" "/filmstrip/desktop" "{\"d\":\"0\"}" glochid/keyguard)
      (list "altKey&&key=='2'" "/filmstrip/desktop" "{\"d\":\"1\"}" glochid/keyguard)
      (list "altKey&&key=='3'" "/filmstrip/desktop" "{\"d\":\"2\"}" glochid/keyguard)
            (list "altKey&&key=='n'" "/filmstrip/new" "{}" glochid/keyguard)
      (list "altKey&&key=='w'" "/filmstrip/keys/close" "{}" glochid/keyguard)
      (list "altKey&&key=='p'" "/filmstrip/keys/pin" "{}" glochid/keyguard)
      (list "altKey&&key=='s'" "/filmstrip/keys/single" "{}" glochid/keyguard)))
  "THE KEYBOARD MAP, one row per chord: (condition url vals guard).
Rows 1-4 are filmstrip/key-sentinels (alt+arrows: focus, row move);
the rest are glochid's: alt+1/2/3 desktops, alt+n new
session, alt+w/p/s close/pin/single at the FOCUSED pane. A row edit
adds a chord; the renderer below splices the guard into the condition
and the sentinel POSTs the same route the pointing UI does.")

;; Keyboard: rows 1-4 ride the framework registry defaults (the same
;; alt-arrow rows the wholesale renderer restated with an empty
;; guard); only the glochid OWN rows append at boot through the
;; framework register-keys! seam. The guard still splices into the
;; trigger condition exactly as the old renderer did, and the row
;; order is preserved (framework defaults first, extras after).
(filmstrip/register-keys!
  (list/map
    (lambda (row)
      (let ((condn (car row))
            (url (car (cdr row)))
            (vals (car (cdr (cdr row))))
            (gcond (car (cdr (cdr (cdr row))))))
        (list
          url
          vals
              (string-append "keydown[" (car row)
                (if (string-eq gcond "") "" (string-append "&&" gcond))
                  "] from:body"))))
                  ;; rows 5+ ride the boot append (rows 1-4 restate the
                  ;; framework's alt+arrow defaults; the lib registry
                  ;; already carries those, see filmstrip/key-rows.
                  (cdr (cdr (cdr (cdr (glochid/shortcuts)))))))

;; ---- 2e2. FOCUSED-PANE VERBS (alt+w/p/s land here) ----
;; The shell has no JS to read the DOM, and the focused pane is
;; DECLARED workspace state anyway (the focus index in the ws row, the
;; same index alt+right heals). A keys route resolves the focus index
;; into a pane id and re-dispatches the lib verb handler with the id
;; injected (nested route/dispatch carries its own params -- the
;; pane-content seam), so the keyboard wears the EXACT pointing-UI
;; semantics and the lib owns the mutation + OOB grammar.
(define glochid/focused-id
  (lambda ()
    (let ((row (filmstrip/row)))
      (let ((p (filmstrip/pane-at (nth row 1) (nth row 2))))
        (if (null? p) "" (filmstrip/pane-id p)))))
  "The focused pane's id from the ws row, or the empty string when
nothing is focused (filmstrip/pane-at answers () out of range).")

(define glochid/focused-verb
  (lambda (verb)
    (let ((fid (glochid/focused-id)))
      (if (string-eq fid "")
          (render-html (filmstrip/oob-msg "glochid: no focused pane"))
          (render-html
            (route/dispatch
              (string-append "filmstrip/" verb)
              "POST"
              (string-append "/filmstrip/" verb)
              (list (cons "id" fid))
              (list (cons "hx-request" "true")))))))
  "Dispatch one pane verb at the FOCUSED pane; the rendered OOB answer
is re-tagged raw for the outer gate. No focused pane = a status line,
never a 500.")

(define glochid/keys-close
  (lambda () (glochid/focused-verb "close"))
  "POST /filmstrip/keys/close -- alt+w: shelve the focused pane. The
record survives (hx-preserve live DOM); focus heals exactly as the
head glyph's close does.")

(define glochid/keys-pin
  (lambda () (glochid/focused-verb "pin"))
  "POST /filmstrip/keys/pin -- alt+p: trade the focused pane's region
(main/rail); the pane MOVES containers on the region flip.")

(define glochid/keys-single
  (lambda () (glochid/focused-verb "single"))
  "POST /filmstrip/keys/single -- alt+s: toggle the focused pane's
one-up staging (v5 single).")

(define glochid/keys-hint
  (lambda ()
    (html/span
      (html/attrs
        (list
          (html/attr "class" "fs-keys")
          (html/attr "title"
            (string-append
              "alt+←/→ focus · alt+shift+←/→ move "
              "· alt+1/2/3 desktop · alt+n new session "
              "· alt+w close · alt+p pin · alt+s single"))))
      (html/text "⌨")))
  "The keyboard map as a muted glyph in the rail tabs row: the map is
the title tooltip (hover affordance), zero JS.")

(define glochid/railmode-set
  (lambda ()
    (let*
      ((root (as-str (request-param "root")))
        (rows (glochid/boxrows))
        (fresh? (not (glochid/boxrows-has root rows)))
        (target (filmstrip/tree-find-root (filmstrip/tree-roots) root)))
      (seq
        (filmstrip/ws-save
          (dict-set* (filmstrip/ws-fields)
            (list "railmode" root
                  "boxrows" (json-emit (glochid/boxrows-keep root rows)))))
        (render-html
          (html/join
            (list
              (filmstrip/oob-msg "")
              (filmstrip/railmode-sentinel root #t)
              ;; lazy-first-reveal (BORGE-1789950734): a FIRST-visit flip
              ;; OOBs the level rows into the freshly-revealed box; a
              ;; revisit sends no rows -- its expansion DOM stays alive.
              (if (and fresh? (not (null? target)))
                  (filmstrip/tree-oob target)
                  "")))))))
  "POST /filmstrip/railmode: root param -> persist mode + MRU boxrows;
OOB the sentinel (class rm-<root>; CSS shows only that tree); and a
FIRST-visit flip OOBs the level-0 rows into the newly-revealed box
(lazy-first-reveal) -- a revisit re-renders rows only in the stub-less
case, so expansion state is DOM between flips, as v4 promised.")

(define glochid/boxrows
  (lambda ()
    (let ((v (dict-get (filmstrip/ws-fields) "boxrows")))
      (if
        (if (null? v) #t (string-eq (as-str v) ""))
        (list)
        (json-parse (as-str v)))))
  "Visited-rail-box MRU: JSON array of root ids, newest first (ws field
boxrows; absent/empty = none visited this session lineage — save-all
carries the field as \"\" when unset, so the empty string answers the
empty set, never a parse error). Bounded by glochid/boxrows-keep so a
doc render materializes at most the active box + the last visited
one.")

(define glochid/boxrows-has
  (lambda (root rows)
    (if (null? rows) #f
        (if (string-eq (as-str (car rows)) root) #t
            (glochid/boxrows-has root (cdr rows)))))
  "Membership probe for the MRU set (string-eq, order-irrelevant).")

(define glochid/boxrows-keep
  (lambda (root rows)
    (cons root
      (if (null? rows) (list)
          (cons (as-str (car rows))
            (if (null? (cdr rows)) (list) (list))))))
  "MRU-2 projection: the flipped-to root in front + the previous first
entry. Returns a LIST (the caller rides json-emit for storage) — two
slots bound the doc render's eager-box cost no matter how many roots
the operator visits.")

(define glochid/tree-stub
  (lambda (root)
    (let*
      ((id (as-str (car root)))
        (title (as-str (car (cdr root)))))
      (html/tag
        "div"
        (html/attrs
          (list
            (html/attr "id" (string-append "fs-tree-" id))
            (html/attr "class" "fs-cbox fs-rmode")
            (html/attr "data-rmode" id)))
        (html/join
          (list
            (html/span (html/attr "class" "fs-title") (html/text title))
            (html/tag
              "ul"
              (html/attrs
                (list
                  (html/attr "id" (string-append "fs-tree-" id "-root"))
                  (html/attr "class" "fs-tr-kids fs-tr-root")))
              ""))))))
  "Lazy-first-reveal stub (BORGE-1789950734): the rail box chrome with
an EMPTY level-0 UL -- no rows materialized while the box is hidden
(the code arm's eager depth-1 kids ~115k steps stopped riding every
doc render). No collapse-all word by design: nothing to fold yet. The
UL id is the mutation OOB seam, so a first-visit flip fills it in
place.")

(define glochid/tree-boxes
  (lambda ()
    (let*
        ((active (filmstrip/railmode))
        (visited (glochid/boxrows)))
      (list/map
        (lambda (r)
          (let ((id (as-str (car r))))
            (if
              (if (string-eq id active) #t
                  (glochid/boxrows-has id visited))
              (filmstrip/tree-box r)
              (glochid/tree-stub r))))
        (filmstrip/tree-roots))))
  "Lazy-first-reveal (BORGE-1789950734): only the ACTIVE railmode's box
(+ the MRU-2 recently visited ones) materializes rows on a doc render;
the remaining hidden boxes answer empty chrome. The sentinel :has
hiding is unchanged -- the mode flip OOB fills stubs in place.")

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
    (dict-set rec "desktop" (as-str (filmstrip/ws-desktop))))
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
            "<!doctype html><html lang=en><head><meta charset=utf-8><title>glochid shell</title>"
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
        (html/tag "style" "" (force glochid/css-text))
        (html/tag
          "div"
          (html/attrs
            (list
              (html/attr "class" "fs-shell")
              (html/attr "id" "fs-shell")))
          (html/join
            (list
              (glochid/content-area panes focus)
              (filmstrip/railmode-sentinel (filmstrip/railmode) #f)
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
              (filmstrip/desk-sentinel (filmstrip/ws-desktop) #f)))))))
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
        (focus (filmstrip/heal-focus (nth row 1) (nth row 2))))
      (glochid/shell-doc
        frag
        (glochid/shell-inner (nth row 0) (nth row 1) focus))))
  "GET / and /filmstrip: the glochid shell. filmstrip's parts under
glochid chrome; zero JS beyond the vendored htmx assets. Focus heals
to the first visible pane at render when unset — v5: the strip shows
every live main pane of the desktop, so the heal picks the ring only
(the panes are visible regardless).")

;; ---- shell routes ----
;; ---- lookbook (GET/POST /lookbook + POST /lookbook/run;
;; borge/filmstrip.borg lookbook stanza) ----
;; The UI-element inspector over libs/lookbook. Components are ordinary
;; Nopales defs DECLARING UI VALUES; the card (<name>/examples) is the
;; declaration (doc + formals + variants + source); the stage renders a
;; LIVE variant call through the lookbook/eval seam and shows BOTH
;; projections (lookbook/html + lookbook/text) over the SAME value.
;; Pane-local discipline (I5): variant tabs re-render the pane body,
;; the param form re-renders ONLY the stage div. No do-it in v1 (pass
;; two adds edit-back-to-branch with its own stanza). The face is
;; glochid's because the eval seam + layout are host seams; the lib
;; (values, projections, cards) is host-free.
;; DIALECT NOTE: all defs here use the (define (name args) body) sugar
;; WITHOUT trailing docstrings -- (define (f x) body "doc") returns the
;; docstring (the filmstrip make-note gotcha). Docs live in ;; above.
(define (lookbook/head)
  (if (hx-request?)
      "<h1><span class=\"muted\">/lookbook</span></h1>"
      "<h1><a href=\"/\">glochid</a> <span class=\"muted\">/</span> <a href=\"/lookbook\">lookbook</a></h1>"))

;; The <name>/examples card through the lookbook/eval seam (guarded),
;; validated by the lib's lookbook/card: (card-text) or ().
(define (lookbook/fetch-card name)
  (let
    ((r (guard
          (lambda (e) "")
          (lookbook/eval (string-append "(" name "/examples)")))))
    (lookbook/card (as-str r))))

;; Text at key k of a flat (k v ...) kv list.
(define (lb/kv-get kvs k)
  (if (null? kvs)
      ""
      (if (string-eq (as-str (car kvs)) k)
          (as-str (car (cdr kvs)))
          (lb/kv-get (cdr (cdr kvs)) k))))

;; Param kvs for one render: defaults from the selected variant,
;; overridden field-by-field by form values p_<formal> (the POST gate
;; merges the form body into params; GET pane loads ride the variant).
(define (lookbook/variant-kvs card v)
  (let*
    ((formals (lb/field-list card "formals"))
     (variants (lb/field-list card "variants"))
     (n (length variants))
     (vv
       (cond
         ((null? variants) (list))
         ((< v 0) (car variants))
         ((>= v n) (car variants))
         (#t (nth variants v)))))
    (list/foldl
      (lambda
        (acc f)
        (let*
          ((req (request-param (string-append "p_" f)))
           (def (if (null? vv) "" (lb/get vv f)))
           (val (if (null? req) def (as-str req))))
          (append acc (list f val))))
      (list)
      formals)))

;; The variant's call source: (name "v1" ...). Params ride json-emit —
;; Nopales-reader-compatible escapes -- so quote-bearing values stay
;; literal.
(define (lb/stage-id name)
  (string-join (string-split name "/") "-"))
;; The stage id: the element name FLATTENED (/ -> -) -- url-enc keeps
;; escapes for route text, but an id rides a CSS selector in hx-target,
;; so it must be selector-clean.

(define (lb/src-walk kvs out)
  (if (null? kvs)
      out
      (lb/src-walk
        (cdr (cdr kvs))
        (string-append
          out
          " "
          (json-emit (as-str (car (cdr kvs))))))))
;; The variant's call source: (name "v1" ...). Params ride json-emit --
;; Nopales-reader-compatible escapes -- so quote-bearing values stay
;; literal. kvs is the FLAT (k v ...) list from variant-kvs.
(define (lookbook/call-src name kvs)
  (string-append "(" name (lb/src-walk kvs "") ")"))

;; The stage: the LIVE variant call (lookbook/eval), its html
;; projection on stage, and both other views folded beside it --
;; declared value + text projection + the card's source. One value,
;; three views, zero markup authored outside the projections.
(define (lookbook/stage-html card name v)
  (let*
    ((sid (string-append "lb-stage-" (lb/stage-id name)))
     (kvs (lookbook/variant-kvs card v))
     (src (lookbook/call-src name kvs))
     (res (lookbook/eval src))
     (err? (string-eq (lb/get res "t") "err"))
     (proj (if err? "" (lookbook/html res)))
     (txt (if err? "" (lookbook/text res 0))))
    (string-append
      "<div id=\"" sid "\" class=\"lb-stage\">"
      (if err?
          (string-append
            "<p class=\"muted\">lookbook: "
            (html-escape (lb/get res "v"))
            "</p>")
          (string-append
            "<div data-lb-projection=\"html\">" proj "</div>"))
      "<details class=\"lb-fold\"><summary>declared value</summary><pre>"
      (html-escape (as-str res))
      "</pre></details>"
      (if err?
          ""
          (string-append
            "<details class=\"lb-fold\"><summary>text projection</summary><pre>"
            (html-escape txt)
            "</pre></details>"))
      "<details class=\"lb-fold\"><summary>source</summary><pre>"
      (html-escape (lb/get card "source"))
      "</pre></details>"
      "</div>")))

;; The params form: one field per DECLARED formal (never sniffed),
;; POSTing a stage-only re-render (the hx-target pins the stage id --
;; the pane body around it never swaps, the I5 discipline).
(define (lookbook/form-html card name v)
  (let*
    ((formals (lb/field-list card "formals"))
     (kvs (lookbook/variant-kvs card v))
     (sid (string-append "lb-stage-" (lb/stage-id name))))
    (string-append
      "<form class=\"lb-form\" hx-post=\"/lookbook/run\""
      " hx-target=\"#" sid "\" hx-swap=\"outerHTML\">"
      "<input type=\"hidden\" name=\"name\" value=\"" (html-escape name) "\">"
      "<input type=\"hidden\" name=\"v\" value=\"" (as-str v) "\">"
      (html/join
        (list/map
          (lambda (f)
            (string-append
              "<label>"
              (html-escape f)
              " <input name=\"p_"
              (html-escape f)
              "\" value=\"" (html-escape (lb/kv-get kvs f)) "\"></label>"))
          formals))
      "<button class=\"fs-btn\" type=\"submit\">render</button>"
      "</form>")))

;; Variant tabs: one POST per variant, pane-local (closest .fs-body --
;; the pane re-renders with the variant selected). Variants stay
;; declared state; the ui tree lists ELEMENTS, never variants.
(define (lookbook/vtabs-html card name v)
  (let ((variants (lb/field-list card "variants")))
    (if (null? variants)
        ""
        (string-append
          "<div class=\"lb-vtabs\">"
          (html/join
            (filmstrip/map-indexed
              (lambda (vt i)
                (string-append
                  "<button class=\"fs-btn lb-vtab"
                  (if (= i v) " fs-sel" "")
                  "\""
                  " "
                  (html/attr "hx-post" "/lookbook")
                  " "
                  (html/attr "hx-target" "closest .fs-body")
                  " "
                  (html/attr "hx-swap" "innerHTML")
                  " "
                  (html/attr "hx-vals"
                    (string-append
                      "{\"name\":\"" name
                      "\",\"v\":\"" (as-str i) "\"}"))
                  ">" "variant " (as-str i) "</button>"))
              variants
              0))
          "</div>"))))

(define (lookbook/no-card name)
  (string-append
    "<p class=\"muted\">lookbook: no examples card for "
    (html-escape name)
    "</p>"
    "<p class=\"muted\">declare one:"
    " (define " (html-escape name) "/examples (lambda () <card dict>))"
    " -- EXAMPLES ARE THE DECLARATION (borge/filmstrip.borg lookbook;"
    " a card = name + doc + formals + variants + file + source).</p>"))

;; The pane body: name + doc + declaring file, variant tabs, params
;; form, stage. Bad route / missing card render the no-card message --
;; the pane never errors on the browser's behalf.
(define (lookbook/pane-body name v)
  (if (string-eq name "")
      (string-append
        "<p class=\"muted\">the lookbook: declared ui elements with both"
        " projections. Open an element from the ui rail tree, or pick one"
        " from the cascade.</p>")
      (let ((card (lookbook/fetch-card name)))
        (if (null? card)
            (lookbook/no-card name)
            (string-append
              "<h2>" (html-escape (lb/get (car card) "name")) "</h2>"
              "<p class=\"muted\">" (html-escape (lb/get (car card) "doc")) "</p>"
              "<p class=\"muted muted-file\">" (html-escape (lb/get (car card) "file")) "</p>"
              (lookbook/vtabs-html (car card) name v)
              (lookbook/form-html (car card) name v)
              (lookbook/stage-html (car card) name v))))))

;; GET/POST /lookbook: the pane fragment under HX-Request (variant tabs
;; POST here), the standalone page otherwise -- the code/bindings face
;; pattern.
(define (lookbook/page)
  (let*
    ((name (as-str (request-param "name")))
     (vv (request-param "v"))
     (v (if (null? vv) 0 (parse-int (as-str vv)))))
    (if (hx-request?)
        (render-html
          (string-append (lookbook/head) (lookbook/pane-body name v)))
        (glochid/layout "glochid -- lookbook"
          (string-append (lookbook/head) (lookbook/pane-body name v))))))

;; POST /lookbook/run: params re-render ONLY the stage div (the form's
;; hx-target; the pane around it never swaps -- the I5 discipline the
;; v4 review enforced on every fragment surface).
(define (lookbook/run)
  (let*
    ((name (as-str (request-param "name")))
     (vv (request-param "v"))
     (v (if (null? vv) 0 (parse-int (as-str vv))))
     (card (lookbook/fetch-card name)))
    (if (null? card)
        (render-html "<p class=\"muted\">lookbook: no card</p>")
        (render-html (lookbook/stage-html (car card) name v)))))

(route "/lookbook" "lookbook/page")
(route "/lookbook/run" "lookbook/run")

;; habitat-pane faces: the root routes for open-or-focus + route/lookup
;; sanity. Sub-paths ride the OCaml gate plane (fwd prefixes), never
;; the Nopales table.
(route "/todos" "glochid/fwd-todo")
(route "/notes" "glochid/fwd-notes")
(route "/calendar" "glochid/fwd-calendar")

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
(route "/filmstrip/move-to" "filmstrip/move-to")
(route "/filmstrip/width" "filmstrip/width")
(route "/filmstrip/pin" "filmstrip/pin")
(route "/filmstrip/single" "filmstrip/single")
(route "/filmstrip/tree" "filmstrip/tree")
(route "/filmstrip/tree/collapse" "filmstrip/tree-collapse")
(route "/filmstrip/railmode" "glochid/railmode-set")
(route "/filmstrip/desktop" "filmstrip/desktop-set")
(route "/filmstrip/new" "glochid/new")
(route "/filmstrip/resume" "glochid/resume")
(route "/filmstrip/keys/close" "glochid/keys-close")
(route "/filmstrip/keys/pin" "glochid/keys-pin")
(route "/filmstrip/keys/single" "glochid/keys-single")
|pp}

(** Boot-load the embedded Nopales packages + this surface into the
    shared env, registering the route-table routes. Idempotent-ish:
    re-evaluating defines redefines bindings live. *)
(* Inert defaults for the llm-config host prims (glochid/models,
    glochid/model, glochid/base-url, glochid/key-status) so the surface
    renders in non-serve contexts (tests, -p pipe evals).
    [Http_ui.serve] RE-REGISTERS them with live cfg backing — prim
    registration is last-write-wins on the same name. *)
let register_config_default_prims ~(rt : Runtime.t) : unit =
  Runtime.add_prim rt "glochid/models" (fun _ -> Ast.VList []);
  Runtime.add_prim rt "glochid/model" (fun _ -> Ast.VStr "");
  Runtime.add_prim rt "glochid/base-url" (fun _ -> Ast.VStr "");
  Runtime.add_prim rt "glochid/key-status" (fun _ -> Ast.VStr "not set");
  (* inert session prims (sessions-persist) so the pages render in
     non-serve contexts; Http_ui.serve / the REPL re-register with live
     store backing — prim registration is last-write-wins. *)
  Runtime.add_prim rt "session/name" (fun _ -> Ast.VStr "");
  Runtime.add_prim rt "session/list" (fun _ -> Ast.VStr "[]");
  Runtime.add_prim rt "session/data" (fun _ -> Ast.VStr "[]");
  Runtime.add_prim rt "session/new" (fun _ -> Ast.VStr "");
  Runtime.add_prim rt "session/resume" (fun _ -> Ast.VStr "")

(** (lookbook/eval src) — the lookbook face's eval seam (borge/
    filmstrip.borg lookbook): ONE source eval against the shared env
    with a fresh step budget — a variant call "(ui/button \"Save\"
    \"primary\")" or a card fetch "(ui/button/examples)". Callers run
    INSIDE a route eval, so this takes NO eval lock (same discipline
    as Host_seam.apply_value_raw; the eval mutex serializes whole
    requests) — and eval errors surface as an {"t":"err"} card the
    Nopales face renders, never a 500. A budget caps runaway seeds. *)
(* (omarchy/theme) — the host desktop-palette seam: the ACTIVE omarchy
   theme's canonical colors (~/.local/state/omarchy/current/theme/
   colors.toml, where `omarchy theme set` materializes the applied
   theme). Keys are STABLE across every omarchy theme (mode, accent,
   selection, muted, the background/foreground ladder, the color set
   and bright_ variants), so consumers theme from the key names,
   never from one theme's hexes. A quoted-string TOML-only line
   parse; FAIL-SOFT: a
   missing/unreadable file answers {} and the Nopales resolve falls
   back to the pre-theme hardcoded palette. Re-read on every call —
   HUP reload re-resolves the sheet, so `omarchy theme set X` + HUP
   rethemes glochid without a restart. *)
let omarchy_parse_colors_toml (s : string) : (string * string) list =
  s
  |> String.split_on_char '\n'
  |> List.filter_map (fun line ->
          let line = String.trim line in
          if
            line = ""
            || (String.length line > 0 && String.unsafe_get line 0 = '#')
          then None
          else
            match String.index_opt line '=' with
            | None -> None
            | Some i ->
                let k = String.trim (String.sub line 0 i) in
                let v0 =
                  String.trim
                    (String.sub line (i + 1) (String.length line - i - 1))
                in
                let n = String.length v0 in
                let v =
                  if n >= 2 && v0.[0] = '"' && v0.[n - 1] = '"' then
                    String.sub v0 1 (n - 2)
                  else v0
                in
                if k = "" || v = "" then None else Some (k, v))

let register_theme_prims ~(rt : Runtime.t) : unit =
  Runtime.add_prim rt "omarchy/theme" (fun _ ->
      let home = Option.value (Sys.getenv_opt "HOME") ~default:"." in
      let path =
        Filename.concat home ".local/state/omarchy/current/theme/colors.toml"
      in
      (try
         let ic = open_in_bin path in
         let s = really_input_string ic (in_channel_length ic) in
         close_in ic;
         Ast.VStr
           (Yojson.Safe.to_string
              (`Assoc
                (List.map (fun (k, v) -> (k, `String v))
                   (omarchy_parse_colors_toml s))))
       with _ -> Ast.VStr "{}"))

let register_lookbook_prims ~(rt : Runtime.t) : unit =
  Runtime.add_prim rt "lookbook/eval" (fun args ->
      match args with
      | [ Ast.VStr src ] ->
          (try
             Eval.eval_string_direct ~prims:rt.seam.prims rt.seam.env
               ~budget:(Eval.make_budget ()) src
           with e ->
             Ast.VStr
               ("{\"t\":\"err\",\"v\":\"eval failed: "
                ^ String.escaped (Printexc.to_string e)
                ^ "\"}"))
      | _ ->
          raise
            (Ast.Interp_error
               (Ast.Prim_error
                  ("lookbook/eval", "usage: (lookbook/eval src)", "bad-args"))))

let load ~(rt : Runtime.t) : unit =
  register_config_default_prims ~rt;
  register_theme_prims ~rt;
  register_lookbook_prims ~rt;
  (* Boot order (glochid_source header): libs, then the glochid layer
     (whose ws/browse/shell redefinitions must win over the filmstrip
     defaults via env late binding), then the glochid surface. *)
  Runtime.load_source ~rt Libs_source.html_lib;
  Runtime.load_source ~rt Libs_source.css_lib;
  Runtime.load_source ~rt Libs_source.hyper_lib;
  Runtime.load_source ~rt Libs_source.filmstrip_lib;
  (* libs/lookbook: declared ui values + the two projections (the
     lookbook face in surface_source rides it through lookbook/eval). *)
  Runtime.load_source ~rt Libs_source.lookbook_lib;
  Runtime.load_source ~rt glochid_source;
  Runtime.load_source ~rt surface_source
