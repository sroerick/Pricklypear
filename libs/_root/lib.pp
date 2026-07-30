;; package (root) -- exported from live branch system/main
;; Sync: scripts/pp_sync.py pull|push

(define __d
  (notes/doc/get "agents"))

(define __day
  (car (cdr (cdr (cdr (cdr (cdr (cdr __d))))))))

(define __inbox
  (get
    (quote note)
    "053c8ece-a61d-4a82-bb9c-ee99a8abe0b5"))

(define __kind
  (car (cdr (cdr (cdr (cdr (cdr __d)))))))

(define __media-body
  (string-append
    "Do NOT push multi-KiB binaries through POST /api/eval (64 KiB body cap). "
    "Use dedicated media endpoints (HTTP Basic or session). "
    "Write: POST /api/media (max 5 MiB). Allowed: image/png jpeg gif webp svg+xml application/pdf. "
    "Modes: (1) raw body with Content-Type set to the media type and optional X-Filename header. "
    "(2) JSON with content_type and data_b64 and optional filename. "
    "(3) multipart form field name=file. "
    "Success 201 returns ok id url content_type byte_size sha256. "
    "Read GET /media/UUID. List GET /api/media. Delete DELETE /api/media/UUID (author or admin). "
    "Browser GET /media and POST /media/upload. "
    "Client helper: python3 scripts/pp_media.py put ./shot.png using PP_URL PP_USER PP_PASS. "
    "curl raw: curl -u USER:PASS -X POST ORIGIN/api/media -H Content-Type:image/png -H X-Filename:shot.png --data-binary @shot.png. "
    "curl multipart: curl -u USER:PASS -F file=@shot.png ORIGIN/api/media. "
    "Lisp meta only: (media-meta id) (media-list) (media-url id) (media-delete id). "
    "(media-put ct b64 filename) only for tiny fixtures under 256 KiB. "
    "Reference /media/id URLs from notes or HTML (auth required to fetch)."))

(define __ol
  (notes/doc/outline __d))

(define __ol2
  (outline/update
    __ol
    "n-media"
    (lambda (n) (outline/set-body n __media-body))))

(define __t
  (outline/node*
    "root"
    "A"
    ""
    ""
    (list
      (outline/node*
        "c1"
        "C1"
        "b"
        ""
        (list (outline/node* "g" "G" "" "" (list))))
      (outline/node* "c2" "C2" "" "" (list)))))

(define __tags
  (car (cdr (cdr (cdr (cdr __d))))))

(define __title
  (car (cdr (cdr __d))))

(define _sid
  (workout/session-create-complete
    "free"
    ""
    (date-today)
    "smoke test"))

(define _tid
  (todo/create-list "TempDeleteMe2"))

(define add1
  (lambda (x) (+ x 1)))

(define add1-phase
  (lambda (x) (+ x 1)))

(define apropos
  (lambda
    (pat)
    (list/filter
      (lambda (n) (string-contains? (as-str n) (as-str pat)))
      (list-bindings))))

(define blank?
  (lambda (s) (if (null? s) #t (string-eq (as-str s) ""))))

(define body
  (dict-get r "body"))

(define deftwo
  (__macro__
    (base)
    (list
      (quote seq)
      (list
        (quote define)
        (string-append base "-a")
        (list (quote lambda) (list) "A"))
      (list
        (quote define)
        (string-append base "-b")
        (list (quote lambda) (list) "B")))))

(define deftwo2
  (__macro__
    (base)
    (list
      (quote seq)
      (list
        (quote define)
        (string-append base "-a")
        (list (quote lambda) (list) "A"))
      (list
        (quote define)
        (string-append base "-b")
        (list (quote lambda) (list) "B")))))

(define deftwo3
  (__macro__
    (base)
    (list
      (quote seq)
      (list
        (quote define)
        (string-append base "-a")
        (list (quote lambda) (list) (list (quote __str__) "A")))
      (list
        (quote define)
        (string-append base "-b")
        (list (quote lambda) (list) (list (quote __str__) "B"))))))

(define design-patterns-outline
  (outline/node*
    "root"
    "Design Patterns for PP Web Dev"
    "Analysis of current patterns and recommendations based on Paul Graham's Lisp writings and codebase inspection. See [[paul-graham-lisp-notes]] for the research."
    ""
    (list
      (outline/node*
        "s-good"
        "What's Already Good"
        "Patterns that are working well \226\128\148 don't change these."
        ""
        (list
          (outline/node*
            "s-good-layers"
            "Layered Architecture"
            "The dependency graph is already bottom-up: html/css \226\134\146 layout \226\134\146 menu/ui \226\134\146 app packages. This is Graham's 'series of layers, each acting as a language for the one above.' The rss build (xml \226\134\146 http \226\134\146 feed) is the textbook example."
            ""
            (list))
          (outline/node*
            "s-good-outline"
            "Pure Data + Accessors (outline package)"
            "Nodes are plain lists (id title body todo kids); everything accessed via outline/id, outline/title, outline/kids \226\128\148 never by position. Operations are pure functions returning new trees. Best-structured package in the system."
            ""
            (list))
          (outline/node*
            "s-good-crud"
            "Data-First CRUD"
            "dict-set* + create/update/find-rows \226\128\148 data as JSON dicts in Postgres, no ORM. Very Lispy."
            ""
            (list))
          (outline/node*
            "s-good-htmx"
            "HTMX Partials"
            "Outline toolbar hx-post to /partials/notes-node-op, hx-target #outline. Todo view swap. Modular, composable, server logic stays in Lisp."
            ""
            (list))
          (outline/node*
            "s-good-row"
            "Row/ID Convention"
            "(row-id row) / (row-fields row) accessors over (id . fields) pairs, consistent across packages."
            ""
            (list))))
      (outline/node*
        "s-recs"
        "Recommendations"
        "Seven patterns to adopt, ordered by impact."
        ""
        (list
          (outline/node*
            "s-rec-1"
            "1. Route/Handler Dispatch Abstraction [HIGH]"
            "Every page handler reinvents GET/POST dispatch. feed/page, notes/page, workout/home-page all write the same if/request-method check. Should be written once. Declare routes with :get :post :htmx keys; a generic dispatcher calls the right handler. Graham: 'you should never write (if (string-eq (request-method) POST)) more than once.'"
            ""
            (list)))))))

(define dict-set*
  (lambda
    (json kvs)
    (if
      (null? kvs)
      json
      (if
        (null? (cdr kvs))
        json
        (dict-set*
          (dict-set json (car kvs) (car (cdr kvs)))
          (cdr (cdr kvs)))))))

(define doc-smoke
  42)

(define dp-outline
  (outline/node*
    "root"
    "Design Patterns for PP Web Dev"
    "Analysis of current patterns and recommendations based on Paul Graham's Lisp writings and codebase inspection. See [[paul-graham-lisp-notes]] for the research."
    ""
    (list
      (outline/node*
        "s-good"
        "What's Already Good"
        "Patterns that are working well; don't change these."
        ""
        (list
          (outline/node*
            "s-good-layers"
            "Layered Architecture"
            "The dependency graph is already bottom-up: html/css \226\134\146 layout \226\134\146 menu/ui \226\134\146 app packages. This is Graham's series of layers, each acting as a language for the one above. The rss build (xml \226\134\146 http \226\134\146 feed) is the textbook example."
            ""
            (list))
          (outline/node*
            "s-good-outline"
            "Pure Data + Accessors (outline package)"
            "Nodes are plain lists (id title body todo kids); everything accessed via outline/id, outline/title, outline/kids, never by position. Operations are pure functions returning new trees. Best-structured package in the system."
            ""
            (list))
          (outline/node*
            "s-good-crud"
            "Data-First CRUD"
            "dict-set* + create/update/find-rows: data as JSON dicts in Postgres, no ORM. Very Lispy."
            ""
            (list))
          (outline/node*
            "s-good-htmx"
            "HTMX Partials"
            "Outline toolbar hx-post to /partials/notes-node-op, hx-target #outline. Todo view swap. Modular, composable, server logic stays in Lisp."
            ""
            (list))
          (outline/node*
            "s-good-row"
            "Row/ID Convention"
            "(row-id row) / (row-fields row) accessors over (id . fields) pairs, consistent across packages."
            ""
            (list))))
      (outline/node*
        "s-recs"
        "Recommendations"
        "Seven patterns to adopt, ordered by impact."
        ""
        (list
          (outline/node*
            "s-rec-1"
            "1. Route/Handler Dispatch Abstraction [HIGH]"
            "Every page handler reinvents GET/POST dispatch. Should be written once. Declare routes with :get :post :htmx keys; a generic dispatcher calls the right handler. Graham: you should never write (if (string-eq (request-method) POST)) more than once."
            ""
            (list
              (outline/node*
                "s-rec-1a"
                "Current: hand-rolled in every package"
                "(define feed/page (lambda () (if (string-eq (request-method) \"POST\") (feed/handle-post) (feed/get-page)))) \226\128\148 repeated in notes, workout, weight, capture, plan, flash."
                ""
                (list))
              (outline/node*
                "s-rec-1b"
                "Recommended: data-driven route table"
                "(route \"/feed\" :get feed/get-page :post feed/handle-post :htmx feed/partial) \226\128\148 the dispatcher handles method routing, HTMX branching, and redirect-after-POST. Pages become declarations, not functions."
                ""
                (list))))
          (outline/node*
            "s-rec-2"
            "2. Action Dispatch Table [HIGH]"
            "POST handlers use deeply nested if/if/if chains. todo/handle-post is 8 levels deep. notes/ui/handle-post uses cond (better but linear). Actions should be a flat table of name \226\134\146 handler-function pairs. Shorter, extensible, inspectable."
            ""
            (list
              (outline/node*
                "s-rec-2a"
                "Current: nested ifs"
                "(if (string-eq action \"cycle\") ... (if (string-eq action \"delete\") ... (if (string-eq action \"archive\") ...))) \226\128\148 8 levels in todo."
                ""
                (list))
              (outline/node*
                "s-rec-2b"
                "Recommended: flat table + generic dispatcher"
                "(define todo/actions (list (list \"cycle\" todo/cycle) (list \"delete\" todo/delete) ...)) then (dispatch todo/actions action). Actions are data \226\128\148 you can enumerate them, build a help page, add new ones by adding entries."
                ""
                (list))))
          (outline/node*
            "s-rec-3"
            "3. Separate Data Access from HTML Rendering [HIGH]"
            "Many functions do both: feed/feed-html extracts row fields AND renders HTML. workout/session-li queries the DB inside the renderer. weight/chart-svg does math + SVG + min/max + coordinates in one lambda. Lower layers (data) shouldn't know about upper layers (HTML)."
            ""
            (list
              (outline/node*
                "s-rec-3a"
                "Split each fn into data + presentation"
                "feed/feed-fields: pure data extraction (testable via eval). feed/feed-html: pure HTML builder (takes clean data). Composition at page level: (list/map feed/feed-html (list/map feed/feed-fields (feed/list)))."
                ""
                (list))
              (outline/node*
                "s-rec-3b"
                "Why it matters"
                "When schema changes, you touch only the data fn. When layout changes, you touch only the view fn. The data fn is trivially testable from the eval endpoint. This is Graham's layering: modifying lower layers (data) should be rare and serious; modifying upper layers (templates) should be quick and safe."
                ""
                (list))))
          (outline/node*
            "s-rec-4"
            "4. Form Builder DSL [MEDIUM]"
            "Every form is hand-built with html/form + html/text-input + html/label + html/hidden + html/button. feed/subscribe-form, weight/form, notes/ui/create-form, notes/capture-form, workout/start-bar (40 lines!), flash/rating-form \226\128\148 all repeat the same boilerplate. Graham: this is a language waiting to be written. Recommended: declare forms as data, (form/make \"/feed\" (list (form/hidden \"action\" \"subscribe\") (form/text \"url\") (form/submit \"Add\")))."
            ""
            (list))
          (outline/node*
            "s-rec-5"
            "5. Resolve capture Package [MEDIUM]"
            "capture/ package (2 bindings) is a demo that creates capture_template rows nothing reads. notes/ already has a full working capture system. Either delete capture/ or wire it up as the template-definition layer that notes/capture consumes."
            ""
            (list))
          (outline/node*
            "s-rec-6"
            "6. Consistent Error Convention [MEDIUM]"
            "feed/subscribe returns \"already-subscribed\" (string). chat/say-in returns body on success or error string. Some fns return () on failure. notes/ui/do-save redirects on error. No way to programmatically distinguish success from failure without string-sniffing. Recommendation: (ok . result) / (err . msg) pairs, or return result on success and () on failure with a last-error side channel."
            ""
            (list))
          (outline/node*
            "s-rec-7"
            "7. Naming Conventions [LOW]"
            "Handler naming is inconsistent: feed/handle-post, notes/ui/handle-post, workout/handle-home-post. View vs data fn naming varies. Recommend: pkg/page (main handler), pkg/handle-post (POST), pkg/view-* (pure HTML), pkg/get-* (pure data), pkg/partial (HTMX)."
            ""
            (list))))
      (outline/node*
        "s-graham"
        "The Graham Principle"
        "Each recommendation is a case of write the language, not the program. The if/request-method check should be written once in a dispatcher, not forty times. The nested-if action dispatch should be a data-driven table. Hand-built forms should be a DSL. Viaweb's editor was 20-25% macros because Graham kept finding these patterns and encoding them into the language itself. The layers (html \226\134\146 layout \226\134\146 app) are already there; what's missing is pushing more of the boilerplate down into the language layer."
        ""
        (list
          (outline/node*
            "s-graham-quote"
            "Key quote from BBN talk"
            "Rtml was our upgrade path... Having that extra layer of abstraction in our software gave us a big advantage over competitors. It made the design of our software much cleaner... Modifying stuff in lower layers (Rtml itself) was a serious matter to be done rarely. Modifying the top layers (template code) was something you could do quickly."
            ""
            (list))
          (outline/node*
            "s-graham-kw"
            "Keyword parameters lesson"
            "Graham's biggest regret in Rtml: not making every operator take keyword parameters. If I wanted to add another dimension to the behavior of one of the operators, I could just add a new keyword parameter, and everyone's existing templates would continue to work. Nopales lacks keyword params, but the opts-dict pattern can serve the same purpose."
            ""
            (list)))))))

(define dp-outline-1
  (outline/node*
    "root"
    "Design Patterns for PP Web Dev"
    "Analysis of current patterns and recommendations based on Paul Graham's Lisp writings and codebase inspection. See [[paul-graham-lisp-notes]] for the research."
    ""
    (list
      (outline/node*
        "s-good"
        "What's Already Good"
        "Patterns that are working well; don't change these."
        ""
        (list
          (outline/node*
            "s-good-layers"
            "Layered Architecture"
            "The dependency graph is already bottom-up: html/css to layout to menu/ui to app packages. This is Graham's series of layers, each acting as a language for the one above. The rss build (xml to http to feed) is the textbook example."
            ""
            (list))
          (outline/node*
            "s-good-outline"
            "Pure Data + Accessors (outline package)"
            "Nodes are plain lists (id title body todo kids); everything accessed via outline/id, outline/title, outline/kids, never by position. Operations are pure functions returning new trees. Best-structured package in the system."
            ""
            (list))
          (outline/node*
            "s-good-crud"
            "Data-First CRUD"
            "dict-set* + create/update/find-rows: data as JSON dicts in Postgres, no ORM. Very Lispy."
            ""
            (list))
          (outline/node*
            "s-good-htmx"
            "HTMX Partials"
            "Outline toolbar hx-post to /partials/notes-node-op, hx-target #outline. Todo view swap. Modular, composable, server logic stays in Lisp."
            ""
            (list))
          (outline/node*
            "s-good-row"
            "Row/ID Convention"
            "(row-id row) / (row-fields row) accessors over (id . fields) pairs, consistent across packages."
            ""
            (list)))))))

(define dp-outline-2
  (outline/node*
    "root"
    "Design Patterns for PP Web Dev"
    "Analysis of current patterns and recommendations."
    ""
    (list
      (outline/node*
        "s-good"
        "What's Already Good"
        "Working patterns."
        ""
        (list
          (outline/node*
            "s-good-layers"
            "Layered Architecture"
            "Bottom-up dependency graph."
            ""
            (list))))
      (outline/node*
        "s-recs"
        "Recommendations"
        "Seven patterns to adopt, ordered by impact."
        ""
        (list
          (outline/node*
            "s-rec-1"
            "1. Route/Handler Dispatch Abstraction [HIGH]"
            "Every page handler reinvents GET/POST dispatch. Should be written once."
            ""
            (list
              (outline/node*
                "s-rec-1a"
                "Current: hand-rolled in every package"
                "feed/page, notes/page, workout/home-page all write the same if/request-method check."
                ""
                (list))
              (outline/node*
                "s-rec-1b"
                "Recommended: data-driven route table"
                "(route \"/feed\" :get feed/get-page :post feed/handle-post) \226\128\148 dispatcher handles routing."
                ""
                (list))))
          (outline/node*
            "s-rec-2"
            "2. Action Dispatch Table [HIGH]"
            "POST handlers use deeply nested if/if/if chains. todo/handle-post is 8 levels deep."
            ""
            (list
              (outline/node*
                "s-rec-2a"
                "Current: nested ifs"
                "8 levels of nesting in todo/handle-post."
                ""
                (list))
              (outline/node*
                "s-rec-2b"
                "Recommended: flat table + generic dispatcher"
                "(define todo/actions (list (list \"cycle\" todo/cycle) (list \"delete\" todo/delete))) then (dispatch todo/actions action)."
                ""
                (list)))))))))

(define e1
  (outline/node*
    "e1"
    "Capability effects"
    "Prickly Pear uses an algebraic-effects-style capability system to constrain what an eval call can do. Each eval request carries an effects-allow list. Known capabilities: Pure, ReadDb (get, find-row/rows, rows, find-text, find-random), WriteDb (create, update, update-field, delete), Publish (define, route, public-route), Html (ui functions). Dashboard agents get ReadDb+Html but NOT WriteDb or Publish \226\128\148 the compile-time governance model for agent-native BI."
    ""
    (list)))

(define e2
  (outline/node*
    "e2"
    "Budget: 100K step limit"
    "Every eval call runs with a budget of 100000 steps (default_limit, eval.ml line 8435). Each eval step (variable lookup, App, Prim dispatch, car, cdr, let binding, macro expansion) calls tick() which raises Budget_exceeded at limit+1. OCaml built-ins do NOT tick the budget \226\128\148 only the call site counts as 1 step. Each call gets its own budget. Budget exceeded aborts only that call; the session continues. Budget is a safety net, not a performance governor \226\128\148 fix via OCaml migration, not raising the limit."
    ""
    (list)))

(define e3
  (outline/node*
    "e3"
    "Cooperative stepped evaluation"
    "The eval loop is stepped: each Prim invocation yields back to the images event loop. This prevents any single eval from wedging the image. Combined with per-call budgeting, it gives cooperative multitasking without preemption. No server-side cron exists \226\128\148 everything is request-driven with lazy timestamp checks for scheduled work."
    ""
    (list)))

(define e4
  (outline/node*
    "e4"
    "select-for-update and mutex poisoning"
    "select-for-update does SELECT FOR UPDATE row lock. Meaningful only inside with-transaction. Calling outside a transaction can poison Eio mutexes (observed 2026-07-22: Eio mutex Poisoned cascaded into 401 auth failures for 20 min). Action: make it fail cleanly outside transactions. with-transaction form: (with-transaction (lambda () body)). Internal name __with_transaction__ \226\128\148 OCaml built-in. Nested transactions raise clean errors."
    ""
    (list)))

(define e5
  (outline/node*
    "e5"
    "Lisp vs OCaml boundary"
    "All roam/, notes/, wiki/, outline/*, calendar/*, flash/* are Lisp (verified: function-source returns non-empty). Even list/map, list/filter, list/foldl are Lisp \226\128\148 every iteration burns budget. OCaml built-ins: data layer, agent/LLM, branching, routes, core primitives (car, cdr, +, string-eq, etc.), media, ssh. The separation: OCaml = data access + core language + IO. Lisp = application logic. Budget pain is entirely in the Lisp layer."
    ""
    (list)))

(define e6
  (outline/node*
    "e6"
    "Migration priorities"
    "P0: list/map, list/filter, list/foldl \226\128\148 single highest-leverage OCaml-ification. Every large-list operation across all namespaces gets cheaper. P1: roam/reindex \226\128\148 the number one budget killer for note writes. P2: outline/kids, outline/decode, outline/encode, outline/find \226\128\148 cascade impacts all three namespaces. P3: roam/parse-links, roam/backlinks. After P0-P2, 100K budget feels generous while still catching infinite loops."
    ""
    (list)))

(define expand-let*
  (lambda
    (bindings body)
    (list/foldl
      (lambda
        (inner binding)
        (list (quote let) (list binding) inner))
      (cons (quote seq) body)
      (reverse bindings))))

(define explode
  (lambda
    (src)
    (explode-format (explode-ast (read-string src) 0))))

(define explode-args
  (lambda
    (args depth)
    (if
      (null? args)
      (list)
      (if
        (pair? args)
        (let
          ((item (car args)))
          (if
            (pair? item)
            (append
              (explode-ast item depth)
              (explode-args (cdr args) depth))
            (append
              (list (list depth " " (as-str item)))
              (explode-args (cdr args) depth))))
        (list (list depth " " (as-str args)))))))

(define explode-ast
  (lambda
    (ast depth)
    (if
      (null? ast)
      (list (list depth "(" "()") (list depth ")" ""))
      (if
        (pair? ast)
        (append
          (list (list depth "(" (as-str (car ast))))
          (explode-args (cdr ast) (+ depth 1))
          (list (list depth ")" "")))
        (list (list depth " " (as-str ast)))))))

(define explode-format
  (lambda
    (pairs)
    (if
      (null? pairs)
      ""
      (let
        ((p (car pairs)))
        (string-append
          (as-str (car p))
          "|"
          (car (cdr p))
          "|"
          (car (cdr (cdr p)))
          "\n"
          (explode-format (cdr pairs)))))))

(define factorial
  (lambda (n) (if (<= n 1) 1 (* n (factorial (- n 1))))))

(define filter
  (lambda (p xs) (list/filter p xs)))

(define foldl
  (lambda (f init xs) (list/foldl f init xs)))

(define foldr
  (lambda (f init xs) (list/foldr f init xs)))

(define foo
  (__macro__ (x) x))

(define hdrs
  (dict-set "{}" "If-None-Match" "\"abc\""))

(define hi-page
  (lambda
    ()
    (render-html
      "<h1>Hello</h1><p>Your first Pricklypear page. Edit me at /edit/hi-page.</p>")))

(define implode
  (lambda
    (text)
    (implode-simple (string-split text "\n") "")))

(define implode-close-all
  (lambda
    (fn-stack)
    (if
      (null? fn-stack)
      ""
      (string-append ")" (implode-close-all (cdr fn-stack))))))

(define implode-close-to
  (lambda
    (fn-stack depth)
    (if
      (null? fn-stack)
      (list "" fn-stack)
      (if
        (>= (car fn-stack) depth)
        (let
          ((rest (implode-close-to (cdr fn-stack) depth)))
          (list (string-append ")" (car rest)) (car (cdr rest))))
        (list "" fn-stack)))))

(define implode-parse-line
  (lambda
    (line)
    (let
      ((parts (string-split line "|")))
      (if
        (null? (cdr parts))
        (list 0 " " "")
        (let
          ((depth (parse-int (car parts)))
            (marker (car (cdr parts)))
            (token
              (if
                (null? (cdr (cdr parts)))
                ""
                (car (cdr (cdr parts))))))
          (list depth marker token))))))

(define implode-simple
  (lambda
    (lines result)
    (if
      (null? lines)
      result
      (let
        ((line (car lines)) (rest (cdr lines)))
        (if
          (or (null? line) (string-eq line ""))
          (implode-simple rest result)
          (let
            ((parsed (implode-parse-line line)))
            (let
              ((marker (car (cdr parsed)))
                (token (car (cdr (cdr parsed)))))
              (if
                (string-eq marker "(")
                (implode-simple
                  rest
                  (string-append
                    result
                    (if (string-eq result "") "" " ")
                    "("
                    token))
                (if
                  (string-eq marker ")")
                  (implode-simple rest (string-append result ")"))
                  (implode-simple rest (string-append result " " token)))))))))))

(define inc-smoke
  (lambda (x) (+ x 1)))

(define let*
  (__macro__
    (bindings body)
    (if
      (null? bindings)
      body
      (quasiquote
        (let
          ((unquote (car bindings)))
          (let* (unquote (cdr bindings)) (unquote body)))))))

(define let1
  (__macro__
    (name val body)
    (list (quote let) (list (list name val)) body)))

(define let1q
  (__macro__
    (name val body)
    (quasiquote
      (let (((unquote name) (unquote val))) (unquote body)))))

(define let1r
  (__macro__
    (name val . body)
    (list
      (quote let)
      (list (list name val))
      (cons (quote seq) body))))

(define let1r2
  (__macro__
    (name val . body)
    (list
      (quote let)
      (list (list name val))
      (cons (quote seq) body))))

(define let2
  (__macro__
    (bindings . body)
    (if
      (null? bindings)
      (cons (quote seq) body)
      (list
        (quote let)
        (list (car bindings))
        (cons (quote let2) (cons (cdr bindings) body))))))

(define list-or-empty
  (__macro__
    (xs empty-msg item-fn)
    (quasiquote
      (if
        (null? (unquote xs))
        (html/p*
          (html/attr "class" "muted")
          (unquote empty-msg))
        (html/ul
          ""
          (html/join (list/map (unquote item-fn) (unquote xs))))))))

(define list-ref
  nth)

(define map
  (lambda (f xs) (list/map f xs)))

(define mywhen-test-1
  (lambda (t b) (if t b ())))

(define names
  (list-bindings))

(define pg-outline
  (outline/node*
    "root"
    "Paul Graham on Lisp"
    "Research notes on Paul Grahams writings about Lisp, compiled 2026-07-29. Sources: paulgraham.com essays (Beating the Averages, BBN Talk, Revenge of the Nerds, The Hundred-Year Language, Arc at 3 Weeks), the On Lisp preface, and Wikipedia on anaphoric macros. See also [[design-patterns-pp]] for how these ideas apply to Pricklypear development."
    ""
    (list
      (outline/node*
        "pg-overview"
        "Overview of Key Writings"
        "Major works and their themes."
        ""
        (list
          (outline/node*
            "pg-overview-table"
            "Key Writings"
            "On Lisp (1993): advanced Lisp techniques, bottom-up programming, first complete macro description. ANSI Common Lisp (1995): intro textbook. Beating the Averages (2001): Viaweb story, Lisp as secret weapon, the Blub Paradox. Lisp for Web-Based Applications / BBN Talk (2001): specific technical advantages of Lisp in Viaweb. Revenge of the Nerds (2002): Lisp 9 ideas, power continuum, accumulator example. The Hundred-Year Language (2003): language evolution, small clean cores. Arc at 3 Weeks (2001): Arc design philosophy, brevity, pronouns. If Lisp is So Great (2001): why Lisp isnt more widely adopted."
            ""
            (list))))
      (outline/node*
        "pg-bta"
        "Beating the Averages: Viaweb"
        "The Viaweb story, competitive advantage, the secret weapon."
        ""
        (list
          (outline/node*
            "pg-bta-story"
            "The Viaweb Story"
            "Summer 1995: Graham + Robert Morris founded Viaweb, software to let end users build online stores via the web. Arguably the first web-based application. Written primarily in Lisp. Had 20-30 competitors, none could match their software."
            ""
            (list))
          (outline/node*
            "pg-bta-server"
            "Server-side = Language Freedom"
            "When writing software that only runs on your own servers, you can use any language you want. Desktop software had a bias toward the OS language (C). Web apps changed this: you control the server, you have the source to the language and OS, you fix incompatibilities yourself. This freedom is a double-edged sword: now you must choose wisely."
            ""
            (list))
          (outline/node*
            "pg-bta-rapid"
            "Rapid Development as Competitive Weapon"
            "Lisp was known for writing software quickly. Server apps magnify this: release the minute code is done. Graham could sometimes duplicate a competitors newly-announced feature within a day or two of the press release. By the time journalists called Viaweb, the feature already existed."
            ""
            (list))
          (outline/node*
            "pg-bta-secret"
            "The Secret Weapon"
            "Graham kept Lisp usage secret. In business, there is nothing more valuable than a technical advantage your competitors dont understand. Robert Morris later said it didnt matter: If they were that smart theyd already be programming in Lisp."
            ""
            (list))
          (outline/node*
            "pg-bta-jobs"
            "Reading Job Listings"
            "Graham evaluated competitors by job postings. Oracle experience = safe. C++/Java = not threatening. Perl/Python = a bit frightening (real hackers). Lisp hackers = the only thing that would have worried him."
            ""
            (list))
          (outline/node*
            "pg-bta-results"
            "Results"
            "End 1996: ~70 stores. End 1997: 500. Yahoo acquired Viaweb 1998: 1,070 users. As Yahoo Store: ~20,000 users. Yahoo later rewrote in C++/Perl but had to write a Lisp interpreter: the template source files are still Lisp code. Cf. Greenspuns Tenth Rule."
            ""
            (list))))
      (outline/node*
        "pg-blub"
        "The Blub Paradox"
        "The power continuum and why most programmers cant see it."
        ""
        (list
          (outline/node*
            "pg-blub-continuum"
            "The Power Continuum"
            "Programming languages are not equivalent. They fall along a continuum (or lattice) of abstractness/power. Turing equivalence is irrelevant: the kind of power programmers care about refers to features you could only get in the less powerful language by writing an interpreter for the more powerful language in it."
            ""
            (list))
          (outline/node*
            "pg-blub-paradox"
            "The Paradox Itself"
            "Hypothetical language Blub sits in the middle. Looking down at less powerful languages, the Blub programmer knows they are weaker. Looking up at more powerful languages, he doesnt realize hes looking up; they just look weird with hairy stuff. He thinks Blub is good enough because he thinks in Blub. By induction, only programmers who understand the most powerful language can see all the differences in power."
            ""
            (list))))
      (outline/node*
        "pg-nine"
        "The Nine Ideas"
        "What made Lisp different in 1958, and which ideas are still unique."
        ""
        (list
          (outline/node*
            "pg-nine-1"
            "1. Conditionals"
            "if-then-else. Fortran I only had conditional goto."
            ""
            (list))
          (outline/node*
            "pg-nine-2"
            "2. A Function Type"
            "Functions are data: first-class, stored in variables, passed as arguments."
            ""
            (list))
          (outline/node*
            "pg-nine-3"
            "3. Recursion"
            "Lisp was the first language to support it."
            ""
            (list))
          (outline/node*
            "pg-nine-4"
            "4. Dynamic Typing"
            "Variables are pointers; values have types, not variables."
            ""
            (list))
          (outline/node*
            "pg-nine-5"
            "5. Garbage Collection"
            "Introduced ~1960, now widely considered good."
            ""
            (list))
          (outline/node*
            "pg-nine-6"
            "6. Programs Composed of Expressions"
            "Everything returns a value. No statement/expression distinction. Fortran introduced this distinction because you couldnt nest statements; it spread to Algol and descendants despite no longer being necessary."
            ""
            (list))
          (outline/node*
            "pg-nine-7"
            "7. A Symbol Type"
            "Symbols are pointers to strings in a hash table. Equality by pointer comparison."
            ""
            (list))
          (outline/node*
            "pg-nine-8"
            "8. Code as Trees of Symbols and Constants"
            "Code is expressed directly in parse trees (lists). Still largely unique to Lisp. Responsible for Lisps strange appearance and its most distinctive features (macros)."
            ""
            (list))
          (outline/node*
            "pg-nine-9"
            "9. The Whole Language There All the Time"
            "No real distinction between read-time, compile-time, and runtime. You can compile or run code while reading, read or run code while compiling, and read or compile code at runtime. Still largely unique to Lisp."
            ""
            (list))
          (outline/node*
            "pg-nine-why"
            "Why Lisp Isnt Obsolete"
            "Lisp was not designed as a programming language. It emerged from a theoretical exercise by McCarthy (an alternative to the Turing Machine). Steve Russell noticed that McCarthys eval function could be implemented in machine code, creating a Lisp interpreter. Compare Lisp to Quicksort (1960), still the fastest general-purpose sort. Lisp is math, not technology. Mainstream languages have been converging toward Lisp ever since: Its 2002, and programming languages have almost caught up with 1958."
            ""
            (list))))
      (outline/node*
        "pg-onlisp"
        "On Lisp: Bottom-Up Programming"
        "The central thesis, macros, and why you cant just write a library."
        ""
        (list
          (outline/node*
            "pg-onlisp-thesis"
            "The Central Thesis"
            "Instead of just writing your program in Lisp, you can write your own language on Lisp, and write your program in that. Bottom-up programming: write a program as a series of layers, each acting as a programming language for the one above. The lowest layer is the base language; each higher layer adds abstractions until the top layer is practically a domain-specific language. Examples: X Windows, TeX."
            ""
            (list))
          (outline/node*
            "pg-onlisp-macros"
            "Macros: Programs That Write Programs"
            "A Lisp macro is a function that takes unevaluated code (s-expressions) as input and returns transformed code. Because Lisp code IS Lisp data (lists), you can manipulate it programmatically. A Lisp macro can be anything from an abbreviation to a compiler for a new language. Viaweb editor source was 20-25% macros: at least 20-25% of the code is doing things that you cant easily do in any other language."
            ""
            (list))
          (outline/node*
            "pg-onlisp-scope"
            "On Lisp Scope"
            "Covers: functional programming, closures, functions returning functions, macros (tutorial + advanced), variable capture (gensyms), generalized variables, anaphoric macros, read-macros, macro-defining macros, continuations, nondeterminism, ATN parsers, an embedded Prolog in ~180 lines, bottom-up reconstruction of CLOS. On Lisp deals mostly with the kinds of programs you could only write in Lisp."
            ""
            (list))
          (outline/node*
            "pg-onlisp-library"
            "Why You Cant Just Write a Library"
            "Bottom-up programming isnt just about utility functions. Its about extending the language itself: adding new operators, new control structures, new abstractions that are indistinguishable from built-in ones. In most languages you can add libraries; in Lisp you can add language features. The dividing line between library and language dissolves."
            ""
            (list))))
      (outline/node*
        "pg-anaphoric"
        "Anaphoric Macros"
        "aif, alambda, and the pronoun principle from On Lisp Ch. 14."
        ""
        (list
          (outline/node*
            "pg-ana-aif"
            "aif: Anaphoric if"
            "(defmacro aif (test-form then-form else-form) (let ((it test-form)) (if it then-form else-form))). Usage: it is bound to the result of the test. (aif (find-user id) (format nil \"Found ~A\" it) \"Not found\"). Saves you from writing (let ((x (find-user id))) (if x ... ...))."
            ""
            (list))
          (outline/node*
            "pg-ana-alambda"
            "alambda: Self-Referencing Lambda"
            "(defmacro alambda (parms body) (labels ((self parms body)) self)). Usage: self refers to the lambda itself. (alambda (n) (if (= n 0) 1 (* n (self (1- n))))). Gives you anonymous recursive functions without naming them externally."
            ""
            (list))
          (outline/node*
            "pg-ana-principle"
            "The Broader Principle"
            "Arc took this further with pronouns as a first-class design feature: it in if, keep and sum in iteration. If a language lets things be implicit, the programmer always has the option of being explicit, but if the language requires everything to be explicit, the programmer cant make things implicit."
            ""
            (list))))
      (outline/node*
        "pg-bbn"
        "BBN Talk: Technical Advantages for Web Apps"
        "Specific techniques Graham used in Viaweb. Most relevant to Pricklypear."
        ""
        (list
          (outline/node*
            "pg-bbn-lang"
            "Any Language You Want"
            "Server-side deployment removes language lock-in. Youre now subject to market forces in what language you choose. A little startup using a more powerful language will eat your lunch."
            ""
            (list))
          (outline/node*
            "pg-bbn-incremental"
            "Incremental Development"
            "Lisp tradition: start with a tiny program that does almost nothing, then add features gradually, always having working code. Viaweb editor: began as a 120-line program from a book example. Grew to ~25,000 lines. I never once sat down and rewrote the whole thing. I dont think I was ever more than a day or two without running code. Fits naturally with rolling releases of web-based software."
            ""
            (list))
          (outline/node*
            "pg-bbn-toplevel"
            "Interactive Toplevel: Debugging While Users Are on the Phone"
            "When a bug report came in, Graham would load the code into the Lisp interpreter and log into the users account. If he reproduced the bug, hed get a break loop showing exactly what went wrong. Often I could fix the code and release a fix right away. And when I say right away, I mean while the user was still on the phone."
            ""
            (list))
          (outline/node*
            "pg-bbn-html"
            "Macros for HTML"
            "Lisp macros were another big win. The Viaweb editor could accurately be described as one big macro. HTML is prefix notation (like Lisp) and recursive (like Lisp). Macro calls within macro calls generated the most complex HTML, all manageable. Natural fit: HTML tags map to s-expressions."
            ""
            (list))
          (outline/node*
            "pg-bbn-rtml"
            "Embedded Languages: Rtml"
            "Rtml was an embedded DSL (built from Lisp macros) for describing web pages. Named after Robert Morris (username Rtm). Every page was generated by a program written in Rtml, called templates to sound less frightening, but they were real Lisp programs. Users had a structure editor (like Interlisp) for manipulating templates: cut/paste bits of code, no syntax errors possible, parentheses hidden, structure shown by indentation. Designed so no runtime errors were possible: every Rtml program yielded some web page. Compiled to Lisp functions at runtime via compile."
            ""
            (list))
          (outline/node*
            "pg-bbn-rtml-roles"
            "Three Roles Rtml Played"
            "1. Upgrade path for sophisticated users (1 in ~200 wrote templates). Users always want an upgrade path, even though as a rule theyll never take it. 2. Warning sign: when users modified default templates, it showed what was missing. Goal became no one should ever have to use Rtml. 3. Abstraction layer for developers: Having that extra layer of abstraction gave us a big advantage over competitors. Made design cleaner, code easier to modify. Changing Rtml itself = rare and serious; changing template code = quick and low-risk."
            ""
            (list))
          (outline/node*
            "pg-bbn-rtml-kw"
            "Keyword Parameters for Safe Evolution"
            "Adding a new keyword parameter to an Rtml operator didnt break existing templates. If I could go back and start over... Id make every Rtml operator take keyword parameters. A few operators didnt take keyword params because I didnt think Id ever need to change them, and almost every one I ended up kicking myself about later."
            ""
            (list))
          (outline/node*
            "pg-bbn-closures"
            "Closures Simulate Subroutines (Continuation-Passing Style)"
            "HTTP is stateless. Solution: use lexical closures to simulate subroutine-like behavior across HTTP requests, effectively continuation-passing style. A macro stored a closure (code + surrounding context) in a global hash table under a unique ID. The links URL contained that hash key. When clicked, the corresponding code was found and called, carrying its free variables from the original context. When our software generated a link, it could think: if the user clicks on this link, I want to run this piece of code."
            ""
            (list))
          (outline/node*
            "pg-bbn-color"
            "Concrete Example: Color Picker"
            "Editing an object with a color property. Competitors (with single-form CGI) struggled: text field for RGB numbers (users hate it), or a 256-item dropdown menu. Viaweb showed a color swatch + Change button, clicking goes to a color imagemap page, choosing a color returns to the edit page with that property updated. The software could behave as if it were returning from having chosen a color, simulating a subroutine call across HTTP. This made our software visibly more sophisticated than that of our competitors."
            ""
            (list))))
      (outline/node*
        "pg-hyl"
        "The Hundred-Year Language"
        "Language design philosophy: small cores, brevity, wasting programmer time."
        ""
        (list
          (outline/node*
            "pg-hyl-core"
            "Small Clean Cores Survive"
            "The more of a language you can write in itself, the better. The fundamental operators are the most important factor in long-term survival, like location in real estate: everything else you can fix later, but you cant fix the location. Mathematicians have always felt this way about axioms: the fewer, the better."
            ""
            (list))
          (outline/node*
            "pg-hyl-brevity"
            "Brevity Is the Goal"
            "Language design algorithm: look at a program and ask, is there any way to write this thats shorter? Length in distinct syntactic elements/tokens, not characters. What we really like is not abstraction per se but brevity. A way of expressing programs that was more abstract, but made your programs longer, would not be very enticing."
            ""
            (list))
          (outline/node*
            "pg-hyl-waste"
            "Wasting Programmer Time Is the True Inefficiency"
            "Inefficient software isnt gross. What's gross is a language that makes programmers do needless work. Wasting programmer time is the true inefficiency, not wasting machine time. The hundred-year language should let you throw together an unbelievably inefficient version 1 of a program with the least possible effort."
            ""
            (list))
          (outline/node*
            "pg-hyl-strings"
            "Strings as Lists (Arc Experiment)"
            "Arc eliminated strings as a separate type. Strings are just lists of characters. Some operations that would be awkward to describe as regular expressions can be described easily as recursive functions. Having strings in a language seems to be a case of premature optimization."
            ""
            (list))
          (outline/node*
            "pg-hyl-reuse"
            "Bottom-Up = Reusability"
            "Bottom-up programming means writing a program as a series of layers, each of which serves as a language for the one above. This approach tends to yield smaller, more flexible programs. Its also the best route to that holy grail, reusability. What makes OO code reusable is its bottom-upness, not its object-orientedness. Consider libraries: theyre reusable because theyre language, whether theyre written in an OO style or not."
            ""
            (list))))
      (outline/node*
        "pg-arc"
        "Arc: A New Lisp Dialect"
        "Design philosophy, brevity, pronouns, syntax as abbreviation."
        ""
        (list
          (outline/node*
            "pg-arc-philosophy"
            "A Language for Good Programmers"
            "Opposite of Javas design goal. Once you assume the user is a good programmer, you automatically have the answer to any such question: let the user do whatever he wants. The programmable programming language (Foderaro). Arc aims to maximize this: assume that the programmer will want to do things we never even imagined, and so will need to be able to get his hands on everything."
            ""
            (list))
          (outline/node*
            "pg-arc-brevity"
            "Brevity: Fewer Tokens, Not Characters"
            "Not fewer characters: fewer tokens. By brevity I dont mean that programs should require fewer characters. That counts for something, but it is more important to require fewer tokens. Perl is an inspiring example of brevity. Larry Wall broke all the rules, and in the process discovered some good ideas."
            ""
            (list))
          (outline/node*
            "pg-arc-onions"
            "No Onions in the Varnish"
            "Everything is open to question. Even lambda was renamed to fn. Alonzo Church himself wouldnt have used it if he had to write out the word lambda each time. No feature is sacred just because its always been that way."
            ""
            (list))
          (outline/node*
            "pg-arc-dialects"
            "Lisp = Dialects"
            "Lisp is an unusual language because it has dialects. Any language with car, cdr, cons, quote, eq, cond, and a notation for functions made of conses IS a dialect of Lisp, not just by social convention, but formally: with the core operators you can define all the rest. Its about time for a new dialect of Lisp."
            ""
            (list))
          (outline/node*
            "pg-arc-syntax"
            "Syntax as Abbreviation"
            "Arc would have syntax, but only as abbreviation: translating in a clearly defined, redefinable way into underlying s-expressions. Nearly all syntax optional, at the level of individual operators. x.y for (x y), x:y for (x y), [+ _ 1] for (fn (x) (+ x 1)). Lisp is widely considered to be the best substrate for domain-specific little languages, and programmable syntax should help make it even better."
            ""
            (list))
          (outline/node*
            "pg-arc-macros"
            "Macros as First-Class Objects"
            "Arc macros are first-class objects. No macrolet needed: bind a macro with let like any other variable. No hygienic macros required (though they could be a library). As an expert, the user will not be thrown by the prospect of variable capture (indeed will often do it deliberately). Macros are going to be a focus in Arc, because we think theyre one of the biggest wins in Lisp."
            ""
            (list))
          (outline/node*
            "pg-arc-web"
            "Built for Web Apps"
            "Were going to use Arc to write a platform for Web-based applications: the language we wish wed had when we were writing Viaweb. This means that Arc will have to be good at manipulating strings, which has not in the past been a big concern for Lisp."
            ""
            (list))
          (outline/node*
            "pg-arc-pronouns"
            "Iteration with Accumulation (Pronouns)"
            "while captures it (result of test). All iteration operators leave keep and sum bound to accumulation functions. (each x (list al bob joe) (if (> (len x) 2) (keep x))) returns (bob joe). (to x 5 (sum x) (pr x)) prints 01234, returns 10. If a language lets things be implicit, the programmer always has the option of being explicit."
            ""
            (list))))
      (outline/node*
        "pg-acc"
        "The Accumulator Example"
        "Same function in Lisp, Perl, JS, Python, Java. Power is the ultimate elegance."
        ""
        (list
          (outline/node*
            "pg-acc-lisp"
            "Common Lisp"
            "(defun foo (n) (lambda (i) (incf n i)))"
            ""
            (list))
          (outline/node*
            "pg-acc-perl"
            "Perl 5"
            "sub foo { my ($n) = @_; sub {$n += shift} }. Slightly longer: manual parameter extraction."
            ""
            (list))
          (outline/node*
            "pg-acc-js"
            "JavaScript"
            "function foo(n) { return function(i) { return n += i } }. Slightly longer: statement/expression distinction forces explicit returns."
            ""
            (list))
          (outline/node*
            "pg-acc-python"
            "Python (pre-nonlocal)"
            "def foo(n): s = [n]; def bar(i): s[0] += i; return s[0]; return bar. Much longer: cant assign to enclosing scope variables; must use a mutable container."
            ""
            (list))
          (outline/node*
            "pg-acc-java"
            "Java"
            "Need an interface + anonymous class. No closures, no function type. Far longer, and only works for integers, not polymorphic."
            ""
            (list))
          (outline/node*
            "pg-acc-insight"
            "Key Insight"
            "The Lisp version is shortest because it has all the features needed (closures, mutable lexical variables, functions as values, no statement/expression distinction). Each less-powerful language forces the programmer to manually simulate the missing feature. Power is the ultimate elegance."
            ""
            (list))))
      (outline/node*
        "pg-pp"
        "Connections to Pricklypear and Nopales"
        "How Grahams ideas map to PP as a web framework and Nopales as a Lisp dialect."
        ""
        (list
          (outline/node*
            "pg-pp-parallel"
            "Viaweb to Pricklypear Parallels"
            "Server-side app, Lisp on the server = server-side Lisp habitat, image is the API. Interactive toplevel for debugging = POST /api/eval, live eval of Lisp at runtime. Bug fixes while users on the phone = deploy via pp-deploy.sh, immediate. Incremental development (120 to 25k lines) = build libraries incrementally (rss, workout, etc.). Macros for HTML generation = can define route handlers and page generators in Lisp. Rtml embedded DSL for pages = routes + handlers as embedded mini-language. Closures for stateless HTTP = same techniques available in Nopales. Server-side = freedom to use Lisp = PP IS Lisp all the way down."
            ""
            (list))
          (outline/node*
            "pg-pp-nopales"
            "Nopales as a Lisp Dialect"
            "Nopales fits Grahams definition of a dialect of Lisp: it has the core operators (cons, car, cdr, quote, cond, eq, lambda/fn) and builds the rest from there. This means it inherits all of Grahams arguments about bottom-up programming, embedded languages, macros, the programmable programming language, and incremental development."
            ""
            (list))
          (outline/node*
            "pg-pp-techniques"
            "Specific Techniques Applicable to PP"
            "1. HTML generation via macros (div :class store (h1 Welcome) instead of string concat). 2. Anaphoric patterns (aif: test result bound to it). 3. Embedded DSLs (query DSL for find-rows, form-handling DSL, template language). 4. Closures for stateful web interactions (continuation-passing / closure-hash for multi-step wizards). 5. Keyword parameters for safe evolution (Grahams hard-won lesson). 6. Interactive debugging (eval API already provides this: use it aggressively)."
            ""
            (list))))
      (outline/node*
        "pg-quotes"
        "Key Quotes"
        "Twelve memorable quotes spanning all the essays."
        ""
        (list
          (outline/node*
            "pg-quote-1"
            "The Programmable Programming Language"
            "Lisp is the programmable programming language. -- John Foderaro"
            ""
            (list))
          (outline/node*
            "pg-quote-2"
            "Secret Weapon"
            "We wrote our software in a weird AI language, with a bizarre syntax full of parentheses. For years it had annoyed me to hear Lisp described that way. But now it worked to our advantage. -- Beating the Averages"
            ""
            (list))
          (outline/node*
            "pg-quote-3"
            "No Syntax"
            "Lisp code is made out of Lisp data objects. Lisp has no syntax. You write programs in the parse trees that get generated within the compiler when other languages are parsed. But these parse trees are fully accessible to your programs. -- Beating the Averages"
            ""
            (list))
          (outline/node*
            "pg-quote-4"
            "Competitive Advantage"
            "In business, there is nothing more valuable than a technical advantage your competitors dont understand. -- Beating the Averages"
            ""
            (list))
          (outline/node*
            "pg-quote-5"
            "Blub Paradox"
            "By induction, the only programmers in a position to see all the differences in power between the various languages are those who understand the most powerful one. -- Beating the Averages"
            ""
            (list))
          (outline/node*
            "pg-quote-6"
            "On Lisp Thesis"
            "Instead of just writing your program in Lisp, you can write your own language on Lisp, and write your program in that. -- On Lisp"
            ""
            (list))
          (outline/node*
            "pg-quote-7"
            "Programmer Time"
            "Inefficient software isnt gross. What is gross is a language that makes programmers do needless work. Wasting programmer time is the true inefficiency, not wasting machine time. -- The Hundred-Year Language"
            ""
            (list))
          (outline/node*
            "pg-quote-8"
            "Small Cores"
            "The more of a language you can write in itself, the better. -- The Hundred-Year Language"
            ""
            (list))
          (outline/node*
            "pg-quote-9"
            "Good Programmers"
            "Once you assume the user is a good programmer, you automatically have the answer to any such question: let the user do whatever he wants. -- Arc at 3 Weeks"
            ""
            (list))
          (outline/node*
            "pg-quote-10"
            "Brevity"
            "What we really like is not abstraction per se but brevity. -- Arc at 3 Weeks"
            ""
            (list))
          (outline/node*
            "pg-quote-11"
            "Catching Up"
            "Its 2002, and programming languages have almost caught up with 1958. -- Revenge of the Nerds"
            ""
            (list))
          (outline/node*
            "pg-quote-12"
            "The Recipe"
            "If you want to win in a software business, just take on the hardest problem you can find, use the most powerful language you can get, and wait for your competitors pointy-haired bosses to revert to the mean. -- Revenge of the Nerds"
            ""
            (list)))))))

(define plan
  (lambda (body) (plan/set! body)))

(define plan-clear
  (lambda () (plan/clear!)))

(define plan-list
  (lambda
    ()
    (list/filter
      (lambda (pair) (not (blank? (cdr pair))))
      (list/map
        (lambda
          (row)
          (let
            ((f (plan/row-fields row)))
            (cons (plan/fields-username f) (plan/fields-body f))))
        (plan/all-rows)))))

(define plan-of
  (lambda (user) (plan/of user)))

(define pp-test-let*
  (__macro__
    (bindings body)
    (if
      (null? bindings)
      body
      (quasiquote
        (let
          ((unquote (car bindings)))
          (pp-test-let* (unquote (cdr bindings)) (unquote body)))))))

(define prefix-of
  (lambda (name) (car (string-split (as-str name) "/"))))

(define probe
  (__macro__ (x) (as-str x)))

(define r
  (http-get "https://example.com"))

(define raw
  (__macro__ (x) (list (quote quote) x)))

(define respond-post
  (__macro__
    (redirect-path post-action get-html)
    (quasiquote
      (if
        (string-eq (request-method) "POST")
        (seq
          (unquote post-action)
          (if
            (hx-request?)
            (render-html (unquote get-html))
            (redirect (unquote redirect-path))))
        (html/page (unquote get-html))))))

(define show
  (lambda
    (tree)
    (list/map
      (lambda
        (n)
        (list
          (outline/id n)
          (list/map outline/id (outline/kids n))))
      (outline/kids tree))))

(define showparams
  (__macro__ (a b . c) (list (quote quote) (list a b c))))

(define string-find-pos
  (lambda
    (s delim start dlen)
    (if
      (>= start (- (string-length s) (- dlen 1)))
      -1
      (if
        (string-eq (substring s start dlen) delim)
        start
        (string-find-pos s delim (+ start 1) dlen)))))

(define string-split
  (lambda
    (s delim)
    (let
      ((dlen (string-length delim)) (slen (string-length s)))
      (if
        (or (= slen 0) (not (string-contains? s delim)))
        (list s)
        (let
          ((pos (string-find-pos s delim 0 dlen)))
          (if
            (= pos -1)
            (list s)
            (cons
              (substring s 0 pos)
              (string-split
                (substring s (+ pos dlen) (- slen (+ pos dlen)))
                delim))))))))

(define t
  (outline/node*
    "root"
    "R"
    ""
    ""
    (list
      (outline/node*
        "a"
        "A"
        ""
        ""
        (list
          (outline/node* "a1" "A1" "" "" (list))
          (outline/node* "a2" "A2" "" "" (list))))
      (outline/node* "b" "B" "" "" (list))
      (outline/node* "c" "C" "" "" (list)))))

(define td
  (flash/deck-create "ZZ-test-again-order" "throwaway"))

(define test-backtick
  (outline/node*
    "root"
    "Test `code` backtick"
    ""
    ""
    (list)))

(define test-bracket
  (outline/node*
    "root"
    "Test [HIGH] bracket"
    ""
    ""
    (list)))

(define test-colon
  (outline/node* "root" "Test : colon" "" "" (list)))

(define test-long
  (outline/node*
    "root"
    "This is a very long title that has many words and should test whether the issue is about string length or about specific characters in the content of the title"
    ""
    ""
    (list)))

(define test-nested
  (outline/node*
    "root"
    "Root"
    ""
    ""
    (list
      (outline/node* "c1" "Child1" "body" "" (list))
      (outline/node*
        "c2"
        "Child2"
        "body"
        ""
        (list
          (outline/node* "gc" "Grandchild" "body" "" (list)))))))

(define test-nl
  "hello")

(define test-outline
  (outline/node* "root" "Test Note" "body text" "" (list)))

(define test-outline-full
  (outline/node*
    "root"
    "Design Patterns for PP Web Dev"
    "Analysis of current patterns and recommendations based on Paul Graham's Lisp writings and codebase inspection."
    ""
    (list
      (outline/node*
        "s-good"
        "What's Already Good"
        "Patterns that are working well."
        ""
        (list
          (outline/node*
            "s-good-layers"
            "Layered Architecture"
            "The dependency graph is already bottom-up."
            ""
            (list)))))))

(define test-slash
  (outline/node*
    "root"
    "Test feed/page slash"
    ""
    ""
    (list)))

(define test-unicode
  (outline/node*
    "root"
    "Test \226\128\148 em-dash \226\134\146 arrow \194\171guillemet\194\187"
    ""
    ""
    (list)))

(define test-with-upsert
  (seq
    (notes/doc/upsert
      "test-design"
      "Test Design"
      test-outline-full
      "test"
      "note"
      "")
    "done"))

(define tree
  (outline/node*
    "root"
    "Move smoke A"
    ""
    ""
    (list
      (outline/node* "keep" "Keep me" "" "" (list))
      (outline/node*
        "child"
        "Child title"
        "body-x"
        "TODO"
        (list (outline/node* "grand" "Grand" "" "" (list)))))))

(define unless
  (__macro__
    (test body)
    (quasiquote (if (unquote test) () (unquote body)))))

(define when
  (__macro__
    (test body)
    (quasiquote (if (unquote test) (unquote body) ()))))

(define when2
  (__macro__
    (t b)
    (quasiquote (if (unquote t) (unquote b) ()))))

(define x
  1)

