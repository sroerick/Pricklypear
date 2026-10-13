

(define hyper/append-alist
  (lambda (a b)
    (if (null? b) a (cons (car b) (hyper/append-alist a (cdr b)))))
  "Append b onto the end of alist a.")

(define hyper/attr
  (lambda (node name)
    (soup/attr node name))
  "Get an attribute value from a node handle.")

;; ---------- click (the swap loop) ----------
;; (hyper/click page sel params) -> NEW page value
;; Find trigger at sel, read hx-post/hx-get (from element), read
;; hx-target/hx-swap (v1: element only — ancestor walk needs a soup prim),
;; POST in-process with HX-Request header, parse fragment, deep-copy the
;; page, apply swap, return new page.
;;
;; v1 limitations (errors with 'unsupported-feature'):
;;   - inherited hx-target/hx-swap (ancestor walk) — needs soup/ancestor prim
;;   - hx-on, hx-push-url, hx-ext, hx-vals js:, debounce → unsupported
;;   - form serialization: caller passes params explicitly
(define hyper/click
  (lambda (page sel params)
    (let ((matches (soup/select page sel)))
      (if (null? matches)
          (error (string-append "hyper/click: no element matches " sel))
          (hyper/click* page (car matches) params))))
  "Simulate a click: read hx-*, POST, swap, return new page.")

(define hyper/click*
  (lambda (page elt params)
    ;; Read hx-post/hx-get from elt OR nearest ancestor (inherited trigger —
    ;; mirrors htmx compat implicit inheritance).
    (let* ((hxpost (soup/closest-attr elt "hx-post"))
           (hxget (soup/closest-attr elt "hx-get")))
      (cond
        ((not (null? hxpost)) (hyper/do-swap page elt "POST" hxpost params))
        ((not (null? hxget)) (hyper/do-swap page elt "GET" hxget params))
        (#t (error "unsupported-feature: no hx-get/hx-post on element or ancestor")))))
  "Read trigger from element/ancestor; POST or GET then swap.")

(define hyper/count
  (lambda (page sel)
    (count (soup/select page sel)))
  "Count elements matching sel.")

;; ---------- diff + expect ----------
(define hyper/diff
  (lambda (p1 p2)
    (soup/diff p1 p2))
  "Node-level what-changed report between two pages.")

;; Read target + swap, POST, deep-copy page, apply swap, return new page.
;; target/swap are read via closest-attr (inherited from ancestors).
(define hyper/do-swap
  (lambda
    (page elt verb url params)
    (let*
      ((target-raw (soup/closest-attr elt "hx-target"))
        (swap-raw (soup/closest-attr elt "hx-swap"))
        (swap (if (null? swap-raw) "innerHTML" swap-raw))
        (parsed (hyper/parse-url url))
        (path (car parsed))
        (qparams (cdr parsed))
        (all-params (hyper/append-alist qparams params))
        (handler (route/lookup path)))
      (if
        (null? handler)
        (error
          (string-append
            "unsupported-feature: no route for "
            path))
        (begin
          (let*
            ((fragment-html
                (route/dispatch
                  handler
                  verb
                  path
                  (cons (cons "hx-request" "true") all-params)))
              (new-page (soup/page-copy page))
              (target
                (if
                  (null? target-raw)
                  elt
                  (let
                    ((ts (soup/select new-page target-raw)))
                    (if (null? ts) elt (car ts))))))
            (begin (soup/swap! target swap fragment-html) new-page))))))
  "Read target+swap, dispatch, deep-copy, apply swap; return new page.")

(define hyper/expect
  (lambda (page sel expected)
    (let ((actual (hyper/text page sel)))
      (if (equal? actual expected)
          #t
          (error (string-append
                   "hyper/expect: "
                   sel
                   " expected \""
                   expected
                   "\" got \""
                   actual
                   "\"")))))
  "Assert text at sel matches expected; errors on mismatch.")

(define hyper/html
  (lambda (page)
    (soup/serialize page))
  "Serialize the page tree to an HTML string.")

(define hyper/parse-pairs
  (lambda (pairs)
    (if (null? pairs) (list)
        (let* ((kv (car pairs))
               (eqparts (string-split kv "=")))
          (cons (if (null? (cdr eqparts)) (cons kv "")
                    (cons (car eqparts) (car (cdr eqparts))))
                (hyper/parse-pairs (cdr pairs))))))
  "Parse a list of kv strings into an alist.");; hypermedia harness client — headless htmx swap simulator (Nopales layer)
;; Wraps soup/* + route/dispatch prims. Spec: borge/hypermedia-harness.borg
;;
;; A page value is an opaque VStr handle into the soup/ prims' handle table.
;; Each interaction returns a NEW page value (deep-copied via soup/page-copy
;; before mutation), so pages are immutable at this surface — free history
;; and diff. Outside the fixi-five supported subset, signals
;; 'unsupported-feature' so the flow graduates to the CDP tier.

;; ---------- URL parsing ----------
;; Split a URL into (path . query-alist).
(define hyper/parse-url
  (lambda (url)
    (let* ((parts (string-split url "?"))
           (path (car parts))
           (rest (if (null? (cdr parts)) "" (car (cdr parts))))
           (pairs (if (equal? rest "") (list)
                      (string-split rest "&"))))
      (cons path (hyper/parse-pairs pairs))))
  "Parse url into (path . ((key . val) ...)).")

(define hyper/query
  (lambda (page sel)
    (soup/select page sel))
  "List of node handles matching sel.")

;; ---------- sandbox ----------
;; (hyper/sandbox branch body) — v1 stub. Full impl needs a branch checkout
;; with restore (dynamic-wind). Returns unsupported-feature for now.
(define hyper/sandbox
  (lambda (branch body)
    (error "unsupported-feature: hyper/sandbox (branch checkout in simulator)"))
  "Run body on a dev branch (v1 stub).")

;; ---------- stream (multipart replay) ----------
;; (hyper/stream page parts) -> NEW page value
;; Apply an ordered multipart part list onto a page via soup/page-copy +
;; soup/apply-parts!, returning a NEW page (immutability preserved: the
;; input page is never mutated). parts come from the host stream as DATA
;; (agent: loop.ml collects the emitted HX-Target/HX-Swap parts;
;; image: http_agent mp_write_part). Lets hyper/** assert a consumed
;; chat/agent turn deterministically. Each part is an alist
;; (("html" . frag) ("target" . sel) ("swap" . strat)); target/swap
;; absent -> caller element + "innerHTML" (soup/apply-parts! defaults).
(define hyper/stream
  (lambda (page parts)
    (let ((copy (soup/page-copy page)))
      (begin (soup/apply-parts! copy parts)
             copy)))
  "Apply an ordered part list onto a fresh copy of page; return NEW page.")

;; ---------- queries ----------
(define hyper/text
  (lambda (page sel)
    (let ((matches (soup/select page sel)))
      (if (null? matches) ""
          (soup/text (car matches)))))
  "Text content of the first element matching sel.")

;; ---------- visit ----------
;; (hyper/visit url) -> page handle
;; Resolve url -> handler via route/lookup, GET in-process (no HX-Request
;; header, so the handler returns the full page like a browser's initial nav),
;; soup/parse the returned HTML.
(define hyper/visit
  (lambda (url)
    (let* ((parsed (hyper/parse-url url))
           (path (car parsed))
           (params (cdr parsed))
           (handler (route/lookup path)))
      (if (null? handler)
          (error (string-append "unsupported-feature: no route for " path))
          (let ((html (route/dispatch handler "GET" path params)))
            (soup/parse html)))))
  "GET the route handler in-process; parse body -> page value.")
