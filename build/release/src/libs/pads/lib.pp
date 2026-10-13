;; package pads -- exported from live branch system/main
;; Sync: scripts/pp-sync.sh status|pull|snapshot

(define pads/attr
  (lambda
    (k attrs)
    (let ((hit (assoc k attrs))) (if hit (cdr hit) #f)))
  "Value of symbol key k in a symbol-keyed attrs alist, or #f when\nabsent. MUST be #f, not (list): Nopales truthiness is #f/nil-falsy\nbut (list) is an EMPTY LIST VALUE, which is truthy (lisp/lib/eval.ml\ntruthy) — using (list) as the missing marker made every attr lookup\nlook present (class=\"\" padding:px bugs).")

(define pads/attr-string
  (lambda
    (v)
    (cond
      ((and (pair? v) (pair? (cdr v)))
        (cond
          ((equal? (car v) (quote fixed))
            (string-append "fixed:" (as-str (car (cdr v)))))
          ((equal? (car v) (quote grow))
            (string-append "grow:" (as-str (car (cdr v)))))
          (else (as-str v))))
      ((and
          (pair? v)
          (null? (cdr v))
          (equal? (car v) (quote fit)))
        "fit")
      (else (as-str v))))
  "Normalize an attr value to its string form. Size values use the\nscene size grammar (W4): (fixed n) -> fixed:N, (grow n) -> grow:N,\n(fit) -> fit. Everything else goes through as-str.")

(define pads/attrs-html
  (lambda
    (attrs)
    (list/foldl
      (lambda
        (acc a)
        (string-append
          acc
          (if (string-eq acc "") "" " ")
          (html/attr (car a) (cdr a))))
      ""
      attrs))
  "Attrs alist -> space-joined k=\"v\" attribute string with html/attr\nsemantics (values html-escaped, keys as-str). Order = alist order, so\nrenderers stay byte-identical.")

(define pads/box-attr-pairs
  (lambda
    (kind attrs)
    (let
      ((class (pads/attr (quote class) attrs))
        (style (pads/box-style kind attrs)))
      (cond
        ((and class (not (string-eq style "")))
          (list
            (cons (quote class) class)
            (cons (quote style) style)))
        (class (list (cons (quote class) class)))
        ((not (string-eq style ""))
          (list (cons (quote style) style)))
        (else (list)))))
  "HTML attr pairs for a box in canonical order: class, then style\n(only when non-empty).")

(define pads/box-style
  (lambda
    (kind attrs)
    (string-append
      "display:flex;flex-direction:"
      (if (equal? kind (quote row)) "row" "column")
      (let
        ((gap (pads/attr (quote gap) attrs)))
        (if gap (string-append ";gap:" (as-str gap) "px") ""))
      (let
        ((padding (pads/attr (quote padding) attrs)))
        (if
          padding
          (string-append ";padding:" (as-str padding) "px")
          ""))
      (let
        ((f (pads/size-css (pads/attr (quote size) attrs))))
        (if (string-eq f "") "" (string-append ";" f)))))
  "Canonical flex style for a row/column box, in byte-stable order:\ndisplay, flex-direction, gap, padding, then flex from size. Each\noptional part is joined with a ';' separator only when present.")

(define pads/flatten-kids
  (lambda
    (kids)
    (cond
      ((null? kids) (list))
      ((null? (car kids)) (pads/flatten-kids (cdr kids)))
      ((and (pair? (car kids)) (pair? (car (car kids))))
        (append (car kids) (pads/flatten-kids (cdr kids))))
      (else (cons (car kids) (pads/flatten-kids (cdr kids))))))
  "Splice one level: a kid whose car is a pair is a list of kids and\ncontributes its elements; strings, pad nodes, and nil (dropped) pass\nthrough the recursion.")

(define pads/json-escape
  (lambda
    (s)
    (string-replace
      (string-replace
        (string-replace
          (string-replace (as-str s) "\\" "\\\\")
          "\""
          "\\\"")
        "\n"
        "\\n")
      "\r"
      "\\r"))
  "Escape s as a JSON string body: backslash, quote, CR, LF, TAB.")

(define pads/node-attrs
  (lambda
    (node)
    (if
      (and (pair? node) (pair? (cdr node)))
      (car (cdr node))
      (list)))
  "Attrs alist of a pad node (cadr). Non-pad input yields nil.")

(define pads/node-kids
  (lambda
    (node)
    (if
      (and (pair? node) (pair? (cdr node)))
      (cdr (cdr node))
      (list)))
  "Kids of a pad node: (tag attrs kid...) -> (kid...). Non-pad input\nyields the empty list.")

(define pads/op-insert-at
  (lambda
    (id idx node)
    (list (quote insert-at) id idx node))
  "OP, data definition (no consumer yet): insert `node` at index idx\nin the kids of node `id`.")

(define pads/op-move
  (lambda
    (id new-parent idx)
    (list (quote move) id new-parent idx))
  "OP, data definition (no consumer yet): move node `id` to index idx\nunder parent `new-parent`.")

(define pads/op-remove
  (lambda (id) (list (quote remove) id))
  "OP, data definition (no consumer yet): remove node `id` from its\nparent.")

(define pads/op-replace
  (lambda (id subtree) (list (quote replace) id subtree))
  "OP, data definition (no consumer yet — pads/ops-contract): replace\nthe node with stable id `id` by `subtree`. Web lowers to htmx morph;\nnative scene lowers to node replacement.")

(define pads/raw-attrs
  (lambda
    (attrs)
    (cond
      ((null? attrs) (list))
      ((equal? (car (car attrs)) (quote tag))
        (pads/raw-attrs (cdr attrs)))
      (else (cons (car attrs) (pads/raw-attrs (cdr attrs))))))
  "Attrs for a raw passthrough element: drop the (tag . name) pair\nitself, keep the rest in order.")

(define pads/render-box
  (lambda
    (kind attrs kids)
    (html/tag
      "div"
      (pads/attrs-html (pads/box-attr-pairs kind attrs))
      (pads/render-kids kids)))
  "row/column -> div. Attrs honored: class first, then the composed\nflex style; other attrs ignored for HTML v0 (presentation quarantine,\npads/vocabulary-tiers).")

(define pads/render-html
  (lambda
    (node)
    (cond
      ((string? node) (html-escape node))
      ((null? node) "")
      ((not (pair? node)) (html-escape (as-str node)))
      (else
        (let
          ((tag (car node))
            (attrs (pads/node-attrs node))
            (kids (pads/node-kids node)))
          (cond
            ((equal? tag (quote row))
              (pads/render-box (quote row) attrs kids))
            ((equal? tag (quote column))
              (pads/render-box (quote column) attrs kids))
            ((equal? tag (quote heading))
              (let
                ((level (pads/attr (quote level) attrs)))
                (html/tag
                  (string-append "h" (if level (as-str level) "2"))
                  ""
                  (pads/render-kids kids))))
            ((equal? tag (quote text))
              (pads/render-text attrs kids))
            ((equal? tag (quote link))
              (let
                ((href (pads/attr (quote href) attrs)))
                (html/tag
                  "a"
                  (if href (html/attr "href" href) "")
                  (pads/render-kids kids))))
            ((equal? tag (quote raw))
              (html/tag
                (as-str (pads/attr (quote tag) attrs))
                (pads/attrs-html (pads/raw-attrs attrs))
                (pads/render-kids kids)))
            ((string? tag)
              (html/tag
                tag
                (pads/attrs-html attrs)
                (pads/render-kids kids)))
            (else
              (html/tag
                (as-str tag)
                (pads/attrs-html attrs)
                (pads/render-kids kids))))))))
  "Render a pad tree to an HTML fragment string, byte-identical with\nthe libs/html string pipeline (the adoption gate: html/tag + html-escape\nsemantics, pads/renderer-dialects). A string node renders as escaped\ntext. Kinds v0: row/column -> div with composed flex style (class then\nstyle attrs); heading -> h<level> (default 2) with escaped body; text ->\nescaped concatenated body, wrapped in a div only when it carries size or\nclass attrs; link -> a with href; raw (or a string tag) ->\npassthrough element whose attrs render as k=\"v\" pairs; any other\nsymbol tag renders as an element named by the tag (documented escape\nhatch). Non-string non-pair kids render via as-str + escape.")

(define pads/render-kids
  (lambda
    (kids)
    (cond
      ((null? kids) "")
      (else
        (string-append
          (pads/render-html (car kids))
          (pads/render-kids (cdr kids))))))
  "Render each kid and concatenate (html/join semantics).")

(define pads/render-scene
  (lambda
    (node)
    (string-append
      "{\"version\":0,\"root\":"
      (pads/scene-node node "0")
      "}"))
  "SCENE DIALECT v0 (normative; W4 — consumers: native/soil-hello).\nEmits one JSON object string: {\"version\":0,\"root\":<node>} where a\nnode is {\"kind\":...,\"id\":...,\"text\":...? ,\"attrs\":{...},\n\"kids\":[...]} — text fields only on text leaves. Kinds v0: row |\ncolumn | text (heading lowers to text with a level attr; link lowers to\ntext with href). Attrs are string-valued; the size grammar is fit |\ngrow:N | fixed:N. Ids are stable path ids: root \"0\", kids \"0/0\",\n\"0/1\", ... (deterministic, synthesized — structed handshake later).\nNo Nopales eval happens on any consumer frame path (soil contract):\nthis is a pure data render.")

(define pads/render-text
  (lambda
    (attrs kids)
    (let
      ((pairs (pads/text-attr-pairs attrs)))
      (if
        (null? pairs)
        (pads/render-kids kids)
        (html/tag
          "div"
          (pads/attrs-html pairs)
          (pads/render-kids kids)))))
  "text -> escaped body. Bare when it carries no box attrs; with size\nor class attrs it becomes a div wrapper carrying them (a flex ITEM of\nthe surrounding row/column).")

(define pads/scene-attr
  (lambda
    (a)
    (string-append
      "\""
      (pads/json-escape (as-str (car a)))
      "\":\""
      (pads/json-escape (pads/attr-string (cdr a)))
      "\""))
  "One attr pair -> \"key\":\"value\" (values via pads/attr-string,\nso sizes normalize to the size grammar).")

(define pads/scene-attrs
  (lambda
    (attrs)
    (cond
      ((null? attrs) "{}")
      (else
        (string-append "{" (pads/scene-attrs-body attrs) "}"))))
  "Attrs alist -> JSON object string in definition order; empty attrs\n-> {}. Object syntax is built by hand: json-emit lowers alists to\narrays, and dict-set would coerce numeric-looking strings to JSON\nnumbers, but the dialect wants string values.")

(define pads/scene-attrs-body
  (lambda
    (attrs)
    (cond
      ((null? attrs) "")
      ((null? (cdr attrs)) (pads/scene-attr (car attrs)))
      (else
        (string-append
          (pads/scene-attr (car attrs))
          ","
          (pads/scene-attrs-body (cdr attrs))))))
  "Comma-joined attr members (no braces).")

(define pads/scene-box
  (lambda
    (kind attrs kids path)
    (string-append
      "{\"kind\":\""
      kind
      "\",\"id\":\""
      path
      "\",\"attrs\":"
      (pads/scene-attrs attrs)
      ",\"kids\":"
      (pads/scene-kids kids path)
      "}"))
  "A row/column scene object: kind, id, attrs, kids.")

(define pads/scene-kids
  (lambda
    (kids path)
    (string-append
      "["
      (pads/scene-kids-body kids path 0)
      "]"))
  "Kids as a JSON array with path ids parent/0, parent/1, ...")

(define pads/scene-kids-body
  (lambda
    (kids path i)
    (cond
      ((null? kids) "")
      ((null? (cdr kids))
        (pads/scene-node (car kids) (pads/scene-path path i)))
      (else
        (string-append
          (pads/scene-node (car kids) (pads/scene-path path i))
          ","
          (pads/scene-kids-body (cdr kids) path (+ i 1))))))
  "Comma-joined kid objects at incrementing path indices.")

(define pads/scene-node
  (lambda
    (node path)
    (cond
      ((string? node)
        (string-append
          "{\"kind\":\"text\",\"id\":\""
          path
          "\",\"text\":\""
          (pads/json-escape node)
          "\",\"attrs\":{},\"kids\":[]}"))
      ((or (null? node) (not (pair? node)))
        (string-append
          "{\"kind\":\"text\",\"id\":\""
          path
          "\",\"text\":\"\",\"attrs\":{},\"kids\":[]}"))
      (else
        (let
          ((tag (car node))
            (attrs (pads/node-attrs node))
            (kids (pads/node-kids node)))
          (cond
            ((equal? tag (quote row))
              (pads/scene-box "row" attrs kids path))
            ((equal? tag (quote column))
              (pads/scene-box "column" attrs kids path))
            (else
              (string-append
                "{\"kind\":\"text\",\"id\":\""
                path
                "\",\"text\":\""
                (pads/json-escape (pads/text-content node))
                "\",\"attrs\":"
                (pads/scene-attrs attrs)
                ",\"kids\":[]}")))))))
  "One scene node object at path id. Boxes (row/column) carry attrs\nand kids; every non-box node (heading, text, link, raw, string tags)\nlowers to a text leaf carrying its text content plus attrs (level,\nhref, sizes survive as strings).")

(define pads/scene-path
  (lambda (path i) (string-append path "/" (as-str i)))
  "Stable path id for kid i under path: parent/0/2/1 style.")

(define pads/size-css
  (lambda
    (size)
    (cond
      ((and
          (pair? size)
          (pair? (cdr size))
          (equal? (car size) (quote fixed)))
        (string-append
          "flex:0 0 "
          (as-str (car (cdr size)))
          "px"))
      ((and
          (pair? size)
          (pair? (cdr size))
          (equal? (car size) (quote grow)))
        (string-append "flex:" (as-str (car (cdr size)))))
      (else "")))
  "Size value -> one flex style declaration WITHOUT a leading\nsemicolon (empty when fit or absent): (fixed n) -> flex:0 0 npx —\ndirection-independent main-axis basis; (grow n) -> flex:n. Box style\njoins it with ';' separators; standalone text boxes use it bare, so\nthe legacy oracle's style=\"flex:0 0 240px\" matches byte-for-byte.")

(define pads/text-attr-pairs
  (lambda
    (attrs)
    (let
      ((class (pads/attr (quote class) attrs))
        (style (pads/size-css (pads/attr (quote size) attrs))))
      (cond
        ((and class (not (string-eq style "")))
          (list
            (cons (quote class) class)
            (cons (quote style) style)))
        (class (list (cons (quote class) class)))
        ((not (string-eq style ""))
          (list (cons (quote style) style)))
        (else (list)))))
  "HTML attr pairs for a text box: class, then the size flex fragment\nonly (no display:flex — the parent row/column owns the flex context).")

(define pads/text-content
  (lambda
    (node)
    (cond
      ((string? node) node)
      ((null? node) "")
      ((not (pair? node)) "")
      (else (pads/text-content-kids (pads/node-kids node)))))
  "Concatenated text-run content of a pad subtree. Text kids only:\nattr values are never content.")

(define pads/text-content-kids
  (lambda
    (kids)
    (cond
      ((null? kids) "")
      (else
        (string-append
          (pads/text-content (car kids))
          (pads/text-content-kids (cdr kids))))))
  "Text content folded over kids.")
