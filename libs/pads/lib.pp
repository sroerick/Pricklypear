;; package pads -- platform-agnostic UI tree (spec: borge/pads.borg; plan:
;; docs/plans/pads-hello-dualrender.md). Pure Nopales — zero changes under
;; image/, lisp/, pure/ (decision D3).
;; Sync: scripts/pp_sync.py pull|push
;;
;; A pad tree is a proper-list VALUE: (tag attrs kid...) where tag is a
;; symbol (v0 kinds: row column heading text link raw) or a string
;; (passthrough element), attrs is a symbol-keyed alist, and a string in
;; kid position is implicitly a text run (the one dwim rule). Renderers
;; lower the tree; nothing here touches the interpreter or prims.

;; ---------- constructor (layers 1+2 only; D6: no macro sugar) ----------

(define pad
  (lambda (tag attrs &rest kids)
    "Build one pad node: (tag attrs kid...). attrs is a symbol-keyed
alist (string or list values); each kid is a string (text run), a pad
node, or a proper list of kids to splice (dwim, one level — the map
case). Layers 1+2 only: no macro sugar (D6, quote-compounds edge-case)."
    (cons tag (cons attrs (pads/flatten-kids kids)))))

(define pads/flatten-kids
  (lambda (kids)
    "Splice one level: a kid whose car is a pair is a list of kids and
contributes its elements; strings, pad nodes, and nil (dropped) pass
through the recursion."
    (cond
      ((null? kids) (list))
      ((null? (car kids)) (pads/flatten-kids (cdr kids)))
      ((and (pair? (car kids)) (pair? (car (car kids))))
       (append (car kids) (pads/flatten-kids (cdr kids))))
      (else (cons (car kids) (pads/flatten-kids (cdr kids)))))))

;; ---------- node accessors ----------

(define pads/node-kids
  (lambda (node)
    "Kids of a pad node: (tag attrs kid...) -> (kid...). Non-pad input
yields the empty list."
    (if (and (pair? node) (pair? (cdr node)))
        (cdr (cdr node))
        (list))))

(define pads/node-attrs
  (lambda (node)
    "Attrs alist of a pad node (cadr). Non-pad input yields nil."
    (if (and (pair? node) (pair? (cdr node)))
        (car (cdr node))
        (list))))

(define pads/attr
  (lambda (k attrs)
    "Value of symbol key k in a symbol-keyed attrs alist, or #f when
absent. MUST be #f, not (list): Nopales truthiness is #f/nil-falsy
but (list) is an EMPTY LIST VALUE, which is truthy (lisp/lib/eval.ml
truthy) — using (list) as the missing marker made every attr lookup
look present (class=\"\" padding:px bugs)."
    (let ((hit (assoc k attrs)))
      (if hit (cdr hit) #f))))

(define pads/attr-string
  (lambda (v)
    "Normalize an attr value to its string form. Size values use the
scene size grammar (W4): (fixed n) -> fixed:N, (grow n) -> grow:N,
(fit) -> fit. Everything else goes through as-str."
    (cond
      ((and (pair? v) (pair? (cdr v)))
       (cond
         ((equal? (car v) 'fixed)
          (string-append "fixed:" (as-str (car (cdr v)))))
         ((equal? (car v) 'grow)
          (string-append "grow:" (as-str (car (cdr v)))))
         (else (as-str v))))
      ((and (pair? v) (null? (cdr v)) (equal? (car v) 'fit)) "fit")
      (else (as-str v)))))

;; ---------- render-html (V1: byte-identity with libs/html) ----------

(define pads/size-css
  (lambda (size)
    "Size value -> one flex style declaration WITHOUT a leading
semicolon (empty when fit or absent): (fixed n) -> flex:0 0 npx —
direction-independent main-axis basis; (grow n) -> flex:n. Box style
joins it with ';' separators; standalone text boxes use it bare, so
the legacy oracle's style=\"flex:0 0 240px\" matches byte-for-byte."
    (cond
      ((and (pair? size) (pair? (cdr size)) (equal? (car size) 'fixed))
       (string-append "flex:0 0 " (as-str (car (cdr size))) "px"))
      ((and (pair? size) (pair? (cdr size)) (equal? (car size) 'grow))
       (string-append "flex:" (as-str (car (cdr size)))))
      (else ""))))

(define pads/box-style
  (lambda (kind attrs)
    "Canonical flex style for a row/column box, in byte-stable order:
display, flex-direction, gap, padding, then flex from size. Each
optional part is joined with a ';' separator only when present."
    (string-append
      "display:flex;flex-direction:"
      (if (equal? kind 'row) "row" "column")
      (let ((gap (pads/attr 'gap attrs)))
        (if gap (string-append ";gap:" (as-str gap) "px") ""))
      (let ((padding (pads/attr 'padding attrs)))
        (if padding (string-append ";padding:" (as-str padding) "px") ""))
      (let ((f (pads/size-css (pads/attr 'size attrs))))
        (if (string-eq f "") "" (string-append ";" f))))))

(define pads/attrs-html
  (lambda (attrs)
    "Attrs alist -> space-joined k=\"v\" attribute string with html/attr
semantics (values html-escaped, keys as-str). Order = alist order, so
renderers stay byte-identical."
    (list/foldl
      (lambda
        (acc a)
        (string-append
          acc
          (if (string-eq acc "") "" " ")
          (html/attr (car a) (cdr a))))
      ""
      attrs)))

(define pads/render-kids
  (lambda (kids)
    "Render each kid and concatenate (html/join semantics)."
    (cond
      ((null? kids) "")
      (else
        (string-append
          (pads/render-html (car kids))
          (pads/render-kids (cdr kids)))))))

(define pads/text-attr-pairs
  (lambda (attrs)
    "HTML attr pairs for a text box: class, then the size flex fragment
only (no display:flex — the parent row/column owns the flex context)."
    (let ((class (pads/attr 'class attrs))
          (style (pads/size-css (pads/attr 'size attrs))))
      (cond
        ((and class (not (string-eq style "")))
         (list (cons 'class class) (cons 'style style)))
        (class (list (cons 'class class)))
        ((not (string-eq style "")) (list (cons 'style style)))
        (else (list))))))

(define pads/render-text
  (lambda (attrs kids)
    "text -> escaped body. Bare when it carries no box attrs; with size
or class attrs it becomes a div wrapper carrying them (a flex ITEM of
the surrounding row/column)."
    (let ((pairs (pads/text-attr-pairs attrs)))
      (if (null? pairs)
          (pads/render-kids kids)
          (html/tag "div" (pads/attrs-html pairs) (pads/render-kids kids))))))

(define pads/render-box
  (lambda (kind attrs kids)
    "row/column -> div. Attrs honored: class first, then the composed
flex style; other attrs ignored for HTML v0 (presentation quarantine,
pads/vocabulary-tiers)."
    (html/tag
      "div"
      (pads/attrs-html
        (pads/box-attr-pairs kind attrs))
      (pads/render-kids kids))))

(define pads/box-attr-pairs
  (lambda (kind attrs)
    "HTML attr pairs for a box in canonical order: class, then style
(only when non-empty)."
    (let ((class (pads/attr 'class attrs))
          (style (pads/box-style kind attrs)))
      (cond
        ((and class (not (string-eq style "")))
         (list (cons 'class class) (cons 'style style)))
        (class (list (cons 'class class)))
        ((not (string-eq style "")) (list (cons 'style style)))
        (else (list))))))

(define pads/render-html
  (lambda (node)
    "Render a pad tree to an HTML fragment string, byte-identical with
the libs/html string pipeline (the adoption gate: html/tag + html-escape
semantics, pads/renderer-dialects). A string node renders as escaped
text. Kinds v0: row/column -> div with composed flex style (class then
style attrs); heading -> h<level> (default 2) with escaped body; text ->
escaped concatenated body, wrapped in a div only when it carries size or
class attrs; link -> a with href; raw (or a string tag) ->
passthrough element whose attrs render as k=\"v\" pairs; any other
symbol tag renders as an element named by the tag (documented escape
hatch). Non-string non-pair kids render via as-str + escape."
    (cond
      ((string? node) (html-escape node))
      ((null? node) "")
      ((not (pair? node)) (html-escape (as-str node)))
      (else
        (let ((tag (car node))
              (attrs (pads/node-attrs node))
              (kids (pads/node-kids node)))
          (cond
            ((equal? tag 'row) (pads/render-box 'row attrs kids))
            ((equal? tag 'column) (pads/render-box 'column attrs kids))
            ((equal? tag 'heading)
             (let ((level (pads/attr 'level attrs)))
               (html/tag
                 (string-append "h" (if level (as-str level) "2"))
                 ""
                 (pads/render-kids kids))))
            ((equal? tag 'text) (pads/render-text attrs kids))
            ((equal? tag 'link)
             (let ((href (pads/attr 'href attrs)))
               (html/tag
                 "a"
                 (if href (html/attr "href" href) "")
                 (pads/render-kids kids))))
            ((equal? tag 'raw)
             (html/tag
               (as-str (pads/attr 'tag attrs))
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
                (pads/render-kids kids)))))))))

(define pads/raw-attrs
  (lambda (attrs)
    "Attrs for a raw passthrough element: drop the (tag . name) pair
itself, keep the rest in order."
    (cond
      ((null? attrs) (list))
      ((equal? (car (car attrs)) 'tag) (pads/raw-attrs (cdr attrs)))
      (else (cons (car attrs) (pads/raw-attrs (cdr attrs)))))))

;; ---------- render-scene (V2: native soil path; W4 dialect v0) ----------

;; json-escape comes from libs/std (shared escaper, i-1788670711).

(define pads/scene-attr
  (lambda (a)
    "One attr pair -> \"key\":\"value\" (values via pads/attr-string,
so sizes normalize to the size grammar)."
    (string-append
      "\""
      (json-escape (as-str (car a)))
      "\":\""
      (json-escape (pads/attr-string (cdr a)))
      "\"")))

(define pads/scene-attrs
  (lambda (attrs)
    "Attrs alist -> JSON object string in definition order; empty attrs
-> {}. Object syntax is built by hand: json-emit lowers alists to
arrays, and dict-set would coerce numeric-looking strings to JSON
numbers, but the dialect wants string values."
    (cond
      ((null? attrs) "{}")
      (else
        (string-append
          "{"
          (pads/scene-attrs-body attrs)
          "}")))))

(define pads/scene-attrs-body
  (lambda (attrs)
    "Comma-joined attr members (no braces)."
    (cond
      ((null? attrs) "")
      ((null? (cdr attrs)) (pads/scene-attr (car attrs)))
      (else
        (string-append
          (pads/scene-attr (car attrs))
          ","
          (pads/scene-attrs-body (cdr attrs)))))))

(define pads/text-content
  (lambda (node)
    "Concatenated text-run content of a pad subtree. Text kids only:
attr values are never content."
    (cond
      ((string? node) node)
      ((null? node) "")
      ((not (pair? node)) "")
      (else (pads/text-content-kids (pads/node-kids node))))))

(define pads/text-content-kids
  (lambda (kids)
    "Text content folded over kids."
    (cond
      ((null? kids) "")
      (else
        (string-append
          (pads/text-content (car kids))
          (pads/text-content-kids (cdr kids)))))))

(define pads/scene-path
  (lambda (path i)
    "Stable path id for kid i under path: parent/0/2/1 style."
    (string-append path "/" (as-str i))))

(define pads/scene-node
  (lambda (node path)
    "One scene node object at path id. Boxes (row/column) carry attrs
and kids; every non-box node (heading, text, link, raw, string tags)
lowers to a text leaf carrying its text content plus attrs (level,
href, sizes survive as strings)."
    (cond
      ((string? node)
       (string-append
         "{\"kind\":\"text\",\"id\":\""
         path
         "\",\"text\":\""
         (json-escape node)
         "\",\"attrs\":{},\"kids\":[]}"))
      ((or (null? node) (not (pair? node)))
       (string-append
         "{\"kind\":\"text\",\"id\":\""
         path
         "\",\"text\":\"\",\"attrs\":{},\"kids\":[]}"))
      (else
        (let ((tag (car node))
              (attrs (pads/node-attrs node))
              (kids (pads/node-kids node)))
          (cond
            ((equal? tag 'row)
             (pads/scene-box "row" attrs kids path))
            ((equal? tag 'column)
             (pads/scene-box "column" attrs kids path))
            (else
             (string-append
               "{\"kind\":\"text\",\"id\":\""
               path
               "\",\"text\":\""
               (json-escape (pads/text-content node))
               "\",\"attrs\":"
               (pads/scene-attrs attrs)
               ",\"kids\":[]}"))))))))

(define pads/scene-box
  (lambda (kind attrs kids path)
    "A row/column scene object: kind, id, attrs, kids."
    (string-append
      "{\"kind\":\""
      kind
      "\",\"id\":\""
      path
      "\",\"attrs\":"
      (pads/scene-attrs attrs)
      ",\"kids\":"
      (pads/scene-kids kids path)
      "}")))

(define pads/scene-kids
  (lambda (kids path)
    "Kids as a JSON array with path ids parent/0, parent/1, ..."
    (string-append
      "["
      (pads/scene-kids-body kids path 0)
      "]")))

(define pads/scene-kids-body
  (lambda (kids path i)
    "Comma-joined kid objects at incrementing path indices."
    (cond
      ((null? kids) "")
      ((null? (cdr kids)) (pads/scene-node (car kids) (pads/scene-path path i)))
      (else
       (string-append
         (pads/scene-node (car kids) (pads/scene-path path i))
         ","
         (pads/scene-kids-body (cdr kids) path (+ i 1)))))))

(define pads/render-scene
  (lambda (node)
    "SCENE DIALECT v0 (normative; W4 — consumers: native/soil-hello).
Emits one JSON object string: {\"version\":0,\"root\":<node>} where a
node is {\"kind\":...,\"id\":...,\"text\":...? ,\"attrs\":{...},
\"kids\":[...]} — text fields only on text leaves. Kinds v0: row |
column | text (heading lowers to text with a level attr; link lowers to
text with href). Attrs are string-valued; the size grammar is fit |
grow:N | fixed:N. Ids are stable path ids: root \"0\", kids \"0/0\",
\"0/1\", ... (deterministic, synthesized — structed handshake later).
No Nopales eval happens on any consumer frame path (soil contract):
this is a pure data render."
    (string-append
      "{\"version\":0,\"root\":"
      (pads/scene-node node "0")
      "}")))

;; ---------- ops (data definitions only; pads/ops-contract) ----------

(define pads/op-replace
  (lambda (id subtree)
    "OP, data definition (no consumer yet — pads/ops-contract): replace
the node with stable id `id` by `subtree`. Web lowers to htmx morph;
native scene lowers to node replacement."
    (list 'replace id subtree)))

(define pads/op-insert-at
  (lambda (id idx node)
    "OP, data definition (no consumer yet): insert `node` at index idx
in the kids of node `id`."
    (list 'insert-at id idx node)))

(define pads/op-remove
  (lambda (id)
    "OP, data definition (no consumer yet): remove node `id` from its
parent."
    (list 'remove id)))

(define pads/op-move
  (lambda (id new-parent idx)
    "OP, data definition (no consumer yet): move node `id` to index idx
under parent `new-parent`."
    (list 'move id new-parent idx)))
