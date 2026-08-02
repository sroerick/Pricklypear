;; Standard Lisp helpers (package list/, html/, css/, menu/, data/)
;; Loaded by (load-library "std") at image boot.
;; Docs: trailing string on define (or leading multi-body lambda string).

(package std
  (export
    list/map
    list/filter
    list/foldl
    list/range
    map
    filter
    foldl
    foldr
    dict-set*
    html/join
    html/text
    html/attr
    html/attrs
    html/tag
    html/void
    html/h1
    html/h2
    html/h3
    html/p
    html/p*
    html/span
    html/div
    html/ul
    html/li
    html/strong
    html/code
    html/pre
    html/label
    html/button
    html/a
    html/img
    html/form
    html/input
    html/hidden
    html/text-input
    html/textarea
    html/option
    html/select
    html/wrap
    html/page
    menu/link
    menu/group
    menu/raw
    menu/render
    menu/bar
    data/get-field
    css/rule
    css/media
    css/raw
    css/use
    css/sheet
    css/render
    blank?
    string-find-pos))

;; --- list/ ---
;; list/map, list/filter, list/foldl are KERNEL SPECIAL FORMS (eval.ml):
;; OCaml loops + apply, not recursive Lisp. These defines are fallbacks
;; only if the special forms are unavailable (should not happen in image).

(define list/map
  (lambda (f xs)
    (if (null? xs)
        (list)
        (cons (f (car xs)) (list/map f (cdr xs)))))
  "Map f over xs (Lisp fallback; image prefers special form).")

(define list/filter
  (lambda (p xs)
    (if (null? xs)
        (list)
        (if (p (car xs))
            (cons (car xs) (list/filter p (cdr xs)))
            (list/filter p (cdr xs)))))
  "Keep elements of xs where p is truthy (Lisp fallback).")

(define list/foldl
  (lambda (f acc xs)
    (if (null? xs)
        acc
        (list/foldl f (f acc (car xs)) (cdr xs))))
  "Left fold f over xs starting from acc (Lisp fallback).")

;; (list/range n) → (0 1 ... n-1) for synthetic tests / generators.
(define list/range
  (lambda (n)
    (list/range-acc n (list)))
  "List of integers 0 .. n-1.")

(define list/range-acc
  (lambda (n acc)
    (if (<= n 0)
        acc
        (list/range-acc (- n 1) (cons (- n 1) acc))))
  "Internal accumulator for list/range.")

;; --- unprefixed list aliases (conventional Lisp names; thin wrappers
;;     over the kernel list/ special forms). Not part of the frozen
;;     list/ set, but provided by std for ergonomics so (map f xs) etc.
;;     work without the list/ prefix. ---
(define map
  (lambda (f xs) (list/map f xs))
  "Map f over xs (alias of list/map).")
(define filter
  (lambda (p xs) (list/filter p xs))
  "Keep elements of xs where p is truthy (alias of list/filter).")
(define foldl
  (lambda (f init xs) (list/foldl f init xs))
  "Left fold f over xs from init (alias of list/foldl).")
(define foldr
  (lambda (f init xs) (list/foldr f init xs))
  "Right fold f over xs from init (alias of list/foldr).")

;; --- binding/control macros ---
;; Unhygienic defmacro + quasiquote (language-roadmap decision default).
;; Single-body only: defmacro params do not support dotted rest args.

;; (let* ((a 1) (b (+ a 1))) body) — sequential bindings; each value
;; sees earlier names. Expands to nested parallel let.
(defmacro let* (bindings body)
  (if (null? bindings)
      body
      (quasiquote
        (let ((unquote (car bindings)))
          (let* (unquote (cdr bindings)) (unquote body))))))

;; (when test body) — evaluate body only when test is truthy; else ().
(defmacro when (test body)
  (quasiquote (if (unquote test) (unquote body) ())))

;; (unless test body) — evaluate body only when test is falsey.
(defmacro unless (test body)
  (quasiquote (if (unquote test) () (unquote body))))

;; --- dict/ ---

;; (dict-set* json (list "k1" v1 "k2" v2 …)) — flat multi-field update.
;; Prefer this over nested (dict-set (dict-set …)).
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

;; --- html/  (strings → render-html; no raw tag soup in apps) ---
;;
;; Pattern:
;;   (render-html
;;     (html/join (list
;;       (html/h1 "Todos")
;;       (html/div (html/attr "class" "card") body))))
;;
;; Attrs are space-joined strings from html/attr.
;; Body is always a string (use html/join for children).

(define html/join
  (lambda (xs)
    (list/foldl
      (lambda (acc x) (string-append acc (as-str x)))
      ""
      xs))
  "Concatenate list of values into one string.")

(define html/text
  (lambda (s) (html-escape s))
  "HTML-escape s for safe text content.")

(define html/attr
  (lambda (k v)
    (string-append (as-str k) "=\"" (html-escape (as-str v)) "\""))
  "One HTML attribute k=\"escaped-v\".")

(define html/attrs
  (lambda (xs)
    (list/foldl
      (lambda (acc a)
        (if (string-eq acc "")
            (as-str a)
            (string-append acc " " (as-str a))))
      ""
      xs))
  "Space-join attribute strings.")

(define html/tag
  (lambda (name attrs body)
    (let ((n (as-str name))
          (a (as-str attrs))
          (b (as-str body)))
      (if (string-eq a "")
          (string-append "<" n ">" b "</" n ">")
          (string-append "<" n " " a ">" b "</" n ">"))))
  "Element <name attrs>body</name>; attrs may be empty string.")

(define html/void
  (lambda (name attrs)
    (let ((n (as-str name))
          (a (as-str attrs)))
      (if (string-eq a "")
          (string-append "<" n ">")
          (string-append "<" n " " a ">"))))
  "Void element <name attrs> (no close tag).")

(define html/h1
  (lambda (body) (html/tag "h1" "" body))
  "h1 with raw body string (caller escapes if needed).")

(define html/h2
  (lambda (body) (html/tag "h2" "" body))
  "h2 with raw body string.")

(define html/h3
  (lambda (body) (html/tag "h3" "" body))
  "h3 with raw body string.")

(define html/p
  (lambda (body)
    (html/tag "p" "" (html/text body)))
  "Paragraph with HTML-escaped text body.")

(define html/p*
  (lambda (attrs body)
    (html/tag "p" attrs body))
  "Paragraph with attrs and raw body (no auto-escape).")

(define html/span
  (lambda (attrs body)
    (html/tag "span" attrs body))
  "span with attrs and raw body.")

(define html/div
  (lambda (attrs body)
    (html/tag "div" attrs body))
  "div with attrs and raw body.")

(define html/ul
  (lambda (attrs body)
    (html/tag "ul" attrs body))
  "ul with attrs and raw body (usually joined li strings).")

(define html/li
  (lambda (attrs body)
    (html/tag "li" attrs body))
  "li with attrs and raw body.")

(define html/strong
  (lambda (body)
    (html/tag "strong" "" (html/text body)))
  "strong with HTML-escaped text.")

(define html/code
  (lambda (body)
    (html/tag "code" "" (html/text body)))
  "code with HTML-escaped text.")

(define html/pre
  (lambda (body)
    (html/tag "pre" "" body))
  "pre with raw body (no auto-escape).")

(define html/label
  (lambda (attrs body)
    (html/tag "label" attrs body))
  "label with attrs and raw body.")

(define html/button
  (lambda (attrs body)
    (html/tag "button" attrs body))
  "button with attrs and raw body.")

(define html/a
  (lambda (href body)
    (html/tag "a" (html/attr "href" href) body))
  "Anchor with href attr and raw body.")

;; Void <img>. src/alt required; optional class via third arg ("" ok).
(define html/img
  (lambda (src alt class)
    (html/void "img"
      (html/attrs
        (list
          (html/attr "src" src)
          (html/attr "alt" alt)
          (if (string-eq (as-str class) "")
              ""
              (html/attr "class" class))))))
  "img void tag; class may be empty string to omit.")

(define html/form
  (lambda (method action attrs body)
    (html/tag "form"
      (html/attrs
        (list
          (html/attr "method" method)
          (html/attr "action" action)
          attrs))
      body))
  "form with method, action, extra attrs string, and body.")

(define html/input
  (lambda (attrs)
    (html/void "input" attrs))
  "input void tag from attrs string.")

(define html/hidden
  (lambda (name value)
    (html/input
      (html/attrs
        (list
          (html/attr "type" "hidden")
          (html/attr "name" name)
          (html/attr "value" value)))))
  "Hidden input name/value.")

(define html/text-input
  (lambda (name attrs)
    (html/input
      (html/attrs
        (list
          (html/attr "type" "text")
          (html/attr "name" name)
          attrs))))
  "Text input with name and extra attrs string.")

(define html/textarea
  (lambda (name attrs body)
    (html/tag "textarea"
      (html/attrs
        (list
          (html/attr "name" name)
          attrs))
      (html-escape (as-str body))))
  "textarea; body is HTML-escaped.")

;; (html/option value label selected?) — selected? is bool/truthy
(define html/option
  (lambda (value label selected?)
    (html/tag "option"
      (if selected?
          (html/attrs
            (list
              (html/attr "value" value)
              (html/attr "selected" "selected")))
          (html/attr "value" value))
      (html/text label)))
  "option element; selected? adds selected attr.")

;; (html/select name attrs body) — body is options HTML string
(define html/select
  (lambda (name attrs body)
    (html/tag "select"
      (html/attrs
        (list
          (html/attr "name" name)
          attrs))
      body))
  "select with name, attrs, and options body HTML.")

(define html/wrap
  (lambda (title body)
    (render-html
      (html/tag "article" ""
        (html/join
          (list
            (html/h1 (html/text title))
            (html/div "" body))))))
  "render-html article with h1 title and body div.")

(define html/page
  (lambda (parts)
    (render-html (html/join parts)))
  "render-html of joined page parts (list of HTML strings).")

;; --- menu/  (navbar trees as plain lists) ---
;;
;;   (menu/link "home" "/")
;;   (menu/group "apps" (list (menu/link "todos" "/todos") …))
;;   (menu/bar (list item1 item2 …))  →  HTML string for <nav>
;;
;; Leaves are ("link" label href); groups are ("group" label kids-list).

(define menu/link
  (lambda (label href)
    (list "link" (as-str label) (as-str href)))
  "Nav leaf: (\"link\" label href).")

(define menu/group
  (lambda (label kids)
    (list "group" (as-str label) kids))
  "Nav dropdown group: (\"group\" label kids-list).")

;; Embed pre-rendered HTML as a nav item (e.g. capture dropdown form).
(define menu/raw
  (lambda (html-str)
    (list "raw" (as-str html-str) ""))
  "Nav raw HTML item: (\"raw\" html \"\").")

;; depth-aware renderer: d=0 top-level nav, d>0 nested fly-out.
;; Top-level groups share name="pp-nav" so the browser's native
;; <details name> accordion auto-closes siblings (replaces the old
;; chrome toggle JS). Nested groups get name="pp-sub-<label>" per
;; fly-out column, so opening one submenu closes its siblings.
(define menu/render-d
  (lambda
    (item d)
    (let
      ((kind (car item))
        (label (car (cdr item)))
        (rest (car (cdr (cdr item)))))
      (if
        (string-eq kind "link")
        (html/a rest (html/text label))
        (if
          (string-eq kind "group")
          (html/tag
            "details"
            (html/attrs
              (list
                (html/attr "class" "menu-drop")
                (html/attr
                  "name"
                  (if
                    (= d 0)
                    "pp-nav"
                    (string-append "pp-sub-" (as-str label))))))
            (html/join
              (list
                (html/tag
                  "summary"
                  (html/attr "class" "menu-drop-btn")
                  (html/text label))
                (html/div
                  (html/attr "class" "menu-drop-panel")
                  (html/join
                    (list/map
                      (lambda (k) (menu/render-d k (+ d 1)))
                      rest))))))
          (if (string-eq kind "raw") label ""))))))

(define menu/render
  (lambda (item) (menu/render-d item 0))
  "Render one menu item (link, group, or raw) to HTML string.")

(define menu/bar
  (lambda (items)
    (html/join (list/map (lambda (i) (menu/render-d i 0)) items)))
  "Render menu item list to a nav HTML fragment.")

;; --- data/ ---

(define data/get-field
  (lambda (type-name id field)
    (dict-get (get type-name id) field))
  "dict-get field from (get type-name id) row.")

;; --- css/  (sheets → css/render; no raw rule soup in apps) ---
;;
;; Path B product theme (image.product-theme): styles are Lisp data.
;; Selectors stay strings; declarations are flat k v k v lists.
;;
;;   (css/sheet
;;     (list
;;       (css/rule "body" (list "margin" 0 "background" theme/bg))
;;       (css/media "max-width:40rem"
;;         (list (css/rule "main" (list "padding" ".5rem"))))
;;       (css/raw "/* rare escape */")))
;;   (css/render sheet) → CSS string

(define css/rule
  (lambda (selector props)
    (list "rule" (as-str selector) props))
  "CSS rule node: (\"rule\" selector flat-props).")

(define css/media
  (lambda (query items)
    (list "media" (as-str query) items))
  "CSS @media node wrapping child items.")

(define css/raw
  (lambda (s)
    (list "raw" (as-str s)))
  "Raw CSS text node (escape hatch).")

(define css/use
  (lambda (sheet)
    (list "use" sheet))
  "Include another sheet node by reference.")

(define css/sheet
  (lambda (items)
    (list "sheet" items))
  "Top-level CSS sheet: (\"sheet\" items).")

(define css/prop-name
  (lambda (k)
    (as-str k))
  "CSS property name as string.")

(define css/prop-value
  (lambda (v)
    (as-str v))
  "CSS property value as string.")

(define css/render-decls
  (lambda (props)
    (if (null? props)
        ""
        (if (null? (cdr props))
            ""
            (string-append
              (css/prop-name (car props))
              ":"
              (css/prop-value (car (cdr props)))
              ";"
              (css/render-decls (cdr (cdr props)))))))
  "Render flat k v k v prop list to CSS declarations.")

(define css/render-items
  (lambda (items)
    (list/foldl
      (lambda (acc it)
        (string-append acc (css/render-item it)))
      ""
      items))
  "Render list of CSS AST items to a CSS string.")

(define css/render-item
  (lambda (item)
    (if (null? item)
        ""
        (let ((kind (as-str (car item))))
          (if (string-eq kind "rule")
              (let ((sel (car (cdr item)))
                    (props (car (cdr (cdr item)))))
                (string-append
                  (as-str sel) "{" (css/render-decls props) "}\n"))
              (if (string-eq kind "media")
                  (let ((q (car (cdr item)))
                        (kids (car (cdr (cdr item)))))
                    (string-append
                      "@media(" (as-str q) "){\n"
                      (css/render-items kids)
                      "}\n"))
                  (if (string-eq kind "raw")
                      (string-append (as-str (car (cdr item))) "\n")
                      (if (string-eq kind "use")
                          (css/render (car (cdr item)))
                          (if (string-eq kind "sheet")
                              (css/render-items (car (cdr item)))
                              ""))))))))
  "Render one CSS AST node (rule/media/raw/use/sheet).")

(define css/render
  (lambda (sheet)
    (if (string-eq (type-of sheet) "lambda")
        (css/render (sheet))
        (if (null? sheet)
            ""
            (if (string-eq (as-str (car sheet)) "sheet")
                (css/render-items (car (cdr sheet)))
                (css/render-item sheet)))))
  "Render sheet AST or thunk to CSS text.")

;; --- control / macros (loaded after kernel cond/and/or) ---

;; Null or empty string (after as-str). Shared by contacts/plan/chat/…
(define blank?
  (lambda (s)
    (if (null? s)
        #t
        (string-eq (as-str s) "")))
  "True if s is nil or empty string.")

(defmacro when (test body)
  `(if ,test ,body ()))

(defmacro unless (test body)
  `(if ,test () ,body))

;; HTTP page helper: POST runs action then hx fragment or redirect;
;; GET renders page body (already a full html/page or raw fragment).
;; get-html and post-html are expressions (usually (html/page …)).
(defmacro respond-post (redirect-path post-action get-html)
  `(if (string-eq (request-method) "POST")
       (seq
         ,post-action
         (if (hx-request?)
             (render-html ,get-html)
             (redirect ,redirect-path)))
       (html/page ,get-html)))

;; Empty list → muted message; else map items into a ul.
(defmacro list-or-empty (xs empty-msg item-fn)
  `(if (null? ,xs)
       (html/p* (html/attr "class" "muted") ,empty-msg)
       (html/ul ""
         (html/join (list/map ,item-fn ,xs)))))

;; --- discoverability (define-docstring companion) ---

;; Find first index of delim in s at or after start, or -1.
(define string-find-pos
  (lambda (s delim start dlen)
    "Index of delim in s starting at start, or -1."
    (if (>= start (- (string-length s) (- dlen 1)))
        -1
        (if (string-eq (substring s start dlen) delim)
            start
            (string-find-pos s delim (+ start 1) dlen)))))

;; Split s on delim (string). Empty or no match → single-element list.
(define string-split
  (lambda (s delim)
    "Split string s on delimiter string delim."
    (let ((dlen (string-length delim))
          (slen (string-length s)))
      (if (or (= slen 0) (not (string-contains? s delim)))
          (list s)
          (let ((pos (string-find-pos s delim 0 dlen)))
            (if (= pos -1)
                (list s)
                (cons
                  (substring s 0 pos)
                  (string-split
                    (substring s (+ pos dlen) (- slen (+ pos dlen)))
                    delim))))))))

;; Package prefix before first / (or whole name).
(define prefix-of
  (lambda (name)
    "First path segment of a binding name (before /)."
    (car (string-split (as-str name) "/"))))

;; Substring search over visible binding names.
(define apropos
  (lambda (pat)
    "Names from (list-bindings) whose string form contains pat."
    (list/filter
      (lambda (n) (string-contains? (as-str n) (as-str pat)))
      (list-bindings))))
