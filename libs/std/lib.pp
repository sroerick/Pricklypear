;; Standard Lisp helpers (package list/, html/, css/, menu/, data/)
;; Loaded by (load-library "std") at image boot.
;; Docs: trailing string on define (or leading multi-body lambda string).

(package std
  (export
    list/map
    list/filter
    list/foldl
    list/range
    list/take
    list-ref
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
    json-escape
    blank?
    string-find-pos))

;; --- control / macros (loaded after kernel cond/and/or) ---

;; Null or empty string (after as-str). Shared by contacts/plan/chat/…
(define blank?
  (lambda (s)
    (if (null? s)
        #t
        (string-eq (as-str s) "")))
  "True if s is nil or empty string.")

(define css/media
  (lambda (query items)
    (list "media" (as-str query) items))
  "CSS @media node wrapping child items.")

(define css/prop-name
  (lambda (k)
    (as-str k))
  "CSS property name as string.")

(define css/prop-value
  (lambda (v)
    (as-str v))
  "CSS property value as string.")

(define css/raw
  (lambda (s)
    (list "raw" (as-str s)))
  "Raw CSS text node (escape hatch).")

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

(define css/render-items
  (lambda (items)
    (list/foldl
      (lambda (acc it)
        (string-append acc (css/render-item it)))
      ""
      items))
  "Render list of CSS AST items to a CSS string.")

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

(define css/sheet
  (lambda (items)
    (list "sheet" items))
  "Top-level CSS sheet: (\"sheet\" items).")

(define css/use
  (lambda (sheet)
    (list "use" sheet))
  "Include another sheet node by reference.")

;; --- data/ ---

(define data/get-field
  (lambda (type-name id field)
    (dict-get (get type-name id) field))
  "dict-get field from (get type-name id) row.")

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
(define filter
  (lambda (p xs) (list/filter p xs))
  "Keep elements of xs where p is truthy (alias of list/filter).")
(define foldl
  (lambda (f init xs) (list/foldl f init xs))
  "Left fold f over xs from init (alias of list/foldl).")
(define foldr
  (lambda (f init xs) (list/foldr f init xs))
  "Right fold f over xs from init (alias of list/foldr).")

(define html/a
  (lambda (href body)
    (html/tag "a" (html/attr "href" href) body))
  "Anchor with href attr and raw body.")

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

;; O(1) membership for the render hot path (mirrors libs/html, 2026-09-19
;; obituary: the recursive list walk cost ~233k steps per glochid shell
;; render). DELAYED like the libs/html twin — the fold binds without
;; evaluating (the seed-sources no-define-time gate), first (force) per
;; session memoizes, so the hot path still pays the fold ONCE.
(define html/block-lookup
  (delay
    (list/foldl
      (lambda (acc name) (dict-set acc name name))
      "{}"
      html/block-tags)))

(define html/block-member?
  (lambda (n tags)
    (if (null? tags)
        ()
        (if (string-eq n (car tags))
            n
            (html/block-member? n (cdr tags)))))
  "Internal: the first block tag equal to n, else ().")

(define html/block-tag?
  (lambda (name)
    (dict-get (force html/block-lookup) (as-str name)))
  "True when name is in the block-level tag set (exact lowercase match).")

;; --- html block-level whitespace (product-experience.html-builders) ---
;; Block-level elements get ONE newline before the open tag and ONE after
;; the close tag; bodies/attrs/text are verbatim and the newlines sit only
;; OUTSIDE tag boundaries — so no newline can ever land inside
;; pre/textarea/script/style content, and rendering is unchanged (block
;; boundaries already break lines). Inline names render exactly as before.
(define html/block-tags
  (list
    "div" "p" "h1" "h2" "h3" "h4" "h5" "h6"
    "ul" "ol" "li" "dl" "dt" "dd"
    "table" "thead" "tbody" "tfoot" "tr"
    "section" "header" "footer" "main" "nav" "article"
    "form" "fieldset" "blockquote" "figure" "figcaption" "aside"
    "pre" "hr"))

(define html/button
  (lambda (attrs body)
    (html/tag "button" attrs body))
  "button with attrs and raw body.")

(define html/code
  (lambda (body)
    (html/tag "code" "" (html/text body)))
  "code with HTML-escaped text.")

(define html/div
  (lambda (attrs body)
    (html/tag "div" attrs body))
  "div with attrs and raw body.")

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

(define html/h1
  (lambda (body) (html/tag "h1" "" body))
  "h1 with raw body string (caller escapes if needed).")

(define html/h2
  (lambda (body) (html/tag "h2" "" body))
  "h2 with raw body string.")

(define html/h3
  (lambda (body) (html/tag "h3" "" body))
  "h3 with raw body string.")

(define html/hidden
  (lambda (name value)
    (html/input
      (html/attrs
        (list
          (html/attr "type" "hidden")
          (html/attr "name" name)
          (html/attr "value" value)))))
  "Hidden input name/value.")

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

(define html/input
  (lambda (attrs)
    (html/void "input" attrs))
  "input void tag from attrs string.")

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

(define html/label
  (lambda (attrs body)
    (html/tag "label" attrs body))
  "label with attrs and raw body.")

(define html/li
  (lambda (attrs body)
    (html/tag "li" attrs body))
  "li with attrs and raw body.")

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

(define html/p
  (lambda (body)
    (html/tag "p" "" (html/text body)))
  "Paragraph with HTML-escaped text body.")

(define html/p*
  (lambda (attrs body)
    (html/tag "p" attrs body))
  "Paragraph with attrs and raw body (no auto-escape).")

(define html/page
  (lambda (parts)
    (render-html (html/join parts)))
  "render-html of joined page parts (list of HTML strings).")

(define html/pre
  (lambda (body)
    (html/tag "pre" "" body))
  "pre with raw body (no auto-escape).")

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

(define html/span
  (lambda (attrs body)
    (html/tag "span" attrs body))
  "span with attrs and raw body.")

(define html/strong
  (lambda (body)
    (html/tag "strong" "" (html/text body)))
  "strong with HTML-escaped text.")

(define html/tag
  (lambda (name attrs body)
    (html/tag-emit (as-str name) (as-str attrs) (as-str body)))
  "Element <name attrs>body</name>; block-level names emit one newline around the open/close boundary (never inside the element, so pre/textarea/script/style content is untouched). Inline names render as before; attrs may be empty string.")

(define html/tag-core
  (lambda (n a b)
    (if (string-eq a "")
        (string-append "<" n ">" b "</" n ">")
        (string-append "<" n " " a ">" b "</" n ">")))
  "Internal: bare <name attrs>body</name> concatenation.")

(define html/tag-emit
  (lambda (n a b)
    (if (html/block-tag? n)
        (string-append "\n" (html/tag-core n a b) "\n")
        (html/tag-core n a b)))
  "Internal: wrap tag-core output in block-boundary newlines when n is block-level.")

(define html/text
  (lambda (s) (html-escape s))
  "HTML-escape s for safe text content.")

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

(define html/ul
  (lambda (attrs body)
    (html/tag "ul" attrs body))
  "ul with attrs and raw body (usually joined li strings).")

(define html/void
  (lambda (name attrs)
    (let ((n (as-str name))
          (a (as-str attrs)))
      (if (string-eq a "")
          (string-append "<" n ">")
          (string-append "<" n " " a ">"))))
  "Void element <name attrs> (no close tag).")

(define html/wrap
  (lambda (title body)
    (render-html
      (html/tag "article" ""
        (html/join
          (list
            (html/h1 (html/text title))
            (html/div "" body))))))
  "render-html article with h1 title and body div.")

;; --- json (string escaping for JSON string/array literals) ---
;;
;;   (json-escape s) → body of a JSON "..." literal, safe to embed.
;;   ONE shared escaper: the per-package copies (book/json-esc,
;;   pads/json-escape, ntfy/json-esc, upsets/json-esc) diverged —
;;   different escape sets, and a value containing a newline produced
;;   invalid JSON (i-1788670711). Backslash is replaced FIRST so the
;;   escape sequences inserted by the later passes are not re-escaped.

(define json-escape
  (lambda (s)
    (string-replace
      (string-replace
        (string-replace
          (string-replace
            (string-replace (as-str s) "\\" "\\\\")
            "\""
            "\\\"")
          "\n"
          "\\n")
        "\r"
        "\\r")
      "\t"
      "\\t"))
  "Escape s for embedding in a JSON string literal: backslash, quote,
newline, CR, tab (coerces via as-str; backslash first).")

(define list-ref
  nth)

(define list/filter
  (lambda
    (p xs)
    (if
      (null? xs)
      (list)
      (if
        (p (car xs))
        (cons (car xs) (list/filter p (cdr xs)))
        (list/filter p (cdr xs))))))

(define list/foldl
  (lambda
    (f acc xs)
    (if
      (null? xs)
      acc
      (list/foldl f (f acc (car xs)) (cdr xs)))))

(define list/map
  (lambda
    (f xs)
    (if
      (null? xs)
      (list)
      (cons (f (car xs)) (list/map f (cdr xs))))))

;; --- list/ ---
;; list/map, list/filter, list/foldl (and list/foldr) are KERNEL SPECIAL
;; FORMS (eval.ml): OCaml loops + apply, provided by the image; defining
;; them here trips assert_bindable_name (BORGE-1785213238) and aborts std
;; seeding. Do NOT re-add fallback defines — the kernel always provides
;; them. (The libs/list package did exactly that and failed to seed at
;; every boot; deleted 2026-09-09, i-1788670156 — list/take moved here.)

;; (list/range n) → (0 1 ... n-1) for synthetic tests / generators.
(define list/range
  (lambda (n)
    (list/range-acc n (list)))
  "List of integers 0 .. n-1.")

;; (list-ref xs i) — the HISTORICAL name for nth, still called by
;; astro (11), rb (11) and calendar (1) (i-1790560893). OBITUARY: this
;; file used to carry a vault alias (define list-ref nth …, f6c2d8f) so
;; clean bootstraps would seed it — but on any main where the name is
;; already prim-bound the define trips the redefine-kernel-prim guard
;; and aborts (reseed-package "std" died at form 3, 2026-09-29, issue
;; card 6bd8f701). The alias now lives IN THE KERNEL: pure/prims_pure.ml
;; registers ("list-ref", prim_nth), so fresh bootstraps and existing
;; mains both resolve it as a prim and this vault must NOT redefine it.
;; Kept in the export list: the name is part of std's public surface.

(define list/range-acc
  (lambda (n acc)
    (if (<= n 0)
        acc
        (list/range-acc (- n 1) (cons (- n 1) acc))))
  "Internal accumulator for list/range.")

;; (list/take xs n) → first n elements of xs. Was libs/list's only
;; surviving non-kernel define; calendar/upcoming calls it, so it must
;; exist on every clean environment (std seeds first).
(define list/take
  (lambda (xs n)
    (list/take-acc xs n (list)))
  "Return the first n elements of xs.")

(define list/take-acc
  (lambda (xs n acc)
    (if (or (null? xs) (<= n 0))
        (reverse acc)
        (list/take-acc (cdr xs) (- n 1) (cons (car xs) acc))))
  "Internal accumulator for list/take.")

;; --- unprefixed list aliases (conventional Lisp names; thin wrappers
;;     over the kernel list/ special forms). Not part of the frozen
;;     list/ set, but provided by std for ergonomics so (map f xs) etc.
;;     work without the list/ prefix. ---
(define map
  (lambda (f xs) (list/map f xs))
  "Map f over xs (alias of list/map).")

(define menu/bar
  (lambda (items)
    (html/join (list/map (lambda (i) (menu/render-d i 0)) items)))
  "Render menu item list to a nav HTML fragment.")

(define menu/group
  (lambda (label kids)
    (list "group" (as-str label) kids))
  "Nav dropdown group: (\"group\" label kids-list).")

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

;; Embed pre-rendered HTML as a nav item (e.g. capture dropdown form).
(define menu/raw
  (lambda (html-str)
    (list "raw" (as-str html-str) ""))
  "Nav raw HTML item: (\"raw\" html \"\").")

(define menu/render
  (lambda (item) (menu/render-d item 0))
  "Render one menu item (link, group, or raw) to HTML string.")

;; depth-aware renderer: d=0 top-level nav, d>0 nested fly-out.
;; Top-level groups share name="pp-nav" so the browser's native
;; <details name> accordion auto-closes siblings (replaces the old
;; chrome toggle JS). Nested groups get name="pp-sub-<label>" per
;; fly-out column, so opening one submenu closes its siblings.
(define menu/render-d
  (lambda
    (item d)
    "Depth-aware menu renderer: link/group/raw item to HTML; open groups get accordion names by depth."
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
