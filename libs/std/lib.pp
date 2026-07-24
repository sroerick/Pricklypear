;; Standard Lisp helpers (package list/, html/, css/, menu/, data/)
;; Loaded by (load-library "std") at image boot.

;; --- list/ ---
;; list/map, list/filter, list/foldl are KERNEL SPECIAL FORMS (eval.ml):
;; OCaml loops + apply, not recursive Lisp. These defines are fallbacks
;; only if the special forms are unavailable (should not happen in image).

(define list/map
  (lambda (f xs)
    (if (null? xs)
        (list)
        (cons (f (car xs)) (list/map f (cdr xs))))))

(define list/filter
  (lambda (p xs)
    (if (null? xs)
        (list)
        (if (p (car xs))
            (cons (car xs) (list/filter p (cdr xs)))
            (list/filter p (cdr xs))))))

(define list/foldl
  (lambda (f acc xs)
    (if (null? xs)
        acc
        (list/foldl f (f acc (car xs)) (cdr xs)))))

;; (list/range n) → (0 1 ... n-1) for synthetic tests / generators.
(define list/range
  (lambda (n)
    (list/range-acc n (list))))

(define list/range-acc
  (lambda (n acc)
    (if (<= n 0)
        acc
        (list/range-acc (- n 1) (cons (- n 1) acc)))))

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
              (cdr (cdr kvs)))))))

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
      xs)))

(define html/text
  (lambda (s) (html-escape s)))

(define html/attr
  (lambda (k v)
    (string-append (as-str k) "=\"" (html-escape (as-str v)) "\"")))

(define html/attrs
  (lambda (xs)
    (list/foldl
      (lambda (acc a)
        (if (string-eq acc "")
            (as-str a)
            (string-append acc " " (as-str a))))
      ""
      xs)))

(define html/tag
  (lambda (name attrs body)
    (let ((n (as-str name))
          (a (as-str attrs))
          (b (as-str body)))
      (if (string-eq a "")
          (string-append "<" n ">" b "</" n ">")
          (string-append "<" n " " a ">" b "</" n ">")))))

(define html/void
  (lambda (name attrs)
    (let ((n (as-str name))
          (a (as-str attrs)))
      (if (string-eq a "")
          (string-append "<" n ">")
          (string-append "<" n " " a ">")))))

(define html/h1 (lambda (body) (html/tag "h1" "" body)))
(define html/h2 (lambda (body) (html/tag "h2" "" body)))
(define html/h3 (lambda (body) (html/tag "h3" "" body)))
(define html/p
  (lambda (body)
    (html/tag "p" "" (html/text body))))
(define html/p*
  (lambda (attrs body)
    (html/tag "p" attrs body)))
(define html/span
  (lambda (attrs body)
    (html/tag "span" attrs body)))
(define html/div
  (lambda (attrs body)
    (html/tag "div" attrs body)))
(define html/ul
  (lambda (attrs body)
    (html/tag "ul" attrs body)))
(define html/li
  (lambda (attrs body)
    (html/tag "li" attrs body)))
(define html/strong
  (lambda (body)
    (html/tag "strong" "" (html/text body))))
(define html/code
  (lambda (body)
    (html/tag "code" "" (html/text body))))
(define html/pre
  (lambda (body)
    (html/tag "pre" "" body)))
(define html/label
  (lambda (attrs body)
    (html/tag "label" attrs body)))
(define html/button
  (lambda (attrs body)
    (html/tag "button" attrs body)))
(define html/a
  (lambda (href body)
    (html/tag "a" (html/attr "href" href) body)))
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
              (html/attr "class" class)))))))
(define html/form
  (lambda (method action attrs body)
    (html/tag "form"
      (html/attrs
        (list
          (html/attr "method" method)
          (html/attr "action" action)
          attrs))
      body)))
(define html/input
  (lambda (attrs)
    (html/void "input" attrs)))
(define html/hidden
  (lambda (name value)
    (html/input
      (html/attrs
        (list
          (html/attr "type" "hidden")
          (html/attr "name" name)
          (html/attr "value" value))))))
(define html/text-input
  (lambda (name attrs)
    (html/input
      (html/attrs
        (list
          (html/attr "type" "text")
          (html/attr "name" name)
          attrs)))))

(define html/textarea
  (lambda (name attrs body)
    (html/tag "textarea"
      (html/attrs
        (list
          (html/attr "name" name)
          attrs))
      (html-escape (as-str body)))))

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
      (html/text label))))

;; (html/select name attrs body) — body is options HTML string
(define html/select
  (lambda (name attrs body)
    (html/tag "select"
      (html/attrs
        (list
          (html/attr "name" name)
          attrs))
      body)))

(define html/wrap
  (lambda (title body)
    (render-html
      (html/tag "article" ""
        (html/join
          (list
            (html/h1 (html/text title))
            (html/div "" body)))))))

(define html/page
  (lambda (parts)
    (render-html (html/join parts))))

;; --- menu/  (navbar trees as plain lists) ---
;;
;;   (menu/link "home" "/")
;;   (menu/group "apps" (list (menu/link "todos" "/todos") …))
;;   (menu/bar (list item1 item2 …))  →  HTML string for <nav>
;;
;; Leaves are ("link" label href); groups are ("group" label kids-list).

(define menu/link
  (lambda (label href)
    (list "link" (as-str label) (as-str href))))

(define menu/group
  (lambda (label kids)
    (list "group" (as-str label) kids)))

;; Embed pre-rendered HTML as a nav item (e.g. capture dropdown form).
(define menu/raw
  (lambda (html-str)
    (list "raw" (as-str html-str) "")))

(define menu/render
  (lambda (item)
    (let ((kind (car item))
          (label (car (cdr item)))
          (rest (car (cdr (cdr item)))))
      (if (string-eq kind "link")
          (html/a rest (html/text label))
          (if (string-eq kind "group")
              (html/tag "details" (html/attr "class" "menu-drop")
                (html/join
                  (list
                    (html/tag "summary" (html/attr "class" "menu-drop-btn")
                      (html/text label))
                    (html/div (html/attr "class" "menu-drop-panel")
                      (html/join (list/map menu/render rest))))))
              (if (string-eq kind "raw")
                  label
                  ""))))))

(define menu/bar
  (lambda (items)
    (html/join (list/map menu/render items))))

;; --- data/ ---

(define data/get-field
  (lambda (type-name id field)
    (dict-get (get type-name id) field)))

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
    (list "rule" (as-str selector) props)))

(define css/media
  (lambda (query items)
    (list "media" (as-str query) items)))

(define css/raw
  (lambda (s)
    (list "raw" (as-str s))))

(define css/use
  (lambda (sheet)
    (list "use" sheet)))

(define css/sheet
  (lambda (items)
    (list "sheet" items)))

(define css/prop-name
  (lambda (k)
    (as-str k)))

(define css/prop-value
  (lambda (v)
    (as-str v)))

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
              (css/render-decls (cdr (cdr props))))))))

(define css/render-items
  (lambda (items)
    (list/foldl
      (lambda (acc it)
        (string-append acc (css/render-item it)))
      ""
      items)))

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
                              "")))))))))

(define css/render
  (lambda (sheet)
    (if (string-eq (type-of sheet) "lambda")
        (css/render (sheet))
        (if (null? sheet)
            ""
            (if (string-eq (as-str (car sheet)) "sheet")
                (css/render-items (car (cdr sheet)))
                (css/render-item sheet))))))

;; --- control / macros (loaded after kernel cond/and/or) ---

;; Null or empty string (after as-str). Shared by contacts/plan/chat/…
(define blank?
  (lambda (s)
    (if (null? s)
        #t
        (string-eq (as-str s) ""))))

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
