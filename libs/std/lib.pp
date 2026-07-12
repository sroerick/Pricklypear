;; Standard Lisp helpers (package list/, html/, data/)
;; Loaded by (load-library "std") at image boot.

;; --- list/ ---

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
              "")))))

(define menu/bar
  (lambda (items)
    (html/join (list/map menu/render items))))

;; --- data/ ---

(define data/get-field
  (lambda (type-name id field)
    (dict-get (get type-name id) field)))
