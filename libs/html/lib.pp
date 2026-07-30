;; package html -- exported from live branch system/main
;; Sync: scripts/pp_sync.py pull|push

(define html/a
  (lambda
    (href body)
    (html/tag "a" (html/attr "href" href) body)))

(define html/attr
  (lambda
    (k v)
    (string-append
      (as-str k)
      "=\""
      (html-escape (as-str v))
      "\"")))

(define html/attrs
  (lambda
    (xs)
    (list/foldl
      (lambda
        (acc a)
        (if
          (string-eq acc "")
          (as-str a)
          (string-append acc " " (as-str a))))
      ""
      xs)))

(define html/button
  (lambda (attrs body) (html/tag "button" attrs body)))

(define html/code
  (lambda (body) (html/tag "code" "" (html/text body))))

(define html/div
  (lambda (attrs body) (html/tag "div" attrs body)))

(define html/form
  (lambda
    (method action attrs body)
    (html/tag
      "form"
      (html/attrs
        (list
          (html/attr "method" method)
          (html/attr "action" action)
          attrs))
      body)))

(define html/h1
  (lambda (body) (html/tag "h1" "" body)))

(define html/h2
  (lambda (body) (html/tag "h2" "" body)))

(define html/h3
  (lambda (body) (html/tag "h3" "" body)))

(define html/hidden
  (lambda
    (name value)
    (html/input
      (html/attrs
        (list
          (html/attr "type" "hidden")
          (html/attr "name" name)
          (html/attr "value" value))))))

(define html/img
  (lambda
    (src alt class)
    (html/void
      "img"
      (html/attrs
        (list
          (html/attr "src" src)
          (html/attr "alt" alt)
          (if
            (string-eq (as-str class) "")
            ""
            (html/attr "class" class)))))))

(define html/input
  (lambda (attrs) (html/void "input" attrs)))

(define html/join
  (lambda
    (xs)
    (list/foldl
      (lambda (acc x) (string-append acc (as-str x)))
      ""
      xs)))

(define html/label
  (lambda (attrs body) (html/tag "label" attrs body)))

(define html/li
  (lambda (attrs body) (html/tag "li" attrs body)))

(define html/option
  (lambda
    (value label selected?)
    (html/tag
      "option"
      (if
        selected?
        (html/attrs
          (list
            (html/attr "value" value)
            (html/attr "selected" "selected")))
        (html/attr "value" value))
      (html/text label))))

(define html/p
  (lambda (body) (html/tag "p" "" (html/text body))))

(define html/p*
  (lambda (attrs body) (html/tag "p" attrs body)))

(define html/page
  (lambda (parts) (render-html (html/join parts))))

(define html/pre
  (lambda (body) (html/tag "pre" "" body)))

(define html/select
  (lambda
    (name attrs body)
    (html/tag
      "select"
      (html/attrs (list (html/attr "name" name) attrs))
      body)))

(define html/span
  (lambda (attrs body) (html/tag "span" attrs body)))

(define html/strong
  (lambda (body) (html/tag "strong" "" (html/text body))))

(define html/tag
  (lambda
    (name attrs body)
    (let
      ((n (as-str name)) (a (as-str attrs)) (b (as-str body)))
      (if
        (string-eq a "")
        (string-append
          "<"
          n
          ">"
          b
          "</"
          n
          ">")
        (string-append
          "<"
          n
          " "
          a
          ">"
          b
          "</"
          n
          ">")))))

(define html/text
  (lambda (s) (html-escape s)))

(define html/text-input
  (lambda
    (name attrs)
    (html/input
      (html/attrs
        (list
          (html/attr "type" "text")
          (html/attr "name" name)
          attrs)))))

(define html/textarea
  (lambda
    (name attrs body)
    (html/tag
      "textarea"
      (html/attrs (list (html/attr "name" name) attrs))
      (html-escape (as-str body)))))

(define html/ul
  (lambda (attrs body) (html/tag "ul" attrs body)))

(define html/void
  (lambda
    (name attrs)
    (let
      ((n (as-str name)) (a (as-str attrs)))
      (if
        (string-eq a "")
        (string-append "<" n ">")
        (string-append "<" n " " a ">")))))

(define html/wrap
  (lambda
    (title body)
    (render-html
      (html/tag
        "article"
        ""
        (html/join
          (list (html/h1 (html/text title)) (html/div "" body)))))))

