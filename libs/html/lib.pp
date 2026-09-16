;; package html -- exported from live branch system/main
;; Sync: scripts/pp_sync.py pull|push

(define html/a
  (lambda
    (href body)
    "An anchor tag with the given href around body."
    (html/tag "a" (html/attr "href" href) body)))

(define html/attr
  (lambda
    (k v)
    "Render one HTML attribute as k="v", escaping v."
    (string-append
      (as-str k)
      "=\""
      (html-escape (as-str v))
      "\"")))

(define html/attrs
  (lambda
    (xs)
    "Render a list of attribute strings space-separated."
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
  (lambda (attrs body) "A button tag with the given attrs and body." (html/tag "button" attrs body)))

(define html/code
  (lambda (body) "A code tag with escaped text body." (html/tag "code" "" (html/text body))))

(define html/div
  (lambda (attrs body) "A div tag with the given attrs and body." (html/tag "div" attrs body)))

(define html/form
  (lambda
    (method action attrs body)
    "A form tag with method and action attributes plus extra attrs."
    (html/tag
      "form"
      (html/attrs
        (list
          (html/attr "method" method)
          (html/attr "action" action)
          attrs))
      body)))

(define html/h1
  (lambda (body) "An h1 tag with the given body." (html/tag "h1" "" body)))

(define html/h2
  (lambda (body) "An h2 tag with the given body." (html/tag "h2" "" body)))

(define html/h3
  (lambda (body) "An h3 tag with the given body." (html/tag "h3" "" body)))

(define html/hidden
  (lambda
    (name value)
    "A hidden input field for name/value."
    (html/input
      (html/attrs
        (list
          (html/attr "type" "hidden")
          (html/attr "name" name)
          (html/attr "value" value))))))

(define html/img
  (lambda
    (src alt class)
    "An img void tag with src, alt, and optional class."
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
  (lambda (attrs) "An input void tag with the given attrs." (html/void "input" attrs)))

(define html/join
  (lambda
    (xs)
    "Concatenate a list of elements into one string."
    (list/foldl
      (lambda (acc x) (string-append acc (as-str x)))
      ""
      xs)))

(define html/label
  (lambda (attrs body) "A label tag with the given attrs and body." (html/tag "label" attrs body)))

(define html/li
  (lambda (attrs body) "An li tag with the given attrs and body." (html/tag "li" attrs body)))

(define html/option
  (lambda
    (value label selected?)
    "An option tag with value, label, and optional selected attrs."
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
  (lambda (body) "A p tag with escaped text body." (html/tag "p" "" (html/text body))))

(define html/p*
  (lambda (attrs body) "A p tag with the given attrs and body." (html/tag "p" attrs body)))

(define html/page
  (lambda (parts) "Render joined parts as a full HTML page." (render-html (html/join parts))))

(define html/pre
  (lambda (body) "A pre tag with the given body." (html/tag "pre" "" body)))

(define html/select
  (lambda
    (name attrs body)
    "A select tag with name attribute plus extra attrs."
    (html/tag
      "select"
      (html/attrs (list (html/attr "name" name) attrs))
      body)))

(define html/span
  (lambda (attrs body) "A span tag with the given attrs and body." (html/tag "span" attrs body)))

(define html/strong
  (lambda (body) "A strong tag with escaped text body." (html/tag "strong" "" (html/text body))))

;; --- block-level whitespace (product-experience.html-builders) ---
;; Mirrors libs/std (the live authority): block-level elements get ONE
;; newline before the open tag and ONE after the close tag; newlines sit
;; only OUTSIDE tag boundaries, so nothing lands inside
;; pre/textarea/script/style content and rendering is unchanged.
(define html/block-tags
  (list
    "div" "p" "h1" "h2" "h3" "h4" "h5" "h6"
    "ul" "ol" "li" "dl" "dt" "dd"
    "table" "thead" "tbody" "tfoot" "tr"
    "section" "header" "footer" "main" "nav" "article"
    "form" "fieldset" "blockquote" "figure" "figcaption" "aside"
    "pre" "hr"))

(define html/block-member?
  (lambda
    (n tags)
    "Internal: the first block tag equal to n, else ()."
    (if
      (null? tags)
      ()
      (if
        (string-eq n (car tags))
        n
        (html/block-member? n (cdr tags))))))

(define html/block-tag?
  (lambda
    (name)
    "True when name is in the block-level tag set (exact lowercase match)."
    (html/block-member? (as-str name) html/block-tags)))

(define html/tag-core
  (lambda
    (n a b)
    "Internal: bare <name attrs>body</name> concatenation."
    (if
      (string-eq a "")
      (string-append "<" n ">" b "</" n ">")
      (string-append "<" n " " a ">" b "</" n ">"))))

(define html/tag-emit
  (lambda
    (n a b)
    "Internal: wrap tag-core output in block-boundary newlines when n is block-level."
    (if
      (html/block-tag? n)
      (string-append "\n" (html/tag-core n a b) "\n")
      (html/tag-core n a b))))

(define html/tag
  (lambda
    (name attrs body)
    "Wrap body in an opening/closing tag, adding attrs when non-empty. Block-level names emit one newline around the open/close boundary — never inside the element, so pre/textarea/script/style content is untouched."
    (html/tag-emit
      (as-str name)
      (as-str attrs)
      (as-str body))))

(define html/text
  (lambda (s) "HTML-escapes the string s." (html-escape s)))

(define html/text-input
  (lambda
    (name attrs)
    "A text input field with name and extra attrs."
    (html/input
      (html/attrs
        (list
          (html/attr "type" "text")
          (html/attr "name" name)
          attrs)))))

(define html/textarea
  (lambda
    (name attrs body)
    "A textarea tag with name, attrs, and escaped body."
    (html/tag
      "textarea"
      (html/attrs (list (html/attr "name" name) attrs))
      (html-escape (as-str body)))))

(define html/ul
  (lambda (attrs body) "A ul tag with the given attrs and body." (html/tag "ul" attrs body)))

(define html/void
  (lambda
    (name attrs)
    "Render a void (self-closing) tag, adding attrs when non-empty."
    (let
      ((n (as-str name)) (a (as-str attrs)))
      (if
        (string-eq a "")
        (string-append "<" n ">")
        (string-append "<" n " " a ">")))))

(define html/wrap
  (lambda
    (title body)
    "Wrap title and body in an article with a header."
    (render-html
      (html/tag
        "article"
        ""
        (html/join
          (list (html/h1 (html/text title)) (html/div "" body)))))))

