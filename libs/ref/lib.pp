;; Live function reference — package ref
;; Depends on std (html/*, apropos, prefix-of) and kernel (function-doc).

(define ref/fn-li
  (lambda (fn)
    "One reference list item: name + optional docstring."
    (let ((doc (function-doc (as-str fn))))
      (if (null? doc)
          (html/li "" (html/code (as-str fn)))
          (html/li ""
            (html/join
              (list
                (html/code (as-str fn))
                (html/span
                  (html/attr "class" "muted")
                  (string-append " — " (as-str doc))))))))))

(define ref/packages
  (lambda ()
    "Distinct package prefixes from visible bindings."
    (list/foldl
      (lambda (acc name)
        (let ((p (prefix-of name)))
          (if (null? (list/filter (lambda (x) (string-eq (as-str x) p)) acc))
              (cons p acc)
              acc)))
      (list)
      (list-bindings))))

(define ref/render-pkg
  (lambda (pkg)
    "Card listing functions in one package prefix."
    (let ((fns
            (list/filter
              (lambda (n)
                (string-eq (prefix-of n) pkg))
              (list-bindings))))
      (html/div
        (html/attr "class" "card")
        (html/join
          (list
            (html/h2 (html/text pkg))
            (html/p*
              (html/attr "class" "muted")
              (string-append (as-str (count fns)) " functions"))
            (html/ul ""
              (html/join (list/map ref/fn-li fns)))))))))

(define ref/page
  (lambda ()
    "GET /ref — browsable function list with docstrings when present."
    (let ((q (request-param "q"))
          (all (list-bindings)))
      (let ((needle (if (null? q) "" (as-str q))))
        (let ((fns
                (if (string-eq needle "")
                    all
                    (apropos needle))))
          (html/page
            (list
              (html/h1 "Function Reference")
              (html/p*
                (html/attr "class" "muted")
                (string-append
                  (as-str (count fns))
                  " of "
                  (as-str (count all))
                  " functions. Search with ?q= or (apropos \"term\"). "
                  "Docstrings via (function-doc name)."))
              (html/form "GET" "/ref" ""
                (html/join
                  (list
                    (html/text-input "q"
                      (html/attrs
                        (list
                          (html/attr "value" needle)
                          (html/attr "placeholder" "apropos…"))))
                    (html/button (html/attr "type" "submit") "Search"))))
              (html/ul ""
                (html/join (list/map ref/fn-li fns))))))))))

(route "/ref" "ref/page")
