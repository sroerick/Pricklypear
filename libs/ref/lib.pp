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
    "Top-level package prefixes (kernel list-packages; budget-safe)."
    (list-packages))
  "Top-level package prefixes (kernel list-packages; budget-safe).")

(define ref/render-pkg
  (lambda (pkg)
    "Card for one package: count + shallow exports + search link (no full dump)."
    (let ((all (package-bindings pkg))
          (ex (package-exports pkg)))
      (html/div
        (html/attr "class" "card")
        (html/join
          (list
            (html/h2
              (html/a
                (string-append "/ref?q=" pkg "/")
                (html/text pkg)))
            (html/p*
              (html/attr "class" "muted")
              (string-append
                (as-str (count all))
                " bindings · "
                (as-str (count ex))
                " shallow exports · "
                (html/a (string-append "/ref?q=" pkg "/") "search")))
            (html/ul ""
              (html/join (list/map ref/fn-li ex)))))))))

(define ref/page
  (lambda ()
    "GET /ref — package index by default; ?q= runs apropos (budget-safe)."
    (let ((q (request-param "q"))
          (all-n (count (list-bindings))))
      (let ((needle (if (null? q) "" (as-str q))))
        (if (string-eq needle "")
            ;; Default: package cards only — full list-bindings map blows the budget.
            (html/page
              (list
                (html/h1 "Function Reference")
                (html/p*
                  (html/attr "class" "muted")
                  (string-append
                    (as-str all-n)
                    " bindings. Browse packages below, or search with ?q= / (apropos \"term\"). "
                    "Lisp: (list-packages) (describe \"todo\") (apropos-docs \"todo/\")."))
                (html/form "GET" "/ref" ""
                  (html/join
                    (list
                      (html/text-input "q"
                        (html/attrs
                          (list
                            (html/attr "value" "")
                            (html/attr "placeholder" "apropos…"))))
                      (html/button (html/attr "type" "submit") "Search"))))
                (html/join (list/map ref/render-pkg (ref/packages)))))
            (let ((fns (apropos needle)))
              (html/page
                (list
                  (html/h1 "Function Reference")
                  (html/p*
                    (html/attr "class" "muted")
                    (string-append
                      (as-str (count fns))
                      " matches for "
                      needle
                      " (of "
                      (as-str all-n)
                      "). Docstrings via (function-doc name)."))
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
                    (html/join (list/map ref/fn-li fns)))))))))))

(route "/ref" "ref/page")
