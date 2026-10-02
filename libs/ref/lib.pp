;; package ref -- exported from live branch system/main
;; Sync: scripts/pp_sync.py pull|push

(define ref/fn-li
  (lambda
    (fn)
    (let
      ((doc (function-doc (as-str fn))))
      (if
        (null? doc)
        (html/li "" (html/code (as-str fn)))
        (html/li
          ""
          (html/join
            (list
              (html/code (as-str fn))
              (html/span
                (html/attr "class" "muted")
                (string-append " &mdash; " (as-str doc))))))))))

(define ref/packages
  (lambda
    ()
    (list/foldl
      (lambda
        (acc name)
        (let
          ((p (prefix-of name)))
          (if
            (null?
              (list/filter (lambda (x) (string-eq (as-str x) p)) acc))
            (cons p acc)
            acc)))
      (list)
      (list-bindings))))

(define ref/page
  (lambda
    ()
    (let
      ((q (request-param "q")) (all (list-bindings)))
      (let
        ((needle (if (null? q) "" (as-str q))))
        (let
          ((fns (if (string-eq needle "") all (apropos needle))))
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
              (html/form
                "GET"
                "/ref"
                ""
                (html/join
                  (list
                    (html/text-input
                      "q"
                      (html/attrs
                        (list
                          (html/attr "value" needle)
                          (html/attr "placeholder" "apropos&hellip;"))))
                    (html/button (html/attr "type" "submit") "Search"))))
              (html/ul "" (html/join (list/map ref/fn-li fns))))))))))

(define ref/pkg-source
  (lambda
    ()
    (let
      ((name (as-str (request-param "name"))))
      (cond
        ((string-eq name "")
          (render-html
            "Usage: GET /pkg/source?name=<package>. One call returns the full lib.pp text of a package on this branch. Enumerate: /ref or (list-packages)."))
        ((member name (list-packages))
          (render-html (export-package name)))
        (else
          (render-html
            (string-append
              "no package named "
              name
              " . Try (list-packages) or /ref.")))))))

(define ref/render-pkg
  (lambda
    (pkg)
    (let
      ((fns
        (list/filter
          (lambda (n) (string-eq (prefix-of n) pkg))
          (list-bindings))))
      (html/div
        (html/attr "class" "card")
        (html/join
          (list
            (html/h2 (html/text pkg))
            (html/p*
              (html/attr "class" "muted")
              (string-append (as-str (count fns)) " functions"))
            (html/ul "" (html/join (list/map ref/fn-li fns)))))))))
