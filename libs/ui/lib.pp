;; Editable UI shell — Functions package "ui"
;; (load-library "ui") or edit ui/nav and ui/home in the browser.
;; Uses std html/* helpers. Chrome re-runs (ui/nav) on every request.

(define ui/nav
  (lambda ()
    (html/page
      (list
        (html/a "/" "home")
        (html/a "/about" "about")
        (html/a "/branches" "branches")
        (html/a "/bindings" "functions")
        (html/a "/types" "data")
        (html/a "/pages" "pages")
        (html/a "/bible" "bible")
        (html/a "/todos" "todos")
        (html/a "/repl" "repl")
        (html/a "/agent" "agent")))))

(define ui/home
  (lambda ()
    (html/page
      (list
        (html/div (html/attr "class" "card")
          (html/join
            (list
              (html/h2 "Workspace")
              (html/p*
                ""
                (html/join
                  (list
                    "Homepage is Lisp function "
                    (html/code "ui/home")
                    ". Edit under Functions / package "
                    (html/strong "ui")
                    ".")))
              (html/ul ""
                (html/join
                  (list
                    (html/li "" (html/a "/about" "About"))
                    (html/li "" (html/a "/bindings" "Functions"))
                    (html/li "" (html/a "/pages" "Pages"))
                    (html/li "" (html/a "/bible" "Bible"))
                    (html/li "" (html/a "/todos" "Todos"))
                    (html/li "" (html/a "/repl" "REPL"))))))))
        (html/div (html/attr "class" "card")
          (html/join
            (list
              (html/h2 "Customize")
              (html/p*
                ""
                (html/join
                  (list
                    (html/a "/bindings?pkg=ui" "ui package")
                    " · "
                    (html/a "/edit/ui%2Fhome" "edit homepage")
                    " · "
                    (html/a "/edit/ui%2Fnav" "edit navbar"))))
              (html/p*
                (html/attr "class" "muted")
                "Brand/user/branch chrome stays system; nav and home body are Lisp."))))))))
