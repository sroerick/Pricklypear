;; Editable UI shell — package ui
;; Product chrome is Lisp (image.system-browser). Prefer thunks so
;; personal pins cannot fail define-time load order.

;; Zero-arg lambda: menu tree built when called, after menu/* are bound.
(define ui/menu
  (lambda ()
    (list
      (menu/link "home" "/")
      (menu/group "apps"
        (list
          (menu/link "notes" "/notes")
          (menu/link "dabar" "/dabar")
          (menu/link "todos" "/todos")
          (menu/link "calendar" "/calendar")
          (menu/link "weather" "/weather")
          (menu/link "strongs" "/strongs")
          (menu/link "agent" "/agent")))
      (menu/group "system"
        (list
          (menu/link "functions" "/bindings")
          (menu/link "pages" "/pages")
          (menu/link "data" "/types")
          (menu/link "branches" "/branches")
          (menu/link "about" "/about")
          (menu/link "repl" "/repl"))))))

;; ui/menu may be a list OR a zero-arg lambda (seed uses lambda).
(define ui/nav
  (lambda ()
    (render-html
      (menu/bar
        (if (string-eq (type-of ui/menu) "lambda")
            (ui/menu)
            ui/menu)))))

(define ui/home
  (lambda ()
    (html/page
      (list
        (weather/widget)
        (html/div (html/attr "class" "card")
          (html/join
            (list
              (html/h2 "Workspace")
              (html/p* ""
                (html/join
                  (list
                    "Live image — edit "
                    (html/code "ui/menu")
                    " for the navbar.")))
              (html/ul ""
                (html/join
                  (list
                    (html/li "" (html/a "/notes" "Notes"))
                    (html/li "" (html/a "/dabar" "Bible"))
                    (html/li "" (html/a "/todos" "Todos"))
                    (html/li "" (html/a "/weather" "Weather"))
                    (html/li "" (html/a "/bindings?pkg=ui" "Edit ui"))))))))
        (html/div (html/attr "class" "card")
          (html/join
            (list
              (html/h2 "Install libraries")
              (html/form "POST" "/api/recipe/dabar" (html/attr "class" "")
                (html/button (html/attr "type" "submit")
                  "Install dabar (Bible study)"))
              (html/form "POST" "/api/recipe/notes" (html/attr "class" "")
                (html/button (html/attr "type" "submit")
                  "Load outline + notes libraries")))))))))
