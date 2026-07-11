;; Editable UI shell — package ui
;; Nav: menu/link + menu/group tree in ui/menu

(define ui/menu
  (list
    (menu/link "home" "/")
    (menu/group "browse"
      (list
        (menu/link "functions" "/bindings")
        (menu/link "pages" "/pages")
        (menu/link "data" "/types")
        (menu/link "branches" "/branches")
        (menu/link "about" "/about")))
    (menu/group "apps"
      (list
        (menu/link "notes" "/notes")
        (menu/link "dabar" "/dabar")
        (menu/link "todos" "/todos")
        (menu/link "agent" "/agent")
        (menu/link "repl" "/repl")))))

(define ui/nav
  (lambda ()
    (render-html (menu/bar ui/menu))))

(define ui/home
  (lambda ()
    (html/page
      (list
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
