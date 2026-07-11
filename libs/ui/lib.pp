;; package ui -- exported from live branch system/main
;; Sync: scripts/pp_sync.py pull|push

(define ui/home
  (lambda
    ()
    (html/page
      (list
        (html/div
          (html/attr "class" "card")
          (html/join
            (list
              (html/h2 "Workspace")
              (html/p*
                ""
                (html/join
                  (list
                    "Homepage is "
                    (html/code "ui/home")
                    "; navbar is "
                    (html/code "ui/menu")
                    " rendered by "
                    (html/code "ui/nav")
                    ".")))
              (html/ul
                ""
                (html/join
                  (list
                    (html/li "" (html/a "/bible" "Bible"))
                    (html/li "" (html/a "/todos" "Todos"))
                    (html/li
                      ""
                      (html/a "/bindings?pkg=ui" "Edit ui package"))
                    (html/li "" (html/a "/agent" "Agent"))))))))
        (html/div
          (html/attr "class" "card")
          (html/join
            (list
              (html/h2 "Customize nav")
              (html/p*
                ""
                (html/join
                  (list
                    "Edit "
                    (html/code "ui/menu")
                    " with "
                    (html/code "menu/link")
                    " and "
                    (html/code "menu/group")
                    ".")))
              (html/p*
                (html/attr "class" "muted")
                "Example: menu/group apps containing menu/link items"))))))))

(define ui/menu
  (list
    (menu/link "home" "/")
    (menu/group
      "browse"
      (list
        (menu/link "functions" "/bindings")
        (menu/link "pages" "/pages")
        (menu/link "data" "/types")
        (menu/link "branches" "/branches")
        (menu/link "about" "/about")))
    (menu/group
      "apps"
      (list
        (menu/link "bible" "/bible")
        (menu/link "todos" "/todos")
        (menu/link "agent" "/agent")
        (menu/link "repl" "/repl")))))

(define ui/nav
  (lambda () (render-html (menu/bar ui/menu))))

