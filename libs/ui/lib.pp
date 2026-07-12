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

;; Home-only CSS so nav equal-width + capture layout work before image rebuild.
(define ui/home-style
  (lambda ()
    (html/tag "style" ""
      (html/join
        (list
          "nav{display:flex;flex-wrap:wrap;gap:.35rem .5rem;align-items:center;width:100%}"
          "nav>a,.menu-drop{flex:0 0 auto}"
          "@media(max-width:40rem){"
          "nav>a,.menu-drop{flex:1 1 0;min-width:0;text-align:center}"
          "nav>a{display:block;text-align:center}"
          ".menu-drop-btn{display:block;width:100%;text-align:center;box-sizing:border-box}"
          "}"
          ".capture-card label{display:block;margin:.35rem 0 .15rem;font-weight:700}"
          ".capture-card .capture-body{width:100%;box-sizing:border-box;min-height:4.5rem}"
          ".capture-card .capture-row{display:flex;flex-wrap:wrap;gap:.5rem;align-items:center;margin-top:.5rem}"
          ".capture-card .capture-row button{width:auto;min-width:6rem}"
          ".capture-card #capture-status{margin-top:.35rem;font-size:.9rem}"
          ".capture-card #capture-status.ok,.capture-card .ok{color:#226622;font-weight:700}"
          ".capture-card #capture-status.err,.capture-card .err{color:#800000;font-weight:700}")))))

(define ui/home
  (lambda ()
    (html/page
      (list
        (ui/home-style)
        (notes/capture-form)
        (weather/widget)))))
