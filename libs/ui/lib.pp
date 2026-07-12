;; Editable UI shell — package ui
;; Product chrome is Lisp (image.system-browser). Prefer thunks so
;; personal pins cannot fail define-time load order.

;; Zero-arg lambda: menu tree built when called, after menu/* are bound.
;; capture is a nav dropdown (notes/capture-nav) — not a home card.
(define ui/menu
  (lambda ()
    (list
      (menu/link "home" "/")
      (menu/raw (notes/capture-nav))
      (menu/group "apps"
        (list
          (menu/link "notes" "/notes")
          (menu/link "inbox" "/notes?slug=inbox")
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
      (html/join
        (list
          (ui/nav-style)
          (menu/bar
            (if (string-eq (type-of ui/menu) "lambda")
                (ui/menu)
                ui/menu)))))))

;; Chrome CSS for equal-width nav + capture dropdown (works before image rebuild).
(define ui/nav-style
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
          ".capture-panel{min-width:16rem;max-width:min(22rem,calc(100vw - 1rem));padding:.4rem}"
          ".capture-nav-form{display:flex;flex-direction:column;gap:.35rem;margin:0}"
          ".capture-nav-form input[type=text],.capture-nav-form textarea{"
          "width:100%;box-sizing:border-box;font-size:.9rem}"
          ".capture-nav-form .capture-body{min-height:4rem}"
          ".capture-nav-form .capture-row{display:flex;flex-wrap:wrap;gap:.35rem;align-items:center}"
          ".capture-nav-form .capture-row button{width:auto;min-width:4.5rem;margin:0}"
          ".capture-nav-form .capture-row a{display:inline-block;padding:.35rem .45rem}"
          ".capture-nav-form #capture-status{font-size:.85rem;min-height:1.1em}"
          ".capture-nav-form .ok{color:#226622;font-weight:700}"
          ".capture-nav-form .err{color:#800000;font-weight:700}")))))

(define ui/home
  (lambda ()
    (html/page
      (list
        (weather/widget)))))
