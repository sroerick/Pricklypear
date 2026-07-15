;; Editable UI shell — package ui
;; Product chrome is Lisp (image.system-browser). Prefer thunks so
;; personal pins cannot fail define-time load order.

;; Zero-arg lambda: menu tree built when called, after menu/* are bound.
;; capture is a nav dropdown (notes/capture/nav) — not a home card.
(define ui/menu
  (lambda ()
    (list
      (menu/link "home" "/")
      (menu/raw (notes/capture/nav))
      (menu/group "apps"
        (list
          (menu/link "notes" "/notes")
          (menu/link "inbox" "/notes?slug=inbox")
          (menu/link "contacts" "/contacts")
          (menu/link "plan" "/plan")
          (menu/link "plans" "/plans")
          (menu/link "chat" "/chat")
          (menu/link "bookmarks" "/bookmarks")
          (menu/link "dabar" "/dabar")
          (menu/link "todos" "/todos")
          (menu/link "calendar" "/calendar")
          (menu/link "flashcards" "/flashcards")
          (menu/link "weather" "/weather")
          (menu/link "strongs" "/strongs")
          (menu/link "agent" "/agent")))
      (menu/group "system"
        (list
          (menu/link "functions" "/bindings")
          (menu/link "pages" "/pages")
          (menu/link "data" "/types")
          (menu/link "branches" "/branches")
          (menu/link "analytics" "/analytics")
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


;; Product theme — Path B (image.product-theme / BORGE-1783837363).
;; Styles are Lisp data (css/sheet); (ui/css) renders to CSS text.
;; Tokens are branch-visible knobs; personal gardens may redefine them.

(define theme/bg "#d4d0c8")
(define theme/ink "#000")
(define theme/panel "#fff")
(define theme/chrome "#c0c0c0")
(define theme/link "#000080")
(define theme/visited "#551a8b")
(define theme/err "#800000")
(define theme/ok "#006600")
(define theme/border "#000")
(define theme/muted "#333")
(define theme/row-alt "#ece9e0")
(define theme/shadow "#808080")

(define ui/theme
  (lambda ()
    (css/sheet
      (list
      (css/rule "*"
        (list "box-sizing" "border-box"))
      (css/rule "html"
        (list "font-size" "16px" "-webkit-text-size-adjust" "100%"))
      (css/rule "body"
        (list
          "margin" "0" "background" theme/bg "color" theme/ink
          "font-family" "Geneva,\"Lucida Grande\",\"DejaVu Sans\",Verdana,sans-serif" "line-height" "1.4"))
      (css/rule "a"
        (list "color" theme/link "text-decoration" "underline"))
      (css/rule "a:visited"
        (list "color" theme/visited))
      (css/rule "a:hover"
        (list "color" theme/ink))
      (css/rule "header"
        (list "background" theme/panel "border-bottom" "2px solid #000" "padding" ".5rem .75rem"))
      (css/rule "header .brand"
        (list
          "font-weight" "700" "font-size" "1.1rem" "letter-spacing" ".02em"
          "color" theme/ink "margin-right" ".75rem"))
      (css/rule "header .meta"
        (list
          "display" "block" "font-family" "\"Courier New\",Courier,monospace" "font-size" ".8rem"
          "color" theme/muted "margin" ".25rem 0 .5rem"))
      (css/rule "nav"
        (list
          "display" "flex" "flex-wrap" "wrap" "gap" ".35rem .5rem"
          "align-items" "center"))
      (css/rule "nav > a,.menu-drop"
        (list "flex" "0 0 auto"))
      (css/rule "nav a,.menu-drop-btn"
        (list
          "display" "inline-block" "padding" ".45rem .55rem" "min-height" "2.5rem"
          "line-height" "1.5" "border" "1px solid #000" "background" theme/chrome
          "color" theme/ink "text-decoration" "none" "font-size" ".9rem"
          "cursor" "pointer" "list-style" "none" "box-sizing" "border-box"))
      (css/rule "nav a:visited"
        (list "color" theme/ink))
      (css/rule "nav a:hover,nav a:focus,.menu-drop-btn:hover"
        (list "background" theme/panel))
      (css/rule "nav.emergency-nav"
        (list "width" "100%" "margin-top" ".35rem"))
      (css/rule ".emergency-banner"
        (list
          "display" "block" "width" "100%" "padding" ".4rem .55rem"
          "border" "2px solid #8b0000" "background" "#fff3cd" "color" "#1a1a1a"
          "font-size" ".85rem" "font-family" "Courier,monospace"))
      (css/rule ".emergency-banner a"
        (list "color" theme/link))
      (css/rule ".emergency-home"
        (list "border" "2px solid #8b0000" "background" "#fff3cd"))
      (css/rule ".menu-drop"
        (list "position" "relative" "display" "inline-block" "margin" "0"))
      (css/rule ".menu-drop>summary"
        (list "list-style" "none"))
      (css/rule ".menu-drop>summary::-webkit-details-marker"
        (list "display" "none"))
      (css/rule ".menu-drop>summary::after"
        (list "content" "\" v\"" "font-size" ".7em"))
      (css/rule ".menu-drop-panel"
        (list
          "display" "none" "position" "absolute" "left" "0"
          "right" "auto" "top" "100%" "z-index" "50"
          "min-width" "10rem" "max-width" "min(16rem,calc(100vw - 1rem))" "margin-top" "2px"
          "padding" ".25rem" "box-sizing" "border-box" "background" theme/panel
          "border" "2px solid #000" "box-shadow" "2px 2px 0 #808080"))
      (css/rule ".menu-drop[open]>.menu-drop-panel"
        (list "display" "block"))
      (css/rule ".menu-drop-panel a"
        (list
          "display" "block" "width" "100%" "margin" "0 0 .2rem"
          "white-space" "nowrap" "box-sizing" "border-box"))
      (css/rule ".menu-drop-panel a:last-child"
        (list "margin-bottom" "0"))
      (css/media "max-width:40rem"
        (list
          (css/rule "header,nav"
            (list "overflow" "visible"))
          (css/rule ".menu-drop-panel"
            (list "left" "auto" "right" "0" "max-width" "calc(100vw - .75rem)"))
          ;; Capture is wider than Apps menus; right-align still overflows
          ;; left of the viewport on narrow screens. Pin to the viewport
          ;; edges under the nav instead of the trigger's right edge.
          (css/rule ".capture-drop"
            (list "position" "static"))
          (css/rule ".capture-drop > .menu-drop-panel.capture-panel"
            (list
              "left" ".5rem" "right" ".5rem" "width" "auto"
              "min-width" "0" "max-width" "none"
              "box-sizing" "border-box"))))
      (css/rule "main"
        (list "padding" ".75rem" "max-width" "52rem" "margin" "0 auto"))
      (css/rule "h1"
        (list
          "font-size" "1.25rem" "font-weight" "700" "margin" ".25rem 0 .75rem"
          "border-bottom" "1px solid #000" "padding-bottom" ".25rem"))
      (css/rule "h2"
        (list "font-size" "1.05rem" "font-weight" "700" "margin" "0 0 .5rem"))
      (css/rule "p"
        (list "margin" ".5rem 0"))
      (css/rule ".card"
        (list
          "background" theme/panel "border" "2px solid #000" "padding" ".75rem"
          "margin" "0 0 .75rem"))
      (css/rule "table"
        (list
          "border-collapse" "collapse" "width" "100%" "background" theme/panel
          "border" "1px solid #000" "font-size" ".9rem"))
      (css/rule "th,td"
        (list
          "border" "1px solid #808080" "padding" ".45rem .5rem" "text-align" "left"
          "vertical-align" "top" "word-break" "break-word"))
      (css/rule "th"
        (list "background" theme/chrome "font-weight" "700" "border-color" theme/ink))
      (css/rule "tr:nth-child(even) td"
        (list "background" theme/row-alt))
      (css/rule "pre,textarea,input[type=text],input[type=search],input:not([type])"
        (list
          "font-family" "\"Courier New\",Courier,monospace" "font-size" "1rem" "background" theme/panel
          "color" theme/ink "border" "1px solid #000" "border-radius" "0"
          "padding" ".5rem" "width" "100%" "max-width" "100%"))
      (css/rule "pre"
        (list "white-space" "pre-wrap" "overflow-x" "auto" "line-height" "1.35"))
      (css/rule "textarea"
        (list
          "min-height" "16rem" "line-height" "1.35" "white-space" "pre"
          "tab-size" "2"))
      (css/rule "button,.btn,input[type=submit]"
        (list
          "font" "inherit" "font-size" "1rem" "cursor" "pointer"
          "background" theme/chrome "color" theme/ink "border" "2px outset #fff"
          "border-radius" "0" "padding" ".5rem .85rem" "min-height" "2.75rem"))
      (css/rule "button:active,.btn:active"
        (list "border-style" "inset"))
      (css/rule "button.secondary,.btn.secondary"
        (list "background" theme/bg))
      (css/rule "a.btn"
        (list
          "display" "inline-block" "text-decoration" "none" "color" theme/ink
          "line-height" "1.2" "vertical-align" "middle"))
      (css/rule "a.btn:visited"
        (list "color" theme/ink))
      (css/rule "label"
        (list "font-weight" "700"))
      (css/rule ".meta,.muted"
        (list "color" theme/muted "font-size" ".9rem"))
      (css/rule ".err"
        (list "color" theme/err "font-weight" "700"))
      (css/rule ".ok"
        (list "color" theme/ok "font-weight" "700"))
      (css/rule "#out"
        (list
          "white-space" "pre-wrap" "min-height" "4rem" "background" theme/panel
          "border" "1px solid #000" "padding" ".5rem" "font-family" "\"Courier New\",Courier,monospace"))
      (css/rule "ul.actions"
        (list "list-style" "none" "padding" "0" "margin" ".5rem 0"))
      (css/rule "ul.actions li"
        (list "margin" ".4rem 0"))
      (css/rule ".ol-root"
        (list "margin" ".5rem 0"))
      (css/rule ".ol-node"
        (list
          "position" "relative" "margin" ".25rem 0 .25rem .35rem" "border-left" "2px solid #808080"
          "padding-left" ".35rem"))
      (css/rule ".ol-sum"
        (list
          "cursor" "pointer" "font-weight" "700" "display" "flex"
          "align-items" "center" "gap" ".35rem" "flex-wrap" "nowrap"
          "min-width" "0" "padding-right" "2.5rem"))
      (css/rule ".ol-node.is-selected > .ol-sum"
        (list "background" "#fff8dc" "outline" "1px solid #000"))
      (css/rule ".ol-fold"
        (list
          "font-size" ".75em" "flex" "0 0 auto" "display" "inline-flex"
          "align-items" "center" "justify-content" "center" "user-select" "none"
          "cursor" "pointer" "line-height" "1" "min-width" "1.6rem"
          "min-height" "1.6rem" "padding" ".2rem" "margin" "0 .05rem 0 -.1rem"
          "color" theme/muted))
      (css/rule ".ol-fold:hover"
        (list "background" theme/panel "border" "1px solid #808080" "padding" "calc(.2rem - 1px)"))
      (css/rule "details.ol-node > .ol-sum .ol-fold::before"
        (list "content" "\"v\""))
      (css/rule "details.ol-node:not([open]) > .ol-sum .ol-fold::before"
        (list "content" "\">\""))
      (css/rule ".ol-todo"
        (list
          "display" "inline-block" "flex" "0 0 auto" "width" "auto"
          "margin" "0 .15rem 0 0" "padding" ".05rem .3rem" "border" "1px solid #666"
          "background" "transparent" "font-size" ".7rem" "font-weight" "700"
          "line-height" "1.3" "letter-spacing" ".02em" "color" theme/muted
          "vertical-align" "middle"))
      (css/rule ".ol-todo.st-TODO"
        (list "border-color" "#886600" "color" "#664400" "background" "#fff8dc"))
      (css/rule ".ol-todo.st-DONE"
        (list "border-color" "#226622" "color" "#226622" "background" "#e8f5e8"))
      (css/rule ".ol-todo.st-ARCHIVED"
        (list "border-color" "#666" "color" "#555" "background" "#eee"))
      (css/rule ".ol-node.st-DONE > .ol-sum .ol-title"
        (list "text-decoration" "line-through" "color" "#555" "font-weight" "500"))
      (css/rule ".ol-node.st-ARCHIVED > .ol-sum .ol-title"
        (list "color" "#777" "font-style" "italic" "font-weight" "500"))
      (css/rule ".ol-node.st-ARCHIVED"
        (list "opacity" "0.72"))
      (css/rule ".ol-title"
        (list
          "outline" "none" "padding" ".15rem .25rem" "border-radius" "0"
          "flex" "1 1 auto" "min-width" "0" "word-break" "break-word"
          "white-space" "normal" "line-height" "1.35" "cursor" "text"
          "font-weight" "700"))
      (css/rule ".ol-title:focus"
        (list "background" theme/panel "border" "1px solid #000"))
      (css/rule ".ol-body"
        (list
          "margin" ".25rem 0 .35rem" "white-space" "pre-wrap" "font-size" ".95rem"
          "outline" "none" "padding" ".25rem" "min-width" "0"
          "word-break" "break-word"))
      (css/rule ".ol-body:focus"
        (list "background" theme/panel "border" "1px solid #000"))
      (css/rule ".ol-kids"
        (list "margin-left" ".15rem" "min-width" "0"))
      ;; Desktop: top selection toolbar. Mobile: per-node ··· menu.
      (css/rule ".ol-toolbar"
        (list
          "display" "flex" "flex-wrap" "wrap" "align-items" "center"
          "gap" ".35rem" "margin" "0 0 .65rem" "padding" ".4rem .5rem"
          "background" theme/panel "border" "2px solid #000"))
      (css/rule ".ol-toolbar-label"
        (list
          "font-size" ".8rem" "color" theme/muted "margin-right" ".25rem"
          "min-width" "4rem"))
      (css/rule ".ol-toolbar .ol-btn"
        (list "min-height" "1.9rem"))
      (css/rule ".ol-toolbar form.ol-refile"
        (list "margin" "0" "display" "inline-flex" "width" "auto"))
      (css/rule "details.ol-actions"
        (list
          "display" "none" "position" "absolute" "top" ".1rem" "right" "0"
          "z-index" "3" "margin" "0"))
      (css/rule "details.ol-actions > summary.ol-actions-sum"
        (list
          "display" "inline-block" "list-style" "none" "cursor" "pointer"
          "user-select" "none" "width" "auto" "min-width" "1.75rem"
          "min-height" "1.75rem" "padding" ".1rem .3rem" "border" "1px solid #666"
          "background" "transparent" "font-weight" "700" "color" theme/muted
          "text-align" "center" "line-height" "1.3"))
      (css/rule "details.ol-actions > summary.ol-actions-sum::-webkit-details-marker"
        (list "display" "none"))
      (css/rule "details.ol-actions > summary.ol-actions-sum:hover, details.ol-actions[open] > summary.ol-actions-sum"
        (list "background" theme/panel "border-color" theme/ink))
      (css/rule ".ol-actions-panel"
        (list
          "display" "none" "position" "absolute" "right" "0" "top" "100%"
          "margin-top" "2px" "flex-direction" "column" "flex-wrap" "nowrap"
          "align-items" "stretch" "min-width" "7rem" "padding" ".2rem"
          "background" theme/panel "border" "1px solid #000" "z-index" "20"))
      (css/rule "details.ol-actions[open] > .ol-actions-panel"
        (list "display" "flex"))
      (css/rule "details.ol-actions[open] > .ol-actions-panel .ol-btn"
        (list "width" "100%" "text-align" "left" "min-height" "2rem"))
      (css/rule ".ol-btn"
        (list
          "font-size" ".75rem" "padding" ".2rem .4rem" "min-height" "1.8rem"
          "width" "auto" "flex" "0 0 auto" "margin" "0"
          "border" "1px solid #000" "background" theme/bg "cursor" "pointer"
          "line-height" "1.2"))
      (css/rule ".ol-btn:hover"
        (list "background" theme/panel))
      (css/rule ".ol-del"
        (list "color" theme/err))
      (css/rule "form.ol-refile"
        (list
          "display" "flex" "flex-wrap" "wrap" "gap" ".25rem"
          "align-items" "center" "margin" ".25rem 0 0"
          "width" "auto" "max-width" "100%"))
      (css/rule "form.ol-refile .ol-refile-sel"
        (list
          "flex" "1 1 8rem" "min-width" "6rem" "max-width" "14rem"
          "font-size" ".75rem" "padding" ".2rem"))
      (css/rule "form.ol-refile button"
        (list "width" "auto" "min-width" "0" "font-size" ".75rem"
              "padding" ".2rem .4rem" "margin" "0"))
      (css/rule ".ol-debug"
        (list "margin" ".75rem 0" "opacity" ".6"))
      (css/rule ".ol-debug > summary"
        (list "cursor" "pointer" "font-size" ".85rem"))
      (css/rule ".br-table-wrap"
        (list "overflow-x" "auto"))
      (css/rule ".br-table"
        (list "width" "100%" "font-size" ".9rem"))
      (css/rule ".br-table th,.br-table td"
        (list "padding" ".35rem .4rem" "vertical-align" "top"))
      (css/rule "tr.br-current"
        (list "background" "#fff8dc"))
      (css/rule ".br-stats"
        (list
          "display" "flex" "flex-wrap" "wrap" "gap" ".5rem"
          "margin" ".5rem 0"))
      (css/rule ".br-stat"
        (list "padding" ".4rem .6rem" "margin" "0"))
      (css/rule ".br-diff"
        (list "display" "grid" "grid-template-columns" "1fr 1fr" "gap" ".5rem"))
      (css/media "max-width:700px"
        (list
          (css/rule ".br-diff"
            (list "grid-template-columns" "1fr"))))
      (css/rule "form.inline-form"
        (list "display" "inline"))
      (css/rule "form.inline-form button"
        (list "margin-left" ".2rem"))
      (css/rule ".todo-layout"
        (list "display" "grid" "grid-template-columns" "12rem 1fr" "gap" ".75rem"))
      (css/media "max-width:700px"
        (list
          (css/rule ".todo-layout"
            (list "grid-template-columns" "1fr"))))
      (css/rule ".todo-nav"
        (list "list-style" "none" "padding" "0" "margin" "0"))
      (css/rule ".todo-nav-item"
        (list "margin" ".25rem 0"))
      (css/rule ".todo-nav-item.is-active a"
        (list "font-weight" "700" "border-left" "3px solid #000" "padding-left" ".35rem"))
      (css/rule ".todo-new-list"
        (list
          "display" "flex" "flex-direction" "column" "gap" ".25rem"
          "margin-top" ".5rem"))
      (css/rule ".todo-new-list-input"
        (list "font-size" ".85rem" "min-height" "2rem" "padding" ".25rem"))
      (css/rule ".todo-box.st-TODO"
        (list "background" "#fff8dc"))
      (css/rule ".todo-box.st-DONE"
        (list "background" "#cfc" "text-decoration" "line-through"))
      (css/rule ".todo-box.st-WAITING"
        (list "background" "#e0e8ff"))
      (css/rule ".todo-box.st-CANCELLED"
        (list "background" "#ddd" "color" "#666"))
      (css/rule ".todo-box.st-ARCHIVED"
        (list "background" "#eee" "color" "#555" "border-style" "dashed"))
      (css/rule "li.todo-item.is-archived .todo-title"
        (list "color" "#888" "font-style" "italic"))
      (css/rule ".todo-from-note .todo-src"
        (list "font-size" ".8rem" "margin-left" ".35rem"))
      (css/rule ".ol-todo-btn"
        (list "cursor" "pointer"))
      (css/rule ".ol-todo-btn.st-TODO"
        (list "background" "#fff8dc"))
      (css/rule ".ol-todo-btn.st-DONE"
        (list "background" "#cfc"))
      (css/rule ".ol-todo-btn.st-WAITING"
        (list "background" "#e0e8ff"))
      (css/rule ".cal-nav"
        (list
          "display" "flex" "align-items" "center" "gap" ".75rem"
          "margin" ".5rem 0" "flex-wrap" "wrap"))
      (css/rule ".cal-nav a"
        (list "text-decoration" "none" "font-weight" "700"))
      (css/rule ".cal-grid"
        (list
          "display" "grid" "grid-template-columns" "repeat(7,1fr)" "gap" "2px"
          "background" theme/shadow "border" "2px solid #000" "max-width" "42rem"))
      (css/rule ".cal-dow"
        (list
          "background" theme/chrome "text-align" "center" "font-weight" "700"
          "padding" ".25rem" "font-size" ".8rem"))
      (css/rule ".cal-cell"
        (list
          "background" theme/panel "min-height" "3.5rem" "padding" ".2rem"
          "display" "block" "color" theme/ink "text-decoration" "none"))
      (css/rule "a .cal-cell:hover"
        (list "background" "#fff8dc"))
      (css/rule ".cal-empty"
        (list "background" theme/bg "min-height" "3.5rem"))
      (css/rule ".cal-today"
        (list "outline" "2px solid #000" "background" "#ffffe0"))
      (css/rule ".cal-dom"
        (list "font-weight" "700" "font-size" ".85rem"))
      (css/rule ".cal-marks"
        (list
          "display" "flex" "flex-wrap" "wrap" "gap" "2px"
          "margin-top" ".15rem"))
      (css/rule ".cal-dot"
        (list
          "font-size" ".65rem" "padding" "0 .2rem" "border" "1px solid #000"
          "background" theme/chrome "line-height" "1.2"))
      (css/rule ".cal-dot-ev"
        (list "background" "#cfc"))
      (css/rule ".cal-dot-todo"
        (list "background" "#fff8dc"))
      (css/rule ".cal-dot-note"
        (list "background" "#e0e8ff"))
      (css/rule ".cal-list"
        (list "list-style" "none" "padding" ".25rem" "margin" "0"))
      (css/rule ".cal-event,.cal-todo"
        (list
          "display" "flex" "gap" ".5rem" "align-items" "center"
          "padding" ".35rem 0" "border-bottom" "1px solid #ccc"))
      (css/rule ".cal-time"
        (list "font-size" ".8rem" "min-width" "3.5rem" "color" theme/muted))
      (css/rule ".cal-day-panel label"
        (list "display" "block" "margin-top" ".4rem"))
      (css/rule ".weather-card .wx-row"
        (list
          "display" "flex" "align-items" "baseline" "gap" ".75rem"
          "flex-wrap" "wrap"))
      (css/rule ".weather-card .wx-temp"
        (list "font-size" "2rem" "font-weight" "700" "line-height" "1"))
      (css/rule ".weather-card .wx-meta"
        (list "font-size" ".9rem" "color" theme/muted))
      (css/rule ".weather-card .wx-loc"
        (list "font-weight" "700" "margin" ".25rem 0"))
      (css/rule ".weather-card .wx-form"
        (list
          "display" "flex" "gap" ".35rem" "flex-wrap" "wrap"
          "margin-top" ".5rem"))
      (css/rule ".weather-card .wx-form input[type=text]"
        (list "flex" "1 1 8rem" "min-width" "0"))
      (css/rule ".weather-card .wx-form button"
        (list "width" "auto" "min-width" "4.5rem"))
      (css/rule ".weather-card .wx-err"
        (list "color" theme/err "font-weight" "700"))
      (css/rule ".capture-panel"
        (list
          "min-width" "min(16rem, calc(100vw - 1rem))"
          "max-width" "min(22rem, calc(100vw - 1rem))"
          "padding" ".4rem"
          "box-sizing" "border-box"))
      (css/rule ".capture-nav-form"
        (list
          "display" "flex" "flex-direction" "column" "gap" ".35rem"
          "margin" "0"))
      (css/rule ".capture-nav-form input[type=text],.capture-nav-form textarea"
        (list "width" "100%" "box-sizing" "border-box" "font-size" ".9rem"))
      (css/rule ".capture-nav-form .capture-body"
        (list "min-height" "4rem"))
      (css/rule ".capture-nav-form .capture-row"
        (list
          "display" "flex" "flex-wrap" "wrap" "gap" ".35rem"
          "align-items" "center"))
      (css/rule ".capture-nav-form .capture-row button"
        (list "width" "auto" "min-width" "4.5rem" "margin" "0"))
      (css/rule ".capture-nav-form #capture-status"
        (list "font-size" ".85rem" "min-height" "1.1em"))
      (css/rule ".capture-nav-form .ok"
        (list "color" "#226622" "font-weight" "700"))
      (css/rule ".capture-nav-form .err"
        (list "color" theme/err "font-weight" "700"))
      (css/rule ".capture-card label"
        (list "display" "block" "margin" ".35rem 0 .15rem" "font-weight" "700"))
      (css/rule ".capture-card .capture-body"
        (list "width" "100%" "box-sizing" "border-box" "min-height" "4.5rem"))
      (css/rule ".capture-card .capture-row"
        (list
          "display" "flex" "flex-wrap" "wrap" "gap" ".5rem"
          "align-items" "center" "margin-top" ".5rem"))
      (css/rule ".capture-card .capture-row button"
        (list "width" "auto" "min-width" "6rem"))
      (css/rule ".todo-add-label"
        (list "display" "block" "font-weight" "bold" "margin" "0 0 .35rem"))
      (css/rule ".todo-add-row"
        (list
          "display" "flex" "gap" ".5rem" "align-items" "stretch"
          "flex-wrap" "wrap"))
      (css/rule ".todo-add-row input[type=text]"
        (list "flex" "1 1 12rem" "min-width" "0"))
      (css/rule ".todo-add-row button"
        (list "flex" "0 0 auto" "width" "auto" "min-width" "5rem"))
      (css/rule "ul.todo-list"
        (list "list-style" "none" "padding" ".25rem .5rem" "margin" "0 0 .75rem"))
      (css/rule "li.todo-item"
        (list
          "display" "flex" "align-items" "center" "gap" ".5rem"
          "padding" ".45rem 0" "border-bottom" "1px solid #c0c0c0"))
      (css/rule "li.todo-item:last-child"
        (list "border-bottom" "none"))
      (css/rule "li.todo-item form"
        (list "margin" "0" "display" "inline"))
      (css/rule "li.todo-item .todo-box"
        (list
          "font-family" "ui-monospace,Menlo,monospace" "min-width" "2.6rem" "padding" ".25rem .35rem"
          "width" "auto"))
      (css/rule "li.todo-item .todo-title"
        (list "flex" "1 1 auto" "min-width" "0" "word-break" "break-word"))
      (css/rule "li.todo-item.is-done .todo-title"
        (list "text-decoration" "line-through" "color" "#666"))
      (css/rule "li.todo-item .todo-del button, li.todo-item .todo-archive button"
        (list "width" "auto" "padding" ".25rem .45rem" "font-size" ".85rem"))
      (css/rule "li.todo-nav-list"
        (list
          "display" "flex" "align-items" "center" "gap" ".25rem"
          "flex-wrap" "wrap"))
      (css/rule "li.todo-nav-list > a"
        (list "flex" "1 1 auto" "min-width" "0"))
      (css/rule "li.todo-nav-list form.todo-list-del"
        (list "margin" "0" "display" "inline"))
      (css/rule "li.todo-nav-list .todo-list-del button"
        (list
          "width" "auto" "min-width" "1.6rem" "padding" ".1rem .3rem"
          "font-size" ".8rem" "line-height" "1.2"))
      (css/rule "li.note-item"
        (list
          "display" "flex" "align-items" "center" "gap" ".5rem"
          "padding" ".35rem 0" "border-bottom" "1px solid #c0c0c0"))
      (css/rule "li.note-item > a"
        (list "flex" "1 1 auto"))
      (css/rule "li.note-item form.note-del"
        (list "margin" "0"))
      (css/rule "li.note-item .note-del button"
        (list "width" "auto" "padding" ".25rem .45rem" "font-size" ".85rem"))
      (css/rule ".note-title-row"
        (list
          "display" "flex" "flex-wrap" "wrap" "align-items" "center"
          "gap" ".75rem"))
      (css/rule ".note-title-row h1"
        (list
          "flex" "1 1 auto" "margin" "0" "border-bottom" "none"
          "padding" "0"))
      (css/rule ".note-title-row .note-del-page button"
        (list "width" "auto"))
      (css/rule "ol.bible-chapter"
        (list "list-style" "none" "padding-left" "0" "margin" ".25rem 0 1rem"))
      (css/rule "ol.bible-chapter>li"
        (list "margin" ".35rem 0" "line-height" "1.45" "padding-left" "0"))
      (css/rule "ol.bible-chapter>li>sup"
        (list
          "font-size" ".75em" "font-weight" "700" "margin-right" ".2em"
          "color" theme/muted "vertical-align" "super"))
      (css/rule ".bible-go label,.bible-search label"
        (list "display" "inline-block" "margin" ".2rem .45rem .2rem 0"))
      (css/rule ".bible-go select,.bible-search input[type=text]"
        (list
          "width" "auto" "min-width" "6rem" "max-width" "100%"
          "margin-right" ".35rem"))
      (css/media "max-width:40rem"
        (list
          (css/rule "main"
            (list "padding" ".5rem"))
          (css/rule "header"
            (list "padding" ".5rem"))
          (css/rule "nav"
            (list "gap" ".3rem" "width" "100%"))
          (css/rule "nav > a,.menu-drop"
            (list "flex" "1 1 0" "min-width" "0" "text-align" "center"))
          (css/rule "nav > a"
            (list "display" "block"))
          (css/rule ".menu-drop-btn"
            (list "display" "block" "width" "100%" "text-align" "center"))
          (css/rule "table"
            (list "display" "block" "overflow-x" "auto" "-webkit-overflow-scrolling" "touch"))
          (css/rule "h1"
            (list "font-size" "1.15rem"))
          (css/rule "button,.btn"
            (list "width" "100%" "margin" ".2rem 0"))
          (css/rule ".card form button,.card form .btn"
            (list "width" "auto" "min-width" "44%"))
          (css/rule "button.ol-btn,button.ol-todo-btn, .ol-btn,.ol-todo,.ol-todo-btn"
            (list "width" "auto" "margin" "0"))
          (css/rule ".ol-node"
            (list "margin-left" ".1rem" "padding-left" ".3rem"))
          (css/rule ".ol-sum"
            (list "padding-right" "2.5rem" "align-items" "center"))
          (css/rule ".ol-toolbar"
            (list "display" "none"))
          (css/rule "details.ol-actions"
            (list "display" "block"))
          (css/rule "details.ol-actions[open] > .ol-actions-panel .ol-btn"
            (list "min-height" "2.1rem" "background" theme/panel))))))))


;; Nav layout + capture dropdown (formerly ui/nav-style inline <style>)
(define ui/theme-nav
  (lambda ()
    (css/sheet
      (list
        (css/rule "nav"
          (list "display" "flex" "flex-wrap" "wrap" "gap" ".35rem .5rem"
                "align-items" "center" "width" "100%"))
        (css/rule "nav>a, .menu-drop"
          (list "flex" "0 0 auto"))
        (css/media "max-width:40rem"
          (list
            (css/rule "nav>a, .menu-drop"
              (list "flex" "1 1 0" "min-width" "0" "text-align" "center"))
            (css/rule "nav>a"
              (list "display" "block" "text-align" "center"))
            (css/rule ".menu-drop-btn"
              (list "display" "block" "width" "100%" "text-align" "center"
                    "box-sizing" "border-box"))))
        (css/rule ".capture-panel"
          (list
            "min-width" "min(16rem, calc(100vw - 1rem))"
            "max-width" "min(22rem, calc(100vw - 1rem))"
            "padding" ".4rem"
            "box-sizing" "border-box"))
        (css/media "max-width:40rem"
          (list
            (css/rule ".capture-drop"
              (list "position" "static"))
            (css/rule ".capture-drop > .menu-drop-panel.capture-panel"
              (list
                "left" ".5rem" "right" ".5rem" "width" "auto"
                "min-width" "0" "max-width" "none"
                "box-sizing" "border-box"))))
        (css/rule ".capture-nav-form"
          (list "display" "flex" "flex-direction" "column" "gap" ".35rem" "margin" "0"))
        (css/rule ".capture-nav-form input[type=text], .capture-nav-form textarea"
          (list "width" "100%" "box-sizing" "border-box" "font-size" ".9rem"))
        (css/rule ".capture-nav-form .capture-body"
          (list "min-height" "4rem"))
        (css/rule ".capture-nav-form .capture-row"
          (list "display" "flex" "flex-wrap" "wrap" "gap" ".35rem" "align-items" "center"))
        (css/rule ".capture-nav-form .capture-row button"
          (list "width" "auto" "min-width" "4.5rem" "margin" "0"))
        (css/rule ".capture-nav-form .capture-row a"
          (list "display" "inline-block" "padding" ".35rem .45rem"))
        (css/rule ".capture-nav-form #capture-status"
          (list "font-size" ".85rem" "min-height" "1.1em"))
        (css/rule ".capture-nav-form .ok"
          (list "color" theme/ok "font-weight" "700"))
        (css/rule ".capture-nav-form .err"
          (list "color" theme/err "font-weight" "700"))))))

(define ui/css
  (lambda ()
    (string-append
      "/* ui/css */\n"
      (css/render
        (css/sheet
          (list
            (css/use (ui/theme))
            (css/use (ui/theme-nav))))))))



(define ui/home
  (lambda ()
    (html/page
      (list
        (weather/widget)))))
