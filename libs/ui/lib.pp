;; package ui -- exported from live branch admin/personal
;; Sync: scripts/pp_sync.py pull|push

(package ui
  (export
    ui/css
    ui/home
    ui/home-style
    ui/menu
    ui/nav
    ui/nav-style
    ui/theme
    ui/theme-button
    ui/theme-form
    ui/theme-header
    ui/theme-mobile
    ui/theme-table
    ui/theme-nav
))

(define ui/css
  (lambda
    ()
    "Render the combined UI stylesheet."
    (string-append
      "/* ui/css */\n"
      (css/render
        (css/sheet
          (list
            (css/use (theme/vars))
            (css/use (ui/theme))
            (css/use (ui/theme-header))
            (css/use (ui/theme-table))
            (css/use (ui/theme-form))
            (css/use (ui/theme-button))
            (css/use (outline/css))
            (css/use (notes/css))
            (css/use (todo/css))
            (css/use (calendar/css))
            (css/use (weather/css))
            (css/use (layout/card-css))
            (css/use (ui/theme-mobile))
            (css/use (ui/theme-nav))
            (css/use notes/tree-css)
            (css/use (theme/extra))))))))

(define ui/home
  (lambda
    ()
    "Render the home page with weather and astro widgets."
    (html/page (list (weather/widget) (astro/widget)))))

(define ui/home-style
  (lambda
    ()
    "Render the home page style block."
    (html/tag
      "style"
      ""
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
          ".capture-card #capture-status.ok,.capture-card .ok{color:var(--ok);font-weight:700}"
          ".capture-card #capture-status.err,.capture-card .err{color:var(--err);font-weight:700}")))))

(define ui/menu
  (lambda
    ()
    "Build the site navigation menu structure."
    (list
      (menu/link "home" "/")
      (menu/raw (notes/capture/nav))
      (menu/group
        "apps"
        (list
          (menu/group
            "writing"
            (list
              (menu/link "notes" "/notes")
              (menu/link "inbox" "/notes?slug=inbox")
              (menu/link "plans" "/plans")
              (menu/link "bookmarks" "/bookmarks")))
          (menu/group
            "study"
            (list
              (menu/link "dabar" "/dabar")
              (menu/link "strongs" "/strongs")
              (menu/link "flashcards" "/flashcards")))
          (menu/group
            "tasks"
            (list
              (menu/link "todos" "/todos")
              (menu/link "calendar" "/calendar")))
          (menu/group
            "connect"
            (list
              (menu/link "contacts" "/contacts")
              (menu/link "chat" "/chat")))
          (menu/group
            "tools"
            (list
              (menu/link "weather" "/weather")
              (menu/link "agent" "/agent")
              (menu/link "calories" "/cal")))))
      (menu/group
        "system"
        (list
          (menu/link "functions" "/bindings")
          (menu/link "ref" "/ref")
          (menu/link "pages" "/pages")
          (menu/link "data" "/types")
          (menu/link "branches" "/branches")
          (menu/link "analytics" "/analytics")
          (menu/link "llm keys" "/settings/keys")
          (menu/link "ssh keys" "/settings/ssh")
          (menu/link "themes" "/settings/theme")
          (menu/link "about" "/about")
          (menu/link "repl" "/repl"))))))

(define ui/nav
  (lambda
    ()
    "Render the site navigation bar."
    (render-html
      (menu/bar
        (if
          (string-eq (type-of ui/menu) "lambda")
          (ui/menu)
          ui/menu)))))

(define ui/nav-style
  (lambda
    ()
    "Render the site navigation bar style block."
    (html/tag
      "style"
      ""
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
          ".capture-nav-form .ok{color:var(--ok);font-weight:700}"
          ".capture-nav-form .err{color:var(--err);font-weight:700}")))))

(define ui/theme
  (lambda
    ()
    "Render the base theme stylesheet rules."
    (css/sheet
      (list
        (css/rule "*" (list "box-sizing" "border-box"))
        (css/rule
          "html"
          (list
            "font-size"
            "16px"
            "-webkit-text-size-adjust"
            "100%"))
        (css/rule
          "body"
          (list
            "margin"
            "0"
            "background"
            "var(--bg)"
            "color"
            "var(--ink)"
            "font-family"
            "var(--font-sans)"
            "line-height"
            "1.4"))
        (css/rule
          "a"
          (list
            "color"
            "var(--link)"
            "text-decoration"
            "underline"))
        (css/rule "a:visited" (list "color" "var(--visited)"))
        (css/rule "a:hover" (list "color" "var(--ink)"))
        (css/rule
          "nav"
          (list
            "display"
            "flex"
            "flex-wrap"
            "wrap"
            "gap"
            ".35rem .5rem"
            "align-items"
            "center"))
        (css/rule "nav > a,.menu-drop" (list "flex" "0 0 auto"))
        (css/rule
          "nav a,.menu-drop-btn"
          (list
            "display"
            "inline-block"
            "padding"
            ".45rem .55rem"
            "min-height"
            "2.5rem"
            "line-height"
            "1.5"
            "border"
            "1px solid #000"
            "background"
            "var(--chrome)"
            "color"
            "var(--ink)"
            "text-decoration"
            "none"
            "font-size"
            ".9rem"
            "cursor"
            "pointer"
            "list-style"
            "none"
            "box-sizing"
            "border-box"))
        (css/rule "nav a:visited" (list "color" "var(--ink)"))
        (css/rule
          "nav a:hover,nav a:focus,.menu-drop-btn:hover"
          (list "background" "var(--panel)"))
        (css/rule
          "nav.emergency-nav"
          (list "width" "100%" "margin-top" ".35rem"))
        (css/rule
          ".emergency-banner"
          (list
            "display"
            "block"
            "width"
            "100%"
            "padding"
            ".4rem .55rem"
            "border"
            "2px solid var(--banner-border)"
            "background"
            "var(--banner-bg)"
            "color"
            "var(--banner-ink)"
            "font-size"
            ".85rem"
            "font-family"
            "var(--font-mono)"))
        (css/rule
          ".emergency-banner a"
          (list "color" "var(--link)"))
        (css/rule
          ".emergency-home"
          (list
            "border"
            "2px solid var(--banner-border)"
            "background"
            "var(--banner-bg)"))
        (css/rule
          ".menu-drop"
          (list
            "position"
            "relative"
            "display"
            "inline-block"
            "margin"
            "0"))
        (css/rule
          ".menu-drop>summary"
          (list "list-style" "none"))
        (css/rule
          ".menu-drop>summary::-webkit-details-marker"
          (list "display" "none"))
        (css/rule
          ".menu-drop>summary::after"
          (list "content" "\" v\"" "font-size" ".7em"))
        (css/rule
          ".menu-drop[open]>summary"
          (list "position" "relative" "z-index" "81"))
        (css/rule
          ".menu-drop[open]>summary::before"
          (list
            "content"
            "\"\""
            "position"
            "fixed"
            "inset"
            "0"
            "z-index"
            "80"
            "cursor"
            "default"
            "background"
            "transparent"))
        (css/rule
          ".menu-drop-panel"
          (list
            "display"
            "none"
            "position"
            "absolute"
            "left"
            "0"
            "right"
            "auto"
            "top"
            "100%"
            "z-index"
            "82"
            "min-width"
            "10rem"
            "max-width"
            "min(16rem,calc(100vw - 1rem))"
            "width"
            "max-content"
            "margin-top"
            "2px"
            "padding"
            ".25rem"
            "box-sizing"
            "border-box"
            "background"
            "var(--panel)"
            "border"
            "2px solid #000"
            "box-shadow"
            "2px 2px 0 #808080"))
        (css/rule
          ".menu-drop[open]>.menu-drop-panel"
          (list "display" "block"))
        (css/rule
          ".menu-drop-panel a"
          (list
            "display"
            "block"
            "width"
            "100%"
            "margin"
            "0 0 .2rem"
            "white-space"
            "nowrap"
            "box-sizing"
            "border-box"))
        (css/rule
          ".menu-drop-panel a:last-child"
          (list "margin-bottom" "0"))
        (css/media
          "max-width:40rem"
          (list
            (css/rule "header,nav" (list "overflow" "visible"))
            (css/rule
              ".menu-drop-panel"
              (list
                "left"
                "auto"
                "right"
                "0"
                "max-width"
                "calc(100vw - .75rem)"))
            (css/rule ".capture-drop" (list "position" "static"))
            (css/rule
              ".capture-drop > .menu-drop-panel.capture-panel"
              (list
                "left"
                ".5rem"
                "right"
                ".5rem"
                "width"
                "auto"
                "min-width"
                "0"
                "max-width"
                "none"
                "box-sizing"
                "border-box"))))
        (css/rule
          "main"
          (list
            "padding"
            ".75rem"
            "max-width"
            "52rem"
            "margin"
            "0 auto"))
        (css/rule
          "h1"
          (list
            "font-size"
            "1.25rem"
            "font-weight"
            "700"
            "margin"
            ".25rem 0 .75rem"
            "border-bottom"
            "1px solid #000"
            "padding-bottom"
            ".25rem"))
        (css/rule
          "h2"
          (list
            "font-size"
            "1.05rem"
            "font-weight"
            "700"
            "margin"
            "0 0 .5rem"))
        (css/rule "p" (list "margin" ".5rem 0"))
        (css/rule "label" (list "font-weight" "700"))
        (css/rule
          ".meta,.muted"
          (list "color" "var(--muted)" "font-size" ".9rem"))
        (css/rule
          ".err"
          (list "color" "var(--err)" "font-weight" "700"))
        (css/rule
          ".ok"
          (list "color" "var(--ok)" "font-weight" "700"))
        (css/rule
          "#out"
          (list
            "white-space"
            "pre-wrap"
            "min-height"
            "4rem"
            "background"
            "var(--panel)"
            "border"
            "1px solid #000"
            "padding"
            ".5rem"
            "font-family"
            "var(--font-mono)"))
        (css/rule
          "ul.actions"
          (list
            "list-style"
            "none"
            "padding"
            "0"
            "margin"
            ".5rem 0"))
        (css/rule "ul.actions li" (list "margin" ".4rem 0"))
        (css/rule
          "#ol-selected-label"
          (list
            "font-size"
            ".85rem"
            "max-width"
            "12rem"
            "overflow"
            "hidden"
            "text-overflow"
            "ellipsis"
            "white-space"
            "nowrap"
            "margin-right"
            ".35rem"))
        (css/rule ".br-table-wrap" (list "overflow-x" "auto"))
        (css/rule
          ".br-table"
          (list "width" "100%" "font-size" ".9rem"))
        (css/rule
          ".br-table th,.br-table td"
          (list "padding" ".35rem .4rem" "vertical-align" "top"))
        (css/rule
          "tr.br-current"
          (list "background" "var(--sel-bg)"))
        (css/rule
          ".br-stats"
          (list
            "display"
            "flex"
            "flex-wrap"
            "wrap"
            "gap"
            ".5rem"
            "margin"
            ".5rem 0"))
        (css/rule
          ".br-stat"
          (list "padding" ".4rem .6rem" "margin" "0"))
        (css/rule
          ".br-diff"
          (list
            "display"
            "grid"
            "grid-template-columns"
            "1fr 1fr"
            "gap"
            ".5rem"))
        (css/media
          "max-width:700px"
          (list
            (css/rule
              ".br-diff"
              (list "grid-template-columns" "1fr"))))
        (css/rule "form.inline-form" (list "display" "inline"))
        (css/rule
          "form.inline-form button"
          (list "margin-left" ".2rem"))
        (css/media
          "max-width:700px"
          (list
            (css/rule
              ".todo-layout"
              (list "grid-template-columns" "1fr"))))
        (css/rule
          ".badge.impact"
          (list
            "background"
            "var(--badge-bg)"
            "border"
            "1px solid var(--badge-border)"
            "padding"
            ".15rem .4rem"
            "font-size"
            ".85rem"))
        (css/rule
          ".bb-browser"
          (list
            "display"
            "flex"
            "flex-direction"
            "column"
            "gap"
            ".75rem"))
        (css/rule ".bb-three" (list "display" "block"))
        (css/rule ".bb-three .bb-col" (list "margin" ".5rem 0"))
        (css/rule
          ".bb-list"
          (list
            "list-style"
            "none"
            "padding"
            "0"
            "margin"
            ".25rem 0"))
        (css/rule
          ".bb-item"
          (list "margin" "0" "line-height" "1.35"))
        (css/rule
          ".bb-item a"
          (list
            "display"
            "block"
            "padding"
            ".2rem .35rem"
            "text-decoration"
            "none"
            "color"
            "var(--link)"
            "border-left"
            "3px solid transparent"))
        (css/rule
          ".bb-item.is-active a"
          (list
            "font-weight"
            "700"
            "background"
            "var(--row-alt)"
            "border-left"
            "3px solid #000"
            "color"
            "var(--ink)"))
        (css/rule
          ".bb-item a:hover"
          (list "background" "var(--chrome)" "color" "var(--ink)"))
        (css/rule
          ".bb-crumb"
          (list "margin" "0 0 .5rem" "font-size" ".9rem"))
        (css/rule
          ".bb-meta h3"
          (list "margin" ".75rem 0 .35rem" "font-size" "1rem"))
        (css/rule ".bb-code" (list "margin" "0"))
        (css/rule
          ".bb-code-head"
          (list
            "display"
            "flex"
            "flex-wrap"
            "wrap"
            "align-items"
            "baseline"
            "justify-content"
            "space-between"
            "gap"
            ".5rem"
            "margin-bottom"
            ".5rem"))
        (css/rule
          ".bb-code-head h2"
          (list "margin" "0" "font-size" "1.1rem"))
        (css/rule ".bb-code-actions" (list "margin" "0"))
        (css/rule
          ".bb-tabs"
          (list
            "display"
            "flex"
            "gap"
            ".35rem"
            "flex-wrap"
            "wrap"
            "margin"
            ".4rem 0 .6rem"
            "align-items"
            "center"))
        (css/rule
          ".bb-tab"
          (list
            "display"
            "inline-block"
            "padding"
            ".28rem .65rem"
            "border"
            "1px solid #000"
            "text-decoration"
            "none"
            "color"
            "inherit"
            "background"
            "var(--panel)"
            "border-radius"
            "2px"))
        (css/rule
          ".bb-tab:hover"
          (list "background" "var(--row-alt)"))
        (css/rule
          ".bb-tab.is-active"
          (list
            "background"
            "var(--ink)"
            "color"
            "var(--panel)"
            "font-weight"
            "700"))
        (css/rule
          ".bb-edit-form textarea"
          (list
            "width"
            "100%"
            "min-height"
            "14rem"
            "font-family"
            "ui-monospace, monospace"))
        (css/rule
          ".bb-code .bb-edit-form .card"
          (list
            "border"
            "none"
            "padding"
            "0"
            "box-shadow"
            "none"))
        (css/rule
          ".bb-source"
          (list
            "margin"
            "0"
            "max-height"
            "55vh"
            "overflow"
            "auto"
            "background"
            "var(--panel)"
            "border"
            "1px solid #000"
            "padding"
            ".6rem"
            "font-size"
            ".9rem"))
        (css/media
          "min-width:900px"
          (list
            (css/rule
              ".bb-three"
              (list
                "display"
                "grid"
                "grid-template-columns"
                "12rem 14rem 1fr"
                "gap"
                ".75rem"
                "align-items"
                "stretch"
                "min-height"
                "16rem"))
            (css/rule
              ".bb-three .bb-col"
              (list
                "max-height"
                "42vh"
                "overflow"
                "auto"
                "margin"
                "0"
                "display"
                "flex"
                "flex-direction"
                "column"))
            (css/rule
              ".bb-three .bb-col .bb-list"
              (list "flex" "1 1 auto" "overflow" "auto"))
            (css/rule
              ".bb-code .bb-source"
              (list "max-height" "50vh"))))
        (css/rule
          ".capture-panel"
          (list
            "min-width"
            "min(16rem, calc(100vw - 1rem))"
            "max-width"
            "min(22rem, calc(100vw - 1rem))"
            "padding"
            ".4rem"
            "box-sizing"
            "border-box"))
        (css/rule
          ".capture-nav-form"
          (list
            "display"
            "flex"
            "flex-direction"
            "column"
            "gap"
            ".35rem"
            "margin"
            "0"))
        (css/rule
          ".capture-nav-form input[type=text],.capture-nav-form textarea"
          (list
            "width"
            "100%"
            "box-sizing"
            "border-box"
            "font-size"
            ".9rem"))
        (css/rule
          ".capture-nav-form .capture-body"
          (list "min-height" "4rem"))
        (css/rule
          ".capture-nav-form .capture-row"
          (list
            "display"
            "flex"
            "flex-wrap"
            "wrap"
            "gap"
            ".35rem"
            "align-items"
            "center"))
        (css/rule
          ".capture-nav-form .capture-row button"
          (list
            "width"
            "auto"
            "min-width"
            "4.5rem"
            "margin"
            "0"))
        (css/rule
          ".capture-nav-form #capture-status"
          (list "font-size" ".85rem" "min-height" "1.1em"))
        (css/rule
          ".capture-nav-form .ok"
          (list "color" "var(--ok)" "font-weight" "700"))
        (css/rule
          ".capture-nav-form .err"
          (list "color" "var(--err)" "font-weight" "700"))
        (css/rule
          ".capture-card label"
          (list
            "display"
            "block"
            "margin"
            ".35rem 0 .15rem"
            "font-weight"
            "700"))
        (css/rule
          ".capture-card .capture-body"
          (list
            "width"
            "100%"
            "box-sizing"
            "border-box"
            "min-height"
            "4.5rem"))
        (css/rule
          ".capture-card .capture-row"
          (list
            "display"
            "flex"
            "flex-wrap"
            "wrap"
            "gap"
            ".5rem"
            "align-items"
            "center"
            "margin-top"
            ".5rem"))
        (css/rule
          ".capture-card .capture-row button"
          (list "width" "auto" "min-width" "6rem"))
(css/rule
          "ol.bible-chapter"
          (list
            "list-style"
            "none"
            "padding-left"
            "0"
            "margin"
            ".25rem 0 1rem"))
(css/rule
          "ol.bible-chapter>li"
          (list
            "margin"
            ".35rem 0"
            "line-height"
            "1.45"
            "padding-left"
            "0"))
(css/rule
          "ol.bible-chapter>li>sup"
          (list
            "font-size"
            ".75em"
            "font-weight"
            "700"
            "margin-right"
            ".2em"
            "color"
            "var(--muted)"
            "vertical-align"
            "super"))
(css/rule
          ".bible-go label,.bible-search label"
          (list
            "display"
            "inline-block"
            "margin"
            ".2rem .45rem .2rem 0"))
(css/rule
          ".bible-go select,.bible-search input[type=text]"
          (list
            "width"
            "auto"
            "min-width"
            "6rem"
            "max-width"
            "100%"
            "margin-right"
            ".35rem"))

))))

(define ui/theme-header
  (lambda
    ()
    "Render the theme-header base stylesheet rules."
    (css/sheet
      (list
        (css/rule
          "header"
          (list
            "background"
            "var(--panel)"
            "border-bottom"
            "2px solid #000"
            "padding"
            ".5rem .75rem"))
        (css/rule
          "header .brand"
          (list
            "font-weight"
            "700"
            "font-size"
            "1.1rem"
            "letter-spacing"
            ".02em"
            "color"
            "var(--ink)"
            "margin-right"
            ".75rem"))
        (css/rule
          "header .meta"
          (list
            "display"
            "block"
            "font-family"
            "var(--font-mono)"
            "font-size"
            ".8rem"
            "color"
            "var(--muted)"
            "margin"
            ".25rem 0 .5rem"))))))

(define ui/theme-table
  (lambda
    ()
    "Render the theme-table base stylesheet rules."
    (css/sheet
      (list
        (css/rule
          "table"
          (list
            "border-collapse"
            "collapse"
            "width"
            "100%"
            "background"
            "var(--panel)"
            "border"
            "1px solid #000"
            "font-size"
            ".9rem"))
        (css/rule
          "th,td"
          (list
            "border"
            "1px solid #808080"
            "padding"
            ".45rem .5rem"
            "text-align"
            "left"
            "vertical-align"
            "top"
            "word-break"
            "break-word"))
        (css/rule
          "th"
          (list
            "background"
            "var(--chrome)"
            "font-weight"
            "700"
            "border-color"
            "var(--ink)"))
        (css/rule
          "tr:nth-child(even) td"
          (list "background" "var(--row-alt)"))))))

(define ui/theme-form
  (lambda
    ()
    "Render the theme-form base stylesheet rules."
    (css/sheet
      (list
        (css/rule
          "pre,textarea,input[type=text],input[type=search],input:not([type])"
          (list
            "font-family"
            "var(--font-mono)"
            "font-size"
            "1rem"
            "background"
            "var(--panel)"
            "color"
            "var(--ink)"
            "border"
            "1px solid #000"
            "border-radius"
            "0"
            "padding"
            ".5rem"
            "width"
            "100%"
            "max-width"
            "100%"))
        (css/rule
          "pre"
          (list
            "white-space"
            "pre-wrap"
            "overflow-x"
            "auto"
            "line-height"
            "1.35"))
        (css/rule
          "textarea"
          (list
            "min-height"
            "16rem"
            "line-height"
            "1.35"
            "white-space"
            "pre"
            "tab-size"
            "2"))))))

(define ui/theme-button
  (lambda
    ()
    "Render the theme-button base stylesheet rules."
    (css/sheet
      (list
        (css/rule
          "button,.btn,input[type=submit]"
          (list
            "font"
            "inherit"
            "font-size"
            "1rem"
            "cursor"
            "pointer"
            "background"
            "var(--chrome)"
            "color"
            "var(--ink)"
            "border"
            "2px outset #fff"
            "border-radius"
            "0"
            "padding"
            ".5rem .85rem"
            "min-height"
            "2.75rem"))
        (css/rule
          "button:active,.btn:active"
          (list "border-style" "inset"))
        (css/rule
          "button.secondary,.btn.secondary"
          (list "background" "var(--bg)"))
        (css/rule
          "a.btn"
          (list
            "display"
            "inline-block"
            "text-decoration"
            "none"
            "color"
            "var(--ink)"
            "line-height"
            "1.2"
            "vertical-align"
            "middle"))
        (css/rule "a.btn:visited" (list "color" "var(--ink)"))))))

(define ui/theme-mobile
  (lambda
    ()
    "Render the theme-mobile mobile override stylesheet rules."
    (css/sheet
      (list
        (css/media
          "max-width:40rem"
          (list
            (css/rule "main" (list "padding" ".5rem"))
            (css/rule "header" (list "padding" ".5rem"))
            (css/rule "nav" (list "gap" ".3rem" "width" "100%"))
            (css/rule
              "nav > a,.menu-drop"
              (list
                "flex"
                "1 1 0"
                "min-width"
                "0"
                "text-align"
                "center"))
            (css/rule "nav > a" (list "display" "block"))
            (css/rule
              ".menu-drop-btn"
              (list
                "display"
                "block"
                "width"
                "100%"
                "text-align"
                "center"))
            (css/rule
              "table"
              (list
                "display"
                "block"
                "overflow-x"
                "auto"
                "-webkit-overflow-scrolling"
                "touch"))
            (css/rule "h1" (list "font-size" "1.15rem"))
            (css/rule
              "button,.btn"
              (list "width" "100%" "margin" ".2rem 0"))
            (css/rule
              ".card form button,.card form .btn"
              (list "width" "auto" "min-width" "44%"))
            (css/rule
              "button.ol-btn,button.ol-todo-btn, .ol-btn,.ol-todo,.ol-todo-btn"
              (list "width" "auto" "margin" "0"))
            (css/rule
              ".ol-node"
              (list "margin-left" ".1rem" "padding-left" ".3rem"))
            (css/rule
              ".ol-sum"
              (list "padding-right" "2.5rem" "align-items" "center"))
            (css/rule ".ol-toolbar" (list "display" "none"))
            (css/rule "details.ol-actions" (list "display" "block"))
            (css/rule ".ol-sum" (list "padding-right" "2.75rem"))
            (css/rule
              "details.ol-actions[open] > .ol-actions-panel"
              (list
                "min-width"
                "9rem"
                "padding"
                ".35rem"
                "box-shadow"
                "2px 2px 0 #808080"))
            (css/rule
              "details.ol-actions[open] > .ol-actions-panel .ol-btn"
              (list
                "min-height"
                "2.4rem"
                "width"
                "100%"
                "margin"
                "0 0 .25rem"
                "background"
                "var(--panel)"
                "text-align"
                "left"))))))))

(define ui/theme-nav
  (lambda
    ()
    "Render the theme navigation stylesheet rules."
    (css/sheet
      (list
        (css/rule
          "nav"
          (list
            "display"
            "flex"
            "flex-wrap"
            "wrap"
            "gap"
            ".35rem .5rem"
            "align-items"
            "center"
            "width"
            "100%"))
        (css/rule "nav>a, .menu-drop" (list "flex" "0 0 auto"))
        (css/rule
          ".menu-drop-btn"
          (list
            "white-space"
            "nowrap"
            "display"
            "inline-flex"
            "align-items"
            "center"
            "justify-content"
            "center"
            "gap"
            ".25rem"))
        (css/media
          "max-width:40rem"
          (list
            (css/rule
              "nav>a, .menu-drop"
              (list
                "flex"
                "1 1 0"
                "min-width"
                "0"
                "text-align"
                "center"))
            (css/rule
              "nav>a"
              (list "display" "block" "text-align" "center"))
            (css/rule
              ".menu-drop-btn"
              (list
                "display"
                "block"
                "width"
                "100%"
                "text-align"
                "center"
                "box-sizing"
                "border-box"))))
        (css/rule
          ".capture-panel"
          (list
            "min-width"
            "min(16rem, calc(100vw - 1rem))"
            "max-width"
            "min(22rem, calc(100vw - 1rem))"
            "padding"
            ".4rem"
            "box-sizing"
            "border-box"))
        (css/media
          "max-width:40rem"
          (list
            (css/rule ".capture-drop" (list "position" "static"))
            (css/rule
              ".capture-drop > .menu-drop-panel.capture-panel"
              (list
                "left"
                ".5rem"
                "right"
                ".5rem"
                "width"
                "auto"
                "min-width"
                "0"
                "max-width"
                "none"
                "box-sizing"
                "border-box"))))
        (css/rule
          ".capture-nav-form"
          (list
            "display"
            "flex"
            "flex-direction"
            "column"
            "gap"
            ".35rem"
            "margin"
            "0"))
        (css/rule
          ".capture-nav-form input[type=text], .capture-nav-form textarea"
          (list
            "width"
            "100%"
            "box-sizing"
            "border-box"
            "font-size"
            ".9rem"))
        (css/rule
          ".capture-nav-form .capture-body"
          (list "min-height" "4rem"))
        (css/rule
          ".capture-nav-form .capture-row"
          (list
            "display"
            "flex"
            "flex-wrap"
            "wrap"
            "gap"
            ".35rem"
            "align-items"
            "center"))
        (css/rule
          ".capture-nav-form .capture-row button"
          (list
            "width"
            "auto"
            "min-width"
            "4.5rem"
            "margin"
            "0"))
        (css/rule
          ".capture-nav-form .capture-row a"
          (list
            "display"
            "inline-block"
            "padding"
            ".35rem .45rem"))
        (css/rule
          ".capture-nav-form #capture-status"
          (list "font-size" ".85rem" "min-height" "1.1em"))
        (css/rule
          ".capture-nav-form .ok"
          (list "color" "var(--ok)" "font-weight" "700"))
        (css/rule
          ".capture-nav-form .err"
          (list "color" "var(--err)" "font-weight" "700"))
        (css/rule
          ".menu-drop-panel .menu-drop"
          (list
            "margin"
            "0"
            "display"
            "block"
            "width"
            "100%"
            "box-sizing"
            "border-box"))
        (css/rule
          ".menu-drop-panel .menu-drop > summary"
          (list
            "display"
            "block"
            "width"
            "100%"
            "text-align"
            "left"
            "box-sizing"
            "border-box"))
        (css/rule
          ".menu-drop-panel > .menu-drop-btn"
          (list
            "display"
            "block"
            "width"
            "100%"
            "text-align"
            "left"
            "box-sizing"
            "border-box"))
        (css/rule
          ".menu-drop-panel .menu-drop > summary::after"
          (list "content" "\" ‾\""))
        (css/rule
          ".menu-drop-panel .menu-drop-panel"
          (list
            "left"
            "100%"
            "top"
            "0"
            "right"
            "auto"
            "margin-top"
            "0"))
        (css/rule
          ".menu-drop-panel .menu-drop:last-child > .menu-drop-panel"
          (list "left" "auto" "right" "100%"))
        (css/media
          "max-width:40rem"
          (list
            (css/rule
              ".menu-drop-panel .menu-drop-panel"
              (list
                "left"
                "0"
                "right"
                "0"
                "top"
                "100%"
                "position"
                "static"
                "margin-top"
                "0"))
            (css/rule
              ".menu-drop-panel .menu-drop > summary::after"
              (list "content" "\" v\""))))))))
