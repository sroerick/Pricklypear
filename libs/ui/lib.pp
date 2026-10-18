;; package ui -- exported from live branch system/main
;; Sync: scripts/pp-sync.sh status|pull|snapshot

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

(define ui/badge
  (lambda
    (text tone)
    (ui/elm
      "span"
      (list "class" (string-append "lb-badge lb-badge-" tone))
      (list (ui/text text))))
  "One inline badge: text, tone class.")

(define ui/badge/examples
  (lambda
    ()
    (lb/obj
      (list
        "name"
        "ui/badge"
        "doc"
        "One inline badge (seed lookbook element)."
        "file"
        "libs/lookbook/lib.pp"
        "formals"
        (json-emit (list "text" "tone"))
        "variants"
        (json-emit
          (list
            (lb/obj (list "text" "stable" "tone" "ok"))
            (lb/obj (list "text" "beta" "tone" "warn"))))
        "source"
        "(ui/badge text tone) ->\n  (ui/elm \"span\"\n    (list \"class\" (string-append \"lb-badge lb-badge-\" tone))\n    (list (ui/text text)))")))
  "The ui/badge card.")

(define ui/bubble
  (lambda
    (kind meta text)
    (ui/elm
      "div"
      (list "class" (string-append "bubble " kind))
      (list
        (ui/elm
          "div"
          (list "class" "bubble-meta")
          (list (ui/text meta)))
        (ui/elm
          "div"
          (list "class" "bubble-body")
          (list (ui/text text))))))
  "One declared message bubble.")

(define ui/bubble/examples
  (lambda
    ()
    (lb/obj
      (list
        "name"
        "ui/bubble"
        "doc"
        "One chat bubble: kind (user|llm|tool) class, meta author/time line, text body."
        "file"
        "libs/chat/lib.pp"
        "formals"
        (json-emit (list "kind" "meta" "text"))
        "variants"
        (json-emit
          (list
            (lb/obj
              (list
                "kind"
                "user"
                "meta"
                "alice · 09-27 12:00"
                "text"
                "hello lobby"))
            (lb/obj
              (list
                "kind"
                "llm"
                "meta"
                "agent-prickle · 09-27 12:01"
                "text"
                "acknowledged"))))
        "source"
        "(ui/bubble kind meta text) ->\n  (ui/elm \"div\"\n    (list \"class\" (string-append \"bubble \" kind))\n    (list (ui/elm \"div\" (list \"class\" \"bubble-meta\") (list (ui/text meta)))\n          (ui/elm \"div\" (list \"class\" \"bubble-body\") (list (ui/text text)))))")))
  "The ui/bubble card.")

(define ui/button
  (lambda
    (label kind)
    (ui/elm
      "button"
      (list
        "class"
        (string-append "lb-btn lb-btn-" kind)
        "type"
        "button")
      (list (ui/text label))))
  "One kind-preset button: label text, the kind rides the class.")

(define ui/button/examples
  (lambda
    ()
    (lb/obj
      (list
        "name"
        "ui/button"
        "doc"
        "One kind-preset button (seed lookbook element)."
        "file"
        "libs/lookbook/lib.pp"
        "formals"
        (json-emit (list "label" "kind"))
        "variants"
        (json-emit
          (list
            (lb/obj (list "label" "Save" "kind" "primary"))
            (lb/obj (list "label" "Delete" "kind" "danger"))))
        "source"
        "(ui/button label kind) ->\n  (ui/elm \"button\"\n    (list \"class\" (string-append \"lb-btn lb-btn-\" kind)\n          \"type\" \"button\")\n    (list (ui/text label)))")))
  "The ui/button card: this def IS the element's declaration.")

(define ui/card
  (lambda
    (title badge-text body)
    (ui/elm
      "section"
      (list "class" "lb-card")
      (list
        (ui/elm
          "h3"
          (list)
          (list (ui/text title) (ui/badge badge-text "ok")))
        (ui/elm "p" (list) (list (ui/text body))))))
  "Card element: composes ui/badge INSIDE its title row -- reuse is a\ndef call, the composed value nests as kids (the reusability proof).")

(define ui/card/examples
  (lambda
    ()
    (lb/obj
      (list
        "name"
        "ui/card"
        "doc"
        "Card with an in-title badge: composes ui/badge (reuse = a def call; the composed value nests as kids)."
        "file"
        "libs/lookbook/lib.pp"
        "formals"
        (json-emit (list "title" "badge-text" "body"))
        "variants"
        (json-emit
          (list
            (lb/obj
              (list
                "title"
                "Notes"
                "badge-text"
                "12 new"
                "body"
                "The register answers /issues and the spec notes docs."))))
        "source"
        "(ui/card title badge-text body) ->\n  (ui/elm \"section\" (list \"class\" \"lb-card\")\n    (list (ui/elm \"h3\" (list)\n            (list (ui/text title) (ui/badge badge-text \"ok\")))\n          (ui/elm \"p\" (list) (list (ui/text body)))))")))
  "The ui/card card -- the composition seed (nesting visible in both\nprojections).")

(define ui/chat-composer
  (lambda
    (action target)
    (ui/elm
      "form"
      (chat/box
        (quote row)
        (quote (fit))
        "chat-composer"
        ""
        (list
          "method"
          "post"
          "action"
          action
          "hx-post"
          action
          "hx-target"
          target
          "hx-swap"
          "innerHTML"
            "hx-on::after-request"
            "if(event.detail.successful){this.reset();var l=document.querySelector(this.getAttribute('hx-target'));if(l){delete l.dataset.read;l.scrollTop=l.scrollHeight}}"))
      (list
        (ui/elm
          "textarea"
          (list
            "name"
            "body"
            "class"
            "chat-input"
            "rows"
            "1"
            "required"
            "required"
            "placeholder"
            "Write a message…"
            "maxlength"
            "4000"
            "style"
            (string-append
              (pads/size-css (quote (grow 1)))
              ";min-height:2.5rem;min-width:0"))
          (list))
        (ui/elm
          "button"
          (list "type" "submit" "class" "chat-send")
          (list (ui/text "Send"))))))
  "One declared composer row.")

(define ui/chat-composer/examples
  (lambda
    ()
    (lb/obj
      (list
        "name"
        "ui/chat-composer"
        "doc"
          "The input row INSIDE the chat shell: a growing, shrinkable text input (flex:1 + min-width:0) + a fitting send button, posting action and swapping target."
        "file"
        "libs/chat/lib.pp"
        "formals"
        (json-emit (list "action" "target"))
        "variants"
        (json-emit
          (list
            (lb/obj
              (list "action" "/chat" "target" "#chat-main-log"))))
          "source"
          "(ui/chat-composer action target) -> a chat-composer form row: hx-post action, hx-target target, hx-swap innerHTML; hx-on::after-request resets the input and snaps the target log to the latest on a successful send; the textarea is flex:1 + min-width:0 so the row shrinks cleanly at small widths")))
    "The ui/chat-composer card.")

(define ui/chat-log
  (lambda
    (id poll body)
    (ui/elm
      "div"
      (append
        (chat/box
          (quote column)
          (quote (grow 1))
          "chat-log"
          ";min-height:0;overflow-y:auto"
          (list "id" id))
        (list
          "hx-on::after-swap"
          "if(!this.dataset.read||this.scrollHeight-this.scrollTop-this.clientHeight<160)this.scrollTop=this.scrollHeight"
          "hx-on:wheel"
          "this.dataset.read=1"
          "hx-on:touchmove"
          "this.dataset.read=1")
        (if
          (string-eq poll "")
          (list)
          (list
            "hx-get"
            poll
            "hx-trigger"
            "load, every 4s"
            "hx-swap"
            "innerHTML")))
      (chat/kid-list body)))
  "One declared transcript region. Autoscroll: every swap snaps to the\n     latest message unless the reader scrolled up (wheel/touchmove sets the\n     read flag); the poll also fires on load, so the initial view lands at\n     the bottom.")

(define ui/chat-log/examples
  (lambda
    ()
    (lb/obj
      (list
        "name"
        "ui/chat-log"
        "doc"
        "The transcript region (swap target): a column that grows to fill the chat shell and scrolls. poll = htmx poll route (\"\" disables)."
        "file"
        "libs/chat/lib.pp"
        "formals"
        (json-emit (list "id" "poll" "body"))
        "variants"
        (json-emit
          (list
            (lb/obj
              (list
                "id"
                "chat-main-log"
                "poll"
                "/chat/partial"
                "body"
                "Say hi to the box."))))
        "source"
          "(ui/chat-log id poll body) -> a chat-log div with autoscroll hooks: hx-on::after-swap snaps to the latest unless the reader scrolled up (wheel/touchmove set a read flag); poll trigger is load, every 4s")))
    "The ui/chat-log card.")

(define ui/chatpage
  (lambda
    (id poll action body)
    (let
      ((log-id (string-append id "-log")))
      (ui/elm
        "div"
        (chat/box
          (quote column)
          (quote (grow 1))
          "chatpage"
          ";min-height:0"
          (list "id" id))
        (list
          (ui/chat-log log-id poll body)
          (ui/chat-composer action (string-append "#" log-id))))))
  "One declared chat column shell.")

(define ui/chatpage/examples
  (lambda
    ()
    (lb/obj
      (list
        "name"
        "ui/chatpage"
        "doc"
        "The chat column shell: transcript grows (ui/chat-log), composer is a row inside it (ui/chat-composer). Composed by def call."
        "file"
        "libs/chat/lib.pp"
        "formals"
        (json-emit (list "id" "poll" "action" "body"))
        "variants"
        (json-emit
          (list
            (lb/obj
              (list
                "id"
                "chat-main"
                "poll"
                "/chat/partial"
                "action"
                "/chat"
                "body"
                "Say hi to the box."))))
        "source"
        "(ui/chatpage id poll action body) ->\n  (ui/elm \"div\"\n    (chat/box 'column '(grow 1) \"chatpage\" \"\" (list \"id\" id))\n    (list (ui/chat-log (string-append id \"-log\") poll body)\n          (ui/chat-composer action (string-append \"#\" id \"-log\"))))")))
  "The ui/chatpage card.")

(define ui/css
  (lambda
    ()
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
            (css/use (bible/css))
            (css/use (ui/theme-menu))
            (css/use (ui/theme-branches))
            (css/use (ui/theme-code-browser))
            (css/use (ui/theme-capture))
            (css/use (ui/theme-mobile))
            (css/use (ui/theme-nav))
            (css/use notes/tree-css)
            (css/use (theme/extra)))))))
  "Render the combined UI stylesheet.")

(define ui/elm
  (lambda
    (tag attrs kids)
    (lb/obj
      (list
        "t"
        "el"
        "tag"
        (as-str tag)
        "attrs"
        (lb/obj attrs)
        "kids"
        (json-emit kids))))
  "A declared element: {\"t\":\"el\",\"tag\":T,\"attrs\":{..},\n\"kids\":[kid ...]}. attrs rides flat (k v ...) pairs, kids a list of\nkid wrappers (ui/text or nested ui/elm dicts). Composite elements\ncall other elements and pass their returned dicts as kids.")

(define ui/home
  (lambda
    ()
    (html/page (list (weather/widget) (astro/widget))))
  "Render the home page with weather and astro widgets.")

(define ui/home-style
  (lambda
    ()
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
          ".capture-card #capture-status.err,.capture-card .err{color:var(--err);font-weight:700}"))))
  "Render the home page style block.")

(define ui/menu
  (lambda
    ()
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
          (menu/link "repl" "/repl")))))
  "Build the site navigation menu structure.")

(define ui/nav
  (lambda
    ()
    (render-html
      (menu/bar
        (if
          (string-eq (type-of ui/menu) "lambda")
          (ui/menu)
          ui/menu))))
  "Render the site navigation bar.")

(define ui/nav-style
  (lambda
    ()
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
          ".capture-nav-form .err{color:var(--err);font-weight:700}"))))
  "Render the site navigation bar style block.")

(define ui/text
  (lambda (s) (lb/obj (list "t" "text" "v" (as-str s))))
  "A declared text kid: {\"t\":\"text\",\"v\":s}.")

(define ui/theme
  (lambda
    ()
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
          ".badge.impact"
          (list
            "background"
            "var(--badge-bg)"
            "border"
            "1px solid var(--badge-border)"
            "padding"
            ".15rem .4rem"
            "font-size"
            ".85rem")))))
  "Render the base theme stylesheet rules.")

(define ui/theme-branches
  (lambda
    ()
    (css/sheet
      (list
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
        (css/rule "form.inline-form" (list "display" "inline"))
        (css/rule
          "form.inline-form button"
          (list "margin-left" ".2rem"))
        (css/media
          "max-width:700px"
          (list
            (css/rule
              ".br-diff"
              (list "grid-template-columns" "1fr")))))))
  "Render the theme-branches stylesheet rules.")

(define ui/theme-button
  (lambda
    ()
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
        (css/rule "a.btn:visited" (list "color" "var(--ink)")))))
  "Render the theme-button base stylesheet rules.")

(define ui/theme-capture
  (lambda
    ()
    (css/sheet
      (list
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
          (list "width" "auto" "min-width" "6rem")))))
  "Render the theme-capture stylesheet rules.")

(define ui/theme-code-browser
  (lambda
    ()
    (css/sheet
      (list
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
              (list "max-height" "50vh")))))))
  "Render the theme-code-browser stylesheet rules.")

(define ui/theme-form
  (lambda
    ()
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
            "2")))))
  "Render the theme-form base stylesheet rules.")

(define ui/theme-header
  (lambda
    ()
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
            ".5rem .75rem"
            "display"
            "flex"
            "align-items"
            "center"
            "gap"
            ".5rem"))
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
          "header .brand-logo"
          (list
            "height"
            "1.7rem"
            "width"
            "auto"
            "display"
            "block"))
        (css/rule
          "header .meta"
          (list
            "margin"
            "0 0 0 auto"
            "text-align"
            "right"
            "font-family"
            "var(--font-mono)"
            "font-size"
            ".8rem"
            "color"
            "var(--muted)")))))
  "Render the theme-header base stylesheet rules.")

(define ui/theme-menu
  (lambda
    ()
    (css/sheet
      (list
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
                "border-box")))))))
  "Render the theme-menu stylesheet rules.")

(define ui/theme-mobile
  (lambda
    ()
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
                "left")))))))
  "Render the theme-mobile mobile override stylesheet rules.")

(define ui/theme-nav
  (lambda
    ()
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
              (list "content" "\" v\"")))))))
  "Render the theme navigation stylesheet rules.")

(define ui/theme-table
  (lambda
    ()
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
          (list "background" "var(--row-alt)")))))
  "Render the theme-table base stylesheet rules.")
