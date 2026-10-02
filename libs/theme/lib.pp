;; package theme -- exported from live branch system/main
;; Sync: scripts/pp_sync.py pull|push

(package theme
  (export
    theme/available
    theme/card-html
    theme/cards-html
    theme/current
    theme/entry-desc
    theme/entry-display
    theme/entry-name
    theme/entry-package
    theme/find
    theme/page
    theme/set
    theme/swatch-html
    theme/vars
    theme/extra
))

(define hi-page
  (lambda () (render-html "<h1>Hello</h1>")))

(define theme/archived-bg
  "#eeeeee")

(define theme/archived-border
  "#666666")

(define theme/archived-ink
  "#555555")

(define theme/available
  (lambda
    ()
    (list/map
      (lambda
        (e)
        (list
          (list "name" (theme/entry-name e))
          (list "display" (theme/entry-display e))
          (list "description" (theme/entry-desc e))
          (list "package" (theme/entry-package e))))
      theme/catalog)))

(define theme/badge-bg
  "#ffe8a3")

(define theme/badge-border
  "#8b6914")

(define theme/banner-bg
  "#fff3cd")

(define theme/banner-border
  "#8b0000")

(define theme/banner-ink
  "#1a1a1a")

(define theme/bg
  "#f4f0e6")

(define theme/border
  "#c4b8a0")

(define theme/cancelled-bg
  "#dddddd")

(define theme/card-html
  (lambda
    (e)
    (let
      ((name (theme/entry-name e))
        (display (theme/entry-display e))
        (desc (theme/entry-desc e))
        (cur (theme/current)))
      (let
        ((is-cur (string-eq name cur)))
        (html/div
          (html/attrs
            (list
              (html/attr
                "class"
                (if
                  is-cur
                  "card theme-card theme-card-current"
                  "card theme-card"))
              (html/attr "data-theme" name)))
          (html/join
            (list
              (html/h3 display)
              (theme/preview-swatch name)
              (html/p* (html/attr "class" "muted") (html/text desc))
              (if
                is-cur
                (html/p* (html/attr "class" "ok") "Current theme")
                (html/form
                  "POST"
                  "/settings/theme"
                  (html/attrs
                    (list
                      (html/attr "hx-post" "/settings/theme")
                      (html/attr "hx-target" "#theme-cards")
                      (html/attr "hx-swap" "outerHTML")))
                  (html/join
                    (list
                      (html/void
                        "input"
                        (html/attrs
                          (list
                            (html/attr "type" "hidden")
                            (html/attr "name" "name")
                            (html/attr "value" name))))
                      (html/button
                        (html/attr "type" "submit")
                        (string-append "Use " display)))))))))))))

(define theme/cards-html
  (lambda
    ()
    (html/div
      (html/attr "id" "theme-cards")
      (html/join (list/map theme/card-html theme/catalog)))))

(define theme/catalog
  (list
    (list
      "paper"
      "Paper"
      "Warm cream paper, dark ink, soft brown chrome. Easy on the eyes."
      "theme-paper")
    (list
      "midnight"
      "Midnight"
      "Soft blue-gray dark theme — no phosphor green."
      "theme-midnight")
    (list
      "classic"
      "Classic Mac"
      "Platinum gray System 7 vibes. The old product default."
      "theme-classic")
    (list
      "as400"
      "AS/400"
      "CRT green on near-black — the original notes-tree terminal skin."
      "theme-as400")))

(define theme/chrome
  "#ddd4c0")

(define theme/current
  (lambda
    ()
    (guard (lambda (_) "paper") (as-str theme/name))))

(define theme/done-bg
  "#cfc")

(define theme/done-ink
  "#226622")

(define theme/done-muted
  "#666666")

(define theme/entry-desc
  (lambda (e) (as-str (car (cdr (cdr e))))))

(define theme/entry-display
  (lambda (e) (as-str (car (cdr e)))))

(define theme/entry-name
  (lambda (e) (as-str (car e))))

(define theme/entry-package
  (lambda (e) (as-str (car (cdr (cdr (cdr e)))))))

(define theme/err
  "#a32020")

(define theme/extra
  (lambda
    ()
    (css/sheet
      (list
        (css/rule
          ".menu-drop-panel .menu-drop[open]>summary::before"
          (list "display" "none"))))))

(define theme/find
  (lambda
    (name)
    (let
      ((want (as-str name)))
      (list/foldl
        (lambda
          (acc e)
          (if
            (null? acc)
            (if (string-eq (theme/entry-name e) want) e acc)
            acc))
        ()
        theme/catalog))))

(define theme/font-mono
  "\"Courier New\",\"Lucida Console\",Monaco,\"DejaVu Sans Mono\",monospace")

(define theme/font-sans
  "Geneva,\"Lucida Grande\",\"DejaVu Sans\",Verdana,sans-serif")

(define theme/ink
  "#1c1916")

(define theme/link
  "#0b57a8")

(define theme/muted
  "#6b6458")

(define theme/name
  "paper")

(define theme/ok
  "#1f6b3a")

(define theme/page
  (lambda
    ()
    (if
      (string-eq (request-method) "POST")
      (let
        ((name (as-str (request-param "name"))))
        (seq
          (theme/set name)
          (if
            (hx-request?)
            (render-html (theme/cards-html))
            (redirect "/settings/theme"))))
      (html/page
        (list
          (html/h1 "Themes")
          (html/p*
            (html/attr "class" "muted")
            "Installable looks for your garden. Switching pins theme tokens on your branch; checkout main restores the system default.")
          (html/p*
            (html/attr "class" "muted")
            (string-append "Current: " (theme/current)))
          (theme/cards-html)
          (html/tag
            "style"
            ""
            (html/join
              (list
                ".theme-card{margin:.75rem 0;max-width:28rem}"
                ".theme-card-current{outline:2px solid var(--ok,#33ff33);outline-offset:2px}"
                ".theme-swatch{display:flex;gap:.35rem;margin:.4rem 0}"
                ".theme-swatch-chip{display:inline-block;width:1.5rem;height:1.5rem;border:1px solid rgba(0,0,0,.25);border-radius:3px}"))))))))

(define theme/panel
  "#ebe4d4")

(define theme/pending-bg
  "#fff8dc")

(define theme/pending-border
  "#886600")

(define theme/pending-ink
  "#664400")

(define theme/preview-swatch
  (lambda
    (name)
    (if
      (string-eq name "paper")
      (theme/swatch-html
        "#f4f0e6"
        "#1c1916"
        "#ebe4d4"
        "#ddd4c0"
        "#0b57a8")
      (if
        (string-eq name "midnight")
        (theme/swatch-html
          "#0f1419"
          "#d8dee9"
          "#1a2332"
          "#15202b"
          "#7aa2f7")
        (if
          (string-eq name "classic")
          (theme/swatch-html
            "#d4d0c8"
            "#000000"
            "#ece9d8"
            "#c0c0c0"
            "#0000ee")
          (if
            (string-eq name "as400")
            (theme/swatch-html
              "#111111"
              "#33cc33"
              "#1a1a1a"
              "#1a2a1a"
              "#33ff33")
            (theme/swatch-html
              "#111111"
              "#33cc33"
              "#1a1a1a"
              "#1a2a1a"
              "#33ff33")))))))

(define theme/row-alt
  "#efe9db")

(define theme/sel-accent
  "#000080")

(define theme/sel-bg
  "#fff8dc")

(define theme/set
  (lambda
    (name)
    (let
      ((e (theme/find name)))
      (if
        (null? e)
        (error
          (string-append
            "theme/set: unknown theme: "
            (as-str name)))
        (let
          ((pkg (theme/entry-package e))
            (want (theme/entry-name e)))
          (seq (load-library pkg) want))))))

(define theme/shadow
  "#c9c0ae")

(define theme/swatch-html
  (lambda
    (bg ink panel chrome link)
    (html/tag
      "div"
      (html/attr "class" "theme-swatch")
      (html/join
        (list
          (html/tag
            "span"
            (html/attrs
              (list
                (html/attr "class" "theme-swatch-chip")
                (html/attr "style" (string-append "background:" bg))))
            "")
          (html/tag
            "span"
            (html/attrs
              (list
                (html/attr "class" "theme-swatch-chip")
                (html/attr "style" (string-append "background:" ink))))
            "")
          (html/tag
            "span"
            (html/attrs
              (list
                (html/attr "class" "theme-swatch-chip")
                (html/attr "style" (string-append "background:" panel))))
            "")
          (html/tag
            "span"
            (html/attrs
              (list
                (html/attr "class" "theme-swatch-chip")
                (html/attr "style" (string-append "background:" chrome))))
            "")
          (html/tag
            "span"
            (html/attrs
              (list
                (html/attr "class" "theme-swatch-chip")
                (html/attr "style" (string-append "background:" link))))
            ""))))))

(define theme/today-bg
  "#ffffe0")

(define theme/vars
  (lambda
    ()
    (css/sheet
      (list
        (css/rule
          ":root"
          (list
            "--bg"
            theme/bg
            "--ink"
            theme/ink
            "--font-sans"
            theme/font-sans
            "--font-mono"
            theme/font-mono
            "--panel"
            theme/panel
            "--chrome"
            theme/chrome
            "--link"
            theme/link
            "--visited"
            theme/visited
            "--muted"
            theme/muted
            "--err"
            theme/err
            "--ok"
            theme/ok
            "--border"
            theme/border
            "--row-alt"
            theme/row-alt
            "--shadow"
            theme/shadow
            "--sel-bg"
            theme/sel-bg
            "--sel-accent"
            theme/sel-accent
            "--pending-bg"
            theme/pending-bg
            "--pending-ink"
            theme/pending-ink
            "--pending-border"
            theme/pending-border
            "--done-bg"
            theme/done-bg
            "--done-ink"
            theme/done-ink
            "--done-muted"
            theme/done-muted
            "--waiting-bg"
            theme/waiting-bg
            "--archived-bg"
            theme/archived-bg
            "--archived-ink"
            theme/archived-ink
            "--archived-border"
            theme/archived-border
            "--cancelled-bg"
            theme/cancelled-bg
            "--today-bg"
            theme/today-bg
            "--banner-bg"
            theme/banner-bg
            "--banner-border"
            theme/banner-border
            "--banner-ink"
            theme/banner-ink
            "--badge-bg"
            theme/badge-bg
            "--badge-border"
            theme/badge-border))))))

(define theme/visited
  "#5b3d8a")

(define theme/waiting-bg
  "#e0e8ff")

(define zz-dot-test
  (lambda () "Pricklypear · hosted · · release"))

(define zz-list
  (lambda () (list "café" "αβγ" "♥")))

(define zz-raw
  (lambda
    ()
    (render-html
      "<b>café</b> — αβ «→» <i>a&b<i> ♥")))

(define zz-unicode-handler
  (lambda () "café — αβγ «→» ♥"))
