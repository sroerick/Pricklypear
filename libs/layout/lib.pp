;; package layout -- exported from live branch system/main
;; Sync: scripts/pp_sync.py pull|push

(define layout/card
  (lambda
    (title body)
    (html/div
      (html/attr "class" "layout-card")
      (html/join
        (list
          (html/tag
            "h3"
            (html/attr "class" "layout-card-title")
            (html/text title))
          (html/div (html/attr "class" "layout-card-body") body))))))

(define layout/cards
  (lambda
    (card-htmls)
    (html/div
      (html/attr "class" "layout-cards")
      (html/join card-htmls))))

(define layout/cta
  (lambda
    (href label)
    (html/tag
      "a"
      (html/attrs
        (list
          (html/attr "class" "layout-cta")
          (html/attr "href" href)))
      (html/text label))))

(define layout/hero
  (lambda
    (title tagline actions-html)
    (html/div
      (html/attr "class" "layout-hero")
      (html/join
        (list
          (layout/logo)
          (html/tag
            "h1"
            (html/attr "class" "layout-hero-title")
            (html/text title))
          (html/tag
            "p"
            (html/attr "class" "layout-hero-tagline")
            (html/text tagline))
          (html/div
            (html/attr "class" "layout-hero-actions")
            actions-html))))))

(define layout/logo
  (lambda
    ()
    (html/void
      "img"
      (html/attrs
        (list
          (html/attr "src" "/assets/pplogo.png")
          (html/attr "alt" "Pricklypear")
          (html/attr "class" "layout-logo")
          (html/attr "width" "160")
          (html/attr "height" "156"))))))

(define layout/nc-badge
  (lambda
    (href bg fg label)
    (html/tag
      "a"
      (html/attrs
        (list
          (html/attr "class" "nc-badge")
          (html/attr "href" href)
          (html/attr
            "style"
            (string-append "background:" bg ";color:" fg))))
      (html/text label))))

(define layout/neocities-banner
  (lambda
    ()
    (html/div
      (html/attr "class" "nc-banner")
      (html/join
        (list
          (html/tag
            "span"
            (html/attr "class" "nc-construction")
            (html/text "UNDER CONSTRUCTION"))
          (html/text " ")
          (html/tag
            "span"
            (html/attr "class" "nc-blink")
            (html/text "welcome to my homepage!!"))
          (html/text
            " \226\128\148 last updated 2026 \226\128\148 "))))))

(define layout/neocities-foot
  (lambda
    ()
    (html/div
      (html/attr "class" "nc-foot")
      (html/join
        (list
          (html/div
            (html/attr "class" "nc-webring")
            (html/join
              (list
                (html/text "[ ")
                (html/tag
                  "a"
                  (html/attr "href" "/welcome")
                  (html/text "prev"))
                (html/text " | ")
                (html/tag
                  "a"
                  (html/attr "href" "/background")
                  (html/text "list"))
                (html/text " | ")
                (html/tag
                  "a"
                  (html/attr "href" "/roadmap")
                  (html/text "random"))
                (html/text " | ")
                (html/tag
                  "a"
                  (html/attr "href" "/code")
                  (html/text "next"))
                (html/text " ]  the pricklypear webring"))))
          (html/div
            (html/attr "class" "nc-badges")
            (html/join
              (list
                (layout/nc-badge
                  "/welcome"
                  "#000080"
                  "#ff0"
                  "BEST VIEWED IN NETSCAPE")
                (layout/nc-badge
                  "/code"
                  "#000"
                  "#fff"
                  "HAND-CODED LISP")
                (layout/nc-badge
                  "/book.pdf"
                  "#600"
                  "#fff"
                  "NO JAVASCRIPT")
                (layout/nc-badge
                  "/code"
                  "#070"
                  "#fff"
                  "POSTGRES POWERED")
                (layout/nc-badge
                  "/agent.txt"
                  "#036"
                  "#fff"
                  "AGENT READY")
                (layout/nc-badge
                  "/guestbook"
                  "#609"
                  "#fff"
                  "SIGN GUESTBOOK")
                (layout/nc-badge "/welcome" "#fc0" "#000" "Y2K READY")
                (layout/nc-badge
                  "/welcome"
                  "#fff"
                  "#000"
                  "VALID HTML 4.01"))))
          (html/div
            (html/attr "class" "nc-counter")
            (html/join
              (list
                (html/tag
                  "span"
                  (html/attr "class" "nc-counter-label")
                  (html/text "you are visitor"))
                (html/tag
                  "span"
                  (html/attr "class" "nc-counter-digits")
                  (html/text "0000042"))
                (html/tag
                  "span"
                  (html/attr "class" "nc-counter-label")
                  (html/text "since 2026")))))
          (html/void "hr" (html/attr "class" "nc-rainbow-hr"))
          (html/div
            (html/attr "class" "nc-colophon")
            (html/text
              "this site is hand-coded in a lisp machine and rendered by a cactus \240\159\140\181 powered by postgres, htmx, and questionable life choices. no javascript was harmed. best viewed at 800x600.")))))))

(define layout/neocities-marquee-msg
  (lambda
    ()
    (html/tag
      "span"
      (html/attr "class" "nc-marquee-msg")
      (html/text
        "\240\159\154\167 UNDER CONSTRUCTION \240\159\154\167 welcome to pricklypear \240\159\140\181 a lisp machine that lives in postgres \194\183 sign the guestbook \194\183 best viewed in netscape 4.0 at 800x600 \194\183 built in public \194\183 "))))

(define layout/neocities-style
  (lambda
    ()
    (html/tag "style" "" (layout/neocities-theme))))

(define layout/neocities-theme
  (lambda
    ()
    (html/join
      (list
        "body{background-color:#2a6a6a;background-image:repeating-linear-gradient(45deg,rgba(255,255,255,.05) 0 2px,transparent 2px 14px),repeating-linear-gradient(-45deg,rgba(0,0,0,.06) 0 2px,transparent 2px 14px)}"
        ".layout-page{background:#faf7f4;border:2px solid #000;box-shadow:6px 6px 0 #000}"
        ".layout-hero{border:2px solid #000;background:linear-gradient(180deg,#ffd1a4,#ffe9c8);margin:1rem 0;box-shadow:4px 4px 0 #000}"
        ".layout-card{box-shadow:3px 3px 0 #000}"
        "a.layout-cta{box-shadow:3px 3px 0 #000}"
        "a.layout-cta:hover{background:#000;color:#ff0}"
        "a.layout-cta:visited:hover{color:#ff0}"
        ".nc-marquee-bar{background:#000;color:#ff0;border-bottom:2px solid #000;overflow:hidden;white-space:nowrap;font-family:monospace;font-size:.85rem;padding:.3rem 0}"
        ".nc-marquee-track{display:inline-block;white-space:nowrap;animation:nc-scroll 30s linear infinite}"
        "@keyframes nc-scroll{0%{transform:translateX(0)}100%{transform:translateX(-50%)}}"
        ".nc-blink{animation:nc-blink 1.1s steps(2,start) infinite}"
        "@keyframes nc-blink{50%{opacity:0}}"
        ".nc-banner{text-align:center;padding:.6rem .5rem;font-family:monospace;font-size:.85rem}"
        ".nc-construction{display:inline-block;background:repeating-linear-gradient(45deg,#fc0 0 12px,#000 12px 16px);color:#000;font-weight:700;padding:.12rem .4rem;border:2px solid #000;letter-spacing:.05em;text-shadow:0 0 2px #fc0,0 0 2px #fc0}"
        ".nc-foot{background:#000040;color:#cdd;border-top:2px solid #000;padding:1rem .75rem;text-align:center;font-family:monospace;font-size:.8rem}"
        ".nc-foot a{color:#9cf}"
        ".nc-webring{font-family:monospace;font-size:.8rem;margin:.4rem 0}"
        ".nc-webring a{color:#ff0;text-decoration:none}"
        ".nc-webring a:hover{text-decoration:underline}"
        ".nc-badges{display:flex;flex-wrap:wrap;gap:.4rem;justify-content:center;margin:.6rem 0}"
        "a.nc-badge{display:inline-flex;align-items:center;justify-content:center;width:88px;height:31px;border:2px outset #bbb;font-family:monospace;font-size:8px;line-height:1.1;text-align:center;text-decoration:none;padding:1px;box-sizing:border-box;overflow:hidden}"
        "a.nc-badge:hover{border-style:inset}"
        ".nc-counter{display:inline-flex;align-items:center;gap:.35rem;font-family:monospace;font-size:.78rem;margin:.5rem 0}"
        ".nc-counter-digits{background:#000;border:2px inset #555;color:#0f0;padding:.12rem .4rem;font-family:monospace;font-weight:700;letter-spacing:.18em}"
        ".nc-counter-label{color:#9cf}"
        ".nc-rainbow-hr{height:4px;border:0;background:linear-gradient(90deg,red,orange,#fc0,green,blue,#a0f);margin:1rem auto;width:80%}"
        ".nc-colophon{color:#88a;font-size:.72rem;margin-top:.4rem;line-height:1.5}"
        ".err{color:#c00;font-weight:700}"
        ".ok{color:#070;font-weight:700}"
        ".gb-list{list-style:none;padding:0;margin:1rem 0}"
        ".gb-entry{border:2px solid #000;background:#fff;padding:.6rem;margin:.5rem 0;box-shadow:3px 3px 0 #000}"
        ".gb-name{font-weight:700;color:#000}"
        ".gb-site{color:#000080;text-decoration:underline;font-size:.85rem}"
        ".gb-msg{margin-top:.3rem;color:#222;line-height:1.4}"
        ".gb-form{max-width:30rem}"
        ".gb-form input,.gb-form textarea{width:100%;font-family:monospace;padding:.3rem;margin:.2rem 0;border:2px solid #000;background:#fff}"))))

(define layout/neocities-top
  (lambda
    ()
    (html/div
      (html/attr "class" "nc-marquee-bar")
      (html/div
        (html/attr "class" "nc-marquee-track")
        (html/join
          (list
            (layout/neocities-marquee-msg)
            (layout/neocities-marquee-msg)))))))

(define layout/page
  (lambda
    (parts)
    (render-html
      (html/join
        (list
          (layout/style)
          (layout/neocities-style)
          (layout/neocities-top)
          (html/div
            (html/attr "class" "layout-page")
            (html/join
              (list (layout/neocities-banner) (html/join parts))))
          (layout/neocities-foot))))))

(define layout/page-with-css
  (lambda
    (parts)
    (render-html
      (html/join
        (list
          (layout/style)
          (html/div
            (html/attr "class" "layout-page")
            (html/join parts)))))))

(define layout/section
  (lambda
    (title body)
    (html/div
      (html/attr "class" "layout-section")
      (html/join
        (list
          (html/tag
            "h2"
            (html/attr "class" "layout-section-title")
            (html/text title))
          (html/div
            (html/attr "class" "layout-section-body")
            body))))))

(define layout/style
  (lambda
    ()
    (html/tag "style" "" (css/render (layout/theme)))))

(define layout/theme
  (lambda
    ()
    (css/sheet
      (list
        (css/rule
          ".layout-hero"
          (list
            "padding"
            "2.5rem 1.25rem 2rem"
            "text-align"
            "center"
            "border-bottom"
            "2px solid #000"
            "background"
            "#faf7f4"
            "margin"
            "0 0 1.25rem"))
        (css/rule
          ".layout-logo"
          (list
            "display"
            "block"
            "width"
            "10rem"
            "height"
            "auto"
            "max-width"
            "40vw"
            "margin"
            "0 auto 1rem"
            "image-rendering"
            "auto"))
        (css/rule
          ".layout-hero-title"
          (list
            "font-size"
            "2rem"
            "font-weight"
            "700"
            "margin"
            "0 0 .5rem"
            "letter-spacing"
            "-0.02em"))
        (css/rule
          ".layout-hero-tagline"
          (list
            "font-size"
            "1.1rem"
            "max-width"
            "36rem"
            "margin"
            "0 auto 1.25rem"
            "line-height"
            "1.5"
            "color"
            "#333"))
        (css/rule
          ".layout-hero-actions"
          (list
            "display"
            "flex"
            "flex-wrap"
            "wrap"
            "gap"
            ".5rem"
            "justify-content"
            "center"
            "align-items"
            "center"))
        (css/rule
          "a.layout-cta"
          (list
            "display"
            "inline-block"
            "padding"
            ".45rem .9rem"
            "border"
            "2px solid #000"
            "background"
            "#fff"
            "color"
            "#000"
            "text-decoration"
            "none"
            "font-weight"
            "700"
            "font-size"
            ".95rem"))
        (css/rule
          "a.layout-cta:hover"
          (list "background" "#000" "color" "#fff"))
        (css/rule "a.layout-cta:visited" (list "color" "#000"))
        (css/rule
          "a.layout-cta:visited:hover"
          (list "color" "#fff"))
        (css/rule
          ".layout-cards"
          (list
            "display"
            "grid"
            "grid-template-columns"
            "repeat(auto-fit,minmax(14rem,1fr))"
            "gap"
            ".75rem"
            "margin"
            "1rem 0"))
        (css/rule
          ".layout-card"
          (list
            "border"
            "2px solid #000"
            "background"
            "#fff"
            "padding"
            ".85rem"
            "text-align"
            "left"))
        (css/rule
          ".layout-card-title"
          (list "margin" "0 0 .4rem" "font-size" "1.05rem"))
        (css/rule
          ".layout-card-body"
          (list
            "font-size"
            ".95rem"
            "line-height"
            "1.45"
            "color"
            "#222"))
        (css/rule
          ".layout-section"
          (list "margin" "1.5rem 0" "padding" "0 .25rem"))
        (css/rule
          ".layout-section-title"
          (list
            "font-size"
            "1.25rem"
            "border-bottom"
            "1px solid #808080"
            "padding-bottom"
            ".25rem"
            "margin"
            "0 0 .75rem"))
        (css/rule
          ".layout-section-body"
          (list "line-height" "1.55" "max-width" "40rem"))
        (css/rule
          ".layout-page"
          (list
            "max-width"
            "52rem"
            "margin"
            "0 auto"
            "padding"
            "0 .75rem 2rem"))
        (css/rule
          ".layout-foot"
          (list
            "margin-top"
            "2rem"
            "padding-top"
            ".75rem"
            "border-top"
            "1px solid #808080"
            "font-size"
            ".85rem"
            "color"
            "#333"
            "text-align"
            "center"))
        (css/media
          "max-width:40rem"
          (list
            (css/rule
              ".layout-hero"
              (list "padding" "1.5rem .75rem 1.25rem"))
            (css/rule
              ".layout-hero-title"
              (list "font-size" "1.55rem"))))))))

(define layout/theme-vars
  (lambda
    ()
    (css/sheet
      (list
        (css/rule
          ":root"
          (list
            "--cactus-green"
            "#2d5a27"
            "--cactus-light"
            "#4a7c43"
            "--cactus-lighter"
            "#6b9b5c"
            "--cactus-yellow"
            "#f4d03f"
            "--cactus-flower"
            "#e74c3c"
            "--cactus-spike"
            "#1a1a1a"
            "--cactus-shadow"
            "4px 4px 0 #1a1a1a"))))))

