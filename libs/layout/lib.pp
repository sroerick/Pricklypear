;; layout/* — reusable page composition blocks (hero, cards, sections).
;; NOT named public/*: "public" is auth/route visibility only.
;; Pure HTML strings via html/*; wrap with render-html at page level.

(define layout/cta
  (lambda (href label)
    (html/tag "a"
      (html/attrs
        (list
          (html/attr "class" "layout-cta")
          (html/attr "href" href)))
      (html/text label))))

;; Wordmark / mark from image/static (served at /assets/…).
(define layout/logo
  (lambda ()
    (html/void "img"
      (html/attrs
        (list
          (html/attr "src" "/assets/pplogo.png")
          (html/attr "alt" "Pricklypear")
          (html/attr "class" "layout-logo")
          (html/attr "width" "160")
          (html/attr "height" "156"))))))

(define layout/hero
  (lambda (title tagline actions-html)
    (html/div
      (html/attr "class" "layout-hero")
      (html/join
        (list
          (layout/logo)
          (html/tag "h1" (html/attr "class" "layout-hero-title")
            (html/text title))
          (html/tag "p" (html/attr "class" "layout-hero-tagline")
            (html/text tagline))
          (html/div
            (html/attr "class" "layout-hero-actions")
            actions-html))))))

(define layout/card
  (lambda (title body)
    (html/div
      (html/attr "class" "layout-card")
      (html/join
        (list
          (html/tag "h3" (html/attr "class" "layout-card-title")
            (html/text title))
          (html/div (html/attr "class" "layout-card-body") body))))))

(define layout/cards
  (lambda (card-htmls)
    (html/div
      (html/attr "class" "layout-cards")
      (html/join card-htmls))))

(define layout/section
  (lambda (title body)
    (html/div
      (html/attr "class" "layout-section")
      (html/join
        (list
          (html/tag "h2" (html/attr "class" "layout-section-title")
            (html/text title))
          (html/div (html/attr "class" "layout-section-body") body))))))

(define layout/theme
  (lambda ()
    (css/sheet
      (list
        (css/rule ".layout-hero"
          (list
            "padding" "2.5rem 1.25rem 2rem"
            "text-align" "center"
            "border-bottom" "2px solid #000"
            "background" "#faf7f4"
            "margin" "0 0 1.25rem"))
        (css/rule ".layout-logo"
          (list
            "display" "block"
            "width" "10rem"
            "height" "auto"
            "max-width" "40vw"
            "margin" "0 auto 1rem"
            "image-rendering" "auto"))
        (css/rule ".layout-hero-title"
          (list
            "font-size" "2rem"
            "font-weight" "700"
            "margin" "0 0 .5rem"
            "letter-spacing" "-0.02em"))
        (css/rule ".layout-hero-tagline"
          (list
            "font-size" "1.1rem"
            "max-width" "36rem"
            "margin" "0 auto 1.25rem"
            "line-height" "1.5"
            "color" "#333"))
        (css/rule ".layout-hero-actions"
          (list
            "display" "flex"
            "flex-wrap" "wrap"
            "gap" ".5rem"
            "justify-content" "center"
            "align-items" "center"))
        (css/rule "a.layout-cta"
          (list
            "display" "inline-block"
            "padding" ".45rem .9rem"
            "border" "2px solid #000"
            "background" "#fff"
            "color" "#000"
            "text-decoration" "none"
            "font-weight" "700"
            "font-size" ".95rem"))
        (css/rule "a.layout-cta:hover"
          (list
            "background" "#000"
            "color" "#fff"))
        (css/rule "a.layout-cta:visited"
          (list "color" "#000"))
        (css/rule "a.layout-cta:visited:hover"
          (list "color" "#fff"))
        (css/rule ".layout-cards"
          (list
            "display" "grid"
            "grid-template-columns" "repeat(auto-fit,minmax(14rem,1fr))"
            "gap" ".75rem"
            "margin" "1rem 0"))
        (css/rule ".layout-card"
          (list
            "border" "2px solid #000"
            "background" "#fff"
            "padding" ".85rem"
            "text-align" "left"))
        (css/rule ".layout-card-title"
          (list
            "margin" "0 0 .4rem"
            "font-size" "1.05rem"))
        (css/rule ".layout-card-body"
          (list
            "font-size" ".95rem"
            "line-height" "1.45"
            "color" "#222"))
        (css/rule ".layout-section"
          (list
            "margin" "1.5rem 0"
            "padding" "0 .25rem"))
        (css/rule ".layout-section-title"
          (list
            "font-size" "1.25rem"
            "border-bottom" "1px solid #808080"
            "padding-bottom" ".25rem"
            "margin" "0 0 .75rem"))
        (css/rule ".layout-section-body"
          (list
            "line-height" "1.55"
            "max-width" "40rem"))
        (css/rule ".layout-page"
          (list
            "max-width" "52rem"
            "margin" "0 auto"
            "padding" "0 .75rem 2rem"))
        (css/rule ".layout-foot"
          (list
            "margin-top" "2rem"
            "padding-top" ".75rem"
            "border-top" "1px solid #808080"
            "font-size" ".85rem"
            "color" "#333"
            "text-align" "center"))
        (css/media "max-width:40rem"
          (list
            (css/rule ".layout-hero"
              (list "padding" "1.5rem .75rem 1.25rem"))
            (css/rule ".layout-hero-title"
              (list "font-size" "1.55rem"))))))))

;; Inject layout CSS once per page body (works with product chrome too).
(define layout/style
  (lambda ()
    (html/tag "style" "" (css/render (layout/theme)))))

(define layout/page
  (lambda (parts)
    (render-html
      (html/join
        (list
          (layout/style)
          (html/div
            (html/attr "class" "layout-page")
            (html/join parts)))))))
