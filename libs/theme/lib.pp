;; package theme — default tokens + install/switch API (image.themes)
;; Sync: scripts/pp_sync.py pull|push
;;
;; A theme package defines theme/* tokens (+ theme/name). Switching
;; is (theme/set "paper") → (load-library "theme-paper") which pins
;; those defines onto the current branch and refreshes ui/css.

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
    theme/extra))

;; --- default tokens (phosphor) — active on system/main after seed ---

(define theme/name
  "phosphor")

(define theme/bg
  "#111111")

(define theme/border
  "#33cc33")

(define theme/chrome
  "#1a2a1a")

(define theme/err
  "#ff4444")

(define theme/ink
  "#33cc33")

(define theme/link
  "#33ff33")

(define theme/muted
  "#aaaaaa")

(define theme/ok
  "#33ff33")

(define theme/panel
  "#1a1a1a")

(define theme/row-alt
  "#151515")

(define theme/shadow
  "#222222")

(define theme/visited
  "#2a9a2a")

;; --- semantic style tokens (phosphor defaults) ----------------------
;; Phase 3: the structural sheet (ui/theme) used to bake ~77 hardcoded
;; hex literals (selection highlight, todo done/waiting/archived states,
;; emergency banner, calendar-today, impact badge, …) into the string, so
;; they never responded to the active theme. Those literals now resolve
;; through var(--*) emitted here, so every theme overrides them with its
;; own light/dark values. Truly structural colors (#000 borders, #808080
;; shadows) stay as-is in ui/theme (not tokens).
;;
;; A given theme package only needs to redefine the tokens it wants to
;; differ from the phosphor defaults; all are optional per theme.

(define theme/sel-bg
  "#1a3a1a")

(define theme/sel-accent
  "#2a9a2a")

(define theme/pending-bg
  "#1f3a1f")

(define theme/pending-ink
  "#88cc44")

(define theme/pending-border
  "#886600")

(define theme/done-bg
  "#0f2a10")

(define theme/done-ink
  "#55cc55")

(define theme/done-muted
  "#4a6a4a")

(define theme/waiting-bg
  "#1a2a3a")

(define theme/archived-bg
  "#1a1a1a")

(define theme/archived-ink
  "#777777")

(define theme/archived-border
  "#555555")

(define theme/cancelled-bg
  "#222222")

(define theme/today-bg
  "#333a1a")

(define theme/banner-bg
  "#3a2a00")

(define theme/banner-border
  "#886600")

(define theme/banner-ink
  "#ffdd88")

(define theme/badge-bg
  "#3a2f00")

(define theme/badge-border
  "#8b6914")

;; --- CSS custom-property emitter ------------------------------------
;; Emit a single :root rule mapping the theme/* tokens onto CSS custom
;; properties. The structural sheet (ui/theme) reads var(--*) so it is
;; static and theme-independent; switching themes only swaps this :root
;; block (resolved by the browser cascade, no re-generation).

(define theme/vars
  (lambda
    ()
    (css/sheet
      (list
        (css/rule
          ":root"
          (list
            "--bg" theme/bg
            "--ink" theme/ink
            "--panel" theme/panel
            "--chrome" theme/chrome
            "--link" theme/link
            "--visited" theme/visited
            "--muted" theme/muted
            "--err" theme/err
            "--ok" theme/ok
            "--border" theme/border
            "--row-alt" theme/row-alt
            "--shadow" theme/shadow
            "--sel-bg" theme/sel-bg
            "--sel-accent" theme/sel-accent
            "--pending-bg" theme/pending-bg
            "--pending-ink" theme/pending-ink
            "--pending-border" theme/pending-border
            "--done-bg" theme/done-bg
            "--done-ink" theme/done-ink
            "--done-muted" theme/done-muted
            "--waiting-bg" theme/waiting-bg
            "--archived-bg" theme/archived-bg
            "--archived-ink" theme/archived-ink
            "--archived-border" theme/archived-border
            "--cancelled-bg" theme/cancelled-bg
            "--today-bg" theme/today-bg
            "--banner-bg" theme/banner-bg
            "--banner-border" theme/banner-border
            "--banner-ink" theme/banner-ink
            "--badge-bg" theme/badge-bg
            "--badge-border" theme/badge-border))))))

;; --- per-theme structural sheet hook (default: no-op) --------------
;; A theme package MAY define theme/extra — a css/sheet of *structural*
;; (non-color) chrome layered on top of ui/theme (e.g. AS/400's monospace
;; body, uppercase headings, chunky borders). ui/css includes
;; (css/use (theme/extra)) LAST so an active theme can retint/reshape
;; without clobbering the shared structural sheet. The base theme package
;; ships a no-op so most themes add nothing.

(define theme/extra
  (lambda
    ()
    (css/sheet (list))))

;; --- registry -------------------------------------------------------
;; Each entry: (name display description package)
;; package = libs/<package>/lib.pp loaded by theme/set.

(define theme/catalog
  (list
    (list
      "phosphor"
      "Phosphor"
      "CRT green on near-black. The classic habitat look."
      "theme")
    (list
      "paper"
      "Paper"
      "Warm cream paper, dark ink, soft brown chrome. Easy on the eyes."
      "theme-paper")
    (list
      "midnight"
      "Midnight"
      "WHITEOUT PROBE: every token = magenta. Anything NOT magenta is a hardcoded-color leak."
      "theme-midnight")
    (list
      "classic"
      "Classic Mac"
      "Platinum gray System 7 vibes. The old product default."
      "theme-classic")
    (list
      "as400"
      "AS/400"
      "CRT green on near-black — the original notes-tree terminal skin, now its own theme."
      "theme-as400")))

(define theme/entry-name
  (lambda (e) (as-str (car e))))

(define theme/entry-display
  (lambda (e) (as-str (car (cdr e)))))

(define theme/entry-desc
  (lambda (e) (as-str (car (cdr (cdr e))))))

(define theme/entry-package
  (lambda (e) (as-str (car (cdr (cdr (cdr e)))))))

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

(define theme/current
  (lambda
    ()
    (guard
      (lambda (_) "phosphor")
      (as-str theme/name))))

(define theme/set
  (lambda
    (name)
    (let
      ((e (theme/find name)))
      (if
        (null? e)
        (error
          (string-append "theme/set: unknown theme: " (as-str name)))
        (let
          ((pkg (theme/entry-package e))
            (want (theme/entry-name e)))
          (seq
            (load-library pkg)
            ;; load-library refreshes ui/css; return the name for callers.
            want))))))

;; --- picker UI ------------------------------------------------------

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
                (html/attr
                  "style"
                  (string-append "background:" bg))))
            "")
          (html/tag
            "span"
            (html/attrs
              (list
                (html/attr "class" "theme-swatch-chip")
                (html/attr
                  "style"
                  (string-append "background:" ink))))
            "")
          (html/tag
            "span"
            (html/attrs
              (list
                (html/attr "class" "theme-swatch-chip")
                (html/attr
                  "style"
                  (string-append "background:" panel))))
            "")
          (html/tag
            "span"
            (html/attrs
              (list
                (html/attr "class" "theme-swatch-chip")
                (html/attr
                  "style"
                  (string-append "background:" chrome))))
            "")
          (html/tag
            "span"
            (html/attrs
              (list
                (html/attr "class" "theme-swatch-chip")
                (html/attr
                  "style"
                  (string-append "background:" link))))
            ""))))))

(define theme/preview-swatch
  (lambda
    (name)
    ;; Hardcoded previews so the picker doesn't have to load each package.
    (if
      (string-eq name "paper")
      (theme/swatch-html "#f4f0e6" "#1c1916" "#ebe4d4" "#ddd4c0" "#0b57a8")
      (if
        (string-eq name "midnight")
        (theme/swatch-html "magenta" "magenta" "magenta" "magenta" "magenta")
        (if
          (string-eq name "classic")
          (theme/swatch-html "#d4d0c8" "#000000" "#ece9d8" "#c0c0c0" "#0000ee")
          (if
            (string-eq name "as400")
            (theme/swatch-html "#111111" "#33cc33" "#1a1a1a" "#1a2a1a" "#33ff33")
            (theme/swatch-html "#111111" "#33cc33" "#1a1a1a" "#1a2a1a" "#33ff33")))))))

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
                (if is-cur "card theme-card theme-card-current" "card theme-card"))
              (html/attr "data-theme" name)))
          (html/join
            (list
              (html/h3 display)
              (theme/preview-swatch name)
              (html/p*
                (html/attr "class" "muted")
                (html/text desc))
              (if
                is-cur
                (html/p*
                  (html/attr "class" "ok")
                  "Current theme")
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

(route "/settings/theme" "theme/page")
