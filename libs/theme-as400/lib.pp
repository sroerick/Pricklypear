;; package theme-as400 — AS/400 CRT green-on-black theme (image.themes)
;; Loaded by (theme/set "as400") / (load-library "theme-as400").
;;
;; This is the extracted home of the old "AS/400 terminal-inspired"
;; phosphor skin that used to hide inside notes/tree-css and clobber the
;; ENTIRE UI regardless of the selected theme. It is now a first-class
;; installable theme: active only when the user picks it.
;;
;;  - Tokens: green/black palette (the historic CRT look).
;;  - theme/extra: the AS/400 *structural* sheet — monospace body,
;;    uppercase headings/buttons, chunky 2px green borders — that used
;;    to be the 34 global + 7 patch-global rules in notes/tree-css.
;;    Colors are expressed as var(--*) so they resolve from theme/vars
;;    (this package's own tokens) at render time. Non-color structural
;;    bits (font, text-transform, letter-spacing, border widths) live
;;    here verbatim.
;;
;; ui/css should include (css/use (theme/extra)) AFTER notes/tree-css so
;; the CRT chrome wins only under this theme. Other theme packages
;; provide a no-op theme/extra (see libs/theme/lib.pp default), so the
;; structural sheet is additive per-theme and never cross-clobbers.

(package theme-as400
  (export
    theme/name
    theme/bg
    theme/border
    theme/chrome
    theme/err
    theme/ink
    theme/link
    theme/muted
    theme/ok
    theme/panel
    theme/row-alt
    theme/shadow
    theme/visited
    theme/sel-bg
    theme/sel-accent
    theme/pending-bg
    theme/pending-ink
    theme/pending-border
    theme/done-bg
    theme/done-ink
    theme/done-muted
    theme/waiting-bg
    theme/archived-bg
    theme/archived-ink
    theme/archived-border
    theme/cancelled-bg
    theme/today-bg
    theme/banner-bg
    theme/banner-border
    theme/banner-ink
    theme/badge-bg
    theme/badge-border
    theme/extra))

(define theme/name
  "as400")

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

;; --- semantic style tokens (dark CRT values, matching phosphor) -----
(define theme/sel-bg "#1a3a1a")
(define theme/sel-accent "#2a9a2a")
(define theme/pending-bg "#1f3a1f")
(define theme/pending-ink "#88cc44")
(define theme/pending-border "#886600")
(define theme/done-bg "#0f2a10")
(define theme/done-ink "#55cc55")
(define theme/done-muted "#4a6a4a")
(define theme/waiting-bg "#1a2a3a")
(define theme/archived-bg "#1a1a1a")
(define theme/archived-ink "#777777")
(define theme/archived-border "#555555")
(define theme/cancelled-bg "#222222")
(define theme/today-bg "#333a1a")
(define theme/banner-bg "#3a2a00")
(define theme/banner-border "#886600")
(define theme/banner-ink "#ffdd88")
(define theme/badge-bg "#3a2f00")
(define theme/badge-border "#8b6914")

;; --- AS/400 structural sheet (former notes/tree-css + tree-css-patch) ---
;; Colors resolve through var(--*) → this package's tokens when active.
;; Structural literals (fonts, transforms, spacing, border widths) are
;; kept verbatim to reproduce the CRT look faithfully.
;;
;; Sentinels: parent theme libs/theme defines a no-op theme/extra; ui/css
;; must (css/use (theme/extra)) after notes/tree-css to layer this on.

(define theme/extra
  (lambda
    ()
    (css/sheet
      (list
        (css/raw
          "/* theme-as400: AS/400 terminal-inspired chrome (extracted from notes/tree-css) */")
        (css/rule
          "body"
          (list
            "font-family"
            "\"Courier New\",\"Lucida Console\",Monaco,\"DejaVu Sans Mono\",monospace"
            "font-weight"
            "400"
            "-webkit-font-smoothing"
            "antialiased"))
        (css/rule
          "header"
          (list
            "border-bottom"
            "2px solid var(--border)"
            "padding"
            ".5rem .75rem"))
        (css/rule
          "header .brand"
          (list
            "color"
            "var(--link)"
            "text-transform"
            "uppercase"
            "letter-spacing"
            ".12em"
            "font-weight"
            "700"))
        (css/rule "header .meta" (list "color" "var(--muted)"))
        (css/rule
          "nav"
          (list
            "gap"
            "0"
            "border"
            "2px solid var(--border)"
            "padding"
            ".25rem"))
        (css/rule
          "nav a,.menu-drop-btn"
          (list
            "border"
            "1px solid var(--border)"
            "background"
            "var(--chrome)"
            "color"
            "var(--ink)"
            "font-family"
            "\"Courier New\",\"Lucida Console\",Monaco,monospace"
            "font-size"
            ".85rem"
            "font-weight"
            "700"
            "text-transform"
            "uppercase"
            "letter-spacing"
            ".06em"
            "padding"
            ".4rem .6rem"
            "margin"
            "0"
            "border-radius"
            "0"))
        (css/rule
          "nav a:hover,nav a:focus,.menu-drop-btn:hover"
          (list "background" "var(--ink)" "color" "var(--bg)"))
        (css/rule "main" (list "max-width" "56rem"))
        (css/rule
          "h1"
          (list
            "color"
            "var(--link)"
            "text-transform"
            "uppercase"
            "letter-spacing"
            ".1em"
            "border-bottom"
            "2px solid var(--border)"
            "font-size"
            "1.15rem"
            "font-weight"
            "700"))
        (css/rule
          "h2"
          (list
            "color"
            "var(--ink)"
            "text-transform"
            "uppercase"
            "letter-spacing"
            ".05em"
            "font-size"
            "1rem"
            "font-weight"
            "700"))
        (css/rule
          "h3"
          (list
            "color"
            "var(--ink)"
            "font-size"
            ".95rem"
            "font-weight"
            "700"))
        (css/rule
          ".card"
          (list
            "border"
            "2px solid var(--border)"
            "background"
            "var(--panel)"))
        (css/rule
          "table"
          (list "border" "2px solid var(--border)" "font-size" ".85rem"))
        (css/rule
          "th"
          (list
            "background"
            "var(--chrome)"
            "color"
            "var(--link)"
            "text-transform"
            "uppercase"
            "letter-spacing"
            ".05em"
            "border-color"
            "var(--border)"
            "font-weight"
            "700"))
        (css/rule "td" (list "border-color" "#222222"))
        (css/rule
          "tr:nth-child(even) td"
          (list "background" "var(--row-alt)"))
        (css/rule
          "button,.btn,input[type=submit]"
          (list
            "background"
            "var(--chrome)"
            "color"
            "var(--ink)"
            "border"
            "2px solid var(--border)"
            "font-family"
            "\"Courier New\",\"Lucida Console\",Monaco,monospace"
            "font-size"
            ".85rem"
            "font-weight"
            "700"
            "text-transform"
            "uppercase"
            "letter-spacing"
            ".05em"
            "border-radius"
            "0"
            "padding"
            ".45rem .85rem"))
        (css/rule
          "button:hover,.btn:hover"
          (list "background" "var(--ink)" "color" "var(--bg)"))
        (css/rule
          "button:active,.btn:active"
          (list "border-style" "solid"))
        (css/rule
          "button.secondary,.btn.secondary"
          (list "background" "var(--panel)"))
        (css/rule
          "pre,textarea,input[type=text],input[type=search],input:not([type])"
          (list
            "background"
            "#0d0d0d"
            "color"
            "var(--ink)"
            "border"
            "1px solid var(--border)"
            "font-family"
            "\"Courier New\",\"Lucida Console\",Monaco,monospace"
            "font-weight"
            "400"))
        (css/rule
          "pre:focus,textarea:focus,input[type=text]:focus,input[type=search]:focus,input:not([type]):focus"
          (list "border-color" "var(--link)" "outline" "none"))
        (css/rule "a" (list "color" "var(--link)"))
        (css/rule "a:visited" (list "color" "var(--visited)"))
        (css/rule "a:hover" (list "color" "#66ff66"))
        (css/rule ".meta,.muted" (list "color" "var(--muted)"))
        (css/rule ".err" (list "color" "var(--err)"))
        (css/rule ".ok" (list "color" "var(--ok)"))
        (css/rule
          "li.note-item"
          (list
            "border-bottom"
            "1px solid #222222"
            "padding"
            ".35rem 0"))
        (css/rule "li.note-item > a" (list "color" "var(--ink)"))
        (css/rule
          "li.note-item > a:hover"
          (list "color" "#66ff66"))
        (css/rule
          "li.note-item .muted"
          (list "color" "var(--muted)"))
        (css/rule
          "li.note-item .note-del button"
          (list
            "border"
            "1px solid var(--border)"
            "background"
            "var(--panel)"
            "color"
            "var(--muted)"
            "padding"
            ".2rem .4rem"
            "font-size"
            ".8rem"
            "min-height"
            "auto"
            "width"
            "auto"))
        (css/rule
          "li.note-item .note-del button:hover"
          (list "background" "var(--ink)" "color" "var(--bg)"))
        ;; --- v4 patch globals (from notes/tree-css-patch) ---
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
        (css/rule
          ".menu-drop>summary::after"
          (list "display" "inline-block"))
        (css/rule "header .meta" (list "color" "var(--muted)"))
        (css/rule ".meta,.muted" (list "color" "var(--muted)"))
        (css/rule
          "li.note-item .muted"
          (list "color" "var(--muted)"))
        (css/rule
          "li.note-item .note-del button, .notes-tree li.note-item form.note-del button"
          (list
            "color"
            "var(--ink)"
            "border"
            "1px solid var(--border)"
            "background"
            "var(--panel)"))
        (css/rule
          "li.note-item .note-del button:hover, .notes-tree li.note-item form.note-del button:hover"
          (list "background" "var(--ink)" "color" "var(--bg)"))))))
