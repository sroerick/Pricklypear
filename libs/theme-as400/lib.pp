;; package theme-as400 — AS/400 CRT green-on-black theme (image.themes)
;; Loaded by (theme/set "as400") / (load-library "theme-as400").
;;
;; Token-only theme: green/black palette + monospace font stacks. The
;; CRT feel comes from color + font alone — no structural sheet. The
;; old global CRT clobber (monospace body, uppercase headings, 2px
;; borders) that used to hide in notes/tree-css is gone; structure lives
;; in components now, themes only declare tokens.

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
    theme/font-sans
    theme/font-mono
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

;; --- font stacks (as400: monospace everywhere — the CRT look) -----
(define theme/font-sans
  "\"Courier New\",\"Lucida Console\",Monaco,\"DejaVu Sans Mono\",monospace")
(define theme/font-mono
  "\"Courier New\",\"Lucida Console\",Monaco,\"DejaVu Sans Mono\",monospace")

;; --- per-theme structural sheet hook (no-op) ----------------------
;; Themes are token-only (colors + fonts); no structural sheets. The
;; CRT look is just green-on-black tokens + monospace fonts.
(define theme/extra
  (lambda
    ()
    "No-op: themes are token-only."
    (css/sheet (list))))
