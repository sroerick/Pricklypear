;; package theme-prickly — desert day: sand & sky, green-olive ink (image.themes)
;; Loaded by (theme/set "prickly") / (load-library "theme-prickly").
;; Token-only: pins theme/* onto the current branch.
;; Family: prickly (light) / prickly-night (dark) — shared semantic hues
;; (yellow pending, green done, blue waiting, magenta accent), Catppuccin-style
;; same-accents-different-base. All ink/muted values checked >= 4.5:1 WCAG AA.

(define theme/name
  "prickly")

(define theme/bg
  "#f6f0e0")

(define theme/ink
  "#2c3126")

(define theme/panel
  "#eee6d1")

(define theme/chrome
  "#e2d8bf")

(define theme/link
  "#3f6fae")

(define theme/visited
  "#a34d7d")

(define theme/muted
  "#6a6553")

(define theme/err
  "#b23b3b")

(define theme/ok
  "#3d7a3a")

(define theme/border
  "#c2b48a")

(define theme/row-alt
  "#f1ead9")

(define theme/shadow
  "#c9b892")

;; --- semantic style tokens (desert day values) ----------------------
;; Selection is magenta — the pricklypear fruit. Visited links too.
(define theme/sel-bg "#f2d6e3")
(define theme/sel-accent "#a34d7d")
(define theme/pending-bg "#f2e2ae")
(define theme/pending-ink "#6b5410")
(define theme/pending-border "#a8892a")
(define theme/done-bg "#d3e5c2")
(define theme/done-ink "#2f6b2f")
(define theme/done-muted "#57634a")
(define theme/waiting-bg "#cfe0f0")
(define theme/archived-bg "#e8e2d2")
(define theme/archived-ink "#6a6553")
(define theme/archived-border "#b9ac8d")
(define theme/cancelled-bg "#e4ddd2")
(define theme/today-bg "#f8e9a8")
(define theme/banner-bg "#f7e9d0")
(define theme/banner-border "#a35b1f")
(define theme/banner-ink "#3c2f1a")
(define theme/badge-bg "#f6dfe9")
(define theme/badge-border "#a34d7d")

;; --- font stacks (prickly: sans body, mono code) ------------------
(define theme/font-sans
  "Geneva,\"Lucida Grande\",\"DejaVu Sans\",Verdana,sans-serif")
(define theme/font-mono
  "\"Courier New\",\"Lucida Console\",Monaco,\"DejaVu Sans Mono\",monospace")

;; --- per-theme structural sheet hook (no-op) ----------------------
;; Themes are token-only (colors + fonts); no structural sheets.
(define theme/extra
  (lambda
    ()
    "No-op: themes are token-only."
    (css/sheet (list))))
