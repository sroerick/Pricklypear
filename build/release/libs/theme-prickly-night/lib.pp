;; package theme-prickly-night — desert night: blue sky, brown earth (image.themes)
;; Loaded by (theme/set "prickly-night") / (load-library "theme-prickly-night").
;; Token-only: pins theme/* onto the current branch.
;; Signature move: blue-slate page/chrome over warm brown panels —
;; night sky above, desert floor below. NOT black: gruvbox-warm-dark
;; midtones, cream ink, subtle borders by design (~1.5:1). Chips are
;; darker/hue-shifted washes on the brown panel (pending 1.20:1, today
;; 1.28:1). Family identity shared with theme-prickly: yellow pending,
;; green done, blue waiting, magenta accent/selection.

(define theme/name
  "prickly-night")

(define theme/bg
  "#272b33")

(define theme/ink
  "#e9e4d5")

(define theme/panel
  "#332c24")

(define theme/chrome
  "#303740")

(define theme/link
  "#82aacc")

(define theme/visited
  "#a592d4")

(define theme/muted
  "#a8a292")

(define theme/err
  "#e57676")

(define theme/ok
  "#8faf7a")

(define theme/border
  "#4d463e")

(define theme/row-alt
  "#2c3038")

(define theme/shadow
  "#14161c")

;; --- semantic style tokens (desert night values) --------------------
;; Selection is a dark magenta wash with a fruit-pink accent.
(define theme/sel-bg "#4a2f42")
(define theme/sel-accent "#e59ac2")
(define theme/pending-bg "#3f3a1e")
(define theme/pending-ink "#e3cf8f")
(define theme/pending-border "#8a7c3a")
(define theme/done-bg "#31402a")
(define theme/done-ink "#a9c894")
(define theme/done-muted "#9db08a")
(define theme/waiting-bg "#2e3d50")
(define theme/archived-bg "#38322b")
(define theme/archived-ink "#b3ab99")
(define theme/archived-border "#5a5346")
(define theme/cancelled-bg "#35302a")
(define theme/today-bg "#453e1c")
(define theme/banner-bg "#4a3320")
(define theme/banner-border "#b97c3e")
(define theme/banner-ink "#f0e4d0")
(define theme/badge-bg "#4a2f42")
(define theme/badge-border "#c27ba0")

;; --- font stacks (prickly-night: sans body, mono code) ------------
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
