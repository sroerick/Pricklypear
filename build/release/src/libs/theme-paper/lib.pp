;; package theme-paper — warm cream paper, dark ink (image.themes)
;; Loaded by (theme/set "paper") / (load-library "theme-paper").
;; Token-only: pins theme/* onto the current branch.

(define theme/name
  "paper")

(define theme/bg
  "#f4f0e6")

(define theme/ink
  "#1c1916")

(define theme/panel
  "#ebe4d4")

(define theme/chrome
  "#ddd4c0")

(define theme/link
  "#0b57a8")

(define theme/visited
  "#5b3d8a")

(define theme/muted
  "#6b6458")

(define theme/err
  "#a32020")

(define theme/ok
  "#1f6b3a")

(define theme/border
  "#c4b8a0")

(define theme/row-alt
  "#efe9db")

(define theme/shadow
  "#c9c0ae")

;; --- semantic style tokens (light paper values) ---------------------
(define theme/sel-bg "#fff8dc")
(define theme/sel-accent "#000080")
(define theme/pending-bg "#fff8dc")
(define theme/pending-ink "#664400")
(define theme/pending-border "#886600")
(define theme/done-bg "#cfc")
(define theme/done-ink "#226622")
(define theme/done-muted "#666666")
(define theme/waiting-bg "#e0e8ff")
(define theme/archived-bg "#eeeeee")
(define theme/archived-ink "#555555")
(define theme/archived-border "#666666")
(define theme/cancelled-bg "#dddddd")
(define theme/today-bg "#ffffe0")
(define theme/banner-bg "#fff3cd")
(define theme/banner-border "#8b0000")
(define theme/banner-ink "#1a1a1a")
(define theme/badge-bg "#ffe8a3")
(define theme/badge-border "#8b6914")

;; --- font stacks (paper: sans body, mono code) -------------------
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
