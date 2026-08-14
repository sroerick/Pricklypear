;; package theme-classic — platinum System 7 gray (image.themes)
;; Loaded by (theme/set "classic") / (load-library "theme-classic").
;; The old product default look people either love or flee from.

(define theme/name
  "classic")

(define theme/bg
  "#d4d0c8")

(define theme/ink
  "#000000")

(define theme/panel
  "#ece9d8")

(define theme/chrome
  "#c0c0c0")

(define theme/link
  "#0000ee")

(define theme/visited
  "#551a8b")

(define theme/muted
  "#555555")

(define theme/err
  "#800000")

(define theme/ok
  "#006600")

(define theme/border
  "#808080")

(define theme/row-alt
  "#e8e4d8")

(define theme/shadow
  "#808080")

;; --- semantic style tokens (platinum light values) -------------------
(define theme/sel-bg "#fff8dc")
(define theme/sel-accent "#000080")
(define theme/pending-bg "#fff3cd")
(define theme/pending-ink "#664400")
(define theme/pending-border "#886600")
(define theme/done-bg "#cce8cc")
(define theme/done-ink "#006600")
(define theme/done-muted "#555555")
(define theme/waiting-bg "#d8e4f0")
(define theme/archived-bg "#e4e0d8")
(define theme/archived-ink "#555555")
(define theme/archived-border "#808080")
(define theme/cancelled-bg "#ddd8d0")
(define theme/today-bg "#ffffe0")
(define theme/banner-bg "#fff3cd")
(define theme/banner-border "#800000")
(define theme/banner-ink "#000000")
(define theme/badge-bg "#ffe8a3")
(define theme/badge-border "#8b6914")

;; --- per-theme structural sheet hook (no-op) ---------------
;; Ensure switching away from theme-as400 clears its CRT chrome.
(define theme/extra
  (lambda
    ()
    (css/sheet (list))))
