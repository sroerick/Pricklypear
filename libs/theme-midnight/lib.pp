;; package theme-midnight — soft blue-gray dark (image.themes)
;; Loaded by (theme/set "midnight") / (load-library "theme-midnight").

(define theme/name
  "midnight")

(define theme/bg
  "#0f1419")

(define theme/ink
  "#d8dee9")

(define theme/panel
  "#1a2332")

(define theme/chrome
  "#15202b")

(define theme/link
  "#7aa2f7")

(define theme/visited
  "#b48ead")

(define theme/muted
  "#8b95a8")

(define theme/err
  "#bf616a")

(define theme/ok
  "#a3be8c")

(define theme/border
  "#3b4558")

(define theme/row-alt
  "#141b26")

(define theme/shadow
  "#0a0e13")

;; --- semantic style tokens (dark blue-gray values) ------------------
(define theme/sel-bg "#1a2a44")
(define theme/sel-accent "#7aa2f7")
(define theme/pending-bg "#2a3044")
(define theme/pending-ink "#d8dee9")
(define theme/pending-border "#8b6914")
(define theme/done-bg "#14301c")
(define theme/done-ink "#a3be8c")
(define theme/done-muted "#4a5a4a")
(define theme/waiting-bg "#1a2a3a")
(define theme/archived-bg "#141b26")
(define theme/archived-ink "#8b95a8")
(define theme/archived-border "#3b4558")
(define theme/cancelled-bg "#1f2730")
(define theme/today-bg "#2a3520")
(define theme/banner-bg "#3a2a00")
(define theme/banner-border "#886600")
(define theme/banner-ink "#ffdd88")
(define theme/badge-bg "#3a2f00")
(define theme/badge-border "#8b6914")

;; --- per-theme structural sheet hook (no-op) ---------------
;; Ensure switching away from theme-as400 clears its CRT chrome.
(define theme/extra
  (lambda
    ()
    "Install this theme's CSS sheet (empty for midnight)."
    (css/sheet (list))))
