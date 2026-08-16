;; package theme-midnight — soft blue-gray dark (image.themes)
;; Loaded by (theme/set "midnight") / (load-library "theme-midnight").

(define theme/name
  "midnight")

;; WHITEPURPOSE: theme-midnight is THE WHITEOUT/PROBE THEME — every
;; core token is magenta so ANY element still painted by a hardcoded
;; hex (not var(--*)) glows magenta and exposes the leak. Real users
;; pick midnight only to audit CSS theming coverage. It is registered
;; as package "theme" in theme/catalog's midnight entry (fixed:
;; was "theme", so "midnight" switched back to phosphor tokens).

(define theme/bg
  "magenta")

(define theme/ink
  "magenta")

(define theme/panel
  "magenta")

(define theme/chrome
  "magenta")

(define theme/link
  "magenta")

(define theme/visited
  "magenta")

(define theme/muted
  "magenta")

(define theme/err
  "#bf616a")

(define theme/ok
  "#a3be8c")

(define theme/border
  "#3b4558")

(define theme/row-alt
  "#141b26")

(define theme/shadow
  "magenta")

;; --- semantic style tokens (dark blue-gray values) ------------------
(define theme/sel-bg "magenta")
(define theme/sel-accent "magenta")
(define theme/pending-bg "magenta")
(define theme/pending-ink "magenta")
(define theme/pending-border "magenta")
(define theme/done-bg "magenta")
(define theme/done-ink "magenta")
(define theme/done-muted "magenta")
(define theme/waiting-bg "magenta")
(define theme/archived-bg "magenta")
(define theme/archived-ink "magenta")
(define theme/archived-border "magenta")
(define theme/cancelled-bg "magenta")
(define theme/today-bg "magenta")
(define theme/banner-bg "magenta")
(define theme/banner-border "magenta")
(define theme/banner-ink "magenta")
(define theme/badge-bg "magenta")
(define theme/badge-border "magenta")

;; --- per-theme structural sheet hook (no-op) ---------------
;; Ensure switching away from theme-as400 clears its CRT chrome.
(define theme/extra
  (lambda
    ()
    (css/sheet (list))))
