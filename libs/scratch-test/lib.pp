;; scratch-test - debug harness, route removed 2026-10-19 (was claiming /quake)
(package scratch-test

(define st/bar
  (lambda (pairs title unit)
    "<svg/>"))

(define st/page
  (lambda ()
    (html/page (list (html/h1 "scratch debug"))))))
