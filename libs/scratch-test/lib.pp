(package scratch-test

(define st/d (quote (("a" 1))))

(define quake/page
  (lambda ()
    (html/page
      (list
        (html/h1 "T")
        (html/p* (html/attr "class" "muted") "body")))))

(define st/bar
  (lambda (pairs title unit)
    "<svg/>"))

(route "/scratch-test" "quake/page"))
