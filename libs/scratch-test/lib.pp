(package scratch-test

(define st/bar
  (lambda (pairs title unit)
    "<svg/>")
  "Stub docstring.")

(define st/page
  (lambda ()
    (html/page (list (html/h1 "T"))))

(route "/scratch-test" "st/page"))
)
