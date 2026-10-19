;; scratch-test round 2 - docstring-after-lambda define shape
(package scratch-test

(define st/data
  (quote
    (("a" 1)
     ("b" 2))))

(define st/fn
  (lambda ()
    "ok")
  "Returns ok - docstring after the lambda body.")

(route "/scratch-test" "st/fn"))
