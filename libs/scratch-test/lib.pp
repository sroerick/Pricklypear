;; scratch-test - minimal shapes for the loader's define gate
(package scratch-test

(define st/data
  (quote
    (("a" 1)
     ("b" 2))))

(define st/fn
  (lambda ()
    "ok"))

(route "/scratch-test" "st/fn"))
