;; scratch-test round 9 - parens inside strings
(package scratch-test

(define st/fn
  (lambda ()
    (string-append "a(b)c" " log10(x) test"))

(route "/scratch-test" "st/fn"))
