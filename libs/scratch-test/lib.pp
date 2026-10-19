;; scratch-test round 3 - replicate quake shapes
(package scratch-test

(define st/data
  (quote
    (("1975" 3344)
     ("1976" 3631))))

(define st/range
  (lambda
    (lo hi)
    (if
      (>= lo hi)
      ()
      (cons lo (st/range (+ lo 1)))))
  "Integers lo..hi-1.")

(define st/page
  (lambda ()
    (list
      (html/h1 "T")
      (html/p*
        (html/attr "class" "muted")
        "body text")))
  "Page fn.")

(route "/scratch-test" "st/page"))
