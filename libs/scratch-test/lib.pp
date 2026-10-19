(package scratch-test

(define st/d (quote (("a" 1))))

(define quake/range
  (lambda
    (lo hi)
    (if
      (>= lo hi)
      ()
      (cons lo (quake/range (+ lo 1)))))
)

(define st/page
  (lambda ()
    (html/page (list (html/h1 "T")))))

(route "/scratch-test" "st/page"))
