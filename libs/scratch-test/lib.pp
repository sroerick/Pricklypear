(package scratch-test

(define st/d (quote (("a" 1))))

(define st/p1 (lambda () (html/page (list (html/h1 "T")))))

(define st/p2 (lambda () (html/page (list (html/h1 "T2")))))

(define st/p3 (lambda () (html/page (list (html/h1 "T3")))))

(define st/p4 (lambda () (html/page (list (html/h1 "T4")))))

(define st/p5 (lambda () (html/page (list (html/h1 "T5")))))

(define st/p6 (lambda () (html/page (list (html/h1 "T6")))))

(route "/scratch-test" "st/p1"))
