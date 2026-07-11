;; Standard Lisp helpers (package list/, html/, data/)
;; Loaded by (load-library "std") at image boot if missing.

(define list/map
  (lambda (f xs)
    (if (null? xs)
        (list)
        (cons (f (car xs)) (list/map f (cdr xs))))))

(define list/filter
  (lambda (p xs)
    (if (null? xs)
        (list)
        (if (p (car xs))
            (cons (car xs) (list/filter p (cdr xs)))
            (list/filter p (cdr xs))))))

(define list/foldl
  (lambda (f acc xs)
    (if (null? xs)
        acc
        (list/foldl f (f acc (car xs)) (cdr xs)))))

(define html/wrap
  (lambda (title body)
    (render-html
      (string-append
        "<article><h1>" (str title) "</h1><div>" (str body) "</div></article>"))))

(define data/get-field
  (lambda (type-name id field)
    (dict-get (get type-name id) field)))
