;; package list -- exported from live branch system/main
;; Sync: scripts/pp_sync.py pull|push

(define list/filter
  (lambda
    (p xs)
    "Return the elements of xs for which p is true."
    (if
      (null? xs)
      (list)
      (if
        (p (car xs))
        (cons (car xs) (list/filter p (cdr xs)))
        (list/filter p (cdr xs))))))

(define list/foldl
  (lambda
    (f acc xs)
    "Left-fold f over xs with initial accumulator acc."
    (if
      (null? xs)
      acc
      (list/foldl f (f acc (car xs)) (cdr xs)))))

(define list/map
  (lambda
    (f xs)
    "Apply f to each element of xs, returning a new list."
    (if
      (null? xs)
      (list)
      (cons (f (car xs)) (list/map f (cdr xs))))))

(define list/range
  (lambda (n) "Build the list (0 1 ... n-1) of length n." (list/range-acc n (list))))

(define list/range-acc
  (lambda
    (n acc)
    "Accumulator helper for list/range."
    (if
      (<= n 0)
      acc
      (list/range-acc (- n 1) (cons (- n 1) acc)))))

(define list/take
  (lambda (xs n) "Return the first n elements of xs." (list/take-acc xs n (list))))

(define list/take-acc
  (lambda
    (xs n acc)
    "Accumulator helper for list/take."
    (if
      (or (null? xs) (<= n 0))
      (reverse acc)
      (list/take-acc (cdr xs) (- n 1) (cons (car xs) acc)))))

