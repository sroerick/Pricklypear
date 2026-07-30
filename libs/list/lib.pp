;; package list -- exported from live branch system/main
;; Sync: scripts/pp_sync.py pull|push

(define list/filter
  (lambda
    (p xs)
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
    (if
      (null? xs)
      acc
      (list/foldl f (f acc (car xs)) (cdr xs)))))

(define list/map
  (lambda
    (f xs)
    (if
      (null? xs)
      (list)
      (cons (f (car xs)) (list/map f (cdr xs))))))

(define list/range
  (lambda (n) (list/range-acc n (list))))

(define list/range-acc
  (lambda
    (n acc)
    (if
      (<= n 0)
      acc
      (list/range-acc (- n 1) (cons (- n 1) acc)))))

(define list/take
  (lambda (xs n) (list/take-acc xs n (list))))

(define list/take-acc
  (lambda
    (xs n acc)
    (if
      (or (null? xs) (<= n 0))
      (reverse acc)
      (list/take-acc (cdr xs) (- n 1) (cons (car xs) acc)))))

