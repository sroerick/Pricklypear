;; package structed/list -- exported from live branch system/main
;; Sync: scripts/pp_sync.py pull|push

(define structed/list/drop
  (lambda
    (xs n)
    "Return xs minus its first n elements."
    (if
      (or (null? xs) (<= n 0))
      xs
      (structed/list/drop (cdr xs) (- n 1)))))

(define structed/list/index-of
  (lambda
    (xs x eq?)
    "Index of first element equal to x, or -1."
    (structed/list/index-of-acc xs x eq? 0)))

(define structed/list/index-of-acc
  (lambda
    (xs x eq? i)
    "Accumulator for index-of starting the scan at i."
    (if
      (null? xs)
      -1
      (if
        (eq? (car xs) x)
        i
        (structed/list/index-of-acc (cdr xs) x eq? (+ i 1))))))

(define structed/list/insert-at
  (lambda
    (xs i v)
    "Insert v into xs at index i (clamped to front)."
    (if
      (or (null? xs) (<= i 0))
      (cons v xs)
      (cons
        (car xs)
        (structed/list/insert-at (cdr xs) (- i 1) v)))))

(define structed/list/insert-list-at
  (lambda
    (xs i ys)
    "Insert every element of ys into xs starting at index i."
    (if
      (null? ys)
      xs
      (structed/list/insert-list-at
        (structed/list/insert-at xs i (car ys))
        (+ i 1)
        (cdr ys)))))

(define structed/list/nth
  (lambda
    (xs i)
    "Element at index i of xs, or () when out of range."
    (if
      (null? xs)
      ()
      (if
        (<= i 0)
        (car xs)
        (structed/list/nth (cdr xs) (- i 1))))))

(define structed/list/remove-at
  (lambda
    (xs i)
    "xs with the element at index i removed."
    (if
      (null? xs)
      (list)
      (if
        (<= i 0)
        (cdr xs)
        (cons
          (car xs)
          (structed/list/remove-at (cdr xs) (- i 1)))))))

(define structed/list/replace-at
  (lambda
    (xs i v)
    "xs with the element at index i replaced by v."
    (if
      (null? xs)
      (list)
      (if
        (<= i 0)
        (cons v (cdr xs))
        (cons
          (car xs)
          (structed/list/replace-at (cdr xs) (- i 1) v))))))

(define structed/list/split-at
  (lambda
    (xs i)
    "Split xs into (taken dropped) at index i."
    (list
      (structed/list/take xs i)
      (structed/list/drop xs i))))

(define structed/list/take
  (lambda
    (xs n)
    "First n elements of xs."
    (if
      (or (null? xs) (<= n 0))
      (list)
      (cons (car xs) (structed/list/take (cdr xs) (- n 1))))))

