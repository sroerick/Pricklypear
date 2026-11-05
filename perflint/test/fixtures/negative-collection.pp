;; NEGATIVE: the DB prim IS the collection being folded, so it runs once.
;; notes/issues/cards call (tree/list "data.u" 3 5000) outside their map.

(define notes/cards
  (lambda
    ()
    (list/map
      (lambda
        (n)
        (html/div (as-str (dict-get n "title"))))
      (tree/list "data.u" 3 5000))))

(define notes/count
  (lambda
    ()
    (list/foldl
      (lambda
        (acc n)
        (+ acc 1))
      0
      (tree/list "data.u" 3 5000))))
