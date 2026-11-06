;; NEGATIVE: already-fetched rows, the loop calls no prim.
;; (rows ...) runs once, outside the lambda.

(define kanban/rows
  (lambda
    ()
    (let
      ((rs (rows "card" 100)))
      (list/map
        (lambda
          (r)
          (html/div (as-str (dict-get r "title"))))
        rs))))

(define kanban/names
  (lambda
    (rs)
    (list/filter
      (lambda
        (r)
        (not (string-eq (as-str (dict-get r "title")) "")))
      rs)))
