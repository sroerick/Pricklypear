;; package bi/metric -- exported from live branch system/main
;; Sync: scripts/pp_sync.py pull|push

(define bi/metric/expense-by-category
  (lambda
    ()
    (list/foldl
      (lambda
        (acc row)
        (let
          ((cat (as-str (dict-get (cdr row) "category")))
            (amt (bi/row-amount row)))
          (let
            ((prev (dict-get acc cat)))
            (dict-set
              acc
              cat
              (as-str
                (if (null? prev) amt (+ (bi/parse-amount prev) amt)))))))
      "{}"
      (rows (quote bi-expense) 500))))

(define bi/metric/expense-total
  (lambda
    ()
    (bi/sum-amounts (rows (quote bi-expense) 500))))

(define bi/metric/profit
  (lambda
    ()
    (- (bi/metric/revenue-total) (bi/metric/expense-total))))

(define bi/metric/profit-margin
  (lambda
    ()
    (let
      ((rev (bi/metric/revenue-total)))
      (if (= rev 0.0) 0.0 (div (bi/metric/profit) rev)))))

(define bi/metric/revenue-total
  (lambda
    ()
    (bi/sum-amounts (rows (quote bi-revenue) 500))))

