;; Effect inference fixtures. The classes are the change-set gate vocabulary
;; (pure | db | tx | egress | unknown); de_effects keeps the finer names.
(define (eff-pure x) (+ x 1))

(define (eff-read) (get "note" "1"))

;; transitive: a pure-looking wrapper that calls a DB reader is db.
(define (eff-via) (eff-read))

;; two writes, no transaction: the advisory hint is on, the class is db.
(define (eff-write)
  (seq
    (create (quote note) "{}")
    (update (quote note) "1" "{}")))

;; the same writes inside with-transaction: tx joins db.
(define (eff-txn)
  (with-transaction
    (lambda ()
      (seq
        (create (quote note) "{}")
        (update (quote note) "1" "{}")))))
