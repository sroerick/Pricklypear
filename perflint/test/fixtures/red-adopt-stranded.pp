;; RED fixture: the todo/adopt-stranded shape that motivated R1.
;; A tree/get sits in the inner foldl lambda (per row of a 4000-row list),
;; with tree/move behind it. tree/list-kind is the COLLECTION, not the loop.

(define todo/admitted-types
  (list "todo-list" "todo-item"))

(define todo/adopt-stranded
  (lambda
    ()
    (with-transaction
      (lambda
        ()
        (let
          ((scan
            (list/foldl
              (lambda
                (acc t)
                (list/foldl
                  (lambda
                    (a n)
                    (if
                      (string-eq (as-str (dict-get n "type")) t)
                      (let
                        ((old (as-str (dict-get n "path"))))
                        (let
                          ((id (as-str (dict-get n "id"))))
                          (let
                            ((new (todo/space-path t id)))
                            (let
                              ((has-new (tree/get new)))
                              (if
                                (null? has-new)
                                (seq
                                  (tree/move old new)
                                  (cons (+ (car a) 1) (cdr a)))
                                (cons (car a) (+ (cdr a) 1)))))))
                      a))
                  acc
                  (tree/list-kind "data.u" "datum" 4000)))
              (cons 0 0)
              todo/admitted-types)))
          (json-emit
            (dict-set*
              "{}"
              (list
                "moved"
                (car scan)
                "skipped"
                (cdr scan))))))))
  "One-time adoption of stranded todo nodes.")
