;; AMBER fixture: the prim is in a function the loop lambda calls, not in the
;; loop lambda itself.
;;   (a) a define in the same file
;;   (b) a lambda bound in the same let
;;   (c) a package-local HOF wrapper (my/map) whose lambda calls (a)

(define amber/load
  (lambda (id)
    (find-row "todo-item" "id" id)))

(define amber/peek
  (lambda (id)
    (tree/get (as-str id))))

(define amber/decorate
  (lambda
    (row)
    (html/div (as-str (dict-get row "title")))))

(define amber/cards
  (lambda
    (ids)
    (list/map
      (lambda
        (id)
        (amber/decorate (amber/load id)))
      ids)))

(define amber/local
  (lambda
    (ids)
    (let
      ((title-of
        (lambda
          (id)
          (as-str (dict-get (get "todo-item" id) "title")))))
      (list/map
        (lambda
          (id)
          (title-of id))
        ids))))

(define amber/wrapped
  (lambda
    (ids)
    (my/map
      (lambda
        (id)
        (amber/peek id))
      ids)))

(define amber/clean
  (lambda
    (rows)
    (list/map
      (lambda
        (r)
        (html/div (as-str (dict-get r "title"))))
      rows)))
