;; package todo/tree -- exported from live branch system/main
;; Sync: scripts/pp_sync.py pull|push

(define todo/tree/create
  (lambda
    (t fields)
    (let
      ((r (tree/upsert (node-path t "new") "datum" t fields)))
      (if
        (todo/tree/path-ok? r)
        (todo/tree/redash (todo/tree/tail r))
        r))))

(define todo/tree/del
  (lambda
    (t id)
    (seq (tree/delete (node-path t (as-str id))) id)))

(define todo/tree/get
  (lambda
    (t id)
    (let
      ((n (tree/get (node-path t (as-str id)))))
      (if (null? n) (list) (dict-get n "body")))))

(define todo/tree/insert-by-created
  (lambda
    (nj sorted)
    (if
      (null? sorted)
      (list nj)
      (if
        (string<?
          (as-str (dict-get nj "created_at"))
          (as-str (dict-get (car sorted) "created_at")))
        (cons nj sorted)
        (cons
          (car sorted)
          (todo/tree/insert-by-created nj (cdr sorted)))))))

(define todo/tree/node->pair
  (lambda
    (nj)
    (cons
      (todo/tree/redash (todo/tree/tail (dict-get nj "path")))
      (dict-get nj "body"))))

(define todo/tree/path-ok?
  (lambda
    (r)
    (if
      (null? r)
      #f
      (string-eq (substring (as-str r) 0 5) "data."))))

(define todo/tree/prefix
  (lambda
    (t)
    (let
      ((p (node-path t "")))
      (substring p 0 (- (string-length p) 1)))))

(define todo/tree/redash
  (lambda
    (h)
    (string-append
      (substring h 0 8)
      "-"
      (substring h 8 4)
      "-"
      (substring h 12 4)
      "-"
      (substring h 16 4)
      "-"
      (substring h 20 12))))

(define todo/tree/rows
  (lambda
    (t n)
    (list/map
      todo/tree/node->pair
      (tree/list (todo/tree/prefix t) 1 n))))

(define todo/tree/save
  (lambda
    (t id fields)
    (seq
      (tree/upsert (node-path t (as-str id)) "datum" t fields)
      id)))

(define todo/tree/set-field
  (lambda
    (t id f v)
    (let
      ((cur (todo/tree/get t id)))
      (if
        (null? cur)
        (list)
        (todo/tree/save
          t
          id
          (dict-set* cur (list f (as-str v))))))))

(define todo/tree/sort-by-created
  (lambda
    (nodes)
    (list/foldl
      (lambda (acc n) (todo/tree/insert-by-created n acc))
      (list)
      nodes)))

(define todo/tree/tail
  (lambda (p) (substring p (- (string-length p) 32) 32)))
