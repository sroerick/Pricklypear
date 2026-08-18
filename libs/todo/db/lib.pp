;; package todo/db -- exported from live branch system/main
;; Sync: scripts/pp_sync.py pull|push

(define todo/db/create
  (lambda
    (type fields)
    (let
      ((r
        (tree/upsert
          (node-path type "new")
          "datum"
          (as-str type)
          fields)))
      (if
        (string-starts-with? r "data.")
        (todo/db/path-id r)
        r))))

(define todo/db/delete
  (lambda
    (type id)
    (let
      ((p (node-path type (as-str id))))
      (if
        (null? (tree/get p))
        ()
        (seq (tree/delete p) (as-str id))))))

(define todo/db/get
  (lambda
    (type id)
    (let
      ((n (tree/get (node-path type (as-str id)))))
      (if (null? n) () (dict-get n "body")))))

(define todo/db/insert-by-created
  (lambda
    (n sorted)
    (if
      (null? sorted)
      (list n)
      (if
        (string<?
          (as-str (dict-get n "created_at"))
          (as-str (dict-get (car sorted) "created_at")))
        (cons n sorted)
        (cons
          (car sorted)
          (todo/db/insert-by-created n (cdr sorted)))))))

(define todo/db/node-row
  (lambda
    (node)
    (cons
      (todo/db/path-id (dict-get node "path"))
      (dict-get node "body"))))

(define todo/db/path-id
  (lambda
    (path)
    (let
      ((h (substring path (- (string-length path) 32) 32)))
      (string-append
        (substring h 0 8)
        "-"
        (substring h 8 4)
        "-"
        (substring h 12 4)
        "-"
        (substring h 16 4)
        "-"
        (substring h 20 12)))))

(define todo/db/prefix
  (lambda
    (type)
    (let
      ((p (node-path type "")))
      (substring p 0 (- (string-length p) 1)))))

(define todo/db/rows
  (lambda
    (type limit)
    (map
      todo/db/node-row
      (tree/list-kind (todo/db/prefix type) "datum" limit))))

(define todo/db/sort-by-created
  (lambda
    (ns)
    (if
      (null? ns)
      (list)
      (todo/db/insert-by-created
        (car ns)
        (todo/db/sort-by-created (cdr ns))))))

(define todo/db/update
  (lambda
    (type id fields)
    (let
      ((p (node-path type (as-str id))))
      (if
        (null? (tree/get p))
        ()
        (let
          ((r (tree/upsert p "datum" (as-str type) fields)))
          (if (string-starts-with? r "data.") (as-str id) r))))))

(define todo/db/update-field
  (lambda
    (type id field value)
    (let
      ((fields (todo/db/get type (as-str id))))
      (if
        (null? fields)
        ()
        (todo/db/update type id (dict-set fields field value))))))
