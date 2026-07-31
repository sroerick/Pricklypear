;; package notes/doc -- exported from live branch system/main
;; Sync: scripts/pp_sync.py pull|push

(define notes/doc/daily
  (lambda
    (day)
    (let
      ((slug (string-append "daily-" (as-str day))))
      (if
        (null? (notes/doc/get slug))
        (seq
          (notes/doc/upsert
            slug
            (string-append "Daily " (as-str day))
            (notes/doc/empty-outline
              (string-append "Daily " (as-str day)))
            "daily"
            "daily"
            (as-str day))
          (notes/doc/get slug))
        (notes/doc/get slug)))))

(define notes/doc/day
  (lambda (n) (car (cdr (cdr (cdr (cdr (cdr (cdr n)))))))))

(define notes/doc/delete
  (lambda
    (slug)
    (let
      ((row (notes/doc/row (as-str slug))))
      (if
        (null? row)
        ()
        (seq
          (roam/delete-from-slug (as-str slug))
          (delete (quote note) (car row)))))))

(define notes/doc/empty-outline
  (lambda (title) (outline/node "root" title)))

(define notes/doc/ensure-inbox
  (lambda
    ()
    (if
      (null? (notes/doc/get "inbox"))
      (seq
        (notes/doc/upsert
          "inbox"
          "Inbox"
          (notes/doc/empty-outline "Inbox")
          "inbox"
          "inbox"
          "")
        (notes/doc/get "inbox"))
      (notes/doc/get "inbox"))))

(define notes/doc/folder
  (lambda
    (n)
    (if
      (null? n)
      ""
      (let
        ((f (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr n))))))))))
        (if (null? f) "" (as-str f))))))

(define notes/doc/folder-names
  (lambda
    ()
    (list/foldl
      (lambda
        (acc row)
        (let
          ((f (notes/doc/row-folder row)))
          (if
            (notes/doc/folder-seen? acc f)
            acc
            (append acc (list f)))))
      (list)
      (notes/doc/index-list))))

(define notes/doc/folder-seen?
  (lambda
    (xs f)
    (if
      (null? xs)
      #f
      (if
        (string-eq (car xs) f)
        #t
        (notes/doc/folder-seen? (cdr xs) f)))))

(define notes/doc/get
  (lambda
    (slug)
    (let
      ((row (notes/doc/row slug)))
      (if
        (null? row)
        ()
        (let
          ((f (cdr row)))
          (list
            (car row)
            (dict-get f "slug")
            (dict-get f "title")
            (outline/decode (dict-get f "outline"))
            (dict-get f "tags")
            (dict-get f "kind")
            (dict-get f "day")
            (let
              ((folder (dict-get f "folder")))
              (if (null? folder) "" folder))))))))

(define notes/doc/hidden?
  (lambda
    (row)
    (let
      ((f (cdr row)))
      (let
        ((kind (as-str (dict-get f "kind")))
          (tags (as-str (dict-get f "tags")))
          (slug (as-str (dict-get f "slug"))))
        (if
          (string-eq kind "inbox")
          #t
          (if
            (string-eq kind "hidden")
            #t
            (if
              (string-eq slug "inbox")
              #t
              (string-contains? tags "hidden"))))))))

(define notes/doc/index-list
  (lambda
    ()
    (list/filter
      (lambda (row) (not (notes/doc/hidden? row)))
      (notes/doc/list))))

(define notes/doc/kind
  (lambda (n) (car (cdr (cdr (cdr (cdr (cdr n))))))))

(define notes/doc/list
  (lambda
    ()
    ;; index-read-path I4: tree only — no data_rows fallback
    (if (notes/tree/enabled?) (notes/tree/list-notes) (list))))

(define notes/doc/list-folder
  (lambda
    (folder)
    (let
      ((want (as-str folder)))
      (list/filter
        (lambda
          (row)
          (string-eq (notes/doc/row-folder row) want))
        (notes/doc/index-list)))))

(define notes/doc/make-fields
  (lambda
    (slug
      title
      outline
      tags
      kind
      day
      folder)
    (dict-set*
      "{}"
      (list
        "slug"
        (as-str slug)
        "title"
        (as-str title)
        "outline"
        (outline/encode outline)
        "tags"
        (as-str tags)
        "kind"
        (as-str kind)
        "day"
        (as-str day)
        "folder"
        (as-str folder)))))

(define notes/doc/outline
  (lambda (n) (car (cdr (cdr (cdr n))))))

(define notes/doc/row
  (lambda
    (slug)
    ;; index-read-path I4: tree only — no find-rows fallback
    (if (notes/tree/enabled?) (notes/tree/find-slug slug) ())))

(define notes/doc/row-folder
  (lambda
    (row)
    (if
      (null? row)
      ""
      (let
        ((f (dict-get (cdr row) "folder")))
        (if (null? f) "" (as-str f))))))

(define notes/doc/search
  (lambda
    (q)
    (find-text (quote note) "title" (as-str q) 40)))

(define notes/doc/set-folder
  (lambda
    (slug folder)
    (let
      ((row (notes/doc/row (as-str slug))))
      (if
        (null? row)
        ()
        (update-field
          (quote note)
          (car row)
          "folder"
          (as-str folder))))))

(define notes/doc/show-archived?
  (lambda
    ()
    (string-eq (as-str (request-param "show")) "archived")))

(define notes/doc/tags
  (lambda (n) (car (cdr (cdr (cdr (cdr n)))))))

(define notes/doc/title
  (lambda (n) (car (cdr (cdr n)))))

(define notes/doc/upsert
  (lambda
    (slug title outline tags kind day)
    (let
      ((row (notes/doc/row slug)))
      (let
        ((folder (notes/doc/row-folder row))
          (fields
            (notes/doc/make-fields
              slug
              title
              outline
              tags
              kind
              day
              (notes/doc/row-folder row))))
        (seq
          (if
            (null? row)
            (create (quote note) fields)
            (update (quote note) (car row) fields))
          (roam/reindex (as-str slug) outline))))))

(define notes/doc/upsert-in
  (lambda
    (slug
      title
      outline
      tags
      kind
      day
      folder)
    (seq
      (notes/doc/upsert
        slug
        title
        outline
        tags
        kind
        day)
      (notes/doc/set-folder slug folder)
      slug)))

