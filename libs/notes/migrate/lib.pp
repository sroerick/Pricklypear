;; package notes/migrate -- M0 complete + F3/F4 cutover finish
;; Sync: scripts/pp_sync.py pull|push

(define notes/migrate/legacy-all
  (lambda
    ()
    (list/map
      notes/migrate/legacy-one
      (rows (quote note) 2000))))

(define notes/migrate/legacy-force-one
  (lambda
    (row)
    (if
      (null? row)
      "skip-empty"
      (let
        ((id (as-str (car row))) (f (cdr row)))
        (let
          ((slug (as-str (dict-get f "slug")))
            (title (as-str (dict-get f "title")))
            (outline (outline/decode (dict-get f "outline"))))
          (let
            ((existing (tree/get-uuid id)))
            (if
              (null? existing)
              (notes/migrate/legacy-one row)
              (let
                ((path (as-str (dict-get existing "path"))))
                (seq
                  (notes/tree/save-outline path outline)
                  (roam/reindex slug outline)
                  (string-append "reoutlined:" slug))))))))))

(define notes/migrate/legacy-force-all
  (lambda
    ()
    (list/map
      notes/migrate/legacy-force-one
      (rows (quote note) 2000))))

(define notes/migrate/legacy-delete-all
  (lambda
    ()
    (list/map
      (lambda
        (row)
        (let
          ((id (as-str (car row)))
            (slug (as-str (dict-get (cdr row) "slug"))))
          (if
            (null? (tree/get-uuid id))
            (string-append "kept-missing-tree:" slug)
            (seq
              (delete (quote note) id)
              (string-append "deleted:" slug)))))
      (rows (quote note) 2000))))

(define notes/migrate/legacy-one
  (lambda
    (row)
    (if
      (null? row)
      "skip-empty"
      (let
        ((id (as-str (car row))) (f (cdr row)))
        (let
          ((slug (as-str (dict-get f "slug")))
            (title (as-str (dict-get f "title")))
            (tags (as-str (dict-get f "tags")))
            (kind (as-str (dict-get f "kind")))
            (day (as-str (dict-get f "day")))
            (folder (as-str (dict-get f "folder")))
            (outline (outline/decode (dict-get f "outline"))))
          (let
            ((existing (tree/get-uuid id))
              (by-slug (notes/tree/find-slug slug)))
            (if
              (not (null? existing))
              (string-append "exists-uuid:" slug)
              (if
                (not (null? by-slug))
                (string-append "exists-slug:" slug)
                (let
                  ((path (notes/tree/note-path folder slug))
                    (body
                      (dict-set*
                        "{}"
                        (list
                          "uuid" id
                          "slug" slug
                          "title" title
                          "tags" tags
                          "kind" (if (string-eq kind "") "note" kind)
                          "day" day))))
                  (seq
                    (notes/tree/ensure-folder (notes/tree/root) (whoami))
                    (tree/upsert path "note" "note" body)
                    (notes/tree/save-outline path outline)
                    (roam/reindex slug outline)
                    (string-append "migrated:" slug)))))))))))
