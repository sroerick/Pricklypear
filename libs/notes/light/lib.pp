;; package notes/light -- exported from live branch system/main
;; Sync: scripts/pp_sync.py pull|push

(define notes/light/add-prefixes
  (lambda
    (parts prefix acc)
    (if
      (null? parts)
      acc
      (let*
        ((seg (as-str (car parts)))
          (full
            (if
              (string-eq prefix "")
              seg
              (string-append prefix "/" seg))))
        (notes/light/add-prefixes
          (cdr parts)
          full
          (notes/light/cons-unique full acc))))))

(define notes/light/all-folder-paths
  (lambda
    (all-notes)
    (notes/light/dedupe
      (notes/tree/sort-by-name
        (list/foldl
          (lambda
            (acc note)
            (let
              ((f (as-str (notes/light/nth note 1))))
              (if
                (string-eq f "")
                acc
                (notes/light/add-prefixes (string-split f "/") "" acc))))
          (list)
          all-notes)))))

(define notes/light/children-of
  (lambda
    (parent paths)
    (list/filter
      (lambda
        (p)
        (string-eq (notes/light/parent-of p) parent))
      paths)))

(define notes/light/cons-unique
  (lambda
    (x xs)
    (if (notes/light/contains? x xs) xs (cons x xs))))

(define notes/light/contains?
  (lambda
    (x xs)
    (if
      (null? xs)
      #f
      (if
        (string-eq x (as-str (car xs)))
        #t
        (notes/light/contains? x (cdr xs))))))

(define notes/light/dedupe
  (lambda
    (xs)
    (if
      (null? xs)
      (list)
      (notes/light/cons-unique
        (car xs)
        (notes/light/dedupe (cdr xs))))))

(define notes/light/folder-section
  (lambda
    (folder)
    (let*
      ((all-notes (notes/list-light))
        (items (notes/light/notes-in folder all-notes))
        (label
          (if (string-eq folder "") "(uncategorized)" folder))
        (href (string-append "/notes?folder=" folder)))
      (html/div
        (html/attr "class" "notes-folder card")
        (html/join
          (list
            (html/h3 (html/a href (html/text label)))
            (if
              (null? items)
              (html/p*
                (html/attr "class" "muted")
                "No notes in this folder.")
              (notes/light/render-notes items))))))))

(define notes/light/last
  (lambda
    (xs)
    (if
      (null? (cdr xs))
      (car xs)
      (notes/light/last (cdr xs)))))

(define notes/light/last-segment
  (lambda
    (path)
    (if
      (string-eq path "")
      ""
      (notes/light/last (string-split path "/")))))

(define notes/light/note-li
  (lambda
    (note)
    (let*
      ((title (as-str (notes/light/nth note 2)))
        (slug (as-str (notes/light/nth note 3))))
      (html/li
        (html/attr "class" "note-item")
        (html/a
          (string-append "/notes?slug=" slug)
          (html/text title))))))

(define notes/light/notes-in
  (lambda
    (folder all-notes)
    (list/filter
      (lambda
        (n)
        (string-eq (as-str (notes/light/nth n 1)) folder))
      all-notes)))

(define notes/light/nth
  (lambda
    (xs i)
    (if
      (<= i 0)
      (car xs)
      (notes/light/nth (cdr xs) (- i 1)))))

(define notes/light/page
  (lambda
    ()
    (let
      ((slug (request-param "slug")))
      (if
        (if (null? slug) #t (string-eq (as-str slug) ""))
        (let
          ((folder (request-param "folder")))
          (html/page
            (list
              (notes/tree/style)
              (html/h1 "Notes")
              (html/h2 "Folders")
              (if
                (if
                  (null? folder)
                  #f
                  (not (string-eq (as-str folder) "")))
                (notes/light/folder-section (as-str folder))
                (notes/light/render)))))
        (html/page (list (notes/ui/show (as-str slug))))))))

(define notes/light/parent-of
  (lambda
    (path)
    (if
      (string-eq path "")
      ""
      (let
        ((parts (string-split path "/")))
        (if
          (<= (length parts) 1)
          ""
          (notes/tree/join-slash (notes/tree/butlast parts)))))))

(define notes/light/render
  (lambda
    ()
    (let*
      ((all-notes (notes/list-light))
        (paths (notes/light/all-folder-paths all-notes))
        (roots
          (notes/tree/sort-by-name
            (notes/light/children-of "" paths)))
        (uncat (notes/light/notes-in "" all-notes)))
      (html/div
        (html/attr "class" "notes-tree")
        (html/join
          (list
            (if
              (null? uncat)
              ""
              (html/tag
                "details"
                (html/attrs
                  (list
                    (html/attr "class" "tree-folder tree-uncategorized")
                    (html/attr "open" "open")))
                (html/join
                  (list
                    (html/tag
                      "summary"
                      (html/attr "class" "tree-folder-header")
                      (html/join
                        (list
                          (html/span
                            (html/attr "class" "tree-folder-icon")
                            "&#x1F4C2; ")
                          (html/text "(uncategorized)")
                          (html/span
                            (html/attr "class" "muted")
                            (html/text
                              (string-append " (" (as-str (length uncat)) ")"))))))
                    (notes/light/render-notes uncat)))))
            (html/join
              (list/map
                (lambda
                  (root)
                  (notes/light/render-folder root paths all-notes))
                roots))))))))

(define notes/light/render-folder
  (lambda
    (path paths all-notes)
    (let*
      ((label (notes/light/last-segment path))
        (children (notes/light/children-of path paths))
        (items (notes/light/notes-in path all-notes))
        (item-count (length items)))
      (html/tag
        "details"
        (html/attrs
          (list
            (html/attr "class" "tree-folder")
            (html/attr "open" "open")))
        (html/join
          (list
            (html/tag
              "summary"
              (html/attr "class" "tree-folder-header")
              (html/join
                (list
                  (html/span
                    (html/attr "class" "tree-folder-icon")
                    "&#x1F4C1; ")
                  (html/a
                    (string-append "/notes?folder=" path)
                    (html/text label))
                  (html/span
                    (html/attr "class" "muted")
                    (html/text (string-append " (" (as-str item-count) ")"))))))
            (notes/light/render-notes items)
            (if
              (null? children)
              ""
              (html/join
                (list/map
                  (lambda
                    (child)
                    (notes/light/render-folder child paths all-notes))
                  (notes/tree/sort-by-name children))))))))))

(define notes/light/render-notes
  (lambda
    (note-list)
    (if
      (null? note-list)
      ""
      (html/ul
        (html/attr "class" "tree-items")
        (html/join
          (list/map
            (lambda (n) (notes/light/note-li n))
            note-list))))))
