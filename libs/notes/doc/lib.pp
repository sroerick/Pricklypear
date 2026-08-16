;; package notes/doc -- exported from live branch admin/personal
;; Sync: scripts/pp_sync.py pull|push

(define notes/doc/daily
  (lambda
    (day)
    "Fetch the daily note for day; create it if absent, returning its row."
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
  (lambda (n) "The day field of a doc row n." (car (cdr (cdr (cdr (cdr (cdr (cdr n)))))))))

(define notes/doc/delete
  (lambda
    (slug)
    "Delete the note with slug (roam links and tree path), returning slug."
    (let
      ((row (notes/doc/row (as-str slug))))
      (if
        (null? row)
        ()
        (seq
          (roam/delete-from-slug (as-str slug))
          (let
            ((p (notes/doc/row-path row)))
            (if
              (and (notes/tree/enabled?) (not (string-eq p "")))
              (tree/delete p "recursive")
              (delete (quote note) (car row))))
          slug)))))

(define notes/doc/empty-outline
  (lambda (title) "Build a root outline node with title." (outline/node "root" title)))

(define notes/doc/ensure-inbox
  (lambda
    ()
    "Ensure the inbox note exists, creating it if absent and returning it."
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
    "The folder field of a doc row n; empty when absent."
    (if
      (null? n)
      ""
      (let
        ((f (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr n))))))))))
        (if (null? f) "" (as-str f))))))

;; Display folder for the index: strip import garden/ prefix; kebab labels.
(define notes/doc/display-folder
  (lambda
    (folder)
    "Display folder for the index: strip a garden/ prefix and kebab labels."
    (let
      ((f (as-str folder)))
      (let
        ((stripped
          (if
            (string-starts-with? f "garden/")
            (string-replace f "garden/" "")
            (if (string-eq f "garden") "" f))))
        (string-replace stripped "_" "-")))))

(define notes/doc/folder-names
  (lambda
    ()
    "Distinct folder names across all visible notes in index order."
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
    "True if f is already in the string list xs."
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
    "Fetch a doc row for slug as (id slug title outline tags kind day folder); () when absent."
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
    "True if row should be hidden (inbox/hidden kind or hidden tag)."
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
    "All non-hidden notes."
    (list/filter
      (lambda (row) (not (notes/doc/hidden? row)))
      (notes/doc/list))))

(define notes/doc/kind
  (lambda (n) "The kind field of a doc row n." (car (cdr (cdr (cdr (cdr (cdr n))))))))

(define notes/doc/list
  (lambda
    ()
    ;; index-read-path I4 / F1: tree only — no data_rows fallback
    "List all notes from the tree when enabled, else ()."
    (if (notes/tree/enabled?) (notes/tree/list-notes) (list))))

(define notes/doc/list-folder
  (lambda
    (folder)
    "All visible notes in the given folder."
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
    "Build the fields JSON string for a note row."
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
  (lambda (n) "The decoded outline of a doc row n." (car (cdr (cdr (cdr n))))))

(define notes/doc/row
  (lambda
    (slug)
    "Fetch the raw note row for slug from tree or data_rows; () when absent."
    (let
      ((tr
        (if
          (notes/tree/enabled?)
          (notes/tree/find-slug slug)
          ())))
      (if
        (null? tr)
        (let
          ((rs (find-rows (quote note) "slug" (as-str slug) 1)))
          (if (null? rs) () (car rs)))
        tr))))

(define notes/doc/row-folder
  (lambda
    (row)
    "The folder field of a raw row; empty when absent."
    (if
      (null? row)
      ""
      (let
        ((f (dict-get (cdr row) "folder")))
        (if (null? f) "" (as-str f))))))

(define notes/doc/row-path
  (lambda
    (row)
    "The tree path of a raw row; empty when absent."
    (if
      (null? row)
      ""
      (let
        ((p (dict-get (cdr row) "_path")))
        (if (null? p) "" (as-str p))))))

;; Postgres-backed garden search (notes.search-modal / BORGE-1785605474).
;; Returns list of hit JSON strings — never enumerates the garden in Nopales.
(define notes/doc/search
  (lambda
    (q)
    "Full-text search over notes; returns a list of hit JSON strings."
    (let
      ((needle (string-trim (as-str q))))
      (if
        (string-eq needle "")
        (list)
        (list/map
          (lambda (j) (if (null? j) "{}" (as-str j)))
          (tree/search-text needle 20))))))

(define notes/doc/set-folder
  (lambda
    (slug folder)
    "Move the note with slug into folder (tree move or data_rows update)."
    (let
      ((row (notes/doc/row (as-str slug))))
      (if
        (null? row)
        ()
        (if
          (notes/tree/enabled?)
          (let
            ((old (notes/doc/row-path row))
              (newp (notes/tree/note-path folder (as-str slug))))
            (if
              (string-eq old "")
              (notes/doc/upsert-in
                slug
                (dict-get (cdr row) "title")
                (outline/decode (dict-get (cdr row) "outline"))
                (dict-get (cdr row) "tags")
                (dict-get (cdr row) "kind")
                (dict-get (cdr row) "day")
                folder)
              (if (string-eq old newp) old (tree/move old newp))))
          (update-field
            (quote note)
            (car row)
            "folder"
            (as-str folder)))))))

(define notes/doc/show-archived?
  (lambda
    ()
    "True when the request asks to show archived notes."
    (string-eq (as-str (request-param "show")) "archived")))

(define notes/doc/tags
  (lambda (n) "The tags field of a doc row n." (car (cdr (cdr (cdr (cdr n)))))))

(define notes/doc/title
  (lambda (n) "The title field of a doc row n." (car (cdr (cdr n)))))

(define notes/doc/upsert
  (lambda
    (slug title outline tags kind day)
    "Create or update the note with slug, preserving its folder, returning slug."
    (let
      ((row (notes/doc/row slug)))
      (notes/doc/upsert-in
        slug
        title
        outline
        tags
        kind
        day
        (if (null? row) "" (notes/doc/row-folder row))))))

(define notes/doc/upsert-in
  (lambda
    (slug
      title
      outline
      tags
      kind
      day
      folder)
    "Write the note with slug into the given folder, returning slug."
    (if
      (notes/tree/enabled?)
      (let
        ((prev (notes/doc/row slug)))
        (let
          ((uuid (if (null? prev) (random-uuid) (car prev)))
            (oldp (if (null? prev) "" (notes/doc/row-path prev))))
          (let
            ((path
                (if
                  (string-eq oldp "")
                  (notes/tree/note-path folder slug)
                  oldp))
              (body
                (dict-set*
                  "{}"
                  (list
                    "uuid"
                    uuid
                    "slug"
                    (as-str slug)
                    "title"
                    (as-str title)
                    "tags"
                    (as-str tags)
                    "kind"
                    (as-str kind)
                    "day"
                    (as-str day)))))
            (seq
              (notes/tree/ensure-folder (notes/tree/root) (whoami))
              (tree/upsert path "note" "note" body)
              (notes/tree/save-outline path outline)
              (if
                (and
                  (not (null? prev))
                  (not (string-eq oldp ""))
                  (not
                    (string-eq (as-str folder) (notes/doc/row-folder prev))))
                (notes/doc/set-folder slug folder)
                0)
              (roam/reindex (as-str slug) outline)
              slug))))
      (let
        ((row (notes/doc/row slug))
          (fields
            (notes/doc/make-fields
              slug
              title
              outline
              tags
              kind
              day
              folder)))
        (seq
          (if
            (null? row)
            (create (quote note) fields)
            (update (quote note) (car row) fields))
          (roam/reindex (as-str slug) outline)
          slug)))))

