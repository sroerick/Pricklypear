;; Outline-first notes. Depends on outline/ + html/*.
;; (load-library "outline") then (load-library "notes")

(deftype 'note 'title 'slug 'outline 'tags 'kind 'day 'body)

(define notes/get-title (lambda (n) (car (cdr (cdr n)))))
(define notes/get-outline (lambda (n) (car (cdr (cdr (cdr n))))))
(define notes/get-tags (lambda (n) (car (cdr (cdr (cdr (cdr n)))))))
(define notes/get-kind (lambda (n) (car (cdr (cdr (cdr (cdr (cdr n))))))))
(define notes/get-day (lambda (n) (car (cdr (cdr (cdr (cdr (cdr (cdr n)))))))))

(define notes/empty-outline
  (lambda (title)
    (outline/node "root" title)))

(define notes/row
  (lambda (slug)
    (let ((rs (find-rows 'note "slug" (as-str slug) 1)))
      (if (null? rs) () (car rs)))))

(define notes/get
  (lambda (slug)
    (let ((row (notes/row slug)))
      (if (null? row)
          ()
          (let ((f (cdr row)))
            (list
              (car row)
              (dict-get f "slug")
              (dict-get f "title")
              (outline/decode (dict-get f "outline"))
              (dict-get f "tags")
              (dict-get f "kind")
              (dict-get f "day")))))))

(define notes/make-fields
  (lambda (slug title outline tags kind day)
    (dict-set
      (dict-set
        (dict-set
          (dict-set
            (dict-set
              (dict-set "{}" "slug" (as-str slug))
              "title" (as-str title))
            "outline" (outline/encode outline))
          "tags" (as-str tags))
        "kind" (as-str kind))
      "day" (as-str day))))

(define notes/upsert
  (lambda (slug title outline tags kind day)
    (let ((row (notes/row slug)))
      (let ((fields (notes/make-fields slug title outline tags kind day)))
        (if (null? row)
            (create 'note fields)
            (update 'note (car row) fields)))))

(define notes/list (lambda () (rows 'note 100)))

(define notes/search
  (lambda (q)
    (find-text 'note "title" (as-str q) 40)))

(define notes/daily
  (lambda (day)
    (let ((slug (string-append "daily-" (as-str day))))
      (if (null? (notes/get slug))
          (seq
            (notes/upsert slug
              (string-append "Daily " (as-str day))
              (notes/empty-outline (string-append "Daily " (as-str day)))
              "daily" "daily" (as-str day))
            (notes/get slug))
          (notes/get slug)))))

(define notes/capture
  (lambda (title body)
    (let ((slug
            (string-append "note-"
              (as-str (count (notes/list)))
              "-"
              (as-str (string-length (as-str title))))))
      (seq
        (notes/upsert slug title
          (outline/set-body (outline/node "root" title) body)
          "inbox" "note" "")
        slug)))

;; --- HTML pieces ---

(define notes/item-li
  (lambda (row)
    (let ((f (cdr row)))
      (html/li ""
        (html/a
          (string-append "/notes?slug=" (as-str (dict-get f "slug")))
          (html/text (as-str (dict-get f "title")))))))

(define notes/recent-list
  (lambda ()
    (list/foldl
      (lambda (acc row) (string-append acc (notes/item-li row)))
      ""
      (notes/list))))

(define notes/create-form
  (lambda ()
    (html/form "POST" "/notes" (html/attr "class" "card")
      (html/join
        (list
          (html/hidden "action" "create")
          (html/label "" "Title ")
          (html/text-input "title" (html/attr "placeholder" "New note"))
          " "
          (html/label "" "Slug ")
          (html/text-input "slug" (html/attr "placeholder" "my-note"))
          " "
          (html/button (html/attr "type" "submit") "Create")))))

(define notes/index-html
  (lambda ()
    (html/join
      (list
        (html/h1 "Notes")
        (html/p* (html/attr "class" "muted")
          "Outline-first notes. Create or open a slug.")
        (notes/create-form)
        (html/h2 "Recent")
        (html/ul (html/attr "class" "card") (notes/recent-list)))))

(define notes/missing-html
  (lambda (slug)
    (html/join
      (list
        (html/h1 "Missing")
        (html/p* (html/attr "class" "err")
          (string-append "No note " (as-str slug)))
        (html/p* "" (html/a "/notes" "All notes")))))

(define notes/edit-form
  (lambda (slug outline)
    (html/form "POST" "/notes" ""
      (html/join
        (list
          (html/hidden "action" "save")
          (html/hidden "slug" slug)
          (html/tag "textarea"
            (html/attrs
              (list
                (html/attr "name" "outline")
                (html/attr "rows" "16")))
            (outline/encode outline))
          (html/p* ""
            (html/button (html/attr "type" "submit") "Save outline"))))))

(define notes/show-body
  (lambda (slug title outline tags)
    (html/join
      (list
        (html/p* (html/attr "class" "muted")
          (html/join
            (list
              (html/a "/notes" "Notes")
              " / "
              (html/code (as-str slug)))))
        (html/h1 (html/text title))
        (html/p* (html/attr "class" "muted")
          (string-append "tags: " (as-str tags)))
        (html/div (html/attr "id" "outline")
          (outline/html outline))
        (html/div (html/attr "class" "card")
          (html/join
            (list
              (html/h2 "Edit outline (JSON)")
              (notes/edit-form slug outline)))))))

(define notes/show-html
  (lambda (slug)
    (let ((n (notes/get slug)))
      (if (null? n)
          (notes/missing-html slug)
          (notes/show-body slug
            (notes/get-title n)
            (notes/get-outline n)
            (notes/get-tags n)))))

;; --- POST ---

(define notes/do-create
  (lambda (title slug)
    (let ((s
            (if (or (null? slug) (string-eq (as-str slug) ""))
                (string-append "note-" (as-str (count (notes/list))))
                (as-str slug))))
      (seq
        (notes/upsert s (as-str title)
          (notes/empty-outline (as-str title))
          "" "note" "")
        (redirect (string-append "/notes?slug=" s))))))

(define notes/do-save
  (lambda (slug raw)
    (let ((tree (outline/decode raw))
          (prev (notes/get slug)))
      (let ((title (if (null? prev) (as-str slug) (notes/get-title prev)))
            (tags (if (null? prev) "" (notes/get-tags prev)))
            (kind (if (null? prev) "note" (notes/get-kind prev)))
            (day (if (null? prev) "" (notes/get-day prev))))
        (seq
          (notes/upsert (as-str slug) title tree tags kind day)
          (redirect (string-append "/notes?slug=" (as-str slug)))))))

(define notes/handle-post
  (lambda ()
    (let ((action (request-param "action")))
      (if (string-eq action "create")
          (notes/do-create (request-param "title") (request-param "slug"))
          (if (string-eq action "save")
              (notes/do-save (request-param "slug") (request-param "outline"))
              (redirect "/notes"))))))

;; --- routes ---

(define notes/page
  (lambda ()
    (if (string-eq (request-method) "POST")
        (notes/handle-post)
        (let ((slug (request-param "slug")))
          (if (or (null? slug) (string-eq (as-str slug) ""))
              (html/page (list (notes/index-html)))
              (html/page (list (notes/show-html (as-str slug))))))))

(define notes/outline-partial
  (lambda ()
    (let ((n (notes/get (request-param "slug"))))
      (if (null? n)
          (render-html (html/p* (html/attr "class" "err") "missing"))
          (render-html (outline/html (notes/get-outline n))))))

(route "/notes" "notes/page")
(route "/partials/notes-outline" "notes/outline-partial")
