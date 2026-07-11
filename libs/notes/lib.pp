;; Outline-first notes (org/roam-shaped). Depends on outline/ + html/*.
;; Install: (load-library "outline") then (load-library "notes")
;;           or (load-library "notes") after outline is on the branch.
;;
;; Storage: data_rows type note — outline field is JSON list tree.
;; Routes: /notes  /notes?slug=  /partials/notes-outline

(deftype 'note 'title 'slug 'outline 'tags 'kind 'day 'body)

(define notes/empty-outline
  (lambda (title)
    (outline/node "root" title)))

(define notes/row
  (lambda (slug)
    (let ((rs (find-rows 'note "slug" (as-str slug) 1)))
      (if (null? rs) () (car rs)))))

(define notes/fields
  (lambda (row)
    (if (null? row) () (cdr row))))

(define notes/id
  (lambda (row)
    (if (null? row) () (car row))))

(define notes/get
  (lambda (slug)
    (let ((row (notes/row slug)))
      (if (null? row)
          ()
          (let ((f (notes/fields row)))
            (list
              (notes/id row)
              (dict-get f "slug")
              (dict-get f "title")
              (outline/decode (dict-get f "outline"))
              (dict-get f "tags")
              (dict-get f "kind")
              (dict-get f "day")))))))

(define notes/fields-json
  (lambda (slug title outline tags kind day body)
    (dict-set
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
        "day" (as-str day))
      "body" (as-str body))))

(define notes/upsert
  (lambda (slug title outline tags kind day)
    (let ((row (notes/row slug))
          (fields (notes/fields-json slug title outline tags kind day "")))
      (if (null? row)
          (create 'note fields)
          (update 'note (notes/id row) fields)))))

(define notes/list
  (lambda ()
    (rows 'note 100)))

(define notes/search
  (lambda (q)
    (find-text 'note "title" (as-str q) 40)))

(define notes/daily-slug
  (lambda (day)
    (string-append "daily-" (as-str day))))

;; day as string YYYY-MM-DD from caller; v1 no date prim — pass explicitly
;; or use notes/daily-today via request later
(define notes/daily
  (lambda (day)
    (let ((slug (notes/daily-slug day)))
      (let ((existing (notes/get slug)))
        (if (null? existing)
            (seq
              (notes/upsert slug
                (string-append "Daily " (as-str day))
                (notes/empty-outline (string-append "Daily " (as-str day)))
                "daily" "daily" (as-str day))
              (notes/get slug))
            existing)))))

(define notes/capture
  (lambda (title body)
    (let ((slug
            (string-append "note-"
              (as-str (count (notes/list)))
              "-"
              (as-str (string-length (as-str title))))))
      (let ((root
              (outline/set-body
                (outline/node "root" title)
                body)))
        (seq
          (notes/upsert slug title root "inbox" "note" "")
          slug)))))

(define notes/index-html
  (lambda ()
    (let ((items (notes/list)))
      (html/join
        (list
          (html/h1 "Notes")
          (html/p* (html/attr "class" "muted")
            "Outline-first notes (org-shaped). Create below or open a slug.")
          (html/form "POST" "/notes" (html/attr "class" "card")
            (html/join
              (list
                (html/hidden "action" "create")
                (html/label "" "Title ")
                (html/text-input "title"
                  (html/attrs
                    (list
                      (html/attr "required" "required")
                      (html/attr "placeholder" "New note"))))
                " "
                (html/label "" "Slug ")
                (html/text-input "slug"
                  (html/attr "placeholder" "my-note"))
                " "
                (html/button (html/attr "type" "submit") "Create"))))
          (html/form "GET" "/notes" (html/attr "class" "card")
            (html/join
              (list
                (html/label "" "Open slug ")
                (html/text-input "slug" (html/attr "placeholder" "daily-…"))
                " "
                (html/button (html/attr "type" "submit") "Go"))))
          (html/h2 "Recent")
          (html/ul (html/attr "class" "card")
            (list/foldl
              (lambda (acc row)
                (let ((f (cdr row)))
                  (string-append acc
                    (html/li ""
                      (html/a
                        (string-append "/notes?slug="
                          (as-str (dict-get f "slug")))
                        (html/text
                          (as-str (dict-get f "title"))))))))
              ""
              items)))))))

(define notes/show-html
  (lambda (slug)
    (let ((n (notes/get slug)))
      (if (null? n)
          (html/join
            (list
              (html/h1 "Missing")
              (html/p* (html/attr "class" "err")
                (string-append "No note " (as-str slug)))
              (html/p* "" (html/a "/notes" "All notes"))))
          (let ((title (car (cdr (cdr n))))
                (outline (car (cdr (cdr (cdr n)))))
                (tags (car (cdr (cdr (cdr (cdr n)))))))
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
                      (html/h2 "Edit outline (sexp list)")
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
                              (html/button (html/attr "type" "submit")
                                "Save outline"))))))))))))))

(define notes/handle-post
  (lambda ()
    (let ((action (request-param "action")))
      (if (string-eq action "create")
          (let ((title (request-param "title"))
                (slug (request-param "slug")))
            (let ((s
                    (if (or (null? slug) (string-eq (as-str slug) ""))
                        (string-append "note-"
                          (as-str (count (notes/list))))
                        (as-str slug))))
              (seq
                (notes/upsert s (as-str title)
                  (notes/empty-outline (as-str title))
                  "" "note" "")
                (redirect (string-append "/notes?slug=" s)))))
          (if (string-eq action "save")
              (let ((slug (request-param "slug"))
                    (raw (request-param "outline")))
                (let ((tree (outline/decode raw))
                      (prev (notes/get slug)))
                  (let ((title
                          (if (null? prev)
                              (as-str slug)
                              (car (cdr (cdr prev)))))
                        (tags
                          (if (null? prev) ""
                              (car (cdr (cdr (cdr (cdr prev)))))))
                        (kind
                          (if (null? prev) "note"
                              (car (cdr (cdr (cdr (cdr (cdr prev))))))))
                        (day
                          (if (null? prev) ""
                              (car (cdr (cdr (cdr (cdr (cdr (cdr prev))))))))))
                    (seq
                      (notes/upsert (as-str slug) title tree tags kind day)
                      (redirect
                        (string-append "/notes?slug=" (as-str slug)))))))
              (redirect "/notes"))))))

(define notes/page
  (lambda ()
    (if (string-eq (request-method) "POST")
        (notes/handle-post)
        (let ((slug (request-param "slug")))
          (if (or (null? slug) (string-eq (as-str slug) ""))
              (html/page (list (notes/index-html)))
              (html/page (list (notes/show-html (as-str slug)))))))))

(define notes/outline-partial
  (lambda ()
    (let ((slug (request-param "slug")))
      (let ((n (notes/get slug)))
        (if (null? n)
            (render-html (html/p* (html/attr "class" "err") "missing"))
            (render-html
              (outline/html (car (cdr (cdr (cdr n)))))))))))

(route "/notes" "notes/page")
(route "/partials/notes-outline" "notes/outline-partial")
