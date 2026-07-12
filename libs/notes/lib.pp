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
    (dict-set* "{}"
      (list
        "slug" (as-str slug)
        "title" (as-str title)
        "outline" (outline/encode outline)
        "tags" (as-str tags)
        "kind" (as-str kind)
        "day" (as-str day)))))

(define notes/upsert
  (lambda (slug title outline tags kind day)
    (let ((row (notes/row slug)))
      (let ((fields (notes/make-fields slug title outline tags kind day)))
        (if (null? row)
            (create 'note fields)
            (update 'note (car row) fields))))))
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

;; One durable note doc (slug "inbox"). Captures are child nodes under root.
(define notes/ensure-inbox
  (lambda ()
    (if (null? (notes/get "inbox"))
        (seq
          (notes/upsert "inbox" "Inbox"
            (notes/empty-outline "Inbox")
            "inbox" "inbox" "")
          (notes/get "inbox"))
        (notes/get "inbox"))))

(define notes/capture-title
  (lambda (title body)
    (let ((t (string-trim (as-str title))))
      (if (not (string-eq t ""))
          t
          (let ((b (string-trim (as-str body))))
            (if (string-eq b "")
                "capture"
                (if (<= (string-length b) 48)
                    b
                    (string-append (substring b 0 45) "…"))))))))

(define notes/capture-node-id
  (lambda (tree)
    (string-append "c"
      (as-str (count (outline/kids tree)))
      "-"
      (as-str (string-length (outline/encode tree))))))

;; Append a capture as a child of the Inbox outline root. Returns "inbox".
(define notes/capture
  (lambda (title body)
    (let ((prev (notes/ensure-inbox)))
      (let ((tree (notes/get-outline prev))
            (t (notes/capture-title title body))
            (b (as-str body)))
        (let ((child
                (outline/set-body
                  (outline/node (notes/capture-node-id tree) t)
                  b)))
          (let ((new-tree (outline/add-child-by-id tree "root" child)))
            (seq
              (notes/upsert "inbox" "Inbox" new-tree
                "inbox" "inbox" "")
              "inbox")))))))

;; Compact capture control for the top nav (details dropdown).
(define notes/capture-nav
  (lambda ()
    (html/tag "details" (html/attr "class" "menu-drop capture-drop")
      (html/join
        (list
          (html/tag "summary" (html/attr "class" "menu-drop-btn") "capture")
          (html/div (html/attr "class" "menu-drop-panel capture-panel")
            (html/form "POST" "/notes"
              (html/attrs
                (list
                  (html/attr "class" "capture-nav-form")
                  (html/attr "hx-post" "/notes")
                  (html/attr "hx-target" "#capture-status")
                  (html/attr "hx-swap" "innerHTML")
                  (html/attr "hx-on::after-request"
                    "if(event.detail.successful){this.reset()}")))
              (html/join
                (list
                  (html/hidden "action" "capture")
                  (html/text-input "title"
                    (html/attrs
                      (list
                        (html/attr "placeholder" "title (optional)")
                        (html/attr "autocomplete" "off")
                        (html/attr "class" "capture-nav-title"))))
                  (html/textarea "body"
                    (html/attrs
                      (list
                        (html/attr "rows" "3")
                        (html/attr "class" "capture-body")
                        (html/attr "placeholder" "Quick capture…")
                        (html/attr "required" "required")))
                    "")
                  (html/div (html/attr "class" "capture-row")
                    (html/join
                      (list
                        (html/button (html/attr "type" "submit") "Save")
                        (html/a "/notes?slug=inbox" "inbox"))))
                  (html/div (html/attr "id" "capture-status") ""))))))))))

;; Full-page form (also used if someone hits /capture).
(define notes/capture-form
  (lambda ()
    (html/form "POST" "/notes"
      (html/attrs
        (list
          (html/attr "class" "card capture-card")
          (html/attr "hx-post" "/notes")
          (html/attr "hx-target" "#capture-status")
          (html/attr "hx-swap" "innerHTML")
          (html/attr "hx-on::after-request"
            "if(event.detail.successful){this.reset()}")))
      (html/join
        (list
          (html/hidden "action" "capture")
          (html/h2 "Capture")
          (html/p* (html/attr "class" "muted")
            "Drops a node into the Inbox note.")
          (html/label (html/attr "for" "capture-title") "Title")
          (html/text-input "title"
            (html/attrs
              (list
                (html/attr "id" "capture-title")
                (html/attr "placeholder" "optional")
                (html/attr "autocomplete" "off"))))
          (html/label (html/attr "for" "capture-body") "Note")
          (html/textarea "body"
            (html/attrs
              (list
                (html/attr "id" "capture-body")
                (html/attr "rows" "3")
                (html/attr "class" "capture-body")
                (html/attr "placeholder" "Quick capture…")
                (html/attr "required" "required")))
            "")
          (html/div (html/attr "class" "capture-row")
            (html/join
              (list
                (html/button (html/attr "type" "submit") "Capture")
                (html/a "/notes?slug=inbox" "Open inbox"))))
          (html/div (html/attr "id" "capture-status") ""))))))

(define notes/capture-page
  (lambda ()
    (if (string-eq (request-method) "POST")
        (notes/do-capture)
        (html/page
          (list
            (html/h1 "Capture")
            (notes/capture-form))))))

(define notes/capture-status-html
  (lambda (ok msg)
    (html/p*
      (html/attr "class" (if ok "ok" "err"))
      msg)))
(define notes/delete
  (lambda (slug)
    (let ((row (notes/row (as-str slug))))
      (if (null? row)
          ()
          (delete 'note (car row))))))

(define notes/item-li
  (lambda (row)
    (let ((f (cdr row))
          (id (car row)))
      (let ((slug (as-str (dict-get f "slug")))
            (title (as-str (dict-get f "title"))))
        (html/li (html/attr "class" "note-item")
          (html/join
            (list
              (html/a
                (string-append "/notes?slug=" slug)
                (html/text title))
              (html/form "POST" "/notes"
                (html/attrs
                  (list
                    (html/attr "class" "note-del")
                    (html/attr "hx-confirm" "Delete this note?")))
                (html/join
                  (list
                    (html/hidden "action" "delete")
                    (html/hidden "slug" slug)
                    (html/button
                      (html/attrs
                        (list
                          (html/attr "type" "submit")
                          (html/attr "class" "secondary")
                          (html/attr "title" "delete note")))
                      "del")))))))))))
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
          (html/button (html/attr "type" "submit") "Create"))))))
(define notes/index-html
  (lambda ()
    (html/join
      (list
        (html/h1 "Notes")
        (html/p* (html/attr "class" "muted")
          "Outline-first notes. Create or open a slug.")
        (notes/create-form)
        (html/h2 "Recent")
        (html/ul (html/attr "class" "card") (notes/recent-list))))))
(define notes/missing-html
  (lambda (slug)
    (html/join
      (list
        (html/h1 "Missing")
        (html/p* (html/attr "class" "err")
          (string-append "No note " (as-str slug)))
        (html/p* "" (html/a "/notes" "All notes"))))))
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
            (html/button (html/attr "type" "submit") "Save outline")))))))
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
        (html/div (html/attr "class" "note-title-row")
          (html/join
            (list
              (html/h1 (html/text title))
              (html/form "POST" "/notes"
                (html/attrs
                  (list
                    (html/attr "class" "note-del note-del-page")
                    (html/attr "hx-confirm" "Delete this entire note?")))
                (html/join
                  (list
                    (html/hidden "action" "delete")
                    (html/hidden "slug" (as-str slug))
                    (html/button
                      (html/attrs
                        (list
                          (html/attr "type" "submit")
                          (html/attr "class" "secondary")
                          (html/attr "title" "delete note")))
                      "Delete note")))))))
        (html/p* (html/attr "class" "muted")
          (string-append "tags: " (as-str tags)))
        (outline/html slug outline)
        (html/tag "details" (html/attr "class" "ol-debug")
          (html/join
            (list
              (html/tag "summary" (html/attr "class" "muted") "Raw JSON (debug)")
              (notes/edit-form slug outline))))
        (html/tag "script" ""
          (string-append
            "document.getElementById('outline').addEventListener('blur',function(e){"
            "var t=e.target;if(!t.dataset||!t.dataset.field)return;"
            "var p='slug=" slug "&op=set-'+t.dataset.field+'&node-id='+t.dataset.id;"
            "p+='&'+t.dataset.field+'='+encodeURIComponent(t.innerText);"
            "fetch('/partials/notes-node-op',{method:'POST',headers:{'Content-Type':'application/x-www-form-urlencoded'},body:p});"
            "},true);"))))))
(define notes/show-html
  (lambda (slug)
    (let ((n (notes/get slug)))
      (if (null? n)
          (notes/missing-html slug)
          (notes/show-body slug
            (notes/get-title n)
            (notes/get-outline n)
            (notes/get-tags n))))))
(define notes/do-create
  (lambda (title slug)
    (let ((s
            (if (if (null? slug) #t (string-eq (string-trim (as-str slug)) ""))
                (string-append "note-" (as-str (count (notes/list))))
                (string-trim (as-str slug)))))
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
          (redirect (string-append "/notes?slug=" (as-str slug))))))))
(define notes/do-capture
  (lambda ()
    (let ((title (request-param "title"))
          (body (request-param "body")))
      (let ((b (if (null? body) "" (as-str body))))
        (if (string-eq (string-trim b) "")
            (if (hx-request?)
                (render-html
                  (notes/capture-status-html #f "Write something first."))
                (redirect "/"))
            (seq
              (notes/capture
                (if (null? title) "" title)
                b)
              (if (hx-request?)
                  (render-html
                    (notes/capture-status-html #t
                      "Saved to Inbox."))
                  (redirect "/notes?slug=inbox"))))))))

(define notes/do-delete
  (lambda ()
    (let ((slug (as-str (request-param "slug"))))
      (seq
        (notes/delete slug)
        (redirect "/notes")))))

(define notes/handle-post
  (lambda ()
    (let ((action (as-str (request-param "action"))))
      (if (string-eq action "create")
          (notes/do-create (request-param "title") (request-param "slug"))
          (if (string-eq action "save")
              (notes/do-save (request-param "slug") (request-param "outline"))
              (if (string-eq action "capture")
                  (notes/do-capture)
                  (if (string-eq action "delete")
                      (notes/do-delete)
                      (redirect "/notes"))))))))

(define notes/page
  (lambda ()
    (if (string-eq (request-method) "POST")
        (notes/handle-post)
        (let ((slug (request-param "slug")))
          (if (if (null? slug) #t (string-eq (as-str slug) ""))
              (html/page (list (notes/index-html)))
              (html/page (list (notes/show-html (as-str slug)))))))))
(define notes/outline-partial
  (lambda ()
    (let ((n (notes/get (request-param "slug"))))
      (if (null? n)
          (render-html (html/p* (html/attr "class" "err") "missing"))
          (render-html (outline/html (as-str (request-param "slug")) (notes/get-outline n)))))))

;; --- node op handler ---

(define notes/apply-op
  (lambda (slug tree op nid)
    (if (string-eq op "add-child")
        (outline/add-child-by-id tree nid (outline/node "n-new" "New node"))
        (if (string-eq op "add-sibling")
            (outline/insert-sibling tree nid (outline/node "n-new" "New node"))
            (if (string-eq op "delete")
                (outline/delete tree nid)
                (if (string-eq op "move-up")
                    (outline/move-up tree nid)
                    (if (string-eq op "move-down")
                        (outline/move-down tree nid)
                        (if (string-eq op "set-title")
                            (outline/update tree nid
                              (lambda (n) (outline/set-title n (request-param "title"))))
                            (if (string-eq op "set-body")
                                (outline/update tree nid
                                  (lambda (n) (outline/set-body n (request-param "body"))))
                                (if (string-eq op "set-todo")
                                    (outline/update tree nid
                                      (lambda (n) (outline/set-todo n (request-param "todo"))))
                                    (if (string-eq op "cycle-todo")
                                        (outline/update tree nid
                                          (lambda (n) (outline/cycle-todo n)))
                                        tree)))))))))))

;; (slug node-id title state)*
(define notes/all-todos
  (lambda ()
    (list/foldl
      (lambda (acc row)
        (let ((f (cdr row)))
          (let ((slug (as-str (dict-get f "slug")))
                (tree (outline/decode (dict-get f "outline"))))
            (list/foldl
              (lambda (a t)
                (cons (list slug (car t) (car (cdr t)) (car (cdr (cdr t)))) a))
              acc
              (outline/todos tree)))))
      (list)
      (notes/list))))

(define notes/open-todos
  (lambda ()
    (list/filter
      (lambda (t) (outline/todo-open? (car (cdr (cdr (cdr t))))))
      (notes/all-todos))))

(define notes/cycle-node-todo
  (lambda (slug nid)
    (let ((prev (notes/get slug)))
      (if (null? prev)
          ()
          (let ((tree (notes/get-outline prev))
                (title (notes/get-title prev))
                (tags (notes/get-tags prev))
                (kind (notes/get-kind prev))
                (day (notes/get-day prev)))
            (let ((new-tree
                    (outline/update tree nid
                      (lambda (n) (outline/cycle-todo n)))))
              (notes/upsert slug title new-tree tags kind day)))))))

(define notes/node-op
  (lambda ()
    (let ((slug (as-str (request-param "slug")))
          (op (as-str (request-param "op")))
          (nid (as-str (request-param "node-id"))))
      (let ((prev (notes/get slug)))
        (if (null? prev)
            (render-html (html/p* (html/attr "class" "err") "missing"))
            (let ((tree (notes/get-outline prev))
                  (title (notes/get-title prev))
                  (tags (notes/get-tags prev))
                  (kind (notes/get-kind prev))
                  (day (notes/get-day prev)))
              (let ((new-tree (notes/apply-op slug tree op nid)))
                (seq
                  (notes/upsert slug title new-tree tags kind day)
                  (render-html (outline/html slug new-tree))))))))))

(route "/notes" "notes/page")
(route "/capture" "notes/capture-page")
(route "/partials/notes-outline" "notes/outline-partial")
(route "/partials/notes-node-op" "notes/node-op")
