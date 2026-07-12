;; Basecamp-shaped lists + org scrape. Unified /todos.
;; Depends: std, outline (states), notes (for from-notes scrape).
;; Install: (load-library "todo") — seed also loads outline+notes.

(deftype 'todo-list 'title 'description 'is_default)
(deftype 'todo-item 'list_id 'title 'notes 'state 'due 'position 'done)

;; --- states (same vocabulary as outline) ---

(define todo/states outline/todo-states)

(define todo/next-state
  (lambda (cur) (outline/todo-next cur)))

(define todo/open?
  (lambda (s) (outline/todo-open? s)))

(define todo/item-state
  (lambda (fields)
    (let ((s (dict-get fields "state")))
      (if (null? s)
          (let ((d (dict-get fields "done")))
            (if (null? d)
                "TODO"
                (if (string-eq d "true") "DONE" "TODO")))
          (as-str s)))))

(define todo/item-title
  (lambda (fields)
    (as-str (dict-get fields "title"))))

(define todo/item-list-id
  (lambda (fields)
    (let ((lid (dict-get fields "list_id")))
      (if (null? lid) "" (as-str lid)))))

;; --- lists ---

(define todo/lists
  (lambda () (rows 'todo-list 50)))

(define todo/list-title
  (lambda (fields) (as-str (dict-get fields "title"))))

(define todo/list-default?
  (lambda (fields)
    (let ((d (dict-get fields "is_default")))
      (if (null? d) #f (string-eq d "true")))))

(define todo/find-default-list
  (lambda ()
    (todo/find-default-in (todo/lists))))

(define todo/find-default-in
  (lambda (xs)
    (if (null? xs)
        ()
        (if (todo/list-default? (cdr (car xs)))
            (car xs)
            (let ((t (todo/list-title (cdr (car xs)))))
              (if (string-eq t "Inbox")
                  (car xs)
                  (todo/find-default-in (cdr xs))))))))

(define todo/ensure-inbox
  (lambda ()
    (let ((found (todo/find-default-list)))
      (if (null? found)
          (create 'todo-list
            (dict-set
              (dict-set
                (dict-set "{}" "title" "Inbox")
                "description" "Default list")
              "is_default" "true"))
          (car found)))))

(define todo/create-list
  (lambda (title)
    (create 'todo-list
      (dict-set
        (dict-set
          (dict-set "{}" "title" (as-str title))
          "description" "")
        "is_default" "false"))))

(define todo/get-list
  (lambda (id)
    (get 'todo-list id)))

;; --- items ---

(define todo/all-items
  (lambda () (rows 'todo-item 200)))

(define todo/items-for-list
  (lambda (list-id)
    (let ((lid (as-str list-id))
          (inbox-id (todo/ensure-inbox)))
      (list/filter
        (lambda (row)
          (let ((fl (todo/item-list-id (cdr row))))
            (if (string-eq fl lid)
                #t
                (if (string-eq fl "")
                    (string-eq lid inbox-id)
                    #f))))
        (todo/all-items)))))

(define todo/add
  (lambda (list-id title)
    (let ((lid (if (string-eq (as-str list-id) "")
                   (todo/ensure-inbox)
                   (as-str list-id))))
      (create 'todo-item
        (dict-set
          (dict-set
            (dict-set
              (dict-set
                (dict-set
                  (dict-set "{}" "list_id" lid)
                  "title" (as-str title))
                "notes" "")
              "state" "TODO")
            "due" "")
          "position" "0")))))

(define todo/set-state
  (lambda (id state)
    (update-field 'todo-item id "state" (as-str state))))

(define todo/cycle
  (lambda (id)
    (let ((row (get 'todo-item id)))
      (if (null? row)
          ()
          (todo/set-state id (todo/next-state (todo/item-state row)))))))

(define todo/delete
  (lambda (id)
    (delete 'todo-item id)))

(define todo/count-open-in
  (lambda (items)
    (list/foldl
      (lambda (acc row)
        (if (todo/open? (todo/item-state (cdr row)))
            (+ acc 1)
            acc))
      0
      items)))

(define todo/count-all-in
  (lambda (items)
    (list/foldl (lambda (acc _) (+ acc 1)) 0 items)))

(define todo/ratio
  (lambda (items)
    (let ((n (todo/count-all-in items))
          (open (todo/count-open-in items)))
      (string-append (as-str (- n open)) "/" (as-str n)))))

;; --- view key: everything | from-notes | <list-uuid> ---

(define todo/view
  (lambda ()
    (let ((v (request-param "view")))
      (if (null? v) "everything" (as-str v)))))

;; --- HTML ---

(define todo/nav-link
  (lambda (view key label extra)
    (html/li
      (html/attr "class"
        (if (string-eq view key) "todo-nav-item is-active" "todo-nav-item"))
      (html/a
        (string-append "/todos?view=" key)
        (string-append label
          (if (string-eq extra "") "" (string-append " " extra)))))))

(define todo/nav-html
  (lambda (view)
    (let ((lists (todo/lists))
          (inbox (todo/ensure-inbox)))
      (html/ul (html/attr "class" "todo-nav")
        (html/join
          (list
            (todo/nav-link view "everything" "Everything" "")
            (todo/nav-link view "from-notes" "From notes" "")
            (list/foldl
              (lambda (acc row)
                (let ((id (car row))
                      (f (cdr row))
                      (items (todo/items-for-list (car row))))
                  (string-append acc
                    (todo/nav-link view id
                      (todo/list-title f)
                      (string-append "(" (todo/ratio items) ")")))))
              ""
              lists)
            (html/li (html/attr "class" "todo-nav-item")
              (html/form "POST" "/todos"
                (html/attrs
                  (list
                    (html/attr "class" "todo-new-list")
                    (html/attr "hx-post" "/todos")
                    (html/attr "hx-target" "#todo-main")
                    (html/attr "hx-swap" "innerHTML")))
                (html/join
                  (list
                    (html/hidden "action" "create-list")
                    (html/hidden "view" view)
                    (html/text-input "title"
                      (html/attrs
                        (list
                          (html/attr "placeholder" "New list")
                          (html/attr "required" "required")
                          (html/attr "class" "todo-new-list-input"))))
                    (html/button (html/attr "type" "submit") "+ list")))))))))))

(define todo/list-item-li
  (lambda (row view)
    (let ((id (car row))
          (fields (cdr row)))
      (let ((title (todo/item-title fields))
            (state (todo/item-state fields))
            (open (todo/open? (todo/item-state fields))))
        (html/li
          (html/attr "class"
            (if open "todo-item" "todo-item is-done"))
          (html/join
            (list
              (html/form "POST" "/todos"
                (html/attrs
                  (list
                    (html/attr "class" "todo-check")
                    (html/attr "hx-post" "/todos")
                    (html/attr "hx-target" "#todo-main")
                    (html/attr "hx-swap" "innerHTML")))
                (html/join
                  (list
                    (html/hidden "action" "cycle")
                    (html/hidden "id" id)
                    (html/hidden "view" view)
                    (html/button
                      (html/attrs
                        (list
                          (html/attr "type" "submit")
                          (html/attr "class"
                            (string-append "todo-box st-" state))
                          (html/attr "title" "cycle state")))
                      (if (string-eq state "") "·" state)))))
              (html/span (html/attr "class" "todo-title")
                (html/text title))
              (html/form "POST" "/todos"
                (html/attrs
                  (list
                    (html/attr "class" "todo-del")
                    (html/attr "hx-post" "/todos")
                    (html/attr "hx-target" "#todo-main")
                    (html/attr "hx-swap" "innerHTML")))
                (html/join
                  (list
                    (html/hidden "action" "delete")
                    (html/hidden "id" id)
                    (html/hidden "view" view)
                    (html/button
                      (html/attrs
                        (list
                          (html/attr "type" "submit")
                          (html/attr "class" "secondary")
                          (html/attr "title" "delete")))
                      "del")))))))))))

(define todo/note-item-li
  (lambda (t view)
    ;; t = (slug nid title state)
    (let ((slug (car t))
          (nid (car (cdr t)))
          (title (car (cdr (cdr t))))
          (state (car (cdr (cdr (cdr t))))))
      (let ((open (todo/open? state)))
        (html/li
          (html/attr "class"
            (if open "todo-item todo-from-note" "todo-item todo-from-note is-done"))
          (html/join
            (list
              (html/form "POST" "/todos"
                (html/attrs
                  (list
                    (html/attr "class" "todo-check")
                    (html/attr "hx-post" "/todos")
                    (html/attr "hx-target" "#todo-main")
                    (html/attr "hx-swap" "innerHTML")))
                (html/join
                  (list
                    (html/hidden "action" "cycle-note")
                    (html/hidden "slug" slug)
                    (html/hidden "node-id" nid)
                    (html/hidden "view" view)
                    (html/button
                      (html/attrs
                        (list
                          (html/attr "type" "submit")
                          (html/attr "class"
                            (string-append "todo-box st-" state))
                          (html/attr "title" "cycle note TODO")))
                      (if (string-eq state "") "·" state)))))
              (html/span (html/attr "class" "todo-title")
                (html/text title))
              (html/a
                (string-append "/notes?slug=" slug)
                (html/span (html/attr "class" "muted todo-src")
                  (html/text slug))))))))))

(define todo/add-form
  (lambda (view list-id)
    (if (string-eq view "from-notes")
        (html/p* (html/attr "class" "muted")
          "Mark nodes TODO in notes — they appear here. Toggle writes back.")
        (html/form "POST" "/todos"
          (html/attrs
            (list
              (html/attr "class" "card todo-add")
              (html/attr "hx-post" "/todos")
              (html/attr "hx-target" "#todo-main")
              (html/attr "hx-swap" "innerHTML")
              (html/attr "hx-on::after-request"
                "if(event.detail.successful){this.reset()}")))
          (html/join
            (list
              (html/hidden "action" "add")
              (html/hidden "view" view)
              (html/hidden "list_id" list-id)
              (html/label
                (html/attrs
                  (list
                    (html/attr "class" "todo-add-label")
                    (html/attr "for" "title")))
                "New item")
              (html/div (html/attr "class" "todo-add-row")
                (html/join
                  (list
                    (html/text-input "title"
                      (html/attrs
                        (list
                          (html/attr "id" "title")
                          (html/attr "required" "required")
                          (html/attr "placeholder" "buy milk")
                          (html/attr "autocomplete" "off"))))
                    (html/button (html/attr "type" "submit") "Add"))))))))))

(define todo/items-html
  (lambda (view)
    (if (string-eq view "from-notes")
        (html/ul (html/attr "class" "card todo-list")
          (list/foldl
            (lambda (acc t)
              (string-append acc (todo/note-item-li t view)))
            ""
            (notes/all-todos)))
        (if (string-eq view "everything")
            (html/join
              (list
                (html/h2 "Lists")
                (html/ul (html/attr "class" "card todo-list")
                  (list/foldl
                    (lambda (acc row)
                      (string-append acc (todo/list-item-li row view)))
                    ""
                    (todo/all-items)))
                (html/h2 "From notes")
                (html/ul (html/attr "class" "card todo-list")
                  (list/foldl
                    (lambda (acc t)
                      (if (todo/open? (car (cdr (cdr (cdr t)))))
                          (string-append acc (todo/note-item-li t view))
                          acc))
                    ""
                    (notes/all-todos)))))
            ;; real list id
            (html/ul (html/attr "class" "card todo-list")
              (list/foldl
                (lambda (acc row)
                  (string-append acc (todo/list-item-li row view)))
                ""
                (todo/items-for-list view)))))))

(define todo/main-html
  (lambda (view)
    (let ((list-id
            (if (string-eq view "everything")
                (todo/ensure-inbox)
                (if (string-eq view "from-notes")
                    ""
                    view))))
      (html/div (html/attr "id" "todo-main")
        (html/join
          (list
            (html/div (html/attr "class" "todo-layout")
              (html/join
                (list
                  (html/div (html/attr "class" "todo-side")
                    (todo/nav-html view))
                  (html/div (html/attr "class" "todo-body")
                    (html/join
                      (list
                        (todo/add-form view list-id)
                        (todo/items-html view)))))))))))))

(define todo/handle-post
  (lambda ()
    (let ((action (as-str (request-param "action"))))
      (if (string-eq action "cycle")
          (todo/cycle (request-param "id"))
          (if (string-eq action "delete")
              (todo/delete (request-param "id"))
              (if (string-eq action "add")
                  (todo/add (request-param "list_id") (request-param "title"))
                  (if (string-eq action "create-list")
                      (todo/create-list (request-param "title"))
                      (if (string-eq action "cycle-note")
                          (notes/cycle-node-todo
                            (request-param "slug")
                            (request-param "node-id"))
                          (todo/add (todo/ensure-inbox)
                            (request-param "title"))))))))))

(define todo/page
  (lambda ()
    (seq
      (todo/ensure-inbox)
      (if (string-eq (request-method) "POST")
          (seq
            (todo/handle-post)
            (let ((view
                    (let ((v (request-param "view")))
                      (if (null? v) "everything" (as-str v)))))
              (if (hx-request?)
                  (render-html (todo/main-html view))
                  (redirect (string-append "/todos?view=" view)))))
          (html/page
            (list
              (html/h1 "To-dos")
              (html/p* (html/attr "class" "muted")
                "Lists (Basecamp) + notes scrape (org). States: TODO · DONE · WAITING · CANCELLED.")
              (todo/main-html (todo/view))))))))

(define todo/partial
  (lambda ()
    (seq
      (todo/ensure-inbox)
      (render-html (todo/main-html (todo/view))))))

(route "/todos" "todo/page")
(route "/partials/todo-list" "todo/partial")
