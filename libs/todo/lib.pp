;; package todo -- exported from live branch system/main
;; Sync: scripts/pp_sync.py pull|push

(package todo
  (export
    todo/add
    todo/active-items
    todo/list
    todo/page
    todo/archive
    todo/cycle
    todo/delete
    todo/item/add
    todo/item/active
    todo/item/archive
    todo/item/cycle
    todo/item/delete
    todo/list/all
    todo/list/create
    todo/list/delete
    todo/ensure-inbox
))

(define todo/active-items
  (lambda
    (items)
    (list/filter
      (lambda
        (row)
        (not (todo/archived? (todo/item-state (cdr row)))))
      items)))

(define todo/add
  (lambda
    (list-id title)
    (let
      ((lid
        (if
          (string-eq (as-str list-id) "")
          (todo/ensure-inbox)
          (as-str list-id))))
      (create
        (quote todo-item)
        (dict-set*
          "{}"
          (list
            "list_id"
            lid
            "title"
            (as-str title)
            "notes"
            ""
            "state"
            "TODO"
            "due"
            ""
            "position"
            "0"))))))

(define todo/add-form
  (lambda
    (view list-id)
    (if
      (string-eq view "from-notes")
      (html/p*
        (html/attr "class" "muted")
        "Mark nodes TODO in notes — they appear here. Toggle writes back.")
      (if
        (string-eq view "archived")
        (html/p*
          (html/attr "class" "muted")
          "Archived tasks. Unarchive from detail (set state back), or delete.")
        (html/form
          "POST"
          "/todos"
          (html/attrs
            (list
              (html/attr "class" "card todo-add")
              (html/attr "hx-post" "/todos")
              (html/attr "hx-target" "#todo-main")
              (html/attr "hx-swap" "innerHTML")
              (html/attr
                "hx-on::after-request"
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
              (html/div
                (html/attr "class" "todo-add-row")
                (html/join
                  (list
                    (html/text-input
                      "title"
                      (html/attrs
                        (list
                          (html/attr "id" "title")
                          (html/attr "required" "required")
                          (html/attr "placeholder" "buy milk")
                          (html/attr "autocomplete" "off"))))
                    (html/button (html/attr "type" "submit") "Add")))))))))))

(define todo/all-items
  (lambda () (rows (quote todo-item) 200)))

(define todo/archive
  (lambda (id) (todo/set-state id "ARCHIVED")))

(define todo/archived-items
  (lambda
    ()
    (list/filter
      (lambda
        (row)
        (todo/archived? (todo/item-state (cdr row))))
      (todo/all-items))))

(define todo/archived?
  (lambda (s) (string-eq (as-str s) "ARCHIVED")))

(define todo/count-all-in
  (lambda
    (items)
    (list/foldl (lambda (acc _) (+ acc 1)) 0 items)))

(define todo/count-open-in
  (lambda
    (items)
    (list/foldl
      (lambda
        (acc row)
        (if
          (todo/open? (todo/item-state (cdr row)))
          (+ acc 1)
          acc))
      0
      items)))

(define todo/create-list
  (lambda
    (title)
    (create
      (quote todo-list)
      (dict-set*
        "{}"
        (list
          "title"
          (as-str title)
          "description"
          ""
          "is_default"
          "false")))))

(define todo/cycle
  (lambda
    (id)
    (let
      ((row (get (quote todo-item) id)))
      (if
        (null? row)
        ()
        (let
          ((cur (todo/item-state row)))
          (if
            (todo/archived? cur)
            cur
            (todo/set-state id (todo/next-state cur))))))))

(define todo/delete
  (lambda (id) (delete (quote todo-item) id)))

(define todo/delete-list
  (lambda
    (list-id)
    (let
      ((lid (as-str list-id)))
      (if
        (string-eq lid "")
        "missing"
        (let
          ((fields (todo/get-list lid)))
          (if
            (null? fields)
            "missing"
            (if
              (todo/list-default? fields)
              "default"
              (seq
                (todo/delete-list-items (todo/all-items) lid)
                (seq (delete (quote todo-list) lid) "ok")))))))))

(define todo/delete-list-items
  (lambda
    (items lid)
    (if
      (null? items)
      ()
      (seq
        (if
          (string-eq
            (todo/item-list-id (cdr (car items)))
            (as-str lid))
          (todo/delete (car (car items)))
          ())
        (todo/delete-list-items (cdr items) lid)))))

(define todo/detail-html
  (lambda
    (id view)
    (let
      ((fields (todo/get-item id)))
      (if
        (null? fields)
        (html/div
          (html/attr "class" "card")
          (html/join
            (list
              (html/p* (html/attr "class" "err") "Item not found.")
              (html/a (string-append "/todos?view=" view) "← back"))))
        (let
          ((title (todo/item-title fields)))
          (let
            ((notes (todo/item-notes fields)))
            (let
              ((due (todo/item-due fields)))
              (let
                ((state (todo/item-state fields)))
                (let
                  ((lid (todo/item-list-id fields)))
                  (html/div
                    (html/attr "class" "card todo-detail")
                    (html/join
                      (list
                        (html/p*
                          (html/attr "class" "muted")
                          (html/a
                            (string-append "/todos?view=" view)
                            "← back to list"))
                        (html/h2 (html/text title))
                        (html/form
                          "POST"
                          "/todos"
                          (html/attrs
                            (list (html/attr "class" "todo-detail-form")))
                          (html/join
                            (list
                              (html/hidden "action" "save")
                              (html/hidden "id" id)
                              (html/hidden "view" view)
                              (html/label (html/attr "for" "title") "Title")
                              (html/text-input
                                "title"
                                (html/attrs
                                  (list
                                    (html/attr "id" "title")
                                    (html/attr "value" title)
                                    (html/attr "required" "required"))))
                              (html/label (html/attr "for" "state") "State")
                              (html/select
                                "state"
                                (html/attr "id" "state")
                                (todo/state-options-all state))
                              (html/label (html/attr "for" "due") "Due (YYYY-MM-DD)")
                              (html/text-input
                                "due"
                                (html/attrs
                                  (list
                                    (html/attr "id" "due")
                                    (html/attr "value" due)
                                    (html/attr "placeholder" "2026-07-15"))))
                              (html/label (html/attr "for" "list_id") "List")
                              (html/select
                                "list_id"
                                (html/attr "id" "list_id")
                                (todo/list-options lid))
                              (html/label (html/attr "for" "notes") "Notes")
                              (html/textarea
                                "notes"
                                (html/attrs
                                  (list
                                    (html/attr "id" "notes")
                                    (html/attr "rows" "8")
                                    (html/attr "class" "todo-notes")))
                                notes)
                              (html/div
                                ""
                                (html/join
                                  (list
                                    (html/button (html/attr "type" "submit") "Save")
                                    " "
                                    (html/a (string-append "/todos?view=" view) "Cancel")))))))))))))))))))

(define todo/done?
  (lambda
    (fields)
    (let
      ((d (dict-get fields "done")))
      (if (null? d) false (string-eq d "true")))))

(define todo/ensure-inbox
  (lambda
    ()
    (let
      ((found (todo/find-default-list)))
      (if
        (null? found)
        (create
          (quote todo-list)
          (dict-set*
            "{}"
            (list
              "title"
              "Inbox"
              "description"
              "Default list"
              "is_default"
              "true")))
        (car found)))))

(define todo/find-default-in
  (lambda
    (xs)
    (if
      (null? xs)
      ()
      (if
        (todo/list-default? (cdr (car xs)))
        (car xs)
        (let
          ((t (todo/list-title (cdr (car xs)))))
          (if
            (string-eq t "Inbox")
            (car xs)
            (todo/find-default-in (cdr xs))))))))

(define todo/find-default-list
  (lambda () (todo/find-default-in (todo/lists))))

(define todo/get-item
  (lambda (id) (get (quote todo-item) id)))

(define todo/get-list
  (lambda (id) (get (quote todo-list) id)))

(define todo/handle-post
  (lambda
    ()
    (let
      ((action (as-str (request-param "action"))))
      (if
        (string-eq action "cycle")
        (todo/cycle (request-param "id"))
        (if
          (string-eq action "delete")
          (todo/delete (request-param "id"))
          (if
            (string-eq action "archive")
            (todo/archive (request-param "id"))
            (if
              (string-eq action "add")
              (todo/add
                (request-param "list_id")
                (request-param "title"))
              (if
                (string-eq action "create-list")
                (todo/create-list (request-param "title"))
                (if
                  (string-eq action "delete-list")
                  (todo/delete-list (request-param "list_id"))
                  (if
                    (string-eq action "cycle-note")
                    (notes/cycle-node-todo
                      (request-param "slug")
                      (request-param "node-id"))
                    (if
                      (string-eq action "save")
                      (todo/save
                        (request-param "id")
                        (request-param "title")
                        (request-param "notes")
                        (request-param "due")
                        (request-param "state")
                        (request-param "list_id"))
                      (todo/add (todo/ensure-inbox) (request-param "title")))))))))))))

(define todo/item-due
  (lambda
    (fields)
    (let
      ((d (dict-get fields "due")))
      (if (null? d) "" (as-str d)))))

(define todo/item-li
  (lambda
    (row)
    (let
      ((id (car row)))
      (let
        ((fields (cdr row)))
        (let
          ((title (dict-get fields "title")))
          (let
            ((done (todo/done? fields)))
            (html/li
              (html/attrs
                (list
                  (html/attr
                    "class"
                    (if done "todo-item is-done" "todo-item"))))
              (html/join
                (list
                  (html/form
                    "POST"
                    "/todos"
                    (html/attrs
                      (list
                        (html/attr "class" "todo-check")
                        (html/attr "hx-post" "/todos")
                        (html/attr "hx-target" "#todo-list")
                        (html/attr "hx-swap" "innerHTML")))
                    (html/join
                      (list
                        (html/hidden "action" "toggle")
                        (html/hidden "id" id)
                        (html/button
                          (html/attrs
                            (list
                              (html/attr "type" "submit")
                              (html/attr "class" "todo-box")
                              (html/attr "title" "toggle done")
                              (html/attr "aria-label" "toggle")))
                          (if done "[x]" "[ ]")))))
                  (html/span
                    (html/attr "class" "todo-title")
                    (html/text title))
                  (html/form
                    "POST"
                    "/todos"
                    (html/attrs
                      (list
                        (html/attr "class" "todo-del")
                        (html/attr "hx-post" "/todos")
                        (html/attr "hx-target" "#todo-list")
                        (html/attr "hx-swap" "innerHTML")))
                    (html/join
                      (list
                        (html/hidden "action" "delete")
                        (html/hidden "id" id)
                        (html/button
                          (html/attrs
                            (list
                              (html/attr "type" "submit")
                              (html/attr "class" "secondary")
                              (html/attr "title" "delete")))
                          "del")))))))))))))

(define todo/item-list-id
  (lambda
    (fields)
    (let
      ((lid (dict-get fields "list_id")))
      (if (null? lid) "" (as-str lid)))))

(define todo/item-notes
  (lambda
    (fields)
    (let
      ((n (dict-get fields "notes")))
      (if (null? n) "" (as-str n)))))

(define todo/item-state
  (lambda
    (fields)
    (let
      ((s (dict-get fields "state")))
      (if
        (null? s)
        (let
          ((d (dict-get fields "done")))
          (if
            (null? d)
            "TODO"
            (if (string-eq d "true") "DONE" "TODO")))
        (as-str s)))))

(define todo/item-title
  (lambda (fields) (as-str (dict-get fields "title"))))

(define todo/item/active
  (lambda
    (items)
    (list/filter
      (lambda
        (row)
        (not (todo/state/archived? (todo/item/state (cdr row)))))
      items)))

(define todo/item/add
  (lambda
    (list-id title)
    (let
      ((lid
        (if
          (string-eq (as-str list-id) "")
          (todo/list/ensure-inbox)
          (as-str list-id))))
      (create
        (quote todo-item)
        (dict-set*
          "{}"
          (list
            "list_id"
            lid
            "title"
            (as-str title)
            "notes"
            ""
            "state"
            "TODO"
            "due"
            ""
            "position"
            "0"))))))

(define todo/item/all
  (lambda () (rows (quote todo-item) 200)))

(define todo/item/archive
  (lambda (id) (todo/item/set-state id "ARCHIVED")))

(define todo/item/archived
  (lambda
    ()
    (list/filter
      (lambda
        (row)
        (todo/state/archived? (todo/item/state (cdr row))))
      (todo/item/all))))

(define todo/item/count-all
  (lambda
    (items)
    (list/foldl (lambda (acc _) (+ acc 1)) 0 items)))

(define todo/item/count-open
  (lambda
    (items)
    (list/foldl
      (lambda
        (acc row)
        (if
          (todo/state/open? (todo/item/state (cdr row)))
          (+ acc 1)
          acc))
      0
      items)))

(define todo/item/cycle
  (lambda
    (id)
    (let
      ((row (get (quote todo-item) id)))
      (if
        (null? row)
        ()
        (let
          ((cur (todo/item/state row)))
          (if
            (todo/state/archived? cur)
            cur
            (todo/item/set-state id (todo/state/next cur))))))))

(define todo/item/delete
  (lambda (id) (delete (quote todo-item) id)))

(define todo/item/due
  (lambda
    (fields)
    (let
      ((d (dict-get fields "due")))
      (if (null? d) "" (as-str d)))))

(define todo/item/for-list
  (lambda
    (list-id)
    (let
      ((lid (as-str list-id))
        (inbox-id (todo/list/ensure-inbox)))
      (list/filter
        (lambda
          (row)
          (let
            ((fl (todo/item/list-id (cdr row))))
            (if
              (string-eq fl lid)
              #t
              (if (string-eq fl "") (string-eq lid inbox-id) #f))))
        (todo/item/all)))))

(define todo/item/get
  (lambda (id) (get (quote todo-item) id)))

(define todo/item/html
  (lambda
    (view)
    (if
      (string-eq view "from-notes")
      (html/ul
        (html/attr "class" "card todo-list")
        (list/foldl
          (lambda
            (acc t)
            (string-append acc (todo/item/note-li t view)))
          ""
          (notes/outline/all-todos)))
      (if
        (string-eq view "archived")
        (html/ul
          (html/attr "class" "card todo-list")
          (list/foldl
            (lambda
              (acc row)
              (string-append acc (todo/item/list-li row view)))
            ""
            (todo/item/archived)))
        (if
          (string-eq view "everything")
          (html/join
            (list
              (html/h2 "Lists")
              (html/ul
                (html/attr "class" "card todo-list")
                (list/foldl
                  (lambda
                    (acc row)
                    (string-append acc (todo/item/list-li row view)))
                  ""
                  (todo/item/active (todo/item/all))))
              (html/h2 "From notes")
              (html/ul
                (html/attr "class" "card todo-list")
                (list/foldl
                  (lambda
                    (acc t)
                    (if
                      (todo/state/open? (car (cdr (cdr (cdr t)))))
                      (string-append acc (todo/item/note-li t view))
                      acc))
                  ""
                  (notes/outline/all-todos)))))
          (html/ul
            (html/attr "class" "card todo-list")
            (list/foldl
              (lambda
                (acc row)
                (string-append acc (todo/item/list-li row view)))
              ""
              (todo/item/active (todo/item/for-list view)))))))))

(define todo/item/list-id
  (lambda
    (fields)
    (let
      ((lid (dict-get fields "list_id")))
      (if (null? lid) "" (as-str lid)))))

(define todo/item/list-li
  (lambda
    (row view)
    (let
      ((id (car row)) (fields (cdr row)))
      (let
        ((title (todo/item/title fields))
          (state (todo/item/state fields))
          (open (todo/state/open? (todo/item/state fields))))
        (html/li
          (html/attr
            "class"
            (if
              open
              "todo-item"
              (if
                (todo/state/archived? state)
                "todo-item is-done is-archived"
                "todo-item is-done")))
          (html/join
            (list
              (html/form
                "POST"
                "/todos"
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
                          (html/attr "class" (string-append "todo-box st-" state))
                          (html/attr "title" "cycle state")))
                      (if (string-eq state "") "·" state)))))
              (html/a
                (string-append "/todos?id=" id "&view=" view)
                (html/span
                  (html/attr "class" "todo-title")
                  (html/text title)))
              (if
                (string-eq state "DONE")
                (html/form
                  "POST"
                  "/todos"
                  (html/attrs
                    (list
                      (html/attr "class" "todo-archive")
                      (html/attr "hx-post" "/todos")
                      (html/attr "hx-target" "#todo-main")
                      (html/attr "hx-swap" "innerHTML")))
                  (html/join
                    (list
                      (html/hidden "action" "archive")
                      (html/hidden "id" id)
                      (html/hidden "view" view)
                      (html/button
                        (html/attrs
                          (list
                            (html/attr "type" "submit")
                            (html/attr "class" "secondary")
                            (html/attr "title" "archive done task")))
                        "arch"))))
                "")
              (html/form
                "POST"
                "/todos"
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

(define todo/item/note-li
  (lambda
    (t view)
    (let
      ((slug (car t))
        (nid (car (cdr t)))
        (title (car (cdr (cdr t))))
        (state (car (cdr (cdr (cdr t))))))
      (let
        ((open (todo/state/open? state)))
        (html/li
          (html/attr
            "class"
            (if
              open
              "todo-item todo-from-note"
              "todo-item todo-from-note is-done"))
          (html/join
            (list
              (html/form
                "POST"
                "/todos"
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
                          (html/attr "class" (string-append "todo-box st-" state))
                          (html/attr "title" "cycle note TODO")))
                      (if (string-eq state "") "·" state)))))
              (html/span
                (html/attr "class" "todo-title")
                (html/text title))
              (html/a
                (string-append "/notes?slug=" slug)
                (html/span
                  (html/attr "class" "muted todo-src")
                  (html/text slug))))))))))

(define todo/item/notes
  (lambda
    (fields)
    (let
      ((n (dict-get fields "notes")))
      (if (null? n) "" (as-str n)))))

(define todo/item/save
  (lambda
    (id title notes due state list-id)
    (let
      ((fields (todo/item/get id)))
      (if
        (null? fields)
        ()
        (let
          ((lid
            (if
              (string-eq (as-str list-id) "")
              (todo/list/ensure-inbox)
              (as-str list-id))))
          (update
            (quote todo-item)
            id
            (dict-set*
              fields
              (list
                "title"
                (as-str title)
                "notes"
                (as-str notes)
                "due"
                (as-str due)
                "state"
                (as-str state)
                "list_id"
                lid))))))))

(define todo/item/set-state
  (lambda
    (id state)
    (update-field
      (quote todo-item)
      id
      "state"
      (as-str state))))

(define todo/item/state
  (lambda
    (fields)
    (let
      ((s (dict-get fields "state")))
      (if
        (null? s)
        (let
          ((d (dict-get fields "done")))
          (if
            (null? d)
            "TODO"
            (if (string-eq d "true") "DONE" "TODO")))
        (as-str s)))))

(define todo/item/title
  (lambda (fields) (as-str (dict-get fields "title"))))

(define todo/items-for-list
  (lambda
    (list-id)
    (let
      ((lid (as-str list-id)) (inbox-id (todo/ensure-inbox)))
      (list/filter
        (lambda
          (row)
          (let
            ((fl (todo/item-list-id (cdr row))))
            (if
              (string-eq fl lid)
              #t
              (if (string-eq fl "") (string-eq lid inbox-id) #f))))
        (todo/all-items)))))

(define todo/items-html
  (lambda
    (view)
    (if
      (string-eq view "from-notes")
      (html/ul
        (html/attr "class" "card todo-list")
        (list/foldl
          (lambda
            (acc t)
            (string-append acc (todo/note-item-li t view)))
          ""
          (notes/all-todos)))
      (if
        (string-eq view "archived")
        (html/ul
          (html/attr "class" "card todo-list")
          (list/foldl
            (lambda
              (acc row)
              (string-append acc (todo/list-item-li row view)))
            ""
            (todo/archived-items)))
        (if
          (string-eq view "everything")
          (html/join
            (list
              (html/h2 "Lists")
              (html/ul
                (html/attr "class" "card todo-list")
                (list/foldl
                  (lambda
                    (acc row)
                    (string-append acc (todo/list-item-li row view)))
                  ""
                  (todo/active-items (todo/all-items))))
              (html/h2 "From notes")
              (html/ul
                (html/attr "class" "card todo-list")
                (list/foldl
                  (lambda
                    (acc t)
                    (if
                      (todo/open? (car (cdr (cdr (cdr t)))))
                      (string-append acc (todo/note-item-li t view))
                      acc))
                  ""
                  (notes/all-todos)))))
          (html/ul
            (html/attr "class" "card todo-list")
            (list/foldl
              (lambda
                (acc row)
                (string-append acc (todo/list-item-li row view)))
              ""
              (todo/active-items (todo/items-for-list view)))))))))

(define todo/land-ping
  1)

(define todo/list
  (lambda () (rows (quote todo-item) 50)))

(define todo/list-default?
  (lambda
    (fields)
    (let
      ((d (dict-get fields "is_default")))
      (if (null? d) #f (string-eq d "true")))))

(define todo/list-html
  (lambda
    ()
    (html/ul
      (html/attr "class" "card todo-list")
      (list/foldl
        (lambda
          (acc row)
          (string-append acc (todo/item-li row)))
        ""
        (todo/list)))))

(define todo/list-item-li
  (lambda
    (row view)
    (let
      ((id (car row)) (fields (cdr row)))
      (let
        ((title (todo/item-title fields))
          (state (todo/item-state fields))
          (open (todo/open? (todo/item-state fields))))
        (html/li
          (html/attr
            "class"
            (if
              open
              "todo-item"
              (if
                (todo/archived? state)
                "todo-item is-done is-archived"
                "todo-item is-done")))
          (html/join
            (list
              (html/form
                "POST"
                "/todos"
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
                          (html/attr "class" (string-append "todo-box st-" state))
                          (html/attr "title" "cycle state")))
                      (if (string-eq state "") "·" state)))))
              (html/a
                (string-append "/todos?id=" id "&view=" view)
                (html/span
                  (html/attr "class" "todo-title")
                  (html/text title)))
              (if
                (string-eq state "DONE")
                (html/form
                  "POST"
                  "/todos"
                  (html/attrs
                    (list
                      (html/attr "class" "todo-archive")
                      (html/attr "hx-post" "/todos")
                      (html/attr "hx-target" "#todo-main")
                      (html/attr "hx-swap" "innerHTML")))
                  (html/join
                    (list
                      (html/hidden "action" "archive")
                      (html/hidden "id" id)
                      (html/hidden "view" view)
                      (html/button
                        (html/attrs
                          (list
                            (html/attr "type" "submit")
                            (html/attr "class" "secondary")
                            (html/attr "title" "archive done task")))
                        "arch"))))
                "")
              (html/form
                "POST"
                "/todos"
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

(define todo/list-options
  (lambda
    (selected)
    (list/foldl
      (lambda
        (acc row)
        (let
          ((id (car row)))
          (let
            ((title (todo/list-title (cdr row))))
            (string-append
              acc
              (html/option
                id
                title
                (string-eq (as-str id) (as-str selected)))))))
      ""
      (todo/lists))))

(define todo/list-title
  (lambda (fields) (as-str (dict-get fields "title"))))

(define todo/list/all
  (lambda () (rows (quote todo-list) 50)))

(define todo/list/create
  (lambda
    (title)
    (create
      (quote todo-list)
      (dict-set*
        "{}"
        (list
          "title"
          (as-str title)
          "description"
          ""
          "is_default"
          "false")))))

(define todo/list/default?
  (lambda
    (fields)
    (let
      ((d (dict-get fields "is_default")))
      (if (null? d) #f (string-eq d "true")))))

(define todo/list/delete
  (lambda
    (list-id)
    (let
      ((lid (as-str list-id)))
      (if
        (string-eq lid "")
        "missing"
        (let
          ((fields (todo/list/get lid)))
          (if
            (null? fields)
            "missing"
            (if
              (todo/list/default? fields)
              "default"
              (seq
                (todo/list/delete-items (todo/item/all) lid)
                (seq (delete (quote todo-list) lid) "ok")))))))))

(define todo/list/delete-items
  (lambda
    (items lid)
    (if
      (null? items)
      ()
      (seq
        (if
          (string-eq
            (todo/item/list-id (cdr (car items)))
            (as-str lid))
          (todo/item/delete (car (car items)))
          ())
        (todo/list/delete-items (cdr items) lid)))))

(define todo/list/ensure-inbox
  (lambda
    ()
    (let
      ((found (todo/list/find-default)))
      (if
        (null? found)
        (create
          (quote todo-list)
          (dict-set*
            "{}"
            (list
              "title"
              "Inbox"
              "description"
              "Default list"
              "is_default"
              "true")))
        (car found)))))

(define todo/list/find-default
  (lambda () (todo/list/find-default-in (todo/list/all))))

(define todo/list/find-default-in
  (lambda
    (xs)
    (if
      (null? xs)
      ()
      (if
        (todo/list/default? (cdr (car xs)))
        (car xs)
        (let
          ((t (todo/list/title (cdr (car xs)))))
          (if
            (string-eq t "Inbox")
            (car xs)
            (todo/list/find-default-in (cdr xs))))))))

(define todo/list/get
  (lambda (id) (get (quote todo-list) id)))

(define todo/list/nav-li
  (lambda
    (view row)
    (let
      ((id (car row))
        (f (cdr row))
        (items
          (todo/item/active (todo/item/for-list (car row)))))
      (html/li
        (html/attr
          "class"
          (if
            (string-eq view id)
            "todo-nav-item is-active todo-nav-list"
            "todo-nav-item todo-nav-list"))
        (html/join
          (list
            (html/a
              (string-append "/todos?view=" id)
              (string-append
                (todo/list/title f)
                " ("
                (todo/ui/ratio items)
                ")"))
            (if
              (todo/list/default? f)
              ""
              (html/form
                "POST"
                "/todos"
                (html/attrs
                  (list
                    (html/attr "class" "todo-list-del")
                    (html/attr "hx-post" "/todos")
                    (html/attr "hx-target" "#todo-main")
                    (html/attr "hx-swap" "innerHTML")
                    (html/attr
                      "hx-confirm"
                      "Delete this entire list and its items?")))
                (html/join
                  (list
                    (html/hidden "action" "delete-list")
                    (html/hidden "list_id" id)
                    (html/hidden "view" "everything")
                    (html/button
                      (html/attrs
                        (list
                          (html/attr "type" "submit")
                          (html/attr "class" "secondary")
                          (html/attr "title" "delete list")))
                      "×")))))))))))

(define todo/list/options
  (lambda
    (selected)
    (list/foldl
      (lambda
        (acc row)
        (let
          ((id (car row)))
          (let
            ((title (todo/list/title (cdr row))))
            (string-append
              acc
              (html/option
                id
                title
                (string-eq (as-str id) (as-str selected)))))))
      ""
      (todo/list/all))))

(define todo/list/title
  (lambda (fields) (as-str (dict-get fields "title"))))

(define todo/lists
  (lambda () (rows (quote todo-list) 50)))

(define todo/main-html
  (lambda
    (view)
    (let
      ((id-param (request-param "id"))
        (is-get (string-eq (request-method) "GET")))
      (if
        (if
          is-get
          (if
            (null? id-param)
            #f
            (not (string-eq (as-str id-param) "")))
          #f)
        (html/div
          (html/attr "id" "todo-main")
          (html/join
            (list
              (html/div
                (html/attr "class" "todo-layout")
                (html/join
                  (list
                    (html/div
                      (html/attr "class" "todo-side")
                      (todo/nav-html view))
                    (html/div
                      (html/attr "class" "todo-body")
                      (todo/detail-html (as-str id-param) view))))))))
        (let
          ((list-id
            (if
              (string-eq view "everything")
              (todo/ensure-inbox)
              (if
                (string-eq view "from-notes")
                ""
                (if (string-eq view "archived") "" view)))))
          (html/div
            (html/attr "id" "todo-main")
            (html/join
              (list
                (html/div
                  (html/attr "class" "todo-layout")
                  (html/join
                    (list
                      (html/div
                        (html/attr "class" "todo-side")
                        (todo/nav-html view))
                      (html/div
                        (html/attr "class" "todo-body")
                        (html/join
                          (list
                            (todo/add-form view list-id)
                            (todo/items-html view)))))))))))))))

(define todo/nav-html
  (lambda
    (view)
    (let
      ((lists (todo/lists))
        (inbox (todo/ensure-inbox))
        (arch-n (todo/count-all-in (todo/archived-items))))
      (html/ul
        (html/attr "class" "todo-nav")
        (html/join
          (list
            (todo/nav-link view "everything" "Everything" "")
            (todo/nav-link view "from-notes" "From notes" "")
            (todo/nav-link
              view
              "archived"
              "Archived"
              (string-append "(" (as-str arch-n) ")"))
            (list/foldl
              (lambda
                (acc row)
                (string-append acc (todo/nav-list-li view row)))
              ""
              lists)
            (html/li
              (html/attr "class" "todo-nav-item")
              (html/form
                "POST"
                "/todos"
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
                    (html/text-input
                      "title"
                      (html/attrs
                        (list
                          (html/attr "placeholder" "New list")
                          (html/attr "required" "required")
                          (html/attr "class" "todo-new-list-input"))))
                    (html/button (html/attr "type" "submit") "+ list")))))))))))

(define todo/nav-link
  (lambda
    (view key label extra)
    (html/li
      (html/attr
        "class"
        (if
          (string-eq view key)
          "todo-nav-item is-active"
          "todo-nav-item"))
      (html/a
        (string-append "/todos?view=" key)
        (string-append
          label
          (if (string-eq extra "") "" (string-append " " extra)))))))

(define todo/nav-list-li
  (lambda
    (view row)
    (let
      ((id (car row))
        (f (cdr row))
        (items
          (todo/active-items (todo/items-for-list (car row)))))
      (html/li
        (html/attr
          "class"
          (if
            (string-eq view id)
            "todo-nav-item is-active todo-nav-list"
            "todo-nav-item todo-nav-list"))
        (html/join
          (list
            (html/a
              (string-append "/todos?view=" id)
              (string-append
                (todo/list-title f)
                " ("
                (todo/ratio items)
                ")"))
            (if
              (todo/list-default? f)
              ""
              (html/form
                "POST"
                "/todos"
                (html/attrs
                  (list
                    (html/attr "class" "todo-list-del")
                    (html/attr "hx-post" "/todos")
                    (html/attr "hx-target" "#todo-main")
                    (html/attr "hx-swap" "innerHTML")
                    (html/attr
                      "hx-confirm"
                      "Delete this entire list and its items?")))
                (html/join
                  (list
                    (html/hidden "action" "delete-list")
                    (html/hidden "list_id" id)
                    (html/hidden "view" "everything")
                    (html/button
                      (html/attrs
                        (list
                          (html/attr "type" "submit")
                          (html/attr "class" "secondary")
                          (html/attr "title" "delete list")))
                      "×")))))))))))

(define todo/next-state
  (lambda
    (cur)
    (outline/todo-next-in todo/states (as-str cur))))

(define todo/note-item-li
  (lambda
    (t view)
    (let
      ((slug (car t))
        (nid (car (cdr t)))
        (title (car (cdr (cdr t))))
        (state (car (cdr (cdr (cdr t))))))
      (let
        ((open (todo/open? state)))
        (html/li
          (html/attr
            "class"
            (if
              open
              "todo-item todo-from-note"
              "todo-item todo-from-note is-done"))
          (html/join
            (list
              (html/form
                "POST"
                "/todos"
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
                          (html/attr "class" (string-append "todo-box st-" state))
                          (html/attr "title" "cycle note TODO")))
                      (if (string-eq state "") "·" state)))))
              (html/span
                (html/attr "class" "todo-title")
                (html/text title))
              (html/a
                (string-append "/notes?slug=" slug)
                (html/span
                  (html/attr "class" "muted todo-src")
                  (html/text slug))))))))))

(define todo/open?
  (lambda
    (s)
    (let
      ((t (as-str s)))
      (if
        (string-eq t "")
        #f
        (if
          (string-eq t "DONE")
          #f
          (if
            (string-eq t "CANCELLED")
            #f
            (if (string-eq t "ARCHIVED") #f #t)))))))

(define todo/page
  (lambda
    ()
    (seq
      (todo/list/ensure-inbox)
      (if
        (string-eq (request-method) "POST")
        (seq
          (todo/ui/handle-post)
          (let
            ((view
              (let
                ((v (request-param "view")))
                (if (null? v) "everything" (as-str v)))))
            (if
              (hx-request?)
              (render-html (todo/ui/main view))
              (redirect (string-append "/todos?view=" view)))))
        (html/page
          (list
            (html/h1 "To-dos")
            (html/p*
              (html/attr "class" "muted")
              "Lists (Basecamp) + notes scrape (org). Click a title for details.")
            (todo/ui/main (todo/ui/view))))))))

(define todo/partial
  (lambda
    ()
    (seq
      (todo/list/ensure-inbox)
      (render-html (todo/ui/main (todo/ui/view))))))

(define todo/ratio
  (lambda
    (items)
    (let
      ((n (todo/count-all-in items))
        (open (todo/count-open-in items)))
      (string-append (as-str (- n open)) "/" (as-str n)))))

(define todo/save
  (lambda
    (id title notes due state list-id)
    (let
      ((fields (todo/get-item id)))
      (if
        (null? fields)
        ()
        (let
          ((lid
            (if
              (string-eq (as-str list-id) "")
              (todo/ensure-inbox)
              (as-str list-id))))
          (update
            (quote todo-item)
            id
            (dict-set*
              fields
              (list
                "title"
                (as-str title)
                "notes"
                (as-str notes)
                "due"
                (as-str due)
                "state"
                (as-str state)
                "list_id"
                lid))))))))

(define todo/set-state
  (lambda
    (id state)
    (update-field
      (quote todo-item)
      id
      "state"
      (as-str state))))

(define todo/state-options
  (lambda
    (selected)
    (list/foldl
      (lambda
        (acc st)
        (let
          ((lab (if (string-eq st "") "(none)" st)))
          (string-append
            acc
            (html/option st lab (string-eq st (as-str selected))))))
      ""
      todo/states)))

(define todo/state-options-all
  (lambda
    (selected)
    (todo/state-options-from
      selected
      (append todo/states (list "ARCHIVED")))))

(define todo/state-options-from
  (lambda
    (selected states)
    (list/foldl
      (lambda
        (acc st)
        (let
          ((lab (if (string-eq st "") "(none)" st)))
          (string-append
            acc
            (html/option st lab (string-eq st (as-str selected))))))
      ""
      states)))

(define todo/state/archived?
  (lambda (s) (string-eq (as-str s) "ARCHIVED")))

(define todo/state/list
  (list "" "TODO" "DONE" "WAITING" "CANCELLED"))

(define todo/state/next
  (lambda
    (cur)
    (outline/todo-next-in todo/state/list (as-str cur))))

(define todo/state/open?
  (lambda
    (s)
    (let
      ((t (as-str s)))
      (if
        (string-eq t "")
        #f
        (if
          (string-eq t "DONE")
          #f
          (if
            (string-eq t "CANCELLED")
            #f
            (if (string-eq t "ARCHIVED") #f #t)))))))

(define todo/state/options
  (lambda
    (selected)
    (list/foldl
      (lambda
        (acc st)
        (let
          ((lab (if (string-eq st "") "(none)" st)))
          (string-append
            acc
            (html/option st lab (string-eq st (as-str selected))))))
      ""
      todo/state/list)))

(define todo/state/options-all
  (lambda
    (selected)
    (todo/state/options-from
      selected
      (append todo/state/list (list "ARCHIVED")))))

(define todo/state/options-from
  (lambda
    (selected states)
    (list/foldl
      (lambda
        (acc st)
        (let
          ((lab (if (string-eq st "") "(none)" st)))
          (string-append
            acc
            (html/option st lab (string-eq st (as-str selected))))))
      ""
      states)))

(define todo/states
  (list "" "TODO" "DONE" "WAITING" "CANCELLED"))

(define todo/toggle
  (lambda
    (id)
    (let
      ((row (get (quote todo-item) id)))
      (if
        (null? row)
        ()
        (update-field
          (quote todo-item)
          id
          "done"
          (if (todo/done? row) "false" "true"))))))

(define todo/ui/add-form
  (lambda
    (view list-id)
    (if
      (string-eq view "from-notes")
      (html/p*
        (html/attr "class" "muted")
        "Mark nodes TODO in notes — they appear here. Toggle writes back.")
      (if
        (string-eq view "archived")
        (html/p*
          (html/attr "class" "muted")
          "Archived tasks. Unarchive from detail (set state back), or delete.")
        (html/form
          "POST"
          "/todos"
          (html/attrs
            (list
              (html/attr "class" "card todo-add")
              (html/attr "hx-post" "/todos")
              (html/attr "hx-target" "#todo-main")
              (html/attr "hx-swap" "innerHTML")
              (html/attr
                "hx-on::after-request"
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
              (html/div
                (html/attr "class" "todo-add-row")
                (html/join
                  (list
                    (html/text-input
                      "title"
                      (html/attrs
                        (list
                          (html/attr "id" "title")
                          (html/attr "required" "required")
                          (html/attr "placeholder" "buy milk")
                          (html/attr "autocomplete" "off"))))
                    (html/button (html/attr "type" "submit") "Add")))))))))))

(define todo/ui/detail
  (lambda
    (id view)
    (let
      ((fields (todo/item/get id)))
      (if
        (null? fields)
        (html/div
          (html/attr "class" "card")
          (html/join
            (list
              (html/p* (html/attr "class" "err") "Item not found.")
              (html/a (string-append "/todos?view=" view) "← back"))))
        (let
          ((title (todo/item/title fields)))
          (let
            ((notes (todo/item/notes fields)))
            (let
              ((due (todo/item/due fields)))
              (let
                ((state (todo/item/state fields)))
                (let
                  ((lid (todo/item/list-id fields)))
                  (html/div
                    (html/attr "class" "card todo-detail")
                    (html/join
                      (list
                        (html/p*
                          (html/attr "class" "muted")
                          (html/a
                            (string-append "/todos?view=" view)
                            "← back to list"))
                        (html/h2 (html/text title))
                        (html/form
                          "POST"
                          "/todos"
                          (html/attrs
                            (list (html/attr "class" "todo-detail-form")))
                          (html/join
                            (list
                              (html/hidden "action" "save")
                              (html/hidden "id" id)
                              (html/hidden "view" view)
                              (html/label (html/attr "for" "title") "Title")
                              (html/text-input
                                "title"
                                (html/attrs
                                  (list
                                    (html/attr "id" "title")
                                    (html/attr "value" title)
                                    (html/attr "required" "required"))))
                              (html/label (html/attr "for" "state") "State")
                              (html/select
                                "state"
                                (html/attr "id" "state")
                                (todo/state/options-all state))
                              (html/label (html/attr "for" "due") "Due (YYYY-MM-DD)")
                              (html/text-input
                                "due"
                                (html/attrs
                                  (list
                                    (html/attr "id" "due")
                                    (html/attr "value" due)
                                    (html/attr "placeholder" "2026-07-15"))))
                              (html/label (html/attr "for" "list_id") "List")
                              (html/select
                                "list_id"
                                (html/attr "id" "list_id")
                                (todo/list/options lid))
                              (html/label (html/attr "for" "notes") "Notes")
                              (html/textarea
                                "notes"
                                (html/attrs
                                  (list
                                    (html/attr "id" "notes")
                                    (html/attr "rows" "8")
                                    (html/attr "class" "todo-notes")))
                                notes)
                              (html/div
                                ""
                                (html/join
                                  (list
                                    (html/button (html/attr "type" "submit") "Save")
                                    " "
                                    (html/a (string-append "/todos?view=" view) "Cancel")))))))))))))))))))

(define todo/ui/handle-post
  (lambda
    ()
    (let
      ((action (as-str (request-param "action"))))
      (cond
        ((string-eq action "cycle")
          (todo/item/cycle (request-param "id")))
        ((string-eq action "delete")
          (todo/item/delete (request-param "id")))
        ((string-eq action "archive")
          (todo/item/archive (request-param "id")))
        ((string-eq action "add")
          (todo/item/add
            (request-param "list_id")
            (request-param "title")))
        ((string-eq action "create-list")
          (todo/list/create (request-param "title")))
        ((string-eq action "delete-list")
          (todo/list/delete (request-param "list_id")))
        ((string-eq action "cycle-note")
          (notes/outline/cycle-todo
            (request-param "slug")
            (request-param "node-id")))
        ((string-eq action "save")
          (todo/item/save
            (request-param "id")
            (request-param "title")
            (request-param "notes")
            (request-param "due")
            (request-param "state")
            (request-param "list_id")))
        (else
          (todo/item/add
            (todo/list/ensure-inbox)
            (request-param "title")))))))

(define todo/ui/main
  (lambda
    (view)
    (let
      ((id-param (request-param "id"))
        (is-get (string-eq (request-method) "GET")))
      (if
        (if
          is-get
          (if
            (null? id-param)
            #f
            (not (string-eq (as-str id-param) "")))
          #f)
        (html/div
          (html/attr "id" "todo-main")
          (html/join
            (list
              (html/div
                (html/attr "class" "todo-layout")
                (html/join
                  (list
                    (html/div
                      (html/attr "class" "todo-side")
                      (todo/ui/nav view))
                    (html/div
                      (html/attr "class" "todo-body")
                      (todo/ui/detail (as-str id-param) view))))))))
        (let
          ((list-id
            (if
              (string-eq view "everything")
              (todo/list/ensure-inbox)
              (if
                (string-eq view "from-notes")
                ""
                (if (string-eq view "archived") "" view)))))
          (html/div
            (html/attr "id" "todo-main")
            (html/join
              (list
                (html/div
                  (html/attr "class" "todo-layout")
                  (html/join
                    (list
                      (html/div
                        (html/attr "class" "todo-side")
                        (todo/ui/nav view))
                      (html/div
                        (html/attr "class" "todo-body")
                        (html/join
                          (list
                            (todo/ui/add-form view list-id)
                            (todo/item/html view)))))))))))))))

(define todo/ui/nav
  (lambda
    (view)
    (let
      ((lists (todo/list/all))
        (inbox (todo/list/ensure-inbox))
        (arch-n (todo/item/count-all (todo/item/archived))))
      (html/ul
        (html/attr "class" "todo-nav")
        (html/join
          (list
            (todo/ui/nav-link view "everything" "Everything" "")
            (todo/ui/nav-link view "from-notes" "From notes" "")
            (todo/ui/nav-link
              view
              "archived"
              "Archived"
              (string-append "(" (as-str arch-n) ")"))
            (list/foldl
              (lambda
                (acc row)
                (string-append acc (todo/list/nav-li view row)))
              ""
              lists)
            (html/li
              (html/attr "class" "todo-nav-item")
              (html/form
                "POST"
                "/todos"
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
                    (html/text-input
                      "title"
                      (html/attrs
                        (list
                          (html/attr "placeholder" "New list")
                          (html/attr "required" "required")
                          (html/attr "class" "todo-new-list-input"))))
                    (html/button (html/attr "type" "submit") "+ list")))))))))))

(define todo/ui/nav-link
  (lambda
    (view key label extra)
    (html/li
      (html/attr
        "class"
        (if
          (string-eq view key)
          "todo-nav-item is-active"
          "todo-nav-item"))
      (html/a
        (string-append "/todos?view=" key)
        (string-append
          label
          (if (string-eq extra "") "" (string-append " " extra)))))))

(define todo/ui/ratio
  (lambda
    (items)
    (let
      ((n (todo/item/count-all items))
        (open (todo/item/count-open items)))
      (string-append (as-str (- n open)) "/" (as-str n)))))

(define todo/ui/view
  (lambda
    ()
    (let
      ((v (request-param "view")))
      (if (null? v) "everything" (as-str v)))))

(define todo/view
  (lambda
    ()
    (let
      ((v (request-param "view")))
      (if (null? v) "everything" (as-str v)))))

