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
    todo/item/assign
    todo/item/assignee
    todo/item/cycle
    todo/item/delete
    todo/item/for-user
    todo/item/priority
    todo/item/queue
    todo/item/set-priority
    todo/item/unassign
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
  "Non-archived items from a list of rows."

(define todo/add
  (lambda
    (list-id title)
    (let
      ((lid
        (if
          (string-eq (as-str list-id) "")
          (todo/ensure-inbox)
          (as-str list-id))))
      (todo/tree/create
        "todo-item"
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
            "0"
            "done"
            "false"))))))
  "Create a todo item in list-id (default inbox) with the given title."

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
  "HTML add form (or hint) for the given view/list."

(define todo/all-items
  (lambda () (todo/tree/rows "todo-item" 200)))
  "All todo-item rows (up to 200)."

(define todo/archive
  (lambda (id) (todo/set-state id "ARCHIVED")))
  "Set an item state to ARCHIVED."

(define todo/archived-items
  (lambda
    ()
    (list/filter
      (lambda
        (row)
        (todo/archived? (todo/item-state (cdr row))))
      (todo/all-items))))
  "Items whose state is ARCHIVED."

(define todo/archived?
  (lambda (s) (string-eq (as-str s) "ARCHIVED")))
  "True when a state string is ARCHIVED."

(define todo/count-all-in
  (lambda
    (items)
    (list/foldl (lambda (acc _) (+ acc 1)) 0 items)))
  "Count of rows in items."

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
  "Count of open items in items."

(define todo/create-list
  (lambda
    (title)
    (todo/tree/create
      "todo-list"
      (dict-set*
        "{}"
        (list
          "title"
          (as-str title)
          "description"
          ""
          "is_default"
          "false")))))
  "Create a todo-list with the given title."

(define todo/cycle
  (lambda
    (id)
    "Advance item id to its next state; () when missing. Cycles within the
     item's OWN list's state set (custom columns when present, else the global
     default) — BORGE-1786336755, so /todos exactly mirrors /kanban."
    (let
      ((row (todo/tree/get "todo-item" id)))
      (if
        (null? row)
        ()
        (let
          ((cur (todo/item-state row))
            (list-id (todo/item/list-id row)))
          (if
            (todo/archived? cur)
            cur
            (todo/set-state id (todo/state/next-for-list cur list-id))))))))

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
  "Create a datum node of type with fields; returns the id."

(define todo/db/delete
  (lambda
    (type id)
    (let
      ((p (node-path type (as-str id))))
      (if
        (null? (tree/get p))
        ()
        (seq (tree/delete p) (as-str id))))))
  "Delete a datum node; returns the id or ()."

(define todo/db/get
  (lambda
    (type id)
    (let
      ((n (tree/get (node-path type (as-str id)))))
      (if (null? n) () (dict-get n "body")))))
  "Fields dict for a datum node; () when absent."

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
  "Insert node n into a sorted list by created_at."

(define todo/db/node-row
  (lambda
    (node)
    (cons
      (todo/db/path-id (dict-get node "path"))
      (dict-get node "body"))))
  "Convert a tree node to an (id . fields) pair."

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
  "Derive an id string from a data path."

(define todo/db/prefix
  (lambda
    (type)
    (let
      ((p (node-path type "")))
      (substring p 0 (- (string-length p) 1)))))
  "The path prefix for a type."

(define todo/db/rows
  (lambda
    (type limit)
    (map
      todo/db/node-row
      (tree/list-kind (todo/db/prefix type) "datum" limit))))
  "Rows of a type as (id . fields) pairs up to limit."

(define todo/db/sort-by-created
  (lambda
    (ns)
    (if
      (null? ns)
      (list)
      (todo/db/insert-by-created
        (car ns)
        (todo/db/sort-by-created (cdr ns))))))
  "Sort nodes by created_at."

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
  "Update a datum node fields; returns the id."

(define todo/db/update-field
  (lambda
    (type id field value)
    (let
      ((fields (todo/db/get type (as-str id))))
      (if
        (null? fields)
        ()
        (todo/db/update type id (dict-set fields field value))))))
  "Set one field on a datum node and persist."

(define todo/delete
  (lambda (id) (todo/tree/del "todo-item" id)))
  "Delete a todo-item by id."

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
                (seq (todo/tree/del "todo-list" lid) "ok")))))))))
  "Delete a list and its items; guards the default list."

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
  "Delete items belonging to lid from the items list."

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
  "HTML detail/edit card for an item by id."

(define todo/done?
  (lambda
    (fields)
    (let
      ((d (dict-get fields "done")))
      (if (null? d) false (string-eq d "true")))))
  "True when the item done field is "true"."

(define todo/ensure-inbox
  (lambda
    ()
    (let
      ((found (todo/find-default-list)))
      (if
        (null? found)
        (todo/tree/create
          "todo-list"
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
  "The default list id, creating an Inbox when none exists."

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
  "Find the default (or Inbox-titled) list in xs."

(define todo/find-default-list
  (lambda () (todo/find-default-in (todo/lists))))
  "The default list, or ()."

(define todo/get-item
  (lambda (id) (todo/tree/get "todo-item" id)))
  "Fields of a todo-item by id; () when absent."

(define todo/get-list
  (lambda (id) (todo/tree/get "todo-list" id)))
  "Fields of a todo-list by id; () when absent."

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
  "Dispatch a POST /todos action to the matching handler."

(define todo/item-due
  (lambda
    (fields)
    (let
      ((d (dict-get fields "due")))
      (if (null? d) "" (as-str d)))))
  "An item due string; "" when absent."

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
  "HTML <li> for a todo item row."

(define todo/item-list-id
  (lambda
    (fields)
    (let
      ((lid (dict-get fields "list_id")))
      (if (null? lid) "" (as-str lid)))))
  "An item list_id string; "" when absent."

(define todo/item-notes
  (lambda
    (fields)
    (let
      ((n (dict-get fields "notes")))
      (if (null? n) "" (as-str n)))))
  "An item notes string; "" when absent."

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
  "An item state string, deriving from done when absent."

(define todo/item-title
  (lambda (fields) (as-str (dict-get fields "title"))))
  "An item title string."

(define todo/item/active
  (lambda
    (items)
    (list/filter
      (lambda
        (row)
        (not (todo/state/archived? (todo/item/state (cdr row)))))
      items)))
  "Non-archived items from a list of rows (item/ set)."

(define todo/item/add
  (lambda
    (list-id title)
    (let
      ((lid
        (if
          (string-eq (as-str list-id) "")
          (todo/list/ensure-inbox)
          (as-str list-id))))
      (todo/tree/create
        "todo-item"
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
            "0"
            "done"
            "false"))))))
  "Create a todo item in list-id with the given title (item/ set)."

(define todo/item/all
  (lambda () (todo/tree/rows "todo-item" 200)))
  "All todo-item rows (up to 200) (item/ set)."

(define todo/item/archive
  (lambda (id) (todo/item/set-state id "ARCHIVED")))
  "Set an item state to ARCHIVED (item/ set)."

(define todo/item/archived
  (lambda
    ()
    (list/filter
      (lambda
        (row)
        (todo/state/archived? (todo/item/state (cdr row))))
      (todo/item/all))))
  "Items whose state is ARCHIVED (item/ set)."

(define todo/item/count-all
  (lambda
    (items)
    (list/foldl (lambda (acc _) (+ acc 1)) 0 items)))
  "Count of rows in items (item/ set)."

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
  "Count of open items in items (item/ set)."

(define todo/item/cycle
  (lambda
    (id)
    (let
      ((row (todo/tree/get "todo-item" id)))
      (if
        (null? row)
        ()
        (let
          ((cur (todo/item/state row))
            (list-id (todo/item/list-id row)))
          (if
            (todo/state/archived? cur)
            cur
            (todo/item/set-state id (todo/state/next-for-list cur list-id))))))))

(define todo/item/delete
  (lambda (id) (todo/tree/del "todo-item" id)))
  "Delete a todo-item by id (item/ set)."

(define todo/item/due
  (lambda
    (fields)
    (let
      ((d (dict-get fields "due")))
      (if (null? d) "" (as-str d)))))
  "An item due string; "" when absent (item/ set)."

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
  "Items for list-id, treating a blank list_id as the inbox (item/ set)."

(define todo/item/get
  (lambda (id) (todo/tree/get "todo-item" id)))
  "Fields of a todo-item by id; () when absent (item/ set)."

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
  "HTML <ul> of items per view (item/ set)."

(define todo/item/list-id
  (lambda
    (fields)
    (let
      ((lid (dict-get fields "list_id")))
      (if (null? lid) "" (as-str lid)))))
  "An item list_id string; "" when absent (item/ set)."

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
  "HTML <li> for a todo item row in a list view (item/ set)."

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
  "HTML <li> for a note-derived TODO (item/ set)."

(define todo/item/notes
  (lambda
    (fields)
    (let
      ((n (dict-get fields "notes")))
      (if (null? n) "" (as-str n)))))
  "An item notes string; "" when absent (item/ set)."

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
          (todo/tree/save
            "todo-item"
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
  "Save edited fields on an item; () when missing."

(define todo/item/set-state
  (lambda
    (id state)
    (todo/tree/set-field
      "todo-item"
      id
      "state"
      (as-str state))))
  "Set an item state field (item/ set)."

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
  "An item state string, deriving from done when absent (item/ set)."

(define todo/item/title
  (lambda (fields) (as-str (dict-get fields "title"))))
  "An item title string (item/ set)."

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
  "Items for list-id, treating a blank list_id as the inbox."

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
  "HTML <ul> of items per view."

(define todo/land-ping
  1)

(define todo/list
  (lambda () (todo/tree/rows "todo-item" 50)))
  "Most recent todo-item rows (up to 50)."

(define todo/list-default?
  (lambda
    (fields)
    (let
      ((d (dict-get fields "is_default")))
      (if (null? d) #f (string-eq d "true")))))
  "True when list fields mark it default."

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
  "HTML <ul> of the most recent todo items."

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
  "HTML <li> for a todo item row in a list view."

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
  "HTML <option> elements over all lists with selected."

(define todo/list-title
  (lambda (fields) (as-str (dict-get fields "title"))))
  "A list title string."

(define todo/list/all
  (lambda () (todo/tree/rows "todo-list" 50)))
  "All todo-list rows (up to 50) (list/ set)."

(define todo/list/create
  (lambda
    (title)
    (todo/tree/create
      "todo-list"
      (dict-set*
        "{}"
        (list
          "title"
          (as-str title)
          "description"
          ""
          "is_default"
          "false")))))
  "Create a todo-list with the given title (list/ set)."

(define todo/list/default?
  (lambda
    (fields)
    (let
      ((d (dict-get fields "is_default")))
      (if (null? d) #f (string-eq d "true")))))
  "True when list fields mark it default (list/ set)."

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
                (seq (todo/tree/del "todo-list" lid) "ok")))))))))
  "Delete a list and its items; guards the default list (list/ set)."

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
  "Delete items belonging to lid from the items list (list/ set)."

(define todo/list/ensure-inbox
  (lambda
    ()
    (let
      ((found (todo/list/find-default)))
      (if
        (null? found)
        (todo/tree/create
          "todo-list"
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
  "Default list id, creating an Inbox when none exists (list/ set)."

(define todo/list/find-default
  (lambda () (todo/list/find-default-in (todo/list/all))))
  "The default list, or () (list/ set)."

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
  "Find the default Inbox list in xs (list/ set)."

(define todo/list/get
  (lambda (id) (todo/tree/get "todo-list" id)))
  "Fields of a todo-list by id; () when absent (list/ set)."

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
  "HTML nav <li> for one list with an open/count ratio."

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
  "HTML <option> elements over all lists with selected (list/ set)."

(define todo/list/title
  (lambda (fields) (as-str (dict-get fields "title"))))
  "A list title string (list/ set)."

(define todo/lists
  (lambda () (todo/tree/rows "todo-list" 50)))
  "All todo-list rows (up to 50)."

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
  "Main todo body HTML for a view, or the detail card when an id is present."

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
  "HTML <ul> navigation with view links, lists, and a new-list form."

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
  "HTML nav <li> link for a view key with active state."

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
  "HTML nav <li> for one list with an open/count ratio."

(define todo/next-state
  (lambda
    (cur)
    (outline/todo-next-in todo/states (as-str cur))))
  "The next state after cur in todo/states."

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
  "HTML <li> for a note-derived TODO."

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
  "True when a state string is not a closed state."

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
  "Full /todos page: ensure inbox, handle POST, render HTML."

(define todo/partial
  (lambda
    ()
    (seq
      (todo/list/ensure-inbox)
      (render-html (todo/ui/main (todo/ui/view))))))
  "Render the todo main partial for an HTMX request."

(define todo/ratio
  (lambda
    (items)
    (let
      ((n (todo/count-all-in items))
        (open (todo/count-open-in items)))
      (string-append (as-str (- n open)) "/" (as-str n)))))
  "String "done/total" for items."

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
          (todo/tree/save
            "todo-item"
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
  "Save edited fields on an item; () when missing."

(define todo/seqtest
  (lambda () 7))
  "Test helper returning 7."

(define todo/set-state
  (lambda
    (id state)
    (todo/tree/set-field
      "todo-item"
      id
      "state"
      (as-str state))))
  "Set an item state field."

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
  "HTML <option> elements over todo/states with selected."

(define todo/state-options-all
  (lambda
    (selected)
    (todo/state-options-from
      selected
      (append todo/states (list "ARCHIVED")))))
  "HTML <option> elements over states plus ARCHIVED."

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
  "HTML <option> elements over the given states with selected."

(define todo/state/archived?
  (lambda (s) (string-eq (as-str s) "ARCHIVED")))
  "True when a state string is ARCHIVED (state/ set)."

(define todo/state/list
  (list "" "TODO" "DONE" "WAITING" "CANCELLED"))

(define todo/state/next
  (lambda
    (cur)
    (outline/todo-next-in todo/state/list (as-str cur))))
  "The next state after cur in todo/state/list (state/ set)."

(define todo/state/columns-for-list
  (lambda
    (list-id)
    "The state set (list of state strings) for a todo-list: the list's own
     `columns` field when present and non-empty, else the global default
     todo/state/list. THE per-list override kanban columns and the /todos
     state machine both read through, so a list with custom columns cycles
     only those states everywhere."
    (let
      ((lf (get (quote todo-list) (as-str list-id))))
      (if
        (null? lf)
        todo/state/list
        (let
          ((cols (dict-get lf "columns")))
          (if
            (null? cols)
            todo/state/list
            (let
              ((cs (as-str cols)))
              (if
                (string-eq cs "")
                todo/state/list
                (json-parse cs)))))))))

(define todo/state/todo-next-in
  (lambda
    (xs cur)
    "State following cur within the state list xs; xs's own head on wrap (last
     -> first) and on a not-found cur, so the cycle never leaves the list's
     columns. (A per-set wrap, distinct from outline/todo-next-in which
     hardcodes the global default state list's head.)"
    (if
      (null? xs)
      "TODO"
      (todo/state/todo-next-in-at xs cur (as-str (car xs))))))

(define todo/state/todo-next-in-at
  (lambda
    (xs cur head)
    "Accumulating: find cur in xs, return the next; wrap to head at the end or
     on absent."
    (if
      (null? xs)
      head
      (if
        (string-eq (as-str (car xs)) cur)
        (if
          (null? (cdr xs))
          head
          (as-str (car (cdr xs))))
        (todo/state/todo-next-in-at (cdr xs) cur head)))))

(define todo/state/next-for-list
  (lambda
    (cur list-id)
    "Next state after cur within list-id's own state set (custom columns when
     present, else the global default). Wraps around: last column -> first of
     that same set."
    (todo/state/todo-next-in (todo/state/columns-for-list list-id) (as-str cur))))

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
  "True when a state string is not a closed state (state/ set)."

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
  "HTML <option> elements over todo/state/list with selected (state/ set)."

(define todo/state/options-all
  (lambda
    (selected)
    (todo/state/options-from
      selected
      (append todo/state/list (list "ARCHIVED")))))
  "HTML <option> elements over states plus ARCHIVED (state/ set)."

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
  "HTML <option> elements over the given states with selected (state/ set)."

(define todo/state/options-for-list
  (lambda
    (selected list-id)
    "HTML options over list-id's own state set PLUS ARCHIVED, with selected
     marked — the detail <select> for an item in that list. Uses the list's
     custom columns when present, else the canonical options-all state set."
    (todo/state/options-from
      selected
      (append (todo/state/columns-for-list list-id) (list "ARCHIVED")))))

(define todo/states
  (list "" "TODO" "DONE" "WAITING" "CANCELLED"))

(define todo/toggle
  (lambda
    (id)
    (let
      ((row (todo/tree/get "todo-item" id)))
      (if
        (null? row)
        ()
        (todo/tree/set-field
          "todo-item"
          id
          "done"
          (if (todo/done? row) "false" "true"))))))
  "Flip an item done flag."

(define todo/tree/create
  (lambda
    (t fields)
    (let
      ((r (tree/upsert (node-path t "new") "datum" t fields)))
      (if
        (todo/tree/path-ok? r)
        (todo/tree/redash (todo/tree/tail r))
        r))))
  "Create a datum node; returns the new id."

(define todo/tree/del
  (lambda
    (t id)
    (seq (tree/delete (node-path t (as-str id))) id)))
  "Delete a datum node by id; returns the id."

(define todo/tree/get
  (lambda
    (t id)
    (let
      ((n (tree/get (node-path t (as-str id)))))
      (if (null? n) (list) (dict-get n "body")))))
  "Fields of a datum node by id; () when absent."

(define todo/tree/insert-by-created
  (lambda
    (nj sorted)
    (if
      (null? sorted)
      (list nj)
      (if
        (string<?
          (as-str (dict-get nj "created_at"))
          (as-str (dict-get (car sorted) "created_at")))
        (cons nj sorted)
        (cons
          (car sorted)
          (todo/tree/insert-by-created nj (cdr sorted)))))))
  "Insert a node into a sorted list by created_at."

(define todo/tree/node->pair
  (lambda
    (nj)
    (cons
      (todo/tree/redash (todo/tree/tail (dict-get nj "path")))
      (dict-get nj "body"))))
  "Convert a tree node to an (id . fields) pair."

(define todo/tree/path-ok?
  (lambda
    (r)
    (if
      (null? r)
      #f
      (string-eq (substring (as-str r) 0 5) "data."))))
  "True when an upsert result path starts with "data."."

(define todo/tree/prefix
  (lambda
    (t)
    (let
      ((p (node-path t "")))
      (substring p 0 (- (string-length p) 1)))))
  "The path prefix for a type."

(define todo/tree/redash
  (lambda
    (h)
    (string-append
      (substring h 0 8)
      "-"
      (substring h 8 4)
      "-"
      (substring h 12 4)
      "-"
      (substring h 16 4)
      "-"
      (substring h 20 12))))
  "Reformat a trailing ID hex slice into dashed UUID form."

(define todo/tree/rows
  (lambda
    (t n)
    (list/map
      todo/tree/node->pair
      (tree/list (todo/tree/prefix t) 1 n))))
  "Rows of a type as (id . fields) pairs up to n."

(define todo/tree/save
  (lambda
    (t id fields)
    (seq
      (tree/upsert (node-path t (as-str id)) "datum" t fields)
      id)))
  "Upsert a datum node fields; returns the id."

(define todo/tree/set-field
  (lambda
    (t id f v)
    (let
      ((cur (todo/tree/get t id)))
      (if
        (null? cur)
        (list)
        (todo/tree/save
          t
          id
          (dict-set* cur (list f (as-str v))))))))
  "Set one field on a datum node and persist."

(define todo/tree/sort-by-created
  (lambda
    (nodes)
    (list/foldl
      (lambda (acc n) (todo/tree/insert-by-created n acc))
      (list)
      nodes)))
  "Sort nodes by created_at."

(define todo/tree/tail
  (lambda (p) (substring p (- (string-length p) 32) 32)))
  "Last 32 chars of a path (the id hex)."

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
  "HTML add form (or hint) for the given view/list (ui set)."

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
                                (todo/state/options-for-list state lid))
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
  "HTML detail/edit card for an item by id (ui set)."

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
  "Dispatch a POST /todos action to the matching handler (ui set)."

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
  "Main todo body HTML for a view, or the detail card when an id is present (ui set)."

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
  "HTML <ul> navigation with view links, lists, and a new-list form (ui set)."

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
  "HTML nav <li> link for a view key with active state (ui set)."

(define todo/ui/ratio
  (lambda
    (items)
    (let
      ((n (todo/item/count-all items))
        (open (todo/item/count-open items)))
      (string-append (as-str (- n open)) "/" (as-str n)))))
  "String "done/total" for items (ui set)."

(define todo/ui/view
  (lambda
    ()
    (let
      ((v (request-param "view")))
      (if (null? v) "everything" (as-str v)))))
  "The current view param, defaulting to "everything" (ui set)."

(define todo/view
  (lambda
    ()
    (let
      ((v (request-param "view")))
      (if (null? v) "everything" (as-str v)))))
  "The current view param, defaulting to "everything"."


(define todo/css
  (lambda
    ()
    "Render the todo component stylesheet (extracted from ui/theme)."
    (css/sheet
      (list
        (css/rule
          ".todo-layout"
          (list
            "display"
            "grid"
            "grid-template-columns"
            "12rem 1fr"
            "gap"
            ".75rem"))
        (css/rule
          ".todo-nav"
          (list
            "list-style"
            "none"
            "padding"
            "0"
            "margin"
            "0"))
        (css/rule ".todo-nav-item" (list "margin" ".25rem 0"))
        (css/rule
          ".todo-nav-item.is-active a"
          (list
            "font-weight"
            "700"
            "border-left"
            "3px solid #000"
            "padding-left"
            ".35rem"))
        (css/rule
          ".todo-new-list"
          (list
            "display"
            "flex"
            "flex-direction"
            "column"
            "gap"
            ".25rem"
            "margin-top"
            ".5rem"))
        (css/rule
          ".todo-new-list-input"
          (list
            "font-size"
            ".85rem"
            "min-height"
            "2rem"
            "padding"
            ".25rem"))
        (css/rule
          ".todo-box.st-TODO"
          (list "background" "var(--pending-bg)"))
        (css/rule
          ".todo-box.st-DONE"
          (list
            "background"
            "var(--done-bg)"
            "text-decoration"
            "line-through"))
        (css/rule
          ".todo-box.st-WAITING"
          (list "background" "var(--waiting-bg)"))
        (css/rule
          ".todo-box.st-CANCELLED"
          (list
            "background"
            "var(--cancelled-bg)"
            "color"
            "var(--archived-ink)"))
        (css/rule
          ".todo-box.st-ARCHIVED"
          (list
            "background"
            "var(--archived-bg)"
            "color"
            "var(--archived-ink)"
            "border-style"
            "dashed"))
        (css/rule
          "li.todo-item.is-archived .todo-title"
          (list
            "color"
            "var(--archived-ink)"
            "font-style"
            "italic"))
        (css/rule
          ".todo-from-note .todo-src"
          (list "font-size" ".8rem" "margin-left" ".35rem"))
        (css/rule
          ".todo-add-label"
          (list
            "display"
            "block"
            "font-weight"
            "bold"
            "margin"
            "0 0 .35rem"))
        (css/rule
          ".todo-add-row"
          (list
            "display"
            "flex"
            "gap"
            ".5rem"
            "align-items"
            "stretch"
            "flex-wrap"
            "wrap"))
        (css/rule
          ".todo-add-row input[type=text]"
          (list "flex" "1 1 12rem" "min-width" "0"))
        (css/rule
          ".todo-add-row button"
          (list
            "flex"
            "0 0 auto"
            "width"
            "auto"
            "min-width"
            "5rem"))
        (css/rule
          "ul.todo-list"
          (list
            "list-style"
            "none"
            "padding"
            ".25rem .5rem"
            "margin"
            "0 0 .75rem"))
        (css/rule
          "li.todo-item"
          (list
            "display"
            "flex"
            "align-items"
            "center"
            "gap"
            ".5rem"
            "padding"
            ".45rem 0"
            "border-bottom"
            "1px solid #c0c0c0"))
        (css/rule
          "li.todo-item:last-child"
          (list "border-bottom" "none"))
        (css/rule
          "li.todo-item form"
          (list "margin" "0" "display" "inline"))
        (css/rule
          "li.todo-item .todo-box"
          (list
            "font-family"
            "ui-monospace,Menlo,monospace"
            "min-width"
            "2.6rem"
            "padding"
            ".25rem .35rem"
            "width"
            "auto"))
        (css/rule
          "li.todo-item .todo-title"
          (list
            "flex"
            "1 1 auto"
            "min-width"
            "0"
            "word-break"
            "break-word"))
        (css/rule
          "li.todo-item.is-done .todo-title"
          (list
            "text-decoration"
            "line-through"
            "color"
            "var(--done-muted)"))
        (css/rule
          "li.todo-item .todo-del button, li.todo-item .todo-archive button"
          (list
            "width"
            "auto"
            "padding"
            ".25rem .45rem"
            "font-size"
            ".85rem"))
        (css/rule
          "li.todo-nav-list"
          (list
            "display"
            "flex"
            "align-items"
            "center"
            "gap"
            ".25rem"
            "flex-wrap"
            "wrap"))
        (css/rule
          "li.todo-nav-list > a"
          (list "flex" "1 1 auto" "min-width" "0"))
        (css/rule
          "li.todo-nav-list form.todo-list-del"
          (list "margin" "0" "display" "inline"))
        (css/rule
          "li.todo-nav-list .todo-list-del button"
          (list
            "width"
            "auto"
            "min-width"
            "1.6rem"
            "padding"
            ".1rem .3rem"
            "font-size"
            ".8rem"
            "line-height"
            "1.2"))
        (css/media
          "max-width:700px"
          (list
            (css/rule
              ".todo-layout"
              (list "grid-template-columns" "1fr"))))
      ))))
