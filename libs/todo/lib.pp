;; Hierarchical names (optional depth under todo/):
;;   todo/item/*  row CRUD + filters
;;   todo/list/*  lists / inbox
;;   todo/state/* checklist states
;;   todo/ui/*    HTML + handlers
;; Public routes stay shallow: todo/page, todo/partial.
;; Basecamp-shaped lists + org scrape. Unified /todos.
;; Depends: std, outline (states), notes (for from-notes scrape).
;; Install: (load-library "todo") — seed also loads outline+notes.

(deftype 'todo-list 'title 'description 'is_default)
(deftype 'todo-item 'list_id 'title 'notes 'state 'due 'position 'done)

;; --- states (same vocabulary as outline) ---

;; Literal list so rehydrate does not depend on outline/todo-states load order.
;; ARCHIVED is terminal (not in the cycle); set via archive action from DONE.
(define todo/state/list (list "" "TODO" "DONE" "WAITING" "CANCELLED")
  "Checklist states cycle list (empty, TODO, DONE, WAITING, CANCELLED).")

(define todo/state/next
  (lambda (cur) (outline/todo-next-in todo/state/list (as-str cur)))
  "Next state after cur in todo/state/list (via outline/todo-next-in).")

;; Open = actionable work. DONE / CANCELLED / ARCHIVED / empty are closed.
(define todo/state/open?
  (lambda (s)
    (let ((t (as-str s)))
      (if (string-eq t "")
          #f
          (if (string-eq t "DONE")
              #f
              (if (string-eq t "CANCELLED")
                  #f
                  (if (string-eq t "ARCHIVED") #f #t))))))
  "True if state is actionable (not empty/DONE/CANCELLED/ARCHIVED).")

(define todo/state/archived?
  (lambda (s) (string-eq (as-str s) "ARCHIVED"))
  "True if state is ARCHIVED.")

(define todo/item/state
  (lambda (fields)
    (let ((s (dict-get fields "state")))
      (if (null? s)
          (let ((d (dict-get fields "done")))
            (if (null? d)
                "TODO"
                (if (string-eq d "true") "DONE" "TODO")))
          (as-str s))))
  "Read item state from fields (falls back to done bool then TODO or DONE).")

(define todo/item/title
  (lambda (fields)
    (as-str (dict-get fields "title")))
  "Item title string from fields.")

(define todo/item/list-id
  (lambda (fields)
    (let ((lid (dict-get fields "list_id")))
      (if (null? lid) "" (as-str lid))))
  "Item list_id string from fields (empty if missing).")

(define todo/item/notes
  (lambda (fields)
    (let ((n (dict-get fields "notes")))
      (if (null? n) "" (as-str n))))
  "Item notes string from fields.")

(define todo/item/due
  (lambda (fields)
    (let ((d (dict-get fields "due")))
      (if (null? d) "" (as-str d))))
  "Item due date string from fields.")

(define todo/item/get
  (lambda (id)
    (get 'todo-item id))
  "Fetch todo-item row by id.")

(define todo/item/save
  (lambda (id title notes due state list-id)
    (let ((fields (todo/item/get id)))
      (if (null? fields)
          ()
          (let ((lid
                  (if (string-eq (as-str list-id) "")
                      (todo/list/ensure-inbox)
                      (as-str list-id))))
            (update 'todo-item id
              (dict-set* fields
                (list
                  "title" (as-str title)
                  "notes" (as-str notes)
                  "due" (as-str due)
                  "state" (as-str state)
                  "list_id" lid)))))))
  "Update title/notes/due/state/list_id on an existing item.")

;; --- lists ---

(define todo/list/all
  (lambda () (rows 'todo-list 50))
  "Up to 50 todo-list rows.")

(define todo/list/title
  (lambda (fields) (as-str (dict-get fields "title")))
  "List title string from fields.")

(define todo/list/default?
  (lambda (fields)
    (let ((d (dict-get fields "is_default")))
      (if (null? d) #f (string-eq d "true"))))
  "True if list is_default flag equals the string true.")

(define todo/list/find-default
  (lambda ()
    (todo/list/find-default-in (todo/list/all)))
  "Find default or Inbox list row among all lists.")

(define todo/list/find-default-in
  (lambda (xs)
    (if (null? xs)
        ()
        (if (todo/list/default? (cdr (car xs)))
            (car xs)
            (let ((t (todo/list/title (cdr (car xs)))))
              (if (string-eq t "Inbox")
                  (car xs)
                  (todo/list/find-default-in (cdr xs)))))))
  "Scan xs for default or title Inbox; return row or empty.")

(define todo/list/ensure-inbox
  (lambda ()
    (let ((found (todo/list/find-default)))
      (if (null? found)
          (create 'todo-list
            (dict-set* "{}"
              (list
                "title" "Inbox"
                "description" "Default list"
                "is_default" "true")))
          (car found))))
  "Return default list id, creating Inbox if needed.")

(define todo/list/create
  (lambda (title)
    (create 'todo-list
      (dict-set* "{}"
        (list
          "title" (as-str title)
          "description" ""
          "is_default" "false"))))
  "Create a non-default list with title; return id.")

(define todo/list/get
  (lambda (id)
    (get 'todo-list id))
  "Fetch todo-list row by id.")

;; Delete every item on the list, then the list row. Refuses default/Inbox.
(define todo/list/delete-items
  (lambda (items lid)
    (if (null? items)
        ()
        (seq
          (if (string-eq (todo/item/list-id (cdr (car items))) (as-str lid))
              (todo/item/delete (car (car items)))
              ())
          (todo/list/delete-items (cdr items) lid))))
  "Delete items whose list_id matches lid (walk items).")

(define todo/list/delete
  (lambda (list-id)
    (let ((lid (as-str list-id)))
      (if (string-eq lid "")
          "missing"
          (let ((fields (todo/list/get lid)))
            (if (null? fields)
                "missing"
                (if (todo/list/default? fields)
                    "default"
                    (seq
                      (todo/list/delete-items (todo/item/all) lid)
                      (seq
                        (delete 'todo-list lid)
                        "ok"))))))))
  "Delete list and its items; refuses default. Returns ok, missing, or default.")

;; --- items ---

(define todo/item/all
  (lambda () (rows 'todo-item 200))
  "Up to 200 todo-item rows.")

(define todo/item/for-list
  (lambda (list-id)
    (let ((lid (as-str list-id))
          (inbox-id (todo/list/ensure-inbox)))
      (list/filter
        (lambda (row)
          (let ((fl (todo/item/list-id (cdr row))))
            (if (string-eq fl lid)
                #t
                (if (string-eq fl "")
                    (string-eq lid inbox-id)
                    #f))))
        (todo/item/all))))
  "Items belonging to list-id (empty list_id counts as inbox).")

(define todo/item/add
  (lambda (list-id title)
    (let ((lid (if (string-eq (as-str list-id) "")
                   (todo/list/ensure-inbox)
                   (as-str list-id))))
      (create 'todo-item
        (dict-set* "{}"
          (list
            "list_id" lid
            "title" (as-str title)
            "notes" ""
            "state" "TODO"
            "due" ""
            "position" "0")))))
  "Create TODO item on list-id (default inbox) with title.")

(define todo/item/set-state
  (lambda (id state)
    (update-field 'todo-item id "state" (as-str state)))
  "Set todo-item state field.")

(define todo/item/cycle
  (lambda (id)
    (let ((row (get 'todo-item id)))
      (if (null? row)
          ()
          ;; ARCHIVED is not in the cycle list; leave it alone on cycle.
          (let ((cur (todo/item/state row)))
            (if (todo/state/archived? cur)
                cur
                (todo/item/set-state id (todo/state/next cur)))))))
  "Advance item state; leaves ARCHIVED unchanged.")

(define todo/item/archive
  (lambda (id)
    (todo/item/set-state id "ARCHIVED"))
  "Set item state to ARCHIVED.")

(define todo/item/delete
  (lambda (id)
    (delete 'todo-item id))
  "Delete todo-item by id.")

(define todo/item/count-open
  (lambda (items)
    (list/foldl
      (lambda (acc row)
        (if (todo/state/open? (todo/item/state (cdr row)))
            (+ acc 1)
            acc))
      0
      items))
  "Count open (actionable) items in a row list.")

(define todo/item/count-all
  (lambda (items)
    (list/foldl (lambda (acc _) (+ acc 1)) 0 items))
  "Count rows in items list.")

(define todo/ui/ratio
  (lambda (items)
    (let ((n (todo/item/count-all items))
          (open (todo/item/count-open items)))
      (string-append (as-str (- n open)) "/" (as-str n))))
  "Done/total ratio string for items as closed/total.")

;; --- view key: everything | from-notes | <list-uuid> ---

(define todo/ui/view
  (lambda ()
    (let ((v (request-param "view")))
      (if (null? v) "everything" (as-str v))))
  "Current view key from request view param (default everything).")

;; --- HTML ---

(define todo/ui/nav-link
  (lambda (view key label extra)
    (html/li
      (html/attr "class"
        (if (string-eq view key) "todo-nav-item is-active" "todo-nav-item"))
      (html/a
        (string-append "/todos?view=" key)
        (string-append label
          (if (string-eq extra "") "" (string-append " " extra))))))
  "Sidebar nav li linking to /todos with view key.")

(define todo/item/active
  (lambda (items)
    (list/filter
      (lambda (row) (not (todo/state/archived? (todo/item/state (cdr row)))))
      items))
  "Filter out ARCHIVED items.")

(define todo/item/archived
  (lambda ()
    (list/filter
      (lambda (row) (todo/state/archived? (todo/item/state (cdr row))))
      (todo/item/all)))
  "All ARCHIVED items.")

(define todo/list/nav-li
  (lambda (view row)
    (let ((id (car row))
          (f (cdr row))
          (items (todo/item/active (todo/item/for-list (car row)))))
      (html/li
        (html/attr "class"
          (if (string-eq view id) "todo-nav-item is-active todo-nav-list"
              "todo-nav-item todo-nav-list"))
        (html/join
          (list
            (html/a
              (string-append "/todos?view=" id)
              (string-append (todo/list/title f)
                " (" (todo/ui/ratio items) ")"))
            (if (todo/list/default? f)
                ""
                (html/form "POST" "/todos"
                  (html/attrs
                    (list
                      (html/attr "class" "todo-list-del")
                      (html/attr "hx-post" "/todos")
                      (html/attr "hx-target" "#todo-main")
                      (html/attr "hx-swap" "innerHTML")
                      (html/attr "hx-confirm"
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
                        "×"))))))))))
  "Sidebar li for one list with ratio and optional delete.")

(define todo/ui/nav
  (lambda (view)
    (let ((lists (todo/list/all))
          (inbox (todo/list/ensure-inbox))
          (arch-n (todo/item/count-all (todo/item/archived))))
      (html/ul (html/attr "class" "todo-nav")
        (html/join
          (list
            (todo/ui/nav-link view "everything" "Everything" "")
            (todo/ui/nav-link view "from-notes" "From notes" "")
            (todo/ui/nav-link view "archived" "Archived"
              (string-append "(" (as-str arch-n) ")"))
            (list/foldl
              (lambda (acc row)
                (string-append acc (todo/list/nav-li view row)))
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
                    (html/button (html/attr "type" "submit") "+ list"))))))))))
  "Full todos sidebar: views, lists, and new-list form.")

(define todo/item/list-li
  (lambda (row view)
    (let ((id (car row))
          (fields (cdr row)))
      (let ((title (todo/item/title fields))
            (state (todo/item/state fields))
            (open (todo/state/open? (todo/item/state fields))))
        (html/li
          (html/attr "class"
            (if open
                "todo-item"
                (if (todo/state/archived? state)
                    "todo-item is-done is-archived"
                    "todo-item is-done")))
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
              (html/a
                (string-append "/todos?id=" id "&view=" view)
                (html/span (html/attr "class" "todo-title")
                  (html/text title)))
              ;; Archive DONE items (terminal state, hidden from active lists).
              (if (string-eq state "DONE")
                  (html/form "POST" "/todos"
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
                      "del"))))))))))
  "One list-item row: cycle, title link, archive, delete.")

(define todo/item/note-li
  (lambda (t view)
    ;; t = (slug nid title state)
    (let ((slug (car t))
          (nid (car (cdr t)))
          (title (car (cdr (cdr t))))
          (state (car (cdr (cdr (cdr t))))))
      (let ((open (todo/state/open? state)))
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
                  (html/text slug)))))))))
  "One from-notes row: cycle note TODO and link to note.")

(define todo/ui/add-form
  (lambda (view list-id)
    (if (string-eq view "from-notes")
        (html/p* (html/attr "class" "muted")
          "Mark nodes TODO in notes — they appear here. Toggle writes back.")
        (if (string-eq view "archived")
            (html/p* (html/attr "class" "muted")
              "Archived tasks. Unarchive from detail (set state back), or delete.")
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
  "Add-item form (or muted help for from-notes/archived).")

(define todo/item/html
  (lambda (view)
    (if (string-eq view "from-notes")
        (html/ul (html/attr "class" "card todo-list")
          (list/foldl
            (lambda (acc t)
              (string-append acc (todo/item/note-li t view)))
            ""
            (notes/outline/all-todos)))
        (if (string-eq view "archived")
            (html/ul (html/attr "class" "card todo-list")
              (list/foldl
                (lambda (acc row)
                  (string-append acc (todo/item/list-li row view)))
                ""
                (todo/item/archived)))
            (if (string-eq view "everything")
                (html/join
                  (list
                    (html/h2 "Lists")
                    (html/ul (html/attr "class" "card todo-list")
                      (list/foldl
                        (lambda (acc row)
                          (string-append acc (todo/item/list-li row view)))
                        ""
                        (todo/item/active (todo/item/all))))
                    (html/h2 "From notes")
                    (html/ul (html/attr "class" "card todo-list")
                      (list/foldl
                        (lambda (acc t)
                          (if (todo/state/open? (car (cdr (cdr (cdr t)))))
                              (string-append acc (todo/item/note-li t view))
                              acc))
                        ""
                        (notes/outline/all-todos)))))
                ;; real list id — hide archived from active lists
                (html/ul (html/attr "class" "card todo-list")
                  (list/foldl
                    (lambda (acc row)
                      (string-append acc (todo/item/list-li row view)))
                    ""
                    (todo/item/active (todo/item/for-list view))))))))
  "Main list HTML for view (everything/from-notes/archived/list id).")

(define todo/list/options
  (lambda (selected)
    (list/foldl
      (lambda (acc row)
        (let ((id (car row)))
          (let ((title (todo/list/title (cdr row))))
            (string-append acc
              (html/option id title (string-eq (as-str id) (as-str selected)))))))
      ""
      (todo/list/all)))
  "HTML option tags for all lists; selected id marked.")

(define todo/state/options
  (lambda (selected)
    (list/foldl
      (lambda (acc st)
        (let ((lab (if (string-eq st "") "(none)" st)))
          (string-append acc
            (html/option st lab (string-eq st (as-str selected))))))
      ""
      todo/state/list))
  "HTML options for todo/state/list.")

;; Detail form: cycle states + ARCHIVED (terminal).
(define todo/state/options-all
  (lambda (selected)
    (todo/state/options-from
      selected
      (append todo/state/list (list "ARCHIVED"))))
  "State options including terminal ARCHIVED.")

(define todo/state/options-from
  (lambda (selected states)
    (list/foldl
      (lambda (acc st)
        (let ((lab (if (string-eq st "") "(none)" st)))
          (string-append acc
            (html/option st lab (string-eq st (as-str selected))))))
      ""
      states))
  "HTML options from explicit states list.")

(define todo/ui/detail
  (lambda (id view)
    (let ((fields (todo/item/get id)))
      (if (null? fields)
          (html/div (html/attr "class" "card")
            (html/join
              (list
                (html/p* (html/attr "class" "err") "Item not found.")
                (html/a (string-append "/todos?view=" view) "← back"))))
          (let ((title (todo/item/title fields)))
            (let ((notes (todo/item/notes fields)))
              (let ((due (todo/item/due fields)))
                (let ((state (todo/item/state fields)))
                  (let ((lid (todo/item/list-id fields)))
                    (html/div (html/attr "class" "card todo-detail")
                      (html/join
                        (list
                          (html/p*
                            (html/attr "class" "muted")
                            (html/a
                              (string-append "/todos?view=" view)
                              "← back to list"))
                          (html/h2 (html/text title))
                          (html/form "POST" "/todos"
                            (html/attrs
                              (list
                                (html/attr "class" "todo-detail-form")))
                            (html/join
                              (list
                                (html/hidden "action" "save")
                                (html/hidden "id" id)
                                (html/hidden "view" view)
                                (html/label (html/attr "for" "title") "Title")
                                (html/text-input "title"
                                  (html/attrs
                                    (list
                                      (html/attr "id" "title")
                                      (html/attr "value" title)
                                      (html/attr "required" "required"))))
                                (html/label (html/attr "for" "state") "State")
                                (html/select "state" (html/attr "id" "state")
                                  ;; Include ARCHIVED in the detail picker even though
                                  ;; it is not part of the cycle list.
                                  (todo/state/options-all state))
                                (html/label (html/attr "for" "due")
                                  "Due (YYYY-MM-DD)")
                                (html/text-input "due"
                                  (html/attrs
                                    (list
                                      (html/attr "id" "due")
                                      (html/attr "value" due)
                                      (html/attr "placeholder" "2026-07-15"))))
                                (html/label (html/attr "for" "list_id") "List")
                                (html/select "list_id" (html/attr "id" "list_id")
                                  (todo/list/options lid))
                                (html/label (html/attr "for" "notes") "Notes")
                                (html/textarea "notes"
                                  (html/attrs
                                    (list
                                      (html/attr "id" "notes")
                                      (html/attr "rows" "8")
                                      (html/attr "class" "todo-notes")))
                                  notes)
                                (html/div ""
                                  (html/join
                                    (list
                                      (html/button (html/attr "type" "submit")
                                        "Save")
                                      " "
                                      (html/a
                                        (string-append "/todos?view=" view)
                                        "Cancel"))))))))))))))))))
  "Item detail card/form for id under view.")

(define todo/ui/main
  (lambda (view)
    ;; Detail only on GET ?id=… — cycle/save/archive POSTs also carry id for
    ;; the action target and must stay on the list view (not open detail).
    (let ((id-param (request-param "id"))
          (is-get (string-eq (request-method) "GET")))
      (if (if is-get
              (if (null? id-param) #f (not (string-eq (as-str id-param) "")))
              #f)
          (html/div (html/attr "id" "todo-main")
            (html/join
              (list
                (html/div (html/attr "class" "todo-layout")
                  (html/join
                    (list
                      (html/div (html/attr "class" "todo-side")
                        (todo/ui/nav view))
                      (html/div (html/attr "class" "todo-body")
                        (todo/ui/detail (as-str id-param) view))))))))
          (let ((list-id
                  (if (string-eq view "everything")
                      (todo/list/ensure-inbox)
                      (if (string-eq view "from-notes")
                          ""
                          (if (string-eq view "archived")
                              ""
                              view)))))
            (html/div (html/attr "id" "todo-main")
              (html/join
                (list
                  (html/div (html/attr "class" "todo-layout")
                    (html/join
                      (list
                        (html/div (html/attr "class" "todo-side")
                          (todo/ui/nav view))
                        (html/div (html/attr "class" "todo-body")
                          (html/join
                            (list
                              (todo/ui/add-form view list-id)
                              (todo/item/html view))))))))))))))
  "Layout: nav plus body (list or GET detail).")

(define todo/ui/handle-post
  (lambda ()
    (let ((action (as-str (request-param "action"))))
      (cond
        ((string-eq action "cycle")
         (todo/item/cycle (request-param "id")))
        ((string-eq action "delete")
         (todo/item/delete (request-param "id")))
        ((string-eq action "archive")
         (todo/item/archive (request-param "id")))
        ((string-eq action "add")
         (todo/item/add (request-param "list_id") (request-param "title")))
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
         (todo/item/add (todo/list/ensure-inbox)
           (request-param "title"))))))
  "Dispatch POST action (cycle/add/save/delete and friends).")

(define todo/page
  (lambda ()
    (seq
      (todo/list/ensure-inbox)
      (if (string-eq (request-method) "POST")
          (seq
            (todo/ui/handle-post)
            (let ((view
                    (let ((v (request-param "view")))
                      (if (null? v) "everything" (as-str v)))))
              ;; After any mutation (incl. save), return to the list — not detail.
              ;; Cycle/save POSTs carry id as the action target; that must not
              ;; open the detail page (GET ?id= is the only detail entry).
              (if (hx-request?)
                  (render-html (todo/ui/main view))
                  (redirect (string-append "/todos?view=" view)))))
          (html/page
            (list
              (html/h1 "To-dos")
              (html/p* (html/attr "class" "muted")
                "Lists (Basecamp) + notes scrape (org). Click a title for details.")
              (todo/ui/main (todo/ui/view)))))))
  "GET/POST /todos page handler.")

(define todo/partial
  (lambda ()
    (seq
      (todo/list/ensure-inbox)
      (render-html (todo/ui/main (todo/ui/view)))))
  "HTMX partial: render todo/ui/main for current view.")

(route "/todos" "todo/page")
(route "/partials/todo-list" "todo/partial")
