;; package todo -- exported from live branch admin/personal
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
    todo/comments-init!
    todo/comment/add
    todo/comment/list
    todo/event/record
    todo/event/transition!
    todo/event/list
    todo/feed
    todo/item/acceptance
    todo/item/branch
    todo/item/set-acceptance
    todo/item/set-branch
    todo/recipes-default
    todo/recipes-for-list
    todo/recipe
    todo/recipe-set
    todo/flow-brief
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
            "false"
            "acceptance"
            ""
            "branch"
            ""))))))

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

(define todo/alist-get
  (lambda
    (alist k)
    (if
      (null? alist)
      (list)
      (if
        (string-eq (car (car alist)) k)
        (cdr (car alist))
        (todo/alist-get (cdr alist) k))))
  "bucket items for key k (or ())")

(define todo/alist-push
  (lambda
    (k v alist)
    (if
      (null? alist)
      (list (cons k (list v)))
      (if
        (string-eq (car (car alist)) k)
        (cons (cons k (cons v (cdr (car alist)))) (cdr alist))
        (cons (car alist) (todo/alist-push k v (cdr alist))))))
  "push v onto alist bucket k (pure, one pass over groups)")

(define todo/all-items
  (lambda () (todo/tree/rows "todo-item" 200)))

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

(define todo/browse
  (lambda
    (path)
    (filmstrip/browse-walk
      todo/browse-items
      (list (todo/browse-lists))
      path))
  "todo cascade: lists -> items. Item entries leaf to /todos?id=<item>\n(the only granularity /todos serves; lists drill to the item column).")

(define todo/browse-items
  (lambda
    (list-id)
    (list/map
      (lambda
        (row)
        (let
          ((f (cdr row)))
          (list
            (as-str (dict-get f "title"))
            (string-append "/todos?id=" (as-str (car row))))))
      (list/filter
        (lambda
          (row)
          (string-eq
            (as-str (dict-get (cdr row) "list_id"))
            list-id))
        (todo/item/all))))
  "Items of one list; each entry is a leaf to the item's /todos detail.")

(define todo/browse-lists
  (lambda
    ()
    (list/map
      (lambda
        (row)
        (let
          ((f (cdr row)))
          (list (as-str (dict-get f "title")) (as-str (car row)))))
      (todo/list/all)))
  "Column of todo lists: (title . list-id).")

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

(define todo/css
  (lambda
    ()
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
              (list "grid-template-columns" "1fr")))))))
  "Render the todo component stylesheet (extracted from ui/theme).")

(define todo/cycle
  (lambda
    (id)
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
            (todo/set-state
              id
              (todo/state/next-for-list cur list-id)))))))
  "Advance item id to its next state; () when missing. Cycles within the\n     item's OWN list's state set (custom columns when present, else the global\n     default) — BORGE-1786336755, so /todos exactly mirrors /kanban.")

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

(define todo/db/delete
  (lambda
    (type id)
    (let
      ((p (node-path type (as-str id))))
      (if
        (null? (tree/get p))
        ()
        (seq (tree/delete p) (as-str id))))))

(define todo/db/get
  (lambda
    (type id)
    (let
      ((n (tree/get (node-path type (as-str id)))))
      (if (null? n) () (dict-get n "body")))))

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

(define todo/db/node-row
  (lambda
    (node)
    (cons
      (todo/db/path-id (dict-get node "path"))
      (dict-get node "body"))))

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

(define todo/db/prefix
  (lambda
    (type)
    (let
      ((p (node-path type "")))
      (substring p 0 (- (string-length p) 1)))))

(define todo/db/rows
  (lambda
    (type limit)
    (map
      todo/db/node-row
      (tree/list-kind (todo/db/prefix type) "datum" limit))))

(define todo/db/sort-by-created
  (lambda
    (ns)
    (if
      (null? ns)
      (list)
      (todo/db/insert-by-created
        (car ns)
        (todo/db/sort-by-created (cdr ns))))))

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

(define todo/db/update-field
  (lambda
    (type id field value)
    (let
      ((fields (todo/db/get type (as-str id))))
      (if
        (null? fields)
        ()
        (todo/db/update type id (dict-set fields field value))))))

(define todo/dbg-scrape
  (lambda
    ()
    (let
      ((n (notes/doc/list)))
      (let
        ((row (todo/notes-cache-row)))
        (let
          ((f
            (dict-set*
              (if (null? row) "{}" (cdr row))
              (list
                "dbg_notes"
                (as-str (count n))
                "dbg_root"
                (notes/tree/root)
                "dbg_todos"
                (as-str (count (notes/outline/all-todos)))
                "dbg_who"
                (whoami)))))
          (seq (update (quote todo-cache) (car row) f) "dbg-done"))))))

(define todo/delete
  (lambda (id) (todo/tree/del "todo-item" id)))

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
                  ((acceptance (todo/item/acceptance fields))
                    (branch (todo/item/branch fields)))
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
                              (html/p*
                                (html/attr "class" "muted")
                                (html/join
                                  (list
                                    (html/strong "Branch: ")
                                    (html/code branch))))
                              (if
                                (string-eq acceptance "")
                                ()
                                (html/div
                                  (html/attr "class" "todo-acceptance")
                                  (html/join
                                    (list
                                      (html/p*
                                        (html/attr "class" "muted")
                                        "Acceptance")
                                      (html/pre (html/text acceptance))))))
                              (html/div
                                ""
                                (html/join
                                  (list
                                    (html/button (html/attr "type" "submit") "Save")
                                    " "
                                    (html/a (string-append "/todos?view=" view) "Cancel"))))))) (todo/ui/feed-html id view))))))))))))))

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
  (lambda (id) (todo/tree/get "todo-item" id)))

(define todo/get-list
  (lambda (id) (todo/tree/get "todo-list" id)))

(define todo/group-by-list
  (lambda
    (items inbox-id)
    (list/foldl
      (lambda
        (acc item)
        (let
          ((fl (as-str (todo/item/list-id (cdr item)))))
          (todo/alist-push
            (if (string-eq fl "") inbox-id fl)
            item
            acc)))
      (list)
      items))
  "group item rows by their list_id (empty -> inbox) in ONE pass; replaces the per-list 200-item refilters that cost ~7k ticks per nav entry")

(define todo/handle-post
  (lambda
    ()
    (let
      ((action (as-str (request-param "action"))))
      (if
        (string-eq action "comment")
        (let
          ((c (request-param "comment")))
          (if
            (or (null? c) (string-eq (as-str c) ""))
            ()
            (todo/comment/add (request-param "id") (as-str c))))
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
                      (todo/add (todo/ensure-inbox) (request-param "title"))))))))))))))

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
            "false"
            "acceptance"
            ""
            "branch"
            ""))))))

(define todo/item/all
  (lambda () (todo/tree/rows "todo-item" 200)))

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
            (todo/item/set-state
              id
              (todo/state/next-for-list cur list-id))))))))

(define todo/item/delete
  (lambda (id) (todo/tree/del "todo-item" id)))

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
  (lambda (id) (todo/tree/get "todo-item" id)))

(define todo/item/html
  (lambda
    (view)
    (if
      (string-eq view "from-notes")
      (html/ul
        (html/attr "class" "card todo-list")
        (todo/notes-block "from-notes"))
      (if
        (string-eq view "archived")
        (html/ul
          (html/attr "class" "card todo-list")
          (list/foldl
            (lambda
              (acc row)
              (string-append acc (todo/item/list-li row view)))
            ""
            (list/take (todo/item/archived) 80)))
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
                  (list/take (todo/item/active (todo/item/all)) 80)))
              (html/h2 "From notes")
              (html/ul
                (html/attr "class" "card todo-list")
                (todo/notes-block "everything"))))
          (html/ul
            (html/attr "class" "card todo-list")
            (list/foldl
              (lambda
                (acc row)
                (string-append acc (todo/item/list-li row view)))
              ""
              (list/take
                (todo/item/active (todo/item/for-list view))
                150))))))))

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
          ((prev (todo/item/state fields))
            (next (as-str state))
            (lid
              (if
                (string-eq (as-str list-id) "")
                (todo/list/ensure-inbox)
                (as-str list-id))))
          (seq
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
                  next
                  "list_id"
                  lid)))
            (todo/event/transition! id prev next)))))))

(define todo/item/set-state
  (lambda
    (id state)
    "Set card state, recording a todo-event when it actually changes. Returns
     the event id (real transition) or () (no change / missing card). The
     choke point: todo/item/cycle, archive, the item save, and future move
     writers all record transitions through here."
    (let
      ((cur (todo/tree/get "todo-item" (as-str id))))
      (if
        (null? cur)
        (list)
        (let
          ((prev (todo/item/state cur))
            (next (as-str state)))
          (seq
            (todo/tree/set-field "todo-item" (as-str id) "state" next)
            (todo/event/transition! id prev next)))))))

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

;; ------------------------------------------------------------------
;; assignment (claim) + priority/queue — ported from gregor's
;; todo-assignment branch (0f1b9a4 + b7059e6) into master's single
;; libs/todo/lib.pp. Provides the claim/queue prims the workstreams
;; orchestration protocol depends on. (The kanban make-card/move-card/
;; stale + /kanban-review functions from 133b228/89e5ddf already live in
;; libs/kanban/lib.pp and are not duplicated here.)
;; ------------------------------------------------------------------

(define todo/item/assign
  (lambda
    (id user)
    "Assign item id to user (empty user clears). Returns id, or () when the item is missing."
    (let
      ((row (todo/tree/get "todo-item" (as-str id))))
      (if
        (null? row)
        ()
        (seq
          (todo/tree/set-field "todo-item" (as-str id) "assignee" (as-str user))
          (as-str id))))))

(define todo/item/assignee
  (lambda
    (fields)
    "The assignee field of item fields; empty when absent."
    (let
      ((a (dict-get fields "assignee")))
      (if
        (null? a)
        ""
        (as-str a)))))

(define todo/item/unassign
  (lambda
    (id)
    "Clear item id's assignee."
    (todo/item/assign id "")))

(define todo/item/for-user
  (lambda
    (user)
    "All items assigned to user, as rows."
    (let
      ((u (as-str user)))
      (list/filter
        (lambda
          (row)
          (string-eq (todo/item/assignee (cdr row)) u))
        (todo/item/all)))))

(define todo/item/priority
  (lambda
    (fields)
    "The priority field of item fields (P0..P3); empty when absent."
    (let
      ((p (dict-get fields "priority")))
      (if
        (null? p)
        ""
        (as-str p)))))

(define todo/item/set-priority
  (lambda
    (id p)
    "Set item id's priority (P0 highest .. P3, empty clears). Returns id, or () when the item is missing."
    (let
      ((row (todo/tree/get "todo-item" (as-str id))))
      (if
        (null? row)
        ()
        (seq
          (todo/tree/set-field "todo-item" (as-str id) "priority" (as-str p))
          (as-str id))))))

(define todo/item/prio-rank
  (lambda
    (fields)
    "Numeric work rank of the priority field: P0=0 .. P3=3, unknown or empty = 4."
    (let
      ((p (todo/item/priority fields)))
      (if
        (string-eq p "P0")
        0
        (if
          (string-eq p "P1")
          1
          (if
            (string-eq p "P2")
            2
            (if
              (string-eq p "P3")
              3
              4)))))))

(define todo/item/prio-pos
  (lambda
    (fields)
    "The position field as a number; 0 when absent or non-numeric."
    (let
      ((pos (dict-get fields "position")))
      (if
        (null? pos)
        0
        (let
          ((s (as-str pos)))
          (if
            (string-eq s "")
            0
            (parse-int s)))))))

(define todo/item/prio-le?
  (lambda
    (a b)
    "True when row a sorts before or equal to row b by (priority rank, position, id)."
    (let
      ((ra (todo/item/prio-rank (cdr a)))
        (rb (todo/item/prio-rank (cdr b))))
      (if
        (< ra rb)
        #t
        (if
          (> ra rb)
          #f
          (let
            ((pa (todo/item/prio-pos (cdr a)))
              (pb (todo/item/prio-pos (cdr b))))
            (if
              (< pa pb)
              #t
              (if
                (> pa pb)
                #f
                (string<=? (car a) (car b))))))))))

(define todo/item/prio-insert
  (lambda
    (sorted row)
    "Insert row into a prio-ordered list (insertion sort step)."
    (if
      (null? sorted)
      (list row)
      (if
        (todo/item/prio-le? row (car sorted))
        (cons row sorted)
        (cons (car sorted) (todo/item/prio-insert (cdr sorted) row))))))

(define todo/item/prio-sorted
  (lambda
    (rows)
    "Rows ordered by priority rank, then position, then id (insertion sort; no sort prim in-image)."
    (list/foldl todo/item/prio-insert () rows)))

(define todo/item/queue
  (lambda
    (user)
    "Open items in work order (priority, then position, then id). Empty user = all assignees."
    (let
      ((u (as-str user)))
      (todo/item/prio-sorted
        (list/filter
          (lambda
            (row)
            (if
              (string-eq u "")
              #t
              (string-eq (todo/item/assignee (cdr row)) u)))
          (todo/item/active (todo/item/all)))))))

;; acceptance + branch fields — borge/workstreams.borg acceptance. These ride
;; the todo-item datum like notes/assignee: todo/add + todo/item/add seed them
;; (default ""), the save writers preserve them via dict-set* on the existing
;; fields, and kanban's dict-set* reorders/renames also preserve them. New
;; cards created by libs/kanban/make-card (a fresh body) simply have "" until
;; set with todo/item/set-acceptance / todo/item/set-branch. Setters write via
;; tree/set-field — a content edit, NOT a state transition, so they deliberately
;; do NOT mint a todo-event.
(define todo/item/acceptance
  (lambda
    (fields)
    "The acceptance criteria body of item fields; empty when absent."
    (let
      ((a (dict-get fields "acceptance")))
      (if (null? a) "" (as-str a)))))

(define todo/item/branch
  (lambda
    (fields)
    "The target branch for the work; empty when absent."
    (let
      ((b (dict-get fields "branch")))
      (if (null? b) "" (as-str b)))))

(define todo/item/set-acceptance
  (lambda
    (id text)
    "Set card id's acceptance criteria (empty clears). Returns id, or () when the item is missing."
    (let
      ((row (todo/tree/get "todo-item" (as-str id))))
      (if
        (null? row)
        ()
        (seq
          (todo/tree/set-field "todo-item" (as-str id) "acceptance" (as-str text))
          (as-str id))))))

(define todo/item/set-branch
  (lambda
    (id branch)
    "Set card id's target branch (empty clears). Returns id, or () when the item is missing."
    (let
      ((row (todo/tree/get "todo-item" (as-str id))))
      (if
        (null? row)
        ()
        (seq
          (todo/tree/set-field "todo-item" (as-str id) "branch" (as-str branch))
          (as-str id))))))

;; ------------------------------------------------------------------
;; comment + event datums (card detail v2 feed) — borge/workstreams.borg
;; card-comments. Two datum types seeded on load (guarded, idempotent):
;;   todo-comment (item_id, body) — author + timestamps come from the ROW
;;     METADATA (node author / created_at), not body fields, per spec: the
;;     datum itself is deliberately minimal.
;;   todo-event   (item_id, from_state, to_state) — auto-written by every
;;     real card-state transition. Wired (task 3): todo/set-state,
;;     todo/item/set-state, todo/save, todo/item/save all route through
;;     todo/event/transition! (record only when the state actually changed),
;;     so todo/cycle, todo/item/cycle, archive, the /todos detail form, and
;;     the future move writers all append events for free. The kanban
;;     move-card renumber (a position-reorder in libs/kanban) writes state
;;     directly via tree/upsert and is intentionally out of scope here (the
;;     kanban lib is off-limits this loop).
;; todo/feed interleaves events + comments chronologically (Linear-style
;; activity feed) for card detail v2.
;; ------------------------------------------------------------------

(define todo/comments-init!
  (lambda
    ()
    "Ensure the todo-comment / todo-event datum types exist. Guarded so a
     re-load (load-library) is safe and a non-admin load degrades to a no-op
     value instead of an error; boot seeds these onto system/main as admin."
    (seq
      (if
        (null? (type-fields (quote todo-comment)))
        (deftype (quote todo-comment) (quote item_id) (quote body))
        "todo-comment ok")
      (if
        (null? (type-fields (quote todo-event)))
        (deftype (quote todo-event) (quote item_id) (quote from_state) (quote to_state))
        "todo-event ok"))))

(define todo/datum/rows-for-item
  (lambda
    (type item limit)
    "Raw node-JSONs (id/path/kind/type/body/author/created_at/updated_at/rank)
     of all `type` datums whose body.item_id === item, in store order (callers
     that need chronology sort by created_at — see todo/datum/insert-by-ts)."
    (list/filter
      (lambda
        (n)
        (let
          ((b (dict-get n "body")))
          (let
            ((i (if (null? b) () (dict-get b "item_id"))))
            (and (not (null? i)) (string-eq (as-str i) (as-str item))))))
      (tree/list (todo/tree/prefix type) 1 limit))))

(define todo/datum/feed-entry
  (lambda
    (kind n)
    "One feed entry dict (JSON string) from a datum node-JSON: kind + id + ts
     + author from row metadata, plus the payload fields the kind needs."
    (let
      ((b (dict-get n "body")))
      (if
        (string-eq kind "comment")
        (dict-set*
          "{}"
          (list
            "kind" "comment"
            "id" (as-str (dict-get n "id"))
            "ts" (as-str (dict-get n "created_at"))
            "author" (as-str (dict-get n "author"))
            "text" (as-str (dict-get b "body"))))
        (dict-set*
          "{}"
          (list
            "kind" "event"
            "id" (as-str (dict-get n "id"))
            "ts" (as-str (dict-get n "created_at"))
            "author" (as-str (dict-get n "author"))
            "from_state" (as-str (dict-get b "from_state"))
            "to_state" (as-str (dict-get b "to_state"))))))))

(define todo/datum/insert-by-ts
  (lambda
    (e sorted)
    "Insert feed entry e into a ts-ascending sorted list (oldest first). The
     node created_at strings are PG timestamptz text, uniformly shaped, so a
     lexicographic compare IS chronological order."
    (if
      (null? sorted)
      (list e)
      (if
        (string<?
          (as-str (dict-get e "ts"))
          (as-str (dict-get (car sorted) "ts")))
        (cons e sorted)
        (cons
          (car sorted)
          (todo/datum/insert-by-ts e (cdr sorted)))))))

(define todo/datum/merge-by-ts
  (lambda
    (a b)
    "Merge two feed-entry lists (ANY order) into one ts-ascending list.
     Walks both through the sorted insetter from an empty list, so the
     result is chronological regardless of the helpers' own ordering.
     (rows-for-item feeds NEWEST-first; this rebuilds oldest-first.)"
    (list/foldl
      (lambda (acc e) (todo/datum/insert-by-ts e acc))
      (list)
      (append a b))))

(define todo/comment/add
  (lambda
    (item body)
    "Append a comment datum to card item; returns the comment id. Author +
     ts ride the row metadata (session user / created_at)."
    (todo/comments-init!)
    (as-str
      (todo/tree/create
        "todo-comment"
        (dict-set*
          "{}"
          (list "item_id" (as-str item) "body" (as-str body)))))))

(define todo/event/record
  (lambda
    (item from to)
    "Record a card state transition datum (item_id, from_state, to_state);
     returns the event id. Private-ish: user code should call
     todo/event/transition!, which records ONLY on a real change."
    (todo/comments-init!)
    (as-str
      (todo/tree/create
        "todo-event"
        (dict-set*
          "{}"
          (list
            "item_id" (as-str item)
            "from_state" (as-str from)
            "to_state" (as-str to)))))))

(define todo/event/transition!
  (lambda
    (id from to)
    "Record a todo-event for card id when the state actually changed
     (from != to); no-op otherwise. Returns the event id, or () when the
     states are equal. THE choke point every card-state writer calls:
     todo/set-state, todo/item/set-state, todo/save, todo/item/save."
    (if
      (string-eq (as-str from) (as-str to))
      ()
      (todo/event/record id from to))))

(define todo/comment/list
  (lambda
    (item)
    "All comment node-JSONs for card item, newest first."
    (todo/datum/rows-for-item "todo-comment" item 200)))

(define todo/event/list
  (lambda
    (item)
    "All event node-JSONs for card item, newest first."
    (todo/datum/rows-for-item "todo-event" item 200)))

(define todo/feed
  (lambda
    (item)
    "Chronological activity feed for card item: todo-event transitions and
     todo-comment rows interleaved by created_at, oldest first. Returns a
     list of feed-entry dicts (JSON strings) tagged kind = comment | event."
    (todo/comments-init!)
    (todo/datum/merge-by-ts
      (list/map
        (lambda (n) (todo/datum/feed-entry "comment" n))
        (todo/comment/list item))
      (list/map
        (lambda (n) (todo/datum/feed-entry "event" n))
        (todo/event/list item)))))

;; seed the datum types on load (idempotent; no-ops when already present)
(todo/comments-init!)

;; ------------------------------------------------------------------
;; column recipes (workstreams.borg column-recipes) — the bridge between
;; RECORD and EXECUTOR. The workstream list datum carries an optional
;; `recipes` JSON dict field mapping COLUMN NAME -> instruction text (the
;; per-column agent recipe). Reads fall back to the built-in default family
;; (todo/recipes-default) so every list gets actionable instructions even
;; before anyone configures it; recipe-set writes the explicit dict.
;; NAMING NOTE (orchestration task 6 resolution): the plan originally named
;; these kanban/recipes-for-list + kanban/recipe-set + kanban/recipe, but the
;; kanban lib (libs/kanban/lib.pp) is OFF-LIMITS this loop (Do-NOT rule), and
;; master's convention is one clean namespace per family file. These live in
;; the todo family as todo/recipes-* (they operate on the todo-list datum);
;; the shared column taxonomy they cover is SPEC/CODE/LOGIC-REVIEW/UI-REVIEW/
;; HUMAN-REVIEW/DONE/PARKED (borge/work-triage.borg board).
;; ------------------------------------------------------------------

(define todo/recipes-default
  (list
    (cons "SPEC"
      "You are the SPEC author for Pricklypear (Nopales/OCaml Lisp OS; the borge book is the spec and the source of truth — code drift means code is wrong). The card above is your work item; its acceptance criteria + target branch ride on the datum. Write/update the borge .borg chapter for it. Finish with `borge drift` clean and the spec committed on the target branch. Then move the card to CODE and post a comment: what you specified + how the reader verifies it.")
    (cons "CODE"
      "You are the IMPLEMENTER for Pricklypear (OCaml 5.4 / Nopales, dune build, borge spec). Implement the card above against its acceptance criteria, on its target branch, in /home/roerick/dev/wyo.tech/pricklypear. Build/test: eval $(opam env --switch=poohstack --set-switch); dune build; dune runtest (lisp/common at least). After .borg edits run `borge drift`. Live-smoke on the target host before write-back. When done, move the card to LOGIC-REVIEW and post a comment: what shipped + how you verified it.")
    (cons "LOGIC-REVIEW"
      "You are the LOGIC REVIEWER for Pricklypear. Review the card's work on its target branch against its acceptance criteria: read the change, check borge drift + build + the stated verification, judge logic. PASS -> move the card to HUMAN-REVIEW and comment your verdict + evidence. FAIL -> move it BACK to CODE with a comment listing exactly what to fix.")
    (cons "UI-REVIEW"
      "You are the UI REVIEWER for Pricklypear. Exercise the card's work on its target branch for interaction + rendering against its acceptance criteria (load the routes, click through). PASS -> move the card to HUMAN-REVIEW and comment. FAIL -> move it BACK to CODE with a comment of what to fix.")
    (cons "HUMAN-REVIEW"
      "ROERICK GATE — do NOT move this card yourself. It is ready for the human gate on its target branch; acceptance criteria are on the datum. Post any notes as comments; leave the state in HUMAN-REVIEW. Only the human passes this gate.")
    (cons "DONE"
      "This card is done. Nothing to execute. If it landed here by accident, move it back to the correct column and comment why.")
    (cons "PARKED"
      "This card is deliberately parked. Do not work it. If the park looks stale, comment and leave it — only the human unparks.")
    (cons "TODO"
      "Intake: place the card into the correct pipeline column — SPEC if it needs a spec/decision before code, CODE if it is executable now — set priority if useful, assign an owner if known, then comment where you placed it and why."))
  "The built-in default recipe family per shared column (work-triage taxonomy).
     todo/recipe falls back to these when a list has no explicit recipe for a
     column. Columns without an entry fall to the generic fallback at read
     time, so a brief always has instructions.")

(define todo/recipes-for-list
  (lambda
    (list-id)
    (let
      ((lf (todo/tree/get "todo-list" (as-str list-id))))
      (if
        (null? lf)
        (list)
        (let
          ((r (dict-get lf "recipes")))
          (if
            (null? r)
            (list)
            (list/map
              (lambda
                (k)
                (cons
                  (as-str k)
                  (as-str (dict-get r (as-str k)))))
              (dict-keys r)))))))
  "The list's EXPLICIT `recipes` dict as an assoc list ((column . text) ...),
     stored on the todo-list datum; () when the list is missing or has no
     recipes field. Defaults are NOT mixed in — use todo/recipe for the
     effective (fallback-aware) text of one column.")

(define todo/recipe
  (lambda
    (list-id column)
    (let
      ((col (as-str column))
        (explicit (assoc (as-str column) (todo/recipes-for-list list-id))))
      (if
        (if
          explicit
          (not (string-eq (as-str (cdr explicit)) ""))
          #f)
        (as-str (cdr explicit))
        (let
          ((dflt (assoc col todo/recipes-default)))
          (if
            dflt
            (as-str (cdr dflt))
            (string-append
              col
              ": work the card above on Pricklypear — follow its acceptance criteria and target branch; build/test with dune; verify; then advance the card one column and comment what you did + evidence."))))))
  "The EFFECTIVE recipe instruction text for column in list-id: the list's
     explicit recipe when set (non-empty), else the default family entry for
     that column (todo/recipes-default), else a generic fallback. Never
     returns () — every column in every list produces an instruction, so a
     flow/brief always has text to hand the executor.")

(define todo/recipes-drop
  (lambda
    (json key)
    (let
      ((k (as-str key)))
      (list/foldl
        (lambda
          (acc jk)
          (if
            (string-eq (as-str jk) k)
            acc
            (dict-set acc (as-str jk) (as-str (dict-get json (as-str jk))))))
        "{}"
        (dict-keys json))))
  "Rebuild a JSON dict without key (empty/absent dict stays {}). Helper for
     todo/recipe-set removal — there is no dict-del prim, so drop = fold.")

(define todo/recipe-set
  (lambda
    (list-id column text)
    (let
      ((lf (todo/tree/get "todo-list" (as-str list-id))))
      (if
        (null? lf)
        "no such list"
        (let
          ((cur (dict-get lf "recipes")))
          (let
            ((base (if (null? cur) "" (as-str cur)))
              (col (as-str column))
              (tx (as-str text)))
            (let
              ((newr
                (if
                  (string-eq tx "")
                  (todo/recipes-drop base col)
                  (dict-set base col tx))))
              (let
                ((body (dict-set* lf (list "recipes" newr))))
                (seq
                  (tree/upsert
                    (node-path (quote todo-list) (as-str list-id))
                    "datum" "todo-list" body)
                  (as-str list-id)))))))))
  "Set the recipe instruction text for column on list-id (content edit of the
     LIST datum via tree/upsert — NOT a card state transition, so no
     todo-event is minted). Empty text REMOVES that column's entry. Returns
     the list id, or a no-such-list message.")

;; ------------------------------------------------------------------
;; flow/brief — the executor brief (borge/workstreams.borg
;; claim-brief-protocol). todo/flow-brief renders card + column recipe +
;; context (repo path, build/tests invocation, target host, write-back eval
;; snippet) into ONE self-contained text prompt: the plan file for a ralph
;; loop, the task string for a subagent. NAMING NOTE (orchestration task 7
;; resolution): the plan named this flow/brief; a `libs/flow` package would be
;; a NEW family file, and the Do-NOT rule limits this loop to todo/standup/
;; workstreams libs — so it lives in the todo family as todo/flow-brief.
;; ------------------------------------------------------------------

(define todo/flow-brief
  (lambda
    (card-id)
    (let
      ((fields (todo/tree/get "todo-item" (as-str card-id))))
      (if
        (null? fields)
        ()
        (let
          ((cid (as-str card-id))
            (title (todo/item/title fields))
            (state (todo/item/state fields))
            (list-id (todo/item/list-id fields))
            (acceptance (todo/item/acceptance fields))
            (branch (todo/item/branch fields))
            (assignee (todo/item/assignee fields)))
          (let
            ((lname
              (if
                (string-eq list-id "")
                "(no list)"
                (let
                  ((lf (todo/tree/get "todo-list" list-id)))
                  (if
                    (null? lf)
                    list-id
                    (todo/list/title lf))))))
            (string-append
              "PRICKLYPEAR EXECUTOR BRIEF — claim/brief protocol (borge/workstreams.borg)\n"
              "Self-contained task: work the card below on the laptop repo. Follow the column\n"
              "recipe. Do not promote to prod. YOU DO NOT WRITE BACK — pp-flow owns the card\n"
              "transition and will move the card one column + post your evidence comment when\n"
              "you exit 0. Your only job is the work + a clear EVIDENCE section.\n\n== CARD ==\n"
              "  card id:    "
              cid
              "\n  title:      "
              title
              "\n  state:      "
              state
              "\n  list:       "
              lname
              " ("
              list-id
              ")\n  assignee:   "
              assignee
              "\n  acceptance: "
              (if (string-eq acceptance "") "(none)" acceptance)
              "\n  branch:     "
              (if (string-eq branch "") "(default)" branch)
              "\n\n== COLUMN RECIPE ("
              state
              ") ==\n"
              (todo/recipe list-id state)
              "\n\n== CONTEXT ==\n"
              "  repo:    /home/roerick/dev/wyo.tech/pricklypear\n"
              "  build:   eval $(opam env --switch=poohstack --set-switch); dune build\n"
              "  tests:   dune runtest lisp; dune runtest common\n"
              "  spec:    the .borg book is the spec + source of truth; run `borge drift` after spec edits\n\n"
              "== EVIDENCE (what pp-flow will post on your behalf) ==\n"
              "  End your output with an EVIDENCE block:\n"
              "  EVIDENCE: <what you did + how you verified it (build/tests/live smoke;\n"
              "  keep under ~200 chars, will be trimmed to 500 by the driver)>.\n"
              "  pp-flow moves the card ONE column and posts this as your comment on the\n"
              "  card thread. If the recipe demands a different destination than ONE\n"
              "  forward column (e.g. a review FAIL stays/backtracks), write:\n"
              "  DESTINATION: <COLUMN> — and pp-flow will use that instead."))))))
  "Render card + column recipe + context into ONE self-contained text prompt
     (the executor brief). Includes the card title/state/list/acceptance/
     branch/assignee, the effective column recipe (todo/recipe), repo + build
     context, and the write-back eval snippet (advance one column + post a
     comment). Returns () for a missing card. For a ralph loop this IS the
     plan file; for a subagent it's the task.")

(define todo/items-for
  (lambda
    (lid all-items inbox-id)
    (list/filter
      (lambda
        (row)
        (let
          ((fl (as-str (todo/item/list-id (cdr row)))))
          (if
            (string-eq fl lid)
            #t
            (if (string-eq fl "") (string-eq lid inbox-id) #f))))
      all-items))
  "pure: rows of one list from a prefetched items list (replaces per-list DB re-fetch)")

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
  (lambda () (todo/tree/rows "todo-item" 50)))

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
  (lambda () (todo/tree/rows "todo-list" 50)))

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
                (seq (todo/tree/del "todo-list" lid) "ok")))))))))

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
  (lambda (id) (todo/tree/get "todo-list" id)))

(define todo/list/nav-li
  (lambda
    (view row)
    (let
      ((id (car row)) (f (cdr row)))
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
              (todo/list/title f))
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
                      "×"))))))))))
  "nav entry for one list: title link + delete form. Ratio counts DROPPED — per-list ratios cost a dict-get JSON parse per item (~1.5k ticks each) and blew the 400k route budget; the list page shows its own counts.")

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
  (lambda () (todo/tree/rows "todo-list" 50)))

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

(define todo/notes-block
  (lambda
    (view)
    (let
      ((row (todo/notes-cache-row)))
      (let
        ((fields (if (null? row) () (cdr row))))
        (if
          (todo/notes-cache-fresh? fields 900)
          (todo/notes-cache-html fields view)
          (seq
            (todo/notes-cache-request)
            (todo/notes-cache-html fields view))))))
  "HTML <li> block for a view from the scrape cache. Never computes inline: serves cached (possibly stale) HTML and asks the jobs runner to refresh. The inline scrape cost 8-78s per render and blew route budgets.")

(define todo/notes-cache-fresh?
  (lambda
    (fields ttl)
    (if
      (null? fields)
      #f
      (let
        ((ts (dict-get fields "ts")))
        (if
          (null? ts)
          #f
          (let
            ((age (- (now) (parse-int (as-str ts)))))
            (if (> age ttl) #f #t))))))
  "whether the scrape cache is within ttl seconds")

(define todo/notes-cache-html
  (lambda
    (fields view)
    (let
      ((k
        (if
          (string-eq view "everything")
          "html_open"
          "html_all")))
      (let
        ((v (dict-get fields k)))
        (if (null? v) "" (as-str v)))))
  "cached <li> block for a view")

(define todo/notes-cache-refresh
  (lambda
    ()
    (let
      ((todos (notes/outline/all-todos)))
      (let
        ((h-all
            (string-join
              (list/map
                (lambda (t) (todo/item/note-li t "from-notes"))
                todos)
              ""))
          (h-open
            (string-join
              (list/map
                (lambda (t) (todo/item/note-li t "everything"))
                (list/filter
                  (lambda
                    (t)
                    (todo/state/open? (car (cdr (cdr (cdr t))))))
                  todos))
              "")))
        (let
          ((row (todo/notes-cache-row))
            (fields
              (dict-set*
                "{}"
                (list
                  "key"
                  "notes-todos"
                  "ts"
                  (as-str (now))
                  "req"
                  (as-str (now))
                  "html_all"
                  h-all
                  "html_open"
                  h-open))))
          (seq
            (if
              (null? row)
              (create (quote todo-cache) fields)
              (update (quote todo-cache) (car row) fields))
            "refreshed")))))
  "recompute the notes-todo scrape once; O(n) string-join (was O(n^2) fold). Runs as a JOB (unbudgeted), never on the request path.")

(define todo/notes-cache-request
  (lambda
    ()
    (let
      ((row (todo/notes-cache-row)))
      (let
        ((fields (if (null? row) () (cdr row))))
        (let
          ((req
            (if
              (null? fields)
              0
              (let
                ((r (dict-get fields "req")))
                (if (null? r) 0 (parse-int (as-str r)))))))
          (if
            (> (- (now) req) 120)
            (seq
              (if
                (null? row)
                (create
                  (quote todo-cache)
                  (dict-set*
                    "{}"
                    (list
                      "key"
                      "notes-todos"
                      "req"
                      (as-str (now))
                      "ts"
                      "0"
                      "html_all"
                      ""
                      "html_open"
                      "")))
                (update
                  (quote todo-cache)
                  (car row)
                  (dict-set* (cdr row) (list "req" (as-str (now))))))
              (submit-job "todo/notes-cache-refresh" ""))
            "already-requested")))))
  "ask the UNBUDGETED jobs runner to refresh the scrape; bounded to one request/120s so renders never compute inline")

(define todo/notes-cache-row
  (lambda
    ()
    (let
      ((rs
        (find-rows (quote todo-cache) "key" "notes-todos" 1)))
      (if (null? rs) () (car rs))))
  "cache row for the notes-todo scrape (or ())")

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
          ((prev (todo/item-state fields))
            (next (as-str state))
            (lid
              (if
                (string-eq (as-str list-id) "")
                (todo/ensure-inbox)
                (as-str list-id))))
          (seq
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
                  next
                  "list_id"
                  lid)))
            (todo/event/transition! id prev next)))))))

(define todo/seqtest
  (lambda () 7))

(define todo/set-state
  (lambda
    (id state)
    "Set card state, recording a todo-event when it actually changes. Returns
     the event id (real transition) or () (no change / missing card). The
     choke point: todo/cycle, archive, the save writer, and future move
     writers all record transitions through here."
    (let
      ((cur (todo/tree/get "todo-item" (as-str id))))
      (if
        (null? cur)
        (list)
        (let
          ((prev (todo/item-state cur))
            (next (as-str state)))
          (seq
            (todo/tree/set-field "todo-item" (as-str id) "state" next)
            (todo/event/transition! id prev next)))))))

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

(define todo/state/columns-for-list
  (lambda
    (list-id)
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
              (if (string-eq cs "") todo/state/list (json-parse cs))))))))
  "The state set (list of state strings) for a todo-list: the list's own\n     `columns` field when present and non-empty, else the global default\n     todo/state/list. THE per-list override kanban columns and the /todos\n     state machine both read through, so a list with custom columns cycles\n     only those states everywhere.")

(define todo/state/list
  (list "" "TODO" "DONE" "WAITING" "CANCELLED"))

(define todo/state/next
  (lambda
    (cur)
    (outline/todo-next-in todo/state/list (as-str cur))))

(define todo/state/next-for-list
  (lambda
    (cur list-id)
    (todo/state/todo-next-in
      (todo/state/columns-for-list list-id)
      (as-str cur)))
  "Next state after cur within list-id's own state set (custom columns when\n     present, else the global default). Wraps around: last column -> first of\n     that same set.")

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

(define todo/state/options-for-list
  (lambda
    (selected list-id)
    (todo/state/options-from
      selected
      (append
        (todo/state/columns-for-list list-id)
        (list "ARCHIVED"))))
  "HTML options over list-id's own state set PLUS ARCHIVED, with selected\n     marked — the detail <select> for an item in that list. Uses the list's\n     custom columns when present, else the canonical options-all state set.")

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

(define todo/state/todo-next-in
  (lambda
    (xs cur)
    (if
      (null? xs)
      "TODO"
      (todo/state/todo-next-in-at xs cur (as-str (car xs)))))
  "State following cur within the state list xs; xs's own head on wrap (last\n     -> first) and on a not-found cur, so the cycle never leaves the list's\n     columns. (A per-set wrap, distinct from outline/todo-next-in which\n     hardcodes the global default state list's head.)")

(define todo/state/todo-next-in-at
  (lambda
    (xs cur head)
    (if
      (null? xs)
      head
      (if
        (string-eq (as-str (car xs)) cur)
        (if (null? (cdr xs)) head (as-str (car (cdr xs))))
        (todo/state/todo-next-in-at (cdr xs) cur head))))
  "Accumulating: find cur in xs, return the next; wrap to head at the end or\n     on absent.")

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

(define todo/tree/create
  (lambda
    (t fields)
    (let
      ((r (tree/upsert (node-path t "new") "datum" t fields)))
      (if
        (todo/tree/path-ok? r)
        (todo/tree/redash (todo/tree/tail r))
        r))))

(define todo/tree/del
  (lambda
    (t id)
    (seq (tree/delete (node-path t (as-str id))) id)))

(define todo/tree/get
  (lambda
    (t id)
    (let
      ((n (tree/get (node-path t (as-str id)))))
      (if (null? n) (list) (dict-get n "body")))))

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

(define todo/tree/node->pair
  (lambda
    (nj)
    (cons
      (todo/tree/redash (todo/tree/tail (dict-get nj "path")))
      (dict-get nj "body"))))

(define todo/tree/path-ok?
  (lambda
    (r)
    (if
      (null? r)
      #f
      (string-eq (substring (as-str r) 0 5) "data."))))

(define todo/tree/prefix
  (lambda
    (t)
    (let
      ((p (node-path t "")))
      (substring p 0 (- (string-length p) 1)))))

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

(define todo/tree/rows
  (lambda
    (t n)
    (list/map
      todo/tree/node->pair
      (tree/list (todo/tree/prefix t) 1 n))))

(define todo/tree/save
  (lambda
    (t id fields)
    (seq
      (tree/upsert (node-path t (as-str id)) "datum" t fields)
      id)))

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

(define todo/tree/sort-by-created
  (lambda
    (nodes)
    (list/foldl
      (lambda (acc n) (todo/tree/insert-by-created n acc))
      (list)
      nodes)))

(define todo/tree/tail
  (lambda (p) (substring p (- (string-length p) 32) 32)))

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

;; ------------------------------------------------------------------
;; card detail v2: interleaved activity feed + comment composer.
;; borge/workstreams.borg card-comments / card-detail-v2. todo/feed gives
;; chronological entries; these renderers turn them into <li>s and the
;; composer POSTs action=comment back to the SAME /todos detail page
;; (no route fork - the spec's card-detail-v2 shape). Author + timestamp
;; ride the row metadata (todo/datum/feed-entry).
;; ------------------------------------------------------------------

(define todo/ui/feed-item-html
  (lambda
    (e)
    "One feed <li> from a feed entry dict (JSON): events render as a
     state-arrow line, comments as their body text. Oldest-first order
     comes from todo/feed."
    (let
      ((kind (as-str (dict-get e "kind")))
        (author (as-str (dict-get e "author")))
        (ts (as-str (dict-get e "ts"))))
      (let
        ((stamp
          (html/span
            (html/attr "class" "muted todo-feed-stamp")
            (html/text (string-append author " @ " ts)))))
        (if
          (string-eq kind "event")
          (html/li
            (html/attr "class" "todo-feed-item todo-feed-event")
            (html/join
              (list
                stamp
                (html/span
                  (html/attr "class" "todo-feed-arrow")
                  (html/text
                    (string-append
                      (as-str (dict-get e "from_state"))
                      " -> "
                      (as-str (dict-get e "to_state"))))))))
          (html/li
            (html/attr "class" "todo-feed-item todo-feed-comment")
            (html/join
              (list
                stamp
                (html/p (html/text (as-str (dict-get e "text"))))))))))))

(define todo/ui/feed-html
  (lambda
    (id view)
    "The card-detail v2 activity section: an h2 + <ul> of the interleaved
     comment/event feed, then a comment composer. Plain POST action=comment
     to /todos (same shape as the Save form) — todo/page posts the comment
     then redirects back to the current view."
    (let
      ((entries (todo/feed id)))
      (let
        ((items
          (if
            (null? entries)
            (html/p* (html/attr "class" "muted") "No activity yet.")
            (html/ul
              (html/attr "class" "todo-feed")
              (html/join
                (list/map
                  (lambda (e) (todo/ui/feed-item-html e))
                  entries))))))
        (html/div
          (html/attr "class" "todo-feed-section")
          (html/join
            (list
              (html/h2 (html/text "Activity"))
              items
              (html/form
                "POST"
                "/todos"
                (html/attrs
                  (list
                    (html/attr "class" "todo-comment-form")))
                (html/join
                  (list
                    (html/hidden "action" "comment")
                    (html/hidden "id" id)
                    (html/hidden "view" view)
                    (html/label (html/attr "for" "comment") "Comment")
                    (html/textarea
                      "comment"
                      (html/attrs
                        (list
                          (html/attr "id" "comment")
                          (html/attr "rows" "3")
                          (html/attr "class" "todo-comment-input")))
                      "")
                    (html/div
                      ""
                      (html/button (html/attr "type" "submit") "Comment"))))))))))))

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
                  ((acceptance (todo/item/acceptance fields))
                    (branch (todo/item/branch fields)))
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
                              (html/p*
                                (html/attr "class" "muted")
                                (html/join
                                  (list
                                    (html/strong "Branch: ")
                                    (html/code branch))))
                              (if
                                (string-eq acceptance "")
                                ()
                                (html/div
                                  (html/attr "class" "todo-acceptance")
                                  (html/join
                                    (list
                                      (html/p*
                                        (html/attr "class" "muted")
                                        "Acceptance")
                                      (html/pre (html/text acceptance))))))
                              (html/div
                                ""
                                (html/join
                                  (list
                                    (html/button (html/attr "type" "submit") "Save")
                                    " "
                                    (html/a (string-append "/todos?view=" view) "Cancel"))))))) (todo/ui/feed-html id view))))))))))))))

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
        ((string-eq action "comment")
          (let
            ((c (request-param "comment")))
            (if
              (or (null? c) (string-eq (as-str c) ""))
              ()
              (todo/comment/add (request-param "id") (as-str c)))))
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
        (items (todo/item/all))
        (inbox (todo/list/ensure-inbox)))
      (let
        ((arch-n
          (todo/item/count-all
            (list/filter
              (lambda
                (row)
                (todo/state/archived? (todo/item/state (cdr row))))
              items))))
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
  "nav: ONE items fetch (archived count only); lists render without per-list ratios — dict-get JSON parses per item blew the route budget")

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
