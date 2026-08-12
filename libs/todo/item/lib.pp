;; package todo/item -- exported from live branch system/main
;; Sync: scripts/pp_sync.py pull|push

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
            "false"))))))

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
          ((cur (todo/item/state row)))
          (if
            (todo/state/archived? cur)
            cur
            (todo/item/set-state id (todo/state/next cur))))))))

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

(define todo/item/set-state
  (lambda
    (id state)
    (todo/tree/set-field
      "todo-item"
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

