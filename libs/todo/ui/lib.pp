;; package todo/ui -- exported from live branch system/main
;; Sync: scripts/pp_sync.py pull|push

(define todo/ui/add-form
  (lambda
    (view list-id)
    (if
      (string-eq view "from-notes")
      (html/p*
        (html/attr "class" "muted")
        "Mark nodes TODO in notes \226\128\148 they appear here. Toggle writes back.")
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
              (html/a
                (string-append "/todos?view=" view)
                "\226\134\144 back"))))
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
                            "\226\134\144 back to list"))
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

