;; package todo/list -- exported from live branch system/main
;; Sync: scripts/pp_sync.py pull|push

(define todo/list
  (lambda () (rows (quote todo-item) 50)))

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
                      "\195\151")))))))))))

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

