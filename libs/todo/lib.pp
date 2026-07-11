;; Todo dogfood library — same text as in git and in the live system.
;; Install: (load-library "todo") or POST /api/recipe/todo
;;
;; POST actions on /todos:
;;   action=add    + title=...
;;   action=toggle + id=...
;;   action=delete + id=...
;;
;; HTML via std html/* helpers (not raw tag soup).

(deftype 'todo-item 'title 'done)

(define todo/add
  (lambda (title)
    (create 'todo-item
      (dict-set (dict-set "{}" "title" title) "done" "false"))))

(define todo/list
  (lambda () (rows 'todo-item 50)))

(define todo/done?
  (lambda (fields)
    (let ((d (dict-get fields "done")))
      (if (null? d) false (string-eq d "true")))))

(define todo/toggle
  (lambda (id)
    (let ((row (get 'todo-item id)))
      (if (null? row)
          ()
          (update-field 'todo-item id "done"
            (if (todo/done? row) "false" "true"))))))

(define todo/delete
  (lambda (id)
    (delete 'todo-item id)))

(define todo/item-li
  (lambda (row)
    (let ((id (car row)))
      (let ((fields (cdr row)))
        (let ((title (dict-get fields "title")))
          (let ((done (todo/done? fields)))
            (html/li
              (html/attrs
                (list
                  (html/attr "class"
                    (if done "todo-item is-done" "todo-item"))))
              (html/join
                (list
                  (html/form "POST" "/todos" (html/attr "class" "todo-check")
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
                  (html/span (html/attr "class" "todo-title")
                    (html/text title))
                  (html/form "POST" "/todos" (html/attr "class" "todo-del")
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

(define todo/page
  (lambda ()
    (if (string-eq (request-method) "POST")
        (let ((action (request-param "action")))
          (seq
            (if (null? action)
                (todo/add (request-param "title"))
                (if (string-eq action "toggle")
                    (todo/toggle (request-param "id"))
                    (if (string-eq action "delete")
                        (todo/delete (request-param "id"))
                        (todo/add (request-param "title")))))
            (redirect "/todos")))
        (html/page
          (list
            (html/h1 "Todos")
            (html/form "POST" "/todos"
              (html/attr "class" "card todo-add")
              (html/join
                (list
                  (html/hidden "action" "add")
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
                        (html/button (html/attr "type" "submit") "Add")))))))
            (html/ul (html/attr "class" "card todo-list")
              (list/foldl
                (lambda (acc row) (string-append acc (todo/item-li row)))
                ""
                (todo/list)))
            (html/p*
              (html/attr "class" "muted")
              (string-append
                (str (count (todo/list)))
                " item(s) · click [ ] to check off")))))))

(route "/todos" "todo/page")
