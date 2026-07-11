;; Todo dogfood library — same text as in git and in the live system.
;; Install: (load-library "todo") or POST /api/recipe/todo
;;
;; POST actions on /todos:
;;   action=add    + title=...
;;   action=toggle + id=...
;;   action=delete + id=...

(deftype 'todo-item 'title 'done)

(define todo/add
  (lambda (title)
    (create 'todo-item
      (dict-set (dict-set "{}" "title" title) "done" "false"))))

(define todo/list
  (lambda () (rows 'todo-item 50)))

(define todo/done?
  (lambda (fields)
    (string-eq (dict-get fields "done") "true")))

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
            (string-append
              "<li class=\"todo-item"
              (if done " is-done" "")
              "\">"
              "<form method=\"POST\" action=\"/todos\" class=\"todo-check\">"
              "<input type=\"hidden\" name=\"action\" value=\"toggle\">"
              "<input type=\"hidden\" name=\"id\" value=\"" id "\">"
              "<button type=\"submit\" class=\"todo-box\" title=\"toggle done\" "
              "aria-label=\"toggle\">"
              (if done "[x]" "[ ]")
              "</button></form>"
              "<span class=\"todo-title\">" title "</span>"
              "<form method=\"POST\" action=\"/todos\" class=\"todo-del\">"
              "<input type=\"hidden\" name=\"action\" value=\"delete\">"
              "<input type=\"hidden\" name=\"id\" value=\"" id "\">"
              "<button type=\"submit\" class=\"secondary\" title=\"delete\">del</button>"
              "</form>"
              "</li>")))))))

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
        (render-html
          (string-append
            "<h1>Todos</h1>"
            "<form method=\"POST\" action=\"/todos\" class=\"card todo-add\">"
            "<input type=\"hidden\" name=\"action\" value=\"add\">"
            "<label class=\"todo-add-label\" for=\"title\">New item</label>"
            "<div class=\"todo-add-row\">"
            "<input id=\"title\" type=\"text\" name=\"title\" required "
            "placeholder=\"buy milk\" autocomplete=\"off\">"
            "<button type=\"submit\">Add</button>"
            "</div></form>"
            "<ul class=\"card todo-list\">"
            (list/foldl
              (lambda (acc row) (string-append acc (todo/item-li row)))
              ""
              (todo/list))
            "</ul>"
            "<p class=\"muted\">"
            (str (count (todo/list)))
            " item(s) · click [ ] to check off</p>")))))

(route "/todos" "todo/page")
