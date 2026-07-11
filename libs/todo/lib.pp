;; Todo dogfood library — same text as in git and in the live system.
;; Install: (load-library "todo") or POST /api/recipe/todo

(deftype 'todo-item 'title 'done)

(define todo/add
  (lambda (title)
    (create 'todo-item
      (dict-set (dict-set "{}" "title" title) "done" "false"))))

(define todo/list
  (lambda () (rows 'todo-item 50)))

(define todo/item-li
  (lambda (row)
    (string-append
      "<li><strong>"
      (dict-get (cdr row) "title")
      "</strong></li>")))

(define todo/page
  (lambda ()
    (if (string-eq (request-method) "POST")
        (seq
          (todo/add (request-param "title"))
          (redirect "/todos"))
        (render-html
          (string-append
            "<h1>Todos</h1>"
            "<form method=\"POST\" action=\"/todos\" class=\"card\">"
            "<p><label>New item "
            "<input type=\"text\" name=\"title\" required placeholder=\"buy milk\">"
            "</label></p>"
            "<p><button type=\"submit\">Add</button></p></form>"
            "<ul class=\"card\">"
            (list/foldl
              (lambda (acc row) (string-append acc (todo/item-li row)))
              ""
              (todo/list))
            "</ul>"
            "<p class=\"muted\">"
            (str (count (todo/list)))
            " item(s)</p>")))))

(route "/todos" "todo/page")
