;; Pure outline trees (org-mode shaped). No DB.
;; Node = (id title body todo kids)  — kids is a list of nodes.
;; Load: (load-library "outline")

(define outline/id (lambda (n) (car n)))
(define outline/title (lambda (n) (car (cdr n))))
(define outline/body (lambda (n) (car (cdr (cdr n)))))
(define outline/todo (lambda (n) (car (cdr (cdr (cdr n))))))
(define outline/kids (lambda (n) (car (cdr (cdr (cdr (cdr n)))))))

(define outline/node
  (lambda (id title)
    (list (as-str id) (as-str title) "" "" (list))))

(define outline/node*
  (lambda (id title body todo kids)
    (list (as-str id) (as-str title) (as-str body) (as-str todo) kids)))

(define outline/set-title
  (lambda (n title)
    (outline/node* (outline/id n) title (outline/body n) (outline/todo n) (outline/kids n))))

(define outline/set-body
  (lambda (n body)
    (outline/node* (outline/id n) (outline/title n) body (outline/todo n) (outline/kids n))))

(define outline/set-kids
  (lambda (n kids)
    (outline/node* (outline/id n) (outline/title n) (outline/body n) (outline/todo n) kids)))

(define outline/add-child
  (lambda (n child)
    (outline/set-kids n (cons child (outline/kids n)))))

(define outline/html-node
  (lambda (n)
    (let ((todo (outline/todo n))
          (body (outline/body n))
          (kids (outline/kids n)))
      (html/tag "details"
        (html/attrs
          (list
            (html/attr "class" "ol-node")
            (html/attr "open" "open")
            (html/attr "data-id" (outline/id n))))
        (html/join
          (list
            (html/tag "summary" (html/attr "class" "ol-sum")
              (html/join
                (list
                  (if (string-eq todo "")
                      ""
                      (html/span (html/attr "class" "ol-todo")
                        (html/text todo)))
                  (html/text (outline/title n)))))
            (if (string-eq body "")
                ""
                (html/div (html/attr "class" "ol-body")
                  (html/text body)))
            (html/div (html/attr "class" "ol-kids")
              (list/foldl
                (lambda (acc k) (string-append acc (outline/html-node k)))
                ""
                kids))))))))

(define outline/html
  (lambda (root)
    (html/div (html/attr "class" "ol-root")
      (outline/html-node root))))

(define outline/encode
  (lambda (root) (json-emit root)))

(define outline/decode
  (lambda (s)
    (if (null? s)
        (outline/node "root" "Untitled")
        (if (string-eq (as-str s) "")
            (outline/node "root" "Untitled")
            (json-parse (as-str s))))))
