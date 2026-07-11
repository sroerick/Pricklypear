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

(define outline/set-todo
  (lambda (n todo)
    (outline/node* (outline/id n) (outline/title n) (outline/body n) todo (outline/kids n))))

(define outline/set-kids
  (lambda (n kids)
    (outline/node* (outline/id n) (outline/title n) (outline/body n) (outline/todo n) kids)))

(define outline/add-child
  (lambda (n child)
    (outline/set-kids n (cons child (outline/kids n)))))

;; fresh id generator (simple counter-based; good enough for UI)
(define outline/fresh-id
  (lambda ()
    (string-append "n" (as-str (string-length (as-str (list)))))))

;; --- tree traversal: find, update, delete by id ---

(define outline/find
  (lambda (tree id)
    (if (string-eq (outline/id tree) id)
        tree
        (outline/find-in-kids (outline/kids tree) id))))

(define outline/find-in-kids
  (lambda (kids id)
    (if (null? kids)
        ()
        (let ((found (outline/find (car kids) id)))
          (if (null? found)
              (outline/find-in-kids (cdr kids) id)
              found)))))

(define outline/update
  (lambda (tree id f)
    (if (string-eq (outline/id tree) id)
        (f tree)
        (outline/set-kids tree
          (outline/update-kids (outline/kids tree) id f)))))

(define outline/update-kids
  (lambda (kids id f)
    (if (null? kids)
        (list)
        (cons (outline/update (car kids) id f)
          (outline/update-kids (cdr kids) id f)))))

;; delete node by id (returns tree without it)
(define outline/delete
  (lambda (tree id)
    (if (string-eq (outline/id tree) id)
        ()  ; deleting root = nil (shouldn't happen)
        (outline/set-kids tree
          (outline/delete-from-kids (outline/kids tree) id)))))

(define outline/delete-from-kids
  (lambda (kids id)
    (if (null? kids)
        (list)
        (if (string-eq (outline/id (car kids)) id)
            (cdr kids)
            (cons (outline/delete (car kids) id)
              (outline/delete-from-kids (cdr kids) id))))))

;; add child to node by id
(define outline/add-child-by-id
  (lambda (tree id child)
    (outline/update tree id
      (lambda (n) (outline/add-child n child)))))

;; insert sibling after node by id
(define outline/insert-sibling
  (lambda (tree id new-node)
    (outline/insert-sibling-kids tree id new-node)))

(define outline/insert-sibling-kids
  (lambda (tree id new-node)
    (if (string-eq (outline/id tree) id)
        tree  ; can't add sibling to root
        (outline/set-kids tree
          (outline/insert-sib-list (outline/kids tree) id new-node)))))

(define outline/insert-sib-list
  (lambda (kids id new-node)
    (if (null? kids)
        (list)
        (if (string-eq (outline/id (car kids)) id)
            (cons (car kids) (cons new-node (cdr kids)))
            (cons (outline/insert-sibling-kids (car kids) id new-node)
              (outline/insert-sib-list (cdr kids) id new-node))))))

;; promote: move node up one level (remove from parent, insert as sibling after parent)
;; demote: move node down one level (become child of previous sibling)
;; These are complex; v1 simplified versions:

;; move-up: swap with previous sibling
(define outline/move-up
  (lambda (tree id)
    (outline/move-up-kids tree id)))

(define outline/move-up-kids
  (lambda (tree id)
    (if (string-eq (outline/id tree) id)
        tree
        (outline/set-kids tree
          (outline/swap-prev (outline/kids tree) id)))))

(define outline/swap-prev
  (lambda (kids id)
    (if (null? kids)
        (list)
        (if (null? (cdr kids))
            kids
            (if (string-eq (outline/id (car (cdr kids))) id)
                (cons (car (cdr kids)) (cons (car kids) (cdr (cdr kids))))
                (cons (outline/move-up-kids (car kids) id)
                  (outline/swap-prev (cdr kids) id)))))))

;; move-down: swap with next sibling
(define outline/move-down
  (lambda (tree id)
    (outline/move-down-kids tree id)))

(define outline/move-down-kids
  (lambda (tree id)
    (if (string-eq (outline/id tree) id)
        tree
        (outline/set-kids tree
          (outline/swap-next (outline/kids tree) id)))))

(define outline/swap-next
  (lambda (kids id)
    (if (null? kids)
        (list)
        (if (string-eq (outline/id (car kids)) id)
            (if (null? (cdr kids))
                kids
                (cons (car (cdr kids)) (cons (car kids) (cdr (cdr kids)))))
            (cons (outline/move-down-kids (car kids) id)
              (outline/swap-next (cdr kids) id))))))

;; --- HTML with editor controls ---

(define outline/html-node
  (lambda (slug n)
    (let ((nid (outline/id n))
          (todo (outline/todo n))
          (body (outline/body n))
          (kids (outline/kids n)))
      (html/tag "details"
        (html/attrs
          (list
            (html/attr "class" "ol-node")
            (html/attr "open" "open")
            (html/attr "data-id" nid)))
        (html/join
          (list
            (html/tag "summary" (html/attr "class" "ol-sum")
              (html/join
                (list
                  (html/span (html/attr "class" "ol-fold") "")
                  (if (string-eq todo "")
                      ""
                      (html/span (html/attr "class" "ol-todo")
                        (html/text todo)))
                  (html/span
                    (html/attrs
                      (list
                        (html/attr "class" "ol-title")
                        (html/attr "contenteditable" "true")
                        (html/attr "data-id" nid)
                        (html/attr "data-field" "title")))
                    (html/text (outline/title n))))))
            (if (string-eq body "")
                ""
                (html/div
                  (html/attrs
                    (list
                      (html/attr "class" "ol-body")
                      (html/attr "contenteditable" "true")
                      (html/attr "data-id" nid)
                      (html/attr "data-field" "body")))
                  (html/text body)))
            (html/div (html/attr "class" "ol-kids")
              (list/foldl
                (lambda (acc k)
                  (string-append acc (outline/html-node slug k)))
                ""
                kids))
            (html/div (html/attr "class" "ol-actions")
              (html/join
                (list
                  (html/tag "button"
                    (html/attrs
                      (list
                        (html/attr "class" "ol-btn")
                        (html/attr "hx-post"
                          (string-append "/partials/notes-node-op?slug=" slug
                            "&op=add-child&node-id=" nid))
                        (html/attr "hx-target" "#outline")
                        (html/attr "hx-swap" "innerHTML")))
                    "+child")
                  (html/tag "button"
                    (html/attrs
                      (list
                        (html/attr "class" "ol-btn")
                        (html/attr "hx-post"
                          (string-append "/partials/notes-node-op?slug=" slug
                            "&op=add-sibling&node-id=" nid))
                        (html/attr "hx-target" "#outline")
                        (html/attr "hx-swap" "innerHTML")))
                    "+sib")
                  (html/tag "button"
                    (html/attrs
                      (list
                        (html/attr "class" "ol-btn")
                        (html/attr "hx-post"
                          (string-append "/partials/notes-node-op?slug=" slug
                            "&op=move-up&node-id=" nid))
                        (html/attr "hx-target" "#outline")
                        (html/attr "hx-swap" "innerHTML")))
                    "up")
                  (html/tag "button"
                    (html/attrs
                      (list
                        (html/attr "class" "ol-btn")
                        (html/attr "hx-post"
                          (string-append "/partials/notes-node-op?slug=" slug
                            "&op=move-down&node-id=" nid))
                        (html/attr "hx-target" "#outline")
                        (html/attr "hx-swap" "innerHTML")))
                    "down")
                  (html/tag "button"
                    (html/attrs
                      (list
                        (html/attr "class" "ol-btn ol-del")
                        (html/attr "hx-post"
                          (string-append "/partials/notes-node-op?slug=" slug
                            "&op=delete&node-id=" nid))
                        (html/attr "hx-target" "#outline")
                        (html/attr "hx-swap" "innerHTML")
                        (html/attr "hx-confirm" "Delete this node?")))
                    "del"))))))))))

(define outline/html
  (lambda (slug root)
    (html/div (html/attr "id" "outline")
      (outline/html-node slug root))))

(define outline/encode
  (lambda (root) (json-emit root)))

(define outline/decode
  (lambda (s)
    (if (null? s)
        (outline/node "root" "Untitled")
        (if (string-eq (as-str s) "")
            (outline/node "root" "Untitled")
            (json-parse (as-str s))))))
