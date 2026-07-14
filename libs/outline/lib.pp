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

;; Outline checklist states. Empty = not a todo. Cycle: "" → TODO → DONE → "".
;; (Legacy WAITING/CANCELLED map to TODO on next cycle via todo-next-in miss.)
(define outline/todo-states
  (list "" "TODO" "DONE"))

(define outline/todo-next
  (lambda (cur)
    (outline/todo-next-in outline/todo-states (as-str cur))))

(define outline/todo-next-in
  (lambda (xs cur)
    (if (null? xs)
        "TODO"
        (if (string-eq (as-str (car xs)) cur)
            (if (null? (cdr xs)) (as-str (car outline/todo-states))
                (as-str (car (cdr xs))))
            (outline/todo-next-in (cdr xs) cur)))))

(define outline/todo-open?
  (lambda (s)
    (let ((t (as-str s)))
      (if (string-eq t "")
          #f
          (if (string-eq t "DONE")
              #f
              (if (string-eq t "CANCELLED")
                  #f
                  (if (string-eq t "ARCHIVED") #f #t)))))))

(define outline/archived?
  (lambda (s)
    (string-eq (as-str s) "ARCHIVED")))

;; Terminal state (not in cycle). Unarchive returns to TODO.
(define outline/archive
  (lambda (n)
    (outline/set-todo n "ARCHIVED")))

(define outline/unarchive
  (lambda (n)
    (outline/set-todo n "TODO")))

(define outline/cycle-todo
  (lambda (n)
    (if (outline/archived? (outline/todo n))
        n
        (outline/set-todo n (outline/todo-next (outline/todo n))))))

;; Collect (id title state) for every node with non-empty todo.
(define outline/todos
  (lambda (tree)
    (outline/todos-acc tree (list))))

(define outline/todos-acc
  (lambda (n acc)
    (let ((todo (as-str (outline/todo n))))
      (let ((acc1
              (if (string-eq todo "")
                  acc
                  (cons (list (outline/id n) (outline/title n) todo) acc))))
        (list/foldl
          (lambda (a k) (outline/todos-acc k a))
          acc1
          (outline/kids n))))))

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
;; Actions sit right after the title row (before body/kids) so parent
;; controls never render under nested children. Desktop: right-justified
;; on the title line. Mobile: ⋯ dropdown (see .ol-actions CSS).

;; Optional extra action HTML (e.g. refile form). Notes redefines this after load.
(define outline/extra-actions
  (lambda (slug nid) ""))

;; hx-swap outerHTML: response is a full #outline root from outline/html.
;; innerHTML nested a second #outline and broke subsequent ops.
;; show-archived: when #t, append &show=archived so partials keep the filter on.
(define outline/action-btn
  (lambda (slug nid op label class show-archived)
    (html/tag "button"
      (html/attrs
        (list
          (html/attr "class" class)
          (html/attr "type" "button")
          (html/attr "title" op)
          (html/attr "hx-post"
            (string-append "/partials/notes-node-op?slug=" slug
              "&op=" op "&node-id=" nid
              (if show-archived "&show=archived" "")))
          (html/attr "hx-target" "#outline")
          (html/attr "hx-swap" "outerHTML")))
      label)))

;; Always-visible control bar (div). A closed <details> hides non-summary
;; children in browsers, so desktop CSS that hid the ··· summary made the
;; whole panel disappear. Mobile still wraps buttons via CSS.
(define outline/actions
  (lambda (slug nid todo show-archived)
    (html/div (html/attr "class" "ol-actions")
      (html/div (html/attr "class" "ol-actions-panel")
        (html/join
          (list
            (outline/action-btn slug nid "cycle-todo" "todo" "ol-btn" show-archived)
            (if (outline/archived? todo)
                (outline/action-btn slug nid "unarchive" "unarch" "ol-btn" show-archived)
                (outline/action-btn slug nid "archive" "arch" "ol-btn" show-archived))
            (outline/action-btn slug nid "add-child" "+kid" "ol-btn" show-archived)
            (outline/action-btn slug nid "add-sibling" "+sib" "ol-btn" show-archived)
            (outline/action-btn slug nid "move-up" "up" "ol-btn" show-archived)
            (outline/action-btn slug nid "move-down" "dn" "ol-btn" show-archived)
            (outline/extra-actions slug nid)
            (html/tag "button"
              (html/attrs
                (list
                  (html/attr "class" "ol-btn ol-del")
                  (html/attr "type" "button")
                  (html/attr "hx-post"
                    (string-append "/partials/notes-node-op?slug=" slug
                      "&op=delete&node-id=" nid
                      (if show-archived "&show=archived" "")))
                  (html/attr "hx-target" "#outline")
                  (html/attr "hx-swap" "outerHTML")
                  (html/attr "hx-confirm" "Delete this node?")))
              "del")))))))

;; Non-interactive badge — when node has a non-empty todo/state.
(define outline/todo-badge
  (lambda (todo)
    (let ((t (as-str todo)))
      (if (string-eq t "")
          ""
          (html/span
            (html/attr "class" (string-append "ol-todo st-" t))
            (html/text t))))))

(define outline/html-node
  (lambda (slug n show-archived)
    (let ((nid (outline/id n))
          (todo (outline/todo n))
          (body (outline/body n))
          (kids (outline/kids n)))
      (html/tag "details"
        (html/attrs
          (list
            (html/attr "class"
              (if (string-eq (as-str todo) "")
                  "ol-node"
                  (string-append "ol-node is-todo st-" (as-str todo))))
            (html/attr "open" "open")
            (html/attr "data-id" nid)))
        (html/join
          (list
            (html/tag "summary" (html/attr "class" "ol-sum")
              (html/join
                (list
                  (html/span (html/attr "class" "ol-fold") "")
                  (outline/todo-badge todo)
                  (html/span
                    (html/attrs
                      (list
                        (html/attr "class" "ol-title")
                        (html/attr "contenteditable" "true")
                        (html/attr "data-id" nid)
                        (html/attr "data-field" "title")))
                    (html/text (outline/title n))))))
            ;; Before body/kids so controls belong to this row, not under children.
            (outline/actions slug nid todo show-archived)
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
                  (if (if (outline/archived? (outline/todo k))
                          show-archived
                          #t)
                      (string-append acc (outline/html-node slug k show-archived))
                      acc))
                ""
                kids))))))))

;; show-archived: #f hides ARCHIVED kids (default). Root always rendered.
(define outline/html
  (lambda (slug root show-archived)
    (html/div (html/attr "id" "outline")
      (outline/html-node slug root show-archived))))

(define outline/encode
  (lambda (root) (json-emit root)))

(define outline/decode
  (lambda (s)
    (if (null? s)
        (outline/node "root" "Untitled")
        (if (string-eq (as-str s) "")
            (outline/node "root" "Untitled")
            (json-parse (as-str s))))))
