;; package outline -- exported from live branch admin/personal
;; Sync: scripts/pp_sync.py pull|push

(define outline/action-btn
  (lambda
    (slug nid op label class show-archived)
    (html/tag
      "button"
      (html/attrs
        (list
          (html/attr "class" class)
          (html/attr "type" "button")
          (html/attr "title" op)
          (html/attr
            "hx-post"
            (string-append
              "/partials/notes-node-op?slug="
              slug
              "&op="
              op
              "&node-id="
              nid
              (if show-archived "&show=archived" "")))
          (html/attr "hx-target" "#outline")
          (html/attr "hx-swap" "outerHTML")))
      label)))

(define outline/actions
  (lambda
    (slug nid todo show-archived)
    (html/tag
      "details"
      (html/attr "class" "ol-actions")
      (html/join
        (list
          (html/tag
            "summary"
            (html/attr "class" "ol-actions-sum")
            (html/text "194183194183194183"))
          (html/div
            (html/attr "class" "ol-actions-panel")
            (html/join
              (list
                (outline/action-btn
                  slug
                  nid
                  "cycle-todo"
                  "todo"
                  "ol-btn"
                  show-archived)
                (if
                  (outline/archived? todo)
                  (outline/action-btn
                    slug
                    nid
                    "unarchive"
                    "unarch"
                    "ol-btn"
                    show-archived)
                  (outline/action-btn
                    slug
                    nid
                    "archive"
                    "arch"
                    "ol-btn"
                    show-archived))
                (outline/action-btn
                  slug
                  nid
                  "add-child"
                  "+kid"
                  "ol-btn"
                  show-archived)
                (outline/action-btn
                  slug
                  nid
                  "add-sibling"
                  "+sib"
                  "ol-btn"
                  show-archived)
                (outline/action-btn
                  slug
                  nid
                  "promote"
                  "<<"
                  "ol-btn"
                  show-archived)
                (outline/action-btn
                  slug
                  nid
                  "demote"
                  ">>"
                  "ol-btn"
                  show-archived)
                (outline/action-btn
                  slug
                  nid
                  "move-up"
                  "up"
                  "ol-btn"
                  show-archived)
                (outline/action-btn
                  slug
                  nid
                  "move-down"
                  "dn"
                  "ol-btn"
                  show-archived)
                (outline/extra-actions slug nid)
                (html/tag
                  "button"
                  (html/attrs
                    (list
                      (html/attr "class" "ol-btn ol-del")
                      (html/attr "type" "button")
                      (html/attr "title" "delete")
                      (html/attr
                        "hx-post"
                        (string-append
                          "/partials/notes-node-op?slug="
                          slug
                          "&op=delete&node-id="
                          nid
                          (if show-archived "&show=archived" "")))
                      (html/attr "hx-target" "#outline")
                      (html/attr "hx-swap" "outerHTML")
                      (html/attr "hx-confirm" "Delete this node?")))
                  "del")))))))))

(define outline/add-child
  (lambda
    (n child)
    (outline/set-kids n (cons child (outline/kids n)))))

(define outline/add-child-by-id
  (lambda
    (tree id child)
    (outline/update
      tree
      id
      (lambda (n) (outline/add-child n child)))))

(define outline/archive
  (lambda (n) (outline/set-todo n "ARCHIVED")))

(define outline/archived?
  (lambda (s) (string-eq (as-str s) "ARCHIVED")))

(define outline/body
  (lambda (n) (car (cdr (cdr n)))))

(define outline/cycle-todo
  (lambda
    (n)
    (if
      (outline/archived? (outline/todo n))
      n
      (outline/set-todo
        n
        (outline/todo-next (outline/todo n))))))

(define outline/decode
  (lambda
    (s)
    (if
      (null? s)
      (outline/node "root" "Untitled")
      (if
        (string-eq (as-str s) "")
        (outline/node "root" "Untitled")
        (let
          ((parsed (json-parse (as-str s))))
          (if
            (outline/valid? parsed)
            parsed
            (outline/node "root" "Untitled")))))))

(define outline/delete
  (lambda
    (tree id)
    (if
      (string-eq (outline/id tree) id)
      tree
      (outline/set-kids
        tree
        (outline/delete-from-kids (outline/kids tree) id)))))

(define outline/delete-from-kids
  (lambda
    (kids id)
    (if
      (null? kids)
      (list)
      (if
        (string-eq (outline/id (car kids)) id)
        (cdr kids)
        (cons
          (outline/delete (car kids) id)
          (outline/delete-from-kids (cdr kids) id))))))

(define outline/demote
  (lambda
    (tree id)
    (let
      ((tid (as-str id)))
      (if
        (string-eq (outline/id tree) tid)
        tree
        (outline/set-kids
          tree
          (outline/demote-kids (outline/kids tree) tid))))))

(define outline/demote-kids
  (lambda
    (kids id)
    (if
      (null? kids)
      (list)
      (if
        (null? (cdr kids))
        (list (outline/demote (car kids) id))
        (if
          (string-eq (outline/id (car (cdr kids))) id)
          (let
            ((prev (car kids))
              (node (car (cdr kids)))
              (rest (cdr (cdr kids))))
            (cons
              (outline/set-kids
                prev
                (append (outline/kids prev) (list node)))
              rest))
          (cons
            (outline/demote (car kids) id)
            (outline/demote-kids (cdr kids) id)))))))

(define outline/detach
  (lambda
    (tree id)
    (let
      ((tid (as-str id)))
      (if
        (string-eq (outline/id tree) tid)
        ()
        (let
          ((pair (outline/detach-in-kids (outline/kids tree) tid)))
          (if
            (null? pair)
            ()
            (list
              (car pair)
              (outline/set-kids tree (car (cdr pair))))))))))

(define outline/detach-direct
  (lambda
    (n id)
    (let
      ((pair (outline/take-from-list (outline/kids n) id)))
      (if
        (null? pair)
        ()
        (list (car pair) (outline/set-kids n (car (cdr pair))))))))

(define outline/detach-in-kids
  (lambda
    (kids id)
    (if
      (null? kids)
      ()
      (if
        (string-eq (outline/id (car kids)) id)
        (list (car kids) (cdr kids))
        (let
          ((sub (outline/detach (car kids) id)))
          (if
            (null? sub)
            (let
              ((rest (outline/detach-in-kids (cdr kids) id)))
              (if
                (null? rest)
                ()
                (list (car rest) (cons (car kids) (car (cdr rest))))))
            (list (car sub) (cons (car (cdr sub)) (cdr kids)))))))))

(define outline/encode
  (lambda (root) (json-emit root)))

(define outline/extra-actions
  (lambda (slug nid) (notes/refile/extract-form slug nid)))

(define outline/find
  (lambda
    (tree id)
    (if
      (string-eq (outline/id tree) id)
      tree
      (outline/find-in-kids (outline/kids tree) id))))

(define outline/find-in-kids
  (lambda
    (kids id)
    (if
      (null? kids)
      ()
      (let
        ((found (outline/find (car kids) id)))
        (if
          (null? found)
          (outline/find-in-kids (cdr kids) id)
          found)))))

(define outline/fresh-id
  (lambda
    ()
    (string-append
      "n"
      (as-str (string-length (as-str (list)))))))

(define outline/html
  (lambda
    (slug root show-archived)
    (html/div
      (html/attr "id" "outline")
      (outline/html-node slug root show-archived #t))))

(define outline/html-lite
  (lambda
    (slug root show-archived)
    (html/div
      (html/attr "id" "outline")
      (outline/html-node slug root show-archived #f))))

(define outline/html-node
  (lambda
    (slug n show-archived with-actions)
    (let
      ((nid (outline/id n))
        (todo (outline/todo n))
        (body (outline/body n))
        (kids (outline/kids n)))
      (html/tag
        "details"
        (html/attrs
          (list
            (html/attr
              "class"
              (if
                (string-eq (as-str todo) "")
                "ol-node"
                (string-append "ol-node is-todo st-" (as-str todo))))
            (html/attr "open" "open")
            (html/attr "data-id" nid)))
        (html/join
          (list
            (html/tag
              "summary"
              (html/attr "class" "ol-sum")
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
            (if
              with-actions
              (outline/actions slug nid todo show-archived)
              "")
            (if
              (string-eq body "")
              ""
              (html/div
                (html/attrs
                  (list
                    (html/attr "class" "ol-body")
                    (html/attr "contenteditable" "true")
                    (html/attr "data-id" nid)
                    (html/attr "data-field" "body")))
                (outline/render-body body)))
            (html/div
              (html/attr "class" "ol-kids")
              (list/foldl
                (lambda
                  (acc k)
                  (string-append
                    acc
                    (outline/html-node slug k show-archived with-actions)))
                ""
                kids))))))))

(define outline/id
  (lambda (n) (car n)))

(define outline/insert-sib-list
  (lambda
    (kids id new-node)
    (if
      (null? kids)
      (list)
      (if
        (string-eq (outline/id (car kids)) id)
        (cons (car kids) (cons new-node (cdr kids)))
        (cons
          (outline/insert-sibling-kids (car kids) id new-node)
          (outline/insert-sib-list (cdr kids) id new-node))))))

(define outline/insert-sibling
  (lambda
    (tree id new-node)
    (outline/insert-sibling-kids tree id new-node)))

(define outline/insert-sibling-kids
  (lambda
    (tree id new-node)
    (if
      (string-eq (outline/id tree) id)
      (outline/set-kids
        tree
        (append (outline/kids tree) (list new-node)))
      (outline/set-kids
        tree
        (outline/insert-sib-list
          (outline/kids tree)
          id
          new-node)))))

(define outline/kids
  (lambda (n) (car (cdr (cdr (cdr (cdr n)))))))

(define outline/move-down
  (lambda (tree id) (outline/move-down-kids tree id)))

(define outline/move-down-kids
  (lambda
    (tree id)
    (if
      (string-eq (outline/id tree) (as-str id))
      tree
      (outline/set-kids
        tree
        (outline/swap-next (outline/kids tree) (as-str id))))))

(define outline/move-up
  (lambda (tree id) (outline/move-up-kids tree id)))

(define outline/move-up-kids
  (lambda
    (tree id)
    (if
      (string-eq (outline/id tree) (as-str id))
      tree
      (outline/set-kids
        tree
        (outline/swap-prev (outline/kids tree) (as-str id))))))

(define outline/node
  (lambda
    (id title)
    (list (as-str id) (as-str title) "" "" (list))))

(define outline/node*
  (lambda
    (id title body todo kids)
    (list
      (as-str id)
      (as-str title)
      (as-str body)
      (as-str todo)
      kids)))

(define outline/promote
  (lambda
    (tree id)
    (let
      ((tid (as-str id)))
      (if
        (string-eq (outline/id tree) tid)
        tree
        (outline/set-kids
          tree
          (outline/promote-kids (outline/kids tree) tid))))))

(define outline/promote-kids
  (lambda
    (kids id)
    (outline/promote-lift
      (list/map (lambda (k) (outline/promote k id)) kids)
      id)))

(define outline/promote-lift
  (lambda
    (kids id)
    (if
      (null? kids)
      (list)
      (let
        ((pair (outline/detach-direct (car kids) id)))
        (if
          (null? pair)
          (cons (car kids) (outline/promote-lift (cdr kids) id))
          (cons (car (cdr pair)) (cons (car pair) (cdr kids))))))))

(define outline/render-body
  (lambda (body)
    (let
      ((br (bible/render-body body)))
      (if (null? br) (roam/render-body body) br))))

(define outline/set-body
  (lambda
    (n body)
    (outline/node*
      (outline/id n)
      (outline/title n)
      body
      (outline/todo n)
      (outline/kids n))))

(define outline/set-kids
  (lambda
    (n kids)
    (outline/node*
      (outline/id n)
      (outline/title n)
      (outline/body n)
      (outline/todo n)
      kids)))

(define outline/set-title
  (lambda
    (n title)
    (outline/node*
      (outline/id n)
      title
      (outline/body n)
      (outline/todo n)
      (outline/kids n))))

(define outline/set-todo
  (lambda
    (n todo)
    (outline/node*
      (outline/id n)
      (outline/title n)
      (outline/body n)
      todo
      (outline/kids n))))

(define outline/swap-next
  (lambda
    (kids id)
    (if
      (null? kids)
      (list)
      (if
        (string-eq (outline/id (car kids)) id)
        (if
          (null? (cdr kids))
          kids
          (cons
            (car (cdr kids))
            (cons (car kids) (cdr (cdr kids)))))
        (cons
          (outline/move-down-kids (car kids) id)
          (outline/swap-next (cdr kids) id))))))

(define outline/swap-prev
  (lambda
    (kids id)
    (if
      (null? kids)
      (list)
      (if
        (null? (cdr kids))
        kids
        (if
          (string-eq (outline/id (car (cdr kids))) id)
          (cons
            (car (cdr kids))
            (cons (car kids) (cdr (cdr kids))))
          (cons
            (outline/move-up-kids (car kids) id)
            (outline/swap-prev (cdr kids) id)))))))

(define outline/take-from-list
  (lambda
    (kids id)
    (if
      (null? kids)
      ()
      (if
        (string-eq (outline/id (car kids)) (as-str id))
        (list (car kids) (cdr kids))
        (let
          ((rest (outline/take-from-list (cdr kids) id)))
          (if
            (null? rest)
            ()
            (list (car rest) (cons (car kids) (car (cdr rest))))))))))

(define outline/title
  (lambda (n) (car (cdr n))))

(define outline/todo
  (lambda (n) (car (cdr (cdr (cdr n))))))

(define outline/todo-badge
  (lambda
    (todo)
    (let
      ((t (as-str todo)))
      (if
        (string-eq t "")
        ""
        (html/span
          (html/attr "class" (string-append "ol-todo st-" t))
          (html/text t))))))

(define outline/todo-next
  (lambda
    (cur)
    (outline/todo-next-in outline/todo-states (as-str cur))))

(define outline/todo-next-in
  (lambda
    (xs cur)
    (if
      (null? xs)
      "TODO"
      (if
        (string-eq (as-str (car xs)) cur)
        (if
          (null? (cdr xs))
          (as-str (car outline/todo-states))
          (as-str (car (cdr xs))))
        (outline/todo-next-in (cdr xs) cur)))))

(define outline/todo-open?
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

(define outline/todo-states
  (list "" "TODO" "DONE"))

(define outline/todos
  (lambda (tree) (outline/todos-acc tree (list))))

(define outline/todos-acc
  (lambda
    (n acc)
    (let
      ((todo (as-str (outline/todo n))))
      (let
        ((acc1
          (if
            (string-eq todo "")
            acc
            (cons (list (outline/id n) (outline/title n) todo) acc))))
        (list/foldl
          (lambda (a k) (outline/todos-acc k a))
          acc1
          (outline/kids n))))))

(define outline/unarchive
  (lambda (n) (outline/set-todo n "TODO")))

(define outline/update
  (lambda
    (tree id f)
    (if
      (string-eq (outline/id tree) id)
      (f tree)
      (outline/set-kids
        tree
        (outline/update-kids (outline/kids tree) id f)))))

(define outline/update-kids
  (lambda
    (kids id f)
    (if
      (null? kids)
      (list)
      (cons
        (outline/update (car kids) id f)
        (outline/update-kids (cdr kids) id f)))))

(define outline/valid?
  (lambda
    (n)
    (if (null? n) () (if (pair? n) (= (count n) 5) ()))))

