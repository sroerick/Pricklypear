;; Pure outline trees (org-mode shaped). No DB.
;; Node = (id title body todo kids)  — kids is a list of nodes.
;; Load: (load-library "outline")

(define outline/id (lambda (n) (car n))
  "Node id field.")
(define outline/title (lambda (n) (car (cdr n)))
  "Node title field.")
(define outline/body (lambda (n) (car (cdr (cdr n))))
  "Node body field.")
(define outline/todo (lambda (n) (car (cdr (cdr (cdr n)))))
  "Node todo state field.")
(define outline/kids (lambda (n) (car (cdr (cdr (cdr (cdr n))))))
  "Node children list field.")

(define outline/node
  (lambda (id title)
    (list (as-str id) (as-str title) "" "" (list)))
  "Construct outline node dict from id title body todo kids.")

(define outline/node*
  (lambda (id title body todo kids)
    (list (as-str id) (as-str title) (as-str body) (as-str todo) kids))
  "Construct node with defaults for missing pieces.")

(define outline/set-title
  (lambda (n title)
    (outline/node* (outline/id n) title (outline/body n) (outline/todo n) (outline/kids n)))
  "Return node with updated title.")

(define outline/set-body
  (lambda (n body)
    (outline/node* (outline/id n) (outline/title n) body (outline/todo n) (outline/kids n)))
  "Return node with updated body.")

(define outline/set-todo
  (lambda (n todo)
    (outline/node* (outline/id n) (outline/title n) (outline/body n) todo (outline/kids n)))
  "Return node with updated todo state.")

;; Outline checklist states. Empty = not a todo. Cycle: "" → TODO → DONE → "".
;; (Legacy WAITING/CANCELLED map to TODO on next cycle via todo-next-in miss.)
(define outline/todo-states
  (list "" "TODO" "DONE")
  "Default todo state cycle list.")

(define outline/todo-next
  (lambda (cur)
    (outline/todo-next-in outline/todo-states (as-str cur)))
  "Next todo state after cur in default cycle.")

(define outline/todo-next-in
  (lambda (xs cur)
    (if (null? xs)
        "TODO"
        (if (string-eq (as-str (car xs)) cur)
            (if (null? (cdr xs)) (as-str (car outline/todo-states))
                (as-str (car (cdr xs))))
            (outline/todo-next-in (cdr xs) cur))))
  "Next todo state after cur in explicit states list.")

(define outline/todo-open?
  (lambda (s)
    (let ((t (as-str s)))
      (if (string-eq t "")
          #f
          (if (string-eq t "DONE")
              #f
              (if (string-eq t "CANCELLED")
                  #f
                  (if (string-eq t "ARCHIVED") #f #t))))))
  "True if todo state is actionable.")

(define outline/archived?
  (lambda (s)
    (string-eq (as-str s) "ARCHIVED"))
  "True if node todo is ARCHIVED.")

;; Terminal state (not in cycle). Unarchive returns to TODO.
(define outline/archive
  (lambda (n)
    (outline/set-todo n "ARCHIVED"))
  "Set node todo to ARCHIVED.")

(define outline/unarchive
  (lambda (n)
    (outline/set-todo n "TODO"))
  "Clear ARCHIVED back to TODO.")

(define outline/cycle-todo
  (lambda (n)
    (if (outline/archived? (outline/todo n))
        n
        (outline/set-todo n (outline/todo-next (outline/todo n)))))
  "Advance node todo state in cycle.")

;; Collect (id title state) for every node with non-empty todo.
(define outline/todos
  (lambda (tree)
    (outline/todos-acc tree (list)))
  "Collect todo-bearing nodes under tree.")

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
          (outline/kids n)))))
  "Accumulator helper for outline/todos.")

(define outline/set-kids
  (lambda (n kids)
    (outline/node* (outline/id n) (outline/title n) (outline/body n) (outline/todo n) kids))
  "Return node with kids list replaced.")

(define outline/add-child
  (lambda (n child)
    (outline/set-kids n (cons child (outline/kids n))))
  "Append child node under parent.")

;; fresh id generator (simple counter-based; good enough for UI)
(define outline/fresh-id
  (lambda ()
    (string-append "n" (as-str (string-length (as-str (list))))))
  "Fresh outline node id string.")

;; --- tree traversal: find, update, delete by id ---

(define outline/find
  (lambda (tree id)
    (if (string-eq (outline/id tree) id)
        tree
        (outline/find-in-kids (outline/kids tree) id)))
  "Find node by id in tree.")

(define outline/find-in-kids
  (lambda (kids id)
    (if (null? kids)
        ()
        (let ((found (outline/find (car kids) id)))
          (if (null? found)
              (outline/find-in-kids (cdr kids) id)
              found))))
  "Find node by id among kids list.")

(define outline/update
  (lambda (tree id f)
    (if (string-eq (outline/id tree) id)
        (f tree)
        (outline/set-kids tree
          (outline/update-kids (outline/kids tree) id f))))
  "Map update-fn over node matching id.")

(define outline/update-kids
  (lambda (kids id f)
    (if (null? kids)
        (list)
        (cons (outline/update (car kids) id f)
          (outline/update-kids (cdr kids) id f))))
  "update helper over kids list.")

;; delete node by id (returns tree without it).
;; NEVER delete root — that wiped the whole note (empty tree → Untitled).
(define outline/delete
  (lambda (tree id)
    (if (string-eq (outline/id tree) id)
        tree
        (outline/set-kids tree
          (outline/delete-from-kids (outline/kids tree) id))))
  "Delete node id from tree; return new tree.")

(define outline/delete-from-kids
  (lambda (kids id)
    (if (null? kids)
        (list)
        (if (string-eq (outline/id (car kids)) id)
            (cdr kids)
            (cons (outline/delete (car kids) id)
              (outline/delete-from-kids (cdr kids) id)))))
  "Delete helper over kids list.")

;; add child to node by id
(define outline/add-child-by-id
  (lambda (tree id child)
    (outline/update tree id
      (lambda (n) (outline/add-child n child))))
  "Add child under parent id in tree.")

;; insert sibling after node by id
(define outline/insert-sibling
  (lambda (tree id new-node)
    (outline/insert-sibling-kids tree id new-node))
  "Insert sibling after node id.")

(define outline/insert-sibling-kids
  (lambda (tree id new-node)
    (if (string-eq (outline/id tree) id)
        ;; Sibling of root = new top-level item under root (last child).
        ;; Previously a silent no-op — felt like +sib was broken at top level.
        (outline/set-kids tree
          (append (outline/kids tree) (list new-node)))
        (outline/set-kids tree
          (outline/insert-sib-list (outline/kids tree) id new-node))))
  "insert-sibling helper on kids.")

(define outline/insert-sib-list
  (lambda (kids id new-node)
    (if (null? kids)
        (list)
        (if (string-eq (outline/id (car kids)) id)
            (cons (car kids) (cons new-node (cdr kids)))
            (cons (outline/insert-sibling-kids (car kids) id new-node)
              (outline/insert-sib-list (cdr kids) id new-node)))))
  "Insert node into list after target id.")

;; --- pure structural movement (sexp trees only; no DB) ---
;;
;; detach: pull a non-root node out; returns (node new-tree) or ().
;; promote / demote: org-style indent levels (spec outline-model).
;; move-up / move-down: reorder among siblings.

;; Remove first occurrence of id from a sibling list.
;; -> (node remaining-kids) | ()
(define outline/take-from-list
  (lambda (kids id)
    (if (null? kids)
        ()
        (if (string-eq (outline/id (car kids)) (as-str id))
            (list (car kids) (cdr kids))
            (let ((rest (outline/take-from-list (cdr kids) id)))
              (if (null? rest)
                  ()
                  (list (car rest)
                        (cons (car kids) (car (cdr rest)))))))))
  "Remove and return node id from list as (node rest).")

;; Detach direct child id from node n -> (child new-n) | ()
(define outline/detach-direct
  (lambda (n id)
    (let ((pair (outline/take-from-list (outline/kids n) id)))
      (if (null? pair)
          ()
          (list (car pair)
                (outline/set-kids n (car (cdr pair)))))))
  "Detach node id from immediate kids.")

;; Detach id anywhere under tree (not the root itself).
;; -> (node tree-without-node) | ()
(define outline/detach
  (lambda (tree id)
    (let ((tid (as-str id)))
      (if (string-eq (outline/id tree) tid)
          ()
          (let ((pair (outline/detach-in-kids (outline/kids tree) tid)))
            (if (null? pair)
                ()
                (list (car pair)
                      (outline/set-kids tree (car (cdr pair)))))))))
  "Detach node id from tree; return (node tree).")

(define outline/detach-in-kids
  (lambda (kids id)
    (if (null? kids)
        ()
        (if (string-eq (outline/id (car kids)) id)
            (list (car kids) (cdr kids))
            (let ((sub (outline/detach (car kids) id)))
              (if (null? sub)
                  (let ((rest (outline/detach-in-kids (cdr kids) id)))
                    (if (null? rest)
                        ()
                        (list (car rest)
                              (cons (car kids) (car (cdr rest))))))
                  (list (car sub)
                        (cons (car (cdr sub)) (cdr kids))))))))
  "detach helper over kids.")

;; promote: remove node from its parent; insert as sibling after that parent.
;; No-op if missing, root, or direct child of root (nowhere above root).
(define outline/promote
  (lambda (tree id)
    (let ((tid (as-str id)))
      (if (string-eq (outline/id tree) tid)
          tree
          (outline/set-kids tree
            (outline/promote-kids (outline/kids tree) tid)))))
  "Promote node id one level up.")

(define outline/promote-kids
  (lambda (kids id)
    ;; Deeper promotions first, then lift any direct-grandchild at this level.
    (outline/promote-lift
      (list/map (lambda (k) (outline/promote k id)) kids)
      id))
  "promote helper over kids.")

(define outline/promote-lift
  (lambda (kids id)
    (if (null? kids)
        (list)
        (let ((pair (outline/detach-direct (car kids) id)))
          (if (null? pair)
              (cons (car kids) (outline/promote-lift (cdr kids) id))
              ;; pair = (node parent-without-node); splice node after parent.
              (cons (car (cdr pair))
                    (cons (car pair) (cdr kids)))))))
  "Lift node after parent in grandparent list.")

;; demote: become last child of the previous sibling. No-op if first sibling.
(define outline/demote
  (lambda (tree id)
    (let ((tid (as-str id)))
      (if (string-eq (outline/id tree) tid)
          tree
          (outline/set-kids tree
            (outline/demote-kids (outline/kids tree) tid)))))
  "Demote node id under previous sibling.")

(define outline/demote-kids
  (lambda (kids id)
    (if (null? kids)
        (list)
        (if (null? (cdr kids))
            (list (outline/demote (car kids) id))
            (if (string-eq (outline/id (car (cdr kids))) id)
                (let ((prev (car kids))
                      (node (car (cdr kids)))
                      (rest (cdr (cdr kids))))
                  (cons
                    (outline/set-kids prev
                      (append (outline/kids prev) (list node)))
                    rest))
                (cons (outline/demote (car kids) id)
                      (outline/demote-kids (cdr kids) id))))))
  "demote helper over kids.")

;; move-up: swap with previous sibling
(define outline/move-up
  (lambda (tree id)
    (outline/move-up-kids tree id))
  "Swap node id with previous sibling.")

(define outline/move-up-kids
  (lambda (tree id)
    (if (string-eq (outline/id tree) (as-str id))
        tree
        (outline/set-kids tree
          (outline/swap-prev (outline/kids tree) (as-str id)))))
  "move-up helper over kids.")

(define outline/swap-prev
  (lambda (kids id)
    (if (null? kids)
        (list)
        (if (null? (cdr kids))
            kids
            (if (string-eq (outline/id (car (cdr kids))) id)
                (cons (car (cdr kids)) (cons (car kids) (cdr (cdr kids))))
                (cons (outline/move-up-kids (car kids) id)
                  (outline/swap-prev (cdr kids) id))))))
  "Swap item with previous in list by id.")

;; move-down: swap with next sibling
(define outline/move-down
  (lambda (tree id)
    (outline/move-down-kids tree id))
  "Swap node id with next sibling.")

(define outline/move-down-kids
  (lambda (tree id)
    (if (string-eq (outline/id tree) (as-str id))
        tree
        (outline/set-kids tree
          (outline/swap-next (outline/kids tree) (as-str id)))))
  "move-down helper over kids.")

(define outline/swap-next
  (lambda (kids id)
    (if (null? kids)
        (list)
        (if (string-eq (outline/id (car kids)) id)
            (if (null? (cdr kids))
                kids
                (cons (car (cdr kids)) (cons (car kids) (cdr (cdr kids)))))
            (cons (outline/move-down-kids (car kids) id)
              (outline/swap-next (cdr kids) id)))))
  "Swap item with next in list by id.")

;; --- HTML with editor controls ---
;; Desktop: top selection toolbar (notes/ui/toolbar) acts on selected node.
;; Mobile: per-node ··· details menu (outline/actions). See .ol-actions CSS.

;; Optional extra action HTML (e.g. refile form). Notes redefines this after load.
(define outline/extra-actions
  (lambda (slug nid) "")
  "Extra action specs list for outline UI.")

;; hx-swap outerHTML: response is a full #outline root from outline/html.
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
      label))
  "HTML button for one outline action.")

;; Mobile per-node menu (hidden on wide screens via CSS).
(define outline/actions
  (lambda (slug nid todo show-archived)
    (html/tag "details" (html/attr "class" "ol-actions")
      (html/join
        (list
          (html/tag "summary" (html/attr "class" "ol-actions-sum")
            (html/text "···"))
          (html/div (html/attr "class" "ol-actions-panel")
            (html/join
              (list
                (outline/action-btn slug nid "cycle-todo" "todo" "ol-btn" show-archived)
                (if (outline/archived? todo)
                    (outline/action-btn slug nid "unarchive" "unarch" "ol-btn" show-archived)
                    (outline/action-btn slug nid "archive" "arch" "ol-btn" show-archived))
                (outline/action-btn slug nid "add-child" "+kid" "ol-btn" show-archived)
                (outline/action-btn slug nid "add-sibling" "+sib" "ol-btn" show-archived)
                (outline/action-btn slug nid "promote" "<<" "ol-btn" show-archived)
                (outline/action-btn slug nid "demote" ">>" "ol-btn" show-archived)
                (outline/action-btn slug nid "move-up" "up" "ol-btn" show-archived)
                (outline/action-btn slug nid "move-down" "dn" "ol-btn" show-archived)
                (outline/extra-actions slug nid)
                (html/tag "button"
                  (html/attrs
                    (list
                      (html/attr "class" "ol-btn ol-del")
                      (html/attr "type" "button")
                      (html/attr "title" "delete")
                      (html/attr "hx-post"
                        (string-append "/partials/notes-node-op?slug=" slug
                          "&op=delete&node-id=" nid
                          (if show-archived "&show=archived" "")))
                      (html/attr "hx-target" "#outline")
                      (html/attr "hx-swap" "outerHTML")
                      (html/attr "hx-confirm" "Delete this node?")))
                  "del"))))))))
  "Action toolbar HTML for node.")

;; Non-interactive badge — when node has a non-empty todo/state.
(define outline/todo-badge
  (lambda (todo)
    (let ((t (as-str todo)))
      (if (string-eq t "")
          ""
          (html/span
            (html/attr "class" (string-append "ol-todo st-" t))
            (html/text t)))))
  "HTML badge for todo state.")

;; Body-render hook. Pure default escapes the body as text. Notes redefines
;; this to roam/render-body so [[slug]] wiki links become anchors. Keep the
;; name outline/render-body so outline stays free of a notes dependency.
(define outline/render-body
  (lambda (body)
    (html/text (as-str body)))
  "Render node body HTML (links etc.).")

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
                  ;; outline/render-body is a hook: default = html/text (escaped).
                  ;; notes redefines it to roam/render-body so [[slug]] becomes links.
                  (outline/render-body body)))
            (html/div (html/attr "class" "ol-kids")
              (list/foldl
                (lambda (acc k)
                  ;; Always show ARCHIVED kids (dimmed via .st-ARCHIVED).
                  ;; Hiding them made archive look like a no-op / silent delete.
                  ;; show-archived reserved for future filter toggles.
                  (string-append acc (outline/html-node slug k show-archived)))
                ""
                kids)))))))
  "Recursive HTML for one outline node.")

;; show-archived: #f hides ARCHIVED kids (default). Root always rendered.
(define outline/html
  (lambda (slug root show-archived)
    (html/div (html/attr "id" "outline")
      (outline/html-node slug root show-archived)))
  "Render full outline tree to HTML.")

(define outline/encode
  (lambda (root) (json-emit root))
  "Encode outline tree to transport string/JSON.")

;; A node is a 5-element list (id title body todo kids). Decode is the
;; boundary where untrusted DB text enters the pure outline model, so
;; validate here: anything that isn't a 5-element list falls back to a
;; default root node instead of crashing field accessors with
;; "car needs a list or pair" on a short/atom/nil decode.
(define outline/valid?
  (lambda (n)
    (if (null? n)
        ()
        (if (pair? n)
            (= (count n) 5)
            ())))
  "True if value looks like an outline node tree.")

(define outline/decode
  (lambda (s)
    (if (null? s)
        (outline/node "root" "Untitled")
        (if (string-eq (as-str s) "")
            (outline/node "root" "Untitled")
            (let ((parsed (json-parse (as-str s))))
              (if (outline/valid? parsed)
                  parsed
                  (outline/node "root" "Untitled"))))))
  "Decode transport string to outline tree.")
