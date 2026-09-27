;; package outline -- exported from live branch admin/personal
;; Sync: scripts/pp_sync.py pull|push

(define outline/action-btn
  (lambda
    (slug nid op label class show-archived)
    "Render a node-operation button with an htmx post."
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
    "Render the per-node actions dropdown menu."
    (html/tag
      "details"
      (html/attr "class" "ol-actions")
      (html/join
        (list
          (html/tag
            "summary"
            (html/attr "class" "ol-actions-sum")
            (html/text "···"))
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
    "Prepend child to node n's kids list."
    (outline/set-kids n (cons child (outline/kids n)))))

(define outline/add-child-by-id
  (lambda
    (tree id child)
    "Add child to the node with id, returning a new tree."
    (outline/update
      tree
      id
      (lambda (n) (outline/add-child n child)))))

(define outline/archive
  (lambda (n) "Mark a node as ARCHIVED." (outline/set-todo n "ARCHIVED")))

(define outline/archived?
  (lambda (s) "True when the todo string is ARCHIVED." (string-eq (as-str s) "ARCHIVED")))

(define outline/body
  (lambda (n) "Return the body string of a node." (car (cdr (cdr n)))))

(define outline/cycle-todo
  (lambda
    (n)
    "Advance a node's todo state, leaving archived untouched."
    (if
      (outline/archived? (outline/todo n))
      n
      (outline/set-todo
        n
        (outline/todo-next (outline/todo n))))))

(define outline/decode
  (lambda
    (s)
    "Parse a JSON outline string into a tree, defaulting on error."
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
    "Return a copy of tree with the node id removed."
    (if
      (string-eq (outline/id tree) id)
      tree
      (outline/set-kids
        tree
        (outline/delete-from-kids (outline/kids tree) id)))))

(define outline/delete-from-kids
  (lambda
    (kids id)
    "Remove the node id from a kids list, recursing into children."
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
    "Return a copy of tree with node id demoted one level."
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
    "Demote node id within a kids list, shifting it under its sibling."
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
    "Return the detached node and the tree without it; () when top-level."
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
    "Detach a direct child id from node n; () when absent."
    (let
      ((pair (outline/take-from-list (outline/kids n) id)))
      (if
        (null? pair)
        ()
        (list (car pair) (outline/set-kids n (car (cdr pair))))))))

(define outline/detach-in-kids
  (lambda
    (kids id)
    "Detach node id from a kids list, recursing into children."
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
  (lambda (root) "Serialise a tree to a JSON string." (json-emit root)))

(define outline/extra-actions
  (lambda (slug nid) "Render the extra refile actions for a node." (notes/refile/extract-form slug nid)))

(define outline/find
  (lambda
    (tree id)
    "Find the node with id in tree; () when absent."
    (if
      (string-eq (outline/id tree) id)
      tree
      (outline/find-in-kids (outline/kids tree) id))))

(define outline/find-in-kids
  (lambda
    (kids id)
    "Find node id within a kids list; () when absent."
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
    "Generate a cheap unique node id from the empty list length."
    (string-append
      "n"
      (as-str (string-length (as-str (list)))))))

(define outline/html
  (lambda
    (slug root show-archived)
    "Render the full outline HTML with per-node actions."
    (html/div
      (html/attr "id" "outline")
      (outline/html-node slug root show-archived #t))))

(define outline/html-lite
  (lambda
    (slug root show-archived)
    "Render the outline HTML without per-node actions."
    (html/div
      (html/attr "id" "outline")
      (outline/html-node slug root show-archived #f))))

(define outline/html-node
  (lambda (slug n show-archived with-actions)
    "Render one node and its children recursively."
    (let ((nid (outline/id n))
          (todo (outline/todo n))
          (body (outline/body n))
          (kids (outline/kids n)))
      (let ((is-ref (bible/ref-body? body)))
        (html/tag
          "details"
          (html/attrs
            (list
              (html/attr
                "class"
                (if (string-eq (as-str todo) "")
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
              (if with-actions
                  (outline/actions slug nid todo show-archived)
                  "")
              (if (string-eq body "")
                  ""
                  (html/div
                    (html/attrs
                      (if is-ref
                          (list
                            (html/attr "class" "ol-body ol-body-ref")
                            (html/attr "data-id" nid)
                            (html/attr "data-bible-ref" "1"))
                          (list
                            (html/attr "class" "ol-body")
                            (html/attr "contenteditable" "true")
                            (html/attr "data-id" nid)
                            (html/attr "data-field" "body"))))
                    (outline/render-body body)))
              (html/div
                (html/attr "class" "ol-kids")
                (html/join
                  (list/map
                    (lambda (k)
                      (outline/html-node slug k show-archived with-actions))
                    kids))))))))))

(define outline/id
  (lambda (n) "Return the id of a node." (car n)))

(define outline/insert-sib-list
  (lambda
    (kids id new-node)
    "Insert new-node after the sibling with id in a kids list."
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
    "Return a copy of tree with new-node inserted after node id."
    (outline/insert-sibling-kids tree id new-node)))

(define outline/insert-sibling-kids
  (lambda
    (tree id new-node)
    "Append new-node after node id, or append when tree itself matches."
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
  (lambda (n) "Return the kids list of a node." (car (cdr (cdr (cdr (cdr n)))))))

(define outline/move-down
  (lambda (tree id) "Move node id down one position in its sibling list." (outline/move-down-kids tree id)))

(define outline/move-down-kids
  (lambda
    (tree id)
    "Move a child down among its siblings in the tree."
    (if
      (string-eq (outline/id tree) (as-str id))
      tree
      (outline/set-kids
        tree
        (outline/swap-next (outline/kids tree) (as-str id))))))

(define outline/move-up
  (lambda (tree id) "Move node id up one position in its sibling list." (outline/move-up-kids tree id)))

(define outline/move-up-kids
  (lambda
    (tree id)
    "Move a child up among its siblings in the tree."
    (if
      (string-eq (outline/id tree) (as-str id))
      tree
      (outline/set-kids
        tree
        (outline/swap-prev (outline/kids tree) (as-str id))))))

(define outline/node
  (lambda
    (id title)
    "Build an empty outline node with the given id and title."
    (list (as-str id) (as-str title) "" "" (list))))

(define outline/node*
  (lambda
    (id title body todo kids)
    "Build a full outline node with all fields."
    (list
      (as-str id)
      (as-str title)
      (as-str body)
      (as-str todo)
      kids)))

(define outline/promote
  (lambda
    (tree id)
    "Return a copy of tree with node id promoted one level."
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
    "Pull node id up past its siblings across a kids list."
    (outline/promote-lift
      (list/map (lambda (k) (outline/promote k id)) kids)
      id)))

(define outline/promote-lift
  (lambda
    (kids id)
    "Lift the node id out of each sibling's kids, placing it earlier."
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
    "Render a node body block, preferring bible then roam renderers."
    (let
      ((br (bible/render-body body)))
      (if (null? br) (roam/render-body body) br))))

(define outline/set-body
  (lambda
    (n body)
    "Return a copy of node n with a new body."
    (outline/node*
      (outline/id n)
      (outline/title n)
      body
      (outline/todo n)
      (outline/kids n))))

(define outline/set-kids
  (lambda
    (n kids)
    "Return a copy of node n with a new kids list."
    (outline/node*
      (outline/id n)
      (outline/title n)
      (outline/body n)
      (outline/todo n)
      kids)))

(define outline/set-title
  (lambda
    (n title)
    "Return a copy of node n with a new title."
    (outline/node*
      (outline/id n)
      title
      (outline/body n)
      (outline/todo n)
      (outline/kids n))))

(define outline/set-todo
  (lambda
    (n todo)
    "Return a copy of node n with a new todo state."
    (outline/node*
      (outline/id n)
      (outline/title n)
      (outline/body n)
      todo
      (outline/kids n))))

(define outline/swap-next
  (lambda
    (kids id)
    "Swap node id downward with its next sibling in a kids list."
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
    "Swap node id upward with its previous sibling in a kids list."
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
    "Split off node id from a kids list, returning (node rest)."
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
  (lambda (n) "Return the title of a node." (car (cdr n))))

(define outline/todo
  (lambda (n) "Return the todo state of a node." (car (cdr (cdr (cdr n))))))

(define outline/todo-badge
  (lambda
    (todo)
    "Render the todo state badge for a node, or empty when none."
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
    "Return the next todo state after cur in the state list."
    (outline/todo-next-in outline/todo-states (as-str cur))))

(define outline/todo-next-in
  (lambda
    (xs cur)
    "Find the state following cur in the given state list."
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
    "True when a todo state is an active (not closed) state."
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
  (lambda (tree) "Collect all non-empty todo entries in a tree." (outline/todos-acc tree (list))))

(define outline/todos-acc
  (lambda
    (n acc)
    "Accumulate todo entries while walking the tree."
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
  (lambda (n) "Un-archive a node, resetting its todo to TODO." (outline/set-todo n "TODO")))

(define outline/update
  (lambda
    (tree id f)
    "Return a copy of tree with f applied to the node id."
    (if
      (string-eq (outline/id tree) id)
      (f tree)
      (outline/set-kids
        tree
        (outline/update-kids (outline/kids tree) id f)))))

(define outline/update-kids
  (lambda
    (kids id f)
    "Apply f to node id throughout a kids list."
    (if
      (null? kids)
      (list)
      (cons
        (outline/update (car kids) id f)
        (outline/update-kids (cdr kids) id f)))))

(define outline/valid?
  (lambda
    (n)
    "True when n is a proper 5-field outline node whose kids are all
     valid nodes, RECURSIVELY.

     Was: a shallow (count n) = 5 check on the node it was handed. A tree
     whose ROOT is well-formed but whose CHILD is not (e.g. a 2-element
     [id title]) passed, and the malformed child only blew up later —
     inside notes/tree/write-heading, after save-outline had already
     cleared the note's headings (2026-09-27 incident: improperly formed
     upserts could destroy a note). Validating the whole tree at the
     boundary refuses the write before anything is touched."
    (if
      (null? n)
      ()
      (if
        (pair? n)
        (if
          (= (count n) 5)
          (outline/valid-kids? (outline/kids n))
          ())
        ()))))

(define outline/valid-kids?
  (lambda
    (kids)
    "Every kid of a node is itself a valid outline node."
    (if
      (null? kids)
      #t
      (if
        (pair? kids)
        (if
          (pair? (car kids))
          (if
            (outline/valid? (car kids))
            (outline/valid-kids? (cdr kids))
            #f)
          #f)
        #f))))


(define outline/css
  (lambda
    ()
    "Render the outline component stylesheet (extracted from ui/theme)."
    (css/sheet
      (list
        (css/rule
          "#ol-selected-label"
          (list
            "font-size"
            ".85rem"
            "max-width"
            "12rem"
            "overflow"
            "hidden"
            "text-overflow"
            "ellipsis"
            "white-space"
            "nowrap"
            "margin-right"
            ".35rem"))
        (css/rule ".ol-root" (list "margin" ".5rem 0"))
        (css/rule
          ".ol-node"
          (list
            "position"
            "relative"
            "margin"
            ".25rem 0 .25rem .35rem"
            "border-left"
            "2px solid #808080"
            "padding-left"
            ".35rem"))
        (css/rule
          ".ol-sum"
          (list
            "cursor"
            "pointer"
            "font-weight"
            "700"
            "display"
            "flex"
            "align-items"
            "center"
            "gap"
            ".35rem"
            "flex-wrap"
            "nowrap"
            "min-width"
            "0"
            "padding-right"
            "2.5rem"))
        (css/rule
          ".ol-node.is-selected > .ol-sum"
          (list
            "background"
            "var(--sel-bg)"
            "outline"
            "2px solid #000"
            "box-shadow"
            "inset 3px 0 0 var(--sel-accent)"))
        (css/rule
          ".ol-fold"
          (list
            "font-size"
            ".75em"
            "flex"
            "0 0 auto"
            "display"
            "inline-flex"
            "align-items"
            "center"
            "justify-content"
            "center"
            "user-select"
            "none"
            "cursor"
            "pointer"
            "line-height"
            "1"
            "min-width"
            "1.6rem"
            "min-height"
            "1.6rem"
            "padding"
            ".2rem"
            "margin"
            "0 .05rem 0 -.1rem"
            "color"
            "var(--muted)"))
        (css/rule
          ".ol-fold:hover"
          (list
            "background"
            "var(--panel)"
            "border"
            "1px solid #808080"
            "padding"
            "calc(.2rem - 1px)"))
        (css/rule
          "details.ol-node > .ol-sum .ol-fold::before"
          (list "content" "\"v\""))
        (css/rule
          "details.ol-node:not([open]) > .ol-sum .ol-fold::before"
          (list "content" "\">\""))
        (css/rule
          ".ol-todo"
          (list
            "display"
            "inline-block"
            "flex"
            "0 0 auto"
            "width"
            "auto"
            "margin"
            "0 .15rem 0 0"
            "padding"
            ".05rem .3rem"
            "border"
            "1px solid #666"
            "background"
            "transparent"
            "font-size"
            ".7rem"
            "font-weight"
            "700"
            "line-height"
            "1.3"
            "letter-spacing"
            ".02em"
            "color"
            "var(--muted)"
            "vertical-align"
            "middle"))
        (css/rule
          ".ol-todo.st-TODO"
          (list
            "border-color"
            "var(--pending-border)"
            "color"
            "var(--pending-ink)"
            "background"
            "var(--pending-bg)"))
        (css/rule
          ".ol-todo.st-DONE"
          (list
            "border-color"
            "var(--done-ink)"
            "color"
            "var(--done-ink)"
            "background"
            "var(--done-bg)"))
        (css/rule
          ".ol-todo.st-ARCHIVED"
          (list
            "border-color"
            "var(--archived-border)"
            "color"
            "var(--archived-ink)"
            "background"
            "var(--archived-bg)"))
        (css/rule
          ".ol-node.st-DONE > .ol-sum .ol-title"
          (list
            "text-decoration"
            "line-through"
            "color"
            "var(--done-muted)"
            "font-weight"
            "500"))
        (css/rule
          ".ol-node.st-ARCHIVED > .ol-sum .ol-title"
          (list
            "color"
            "var(--archived-ink)"
            "font-style"
            "italic"
            "font-weight"
            "500"))
        (css/rule
          ".ol-node.st-ARCHIVED"
          (list "opacity" "0.72"))
        (css/rule
          ".ol-title"
          (list
            "outline"
            "none"
            "padding"
            ".15rem .25rem"
            "border-radius"
            "0"
            "flex"
            "1 1 auto"
            "min-width"
            "0"
            "word-break"
            "break-word"
            "white-space"
            "normal"
            "line-height"
            "1.35"
            "cursor"
            "text"
            "font-weight"
            "700"))
        (css/rule
          ".ol-title:focus"
          (list
            "background"
            "var(--panel)"
            "border"
            "1px solid #000"))
        (css/rule
          ".ol-body"
          (list
            "margin"
            ".25rem 0 .35rem"
            "white-space"
            "pre-wrap"
            "font-size"
            ".95rem"
            "outline"
            "none"
            "padding"
            ".25rem"
            "min-width"
            "0"
            "word-break"
            "break-word"))
        (css/rule
          ".ol-body:focus"
          (list
            "background"
            "var(--panel)"
            "border"
            "1px solid #000"))
        (css/rule
          ".ol-kids"
          (list "margin-left" ".15rem" "min-width" "0"))
        (css/rule
          ".ol-toolbar"
          (list
            "display"
            "flex"
            "flex-wrap"
            "wrap"
            "align-items"
            "center"
            "gap"
            ".3rem"
            "margin"
            "0 0 .65rem"
            "padding"
            ".35rem .5rem"
            "background"
            "var(--panel)"
            "border"
            "2px solid #000"
            "position"
            "sticky"
            "top"
            "0"
            "z-index"
            "40"
            "box-shadow"
            "0 2px 0 #808080"))
        (css/rule
          ".ol-toolbar-label"
          (list
            "font-size"
            ".8rem"
            "color"
            "var(--muted)"
            "margin-right"
            ".2rem"
            "min-width"
            "auto"
            "font-weight"
            "700"))
        (css/rule
          ".ol-toolbar .ol-btn"
          (list "min-height" "2rem" "padding" ".2rem .45rem"))
        (css/rule
          ".ol-toolbar form.ol-refile"
          (list
            "margin"
            "0"
            "display"
            "inline-flex"
            "width"
            "auto"))
        (css/rule
          "details.ol-actions"
          (list
            "display"
            "none"
            "position"
            "absolute"
            "top"
            "0"
            "right"
            "0"
            "z-index"
            "5"
            "margin"
            "0"))
        (css/rule
          "details.ol-actions > summary.ol-actions-sum"
          (list
            "display"
            "inline-flex"
            "align-items"
            "center"
            "justify-content"
            "center"
            "list-style"
            "none"
            "cursor"
            "pointer"
            "user-select"
            "none"
            "width"
            "auto"
            "min-width"
            "2.25rem"
            "min-height"
            "2.25rem"
            "padding"
            ".2rem .35rem"
            "border"
            "1px solid #666"
            "background"
            "var(--chrome)"
            "font-weight"
            "700"
            "color"
            "var(--ink)"
            "text-align"
            "center"
            "line-height"
            "1.2"))
        (css/rule
          "details.ol-actions > summary.ol-actions-sum::-webkit-details-marker"
          (list "display" "none"))
        (css/rule
          "details.ol-actions > summary.ol-actions-sum:hover, details.ol-actions[open] > summary.ol-actions-sum"
          (list
            "background"
            "var(--panel)"
            "border-color"
            "var(--ink)"))
        (css/rule
          ".ol-actions-panel"
          (list
            "display"
            "none"
            "position"
            "absolute"
            "right"
            "0"
            "top"
            "100%"
            "margin-top"
            "2px"
            "flex-direction"
            "column"
            "flex-wrap"
            "nowrap"
            "align-items"
            "stretch"
            "min-width"
            "7rem"
            "padding"
            ".2rem"
            "background"
            "var(--panel)"
            "border"
            "1px solid #000"
            "z-index"
            "20"))
        (css/rule
          "details.ol-actions[open] > .ol-actions-panel"
          (list "display" "flex"))
        (css/rule
          "details.ol-actions[open] > .ol-actions-panel .ol-btn"
          (list
            "width"
            "100%"
            "text-align"
            "left"
            "min-height"
            "2rem"))
        (css/rule
          ".ol-btn"
          (list
            "font-size"
            ".75rem"
            "padding"
            ".2rem .4rem"
            "min-height"
            "1.8rem"
            "width"
            "auto"
            "flex"
            "0 0 auto"
            "margin"
            "0"
            "border"
            "1px solid #000"
            "background"
            "var(--bg)"
            "cursor"
            "pointer"
            "line-height"
            "1.2"))
        (css/rule
          ".ol-btn:hover"
          (list "background" "var(--panel)"))
        (css/rule ".ol-del" (list "color" "var(--err)"))
        (css/rule
          "form.ol-refile"
          (list
            "display"
            "flex"
            "flex-wrap"
            "wrap"
            "gap"
            ".25rem"
            "align-items"
            "center"
            "margin"
            ".25rem 0 0"
            "width"
            "auto"
            "max-width"
            "100%"))
        (css/rule
          "form.ol-refile .ol-refile-sel"
          (list
            "flex"
            "1 1 8rem"
            "min-width"
            "6rem"
            "max-width"
            "14rem"
            "font-size"
            ".75rem"
            "padding"
            ".2rem"))
        (css/rule
          "form.ol-refile button"
          (list
            "width"
            "auto"
            "min-width"
            "0"
            "font-size"
            ".75rem"
            "padding"
            ".2rem .4rem"
            "margin"
            "0"))
        (css/rule
          ".ol-debug"
          (list "margin" ".75rem 0" "opacity" ".6"))
        (css/rule
          ".ol-debug > summary"
          (list "cursor" "pointer" "font-size" ".85rem"))
        (css/rule ".ol-todo-btn" (list "cursor" "pointer"))
        (css/rule
          ".ol-todo-btn.st-TODO"
          (list "background" "var(--pending-bg)"))
        (css/rule
          ".ol-todo-btn.st-DONE"
          (list "background" "var(--done-bg)"))
        (css/rule
          ".ol-todo-btn.st-WAITING"
          (list "background" "var(--waiting-bg)"))))))
