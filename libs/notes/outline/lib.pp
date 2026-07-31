;; package notes/outline -- exported from live branch admin/personal
;; Sync: scripts/pp_sync.py pull|push

(define notes/outline/all-todos
  (lambda
    ()
    (list/foldl
      (lambda
        (acc row)
        (let
          ((f (cdr row)))
          (let
            ((slug (as-str (dict-get f "slug")))
              (tree (outline/decode (dict-get f "outline"))))
            (list/foldl
              (lambda
                (a t)
                (cons
                  (list slug (car t) (car (cdr t)) (car (cdr (cdr t))))
                  a))
              acc
              (outline/todos tree)))))
      (list)
      (notes/doc/list))))

(define notes/outline/apply-op
  (lambda
    (slug tree op nid)
    (cond
      ((string-eq op "add-child")
        (outline/add-child-by-id
          tree
          nid
          (notes/outline/new-node)))
      ((string-eq op "add-sibling")
        (outline/insert-sibling
          tree
          nid
          (notes/outline/new-node)))
      ((string-eq op "delete")
        (if
          (string-eq (as-str nid) "root")
          tree
          (outline/delete tree nid)))
      ((string-eq op "move-up") (outline/move-up tree nid))
      ((string-eq op "move-down")
        (outline/move-down tree nid))
      ((string-eq op "promote")
        (if
          (string-eq (as-str nid) "root")
          tree
          (outline/promote tree nid)))
      ((string-eq op "demote")
        (if
          (string-eq (as-str nid) "root")
          tree
          (outline/demote tree nid)))
      ((string-eq op "set-title")
        (outline/update
          tree
          nid
          (lambda
            (n)
            (outline/set-title n (request-param "title")))))
      ((string-eq op "set-body")
        (outline/update
          tree
          nid
          (lambda (n) (outline/set-body n (request-param "body")))))
      ((string-eq op "set-todo")
        (outline/update
          tree
          nid
          (lambda (n) (outline/set-todo n (request-param "todo")))))
      ((string-eq op "cycle-todo")
        (outline/update
          tree
          nid
          (lambda (n) (outline/cycle-todo n))))
      ((string-eq op "archive")
        (if
          (string-eq (as-str nid) "root")
          tree
          (outline/update
            tree
            nid
            (lambda (n) (outline/archive n)))))
      ((string-eq op "unarchive")
        (if
          (string-eq (as-str nid) "root")
          tree
          (outline/update
            tree
            nid
            (lambda (n) (outline/unarchive n)))))
      ((string-eq op "slurp-forward")
        (if
          (string-eq (as-str nid) "root")
          tree
          (car
            (structed/slurp-forward
              outline/kids
              outline/set-kids
              tree
              nid))))
      ((string-eq op "barf-forward")
        (if
          (string-eq (as-str nid) "root")
          tree
          (car
            (structed/barf-forward
              outline/kids
              outline/set-kids
              tree
              nid))))
      (else tree))))

(define notes/outline/apply-op-sel
  (lambda
    (slug tree op nid)
    (let
      ((new-tree (notes/outline/apply-op slug tree op nid)))
      (let
        ((sel (notes/outline/sel-for op nid new-tree tree)))
        (list new-tree sel)))))

(define notes/outline/cycle-todo
  (lambda
    (slug nid)
    (let
      ((prev (notes/doc/get slug)))
      (if
        (null? prev)
        ()
        (let
          ((tree (notes/doc/outline prev))
            (title (notes/doc/title prev))
            (tags (notes/doc/tags prev))
            (kind (notes/doc/kind prev))
            (day (notes/doc/day prev)))
          (let
            ((new-tree
              (outline/update
                tree
                nid
                (lambda (n) (outline/cycle-todo n)))))
            (notes/doc/upsert
              slug
              title
              new-tree
              tags
              kind
              day)))))))

(define notes/outline/eval-form
  (lambda
    (slug nid)
    (html/form
      "POST"
      "/partials/notes-node-op"
      (html/attrs
        (list
          (html/attr "class" "ol-eval")
          (html/attr "hx-post" "/partials/notes-node-op")
          (html/attr "hx-target" "#outline")
          (html/attr "hx-swap" "outerHTML")))
      (html/join
        (list
          (html/hidden "slug" (as-str slug))
          (html/hidden "op" "eval")
          (html/hidden "node-id" (as-str nid))
          (html/text-input
            "expr"
            (html/attrs
              (list
                (html/attr
                  "placeholder"
                  "(outline/node (gensym \"n\") \"hi\")")
                (html/attr "class" "ol-eval-expr")
                (html/attr "style" "width:12rem"))))
          (html/select
            "target"
            (html/attr "class" "ol-eval-tgt")
            (html/join
              (list
                (html/option "child" "child" #t)
                (html/option "sibling" "sibling" #f)
                (html/option "body" "body" #f))))
          (html/button
            (html/attrs
              (list
                (html/attr "type" "submit")
                (html/attr "class" "ol-btn")
                (html/attr "title" "eval Lisp and splice result")))
            "eval"))))))

(define notes/outline/eval-result
  (lambda
    (slug nid expr)
    (let
      ((prev (notes/doc/get slug)))
      (if
        (null? prev)
        ()
        (let
          ((tree (notes/doc/outline prev)))
          (let
            ((node (outline/find tree (as-str nid))))
            (if
              (null? node)
              ()
              (seq
                (load-string
                  (string-append
                    "(define __notes_eval_fn (lambda (self doc slug) "
                    (as-str expr)
                    "))"))
                (__notes_eval_fn node prev (as-str slug))))))))))

(define notes/outline/eval-target
  (lambda
    ()
    (let
      ((tg (request-param "target")))
      (if
        (if (null? tg) #t (string-eq (as-str tg) ""))
        "child"
        (as-str tg)))))

(define notes/outline/new-node
  (lambda () (outline/node (gensym "n") "New node")))

(define notes/outline/node-op
  (lambda
    ()
    (let
      ((slug (as-str (request-param "slug")))
        (op (as-str (request-param "op")))
        (nid (as-str (request-param "node-id")))
        (show-archived (notes/doc/show-archived?)))
      (let
        ((prev (notes/doc/get slug)))
        (if
          (null? prev)
          (render-html
            (html/p* (html/attr "class" "err") "missing"))
          (let
            ((tree (notes/doc/outline prev))
              (title (notes/doc/title prev))
              (tags (notes/doc/tags prev))
              (kind (notes/doc/kind prev))
              (day (notes/doc/day prev)))
            (let
              ((res
                (if
                  (string-eq op "eval")
                  (notes/outline/node-op-eval tree nid slug)
                  (notes/outline/apply-op-sel slug tree op nid))))
              (let
                ((new-tree (car res)) (sel (as-str (car (cdr res)))))
                (seq
                  (notes/doc/upsert
                    slug
                    title
                    new-tree
                    tags
                    kind
                    day)
                  (render-html
                    (string-append
                      (outline/html slug new-tree show-archived)
                      (html/void
                        "input"
                        (html/attrs
                          (list
                            (html/attr "type" "hidden")
                            (html/attr "id" "ol-selected-id")
                            (html/attr "hx-swap-oob" "true")
                            (html/attr "value" sel)))))))))))))))

(define notes/outline/node-op-eval
  (lambda
    (tree nid slug)
    (let
      ((et
        (notes/outline/splice-eval
          tree
          nid
          (notes/outline/eval-target)
          (notes/outline/eval-result
            slug
            nid
            (request-param "expr")))))
      (list et nid))))

(define notes/outline/open-todos
  (lambda
    ()
    (list/filter
      (lambda
        (t)
        (outline/todo-open? (car (cdr (cdr (cdr t))))))
      (notes/outline/all-todos))))

(define notes/outline/partial
  (lambda
    ()
    (let
      ((n (notes/doc/get (request-param "slug")))
        (show-archived (notes/doc/show-archived?)))
      (if
        (null? n)
        (render-html
          (html/p* (html/attr "class" "err") "missing"))
        (render-html
          (outline/html
            (as-str (request-param "slug"))
            (notes/doc/outline n)
            show-archived))))))

(define notes/outline/sel-after-add-child
  (lambda
    (new-tree nid)
    (let
      ((node (structed/find outline/kids new-tree nid)))
      (if
        (null? node)
        nid
        (let
          ((ks (outline/kids node)))
          (if
            (null? ks)
            nid
            (structed/id (structed/list/nth ks 0))))))))

(define notes/outline/sel-after-add-sibling
  (lambda
    (new-tree nid)
    (let
      ((parent
        (structed/find-parent outline/kids new-tree nid)))
      (if
        (null? parent)
        nid
        (let
          ((sibs (outline/kids parent)))
          (let
            ((i (structed/sibling-index outline/kids new-tree nid)))
            (if
              (>= (+ i 1) (structed/count sibs))
              nid
              (structed/id (structed/list/nth sibs (+ i 1))))))))))

(define notes/outline/sel-after-delete
  (lambda
    (tree nid)
    (let
      ((parent (structed/find-parent outline/kids tree nid)))
      (if
        (null? parent)
        nid
        (notes/outline/sel-after-delete-in
          parent
          (outline/kids parent)
          (structed/sibling-index outline/kids tree nid))))))

(define notes/outline/sel-after-delete-in
  (lambda
    (parent sibs i)
    (let
      ((after (structed/list/drop sibs (+ i 1))))
      (if
        (not (null? after))
        (structed/id (car after))
        (if
          (> i 0)
          (structed/id (structed/list/nth sibs (- i 1)))
          (structed/id parent))))))

(define notes/outline/sel-for
  (lambda
    (op nid new-tree old-tree)
    (cond
      ((string-eq op "delete")
        (if
          (string-eq (as-str nid) "root")
          nid
          (notes/outline/sel-after-delete old-tree nid)))
      ((string-eq op "add-child")
        (notes/outline/sel-after-add-child new-tree nid))
      ((string-eq op "add-sibling")
        (notes/outline/sel-after-add-sibling new-tree nid))
      (else nid))))

(define notes/outline/splice-eval
  (lambda
    (tree nid target result)
    (let
      ((t (as-str target)))
      (if
        (string-eq t "body")
        (outline/update
          tree
          nid
          (lambda (n) (outline/set-body n (as-str result))))
        (let
          ((child
            (if
              (outline/valid? result)
              result
              (outline/node (gensym "n") (as-str result)))))
          (if
            (string-eq t "sibling")
            (outline/insert-sibling tree nid child)
            (outline/add-child-by-id tree nid child)))))))

