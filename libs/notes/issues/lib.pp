;; package notes/issues -- exported from live branch system/main
;; Sync: scripts/pp_sync.py pull|push

(define notes/issues/add
  (lambda
    (kind title body)
    (let
      ((n (notes/issues/ensure)))
      (let
        ((tree (notes/doc/outline n))
          (full-title
            (string-append
              (as-str kind)
              ": "
              (notes/issues/title-or-kind kind title))))
        (let
          ((child
            (outline/set-body
              (outline/node (notes/issues/node-id tree) full-title)
              (as-str body))))
          (let
            ((new-tree (outline/add-child-by-id tree "root" child)))
            (notes/issues/upsert new-tree)))))))

(define notes/issues/body
  (lambda
    ()
    (html/join
      (list
        (html/h1 "Issues")
        (html/p*
          (html/attr "class" "muted")
          (html/join
            (list
              (html/a "/notes" "Notes")
              " · "
              (html/a "/notes?slug=log/issues" "Raw note"))))
        (notes/issues/form)
        (notes/issues/list-html)))))

(define notes/issues/delete
  (lambda
    (id)
    (let
      ((n (notes/doc/get "log/issues")))
      (if
        (null? n)
        #f
        (let
          ((tree (notes/doc/outline n)))
          (let
            ((new-tree (outline/delete tree (as-str id))))
            (seq (notes/issues/upsert new-tree) #t)))))))

(define notes/issues/ensure
  (lambda
    ()
    (let
      ((n (notes/doc/get "log/issues")))
      (if
        (null? n)
        (seq
          (notes/doc/upsert
            "log/issues"
            "log/issues"
            (notes/empty-outline "log/issues")
            "log"
            "issue"
            "")
          (notes/doc/get "log/issues"))
        n))))

(define notes/issues/form
  (lambda
    ()
    (html/form
      "POST"
      "/issues"
      (html/attr "class" "card")
      (html/join
        (list
          (html/h2 "File an issue")
          (html/hidden "action" "issue-add")
          (html/label "" "Kind ")
          (html/select
            "kind"
            ""
            (html/join
              (list
                (html/option "BUG" "BUG" #t)
                (html/option "DESIGN" "DESIGN" #f)
                (html/option "SMELL" "SMELL" #f)
                (html/option "CLEANUP" "CLEANUP" #f))))
          " "
          (html/label "" "Title ")
          (html/text-input
            "title"
            (html/attr "placeholder" "One-line title"))
          " "
          (html/textarea
            "body"
            (html/attrs
              (list
                (html/attr "rows" "3")
                (html/attr "placeholder" "Details")))
            "")
          " "
          (html/button
            (html/attr "type" "submit")
            (html/text "File issue")))))))

(define notes/issues/handle-post
  (lambda
    ()
    (let
      ((action (as-str (request-param "action"))))
      (cond
        ((string-eq action "issue-add")
          (seq
            (notes/issues/add
              (request-param "kind")
              (request-param "title")
              (request-param "body"))
            (redirect "/issues")))
        ((string-eq action "issue-resolve")
          (seq
            (notes/issues/resolve (request-param "id"))
            (redirect "/issues")))
        ((string-eq action "issue-reopen")
          (seq
            (notes/issues/reopen (request-param "id"))
            (redirect "/issues")))
        ((string-eq action "issue-delete")
          (seq
            (notes/issues/delete (request-param "id"))
            (redirect "/issues")))
        (else (redirect "/issues"))))))

(define notes/issues/list-html
  (lambda
    ()
    (let
      ((n (notes/doc/get "log/issues")))
      (if
        (null? n)
        (html/p* (html/attr "class" "muted") "No issues yet.")
        (let
          ((tree (notes/doc/outline n)))
          (html/join
            (list
              (html/h2 "Register")
              (html/ul
                (html/attr "class" "card")
                (notes/issues/nodes-html (outline/kids tree))))))))))

(define notes/issues/node-html
  (lambda
    (node)
    (let
      ((id (outline/id node))
        (title (outline/title node))
        (body (outline/body node))
        (done (string-eq (outline/todo node) "DONE")))
      (html/li
        (html/attr "class" "issue-item")
        (html/join
          (list
            (html/div
              (html/attr
                "class"
                (if done "issue-head done" "issue-head"))
              (html/join
                (list (html/code id) " " (html/strong title))))
            (if
              (string-eq (as-str body) "")
              ""
              (html/p* (html/attr "class" "issue-body") (as-str body)))
            (html/div
              (html/attr "class" "issue-actions")
              (html/join
                (list
                  (html/form
                    "POST"
                    "/issues"
                    ""
                    (html/join
                      (list
                        (html/hidden
                          "action"
                          (if done "issue-reopen" "issue-resolve"))
                        (html/hidden "id" id)
                        (html/button
                          (html/attr "type" "submit")
                          (html/text (if done "Reopen" "Resolve"))))))
                  " "
                  (html/form
                    "POST"
                    "/issues"
                    ""
                    (html/join
                      (list
                        (html/hidden "action" "issue-delete")
                        (html/hidden "id" id)
                        (html/button
                          (html/attrs
                            (list
                              (html/attr "type" "submit")
                              (html/attr "class" "secondary")))
                          (html/text "Del"))))))))))))))

(define notes/issues/node-id
  (lambda (tree) (string-append "i-" (as-str (now)))))

(define notes/issues/nodes-html
  (lambda
    (nodes)
    (if
      (null? nodes)
      ""
      (html/join
        (list
          (notes/issues/node-html (car nodes))
          (notes/issues/nodes-html (cdr nodes)))))))

(define notes/issues/page
  (lambda
    ()
    (if
      (string-eq (request-method) "POST")
      (notes/issues/handle-post)
      (html/page (list (notes/issues/body))))))

(define notes/issues/reopen
  (lambda (id) (notes/issues/set-todo id "")))

(define notes/issues/resolve
  (lambda (id) (notes/issues/set-todo id "DONE")))

(define notes/issues/set-todo
  (lambda
    (id todo)
    (let
      ((n (notes/doc/get "log/issues")))
      (if
        (null? n)
        #f
        (let
          ((tree (notes/doc/outline n)))
          (let
            ((new-tree
              (outline/update
                tree
                (as-str id)
                (lambda (node) (outline/set-todo node todo)))))
            (seq (notes/issues/upsert new-tree) #t)))))))

(define notes/issues/title-or-kind
  (lambda
    (kind title)
    (let
      ((t (string-trim (as-str title))))
      (if (string-eq t "") (as-str kind) t))))

(define notes/issues/upsert
  (lambda
    (tree)
    (let
      ((n (notes/doc/get "log/issues")))
      (notes/doc/upsert
        "log/issues"
        "log/issues"
        tree
        (if (null? n) "log" (notes/doc/tags n))
        "issue"
        (if (null? n) "" (notes/doc/day n))))))
