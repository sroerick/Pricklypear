;; package notes -- exported from live branch system/main
;; Sync: scripts/pp_sync.py pull|push

(define notes/all-todos
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
      (notes/list))))

(define notes/apply-op
  (lambda
    (slug tree op nid)
    (if
      (string-eq op "add-child")
      (outline/add-child-by-id
        tree
        nid
        (outline/node "n-new" "New node"))
      (if
        (string-eq op "add-sibling")
        (outline/insert-sibling
          tree
          nid
          (outline/node "n-new" "New node"))
        (if
          (string-eq op "delete")
          (outline/delete tree nid)
          (if
            (string-eq op "move-up")
            (outline/move-up tree nid)
            (if
              (string-eq op "move-down")
              (outline/move-down tree nid)
              (if
                (string-eq op "set-title")
                (outline/update
                  tree
                  nid
                  (lambda
                    (n)
                    (outline/set-title n (request-param "title"))))
                (if
                  (string-eq op "set-body")
                  (outline/update
                    tree
                    nid
                    (lambda (n) (outline/set-body n (request-param "body"))))
                  (if
                    (string-eq op "set-todo")
                    (outline/update
                      tree
                      nid
                      (lambda (n) (outline/set-todo n (request-param "todo"))))
                    (if
                      (string-eq op "cycle-todo")
                      (outline/update
                        tree
                        nid
                        (lambda (n) (outline/cycle-todo n)))
                      tree)))))))))))

(define notes/capture
  (lambda
    (title body)
    (let
      ((prev (notes/ensure-inbox)))
      (let
        ((tree (notes/get-outline prev))
          (t (notes/capture-title title body))
          (b (as-str body)))
        (let
          ((child
            (outline/set-body
              (outline/node (notes/capture-node-id tree) t)
              b)))
          (let
            ((new-tree (outline/add-child-by-id tree "root" child)))
            (seq
              (notes/upsert
                "inbox"
                "Inbox"
                new-tree
                "inbox"
                "inbox"
                "")
              "inbox")))))))

(define notes/capture-form
  (lambda
    ()
    (html/form
      "POST"
      "/notes"
      (html/attrs
        (list
          (html/attr "class" "card capture-card")
          (html/attr "hx-post" "/notes")
          (html/attr "hx-target" "#capture-status")
          (html/attr "hx-swap" "innerHTML")
          (html/attr
            "hx-on::after-request"
            "if(event.detail.successful){this.reset()}")))
      (html/join
        (list
          (html/hidden "action" "capture")
          (html/h2 "Capture")
          (html/p*
            (html/attr "class" "muted")
            "Drops a node into the Inbox note.")
          (html/label (html/attr "for" "capture-title") "Title")
          (html/text-input
            "title"
            (html/attrs
              (list
                (html/attr "id" "capture-title")
                (html/attr "placeholder" "optional")
                (html/attr "autocomplete" "off"))))
          (html/label (html/attr "for" "capture-body") "Note")
          (html/textarea
            "body"
            (html/attrs
              (list
                (html/attr "id" "capture-body")
                (html/attr "rows" "3")
                (html/attr "class" "capture-body")
                (html/attr "placeholder" "Quick capture\226\128\166")
                (html/attr "required" "required")))
            "")
          (html/div
            (html/attr "class" "capture-row")
            (html/join
              (list
                (html/button (html/attr "type" "submit") "Capture")
                (html/a "/notes?slug=inbox" "Open inbox"))))
          (html/div (html/attr "id" "capture-status") ""))))))

(define notes/capture-nav
  (lambda
    ()
    (html/tag
      "details"
      (html/attr "class" "menu-drop capture-drop")
      (html/join
        (list
          (html/tag
            "summary"
            (html/attr "class" "menu-drop-btn")
            "capture")
          (html/div
            (html/attr "class" "menu-drop-panel capture-panel")
            (html/form
              "POST"
              "/notes"
              (html/attrs
                (list
                  (html/attr "class" "capture-nav-form")
                  (html/attr "hx-post" "/notes")
                  (html/attr "hx-target" "#capture-status")
                  (html/attr "hx-swap" "innerHTML")
                  (html/attr
                    "hx-on::after-request"
                    "if(event.detail.successful){this.reset()}")))
              (html/join
                (list
                  (html/hidden "action" "capture")
                  (html/text-input
                    "title"
                    (html/attrs
                      (list
                        (html/attr "placeholder" "title (optional)")
                        (html/attr "autocomplete" "off")
                        (html/attr "class" "capture-nav-title"))))
                  (html/textarea
                    "body"
                    (html/attrs
                      (list
                        (html/attr "rows" "3")
                        (html/attr "class" "capture-body")
                        (html/attr "placeholder" "Quick capture\226\128\166")
                        (html/attr "required" "required")))
                    "")
                  (html/div
                    (html/attr "class" "capture-row")
                    (html/join
                      (list
                        (html/button (html/attr "type" "submit") "Save")
                        (html/a "/notes?slug=inbox" "inbox"))))
                  (html/div (html/attr "id" "capture-status") ""))))))))))

(define notes/capture-node-id
  (lambda
    (tree)
    (string-append
      "c"
      (as-str (count (outline/kids tree)))
      "-"
      (as-str (string-length (outline/encode tree))))))

(define notes/capture-page
  (lambda
    ()
    (if
      (string-eq (request-method) "POST")
      (notes/do-capture)
      (html/page
        (list (html/h1 "Capture") (notes/capture-form))))))

(define notes/capture-status-html
  (lambda
    (ok msg)
    (html/p* (html/attr "class" (if ok "ok" "err")) msg)))

(define notes/capture-title
  (lambda
    (title body)
    (let
      ((t (string-trim (as-str title))))
      (if
        (not (string-eq t ""))
        t
        (let
          ((b (string-trim (as-str body))))
          (if
            (string-eq b "")
            "capture"
            (if
              (<= (string-length b) 48)
              b
              (string-append (substring b 0 45) "\226\128\166"))))))))

(define notes/capture/add
  (lambda
    (title body)
    (let
      ((prev (notes/doc/ensure-inbox)))
      (let
        ((tree (notes/doc/outline prev))
          (t (notes/capture/title title body))
          (b (as-str body)))
        (let
          ((child
            (outline/set-body
              (outline/node (notes/capture/node-id tree) t)
              b)))
          (let
            ((new-tree (outline/add-child-by-id tree "root" child)))
            (seq
              (notes/doc/upsert
                "inbox"
                "Inbox"
                new-tree
                "inbox"
                "inbox"
                "")
              "inbox")))))))

(define notes/capture/form
  (lambda
    ()
    (html/form
      "POST"
      "/notes"
      (html/attrs
        (list
          (html/attr "class" "card capture-card")
          (html/attr "hx-post" "/notes")
          (html/attr "hx-target" "#capture-status")
          (html/attr "hx-swap" "innerHTML")
          (html/attr
            "hx-on::after-request"
            "if(event.detail.successful){this.reset()}")))
      (html/join
        (list
          (html/hidden "action" "capture")
          (html/h2 "Capture")
          (html/p*
            (html/attr "class" "muted")
            "Drops a node into the Inbox note.")
          (html/label (html/attr "for" "capture-title") "Title")
          (html/text-input
            "title"
            (html/attrs
              (list
                (html/attr "id" "capture-title")
                (html/attr "placeholder" "optional")
                (html/attr "autocomplete" "off"))))
          (html/label (html/attr "for" "capture-body") "Note")
          (html/textarea
            "body"
            (html/attrs
              (list
                (html/attr "id" "capture-body")
                (html/attr "rows" "3")
                (html/attr "class" "capture-body")
                (html/attr "placeholder" "Quick capture\226\128\166")
                (html/attr "required" "required")))
            "")
          (html/div
            (html/attr "class" "capture-row")
            (html/join
              (list
                (html/button (html/attr "type" "submit") "Capture")
                (html/a "/notes?slug=inbox" "Open inbox"))))
          (html/div (html/attr "id" "capture-status") ""))))))

(define notes/capture/handle
  (lambda
    ()
    (let
      ((title (request-param "title"))
        (body (request-param "body")))
      (let
        ((b (if (null? body) "" (as-str body))))
        (if
          (string-eq (string-trim b) "")
          (if
            (hx-request?)
            (render-html
              (notes/capture/status #f "Write something first."))
            (redirect "/"))
          (seq
            (notes/capture/add (if (null? title) "" title) b)
            (if
              (hx-request?)
              (render-html
                (notes/capture/status #t "Saved to Inbox."))
              (redirect "/notes?slug=inbox"))))))))

(define notes/capture/nav
  (lambda
    ()
    (html/tag
      "details"
      (html/attr "class" "menu-drop capture-drop")
      (html/join
        (list
          (html/tag
            "summary"
            (html/attr "class" "menu-drop-btn")
            "capture")
          (html/div
            (html/attr "class" "menu-drop-panel capture-panel")
            (html/form
              "POST"
              "/notes"
              (html/attrs
                (list
                  (html/attr "class" "capture-nav-form")
                  (html/attr "hx-post" "/notes")
                  (html/attr "hx-target" "#capture-status")
                  (html/attr "hx-swap" "innerHTML")
                  (html/attr
                    "hx-on::after-request"
                    "if(event.detail.successful){this.reset()}")))
              (html/join
                (list
                  (html/hidden "action" "capture")
                  (html/text-input
                    "title"
                    (html/attrs
                      (list
                        (html/attr "placeholder" "title (optional)")
                        (html/attr "autocomplete" "off")
                        (html/attr "class" "capture-nav-title"))))
                  (html/textarea
                    "body"
                    (html/attrs
                      (list
                        (html/attr "rows" "3")
                        (html/attr "class" "capture-body")
                        (html/attr "placeholder" "Quick capture\226\128\166")
                        (html/attr "required" "required")))
                    "")
                  (html/div
                    (html/attr "class" "capture-row")
                    (html/join
                      (list
                        (html/button (html/attr "type" "submit") "Save")
                        (html/a "/notes?slug=inbox" "inbox"))))
                  (html/div (html/attr "id" "capture-status") ""))))))))))

(define notes/capture/node-id
  (lambda
    (tree)
    (string-append
      "c"
      (as-str (count (outline/kids tree)))
      "-"
      (as-str (string-length (outline/encode tree))))))

(define notes/capture/page
  (lambda
    ()
    (if
      (string-eq (request-method) "POST")
      (notes/capture/handle)
      (html/page
        (list (html/h1 "Capture") (notes/capture/form))))))

(define notes/capture/status
  (lambda
    (ok msg)
    (html/p* (html/attr "class" (if ok "ok" "err")) msg)))

(define notes/capture/title
  (lambda
    (title body)
    (let
      ((t (string-trim (as-str title))))
      (if
        (not (string-eq t ""))
        t
        (let
          ((b (string-trim (as-str body))))
          (if
            (string-eq b "")
            "capture"
            (if
              (<= (string-length b) 48)
              b
              (string-append (substring b 0 45) "..."))))))))

(define notes/create-form
  (lambda
    ()
    (html/form
      "POST"
      "/notes"
      (html/attr "class" "card")
      (html/join
        (list
          (html/hidden "action" "create")
          (html/label "" "Title ")
          (html/text-input
            "title"
            (html/attr "placeholder" "New note"))
          " "
          (html/label "" "Slug ")
          (html/text-input
            "slug"
            (html/attr "placeholder" "my-note"))
          " "
          (html/button (html/attr "type" "submit") "Create"))))))

(define notes/cycle-node-todo
  (lambda
    (slug nid)
    (let
      ((prev (notes/get slug)))
      (if
        (null? prev)
        ()
        (let
          ((tree (notes/get-outline prev))
            (title (notes/get-title prev))
            (tags (notes/get-tags prev))
            (kind (notes/get-kind prev))
            (day (notes/get-day prev)))
          (let
            ((new-tree
              (outline/update
                tree
                nid
                (lambda (n) (outline/cycle-todo n)))))
            (notes/upsert
              slug
              title
              new-tree
              tags
              kind
              day)))))))

(define notes/daily
  (lambda
    (day)
    (let
      ((slug (string-append "daily-" (as-str day))))
      (if
        (null? (notes/get slug))
        (seq
          (notes/upsert
            slug
            (string-append "Daily " (as-str day))
            (notes/empty-outline
              (string-append "Daily " (as-str day)))
            "daily"
            "daily"
            (as-str day))
          (notes/get slug))
        (notes/get slug)))))

(define notes/delete
  (lambda
    (slug)
    (let
      ((row (notes/row (as-str slug))))
      (if (null? row) () (delete (quote note) (car row))))))

(define notes/do-capture
  (lambda
    ()
    (let
      ((title (request-param "title"))
        (body (request-param "body")))
      (let
        ((b (if (null? body) "" (as-str body))))
        (if
          (string-eq (string-trim b) "")
          (if
            (hx-request?)
            (render-html
              (notes/capture-status-html #f "Write something first."))
            (redirect "/"))
          (seq
            (notes/capture (if (null? title) "" title) b)
            (if
              (hx-request?)
              (render-html
                (notes/capture-status-html #t "Saved to Inbox."))
              (redirect "/notes?slug=inbox"))))))))

(define notes/do-create
  (lambda
    (title slug)
    (let
      ((s
        (if
          (if
            (null? slug)
            #t
            (string-eq (string-trim (as-str slug)) ""))
          (string-append "note-" (as-str (count (notes/list))))
          (string-trim (as-str slug)))))
      (seq
        (notes/upsert
          s
          (as-str title)
          (notes/empty-outline (as-str title))
          ""
          "note"
          "")
        (redirect (string-append "/notes?slug=" s))))))

(define notes/do-delete
  (lambda
    ()
    (let
      ((slug (as-str (request-param "slug"))))
      (seq (notes/delete slug) (redirect "/notes")))))

(define notes/do-refile
  (lambda
    ()
    (let
      ((src (as-str (request-param "src_slug")))
        (nid (as-str (request-param "node_id")))
        (dst (as-str (request-param "dst_slug"))))
      (seq
        (notes/refile src nid dst)
        (redirect (string-append "/notes?slug=" src))))))

(define notes/do-save
  (lambda
    (slug raw)
    (let
      ((tree (outline/decode raw)) (prev (notes/get slug)))
      (let
        ((title
            (if (null? prev) (as-str slug) (notes/get-title prev)))
          (tags (if (null? prev) "" (notes/get-tags prev)))
          (kind (if (null? prev) "note" (notes/get-kind prev)))
          (day (if (null? prev) "" (notes/get-day prev))))
        (seq
          (notes/upsert
            (as-str slug)
            title
            tree
            tags
            kind
            day)
          (redirect (string-append "/notes?slug=" (as-str slug))))))))

(define notes/doc/daily
  (lambda
    (day)
    (let
      ((slug (string-append "daily-" (as-str day))))
      (if
        (null? (notes/doc/get slug))
        (seq
          (notes/doc/upsert
            slug
            (string-append "Daily " (as-str day))
            (notes/doc/empty-outline
              (string-append "Daily " (as-str day)))
            "daily"
            "daily"
            (as-str day))
          (notes/doc/get slug))
        (notes/doc/get slug)))))

(define notes/doc/day
  (lambda (n) (car (cdr (cdr (cdr (cdr (cdr (cdr n)))))))))

(define notes/doc/delete
  (lambda
    (slug)
    (let
      ((row (notes/doc/row (as-str slug))))
      (if
        (null? row)
        ()
        (seq
          (roam/delete-from-slug (as-str slug))
          (delete (quote note) (car row)))))))

(define notes/doc/empty-outline
  (lambda (title) (outline/node "root" title)))

(define notes/doc/ensure-inbox
  (lambda
    ()
    (if
      (null? (notes/doc/get "inbox"))
      (seq
        (notes/doc/upsert
          "inbox"
          "Inbox"
          (notes/doc/empty-outline "Inbox")
          "inbox"
          "inbox"
          "")
        (notes/doc/get "inbox"))
      (notes/doc/get "inbox"))))

(define notes/doc/folder
  (lambda
    (n)
    (if
      (null? n)
      ""
      (let
        ((f (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr n))))))))))
        (if (null? f) "" (as-str f))))))

(define notes/doc/folder-names
  (lambda
    ()
    (list/foldl
      (lambda
        (acc row)
        (let
          ((f (notes/doc/row-folder row)))
          (if
            (notes/doc/folder-seen? acc f)
            acc
            (append acc (list f)))))
      (list)
      (notes/doc/index-list))))

(define notes/doc/folder-seen?
  (lambda
    (xs f)
    (if
      (null? xs)
      #f
      (if
        (string-eq (car xs) f)
        #t
        (notes/doc/folder-seen? (cdr xs) f)))))

(define notes/doc/get
  (lambda
    (slug)
    (let
      ((row (notes/doc/row slug)))
      (if
        (null? row)
        ()
        (let
          ((f (cdr row)))
          (list
            (car row)
            (dict-get f "slug")
            (dict-get f "title")
            (outline/decode (dict-get f "outline"))
            (dict-get f "tags")
            (dict-get f "kind")
            (dict-get f "day")
            (let
              ((folder (dict-get f "folder")))
              (if (null? folder) "" folder))))))))

(define notes/doc/hidden?
  (lambda
    (row)
    (let
      ((f (cdr row)))
      (let
        ((kind (as-str (dict-get f "kind")))
          (tags (as-str (dict-get f "tags")))
          (slug (as-str (dict-get f "slug"))))
        (if
          (string-eq kind "inbox")
          #t
          (if
            (string-eq kind "hidden")
            #t
            (if
              (string-eq slug "inbox")
              #t
              (string-contains? tags "hidden"))))))))

(define notes/doc/index-list
  (lambda
    ()
    (list/filter
      (lambda (row) (not (notes/doc/hidden? row)))
      (notes/doc/list))))

(define notes/doc/kind
  (lambda (n) (car (cdr (cdr (cdr (cdr (cdr n))))))))

(define notes/doc/list
  (lambda () (rows (quote note) 100)))

(define notes/doc/list-folder
  (lambda
    (folder)
    (let
      ((want (as-str folder)))
      (list/filter
        (lambda
          (row)
          (string-eq (notes/doc/row-folder row) want))
        (notes/doc/index-list)))))

(define notes/doc/make-fields
  (lambda
    (slug
      title
      outline
      tags
      kind
      day
      folder)
    (dict-set*
      "{}"
      (list
        "slug"
        (as-str slug)
        "title"
        (as-str title)
        "outline"
        (outline/encode outline)
        "tags"
        (as-str tags)
        "kind"
        (as-str kind)
        "day"
        (as-str day)
        "folder"
        (as-str folder)))))

(define notes/doc/outline
  (lambda (n) (car (cdr (cdr (cdr n))))))

(define notes/doc/row
  (lambda
    (slug)
    (let
      ((rs (find-rows (quote note) "slug" (as-str slug) 1)))
      (if (null? rs) () (car rs)))))

(define notes/doc/row-folder
  (lambda
    (row)
    (if
      (null? row)
      ""
      (let
        ((f (dict-get (cdr row) "folder")))
        (if (null? f) "" (as-str f))))))

(define notes/doc/search
  (lambda
    (q)
    (find-text (quote note) "title" (as-str q) 40)))

(define notes/doc/set-folder
  (lambda
    (slug folder)
    (let
      ((row (notes/doc/row (as-str slug))))
      (if
        (null? row)
        ()
        (update-field
          (quote note)
          (car row)
          "folder"
          (as-str folder))))))

(define notes/doc/show-archived?
  (lambda
    ()
    (string-eq (as-str (request-param "show")) "archived")))

(define notes/doc/tags
  (lambda (n) (car (cdr (cdr (cdr (cdr n)))))))

(define notes/doc/title
  (lambda (n) (car (cdr (cdr n)))))

(define notes/doc/upsert
  (lambda
    (slug title outline tags kind day)
    (let
      ((row (notes/doc/row slug)))
      (let
        ((folder (notes/doc/row-folder row))
          (fields
            (notes/doc/make-fields
              slug
              title
              outline
              tags
              kind
              day
              (notes/doc/row-folder row))))
        (seq
          (if
            (null? row)
            (create (quote note) fields)
            (update (quote note) (car row) fields))
          (roam/reindex (as-str slug) outline))))))

(define notes/doc/upsert-in
  (lambda
    (slug
      title
      outline
      tags
      kind
      day
      folder)
    (seq
      (notes/doc/upsert
        slug
        title
        outline
        tags
        kind
        day)
      (notes/doc/set-folder slug folder)
      slug)))

(define notes/edit-form
  (lambda
    (slug outline)
    (html/form
      "POST"
      "/notes"
      ""
      (html/join
        (list
          (html/hidden "action" "save")
          (html/hidden "slug" slug)
          (html/tag
            "textarea"
            (html/attrs
              (list
                (html/attr "name" "outline")
                (html/attr "rows" "16")))
            (outline/encode outline))
          (html/p*
            ""
            (html/button (html/attr "type" "submit") "Save outline")))))))

(define notes/empty-outline
  (lambda (title) (outline/node "root" title)))

(define notes/ensure-inbox
  (lambda
    ()
    (if
      (null? (notes/get "inbox"))
      (seq
        (notes/upsert
          "inbox"
          "Inbox"
          (notes/empty-outline "Inbox")
          "inbox"
          "inbox"
          "")
        (notes/get "inbox"))
      (notes/get "inbox"))))

(define notes/get
  (lambda
    (slug)
    (let
      ((row (notes/row slug)))
      (if
        (null? row)
        ()
        (let
          ((f (cdr row)))
          (list
            (car row)
            (dict-get f "slug")
            (dict-get f "title")
            (outline/decode (dict-get f "outline"))
            (dict-get f "tags")
            (dict-get f "kind")
            (dict-get f "day")))))))

(define notes/get-day
  (lambda (n) (car (cdr (cdr (cdr (cdr (cdr (cdr n)))))))))

(define notes/get-kind
  (lambda (n) (car (cdr (cdr (cdr (cdr (cdr n))))))))

(define notes/get-outline
  (lambda (n) (car (cdr (cdr (cdr n))))))

(define notes/get-tags
  (lambda (n) (car (cdr (cdr (cdr (cdr n)))))))

(define notes/get-title
  (lambda (n) (car (cdr (cdr n)))))

(define notes/handle-post
  (lambda
    ()
    (let
      ((action (as-str (request-param "action"))))
      (if
        (string-eq action "create")
        (notes/do-create
          (request-param "title")
          (request-param "slug"))
        (if
          (string-eq action "save")
          (notes/do-save
            (request-param "slug")
            (request-param "outline"))
          (if
            (string-eq action "capture")
            (notes/do-capture)
            (if
              (string-eq action "delete")
              (notes/do-delete)
              (redirect "/notes"))))))))

(define notes/hidden-from-index?
  (lambda
    (row)
    (let
      ((f (cdr row)))
      (let
        ((kind (as-str (dict-get f "kind")))
          (tags (as-str (dict-get f "tags")))
          (slug (as-str (dict-get f "slug"))))
        (if
          (string-eq kind "inbox")
          #t
          (if
            (string-eq kind "hidden")
            #t
            (if
              (string-eq slug "inbox")
              #t
              (string-contains? tags "hidden"))))))))

(define notes/index-html
  (lambda
    ()
    (html/join
      (list
        (html/h1 "Notes")
        (html/p*
          (html/attr "class" "muted")
          "Outline-first notes. Create or open a slug.")
        (notes/create-form)
        (html/h2 "Recent")
        (html/ul (html/attr "class" "card") (notes/recent-list))))))

(define notes/index-list
  (lambda
    ()
    (list/filter
      (lambda (row) (not (notes/hidden-from-index? row)))
      (notes/list))))

(define notes/item-li
  (lambda
    (row)
    (let
      ((f (cdr row)) (id (car row)))
      (let
        ((slug (as-str (dict-get f "slug")))
          (title (as-str (dict-get f "title"))))
        (html/li
          (html/attr "class" "note-item")
          (html/join
            (list
              (html/a
                (string-append "/notes?slug=" slug)
                (html/text title))
              (html/form
                "POST"
                "/notes"
                (html/attrs
                  (list
                    (html/attr "class" "note-del")
                    (html/attr "hx-confirm" "Delete this note?")))
                (html/join
                  (list
                    (html/hidden "action" "delete")
                    (html/hidden "slug" slug)
                    (html/button
                      (html/attrs
                        (list
                          (html/attr "type" "submit")
                          (html/attr "class" "secondary")
                          (html/attr "title" "delete note")))
                      "del")))))))))))

(define notes/list
  (lambda () (rows (quote note) 100)))

(define notes/make-fields
  (lambda
    (slug title outline tags kind day)
    (dict-set*
      "{}"
      (list
        "slug"
        (as-str slug)
        "title"
        (as-str title)
        "outline"
        (outline/encode outline)
        "tags"
        (as-str tags)
        "kind"
        (as-str kind)
        "day"
        (as-str day)))))

(define notes/missing-html
  (lambda
    (slug)
    (html/join
      (list
        (html/h1 "Missing")
        (html/p*
          (html/attr "class" "err")
          (string-append "No note " (as-str slug)))
        (html/p* "" (html/a "/notes" "All notes"))))))

(define notes/node-op
  (lambda
    ()
    (let
      ((slug (as-str (request-param "slug")))
        (op (as-str (request-param "op")))
        (nid (as-str (request-param "node-id"))))
      (let
        ((prev (notes/get slug)))
        (if
          (null? prev)
          (render-html
            (html/p* (html/attr "class" "err") "missing"))
          (let
            ((tree (notes/get-outline prev))
              (title (notes/get-title prev))
              (tags (notes/get-tags prev))
              (kind (notes/get-kind prev))
              (day (notes/get-day prev)))
            (let
              ((new-tree (notes/apply-op slug tree op nid)))
              (seq
                (notes/upsert
                  slug
                  title
                  new-tree
                  tags
                  kind
                  day)
                (render-html (outline/html slug new-tree))))))))))

(define notes/open-todos
  (lambda
    ()
    (list/filter
      (lambda
        (t)
        (outline/todo-open? (car (cdr (cdr (cdr t))))))
      (notes/all-todos))))

(define notes/outline-partial
  (lambda
    ()
    (let
      ((n (notes/get (request-param "slug"))))
      (if
        (null? n)
        (render-html
          (html/p* (html/attr "class" "err") "missing"))
        (render-html
          (outline/html
            (as-str (request-param "slug"))
            (notes/get-outline n)))))))

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

(define notes/page
  (lambda
    ()
    (if
      (string-eq (request-method) "POST")
      (notes/ui/handle-post)
      (let
        ((slug (request-param "slug")))
        (if
          (if (null? slug) #t (string-eq (as-str slug) ""))
          (html/page (list (notes/ui/index)))
          (html/page (list (notes/ui/show (as-str slug)))))))))

(define notes/recent-list
  (lambda
    ()
    (list/foldl
      (lambda
        (acc row)
        (string-append acc (notes/item-li row)))
      ""
      (notes/list))))

(define notes/refile
  (lambda
    (src-slug nid dst-slug)
    (let
      ((src (notes/get src-slug))
        (dst (notes/get dst-slug))
        (sid (as-str src-slug))
        (did (as-str dst-slug))
        (id (as-str nid)))
      (if
        (if (null? src) #t (null? dst))
        #f
        (if
          (string-eq sid did)
          #f
          (if
            (string-eq id "root")
            #f
            (let
              ((src-tree (notes/get-outline src))
                (dst-tree (notes/get-outline dst)))
              (let
                ((node (outline/find src-tree id)))
                (if
                  (null? node)
                  #f
                  (seq
                    (notes/upsert
                      did
                      (notes/get-title dst)
                      (outline/add-child-by-id dst-tree "root" node)
                      (notes/get-tags dst)
                      (notes/get-kind dst)
                      (notes/get-day dst))
                    (seq
                      (notes/upsert
                        sid
                        (notes/get-title src)
                        (outline/delete src-tree id)
                        (notes/get-tags src)
                        (notes/get-kind src)
                        (notes/get-day src))
                      #t)))))))))))

(define notes/refile-form
  (lambda
    (src-slug nid)
    (if
      (string-eq (as-str nid) "root")
      ""
      (html/form
        "POST"
        "/notes"
        (html/attr "class" "ol-refile")
        (html/join
          (list
            (html/hidden "action" "refile")
            (html/hidden "src_slug" (as-str src-slug))
            (html/hidden "node_id" (as-str nid))
            (html/select
              "dst_slug"
              (html/attr "class" "ol-refile-sel")
              (notes/refile-options src-slug))
            (html/button
              (html/attrs
                (list
                  (html/attr "type" "submit")
                  (html/attr "class" "ol-btn")
                  (html/attr "title" "move node to another note")))
              "refile")))))))

(define notes/refile-options
  (lambda
    (src-slug)
    (list/foldl
      (lambda
        (acc row)
        (let
          ((f (cdr row)))
          (let
            ((slug (as-str (dict-get f "slug")))
              (title (as-str (dict-get f "title"))))
            (if
              (string-eq slug (as-str src-slug))
              acc
              (string-append
                acc
                (html/option
                  slug
                  (if (string-eq title "") slug title)
                  #f))))))
      ""
      (notes/list))))

(define notes/refile/extract
  (lambda
    (src-slug nid)
    (let
      ((src (notes/doc/get src-slug)) (id (as-str nid)))
      (if
        (null? src)
        ()
        (if
          (string-eq id "root")
          ()
          (let
            ((src-tree (notes/doc/outline src)))
            (let
              ((pair (outline/detach src-tree id)))
              (if
                (null? pair)
                ()
                (let
                  ((node (car pair)) (src-without (car (cdr pair))))
                  (let
                    ((title (as-str (outline/title node)))
                      (body (as-str (outline/body node)))
                      (kids (outline/kids node))
                      (tags (notes/doc/tags src)))
                    (let
                      ((new-slug
                          (notes/refile/unique-slug (notes/refile/slugify title)))
                        (new-tree
                          (outline/set-kids
                            (outline/set-body (outline/node "root" title) body)
                            kids)))
                      (with-transaction
                        (lambda
                          ()
                          (seq
                            (notes/doc/upsert
                              new-slug
                              title
                              new-tree
                              tags
                              "note"
                              "")
                            (notes/doc/upsert
                              (as-str src-slug)
                              (notes/doc/title src)
                              src-without
                              (notes/doc/tags src)
                              (notes/doc/kind src)
                              (notes/doc/day src))
                            new-slug))))))))))))))

(define notes/refile/extract-form
  (lambda
    (src-slug nid)
    (if
      (string-eq (as-str nid) "root")
      ""
      (html/form
        "POST"
        "/notes"
        (html/attr "class" "ol-extract")
        (html/join
          (list
            (html/hidden "action" "extract")
            (html/hidden "src_slug" (as-str src-slug))
            (html/hidden "node_id" (as-str nid))
            (html/button
              (html/attrs
                (list
                  (html/attr "type" "submit")
                  (html/attr "class" "ol-btn")
                  (html/attr "title" "extract node into its own note")))
              "extract")))))))

(define notes/refile/extract-handle
  (lambda
    ()
    (let
      ((src (as-str (request-param "src_slug")))
        (nid (as-str (request-param "node_id"))))
      (let
        ((new (notes/refile/extract src nid)))
        (if
          (null? new)
          (redirect (string-append "/notes?slug=" src))
          (redirect (string-append "/notes?slug=" new)))))))

(define notes/refile/form
  (lambda
    (src-slug nid)
    (if
      (string-eq (as-str nid) "root")
      ""
      (html/form
        "POST"
        "/notes"
        (html/attr "class" "ol-refile")
        (html/join
          (list
            (html/hidden "action" "refile")
            (html/hidden "src_slug" (as-str src-slug))
            (html/hidden "node_id" (as-str nid))
            (html/select
              "dst_slug"
              (html/attr "class" "ol-refile-sel")
              (notes/refile/options src-slug))
            (html/button
              (html/attrs
                (list
                  (html/attr "type" "submit")
                  (html/attr "class" "ol-btn")
                  (html/attr "title" "move node to another note")))
              "refile")))))))

(define notes/refile/handle
  (lambda
    ()
    (let
      ((src (as-str (request-param "src_slug")))
        (nid (as-str (request-param "node_id")))
        (dst (as-str (request-param "dst_slug"))))
      (seq
        (notes/refile/move src nid dst)
        (redirect (string-append "/notes?slug=" src))))))

(define notes/refile/move
  (lambda
    (src-slug nid dst-slug)
    (let
      ((src (notes/doc/get src-slug))
        (dst (notes/doc/get dst-slug))
        (sid (as-str src-slug))
        (did (as-str dst-slug))
        (id (as-str nid)))
      (if
        (if (null? src) #t (null? dst))
        #f
        (if
          (string-eq sid did)
          #f
          (if
            (string-eq id "root")
            #f
            (let
              ((src-tree (notes/doc/outline src))
                (dst-tree (notes/doc/outline dst)))
              (let
                ((pair (outline/detach src-tree id)))
                (if
                  (null? pair)
                  #f
                  (let
                    ((node (car pair)) (src-without (car (cdr pair))))
                    (with-transaction
                      (lambda
                        ()
                        (seq
                          (notes/doc/upsert
                            did
                            (notes/doc/title dst)
                            (outline/add-child-by-id dst-tree "root" node)
                            (notes/doc/tags dst)
                            (notes/doc/kind dst)
                            (notes/doc/day dst))
                          (notes/doc/upsert
                            sid
                            (notes/doc/title src)
                            src-without
                            (notes/doc/tags src)
                            (notes/doc/kind src)
                            (notes/doc/day src))
                          #t)))))))))))))

(define notes/refile/options
  (lambda
    (src-slug)
    (list/foldl
      (lambda
        (acc row)
        (let
          ((f (cdr row)))
          (let
            ((slug (as-str (dict-get f "slug")))
              (title (as-str (dict-get f "title"))))
            (if
              (string-eq slug (as-str src-slug))
              acc
              (string-append
                acc
                (html/option
                  slug
                  (if (string-eq title "") slug title)
                  #f))))))
      ""
      (notes/doc/list))))

(define notes/refile/slug-char-ok?
  (lambda
    (c)
    (string-contains?
      "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
      (as-str c))))

(define notes/refile/slugify
  (lambda
    (title)
    (let
      ((raw
        (notes/refile/slugify-acc
          (string-trim (as-str title))
          0
          ""
          #t)))
      (let
        ((n (string-length raw)))
        (let
          ((s
            (if
              (if
                (> n 0)
                (string-eq (substring raw (- n 1) 1) "-")
                #f)
              (substring raw 0 (- n 1))
              raw)))
          (if
            (string-eq s "")
            (string-append "note-" (as-str (gensym "n")))
            (if (> (string-length s) 48) (substring s 0 48) s)))))))

(define notes/refile/slugify-acc
  (lambda
    (s i acc last-hyphen?)
    (if
      (>= i (string-length s))
      acc
      (let
        ((c (substring s i 1)))
        (if
          (notes/refile/slug-char-ok? c)
          (notes/refile/slugify-acc
            s
            (+ i 1)
            (string-append acc c)
            #f)
          (if
            last-hyphen?
            (notes/refile/slugify-acc s (+ i 1) acc #t)
            (notes/refile/slugify-acc
              s
              (+ i 1)
              (string-append acc "-")
              #t)))))))

(define notes/refile/unique-slug
  (lambda
    (base)
    (if
      (null? (notes/doc/row (as-str base)))
      (as-str base)
      (notes/refile/unique-slug
        (string-append (as-str base) "-" (as-str (gensym "x")))))))

(define notes/row
  (lambda
    (slug)
    (let
      ((rs (find-rows (quote note) "slug" (as-str slug) 1)))
      (if (null? rs) () (car rs)))))

(define notes/search
  (lambda
    (q)
    (find-text (quote note) "title" (as-str q) 40)))

(define notes/show-archived?
  (lambda
    ()
    (string-eq (as-str (request-param "show")) "archived")))

(define notes/show-body
  (lambda
    (slug title outline tags)
    (html/join
      (list
        (html/p*
          (html/attr "class" "muted")
          (html/join
            (list
              (html/a "/notes" "Notes")
              " / "
              (html/code (as-str slug)))))
        (html/div
          (html/attr "class" "note-title-row")
          (html/join
            (list
              (html/h1 (html/text title))
              (html/form
                "POST"
                "/notes"
                (html/attrs
                  (list
                    (html/attr "class" "note-del note-del-page")
                    (html/attr "hx-confirm" "Delete this entire note?")))
                (html/join
                  (list
                    (html/hidden "action" "delete")
                    (html/hidden "slug" (as-str slug))
                    (html/button
                      (html/attrs
                        (list
                          (html/attr "type" "submit")
                          (html/attr "class" "secondary")
                          (html/attr "title" "delete note")))
                      "Delete note")))))))
        (html/p*
          (html/attr "class" "muted")
          (string-append "tags: " (as-str tags)))
        (outline/html slug outline)
        (html/tag
          "details"
          (html/attr "class" "ol-debug")
          (html/join
            (list
              (html/tag
                "summary"
                (html/attr "class" "muted")
                "Raw JSON (debug)")
              (notes/edit-form slug outline))))
        (html/tag
          "script"
          ""
          (string-append
            "document.getElementById('outline').addEventListener('blur',function(e){"
            "var t=e.target;if(!t.dataset||!t.dataset.field)return;"
            "var p='slug="
            slug
            "&op=set-'+t.dataset.field+'&node-id='+t.dataset.id;"
            "p+='&'+t.dataset.field+'='+encodeURIComponent(t.innerText);"
            "fetch('/partials/notes-node-op',{method:'POST',headers:{'Content-Type':'application/x-www-form-urlencoded'},body:p});"
            "},true);"))))))

(define notes/show-html
  (lambda
    (slug)
    (let
      ((n (notes/get slug)))
      (if
        (null? n)
        (notes/missing-html slug)
        (notes/show-body
          slug
          (notes/get-title n)
          (notes/get-outline n)
          (notes/get-tags n))))))

(define notes/tree-css
  (css/sheet
    (list
      (css/raw
        "/* AS/400 terminal-inspired theme overrides v3 */")
      (css/rule
        "body"
        (list
          "font-family"
          "\"Courier New\",\"Lucida Console\",Monaco,\"DejaVu Sans Mono\",monospace"
          "font-weight"
          "400"
          "-webkit-font-smoothing"
          "antialiased"))
      (css/rule
        "header"
        (list
          "border-bottom"
          "2px solid #33cc33"
          "padding"
          ".5rem .75rem"))
      (css/rule
        "header .brand"
        (list
          "color"
          "#33ff33"
          "text-transform"
          "uppercase"
          "letter-spacing"
          ".12em"
          "font-weight"
          "700"))
      (css/rule "header .meta" (list "color" "#888888"))
      (css/rule
        "nav"
        (list
          "gap"
          "0"
          "border"
          "2px solid #33cc33"
          "padding"
          ".25rem"))
      (css/rule
        "nav a,.menu-drop-btn"
        (list
          "border"
          "1px solid #33cc33"
          "background"
          "#1a2a1a"
          "color"
          "#33cc33"
          "font-family"
          "\"Courier New\",\"Lucida Console\",Monaco,monospace"
          "font-size"
          ".85rem"
          "font-weight"
          "700"
          "text-transform"
          "uppercase"
          "letter-spacing"
          ".06em"
          "padding"
          ".4rem .6rem"
          "margin"
          "0"
          "border-radius"
          "0"))
      (css/rule
        "nav a:hover,nav a:focus,.menu-drop-btn:hover"
        (list "background" "#33cc33" "color" "#111111"))
      (css/rule "main" (list "max-width" "56rem"))
      (css/rule
        "h1"
        (list
          "color"
          "#33ff33"
          "text-transform"
          "uppercase"
          "letter-spacing"
          ".1em"
          "border-bottom"
          "2px solid #33cc33"
          "font-size"
          "1.15rem"
          "font-weight"
          "700"))
      (css/rule
        "h2"
        (list
          "color"
          "#33cc33"
          "text-transform"
          "uppercase"
          "letter-spacing"
          ".05em"
          "font-size"
          "1rem"
          "font-weight"
          "700"))
      (css/rule
        "h3"
        (list
          "color"
          "#33cc33"
          "font-size"
          ".95rem"
          "font-weight"
          "700"))
      (css/rule
        ".card"
        (list
          "border"
          "2px solid #33cc33"
          "background"
          "#1a1a1a"))
      (css/rule
        "table"
        (list "border" "2px solid #33cc33" "font-size" ".85rem"))
      (css/rule
        "th"
        (list
          "background"
          "#1a2a1a"
          "color"
          "#33ff33"
          "text-transform"
          "uppercase"
          "letter-spacing"
          ".05em"
          "border-color"
          "#33cc33"
          "font-weight"
          "700"))
      (css/rule "td" (list "border-color" "#222222"))
      (css/rule
        "tr:nth-child(even) td"
        (list "background" "#151515"))
      (css/rule
        "button,.btn,input[type=submit]"
        (list
          "background"
          "#1a2a1a"
          "color"
          "#33cc33"
          "border"
          "2px solid #33cc33"
          "font-family"
          "\"Courier New\",\"Lucida Console\",Monaco,monospace"
          "font-size"
          ".85rem"
          "font-weight"
          "700"
          "text-transform"
          "uppercase"
          "letter-spacing"
          ".05em"
          "border-radius"
          "0"
          "padding"
          ".45rem .85rem"))
      (css/rule
        "button:hover,.btn:hover"
        (list "background" "#33cc33" "color" "#111111"))
      (css/rule
        "button:active,.btn:active"
        (list "border-style" "solid"))
      (css/rule
        "button.secondary,.btn.secondary"
        (list "background" "#1a1a1a"))
      (css/rule
        "pre,textarea,input[type=text],input[type=search],input:not([type])"
        (list
          "background"
          "#0d0d0d"
          "color"
          "#33cc33"
          "border"
          "1px solid #33cc33"
          "font-family"
          "\"Courier New\",\"Lucida Console\",Monaco,monospace"
          "font-weight"
          "400"))
      (css/rule
        "pre:focus,textarea:focus,input[type=text]:focus,input[type=search]:focus,input:not([type]):focus"
        (list "border-color" "#33ff33" "outline" "none"))
      (css/rule "a" (list "color" "#33ff33"))
      (css/rule "a:visited" (list "color" "#2a9a2a"))
      (css/rule "a:hover" (list "color" "#66ff66"))
      (css/rule ".meta,.muted" (list "color" "#888888"))
      (css/rule ".err" (list "color" "#ff4444"))
      (css/rule ".ok" (list "color" "#33ff33"))
      (css/rule
        ".notes-tree"
        (list
          "list-style"
          "none"
          "padding"
          "0"
          "margin"
          "0"))
      (css/rule
        ".notes-tree ul.tree-root"
        (list
          "list-style"
          "none"
          "padding"
          "0"
          "margin"
          "0"))
      (css/rule
        ".notes-tree .tree-folder"
        (list
          "margin"
          ".25rem 0"
          "padding-left"
          ".5rem"
          "border-left"
          "2px solid #33cc33"))
      (css/rule
        ".notes-tree .tree-folder-header"
        (list
          "display"
          "flex"
          "align-items"
          "center"
          "gap"
          ".35rem"
          "padding"
          ".15rem 0"
          "font-weight"
          "700"))
      (css/rule
        ".notes-tree .tree-folder-header a"
        (list "text-decoration" "none" "color" "#33ff33"))
      (css/rule
        ".notes-tree .tree-folder-header a:hover"
        (list "text-decoration" "underline" "color" "#66ff66"))
      (css/rule
        ".notes-tree .tree-folder-icon"
        (list "font-size" ".9em"))
      (css/rule
        ".notes-tree ul.tree-children"
        (list
          "list-style"
          "none"
          "padding"
          "0 0 0 1rem"
          "margin"
          ".15rem 0"))
      (css/rule
        ".notes-tree li.note-item"
        (list "border-bottom" "none" "padding" ".15rem 0"))
      (css/rule
        ".notes-tree li.note-item > a"
        (list "flex" "1 1 auto" "color" "#33cc33"))
      (css/rule
        ".notes-tree li.note-item > a:hover"
        (list "color" "#66ff66"))
      (css/rule
        ".notes-tree .tree-uncategorized"
        (list
          "margin-top"
          "1rem"
          "padding-top"
          ".5rem"
          "border-top"
          "1px solid #33cc33"))
      (css/rule
        ".notes-tree .tree-uncategorized h3"
        (list
          "font-size"
          ".95rem"
          "color"
          "#888888"
          "margin"
          ".5rem 0 .25rem"
          "text-transform"
          "uppercase"
          "letter-spacing"
          ".05em"))
      (css/rule
        ".notes-tree li.note-item form.note-del button"
        (list
          "border"
          "1px solid #33cc33"
          "background"
          "#1a1a1a"
          "color"
          "#888888"
          "padding"
          ".2rem .4rem"
          "font-size"
          ".8rem"
          "min-height"
          "auto"
          "width"
          "auto"))
      (css/rule
        ".notes-tree li.note-item form.note-del button:hover"
        (list "background" "#33cc33" "color" "#111111"))
      (css/rule
        "li.note-item"
        (list
          "border-bottom"
          "1px solid #222222"
          "padding"
          ".35rem 0"))
      (css/rule "li.note-item > a" (list "color" "#33cc33"))
      (css/rule
        "li.note-item > a:hover"
        (list "color" "#66ff66"))
      (css/rule
        "li.note-item .muted"
        (list "color" "#888888"))
      (css/rule
        "li.note-item .note-del button"
        (list
          "border"
          "1px solid #33cc33"
          "background"
          "#1a1a1a"
          "color"
          "#888888"
          "padding"
          ".2rem .4rem"
          "font-size"
          ".8rem"
          "min-height"
          "auto"
          "width"
          "auto"))
      (css/rule
        "li.note-item .note-del button:hover"
        (list "background" "#33cc33" "color" "#111111")))))

(define notes/tree-css-patch
  (css/sheet
    (list
      (css/raw
        "/* v4 patch: fix capture btn wrap + brighten muted greys */")
      (css/rule
        ".menu-drop-btn"
        (list
          "white-space"
          "nowrap"
          "display"
          "inline-flex"
          "align-items"
          "center"
          "justify-content"
          "center"
          "gap"
          ".25rem"))
      (css/rule
        ".menu-drop>summary::after"
        (list "display" "inline-block"))
      (css/rule "header .meta" (list "color" "#aaaaaa"))
      (css/rule ".meta,.muted" (list "color" "#aaaaaa"))
      (css/rule ".notes-tree .muted" (list "color" "#aaaaaa"))
      (css/rule
        "li.note-item .muted"
        (list "color" "#aaaaaa"))
      (css/rule
        ".notes-tree .tree-uncategorized h3"
        (list "color" "#aaaaaa"))
      (css/rule
        "li.note-item .note-del button, .notes-tree li.note-item form.note-del button"
        (list
          "color"
          "#33cc33"
          "border"
          "1px solid #33cc33"
          "background"
          "#1a1a1a"))
      (css/rule
        "li.note-item .note-del button:hover, .notes-tree li.note-item form.note-del button:hover"
        (list "background" "#33cc33" "color" "#111111")))))

(define notes/tree/all-paths
  (lambda
    (names)
    (list/foldl
      (lambda
        (acc name)
        (if
          (string-eq name "")
          acc
          (let
            ((parent (notes/tree/parent-of name)))
            (if
              (string-eq parent "")
              acc
              (if
                (notes/doc/folder-seen? acc parent)
                acc
                (if
                  (notes/doc/folder-seen? names parent)
                  acc
                  (append acc (list parent))))))))
      (list)
      names)))

(define notes/tree/children-of
  (lambda
    (parent names)
    (list/filter
      (lambda
        (name)
        (if
          (string-eq name "")
          #f
          (let
            ((p (notes/tree/parent-of name)))
            (string-eq p parent))))
      names)))

(define notes/tree/details-css
  (lambda
    ()
    (css/sheet
      (list
        (css/raw "/* details/summary tree nesting */")
        (css/rule
          "summary.tree-folder-header"
          (list "cursor" "pointer" "list-style" "none"))
        (css/rule
          "summary.tree-folder-header::-webkit-details-marker"
          (list "display" "none"))
        (css/rule
          "summary.tree-folder-header::marker"
          (list "content" ""))
        (css/rule
          ".notes-tree ul.tree-items"
          (list
            "list-style"
            "none"
            "padding"
            "0 0 0 1rem"
            "margin"
            ".15rem 0"))
        (css/rule
          ".notes-tree > details.tree-folder"
          (list "margin" ".25rem 0"))))))

(define notes/tree/folder-label
  (lambda
    (path)
    (let
      ((pos (string-find-pos path "/" 0 1)))
      (if
        (< pos 0)
        path
        (substring
          path
          (+ pos 1)
          (- (string-length path) (+ pos 1)))))))

(define notes/tree/parent-of
  (lambda
    (path)
    (let
      ((pos (string-find-pos path "/" 0 1)))
      (if (< pos 0) "" (substring path 0 pos)))))

(define notes/tree/render
  (lambda
    ()
    (let*
      ((names (notes/doc/folder-names))
        (all-names (append names (notes/tree/all-paths names)))
        (roots
          (notes/tree/sort-by-name
            (notes/tree/children-of "" all-names)))
        (uncat (notes/doc/list-folder "")))
      (html/div
        (html/attr "class" "notes-tree")
        (html/join
          (list
            (if
              (null? uncat)
              ""
              (html/tag
                "details"
                (html/attrs
                  (list
                    (html/attr "class" "tree-folder tree-uncategorized")
                    (html/attr "open" "open")))
                (html/join
                  (list
                    (html/tag
                      "summary"
                      (html/attr "class" "tree-folder-header")
                      (html/join
                        (list
                          (html/span
                            (html/attr "class" "tree-folder-icon")
                            "&#x1F4C2; ")
                          (html/text "(uncategorized)")
                          (html/span
                            (html/attr "class" "muted")
                            (html/text
                              (string-append " (" (as-str (length uncat)) ")"))))))
                    (html/ul
                      (html/attr "class" "tree-items")
                      (notes/ui/folder-list ""))))))
            (html/join
              (list/map
                (lambda
                  (root)
                  (notes/tree/render-folder root all-names 0))
                roots))))))))

(define notes/tree/render-folder
  (lambda
    (path names depth)
    (let*
      ((label (notes/tree/folder-label path))
        (children (notes/tree/children-of path names))
        (items (notes/doc/list-folder path))
        (item-count (length items)))
      (html/tag
        "details"
        (html/attrs
          (list
            (html/attr "class" "tree-folder")
            (html/attr "open" "open")))
        (html/join
          (list
            (html/tag
              "summary"
              (html/attr "class" "tree-folder-header")
              (html/join
                (list
                  (html/span
                    (html/attr "class" "tree-folder-icon")
                    "&#x1F4C1; ")
                  (html/a
                    (string-append "/notes?folder=" path)
                    (html/text label))
                  (html/span
                    (html/attr "class" "muted")
                    (html/text (string-append " (" (as-str item-count) ")"))))))
            (if
              (null? items)
              ""
              (html/ul
                (html/attr "class" "tree-items")
                (notes/ui/folder-list path)))
            (if
              (null? children)
              ""
              (html/join
                (list/map
                  (lambda
                    (child)
                    (notes/tree/render-folder child names (+ depth 1)))
                  (notes/tree/sort-by-name children))))))))))

(define notes/tree/sort-by-name
  (lambda
    (names)
    (if
      (null? names)
      (list)
      (let
        ((pivot (car names)) (rest (cdr names)))
        (append
          (notes/tree/sort-by-name
            (list/filter (lambda (x) (string<? x pivot)) rest))
          (list pivot)
          (notes/tree/sort-by-name
            (list/filter (lambda (x) (not (string<? x pivot))) rest)))))))

(define notes/tree/style
  (lambda
    ()
    (html/tag
      "style"
      ""
      (string-append
        (css/render notes/tree-css)
        (css/render notes/tree-css-patch)
        (css/render (notes/tree/details-css))))))

(define notes/ui/create-form
  (lambda
    ()
    (html/form
      "POST"
      "/notes"
      (html/attr "class" "card")
      (html/join
        (list
          (html/hidden "action" "create")
          (html/label "" "Title ")
          (html/text-input
            "title"
            (html/attr "placeholder" "New note"))
          " "
          (html/label "" "Slug ")
          (html/text-input
            "slug"
            (html/attr "placeholder" "my-note"))
          " "
          (html/label "" "Folder ")
          (html/text-input
            "folder"
            (html/attr "placeholder" "wiki/entities"))
          " "
          (html/button (html/attr "type" "submit") "Create"))))))

(define notes/ui/do-create
  (lambda
    (title slug folder)
    (let
      ((s
          (if
            (if
              (null? slug)
              #t
              (string-eq (string-trim (as-str slug)) ""))
            (string-append
              "note-"
              (as-str (count (notes/doc/list))))
            (string-trim (as-str slug))))
        (fld
          (if (null? folder) "" (string-trim (as-str folder)))))
      (seq
        (notes/doc/upsert-in
          s
          (as-str title)
          (notes/doc/empty-outline (as-str title))
          ""
          "note"
          ""
          fld)
        (redirect (string-append "/notes?slug=" s))))))

(define notes/ui/do-delete
  (lambda
    ()
    (let
      ((slug (as-str (request-param "slug"))))
      (seq (notes/doc/delete slug) (redirect "/notes")))))

(define notes/ui/do-save
  (lambda
    (slug raw)
    (let
      ((slug-s (as-str slug))
        (prev (notes/doc/get (as-str slug))))
      (let
        ((title (if (null? prev) slug-s (notes/doc/title prev)))
          (tags (if (null? prev) "" (notes/doc/tags prev)))
          (kind (if (null? prev) "note" (notes/doc/kind prev)))
          (day (if (null? prev) "" (notes/doc/day prev))))
        (if
          (or
            (null? raw)
            (string-eq (string-trim (as-str raw)) ""))
          (seq
            (notes/doc/upsert
              slug-s
              title
              (outline/node "root" "Untitled")
              tags
              kind
              day)
            (redirect (string-append "/notes?slug=" slug-s)))
          (let
            ((parsed (json-parse (as-str raw))))
            (if
              (outline/valid? parsed)
              (seq
                (notes/doc/upsert
                  slug-s
                  title
                  parsed
                  tags
                  kind
                  day)
                (redirect (string-append "/notes?slug=" slug-s)))
              (html/page
                (list
                  (html/h1 (string-append "Could not save " slug-s))
                  (html/p*
                    (html/attr "class" "err")
                    "The outline is malformed. Each node must be a 5-element list [id title body todo kids]. The note was not changed.")
                  (html/p*
                    ""
                    (html/a
                      (string-append "/notes?slug=" slug-s)
                      "Back to the note"))
                  (html/tag
                    "pre"
                    (html/attrs (list (html/attr "class" "err")))
                    (as-str raw)))))))))))

(define notes/ui/edit-form
  (lambda
    (slug outline)
    (html/form
      "POST"
      "/notes"
      ""
      (html/join
        (list
          (html/hidden "action" "save")
          (html/hidden "slug" slug)
          (html/tag
            "textarea"
            (html/attrs
              (list
                (html/attr "name" "outline")
                (html/attr "rows" "16")))
            (outline/encode outline))
          (html/p*
            ""
            (html/button (html/attr "type" "submit") "Save outline")))))))

(define notes/ui/folder-form
  (lambda
    (slug folder)
    (html/form
      "POST"
      "/notes"
      (html/attr "class" "notes-folder-form")
      (html/join
        (list
          (html/hidden "action" "set-folder")
          (html/hidden "slug" (as-str slug))
          (html/label "" "folder ")
          (html/text-input
            "folder"
            (html/attrs
              (list
                (html/attr "value" (as-str folder))
                (html/attr "placeholder" "wiki/entities")
                (html/attr "style" "width:12rem"))))
          " "
          (html/button (html/attr "type" "submit") "move"))))))

(define notes/ui/folder-list
  (lambda
    (folder)
    (list/foldl
      (lambda
        (acc row)
        (string-append acc (notes/ui/item-li row)))
      ""
      (notes/doc/list-folder folder))))

(define notes/ui/folder-section
  (lambda
    (folder)
    (let
      ((label
          (if
            (string-eq (as-str folder) "")
            "(uncategorized)"
            (as-str folder)))
        (href
          (if
            (string-eq (as-str folder) "")
            "/notes?folder="
            (string-append "/notes?folder=" (as-str folder)))))
      (html/div
        (html/attr "class" "notes-folder card")
        (html/join
          (list
            (html/h3 (html/a href (html/text label)))
            (html/ul "" (notes/ui/folder-list folder))))))))

(define notes/ui/folders-html
  (lambda
    ()
    (let
      ((filter (request-param "folder")))
      (if
        (if
          (null? filter)
          #f
          (not (string-eq (as-str filter) "")))
        (notes/ui/folder-section (as-str filter))
        (let
          ((names (notes/doc/folder-names)))
          (if
            (null? names)
            (html/p* (html/attr "class" "muted") "No notes yet.")
            (notes/tree/render)))))))

(define notes/ui/handle-post
  (lambda
    ()
    (let
      ((action (as-str (request-param "action"))))
      (cond
        ((string-eq action "create")
          (notes/ui/do-create
            (request-param "title")
            (request-param "slug")
            (request-param "folder")))
        ((string-eq action "set-folder")
          (seq
            (notes/doc/set-folder
              (request-param "slug")
              (if
                (null? (request-param "folder"))
                ""
                (request-param "folder")))
            (redirect
              (string-append
                "/notes?slug="
                (as-str (request-param "slug"))))))
        ((string-eq action "save")
          (notes/ui/do-save
            (request-param "slug")
            (request-param "outline")))
        ((string-eq action "capture") (notes/capture/handle))
        ((string-eq action "delete") (notes/ui/do-delete))
        ((string-eq action "refile") (notes/refile/handle))
        ((string-eq action "extract")
          (notes/refile/extract-handle))
        (else (redirect "/notes"))))))

(define notes/ui/index
  (lambda
    ()
    (html/join
      (list
        (notes/tree/style)
        (html/h1 "Notes")
        (html/p*
          (html/attr "class" "muted")
          "Outline-first notes. Grouped by folder (optional path like wiki/entities).")
        (html/p*
          (html/attr "class" "muted")
          (html/join
            (list
              (html/a "/notes?slug=inbox" "Open inbox")
              " \194\183 "
              (html/a "/notes" "All folders")
              " \194\183 system notes (inbox) are hidden from the index.")))
        (notes/ui/create-form)
        (html/h2 "Folders")
        (notes/ui/folders-html)))))

(define notes/ui/item-li
  (lambda
    (row)
    (let
      ((f (cdr row)) (id (car row)))
      (let
        ((slug (as-str (dict-get f "slug")))
          (title (as-str (dict-get f "title")))
          (folder (notes/doc/row-folder row)))
        (html/li
          (html/attr "class" "note-item")
          (html/join
            (list
              (html/a
                (string-append "/notes?slug=" slug)
                (html/text title))
              (if
                (string-eq folder "")
                ""
                (html/span
                  (html/attr "class" "muted")
                  (html/text (string-append " \194\183 " folder))))
              (html/form
                "POST"
                "/notes"
                (html/attrs
                  (list
                    (html/attr "class" "note-del")
                    (html/attr "hx-confirm" "Delete this note?")))
                (html/join
                  (list
                    (html/hidden "action" "delete")
                    (html/hidden "slug" slug)
                    (html/button
                      (html/attrs
                        (list
                          (html/attr "type" "submit")
                          (html/attr "class" "secondary")
                          (html/attr "title" "delete note")))
                      "del")))))))))))

(define notes/ui/missing
  (lambda
    (slug)
    (html/join
      (list
        (html/h1 "Missing")
        (html/p*
          (html/attr "class" "err")
          (string-append "No note " (as-str slug)))
        (html/p* "" (html/a "/notes" "All notes"))))))

(define notes/ui/outline-script
  (lambda
    (slug show-archived)
    (html/tag
      "script"
      ""
      (string-append
        "(function(){var slug='"
        (as-str slug)
        "';var showArch="
        (if show-archived "true" "false")
        ";function selId(){var el=document.getElementById('ol-selected-id');return el?el.value:'root';}"
        "function findNode(id){var all=document.querySelectorAll('.ol-node');for(var i=0;i<all.length;i++){if(all[i].getAttribute('data-id')===id)return all[i];}return null;}"
        "function ownTitle(node){if(!node||!node.children)return null;for(var i=0;i<node.children.length;i++){var ch=node.children[i];if(ch.classList&&ch.classList.contains('ol-sum')){return ch.querySelector('.ol-title');}}return null;}"
        "var __selPin=null,__selPinT=null;"
        "function setSel(id){"
        "if(!id)return;"
        "if(__selPin&&id==='root'&&__selPin!=='root')return;"
        "var node=findNode(id);if(!node)return;"
        "var hid=document.getElementById('ol-selected-id');if(hid)hid.value=id;"
        "var lab=document.getElementById('ol-selected-label');"
        "if(lab){var t=ownTitle(node);if(!t)t=node.querySelector('.ol-title');"
        "lab.textContent=(t&&t.innerText)?t.innerText.trim():id;lab.title=id;}"
        "var rf=document.getElementById('ol-refile-nid');if(rf)rf.value=id;"
        "var ex=document.getElementById('ol-extract-nid');if(ex)ex.value=id;"
        "document.querySelectorAll('.ol-node.is-selected').forEach(function(n){n.classList.remove('is-selected');});"
        "node.classList.add('is-selected');"
        "if(id!=='root'){__selPin=id;if(__selPinT)clearTimeout(__selPinT);__selPinT=setTimeout(function(){__selPin=null;},400);}"
        "else{__selPin='root';}"
        "}"
        "function postOp(op){var id=selId();"
        "if(op==='delete'){if(id==='root'){alert('Cannot delete the root node.');return;}if(!confirm('Delete this node?'))return;}"
        "if(op==='archive'&&id==='root'){alert('Cannot archive the root node.');return;}"
        "var q='slug='+encodeURIComponent(slug)+'&op='+encodeURIComponent(op)+'&node-id='+encodeURIComponent(id);"
        "if(showArch)q+='&show=archived';"
        "var keep=id;__selPin=keep;"
        "fetch('/partials/notes-node-op?'+q,{method:'POST',credentials:'same-origin'}).then(function(r){return r.text();}).then(function(html){"
        "var doc=new DOMParser().parseFromString(html,'text/html');"
        "var oobSel=doc.querySelector('#ol-selected-id[hx-swap-oob]');"
        "var olHTML=doc.querySelector('#outline');"
        "if(olHTML){var t=document.getElementById('outline');if(t)t.outerHTML=olHTML.outerHTML;}"
        "var selIdVal=oobSel?oobSel.getAttribute('value'):null;"
        "if(selIdVal){var hid=document.getElementById('ol-selected-id');if(hid)hid.value=selIdVal;}"
        "var target=selIdVal||keep;"
        "if(findNode(target))setSel(target);else if(findNode(keep))setSel(keep);else{__selPin=null;setSel('root');}"
        "if(window.htmx){try{htmx.process(document.getElementById('outline'));}catch(e){}}"
        "}).catch(function(e){console.error('notes postOp',e);});}"
        "function pickNode(e){"
        "if(e.target.closest&&e.target.closest('[data-ol-op]'))return null;"
        "if(e.target.closest&&e.target.closest('.ol-actions'))return null;"
        "if(e.target.closest&&e.target.closest('.ol-fold'))return null;"
        "var node=e.target.closest&&e.target.closest('.ol-node');"
        "return(node&&node.dataset&&node.dataset.id)?node:null;}"
        "document.addEventListener('pointerdown',function(e){"
        "if(e.button!==0&&e.button!==undefined)return;"
        "var node=pickNode(e);if(node)setSel(node.dataset.id);},true);"
        "document.addEventListener('click',function(e){"
        "var btn=e.target.closest&&e.target.closest('[data-ol-op]');"
        "if(btn){e.preventDefault();postOp(btn.getAttribute('data-ol-op'));return;}"
        "var node=pickNode(e);if(node)setSel(node.dataset.id);},true);"
        "document.addEventListener('focusin',function(e){"
        "var t=e.target;if(!t||!t.getAttribute)return;"
        "var id=t.getAttribute('data-id');var field=t.getAttribute('data-field');"
        "if(id&&(field==='title'||field==='body'))setSel(id);},true);"
        "var root=document.getElementById('outline');if(root){root.addEventListener('blur',function(e){"
        "var t=e.target;if(!t.dataset||!t.dataset.field)return;"
        "var p='slug='+encodeURIComponent(slug)+'&op=set-'+t.dataset.field+'&node-id='+encodeURIComponent(t.dataset.id);"
        "if(showArch)p+='&show=archived';"
        "p+='&'+t.dataset.field+'='+encodeURIComponent(t.innerText);"
        "fetch('/partials/notes-node-op',{method:'POST',headers:{'Content-Type':'application/x-www-form-urlencoded'},body:p});},true);}"
        "document.addEventListener('keydown',function(e){"
        "if(!e.altKey)return;"
        "var k=e.key;var op=null;"
        "if(e.shiftKey&&k==='ArrowLeft')op='promote';"
        "else if(e.shiftKey&&k==='ArrowRight')op='demote';"
        "else if(k==='ArrowUp')op='move-up';"
        "else if(k==='ArrowDown')op='move-down';"
        "else if(k==='Enter')op='add-sibling';"
        "else if(k==='ArrowRight')op='slurp-forward';"
        "else if(k==='ArrowLeft')op='barf-forward';"
        "if(op){e.preventDefault();postOp(op);}"
        "},true);"
        "setSel('root');document.body.addEventListener('htmx:afterSwap',function(){setSel(selId());});})();"))))

(define notes/ui/recent-list
  (lambda
    ()
    (list/foldl
      (lambda
        (acc row)
        (string-append acc (notes/ui/item-li row)))
      ""
      (notes/doc/index-list))))

(define notes/ui/show
  (lambda
    (slug)
    (let
      ((n (notes/doc/get slug))
        (show-archived (notes/doc/show-archived?)))
      (if
        (null? n)
        (notes/ui/missing slug)
        (notes/ui/show-body
          slug
          (notes/doc/title n)
          (notes/doc/outline n)
          (notes/doc/tags n)
          show-archived
          (notes/doc/folder n))))))

(define notes/ui/show-body
  (lambda
    (slug title outline tags show-archived folder)
    (html/join
      (list
        (html/p*
          (html/attr "class" "muted")
          (html/join
            (list
              (html/a "/notes" "Notes")
              (if
                (string-eq (as-str folder) "")
                ""
                (html/join
                  (list
                    " / "
                    (html/a
                      (string-append "/notes?folder=" (as-str folder))
                      (html/text (as-str folder))))))
              " / "
              (html/code (as-str slug)))))
        (html/div
          (html/attr "class" "note-title-row")
          (html/join
            (list
              (html/h1 (html/text title))
              (html/form
                "POST"
                "/notes"
                (html/attrs
                  (list
                    (html/attr "class" "note-del note-del-page")
                    (html/attr "hx-confirm" "Delete this entire note?")))
                (html/join
                  (list
                    (html/hidden "action" "delete")
                    (html/hidden "slug" (as-str slug))
                    (html/button
                      (html/attrs
                        (list
                          (html/attr "type" "submit")
                          (html/attr "class" "secondary")
                          (html/attr "title" "delete note")))
                      "Delete note")))))))
        (html/p*
          (html/attr "class" "muted")
          (string-append "tags: " (as-str tags)))
        (notes/ui/folder-form slug folder)
        (notes/ui/toolbar slug show-archived)
        (outline/html slug outline show-archived)
        (html/div
          (html/attr "class" "roam-backlinks-panel")
          (html/join
            (list (html/h3 "Backlinks") (roam/backlinks-html slug))))
        (html/tag
          "details"
          (html/attr "class" "ol-debug")
          (html/join
            (list
              (html/tag
                "summary"
                (html/attr "class" "muted")
                "Raw JSON (debug)")
              (notes/ui/edit-form slug outline))))
        (notes/ui/outline-script slug show-archived)))))

(define notes/ui/tb-btn
  (lambda
    (op label)
    (html/tag
      "button"
      (html/attrs
        (list
          (html/attr "type" "button")
          (html/attr "class" "ol-btn")
          (html/attr "title" op)
          (html/attr "data-ol-op" op)))
      label)))

(define notes/ui/toolbar
  (lambda
    (slug show-archived)
    (html/div
      (html/attr "class" "ol-toolbar")
      (html/join
        (list
          (html/span
            (html/attr "class" "ol-toolbar-label")
            "Selected:")
          (html/span (html/attr "id" "ol-selected-label") "root")
          (html/void
            "input"
            (html/attrs
              (list
                (html/attr "type" "hidden")
                (html/attr "id" "ol-selected-id")
                (html/attr "value" "root"))))
          (notes/ui/tb-btn "cycle-todo" "todo")
          (notes/ui/tb-btn "archive" "arch")
          (notes/ui/tb-btn "unarchive" "unarch")
          (notes/ui/tb-btn "add-child" "+kid")
          (notes/ui/tb-btn "add-sibling" "+sib")
          (notes/ui/tb-btn "promote" "<<")
          (notes/ui/tb-btn "demote" ">>")
          (notes/ui/tb-btn "move-up" "up")
          (notes/ui/tb-btn "move-down" "dn")
          (notes/ui/tb-btn "delete" "del")
          (html/form
            "POST"
            "/notes"
            (html/attr "class" "ol-refile")
            (html/join
              (list
                (html/hidden "action" "refile")
                (html/hidden "src_slug" (as-str slug))
                (html/void
                  "input"
                  (html/attrs
                    (list
                      (html/attr "type" "hidden")
                      (html/attr "name" "node_id")
                      (html/attr "id" "ol-refile-nid")
                      (html/attr "value" "root"))))
                (html/select
                  "dst_slug"
                  (html/attr "class" "ol-refile-sel")
                  (notes/refile/options slug))
                (html/button
                  (html/attrs
                    (list
                      (html/attr "type" "submit")
                      (html/attr "class" "ol-btn")
                      (html/attr "title" "refile selected node")))
                  "refile"))))
          (html/form
            "POST"
            "/notes"
            (html/attr "class" "ol-extract")
            (html/join
              (list
                (html/hidden "action" "extract")
                (html/hidden "src_slug" (as-str slug))
                (html/void
                  "input"
                  (html/attrs
                    (list
                      (html/attr "type" "hidden")
                      (html/attr "name" "node_id")
                      (html/attr "id" "ol-extract-nid")
                      (html/attr "value" "root"))))
                (html/button
                  (html/attrs
                    (list
                      (html/attr "type" "submit")
                      (html/attr "class" "ol-btn")
                      (html/attr
                        "title"
                        "extract selected node into its own note")))
                  "extract"))))
          (if
            show-archived
            (html/a
              (string-append "/notes?slug=" slug)
              "Hide archived")
            (html/a
              (string-append "/notes?slug=" slug "&show=archived")
              "Show archived")))))))

(define notes/upsert
  (lambda
    (slug title outline tags kind day)
    (let
      ((row (notes/row slug)))
      (let
        ((fields
          (notes/make-fields
            slug
            title
            outline
            tags
            kind
            day)))
        (if
          (null? row)
          (create (quote note) fields)
          (update (quote note) (car row) fields))))))

