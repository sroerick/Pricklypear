;; package notes -- exported from live branch admin/personal
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
      (notes/tree/full-notes)))
  "Collect todo rows (slug title nid done?) across every note.\n   log/issues fix (note-index omits outline): folds the FULL index rows\n   (tree/note-index-full carries the real outline); the plain index rows\n   hardcode outline [] which silently killed this scrape.")

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
                      tree))))))))))
  "Apply a single op to an outline node by id, returning the new tree.")

(define notes/browse
  (lambda
    (path)
    (filmstrip/browse-walk
      notes/browse-headings
      (list (notes/browse-docs))
      path))
  "notes cascade: docs -> outline headings. Heading entries leaf to\n/notes?slug=<doc> (headings have no route of their own).")

(define notes/browse-docs
  (lambda
    ()
    (list/map
      (lambda (r) (list (nth r 2) (nth r 3)))
      (notes/list-light)))
  "Column of docs: (title . slug).")

(define notes/browse-headings
  (lambda
    (slug)
    (let
      ((n (notes/get slug)))
      (if
        (null? n)
        (list)
        (list/map
          (lambda
            (kid)
            (list
              (as-str (outline/body kid))
              (string-append "/notes?slug=" slug)))
          (outline/kids (notes/get-outline n))))))
  "Top-level outline headings of one doc, each a leaf to the doc view.")

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
              "inbox"))))))
  "Capture a new node into the Inbox note; returns the inbox slug.")

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
                (html/attr "placeholder" "Quick capture226128166")
                (html/attr "required" "required")))
            "")
          (html/div
            (html/attr "class" "capture-row")
            (html/join
              (list
                (html/button (html/attr "type" "submit") "Capture")
                (html/a "/notes?slug=inbox" "Open inbox"))))
          (html/div (html/attr "id" "capture-status") "")))))
  "HTML capture form posting a new node into the Inbox.")

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
                        (html/attr "placeholder" "Quick capture226128166")
                        (html/attr "required" "required")))
                    "")
                  (html/div
                    (html/attr "class" "capture-row")
                    (html/join
                      (list
                        (html/button (html/attr "type" "submit") "Save")
                        (html/a "/notes?slug=inbox" "inbox"))))
                  (html/div (html/attr "id" "capture-status") "")))))))))
  "Collapsible capture form for the nav bar.")

(define notes/capture-node-id
  (lambda
    (tree)
    (string-append
      "c"
      (as-str (count (outline/kids tree)))
      "-"
      (as-str (string-length (outline/encode tree)))))
  "Derive a fresh node id from tree size and encoded length.")

(define notes/capture-page
  (lambda
    ()
    (if
      (string-eq (request-method) "POST")
      (notes/do-capture)
      (html/page
        (list (html/h1 "Capture") (notes/capture-form)))))
  "Serve the capture page: handle a POST, else render the form.")

(define notes/capture-status-html
  (lambda
    (ok msg)
    (html/p* (html/attr "class" (if ok "ok" "err")) msg))
  "HTML capture status paragraph, ok/err classed.")

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
              (string-append (substring b 0 45) "226128166")))))))
  "Page title for a capture: title, else the first 45 chars of body, else the default 'capture'.")

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
              "inbox"))))))
  "Capture a node into the Inbox note via the doc store; returns the inbox slug.")

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
                (html/attr "placeholder" "Quick capture226128166")
                (html/attr "required" "required")))
            "")
          (html/div
            (html/attr "class" "capture-row")
            (html/join
              (list
                (html/button (html/attr "type" "submit") "Capture")
                (html/a "/notes?slug=inbox" "Open inbox"))))
          (html/div (html/attr "id" "capture-status") "")))))
  "HTML capture form (doc variant) posting a new node into the Inbox.")

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
              (redirect "/notes?slug=inbox")))))))
  "Handle a capture POST: validate body, save, then redirect or render status.")

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
                        (html/attr "placeholder" "Quick capture226128166")
                        (html/attr "required" "required")))
                    "")
                  (html/div
                    (html/attr "class" "capture-row")
                    (html/join
                      (list
                        (html/button (html/attr "type" "submit") "Save")
                        (html/a "/notes?slug=inbox" "inbox"))))
                  (html/div (html/attr "id" "capture-status") "")))))))))
  "Collapsible capture form for the nav bar (doc variant).")

(define notes/capture/node-id
  (lambda
    (tree)
    (string-append
      "c"
      (as-str (count (outline/kids tree)))
      "-"
      (as-str (string-length (outline/encode tree)))))
  "Derive a fresh node id from tree size and encoded length (doc variant).")

(define notes/capture/outline-of-doc
  (lambda (note) (notes/doc/outline note))
  "Outline tree for a notes/doc row; when notes/doc/ensure-inbox has already patched in the loaded tree, return it without a second tree walk.")

(define notes/capture/page
  (lambda
    ()
    (if
      (string-eq (request-method) "POST")
      (notes/capture/handle)
      (html/page
        (list (html/h1 "Capture") (notes/capture/form)))))
  "Serve the capture page: handle a POST, else render the form (doc variant).")

(define notes/capture/status
  (lambda
    (ok msg)
    (html/p* (html/attr "class" (if ok "ok" "err")) msg))
  "HTML capture status paragraph, ok/err classed (doc variant).")

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
              (string-append (substring b 0 45) "...")))))))
  "Page title for a capture: title, else the first 45 chars of body, else the default 'capture'.")

(define notes/capture/upsert-inbox
  (lambda
    (tree)
    (seq
      (notes/doc/upsert-in
        "inbox"
        "Inbox"
        tree
        "inbox"
        "inbox"
        ""
        (notes/doc/row-folder (notes/doc/row "inbox")))
      "inbox"))
  "Upsert the inbox note from an already-decoded outline tree. Skips notes/doc/upsert's own outline reload.")

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
          (html/button (html/attr "type" "submit") "Create")))))
  "HTML form to create a new note by title and slug.")

(define notes/css
  (lambda
    ()
    (css/sheet
      (list
        (css/rule
          "img.note-media"
          (list
            "display"
            "block"
            "max-width"
            "100%"
            "max-height"
            "28rem"
            "height"
            "auto"
            "margin"
            ".5rem 0"
            "border"
            "1px solid #000"
            "background"
            "var(--panel)"))
        (css/rule
          ".note-media-cap"
          (list "margin" "0 0 .5rem" "font-size" ".85rem"))
        (css/rule
          "li.note-item"
          (list
            "display"
            "flex"
            "align-items"
            "center"
            "gap"
            ".5rem"
            "padding"
            ".35rem 0"
            "border-bottom"
            "1px solid #c0c0c0"))
        (css/rule "li.note-item > a" (list "flex" "1 1 auto"))
        (css/rule
          "li.note-item form.note-del"
          (list "margin" "0"))
        (css/rule
          "li.note-item .note-del button"
          (list
            "width"
            "auto"
            "padding"
            ".25rem .45rem"
            "font-size"
            ".85rem"))
        (css/rule
          ".note-title-row"
          (list
            "display"
            "flex"
            "flex-wrap"
            "wrap"
            "align-items"
            "center"
            "gap"
            ".75rem"))
        (css/rule
          ".note-title-row h1"
          (list
            "flex"
            "1 1 auto"
            "margin"
            "0"
            "border-bottom"
            "none"
            "padding"
            "0"))
        (css/rule
          ".note-title-row .note-del-page button"
          (list "width" "auto")))))
  "Render the notes component stylesheet (extracted from ui/theme).")

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
              day))))))
  "Cycle the todo state of a node in a note.")

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
        (notes/get slug))))
  "Get the daily note for a day, creating it if missing.")

(define notes/delete
  (lambda
    (slug)
    (let
      ((row (notes/row (as-str slug))))
      (if (null? row) () (delete (quote note) (car row)))))
  "Delete a note row by slug.")

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
              (redirect "/notes?slug=inbox")))))))
  "Handle a capture POST: validate, save, then redirect or render status.")

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
        (redirect (string-append "/notes?slug=" s)))))
  "Create a note with an auto or given slug, then redirect to it.")

(define notes/do-delete
  (lambda
    ()
    (let
      ((slug (as-str (request-param "slug"))))
      (seq (notes/delete slug) (redirect "/notes"))))
  "Delete the note named by the slug request param, then redirect.")

(define notes/do-refile
  (lambda
    ()
    (let
      ((src (as-str (request-param "src_slug")))
        (nid (as-str (request-param "node_id")))
        (dst (as-str (request-param "dst_slug"))))
      (seq
        (notes/refile src nid dst)
        (redirect (string-append "/notes?slug=" src)))))
  "Refile a node from the src to the dst note, then redirect to source.")

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
          (redirect (string-append "/notes?slug=" (as-str slug)))))))
  "Save an outline (from raw JSON) into a note, then redirect to it.")

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
        (notes/doc/get slug))))
  "Get the daily note for a day via the doc store, creating if missing.")

(define notes/doc/day
  (lambda (n) (car (cdr (cdr (cdr (cdr (cdr (cdr n))))))))
  "Extract the day field from a decoded note row.")

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
          (let
            ((p (notes/doc/row-path row)))
            (if
              (and (notes/tree/enabled?) (not (string-eq p "")))
              (tree/delete p "recursive")
              (delete (quote note) (car row))))
          slug))))
  "Delete a note (and its tree node) by slug.")

(define notes/doc/display-folder
  (lambda
    (folder)
    (let
      ((f (as-str folder)))
      (let
        ((stripped
          (if
            (string-starts-with? f "garden/")
            (string-replace f "garden/" "")
            (if (string-eq f "garden") "" f))))
        (string-replace stripped "_" "-")))))

(define notes/doc/empty-outline
  (lambda (title) (outline/node "root" title))
  "Build an empty root-only outline titled title.")

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
      (notes/doc/get "inbox")))
  "Get the Inbox note, creating it if missing.")

(define notes/doc/folder
  (lambda
    (n)
    (if
      (null? n)
      ""
      (let
        ((f (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr n))))))))))
        (if (null? f) "" (as-str f)))))
  "Extract the folder field from a decoded note row (empty when absent).")

(define notes/doc/folder-names
  (lambda () (tree/folder-index (notes/tree/root))))

(define notes/doc/folder-seen?
  (lambda
    (xs f)
    (if
      (null? xs)
      #f
      (if
        (string-eq (car xs) f)
        #t
        (notes/doc/folder-seen? (cdr xs) f))))
  "Whether a folder already appears in a list.")

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
              (if (null? folder) "" folder)))))))
  "Decode a note row by slug into (id slug title outline tags kind day folder).")

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
              (string-eq kind "issue")
              #t
              (if
                (string-eq slug "inbox")
                #t
                (string-contains? tags "hidden"))))))))
  "Whether a row should be hidden from the index (inbox/hidden/tagged).")

(define notes/doc/index-list
  (lambda
    ()
    (list/filter
      (lambda (row) (not (notes/doc/hidden? row)))
      (notes/doc/list)))
  "All visible (non-hidden) notes for the index.")

(define notes/doc/kind
  (lambda (n) (car (cdr (cdr (cdr (cdr (cdr n)))))))
  "Extract the kind field from a decoded note row.")

(define notes/doc/list
  (lambda
    ()
    (if
      (notes/tree/enabled?)
      (notes/tree/list-notes)
      (list))))

(define notes/doc/list-folder
  (lambda
    (folder)
    (let
      ((want (as-str folder)))
      (list/filter
        (lambda
          (row)
          (string-eq (notes/doc/row-folder row) want))
        (notes/doc/index-list))))
  "All visible notes whose folder matches.")

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
        (as-str folder))))
  "Build the JSON fields string for a note row.")

(define notes/doc/outline
  (lambda (n) (car (cdr (cdr (cdr n)))))
  "Extract the outline tree from a decoded note row.")

(define notes/doc/row
  (lambda
    (slug)
    (let
      ((tr
        (if
          (notes/tree/enabled?)
          (notes/tree/find-slug slug)
          ())))
      (if
        (null? tr)
        (let
          ((rs (find-rows (quote note) "slug" (as-str slug) 1)))
          (if (null? rs) () (car rs)))
        tr)))
  "Look up a note row by slug from the tree or data_rows.")

(define notes/doc/row-folder
  (lambda
    (row)
    (if
      (null? row)
      ""
      (let
        ((f (dict-get (cdr row) "folder")))
        (if (null? f) "" (as-str f)))))
  "Folder field of a row (empty when absent).")

(define notes/doc/row-path
  (lambda
    (row)
    (if
      (null? row)
      ""
      (let
        ((p (dict-get (cdr row) "_path")))
        (if (null? p) "" (as-str p)))))
  "Tree path of a row (empty when absent).")

(define notes/doc/search
  (lambda
    (q)
    (let
      ((needle (string-trim (as-str q))))
      (if
        (string-eq needle "")
        (list)
        (list/map
          (lambda (j) (if (null? j) "{}" (as-str j)))
          (tree/search-text needle 20)))))
  "Full-text search the garden, returning hit dicts.")

(define notes/doc/set-folder
  (lambda
    (slug folder)
    (let
      ((row (notes/doc/row (as-str slug))))
      (if
        (null? row)
        ()
        (if
          (notes/tree/enabled?)
          (let
            ((old (notes/doc/row-path row))
              (newp (notes/tree/note-path folder (as-str slug))))
            (if
              (string-eq old "")
              (notes/doc/upsert-in
                slug
                (dict-get (cdr row) "title")
                (outline/decode (dict-get (cdr row) "outline"))
                (dict-get (cdr row) "tags")
                (dict-get (cdr row) "kind")
                (dict-get (cdr row) "day")
                folder)
              (if (string-eq old newp) old (tree/move old newp))))
          (update-field
            (quote note)
            (car row)
            "folder"
            (as-str folder))))))
  "Move a note into a folder (tree move or folder-field update).")

(define notes/doc/show-archived?
  (lambda
    ()
    (string-eq (as-str (request-param "show")) "archived"))
  "Whether the show=archived request param is set.")

(define notes/doc/tags
  (lambda (n) (car (cdr (cdr (cdr (cdr n))))))
  "Extract the tags field from a decoded note row.")

(define notes/doc/title
  (lambda (n) (car (cdr (cdr n))))
  "Extract the title field from a decoded note row.")

(define notes/doc/upsert
  (lambda
    (slug title outline tags kind day)
    (let
      ((row (notes/doc/row slug)))
      (notes/doc/upsert-in
        slug
        title
        outline
        tags
        kind
        day
        (if (null? row) "" (notes/doc/row-folder row)))))
  "Insert or update a note, preserving its folder.")

(define notes/doc/upsert-in
  (lambda
    (slug
      title
      outline
      tags
      kind
      day
      folder)
    (if
      (notes/tree/enabled?)
      (if
        (outline/valid? outline)
        (let
          ((prev (notes/doc/row slug)))
        (let
          ((uuid (if (null? prev) (random-uuid) (car prev)))
            (oldp (if (null? prev) "" (notes/doc/row-path prev))))
          (let
            ((path
                (if
                  (string-eq oldp "")
                  (notes/tree/note-path folder slug)
                  oldp))
              (body
                (dict-set*
                  "{}"
                  (list
                    "uuid"
                    uuid
                    "slug"
                    (as-str slug)
                    "title"
                    (as-str title)
                    "tags"
                    (as-str tags)
                    "kind"
                    (as-str kind)
                    "day"
                    (as-str day)))))
            (seq
              (notes/tree/ensure-folder (notes/tree/root) (whoami))
              (tree/upsert path "note" "note" body)
              (notes/tree/save-outline path outline)
              (if
                (and
                  (not (null? prev))
                  (not (string-eq oldp ""))
                  (not
                    (string-eq (as-str folder) (notes/doc/row-folder prev))))
                (notes/doc/set-folder slug folder)
                0)
              (roam/reindex (as-str slug) outline)
              slug))))
        (string-append
          "refused: outline is malformed (every node must be a 5-element list [id title body todo kids], recursively); note not changed: "
          (as-str slug)))
      (let
        ((row (notes/doc/row slug))
          (fields
            (notes/doc/make-fields
              slug
              title
              outline
              tags
              kind
              day
              folder)))
        (seq
          (if
            (null? row)
            (create (quote note) fields)
            (update (quote note) (car row) fields))
          (roam/reindex (as-str slug) outline)
          slug))))
  "Insert or update a note into a given folder.")

;; (notes/doc/get-dict slug) — dict view of doc/get: {id,slug,title,outline,
;; tags,kind,day,folder}, outline as a JSON array string (json-emit). Stops
;; callers car/cdr-laddering the positional tuple (Board card b0863753).
(define notes/doc/get-dict
  (lambda (slug)
    (let* ((row (notes/doc/get slug)))
      (if (null? row)
          ()
          (let* ((d0 (dict-set "{}" "id" (as-str (nth row 0))))
                 (d1 (dict-set d0 "slug" (as-str (nth row 1))))
                 (d2 (dict-set d1 "title" (as-str (nth row 2))))
                 (d3 (dict-set d2 "outline" (json-emit (nth row 3))))
                 (d4 (dict-set d3 "tags" (as-str (nth row 4))))
                 (d5 (dict-set d4 "kind" (as-str (nth row 5))))
                 (d6 (dict-set d5 "day" (as-str (nth row 6)))))
            (dict-set d6 "folder" (as-str (nth row 7))))))))

;; (notes/doc/append-node slug node-slug title body) — ONE call read-modify-
;; upsert: append a top-level heading to the note's outline (or create the
;; note when missing). Covers log/sessions-style registers (Board b0863753).
;; The outline arg is the ROOT node [id title body todo kids]; append goes
;; into its kids. Day rides now-iso on create.
(define notes/doc/append-node
  (lambda (slug node-slug title body)
    (let* ((node (list (as-str node-slug) (as-str title) (as-str body) "" (list)))
           (row (notes/doc/get slug)))
      (if (null? row)
          (notes/doc/upsert slug (as-str title)
                            (list "root" (as-str title) "" "" (list node))
                            "" "" (substring (now-iso) 0 10))
          (let* ((root (nth row 3))
                 (new-root (outline/set-kids root (append (outline/kids root) (list node)))))
            (notes/doc/upsert (as-str (nth row 1)) (as-str (nth row 2)) new-root
                              (as-str (nth row 4)) (as-str (nth row 5)) (as-str (nth row 6))))))))

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
            (html/button (html/attr "type" "submit") "Save outline"))))))
  "HTML textarea form to save a note's outline.")

(define notes/empty-outline
  (lambda (title) (outline/node "root" title))
  "Build an empty root-only outline titled title.")

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
      (notes/get "inbox")))
  "Get the Inbox note, creating it if missing.")

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
            (dict-get f "day"))))))
  "Decode a note row by slug into (id slug title outline tags kind day).")

(define notes/get-day
  (lambda (n) (car (cdr (cdr (cdr (cdr (cdr (cdr n))))))))
  "Extract the day field from a decoded note row.")

(define notes/get-kind
  (lambda (n) (car (cdr (cdr (cdr (cdr (cdr n)))))))
  "Extract the kind field from a decoded note row.")

(define notes/get-outline
  (lambda (n) (car (cdr (cdr (cdr n)))))
  "Extract the outline tree from a decoded note row.")

(define notes/get-tags
  (lambda (n) (car (cdr (cdr (cdr (cdr n))))))
  "Extract the tags field from a decoded note row.")

(define notes/get-title
  (lambda (n) (car (cdr (cdr n))))
  "Extract the title field from a decoded note row.")

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
              (redirect "/notes")))))))
  "Dispatch a notes POST by action to the matching handler.")

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
              (string-eq kind "issue")
              #t
              (if
                (string-eq slug "inbox")
                #t
                (string-contains? tags "hidden"))))))))
  "Whether a row should be hidden from the index (inbox/hidden/tagged).")

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
        (html/ul (html/attr "class" "card") (notes/recent-list)))))
  "HTML index page: create form and recent notes.")

(define notes/index-list
  (lambda
    ()
    (list/filter
      (lambda (row) (not (notes/hidden-from-index? row)))
      (notes/list)))
  "All visible (non-hidden) notes for the index.")

(define notes/issues/register-html
  (lambda
    ()
    (let
      ((tree (notes/issues-fast-tree)))
      (if
        (null? tree)
        ""
        (html/join
          (list
            (html/h2 "Register")
            (html/ul
              (html/attr "class" "card")
              (notes/issues/nodes-html (outline/kids tree))))))))
  "The frozen legacy ledger register, rendered LIVE from the issues
     outline (one tree/find-by + one snapshot fold). Was stored as cached
     HTML by notes/issues-cache-refresh (deleted with the notes-cache
     apparatus — borge/notes.outline-read-path).")

(define notes/issues-fast-tree
  (lambda
    ()
    (let
      ((hits (tree/find-by "note" "slug" "log/issues" 1)))
      (if
        (null? hits)
        (list)
        (let
          ((n (notes/tree/parse (car hits))))
          (if
            (null? n)
            (list)
            (let
              ((b (notes/tree/body n))
                (path (as-str (dict-get n "path"))))
              (outline/node*
                "root"
                (as-str (dict-get b "title"))
                ""
                ""
                (notes/tree/heading-kids path))))))))
  "register outline via ONE subtree snapshot (the same fold the live
     doc/get read path now runs — see notes/tree/heading-kids obituary)")

(define notes/issues/add
  (lambda
    (kind title body)
    (let*
      ((np (notes/tree/note-path "" "log/issues"))
        (full-title
          (string-append
            (as-str kind)
            ": "
            (notes/issues/title-or-kind kind title)))
        (nid (notes/issues/node-id (list))))
      (seq
        (notes/issues/ensure)
        (notes/node/upsert np nid full-title (as-str body) "")
        (notes/issues/card-create
          nid
          (as-str kind)
          full-title
          (as-str body))
        nid)))
  "Add an issue to log/issues (single per-node write) AND create its\n   kind-tagged Board card (linked via the card's issue_id field) — the\n   issues<->cards convergence. Returns the new ledger node id. Resolve/\n   reopen/delete on either face reconcile both.")

(define notes/issues/board-list-id
  (lambda
    ()
    (let
      ((hit
        (list/filter
          (lambda
            (l)
            (string-eq (as-str (dict-get (cdr l) "title")) "Board"))
          (todo/list/all))))
      (if
        (null? hit)
        (todo/list/ensure-inbox)
        (as-str (car (car hit))))))
  "The shared work board list id: the todo-list titled \"Board\", falling\n     back to the Inbox list when the Board is absent.")

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

(define notes/issues/card-create
  (lambda
    (issue-id kind title body)
    (let
      ((existing (notes/issues/card-for issue-id)))
      (if
        (null? existing)
        (let
          ((cid
            (as-str
              (todo/item/add
                (notes/issues/board-list-id)
                (as-str title)))))
          (seq
            (todo/tree/save
              "todo-item"
              cid
              (dict-set*
                (todo/item/get cid)
                (list
                  "kind"
                  (as-str kind)
                  "issue_id"
                  (as-str issue-id)
                  "notes"
                  (as-str body))))
            cid))
        (notes/issues/card-id existing))))
  "Create the Board card for ledger issue id: kind tag, issue_id link,\n     ledger body -> card notes. IDEMPOTENT — returns the existing linked\n     card id when one is already present (safe to re-run).")

(define notes/issues/card-delete
  (lambda
    (id)
    (let
      ((cid (as-str id))
        (iid (notes/issues/card-issue-id id)))
      (seq
        (todo/item/delete cid)
        (if
          (string-eq iid "")
          #f
          (notes/node/delete
            (notes/tree/note-path "" "log/issues")
            iid)))))
  "Delete card id AND its linked ledger issue node (one issue, two views;\n     was: left the issue rows entirely, which re-synced a ghost card).")

(define notes/issues/card-for
  (lambda
    (issue-id)
    (let
      ((hits
        (tree/find-by
          "datum"
          "issue_id"
          (as-str issue-id)
          1
          (todo/tree/prefix "todo-item"))))
      (if (null? hits) () (car hits))))
  "The todo-item node JSON linked to ledger issue id via its `issue_id`\n     body field (first hit), or () when the issue has no card. Cards are\n     `datum`-kind nodes under the todo-item type prefix, so the lookup is\n     kind=\"datum\" + the todo-item path prefix (a bare garden-scoped find\n     does NOT see datum rows).")

(define notes/issues/card-html
  (lambda
    (row)
    (let*
      ((id (as-str (car row)))
        (fields (cdr row))
        (title (as-str (dict-get fields "title")))
        (kind (as-str (dict-get fields "kind")))
        (state (todo/item/state fields))
        (cols (todo/state/columns-for-item id))
        (terminal (notes/issues/last cols))
        (done (string-eq state terminal)))
      (html/li
        (html/attr "class" "issue-item")
        (html/join
          (list
            (html/div
              (html/attr
                "class"
                (if done "issue-head done" "issue-head"))
              (html/join
                (list
                  (html/code kind)
                  " "
                  (html/code id)
                  " "
                  (html/strong title))))
            (html/p*
              (html/attr "class" "issue-body")
              (html/join
                (list
                  (html/text "state ")
                  (html/code state)
                  (if
                    (string-eq (todo/state/pipeline-for-item id) "own")
                    (html/text " · own pipeline")
                    ""))))
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
                          (if done "card-reopen" "card-resolve"))
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
                        (html/hidden "action" "card-delete")
                        (html/hidden "id" id)
                        (html/button
                          (html/attrs
                            (list
                              (html/attr "type" "submit")
                              (html/attr "class" "secondary")))
                          (html/text "Del")))))))))))))
  "HTML for one card-as-issue row: kind badge + id + title, its effective\n     pipeline STATE (own override > list columns > global default), and\n     Resolve/Reopen mapped to the pipeline terminator/head (the state machine\n     terminal) so the card never leaves its pipeline.")

(define notes/issues/card-id
  (lambda
    (node)
    (if
      (null? node)
      ""
      (as-str
        (todo/tree/redash
          (todo/tree/tail (dict-get node "path"))))))
  "Card id from a todo-item node JSON: the redashed path tail (the same id\n     todo/item/all rows carry — NOT the node's `id` uuid field); \"\" when\n     absent. Mirrors todo/tree/node->pair.")

(define notes/issues/card-issue-id
  (lambda
    (card-id)
    (let
      ((row (todo/item/get (as-str card-id))))
      (if (null? row) "" (as-str (dict-get row "issue_id")))))
  "The ledger issue id linked from card id (its `issue_id` body field);\n     \"\" when the card is unlinked or missing.")

(define notes/issues/card-reopen
  (lambda
    (id)
    (let
      ((cols (todo/state/columns-for-item (as-str id))))
      (if
        (null? cols)
        (as-str id)
        (seq
          (todo/item/set-state (as-str id) (as-str (car cols)))
          (notes/issues/card-sync-ledger (as-str id) "")))))
  "Move card id back to its pipeline head AND clear the linked ledger\n     node's todo status.")

(define notes/issues/card-resolve
  (lambda
    (id)
    (let*
      ((cols (todo/state/columns-for-item (as-str id)))
        (terminal
          (if
            (not
              (null?
                (list/filter
                  (lambda (c) (string-eq (as-str c) "DONE"))
                  cols)))
            "DONE"
            (notes/issues/last cols))))
      (if
        (null? cols)
        (as-str id)
        (seq
          (todo/item/set-state (as-str id) terminal)
          (notes/issues/card-sync-ledger (as-str id) "DONE")))))
  "Move card id to its resolve state AND set the linked ledger node DONE.\n     Resolve state = DONE when the item's effective pipeline has one, else the\n     last column (old behavior; obituary: Board's column order ends PARKED,\n     so the old last-column terminator PARKED resolved cards instead of\n     finishing them).")

(define notes/issues/card-sync-ledger
  (lambda
    (card-id todo)
    (let
      ((iid (notes/issues/card-issue-id card-id)))
      (if
        (string-eq iid "")
        ""
        (notes/issues/set-todo iid todo))))
  "Reconcile the LEDGER side of card-id: set the linked ledger node's todo\n     status (\"DONE\" or \"\") via one per-node write. No-op for unlinked\n     cards. Low-level: does NOT recurse back into card-resolve/reopen.")

(define notes/issues/cards
  (lambda
    ()
    (list/filter
      (lambda
        (row)
        (not
          (or
            (todo/state/archived? (todo/item/state (cdr row)))
            (string-eq (as-str (dict-get (cdr row) "kind")) ""))))
      (todo/item/all)))
  "Todo-item rows tagged with an issue kind (kind != \"\") — the /issues\n     convergence: cards carry a kind (BUG/DESIGN/SMELL/CLEANUP) convention and\n     /issues reads them AS ISSUE ROWS through the shared pipeline\n     (borge/workstreams.borg pipeline-model). Archived cards are excluded\n     (archived is closed). The legacy note-ledger stays a frozen archive; this\n     is the live issue face (no corpus migration in this task).")

(define notes/issues/cards-html
  (lambda
    (rows)
    (if
      (null? rows)
      ""
      (html/join
        (list
          (notes/issues/card-html (car rows))
          (notes/issues/cards-html (cdr rows))))))
  "Concatenated card-as-issue HTML for rows.")

(define notes/issues/cards-section
  (lambda
    ()
    (let
      ((rows (notes/issues/cards)))
      (html/join
        (list
          (html/h2 "Issue cards")
          (if
            (null? rows)
            (html/p*
              (html/attr "class" "muted")
              "No cards tagged with a kind (BUG/DESIGN/SMELL/CLEANUP) yet — set a card's kind and it shows here as an issue row through the shared pipeline.")
            (html/ul
              (html/attr "class" "card")
              (notes/issues/cards-html rows)))))))
  "The 'Issue cards' section rendered on /issues: todo-items tagged with a\n     kind, through the shared pipeline. Rendered inline after the frozen legacy\n     ledger register (the cache row it used to ride is deleted —\n     borge/notes.outline-read-path).")

(define notes/issues/delete
  (lambda
    (id)
    (let
      ((card (notes/issues/card-for id)))
      (seq
        (notes/node/delete
          (notes/tree/note-path "" "log/issues")
          id)
        (if
          (null? card)
          #f
          (todo/item/delete (notes/issues/card-id card))))))
  "Delete ledger issue id AND its linked card (one issue, two views).\n     Returns #t when the ledger node was found.")

(define notes/issues/ensure
  (lambda
    ()
    (let
      ((found (tree/find-by "note" "slug" "log/issues" 1)))
      (if
        (null? found)
        (notes/doc/upsert
          "log/issues"
          "log/issues"
          (notes/doc/empty-outline "log/issues")
          "log"
          "issue"
          "")
        (car found))))
  "ensure the register exists via ONE find-by (was: full doc/get walk, 100-300s under DB degradation).
     Arity fix 2026-09-27: this call passed EIGHT args to the six-arg
     notes/doc/upsert (a duplicated slug and a stray \"\") and would have
     raised 'arity mismatch' the first time the register had to be created
     from scratch — latent, because the register already exists on every
     live habitat. tags/kind/day now match notes/issues/upsert, the
     register's other writer ('log' / 'issue' / ''), so a fresh register
     is shaped identically to a repaired one.")

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
        ((string-eq action "card-resolve")
          (seq
            (notes/issues/card-resolve (request-param "id"))
            (redirect "/issues")))
        ((string-eq action "card-reopen")
          (seq
            (notes/issues/card-reopen (request-param "id"))
            (redirect "/issues")))
        ((string-eq action "card-delete")
          (seq
            (notes/issues/card-delete (request-param "id"))
            (redirect "/issues")))
        (else (redirect "/issues")))))
  "mutations redirect; the next GET renders the register + cards LIVE
     from the store (borge/notes.outline-read-path)")

(define notes/issues/kind-of
  (lambda
    (title)
    (let
      ((parts (string-split (as-str title) ":")))
      (if
        (< (count parts) 2)
        ""
        (let
          ((k (string-trim (car parts))))
          (if
            (and
              (not (string-eq k ""))
              (< (string-length k) 12)
              (string-eq k (string-upcase k)))
            k
            "")))))
  "The issue kind of a ledger heading title: the ALL-CAPS token before the\n     first \":\" (BUG:, DESIGN:, WANT:, PERF:, ...), empty when the title has\n     no such prefix.")

(define notes/issues/last
  (lambda
    (xs)
    (if
      (null? xs)
      ""
      (if
        (null? (cdr xs))
        (as-str (car xs))
        (notes/issues/last (cdr xs)))))
  "The last element of xs (as a string); \"\" when xs is empty.")

(define notes/issues/list-html
  (lambda
    ()
    (let
      ((html (string-append
               (notes/issues/register-html)
               (notes/issues/cards-section))))
      (if
        (string-eq html "")
        (html/p* (html/attr "class" "muted") "No issues yet.")
        html)))
  "register (frozen legacy ledger) + issue cards, rendered LIVE from the
     store on every request (borge/notes.outline-read-path: was a cached
     HTML row refreshed by an unbudgeted job, staleness by design)")

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
  (lambda
    (tree)
    (string-append "i-" (as-str (now)) "-" (random-uuid)))
  "Generate a collision-free issue node id: i-<unix-seconds>-<uuid>. Fixes same-second collision in i-1786862188.")

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
  (lambda
    (id)
    (seq
      (notes/issues/set-todo id "")
      (let
        ((card (notes/issues/card-for id)))
        (if
          (null? card)
          ""
          (notes/issues/card-reopen (notes/issues/card-id card))))))
  "Reopen ledger issue id AND its linked card (card -> pipeline head).")

(define notes/issues/resolve
  (lambda
    (id)
    (seq
      (notes/issues/set-todo id "DONE")
      (let
        ((card (notes/issues/card-for id)))
        (if
          (null? card)
          ""
          (notes/issues/card-resolve (notes/issues/card-id card))))))
  "Resolve ledger issue id AND its linked card (card -> pipeline terminal).\n     Both faces stay consistent; safe when the issue has no card.")

(define notes/issues/set-todo
  (lambda
    (id todo)
    (notes/node/set-todo
      (notes/tree/note-path "" "log/issues")
      id
      todo))
  "Set todo status of one issue id in log/issues. Single per-node write (no whole-tree rewrite). Returns #t if found, #f if absent. Fixes i-1786862188 and the data-loss clobber.")

(define notes/issues/sync
  (lambda
    ()
    (let
      ((tree (notes/issues-fast-tree)))
      (if
        (null? tree)
        (cons 0 0)
        (let
          ((kids (outline/kids tree))
            (created (notes/issues/sync-kids (outline/kids tree))))
          (cons
            created
            (count
              (list/filter
                (lambda
                  (k)
                  (not (string-eq (as-str (outline/todo k)) "DONE")))
                kids)))))))
  "Backfill the convergence: Board cards for OPEN ledger issues missing\n     them (kind = heading prefix, notes = ledger body), and cards resolved\n     for ledger issues already DONE. Idempotent — re-running creates\n     nothing. Returns (created . total-open-after).")

(define notes/issues/sync-kids
  (lambda
    (kids)
    (if
      (null? kids)
      0
      (let*
        ((k (car kids))
          (id (as-str (outline/id k)))
          (done (string-eq (as-str (outline/todo k)) "DONE"))
          (card
            (if (string-eq id "") () (notes/issues/card-for id)))
          (here
            (cond
              ((and (not done) (null? card))
                (seq
                  (notes/issues/card-create
                    id
                    (notes/issues/kind-of (outline/title k))
                    (outline/title k)
                    (outline/body k))
                  1))
              ((and done (not (null? card)))
                (seq
                  (notes/issues/card-resolve (notes/issues/card-id card))
                  0))
              (else 0))))
        (+ here (notes/issues/sync-kids (cdr kids))))))
  "Fold one register snapshot level: create a card per OPEN ledger issue\n     without one; resolve cards whose ledger issue is DONE. Returns the\n     number of cards CREATED.")

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
                      "del"))))))))))
  "HTML list item for a note row with a delete button.")

(define notes/legacy/tree-render
  (lambda
    ()
    (html/p*
      (html/attr "class" "muted")
      "notes/tree/render retired — use notes/ui/folders-html (flat index)."))
  "HTML note that the retired tree/render is gone.")

(define notes/legacy/tree-render-folder
  (lambda
    (path names depth)
    (html/p*
      (html/attr "class" "muted")
      (string-append
        "notes/tree/render-folder retired ("
        (as-str path)
        ")")))
  "HTML note that the retired tree/render-folder is gone.")

(define notes/light/add-prefixes
  (lambda
    (parts prefix acc)
    (if
      (null? parts)
      acc
      (let*
        ((seg (as-str (car parts)))
          (full
            (if
              (string-eq prefix "")
              seg
              (string-append prefix "/" seg))))
        (notes/light/add-prefixes
          (cdr parts)
          full
          (notes/light/cons-unique full acc)))))
  "Collect every ancestor folder prefix of a path into acc.")

(define notes/light/all-folder-paths
  (lambda
    (all-notes)
    (notes/light/dedupe
      (notes/tree/sort-by-name
        (list/foldl
          (lambda
            (acc note)
            (let
              ((f (as-str (notes/light/nth note 1))))
              (if
                (string-eq f "")
                acc
                (notes/light/add-prefixes (string-split f "/") "" acc))))
          (list)
          all-notes))))
  "All distinct folder paths (with ancestors) across notes.")

(define notes/light/children-of
  (lambda
    (parent paths)
    (list/filter
      (lambda
        (p)
        (string-eq (notes/light/parent-of p) parent))
      paths))
  "Paths whose immediate parent equals parent.")

(define notes/light/cons-unique
  (lambda
    (x xs)
    (if (notes/light/contains? x xs) xs (cons x xs)))
  "Prepend x to xs unless already present.")

(define notes/light/contains?
  (lambda
    (x xs)
    (if
      (null? xs)
      #f
      (if
        (string-eq x (as-str (car xs)))
        #t
        (notes/light/contains? x (cdr xs)))))
  "Whether a string occurs in a list.")

(define notes/light/dedupe
  (lambda
    (xs)
    (if
      (null? xs)
      (list)
      (notes/light/cons-unique
        (car xs)
        (notes/light/dedupe (cdr xs)))))
  "Remove duplicate strings, keeping first-occurrence order.")

(define notes/light/folder-section
  (lambda
    (folder)
    (let*
      ((all-notes (notes/list-light))
        (items (notes/light/notes-in folder all-notes))
        (label
          (if (string-eq folder "") "(uncategorized)" folder))
        (href (string-append "/notes?folder=" folder)))
      (html/div
        (html/attr "class" "notes-folder card")
        (html/join
          (list
            (html/h3 (html/a href (html/text label)))
            (if
              (null? items)
              (html/p*
                (html/attr "class" "muted")
                "No notes in this folder.")
              (notes/light/render-notes items)))))))
  "HTML card section for a folder's notes.")

(define notes/light/last
  (lambda
    (xs)
    (if
      (null? (cdr xs))
      (car xs)
      (notes/light/last (cdr xs))))
  "Last element of a non-empty list.")

(define notes/light/last-segment
  (lambda
    (path)
    (if
      (string-eq path "")
      ""
      (notes/light/last (string-split path "/"))))
  "Final slash-separated segment of a path.")

(define notes/light/note-li
  (lambda
    (note)
    (let*
      ((title (as-str (notes/light/nth note 2)))
        (slug (as-str (notes/light/nth note 3))))
      (html/li
        (html/attr "class" "note-item")
        (html/a
          (string-append "/notes?slug=" slug)
          (html/text title)))))
  "HTML list item for a light note row.")

(define notes/light/notes-in
  (lambda
    (folder all-notes)
    (list/filter
      (lambda
        (n)
        (string-eq (as-str (notes/light/nth n 1)) folder))
      all-notes))
  "Notes whose folder equals folder.")

(define notes/light/nth
  (lambda
    (xs i)
    (if
      (<= i 0)
      (car xs)
      (notes/light/nth (cdr xs) (- i 1))))
  "The i-th element of a list (0-based).")

(define notes/light/page
  (lambda
    ()
    (let
      ((slug (request-param "slug")))
      (if
        (if (null? slug) #t (string-eq (as-str slug) ""))
        (let
          ((folder (request-param "folder")))
          (html/page
            (list
              (notes/tree/style)
              (html/h1 "Notes")
              (html/h2 "Folders")
              (if
                (if
                  (null? folder)
                  #f
                  (not (string-eq (as-str folder) "")))
                (notes/light/folder-section (as-str folder))
                (notes/light/render)))))
        (html/page (list (notes/ui/show (as-str slug)))))))
  "HTML page for a slug or the flat folder index.")

(define notes/light/parent-of
  (lambda
    (path)
    (if
      (string-eq path "")
      ""
      (let
        ((parts (string-split path "/")))
        (if
          (<= (length parts) 1)
          ""
          (notes/tree/join-slash (notes/tree/butlast parts))))))
  "Parent folder of a slash path (empty for a root).")

(define notes/light/render
  (lambda
    ()
    (let*
      ((all-notes (notes/list-light))
        (paths (notes/light/all-folder-paths all-notes))
        (roots
          (notes/tree/sort-by-name
            (notes/light/children-of "" paths)))
        (uncat (notes/light/notes-in "" all-notes)))
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
                    (notes/light/render-notes uncat)))))
            (html/join
              (list/map
                (lambda
                  (root)
                  (notes/light/render-folder root paths all-notes))
                roots)))))))
  "HTML flat folder tree of all notes.")

(define notes/light/render-folder
  (lambda
    (path paths all-notes)
    (let*
      ((label (notes/light/last-segment path))
        (children (notes/light/children-of path paths))
        (items (notes/light/notes-in path all-notes))
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
            (notes/light/render-notes items)
            (if
              (null? children)
              ""
              (html/join
                (list/map
                  (lambda
                    (child)
                    (notes/light/render-folder child paths all-notes))
                  (notes/tree/sort-by-name children)))))))))
  "HTML tree <details> block for one folder and its children.")

(define notes/light/render-notes
  (lambda
    (note-list)
    (if
      (null? note-list)
      ""
      (html/ul
        (html/attr "class" "tree-items")
        (html/join
          (list/map
            (lambda (n) (notes/light/note-li n))
            note-list)))))
  "HTML list of note items for a folder.")

(define notes/list
  (lambda () (notes/doc/list))
  "List all notes (doc store).")

(define notes/list-light
  (lambda
    ()
    (map
      (lambda
        (r)
        (let
          ((d (cdr r)))
          (list
            (car r)
            (as-str (dict-get d "folder"))
            (as-str (dict-get d "title"))
            (as-str (dict-get d "slug")))))
      (notes/doc/index-list)))
  "Light rows (id folder title slug) for every visible note.")

(define notes/list-light-cap
  (lambda () (notes/list-take (notes/list-light) 40)))

(define notes/list-take
  (lambda
    (xs n)
    (if
      (or (null? xs) (<= n 0))
      (list)
      (cons (car xs) (notes/list-take (cdr xs) (- n 1)))))
  "First n elements of a list.")

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
        (as-str day))))
  "Build the JSON fields string for a note row.")

(define notes/migrate/legacy-all
  (lambda
    ()
    (list/map
      notes/migrate/legacy-one
      (rows (quote note) 2000)))
  "Migrate every legacy note row into the tree.")

(define notes/migrate/legacy-delete-all
  (lambda
    ()
    (list/map
      (lambda
        (row)
        (let
          ((id (as-str (car row)))
            (slug (as-str (dict-get (cdr row) "slug"))))
          (if
            (null? (tree/get-uuid id))
            (string-append "kept-missing-tree:" slug)
            (seq
              (delete (quote note) id)
              (string-append "deleted:" slug)))))
      (rows (quote note) 2000))))

(define notes/migrate/legacy-force-all
  (lambda
    ()
    (list/map
      notes/migrate/legacy-force-one
      (rows (quote note) 2000))))

(define notes/migrate/legacy-force-one
  (lambda
    (row)
    (if
      (null? row)
      "skip-empty"
      (let
        ((id (as-str (car row))) (f (cdr row)))
        (let
          ((slug (as-str (dict-get f "slug")))
            (title (as-str (dict-get f "title")))
            (outline (outline/decode (dict-get f "outline"))))
          (let
            ((existing (tree/get-uuid id)))
            (if
              (null? existing)
              (notes/migrate/legacy-one row)
              (let
                ((path (as-str (dict-get existing "path"))))
                (seq
                  (notes/tree/save-outline path outline)
                  (roam/reindex slug outline)
                  (string-append "reoutlined:" slug)))))))))
  "Force-write legacy row headings into the tree, re-outlining existing rows.")

(define notes/migrate/legacy-one
  (lambda
    (row)
    (if
      (null? row)
      "skip-empty"
      (let
        ((id (as-str (car row))) (f (cdr row)))
        (let
          ((slug (as-str (dict-get f "slug")))
            (title (as-str (dict-get f "title")))
            (tags (as-str (dict-get f "tags")))
            (kind (as-str (dict-get f "kind")))
            (day (as-str (dict-get f "day")))
            (folder (as-str (dict-get f "folder")))
            (outline (outline/decode (dict-get f "outline"))))
          (let
            ((existing (tree/get-uuid id))
              (by-slug (notes/tree/find-slug slug)))
            (if
              (not (null? existing))
              (string-append "exists-uuid:" slug)
              (if
                (not (null? by-slug))
                (string-append "exists-slug:" slug)
                (let
                  ((path (notes/tree/note-path folder slug))
                    (body
                      (dict-set*
                        "{}"
                        (list
                          "uuid"
                          id
                          "slug"
                          slug
                          "title"
                          title
                          "tags"
                          tags
                          "kind"
                          (if (string-eq kind "") "note" kind)
                          "day"
                          day))))
                  (seq
                    (notes/tree/ensure-folder (notes/tree/root) (whoami))
                    (tree/upsert path "note" "note" body)
                    (notes/tree/save-outline path outline)
                    (roam/reindex slug outline)
                    (string-append "migrated:" slug))))))))))
  "Migrate a single legacy note row into the tree.")

(define notes/missing-html
  (lambda
    (slug)
    (html/join
      (list
        (html/h1 "Missing")
        (html/p*
          (html/attr "class" "err")
          (string-append "No note " (as-str slug)))
        (html/p* "" (html/a "/notes" "All notes")))))
  "HTML 'no note' page for a missing slug.")

(define notes/move-folder
  (lambda
    (slug folder)
    (let*
      ((r (notes/doc/row (as-str slug)))
        (f (if (null? r) "{}" (cdr r)))
        (bv (dict-get f "body"))
        (f2 (if (null? bv) (dict-set f "body" "") f))
        (fields (dict-set f2 "folder" (as-str folder))))
      (if (null? r) () (update (quote note) (car r) fields)))))

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
                (render-html (outline/html-lite slug new-tree #f)))))))))
  "Handle a notes-node-op POST applied to a note's outline.")

(define notes/node/delete
  (lambda
    (note-path node-id)
    (let
      ((p
        (string-append
          note-path
          ".h."
          (tree/path-label (as-str node-id)))))
      (let
        ((row (tree/get p)))
        (if (null? row) #f (seq (tree/delete p "recursive") #t)))))
  "Delete one heading node (and its subtree) at note-path.h.<node-id>. One tree write. Returns #t if deleted, #f if absent.")

(define notes/node/get
  (lambda
    (note-path node-id)
    (let
      ((p
        (string-append
          note-path
          ".h."
          (tree/path-label (as-str node-id)))))
      (let
        ((row (tree/get p)))
        (if
          (null? row)
          ()
          (notes/tree/body (notes/tree/parse row))))))
  "Read one heading node's body dict at note-path.h.<node-id>, or () if absent. Single-node read.")

(define notes/node/path
  (lambda
    (note-path node-id)
    (string-append
      note-path
      ".h."
      (tree/path-label (as-str node-id))))
  "The heading tree path note-path.h.<sanitized-node-id>. Pure string computation.")

(define notes/node/set-todo
  (lambda
    (note-path node-id todo)
    (let
      ((p
        (string-append
          note-path
          ".h."
          (tree/path-label (as-str node-id)))))
      (let
        ((row (tree/get p)))
        (if
          (null? row)
          #f
          (seq
            (tree/upsert
              p
              "heading"
              "note"
              (dict-set
                (notes/tree/body (notes/tree/parse row))
                "todo"
                (as-str todo)))
            #t)))))
  "Set the todo field of one heading node at note-path.h.<node-id>. One read + one write on a single node (no whole-tree rewrite). Returns #t if the node exists, #f otherwise.")

(define notes/node/upsert
  (lambda
    (note-path node-id title body todo)
    (let
      ((p
        (string-append
          note-path
          ".h."
          (tree/path-label (as-str node-id)))))
      (seq
        (tree/upsert
          p
          "heading"
          "note"
          (dict-set*
            "{}"
            (list
              "id"
              (as-str node-id)
              "title"
              (as-str title)
              "body"
              (as-str body)
              "todo"
              (as-str todo))))
        (as-str node-id))))
  "Upsert a single leaf heading node at note-path.h.<node-id>. One tree write (no whole-tree rewrite, no notes/doc/row read). Returns node-id. Fixes i-1787030118 / the data-loss clobber.")

(define notes/open-todos
  (lambda
    ()
    (list/filter
      (lambda
        (t)
        (outline/todo-open? (car (cdr (cdr (cdr t))))))
      (notes/all-todos)))
  "All open (incomplete) todo rows across notes.")

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
          (outline/html-lite
            (as-str (request-param "slug"))
            (notes/get-outline n)
            #f)))))
  "HTML partial of a note's outline for htmx.")

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
      (notes/tree/full-notes)))
  "Collect todo rows across every note (doc store).\n   log/issues fix: full index rows (real outline), not the light\n   tree/note-index rows whose outline is hardcoded [].")

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
      (else tree)))
  "Apply a single op to an outline node, returning the new tree.")

(define notes/outline/apply-op-sel
  (lambda
    (slug tree op nid)
    (let
      ((new-tree (notes/outline/apply-op slug tree op nid)))
      (let
        ((sel (notes/outline/sel-for op nid new-tree tree)))
        (list new-tree sel))))
  "Apply an op and return (new-tree selected-id).")

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
              day))))))
  "Cycle the todo state of a node in a note (doc store).")

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
            "eval")))))
  "HTML form to eval a Lisp expression and splice the result.")

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
                (__notes_eval_fn node prev (as-str slug)))))))))
  "Eval expr against a node and return the result value.")

(define notes/outline/eval-target
  (lambda
    ()
    (let
      ((tg (request-param "target")))
      (if
        (if (null? tg) #t (string-eq (as-str tg) ""))
        "child"
        (as-str tg))))
  "Request eval-splice target (child/sibling/body), default child.")

(define notes/outline/new-node
  (lambda () (outline/node (gensym "n") "New node"))
  "A fresh outline node with a generated id and default title.")

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
                      (outline/html-lite slug new-tree show-archived)
                      (html/void
                        "input"
                        (html/attrs
                          (list
                            (html/attr "type" "hidden")
                            (html/attr "id" "ol-selected-id")
                            (html/attr "name" "node-id")
                            (html/attr "hx-swap-oob" "true")
                            (html/attr "value" sel))))))))))))))
  "Handle a node-op, persist the tree, and return updated outline HTML.")

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
      (list et nid)))
  "Splice an eval result into the tree at a target; returns (new-tree nid).")

(define notes/outline/open-todos
  (lambda
    ()
    (list/filter
      (lambda
        (t)
        (outline/todo-open? (car (cdr (cdr (cdr t))))))
      (notes/outline/all-todos)))
  "All open (incomplete) todo rows across notes (doc store).")

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
          (outline/html-lite
            (as-str (request-param "slug"))
            (notes/doc/outline n)
            show-archived)))))
  "HTML partial of a note's outline for htmx (doc store).")

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
            (structed/id (structed/list/nth ks 0)))))))
  "Selected node id after adding a child (the new child).")

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
              (structed/id (structed/list/nth sibs (+ i 1)))))))))
  "Selected node id after adding a sibling (the node after).")

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
          (structed/sibling-index outline/kids tree nid)))))
  "Selected node id after deleting a node.")

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
          (structed/id parent)))))
  "Pick the id to select after a deletion within a sibling list.")

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
      (else nid)))
  "Selected id after an op, given the old and new trees.")

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
            (outline/add-child-by-id tree nid child))))))
  "Splice an eval result into the tree as body, child, or sibling.")

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
          (html/page (list (notes/ui/show (as-str slug))))))))
  "Serve the notes page: handle a POST, else render index or show.")

(define notes/recent-list
  (lambda
    ()
    (list/foldl
      (lambda
        (acc row)
        (string-append acc (notes/item-li row)))
      ""
      (notes/list)))
  "Concatenated HTML of recent note list items.")

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
                      #t))))))))))
  "Move a node from one note into another, returning success.")

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
              "refile"))))))
  "HTML refile select form for a node.")

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
      (notes/list)))
  "HTML option list of destination slugs except the source.")

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
                            new-slug)))))))))))))
  "Extract a node into its own new note; returns the new slug.")

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
              "extract"))))))
  "HTML extract button form for a node.")

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
          (redirect (string-append "/notes?slug=" new))))))
  "Handle an extract POST, redirecting to the new note.")

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
              "refile"))))))
  "HTML refile select form for a node (doc variant).")

(define notes/refile/handle
  (lambda
    ()
    (let
      ((src (as-str (request-param "src_slug")))
        (nid (as-str (request-param "node_id")))
        (dst (as-str (request-param "dst_slug")))
        (dst-nid
          (if
            (null? (request-param "dst_node_id"))
            ""
            (as-str (request-param "dst_node_id")))))
      (seq
        (notes/refile/move src nid dst dst-nid)
        (redirect (string-append "/notes?slug=" src)))))
  "Handle a refile POST, moving the node and redirecting.")

(define notes/refile/move
  (lambda
    (src-slug nid dst-slug dst-nid)
    (let
      ((src (notes/doc/get src-slug))
        (dst (notes/doc/get dst-slug))
        (sid (as-str src-slug))
        (did (as-str dst-slug))
        (id (as-str nid))
        (parent
          (let
            ((p (as-str dst-nid)))
            (if (string-eq p "") "root" p))))
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
                            (outline/add-child-by-id dst-tree parent node)
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
                          #t))))))))))))
  "Move a node between notes, returning success.")

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
      (notes/doc/list)))
  "HTML option list of destination slugs except the source (doc variant).")

(define notes/refile/slug-char-ok?
  (lambda
    (c)
    (string-contains?
      "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
      (as-str c)))
  "Whether a character is safe in a slug.")

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
            (if (> (string-length s) 48) (substring s 0 48) s))))))
  "Slugify a title, trimming, hyphenating invalid runs, capping length.")

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
              #t))))))
  "Accumulate a slug from a title, collapsing invalid runs to single hyphens.")

(define notes/refile/unique-slug
  (lambda
    (base)
    (if
      (null? (notes/doc/row (as-str base)))
      (as-str base)
      (notes/refile/unique-slug
        (string-append (as-str base) "-" (as-str (gensym "x"))))))
  "A slug that does not collide with an existing note.")

(define notes/row
  (lambda
    (slug)
    (let
      ((rs (find-rows (quote note) "slug" (as-str slug) 1)))
      (if (null? rs) () (car rs))))
  "Look up a note row by slug from data_rows.")

(define notes/search
  (lambda (q) (notes/doc/search q)))

(define notes/show-archived?
  (lambda
    ()
    (string-eq (as-str (request-param "show")) "archived"))
  "Whether the show=archived request param is set.")

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
        (outline/html-lite slug outline #f)
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
            "},true);")))))
  "HTML body for a note page with outline and controls.")

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
          (notes/get-tags n)))))
  "HTML page for a note slug (legacy store).")

(define notes/tree-css
  (css/sheet
    (list
      (css/raw
        "/* notes-tree layout (component-scoped; colors via var(--*)) */")
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
          "2px solid var(--border)"))
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
        (list "text-decoration" "none" "color" "var(--link)"))
      (css/rule
        ".notes-tree .tree-folder-header a:hover"
        (list
          "text-decoration"
          "underline"
          "color"
          "var(--ink)"))
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
        (list "flex" "1 1 auto" "color" "var(--link)"))
      (css/rule
        ".notes-tree li.note-item > a:hover"
        (list "color" "var(--ink)"))
      (css/rule
        ".notes-tree .tree-uncategorized"
        (list
          "margin-top"
          "1rem"
          "padding-top"
          ".5rem"
          "border-top"
          "1px solid var(--border)"))
      (css/rule
        ".notes-tree .tree-uncategorized h3"
        (list
          "font-size"
          ".95rem"
          "color"
          "var(--muted)"
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
          "1px solid var(--border)"
          "background"
          "var(--panel)"
          "color"
          "var(--muted)"
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
        (list "background" "var(--ink)" "color" "var(--bg)")))))

(define notes/tree-css-patch
  (css/sheet
    (list
      (css/raw
        "/* notes/tree-css-patch: retired (was v4 global clobber; now empty) */"))))

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
      names))
  "Parents of each path missing from names, for rendering.")

(define notes/tree/body
  (lambda
    (node)
    (if (null? node) () (dict-get node "body")))
  "The content dict of a parsed tree node.")

(define notes/tree/butlast
  (lambda
    (xs)
    (if
      (null? xs)
      (list)
      (if
        (null? (cdr xs))
        (list)
        (cons (car xs) (notes/tree/butlast (cdr xs))))))
  "All but the last element of a list.")

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
      names))
  "Names whose immediate parent equals parent.")

(define notes/tree/clear-headings
  (lambda
    (note-path)
    (list/foldl
      (lambda
        (acc nj)
        (let
          ((n (notes/tree/parse nj)))
          (if
            (null? n)
            acc
            (if
              (string-eq (as-str (dict-get n "kind")) "heading")
              (seq
                (tree/delete (as-str (dict-get n "path")) "recursive")
                acc)
              acc))))
      0
      (tree/list note-path 1)))
  "Delete all heading nodes under a note path.")

(define notes/tree/clear-stale-headings
  (lambda
    (note-path keep)
    (list/foldl
      (lambda
        (acc nj)
        (let
          ((n (notes/tree/parse nj)))
          (if
            (null? n)
            acc
            (let
              ((p (as-str (dict-get n "path"))))
              (if
                (and
                  (string-eq (as-str (dict-get n "kind")) "heading")
                  (not (notes/doc/folder-seen? keep p)))
                (seq (tree/delete p "recursive") acc)
                acc)))))
      0
      (tree/subtree note-path)))
  "Ghost sweep: delete heading nodes under note-path whose path is not in keep. tree/delete on an already-deleted path is a no-op, so ancestor-first order is safe.")

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
          (list "margin" ".25rem 0"))
        (css/raw
          "/* refile modal (slug picker — no O(n) options) */")
        (css/rule
          "details.ol-refile-modal"
          (list "display" "inline-block" "position" "relative"))
        (css/rule
          "details.ol-refile-modal > summary.ol-btn"
          (list "cursor" "pointer" "list-style" "none"))
        (css/rule
          "details.ol-refile-modal > summary.ol-btn::-webkit-details-marker"
          (list "display" "none"))
        (css/rule
          "details.ol-refile-modal[open] > .ol-refile-body"
          (list
            "position"
            "absolute"
            "z-index"
            "20"
            "background"
            "var(--card,#fff)"
            "border"
            "1px solid var(--muted,#888)"
            "padding"
            ".5rem"
            "min-width"
            "16rem"
            "box-shadow"
            "0 2px 8px rgba(0,0,0,.12)"))
        (css/rule
          ".ol-refile-body .ol-refile-sel"
          (list "width" "12rem" "max-width" "100%"))
        (css/rule
          ".ol-refile-body .ol-refile-label"
          (list "font-size" ".85rem" "margin-right" ".25rem"))
        (css/raw "/* find / search modal */")
        (css/rule
          "details.ol-find-modal"
          (list "display" "inline-block" "position" "relative"))
        (css/rule
          "details.ol-find-modal > summary.ol-btn"
          (list "cursor" "pointer" "list-style" "none"))
        (css/rule
          "details.ol-find-modal > summary.ol-btn::-webkit-details-marker"
          (list "display" "none"))
        (css/rule
          "details.ol-find-modal[open] > .ol-find-body,\n           details.ol-refile-modal[open] > .ol-refile-body.ol-find-wide"
          (list
            "position"
            "absolute"
            "z-index"
            "30"
            "right"
            "0"
            "background"
            "var(--card,#fff)"
            "border"
            "1px solid var(--muted,#888)"
            "padding"
            ".5rem"
            "min-width"
            "22rem"
            "max-width"
            "min(36rem, 92vw)"
            "box-shadow"
            "0 4px 16px rgba(0,0,0,.14)"))
        (css/rule
          ".ol-find-input"
          (list
            "width"
            "100%"
            "box-sizing"
            "border-box"
            "padding"
            ".35rem .5rem"
            "margin"
            "0 0 .35rem 0"))
        (css/rule
          ".ol-find-results"
          (list
            "max-height"
            "18rem"
            "overflow"
            "auto"
            "margin"
            "0"
            "padding"
            "0"
            "list-style"
            "none"))
        (css/rule
          ".ol-find-group"
          (list
            "font-size"
            ".75rem"
            "font-weight"
            "600"
            "text-transform"
            "uppercase"
            "letter-spacing"
            ".04em"
            "color"
            "var(--muted,#666)"
            "padding"
            ".4rem .35rem .15rem"))
        (css/rule
          "a.ol-find-hit, button.ol-find-hit"
          (list
            "display"
            "block"
            "width"
            "100%"
            "text-align"
            "left"
            "border"
            "0"
            "background"
            "transparent"
            "padding"
            ".35rem .5rem .35rem 1rem"
            "cursor"
            "pointer"
            "color"
            "inherit"
            "text-decoration"
            "none"
            "font"
            "inherit"
            "box-sizing"
            "border-box"))
        (css/rule
          "a.ol-find-hit:hover, button.ol-find-hit:hover,\n           a.ol-find-hit.is-active, button.ol-find-hit.is-active"
          (list "background" "var(--accent-bg,#eef3ff)"))
        (css/rule
          ".ol-find-hit-path"
          (list "font-size" ".85rem"))
        (css/rule
          ".ol-find-hit-meta"
          (list
            "font-size"
            ".75rem"
            "color"
            "var(--muted,#666)"
            "margin-left"
            ".35rem"))
        (css/rule
          ".ol-find-empty"
          (list
            "padding"
            ".5rem"
            "color"
            "var(--muted,#666)"
            "font-size"
            ".9rem"))
        (css/rule
          ".ol-find-bar"
          (list
            "display"
            "flex"
            "gap"
            ".5rem"
            "align-items"
            "center"
            "margin"
            "0 0 .75rem 0"))
        (css/rule
          ".ol-find-bar details.ol-find-modal"
          (list "flex" "1 1 auto"))
        (css/rule
          ".ol-find-bar .ol-find-body"
          (list "left" "0" "right" "auto"))
        (css/raw "/* create form collapsed behind summary */")
        (css/rule
          "details.notes-create > summary.notes-create-sum"
          (list
            "cursor"
            "pointer"
            "font-weight"
            "600"
            "list-style"
            "none"))
        (css/rule
          "details.notes-create > summary.notes-create-sum::-webkit-details-marker"
          (list "display" "none"))
        (css/rule
          "details.notes-create > .notes-create-body"
          (list "margin-top" ".5rem")))))
  "CSS stylesheet for the drawer-style folder tree.")

(define notes/tree/dots
  (lambda
    (s i acc)
    (let
      ((pos (string-find-pos s "." i 1)))
      (if
        (< pos 0)
        acc
        (notes/tree/dots s (+ pos 1) (+ acc 1)))))
  "count dots in s from index i (tail-recursive)")

(define notes/tree/enabled?
  (lambda () #t)
  "Whether the one-tree storage path is enabled.")

(define notes/tree/ensure-chain
  (lambda
    (base segs)
    (if
      (null? segs)
      base
      (let
        ((next
          (string-append base "." (tree/path-label (car segs)))))
        (seq
          (notes/tree/ensure-folder next (car segs))
          (notes/tree/ensure-chain next (cdr segs))))))
  "Ensure the folder chain for base plus segs exists.")

(define notes/tree/ensure-folder
  (lambda
    (path title)
    (let
      ((cur (tree/get path)))
      (if
        (null? cur)
        (tree/upsert
          path
          "folder"
          "note"
          (dict-set*
            "{}"
            (list "uuid" (random-uuid) "title" (as-str title))))
        path)))
  "Create a folder node at path if missing.")

(define notes/tree/find-slug
  (lambda
    (slug)
    (let
      ((hits (tree/find-by "note" "slug" (as-str slug) 5)))
      (if
        (null? hits)
        ()
        (notes/tree/node->row (car hits) #t))))
  "Tree row for a note slug, or () if absent.")

(define notes/tree/flat-step
  (lambda
    (rest stack n)
    (let
      ((b (notes/tree/body n)))
      (let
        ((p (as-str (dict-get n "path"))))
        (let
          ((d (notes/tree/dots p 0 0)))
          (let
            ((node
              (outline/node*
                (as-str (dict-get b "id"))
                (as-str (dict-get b "title"))
                (as-str (dict-get b "body"))
                (as-str (dict-get b "todo"))
                (list))))
            (notes/tree/of-flat
              rest
              (notes/tree/stack-pop stack d node)))))))
  "process one parsed heading node: build outline node, attach at its dot-depth, continue")

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
          (- (string-length path) (+ pos 1))))))
  "Last dot-separated label of a folder path.")

(define notes/tree/folder-of-path
  (lambda
    (path)
    (let
      ((root (notes/tree/root)) (p (as-str path)))
      (if
        (string-starts-with? p (string-append root "."))
        (let
          ((rest
            (substring
              p
              (+ (string-length root) 1)
              (- (string-length p) (+ (string-length root) 1)))))
          (let
            ((parts (string-split rest ".")))
            (notes/tree/join-slash (notes/tree/butlast parts))))
        "")))
  "Folder name of a tree path (empty for the root).")

(define notes/tree/full-notes
  (lambda
    ()
    (tree/note-index-full (notes/tree/root) 50000))
  "All notes with REAL outlines (full index rows; scrape source).\n   The plain tree/note-index rows hardcode outline [] to keep the /notes\n   UI light — scrapes must read the full variant.")

(define notes/tree/heading-kids
  (lambda
    (parent-path)
    (let
      ((flat (tree/list parent-path)))
      (let
        ((stack
          (notes/tree/of-flat
            flat
            (list (cons -1 (outline/node* "root" "" "" "" (list)))))))
        (outline/kids (cdr (car stack))))))
  "Rebuild the outline under one note path from ONE (tree/list path)
     subtree snapshot, folded flat in memory by notes/tree/of-flat
     (subtrees are lexicographically contiguous in path-ASC order).
     OBITUARY (borge/notes.outline-read-path, 2026-12): this binding was
     the recursive per-node walk — one tree/list query PER HEADING NODE,
     155 queries for the 156-node log/issues register, minutes on a
     degraded DB (the 2026-09-14 promote-gate floor). The snapshot fold
     (the old heading-kids-snap, job-only because the VStr-JSON boundary
     tax burned ~380k ticks) is now the LIVE read path: with
     json-values-alist the fold fits the route budget. The old recursive
     body is deleted, not kept as a fallback.")

(define notes/tree/join-slash
  (lambda
    (xs)
    (if
      (null? xs)
      ""
      (if
        (null? (cdr xs))
        (as-str (car xs))
        (string-append
          (as-str (car xs))
          "/"
          (notes/tree/join-slash (cdr xs))))))
  "Join a list of strings with a slash.")

(define notes/tree/list-notes
  (lambda () (tree/note-index (notes/tree/root) 50000))
  "All notes in the garden via the tree index.")

(define notes/tree/load-outline
  (lambda
    (note-path title)
    (outline/node*
      "root"
      (as-str title)
      (notes/tree/root-body note-path)
      ""
      (notes/tree/heading-kids note-path)))
  "Reconstruct an outline for a note path from its stored headings.")

(define notes/tree/node->row
  (lambda
    (nj load-outline?)
    (let
      ((n (notes/tree/parse nj)))
      (if
        (null? n)
        ()
        (if
          (not (string-eq (as-str (dict-get n "kind")) "note"))
          ()
          (let
            ((b (notes/tree/body n))
              (path (as-str (dict-get n "path"))))
            (let
              ((u (dict-get b "uuid"))
                (title (as-str (dict-get b "title"))))
              (let
                ((uuid
                  (if (null? u) (as-str (dict-get n "id")) (as-str u))))
                (let
                  ((fields
                    (list
                      "slug"
                      (as-str (dict-get b "slug"))
                      "title"
                      title
                      "tags"
                      (as-str (dict-get b "tags"))
                      "kind"
                      (as-str (dict-get b "kind"))
                      "day"
                      (as-str (dict-get b "day"))
                      "folder"
                      (notes/tree/folder-of-path path)
                      "_path"
                      path)))
                  (cons
                    uuid
                    (dict-set*
                      "{}"
                      (if
                        load-outline?
                        (append
                          fields
                          (list
                            "outline"
                            (outline/encode (notes/tree/load-outline path title))))
                        (append fields (list "outline" "[]")))))))))))))
  "Convert a parsed tree note node into a row cons (uuid . fields).")

(define notes/tree/note-path
  (lambda
    (folder slug)
    (let
      ((base
        (notes/tree/ensure-chain
          (notes/tree/root)
          (notes/tree/split-folder folder))))
      (string-append base "." (tree/path-label slug))))
  "Tree path for a slug within a folder.")

(define notes/tree/of-flat
  (lambda
    (flat stack)
    (if
      (null? flat)
      stack
      (let
        ((n (notes/tree/parse (car flat))))
        (if
          (null? n)
          (notes/tree/of-flat (cdr flat) stack)
          (if
            (string-eq (as-str (dict-get n "kind")) "heading")
            (notes/tree/flat-step (cdr flat) stack n)
            (notes/tree/of-flat (cdr flat) stack))))))
  "single pass over a path-ASC subtree snapshot; stack keyed by dot-depth (subtrees are lexicographically contiguous in path order)")

(define notes/tree/outline-paths
  (lambda
    (parent-path kids)
    (list/foldl
      (lambda
        (acc node)
        (let
          ((p
            (string-append
              parent-path
              ".h."
              (tree/path-label (outline/id node)))))
          (append
            acc
            (cons
              p
              (notes/tree/outline-paths p (outline/kids node))))))
      (list)
      kids))
  "Flat list of every heading path the outline occupies under parent-path (matches write-heading layout).")

(define notes/tree/parent-of
  (lambda
    (path)
    (let
      ((pos (string-find-pos path "/" 0 1)))
      (if (< pos 0) "" (substring path 0 pos))))
  "Parent folder of a slash path (empty for a root).")

(define notes/tree/parse
  (lambda (s) (if (null? s) () (json-parse (as-str s))))
  "JSON-parse a node string, or () when null.")

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
                roots)))))))
  "HTML flat folder tree of all notes (doc store).")

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
                  (notes/tree/sort-by-name children)))))))))
  "HTML block for a folder and its children.")

(define notes/tree/root
  (lambda () (tree/garden-root))
  "The garden root tree path.")

(define notes/tree/root-body
  (lambda
    (note-path)
    (let
      ((cur (tree/get note-path)))
      (if
        (null? cur)
        ""
        (let
          ((b
            (dict-get
              (notes/tree/body (notes/tree/parse cur))
              "body")))
          (if (null? b) "" (as-str b)))))))

(define notes/tree/save-outline
  (lambda
    (note-path outline)
    (with-transaction
      (lambda
        ()
        (seq
          (notes/tree/write-root-body
            note-path
            (outline/body outline))
          (list/foldl
            (lambda
              (acc kid)
              (seq (notes/tree/write-heading note-path kid) acc))
            0
            (outline/kids outline))
          (notes/tree/clear-stale-headings
            note-path
            (notes/tree/outline-paths note-path (outline/kids outline)))
          note-path))))
  "Persist a note's outline under its tree path.

   WRITE-THEN-SWEEP + ATOMIC (2026-09-27, after a data-loss incident).
   This used to clear-headings FIRST — a recursive delete of every heading —
   and then write the new ones, so any failure in the write half (or in
   write-root-body above it) left the note with its old content DELETED and
   nothing put back. Two things made that fire: tree/upsert rejected a
   structured body (dict-set* over a parsed node returns an alist,
   json-values-alist), and the clear ran before the rejection was known.
   Live cost: the shared log/sessions note lost all headings on a doc upsert.

   Both required guarantees of notes.storage SAVE ORDERING CONTRACT:
     1. ORDER — new headings are upserted first (idempotent, keyed by path
        and id), then headings NOT in the new outline are swept. A failure
        mid-write leaves the previous outline in place PLUS whatever was
        written — degraded, never empty.
     2. ATOMICITY — the whole sequence runs in ONE with-transaction, so a
        raise anywhere rolls the save back entirely and the old outline is
        intact. /api/eval does NOT wrap a request in a transaction
        (verified live 2026-09-27), so this wrap is what makes the save
        all-or-nothing. Nested with-transaction JOINS
        (store_db.with_dynamic_transaction rides an active tx instead of
        raising), so callers that already wrapped (notes/refile, dabar)
        stay correct.

   An empty outline still clears the note: the sweep's keep-list is empty,
   so every heading under the note goes.")

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
            (list/filter (lambda (x) (not (string<? x pivot))) rest))))))
  "Sort names lexicographically (quicksort).")

(define notes/tree/split-folder
  (lambda
    (folder)
    (list/filter
      (lambda (s) (not (string-eq (as-str s) "")))
      (string-split (as-str folder) "/")))
  "Split a folder into non-empty segments.")

(define notes/tree/stack-pop
  (lambda
    (stack d node)
    (if
      (null? stack)
      (list (cons d node))
      (if
        (>= (car (car stack)) d)
        (notes/tree/stack-pop (cdr stack) d node)
        (let
          ((top (car stack)))
          (cons
            (cons
              (car top)
              (outline/set-kids
                (cdr top)
                (append (outline/kids (cdr top)) (list node))))
            (cdr stack))))))
  "pop completed subtrees (dot-depth >= d), append node to the new top's kids, push (depth . node). Popped entries are already linked into their parents - discarding them is correct.")

(define notes/tree/style
  (lambda () (html/tag "style" "" (notes/tree/style-css))))

(define notes/tree/style-css
  (lambda
    ()
    (string-append
      (css/render notes/tree-css)
      (css/render notes/tree-css-patch)
      (css/render (notes/tree/details-css)))))

(define notes/tree/write-heading
  (lambda
    (parent-path node)
    (let
      ((nid (tree/path-label (outline/id node)))
        (path
          (string-append
            parent-path
            ".h."
            (tree/path-label (outline/id node)))))
      (seq
        (tree/upsert
          path
          "heading"
          "note"
          (dict-set*
            "{}"
            (list
              "id"
              (as-str (outline/id node))
              "title"
              (as-str (outline/title node))
              "body"
              (as-str (outline/body node))
              "todo"
              (as-str (outline/todo node)))))
        (list/foldl
          (lambda
            (acc kid)
            (seq (notes/tree/write-heading path kid) acc))
          0
          (outline/kids node))
        path)))
  "Persist an outline node and its children under a parent path.")

(define notes/tree/write-root-body
  (lambda
    (note-path body)
    (let
      ((cur (tree/get note-path)))
      (if
        (null? cur)
        note-path
        (let*
          ((n (notes/tree/parse cur)) (b (notes/tree/body n)))
          (seq
            (tree/upsert
              note-path
              "note"
              "note"
              (dict-set b "body" (as-str body)))
            note-path))))))

(define notes/ui/create-form
  (lambda
    ()
    (html/tag
      "details"
      (html/attr "class" "notes-create card")
      (html/join
        (list
          (html/tag
            "summary"
            (html/attr "class" "notes-create-sum")
            "New note")
          (html/form
            "POST"
            "/notes"
            (html/attr "class" "notes-create-body")
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
                (html/button (html/attr "type" "submit") "Create"))))))))
  "Collapsible HTML form to create a new note.")

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
        (redirect (string-append "/notes?slug=" s)))))
  "Create a note with an auto or given slug and folder, then redirect.")

(define notes/ui/do-delete
  (lambda
    ()
    (let
      ((slug (as-str (request-param "slug"))))
      (seq (notes/doc/delete slug) (redirect "/notes"))))
  "Delete the note named by slug, then redirect.")

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
                    (as-str raw))))))))))
  "Save a note outline, validating JSON, then redirect or show errors.")

(define notes/ui/do-undo
  (lambda
    ()
    (let
      ((slug (as-str (request-param "slug")))
        (n (notes/doc/get (as-str (request-param "slug")))))
      (let
        ((folder (notes/doc/folder n))
          (path (notes/tree/note-path folder slug)))
        (seq
          (undo path)
          (redirect (string-append "/notes?slug=" slug)))))))

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
            (html/button (html/attr "type" "submit") "Save outline"))))))
  "HTML textarea form to save a note's outline.")

(define notes/ui/find-modal
  (lambda
    ()
    (html/tag
      "details"
      (html/attrs
        (list
          (html/attr "class" "ol-find-modal")
          (html/attr "id" "ol-find-modal")))
      (html/join
        (list
          (html/tag
            "summary"
            (html/attrs
              (list
                (html/attr "class" "ol-btn")
                (html/attr "title" "Find note (Ctrl/Cmd-K)")))
            "find")
          (html/tag
            "div"
            (html/attr "class" "ol-find-body")
            (html/join
              (list
                (html/void
                  "input"
                  (html/attrs
                    (list
                      (html/attr "type" "search")
                      (html/attr "class" "ol-find-input")
                      (html/attr "name" "q")
                      (html/attr "placeholder" "Find note or header…")
                      (html/attr "autocomplete" "off")
                      (html/attr "hx-get" "/partials/notes-search")
                      (html/attr
                        "hx-trigger"
                        "input changed delay:200ms, search")
                      (html/attr "hx-target" "next .ol-find-results")
                      (html/attr "hx-swap" "outerHTML")
                      (html/attr "hx-include" "this"))))
                (html/div
                  (html/attr "class" "ol-find-results")
                  (html/div
                    (html/attr "class" "ol-find-empty")
                    "Type to find a note or header…"))))))))))

(define notes/ui/find-script
  (lambda
    ()
    (html/tag
      "script"
      ""
      (string-append
        "(function(){"
        "function hits(root){return root?Array.prototype.slice.call(root.querySelectorAll('[data-ol-find-hit]')):[];}"
        "function setActive(root,i){var hs=hits(root);if(!hs.length)return;hs.forEach(function(h){h.classList.remove('is-active');});"
        "var n=((i%hs.length)+hs.length)%hs.length;hs[n].classList.add('is-active');hs[n].scrollIntoView({block:'nearest'});root.dataset.olFindIdx=String(n);}"
        "function openFind(sel){var d=document.querySelector(sel||'details.ol-find-modal');if(!d)return;d.open=true;"
        "var inp=d.querySelector('.ol-find-input');if(inp){inp.focus();inp.select();}}"
        "document.addEventListener('keydown',function(e){"
        "var t=e.target;var typing=t&&(t.tagName==='INPUT'||t.tagName==='TEXTAREA'||t.isContentEditable);"
        "if((e.metaKey||e.ctrlKey)&&!e.shiftKey&&(e.key==='k'||e.key==='K')){e.preventDefault();openFind('details.ol-find-modal');return;}"
        "if(e.key==='Escape'){document.querySelectorAll('details.ol-find-modal[open],details.ol-refile-modal[open]').forEach(function(d){d.open=false;});return;}"
        "var body=t&&t.closest&&t.closest('.ol-find-body,.ol-refile-body');if(!body)return;"
        "var box=body.querySelector('.ol-find-results')||body;"
        "if(e.key==='ArrowDown'){e.preventDefault();setActive(box,(parseInt(box.dataset.olFindIdx||'-1',10)+1));return;}"
        "if(e.key==='ArrowUp'){e.preventDefault();setActive(box,(parseInt(box.dataset.olFindIdx||'0',10)-1));return;}"
        "if(e.key==='Enter'&&!e.shiftKey){var hs=hits(box);var i=parseInt(box.dataset.olFindIdx||'-1',10);"
        "if(i>=0&&hs[i]){e.preventDefault();if(hs[i].tagName==='BUTTON'){hs[i].click();}else if(hs[i].tagName==='A'){window.location=hs[i].href;}else{var b=hs[i].querySelector('button,[type=submit]');if(b)b.click();else hs[i].click();}}}"
        "});"
        "document.body.addEventListener('htmx:afterSwap',function(ev){var t=ev.target;if(!t||!t.classList)return;"
        "if(t.classList.contains('ol-find-results')||t.querySelector&&t.querySelector('.ol-find-results')){var box=t.classList.contains('ol-find-results')?t:t.querySelector('.ol-find-results');if(box)setActive(box,0);}});"
        "document.addEventListener('toggle',function(e){var d=e.target;if(!d||!d.classList)return;"
        "if((d.classList.contains('ol-find-modal')||d.classList.contains('ol-refile-modal'))&&d.open){var inp=d.querySelector('.ol-find-input');if(inp)setTimeout(function(){inp.focus();},0);}}"
        ",true);"
        "document.addEventListener('pointerdown',function(e){var rf=e.target.closest&&e.target.closest('.ol-find-refile-form');"
        "if(!rf)return;var src=document.getElementById('ol-refile-nid');var m=rf.querySelector('.ol-refile-nid-mirror');"
        "if(src&&m)m.value=src.value;},true);"
        "})();"))))

(define notes/ui/folder-block
  (lambda
    (folder notes)
    (let
      ((items (notes/light/notes-in folder notes))
        (label
          (if (string-eq folder "") "(uncategorized)" folder)))
      (if
        (null? items)
        ""
        (html/tag
          "details"
          (html/attrs
            (list
              (html/attr
                "class"
                (if
                  (string-eq folder "")
                  "tree-folder tree-uncategorized"
                  "tree-folder"))
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
                      (if (string-eq folder "") "&#x1F4C2; " "&#x1F4C1; "))
                    (html/a
                      (string-append "/notes?folder=" folder)
                      (html/text label))
                    (html/span
                      (html/attr "class" "muted")
                      (html/text
                        (string-append " (" (as-str (length items)) ")"))))))
              (notes/light/render-notes items))))))))

(define notes/ui/folder-block-grouped
  (lambda
    (pair)
    (let
      ((folder (as-str (car pair))) (items (car (cdr pair))))
      (let
        ((label
            (if (string-eq folder "") "(uncategorized)" folder))
          (n (length items)))
        (html/tag
          "details"
          (html/attrs
            (list
              (html/attr
                "class"
                (if
                  (string-eq folder "")
                  "tree-folder tree-uncategorized"
                  "tree-folder"))
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
                      (if (string-eq folder "") "&#x1F4C2; " "&#x1F4C1; "))
                    (html/text label)
                    (html/span
                      (html/attr "class" "muted")
                      (html/text (string-append " (" (as-str n) ")"))))))
              (notes/light/render-notes items))))))))

(define notes/ui/folder-details
  (lambda
    (n)
    (let
      ((path (notes/ui/node-path n))
        (label (notes/ui/node-title n)))
      (html/tag
        "details"
        (html/attrs
          (list
            (html/attr "class" "tree-folder")
            (html/attr
              "hx-get"
              (string-append "/partials/notes-tree?path=" path))
            (html/attr "hx-trigger" "toggle once")
            (html/attr "hx-target" "find .tree-children")
            (html/attr "hx-swap" "innerHTML")))
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
                  (html/text label))))
            (html/div
              (html/attr "class" "tree-children")
              (html/span
                (html/attr "class" "muted")
                "open to load…"))))))))

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
          (html/button (html/attr "type" "submit") "move")))))
  "HTML form to move a note into a folder.")

(define notes/ui/folder-group-keys
  (lambda
    (notes)
    (notes/light/dedupe
      (map (lambda (n) (as-str (notes/light/nth n 1))) notes))))

(define notes/ui/folder-list
  (lambda
    (folder)
    (list/foldl
      (lambda
        (acc row)
        (string-append acc (notes/ui/item-li row)))
      ""
      (notes/doc/list-folder folder)))
  "Concatenated HTML of list items for a folder.")

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
            (html/ul "" (notes/ui/folder-list folder)))))))
  "HTML card section for a folder's notes.")

(define notes/ui/folders-html
  (lambda
    ()
    (let
      ((root (notes/tree/root)))
      (html/div
        (html/attr "class" "notes-tree")
        (notes/ui/tree-children-html root root)))))

(define notes/ui/group-add
  (lambda
    (acc k slug title)
    (if
      (null? acc)
      (list (list k (list (list slug title))))
      (if
        (string-eq (as-str (car (car acc))) k)
        (cons
          (list k (cons (list slug title) (car (cdr (car acc)))))
          (cdr acc))
        (cons
          (car acc)
          (notes/ui/group-add (cdr acc) k slug title))))))

(define notes/ui/group-notes
  (lambda
    ()
    (list/foldl
      (lambda
        (acc r)
        (let
          ((d (cdr r)))
          (notes/ui/group-add
            acc
            (notes/ui/top-folder (dict-get d "folder"))
            (as-str (dict-get d "slug"))
            (as-str (dict-get d "title")))))
      (list)
      (notes/doc/index-list))))

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
        ((string-eq action "undo") (notes/ui/do-undo))
        ((string-eq action "capture") (notes/capture/handle))
        ((string-eq action "delete") (notes/ui/do-delete))
        ((string-eq action "refile") (notes/refile/handle))
        ((string-eq action "extract")
          (notes/refile/extract-handle))
        (else (redirect "/notes")))))
  "Dispatch a notes POST by action to the matching handler.")

(define notes/ui/index
  (lambda
    ()
    (html/join
      (list
        (notes/tree/style)
        (html/div
          (html/attr "class" "ol-find-bar")
          (html/join
            (list
              (notes/ui/find-modal)
              (html/span (html/attr "class" "muted") "Ctrl/Cmd-K"))))
        (html/h1 "Notes")
        (html/p*
          (html/attr "class" "muted")
          "Outline-first notes. Folders load on open (htmx).")
        (html/p*
          (html/attr "class" "muted")
          (html/join
            (list
              (html/a "/notes?slug=inbox" "Open inbox")
              " · "
              (html/a "/issues" "Issues")
              " · "
              (html/a "/notes" "All folders"))))
        (notes/ui/create-form)
        (html/h2 "Folders")
        (notes/ui/folders-html)
        (notes/ui/find-script)))))

(define notes/ui/index-group
  (lambda
    (notes)
    (list/foldl
      (lambda
        (acc n)
        (let
          ((k (as-str (notes/light/nth n 1))))
          (notes/ui/index-group-add acc k n)))
      (list)
      notes)))

(define notes/ui/index-group-add
  (lambda
    (acc k n)
    (if
      (null? acc)
      (list (list k (list n)))
      (if
        (string-eq (as-str (car (car acc))) k)
        (cons (list k (cons n (car (cdr (car acc))))) (cdr acc))
        (cons
          (car acc)
          (notes/ui/index-group-add (cdr acc) k n))))))

(define notes/ui/is-garden-folder?
  (lambda
    (n root)
    (if
      (not (string-eq (notes/ui/node-kind n) "folder"))
      #f
      (string-eq
        (notes/ui/node-path n)
        (string-append (as-str root) ".garden")))))

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
                  (html/text (string-append " · " folder))))
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
                      "del"))))))))))
  "HTML list item for a note row with folder and delete.")

(define notes/ui/item-li-lite
  (lambda
    (row)
    (let
      ((f (cdr row)))
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
                  (html/text (string-append " · " folder))))))))))
  "HTML list item for a note row (title and folder only).")

(define notes/ui/missing
  (lambda
    (slug)
    (html/join
      (list
        (html/h1 "Missing")
        (html/p*
          (html/attr "class" "err")
          (string-append "No note " (as-str slug)))
        (html/p* "" (html/a "/notes" "All notes")))))
  "HTML 'no note' page for a missing slug.")

(define notes/ui/node-kind
  (lambda (n) (as-str (dict-get n "kind"))))

(define notes/ui/node-path
  (lambda (n) (as-str (dict-get n "path"))))

(define notes/ui/node-title
  (lambda
    (n)
    (let
      ((b (dict-get n "body")))
      (let
        ((t (dict-get b "title")) (s (dict-get b "slug")))
        (if
          (if (null? t) #t (string-eq (as-str t) ""))
          (if
            (if (null? s) #t (string-eq (as-str s) ""))
            (notes/ui/path-label (notes/ui/node-path n))
            (as-str s))
          (as-str t))))))

(define notes/ui/note-li-from-node
  (lambda
    (n)
    (let
      ((slug (as-str (dict-get (dict-get n "body") "slug")))
        (title (notes/ui/node-title n)))
      (let
        ((s
          (if
            (string-eq slug "")
            (notes/ui/path-label (notes/ui/node-path n))
            slug)))
        (html/li
          (html/attr "class" "tree-item")
          (html/a
            (string-append "/notes?slug=" s)
            (html/text title)))))))

(define notes/ui/outline-script
  (lambda
    (slug show-archived tree)
    (html/tag
      "script"
      ""
      (string-append
        "(function(){var slug='"
        (as-str slug)
        "';var showArch="
        (if show-archived "true" "false")
        ";var ppTree="
        (json-emit tree)
        ";window.__ppTree=ppTree;"
        "var __ppReady=false,__ppBooting=false,__opSeq=0;"
        "var st=document.createElement('style');st.textContent='.pp-in-transit{opacity:.5;filter:grayscale(1);}';document.head.appendChild(st);"
        "function ppBoot(){if(__ppReady||__ppBooting)return;__ppBooting=true;"
        "var s=document.createElement('script');s.src='/assets/interp.js';"
        "s.onload=function(){var pp=window.pp;pp.boot();"
        "var pkgs=['html','structed','structed/list','outline','bible'];var done=0;"
        "pkgs.forEach(function(p){fetch('/code/raw/libs/'+p+'/lib.pp',{credentials:'same-origin'}).then(function(r){return r.text();}).then(function(src){pp.load_source(src);done++;if(done===pkgs.length){fetch('/code/raw/client/ops.pp',{credentials:'same-origin'}).then(function(r){return r.text();}).then(function(os){pp.load_source(os);pp.set_tree(JSON.stringify(ppTree));__ppReady=true;__ppBooting=false;});}});});};"
        "document.head.appendChild(s);}"
        "ppBoot();"
        "function selId(){var el=document.getElementById('ol-selected-id');return el?el.value:'root';}"
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
        "if(!__ppReady)ppBoot();"
        "var seq=++__opSeq;var keep=id;__selPin=keep;"
        "var snap=document.getElementById('outline')?document.getElementById('outline').outerHTML:null;"
        "if(__ppReady){try{var prev=window.pp.apply_op(op,id,slug);"
        "if(prev&&prev.length){var t=document.getElementById('outline');if(t){var tmp=document.createElement('div');tmp.innerHTML=prev;var neo=tmp.firstChild;if(neo){neo.classList.add('pp-in-transit');t.replaceWith(neo);}}}"
        "}catch(e){console.warn('pp optimistic failed',e);}}"
        "var q='slug='+encodeURIComponent(slug)+'&op='+encodeURIComponent(op)+'&node-id='+encodeURIComponent(id)+'&op-seq='+seq;"
        "if(showArch)q+='&show=archived';"
        "fetch('/partials/notes-node-op?'+q,{method:'POST',credentials:'same-origin'}).then(function(r){return r.text();}).then(function(html){"
        "if(seq<__opSeq)return;"
        "var doc=new DOMParser().parseFromString(html,'text/html');"
        "var oobSel=doc.querySelector('#ol-selected-id[hx-swap-oob]');"
        "var olHTML=doc.querySelector('#outline');"
        "var cur=document.getElementById('outline');"
        "if(olHTML&&cur){if(window.Idiomorph){Idiomorph.morph(cur,olHTML);}else{cur.outerHTML=olHTML.outerHTML;}}"
        "var selIdVal=oobSel?oobSel.getAttribute('value'):null;"
        "if(selIdVal){var hid=document.getElementById('ol-selected-id');if(hid)hid.value=selIdVal;}"
        "var target=selIdVal||keep;"
        "if(findNode(target))setSel(target);else if(findNode(keep))setSel(keep);else{__selPin=null;setSel('root');}"
        "if(window.htmx){try{htmx.process(document.getElementById('outline'));}catch(e){}}"
        "}).catch(function(e){"
        "if(snap){var tmp=document.createElement('div');tmp.innerHTML=snap;var c=document.getElementById('outline');if(c)c.replaceWith(tmp.firstChild);}"
        "console.error('notes postOp',e);});}"
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
        "setSel('root');document.body.addEventListener('htmx:afterSwap',function(){setSel(selId());});})();")))
  "Inline JS driving the interactive outline editor.")

(define notes/ui/path-label
  (lambda
    (path)
    (let
      ((parts (string-split (as-str path) ".")))
      (if
        (null? parts)
        ""
        (string-replace (notes/light/last parts) "_" "-")))))

(define notes/ui/recent-list
  (lambda
    ()
    (list/foldl
      (lambda
        (acc row)
        (string-append acc (notes/ui/item-li row)))
      ""
      (notes/doc/index-list)))
  "Concatenated HTML of recent note list items.")

(define notes/ui/refile-modal
  (lambda
    (slug)
    (html/tag
      "details"
      (html/attr "class" "ol-refile-modal")
      (html/join
        (list
          (html/tag
            "summary"
            (html/attrs
              (list
                (html/attr "class" "ol-btn")
                (html/attr
                  "title"
                  "refile selected node to another note")))
            "refile")
          (notes/ui/refile-modal-body slug))))))

(define notes/ui/refile-modal-body
  (lambda
    (slug)
    (html/tag
      "div"
      (html/attrs
        (list (html/attr "class" "ol-refile-body ol-find-wide")))
      (html/join
        (list
          (html/void
            "input"
            (html/attrs
              (list
                (html/attr "type" "hidden")
                (html/attr "id" "ol-refile-nid")
                (html/attr "value" "root"))))
          (html/void
            "input"
            (html/attrs
              (list
                (html/attr "type" "search")
                (html/attr "class" "ol-find-input")
                (html/attr "name" "q")
                (html/attr
                  "placeholder"
                  "destination note or header…")
                (html/attr "autocomplete" "off")
                (html/attr
                  "hx-get"
                  (string-append
                    "/partials/notes-search?mode=refile&src="
                    (as-str slug)))
                (html/attr
                  "hx-trigger"
                  "input changed delay:200ms, search")
                (html/attr "hx-target" "next .ol-find-results")
                (html/attr "hx-swap" "outerHTML")
                (html/attr "hx-include" "this"))))
          (html/div
            (html/attr "class" "ol-find-results")
            (html/div
              (html/attr "class" "ol-find-empty")
              "Type a destination note or header…"))
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
                      (html/attr "class" "ol-refile-nid-mirror")
                      (html/attr "value" "root"))))
                (html/label
                  (html/attr "class" "ol-refile-label")
                  "or slug ")
                (html/text-input
                  "dst_slug"
                  (html/attrs
                    (list
                      (html/attr "class" "ol-refile-sel")
                      (html/attr "placeholder" "destination-slug")
                      (html/attr "autocomplete" "off"))))
                " "
                (html/button
                  (html/attrs
                    (list
                      (html/attr "type" "submit")
                      (html/attr "class" "ol-btn")
                      (html/attr "title" "refile selected node")))
                  "go")))))))))

(define notes/ui/refile-partial
  (lambda
    ()
    (let
      ((slug (as-str (request-param "src"))))
      (if
        (string-eq slug "")
        (render-html
          (html/p* (html/attr "class" "muted") "missing src"))
        (render-html
          (html/tag
            "details"
            (html/attrs
              (list
                (html/attr "class" "ol-refile-modal")
                (html/attr "open" "")))
            (html/join
              (list
                (html/tag
                  "summary"
                  (html/attrs
                    (list
                      (html/attr "class" "ol-btn")
                      (html/attr
                        "title"
                        "refile selected node to another note")))
                  "refile")
                (notes/ui/refile-modal-body slug)))))))))

(define notes/ui/render-child
  (lambda
    (n root)
    (let
      ((k (notes/ui/node-kind n)))
      (if
        (string-eq k "heading")
        ""
        (if
          (notes/ui/is-garden-folder? n root)
          (notes/ui/tree-children-html
            (notes/ui/node-path n)
            root)
          (if
            (string-eq k "folder")
            (notes/ui/folder-details n)
            (if
              (string-eq k "note")
              (notes/ui/note-li-from-node n)
              "")))))))

(define notes/ui/search-group
  (lambda
    (hits)
    (list/map
      (lambda (s) (notes/ui/search-hits-for hits s))
      (notes/ui/search-slugs hits))))

(define notes/ui/search-group-html
  (lambda
    (group mode src-slug)
    (if
      (null? group)
      ""
      (let
        ((title (as-str (dict-get (car group) "title")))
          (slug (as-str (dict-get (car group) "slug"))))
        (html/join
          (list
            (html/div
              (html/attr "class" "ol-find-group")
              (html/text (if (string-eq title "") slug title)))
            (html/join
              (list/map
                (lambda
                  (hit)
                  (if
                    (string-eq mode "refile")
                    (notes/ui/search-hit-refile hit src-slug)
                    (notes/ui/search-hit-open hit)))
                group))))))))

(define notes/ui/search-hit-href
  (lambda
    (hit)
    (let
      ((slug (as-str (dict-get hit "slug")))
        (nid (as-str (dict-get hit "node_id"))))
      (if
        (string-eq nid "")
        (string-append "/notes?slug=" slug)
        (string-append "/notes?slug=" slug "&node=" nid)))))

(define notes/ui/search-hit-label
  (lambda
    (hit)
    (let
      ((path (as-str (dict-get hit "header_path")))
        (matched (as-str (dict-get hit "matched")))
        (title (as-str (dict-get hit "title")))
        (nid (as-str (dict-get hit "node_id"))))
      (if
        (string-eq nid "")
        (html/join
          (list
            (html/span
              (html/attr "class" "ol-find-hit-path")
              (html/text
                (if
                  (string-eq title "")
                  (as-str (dict-get hit "slug"))
                  title)))
            (html/span
              (html/attr "class" "ol-find-hit-meta")
              (html/text matched))))
        (html/join
          (list
            (html/span
              (html/attr "class" "ol-find-hit-path")
              (html/text (if (string-eq path "") title path)))
            (html/span
              (html/attr "class" "ol-find-hit-meta")
              (html/text matched))))))))

(define notes/ui/search-hit-open
  (lambda
    (hit)
    (html/tag
      "a"
      (html/attrs
        (list
          (html/attr "href" (notes/ui/search-hit-href hit))
          (html/attr "class" "ol-find-hit")
          (html/attr "data-ol-find-hit" "1")
          (html/attr "data-slug" (as-str (dict-get hit "slug")))
          (html/attr
            "data-node"
            (as-str (dict-get hit "node_id")))))
      (notes/ui/search-hit-label hit))))

(define notes/ui/search-hit-refile
  (lambda
    (hit src-slug)
    (html/form
      "POST"
      "/notes"
      (html/attr "class" "ol-find-refile-form")
      (html/join
        (list
          (html/hidden "action" "refile")
          (html/hidden "src_slug" (as-str src-slug))
          (html/void
            "input"
            (html/attrs
              (list
                (html/attr "type" "hidden")
                (html/attr "name" "node_id")
                (html/attr "class" "ol-refile-nid-mirror")
                (html/attr "value" "root"))))
          (html/hidden "dst_slug" (as-str (dict-get hit "slug")))
          (html/hidden
            "dst_node_id"
            (as-str (dict-get hit "node_id")))
          (html/button
            (html/attrs
              (list
                (html/attr "type" "submit")
                (html/attr "class" "ol-find-hit")
                (html/attr "data-ol-find-hit" "1")
                (html/attr "data-slug" (as-str (dict-get hit "slug")))
                (html/attr
                  "data-node"
                  (as-str (dict-get hit "node_id")))
                (html/attr "title" "refile selected node here")))
            (notes/ui/search-hit-label hit)))))))

(define notes/ui/search-hits-for
  (lambda
    (hits slug)
    (list/filter
      (lambda
        (h)
        (string-eq (as-str (dict-get h "slug")) (as-str slug)))
      hits)))

(define notes/ui/search-partial
  (lambda
    ()
    (let
      ((q
          (if
            (null? (request-param "q"))
            ""
            (as-str (request-param "q"))))
        (mode
          (if
            (null? (request-param "mode"))
            "open"
            (as-str (request-param "mode"))))
        (src
          (if
            (null? (request-param "src"))
            ""
            (as-str (request-param "src")))))
      (render-html (notes/ui/search-results-html q mode src)))))

(define notes/ui/search-results-html
  (lambda
    (q mode src-slug)
    (let
      ((needle (string-trim (as-str q))))
      (if
        (string-eq needle "")
        (html/div
          (html/attr "class" "ol-find-results")
          (html/div
            (html/attr "class" "ol-find-empty")
            (if
              (string-eq mode "refile")
              "Type a destination note or header…"
              "Type to find a note or header…")))
        (let
          ((hits (notes/doc/search needle)))
          (if
            (null? hits)
            (html/div
              (html/attr "class" "ol-find-results")
              (html/div
                (html/attr "class" "ol-find-empty")
                "No matches."))
            (html/div
              (html/attr "class" "ol-find-results")
              (html/join
                (list/map
                  (lambda
                    (g)
                    (notes/ui/search-group-html g mode src-slug))
                  (notes/ui/search-group hits))))))))))

(define notes/ui/search-slug-seen?
  (lambda
    (xs s)
    (if
      (null? xs)
      #f
      (if
        (string-eq (as-str (car xs)) s)
        #t
        (notes/ui/search-slug-seen? (cdr xs) s)))))

(define notes/ui/search-slugs
  (lambda
    (hits)
    (list/foldl
      (lambda
        (acc h)
        (let
          ((s (as-str (dict-get h "slug"))))
          (if
            (notes/ui/search-slug-seen? acc s)
            acc
            (append acc (list s)))))
      (list)
      hits)))

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
          (notes/doc/folder n)))))
  "HTML page for a note slug (doc store).")

(define notes/ui/show-body
  (lambda
    (slug title outline tags show-archived folder)
    (html/join
      (list
        (notes/tree/style)
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
                      "Delete note"))))
              (html/form
                "POST"
                "/notes"
                (html/attrs
                  (list
                    (html/attr "class" "note-undo")
                    (html/attr "title" "undo the last change to this note")))
                (html/join
                  (list
                    (html/hidden "action" "undo")
                    (html/hidden "slug" (as-str slug))
                    (html/button
                      (html/attrs
                        (list
                          (html/attr "type" "submit")
                          (html/attr "class" "secondary")
                          (html/attr "title" "undo last note change")))
                      "Undo")))))))
        (html/p*
          (html/attr "class" "muted")
          (string-append "tags: " (as-str tags)))
        (notes/ui/folder-form slug folder)
        (notes/ui/toolbar slug show-archived)
        (outline/html-lite slug outline show-archived)
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
        (notes/ui/outline-script slug show-archived outline)
        (notes/ui/find-script))))
  "Full HTML body for a note page.")

(define notes/ui/tb-btn
  (lambda
    (op label)
    (html/tag
      "button"
      (html/attrs
        (list
          (html/attr "type" "submit")
          (html/attr "class" "ol-btn")
          (html/attr "title" op)
          (html/attr "data-op" op)
          (html/attr "form" "ol-op-form")))
      label))
  "HTML toolbar button submitting an outline op.")

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
                (html/attr "name" "node-id")
                (html/attr "value" "root"))))
          (html/tag
            "template"
            (html/attr "id" "ol-op-opt")
            (html/span
              (html/attrs
                (list
                  (html/attr "class" "ol-opt-msg")
                  (html/attr "style" "opacity:0.6;font-style:italic")))
              (html/text "updating…")))
          (html/form
            "POST"
            "/partials/notes-node-op"
            (html/attrs
              (list
                (html/attr "id" "ol-op-form")
                (html/attr "class" "ol-op-form")
                (html/attr "hx-post" "/partials/notes-node-op")
                (html/attr "hx-target" "#outline")
                (html/attr "hx-swap" "outerHTML")
                (html/attr "hx-include" "#ol-selected-id")
                (html/attr "hx-optimistic" "#ol-op-opt")))
            (html/join
              (list
                (html/hidden "slug" (as-str slug))
                (if
                  show-archived
                  (html/void
                    "input"
                    (html/attrs
                      (list
                        (html/attr "type" "hidden")
                        (html/attr "name" "show")
                        (html/attr "value" "archived"))))
                  "")
                (html/void
                  "input"
                  (html/attrs
                    (list
                      (html/attr "type" "hidden")
                      (html/attr "name" "op")
                      (html/attr "id" "ol-op"))))
                (notes/ui/tb-btn "cycle-todo" "todo")
                (notes/ui/tb-btn "archive" "arch")
                (notes/ui/tb-btn "unarchive" "unarch")
                (notes/ui/tb-btn "add-child" "+kid")
                (notes/ui/tb-btn "add-sibling" "+sib")
                (notes/ui/tb-btn "promote" "<<")
                (notes/ui/tb-btn "demote" ">>")
                (notes/ui/tb-btn "move-up" "up")
                (notes/ui/tb-btn "move-down" "dn")
                (html/tag
                  "button"
                  (html/attrs
                    (list
                      (html/attr "type" "submit")
                      (html/attr "class" "ol-btn ol-del")
                      (html/attr "title" "delete")
                      (html/attr "data-op" "delete")
                      (html/attr "form" "ol-op-form")
                      (html/attr "hx-confirm" "Delete this node?")))
                  "del"))))
          (notes/ui/find-modal)
          (notes/ui/refile-modal slug)
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
              "Show archived"))))))
  "HTML toolbar of outline ops for the selected node.")

(define notes/ui/top-folder
  (lambda
    (folder)
    (let
      ((f (as-str folder)))
      (let
        ((s
          (if
            (string-starts-with? f "garden/")
            (string-replace f "garden/" "")
            (if (string-eq f "garden") "" f))))
        (let
          ((k (string-replace s "_" "-")))
          (if (string-eq k "") "" (car (string-split k "/"))))))))

(define notes/ui/tree-children-html
  (lambda (path root) (tree/children-html (as-str path))))

(define notes/ui/tree-partial
  (lambda
    ()
    (let
      ((path (as-str (request-param "path")))
        (root (notes/tree/root)))
      (if
        (string-eq path "")
        (render-html
          (html/p* (html/attr "class" "muted") "missing path"))
        (render-html
          (html/div
            (html/attr "class" "tree-children-inner")
            (notes/ui/tree-children-html path root))))))
  "HTML partial of a folder's tree children.")

(define notes/upsert
  (lambda
    (slug title outline tags kind day)
    (notes/doc/upsert
      slug
      title
      outline
      tags
      kind
      day))
  "Upsert a note by slug. Delegates to notes/doc/upsert (tree path); body defaults to empty, folder preserved on update. Returns slug. Fixes i-1786914877.")
