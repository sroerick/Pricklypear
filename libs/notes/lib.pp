;; Hierarchical names (optional depth under notes/):
;;   notes/doc/*      note rows + field accessors + index filters
;;   notes/capture/*  quick capture + inbox append
;;   notes/outline/*  node ops / partials / outline todos
;;   notes/refile/*   move nodes between notes
;;   notes/ui/*       HTML + POST handlers
;; Public routes stay shallow where useful: notes/page;
;;   notes/capture/page, notes/outline/partial, notes/outline/node-op.
;; Outline-first notes. Depends on outline/ + html/*.
;; (load-library "outline") then (load-library "notes")

(deftype 'note 'title 'slug 'outline 'tags 'kind 'day 'body)

(define notes/doc/title (lambda (n) (car (cdr (cdr n)))))
(define notes/doc/outline (lambda (n) (car (cdr (cdr (cdr n))))))
(define notes/doc/tags (lambda (n) (car (cdr (cdr (cdr (cdr n)))))))
(define notes/doc/kind (lambda (n) (car (cdr (cdr (cdr (cdr (cdr n))))))))
(define notes/doc/day (lambda (n) (car (cdr (cdr (cdr (cdr (cdr (cdr n)))))))))
(define notes/doc/empty-outline
  (lambda (title)
    (outline/node "root" title)))

(define notes/doc/row
  (lambda (slug)
    (let ((rs (find-rows 'note "slug" (as-str slug) 1)))
      (if (null? rs) () (car rs)))))

(define notes/doc/get
  (lambda (slug)
    (let ((row (notes/doc/row slug)))
      (if (null? row)
          ()
          (let ((f (cdr row)))
            (list
              (car row)
              (dict-get f "slug")
              (dict-get f "title")
              (outline/decode (dict-get f "outline"))
              (dict-get f "tags")
              (dict-get f "kind")
              (dict-get f "day")))))))

(define notes/doc/make-fields
  (lambda (slug title outline tags kind day)
    (dict-set* "{}"
      (list
        "slug" (as-str slug)
        "title" (as-str title)
        "outline" (outline/encode outline)
        "tags" (as-str tags)
        "kind" (as-str kind)
        "day" (as-str day)))))

(define notes/doc/upsert
  (lambda (slug title outline tags kind day)
    (let ((row (notes/doc/row slug)))
      (let ((fields (notes/doc/make-fields slug title outline tags kind day)))
        (if (null? row)
            (create 'note fields)
            (update 'note (car row) fields))))))
(define notes/doc/list (lambda () (rows 'note 100)))

;; Hide system/hidden docs from the main Recent index.
;; Direct /notes?slug=… still opens them with the same outline UI.
(define notes/doc/hidden?
  (lambda (row)
    (let ((f (cdr row)))
      (let ((kind (as-str (dict-get f "kind")))
            (tags (as-str (dict-get f "tags")))
            (slug (as-str (dict-get f "slug"))))
        (if (string-eq kind "inbox")
            #t
            (if (string-eq kind "hidden")
                #t
                (if (string-eq slug "inbox")
                    #t
                    (string-contains? tags "hidden"))))))))

(define notes/doc/index-list
  (lambda ()
    (list/filter
      (lambda (row) (not (notes/doc/hidden? row)))
      (notes/doc/list))))

(define notes/doc/show-archived?
  (lambda ()
    (string-eq (as-str (request-param "show")) "archived")))

(define notes/doc/search
  (lambda (q)
    (find-text 'note "title" (as-str q) 40)))

(define notes/doc/daily
  (lambda (day)
    (let ((slug (string-append "daily-" (as-str day))))
      (if (null? (notes/doc/get slug))
          (seq
            (notes/doc/upsert slug
              (string-append "Daily " (as-str day))
              (notes/doc/empty-outline (string-append "Daily " (as-str day)))
              "daily" "daily" (as-str day))
            (notes/doc/get slug))
          (notes/doc/get slug)))))

;; One durable note doc (slug "inbox"). Captures are child nodes under root.
(define notes/doc/ensure-inbox
  (lambda ()
    (if (null? (notes/doc/get "inbox"))
        (seq
          (notes/doc/upsert "inbox" "Inbox"
            (notes/doc/empty-outline "Inbox")
            "inbox" "inbox" "")
          (notes/doc/get "inbox"))
        (notes/doc/get "inbox"))))

(define notes/capture/title
  (lambda (title body)
    (let ((t (string-trim (as-str title))))
      (if (not (string-eq t ""))
          t
          (let ((b (string-trim (as-str body))))
            (if (string-eq b "")
                "capture"
                (if (<= (string-length b) 48)
                    b
                    ;; ASCII ellipsis: substring is byte-based; a UTF-8 "…"
                    ;; cut mid-sequence used to corrupt titles (u2026 garbage).
                    (string-append (substring b 0 45) "..."))))))))

(define notes/capture/node-id
  (lambda (tree)
    (string-append "c"
      (as-str (count (outline/kids tree)))
      "-"
      (as-str (string-length (outline/encode tree))))))

;; Append a capture as a child of the Inbox outline root. Returns "inbox".
(define notes/capture/add
  (lambda (title body)
    (let ((prev (notes/doc/ensure-inbox)))
      (let ((tree (notes/doc/outline prev))
            (t (notes/capture/title title body))
            (b (as-str body)))
        (let ((child
                (outline/set-body
                  (outline/node (notes/capture/node-id tree) t)
                  b)))
          (let ((new-tree (outline/add-child-by-id tree "root" child)))
            (seq
              (notes/doc/upsert "inbox" "Inbox" new-tree
                "inbox" "inbox" "")
              "inbox")))))))

;; Compact capture control for the top nav (details dropdown).
(define notes/capture/nav
  (lambda ()
    (html/tag "details" (html/attr "class" "menu-drop capture-drop")
      (html/join
        (list
          (html/tag "summary" (html/attr "class" "menu-drop-btn") "capture")
          (html/div (html/attr "class" "menu-drop-panel capture-panel")
            (html/form "POST" "/notes"
              (html/attrs
                (list
                  (html/attr "class" "capture-nav-form")
                  (html/attr "hx-post" "/notes")
                  (html/attr "hx-target" "#capture-status")
                  (html/attr "hx-swap" "innerHTML")
                  (html/attr "hx-on::after-request"
                    "if(event.detail.successful){this.reset()}")))
              (html/join
                (list
                  (html/hidden "action" "capture")
                  (html/text-input "title"
                    (html/attrs
                      (list
                        (html/attr "placeholder" "title (optional)")
                        (html/attr "autocomplete" "off")
                        (html/attr "class" "capture-nav-title"))))
                  (html/textarea "body"
                    (html/attrs
                      (list
                        (html/attr "rows" "3")
                        (html/attr "class" "capture-body")
                        (html/attr "placeholder" "Quick capture…")
                        (html/attr "required" "required")))
                    "")
                  (html/div (html/attr "class" "capture-row")
                    (html/join
                      (list
                        (html/button (html/attr "type" "submit") "Save")
                        (html/a "/notes?slug=inbox" "inbox"))))
                  (html/div (html/attr "id" "capture-status") ""))))))))))

;; Full-page form (also used if someone hits /capture).
(define notes/capture/form
  (lambda ()
    (html/form "POST" "/notes"
      (html/attrs
        (list
          (html/attr "class" "card capture-card")
          (html/attr "hx-post" "/notes")
          (html/attr "hx-target" "#capture-status")
          (html/attr "hx-swap" "innerHTML")
          (html/attr "hx-on::after-request"
            "if(event.detail.successful){this.reset()}")))
      (html/join
        (list
          (html/hidden "action" "capture")
          (html/h2 "Capture")
          (html/p* (html/attr "class" "muted")
            "Drops a node into the Inbox note.")
          (html/label (html/attr "for" "capture-title") "Title")
          (html/text-input "title"
            (html/attrs
              (list
                (html/attr "id" "capture-title")
                (html/attr "placeholder" "optional")
                (html/attr "autocomplete" "off"))))
          (html/label (html/attr "for" "capture-body") "Note")
          (html/textarea "body"
            (html/attrs
              (list
                (html/attr "id" "capture-body")
                (html/attr "rows" "3")
                (html/attr "class" "capture-body")
                (html/attr "placeholder" "Quick capture…")
                (html/attr "required" "required")))
            "")
          (html/div (html/attr "class" "capture-row")
            (html/join
              (list
                (html/button (html/attr "type" "submit") "Capture")
                (html/a "/notes?slug=inbox" "Open inbox"))))
          (html/div (html/attr "id" "capture-status") ""))))))

(define notes/capture/page
  (lambda ()
    (if (string-eq (request-method) "POST")
        (notes/capture/handle)
        (html/page
          (list
            (html/h1 "Capture")
            (notes/capture/form))))))

(define notes/capture/status
  (lambda (ok msg)
    (html/p*
      (html/attr "class" (if ok "ok" "err"))
      msg)))
(define notes/doc/delete
  (lambda (slug)
    (let ((row (notes/doc/row (as-str slug))))
      (if (null? row)
          ()
          (delete 'note (car row))))))

(define notes/ui/item-li
  (lambda (row)
    (let ((f (cdr row))
          (id (car row)))
      (let ((slug (as-str (dict-get f "slug")))
            (title (as-str (dict-get f "title"))))
        (html/li (html/attr "class" "note-item")
          (html/join
            (list
              (html/a
                (string-append "/notes?slug=" slug)
                (html/text title))
              (html/form "POST" "/notes"
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
(define notes/ui/recent-list
  (lambda ()
    (list/foldl
      (lambda (acc row) (string-append acc (notes/ui/item-li row)))
      ""
      (notes/doc/index-list))))

(define notes/ui/create-form
  (lambda ()
    (html/form "POST" "/notes" (html/attr "class" "card")
      (html/join
        (list
          (html/hidden "action" "create")
          (html/label "" "Title ")
          (html/text-input "title" (html/attr "placeholder" "New note"))
          " "
          (html/label "" "Slug ")
          (html/text-input "slug" (html/attr "placeholder" "my-note"))
          " "
          (html/button (html/attr "type" "submit") "Create"))))))
(define notes/ui/index
  (lambda ()
    (html/join
      (list
        (html/h1 "Notes")
        (html/p* (html/attr "class" "muted")
          "Outline-first notes. Create or open a slug.")
        (html/p* (html/attr "class" "muted")
          (html/join
            (list
              (html/a "/notes?slug=inbox" "Open inbox")
              " · system notes (inbox) are hidden from Recent.")))
        (notes/ui/create-form)
        (html/h2 "Recent")
        (html/ul (html/attr "class" "card") (notes/ui/recent-list))))))
(define notes/ui/missing
  (lambda (slug)
    (html/join
      (list
        (html/h1 "Missing")
        (html/p* (html/attr "class" "err")
          (string-append "No note " (as-str slug)))
        (html/p* "" (html/a "/notes" "All notes"))))))
(define notes/ui/edit-form
  (lambda (slug outline)
    (html/form "POST" "/notes" ""
      (html/join
        (list
          (html/hidden "action" "save")
          (html/hidden "slug" slug)
          (html/tag "textarea"
            (html/attrs
              (list
                (html/attr "name" "outline")
                (html/attr "rows" "16")))
            (outline/encode outline))
          (html/p* ""
            (html/button (html/attr "type" "submit") "Save outline")))))))

;; Desktop top toolbar: acts on selected node (#ol-selected-id). Hidden on mobile.
(define notes/ui/tb-btn
  (lambda (op label)
    (html/tag "button"
      (html/attrs
        (list
          (html/attr "type" "button")
          (html/attr "class" "ol-btn")
          (html/attr "title" op)
          (html/attr "data-ol-op" op)))
      label)))

(define notes/ui/toolbar
  (lambda (slug show-archived)
    (html/div (html/attr "class" "ol-toolbar")
      (html/join
        (list
          (html/span (html/attr "class" "ol-toolbar-label") "Selected:")
          (html/span (html/attr "id" "ol-selected-label") "root")
          (html/void "input"
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
          (notes/ui/tb-btn "move-up" "up")
          (notes/ui/tb-btn "move-down" "dn")
          (notes/ui/tb-btn "delete" "del")
          (html/form "POST" "/notes"
            (html/attr "class" "ol-refile")
            (html/join
              (list
                (html/hidden "action" "refile")
                (html/hidden "src_slug" (as-str slug))
                (html/void "input"
                  (html/attrs
                    (list
                      (html/attr "type" "hidden")
                      (html/attr "name" "node_id")
                      (html/attr "id" "ol-refile-nid")
                      (html/attr "value" "root"))))
                (html/select "dst_slug"
                  (html/attr "class" "ol-refile-sel")
                  (notes/refile/options slug))
                (html/button
                  (html/attrs
                    (list
                      (html/attr "type" "submit")
                      (html/attr "class" "ol-btn")
                      (html/attr "title" "refile selected node")))
                  "refile"))))
          (if show-archived
              (html/a (string-append "/notes?slug=" slug) "Hide archived")
              (html/a (string-append "/notes?slug=" slug "&show=archived")
                "Show archived")))))))

(define notes/ui/outline-script
  (lambda (slug show-archived)
    (html/tag "script" ""
      (string-append
        "(function(){var slug='"
        (as-str slug)
        "';var showArch="
        (if show-archived "true" "false")
        ";function selId(){var el=document.getElementById('ol-selected-id');return el?el.value:'root';}"
        "function setSel(id){var hid=document.getElementById('ol-selected-id');if(hid)hid.value=id;"
        "var lab=document.getElementById('ol-selected-label');if(lab)lab.textContent=id;"
        "var rf=document.getElementById('ol-refile-nid');if(rf)rf.value=id;"
        "document.querySelectorAll('.ol-node.is-selected').forEach(function(n){n.classList.remove('is-selected');});"
        "var node=document.querySelector('.ol-node[data-id=\"'+id+'\"]');if(node)node.classList.add('is-selected');}"
        "function postOp(op){var id=selId();if(op==='delete'&&!confirm('Delete this node?'))return;"
        "var q='slug='+encodeURIComponent(slug)+'&op='+encodeURIComponent(op)+'&node-id='+encodeURIComponent(id);"
        "if(showArch)q+='&show=archived';"
        "if(window.htmx){htmx.ajax('POST','/partials/notes-node-op?'+q,{target:'#outline',swap:'outerHTML'});}"
        "else{fetch('/partials/notes-node-op?'+q,{method:'POST'}).then(function(r){return r.text();}).then(function(html){"
        "var t=document.getElementById('outline');if(t)t.outerHTML=html;});}}"
        "document.addEventListener('click',function(e){"
        "var btn=e.target.closest&&e.target.closest('[data-ol-op]');"
        "if(btn){e.preventDefault();postOp(btn.getAttribute('data-ol-op'));return;}"
        "var node=e.target.closest&&e.target.closest('.ol-node');"
        "if(node&&node.dataset&&node.dataset.id&&!e.target.closest('.ol-actions')&&!(e.target.closest&&e.target.closest('[contenteditable=true]'))){"
        "setSel(node.dataset.id);}},true);"
        "var root=document.getElementById('outline');if(root){root.addEventListener('blur',function(e){"
        "var t=e.target;if(!t.dataset||!t.dataset.field)return;"
        "var p='slug='+encodeURIComponent(slug)+'&op=set-'+t.dataset.field+'&node-id='+encodeURIComponent(t.dataset.id);"
        "if(showArch)p+='&show=archived';"
        "p+='&'+t.dataset.field+'='+encodeURIComponent(t.innerText);"
        "fetch('/partials/notes-node-op',{method:'POST',headers:{'Content-Type':'application/x-www-form-urlencoded'},body:p);},true);}"
        "setSel('root');document.body.addEventListener('htmx:afterSwap',function(){setSel(selId());});})();"))))

(define notes/ui/show-body
  (lambda (slug title outline tags show-archived)
    (html/join
      (list
        (html/p* (html/attr "class" "muted")
          (html/join
            (list
              (html/a "/notes" "Notes")
              " / "
              (html/code (as-str slug)))))
        (html/div (html/attr "class" "note-title-row")
          (html/join
            (list
              (html/h1 (html/text title))
              (html/form "POST" "/notes"
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
        (html/p* (html/attr "class" "muted")
          (string-append "tags: " (as-str tags)))
        (notes/ui/toolbar slug show-archived)
        (outline/html slug outline show-archived)
        (html/tag "details" (html/attr "class" "ol-debug")
          (html/join
            (list
              (html/tag "summary" (html/attr "class" "muted") "Raw JSON (debug)")
              (notes/ui/edit-form slug outline))))
        (notes/ui/outline-script slug show-archived)))))

(define notes/ui/show
  (lambda (slug)
    (let ((n (notes/doc/get slug))
          (show-archived (notes/doc/show-archived?)))
      (if (null? n)
          (notes/ui/missing slug)
          (notes/ui/show-body slug
            (notes/doc/title n)
            (notes/doc/outline n)
            (notes/doc/tags n)
            show-archived)))))
(define notes/ui/do-create
  (lambda (title slug)
    (let ((s
            (if (if (null? slug) #t (string-eq (string-trim (as-str slug)) ""))
                (string-append "note-" (as-str (count (notes/doc/list))))
                (string-trim (as-str slug)))))
      (seq
        (notes/doc/upsert s (as-str title)
          (notes/doc/empty-outline (as-str title))
          "" "note" "")
        (redirect (string-append "/notes?slug=" s))))))
(define notes/ui/do-save
  (lambda (slug raw)
    (let ((tree (outline/decode raw))
          (prev (notes/doc/get slug)))
      (let ((title (if (null? prev) (as-str slug) (notes/doc/title prev)))
            (tags (if (null? prev) "" (notes/doc/tags prev)))
            (kind (if (null? prev) "note" (notes/doc/kind prev)))
            (day (if (null? prev) "" (notes/doc/day prev))))
        (seq
          (notes/doc/upsert (as-str slug) title tree tags kind day)
          (redirect (string-append "/notes?slug=" (as-str slug))))))))
(define notes/capture/handle
  (lambda ()
    (let ((title (request-param "title"))
          (body (request-param "body")))
      (let ((b (if (null? body) "" (as-str body))))
        (if (string-eq (string-trim b) "")
            (if (hx-request?)
                (render-html
                  (notes/capture/status #f "Write something first."))
                (redirect "/"))
            (seq
              (notes/capture/add
                (if (null? title) "" title)
                b)
              (if (hx-request?)
                  (render-html
                    (notes/capture/status #t
                      "Saved to Inbox."))
                  (redirect "/notes?slug=inbox"))))))))

(define notes/ui/do-delete
  (lambda ()
    (let ((slug (as-str (request-param "slug"))))
      (seq
        (notes/doc/delete slug)
        (redirect "/notes")))))

;; Target note <option> list for refile (exclude source slug).
(define notes/refile/options
  (lambda (src-slug)
    (list/foldl
      (lambda (acc row)
        (let ((f (cdr row)))
          (let ((slug (as-str (dict-get f "slug")))
                (title (as-str (dict-get f "title"))))
            (if (string-eq slug (as-str src-slug))
                acc
                (string-append acc
                  (html/option slug
                    (if (string-eq title "") slug title)
                    #f))))))
      ""
      (notes/doc/list))))

(define notes/refile/form
  (lambda (src-slug nid)
    (if (string-eq (as-str nid) "root")
        ""
        (html/form "POST" "/notes"
          (html/attr "class" "ol-refile")
          (html/join
            (list
              (html/hidden "action" "refile")
              (html/hidden "src_slug" (as-str src-slug))
              (html/hidden "node_id" (as-str nid))
              (html/select "dst_slug"
                (html/attr "class" "ol-refile-sel")
                (notes/refile/options src-slug))
              (html/button
                (html/attrs
                  (list
                    (html/attr "type" "submit")
                    (html/attr "class" "ol-btn")
                    (html/attr "title" "move node to another note")))
                "refile")))))))

;; Move node nid from src-slug into dst-slug under root. Keeps node id/title/body/todo/kids.
(define notes/refile/move
  (lambda (src-slug nid dst-slug)
    (let ((src (notes/doc/get src-slug))
          (dst (notes/doc/get dst-slug))
          (sid (as-str src-slug))
          (did (as-str dst-slug))
          (id (as-str nid)))
      (if (if (null? src) #t (null? dst))
          #f
          (if (string-eq sid did)
              #f
              (if (string-eq id "root")
                  #f
                  (let ((src-tree (notes/doc/outline src))
                        (dst-tree (notes/doc/outline dst)))
                    (let ((node (outline/find src-tree id)))
                      (if (null? node)
                          #f
                          (seq
                            (notes/doc/upsert did
                              (notes/doc/title dst)
                              (outline/add-child-by-id dst-tree "root" node)
                              (notes/doc/tags dst)
                              (notes/doc/kind dst)
                              (notes/doc/day dst))
                            (seq
                              (notes/doc/upsert sid
                                (notes/doc/title src)
                                (outline/delete src-tree id)
                                (notes/doc/tags src)
                                (notes/doc/kind src)
                                (notes/doc/day src))
                              #t)))))))))))

(define notes/refile/handle
  (lambda ()
    (let ((src (as-str (request-param "src_slug")))
          (nid (as-str (request-param "node_id")))
          (dst (as-str (request-param "dst_slug"))))
      (seq
        (notes/refile/move src nid dst)
        (redirect
          (string-append "/notes?slug=" src))))))

;; Hook refile form into outline action panels (after outline lib loads).
(define outline/extra-actions
  (lambda (slug nid)
    (notes/refile/form slug nid)))

(define notes/ui/handle-post
  (lambda ()
    (let ((action (as-str (request-param "action"))))
      (if (string-eq action "create")
          (notes/ui/do-create (request-param "title") (request-param "slug"))
          (if (string-eq action "save")
              (notes/ui/do-save (request-param "slug") (request-param "outline"))
              (if (string-eq action "capture")
                  (notes/capture/handle)
                  (if (string-eq action "delete")
                      (notes/ui/do-delete)
                      (if (string-eq action "refile")
                          (notes/refile/handle)
                          (redirect "/notes")))))))))

(define notes/page
  (lambda ()
    (if (string-eq (request-method) "POST")
        (notes/ui/handle-post)
        (let ((slug (request-param "slug")))
          (if (if (null? slug) #t (string-eq (as-str slug) ""))
              (html/page (list (notes/ui/index)))
              (html/page (list (notes/ui/show (as-str slug)))))))))
(define notes/outline/partial
  (lambda ()
    (let ((n (notes/doc/get (request-param "slug")))
          (show-archived (notes/doc/show-archived?)))
      (if (null? n)
          (render-html (html/p* (html/attr "class" "err") "missing"))
          (render-html
            (outline/html (as-str (request-param "slug"))
              (notes/doc/outline n) show-archived))))))

;; --- node op handler ---

(define notes/outline/apply-op
  (lambda (slug tree op nid)
    (if (string-eq op "add-child")
        (outline/add-child-by-id tree nid (outline/node "n-new" "New node"))
        (if (string-eq op "add-sibling")
            (outline/insert-sibling tree nid (outline/node "n-new" "New node"))
            (if (string-eq op "delete")
                (outline/delete tree nid)
                (if (string-eq op "move-up")
                    (outline/move-up tree nid)
                    (if (string-eq op "move-down")
                        (outline/move-down tree nid)
                        (if (string-eq op "set-title")
                            (outline/update tree nid
                              (lambda (n) (outline/set-title n (request-param "title"))))
                            (if (string-eq op "set-body")
                                (outline/update tree nid
                                  (lambda (n) (outline/set-body n (request-param "body"))))
                                (if (string-eq op "set-todo")
                                    (outline/update tree nid
                                      (lambda (n) (outline/set-todo n (request-param "todo"))))
                                    (if (string-eq op "cycle-todo")
                                        (outline/update tree nid
                                          (lambda (n) (outline/cycle-todo n)))
                                        (if (string-eq op "archive")
                                            (outline/update tree nid
                                              (lambda (n) (outline/archive n)))
                                            (if (string-eq op "unarchive")
                                                (outline/update tree nid
                                                  (lambda (n) (outline/unarchive n)))
                                                tree)))))))))))))

;; (slug node-id title state)*
(define notes/outline/all-todos
  (lambda ()
    (list/foldl
      (lambda (acc row)
        (let ((f (cdr row)))
          (let ((slug (as-str (dict-get f "slug")))
                (tree (outline/decode (dict-get f "outline"))))
            (list/foldl
              (lambda (a t)
                (cons (list slug (car t) (car (cdr t)) (car (cdr (cdr t)))) a))
              acc
              (outline/todos tree)))))
      (list)
      (notes/doc/list))))

(define notes/outline/open-todos
  (lambda ()
    (list/filter
      (lambda (t) (outline/todo-open? (car (cdr (cdr (cdr t))))))
      (notes/outline/all-todos))))

(define notes/outline/cycle-todo
  (lambda (slug nid)
    (let ((prev (notes/doc/get slug)))
      (if (null? prev)
          ()
          (let ((tree (notes/doc/outline prev))
                (title (notes/doc/title prev))
                (tags (notes/doc/tags prev))
                (kind (notes/doc/kind prev))
                (day (notes/doc/day prev)))
            (let ((new-tree
                    (outline/update tree nid
                      (lambda (n) (outline/cycle-todo n)))))
              (notes/doc/upsert slug title new-tree tags kind day)))))))

(define notes/outline/node-op
  (lambda ()
    (let ((slug (as-str (request-param "slug")))
          (op (as-str (request-param "op")))
          (nid (as-str (request-param "node-id")))
          (show-archived (notes/doc/show-archived?)))
      (let ((prev (notes/doc/get slug)))
        (if (null? prev)
            (render-html (html/p* (html/attr "class" "err") "missing"))
            (let ((tree (notes/doc/outline prev))
                  (title (notes/doc/title prev))
                  (tags (notes/doc/tags prev))
                  (kind (notes/doc/kind prev))
                  (day (notes/doc/day prev)))
              (let ((new-tree (notes/outline/apply-op slug tree op nid)))
                (seq
                  (notes/doc/upsert slug title new-tree tags kind day)
                  (render-html
                    (outline/html slug new-tree show-archived))))))))))

(route "/notes" "notes/page")
(route "/capture" "notes/capture/page")
(route "/partials/notes-outline" "notes/outline/partial")
(route "/partials/notes-node-op" "notes/outline/node-op")
