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
(deftype 'note-link 'from_slug 'from_id 'to_slug 'to_id 'label 'kind)

;; =========================================================
;; links-and-roam: [[slug]] / [[slug|label]] wiki links
;; =========================================================

;; Find index of "]]" starting at i, or () if missing.
(define roam/find-close
  (lambda (s i)
    (if (>= i (- (string-length s) 1))
        ()
        (if (if (string-eq (substring s i 1) "]")
                (string-eq (substring s (+ i 1) 1) "]")
                #f)
            i
            (roam/find-close s (+ i 1))))))

;; Split "slug|label" → (slug label); no pipe → (slug slug).
(define roam/split-label
  (lambda (inner)
    (let ((s (string-trim (as-str inner))))
      (roam/split-label-at s 0))))

(define roam/split-label-at
  (lambda (s i)
    (if (>= i (string-length s))
        (list s s)
        (if (string-eq (substring s i 1) "|")
            (list
              (string-trim (substring s 0 i))
              (string-trim (substring s (+ i 1) (- (string-length s) (+ i 1)))))
            (roam/split-label-at s (+ i 1))))))

;; Parse body → list of (slug label) pairs, left-to-right, de-duped later.
(define roam/parse-links
  (lambda (body)
    (roam/parse-links-acc (as-str body) 0 (list))))

(define roam/parse-links-acc
  (lambda (s i acc)
    (if (>= i (- (string-length s) 3))
        (reverse acc)
        (if (if (string-eq (substring s i 1) "[")
                (string-eq (substring s (+ i 1) 1) "[")
                #f)
            (let ((end (roam/find-close s (+ i 2))))
              (if (null? end)
                  (roam/parse-links-acc s (+ i 1) acc)
                  (let ((inner (substring s (+ i 2) (- end (+ i 2)))))
                    (let ((pair (roam/split-label inner)))
                      (if (string-eq (car pair) "")
                          (roam/parse-links-acc s (+ end 2) acc)
                          (roam/parse-links-acc s (+ end 2) (cons pair acc)))))))
            (roam/parse-links-acc s (+ i 1) acc)))))

;; Collect (from_id slug label) from every node body in the tree.
(define roam/collect-from-tree
  (lambda (tree)
    (roam/collect-node tree (list))))

(define roam/collect-node
  (lambda (n acc)
    (let ((id (as-str (outline/id n)))
          (links (roam/parse-links (outline/body n))))
      (let ((acc1
              (list/foldl
                (lambda (a pair)
                  (cons (list id (car pair) (car (cdr pair))) a))
                acc
                links)))
        (list/foldl
          (lambda (a k) (roam/collect-node k a))
          acc1
          (outline/kids n))))))

;; Delete every note-link row with from_slug = slug (scan + delete by id).
(define roam/delete-from-slug
  (lambda (slug)
    (list/foldl
      (lambda (_ row)
        (seq (delete 'note-link (car row)) 0))
      0
      (find-rows 'note-link "from_slug" (as-str slug) 500))))

;; Seen-key = from_id + "\t" + to_slug. Returns #t if already present.
(define roam/seen?
  (lambda (seen key)
    (if (null? seen)
        #f
        (if (string-eq (car seen) key)
            #t
            (roam/seen? (cdr seen) key)))))

(define roam/insert-collected
  (lambda (from-slug triples seen)
    (if (null? triples)
        seen
        (let ((t (car triples)))
          (let ((fid (as-str (car t)))
                (to (as-str (car (cdr t))))
                (lab (as-str (car (cdr (cdr t))))))
            (let ((key (string-append fid "\t" to)))
              (if (roam/seen? seen key)
                  (roam/insert-collected from-slug (cdr triples) seen)
                  (seq
                    (create 'note-link
                      (dict-set* "{}"
                        (list
                          "from_slug" (as-str from-slug)
                          "from_id" fid
                          "to_slug" to
                          "to_id" ""
                          "label" lab
                          "kind" "wikilink")))
                    (roam/insert-collected from-slug (cdr triples)
                      (cons key seen))))))))))

;; Rebuild note-link rows for one note (called on every upsert).
(define roam/reindex
  (lambda (slug outline-tree)
    (seq
      (roam/delete-from-slug slug)
      (roam/insert-collected slug
        (roam/collect-from-tree outline-tree)
        (list))
      slug)))

;; Inbound links: list of (from_slug from_id from_title label).
(define roam/backlinks
  (lambda (slug)
    (list/map
      (lambda (row)
        (let ((f (cdr row)))
          (let ((fs (as-str (dict-get f "from_slug")))
                (fid (as-str (dict-get f "from_id")))
                (lab (as-str (dict-get f "label"))))
            (let ((src (notes/doc/get fs)))
              (list fs fid
                (if (null? src) fs (notes/doc/title src))
                lab)))))
      (find-rows 'note-link "to_slug" (as-str slug) 200))))

;; Render body with [[slug]] / [[slug|label]] → anchors. Escapes plain text.
(define roam/render-body
  (lambda (body)
    (roam/render-acc (as-str body) 0 "")))

(define roam/at-open?
  (lambda (s i)
    (if (>= i (- (string-length s) 3))
        #f
        (if (string-eq (substring s i 1) "[")
            (string-eq (substring s (+ i 1) 1) "[")
            #f))))

(define roam/render-link
  (lambda (slug lab)
    (html/a
      (string-append "/notes?slug=" slug)
      (html/text lab))))

(define roam/render-acc
  (lambda (s i acc)
    (if (>= i (string-length s))
        acc
        (if (roam/at-open? s i)
            (let ((end (roam/find-close s (+ i 2))))
              (if (null? end)
                  (roam/render-acc s (+ i 1)
                    (string-append acc (html-escape (substring s i 1))))
                  (let ((inner (substring s (+ i 2) (- end (+ i 2)))))
                    (let ((pair (roam/split-label inner)))
                      (let ((slug (car pair))
                            (lab (car (cdr pair))))
                        (if (string-eq slug "")
                            (roam/render-acc s (+ end 2)
                              (string-append acc
                                (html-escape
                                  (substring s i (- (+ end 2) i)))))
                            (roam/render-acc s (+ end 2)
                              (string-append acc
                                (roam/render-link slug lab)))))))))
            (roam/render-acc s (+ i 1)
              (string-append acc (html-escape (substring s i 1))))))))

;; Wire wiki-link rendering into outline HTML (hook defined in outline/).
(define outline/render-body roam/render-body)

;; Backlinks partial HTML for a note page.
(define roam/backlinks-html
  (lambda (slug)
    (let ((bls (roam/backlinks slug)))
      (if (null? bls)
          (html/p* (html/attr "class" "muted") "No backlinks yet.")
          (html/ul (html/attr "class" "roam-backlinks")
            (html/join
              (list/map
                (lambda (b)
                  (html/li ""
                    (html/join
                      (list
                        (html/a
                          (string-append "/notes?slug=" (car b))
                          (html/text (car (cdr (cdr b)))))
                        (html/span (html/attr "class" "muted")
                          (html/text
                            (string-append " · " (car (cdr b)))))))))
                bls)))))))

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
        (seq
          (if (null? row)
              (create 'note fields)
              (update 'note (car row) fields))
          ;; Rebuild wiki-link index for this note (links-and-roam).
          (roam/reindex (as-str slug) outline))))))
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
          (seq
            (roam/delete-from-slug (as-str slug))
            (delete 'note (car row)))))))

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
        "var t=document.getElementById('outline');if(t)t.outerHTML=html;"
        "if(findNode(keep))setSel(keep);else{__selPin=null;setSel('root');}"
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
        (html/div (html/attr "class" "roam-backlinks-panel")
          (html/join
            (list
              (html/h3 "Backlinks")
              (roam/backlinks-html slug))))
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

;; Eval form: run a Lisp expr with self/doc/slug bound, splice result.
(define notes/outline/eval-form
  (lambda (slug nid)
    (html/form "POST" "/partials/notes-node-op"
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
          (html/text-input "expr"
            (html/attrs
              (list
                (html/attr "placeholder" "(outline/node (gensym \"n\") \"hi\")")
                (html/attr "class" "ol-eval-expr")
                (html/attr "style" "width:12rem"))))
          (html/select "target" (html/attr "class" "ol-eval-tgt")
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

;; Hook refile + eval into outline action panels (after outline lib loads).
(define outline/extra-actions
  (lambda (slug nid)
    (html/join
      (list
        (notes/refile/form slug nid)
        (notes/outline/eval-form slug nid)))))

(define notes/ui/handle-post
  (lambda ()
    (let ((action (as-str (request-param "action"))))
      (cond
        ((string-eq action "create")
         (notes/ui/do-create (request-param "title") (request-param "slug")))
        ((string-eq action "save")
         (notes/ui/do-save (request-param "slug") (request-param "outline")))
        ((string-eq action "capture")
         (notes/capture/handle))
        ((string-eq action "delete")
         (notes/ui/do-delete))
        ((string-eq action "refile")
         (notes/refile/handle))
        (else (redirect "/notes"))))))

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

(define notes/outline/new-node
  (lambda ()
    (outline/node (gensym "n") "New node")))

(define notes/outline/apply-op
  (lambda (slug tree op nid)
    (cond
      ((string-eq op "add-child")
       (outline/add-child-by-id tree nid (notes/outline/new-node)))
      ((string-eq op "add-sibling")
       (outline/insert-sibling tree nid (notes/outline/new-node)))
      ((string-eq op "delete")
       ;; Guard root even if outline/delete also refuses — clear product rule.
       (if (string-eq (as-str nid) "root")
           tree
           (outline/delete tree nid)))
      ((string-eq op "move-up")
       (outline/move-up tree nid))
      ((string-eq op "move-down")
       (outline/move-down tree nid))
      ((string-eq op "set-title")
       (outline/update tree nid
         (lambda (n) (outline/set-title n (request-param "title")))))
      ((string-eq op "set-body")
       (outline/update tree nid
         (lambda (n) (outline/set-body n (request-param "body")))))
      ((string-eq op "set-todo")
       (outline/update tree nid
         (lambda (n) (outline/set-todo n (request-param "todo")))))
      ((string-eq op "cycle-todo")
       (outline/update tree nid
         (lambda (n) (outline/cycle-todo n))))
      ((string-eq op "archive")
       (if (string-eq (as-str nid) "root")
           tree
           (outline/update tree nid
             (lambda (n) (outline/archive n)))))
      ((string-eq op "unarchive")
       (if (string-eq (as-str nid) "root")
           tree
           (outline/update tree nid
             (lambda (n) (outline/unarchive n)))))
      (else tree))))

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

;; Evaluate expr with self/doc/slug bound. Uses load-string to install a
;; temp lambda (REPL-equivalent; auth gate is the only boundary — v1).
(define notes/outline/eval-result
  (lambda (slug nid expr)
    (let ((prev (notes/doc/get slug)))
      (if (null? prev)
          ()
          (let ((tree (notes/doc/outline prev)))
            (let ((node (outline/find tree (as-str nid))))
              (if (null? node)
                  ()
                  (seq
                    (load-string
                      (string-append
                        "(define __notes_eval_fn (lambda (self doc slug) "
                        (as-str expr)
                        "))"))
                    (__notes_eval_fn node prev (as-str slug))))))))))

;; Splice eval result into the tree at nid.
;; target: body | child | sibling (default child).
(define notes/outline/splice-eval
  (lambda (tree nid target result)
    (let ((t (as-str target)))
      (if (string-eq t "body")
          (outline/update tree nid
            (lambda (n) (outline/set-body n (as-str result))))
          (let ((child
                  (if (outline/valid? result)
                      result
                      (outline/node (gensym "n") (as-str result)))))
            (if (string-eq t "sibling")
                (outline/insert-sibling tree nid child)
                (outline/add-child-by-id tree nid child)))))))

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
              (let ((new-tree
                      (if (string-eq op "eval")
                          (notes/outline/splice-eval tree nid
                            (let ((tg (request-param "target")))
                              (if (if (null? tg) #t (string-eq (as-str tg) ""))
                                  "child"
                                  (as-str tg)))
                            (notes/outline/eval-result slug nid
                              (request-param "expr")))
                          (notes/outline/apply-op slug tree op nid))))
                (seq
                  (notes/doc/upsert slug title new-tree tags kind day)
                  (render-html
                    (outline/html slug new-tree show-archived))))))))))


;; =========================================================
;; wiki-llm-loop: ingest / query / lint as Lisp ops
;; =========================================================

(deftype 'wiki-event 'kind 'op 'slug 'ts 'summary)

(define wiki/default-schema
  (string-append
    "You maintain a personal knowledge wiki of outline notes.\\n"
    "Conventions:\\n"
    "- Pages are notes with kind in {source, entity, concept, query, synthesis, schema}.\\n"
    "- Cross-links use [[slug]] or [[slug|label]].\\n"
    "- Prefer updating existing entity/concept pages over creating duplicates.\\n"
    "- Keep titles short; put detail in outline bodies.\\n"
    "- When asked for JSON, reply with ONE JSON object only (no markdown fences).\\n"))

;; Plain LLM completion (no agent tools). tools=#f forces a single text turn.
(define wiki/ask
  (lambda (prompt)
    (let ((sid
            (agent-session-new
              "You help maintain a personal knowledge wiki. Reply with plain text or JSON as asked. Do not call tools."
              #f)))
      (agent-say sid (as-str prompt)))))

(define wiki/schema
  (lambda ()
    (let ((n (notes/doc/get "wiki-schema")))
      (if (null? n)
          (seq
            (notes/doc/upsert "wiki-schema" "Wiki Schema"
              (outline/node* "root" "Wiki Schema" wiki/default-schema "" (list))
              "schema" "schema" "")
            wiki/default-schema)
          (outline/body (notes/doc/outline n))))))

(define wiki/now
  (lambda ()
    (as-str (date-today))))

(define wiki/log-event
  (lambda (kind op slug summary)
    (create 'wiki-event
      (dict-set* "{}"
        (list
          "kind" (as-str kind)
          "op" (as-str op)
          "slug" (as-str slug)
          "ts" (wiki/now)
          "summary" (as-str summary))))))

(define wiki/events
  (lambda ()
    (rows 'wiki-event 100)))

(define wiki/prune-events
  (lambda (days)
    (let ((cutoff (date-add-days (date-today) (- 0 (if (null? days) 30 (parse-int (as-str days)))))))
      (list/foldl
        (lambda (n row)
          (let ((f (cdr row)))
            (let ((ts (as-str (dict-get f "ts"))))
              (if (if (string-eq ts "") #f (< (date-diff-days ts cutoff) 0))
                  (seq (delete 'wiki-event (car row)) (+ n 1))
                  n))))
        0
        (wiki/events)))))

;; Extract a JSON object substring from LLM text (first { ... last }).
(define wiki/extract-json
  (lambda (s)
    (let ((t (as-str s)))
      (wiki/extract-json-scan t 0 () ()))))

(define wiki/extract-json-scan
  (lambda (s i start end)
    (if (>= i (string-length s))
        (if (if (null? start) #t (null? end))
            "{}"
            (substring s start (+ (- end start) 1)))
        (let ((c (substring s i 1)))
          (if (string-eq c "{")
              (wiki/extract-json-scan s (+ i 1)
                (if (null? start) i start)
                end)
              (if (string-eq c "}")
                  (wiki/extract-json-scan s (+ i 1) start i)
                  (wiki/extract-json-scan s (+ i 1) start end)))))))

(define wiki/parse-plan
  (lambda (text)
    (json-parse (wiki/extract-json text))))

(define wiki/plan-list
  (lambda (plan key)
    (let ((v (dict-get plan key)))
      (if (null? v)
          (list)
          (let ((parsed (json-parse (as-str v))))
            (if (pair? parsed)
                parsed
                (if (null? parsed)
                    (list)
                    (list))))))))

(define wiki/slugify
  (lambda (title)
    (let ((t (string-trim (as-str title))))
      (if (string-eq t "")
          (string-append "wiki-" (as-str (count (notes/doc/list))))
          (string-append "w-"
            (substring t 0
              (if (< (string-length t) 32) (string-length t) 32)))))))

(define wiki/upsert-page
  (lambda (slug title body kind)
    (let ((prev (notes/doc/get slug)))
      (let ((tree
              (if (null? prev)
                  (outline/node* "root" title body "" (list))
                  (outline/set-body
                    (outline/set-title (notes/doc/outline prev) title)
                    body)))
            (tags (if (null? prev) kind (notes/doc/tags prev)))
            (day (if (null? prev) "" (notes/doc/day prev)))
            (sc
              (if (null? prev)
                  "1"
                  (let ((raw (dict-get (cdr (notes/doc/row slug)) "source_count")))
                    (if (null? raw)
                        "1"
                        (as-str (+ 1 (parse-int (as-str raw)))))))))
        (seq
          (notes/doc/upsert slug title tree tags kind day)
          ;; stamp frontmatter after upsert (fields not in make-fields)
          (let ((row (notes/doc/row slug)))
            (if (null? row)
                slug
                (seq
                  (update-field 'note (car row) "updated_at" (wiki/now))
                  (update-field 'note (car row) "updated_by" "llm")
                  (update-field 'note (car row) "source_count" sc)
                  slug))))))))

(define wiki/apply-entity
  (lambda (ent source-slug touched)
    (let ((slug (as-str (dict-get ent "slug")))
          (title (as-str (dict-get ent "title")))
          (body (as-str (dict-get ent "body")))
          (kind (as-str (dict-get ent "kind"))))
      (let ((s (if (string-eq slug "") (wiki/slugify title) slug))
            (t (if (string-eq title "") s title))
            (b (if (string-eq body "")
                    (string-append "From [[" source-slug "]].")
                    (string-append body "\\n\\nSee also [[" source-slug "]].")))
            (k (if (if (string-eq kind "entity") #t (string-eq kind "concept"))
                    kind
                    "entity")))
        (seq
          (wiki/upsert-page s t b k)
          (append touched (list s)))))))

(define wiki/apply-entities
  (lambda (ents source-slug touched)
    (if (null? ents)
        touched
        (wiki/apply-entities (cdr ents) source-slug
          (wiki/apply-entity (car ents) source-slug touched)))))

(define wiki/ingest-prompt
  (lambda (source-text source-slug)
    (string-append
      (wiki/schema)
      "\\n\\nIngest this source into the wiki.\\n"
      "Source slug: " source-slug "\\n"
      "Source text:\\n" (as-str source-text) "\\n\\n"
      "Reply with ONE JSON object only, shape:\\n"
      "{\"summary\":\"...\",\"entities\":[{\"slug\":\"...\",\"title\":\"...\","
      "\"body\":\"... with [[links]]\",\"kind\":\"entity|concept\"}]}\\n"
      "Create 1-5 entities/concepts. Bodies may include [[slug]] links.\\n")))

(define wiki/ingest-from-source
  (lambda (source-text)
    (let ((src-slug
            (string-append "src-"
              (as-str (count (notes/doc/list))))))
      (seq
        (wiki/upsert-page src-slug
          (string-append "Source " src-slug)
          (as-str source-text)
          "source")
        (let ((raw (wiki/ask (wiki/ingest-prompt source-text src-slug))))
          (let ((plan (wiki/parse-plan raw)))
            (let ((summary (as-str (dict-get plan "summary")))
                  (ents (wiki/plan-list plan "entities")))
              (seq
                (when (not (string-eq summary ""))
                  (wiki/upsert-page src-slug
                    (string-append "Source " src-slug)
                    (string-append summary "\\n\\n---\\n" (as-str source-text))
                    "source"))
                (let ((touched
                        (wiki/apply-entities ents src-slug (list src-slug))))
                  (seq
                    (wiki/log-event "ingest" "wiki/ingest-from-source" src-slug
                      (string-append "touched "
                        (as-str (count touched)) " pages"))
                    (list src-slug touched raw)))))))))))

(define wiki/corpus-index
  (lambda ()
    (list/foldl
      (lambda (acc row)
        (let ((f (cdr row)))
          (let ((slug (as-str (dict-get f "slug")))
                (title (as-str (dict-get f "title")))
                (kind (as-str (dict-get f "kind"))))
            (string-append acc "- [" kind "] " title " ([[" slug "]])\\n"))))
      ""
      (notes/doc/index-list))))

(define wiki/query-prompt
  (lambda (question)
    (string-append
      (wiki/schema)
      "\\n\\nCorpus index:\\n" (wiki/corpus-index)
      "\\n\\nQuestion: " (as-str question) "\\n\\n"
      "Answer using the corpus. Cite pages as [[slug]]. "
      "If you create a synthesis, say so. Plain prose is fine.\\n")))

(define wiki/query
  (lambda (question)
    (let ((answer (wiki/ask (wiki/query-prompt question))))
      (seq
        (wiki/log-event "query" "wiki/query" ""
          (substring (as-str question) 0
            (if (< (string-length (as-str question)) 80)
                (string-length (as-str question))
                80)))
        answer))))

(define wiki/lint-prompt
  (lambda ()
    (string-append
      (wiki/schema)
      "\\n\\nCorpus index:\\n" (wiki/corpus-index)
      "\\n\\nLint this wiki. Report:\\n"
      "1. Orphan pages (no inbound links)\\n"
      "2. Missing pages (mentioned via [[slug]] but absent)\\n"
      "3. Contradictions or stale claims\\n"
      "4. Suggested new sources or questions\\n"
      "Be concise. Use bullet lists.\\n")))

(define wiki/lint
  (lambda ()
    (let ((report (wiki/ask (wiki/lint-prompt))))
      (seq
        (wiki/log-event "lint" "wiki/lint" "" "lint pass")
        report))))


(route "/notes" "notes/page")
(route "/capture" "notes/capture/page")
(route "/partials/notes-outline" "notes/outline/partial")
(route "/partials/notes-node-op" "notes/outline/node-op")
