;; package notes/ui -- exported from live branch system/main
;; Sync: scripts/pp_sync.py pull|push

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

