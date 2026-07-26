;; Hierarchical names (optional depth under notes/):
;;   notes/doc/*      note rows + field accessors + index filters
;;   notes/capture/*  quick capture + inbox append
;;   notes/outline/*  node ops / partials / outline todos
;;   notes/refile/*   move nodes between notes; extract node → new note
;;   notes/ui/*       HTML + POST handlers
;; Public routes stay shallow where useful: notes/page;
;;   notes/capture/page, notes/outline/partial, notes/outline/node-op.
;; Outline-first notes. Depends on outline/ + html/*.
;; (load-library "outline") then (load-library "notes")

(deftype 'note 'title 'slug 'outline 'tags 'kind 'day 'body 'folder)
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
            (roam/find-close s (+ i 1)))))
  "Index of closing wiki-link brackets from i, or empty.")

;; Split "slug|label" → (slug label); no pipe → (slug slug).
(define roam/split-label
  (lambda (inner)
    (let ((s (string-trim (as-str inner))))
      (roam/split-label-at s 0)))
  "Split slug|label pair; default label equals slug.")

(define roam/split-label-at
  (lambda (s i)
    (if (>= i (string-length s))
        (list s s)
        (if (string-eq (substring s i 1) "|")
            (list
              (string-trim (substring s 0 i))
              (string-trim (substring s (+ i 1) (- (string-length s) (+ i 1)))))
            (roam/split-label-at s (+ i 1)))))
  "split-label helper from character index.")

;; Parse body → list of (slug label) pairs, left-to-right, de-duped later.
;; OCaml extract-links prim (1 budget tick) — Lisp char-scanner blew 100k
;; steps on ~2-4k char bodies and left reindex silently half-done.
(define roam/parse-links
  (lambda (body)
    (extract-links (as-str body)))
  "Parse wiki links from body via kernel extract-links prim.")

;; Kept for debugging / fallback; prefer roam/parse-links.
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
            (roam/parse-links-acc s (+ i 1) acc))))
  "Lisp fallback wiki-link scanner (prefer roam/parse-links).")

;; Collect (from_id slug label) from every node body in the tree.
(define roam/collect-from-tree
  (lambda (tree)
    (roam/collect-node tree (list)))
  "Collect link triples from every node body in tree.")

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
          (outline/kids n)))))
  "Collect links from one node and its kids.")

;; Delete every note-link row with from_slug = slug (scan + delete by id).
(define roam/delete-from-slug
  (lambda (slug)
    (list/foldl
      (lambda (_ row)
        (seq (delete 'note-link (car row)) 0))
      0
      (find-rows 'note-link "from_slug" (as-str slug) 500)))
  "Delete all note-link rows with from_slug.")

;; Seen-key = from_id + "\t" + to_slug. Returns #t if already present.
(define roam/seen?
  (lambda (seen key)
    (if (null? seen)
        #f
        (if (string-eq (car seen) key)
            #t
            (roam/seen? (cdr seen) key))))
  "True if from_id/to_slug already in seen set.")

(define roam/insert-collected
  (lambda (from-slug triples seen)
    (if (null? triples)
        seen
        (let ((t (car triples)))
          (let ((fid (as-str (car t)))
                (to (as-str (car (cdr t))))
                (lab (as-str (car (cdr (cdr t))))))
            (let ((key (string-append fid "\t" to)))
              ;; Skip media embeds [[media:uuid]] — not note wiki links.
              (if (roam/media-slug? to)
                  (roam/insert-collected from-slug (cdr triples) seen)
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
  "Insert collected link rows skipping duplicates.")

;; Rebuild note-link rows for one note (called on every upsert).
(define roam/reindex
  (lambda (slug outline-tree)
    (seq
      (roam/delete-from-slug slug)
      (roam/insert-collected slug
        (roam/collect-from-tree outline-tree)
        (list))
      slug))
  "Rebuild note-link index for one note slug.")

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
      (find-rows 'note-link "to_slug" (as-str slug) 200)))
  "Inbound links list for slug.")

;; Render body with [[slug]] / [[slug|label]] / [[media:…]] → HTML.
;; OCaml render-links (1 budget tick) — Lisp roam/render-acc blew the
;; 100k step limit on multi-kB bodies during outline/html.
(define roam/render-body
  (lambda (body)
    (render-links (as-str body)))
  "Render body with wiki/media links via kernel prim.")

(define roam/at-open?
  (lambda (s i)
    (if (>= i (- (string-length s) 3))
        #f
        (if (string-eq (substring s i 1) "[")
            (string-eq (substring s (+ i 1) 1) "[")
            #f)))
  "True if index is at open wiki-link brackets.")

(define roam/render-link
  (lambda (slug lab)
    (html/a
      (string-append "/notes?slug=" slug)
      (html/text lab)))
  "HTML for one wiki link slug/label.")

;; Media embeds in note bodies: [[media:<uuid>]] or [[media:<uuid>|alt text]].
;; Renders an authenticated <img src="/media/<uuid>"> (session cookie required).
;; Not a note wiki link — media: prefix is reserved.
(define roam/media-slug?
  (lambda (slug)
    (let ((s (as-str slug)))
      (if (< (string-length s) 7)
          #f
          (string-eq (substring s 0 6) "media:"))))
  "True if slug is a media: embed reference.")

(define roam/media-id
  (lambda (slug)
    (let ((s (as-str slug)))
      (substring s 6 (- (string-length s) 6))))
  "Extract media uuid from media: slug.")

(define roam/render-media
  (lambda (id alt)
    (let ((mid (as-str id))
          (lab (as-str alt)))
      (let ((href (string-append "/media/" mid))
            (label (if (string-eq lab "") mid lab)))
        (html/join
          (list
            (html/a href
              (html/img href label "note-media"))
            (html/p*
              (html/attr "class" "muted note-media-cap")
              (html/a href (html/text label))))))))
  "HTML img tag for media uuid with alt.")

(define roam/render-wiki-or-media
  (lambda (slug lab)
    (if (roam/media-slug? slug)
        (roam/render-media (roam/media-id slug) lab)
        (roam/render-link slug lab)))
  "Dispatch wiki link vs media embed rendering.")

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
                                (roam/render-wiki-or-media slug lab)))))))))
            (roam/render-acc s (+ i 1)
              (string-append acc (html-escape (substring s i 1)))))))
  "Lisp fallback body renderer (prefer roam/render-body).")

;; Wire wiki-link rendering into outline HTML (hook defined in outline/).
(define outline/render-body (lambda (body) (roam/render-body body))
  "Hook: outline body rendering uses roam/render-body (delayed lookup).")

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
                bls))))))
  "Backlinks section HTML for a note page.")

(define notes/doc/title (lambda (n) (car (cdr (cdr n))))
  "Note title from doc fields.")
(define notes/doc/outline (lambda (n) (car (cdr (cdr (cdr n)))))
  "Note outline tree from doc fields.")
(define notes/doc/tags (lambda (n) (car (cdr (cdr (cdr (cdr n))))))
  "Note tags from doc fields.")
(define notes/doc/kind (lambda (n) (car (cdr (cdr (cdr (cdr (cdr n)))))))
  "Note kind from doc fields.")
(define notes/doc/day (lambda (n) (car (cdr (cdr (cdr (cdr (cdr (cdr n))))))))
  "Note day field from doc fields.")
;; 8th field (id slug title outline tags kind day folder)
(define notes/doc/folder
  (lambda (n)
    (if (null? n)
        ""
        (let ((f (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr n))))))))))
          (if (null? f) "" (as-str f)))))
  "Note folder field from doc fields.")

(define notes/doc/empty-outline
  (lambda (title)
    (outline/node "root" title))
  "Empty root outline tree for a new note.")

(define notes/doc/row
  (lambda (slug)
    (let ((rs (find-rows 'note "slug" (as-str slug) 1)))
      (if (null? rs) () (car rs))))
  "Raw note data_row by slug.")

(define notes/doc/row-folder
  (lambda (row)
    (if (null? row)
        ""
        (let ((f (dict-get (cdr row) "folder")))
          (if (null? f) "" (as-str f)))))
  "Folder string from a note row.")

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
              (dict-get f "day")
              (let ((folder (dict-get f "folder")))
                (if (null? folder) "" folder)))))))
  "Fetch note fields by slug.")

(define notes/doc/make-fields
  (lambda (slug title outline tags kind day folder)
    (dict-set* "{}"
      (list
        "slug" (as-str slug)
        "title" (as-str title)
        "outline" (outline/encode outline)
        "tags" (as-str tags)
        "kind" (as-str kind)
        "day" (as-str day)
        "folder" (as-str folder))))
  "Build note fields dict for create/update.")

;; Upsert preserves existing folder on update; new notes get "".
;; Use notes/doc/set-folder to place/move a note.
(define notes/doc/upsert
  (lambda (slug title outline tags kind day)
    (let ((row (notes/doc/row slug)))
      (let ((folder (notes/doc/row-folder row))
            (fields
              (notes/doc/make-fields slug title outline tags kind day
                (notes/doc/row-folder row))))
        (seq
          (if (null? row)
              (create 'note fields)
              (update 'note (car row) fields))
          ;; Rebuild wiki-link index for this note (links-and-roam).
          (roam/reindex (as-str slug) outline)))))
  "Create or update note by slug; reindex links.")

(define notes/doc/set-folder
  (lambda (slug folder)
    (let ((row (notes/doc/row (as-str slug))))
      (if (null? row)
          ()
          (update-field 'note (car row) "folder" (as-str folder)))))
  "Set folder on existing note slug.")

;; Create or update and force folder in one call.
(define notes/doc/upsert-in
  (lambda (slug title outline tags kind day folder)
    (seq
      (notes/doc/upsert slug title outline tags kind day)
      (notes/doc/set-folder slug folder)
      slug))
  "Upsert note and force folder in one call.")

;; Filter notes by folder ("" = uncategorized only).
(define notes/doc/list-folder
  (lambda (folder)
    (let ((want (as-str folder)))
      (list/filter
        (lambda (row)
          (string-eq (notes/doc/row-folder row) want))
        (notes/doc/index-list))))
  "List notes in folder (empty string = uncategorized).")

;; Unique folder names among visible notes, sorted loosely via fold.
(define notes/doc/folder-names
  (lambda ()
    (list/foldl
      (lambda (acc row)
        (let ((f (notes/doc/row-folder row)))
          (if (notes/doc/folder-seen? acc f)
              acc
              (append acc (list f)))))
      (list)
      (notes/doc/index-list)))
  "Unique folder names among visible notes.")

(define notes/doc/folder-seen?
  (lambda (xs f)
    (if (null? xs)
        #f
        (if (string-eq (car xs) f)
            #t
            (notes/doc/folder-seen? (cdr xs) f))))
  "True if folder name already in accumulator list.")
(define notes/doc/list (lambda () (rows 'note 100))
  "List visible note rows.")

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
                    (string-contains? tags "hidden")))))))
  "True if note should hide from main index.")

(define notes/doc/index-list
  (lambda ()
    (list/filter
      (lambda (row) (not (notes/doc/hidden? row)))
      (notes/doc/list)))
  "Notes for index respecting archived filter.")

(define notes/doc/show-archived?
  (lambda ()
    (string-eq (as-str (request-param "show")) "archived"))
  "True if request asks to show archived notes.")

(define notes/doc/search
  (lambda (q)
    (find-text 'note "title" (as-str q) 40))
  "Search notes by query string.")

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
          (notes/doc/get slug))))
  "Get or create daily note for day string.")

;; One durable note doc (slug "inbox"). Captures are child nodes under root.
(define notes/doc/ensure-inbox
  (lambda ()
    (if (null? (notes/doc/get "inbox"))
        (seq
          (notes/doc/upsert "inbox" "Inbox"
            (notes/doc/empty-outline "Inbox")
            "inbox" "inbox" "")
          (notes/doc/get "inbox"))
        (notes/doc/get "inbox")))
  "Ensure inbox note exists; return slug inbox.")

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
                    (string-append (substring b 0 45) "...")))))))
  "Normalize capture title string.")

(define notes/capture/node-id
  (lambda (tree)
    (string-append "c"
      (as-str (count (outline/kids tree)))
      "-"
      (as-str (string-length (outline/encode tree)))))
  "Fresh node id for a capture child.")

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
              "inbox"))))))
  "Append capture under inbox outline root.")

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
                  (html/div (html/attr "id" "capture-status") "")))))))))
  "Nav dropdown HTML for quick capture.")

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
          (html/div (html/attr "id" "capture-status") "")))))
  "Full-page capture form HTML.")

(define notes/capture/page
  (lambda ()
    (if (string-eq (request-method) "POST")
        (notes/capture/handle)
        (html/page
          (list
            (html/h1 "Capture")
            (notes/capture/form)))))
  "GET/POST /capture handler.")

(define notes/capture/status
  (lambda (ok msg)
    (html/p*
      (html/attr "class" (if ok "ok" "err"))
      msg))
  "Capture status fragment HTML.")
(define notes/doc/delete
  (lambda (slug)
    (let ((row (notes/doc/row (as-str slug))))
      (if (null? row)
          ()
          (seq
            (roam/delete-from-slug (as-str slug))
            (delete 'note (car row))))))
  "Delete note by slug and its link rows.")

(define notes/ui/item-li
  (lambda (row)
    (let ((f (cdr row))
          (id (car row)))
      (let ((slug (as-str (dict-get f "slug")))
            (title (as-str (dict-get f "title")))
            (folder (notes/doc/row-folder row)))
        (html/li (html/attr "class" "note-item")
          (html/join
            (list
              (html/a
                (string-append "/notes?slug=" slug)
                (html/text title))
              (if (string-eq folder "")
                  ""
                  (html/span (html/attr "class" "muted")
                    (html/text (string-append " · " folder))))
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
                      "del"))))))))))
  "Index list item HTML for one note.")

(define notes/ui/folder-list
  (lambda (folder)
    (list/foldl
      (lambda (acc row) (string-append acc (notes/ui/item-li row)))
      ""
      (notes/doc/list-folder folder)))
  "List notes HTML for one folder name.")

(define notes/ui/folder-section
  (lambda (folder)
    (let ((label
            (if (string-eq (as-str folder) "")
                "(uncategorized)"
                (as-str folder)))
          (href
            (if (string-eq (as-str folder) "")
                "/notes?folder="
                (string-append "/notes?folder=" (as-str folder)))))
      (html/div (html/attr "class" "notes-folder card")
        (html/join
          (list
            (html/h3
              (html/a href (html/text label)))
            (html/ul "" (notes/ui/folder-list folder)))))))
  "Folder section card for index.")

(define notes/ui/folders-html
  (lambda ()
    (let ((names (notes/doc/folder-names))
          (filter (request-param "folder")))
      (if (if (null? filter) #f (not (string-eq (as-str filter) "")))
          ;; Single-folder view when ?folder= is set (including empty = uncategorized)
          (notes/ui/folder-section (as-str filter))
          (if (null? names)
              (html/p* (html/attr "class" "muted") "No notes yet.")
              (html/join
                (list/map notes/ui/folder-section names))))))
  "All folder sections HTML.")

(define notes/ui/recent-list
  (lambda ()
    (list/foldl
      (lambda (acc row) (string-append acc (notes/ui/item-li row)))
      ""
      (notes/doc/index-list)))
  "Recent notes list HTML (optional folder filter).")

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
          (html/label "" "Folder ")
          (html/text-input "folder" (html/attr "placeholder" "wiki/entities"))
          " "
          (html/button (html/attr "type" "submit") "Create")))))
  "New note create form HTML.")

(define notes/ui/index
  (lambda ()
    (html/join
      (list
        (html/h1 "Notes")
        (html/p* (html/attr "class" "muted")
          "Outline-first notes. Grouped by folder (optional path like wiki/entities).")
        (html/p* (html/attr "class" "muted")
          (html/join
            (list
              (html/a "/notes?slug=inbox" "Open inbox")
              " · "
              (html/a "/notes" "All folders")
              " · system notes (inbox) are hidden from the index.")))
        (notes/ui/create-form)
        (html/h2 "Folders")
        (notes/ui/folders-html))))
  "Notes index page body HTML.")
(define notes/ui/missing
  (lambda (slug)
    (html/join
      (list
        (html/h1 "Missing")
        (html/p* (html/attr "class" "err")
          (string-append "No note " (as-str slug)))
        (html/p* "" (html/a "/notes" "All notes")))))
  "Missing-note message HTML for slug.")
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
            (html/button (html/attr "type" "submit") "Save outline"))))))
  "Note metadata edit form HTML.")

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
      label))
  "Toolbar button HTML helper.")

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
          (notes/ui/tb-btn "promote" "<<")
          (notes/ui/tb-btn "demote" ">>")
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
          (html/form "POST" "/notes"
            (html/attr "class" "ol-extract")
            (html/join
              (list
                (html/hidden "action" "extract")
                (html/hidden "src_slug" (as-str slug))
                (html/void "input"
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
                      (html/attr "title" "extract selected node into its own note")))
                  "extract"))))
          (if show-archived
              (html/a (string-append "/notes?slug=" slug) "Hide archived")
              (html/a (string-append "/notes?slug=" slug "&show=archived")
                "Show archived"))))))
  "Desktop outline toolbar HTML.")

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
        "setSel('root');document.body.addEventListener('htmx:afterSwap',function(){setSel(selId());});})();")))
  "Client script for outline selection/actions.")

(define notes/ui/folder-form
  (lambda (slug folder)
    (html/form "POST" "/notes" (html/attr "class" "notes-folder-form")
      (html/join
        (list
          (html/hidden "action" "set-folder")
          (html/hidden "slug" (as-str slug))
          (html/label "" "folder ")
          (html/text-input "folder"
            (html/attrs
              (list
                (html/attr "value" (as-str folder))
                (html/attr "placeholder" "wiki/entities")
                (html/attr "style" "width:12rem"))))
          " "
          (html/button (html/attr "type" "submit") "move")))))
  "Folder assignment form for current note.")

(define notes/ui/show-body
  (lambda (slug title outline tags show-archived folder)
    (html/join
      (list
        (html/p* (html/attr "class" "muted")
          (html/join
            (list
              (html/a "/notes" "Notes")
              (if (string-eq (as-str folder) "")
                  ""
                  (html/join
                    (list
                      " / "
                      (html/a
                        (string-append "/notes?folder=" (as-str folder))
                        (html/text (as-str folder))))))
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
        (notes/ui/folder-form slug folder)
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
        (notes/ui/outline-script slug show-archived))))
  "Note show panel body (outline + backlinks).")

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
            show-archived
            (notes/doc/folder n)))))
  "Full note show page HTML for slug.")
(define notes/ui/do-create
  (lambda (title slug folder)
    (let ((s
            (if (if (null? slug) #t (string-eq (string-trim (as-str slug)) ""))
                (string-append "note-" (as-str (count (notes/doc/list))))
                (string-trim (as-str slug))))
          (fld
            (if (null? folder) "" (string-trim (as-str folder)))))
      (seq
        (notes/doc/upsert-in s (as-str title)
          (notes/doc/empty-outline (as-str title))
          "" "note" "" fld)
        (redirect (string-append "/notes?slug=" s)))))
  "POST handler: create note from request.")
(define notes/ui/do-save
  (lambda (slug raw)
    (let ((slug-s (as-str slug))
          (prev (notes/doc/get (as-str slug))))
      (let ((title (if (null? prev) slug-s (notes/doc/title prev)))
            (tags (if (null? prev) "" (notes/doc/tags prev)))
            (kind (if (null? prev) "note" (notes/doc/kind prev)))
            (day (if (null? prev) "" (notes/doc/day prev))))
        (if (or (null? raw) (string-eq (string-trim (as-str raw)) ""))
            ;; Blank: preserve prior behavior (bare Untitled root).
            (seq
              (notes/doc/upsert slug-s title (outline/node "root" "Untitled") tags kind day)
              (redirect (string-append "/notes?slug=" slug-s)))
            ;; Non-empty: validate the root before upsert. outline/decode
            ;; silently falls back to ("root" "Untitled") when the root is
            ;; not a 5-element node, so a hand-edited JSON outline that drops a
            ;; field would WIPE the note's title + kids on save. Parse + check
            ;; outline/valid? ourselves and refuse to write on a malformed root.
            (let ((parsed (json-parse (as-str raw))))
              (if (outline/valid? parsed)
                  (seq
                    (notes/doc/upsert slug-s title parsed tags kind day)
                    (redirect (string-append "/notes?slug=" slug-s)))
                  (html/page
                    (list
                      (html/h1 (string-append "Could not save " slug-s))
                      (html/p* (html/attr "class" "err")
                        "The outline is malformed. Each node must be a 5-element list [id title body todo kids]. The note was not changed.")
                      (html/p* ""
                        (html/a (string-append "/notes?slug=" slug-s) "Back to the note"))
                      (html/tag "pre"
                        (html/attrs (list (html/attr "class" "err")))
                        (as-str raw))))))))))
  "POST handler: save note metadata/outline. Refuses to wipe a note on a malformed outline root; shows an error page instead.")
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
                  (redirect "/notes?slug=inbox")))))))
  "POST handler for capture form.")

(define notes/ui/do-delete
  (lambda ()
    (let ((slug (as-str (request-param "slug"))))
      (seq
        (notes/doc/delete slug)
        (redirect "/notes"))))
  "POST handler: delete note.")

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
      (notes/doc/list)))
  "HTML options of target notes for refile.")

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
                "refile"))))))
  "Refile form HTML for node.")

;; Move node nid from src-slug into dst-slug under root. Keeps node id/title/body/todo/kids.
;; Pure detach on the sexp tree, then two upserts inside one transaction so a
;; half-move cannot leave the node in both notes (or neither).
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
                    (let ((pair (outline/detach src-tree id)))
                      (if (null? pair)
                          #f
                          (let ((node (car pair))
                                (src-without (car (cdr pair))))
                            (with-transaction
                              (lambda ()
                                (seq
                                  (notes/doc/upsert did
                                    (notes/doc/title dst)
                                    (outline/add-child-by-id dst-tree "root" node)
                                    (notes/doc/tags dst)
                                    (notes/doc/kind dst)
                                    (notes/doc/day dst))
                                  (notes/doc/upsert sid
                                    (notes/doc/title src)
                                    src-without
                                    (notes/doc/tags src)
                                    (notes/doc/kind src)
                                    (notes/doc/day src))
                                  #t))))))))))))
  "Move node between notes transactionally.")

(define notes/refile/handle
  (lambda ()
    (let ((src (as-str (request-param "src_slug")))
          (nid (as-str (request-param "node_id")))
          (dst (as-str (request-param "dst_slug"))))
      (seq
        (notes/refile/move src nid dst)
        (redirect
          (string-append "/notes?slug=" src)))))
  "POST handler for refile move.")

;; ---- extract: node → its own note ----
;; Takes a non-root node out of src, creates a new note whose root title/body
;; match the node (kids preserved under root), deletes the node from src.
;; Returns new slug string, or () on failure.

(define notes/refile/slug-char-ok?
  (lambda (c)
    (string-contains?
      "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
      (as-str c)))
  "True if char allowed in generated slug.")

(define notes/refile/slugify-acc
  (lambda (s i acc last-hyphen?)
    (if (>= i (string-length s))
        acc
        (let ((c (substring s i 1)))
          (if (notes/refile/slug-char-ok? c)
              (notes/refile/slugify-acc s (+ i 1)
                (string-append acc c) #f)
              (if last-hyphen?
                  (notes/refile/slugify-acc s (+ i 1) acc #t)
                  (notes/refile/slugify-acc s (+ i 1)
                    (string-append acc "-") #t))))))
  "Accumulator for slugify.")

(define notes/refile/slugify
  (lambda (title)
    (let ((raw
            (notes/refile/slugify-acc
              (string-trim (as-str title)) 0 "" #t)))
      (let ((n (string-length raw)))
        (let ((s
                (if (if (> n 0)
                        (string-eq (substring raw (- n 1) 1) "-")
                        #f)
                    (substring raw 0 (- n 1))
                    raw)))
          (if (string-eq s "")
              (string-append "note-" (as-str (gensym "n")))
              (if (> (string-length s) 48)
                  (substring s 0 48)
                  s))))))
  "Slugify title string to note slug candidate.")

(define notes/refile/unique-slug
  (lambda (base)
    (if (null? (notes/doc/row (as-str base)))
        (as-str base)
        (notes/refile/unique-slug
          (string-append (as-str base) "-" (as-str (gensym "x"))))))
  "Unique slug derived from title.")

(define notes/refile/extract
  (lambda (src-slug nid)
    (let ((src (notes/doc/get src-slug))
          (id (as-str nid)))
      (if (null? src)
          ()
          (if (string-eq id "root")
              ()
              (let ((src-tree (notes/doc/outline src)))
                (let ((pair (outline/detach src-tree id)))
                  (if (null? pair)
                      ()
                      (let ((node (car pair))
                            (src-without (car (cdr pair))))
                        (let ((title (as-str (outline/title node)))
                              (body (as-str (outline/body node)))
                              (kids (outline/kids node))
                              (tags (notes/doc/tags src)))
                          (let ((new-slug
                                  (notes/refile/unique-slug
                                    (notes/refile/slugify title)))
                                (new-tree
                                  (outline/set-kids
                                    (outline/set-body
                                      (outline/node "root" title)
                                      body)
                                    kids)))
                            (with-transaction
                              (lambda ()
                                (seq
                                  (notes/doc/upsert new-slug title new-tree
                                    tags "note" "")
                                  (notes/doc/upsert (as-str src-slug)
                                    (notes/doc/title src)
                                    src-without
                                    (notes/doc/tags src)
                                    (notes/doc/kind src)
                                    (notes/doc/day src))
                                  new-slug)))))))))))))
  "Extract node into its own new note.")

(define notes/refile/extract-handle
  (lambda ()
    (let ((src (as-str (request-param "src_slug")))
          (nid (as-str (request-param "node_id"))))
      (let ((new (notes/refile/extract src nid)))
        (if (null? new)
            (redirect (string-append "/notes?slug=" src))
            (redirect (string-append "/notes?slug=" new))))))
  "POST handler for extract-to-note.")

(define notes/refile/extract-form
  (lambda (src-slug nid)
    (if (string-eq (as-str nid) "root")
        ""
        (html/form "POST" "/notes"
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
  "Extract-to-note form HTML.")

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
            "eval")))))
  "Eval-and-splice form HTML for outline toolbar.")

;; Hook refile + eval into outline action panels (after outline lib loads).
;; Per-node extras must stay CHEAP: notes/refile/options scans the whole
;; vault. Putting that form on every node (N × vault size) blew the 100k
;; eval budget on inbox (~20 kids). Toolbar owns refile (one options list);
;; per-node menu only gets extract (fixed size) — eval stays toolbar/partial.
(define outline/extra-actions
  (lambda (slug nid)
    (notes/refile/extract-form slug nid))
  "Hook: per-node extra outline actions (extract).")

(define notes/ui/handle-post
  (lambda ()
    (let ((action (as-str (request-param "action"))))
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
             (if (null? (request-param "folder"))
                 ""
                 (request-param "folder")))
           (redirect
             (string-append "/notes?slug="
               (as-str (request-param "slug"))))))
        ((string-eq action "save")
         (notes/ui/do-save (request-param "slug") (request-param "outline")))
        ((string-eq action "capture")
         (notes/capture/handle))
        ((string-eq action "delete")
         (notes/ui/do-delete))
        ((string-eq action "refile")
         (notes/refile/handle))
        ((string-eq action "extract")
         (notes/refile/extract-handle))
        (else (redirect "/notes")))))
  "Dispatch notes UI POST actions.")

(define notes/page
  (lambda ()
    (if (string-eq (request-method) "POST")
        (notes/ui/handle-post)
        (let ((slug (request-param "slug")))
          (if (if (null? slug) #t (string-eq (as-str slug) ""))
              (html/page (list (notes/ui/index)))
              (html/page (list (notes/ui/show (as-str slug))))))))
  "GET/POST /notes main handler.")
(define notes/outline/partial
  (lambda ()
    (let ((n (notes/doc/get (request-param "slug")))
          (show-archived (notes/doc/show-archived?)))
      (if (null? n)
          (render-html (html/p* (html/attr "class" "err") "missing"))
          (render-html
            (outline/html (as-str (request-param "slug"))
              (notes/doc/outline n) show-archived)))))
  "htmx partial for outline region.")

;; --- node op handler ---

(define notes/outline/new-node
  (lambda ()
    (outline/node (gensym "n") "New node"))
  "Construct a fresh outline child node.")

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
      ((string-eq op "promote")
       (if (string-eq (as-str nid) "root")
           tree
           (outline/promote tree nid)))
      ((string-eq op "demote")
       (if (string-eq (as-str nid) "root")
           tree
           (outline/demote tree nid)))
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
      ((string-eq op "slurp-forward")
       (if (string-eq (as-str nid) "root")
           tree
           (car (structed/slurp-forward outline/kids outline/set-kids tree nid))))
      ((string-eq op "barf-forward")
       (if (string-eq (as-str nid) "root")
           tree
           (car (structed/barf-forward outline/kids outline/set-kids tree nid))))
      (else tree)))
  "Apply named outline op to note tree and save.")

;; apply-op-sel: like apply-op but returns (list tree' selection') so the
;; POST handler can return the new selection authoritatively (op-protocol).
;; Selection semantics:
;;   add-child / add-sibling -> the new node
;;   delete                  -> next sibling of old position, else prev, else parent
;;   splice                  -> first lifted child, else parent
;;   everything else         -> stays on nid
;; Unknown op / no-op -> (list tree nid).
;; selection-after-delete: next sib of old pos, else prev, else parent.
(define notes/outline/sel-after-delete
  (lambda (tree nid)
    (let ((parent (structed/find-parent outline/kids tree nid)))
      (if (null? parent) nid
        (notes/outline/sel-after-delete-in parent (outline/kids parent)
          (structed/sibling-index outline/kids tree nid)))))
  "Selection id after deleting nid.")

(define notes/outline/sel-after-delete-in
  (lambda (parent sibs i)
    (let ((after (structed/list/drop sibs (+ i 1))))
      (if (not (null? after)) (structed/id (car after))
        (if (> i 0) (structed/id (structed/list/nth sibs (- i 1)))
          (structed/id parent)))))
  "Pick next/prev/parent given parent, sibs, index of deleted node.")

;; selection after add-child: the new (first) child of nid.
(define notes/outline/sel-after-add-child
  (lambda (new-tree nid)
    (let ((node (structed/find outline/kids new-tree nid)))
      (if (null? node) nid
        (let ((ks (outline/kids node)))
          (if (null? ks) nid (structed/id (structed/list/nth ks 0)))))))
  "Selection id after add-child: the new first child.")

;; selection after add-sibling: the new sibling after nid.
(define notes/outline/sel-after-add-sibling
  (lambda (new-tree nid)
    (let ((parent (structed/find-parent outline/kids new-tree nid)))
      (if (null? parent) nid
        (let ((sibs (outline/kids parent)))
          (let ((i (structed/sibling-index outline/kids new-tree nid)))
            (if (>= (+ i 1) (structed/count sibs)) nid
              (structed/id (structed/list/nth sibs (+ i 1)))))))))
  "Selection id after add-sibling: the new next sibling.")

;; apply-op-sel: like apply-op but returns (list tree' selection') so the
;; POST handler can return the new selection authoritatively (op-protocol).
(define notes/outline/apply-op-sel
  (lambda (slug tree op nid)
    (let ((new-tree (notes/outline/apply-op slug tree op nid)))
      (let ((sel (notes/outline/sel-for op nid new-tree tree)))
        (list new-tree sel))))
  "Apply op and return (list tree' selection') for the op-protocol.")

(define notes/outline/sel-for
  (lambda (op nid new-tree old-tree)
    (cond
      ((string-eq op "delete")
       (if (string-eq (as-str nid) "root") nid
         (notes/outline/sel-after-delete old-tree nid)))
      ((string-eq op "add-child") (notes/outline/sel-after-add-child new-tree nid))
      ((string-eq op "add-sibling") (notes/outline/sel-after-add-sibling new-tree nid))
      (else nid)))
  "Compute the post-op selection id per op-protocol semantics.")

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
      (notes/doc/list)))
  "All todo nodes across notes as tuples.")

(define notes/outline/open-todos
  (lambda ()
    (list/filter
      (lambda (t) (outline/todo-open? (car (cdr (cdr (cdr t))))))
      (notes/outline/all-todos)))
  "Open (actionable) todos across notes.")

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
              (notes/doc/upsert slug title new-tree tags kind day))))))
  "Cycle todo state on node in note slug.")

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
                    (__notes_eval_fn node prev (as-str slug)))))))))
  "Eval expr with self/doc/slug bound; return value.")

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
                (outline/add-child-by-id tree nid child))))))
  "Splice eval result into outline at node.")

(define notes/outline/node-op-eval
  (lambda (tree nid slug)
    (let ((et (notes/outline/splice-eval tree nid
              (notes/outline/eval-target)
              (notes/outline/eval-result slug nid (request-param "expr")))))
      (list et nid)))
  "Build (list tree sel) for the eval op.")

(define notes/outline/eval-target
  (lambda ()
    (let ((tg (request-param "target")))
      (if (if (null? tg) #t (string-eq (as-str tg) "")) "child" (as-str tg))))
  "eval op target: child (default) | sibling | body.")

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
              (let ((res (if (string-eq op "eval")
                             (notes/outline/node-op-eval tree nid slug)
                             (notes/outline/apply-op-sel slug tree op nid))))
                (let ((new-tree (car res))
                      (sel (as-str (car (cdr res)))))
                  (seq
                    (notes/doc/upsert slug title new-tree tags kind day)
                    (render-html
                      (string-append
                        (outline/html slug new-tree show-archived)
                        (html/void "input"
                          (html/attrs
                            (list
                              (html/attr "type" "hidden")
                              (html/attr "id" "ol-selected-id")
                              (html/attr "hx-swap-oob" "true")
                              (html/attr "value" sel))))))))))))))
  "HTTP node-op entry: apply outline mutation; returns #outline + OOB selection.")


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
    "- When asked for JSON, reply with ONE JSON object only (no markdown fences).\\n")
  "Default wiki entity schema prompt fragment.")

;; Plain LLM completion (no agent tools). tools=#f forces a single text turn.
(define wiki/ask
  (lambda (prompt)
    (let ((sid
            (agent-session-new
              "You help maintain a personal knowledge wiki. Reply with plain text or JSON as asked. Do not call tools."
              #f)))
      (agent-say sid (as-str prompt))))
  "Plain LLM completion without agent tools.")

(define wiki/schema
  (lambda ()
    (let ((n (notes/doc/get "wiki-schema")))
      (if (null? n)
          (seq
            (notes/doc/upsert-in "wiki-schema" "Wiki Schema"
              (outline/node* "root" "Wiki Schema" wiki/default-schema "" (list))
              "schema" "schema" "" "wiki")
            wiki/default-schema)
          (outline/body (notes/doc/outline n)))))
  "Active wiki schema string.")

(define wiki/now
  (lambda ()
    (as-str (date-today)))
  "Current timestamp string for wiki events.")

(define wiki/log-event
  (lambda (kind op slug summary)
    (create 'wiki-event
      (dict-set* "{}"
        (list
          "kind" (as-str kind)
          "op" (as-str op)
          "slug" (as-str slug)
          "ts" (wiki/now)
          "summary" (as-str summary)))))
  "Append wiki pipeline event row.")

(define wiki/events
  (lambda ()
    (rows 'wiki-event 100))
  "Recent wiki event rows.")

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
        (wiki/events))))
  "Drop old wiki events beyond limit.")

;; Extract a JSON object substring from LLM text (first { ... last }).
(define wiki/extract-json
  (lambda (s)
    (let ((t (as-str s)))
      (wiki/extract-json-scan t 0 () ())))
  "Extract first JSON object substring from LLM text.")

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
                  (wiki/extract-json-scan s (+ i 1) start end))))))
  "Scan helper for wiki/extract-json.")

(define wiki/parse-plan
  (lambda (text)
    (json-parse (wiki/extract-json text)))
  "Parse wiki plan structure from JSON text.")

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
                    (list)))))))
  "List entity maps from parsed plan.")

(define wiki/slugify
  (lambda (title)
    (let ((t (string-trim (as-str title))))
      (if (string-eq t "")
          (string-append "wiki-" (as-str (count (notes/doc/list))))
          (string-append "w-"
            (substring t 0
              (if (< (string-length t) 32) (string-length t) 32))))))
  "Slugify string for wiki page names.")

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
          (notes/doc/upsert-in slug title tree tags kind day
            (if (null? prev)
                (if (string-eq kind "source") "wiki/sources" "wiki/entities")
                (notes/doc/folder prev)))
          ;; stamp frontmatter after upsert (fields not in make-fields)
          (let ((row (notes/doc/row slug)))
            (if (null? row)
                slug
                (seq
                  (update-field 'note (car row) "updated_at" (wiki/now))
                  (update-field 'note (car row) "updated_by" "llm")
                  (update-field 'note (car row) "source_count" sc)
                  slug)))))))
  "Upsert a wiki note page from entity fields.")

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
          (append touched (list s))))))
  "Apply one planned entity to the wiki corpus.")

(define wiki/apply-entities
  (lambda (ents source-slug touched)
    (if (null? ents)
        touched
        (wiki/apply-entities (cdr ents) source-slug
          (wiki/apply-entity (car ents) source-slug touched))))
  "Apply list of entities via wiki/apply-entity.")

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
      "Create 1-5 entities/concepts. Bodies may include [[slug]] links.\\n"))
  "Build LLM prompt for wiki ingest from source text.")

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
                    (list src-slug touched raw))))))))))
  "Ingest source text into wiki via LLM plan.")

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
      (notes/doc/index-list)))
  "Index summary of wiki corpus notes.")

(define wiki/query-prompt
  (lambda (question)
    (string-append
      (wiki/schema)
      "\\n\\nCorpus index:\\n" (wiki/corpus-index)
      "\\n\\nQuestion: " (as-str question) "\\n\\n"
      "Answer using the corpus. Cite pages as [[slug]]. "
      "If you create a synthesis, say so. Plain prose is fine.\\n"))
  "Build LLM prompt for wiki query.")

(define wiki/query
  (lambda (question)
    (let ((answer (wiki/ask (wiki/query-prompt question))))
      (seq
        (wiki/log-event "query" "wiki/query" ""
          (substring (as-str question) 0
            (if (< (string-length (as-str question)) 80)
                (string-length (as-str question))
                80)))
        answer)))
  "Answer query against wiki corpus via LLM.")

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
      "Be concise. Use bullet lists.\\n"))
  "Build LLM prompt for wiki lint.")

(define wiki/lint
  (lambda ()
    (let ((report (wiki/ask (wiki/lint-prompt))))
      (seq
        (wiki/log-event "lint" "wiki/lint" "" "lint pass")
        report)))
  "Lint wiki corpus via LLM; return findings.")


(route "/notes" "notes/page")
(route "/capture" "notes/capture/page")
(route "/partials/notes-outline" "notes/outline/partial")
(route "/partials/notes-node-op" "notes/outline/node-op")
