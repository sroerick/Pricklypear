;; package notes/tree -- exported from live branch admin/personal
;; Sync: scripts/pp_sync.py pull|push

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

(define notes/tree/body
  (lambda
    (node)
    (if (null? node) () (dict-get node "body"))))

(define notes/tree/butlast
  (lambda
    (xs)
    (if
      (null? xs)
      (list)
      (if
        (null? (cdr xs))
        (list)
        (cons (car xs) (notes/tree/butlast (cdr xs)))))))

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
      (tree/list note-path 1))))

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

(define notes/tree/enabled?
  (lambda () #t))

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
          (notes/tree/ensure-chain next (cdr segs)))))))

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
        path))))

(define notes/tree/find-slug
  (lambda
    (slug)
    (let
      ((hits (tree/find-by "note" "slug" (as-str slug) 5)))
      (if
        (null? hits)
        ()
        (notes/tree/node->row (car hits) #t)))))

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
        ""))))

(define notes/tree/heading-kids
  (lambda
    (parent-path)
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
              (let
                ((b (notes/tree/body n)))
                (let
                  ((p (as-str (dict-get n "path"))))
                  (append
                    acc
                    (list
                      (outline/node*
                        (as-str (dict-get b "id"))
                        (as-str (dict-get b "title"))
                        (as-str (dict-get b "body"))
                        (as-str (dict-get b "todo"))
                        (notes/tree/heading-kids p))))))
              acc))))
      (list)
      (tree/list parent-path 1))))

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
          (notes/tree/join-slash (cdr xs)))))))

(define notes/tree/list-notes
  (lambda () (tree/note-index (notes/tree/root) 2000)))

;; Read the root outline body persisted by write-root-body (i-rootbody-1).
(define notes/tree/root-body
  (lambda
    (note-path)
    (let
      ((cur (tree/get note-path)))
      (if
        (null? cur)
        ""
        (let
          ((b (dict-get (notes/tree/body (notes/tree/parse cur)) "body")))
          (if (null? b) "" (as-str b)))))))

(define notes/tree/load-outline
  (lambda
    (note-path title)
    (outline/node*
      "root"
      (as-str title)
      (notes/tree/root-body note-path)
      ""
      (notes/tree/heading-kids note-path))))

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
                        (append fields (list "outline" "[]"))))))))))))))

(define notes/tree/note-path
  (lambda
    (folder slug)
    (let
      ((base
        (notes/tree/ensure-chain
          (notes/tree/root)
          (notes/tree/split-folder folder))))
      (string-append base "." (tree/path-label slug)))))

(define notes/tree/parent-of
  (lambda
    (path)
    (let
      ((pos (string-find-pos path "/" 0 1)))
      (if (< pos 0) "" (substring path 0 pos)))))

(define notes/tree/parse
  (lambda (s) (if (null? s) () (json-parse (as-str s)))))

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

(define notes/tree/root
  (lambda () (tree/garden-root)))

(define notes/tree/save-outline
  (lambda
    (note-path outline)
    (seq
      (notes/tree/clear-headings note-path)
      (notes/tree/write-root-body note-path (outline/body outline))
      (list/foldl
        (lambda
          (acc kid)
          (seq (notes/tree/write-heading note-path kid) acc))
        0
        (outline/kids outline))
      note-path)))

;; Persist the top-level (root) outline node's body onto the note node itself.
;; i-rootbody-1: save-outline previously only wrote child headings, so any
;; body kept on the root node was dropped on upsert. Store it as a "body" key
;; on the note's content and read it back in load-outline/root-body.
(define notes/tree/write-root-body
  (lambda
    (note-path body)
    (let
      ((cur (tree/get note-path)))
      (if
        (null? cur)
        note-path
        (let
          ((n (notes/tree/parse cur))
            (b (notes/tree/body n)))
          (seq
            (tree/upsert
              note-path
              "note"
              "note"
              (dict-set b "body" (as-str body)))
            note-path))))))

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

(define notes/tree/split-folder
  (lambda
    (folder)
    (list/filter
      (lambda (s) (not (string-eq (as-str s) "")))
      (string-split (as-str folder) "/"))))

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
        path))))

