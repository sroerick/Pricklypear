;; package notes/refile -- exported from live branch admin/personal
;; Sync: scripts/pp_sync.py pull|push

(define notes/refile
  (lambda
    (src-slug nid dst-slug)
    "Move node nid from src-slug to the root of dst-slug; #t on success."
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

(define notes/refile/extract
  (lambda
    (src-slug nid)
    "Detach node nid as its own note and return its slug."
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
    "HTML extract button for node nid, or empty for the root."
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
    "Handle the extract POST and redirect to the new or source note."
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
    "HTML refile form for node nid, or empty for the root."
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
    "Handle the refile POST by moving the node then redirecting."
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
        (redirect (string-append "/notes?slug=" src))))))

(define notes/refile/move
  (lambda
    (src-slug nid dst-slug dst-nid)
    "Move node nid to parent dst-nid within dst-slug; #t on success."
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
                          #t)))))))))))))

(define notes/refile/options
  (lambda
    (src-slug)
    "HTML option list of all notes, excluding src-slug."
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
    "True when c is an ASCII letter or digit."
    (string-contains?
      "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
      (as-str c))))

(define notes/refile/slugify
  (lambda
    (title)
    "Turn a title into a slug, -joined and truncated to 48 chars."
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
    "Accumulator for slugify, inserting - and coalescing separators."
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
    "Return base if unused, else append a gensym suffix."
    (if
      (null? (notes/doc/row (as-str base)))
      (as-str base)
      (notes/refile/unique-slug
        (string-append (as-str base) "-" (as-str (gensym "x")))))))

