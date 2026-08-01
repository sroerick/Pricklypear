;; package roam -- exported from live branch admin/personal
;; Sync: scripts/pp_sync.py pull|push

(define roam/at-open?
  (lambda
    (s i)
    (if
      (>= i (- (string-length s) 3))
      #f
      (if
        (string-eq (substring s i 1) "[")
        (string-eq (substring s (+ i 1) 1) "[")
        #f))))

(define roam/backlinks
  (lambda
    (slug)
    (let
      ((target (as-str slug)) (tu (roam/note-uuid slug)))
      (list/map
        (lambda
          (row)
          (let
            ((f (cdr row)))
            (let
              ((fs (as-str (dict-get f "from_slug")))
                (fid (as-str (dict-get f "from_id")))
                (lab (as-str (dict-get f "label"))))
              (let
                ((src (notes/doc/get fs)))
                (list
                  fs
                  fid
                  (if (null? src) fs (notes/doc/title src))
                  lab)))))
        (if
          (string-eq tu "")
          (find-rows (quote note-link) "to_slug" target 200)
          (let
            ((by-uuid (find-rows (quote note-link) "to_uuid" tu 200)))
            (if
              (null? by-uuid)
              (find-rows (quote note-link) "to_slug" target 200)
              by-uuid)))))))

(define roam/backlinks-html
  (lambda
    (slug)
    (let
      ((bls (roam/backlinks slug)))
      (if
        (null? bls)
        (html/p*
          (html/attr "class" "muted")
          "No backlinks yet.")
        (html/ul
          (html/attr "class" "roam-backlinks")
          (html/join
            (list/map
              (lambda
                (b)
                (html/li
                  ""
                  (html/join
                    (list
                      (html/a
                        (string-append "/notes?slug=" (car b))
                        (html/text (car (cdr (cdr b)))))
                      (html/span
                        (html/attr "class" "muted")
                        (html/text (string-append " · " (car (cdr b)))))))))
              bls)))))))

(define roam/collect-from-tree
  (lambda (tree) (roam/collect-node tree (list))))

(define roam/collect-node
  (lambda
    (n acc)
    (let
      ((id (as-str (outline/id n)))
        (links (roam/parse-links (outline/body n))))
      (let
        ((acc1
          (list/foldl
            (lambda
              (a pair)
              (cons (list id (car pair) (car (cdr pair))) a))
            acc
            links)))
        (list/foldl
          (lambda (a k) (roam/collect-node k a))
          acc1
          (outline/kids n))))))

(define roam/delete-from-slug
  (lambda
    (slug)
    (list/foldl
      (lambda
        (_ row)
        (seq (delete (quote note-link) (car row)) 0))
      0
      (find-rows
        (quote note-link)
        "from_slug"
        (as-str slug)
        500))))

(define roam/find-close
  (lambda
    (s i)
    (if
      (>= i (- (string-length s) 1))
      ()
      (if
        (if
          (string-eq (substring s i 1) "]")
          (string-eq (substring s (+ i 1) 1) "]")
          #f)
        i
        (roam/find-close s (+ i 1))))))

(define roam/insert-collected
  (lambda
    (from-slug triples seen)
    (let
      ((from-uuid (roam/note-uuid from-slug)))
      (roam/insert-collected-acc
        from-slug
        from-uuid
        triples
        seen))))

(define roam/insert-collected-acc
  (lambda
    (from-slug from-uuid triples seen)
    (if
      (null? triples)
      seen
      (let
        ((t (car triples)))
        (let
          ((fid (as-str (car t)))
            (to (as-str (car (cdr t))))
            (lab (as-str (car (cdr (cdr t))))))
          (let
            ((key (string-append fid "\t" to)))
            (if
              (roam/media-slug? to)
              (roam/insert-collected-acc
                from-slug
                from-uuid
                (cdr triples)
                seen)
              (if
                (roam/seen? seen key)
                (roam/insert-collected-acc
                  from-slug
                  from-uuid
                  (cdr triples)
                  seen)
                (let
                  ((to-uuid (roam/note-uuid to)))
                  (seq
                    (create
                      (quote note-link)
                      (dict-set*
                        "{}"
                        (list
                          "from_slug"
                          (as-str from-slug)
                          "from_uuid"
                          (as-str from-uuid)
                          "from_id"
                          fid
                          "to_slug"
                          to
                          "to_uuid"
                          (as-str to-uuid)
                          "to_id"
                          ""
                          "label"
                          lab
                          "kind"
                          "wikilink")))
                    (roam/insert-collected-acc
                      from-slug
                      from-uuid
                      (cdr triples)
                      (cons key seen))))))))))))

(define roam/media-id
  (lambda
    (slug)
    (let
      ((s (as-str slug)))
      (substring s 6 (- (string-length s) 6)))))

(define roam/media-slug?
  (lambda
    (slug)
    (let
      ((s (as-str slug)))
      (if
        (< (string-length s) 7)
        #f
        (string-eq (substring s 0 6) "media:")))))

(define roam/note-uuid
  (lambda
    (slug)
    (let
      ((row (notes/doc/row (as-str slug))))
      (if (null? row) "" (as-str (car row))))))

(define roam/parse-links
  (lambda (body) (extract-links (as-str body))))

(define roam/parse-links-acc
  (lambda
    (s i acc)
    (if
      (>= i (- (string-length s) 3))
      (reverse acc)
      (if
        (if
          (string-eq (substring s i 1) "[")
          (string-eq (substring s (+ i 1) 1) "[")
          #f)
        (let
          ((end (roam/find-close s (+ i 2))))
          (if
            (null? end)
            (roam/parse-links-acc s (+ i 1) acc)
            (let
              ((inner (substring s (+ i 2) (- end (+ i 2)))))
              (let
                ((pair (roam/split-label inner)))
                (if
                  (string-eq (car pair) "")
                  (roam/parse-links-acc s (+ end 2) acc)
                  (roam/parse-links-acc s (+ end 2) (cons pair acc)))))))
        (roam/parse-links-acc s (+ i 1) acc)))))

(define roam/reindex
  (lambda
    (slug outline-tree)
    (seq
      (roam/delete-from-slug slug)
      (roam/insert-collected
        slug
        (roam/collect-from-tree outline-tree)
        (list))
      slug)))

(define roam/render-acc
  (lambda
    (s i acc)
    (if
      (>= i (string-length s))
      acc
      (if
        (roam/at-open? s i)
        (let
          ((end (roam/find-close s (+ i 2))))
          (if
            (null? end)
            (roam/render-acc
              s
              (+ i 1)
              (string-append acc (html-escape (substring s i 1))))
            (let
              ((inner (substring s (+ i 2) (- end (+ i 2)))))
              (let
                ((pair (roam/split-label inner)))
                (let
                  ((slug (car pair)) (lab (car (cdr pair))))
                  (if
                    (string-eq slug "")
                    (roam/render-acc
                      s
                      (+ end 2)
                      (string-append
                        acc
                        (html-escape (substring s i (- (+ end 2) i)))))
                    (roam/render-acc
                      s
                      (+ end 2)
                      (string-append acc (roam/render-wiki-or-media slug lab)))))))))
        (roam/render-acc
          s
          (+ i 1)
          (string-append acc (html-escape (substring s i 1))))))))

(define roam/render-body
  (lambda (body) (render-links (as-str body))))

(define roam/render-link
  (lambda
    (slug lab)
    (let
      ((s (as-str slug)) (row (notes/doc/row slug)))
      (let
        ((href-slug
            (if (null? row) s (as-str (dict-get (cdr row) "slug"))))
          (title
            (if
              (null? row)
              (as-str lab)
              (let
                ((t (dict-get (cdr row) "title")))
                (if
                  (or (null? t) (string-eq (as-str lab) s))
                  (if (null? t) (as-str lab) (as-str t))
                  (as-str lab))))))
        (html/a
          (string-append "/notes?slug=" href-slug)
          (html/text title))))))

(define roam/render-media
  (lambda
    (id alt)
    (let
      ((mid (as-str id)) (lab (as-str alt)))
      (let
        ((href (string-append "/media/" mid))
          (label (if (string-eq lab "") mid lab)))
        (html/join
          (list
            (html/a href (html/img href label "note-media"))
            (html/p*
              (html/attr "class" "muted note-media-cap")
              (html/a href (html/text label)))))))))

(define roam/render-wiki-or-media
  (lambda
    (slug lab)
    (if
      (roam/media-slug? slug)
      (roam/render-media (roam/media-id slug) lab)
      (roam/render-link slug lab))))

(define roam/seen?
  (lambda
    (seen key)
    (if
      (null? seen)
      #f
      (if
        (string-eq (car seen) key)
        #t
        (roam/seen? (cdr seen) key)))))

(define roam/split-label
  (lambda
    (inner)
    (let
      ((s (string-trim (as-str inner))))
      (roam/split-label-at s 0))))

(define roam/split-label-at
  (lambda
    (s i)
    (if
      (>= i (string-length s))
      (list s s)
      (if
        (string-eq (substring s i 1) "|")
        (list
          (string-trim (substring s 0 i))
          (string-trim
            (substring s (+ i 1) (- (string-length s) (+ i 1)))))
        (roam/split-label-at s (+ i 1))))))

