;; package workstreams -- the LIVE orchestration face
;; (borge/workstreams.borg live-face; orchestration task 9).
;;
;; `/workstreams` is computed at render time (no cron mirror): per-workstream
;; column counts (open cards only), a 'ready for human review' rollup across
;; streams, 'stale' (open >3d) flags, and links into /kanban?list=<id> + the
;; card detail page (/todos?id=<id>).
;;
;; Reads the user's OWN todo-tree through the todo family helpers. A
;; workstream is a non-default todo-list (Board and the per-stream lists are
;; all streams; the default Inbox is intake, not a stream). The shared Board
;; list is the cross-stream overview and renders like any other stream.
;;
;; Depends on the todo family being bound at CALL time (boot seeds todo
;; before workstreams alphabetically); this lib never calls load-library at
;; render time. Staleness = card created before (date-add-days -3) and still
;; open; the PG timestamptz strings are uniform so a YYYY-MM-DD prefix
;; compare IS chronological.

;; ------------------------------------------------------------------
;; data helpers
;; ------------------------------------------------------------------

(define workstreams/len
  (lambda (l) (list/foldl (lambda (acc _) (+ acc 1)) 0 l)))

(define workstreams/date-prefix
  (lambda (s)
    "The YYYY-MM-DD prefix of a PG timestamptz string (or the string itself).
     Uniform 10-char dates compare lexicographically."
    (let ((t (as-str s)))
      (if (>= (string-length t) 10) (substring t 0 10) t))))

(define workstreams/stale-threshold
  (lambda () (as-str (date-add-days -3))))

(define workstreams/old?
  (lambda (created threshold)
    "True when the created date is <= threshold (stale)."
    (string<=? (workstreams/date-prefix created) threshold)))

(define workstreams/stream-rows
  (lambda ()
    "Every workstream as an (id . title) pair: all todo-lists except the
     default Inbox. Board is included like any other stream."
    (list/map
      (lambda (row)
        (cons
          (as-str (car row))
          (as-str (dict-get (cdr row) "title"))))
      (list/filter
        (lambda (row) (not (todo/list/default? (cdr row))))
        (todo/list/all)))))

(define workstreams/item-nodes
  (lambda (list-id)
    "Node-JSONs (id/path/body/author/created_at/...) of todo-items in list-id.
     Uses tree/list so created_at metadata rides along for staleness."
    (list/filter
      (lambda (n)
        (string-eq
          (as-str (dict-get (dict-get n "body") "list_id"))
          (as-str list-id)))
      (tree/list (todo/tree/prefix "todo-item") 1 500))))

(define workstreams/node-id
  (lambda (n)
    "Card id (redashed path tail) from a tree node-JSON. Uses the PATH, not
     the body 'id' field — the two agree only for items, and the path is
     canonical everywhere."
    (todo/tree/redash (todo/tree/tail (dict-get n "path")))))

(define workstreams/node-title
  (lambda (n) (as-str (dict-get (dict-get n "body") "title"))))

(define workstreams/open-node?
  (lambda (n)
    "Open (non-terminal) card node?"
    (todo/state/open? (todo/item-state (dict-get n "body")))))

(define workstreams/open-items
  (lambda (list-id)
    (list/filter workstreams/open-node? (workstreams/item-nodes list-id))))

(define workstreams/stale-items
  (lambda (list-id)
    "Open cards created before the 3-day threshold (oldest first)."
    (let ((t (workstreams/stale-threshold)))
      (list/filter
        (lambda (n)
          (let ((c (dict-get n "created_at")))
            (if (null? c) #f (workstreams/old? c t))))
        (workstreams/open-items list-id)))))

(define workstreams/count-column
  (lambda (nodes col)
    "How many open cards in nodes currently sit in column col."
    (list/foldl
      (lambda (acc n)
        (if
          (workstreams/in-column? col n)
          (+ acc 1)
          acc))
      0 nodes)))

(define workstreams/in-column?
  (lambda (col n)
    (string-eq (todo/item-state (dict-get n "body")) (as-str col))))

(define workstreams/column-counts
  (lambda (list-id)
    "List of (col . open-count) over the list's own state set, non-zero only."
    (let
      ((nodes (workstreams/open-items list-id))
        (cols (todo/state/columns-for-list list-id)))
      (list/filter
        (lambda (p) (> (cdr p) 0))
        (list/map
          (lambda (c) (cons (as-str c) (workstreams/count-column nodes c)))
          cols)))))

(define workstreams/rollup
  (lambda (col)
    "All open cards currently in column col across every stream: list of
     (card-id . stream-title)."
    (list/foldl
      (lambda (acc stream)
        (append
          acc
          (list/map
            (lambda (n)
              (cons (workstreams/node-id n) (cdr stream)))
            (list/filter
              (lambda (n) (workstreams/in-column? col n))
              (workstreams/open-items (car stream))))))
      (list)
      (workstreams/stream-rows))))

;; ------------------------------------------------------------------
;; html
;; ------------------------------------------------------------------

(define workstreams/join-dot
  (lambda (parts)
    "Join strings with ' · '."
    (if
      (null? parts)
      ""
      (list/foldl
        (lambda (acc p) (string-append acc " · " p))
        (car parts)
        (cdr parts)))))

(define workstreams/counts-line
  (lambda (counts)
    "'CODE 2 · LOGIC-REVIEW 1 · HUMAN-REVIEW 3' from (col . n) pairs."
    (workstreams/join-dot
      (list/map
        (lambda (p)
          (string-append (car p) " " (as-str (cdr p))))
        counts))))

(define workstreams/card-link
  (lambda (id label)
    "A card-detail anchor (escaped body)."
    (html/a (string-append "/todos?id=" (as-str id)) (html/text label))))

(define workstreams/stale-html
  (lambda (stale)
    "The stale listing for one stream ('' when nothing is stale)."
    (if
      (null? stale)
      ""
      (html/div
        (html/attr "class" "ws-stale")
        (html/join
          (list
            (html/p* (html/attr "class" "ws-stale-head") "stale (>3d):")
            (html/ul ""
              (html/join
                (list/map
                  (lambda (n)
                    (html/li ""
                      (workstreams/card-link
                        (workstreams/node-id n)
                        (workstreams/node-title n))))
                  stale)))))))))

(define workstreams/stream-section
  (lambda (stream)
    "One stream: title + board link, non-zero column counts, stale listing."
    (let
      ((lid (car stream))
        (title (cdr stream)))
      (let
        ((nodes (workstreams/open-items lid))
          (counts (workstreams/column-counts lid))
          (stale (workstreams/stale-items lid)))
        (html/div
          (html/attr "class" "workstream")
          (html/join
            (list
              (html/h2
                (html/join
                  (list
                    (html/text title)
                    (html/span (html/attr "class" "muted ws-open")
                      (html/text
                        (string-append
                          " (" (as-str (workstreams/len nodes)) " open)"))))))
              (html/p*
                (html/attr "class" "ws-counts")
                (if
                  (null? counts)
                  "(no open cards in a named column)"
                  (workstreams/counts-line counts)))
              (html/p*
                (html/attr "class" "ws-board-link muted")
                (html/a (string-append "/kanban?list=" lid) "open board"))
              (workstreams/stale-html stale)
              (html/void "hr" ""))))))))

(define workstreams/rollup-html
  (lambda ()
    "The 'ready for human review' rollup across all streams."
    (let ((cards (workstreams/rollup "HUMAN-REVIEW")))
      (html/div
        (html/attr "class" "ws-rollup")
        (html/join
          (list
            (html/h2 (html/text "Ready for human review"))
            (if
              (null? cards)
              (html/p* (html/attr "class" "muted")
                "Nothing in HUMAN-REVIEW right now.")
              (html/ul ""
                (html/join
                  (list/map
                    (lambda (c)
                      (html/li ""
                        (html/join
                          (list
                            (workstreams/card-link (car c) (car c))
                            (html/span (html/attr "class" "muted")
                              (html/text (string-append " · " (cdr c))))))))
                    cards))))))))))

(define workstreams/page
  (lambda ()
    "GET /workstreams renders the live orchestration face."
    (todo/ensure-space)
    (html/page
      (list
        (html/h1 (html/text "Workstreams"))
        (html/p*
          (html/attr "class" "muted")
          "Live counts from the todo-tree; open = not DONE/CANCELLED/ARCHIVED,
           stale = open >3 days. The shared Board list is the overview.")
        (workstreams/rollup-html)
        (html/h2 (html/text "Per stream"))
        (html/join
          (list/map workstreams/stream-section
            (workstreams/stream-rows)))))))

(route "/workstreams" "workstreams/page")
