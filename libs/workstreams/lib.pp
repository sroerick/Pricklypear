

(define workstreams/card-link
  (lambda (id label)
    "A card-detail anchor (escaped body)."
    (html/a (string-append "/todos?id=" (as-str id)) (html/text label))))

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

(define workstreams/counts-line
  (lambda (counts)
    "'CODE 2 · LOGIC-REVIEW 1 · HUMAN-REVIEW 3' from (col . n) pairs."
    (workstreams/join-dot
      (list/map
        (lambda (p)
          (string-append (car p) " " (as-str (cdr p))))
        counts))))

(define workstreams/date-prefix
  (lambda (s)
    "The YYYY-MM-DD prefix of a PG timestamptz string (or the string itself).
     Uniform 10-char dates compare lexicographically."
    (let ((t (as-str s)))
      (if (>= (string-length t) 10) (substring t 0 10) t))))

(define workstreams/in-column?
  (lambda (col n)
    (string-eq (todo/item-state (dict-get n "body")) (as-str col))))

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

(define workstreams/join-comma
  (lambda
    (parts)
    (if
      (null? parts)
      ""
      (if
        (null? (cdr parts))
        (car parts)
        (string-append
          (car parts)
          ", "
          (workstreams/join-comma (cdr parts))))))
  "Join strings with ', ' — used by the status-map JSON emitter.")

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
        (cdr parts)))));; package workstreams -- the LIVE orchestration face
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

(define workstreams/map-lookup
  (lambda
    (k al)
    (if
      (null? al)
      ()
      (if
        (string-eq (as-str (car (car al))) (as-str k))
        (cdr (car al))
        (workstreams/map-lookup k (cdr al)))))
  "Value at key k in an assoc list of (col . status), else ().")

(define workstreams/node-id
  (lambda (n)
    "Card id (redashed path tail) from a tree node-JSON. Uses the PATH, not
     the body 'id' field — the two agree only for items, and the path is
     canonical everywhere."
    (todo/tree/redash (todo/tree/tail (dict-get n "path")))))

(define workstreams/node-title
  (lambda (n) (as-str (dict-get (dict-get n "body") "title"))))

(define workstreams/old?
  (lambda (created threshold)
    "True when the created date is <= threshold (stale)."
    (string<=? (workstreams/date-prefix created) threshold)))

(define workstreams/open-items
  (lambda (list-id)
    (list/filter workstreams/open-node? (workstreams/item-nodes list-id))))

(define workstreams/open-node?
  (lambda (n)
    "Open (non-terminal) card node?"
    (todo/state/open? (todo/item-state (dict-get n "body")))))

(define workstreams/page
  (lambda ()
    "GET /workstreams renders the live orchestration face."
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

(define workstreams/pulse-count-cell
  (lambda
    (lid count)
    (if
      (> (cdr count) 0)
      (html/tag
        "td"
        (html/attr "class" "pulse-n")
        (html/a
          (string-append "/kanban?list=" lid)
          (html/text (as-str (cdr count)))))
      (html/tag
        "td"
        (html/attr "class" "muted pulse-n")
        (html/text "·"))))
  "One count cell: link into the board when non-zero, muted dash when 0.")

(define workstreams/pulse-nodes
  (lambda
    ()
    (tree/list (todo/tree/prefix "todo-item") 1 2000))
  "ONE garden scan for the whole face: the caller's todo-item nodes.\n     Cards live in their creator's garden, so an identity sees its own\n     cards plus shared-space cards it can read — by the privacy model.")

(define workstreams/pulse-page
  (lambda
    ()
    (let
      ((streams (workstreams/stream-rows)))
      (let
        ((nodes (workstreams/pulse-nodes)))
        (let
          ((rows
            (list/map
              (lambda
                (s)
                (workstreams/pulse-row-from nodes (car s) (cdr s)))
              streams)))
          (html/page
            (list
              (html/h1 (html/text "Pulse"))
              (html/p*
                (html/attr "class" "muted")
                "Every open card you can see, grouped by the shared vocabulary. Each stream keeps its own real pipeline — open its board for the native columns and the add/move forms.")
              (workstreams/pulse-summary rows)
              (workstreams/pulse-table rows)
              (workstreams/pulse-rollup-from nodes streams)
              (html/p*
                (html/attr "class" "muted ws-board-link")
                (html/a "/workstreams" "per-stream detail"))))))))
  "GET /pulse — the cross-stream indicator. ONE scan feeds the whole\n     page: counts, stale flags, and the HUMAN-REVIEW rollup all derive\n     from the same node list.")

(define workstreams/pulse-rollup-from
  (lambda
    (nodes streams)
    (let
      ((titles
        (list/map
          (lambda (s) (cons (as-str (car s)) (as-str (cdr s))))
          streams)))
      (let
        ((hits
          (list/filter
            (lambda (c) (not (null? (cdr c))))
            (list/map
              (lambda
                (n)
                (cons
                  (workstreams/node-id n)
                  (workstreams/map-lookup
                    (as-str (dict-get (dict-get n "body") "list_id"))
                    titles)))
              (list/filter
                (lambda
                  (n)
                  (and
                    (workstreams/open-node? n)
                    (string-eq
                      (todo/item-state (dict-get n "body"))
                      "HUMAN-REVIEW")))
                nodes)))))
        (html/div
          (html/attr "class" "ws-rollup")
          (html/join
            (list
              (html/h2 (html/text "Ready for human review"))
              (if
                (null? hits)
                (html/p*
                  (html/attr "class" "muted")
                  "Nothing in HUMAN-REVIEW right now.")
                (html/ul
                  ""
                  (html/join
                    (list/map
                      (lambda
                        (c)
                        (html/li
                          ""
                          (html/join
                            (list
                              (workstreams/card-link (car c) (car c))
                              (html/span
                                (html/attr "class" "muted")
                                (html/text (string-append " - " (cdr c))))))))
                      hits)))))))))))

(define workstreams/pulse-row-from
  (lambda
    (nodes lid title)
    (let
      ((override (workstreams/status-map-for-list lid))
        (mine
          (list/filter
            (lambda
              (n)
              (string-eq
                (as-str (dict-get (dict-get n "body") "list_id"))
                (as-str lid)))
            nodes)))
      (let
        ((open-n (list/filter workstreams/open-node? mine))
          (stale-th (workstreams/stale-threshold)))
        (list
          title
          lid
          (list/map
            (lambda
              (st)
              (cons
                st
                (workstreams/len
                  (list/filter
                    (lambda
                      (n)
                      (string-eq
                        (workstreams/status-resolve
                          override
                          (todo/item-state (dict-get n "body")))
                        st))
                    open-n))))
            (workstreams/status-vocab))
          (workstreams/len
            (list/filter
              (lambda (n) (workstreams/stale-node? n stale-th))
              open-n))
          (workstreams/len open-n))))))

(define workstreams/pulse-summary
  (lambda
    (rows)
    (let
      ((open-n
          (list/foldl
            (lambda (acc r) (+ acc (list-ref r 4)))
            0
            rows))
        (stale-n
          (list/foldl
            (lambda (acc r) (+ acc (list-ref r 3)))
            0
            rows))
        (review-n
          (list/foldl
            (lambda
              (acc r)
              (+ acc (workstreams/map-lookup "review" (list-ref r 2))))
            0
            rows)))
      (html/p*
        (html/attr "class" "muted pulse-summary")
        (string-append
          (as-str open-n)
          " open across "
          (as-str (workstreams/len rows))
          " streams · "
          (as-str review-n)
          " in review · "
          (as-str stale-n)
          " stale"))))
  "One line: N open across M streams · K stale · R in review.")

(define workstreams/pulse-table
  (lambda
    (rows)
    (html/tag
      "table"
      (html/attr "class" "pulse")
      (html/join
        (list
          (html/tag
            "tr"
            ""
            (html/join
              (append
                (list (html/tag "th" "" (html/text "stream")))
                (list/map
                  (lambda (s) (html/tag "th" "" (html/text s)))
                  (workstreams/status-vocab))
                (list (html/tag "th" "" (html/text "stale"))))))
          (html/join (list/map workstreams/pulse-tr rows))))))
  "The overview: one row per visible stream, coarse-status columns.\n     Takes the precomputed rows (single-pass).")

(define workstreams/pulse-tr
  (lambda
    (row)
    (let
      ((title (list-ref row 0))
        (lid (list-ref row 1))
        (counts (list-ref row 2))
        (stale (list-ref row 3))
        (open-n (list-ref row 4)))
      (html/tag
        "tr"
        ""
        (html/join
          (list
            (html/tag
              "th"
              (html/attr "class" "pulse-stream")
              (html/join
                (list
                  (html/a
                    (string-append "/kanban?list=" lid)
                    (html/text title))
                  (html/span
                    (html/attr "class" "muted")
                    (html/text
                      (string-append " (" (as-str open-n) " open)"))))))
            (html/join
              (list/map
                (lambda (c) (workstreams/pulse-count-cell lid c))
                counts))
            (html/tag
              "td"
              (html/attr
                "class"
                (if (> stale 0) "pulse-stale" "muted pulse-stale0"))
              (html/text
                (if
                  (> stale 0)
                  (string-append (as-str stale) " stale")
                  "·"))))))))
  "One stream's table row: title | queued active review parked | stale.")

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

(define workstreams/stale-items
  (lambda (list-id)
    "Open cards created before the 3-day threshold (oldest first)."
    (let ((t (workstreams/stale-threshold)))
      (list/filter
        (lambda (n)
          (let ((c (dict-get n "created_at")))
            (if (null? c) #f (workstreams/old? c t))))
        (workstreams/open-items list-id)))))

(define workstreams/stale-threshold
  (lambda () (as-str (date-add-days -3))))

(define workstreams/status-map-default
  (lambda
    ()
    (list
      (cons "" "queued")
      (cons "TODO" "queued")
      (cons "SPEC" "active")
      (cons "CODE" "active")
      (cons "WAITING" "active")
      (cons "LOGIC-REVIEW" "review")
      (cons "UI-REVIEW" "review")
      (cons "HUMAN-REVIEW" "review")
      (cons "PARKED" "parked")
      (cons "DONE" "done")
      (cons "CANCELLED" "done")
      (cons "ARCHIVED" "done")))
  "Column -> coarse status for the STANDARD columns. Covers the shared\n     Board pipeline, the todo default states, and PARKED. Lists with\n     unusual columns either set a `status-map` override or their open\n     items read as active.")

(define workstreams/status-map-for-list
  (lambda
    (list-id)
    (let
      ((lf (get (quote todo-list) (as-str list-id))))
      (if
        (null? lf)
        ()
        (let
          ((sm (dict-get lf "status-map")))
          (if
            (or (null? sm) (string-eq (as-str sm) ""))
            ()
            (let
              ((d (json-parse (as-str sm))))
              (list/map
                (lambda (k) (cons (as-str k) (as-str (dict-get d k))))
                (dict-keys d))))))))
  "The per-list override as an assoc list of (col . status), read from\n     the list's `status-map` field (a JSON object string). () when unset.")

(define workstreams/status-map-set
  (lambda
    (list-id pairs)
    (let
      ((lf (get (quote todo-list) list-id)))
      (if
        (null? lf)
        "no such list"
        (let
          ((path (node-path (quote todo-list) list-id)))
          (let
            ((json
              (string-append
                "{"
                (workstreams/join-comma
                  (list/map
                    (lambda
                      (p)
                      (string-append
                        (json-emit (as-str (car p)))
                        ":"
                        (json-emit (as-str (cdr p)))))
                    pairs))
                "}")))
            (let
              ((body (dict-set* lf (list "status-map" json))))
              (tree/upsert path "datum" "todo-list" body)))))))
  "tree/upsert the LIST datum with a `status-map` override (pairs is an\n     assoc list of (column . coarse-status)). Mirrors kanban/columns-set.\n     Returns the upserted path.")

(define workstreams/status-resolve
  (lambda
    (override col)
    (let
      ((own (workstreams/map-lookup col override)))
      (if
        (null? own)
        (let
          ((def
            (workstreams/map-lookup
              col
              (workstreams/status-map-default))))
          (if (null? def) "active" (as-str def)))
        (as-str own))))
  "Coarse status for column col: the list's override map, else the\n     default map, else active (an open column we have no opinion about).")

(define workstreams/status-vocab
  (lambda () (list "queued" "active" "review" "parked"))
  "The coarse-status columns of /pulse, in display order. Open items\n     only — done work shows in the standup, not the indicator.")

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

;; ---------------------------------------------------------------
;; projects — Basecamp-style grouping above lists
;; spec: borge/workstreams.borg projects-plan; card 4a73947e.
;; A project is a first-class datum (type: project — never
;; todo-project: type names bake into row paths and agent context;
;; the coupling to todos lives in the pointer, list.project_id, not
;; in the project's name). Items inherit the project through their
;; list. The uuid is the identity; the path is the address.

(define workstreams/projects-init!
  (lambda
    ()
    "Ensure the project datum type exists. Guarded so a re-load is safe
     and a non-admin load degrades to a no-op value (the
     todo/comments-init! pattern); boot seeds types onto system/main as
     admin. A delegated session also no-ops here by design."
    (if
      (null? (type-fields (quote project)))
      (deftype
        (quote project)
        (quote title)
        (quote slug)
        (quote owner)
        (quote order)
        (quote color)
        (quote archived)
        (quote notes))
      "project ok")))

(define workstreams/projects-all
  (lambda
    ()
    "Project datums the caller can see: garden rows + shared-space rows
     merged by id, garden first (the todo/list/all dual-scan pattern).
     Each is (id . fields)."
    (todo/list/merge
      (todo/tree/rows "project" 200)
      (list/map
        todo/tree/node->pair
        (list/filter
          (lambda
            (nj)
            (string-eq (as-str (dict-get nj "type")) "project"))
          (tree/list (todo/tree/shared-prefix) 3 5000))))))

(define workstreams/stream-fields
  (lambda
    ()
    "Every non-default workstream as (id . fields): the full datum, not
     just the title — the face needs project_id per stream. Board is
     included like any other stream; without a project_id it renders
     under intake, which is the Board's job anyway."
    (list/filter
      (lambda (row) (not (todo/list/default? (cdr row))))
      (todo/list/all))))

(define workstreams/project-name
  (lambda
    (projects pid)
    "Project display title for a project_id, or () when ungrouped."
    (if
      (or (null? pid) (string-eq (as-str pid) ""))
      ()
      (let
        ((hit
          (list/filter
            (lambda (p) (string-eq (as-str (car p)) (as-str pid)))
            projects)))
        (if (null? hit) () (as-str (dict-get (cdr (car hit)) "title")))))))

(define workstreams/project-section
  (lambda
    (name rows)
    "One project block: heading + the pulse stream table for its rows.
     Returns () when the project has no visible streams, so sections
     compose with append and empty ones vanish."
    (let
      ((mine
        (list/filter
          (lambda (r) (string-eq (as-str (car r)) name))
          rows)))
      (if
        (null? mine)
        ()
        (list
          (html/h2 (html/text name))
          (workstreams/pulse-table (list/map cdr mine)))))))

(define workstreams/projects-sections
  (lambda
    (projects rows)
    "One section per project datum that has visible streams."
    (list/filter
      (lambda (sec) (not (null? sec)))
      (list/map
        (lambda
          (p)
          (workstreams/project-section
            (as-str (dict-get (cdr p) "title"))
            rows))
        projects))))

(define workstreams/intake-section
  (lambda
    (rows)
    "The intake block: every stream with NO project, rendered last.
     Ungrouped rows carry () as their project name — filtering by the
     literal string \"intake\" matches nothing (found by the live test:
     the page rendered 200 with zero sections)."
    (let
      ((mine
        (list/filter
          (lambda (r) (or (null? (car r)) (string-eq (as-str (car r)) "")))
          rows)))
      (if
        (null? mine)
        ()
        (list
          (html/h2 (html/text "intake"))
          (workstreams/pulse-table (list/map cdr mine)))))))

(define workstreams/projects-table
  (lambda
    (projects rows)
    "Project sections, then intake (ungrouped streams) last. Rows are
     (project-name . pulse-row); the pulse-row shape is reused whole, so
     the stream/count table stays byte-identical to /pulse."
    (html/join
      (append
        (apply append (workstreams/projects-sections projects rows))
        (workstreams/intake-section rows)))))

(define workstreams/projects-page
  (lambda
    ()
    "NOTE: projects-init! is NOT called here on purpose — a delegated
     render would abort on the deftype denial. The type is declared at
     boot (the top-level init! call below, boot-admin context) and the
     face renders fine with an undeclared type: reads return no rows and
     the intake section still groups every stream."
    (let
      ((projects (workstreams/projects-all)))
      (let
        ((streams (workstreams/stream-fields)))
        (let
          ((nodes (workstreams/pulse-nodes)))
          (let
            ((rows
              (list/map
                (lambda
                  (s)
                  (cons
                    (workstreams/project-name
                      projects
                      (dict-get (cdr s) "project_id"))
                    (workstreams/pulse-row-from
                      nodes
                      (car s)
                      (as-str (dict-get (cdr s) "title")))))
                streams)))
            (html/page
              (list
                (html/h1 (html/text "Projects"))
                (html/p*
                  (html/attr "class" "muted")
                  "Streams grouped by project. A stream without a project renders under intake — the Board stays the cross-project queue. ONE scan feeds the page: the pulse one-scan pattern.")
                (workstreams/projects-table projects rows)
                (html/p*
                  (html/attr "class" "muted ws-board-link")
                  (html/a "/pulse" "the flat indicator")))))))))
  "GET /projects — project -> streams -> coarse-status counts, reusing
     the pulse row machinery whole. Same single-pass model: one item
     scan, one list scan, one indexed project scan; grouped by
     list.project_id. A stream joins a project with one field write.")
