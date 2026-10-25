

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

(define workstreams/column-counts-from
  (lambda (nodes lid)
    "List of (col . open-count) over the list's own state set, non-zero only,
       derived from a PRECOMPUTED whole-garden scan (single-pass variant of
       column-counts, which still rescans the todo-item subtree per call)."
    (let
      ((mine (workstreams/open-items-from nodes lid))
        (cols (todo/state/columns-for-list lid)))
      (list/filter
        (lambda (p) (> (cdr p) 0))
        (list/map
          (lambda (c) (cons (as-str c) (workstreams/count-column mine c)))
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

(define workstreams/factory-count-state
  (lambda (st)
    "Rows in one run state: DB-filtered read plus the count prim."
    (count (find-rows (quote run) "state" st 100000)))
  "How many run rows sit in one state.")

(define workstreams/factory-counts
  (lambda ()
    "Factory counters as an assoc list of (key . n), computed from
       filtered DB reads instead of a whole-store scan.

         total        every run row, unfiltered read
         open         total minus the five terminal states — i.e. work a
                      supervisor could still pick up
         running      state=running
         reclaimable  non-terminal AND claim_deadline in the past: the
                      only rows a supervisor may actually take over
         failed / succeeded / dead            terminal outcomes

       LIVE vs HISTORY is the point of `reclaimable`. Expiry is
       meaningless on a finished row: 2026-10-25 measured 1118 rows, ALL
       terminal, of which 971 carried a past claim_deadline because they
       finished long ago. Counting those as 'expired leases' reports dead
       history as orphaned work and reads as a permanent alarm — a verdict
       that never changes is a heartbeat, not information. Expired-and-live
       was 0.

       `open` is derived by SUBTRACTION rather than by summing the live
       states, so a run state added later cannot silently vanish from the
       count; and `reclaimable` inspects queued rows too, which run-reap
       skips (a queued row has no deadline, so it never expires)."
    (let
      ((total (count (find-rows (quote run) 100000)))
       (succeeded (workstreams/factory-count-state "succeeded"))
       (failed (workstreams/factory-count-state "failed"))
       (timed-out (workstreams/factory-count-state "timed_out"))
       (canceled (workstreams/factory-count-state "canceled"))
       (dead (workstreams/factory-count-state "dead"))
       (live (workstreams/factory-live-rows)))
      (let
        ((open-n
          (- total
            (+ succeeded (+ failed (+ timed-out (+ canceled dead))))))
         (reclaimable
          (count
            (list/filter
              (lambda (row) (workstreams/run-expired? (cdr row)))
              live))))
        (list
          (cons "total" total)
          (cons "open" open-n)
          (cons "running" (workstreams/factory-count-state "running"))
          (cons "reclaimable" reclaimable)
          (cons "failed" failed)
          (cons "succeeded" succeeded)
          (cons "dead" dead)))))
  "Factory counters from filtered DB reads (no whole-store scan).")

(define workstreams/factory-html
  (lambda ()
    "The factory block of /pulse: is the executor actually running, and is
       anything stuck. Self-contained — it takes no precomputed scan
       because its reads are DB-filtered and cost steps in the dozens, not
       hundreds of thousands (see workstreams/factory-counts).
       Deliberately reports the RECLAIMABLE count, not 'expired leases' —
       see workstreams/factory-counts for why the wider figure is an alarm
       that never clears."
    (let ((c (workstreams/factory-counts)))
      (let ((open-n (workstreams/map-lookup "open" c))
            (running-n (workstreams/map-lookup "running" c))
            (reclaim-n (workstreams/map-lookup "reclaimable" c))
            (failed-n (workstreams/map-lookup "failed" c))
            (total-n (workstreams/map-lookup "total" c)))
        (html/div
          (html/attr "class" "ws-factory")
          (html/join
            (list
              (html/h3 (html/text "Factory"))
              (html/p*
                (html/attr "class" "muted pulse-summary")
                (string-append
                  (as-str open-n)
                  " run"
                  (if (string-eq (as-str open-n) "1") "" "s")
                  " open · "
                  (as-str running-n)
                  " running · "
                  (as-str reclaim-n)
                  " reclaimable · "
                  (as-str failed-n)
                  " failed · "
                  (as-str total-n)
                  " total"))
              (if (string-eq (as-str reclaim-n) "0")
                  (html/p*
                    (html/attr "class" "muted")
                    "No supervisor is holding a dead lease.")
                  (html/p*
                    (html/attr "class" "ws-warn")
                    (string-append
                      (as-str reclaim-n)
                      " run(s) died mid-flight and are reclaimable from "
                      "their step pointer — see borge/run-primitive.borg "
                      "(resume-semantics).")))))))))
  "Factory section for /pulse: run activity + reclaimable leases.")

(define workstreams/factory-live-rows
  (lambda ()
    "Run rows a supervisor could still be holding: queued, claimed, or
       running — read through DB-side field filters, never a scan.

       MEASURED 2026-10-25: one filtered read costs ~10 eval steps. The
       same information through run-all costs 105219 steps for the data.s
       scan alone and 156726 to fold the rows in Lisp, which together do
       not fit beside /pulse's own 170086-step baseline in the 400k
       per-request budget. DB time does not consume steps, so the cheap
       shape is to let the DB filter and to count the result with the
       count prim (a prim, not a per-row loop).

       These three states are exactly the non-terminal vocabulary:
       measured 2026-10-25, the eight run states summed to the 1118 rows
       run-all reports, so no non-terminal row is missed."
    (append
      (find-rows (quote run) "state" "queued" 100000)
      (append
        (find-rows (quote run) "state" "claimed" 100000)
        (find-rows (quote run) "state" "running" 100000))))
  "Non-terminal run rows via filtered DB reads.")

(define workstreams/in-column?
  (lambda (col n)
    (string-eq (todo/item-state (dict-get n "body")) (as-str col))))

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

(define workstreams/open-items-from
  (lambda (nodes lid)
    "Open card nodes for list lid derived from a PRECOMPUTED whole-garden
       scan (workstreams/pulse-nodes) — the single-pass variant of open-items
       (which still rescans the todo-item subtree per call)."
    (list/filter
      (lambda (n)
        (and
          (workstreams/open-node? n)
          (string-eq
            (as-str (dict-get (dict-get n "body") "list_id"))
            (as-str lid))))
      nodes)))

(define workstreams/open-node?
  (lambda (n)
    "Open (non-terminal) card node?"
    (todo/state/open? (todo/item-state (dict-get n "body")))))

(define workstreams/page
  (lambda ()
    "GET /workstreams renders the live orchestration face. ONE garden scan
       (workstreams/pulse-nodes) feeds the rollup AND every stream section —
       the /workstreams side of the rescan fix (card 8972cdfe; was ~4
       todo-item subtree rescans per stream per render)."
    (let ((nodes (workstreams/pulse-nodes)))
      (html/page
        (list
          (html/h1 (html/text "Workstreams"))
          (html/p*
            (html/attr "class" "muted")
            "Live counts from the todo-tree; open = not DONE/CANCELLED/ARCHIVED,
             stale = open >3 days. The shared Board list is the overview.")
          (workstreams/rollup-html-from nodes)
          (html/h2 (html/text "Per stream"))
          (html/join
            (list/map
              (lambda (s) (workstreams/stream-section-from nodes s))
              (workstreams/stream-rows))))))))

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
              (workstreams/factory-html)
              (html/p*
                (html/attr "class" "muted ws-board-link")
                (html/a "/workstreams" "per-stream detail"))))))))
  "GET /pulse — the cross-stream indicator. ONE scan of todo nodes feeds\n     the stream counts, stale flags, and the HUMAN-REVIEW rollup (they all\n     derive from the same node list). The factory block is the one part\n     that does NOT read that scan: run rows are counted through filtered\n     find-rows reads, because folding them in Lisp cost 261945 eval steps\n     and blew the 400k per-request budget on a page that already costs\n     170104 (borge/workstreams.borg factory-indicator).")

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
       (card-id . stream-title). ONE whole-garden scan feeds the fold
       (was: one todo-item subtree rescan per stream — card 8972cdfe)."
    (workstreams/rollup-from (workstreams/pulse-nodes) col)))

(define workstreams/rollup-from
  (lambda (nodes col)
    "All open cards currently in column col across every stream, derived
       from a PRECOMPUTED whole-garden scan (workstreams/pulse-nodes):
       list of (card-id . stream-title)."
    (list/foldl
      (lambda (acc stream)
        (append
          acc
          (list/map
            (lambda (n)
              (cons (workstreams/node-id n) (cdr stream)))
            (list/filter
              (lambda (n) (workstreams/in-column? col n))
              (workstreams/open-items-from nodes (car stream))))))
      (list)
      (workstreams/stream-rows))))

(define workstreams/rollup-html
  (lambda ()
    "The 'ready for human review' rollup across all streams. ONE whole-garden
       scan (was: a per-stream rescan inside rollup — card 8972cdfe)."
    (workstreams/rollup-html-from (workstreams/pulse-nodes))))

(define workstreams/rollup-html-from
  (lambda (nodes)
    "The 'ready for human review' rollup across all streams, derived from a
       PRECOMPUTED whole-garden scan."
    (let ((cards (workstreams/rollup-from nodes "HUMAN-REVIEW")))
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

(define workstreams/run-all
  (lambda ()
    "Every run row in the shared run space as (id . body) pairs, id
     dashed. ONE scan of the shared data root filtered to the run type
     (the todo/item/all-shared pattern — datum paths are
     data.s.<enc-space>.<enc-type>.<uuid-label> and tree_enc_label
     mangles the bare prefix, so a data.s.run scan misses; the proven
     pattern wins)."
    (list/map
      todo/tree/node->pair
      (list/filter
        (lambda (nj)
          (string-eq (as-str (dict-get nj "type")) "run"))
        (tree/list "data.s" 3 2000)))))

(define workstreams/run-claim
  (lambda (run-id supervisor-id instance-id identity ttl-s deadline-iso)
    "CAS queued -> claimed: flips the row ONLY while it is still queued
     (the guarded set; run-primitive.borg run-row). claim = supervisor id
     + lease/heartbeat ts + ttl + deadline ISO (the supervisor computes
     the deadline; the prim set has no seconds math). Returns the
     claimed row, or () when the row is gone, not queued, or already
     claimed."
    (let
      ((cur (workstreams/run-get run-id)))
      (if
        (null? cur)
        ()
        (if
          (string-eq (workstreams/run-state cur) "queued")
          (workstreams/run-patch
            run-id
            (list
              "state" "claimed"
              "claim_supervisor_id" (as-str supervisor-id)
              "claim_lease_ts" (now-iso)
              "claim_heartbeat_at" (now-iso)
              "claim_ttl_s" (as-str ttl-s)
              "claim_deadline" (as-str deadline-iso)
              "runner_instance" (as-str instance-id)
              "runner_identity" (as-str identity)
              "claimed_at" (now-iso)))
          ())))))

(define workstreams/run-expired?
  (lambda (row)
    "Live claim expired? The supervisor's heartbeat deadline has passed
     (ISO 8601 UTC strings compare lexicographically). Rows without a
     deadline never expire here."
    (let
      ((d (as-str (dict-get row "claim_deadline"))))
      (if
        (string-eq d "")
        #f
        (string<? d (now-iso))))))

(define workstreams/run-finish
  (lambda (run-id final-state error-class error-message)
    "Move a live run to a terminal state (succeeded | failed |
     timed_out | canceled | dead) or park it. Guarded: only claimed or
     running rows transition. Returns the row, or () when the guard
     fails (already terminal / missing)."
    (let
      ((cur (workstreams/run-get run-id)))
      (if
        (null? cur)
        ()
        (let
          ((s (workstreams/run-state cur)))
          (if
            (or (string-eq s "claimed") (string-eq s "running"))
            (workstreams/run-patch
              run-id
              (list
                "state" (as-str final-state)
                "error_class" (as-str error-class)
                "error_message" (as-str error-message)
                "ended_at" (now-iso)))
            ()))))))

(define workstreams/run-get
  (lambda (run-id)
    "The run row body dict, or () for an empty/missing id. node-path
     resolves the shared location via the row's space field."
    (if
      (string-eq (as-str run-id) "")
      ()
      (todo/tree/get "run" run-id))))

(define workstreams/run-heartbeat
  (lambda (run-id deadline-iso)
    "Bump the claim's heartbeat (the supervisor tick while a child
     lives) and extend the expiry deadline. Returns the fresh row, or
     () when the row is missing."
    (workstreams/run-patch
      run-id
      (list
        "claim_heartbeat_at" (now-iso)
        "claim_deadline" (as-str deadline-iso)))))

(define workstreams/run-new
  (lambda
    (card-id list-id from-state dest-state brief-hash
     budget-max-tokens budget-max-requests budget-max-wall-s steps-json)
    "Create a QUEUED run row in the shared run space; returns the run id
     (dashed). space \"run\" routes the row into data.s.run.* — no garden
     fallback, by the obituary. Declares the type on first use."
    (seq
      (workstreams/runs-init!)
      (todo/tree/create
        "run"
        (dict-set*
          "{}"
          (list
            "space" "run"
            "card_id" (as-str card-id)
            "list_id" (as-str list-id)
            "from_state" (as-str from-state)
            "dest_state" (as-str dest-state)
            "brief_hash" (as-str brief-hash)
            "state" "queued"
            "claim_supervisor_id" ""
            "claim_lease_ts" ""
            "claim_heartbeat_at" ""
            "claim_ttl_s" ""
            "claim_deadline" ""
            "runner_kind" "pp-flow"
            "runner_instance" ""
            "runner_identity" ""
            "steps_json" (as-str steps-json)
            "budget_max_tokens" (as-str budget-max-tokens)
            "budget_max_requests" (as-str budget-max-requests)
            "budget_max_wall_s" (as-str budget-max-wall-s)
            "usage_requests" "0"
            "usage_input_tokens" "0"
            "usage_cached_tokens" "0"
            "usage_output_tokens" "0"
            "usage_wall_s" "0"
            "result_status" ""
            "result_category" ""
            "result_message" ""
            "trace_id" ""
            "error_class" ""
            "error_message" ""
            "created_at" (now-iso)
            "claimed_at" ""
            "started_at" ""
            "ended_at" ""))))))

(define workstreams/run-open
  (lambda ()
    "Non-terminal run rows — the queue the supervisor works from."
    (list/filter
      (lambda (row) (not (workstreams/run-terminal? (cdr row))))
      (workstreams/run-all))))

(define workstreams/run-open-for-card
  (lambda (card-id)
    "Open run rows for one card: the queue-fill eligibility filter — a
     card with an open run row is not dispatchable again
     (supervisor-tick 4)."
    (list/filter
      (lambda (row)
        (string-eq
          (as-str (dict-get (cdr row) "card_id"))
          (as-str card-id)))
      (workstreams/run-open))))

(define workstreams/run-patch
  (lambda (run-id updates)
    "Read-modify-write one row: updates is a flat (k v k v ...) list.
     Returns the FRESH body after the write, or () when the row is
     gone. The read and the write land in ONE eval — the phase-2 CAS
     contract point."
    (let
      ((cur (workstreams/run-get run-id)))
      (if
        (null? cur)
        ()
        (seq
          (workstreams/run-save run-id (dict-set* cur updates))
          (workstreams/run-get run-id))))))

(define workstreams/run-reap
  (lambda ()
    "Rows whose live claim expired (a supervisor died mid-run): the
     booting supervisor TAKES THESE OVER from the step pointer — never
     a restart from scratch (resume-semantics 3). Returns (id . body)
     pairs for claimed/running rows past their deadline."
    (list/filter
      (lambda (row)
        (and
          (workstreams/run-expired? (cdr row))
          (let
            ((s (workstreams/run-state (cdr row))))
            (or
              (string-eq s "claimed")
              (string-eq s "running")))))
      (workstreams/run-all))))

(define workstreams/run-result
  (lambda (run-id status category message trace-id)
    "Store the sidecar's terminal-output fields (:STATUS / :CATEGORY /
     :MESSAGE / :TRACE-ID) — machine-readable evidence on the row.
     Returns the new body, or () when the row is gone."
    (workstreams/run-patch
      run-id
      (list
        "result_status" (as-str status)
        "result_category" (as-str category)
        "result_message" (as-str message)
        "trace_id" (as-str trace-id)))))

(define workstreams/run-save
  (lambda (run-id fields)
    "Write the whole row body back (owner-confined: the holding
     supervisor). Returns the run id."
    (todo/tree/save "run" run-id fields)))

(define workstreams/run-start
  (lambda (run-id)
    "claimed -> running (the first sidecar spawned). Returns the fresh
     row, or () when the row is missing or not claimed."
    (let
      ((cur (workstreams/run-get run-id)))
      (if
        (null? cur)
        ()
        (if
          (string-eq (workstreams/run-state cur) "claimed")
          (workstreams/run-patch
            run-id
            (list "state" "running" "started_at" (now-iso)))
          ())))))

(define workstreams/run-state
  (lambda (row)
    "The row's state string (queued/claimed/running/...)."
    (as-str (dict-get row "state"))))

(define workstreams/run-step-set
  (lambda (run-id steps-json)
    "Store the supervisor's serialized step list (opaque to this layer).
     Returns the new body, or () when the row is gone."
    (workstreams/run-patch run-id (list "steps_json" (as-str steps-json)))))

(define workstreams/run-terminal?
  (lambda (row)
    "Terminal run? succeeded | failed | timed_out | canceled | dead.
     Terminal runs never auto-retry (resume-semantics: attempts
     exhausted -> dead)."
    (let
      ((s (workstreams/run-state row)))
      (or
        (string-eq s "succeeded")
        (string-eq s "failed")
        (string-eq s "timed_out")
        (string-eq s "canceled")
        (string-eq s "dead")))))

(define workstreams/run-usage-set
  (lambda (run-id requests in-toks cached-toks out-toks wall-s)
    "Store accumulated usage totals. The supervisor owns the math — it
     parses the adapter's terminal :usage; this row is where the counts
     finally land instead of being dropped (run-primitive.borg
     budget-accounting)."
    (workstreams/run-patch
      run-id
      (list
        "usage_requests" (as-str requests)
        "usage_input_tokens" (as-str in-toks)
        "usage_cached_tokens" (as-str cached-toks)
        "usage_output_tokens" (as-str out-toks)
        "usage_wall_s" (as-str wall-s)))))

;; ------------------------------------------------------------------
;; run rows — the run primitive storage layer (D3 phase 2)
;; ------------------------------------------------------------------
;; spec: borge/run-primitive.borg (registered 2026-10-16, commit c4ea7a1).
;; The workstreams obituary in that same commit supersedes the 09-26
;; no-run-datum stance FOR SIDECAR RUNS: durable run rows live in the
;; SHARED space data.s.run.* (the 034 row-level space field routes the
;; insert; kernel path data.s.<space>.<type>.<uuid-label>) — and NEVER
;; in a garden: the data.u.<user>.run.* ban itself stands, so every
;; writer here stamps space "run" and there is deliberately NO garden
;; fallback (the todo/item/add-space register pattern).
;;
;; STATE MACHINE (run-primitive.borg lifecycle): queued -> claimed ->
;; running -> succeeded | failed | timed_out | canceled | dead | parked.
;; Transitions are owner-confined: only the holding supervisor writes
;; state; everyone else reads. The queued->claimed CAS is a GUARDED SET
;; checked in the same eval (state must still be queued with no live
;; claim); under the ratified single-supervisor design that guard is
;; the whole contract today — a kernel-level compare-and-set stays the
;; recorded alternative (run-row section; D4 Q10 accepts guarded writes
;; under the single-supervisor lease).
;;
;; TIME: the prim set has no seconds arithmetic, so the SUPERVISOR
;; computes deadline ISO strings (now + ttl) and passes them in; these
;; helpers store and string-compare them (ISO 8601 UTC sorts
;; lexicographically — the same trick as the stale-date compare).
;;
;; STEPS + USAGE: steps ride steps_json (opaque here, supervisor-
;; serialized); usage totals are written whole by the supervisor — it
;; owns the parse of the adapter's :INPUT-TOKENS / :OUTPUT-TOKENS /
;; :PROVIDER-REQUESTS (pp-flow-autolith.sh:223-226), the counts pp-flow
;; drops today.

(define workstreams/runs-init!
  (lambda ()
    "Declare the run datum type (guarded: a re-load is safe). Seed once
     as admin on main — a delegated supervisor session cannot deftype
     (the projects-init! pattern; boot seeds types onto system/main)."
    (if
      (null? (type-fields (quote run)))
      (deftype
        (quote run)
        (quote card_id)
        (quote list_id)
        (quote from_state)
        (quote dest_state)
        (quote brief_hash)
        (quote state)
        (quote claim_supervisor_id)
        (quote claim_lease_ts)
        (quote claim_heartbeat_at)
        (quote claim_ttl_s)
        (quote claim_deadline)
        (quote runner_kind)
        (quote runner_instance)
        (quote runner_identity)
        (quote steps_json)
        (quote budget_max_tokens)
        (quote budget_max_requests)
        (quote budget_max_wall_s)
        (quote usage_requests)
        (quote usage_input_tokens)
        (quote usage_cached_tokens)
        (quote usage_output_tokens)
        (quote usage_wall_s)
        (quote result_status)
        (quote result_category)
        (quote result_message)
        (quote trace_id)
        (quote error_class)
        (quote error_message)
        (quote created_at)
        (quote claimed_at)
        (quote started_at)
        (quote ended_at))
      "run ok")))

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
        (lambda (n) (workstreams/stale-node? n t))
        (workstreams/open-items list-id)))))

(define workstreams/stale-items-from
  (lambda (nodes lid)
    "Open stale cards for lid derived from a PRECOMPUTED whole-garden scan
       (single-pass variant of stale-items)."
    (let ((t (workstreams/stale-threshold)))
      (list/filter
        (lambda (n) (workstreams/stale-node? n t))
        (workstreams/open-items-from nodes lid)))))

(define workstreams/stale-node?
  (lambda (n t)
    "Stale open-card node: created_at present and older than threshold t."
    (let ((c (dict-get n "created_at")))
      (if (null? c) #f (workstreams/old? c t)))))

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
    "One stream: title + board link, non-zero column counts, stale listing.
       ONE whole-garden scan (was: three todo-item subtree rescans — card 8972cdfe)."
    (workstreams/stream-section-from (workstreams/pulse-nodes) stream)))

(define workstreams/stream-section-from
  (lambda (nodes stream)
    "One stream derived from a PRECOMPUTED whole-garden scan
       (workstreams/pulse-nodes): title + board link, non-zero column
       counts, stale listing."
    (let
      ((lid (car stream))
        (title (cdr stream)))
      (let
        ((mine (workstreams/open-items-from nodes lid))
          (counts (workstreams/column-counts-from nodes lid))
          (stale (workstreams/stale-items-from nodes lid)))
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
                          " (" (as-str (workstreams/len mine)) " open)"))))))
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
