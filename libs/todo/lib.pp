;; package todo -- exported from live branch system/main
;; Sync: scripts/pp_sync.py pull|push

(package todo
  (export
    todo/active-items
    todo/add
    todo/adopt-stranded
    todo/archive
    todo/comment/add
    todo/comment/list
    todo/comments-init!
    todo/css
    todo/cycle
      todo/dep
      todo/dep/blocked-ids
      todo/dep/blocked?
      todo/dep/blockers
      todo/dep/entry-state
      todo/dep/find
      todo/dep/first-open-col
      todo/dep/member?
      todo/dep/open-blockers
      todo/dep/rows
      todo/deps-init!
    todo/ensure-inbox
    todo/ensure-space
    todo/event/list
    todo/event/record
    todo/event/transition!
    todo/feed
    todo/flow-brief
    todo/from-notes-html
    todo/item/acceptance
    todo/item/active
    todo/item/add
    todo/item/add-space
    todo/item/all
    todo/item/archive
    todo/item/assign
    todo/item/assignee
    todo/item/branch
    todo/item/branch-kind
    todo/item/cycle
    todo/item/delete
    todo/item/get
    todo/item/kind
    todo/item/list-id
    todo/item/origin-kind
    todo/item/origin-ref
    todo/item/queue
    todo/item/set-acceptance
    todo/item/set-branch
    todo/item/set-branch-kind
    todo/item/set-kind
    todo/item/set-state
    todo/item/set-target
    todo/item/state
    todo/item/title
    todo/items-for-list
    todo/list/all
    todo/list/create
    todo/list/default?
    todo/list/delete
    todo/list/ensure-inbox
    todo/list/find-by-title
    todo/list/merge
    todo/list/set-columns
    todo/list/space-of
    todo/node/back-link
    todo/node/find-card
    todo/node/mirror
    todo/node/mirror-notice
    todo/node/note-visible?
    todo/node/parse-ref
    todo/node/promote-form
    todo/node/seed-state
    todo/node/marked-keywords
    todo/node/marked-rows
    todo/node/slug-map
    todo/node/row-of
    todo/node/unpromoted
    todo/page
    todo/page/cap
    todo/page/more-note
    todo/promote-handler
    todo/promote-node
    todo/recipe
    todo/recipe-set
    todo/recipes-default
    todo/recipes-for-list
      todo/ready
      todo/ready?
    todo/resolve-auto-lane
    todo/resolve-board
    todo/set-state
    todo/setup-auto-lane
    todo/state/archived?
    todo/state/closed?
    todo/state/columns-for-item
    todo/state/columns-for-list
    todo/state/list
    todo/state/next-for-item
    todo/state/open?
    todo/state/options-for-item
    todo/state/pipeline-for-item
    todo/tree/create
    todo/tree/get
    todo/tree/node->pair
    todo/tree/prefix
    todo/tree/redash
    todo/tree/rows
    todo/tree/save
    todo/tree/shared-prefix
    todo/tree/tail
      todo/undep
    todo/ui/handle-post
    todo/item/unassign
  ))

(define todo/active-items
  (lambda
    (items)
    (list/filter
      (lambda
        (row)
        (not (todo/state/archived? (todo/item/state (cdr row)))))
      items)))

(define todo/add
  (lambda
    (list-id title)
    (let
      ((lid
        (if
          (string-eq (as-str list-id) "")
          (todo/ensure-inbox)
          (as-str list-id))))
      (todo/tree/create
        "todo-item"
        (dict-set*
          "{}"
          (list
            "list_id"
            lid
            "title"
            (as-str title)
            "notes"
            ""
            "state"
            "TODO"
            "due"
            ""
            "position"
            "0"
            "done"
            "false"
            "acceptance"
            ""
            "branch"
            ""
            "branch_kind"
            ""
            "pipeline"
            ""
            "kind"
            ""
            "origin_kind"
            ""
            "origin_ref"
            ""))))))

(define todo/add-form
  (lambda
    (view list-id)
    (if
      (string-eq view "from-notes")
      (html/p*
        (html/attr "class" "muted")
        "Mark nodes TODO in notes — they appear here. Toggle writes back.")
      (if
        (string-eq view "archived")
        (html/p*
          (html/attr "class" "muted")
          "Archived tasks. Unarchive from detail (set state back), or delete.")
        (html/form
          "POST"
          "/todos"
          (html/attrs
            (list
              (html/attr "class" "card todo-add")
              (html/attr "hx-post" "/todos")
              (html/attr "hx-target" "#todo-main")
              (html/attr "hx-swap" "innerHTML")
              (html/attr
                "hx-on::after-request"
                "if(event.detail.successful){this.reset()}")))
          (html/join
            (list
              (html/hidden "action" "add")
              (html/hidden "view" view)
              (html/hidden "list_id" list-id)
              (html/label
                (html/attrs
                  (list
                    (html/attr "class" "todo-add-label")
                    (html/attr "for" "title")))
                "New item")
              (html/div
                (html/attr "class" "todo-add-row")
                (html/join
                  (list
                    (html/text-input
                      "title"
                      (html/attrs
                        (list
                          (html/attr "id" "title")
                          (html/attr "required" "required")
                          (html/attr "placeholder" "buy milk")
                          (html/attr "autocomplete" "off"))))
                    (html/button (html/attr "type" "submit") "Add")))))))))))

(define todo/alist-get
  (lambda
    (alist k)
    (if
      (null? alist)
      (list)
      (if
        (string-eq (car (car alist)) k)
        (cdr (car alist))
        (todo/alist-get (cdr alist) k))))
  "bucket items for key k (or ())")

(define todo/alist-push
  (lambda
    (k v alist)
    (if
      (null? alist)
      (list (cons k (list v)))
      (if
        (string-eq (car (car alist)) k)
        (cons (cons k (cons v (cdr (car alist)))) (cdr alist))
        (cons (car alist) (todo/alist-push k v (cdr alist))))))
  "push v onto alist bucket k (pure, one pass over groups)")



(define todo/archive
  (lambda (id) (todo/set-state id "ARCHIVED")))

(define todo/archived-items
  (lambda
    ()
    (list/filter
      (lambda
        (row)
        (todo/state/archived? (todo/item/state (cdr row))))
      (todo/item/all))))



(define todo/comment/add
  (lambda
    (item body)
    "Append a comment datum to card item; returns the comment id. Author +
     ts ride the row metadata (session user / created_at)."
    (todo/comments-init!)
    (as-str
      (todo/tree/create
        "todo-comment"
        (dict-set*
          "{}"
          (list "item_id" (as-str item) "body" (as-str body)))))))

(define todo/comment/list
  (lambda
    (item)
    "All comment node-JSONs for card item, newest first."
    (todo/datum/rows-for-item "todo-comment" item 200)))

;; ------------------------------------------------------------------
;; comment + event datums (card detail v2 feed) — borge/workstreams.borg
;; card-comments. Two datum types seeded on load (guarded, idempotent):
;;   todo-comment (item_id, body) — author + timestamps come from the ROW
;;     METADATA (node author / created_at), not body fields, per spec: the
;;     datum itself is deliberately minimal.
;;   todo-event   (item_id, from_state, to_state) — auto-written by every
;;     real card-state transition. Wired (task 3): todo/set-state,
;;     todo/item/set-state, todo/save, todo/item/save all route through
;;     todo/event/transition! (record only when the state actually changed),
;;     so todo/cycle, todo/item/cycle, archive, the /todos detail form, and
;;     the future move writers all append events for free. The kanban
;;     move-card renumber (a position-reorder in libs/kanban) writes state
;;     directly via tree/upsert and is intentionally out of scope here (the
;;     kanban lib is off-limits this loop).
;; todo/feed interleaves events + comments chronologically (Linear-style
;; activity feed) for card detail v2.
;; ------------------------------------------------------------------

(define todo/comments-init!
  (lambda
    ()
    "Ensure the todo-comment / todo-event datum types exist. Guarded so a
     re-load (load-library) is safe and a non-admin load degrades to a no-op
     value instead of an error; boot seeds these onto system/main as admin."
    (seq
      (if
        (null? (type-fields (quote todo-comment)))
        (deftype (quote todo-comment) (quote item_id) (quote body))
        "todo-comment ok")
      (if
        (null? (type-fields (quote todo-event)))
        (deftype (quote todo-event) (quote item_id) (quote from_state) (quote to_state))
        "todo-event ok"))))



;; ------------------------------------------------------------------
;; dependency DAG (beads-parity): blocked-by edges as todo-dep datum rows
;; (from_id, to_id) — the from-card is blocked BY the to-card. Same
;; associated-row pattern as todo-comment / todo-event: edge writes never
;; touch card bodies (no version churn, no todo-event noise), space routing
;; rides the TYPE-level admission (todo/admitted-types), and reverse lookups
;; scan the same prefix. blocks is DERIVABLE (reverse scan), never stored.
;;
;; Readiness is a DIRECT-blocker predicate — no graph walk: a card is
;; blocked while any direct blocker is open. Chains resolve in order as
;; blockers close (a DONE/PARKED blocker does not block); an open cycle
;; keeps every member not-ready until one member is closed externally —
;; cycle-safe by construction, no detection pass. A blocker whose card row
;; is gone does NOT block (stale edge, ignored).
;; ------------------------------------------------------------------

(define todo/deps-init!
  (lambda
    ()
    "Ensure the todo-dep datum type exists AND is admitted to the shared
       open todo space (both guarded, idempotent — the same two checks
       todo/ensure-space runs per entry point). Called lazily by the dep
       readers/writers; boot seeds it onto system/main as admin. A missing
       space or a non-owner identity degrades to a no-op (garden routing)."
    (seq
      (if
        (null? (type-fields (quote todo-dep)))
        (deftype (quote todo-dep) (quote from_id) (quote to_id))
        "todo-dep ok")
      (guard
        (lambda (e) #f)
        (if
          (space-exists? todo/open-space)
          (space-admit "todo-dep" todo/open-space)
          #f)))))

(define todo/dep/member?
  (lambda
    (id ids)
    "True when string id is in the id list ids (no std member? exists)."
    (if
      (null? ids)
      #f
      (if
        (string-eq (as-str id) (as-str (car ids)))
        #t
        (todo/dep/member? id (cdr ids))))))

(define todo/dep/rows
  (lambda
    ()
    "All dep edge rows as (edge-id . body) pairs — ONE prefix scan (capped
       at 1000 edges, same shape as the comment feed's 200-row cap)."
    (seq
      (todo/deps-init!)
      (todo/tree/rows "todo-dep" 1000))))

(define todo/dep/find
  (lambda
    (from to rows)
    "The edge id of the from->to edge in PRECOMPUTED rows, or () when absent."
    (if
      (null? rows)
      (list)
      (let
        ((b (cdr (car rows))))
        (if
          (and
            (string-eq (as-str (dict-get b "from_id")) (as-str from))
            (string-eq (as-str (dict-get b "to_id")) (as-str to)))
          (as-str (car (car rows)))
          (todo/dep/find from to (cdr rows)))))))

(define todo/dep
  (lambda
    (from to)
    "Add a blocked-by edge: card FROM is blocked BY card TO. Returns the new
       edge id. Refusals (string, no write): \"self edge\", \"no such card:
       <id>\", \"duplicate edge\"."
    (seq
      (todo/deps-init!)
      (let
        ((f (as-str from))
         (t (as-str to)))
        (if
          (string-eq f t)
          "self edge"
          (if
            (null? (todo/tree/get "todo-item" f))
            (string-append "no such card: " f)
            (if
              (null? (todo/tree/get "todo-item" t))
              (string-append "no such card: " t)
              (if
                (null? (todo/dep/find f t (todo/dep/rows)))
                (as-str
                  (todo/tree/create
                    "todo-dep"
                    (dict-set* "{}" (list "from_id" f "to_id" t))))
                  "duplicate edge"))))))))

(define todo/undep
  (lambda
    (from to)
    "Remove the blocked-by edge from->to. \"removed\" or \"no such edge\"."
    (seq
      (todo/deps-init!)
      (let
        ((e (todo/dep/find (as-str from) (as-str to) (todo/dep/rows))))
        (if
          (null? e)
          "no such edge"
          (seq
            (todo/tree/del "todo-dep" e)
            "removed"))))))

(define todo/dep/blockers
  (lambda
    (id)
    "Direct blocker ids of card id: the to_ids of its from-edges, open or
       not (its OWN scan — batch renders use todo/dep/blocked-ids)."
    (let
      ((f (as-str id)))
      (list/map
        (lambda (r) (as-str (dict-get (cdr r) "to_id")))
        (list/filter
          (lambda (r) (string-eq (as-str (dict-get (cdr r) "from_id")) f))
          (todo/dep/rows))))))

(define todo/dep/open-blockers
  (lambda
    (id)
    "Direct blockers of card id that are still OPEN (todo/state/open?). A
       blocker with no card row does not block (stale edge, ignored)."
    (list/filter
      (lambda
        (bid)
        (let
          ((b (todo/tree/get "todo-item" bid)))
          (and
            (not (null? b))
            (todo/state/open? (todo/item/state b)))))
      (todo/dep/blockers id))))

(define todo/dep/blocked?
  (lambda
    (id)
    "True when card id has at least one OPEN blocked-by edge."
    (not (null? (todo/dep/open-blockers id)))))

(define todo/dep/blocked-ids
  (lambda
    (rows)
    "Distinct from_ids with at least one OPEN blocker, from a PRECOMPUTED
       (todo/dep/rows) scan. One blocker-state read per DISTINCT blocker id
       (memo alist), so a board render pays edges + blockers, not
       edges x cards (the 8972cdfe one-scan rule)."
    (cdr
      (list/foldl
        (lambda
          (acc r)
          (let
            ((memo (car acc))
             (out (cdr acc)))
            (let
              ((b (cdr r)))
              (let
                ((f (as-str (dict-get b "from_id")))
                 (t (as-str (dict-get b "to_id"))))
                (if
                  (todo/dep/member? f out)
                  acc
                  (let
                    ((known (todo/alist-get memo t)))
                    (if
                      (null? known)
                      (let
                        ((bc (todo/tree/get "todo-item" t)))
                        (let
                          ((open
                            (if
                              (null? bc)
                              "shut"
                              (if
                                (todo/state/open? (todo/item/state bc))
                                "open"
                                "shut"))))
                          (if
                            (string-eq open "open")
                            (cons (cons (cons t open) memo) (cons f out))
                            (cons (cons (cons t open) memo) out))))
                      (if
                        (string-eq known "open")
                        (cons memo (cons f out))
                        acc))))))))
        (cons (list) (list))
        rows))))

(define todo/dep/first-open-col
  (lambda
    (cols)
    "First non-empty column name of a pipeline (the default list leads with
       the \"\" pseudo-column, which todo/state/closed? treats as closed)."
    (if
      (null? cols)
      "TODO"
      (if
        (string-eq (as-str (car cols)) "")
        (todo/dep/first-open-col (cdr cols))
        (as-str (car cols))))))

(define todo/dep/entry-state
  (lambda
    (list-id)
    "The list's ACTIONABLE entry column: the first non-empty column of its
       pipeline — TODO on the global default, the list's own first column
       when it carries a columns override. A card counts as READY only in
       this column: later columns are in-flight, not ready (beads-parity)."
    (todo/dep/first-open-col
      (todo/state/columns-for-list (as-str list-id)))))

(define todo/ready?
  (lambda
    (id)
    "True when card id is READY: it exists, sits in its list's actionable
       entry column (todo/dep/entry-state), and has no open blocked-by edge."
    (let
      ((f (todo/tree/get "todo-item" (as-str id))))
      (if
        (null? f)
        #f
        (and
          (string-eq
            (todo/item/state f)
            (todo/dep/entry-state (todo/item/list-id f)))
          (not (todo/dep/blocked? id)))))))

(define todo/ready
  (lambda
    (list-id)
    "The list's READY cards as (id . fields) pairs: entry-column, no open
       blocked-by edge. ONE dep-row scan for the whole list
       (todo/dep/blocked-ids) — the eligibility view the supervisor drains."
    (let
      ((entry (todo/dep/entry-state list-id))
       (blocked (todo/dep/blocked-ids (todo/dep/rows))))
      (list/filter
        (lambda
          (row)
          (and
            (string-eq (todo/item/state (cdr row)) entry)
            (not (todo/dep/member? (as-str (car row)) blocked))))
        (todo/items-for-list list-id)))))

(define todo/count-open-in
  (lambda
    (items)
    (list/foldl
      (lambda
        (acc row)
        (if
          (todo/state/open? (todo/item/state (cdr row)))
          (+ acc 1)
          acc))
      0
      items)))



(define todo/css
  (lambda
    ()
    (css/sheet
      (list
        (css/rule
          ".todo-layout"
          (list
            "display"
            "grid"
            "grid-template-columns"
            "12rem 1fr"
            "gap"
            ".75rem"))
        (css/rule
          ".todo-nav"
          (list
            "list-style"
            "none"
            "padding"
            "0"
            "margin"
            "0"))
        (css/rule ".todo-nav-item" (list "margin" ".25rem 0"))
        (css/rule
          ".todo-nav-item.is-active a"
          (list
            "font-weight"
            "700"
            "border-left"
            "3px solid #000"
            "padding-left"
            ".35rem"))
        (css/rule
          ".todo-new-list"
          (list
            "display"
            "flex"
            "flex-direction"
            "column"
            "gap"
            ".25rem"
            "margin-top"
            ".5rem"))
        (css/rule
          ".todo-new-list-input"
          (list
            "font-size"
            ".85rem"
            "min-height"
            "2rem"
            "padding"
            ".25rem"))
        (css/rule
          ".todo-box.st-TODO"
          (list "background" "var(--pending-bg)"))
        (css/rule
          ".todo-box.st-DONE"
          (list
            "background"
            "var(--done-bg)"
            "text-decoration"
            "line-through"))
        (css/rule
          ".todo-box.st-WAITING"
          (list "background" "var(--waiting-bg)"))
        (css/rule
          ".todo-box.st-CANCELLED"
          (list
            "background"
            "var(--cancelled-bg)"
            "color"
            "var(--archived-ink)"))
        (css/rule
          ".todo-box.st-ARCHIVED"
          (list
            "background"
            "var(--archived-bg)"
            "color"
            "var(--archived-ink)"
            "border-style"
            "dashed"))
        (css/rule
          "li.todo-item.is-archived .todo-title"
          (list
            "color"
            "var(--archived-ink)"
            "font-style"
            "italic"))
        (css/rule
          ".todo-from-note .todo-src"
          (list "font-size" ".8rem" "margin-left" ".35rem"))
        (css/rule
          ".todo-add-label"
          (list
            "display"
            "block"
            "font-weight"
            "bold"
            "margin"
            "0 0 .35rem"))
        (css/rule
          ".todo-add-row"
          (list
            "display"
            "flex"
            "gap"
            ".5rem"
            "align-items"
            "stretch"
            "flex-wrap"
            "wrap"))
        (css/rule
          ".todo-add-row input[type=text]"
          (list "flex" "1 1 12rem" "min-width" "0"))
        (css/rule
          ".todo-add-row button"
          (list
            "flex"
            "0 0 auto"
            "width"
            "auto"
            "min-width"
            "5rem"))
        (css/rule
          "ul.todo-list"
          (list
            "list-style"
            "none"
            "padding"
            ".25rem .5rem"
            "margin"
            "0 0 .75rem"))
        (css/rule
          "li.todo-item"
          (list
            "display"
            "flex"
            "align-items"
            "center"
            "gap"
            ".5rem"
            "padding"
            ".45rem 0"
            "border-bottom"
            "1px solid #c0c0c0"))
        (css/rule
          "li.todo-item:last-child"
          (list "border-bottom" "none"))
        (css/rule
          "li.todo-item form"
          (list "margin" "0" "display" "inline"))
        (css/rule
          "li.todo-item .todo-box"
          (list
            "font-family"
            "ui-monospace,Menlo,monospace"
            "min-width"
            "2.6rem"
            "padding"
            ".25rem .35rem"
            "width"
            "auto"))
        (css/rule
          "li.todo-item .todo-title"
          (list
            "flex"
            "1 1 auto"
            "min-width"
            "0"
            "word-break"
            "break-word"))
        (css/rule
          "li.todo-item.is-done .todo-title"
          (list
            "text-decoration"
            "line-through"
            "color"
            "var(--done-muted)"))
        (css/rule
          "li.todo-item .todo-del button, li.todo-item .todo-archive button"
          (list
            "width"
            "auto"
            "padding"
            ".25rem .45rem"
            "font-size"
            ".85rem"))
        (css/rule
          "li.todo-nav-list"
          (list
            "display"
            "flex"
            "align-items"
            "center"
            "gap"
            ".25rem"
            "flex-wrap"
            "wrap"))
        (css/rule
          "li.todo-nav-list > a"
          (list "flex" "1 1 auto" "min-width" "0"))
        (css/rule
          "li.todo-nav-list form.todo-list-del"
          (list "margin" "0" "display" "inline"))
        (css/rule
          "li.todo-nav-list .todo-list-del button"
          (list
            "width"
            "auto"
            "min-width"
            "1.6rem"
            "padding"
            ".1rem .3rem"
            "font-size"
            ".8rem"
            "line-height"
            "1.2"))
        (css/media
          "max-width:700px"
          (list
            (css/rule
              ".todo-layout"
              (list "grid-template-columns" "1fr")))))))
  "Render the todo component stylesheet (extracted from ui/theme).")

(define todo/cycle
  (lambda
    (id)
    (let
      ((row (todo/tree/get "todo-item" id)))
      (if
        (null? row)
        ()
        (let
          ((cur (todo/item/state row)))
          (if
            (todo/state/archived? cur)
            cur
            (todo/set-state
              id
              (todo/state/next-for-item cur id)))))))
  "Advance item id to its next state; () when missing. Cycles within the\n     item's OWN effective pipeline (own `pipeline` override > list columns >\n     global default) — borge/workstreams.borg pipeline-model, so /todos and\n     /kanban agree and a card on its own pipeline never leaves it.")

(define todo/datum/feed-entry
  (lambda
    (kind n)
    "One feed entry dict (JSON string) from a datum node-JSON: kind + id + ts
     + author from row metadata, plus the payload fields the kind needs."
    (let
      ((b (dict-get n "body")))
      (if
        (string-eq kind "comment")
        (dict-set*
          "{}"
          (list
            "kind" "comment"
            "id" (as-str (dict-get n "id"))
            "ts" (as-str (dict-get n "created_at"))
            "author" (as-str (dict-get n "author"))
            "text" (as-str (dict-get b "body"))))
        (dict-set*
          "{}"
          (list
            "kind" "event"
            "id" (as-str (dict-get n "id"))
            "ts" (as-str (dict-get n "created_at"))
            "author" (as-str (dict-get n "author"))
            "from_state" (as-str (dict-get b "from_state"))
            "to_state" (as-str (dict-get b "to_state"))))))))

(define todo/datum/insert-by-ts
  (lambda
    (e sorted)
    "Insert feed entry e into a ts-ascending sorted list (oldest first). The
     node created_at strings are PG timestamptz text, uniformly shaped, so a
     lexicographic compare IS chronological order."
    (if
      (null? sorted)
      (list e)
      (if
        (string<?
          (as-str (dict-get e "ts"))
          (as-str (dict-get (car sorted) "ts")))
        (cons e sorted)
        (cons
          (car sorted)
          (todo/datum/insert-by-ts e (cdr sorted)))))))

(define todo/datum/merge-by-ts
  (lambda
    (a b)
    "Merge two feed-entry lists (ANY order) into one ts-ascending list.
     Walks both through the sorted insetter from an empty list, so the
     result is chronological regardless of the helpers' own ordering.
     (rows-for-item feeds NEWEST-first; this rebuilds oldest-first.)"
    (list/foldl
      (lambda (acc e) (todo/datum/insert-by-ts e acc))
      (list)
      (append a b))))

(define todo/datum/rows-for-item
  (lambda
    (type item limit)
    "Raw node-JSONs (id/path/kind/type/body/author/created_at/updated_at/rank)
     of all `type` datums whose body.item_id === item, in store order (callers
     that need chronology sort by created_at — see todo/datum/insert-by-ts)."
    (list/filter
      (lambda
        (n)
        (let
          ((b (dict-get n "body")))
          (let
            ((i (if (null? b) () (dict-get b "item_id"))))
            (and (not (null? i)) (string-eq (as-str i) (as-str item))))))
      (tree/list (todo/tree/prefix type) 1 limit))))

(define todo/db/create
  (lambda
    (type fields)
    (let
      ((r
        (tree/upsert
          (node-path type "new")
          "datum"
          (as-str type)
          fields)))
      (if
        (string-starts-with? r "data.")
        (todo/db/path-id r)
        r))))






















(define todo/dbg-scrape
  (lambda
    ()
    (let
      ((n (notes/doc/list)))
      (let
        ((row (todo/notes-cache-row)))
        (let
          ((f
            (dict-set*
              (if (null? row) "{}" (cdr row))
              (list
                "dbg_notes"
                (as-str (count n))
                "dbg_root"
                (notes/tree/root)
                "dbg_todos"
                (as-str (count (notes/outline/all-todos)))
                "dbg_who"
                (whoami)))))
          (seq (update (quote todo-cache) (car row) f) "dbg-done"))))))



(define todo/delete-list
  (lambda
    (list-id)
    (let
      ((lid (as-str list-id)))
      (if
        (string-eq lid "")
        "missing"
        (let
          ((fields (todo/list/get lid)))
          (if
            (null? fields)
            "missing"
            (if
              (todo/list/default? fields)
              "default"
              (seq
                (todo/delete-list-items (todo/item/all) lid)
                (seq (todo/tree/del "todo-list" lid) "ok")))))))))

(define todo/delete-list-items
  (lambda
    (items lid)
    (if
      (null? items)
      ()
      (seq
        (if
          (string-eq
            (todo/item/list-id (cdr (car items)))
            (as-str lid))
          (todo/item/delete (car (car items)))
          ())
        (todo/delete-list-items (cdr items) lid)))))



(define todo/done?
  (lambda
    (fields)
    (let
      ((d (dict-get fields "done")))
      (if (null? d) false (string-eq d "true")))))

(define todo/ensure-inbox
  (lambda
    ()
    (let
      ((found (todo/find-default-list)))
      (if
        (null? found)
        (todo/tree/create
          "todo-list"
          (dict-set*
            "{}"
            (list
              "title"
              "Inbox"
              "description"
              "Default list"
              "is_default"
              "true")))
        (car found)))))

(define todo/event/list
  (lambda
    (item)
    "All event node-JSONs for card item, newest first."
    (todo/datum/rows-for-item "todo-event" item 200)))

(define todo/event/record
  (lambda
    (item from to)
    "Record a card state transition datum (item_id, from_state, to_state);
     returns the event id. Private-ish: user code should call
     todo/event/transition!, which records ONLY on a real change."
    (todo/comments-init!)
    (as-str
      (todo/tree/create
        "todo-event"
        (dict-set*
          "{}"
          (list
            "item_id" (as-str item)
            "from_state" (as-str from)
            "to_state" (as-str to)))))))

(define todo/event/transition!
  (lambda
    (id from to)
    "Record a todo-event for card id when the state actually changed
     (from != to); no-op otherwise. Returns the event id, or () when the
     states are equal. THE choke point every card-state writer calls:
     todo/set-state, todo/item/set-state, todo/save, todo/item/save."
    (if
      (string-eq (as-str from) (as-str to))
      ()
      (todo/event/record id from to))))

(define todo/feed
  (lambda
    (item)
    "Chronological activity feed for card item: todo-event transitions and
     todo-comment rows interleaved by created_at, oldest first. Returns a
     list of feed-entry dicts (JSON strings) tagged kind = comment | event."
    (todo/comments-init!)
    (todo/datum/merge-by-ts
      (list/map
        (lambda (n) (todo/datum/feed-entry "comment" n))
        (todo/comment/list item))
      (list/map
        (lambda (n) (todo/datum/feed-entry "event" n))
        (todo/event/list item)))))

(define todo/find-default-in
  (lambda
    (xs)
    (if
      (null? xs)
      ()
      (if
        (todo/list/default? (cdr (car xs)))
        (car xs)
        (let
          ((t (todo/list/title (cdr (car xs)))))
          (if
            (string-eq t "Inbox")
            (car xs)
            (todo/find-default-in (cdr xs))))))))

(define todo/find-default-list
  (lambda () (todo/find-default-in (todo/list/all))))



;; ------------------------------------------------------------------
;; flow/brief — the executor brief (borge/workstreams.borg
;; claim-brief-protocol). todo/flow-brief renders card + column recipe +
;; context (repo path, build/tests invocation, target host, write-back eval
;; snippet) into ONE self-contained text prompt: the plan file for a ralph
;; loop, the task string for a subagent. NAMING NOTE (orchestration task 7
;; resolution): the plan named this flow/brief; a `libs/flow` package would be
;; a NEW family file, and the Do-NOT rule limits this loop to todo/standup/
;; workstreams libs — so it lives in the todo family as todo/flow-brief.
;; ------------------------------------------------------------------

(define todo/flow-brief
  (lambda
    (card-id)
    (let
      ((fields (todo/tree/get "todo-item" (as-str card-id))))
      (if
        (null? fields)
        ()
        (let
          ((cid (as-str card-id))
            (title (todo/item/title fields))
            (state (todo/item/state fields))
            (list-id (todo/item/list-id fields))
            (acceptance (todo/item/acceptance fields))
            (branch (todo/item/branch fields))
            (branch-kind (todo/item/branch-kind fields))
            (assignee (todo/item/assignee fields)))
          (let
            ((lname
              (if
                (string-eq list-id "")
                "(no list)"
                (let
                  ((lf (todo/tree/get "todo-list" list-id)))
                  (if
                    (null? lf)
                    list-id
                    (todo/list/title lf))))))
            (string-append
              "PRICKLYPEAR EXECUTOR BRIEF — claim/brief protocol (borge/workstreams.borg)\n"
              "Self-contained task: work the card below on the laptop repo. Follow the column\n"
              "recipe. Do not promote to prod. YOU DO NOT WRITE BACK — pp-flow owns the card\n"
              "transition and will move the card one column + post your evidence comment when\n"
              "you exit 0. Your only job is the work + a clear EVIDENCE section.\n\n== CARD ==\n"
              "  card id:    "
              cid
              "\n  title:      "
              title
              "\n  state:      "
              state
              "\n  list:       "
              lname
              " ("
              list-id
              ")\n  assignee:   "
              assignee
              "\n  acceptance: "
              (if (string-eq acceptance "") "(none)" acceptance)
              "\n  branch:     "
              (if
                (string-eq branch "")
                "(none)"
                (if
                  (string-eq branch-kind "")
                  (string-append branch "  [DOMAIN UNSET]")
                  (string-append branch "  [" branch-kind "]")))
              "\n\n== COLUMN RECIPE ("
              state
              ") ==\n"
              (todo/recipe list-id state)
              "\n\n== BRANCH DOMAIN (read this) ==\n"
              "  Pricklypear has TWO branch systems; `branch` above names one of them:\n"
              "  * git     — a git branch in the laptop repo (master, a feature branch).\n"
              "              Repo-side work: .borg spec, OCaml, scripts. Commit + push there.\n"
              "  * habitat — a Nopales/Postgres branch ON the habitat (system/main,\n"
              "              <user>/personal, <user>/dev). Define/route/promote there; it\n"
              "              is NEVER a git branch and does not exist in the repo.\n"
              (if
                (string-eq branch "")
                "  This card names no branch — work on the repo default and say so in your EVIDENCE.\n"
                (if
                  (string-eq branch-kind "")
                  (string-append
                    "  DOMAIN UNSET: is \""
                    branch
                    "\" a git branch or a habitat branch? Do NOT guess — if the work\n"
                    "  you are about to do does not make the answer obvious, STOP and post a\n"
                    "  comment asking, rather than committing to the wrong domain.\n")
                  (if
                    (string-eq branch-kind "git")
                    "  Domain: git — commit to this branch in the laptop repo.\n"
                    "  Domain: habitat — a Nopales/Postgres branch; do NOT git-commit to it.\n")))
              "\n== CONTEXT ==\n"
              "  repo:    /home/roerick/dev/wyo.tech/pricklypear\n"
              "  build:   eval $(opam env --switch=poohstack --set-switch); dune build\n"
              "  tests:   dune runtest lisp; dune runtest common\n"
              "  spec:    the .borg book is the spec + source of truth; run `borge drift` after spec edits\n\n"
              "== EVIDENCE (what pp-flow will post on your behalf) ==\n"
              "  End your output with an EVIDENCE block:\n"
              "  EVIDENCE: <what you did + how you verified it (build/tests/live smoke;\n"
              "  keep under ~200 chars, will be trimmed to 500 by the driver)>.\n"
              "  pp-flow moves the card ONE column and posts this as your comment on the\n"
              "  card thread. If the recipe demands a different destination than ONE\n"
              "  forward column (e.g. a review FAIL stays/backtracks), write:\n"
              "  DESTINATION: <COLUMN> — and pp-flow will use that instead."))))))
  "Render card + column recipe + context into ONE self-contained text prompt
     (the executor brief). Includes the card title/state/list/acceptance/
     branch/assignee, the effective column recipe (todo/recipe), repo + build
     context, and the write-back eval snippet (advance one column + post a
     comment). Returns () for a missing card. For a ralph loop this IS the
     plan file; for a subagent it's the task.")

(define todo/get-item
  (lambda (id) (todo/tree/get "todo-item" id)))


(define todo/group-by-list
  (lambda
    (items inbox-id)
    (list/foldl
      (lambda
        (acc item)
        (let
          ((fl (as-str (todo/item/list-id (cdr item)))))
          (todo/alist-push
            (if (string-eq fl "") inbox-id fl)
            item
            acc)))
      (list)
      items))
  "group item rows by their list_id (empty -> inbox) in ONE pass; replaces the per-list 200-item refilters that cost ~7k ticks per nav entry")















;; acceptance + branch fields — borge/workstreams.borg acceptance. These ride
;; the todo-item datum like notes/assignee: todo/add + todo/item/add seed them
;; (default ""), the save writers preserve them via dict-set* on the existing
;; fields, and kanban's dict-set* reorders/renames also preserve them. New
;; cards created by libs/kanban/make-card (a fresh body) simply have "" until
;; set with todo/item/set-acceptance / todo/item/set-branch. Setters write via
;; tree/set-field — a content edit, NOT a state transition, so they deliberately
;; do NOT mint a todo-event.
(define todo/item/acceptance
  (lambda
    (fields)
    "The acceptance criteria body of item fields; empty when absent."
    (let
      ((a (dict-get fields "acceptance")))
      (if (null? a) "" (as-str a)))))

(define todo/item/active
  (lambda
    (items)
    (list/filter
      (lambda
        (row)
        (not (todo/state/archived? (todo/item/state (cdr row)))))
      items)))

(define todo/item/add
  (lambda
    (list-id title)
    (let
      ((lid
        (if
          (string-eq (as-str list-id) "")
          (todo/list/ensure-inbox)
          (as-str list-id))))
      (todo/tree/create
        "todo-item"
        (dict-set*
          "{}"
          (list
            "list_id"
            lid
            "title"
            (as-str title)
            "notes"
            ""
            "state"
            "TODO"
            "due"
            ""
            "position"
            "0"
            "done"
            "false"
            "acceptance"
            ""
            "branch"
            ""
            "branch_kind"
            ""
            "pipeline"
            ""
            "kind"
            ""
            "origin_kind"
            ""
            "origin_ref"
            ""))))))

(define todo/item/add-space
  (lambda
    (list-id space title)
    (let
      ((lid (as-str list-id)))
      (if
        (string-eq lid "")
        ""
        (todo/tree/create
          "todo-item"
          (dict-set*
            "{}"
            (list
              "list_id"
              lid
              "title"
              (as-str title)
              "notes"
              ""
              "state"
              "TODO"
              "due"
              ""
              "position"
              "0"
              "done"
              "false"
              "acceptance"
              ""
              "branch"
              ""
              "branch_kind"
              ""
              "pipeline"
              ""
              "kind"
              ""
              "space"
              (as-str space)))))))
  "Create a todo-item on `list-id` AND stamp the 034 row-level `space`
     field, so the card lands in that shared data space (e.g. the Board)
     rather than the filer's garden. Same fields as todo/item/add, plus
     space. Empty list-id files nothing (the empty string): the register
     path deliberately has NO ensure-inbox fallback (2026-10-02 spec) so
     a missing Board is a loud empty, never a silent private-Inbox
     vanish.")

(define todo/item/all-shared
  (lambda
    ()
    (list/map
      todo/tree/node->pair
      (list/filter
        (lambda
          (nj)
          (string-eq (as-str (dict-get nj "type")) "todo-item"))
        (tree/list (todo/tree/shared-prefix) 3 5000))))
  "The SHARED data space's todo-items — datum rows under data.s.*. And a
     Board on any space may hold them, so scan the data.s root at type
     depth. Same (id . fields) shape as todo/item/all.")

(define todo/item/all
  (lambda
    ()
    (todo/list/merge (todo/tree/rows "todo-item" 200) (todo/item/all-shared)))
  "All todo-items the identity can see: GARDEN items (data.u.<user>.*)
     merged with SHARED items (data.s.*, the Board). Deduped by id, garden
     rows first. borge/workstreams.borg shared-register: the /issues
     register, /todos and /kanban read this pool, so a card filed to the
     shared Board is visible to EVERY identity. The list pool
     (todo/list/all) got the same merge; without it here the Board's cards
     were invisible after migration 034 moved them into data.s.board.*.")

(define todo/item/archive
  (lambda (id) (todo/item/set-state id "ARCHIVED")))

(define todo/item/archived
  (lambda
    ()
    (list/filter
      (lambda
        (row)
        (todo/state/archived? (todo/item/state (cdr row))))
      (todo/item/all))))

;; ------------------------------------------------------------------
;; assignment (claim) + priority/queue — ported from gregor's
;; todo-assignment branch (0f1b9a4 + b7059e6) into master's single
;; libs/todo/lib.pp. Provides the claim/queue prims the workstreams
;; orchestration protocol depends on. (The kanban make-card/move-card/
;; stale + /kanban-review functions from 133b228/89e5ddf already live in
;; libs/kanban/lib.pp and are not duplicated here.)
;; ------------------------------------------------------------------

(define todo/item/assign
  (lambda
    (id user)
    "Assign item id to user (empty user clears). Returns id, or () when the item is missing."
    (let
      ((row (todo/tree/get "todo-item" (as-str id))))
      (if
        (null? row)
        ()
        (seq
          (todo/tree/set-field "todo-item" (as-str id) "assignee" (as-str user))
          (as-str id))))))

(define todo/item/assignee
  (lambda
    (fields)
    "The assignee field of item fields; empty when absent."
    (let
      ((a (dict-get fields "assignee")))
      (if
        (null? a)
        ""
        (as-str a)))))

(define todo/item/branch
  (lambda
    (fields)
    "The target branch for the work; empty when absent. The NAME alone is
     ambiguous — pair it with todo/item/branch-kind (git | habitat) per
     borge/workstreams.borg branch-domain, or set both with set-target."
    (let
      ((b (dict-get fields "branch")))
      (if (null? b) "" (as-str b)))))

;; branch DOMAIN (branch_kind) — borge/workstreams.borg branch-domain.
;; Pricklypear has TWO branch systems and `branch` alone is ambiguous:
;;   "git"     — a git branch in the laptop repo (master, a feature branch);
;;               the executor commits/pushes there. Repo-side work (.borg,
;;               OCaml, scripts).
;;   "habitat" — a Nopales/Postgres branch on the habitat (system/main,
;;               <user>/personal, <user>/dev); the executor defines/routes/
;;               promotes there. NEVER a git branch.
;; An agent given a bare "admin/dev" cannot know which repo/domain to touch
;; (2026-09-27: a repo-side card was tagged with a habitat branch, and the
;; executor committed to git master — correct, but only by luck). The brief
;; renders the domain explicitly and FLAGS an unset one. Content edit via
;; tree/set-field, NOT a state transition (no todo-event), like branch above.
(define todo/item/branch-kind
  (lambda
    (fields)
    "The branch DOMAIN of item fields: \"git\" (laptop repo branch) or
     \"habitat\" (Nopales/Postgres branch); empty when unset — callers must
     treat empty as AMBIGUOUS, not as a default."
    (let
      ((k (dict-get fields "branch_kind")))
      (if (null? k) "" (as-str k)))))

(define todo/item/count-all
  (lambda
    (items)
    (list/foldl (lambda (acc _) (+ acc 1)) 0 items)))

(define todo/item/count-open
  (lambda
    (items)
    (list/foldl
      (lambda
        (acc row)
        (if
          (todo/state/open? (todo/item/state (cdr row)))
          (+ acc 1)
          acc))
      0
      items)))

(define todo/item/cycle
  (lambda
    (id)
    (let
      ((row (todo/tree/get "todo-item" id)))
      (if
        (null? row)
        ()
        (let
          ((cur (todo/item/state row)))
          (if
            (todo/state/archived? cur)
            cur
            (todo/item/set-state
              id
              (todo/state/next-for-item cur id))))))))

(define todo/item/delete
  (lambda (id) (todo/tree/del "todo-item" id)))

(define todo/item/due
  (lambda
    (fields)
    (let
      ((d (dict-get fields "due")))
      (if (null? d) "" (as-str d)))))

(define todo/item/for-list
  (lambda
    (list-id)
    (let
      ((lid (as-str list-id))
        (inbox-id (todo/list/ensure-inbox)))
      (list/filter
        (lambda
          (row)
          (let
            ((fl (todo/item/list-id (cdr row))))
            (if
              (string-eq fl lid)
              #t
              (if (string-eq fl "") (string-eq lid inbox-id) #f))))
        (todo/item/all)))))

(define todo/item/for-user
  (lambda
    (user)
    "All items assigned to user, as rows."
    (let
      ((u (as-str user)))
      (list/filter
        (lambda
          (row)
          (string-eq (todo/item/assignee (cdr row)) u))
        (todo/item/all)))))

(define todo/item/get
  (lambda (id) (todo/tree/get "todo-item" id)))

;; KIND field precedes; item/html follows.
;; ARITY PRESERVED (2026-10-18 budget fix): (view) stays 1-arg on purpose.
;; Other identities pin this name on their gardens (gregor's is a diverged
;; own pin), and a shared-main arity change would break them the moment
;; they inherit the new todo/ui/main. The expensive duplication this fix
;; targets was 'everything' scanning the board AND the note scrape
;; repeatedly — todo/ui/main now owns the smart view and calls this only
;; for the archived / single-list arms, each of which scans ONCE.
(define todo/item/html
  (lambda
    (view)
    (if
      (string-eq view "archived")
      (html/ul
        (html/attr "class" "card todo-list")
        (list/foldl
          (lambda
            (acc row)
            (string-append acc (todo/item/list-li row view)))
          ""
          (list/take
            (todo/item/active (todo/item/archived))
            todo/page/cap)))
      (html/ul
        (html/attr "class" "card todo-list")
        (list/foldl
          (lambda
            (acc row)
            (string-append acc (todo/item/list-li row view)))
          ""
          (list/take
            (todo/item/active (todo/item/for-list view))
            todo/page/cap)))))
  "Render a view's CARD rows. OBITUARY (2026-10-18 budget fix): the
     'everything' arm used to live HERE and rebuilt the whole smart view
     inline — heading + rows + the from-notes sections — so the page paid
     the card scan three times and the note scrape twice, blowing the 400k
     request budget (500). todo/ui/main now assembles the smart view ONCE
     and calls this for the archived / single-list arms only.")

;; kind field — borge/workstreams.borg pipeline-model issues-convergence:
;; a todo-item with a kind tag (BUG/DESIGN/SMELL/CLEANUP) is READ AS AN
;; ISSUE ROW on /issues, rendered through the shared state pipeline (own
;; override > list columns > global default). Body builders seed "";
;; set-kind is a content edit via tree/set-field (not a state transition,
;; no todo-event), mirroring acceptance/branch above.
(define todo/item/kind
  (lambda
    (fields)
    "The issue-kind tag of item fields (BUG/DESIGN/SMELL/CLEANUP); empty when
     absent. A non-empty kind is the /issues convergence flag: the card shows
     up as an issue row through the shared pipeline."
    (let
      ((k (dict-get fields "kind")))
      (if (null? k) "" (as-str k)))))

(define todo/item/list-id
  (lambda
    (fields)
    (let
      ((lid (dict-get fields "list_id")))
      (if (null? lid) "" (as-str lid)))))

(define todo/item/list-li
  (lambda
    (row view)
    (let
      ((id (car row)) (fields (cdr row)))
      (let
        ((title (todo/item/title fields))
          (state (todo/item/state fields))
          (open (todo/state/open? (todo/item/state fields))))
        (html/li
          (html/attr
            "class"
            (if
              open
              "todo-item"
              (if
                (todo/state/archived? state)
                "todo-item is-done is-archived"
                "todo-item is-done")))
          (html/join
            (list
              (html/form
                "POST"
                "/todos"
                (html/attrs
                  (list
                    (html/attr "class" "todo-check")
                    (html/attr "hx-post" "/todos")
                    (html/attr "hx-target" "#todo-main")
                    (html/attr "hx-swap" "innerHTML")))
                (html/join
                  (list
                    (html/hidden "action" "cycle")
                    (html/hidden "id" id)
                    (html/hidden "view" view)
                    (html/button
                      (html/attrs
                        (list
                          (html/attr "type" "submit")
                          (html/attr "class" (string-append "todo-box st-" state))
                          (html/attr "title" "cycle state")))
                      (if (string-eq state "") "·" state)))))
              (html/a
                (string-append "/todos?id=" id "&view=" view)
                (html/span
                  (html/attr "class" "todo-title")
                  (html/text title)))
              (if
                (string-eq state "DONE")
                (html/form
                  "POST"
                  "/todos"
                  (html/attrs
                    (list
                      (html/attr "class" "todo-archive")
                      (html/attr "hx-post" "/todos")
                      (html/attr "hx-target" "#todo-main")
                      (html/attr "hx-swap" "innerHTML")))
                  (html/join
                    (list
                      (html/hidden "action" "archive")
                      (html/hidden "id" id)
                      (html/hidden "view" view)
                      (html/button
                        (html/attrs
                          (list
                            (html/attr "type" "submit")
                            (html/attr "class" "secondary")
                            (html/attr "title" "archive done task")))
                        "arch"))))
                "")
              (html/form
                "POST"
                "/todos"
                (html/attrs
                  (list
                    (html/attr "class" "todo-del")
                    (html/attr "hx-post" "/todos")
                    (html/attr "hx-target" "#todo-main")
                    (html/attr "hx-swap" "innerHTML")))
                (html/join
                  (list
                    (html/hidden "action" "delete")
                    (html/hidden "id" id)
                    (html/hidden "view" view)
                    (html/button
                      (html/attrs
                        (list
                          (html/attr "type" "submit")
                          (html/attr "class" "secondary")
                          (html/attr "title" "delete")))
                      "del")))))))))))

(define todo/item/note-li
  (lambda
    (t view)
    (let
      ((slug (car t))
        (nid (car (cdr t)))
        (title (car (cdr (cdr t))))
        (state (car (cdr (cdr (cdr t))))))
      (let
        ((open (todo/state/open? state)))
        (html/li
          (html/attr
            "class"
            (if
              open
              "todo-item todo-from-note"
              "todo-item todo-from-note is-done"))
          (html/join
            (list
              (html/form
                "POST"
                "/todos"
                (html/attrs
                  (list
                    (html/attr "class" "todo-check")
                    (html/attr "hx-post" "/todos")
                    (html/attr "hx-target" "#todo-main")
                    (html/attr "hx-swap" "innerHTML")))
                (html/join
                  (list
                    (html/hidden "action" "cycle-note")
                    (html/hidden "slug" slug)
                    (html/hidden "node-id" nid)
                    (html/hidden "view" view)
                    (html/button
                      (html/attrs
                        (list
                          (html/attr "type" "submit")
                          (html/attr "class" (string-append "todo-box st-" state))
                          (html/attr "title" "cycle note TODO")))
                      (if (string-eq state "") "·" state)))))
              (html/span
                (html/attr "class" "todo-title")
                (html/text title))
              (html/a
                (string-append "/notes?slug=" slug)
                (html/span
                  (html/attr "class" "muted todo-src")
                  (html/text slug))))))))))

(define todo/item/notes
  (lambda
    (fields)
    (let
      ((n (dict-get fields "notes")))
      (if (null? n) "" (as-str n)))))

(define todo/item/prio-insert
  (lambda
    (sorted row)
    "Insert row into a prio-ordered list (insertion sort step)."
    (if
      (null? sorted)
      (list row)
      (if
        (todo/item/prio-le? row (car sorted))
        (cons row sorted)
        (cons (car sorted) (todo/item/prio-insert (cdr sorted) row))))))

(define todo/item/prio-le?
  (lambda
    (a b)
    "True when row a sorts before or equal to row b by (priority rank, position, id)."
    (let
      ((ra (todo/item/prio-rank (cdr a)))
        (rb (todo/item/prio-rank (cdr b))))
      (if
        (< ra rb)
        #t
        (if
          (> ra rb)
          #f
          (let
            ((pa (todo/item/prio-pos (cdr a)))
              (pb (todo/item/prio-pos (cdr b))))
            (if
              (< pa pb)
              #t
              (if
                (> pa pb)
                #f
                (string<=? (car a) (car b))))))))))

(define todo/item/prio-pos
  (lambda
    (fields)
    "The position field as a number; 0 when absent or non-numeric."
    (let
      ((pos (dict-get fields "position")))
      (if
        (null? pos)
        0
        (let
          ((s (as-str pos)))
          (if
            (string-eq s "")
            0
            (parse-int s)))))))

(define todo/item/prio-rank
  (lambda
    (fields)
    "Numeric work rank of the priority field: P0=0 .. P3=3, unknown or empty = 4."
    (let
      ((p (todo/item/priority fields)))
      (if
        (string-eq p "P0")
        0
        (if
          (string-eq p "P1")
          1
          (if
            (string-eq p "P2")
            2
            (if
              (string-eq p "P3")
              3
              4)))))))

(define todo/item/prio-sorted
  (lambda
    (rows)
    "Rows ordered by priority rank, then position, then id (insertion sort; no sort prim in-image)."
    (list/foldl todo/item/prio-insert () rows)))

(define todo/item/priority
  (lambda
    (fields)
    "The priority field of item fields (P0..P3); empty when absent."
    (let
      ((p (dict-get fields "priority")))
      (if
        (null? p)
        ""
        (as-str p)))))

(define todo/item/queue
  (lambda
    (user)
    "Open items in work order (priority, then position, then id). Empty user = all assignees."
    (let
      ((u (as-str user)))
      (todo/item/prio-sorted
        (list/filter
          (lambda
            (row)
            (if
              (string-eq u "")
              #t
              (string-eq (todo/item/assignee (cdr row)) u)))
          (todo/item/active (todo/item/all)))))))

(define todo/item/save
  (lambda
    (id title notes due state list-id)
    (let
      ((fields (todo/item/get id)))
      (if
        (null? fields)
        ()
        (let
          ((prev (todo/item/state fields))
            (next (as-str state))
            (lid
              (if
                (string-eq (as-str list-id) "")
                (todo/list/ensure-inbox)
                (as-str list-id))))
          (seq
            (todo/tree/save
              "todo-item"
              id
              (dict-set*
                fields
                (list
                  "title"
                  (as-str title)
                  "notes"
                  (as-str notes)
                  "due"
                  (as-str due)
                  "state"
                  next
                  "list_id"
                  lid)))
            (todo/node/mirror id next)
            (todo/event/transition! id prev next)))))))

(define todo/item/set-acceptance
  (lambda
    (id text)
    "Set card id's acceptance criteria (empty clears). Returns id, or () when the item is missing."
    (let
      ((row (todo/tree/get "todo-item" (as-str id))))
      (if
        (null? row)
        ()
        (seq
          (todo/tree/set-field "todo-item" (as-str id) "acceptance" (as-str text))
          (as-str id))))))

(define todo/item/set-branch
  (lambda
    (id branch)
    "Set card id's target branch (empty clears). Returns id, or () when the
     item is missing. Does NOT set the domain — an unset domain is ambiguous;
     prefer todo/item/set-target for scripting."
    (let
      ((row (todo/tree/get "todo-item" (as-str id))))
      (if
        (null? row)
        ()
        (seq
          (todo/tree/set-field "todo-item" (as-str id) "branch" (as-str branch))
          (as-str id))))))

(define todo/item/set-branch-kind
  (lambda
    (id kind)
    "Set card id's branch domain to \"git\" or \"habitat\" (empty clears).
     Returns id, or () when the item is missing. Empty is treated as
     AMBIGUOUS by the brief, not as a default."
    (let
      ((row (todo/tree/get "todo-item" (as-str id))))
      (if
        (null? row)
        ()
        (seq
          (todo/tree/set-field "todo-item" (as-str id) "branch_kind" (as-str kind))
          (as-str id))))))

(define todo/item/set-kind
  (lambda
    (id kind)
    "Set card id's kind tag (empty clears — card leaves the /issues rows).
     Returns id, or () when the item is missing."
    (let
      ((row (todo/tree/get "todo-item" (as-str id))))
      (if
        (null? row)
        ()
        (seq
          (todo/tree/set-field "todo-item" (as-str id) "kind" (as-str kind))
          (as-str id))))))

(define todo/item/set-priority
  (lambda
    (id p)
    "Set item id's priority (P0 highest .. P3, empty clears). Returns id, or () when the item is missing."
    (let
      ((row (todo/tree/get "todo-item" (as-str id))))
      (if
        (null? row)
        ()
        (seq
          (todo/tree/set-field "todo-item" (as-str id) "priority" (as-str p))
          (as-str id))))))

(define todo/item/set-state
  (lambda
    (id state)
    "Set card state, recording a todo-event when it actually changes. Returns
     the event id (real transition) or () (no change / missing card). The
     choke point: todo/item/cycle, archive, the item save, and future move
     writers all record transitions through here."
    (let
      ((cur (todo/tree/get "todo-item" (as-str id))))
      (if
        (null? cur)
        (list)
        (let
          ((prev (todo/item/state cur))
            (next (as-str state)))
          (seq
            (todo/tree/set-field "todo-item" (as-str id) "state" next)
            (todo/event/transition! id prev next)))))))

(define todo/item/set-target
  (lambda
    (id branch kind)
    "Set card id's target branch AND its domain in one call — kind must be
     \"git\" or \"habitat\"; empty branch clears both. Prefer this over
     set-branch when scripting: a branch without a domain is the ambiguity
     borge/workstreams.borg branch-domain exists to kill. Returns id, or ()
     when the item is missing."
    (let
      ((row (todo/tree/get "todo-item" (as-str id))))
      (if
        (null? row)
        ()
        (seq
          (todo/tree/set-field "todo-item" (as-str id) "branch" (as-str branch))
          (todo/tree/set-field "todo-item" (as-str id) "branch_kind" (as-str kind))
          (as-str id))))))

(define todo/item/state
  (lambda
    (fields)
    (let
      ((s (dict-get fields "state")))
      (if
        (null? s)
        (let
          ((d (dict-get fields "done")))
          (if
            (null? d)
            "TODO"
            (if (string-eq d "true") "DONE" "TODO")))
        (as-str s)))))

(define todo/item/title
  (lambda (fields) (as-str (dict-get fields "title"))))

(define todo/item/unassign
  (lambda
    (id)
    "Clear item id's assignee."
    (todo/item/assign id "")))

(define todo/items-for
  (lambda
    (lid all-items inbox-id)
    (list/filter
      (lambda
        (row)
        (let
          ((fl (as-str (todo/item/list-id (cdr row)))))
          (if
            (string-eq fl lid)
            #t
            (if (string-eq fl "") (string-eq lid inbox-id) #f))))
      all-items))
  "pure: rows of one list from a prefetched items list (replaces per-list DB re-fetch)")

(define todo/items-for-list
  (lambda
    (list-id)
    (let
      ((lid (as-str list-id)) (inbox-id (todo/ensure-inbox)))
      (list/filter
        (lambda
          (row)
          (let
            ((fl (todo/item/list-id (cdr row))))
            (if
              (string-eq fl lid)
              #t
              (if (string-eq fl "") (string-eq lid inbox-id) #f))))
        (todo/item/all)))))



(define todo/land-ping
  1)









(define todo/list-options
  (lambda
    (selected)
    (list/foldl
      (lambda
        (acc row)
        (let
          ((id (car row)))
          (let
            ((title (todo/list/title (cdr row))))
            (string-append
              acc
              (html/option
                id
                title
                (string-eq (as-str id) (as-str selected)))))))
      ""
      (todo/list/all))))



(define todo/list/find-by-title
  (lambda
    (title)
    (let*
      ((want (as-str title))
        (hit
          (list/filter
            (lambda
              (l)
              (string-eq (as-str (dict-get (cdr l) "title")) want))
            (todo/list/all))))
      (if (null? hit) () (car hit))))
  "The (id . fields) pair of the first todo-list whose title equals title,
     or () when none. Used by the shared-space naming migration to resolve
     the board/AUTO lane by TITLE rather than a per-garden uuid.")

(define todo/list/set-columns
  (lambda
    (list-id cols)
    (let
      ((lid (as-str list-id))
        (lf (todo/tree/get "todo-list" (as-str list-id))))
      (if
        (null? lf)
        "no such list"
        (seq
          (todo/tree/save
            "todo-list"
            lid
            (dict-set* lf (list "columns" (json-emit cols))))
          lid))))
  "Set a todo-list's `columns` field (its own pipeline, a JSON array of
     state strings). Returns the list id, or \"no such list\". Local to the
     todo family so it does not depend on the kanban package.")

(define todo/list/space-of
  (lambda
    (row)
    (let
      ((s (dict-get (cdr row) "space")))
      (if (null? s) "" (as-str s))))
  "The 034 row-level space of a list row (body \"space\" field); \"\" for
     an ordinary garden list. Callers route a card to the list's space so
     a shared Board card does not fall back into the filer's garden.")

(define todo/list/all-shared
  (lambda
    ()
    (list/map
      todo/tree/node->pair
      (list/filter
        (lambda
          (nj)
          (string-eq (as-str (dict-get nj "type")) "todo-list"))
        (tree/list (todo/tree/shared-prefix) 3 5000))))
  "The SHARED data space's lists — datum rows under data.s.*.space.*
     (034 row-level routing; the Board lives here). Scans the shared root
     at type depth and keeps todo-list nodes: the space label is dynamic
     (any space may hold one), so a prefix scan is more faithful than a
     single synthesized path. Same (id . fields) shape as todo/list/all.")

(define todo/list/all
  (lambda
    ()
    (todo/list/merge (todo/tree/rows "todo-list" 50) (todo/list/all-shared)))
  "All lists the identity can see: GARDEN lists (data.u.<user>.*) merged
     with SHARED lists (data.s.*, the Board). Deduped by id, garden rows
     first. borge/workstreams.borg shared-register: notes/issues/board-list-id
     runs over this, so the shared Board resolves for EVERY identity.
     additivity caveat: pre-034 pins of an existing title had no space field
     and stay in the garden, so both surfaces are merged rather than one
     replacing the other.")

(define todo/list/merge
  (lambda
    (garden shared)
    (if
      (null? shared)
      garden
      (let
        ((sid (as-str (car (car shared)))))
        (todo/list/merge
          (todo/list/merge-one garden sid (car shared))
          (cdr shared)))))
  "Merge shared `shared` into `garden`, deduping by id: an id already in
     the garden keeps its garden row (no double-listing); a new id is
     appended.")

(define todo/list/merge-one
  (lambda
    (rows id row)
    (if
      (null? rows)
      (list row)
      (if
        (string-eq (as-str (car (car rows))) id)
        rows
        (append rows (list row)))))
  "Cons/normalize helper for todo/list/merge: returns `rows` unchanged when
     `id` is already present, else `rows` with `row` appended as a PROPER list.
     Uses append (not cons) so the result is a VList: a cons-chain is accepted
     by car/cdr/map/filter but rejected by the strict (count), which bit
     todo/item/all once the shared side became non-empty (2026-10-03).")

(define todo/list/create
  (lambda
    (title)
    (todo/tree/create
      "todo-list"
      (dict-set*
        "{}"
        (list
          "title"
          (as-str title)
          "description"
          ""
          "is_default"
          "false")))))

(define todo/list/default?
  (lambda
    (fields)
    (let
      ((d (dict-get fields "is_default")))
      (if (null? d) #f (string-eq d "true")))))

(define todo/list/delete
  (lambda
    (list-id)
    (let
      ((lid (as-str list-id)))
      (if
        (string-eq lid "")
        "missing"
        (let
          ((fields (todo/list/get lid)))
          (if
            (null? fields)
            "missing"
            (if
              (todo/list/default? fields)
              "default"
              (seq
                (todo/list/delete-items (todo/item/all) lid)
                (seq (todo/tree/del "todo-list" lid) "ok")))))))))

(define todo/list/delete-items
  (lambda
    (items lid)
    (if
      (null? items)
      ()
      (seq
        (if
          (string-eq
            (todo/item/list-id (cdr (car items)))
            (as-str lid))
          (todo/item/delete (car (car items)))
          ())
        (todo/list/delete-items (cdr items) lid)))))

(define todo/list/ensure-inbox
  (lambda
    ()
    (let
      ((found (todo/list/find-default)))
      (if
        (null? found)
        (todo/tree/create
          "todo-list"
          (dict-set*
            "{}"
            (list
              "title"
              "Inbox"
              "description"
              "Default list"
              "is_default"
              "true")))
        (car found)))))

(define todo/list/find-default
  (lambda () (todo/list/find-default-in (todo/list/all))))

;; ------------------------------------------------------------------
;; SHARED OPEN SPACE (borge/workstreams.borg shared-store)
;;
;; The board, its lists and its cards are collaboration furniture: every
;; authenticated identity shares ONE board. The mechanism is the existing
;; open-space substrate (`data.s.<space>.<type>.<id>`, migration 030) —
;; the chat lobby is the precedent. Admission is TYPE-level and routes
;; FUTURE writes; the todo types admitted here are todo-list, todo-item,
;; todo-comment, todo-event, todo-dep.
;;
;; ORDERING HAZARD: admission changes (node-path 'todo-item ...), so
;; ensure-space MUST run before any read that computes a prefix. Every
;; todo entry point (todo/page, kanban/page, workstreams/page) calls
;; todo/ensure-space at the top.
;; ------------------------------------------------------------------

(define todo/open-space
  "todo")

(define todo/admitted-types
  (list "todo-list" "todo-item" "todo-comment" "todo-event" "todo-dep"))

(define todo/ensure-space
  (lambda
    ()
    (if
      (string-eq (as-str (whoami)) "anonymous")
      #f
      (guard
        (lambda (e) #f)
        (seq
          (if
            (space-exists? todo/open-space)
            #t
            (space-create todo/open-space (quote open)))
          (list/foldl
            (lambda (acc t)
              (seq
                (guard
                  (lambda (e) #f)
                  (space-admit t todo/open-space))
                acc))
            #t
            todo/admitted-types)))))
  "Create the shared open `todo` space and admit the card types
     (idempotent, best-effort; the first writer races safely). Ensures
     the space exists AND the admissions are present even when the space
     predates an admission — so a repair is `(todo/ensure-space)`.
     Anonymous is a no-op (guests keep their own garden), and a
     non-owner (any non-first identity) silently skips the owner-only
     admit — the space already exists and is open, so membership is
     implicit.")

(define todo/space-path
  (lambda (type-name id)
    (node-path type-name (as-str id)))
  "The space-routed node path for a todo datum (assumes the type is
     admitted). Thin wrapper over node-path for readability.")

(define todo/adopt-stranded
  (lambda
    ()
    (with-transaction
      (lambda
        ()
        (let
          ((scan
            (list/foldl
              (lambda
                (acc t)
                (list/foldl
                  (lambda
                    (a n)
                    (if
                      (string-eq (as-str (dict-get n "type")) t)
                      (let
                        ((old (as-str (dict-get n "path"))))
                        (let
                          ((id (as-str (dict-get n "id"))))
                          (let
                            ((new (todo/space-path t id)))
                            (let
                              ((has-new (tree/get new)))
                              (if
                                (null? has-new)
                                (seq
                                  (tree/move old new)
                                  (cons (+ (car a) 1) (cdr a)))
                                (cons (car a) (+ (cdr a) 1)))))))
                      a))
                  acc
                  (tree/list-kind "data.u" "datum" 4000)))
              (cons 0 0)
              todo/admitted-types)))
          (json-emit
            (dict-set*
              "{}"
              (list
                "moved"
                (car scan)
                "skipped"
                (cdr scan))))))))
  "One-time adoption: move every stranded `data.u.*` node of the four
     todo types (all identities — the reads are not RLS-filtered and the
     spec deliberately publishes the pre-adoption backlog) into the shared
     `todo` space, preserving id, body, author and created_at. Idempotent:
     a row whose target already exists is skipped. The whole scan runs in
     ONE with-transaction (a tree/move is a write; the multi-write sequence
     must be atomic). Returns JSON {\"moved\":N,\"skipped\":M}.")

(define todo/resolve-board
  (lambda
    ()
    (let
      ((hit (todo/list/find-by-title "Board")))
      (if (null? hit) (todo/list/ensure-inbox) (as-str (car hit)))))
  "The shared Board list id: the todo-list titled \"Board\", falling back
     to the Inbox. Resolves by TITLE (never a hardcoded per-garden uuid).")

(define todo/resolve-auto-lane
  (lambda
    ()
    (let
      ((hit (todo/list/find-by-title "AUTO")))
      (if (null? hit) "" (as-str (car hit)))))
  "The AUTO-lane list id: the todo-list titled \"AUTO\", or \"\" when absent.")

(define todo/setup-auto-lane
  (lambda
    ()
    (let
      ((aid (todo/resolve-auto-lane)))
      (if
        (string-eq aid "")
        ""
        (todo/list/set-columns
          aid
          (list "TODO" "CODE" "LOGIC-REVIEW" "DONE" "PARKED")))))
  "Give the AUTO lane its spec-mandated columns (NO HUMAN-REVIEW):
     [TODO CODE LOGIC-REVIEW DONE PARKED]. Returns the list id or \"\".
     borge/workstreams.borg stream-pipelines: an AUTO LOGIC-REVIEW PASS
     lands on DONE, so the human gate must not be a column.")

(define todo/list/find-default-in
  (lambda
    (xs)
    (if
      (null? xs)
      ()
      (if
        (todo/list/default? (cdr (car xs)))
        (car xs)
        (let
          ((t (todo/list/title (cdr (car xs)))))
          (if
            (string-eq t "Inbox")
            (car xs)
            (todo/list/find-default-in (cdr xs))))))))

(define todo/list/get
  (lambda
    (id)
    (let
      ((lid (as-str id)))
      (if
        (string-eq lid "")
        (let
          ((inbox (todo/list/find-default)))
          (if (null? inbox) (list) (cdr inbox)))
        (todo/tree/get "todo-list" lid))))
  "empty list-id resolves the DEFAULT INBOX (fields) instead of asking tree/get for `data.u.<user>.todo-list.` — uuid_label \"\" yields an empty ltree label and PG answers 42601 (reproduced live). find-default is a read (no create side-effect); todo/page already ensure-inbox's, so the default exists on any rendered page. No default list yet -> ().")

(define todo/list/nav-li
  (lambda
    (view row)
    (let
      ((id (car row)) (f (cdr row)))
      (html/li
        (html/attr
          "class"
          (if
            (string-eq view id)
            "todo-nav-item is-active todo-nav-list"
            "todo-nav-item todo-nav-list"))
        (html/join
          (list
            (html/a
              (string-append "/todos?view=" id)
              (todo/list/title f))
            (if
              (todo/list/default? f)
              ""
              (html/form
                "POST"
                "/todos"
                (html/attrs
                  (list
                    (html/attr "class" "todo-list-del")
                    (html/attr "hx-post" "/todos")
                    (html/attr "hx-target" "#todo-main")
                    (html/attr "hx-swap" "innerHTML")
                    (html/attr
                      "hx-confirm"
                      "Delete this entire list and its items?")))
                (html/join
                  (list
                    (html/hidden "action" "delete-list")
                    (html/hidden "list_id" id)
                    (html/hidden "view" "everything")
                    (html/button
                      (html/attrs
                        (list
                          (html/attr "type" "submit")
                          (html/attr "class" "secondary")
                          (html/attr "title" "delete list")))
                      "×"))))))))))
  "nav entry for one list: title link + delete form. Ratio counts DROPPED — per-list ratios cost a dict-get JSON parse per item (~1.5k ticks each) and blew the 400k route budget; the list page shows its own counts.")

(define todo/list/options
  (lambda
    (selected)
    (list/foldl
      (lambda
        (acc row)
        (let
          ((id (car row)))
          (let
            ((title (todo/list/title (cdr row))))
            (string-append
              acc
              (html/option
                id
                title
                (string-eq (as-str id) (as-str selected)))))))
      ""
      (todo/list/all))))

(define todo/list/title
  (lambda (fields) (as-str (dict-get fields "title"))))









(define todo/nav-list-li
  (lambda
    (view row)
    (let
      ((id (car row))
        (f (cdr row))
        (items
          (todo/active-items (todo/items-for-list (car row)))))
      (html/li
        (html/attr
          "class"
          (if
            (string-eq view id)
            "todo-nav-item is-active todo-nav-list"
            "todo-nav-item todo-nav-list"))
        (html/join
          (list
            (html/a
              (string-append "/todos?view=" id)
              (string-append
                (todo/list/title f)
                " ("
                (todo/ratio items)
                ")"))
            (if
              (todo/list/default? f)
              ""
              (html/form
                "POST"
                "/todos"
                (html/attrs
                  (list
                    (html/attr "class" "todo-list-del")
                    (html/attr "hx-post" "/todos")
                    (html/attr "hx-target" "#todo-main")
                    (html/attr "hx-swap" "innerHTML")
                    (html/attr
                      "hx-confirm"
                      "Delete this entire list and its items?")))
                (html/join
                  (list
                    (html/hidden "action" "delete-list")
                    (html/hidden "list_id" id)
                    (html/hidden "view" "everything")
                    (html/button
                      (html/attrs
                        (list
                          (html/attr "type" "submit")
                          (html/attr "class" "secondary")
                          (html/attr "title" "delete list")))
                      "×")))))))))))

(define todo/next-state
  (lambda
    (cur)
    (outline/todo-next-in todo/state/list (as-str cur))))

(define todo/note-item-li
  (lambda
    (t view)
    (let
      ((slug (car t))
        (nid (car (cdr t)))
        (title (car (cdr (cdr t))))
        (state (car (cdr (cdr (cdr t))))))
      (let
        ((open (todo/state/open? state)))
        (html/li
          (html/attr
            "class"
            (if
              open
              "todo-item todo-from-note"
              "todo-item todo-from-note is-done"))
          (html/join
            (list
              (html/form
                "POST"
                "/todos"
                (html/attrs
                  (list
                    (html/attr "class" "todo-check")
                    (html/attr "hx-post" "/todos")
                    (html/attr "hx-target" "#todo-main")
                    (html/attr "hx-swap" "innerHTML")))
                (html/join
                  (list
                    (html/hidden "action" "cycle-note")
                    (html/hidden "slug" slug)
                    (html/hidden "node-id" nid)
                    (html/hidden "view" view)
                    (html/button
                      (html/attrs
                        (list
                          (html/attr "type" "submit")
                          (html/attr "class" (string-append "todo-box st-" state))
                          (html/attr "title" "cycle note TODO")))
                      (if (string-eq state "") "·" state)))))
              (html/span
                (html/attr "class" "todo-title")
                (html/text title))
              (html/a
                (string-append "/notes?slug=" slug)
                (html/span
                  (html/attr "class" "muted todo-src")
                  (html/text slug))))))))))

(define todo/notes-block
  (lambda
    (view)
    (let
      ((row (todo/notes-cache-row)))
      (let
        ((fields (if (null? row) () (cdr row))))
        (if
          (todo/notes-cache-fresh? fields 900)
          (todo/notes-cache-html fields view)
          (seq
            (todo/notes-cache-request)
            (todo/notes-cache-html fields view))))))
  "HTML <li> block for a view from the scrape cache. Never computes inline: serves cached (possibly stale) HTML and asks the jobs runner to refresh. The inline scrape cost 8-78s per render and blew route budgets.")

(define todo/notes-cache-fresh?
  (lambda
    (fields ttl)
    (if
      (null? fields)
      #f
      (let
        ((ts (dict-get fields "ts")))
        (if
          (null? ts)
          #f
          (let
            ((age (- (now) (parse-int (as-str ts)))))
            (if (> age ttl) #f #t))))))
  "whether the scrape cache is within ttl seconds")

(define todo/notes-cache-html
  (lambda
    (fields view)
    (let
      ((k
        (if
          (string-eq view "everything")
          "html_open"
          "html_all")))
      (let
        ((v (dict-get fields k)))
        (if (null? v) "" (as-str v)))))
  "cached <li> block for a view")

(define todo/notes-cache-refresh
  (lambda
    ()
    (let
      ((todos (notes/outline/all-todos)))
      (let
        ((h-all
            (string-join
              (list/map
                (lambda (t) (todo/item/note-li t "from-notes"))
                todos)
              ""))
          (h-open
            (string-join
              (list/map
                (lambda (t) (todo/item/note-li t "everything"))
                (list/filter
                  (lambda
                    (t)
                    (todo/state/open? (car (cdr (cdr (cdr t))))))
                  todos))
              "")))
        (let
          ((row (todo/notes-cache-row))
            (fields
              (dict-set*
                "{}"
                (list
                  "key"
                  "notes-todos"
                  "ts"
                  (as-str (now))
                  "req"
                  (as-str (now))
                  "html_all"
                  h-all
                  "html_open"
                  h-open))))
          (seq
            (if
              (null? row)
              (create (quote todo-cache) fields)
              (update (quote todo-cache) (car row) fields))
            "refreshed")))))
  "recompute the notes-todo scrape once; O(n) string-join (was O(n^2) fold). Runs as a JOB (unbudgeted), never on the request path.")

(define todo/notes-cache-request
  (lambda
    ()
    (let
      ((row (todo/notes-cache-row)))
      (let
        ((fields (if (null? row) () (cdr row))))
        (let
          ((req
            (if
              (null? fields)
              0
              (let
                ((r (dict-get fields "req")))
                (if (null? r) 0 (parse-int (as-str r)))))))
          (if
            (> (- (now) req) 120)
            (seq
              (if
                (null? row)
                (create
                  (quote todo-cache)
                  (dict-set*
                    "{}"
                    (list
                      "key"
                      "notes-todos"
                      "req"
                      (as-str (now))
                      "ts"
                      "0"
                      "html_all"
                      ""
                      "html_open"
                      "")))
                (update
                  (quote todo-cache)
                  (car row)
                  (dict-set* (cdr row) (list "req" (as-str (now))))))
              (submit-job "todo/notes-cache-refresh" ""))
            "already-requested")))))
  "ask the UNBUDGETED jobs runner to refresh the scrape; bounded to one request/120s so renders never compute inline")

(define todo/notes-cache-row
  (lambda
    ()
    (let
      ((rs
        (find-rows (quote todo-cache) "key" "notes-todos" 1)))
      (if (null? rs) () (car rs))))
  "cache row for the notes-todo scrape (or ())")



(define todo/page
  (lambda
    ()
    (seq
      (todo/ensure-space)
      (todo/list/ensure-inbox)
      (if
        (string-eq (request-method) "POST")
        (seq
          (todo/ui/handle-post)
          (let
            ((view
              (let
                ((v (request-param "view")))
                (if (null? v) "everything" (as-str v)))))
            (if
              (hx-request?)
              (render-html (todo/ui/main view))
              (redirect (string-append "/todos?view=" view)))))
        (html/page
          (list
            (html/h1 "To-dos")
            (html/p*
              (html/attr "class" "muted")
              "Lists (Basecamp) + notes scrape (org). Click a title for details.")
            (todo/ui/main (todo/ui/view))))))))



(define todo/ratio
  (lambda
    (items)
    (let
      ((n (todo/item/count-all items))
        (open (todo/count-open-in items)))
      (string-append (as-str (- n open)) "/" (as-str n)))))

(define todo/recipe
  (lambda
    (list-id column)
    (let
      ((col (as-str column))
        (explicit (assoc (as-str column) (todo/recipes-for-list list-id))))
      (if
        (if
          explicit
          (not (string-eq (as-str (cdr explicit)) ""))
          #f)
        (as-str (cdr explicit))
        (let
          ((dflt (assoc col todo/recipes-default)))
          (if
            dflt
            (as-str (cdr dflt))
            (string-append
              col
              ": work the card above on Pricklypear — follow its acceptance criteria and target branch; build/test with dune; verify; then advance the card one column and comment what you did + evidence."))))))
  "The EFFECTIVE recipe instruction text for column in list-id: the list's
     explicit recipe when set (non-empty), else the default family entry for
     that column (todo/recipes-default), else a generic fallback. Never
     returns () — every column in every list produces an instruction, so a
     flow/brief always has text to hand the executor.")

(define todo/recipe-set
  (lambda
    (list-id column text)
    (let
      ((lf (todo/tree/get "todo-list" (as-str list-id))))
      (if
        (null? lf)
        "no such list"
        (let
          ((cur (dict-get lf "recipes")))
          (let
            ((base (if (null? cur) "" (as-str cur)))
              (col (as-str column))
              (tx (as-str text)))
            (let
              ((newr
                (if
                  (string-eq tx "")
                  (todo/recipes-drop base col)
                  (dict-set base col tx))))
              (let
                ((body (dict-set* lf (list "recipes" newr))))
                (seq
                  (tree/upsert
                    (node-path (quote todo-list) (as-str list-id))
                    "datum" "todo-list" body)
                  (as-str list-id)))))))))
  "Set the recipe instruction text for column on list-id (content edit of the
     LIST datum via tree/upsert — NOT a card state transition, so no
     todo-event is minted). Empty text REMOVES that column's entry. Returns
     the list id, or a no-such-list message.")

;; NOTE (2026-09-27): the two deftype forms run at the TOP of this file —
;; see the comment there. They are deliberately NOT called here as
;; (todo/comments-init!): a tail call executes only when every earlier
;; form evaluated AND is not boot-seed-skipped, so one reader error
;; anywhere above silently skipped the declarations (prod 2026-09-27).

;; ------------------------------------------------------------------
;; column recipes (workstreams.borg column-recipes) — the bridge between
;; RECORD and EXECUTOR. The workstream list datum carries an optional
;; `recipes` JSON dict field mapping COLUMN NAME -> instruction text (the
;; per-column agent recipe). Reads fall back to the built-in default family
;; (todo/recipes-default) so every list gets actionable instructions even
;; before anyone configures it; recipe-set writes the explicit dict.
;; NAMING NOTE (orchestration task 6 resolution): the plan originally named
;; these kanban/recipes-for-list + kanban/recipe-set + kanban/recipe, but the
;; kanban lib (libs/kanban/lib.pp) is OFF-LIMITS this loop (Do-NOT rule), and
;; master's convention is one clean namespace per family file. These live in
;; the todo family as todo/recipes-* (they operate on the todo-list datum);
;; the shared column taxonomy they cover is SPEC/CODE/LOGIC-REVIEW/UI-REVIEW/
;; HUMAN-REVIEW/DONE/PARKED (borge/work-triage.borg board).
;; ------------------------------------------------------------------

(define todo/recipes-default
  (list
    (cons "SPEC"
      "You are the SPEC author for Pricklypear (Nopales/OCaml Lisp OS; the borge book is the spec and the source of truth — code drift means code is wrong). The card above is your work item; its acceptance criteria + target branch ride on the datum. Write/update the borge .borg chapter for it. Finish with `borge drift` clean and the spec committed on the target branch. Then move the card to CODE and post a comment: what you specified + how the reader verifies it.")
    (cons "CODE"
      "You are the IMPLEMENTER for Pricklypear (OCaml 5.4 / Nopales, dune build, borge spec). Implement the card above against its acceptance criteria, on its target branch, in /home/roerick/dev/wyo.tech/pricklypear. Build/test: eval $(opam env --switch=poohstack --set-switch); dune build; dune runtest (lisp/common at least). After .borg edits run `borge drift`. Live-smoke on the target host before write-back. When done, move the card to LOGIC-REVIEW and post a comment: what shipped + how you verified it.")
    (cons "LOGIC-REVIEW"
      "You are the LOGIC REVIEWER for Pricklypear. Review the card's work on its target branch against its acceptance criteria: read the change, check borge drift + build + the stated verification, judge logic. PASS -> move the card to HUMAN-REVIEW and comment your verdict + evidence. FAIL -> move it BACK to CODE with a comment listing exactly what to fix.")
    (cons "UI-REVIEW"
      "You are the UI REVIEWER for Pricklypear. Exercise the card's work on its target branch for interaction + rendering against its acceptance criteria (load the routes, click through). PASS -> move the card to HUMAN-REVIEW and comment. FAIL -> move it BACK to CODE with a comment of what to fix.")
    (cons "HUMAN-REVIEW"
      "ROERICK GATE — do NOT move this card yourself. It is ready for the human gate on its target branch; acceptance criteria are on the datum. Post any notes as comments; leave the state in HUMAN-REVIEW. Only the human passes this gate.")
    (cons "DONE"
      "This card is done. Nothing to execute. If it landed here by accident, move it back to the correct column and comment why.")
    (cons "PARKED"
      "This card is deliberately parked. Do not work it. If the park looks stale, comment and leave it — only the human unparks.")
    (cons "TODO"
      "Intake: place the card into the correct pipeline column — SPEC if it needs a spec/decision before code, CODE if it is executable now — set priority if useful, assign an owner if known, then comment where you placed it and why."))
  "The built-in default recipe family per shared column (work-triage taxonomy).
     todo/recipe falls back to these when a list has no explicit recipe for a
     column. Columns without an entry fall to the generic fallback at read
     time, so a brief always has instructions.")

(define todo/recipes-drop
  (lambda
    (json key)
    (let
      ((k (as-str key)))
      (list/foldl
        (lambda
          (acc jk)
          (if
            (string-eq (as-str jk) k)
            acc
            (dict-set acc (as-str jk) (as-str (dict-get json (as-str jk))))))
        "{}"
        (dict-keys json))))
  "Rebuild a JSON dict without key (empty/absent dict stays {}). Helper for
     todo/recipe-set removal — there is no dict-del prim, so drop = fold.")

(define todo/recipes-for-list
  (lambda
    (list-id)
    (let
      ((lf (todo/tree/get "todo-list" (as-str list-id))))
      (if
        (null? lf)
        (list)
        (let
          ((r (dict-get lf "recipes")))
          (if
            (null? r)
            (list)
            (list/map
              (lambda
                (k)
                (cons
                  (as-str k)
                  (as-str (dict-get r (as-str k)))))
              (dict-keys r)))))))
  "The list's EXPLICIT `recipes` dict as an assoc list ((column . text) ...),
     stored on the todo-list datum; () when the list is missing or has no
     recipes field. Defaults are NOT mixed in — use todo/recipe for the
     effective (fallback-aware) text of one column.")

(define todo/save
  (lambda
    (id title notes due state list-id)
    (let
      ((fields (todo/item/get id)))
      (if
        (null? fields)
        ()
        (let
          ((prev (todo/item/state fields))
            (next (as-str state))
            (lid
              (if
                (string-eq (as-str list-id) "")
                (todo/ensure-inbox)
                (as-str list-id))))
          (seq
            (todo/tree/save
              "todo-item"
              id
              (dict-set*
                fields
                (list
                  "title"
                  (as-str title)
                  "notes"
                  (as-str notes)
                  "due"
                  (as-str due)
                  "state"
                  next
                  "list_id"
                  lid)))
            (todo/event/transition! id prev next)))))))

(define todo/seqtest
  (lambda () 7))

(define todo/set-state
  (lambda
    (id state)
    "Set card state, recording a todo-event when it actually changes. Returns
     the event id (real transition) or () (no change / missing card). The
     choke point: todo/cycle, archive, the save writer, and future move
     writers all record transitions through here."
    (let
      ((cur (todo/tree/get "todo-item" (as-str id))))
      (if
        (null? cur)
        (list)
        (let
          ((prev (todo/item/state cur))
            (next (as-str state)))
          (seq
            (todo/tree/set-field "todo-item" (as-str id) "state" next)
            (todo/event/transition! id prev next)))))))







(define todo/state/archived?
  (lambda (s) (string-eq (as-str s) "ARCHIVED")))

(define todo/state/columns-for-item
  (lambda
    (item-id)
    (cdr (todo/state/item-pipeline (as-str item-id))))
  "The effective pipeline (column-state list) for ONE item: its OWN `pipeline`
     override when present, else its list's columns, else the global default.
     A card can ride a different pipeline than its list's board.")

(define todo/state/columns-for-list
  (lambda
    (list-id)
    (let
      ((lf (get (quote todo-list) (as-str list-id))))
      (if
        (null? lf)
        todo/state/list
        (let
          ((cols (dict-get lf "columns")))
          (if
            (null? cols)
            todo/state/list
            (let
              ((cs (as-str cols)))
              (if (string-eq cs "") todo/state/list (json-parse cs))))))))
  "The state set (list of state strings) for a todo-list: the list's own\n     `columns` field when present and non-empty, else the global default\n     todo/state/list. THE per-list override kanban columns and the /todos\n     state machine both read through, so a list with custom columns cycles\n     only those states everywhere.")

(define todo/state/item-pipeline (lambda
  (item-id)
  (let
    ((row (todo/tree/get "todo-item" (as-str item-id))))
    (if
      (null? row)
      (cons "default" todo/state/list)
      (let
        ((list-id (todo/item/list-id row)))
        (cons
          (if
            (todo/state/list-has-columns? list-id)
            "list"
            "default")
          (todo/state/columns-for-list list-id)))))))

(define todo/state/list
  (list
    ""
    "TODO"
    "SPEC"
    "CODE"
    "LOGIC-REVIEW"
    "UI-REVIEW"
    "HUMAN-REVIEW"
    "DONE"
    "PARKED"))
  ;; borge/workstreams.borg default-pipeline: the GLOBAL DEFAULT is the
  ;; engineering taxonomy, ordered TODO -> SPEC -> CODE -> LOGIC-REVIEW ->
  ;; UI-REVIEW -> HUMAN-REVIEW -> DONE -> PARKED, so a columns-less list
  ;; cannot strand a card outside its own pipeline and a bare todo/cycle is
  ;; a safe advance (TODO -> SPEC). The legacy checklist (WAITING/CANCELLED)
  ;; is not deleted, just no longer the default: a list that wants it sets
  ;; its own `columns`.

(define todo/state/list-has-columns?
  (lambda
    (list-id)
    (let
      ((lf (get (quote todo-list) (as-str list-id))))
      (if
        (null? lf)
        #f
        (let
          ((cols (dict-get lf "columns")))
          (and
            (not (null? cols))
            (not (string-eq (as-str cols) "")))))))
  "True when the todo-list datum carries its own non-empty `columns` field\n     (i.e. it IS its own pipeline per borge/workstreams.borg pipeline-model).")

(define todo/state/next
  (lambda
    (cur)
    (outline/todo-next-in todo/state/list (as-str cur))))

(define todo/state/next-for-item
  (lambda
    (cur item-id)
    (todo/state/todo-next-in
      (todo/state/columns-for-item item-id)
      (as-str cur)))
  "Next state after cur within item-id's OWN effective pipeline (own override >
     list columns > global default). Wraps around: last column -> first of that
     same set, so a card on its own pipeline never leaves it.")

(define todo/state/next-for-list
  (lambda
    (cur list-id)
    (todo/state/todo-next-in
      (todo/state/columns-for-list list-id)
      (as-str cur)))
  "Next state after cur within list-id's own state set (custom columns when\n     present, else the global default). Wraps around: last column -> first of\n     that same set.")

(define todo/state/open?
  (lambda (s) (not (todo/state/closed? s))))

(define todo/state/options
  (lambda
    (selected)
    (list/foldl
      (lambda
        (acc st)
        (let
          ((lab (if (string-eq st "") "(none)" st)))
          (string-append
            acc
            (html/option st lab (string-eq st (as-str selected))))))
      ""
      todo/state/list)))

(define todo/state/options-all
  (lambda
    (selected)
    (todo/state/options-from
      selected
      (append todo/state/list (list "ARCHIVED")))))

(define todo/state/options-for-item
  (lambda
    (selected item-id)
    (todo/state/options-from
      selected
      (append
        (todo/state/columns-for-item item-id)
        (list "ARCHIVED"))))
  "HTML options over item-id's OWN effective pipeline PLUS ARCHIVED, with
     selected marked — the card-detail <select>. Own `pipeline` override >
     list columns > global default, so a card on its own pipeline offers only
     its own states (plus ARCHIVED).")

(define todo/state/options-for-list
  (lambda
    (selected list-id)
    (todo/state/options-from
      selected
      (append
        (todo/state/columns-for-list list-id)
        (list "ARCHIVED"))))
  "HTML options over list-id's own state set PLUS ARCHIVED, with selected\n     marked — the detail <select> for an item in that list. Uses the list's\n     custom columns when present, else the canonical options-all state set.")

(define todo/state/options-from
  (lambda
    (selected states)
    (list/foldl
      (lambda
        (acc st)
        (let
          ((lab (if (string-eq st "") "(none)" st)))
          (string-append
            acc
            (html/option st lab (string-eq st (as-str selected))))))
      ""
      states)))

(define todo/state/pipeline-for-item
  (lambda
    (item-id)
    (car (todo/state/item-pipeline (as-str item-id))))
  "Which branch supplied item-id's effective pipeline: 'own | 'list | 'default
     (resolution order a/b/c of borge/workstreams.borg pipeline-model).")

(define todo/state/todo-next-in
  (lambda
    (xs cur)
    (if
      (null? xs)
      "TODO"
      (todo/state/todo-next-in-at xs cur (as-str (car xs)))))
  "State following cur within the state list xs; xs's own head on wrap (last\n     -> first) and on a not-found cur, so the cycle never leaves the list's\n     columns. (A per-set wrap, distinct from outline/todo-next-in which\n     hardcodes the global default state list's head.)")

(define todo/state/todo-next-in-at
  (lambda
    (xs cur head)
    (if
      (null? xs)
      head
      (if
        (string-eq (as-str (car xs)) cur)
        (if (null? (cdr xs)) head (as-str (car (cdr xs))))
        (todo/state/todo-next-in-at (cdr xs) cur head))))
  "Accumulating: find cur in xs, return the next; wrap to head at the end or\n     on absent.")


  ;; alias of todo/state/list; deleted in T4 (lib-hygiene).



(define todo/tree/create
  (lambda
    (t fields)
    (let
      ((r (tree/upsert (node-path t "new") "datum" t fields)))
      (if
        (todo/tree/path-ok? r)
        (todo/tree/redash (todo/tree/tail r))
        r))))

(define todo/tree/del
  (lambda
    (t id)
    (seq (tree/delete (node-path t (as-str id))) id)))

(define todo/tree/get
  (lambda
    (t id)
    (if
      (string-eq (as-str id) "")
      (list)
      (let
        ((n (tree/get (node-path t (as-str id)))))
        (if (null? n) (list) (dict-get n "body"))))))

(define todo/tree/insert-by-created
  (lambda
    (nj sorted)
    (if
      (null? sorted)
      (list nj)
      (if
        (string<?
          (as-str (dict-get nj "created_at"))
          (as-str (dict-get (car sorted) "created_at")))
        (cons nj sorted)
        (cons
          (car sorted)
          (todo/tree/insert-by-created nj (cdr sorted)))))))

(define todo/tree/node->pair
  (lambda
    (nj)
    (cons
      (todo/tree/redash (todo/tree/tail (dict-get nj "path")))
      (dict-get nj "body"))))

(define todo/tree/path-ok?
  (lambda
    (r)
    (if
      (null? r)
      #f
      (string-eq (substring (as-str r) 0 5) "data."))))

(define todo/tree/prefix
  (lambda
    (t)
    (let
      ((p (node-path t "")))
      (substring p 0 (- (string-length p) 1)))))

(define todo/tree/shared-prefix
  (lambda () "data.s")
  "The shared data-space subtree root (all spaces). A PURE constant, not
     node-path-derived: resolving the caller's user label costs a DB round
     trip per call (and lazy-env sessions can't afford it), while the
     data.s root is identity-independent. Shared REGISTER rows live under
     data.s.board.*; a garden Inbox never matches this prefix.")

(define todo/tree/redash
  (lambda
    (h)
    (string-append
      (substring h 0 8)
      "-"
      (substring h 8 4)
      "-"
      (substring h 12 4)
      "-"
      (substring h 16 4)
      "-"
      (substring h 20 12))))

(define todo/tree/rows
  (lambda
    (t n)
    (list/map
      todo/tree/node->pair
      (tree/list (todo/tree/prefix t) 1 n))))

(define todo/tree/save
  (lambda
    (t id fields)
    (seq
      (tree/upsert (node-path t (as-str id)) "datum" t fields)
      id)))

(define todo/tree/set-field
  (lambda
    (t id f v)
    (let
      ((cur (todo/tree/get t id)))
      (if
        (null? cur)
        (list)
        (todo/tree/save
          t
          id
          (dict-set* cur (list f (as-str v))))))))

(define todo/tree/sort-by-created
  (lambda
    (nodes)
    (list/foldl
      (lambda (acc n) (todo/tree/insert-by-created n acc))
      (list)
      nodes)))

(define todo/tree/tail
  (lambda (p) (substring p (- (string-length p) 32) 32)))

(define todo/ui/add-form
  (lambda
    (view list-id)
    (if
      (string-eq view "from-notes")
      (html/p*
        (html/attr "class" "muted")
        "Mark nodes TODO in notes — they appear here. Toggle writes back.")
      (if
        (string-eq view "archived")
        (html/p*
          (html/attr "class" "muted")
          "Archived tasks. Unarchive from detail (set state back), or delete.")
        (html/form
          "POST"
          "/todos"
          (html/attrs
            (list
              (html/attr "class" "card todo-add")
              (html/attr "hx-post" "/todos")
              (html/attr "hx-target" "#todo-main")
              (html/attr "hx-swap" "innerHTML")
              (html/attr
                "hx-on::after-request"
                "if(event.detail.successful){this.reset()}")))
          (html/join
            (list
              (html/hidden "action" "add")
              (html/hidden "view" view)
              (html/hidden "list_id" list-id)
              (html/label
                (html/attrs
                  (list
                    (html/attr "class" "todo-add-label")
                    (html/attr "for" "title")))
                "New item")
              (html/div
                (html/attr "class" "todo-add-row")
                (html/join
                  (list
                    (html/text-input
                      "title"
                      (html/attrs
                        (list
                          (html/attr "id" "title")
                          (html/attr "required" "required")
                          (html/attr "placeholder" "buy milk")
                          (html/attr "autocomplete" "off"))))
                    (html/button (html/attr "type" "submit") "Add")))))))))))

(define todo/ui/detail (lambda
  (id view)
  (let
    ((fields (todo/item/get id)))
    (if
      (null? fields)
      (html/div
        (html/attr "class" "card")
        (html/join
          (list
            (html/p* (html/attr "class" "err") "Item not found.")
            (html/a (string-append "/todos?view=" view) "← back"))))
      (let
        ((title (todo/item/title fields)))
        (let
          ((notes (todo/item/notes fields)))
          (let
            ((due (todo/item/due fields)))
            (let
              ((state (todo/item/state fields)))
              (let
                ((acceptance (todo/item/acceptance fields))
                  (branch (todo/item/branch fields))
                  (branch-kind (todo/item/branch-kind fields)))
                (let
                  ((lid (todo/item/list-id fields)))
                  (html/div
                    (html/attr "class" "card todo-detail")
                    (html/join
                      (list
                        (html/p*
                          (html/attr "class" "muted")
                          (html/a
                            (string-append "/todos?view=" view)
                            "← back to list"))
                        (html/h2 (html/text title))
                        (html/form
                          "POST"
                          "/todos"
                          (html/attrs
                            (list (html/attr "class" "todo-detail-form")))
                          (html/join
                            (list
                              (html/hidden "action" "save")
                              (html/hidden "id" id)
                              (html/hidden "view" view)
                              (html/label (html/attr "for" "title") "Title")
                              (html/text-input
                                "title"
                                (html/attrs
                                  (list
                                    (html/attr "id" "title")
                                    (html/attr "value" title)
                                    (html/attr "required" "required"))))
                              (html/label (html/attr "for" "state") "State")
                              (html/select
                                "state"
                                (html/attr "id" "state")
                                (todo/state/options-for-item state id))
                              (html/label (html/attr "for" "assignee") "Assignee")
                              (html/select
                                "assignee"
                                (html/attr "id" "assignee")
                                (todo/ui/assignee-options (todo/item/assignee fields)))
                              (html/label (html/attr "for" "due") "Due (YYYY-MM-DD)")
                              (html/text-input
                                "due"
                                (html/attrs
                                  (list
                                    (html/attr "id" "due")
                                    (html/attr "value" due)
                                    (html/attr "placeholder" "2026-07-15"))))
                              (html/label (html/attr "for" "list_id") "List")
                              (html/select
                                "list_id"
                                (html/attr "id" "list_id")
                                (todo/list/options lid))
                              (html/label (html/attr "for" "notes") "Notes")
                              (html/textarea
                                "notes"
                                (html/attrs
                                  (list
                                    (html/attr "id" "notes")
                                    (html/attr "rows" "8")
                                    (html/attr "class" "todo-notes")))
                                notes)
                              (html/p*
                                (html/attr "class" "muted")
                                (html/join
                                  (list
                                    (html/strong "Branch: ")
                                    (html/code branch)
                                    (if
                                      (string-eq branch "")
                                      ()
                                      (html/text
                                        (string-append
                                          "  ("
                                          (if
                                            (string-eq branch-kind "")
                                            "domain unset"
                                            branch-kind)
                                          ")"))))))
                              (if
                                (string-eq acceptance "")
                                ()
                                (html/div
                                  (html/attr "class" "todo-acceptance")
                                  (html/join
                                    (list
                                      (html/p* (html/attr "class" "muted") "Acceptance")
                                      (html/pre (html/text acceptance))))))
                              (html/div
                                ""
                                (html/join
                                  (list
                                    (html/button (html/attr "type" "submit") "Save")
                                    " "
                                    (html/a (string-append "/todos?view=" view) "Cancel")))))))
                        (todo/ui/feed-html id view))))))))))))))

(define todo/ui/feed-html
  (lambda
    (id view)
    "The card-detail v2 activity section: an h2 + <ul> of the interleaved
     comment/event feed, then a comment composer. Plain POST action=comment
     to /todos (same shape as the Save form) — todo/page posts the comment
     then redirects back to the current view."
    (let
      ((entries (todo/feed id)))
      (let
        ((items
          (if
            (null? entries)
            (html/p* (html/attr "class" "muted") "No activity yet.")
            (html/ul
              (html/attr "class" "todo-feed")
              (html/join
                (list/map
                  (lambda (e) (todo/ui/feed-item-html e))
                  entries))))))
        (html/div
          (html/attr "class" "todo-feed-section")
          (html/join
            (list
              (html/h2 (html/text "Activity"))
              items
              (html/form
                "POST"
                "/todos"
                (html/attrs
                  (list
                    (html/attr "class" "todo-comment-form")))
                (html/join
                  (list
                    (html/hidden "action" "comment")
                    (html/hidden "id" id)
                    (html/hidden "view" view)
                    (html/label (html/attr "for" "comment") "Comment")
                    (html/textarea
                      "comment"
                      (html/attrs
                        (list
                          (html/attr "id" "comment")
                          (html/attr "rows" "3")
                          (html/attr "class" "todo-comment-input")))
                      "")
                    (html/div
                      ""
                      (html/button (html/attr "type" "submit") "Comment"))))))))))))

;; ------------------------------------------------------------------
;; card detail v2: interleaved activity feed + comment composer.
;; borge/workstreams.borg card-comments / card-detail-v2. todo/feed gives
;; chronological entries; these renderers turn them into <li>s and the
;; composer POSTs action=comment back to the SAME /todos detail page
;; (no route fork - the spec's card-detail-v2 shape). Author + timestamp
;; ride the row metadata (todo/datum/feed-entry).
;; ------------------------------------------------------------------

(define todo/ui/feed-item-html
  (lambda
    (e)
    "One feed <li> from a feed entry dict (JSON): events render as a
     state-arrow line, comments as their body text. Oldest-first order
     comes from todo/feed."
    (let
      ((kind (as-str (dict-get e "kind")))
        (author (as-str (dict-get e "author")))
        (ts (as-str (dict-get e "ts"))))
      (let
        ((stamp
          (html/span
            (html/attr "class" "muted todo-feed-stamp")
            (html/text (string-append author " @ " ts)))))
        (if
          (string-eq kind "event")
          (html/li
            (html/attr "class" "todo-feed-item todo-feed-event")
            (html/join
              (list
                stamp
                (html/span
                  (html/attr "class" "todo-feed-arrow")
                  (html/text
                    (string-append
                      (as-str (dict-get e "from_state"))
                      " -> "
                      (as-str (dict-get e "to_state"))))))))
          (html/li
            (html/attr "class" "todo-feed-item todo-feed-comment")
            (html/join
              (list
                stamp
                (html/p (html/text (as-str (dict-get e "text"))))))))))))

(define todo/ui/handle-post (lambda
  ()
  (let
    ((action (as-str (request-param "action"))))
    (cond
      ((string-eq action "cycle")
        (todo/item/cycle (request-param "id")))
      ((string-eq action "delete")
        (todo/item/delete (request-param "id")))
      ((string-eq action "archive")
        (todo/item/archive (request-param "id")))
      ((string-eq action "add")
        (todo/item/add
          (request-param "list_id")
          (request-param "title")))
      ((string-eq action "create-list")
        (todo/list/create (request-param "title")))
      ((string-eq action "delete-list")
        (todo/list/delete (request-param "list_id")))
      ((string-eq action "cycle-note")
        (notes/outline/cycle-todo
          (request-param "slug")
          (request-param "node-id")))
      ((string-eq action "save")
        (seq
          (todo/item/save
            (request-param "id")
            (request-param "title")
            (request-param "notes")
            (request-param "due")
            (request-param "state")
            (request-param "list_id"))
          (let
            ((a (request-param "assignee")))
            (todo/item/assign
              (request-param "id")
              (if (null? a) "" (as-str a))))))
      ((string-eq action "comment")
        (let
          ((c (request-param "comment")))
          (if
            (or (null? c) (string-eq (as-str c) ""))
            ()
            (todo/comment/add (request-param "id") (as-str c)))))
      (else
        (todo/item/add
          (todo/list/ensure-inbox)
          (request-param "title")))))))
(define todo/ui/assignee-option (lambda (u sel) (html/option u u sel)))
(define todo/ui/assignee-options (lambda
  (cur)
  (html/join
    (cons
      (html/option
        ""
        "(unassigned)"
        (string-eq (as-str cur) ""))
      (list/map
        (lambda
          (u)
          (todo/ui/assignee-option u (string-eq u (as-str cur))))
        (list-usernames))))))


;; ---- todo-shared-spaces: T3/T5/T7 chain (note-todos, cross-identity mirror) ----
(define todo/state/closed?
  (lambda
    (s)
    (let
      ((t (as-str s)))
      (or
        (string-eq t "")
        (string-eq t "DONE")
        (string-eq t "PARKED")
        (string-eq t "CANCELLED")
        (string-eq t "ARCHIVED")))))
(define todo/item/origin-kind
  (lambda
    (fields)
    (let
      ((v (dict-get fields "origin_kind")))
      (if (null? v) "" (as-str v))))
  "The card's origin kind: empty for a hand-made card, `note` when
     promoted from an outline node. Tolerates a pre-T3 card with no
     field, so readers never see null. borge/workstreams.borg note-todos.")
(define todo/node/parse-ref
  (lambda
    (ref)
    (let
      ((parts (string-split (as-str ref) "#")))
      (if
        (or
          (null? parts)
          (string-eq (as-str (car parts)) ""))
        (cons "" "")
        (let
          ((slug (as-str (car parts)))
            (rest (cdr parts)))
          (if
            (null? rest)
            (cons slug "")
            (cons slug (as-str (car rest))))))))
  "Split a card origin_ref \"<slug>#<nid>\" into (slug . nid); (\"\" . \"\")
     for an empty/malformed slug. A bare \"slug\" (no #) also parses, with an
     empty nid, so a note-LEVEL provenance ref round-trips instead of being
     silently dropped. The reconciliation-key parser.")
(define todo/item/origin-ref
  (lambda
    (fields)
    (let
      ((v (dict-get fields "origin_ref")))
      (if (null? v) "" (as-str v))))
  "The reconciliation key linking a promoted card to its origin node,
     `<slug>#<nid>` (empty for a hand-made card). borge/workstreams.borg
     note-todos clause 2.")
(define todo/node/note-path
  (lambda
    (slug)
    (let
      ((row (notes/doc/row (as-str slug))))
      (if (null? row) "" (notes/doc/row-path row))))
  "The tree path of the note with slug (empty when the note is absent).
     Resolves through the folder ACL note, never a hardcoded data.note path.")
(define todo/node/set-todo
  (lambda
    (slug nid todo)
    (let
      ((p (todo/node/note-path slug)))
      (if
        (string-eq p "")
        #f
        (notes/node/set-todo p (as-str nid) (as-str todo)))))
  "Write the `todo` field of ONE heading node (slug nid). One read + one
     write, never a whole-note rewrite. Returns #t when the node exists.")
(define todo/node/note-visible?
  (lambda
    (slug)
    (not
      (string-eq
        (todo/node/note-path slug)
        "")))
  "True when the CALLER can resolve the note slug through the garden-scoped
     notes read (notes/tree/find-slug -> tree/find-by, prefix = caller's
     garden). Visibility, not existence: another identity's private note is
     invisible here (note-todos clause 5). The distinction that lets
     todo/node/mirror tell 'origin gone' from 'origin not mine'.")
(define todo/node/mirror-notice
  (lambda
    (card-id ref state)
    (let*
      ((ref-pair (todo/node/parse-ref ref))
        (reason
          (if (todo/node/note-visible? (car ref-pair))
            "node keyword is gone; only the card moved for"
            "is not visible to"))
        (note
          (string-append
            "origin note ("
            (as-str ref)
            ") "
            reason
            " "
            (whoami)
            " — keyword not mirrored to "
            (as-str state)))
        (seen
          (list/filter
            (lambda
              (n)
              (and
                (string-eq (as-str (dict-get n "author")) (as-str (whoami)))
                (let
                  ((b (dict-get (dict-get n "body") "body")))
                  (and
                    (not (null? b))
                    (string-contains? (as-str b) "origin note (")))))
            (todo/comment/list (as-str card-id)))))
      (if
        (null? seen)
        (todo/comment/add (as-str card-id) note)
        (as-str
          (dict-get
            (dict-get (car seen) "body")
            "body")))))
  "Record ONE attributed comment on the card when its origin note cannot be
     mirrored (note-todos clause 5; T7 option B). IDEMPOTENT per (card,
     author): once this caller has recorded a notice, a later move does NOT
     append again — the first notice already carries the exact state that
     failed to mirror, and the card's own todo-event stream is the record of
     every later transition. Returns the existing notice instead. Never
     edits a row another author wrote. Callers get a value on every move so
     the divergence can never masquerade as a silent no-op.")
(define todo/node/mirror
  (lambda
    (card-id state)
    (let
      ((fields (todo/item/get (as-str card-id))))
      (if
        (null? fields)
        ""
        (if
          (string-eq (todo/item/origin-kind fields) "note")
          (let
            ((ref (todo/node/parse-ref (todo/item/origin-ref fields))))
            (if
              (string-eq (car ref) "")
              ""
              (let
                ((ok (todo/node/set-todo (car ref) (cdr ref) state)))
                (if
                  ok
                  #t
                  (todo/node/mirror-notice
                    (as-str card-id)
                    (todo/item/origin-ref fields)
                    state)))))
          ""))))
  "Mirror a card's new state onto its origin note node, when it has one.
     The write-back half of note-todos clause 3: a card transition updates
     the linked node in the SAME transaction (todo/item/set-state and
     todo/item/save call this beside their tree write). Returns #t when the
     node was written, the notice comment id when the origin note is not
     visible to this caller (T7 option B — the divergence is VISIBLE, never
     silent), and '' for hand-made cards or a malformed ref.")
(define todo/node/find-card
  (lambda
    (ref)
    (if
      (string-eq (as-str ref) "")
      ()
      (car
        (list/filter
          (lambda
            (row)
            (string-eq (todo/item/origin-ref (cdr row)) (as-str ref)))
          (todo/item/all)))))
  "The card whose origin_ref equals ref, or () when unpromoted. The
     idempotency key for promotion and the node->card update path.")
(define todo/node/get
  (lambda
    (slug nid)
    (let
      ((p (todo/node/note-path slug)))
      (if
        (string-eq p "")
        ()
        (notes/node/get p (as-str nid)))))
  "Read one heading node's body dict from (slug nid), or () when the note
     path or node is absent.")
(define todo/node/list-has-state?
  (lambda
    (list-id state)
    (if
      (null? (list/filter
               (lambda (c) (string-eq (as-str c) (as-str state)))
               (todo/state/columns-for-list (as-str list-id))))
      #f
      #t))
  "True when `state` is one of list-id's effective pipeline columns.")
(define todo/node/seed-state
  (lambda
    (keyword list-id)
    (let
      ((k (as-str keyword)))
      (if
        (string-eq k "")
        "TODO"
        (if
          (todo/node/list-has-state? list-id k)
          k
          "SPEC"))))
  "Map a node's outline keyword onto the card pipeline. The outline set
     is 3-state (\"\" TODO DONE); the card machine is the engineering
     taxonomy. A keyword the card's pipeline knows passes through (TODO
     and DONE included); an unknown keyword maps to SPEC — marked but not
     yet specified, an honest advance past intake, never a dropped state.")
(define todo/promote-node
  (lambda
    (slug nid list-id)
    (let
      ((ref (string-append (as-str slug) "#" (as-str nid))))
      (let
        ((existing (todo/node/find-card ref)))
        (if
          (null? existing)
          (let
            ((node (todo/node/get slug nid)))
            (if
              (null? node)
              ""
              (let*
                ((title (as-str (dict-get node "title")))
                  (body (as-str (dict-get node "body")))
                  (kw (as-str (dict-get node "todo")))
                  (lid
                    (if
                      (string-eq (as-str list-id) "")
                      (todo/resolve-board)
                      (as-str list-id)))
                  (cid (todo/item/add lid title))
                  (fields (todo/item/get cid)))
                (seq
                  (if
                    (null? fields)
                    ""
                    (todo/tree/save
                      "todo-item"
                      cid
                      (dict-set*
                        fields
                        (list
                          "notes"
                          body
                          "state"
                          (todo/node/seed-state kw lid)
                          "origin_kind"
                          "note"
                          "origin_ref"
                          ref))))
                  (as-str cid)))))
          (as-str (car existing))))))
  "Promote a note heading node into ONE shared card and return its id.
     origin_ref = \"<slug>#<nid>\" is the reconciliation key: re-promoting
     returns the existing card (idempotent, never a duplicate). The card
     lands in the `todo` space through todo/item/add, so a SECOND identity
     can see and move it; the note itself stays in the owner's garden.
     list-id is optional (empty = the shared Board). Empty string means the
     node or note is missing. borge/workstreams.borg note-todos clause 2.")
(define todo/from-notes-items
  (lambda
    ()
    (list/filter
      (lambda
        (row)
        (string-eq (todo/item/origin-kind (cdr row)) "note"))
      (todo/item/all)))
  "Promoted cards (origin_kind = note) — the board's 'from notes' filter,
     a filter over CARDS, never a second scrape of the note tree.")
(define todo/node/promoted-refs
  (lambda
    ()
    (list/filter
      (lambda (r) (not (string-eq r "")))
      (list/map
        (lambda (row) (todo/item/origin-ref (cdr row)))
        (todo/item/all))))
  "Every non-empty origin_ref on the board, read ONCE (one card scan) so
     the unpromoted filter never does a DB read per note node.")
;; MARKED-HEADING READ PATH (2026-10-18 budget fix). OBITUARY (what/why):
;; todo/node/unpromoted used to fold notes/outline/all-todos, which decodes
;; EVERY note's full outline (tree/note-index-full: 3385 notes / 21981
;; heading nodes on prod) to find the handful of marked headings. That
;; single call exceeds the 400k-step route budget on its own, so
;; /todos?view=everything and ?view=from-notes returned
;; "budget exceeded" (500) while ?view=open/archived stayed up — the
;; note-todos bridge (commit 1adea6e) wired the scrape into
;; todo/item/html and blew the page. The replacement inverts the read:
;; MARKED HEADINGS FIRST via tree/find-by on body.todo (one indexed read
;; per keyword; 106 hits for ~525 steps), then resolve each hit's note
;; slug from the LIGHT note index (no outlines) using the subtree path
;; (a heading path is <note-path>.h.<nid>[, .h.<nid>...], so the note
;; path is the segment before the FIRST .h.). Same (slug nid title
;; keyword) rows in the same shape, ~3x under budget. The full-outline
;; fold stays DELETED here, not kept as a fallback — it can never fit a
;; request (borge/notes.outline-read-path budget-scale).
(define todo/node/marked-keywords
  (list " " "TODO" "DONE" "ARCHIVED" "CANCELLED")
  "Every todo keyword a note heading can carry. Leading-space used to
     match the live data's single-space placeholder keyword.")
(define todo/node/marked-rows
  (lambda
    ()
    (list/foldl
      (lambda (acc kw)
        (append acc (tree/find-by (quote heading) "todo" kw 20000)))
      (list)
      todo/node/marked-keywords))
  "Marked note headings as node JSON, one tree/find-by body-field read
     per keyword. tree/find-by is garden-scoped and viewer-gated by the
     prim, exactly like the full-index path it replaces.")
(define todo/node/slug-map
  (lambda
    (marked)
    (list/foldl
      (lambda (acc nj)
        (let*
          ((p (as-str (dict-get nj "path")))
           (notep (car (string-split p ".h."))))
          (if
            (dict-get acc notep)
            acc
            (let
              ((r (tree/get notep)))
              (if
                (null? r)
                acc
                (let
                  ((slug (dict-get (dict-get r "body") "slug")))
                  (if
                    (null? slug)
                    acc
                    (dict-set acc notep (as-str slug)))))))))
      "{}"
      marked))
  "path -> slug for the notes that OWN a marked heading, resolved from the
     MARKED ROWS themselves. DISTINCT-PATH LAZY (2026-10-18): the first
     version folded the whole light note index (tree/note-index, 3385 rows
     on prod) just to look up ~10 paths — 105k steps of pure binding for a
     10-entry join. Resolving one tree/get per DISTINCT marked note is
     ~4.5k steps total and scales with MARKED notes, not with garden size.
     Only headings of notes that actually carry a mark are ever read.")
(define todo/node/row-of
  (lambda
    (smap nj)
    (let*
      ((p (as-str (dict-get nj "path")))
       (notep (car (string-split p ".h.")))
       (slug (dict-get smap notep))
       (b (dict-get nj "body")))
      (list (if (null? slug) "" (as-str slug))
            (as-str (dict-get b "id"))
            (as-str (dict-get b "title"))
            (as-str (dict-get b "todo")))))
  "One marked heading node -> (slug nid title keyword). A heading path is
     <note-path>.h.<nid>[.h.<nid>...]; splitting on the FIRST .h. yields
     the owning note path, which is the slug-map key.")
(define todo/node/unpromoted
  (lambda
    ()
    (let*
      ((promoted
          (list/filter
            (lambda (r) (not (string-eq r "")))
            (list/map
              (lambda (row) (todo/item/origin-ref (cdr row)))
              (todo/item/all))))
        (marked (todo/node/marked-rows))
        (smap (todo/node/slug-map marked)))
      (list/filter
        (lambda
          (t)
          (let
          ((ref
              (string-append (as-str (car t)) "#" (as-str (car (cdr t))))))
            (null? (list/filter
                     (lambda (r) (string-eq r ref))
                     promoted))))
        (list/map (lambda (nj) (todo/node/row-of smap nj)) marked))))
  "Outline todo rows (slug nid title keyword) with NO card on the board —
     the honest 'notes (unpromoted)' section of /todos (note-todos clause
     4). The board renders only promoted cards; this is the only place a
     raw node still shows, and each row offers promotion.")
(define todo/node/back-link
  (lambda
    (fields)
    (let
      ((ref (todo/item/origin-ref fields)))
      (if
        (string-eq ref "")
        ""
        (let
          ((parts (todo/node/parse-ref ref)))
          (html/a
            (string-append
              "/notes?slug="
              (car parts)
              "&node-id="
              (cdr parts))
            (html/span
              (html/attr "class" "muted todo-src")
              (html/text (string-append "from " ref))))))))
  "Back-link from a promoted card to its origin note node, or \"\" for a
     hand-made card. note-todos clause 4.")
(define todo/node/promote-form
  (lambda
    (t)
    (let
      ((slug (as-str (car t)))
        (nid (as-str (car (cdr t))))
        (title (as-str (car (cdr (cdr t)))))
        (kw (as-str (car (cdr (cdr (cdr t)))))))
      (html/li
        (html/attr "class" "todo-item todo-from-note todo-unpromoted")
        (html/join
          (list
            (html/span
              (html/attr "class" (string-append "todo-box st-" kw))
              (if (string-eq kw "") "·" kw))
            (html/span
              (html/attr "class" "todo-title")
              (html/text title))
            (html/a
              (string-append "/notes?slug=" slug)
              (html/span
                (html/attr "class" "muted todo-src")
                (html/text slug)))
            (html/form
              "POST"
              "/todos"
              (html/attrs
                (list
                  (html/attr "class" "todo-promote")
                  (html/attr "hx-post" "/todos")
                  (html/attr "hx-target" "#todo-main")
                  (html/attr "hx-swap" "innerHTML")))
              (html/join
                (list
                  (html/hidden "action" "promote-node")
                  (html/hidden "slug" slug)
                  (html/hidden "node-id" nid)
                  (html/hidden "view" "from-notes")
                  (html/button
                    (html/attrs
                      (list
                        (html/attr "type" "submit")
                        (html/attr "class" "secondary")
                        (html/attr "title" "promote to a shared board card")))
                    "promote")))))))))
  "One unpromoted node row with a promote button (POST action=promote-node).")
(define todo/from-notes-html
  (lambda
    (view)
    (let
      ((cards (todo/from-notes-items))
        (unpromoted (todo/node/unpromoted)))
      (html/join
        (list
          (html/h2 "From notes")
          (if
            (null? cards)
            (html/p*
              (html/attr "class" "muted")
              "No promoted note todos yet — mark a heading in a note, then promote it.")
            (html/ul
              (html/attr "class" "card todo-list")
              (list/foldl
                (lambda
                  (acc row)
                  (string-append
                    acc
                    (todo/item/list-li row view)
                    (todo/node/back-link (cdr row))))
                ""
                (list/take (todo/item/active cards) todo/page/cap))))
          (html/h2 "Notes (unpromoted)")
          (if
            (null? unpromoted)
            (html/p*
              (html/attr "class" "muted")
              "Every marked note node is on the board.")
            (html/join
              (list
                (html/ul
                  (html/attr "class" "card todo-list")
                  (list/foldl
                    (lambda
                      (acc t)
                      (string-append acc (todo/node/promote-form t)))
                    ""
                    (list/take unpromoted todo/page/cap)))
                (todo/page/more-note (length unpromoted)))))))))
  "The 'from notes' view: promoted cards (shared, workable) plus the
     unpromoted nodes that are NOT on the board yet (note-todos clause 4).
     Replaces the scrape-cache notes block, which showed every node as if
     it were a board item. Takes the page's ONE card snapshot (items) so
     the view never re-scans the board.

     WINDOWED (2026-10-18 budget fix, part 2): rows are capped at
     todo/page/cap (40) and the remainder is reported as a COUNT via
     todo/page/more-note (the spec's own 'point, don't list' decision from
     the package-index budget fix). An uncapped render is ~2.5k steps per
     row: prod's 106 unpromoted rows cost 107k just to lay out, and the
     Lists section another 205k, which together blew the 400k request
     budget even after the scrape itself was made 3x cheaper. The cap
     keeps the honest total visible without paying per-row cost for rows
     nobody reads above the fold.")
(define todo/page/cap
  40
  "Rows rendered per section of /todos before the 'more' pointer takes
     over. Chosen from the measured per-row cost (~2.5k steps) against the
     400k budget: 40 rows/section leaves room for nav, the add form and
     the other sections (2026-10-18 page-latency budget fix).")
(define todo/page/more-note
  (lambda
    (total)
    (if
      (<= total todo/page/cap)
      ""
      (html/p*
        (html/attr "class" "muted")
        (string-append
          "…and "
          (as-str (- total todo/page/cap))
          " more. "
          "Marking is free and private; promote what you are working on."))))
  "Honest truncation pointer: how many rows the cap left unrendered.
     A capped list must SAY it is capped (never a silent partial view).")
(define todo/promote-handler
  (lambda
    ()
    (let
      ((list-id (request-param "list_id")))
        (todo/promote-node
          (request-param "slug")
          (request-param "node-id")
          (if (null? list-id) "" (as-str list-id)))))
  "The /todos POST action=promote-node handler: promote the node named by
     slug/node-id to a shared card (optional list_id). Returns the card id
     (existing card id when already promoted). borge/workstreams.borg
     note-todos clause 2.")

(define todo/ui/main
  (lambda
    (view)
    (let
      ((id-param (request-param "id"))
        (is-get (string-eq (request-method) "GET")))
      (if
        (if
          is-get
          (if
            (null? id-param)
            #f
            (not (string-eq (as-str id-param) "")))
          #f)
        (html/div
          (html/attr "id" "todo-main")
          (html/join
            (list
              (html/div
                (html/attr "class" "todo-layout")
                (html/join
                  (list
                    (html/div
                      (html/attr "class" "todo-side")
                      (todo/ui/nav view (todo/item/all)))
                    (html/div
                      (html/attr "class" "todo-body")
                      (todo/ui/detail (as-str id-param) view))))))))
        (let*
          ((list-id
              (if
                (string-eq view "everything")
                (todo/list/ensure-inbox)
                (if
                  (string-eq view "from-notes")
                  ""
                  (if (string-eq view "archived") "" view))))
            (body
              (cond
                ((string-eq view "everything")
                  (let
                    ((items (todo/item/all)))
                    (let
                      ((active (todo/item/active items)))
                      (html/join
                        (list
                          (html/h2 "Lists")
                          (html/ul
                            (html/attr "class" "card todo-list")
                            (list/foldl
                              (lambda
                                (acc row)
                                (string-append acc (todo/item/list-li row view)))
                              ""
                              (list/take active todo/page/cap)))
                          (todo/page/more-note (length active))
                          (todo/from-notes-html view))))))
                ((string-eq view "from-notes")
                  (todo/from-notes-html view))
                (#t (todo/item/html view)))))
          (html/div
            (html/attr "id" "todo-main")
            (html/join
              (list
                (html/div
                  (html/attr "class" "todo-layout")
                  (html/join
                    (list
                      (html/div
                        (html/attr "class" "todo-side")
                        (todo/ui/nav view))
                      (html/div
                        (html/attr "class" "todo-body")
                        (html/join
                          (list
                            (todo/ui/add-form view list-id)
                            body)))))))))))))
  "The /todos main region for a view. Reads the board ONCE (items) and
     threads that snapshot through the smart view. OBITUARY (2026-10-18
     budget fix): the 'everything' view used to be assembled inside
     todo/item/html, whose sections each re-read the board (todo/item/all)
     and the note scrape — three card scans and two scrapes per page, 500
     budget-exceeded on prod. The smart view is assembled HERE now, from
     ONE snapshot. todo/ui/nav keeps its 1-arg signature for cross-branch
     safety (see todo/item/html's arity note).")

(define todo/ui/nav
  (lambda
    (view)
    (let
      ((lists (todo/list/all))
        (items (todo/item/all))
        (inbox (todo/list/ensure-inbox)))
      (let
        ((arch-n
          (todo/item/count-all
            (list/filter
              (lambda
                (row)
                (todo/state/archived? (todo/item/state (cdr row))))
              items))))
        (html/ul
          (html/attr "class" "todo-nav")
          (html/join
            (list
              (todo/ui/nav-link view "everything" "Everything" "")
              (todo/ui/nav-link view "from-notes" "From notes" "")
              (todo/ui/nav-link
                view
                "archived"
                "Archived"
                (string-append "(" (as-str arch-n) ")"))
              (list/foldl
                (lambda
                  (acc row)
                  (string-append acc (todo/list/nav-li view row)))
                ""
                lists)
              (html/li
                (html/attr "class" "todo-nav-item")
                (html/form
                  "POST"
                  "/todos"
                  (html/attrs
                    (list
                      (html/attr "class" "todo-new-list")
                      (html/attr "hx-post" "/todos")
                      (html/attr "hx-target" "#todo-main")
                      (html/attr "hx-swap" "innerHTML")))
                  (html/join
                    (list
                      (html/hidden "action" "create-list")
                      (html/hidden "view" view)
                      (html/text-input
                        "title"
                        (html/attrs
                          (list
                            (html/attr "placeholder" "New list")
                            (html/attr "required" "required")
                            (html/attr "class" "todo-new-list-input"))))
                      (html/button (html/attr "type" "submit") "+ list")))))))))))
  "nav: ONE items fetch (archived count only); lists render without per-list ratios — dict-get JSON parses per item blew the route budget")

(define todo/ui/nav-link
  (lambda
    (view key label extra)
    (html/li
      (html/attr
        "class"
        (if
          (string-eq view key)
          "todo-nav-item is-active"
          "todo-nav-item"))
      (html/a
        (string-append "/todos?view=" key)
        (string-append
          label
          (if (string-eq extra "") "" (string-append " " extra)))))))

(define todo/ui/ratio
  (lambda
    (items)
    (let
      ((n (todo/item/count-all items))
        (open (todo/item/count-open items)))
      (string-append (as-str (- n open)) "/" (as-str n)))))

(define todo/ui/view
  (lambda
    ()
    (let
      ((v (request-param "view")))
      (if (null? v) "everything" (as-str v)))))

(define todo/view
  (lambda
    ()
    (let
      ((v (request-param "view")))
      (if (null? v) "everything" (as-str v)))))

(define todo/handle-post (lambda
  ()
  (let
    ((action (as-str (request-param "action"))))
    (if
      (string-eq action "comment")
      (let
        ((c (request-param "comment")))
        (if
          (or (null? c) (string-eq (as-str c) ""))
          ()
          (todo/comment/add (request-param "id") (as-str c))))
      (if
        (string-eq action "cycle")
        (todo/cycle (request-param "id"))
        (if
          (string-eq action "delete")
          (todo/delete (request-param "id"))
          (if
            (string-eq action "archive")
            (todo/archive (request-param "id"))
            (if
              (string-eq action "add")
              (todo/add
                (request-param "list_id")
                (request-param "title"))
              (if
                (string-eq action "create-list")
                (todo/create-list (request-param "title"))
                (if
                  (string-eq action "delete-list")
                  (todo/delete-list (request-param "list_id"))
                  (if
                    (string-eq action "cycle-note")
                    (notes/cycle-node-todo
                      (request-param "slug")
                      (request-param "node-id"))
                    (if
                      (string-eq action "save")
                      (todo/save
                        (request-param "id")
                        (request-param "title")
                        (request-param "notes")
                        (request-param "due")
                        (request-param "state")
                        (request-param "list_id"))
                      (todo/add (todo/ensure-inbox) (request-param "title"))))))))))))))
