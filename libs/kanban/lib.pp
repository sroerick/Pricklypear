

(define kanban/add-card
  (lambda (list-id title state)
    "Add a todo-item to the list with the given state (uses create, the
     id-generating convenience; the board re-reads)."
    (let ((lid (if (string-eq (as-str list-id) "") (todo/ensure-inbox) (as-str list-id))))
      (create (quote todo-item)
        (dict-set* "{}"
          (list
            "list_id" lid
            "title" (as-str title)
            "notes" ""
            "state" (as-str state)
            "due" ""
            "position" "0"
            "done" "false"
            "pipeline" ""
            "kind" ""))))))

(define kanban/add-card-form
  (lambda (list-id state)
    "Per-column form to add a card with a title."
    (html/form "POST" "/kanban"
      (html/attrs
        (list
          (html/attr "class" "kb-add-card")
          (html/attr "hx-post" "/kanban")
          (html/attr "hx-target" "#kb-board")
          (html/attr "hx-swap" "innerHTML")))
      (html/join
        (list
          (html/hidden "action" "add-card")
          (html/hidden "list" list-id)
          (html/hidden "state" state)
          (html/text-input "title"
            (html/attrs
              (list
                (html/attr "placeholder" "+ add card")
                (html/attr "class" "kb-add-input"))))
          (html/button (html/attr "type" "submit") "↵"))))))

;; ------------------------------------------------------------------
;; column mutation (add / remove / rename)
;; ------------------------------------------------------------------

(define kanban/add-column
  (lambda (list-id name)
    "Append a new column name to the list's columns. Returns upserted path."
    (let ((cols (kanban/columns-for-list list-id)))
      (kanban/columns-set list-id (append cols (list (as-str name)))))))

(define kanban/add-column-form
  (lambda (list-id)
    "Form to add a new column to the list."
    (html/form "POST" "/kanban"
      (html/attrs
        (list
          (html/attr "class" "kb-add-col")
          (html/attr "hx-post" "/kanban")
          (html/attr "hx-target" "#kb-board")
          (html/attr "hx-swap" "innerHTML")))
      (html/join
        (list
          (html/hidden "action" "add-column")
          (html/hidden "list" list-id)
          (html/text-input "name"
            (html/attrs
              (list (html/attr "placeholder" "+ column"))))
          (html/button (html/attr "type" "submit") "add"))))))

(define kanban/board
  (lambda (list-id)
    "Render the full board: list picker + columns row + other bucket + add-column.
       ONE (todo/items-for-list) scan feeds every column and the other bucket
       (was: one full todo-item rescan per column — card 8972cdfe)."
    (let ((cols (kanban/columns-for-list list-id)))
      (let ((all-cards (todo/items-for-list list-id)))
        (html/div (html/attr "id" "kb-board")
          (html/join
            (list
              (kanban/list-picker list-id)
              (html/div (html/attr "class" "kb-cols")
                (string-append
                  (kanban/columns-html-from list-id all-cards cols)
                  (kanban/add-column-form list-id)))
              (kanban/other-bucket-from list-id all-cards cols)
              (kanban/script))))))))

;; ------------------------------------------------------------------
;; HTML rendering
;; ------------------------------------------------------------------

(define kanban/card-html
  (lambda (list-id row columns)
    "Render one card as a draggable article with up/down/left/right buttons."
    (let ((id (as-str (car row))) (fields (cdr row)))
      (let ((title (as-str (dict-get fields "title")))
            (state (as-str (dict-get fields "state"))))
        (html/tag "article"
          (html/attrs
            (list
              (html/attr "class" "kb-card")
              (html/attr "draggable" "true")
              (html/attr "data-id" id)
              (html/attr "ondragstart" "kbDrag(event)")))
          (html/join
            (list
              (html/div (html/attr "class" "kb-card-title") (html/text title))
              (html/div (html/attr "class" "kb-card-meta")
                (html/text state))
              (html/div (html/attr "class" "kb-card-controls")
                (html/join
                  (list
                    (kanban/move-btn id list-id "left" "←")
                    (kanban/move-btn id list-id "up" "↑")
                    (kanban/move-btn id list-id "down" "↓")
                    (kanban/move-btn id list-id "right" "→")))))))))))

(define kanban/cards-html
  (lambda (list-id cards columns)
    "Concatenated card HTML for the card list."
    (if (null? cards)
      ""
      (string-append
        (kanban/card-html list-id (car cards) columns)
        (kanban/cards-html list-id (cdr cards) columns)))))

;; ------------------------------------------------------------------
;; card helpers (all tree actions)
;; ------------------------------------------------------------------

(define kanban/cards-in
  (lambda (list-id state)
    "Return (id . fields) pairs of items in list-id with the given state."
    (list/filter
      (lambda (row)
        (string-eq (as-str (dict-get (cdr row) "state")) state))
      (todo/items-for-list list-id))))

(define kanban/cards-in-from
  (lambda (all-cards state)
    "Cards in the given state, from a PRECOMPUTED (todo/items-for-list) scan
       — the single-pass variant of cards-in (which rescans per call)."
    (list/filter
      (lambda (row)
        (string-eq (as-str (dict-get (cdr row) "state")) state))
      all-cards)))

(define kanban/cascade-rename-state
  (lambda (list-id old-name new-name)
    "tree/upsert every card in list-id whose state is old-name, setting new-name."
    (let ((matches (list/filter
                     (lambda (row)
                       (string-eq (as-str (dict-get (cdr row) "state")) old-name))
                     (todo/items-for-list list-id))))
      (kanban/rename-state-at matches new-name))))

(define kanban/col-remove-btn
  (lambda (list-id state)
    "Confirming button to remove a column from the list."
    (html/form "POST" "/kanban"
      (html/attrs
        (list
          (html/attr "class" "kb-col-del")
          (html/attr "hx-post" "/kanban")
          (html/attr "hx-target" "#kb-board")
          (html/attr "hx-swap" "innerHTML")
          (html/attr "hx-confirm" "Remove this column? Cards stay in the other bucket.")))
      (html/join
        (list
          (html/hidden "action" "remove-column")
          (html/hidden "list" list-id)
          (html/hidden "name" state)
          (html/button (html/attr "type" "submit" ) "×"))))))

(define kanban/column-html
  (lambda (list-id state columns)
    "Render one column as a form (drop target) with its cards. ONE card scan
       per board render (was: a full todo-item rescan per column — card
       8972cdfe). Cards that belong in the 'other' bucket (kanban/stray? —
       state not a column) are excluded here so a stray card never renders in
       a board column; the other bucket shows it (other-bucket uses the same
       stray? predicate — nothing is hidden or duplicated)."
    (kanban/column-html-from list-id (todo/items-for-list list-id) state columns)))

(define kanban/column-html-from
  (lambda (list-id all-cards state columns)
    "One column rendered from a PRECOMPUTED (todo/items-for-list) scan: form
       (drop target) with its cards; stray? cards are excluded here and shown
       in the other bucket."
    (let ((cards (list/filter
                   (lambda (row) (not (kanban/stray? columns row)))
                   (kanban/cards-in-from all-cards state))))
      (let ((count (as-str (length cards))))
        (html/form "POST" "/kanban"
          (html/attrs
            (list
              (html/attr "class" "kb-col")
              (html/attr "data-state" state)
              (html/attr "hx-post" "/kanban")
              (html/attr "hx-target" "#kb-board")
              (html/attr "hx-swap" "innerHTML")
              (html/attr "ondragover" "event.preventDefault()")
              (html/attr "ondrop" "kbDrop(event)")))
          (html/join
            (list
              (html/hidden "action" "move")
              (html/hidden "id" "")
              (html/hidden "state" state)
              (html/hidden "index" "")
              (html/hidden "list" list-id)
              (html/div (html/attr "class" "kb-col-header")
                (html/join
                  (list
                    (html/text
                      (if (string-eq state "") "(none)" state))
                    (html/span (html/attr "class" "kb-count")
                      (html/text count))
                    (kanban/col-remove-btn list-id state))))
              (html/div (html/attr "class" "kb-cards")
                (kanban/cards-html list-id cards columns))
              (kanban/add-card-form list-id state))))))));; package kanban -- a board view over todos. One todo-list = one board;
;; the list's `columns` field (a JSON array of state-name strings, read via
;; json-parse) overrides todo/state/list when present. Cards are todo-items;
;; every mutation is a TREE ACTION over the card's datum node
;; (one-tree.tree-datum-prims): tree/upsert at the card's node-path, not a
;; create/update call. Reorder/rename wrap multiple tree/upserts in
;; with-transaction. Sync: scripts/pp_sync.py pull|push

;; ------------------------------------------------------------------
;; columns: per-list state set, or the default
;; ------------------------------------------------------------------

(define kanban/columns-default
  (lambda ()
    "The fallback column set when a list has no `columns` field."
    todo/state/list))

(define kanban/columns-equal?
  (lambda (a b)
    "Element-wise equality of two column-state lists (order + names)."
    (if (null? a)
      (null? b)
      (if (null? b)
        #f
        (if (string-eq (as-str (car a)) (as-str (car b)))
          (kanban/columns-equal? (cdr a) (cdr b))
          #f)))))

(define kanban/columns-for-list
  (lambda (list-id)
    "Return the list's columns as a Lisp list of state strings.
     Falls back to todo/state/list when the list has no columns field
     or the field is empty. Single source of truth: todo/state/columns-for-list."
    (todo/state/columns-for-list list-id)))

(define kanban/columns-html
  (lambda (list-id cols)
    "Concatenated column HTML for the column list. ONE card scan feeds all
       columns (was: one todo-item rescan per column — card 8972cdfe)."
    (kanban/columns-html-from list-id (todo/items-for-list list-id) cols)))

(define kanban/columns-html-from
  (lambda (list-id all-cards cols)
    "Concatenated column HTML from a PRECOMPUTED (todo/items-for-list) scan."
    (kanban/columns-html-at-from list-id all-cards cols cols)))

(define kanban/columns-html-at
  (lambda (list-id cols full)
    "Internal: render cols with the FULL board column set `full` passed down
       (see kanban/columns-html-at-from); rescans cards per call."
    (kanban/columns-html-at-from list-id (todo/items-for-list list-id) cols full)))

(define kanban/columns-html-at-from
  (lambda (list-id all-cards cols full)
    "Internal: render cols from a PRECOMPUTED card scan, passing the FULL
       board column set `full` down to every kanban/column-html-from so its
       kanban/stray? test compares each card against the whole board — NOT
       the shrinking cdr-suffix that the recursion otherwise hands a later
       column (a card whose state matches the full board must stay in its
       column, never be bucketed)."
    (if (null? cols)
      ""
      (string-append
        (kanban/column-html-from list-id all-cards (as-str (car cols)) full)
        (kanban/columns-html-at-from list-id all-cards (cdr cols) full)))))

(define kanban/columns-set
  (lambda (list-id cols)
    "tree/upsert the LIST datum with a new columns array (cols is a Lisp list
     of state strings). Returns the upserted path."
    (let ((lf (get (quote todo-list) list-id)))
      (if (null? lf)
        "no such list"
        (let ((path (node-path (quote todo-list) list-id)))
          (let ((body (dict-set* lf (list "columns" (json-emit cols)))))
            (tree/upsert path "datum" "todo-list" body)))))))

(define kanban/digit
  (lambda (c)
    "Integer value of a single digit char, or 0."
    (if (string-eq c "0") 0
      (if (string-eq c "1") 1
        (if (string-eq c "2") 2
          (if (string-eq c "3") 3
            (if (string-eq c "4") 4
              (if (string-eq c "5") 5
                (if (string-eq c "6") 6
                  (if (string-eq c "7") 7
                    (if (string-eq c "8") 8
                      (if (string-eq c "9") 9 0))))))))))))

;; ------------------------------------------------------------------
;; POST dispatch
;; ------------------------------------------------------------------

(define kanban/handle-post
  (lambda ()
    "Read action and dispatch to the tree-action mutation."
    (let ((action (let ((a (request-param "action"))) (if (null? a) "" (as-str a)))))
      (let ((id (let ((x (request-param "id"))) (if (null? x) "" (as-str x))))
            (list-id (let ((x (request-param "list"))) (if (null? x) (todo/ensure-inbox) (as-str x))))
            (state (let ((x (request-param "state"))) (if (null? x) "" (as-str x))))
            (index (let ((x (request-param "index"))) (if (null? x) "" (as-str x))))
            (name (let ((x (request-param "name"))) (if (null? x) "" (as-str x))))
            (title (let ((x (request-param "title"))) (if (null? x) "" (as-str x)))))
        (if (string-eq action "move")
          (kanban/move-card id state (kanban/parse-index index))
          (if (string-eq action "up")
            (kanban/shift id list-id -1)
            (if (string-eq action "down")
              (kanban/shift id list-id 1)
              (if (string-eq action "left")
                (kanban/shift-col id list-id -1)
                (if (string-eq action "right")
                  (kanban/shift-col id list-id 1)
                  (if (string-eq action "add-card")
                    (kanban/add-card list-id title state)
                    (if (string-eq action "add-column")
                      (kanban/add-column list-id name)
                      (if (string-eq action "remove-column")
                        (kanban/remove-column list-id name)
                        (if (string-eq action "rename-column")
                          (kanban/rename-column list-id name state)
                          "unknown action")))))))))))))

(define kanban/in-columns?
  (lambda (columns state)
    "True if state is one of the column names."
    (if (null? columns)
      #f
      (if (string-eq (as-str (car columns)) state)
        #t
        (kanban/in-columns? (cdr columns) state)))))

(define kanban/index-of
  (lambda (cards id i)
    "0-based index of the row whose id matches, or -1."
    (if (null? cards)
      -1
      (if (string-eq (as-str (car (car cards))) (as-str id))
        i
        (kanban/index-of (cdr cards) id (+ i 1))))))

(define kanban/index-of-cols
  (lambda (cols state i)
    "0-based index of state within cols, or -1."
    (if (null? cols)
      -1
      (if (string-eq (as-str (car cols)) state)
        i
        (kanban/index-of-cols (cdr cols) state (+ i 1))))))

(define kanban/insert-at
  (lambda (cards row index)
    "Insert row into cards at the given index (0-based); clamp to end."
    (kanban/insert-at-at cards row index 0 (list))))

(define kanban/insert-at-at
  (lambda (cards row index i acc)
    "Internal accumulating insert of row at index into cards."
    (if (null? cards)
      (append (reverse acc) (list row))
      (if (>= i index)
        (append (reverse acc) (cons row cards))
        (kanban/insert-at-at (cdr cards) row index (+ i 1) (cons (car cards) acc))))))

(define kanban/list-options
  (lambda (lists current-id)
    "Concatenated <option> HTML for each todo-list, marking current-id selected."
    (if (null? lists)
      ""
      (let ((id (as-str (car (car lists))))
            (title (as-str (dict-get (cdr (car lists)) "title"))))
        (string-append
          (html/option id title (string-eq id (as-str current-id)))
          (kanban/list-options (cdr lists) current-id))))))

(define kanban/list-picker
  (lambda (current-id)
    "Render a select of all todo-lists linking to /kanban?list=<id>."
    (let ((lists (todo/list/all)))
      (html/form "GET" "/kanban"
        (html/attr "class" "kb-list-picker")
        (html/join
          (list
            (html/label (html/attr "class" "muted") "board:")
            (html/select "list"
              (html/attrs (list (html/attr "onchange" "this.form.submit()")))
              (kanban/list-options lists current-id))))))))

;; ------------------------------------------------------------------
;; org card vocabulary (agents): make / move / stale
;; ------------------------------------------------------------------

(define kanban/make-card
  (lambda
    (list-id title notes)
    "Create a card with title and notes on list-id (empty = inbox). Returns the new id."
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
            (as-str notes)
            "state"
            "TODO"
            "due"
            ""
            "position"
            "0"
            "done"
            "false"
            "pipeline"
            ""
            "kind"
            ""))))))

(define kanban/move-btn
  (lambda (id list-id action label)
    "Small POST form button for one move action on a card."
    (html/form "POST" "/kanban"
      (html/attrs
        (list
          (html/attr "class" "kb-move-form")
          (html/attr "hx-post" "/kanban")
          (html/attr "hx-target" "#kb-board")
          (html/attr "hx-swap" "innerHTML")))
      (html/join
        (list
          (html/hidden "action" action)
          (html/hidden "id" id)
          (html/hidden "list" list-id)
          (html/button (html/attr "type" "submit") label))))))

(define kanban/move-card
  (lambda
    (id target-state target-index)
    (let
      ((fields (get (quote todo-item) id)))
      (if
        (null? fields)
        "no such card"
        (let
          ((old-state (as-str (dict-get fields "state")))
            (list-id (as-str (dict-get fields "list_id"))))
          (let
            ((all (todo/items-for-list list-id)))
            (let
              ((without (kanban/remove-id all id)))
              (if
                (string-eq old-state target-state)
                (let
                  ((col
                    (list/filter
                      (lambda
                        (row)
                        (string-eq
                          (as-str (dict-get (cdr row) "state"))
                          old-state))
                      without)))
                  (let
                    ((reordered
                      (kanban/insert-at col (cons id fields) target-index)))
                    (kanban/renumber reordered old-state)))
                (let
                  ((old-col
                    (list/filter
                      (lambda
                        (row)
                        (string-eq
                          (as-str (dict-get (cdr row) "state"))
                          old-state))
                      without)))
                  (let
                    ((new-col
                      (list/filter
                        (lambda
                          (row)
                          (string-eq
                            (as-str (dict-get (cdr row) "state"))
                            target-state))
                        without)))
                    (let
                      ((reordered
                        (kanban/insert-at new-col (cons id fields) target-index)))
                      (seq
                        (kanban/renumber old-col old-state)
                        (kanban/renumber reordered target-state))))))))))))
  "Move card id to target-state at target-index (0-based; clamps to end).\n     Renumbers the old and new columns so no two cards share a position.\n     Runs inside the caller's transaction: /api/eval wraps each request in\n     one (nested with-transaction raises begin_work), and page handlers\n     that need atomicity wrap their own. i-kanban-move-card 2026-09-03.")

(define kanban/other-bucket
  (lambda (list-id columns)
    "Cards that belong in the trailing 'other' bucket of a board with columns
       `columns`: state not a column. ONE card scan (was: a full rescan per
       render — card 8972cdfe)."
    (kanban/other-bucket-from list-id (todo/items-for-list list-id) columns)))

(define kanban/other-bucket-from
  (lambda (list-id all-cards columns)
    "The 'other' bucket, from a PRECOMPUTED (todo/items-for-list) scan:
       strays = state not a column."
    (let ((strays (list/filter
                    (lambda (row)
                      (kanban/stray? columns row))
                    all-cards)))
      (if (null? strays)
        ""
        (html/tag "section"
          (html/attrs
            (list (html/attr "class" "kb-col kb-other")))
          (html/join
            (list
              (html/div (html/attr "class" "kb-col-header") (html/text "other"))
              (html/div (html/attr "class" "kb-cards")
                (kanban/cards-html list-id strays columns)))))))))

(define kanban/out-of-bounds?
  (lambda (j len)
    "True if index j is outside [0, len)."
    (if (< j 0) #t (>= j len))))

;; ------------------------------------------------------------------
;; route handler
;; ------------------------------------------------------------------

(define kanban/page
  (lambda ()
    "GET /kanban?list=<id> renders the board; POST dispatches a tree action."
    (let ((list-id
            (let ((x (request-param "list")))
              (if (null? x) (todo/ensure-inbox) (as-str x)))))
      (if (string-eq (request-method) "POST")
        (seq
          (kanban/handle-post)
          (if (hx-request?)
            (render-html (kanban/board list-id))
            (redirect (string-append "/kanban?list=" list-id))))
        (html/page
          (list
            (html/h1 "Kanban")
            (html/p* (html/attr "class" "muted")
              "A board view over your todo-list. Drag cards between columns, or use the arrow buttons. Columns are the list's states; edit them with + column.")
            (kanban/board list-id)))))))

(define kanban/parse-index
  (lambda (s)
    "Parse a decimal index string; empty/non-numeric -> 9999 (append)."
    (if (string-eq s "")
      9999
      (kanban/parse-index-safe s 0))))

(define kanban/parse-index-safe
  (lambda (s acc)
    "Accumulating decimal parse of an index string."
    (if (string-eq s "")
      acc
      (kanban/parse-index-safe
        (substring s 1 (string-length s))
        (+ (* acc 10) (kanban/digit (substring s 0 1)))))))

(define kanban/remove-column
  (lambda (list-id name)
    "Remove a column name from the list's columns. Cards in that state are
     NOT deleted (they land in the 'other' bucket). Returns upserted path."
    (let ((cols (kanban/columns-for-list list-id)))
      (kanban/columns-set list-id (kanban/remove-named cols (as-str name))))))

(define kanban/remove-id
  (lambda (cards id)
    "Filter cards to exclude the row whose id matches."
    (list/filter
      (lambda (row) (not (string-eq (as-str (car row)) (as-str id))))
      cards)))

(define kanban/remove-named
  (lambda (cols name)
    "Drop the column named name from cols."
    (list/filter (lambda (c) (not (string-eq (as-str c) name))) cols)))

(define kanban/rename-column
  (lambda
    (list-id old-name new-name)
    (let
      ((cols (kanban/columns-for-list list-id)))
      (let
        ((renamed
          (kanban/replace-named
            cols
            (as-str old-name)
            (as-str new-name))))
        (seq
          (kanban/columns-set list-id renamed)
          (kanban/cascade-rename-state
            list-id
            (as-str old-name)
            (as-str new-name))))))
  "Rename a column: update the list's columns AND cascade-rename the state\n     field of every card in that column. Runs inside the caller's\n     transaction (see kanban/move-card note).")

(define kanban/rename-state-at
  (lambda (cards new-name)
    "tree/upsert each card with its state set to new-name."
    (if (null? cards)
      ()
      (let ((row (car cards)))
        (let ((id (car row)) (fields (cdr row)))
          (seq
            (tree/upsert (node-path (quote todo-item) id) "datum" "todo-item"
              (dict-set* fields (list "state" new-name)))
            (kanban/rename-state-at (cdr cards) new-name)))))))

(define kanban/renumber
  (lambda (cards state)
    "Renumber cards in the given state from 0, tree/upserting each."
    (kanban/renumber-at cards state 0)))

(define kanban/renumber-at
  (lambda (cards state i)
    "tree/upsert each card with the given state and position = its index.
     Must run inside a with-transaction for atomicity."
    (if (null? cards)
      ()
      (let ((row (car cards)))
        (let ((id (car row)) (fields (cdr row)))
          (seq
            (let ((path (node-path (quote todo-item) id)))
              (tree/upsert path "datum" "todo-item"
                (dict-set* fields (list "state" state "position" (as-str i)))))
            (kanban/renumber-at (cdr cards) state (+ i 1))))))))

(define kanban/replace-named
  (lambda (cols old-name new-name)
    "Rename old-name to new-name throughout cols."
    (list/map
      (lambda (c) (if (string-eq (as-str c) old-name) new-name (as-str c)))
      cols)))

(define kanban/review-ins
  (lambda
    (sorted row)
    "Insertion-sort step for kanban/review-sorted."
    (if
      (null? sorted)
      (list row)
      (if
        (kanban/review-le? row (car sorted))
        (cons row sorted)
        (cons (car sorted) (kanban/review-ins (cdr sorted) row))))))

(define kanban/review-le?
  (lambda
    (a b)
    "True when row a sorts before or equal to row b by (priority rank, position, id)."
    (let
      ((ra (kanban/review-prio-rank (cdr a)))
        (rb (kanban/review-prio-rank (cdr b))))
      (if
        (< ra rb)
        #t
        (if
          (> ra rb)
          #f
          (let
            ((pa (kanban/review-prio-pos (cdr a)))
              (pb (kanban/review-prio-pos (cdr b))))
            (if
              (< pa pb)
              #t
              (if
                (> pa pb)
                #f
                (string<=? (car a) (car b))))))))))
(define kanban/review-li
  (lambda
    (row)
    "Render one card row with priority, assignee, state and truncated notes."
    (let*
      ((f (cdr row))
        (t0 (dict-get f "title"))
        (st0 (dict-get f "state"))
        (pr0 (dict-get f "priority"))
        (as0 (dict-get f "assignee"))
        (nt0 (dict-get f "notes")))
      (let*
        ((t (if (null? t0) "" (as-str t0)))
          (st (if (null? st0) "" (as-str st0)))
          (pr (if (null? pr0) "" (as-str pr0)))
          (as (if (null? as0) "" (as-str as0)))
          (nt (if (null? nt0) "" (as-str nt0))))
        (let*
          ((badge (string-append "[" (if (string-eq pr "") "-" pr) "] "))
            (who (string-append " @" (if (string-eq as "") "unassigned" as) " (" st ")"))
            (nts (if (> (string-length nt) 90) (string-append (substring nt 0 90) "...") nt)))
          (html/li
            (html/attr "class" "todo-item")
            (html/join
              (list
                (html/span (html/attr "class" "prio") (html/text badge))
                (html/span (html/attr "class" "todo-title") (html/text t))
                (html/span (html/attr "class" "muted") (html/text who))
                (html/span (html/attr "class" "muted") (html/text (string-append " " nts)))))))))))

(define kanban/review-page
  (lambda
    ()
    "Work-order view of the default board: priority, then position, then id."
    (html/page
      (list
        (html/h1 "Board review - todo-assignment branch")
        (html/p*
          (html/attr "class" "muted")
          "Queue order: priority P0-P3, then position, then id. Empty bracket = unprioritized, sinks to bottom.")
        (html/h2 "Cards in work order")
        (html/ul
          (html/attr "class" "card todo-list")
          (list/foldl
            (lambda
              (acc row)
              (string-append acc (kanban/review-li row)))
            ""
            (kanban/review-sorted (todo/items-for-list "0b21ffaa-3a67-4bed-a9ac-658c855b4704"))))))))

(define kanban/review-prio-pos
  (lambda
    (fields)
    "The position field as a number; 0 when absent or non-numeric."
    (let
      ((q (dict-get fields "position")))
      (if
        (null? q)
        0
        (let
          ((s (as-str q)))
          (if
            (string-eq s "")
            0
            (parse-int s)))))))
(define kanban/review-prio-rank
  (lambda
    (fields)
    "Numeric work rank of the priority field: P0=0 .. P3=3, unknown or empty = 4."
    (let
      ((p (dict-get fields "priority")))
      (if
        (null? p)
        4
        (let
          ((s (as-str p)))
          (if
            (string-eq s "P0")
            0
            (if
              (string-eq s "P1")
              1
              (if
                (string-eq s "P2")
                2
                (if
                  (string-eq s "P3")
                  3
                  4)))))))))

(define kanban/review-sorted
  (lambda
    (rows)
    "Rows ordered by priority rank, then position, then id."
    (list/foldl kanban/review-ins () rows)))

(define kanban/script
  (lambda ()
    "Minimal DnD shim: dragstart stores the card id; drop fills the column form."
    (render-html
      "<script>
function kbDrag(ev){ window.__kbDrag=ev.target.dataset.id||ev.target.closest('.kb-card').dataset.id; }
function kbDrop(ev){
  ev.preventDefault();
  var f=ev.currentTarget;
  f.querySelector('[name=id]').value=window.__kbDrag||'';
  f.querySelector('[name=index]').value='';
  htmx.trigger(f,'submit');
}
</script>")))

(define kanban/shift
  (lambda (id list-id delta)
    "Shift card up/down within its column by swapping with the neighbor at
     (current-index + delta)."
    (let ((fields (get (quote todo-item) id)))
      (if (null? fields)
        "no such card"
        (let ((state (as-str (dict-get fields "state"))))
          (let ((cards (kanban/cards-in list-id state)))
            (let ((i (kanban/index-of cards id 0)))
              (let ((j (+ i delta)))
                (if (kanban/out-of-bounds? j (length cards))
                  "at edge"
                  (let ((other-row (nth cards j)))
                    (kanban/swap id (as-str (car other-row)))))))))))))

(define kanban/shift-col
  (lambda (id list-id delta)
    "Move card to the adjacent column (delta -1 = left, +1 = right)."
    (let ((fields (get (quote todo-item) id)))
      (if (null? fields)
        "no such card"
        (let ((state (as-str (dict-get fields "state")))
              (cols (kanban/columns-for-list list-id)))
          (let ((i (kanban/index-of-cols cols state 0)))
            (let ((j (+ i delta)))
              (if (kanban/out-of-bounds? j (length cols))
                "at edge"
                (kanban/move-card id (as-str (nth cols j)) 9999)))))))))

;; (The old 2-arg kanban/move-card that lived here was removed 2026-09-03:
;;  it shadowed the 3-arg renumbering move-card above depending on define
;;  order. Callers that just want a state flip can use todo/item/set-state.)
(define kanban/stale
  (lambda
    ()
    "Open cards (state not DONE or ARCHIVED) as (id . fields) rows."
    (list/filter
      (lambda
        (row)
        (todo/state/open? (todo/item/state (cdr row))))
      (todo/item/all))))

(define kanban/stray?
  (lambda
    (columns row)
    (not
      (kanban/in-columns?
        columns
        (as-str (dict-get (cdr row) "state")))))
  "True when a card belongs in the board's 'other' bucket: its state is not      one of the board's columns. OBITUARY (audit 2026-10-05): the own-      pipeline disjunct is retired with the per-item pipeline override      (todo/state/item-pipeline) — one pipeline per list, so stray is simply      not-a-column.")

(define kanban/swap
  (lambda
    (id1 id2)
    (let
      ((f1 (get (quote todo-item) id1)))
      (if
        (null? f1)
        "no such card"
        (let
          ((f2 (get (quote todo-item) id2)))
          (if
            (null? f2)
            "no such card"
            (let
              ((p1 (as-str (dict-get f1 "position")))
                (p2 (as-str (dict-get f2 "position"))))
              (seq
                (tree/upsert
                  (node-path (quote todo-item) id1)
                  "datum"
                  "todo-item"
                  (dict-set* f1 (list "position" p2)))
                (tree/upsert
                  (node-path (quote todo-item) id2)
                  "datum"
                  "todo-item"
                  (dict-set* f2 (list "position" p1))))))))))
  "Swap the position fields of two cards (same column). Runs inside the\n     caller's transaction (see kanban/move-card note).")

(define kanban/ensure-auto-lane
  (lambda
    ()
    "Resolve-or-create the AUTO lane (stream-pipelines: the no-human-gate
     agent stream). Root cause of the Auto accretion (13 dead Auto lists
     in one garden, 2026-10-06): the lane was minted manually per session
     — each run created a fresh garden list instead of reusing. This
     resolves by title across the caller's visible lists (garden first,
     shared next, per the todo/list/all merge) and only creates when
     genuinely absent. Returns (id . fields)."
    (let
      ((hits
        (list/filter
          (lambda (row)
            (string-eq (as-str (dict-get (cdr row) "title")) "Auto"))
          (todo/list/all))))
      (if
        (null? hits)
        (kanban/auto-lane-create)
        (car hits)))))

(define kanban/auto-lane-create
  (lambda
    ()
    "Create the AUTO lane with the canonical columns + recipes verbatim
     from the live lane (2026-10-06 snapshot; the spec source is
     borge/workstreams.borg stream-pipelines). Created in the CALLER's
     garden; promotion to a shared space is a deliberate migration, not
     a side effect."
    (todo/tree/create
      "todo-list"
      (dict-set*
        "{}"
        (list
          "title"
          "Auto"
          "description"
          "Intake (AUTO lane): fully-mechanical agent work — no SPEC column, no human gate. See borge/workstreams.borg stream-pipelines."
          "is_default"
          "false"
          "columns"
          (list "TODO" "CODE" "LOGIC-REVIEW" "HUMAN-REVIEW" "DONE" "PARKED")
          "recipes"
          (dict-set*
            "{}"
            (list
              "TODO"
              "Intake (AUTO lane): this stream has no SPEC column and no human gate. If the card needs a spec decision first or touches UI, schema migrations, auth, store, or prod promote, it does NOT belong in this list — move it to the Board list (FACES/KERNEL) and comment why. Otherwise set the card to CODE and comment the placement."
              "LOGIC-REVIEW"
              "You are the LOGIC REVIEWER for Pricklypear on the AUTO lane (workstreams.borg stream-pipelines) — this stream has NO human gate: your PASS closes the card. Review the card's work on its target branch against its acceptance criteria: read the change, run dune build + dune runtest (lisp/common at minimum), check borge drift after any .borg edit, and independently re-verify the stated evidence (run the smoke/test yourself, do not trust the comment). PASS -> end with DESTINATION: DONE plus your verdict + evidence comment. FAIL -> end with DESTINATION: CODE and a comment listing exactly what to fix. ESCALATION: if the card thread (todo-events + comments) already shows two FAIL verdicts for the same defect, do not loop again — end with DESTINATION: HUMAN-REVIEW and a comment explaining the repeated failure.")))))))
