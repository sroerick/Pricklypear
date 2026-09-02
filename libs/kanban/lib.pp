;; package kanban -- a board view over todos. One todo-list = one board;
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

(define kanban/columns-for-list
  (lambda (list-id)
    "Return the list's columns as a Lisp list of state strings.
     Falls back to todo/state/list when the list has no columns field
     or the field is empty. Single source of truth: todo/state/columns-for-list."
    (todo/state/columns-for-list list-id)))

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

(define kanban/renumber
  (lambda (cards state)
    "Renumber cards in the given state from 0, tree/upserting each."
    (kanban/renumber-at cards state 0)))

(define kanban/remove-id
  (lambda (cards id)
    "Filter cards to exclude the row whose id matches."
    (list/filter
      (lambda (row) (not (string-eq (as-str (car row)) (as-str id))))
      cards)))

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

(define kanban/move-card
  (lambda (id target-state target-index)
    "Move card id to target-state at target-index (0-based; clamps to end).
     Renumbers the old and new columns so no two cards share a position.
     All tree/upserts run in one with-transaction."
    (with-transaction
      (lambda ()
        (let ((fields (get (quote todo-item) id)))
          (if (null? fields)
            "no such card"
            (let ((old-state (as-str (dict-get fields "state")))
                  (list-id (as-str (dict-get fields "list_id"))))
              (let ((all (todo/items-for-list list-id)))
                (let ((without (kanban/remove-id all id)))
                  (if (string-eq old-state target-state)
                    (let ((col (list/filter
                                 (lambda (row)
                                   (string-eq (as-str (dict-get (cdr row) "state")) old-state))
                                 without)))
                      (let ((reordered (kanban/insert-at col (cons id fields) target-index)))
                        (kanban/renumber reordered old-state)))
                    (let ((old-col (list/filter
                                     (lambda (row)
                                       (string-eq (as-str (dict-get (cdr row) "state")) old-state))
                                     without)))
                      (let ((new-col (list/filter
                                       (lambda (row)
                                         (string-eq (as-str (dict-get (cdr row) "state")) target-state))
                                       without)))
                        (let ((reordered (kanban/insert-at new-col (cons id fields) target-index)))
                          (seq
                            (kanban/renumber old-col old-state)
                            (kanban/renumber reordered target-state)))))))))))))))

(define kanban/swap
  (lambda (id1 id2)
    "Swap the position fields of two cards (same column) in one with-transaction."
    (with-transaction
      (lambda ()
        (let ((f1 (get (quote todo-item) id1)))
          (if (null? f1)
            "no such card"
            (let ((f2 (get (quote todo-item) id2)))
              (if (null? f2)
                "no such card"
                (let ((p1 (as-str (dict-get f1 "position")))
                      (p2 (as-str (dict-get f2 "position"))))
                  (seq
                    (tree/upsert (node-path (quote todo-item) id1) "datum" "todo-item"
                      (dict-set* f1 (list "position" p2)))
                    (tree/upsert (node-path (quote todo-item) id2) "datum" "todo-item"
                      (dict-set* f2 (list "position" p1)))))))))))))

(define kanban/index-of
  (lambda (cards id i)
    "0-based index of the row whose id matches, or -1."
    (if (null? cards)
      -1
      (if (string-eq (as-str (car (car cards))) (as-str id))
        i
        (kanban/index-of (cdr cards) id (+ i 1))))))

;; ------------------------------------------------------------------
;; column mutation (add / remove / rename)
;; ------------------------------------------------------------------

(define kanban/add-column
  (lambda (list-id name)
    "Append a new column name to the list's columns. Returns upserted path."
    (let ((cols (kanban/columns-for-list list-id)))
      (kanban/columns-set list-id (append cols (list (as-str name)))))))

(define kanban/remove-column
  (lambda (list-id name)
    "Remove a column name from the list's columns. Cards in that state are
     NOT deleted (they land in the 'other' bucket). Returns upserted path."
    (let ((cols (kanban/columns-for-list list-id)))
      (kanban/columns-set list-id (kanban/remove-named cols (as-str name))))))

(define kanban/remove-named
  (lambda (cols name)
    "Drop the column named name from cols."
    (list/filter (lambda (c) (not (string-eq (as-str c) name))) cols)))

(define kanban/rename-column
  (lambda (list-id old-name new-name)
    "Rename a column: update the list's columns AND cascade-rename the state
     field of every card in that column, in one with-transaction."
    (with-transaction
      (lambda ()
        (let ((cols (kanban/columns-for-list list-id)))
          (let ((renamed (kanban/replace-named cols (as-str old-name) (as-str new-name))))
            (seq
              (kanban/columns-set list-id renamed)
              (kanban/cascade-rename-state list-id (as-str old-name) (as-str new-name)))))))))

(define kanban/replace-named
  (lambda (cols old-name new-name)
    "Rename old-name to new-name throughout cols."
    (list/map
      (lambda (c) (if (string-eq (as-str c) old-name) new-name (as-str c)))
      cols)))

(define kanban/cascade-rename-state
  (lambda (list-id old-name new-name)
    "tree/upsert every card in list-id whose state is old-name, setting new-name."
    (let ((matches (list/filter
                     (lambda (row)
                       (string-eq (as-str (dict-get (cdr row) "state")) old-name))
                     (todo/items-for-list list-id))))
      (kanban/rename-state-at matches new-name))))

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
            "done" "false"))))))

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

(define kanban/column-html
  (lambda (list-id state columns)
    "Render one column as a form (drop target) with its cards."
    (let ((cards (kanban/cards-in list-id state)))
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
              (kanban/add-card-form list-id state))))))))

(define kanban/cards-html
  (lambda (list-id cards columns)
    "Concatenated card HTML for the card list."
    (if (null? cards)
      ""
      (string-append
        (kanban/card-html list-id (car cards) columns)
        (kanban/cards-html list-id (cdr cards) columns)))))

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

(define kanban/other-bucket
  (lambda (list-id columns)
    "Cards whose state is not in the list's columns, rendered in a trailing bucket."
    (let ((all (todo/items-for-list list-id)))
      (let ((strays (list/filter
                      (lambda (row)
                        (not (kanban/in-columns? columns (as-str (dict-get (cdr row) "state")))))
                      all)))
        (if (null? strays)
          ""
          (html/tag "section"
            (html/attrs
              (list (html/attr "class" "kb-col kb-other")))
            (html/join
              (list
                (html/div (html/attr "class" "kb-col-header") (html/text "other"))
                (html/div (html/attr "class" "kb-cards")
                  (kanban/cards-html list-id strays columns))))))))))

(define kanban/in-columns?
  (lambda (columns state)
    "True if state is one of the column names."
    (if (null? columns)
      #f
      (if (string-eq (as-str (car columns)) state)
        #t
        (kanban/in-columns? (cdr columns) state)))))

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

(define kanban/board
  (lambda (list-id)
    "Render the full board: list picker + columns row + other bucket + add-column."
    (let ((cols (kanban/columns-for-list list-id)))
      (html/div (html/attr "id" "kb-board")
        (html/join
          (list
            (kanban/list-picker list-id)
            (html/div (html/attr "class" "kb-cols")
              (string-append
                (kanban/columns-html list-id cols)
                (kanban/add-column-form list-id)))
            (kanban/other-bucket list-id cols)
            (kanban/script)))))))

(define kanban/columns-html
  (lambda (list-id cols)
    "Concatenated column HTML for the column list."
    (if (null? cols)
      ""
      (string-append
        (kanban/column-html list-id (as-str (car cols)) cols)
        (kanban/columns-html list-id (cdr cols))))))

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

(define kanban/out-of-bounds?
  (lambda (j len)
    "True if index j is outside [0, len)."
    (if (< j 0) #t (>= j len))))

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

(define kanban/index-of-cols
  (lambda (cols state i)
    "0-based index of state within cols, or -1."
    (if (null? cols)
      -1
      (if (string-eq (as-str (car cols)) state)
        i
        (kanban/index-of-cols (cdr cols) state (+ i 1))))))

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

(route "/kanban" "kanban/page")

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
            "false"))))))

(define kanban/move-card
  (lambda
    (id state)
    "Move card id to state. Returns id, or () when the card is missing."
    (let
      ((row (todo/tree/get "todo-item" (as-str id))))
      (if
        (null? row)
        ()
        (seq
          (todo/item/set-state (as-str id) (as-str state))
          (as-str id))))))

(define kanban/stale
  (lambda
    ()
    "Open cards (state not DONE or ARCHIVED) as (id . fields) rows."
    (list/filter
      (lambda
        (row)
        (todo/state/open? (todo/item/state (cdr row))))
      (todo/item/all))))
