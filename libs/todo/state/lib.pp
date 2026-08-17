;; package todo/state -- exported from live branch system/main
;; Sync: scripts/pp_sync.py pull|push

(define todo/state/archived?
  (lambda (s) "True when state s is the ARCHIVED value." (string-eq (as-str s) "ARCHIVED")))

(define todo/state/list
  (list "" "TODO" "DONE" "WAITING" "CANCELLED"))

(define todo/state/next
  (lambda
    (cur)
    "Next state after cur in the canonical state list."
    (outline/todo-next-in todo/state/list (as-str cur))))

(define todo/state/columns-for-list
  (lambda
    (list-id)
    "The state set (list of state strings) for a todo-list: the list's own
     `columns` field when present and non-empty, else the global default
     todo/state/list. THE per-list override kanban columns and the /todos
     state machine both read through, so a list with custom columns cycles
     only those states everywhere."
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
              (if
                (string-eq cs "")
                todo/state/list
                (json-parse cs)))))))))

(define todo/state/next-for-list
  (lambda
    (cur list-id)
    "Next state after cur within list-id's own state set (custom columns when
     present, else the global default). Wraps around: last column -> first of
     that same set."
    (todo/state/todo-next-in (todo/state/columns-for-list list-id) (as-str cur))))

(define todo/state/todo-next-in
  (lambda
    (xs cur)
    "State following cur within the state list xs; xs's own head on wrap (last
     -> first) and on a not-found cur, so the cycle never leaves the list's
     columns. (A per-set wrap, distinct from outline/todo-next-in which
     hardcodes the global default state list's head.)"
    (if
      (null? xs)
      "TODO"
      (todo/state/todo-next-in-at xs cur (as-str (car xs))))))

(define todo/state/todo-next-in-at
  (lambda
    (xs cur head)
    "Accumulating: find cur in xs, return the next; wrap to head at the end or
     on absent."
    (if
      (null? xs)
      head
      (if
        (string-eq (as-str (car xs)) cur)
        (if
          (null? (cdr xs))
          head
          (as-str (car (cdr xs))))
        (todo/state/todo-next-in-at (cdr xs) cur head)))))

(define todo/state/open?
  (lambda
    (s)
    "True for any state other than empty/TODO/DONE/CANCELLED/ARCHIVED."
    (let
      ((t (as-str s)))
      (if
        (string-eq t "")
        #f
        (if
          (string-eq t "DONE")
          #f
          (if
            (string-eq t "CANCELLED")
            #f
            (if (string-eq t "ARCHIVED") #f #t)))))))

(define todo/state/options
  (lambda
    (selected)
    "HTML options for the canonical state list with selected marked."
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
    "HTML options over the state list plus ARCHIVED."
    (todo/state/options-from
      selected
      (append todo/state/list (list "ARCHIVED")))))

(define todo/state/options-from
  (lambda
    (selected states)
    "HTML options over the given states with selected marked."
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

(define todo/state/options-for-list
  (lambda
    (selected list-id)
    "HTML options over list-id's own state set PLUS ARCHIVED, with selected
     marked — the detail <select> for an item in that list. Uses the list's
     custom columns when present, else the canonical options-all state set."
    (todo/state/options-from
      selected
      (append (todo/state/columns-for-list list-id) (list "ARCHIVED")))))

