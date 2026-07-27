;; Workout journal — fluid session.doc + standardized exercise facts.
;; Authority: borge/workout.borg. Free-log MVP (structured ksk/qd later).
;; Install: (load-library "workout") — seeded on system/main after calendar.
;; Depends: std (html/*, blank?, list/*, cond, when/unless).

(deftype 'workout-session
  'date 'program 'intensity 'state 'notes 'doc 'duration_sec 'author)

(deftype 'workout-exercise
  'session_id 'exercise 'weight 'unit 'reps 'notes 'detail)

;; --- small helpers ---

(define workout/today (lambda () (date-today))
  "Today YYYY-MM-DD.")

(define workout/row-id (lambda (row) (as-str (car row)))
  "Id from (id . fields).")

(define workout/row-fields (lambda (row) (cdr row))
  "Fields from (id . fields).")

(define workout/field
  (lambda (fields key)
    (let ((v (dict-get fields (as-str key))))
      (if (null? v) "" (as-str v))))
  "String field or empty.")

(define workout/as-int
  (lambda (x)
    (let ((s (as-str x)))
      (if (blank? s) 0 (parse-int s))))
  "Parse int; blank -> 0.")

(define workout/normalize-unit
  (lambda (u)
    (let ((s (as-str u)))
      (cond
        ((blank? s) "")
        ((string-eq s "KG") "kg")
        ((string-eq s "Kg") "kg")
        ((string-eq s "LB") "lb")
        ((string-eq s "Lb") "lb")
        ((string-eq s "lbs") "lb")
        ((string-eq s "LBS") "lb")
        (else s))))
  "Normalize unit to kg|lb when obvious.")

(define workout/normalize-exercise
  (lambda (name) (string-trim (as-str name)))
  "Trim exercise name.")

(define workout/empty-doc
  (lambda (program intensity)
    (dict-set* "{}"
      (list
        "program" (as-str program)
        "intensity" (as-str intensity)
        "params" "{}"
        "progress" "{}"
        "blocks" "[]")))
  "Minimal fluid doc envelope (JSON tree).")

;; Lexicographic compare for YYYY-MM-DD strings.
(define workout/char-rank
  (lambda (c)
    (let ((s (as-str c)))
      (cond
        ((string-eq s "0") 0) ((string-eq s "1") 1) ((string-eq s "2") 2)
        ((string-eq s "3") 3) ((string-eq s "4") 4) ((string-eq s "5") 5)
        ((string-eq s "6") 6) ((string-eq s "7") 7) ((string-eq s "8") 8)
        ((string-eq s "9") 9) ((string-eq s "-") 10)
        (else 50))))
  "Rank digit/dash for date compare.")

(define workout/str-lt-loop
  (lambda (a b i)
    (let ((la (string-length a))
          (lb (string-length b)))
      (if (>= i la)
          (if (>= i lb) #f #t)
          (if (>= i lb)
              #f
              (let ((ca (substring a i 1))
                    (cb (substring b i 1)))
                (if (string-eq ca cb)
                    (workout/str-lt-loop a b (+ i 1))
                    (< (workout/char-rank ca) (workout/char-rank cb))))))))
  "Internal str < loop.")

(define workout/str-lt?
  (lambda (a b)
    (let ((sa (as-str a))
          (sb (as-str b)))
      (if (string-eq sa sb) #f (workout/str-lt-loop sa sb 0))))
  "True if a < b lexicographically.")

(define workout/date-ge?
  (lambda (a b)
    (if (string-eq (as-str a) (as-str b)) #t (not (workout/str-lt? a b))))
  "a >= b.")

(define workout/date-le?
  (lambda (a b)
    (if (string-eq (as-str a) (as-str b)) #t (workout/str-lt? a b)))
  "a <= b.")

(define workout/date-in-range?
  (lambda (day from to)
    (let ((d (as-str day)))
      (if (blank? d)
          #f
          (let ((ok-from
                  (if (blank? (as-str from)) #t (workout/date-ge? d from)))
                (ok-to
                  (if (blank? (as-str to)) #t (workout/date-le? d to))))
            (if ok-from ok-to #f)))))
  "True if day in [from,to] inclusive; blank bounds open.")

;; --- session CRUD ---

(define workout/session-fields
  (lambda (date program intensity state notes doc duration_sec author)
    (dict-set* "{}"
      (list
        "date" (as-str date)
        "program" (as-str program)
        "intensity" (as-str intensity)
        "state" (as-str state)
        "notes" (as-str notes)
        "doc" (as-str doc)
        "duration_sec" (as-str duration_sec)
        "author" (as-str author))))
  "Build workout-session fields.")

(define workout/session-create
  (lambda (program intensity)
    (let ((prog (if (blank? (as-str program)) "free" (as-str program)))
          (inten (if (null? intensity) "" (as-str intensity))))
      (create 'workout-session
        (workout/session-fields
          (workout/today) prog inten "in_progress" ""
          (workout/empty-doc prog inten) "" (as-str (whoami))))))
  "Create in_progress session; return id.")

(define workout/session-create-complete
  (lambda (program intensity date notes)
    (let ((prog (if (blank? (as-str program)) "free" (as-str program)))
          (inten (if (null? intensity) "" (as-str intensity)))
          (d (if (blank? (as-str date)) (workout/today) (as-str date)))
          (n (if (null? notes) "" (as-str notes))))
      (create 'workout-session
        (workout/session-fields
          d prog inten "complete" n
          (workout/empty-doc prog inten) "" (as-str (whoami))))))
  "Create complete session (quick log); return id.")

(define workout/session
  (lambda (id)
    (let ((f (get 'workout-session (as-str id))))
      (if (null? f) () f)))
  "Session fields or ().")

(define workout/session-list
  (lambda (limit)
    (let ((n (if (null? limit) 50 (workout/as-int limit))))
      (rows 'workout-session (if (< n 1) 50 n))))
  "Recent workout-session rows.")

(define workout/session-date (lambda (f) (workout/field f "date")))
(define workout/session-program (lambda (f) (workout/field f "program")))
(define workout/session-intensity (lambda (f) (workout/field f "intensity")))
(define workout/session-state (lambda (f) (workout/field f "state")))
(define workout/session-notes (lambda (f) (workout/field f "notes")))
(define workout/session-doc (lambda (f) (workout/field f "doc")))

(define workout/session-update
  (lambda (id fields)
    (update 'workout-session (as-str id) fields))
  "Replace session fields.")

(define workout/session-complete
  (lambda (id notes)
    (let ((cur (workout/session id)))
      (if (null? cur)
          ()
          (let ((n (if (blank? (as-str notes))
                       (workout/session-notes cur)
                       (as-str notes))))
            (workout/session-update id
              (workout/session-fields
                (workout/session-date cur)
                (workout/session-program cur)
                (workout/session-intensity cur)
                "complete" n
                (workout/session-doc cur)
                (workout/field cur "duration_sec")
                (workout/field cur "author")))))))
  "Mark session complete.")

(define workout/session-abandon
  (lambda (id)
    (let ((cur (workout/session id)))
      (if (null? cur)
          ()
          (workout/session-update id
            (workout/session-fields
              (workout/session-date cur)
              (workout/session-program cur)
              (workout/session-intensity cur)
              "abandoned"
              (workout/session-notes cur)
              (workout/session-doc cur)
              (workout/field cur "duration_sec")
              (workout/field cur "author"))))))
  "Mark session abandoned.")

(define workout/session-in-progress
  (lambda (program)
    (let ((want (if (null? program) "" (as-str program))))
      (list/foldl
        (lambda (acc row)
          (if (null? acc)
              (let ((f (workout/row-fields row)))
                (if (string-eq (workout/session-state f) "in_progress")
                    (if (blank? want)
                        (workout/row-id row)
                        (if (string-eq (workout/session-program f) want)
                            (workout/row-id row)
                            acc))
                    acc))
              acc))
        ()
        (workout/session-list 100))))
  "First in_progress session id (optional program filter).")

;; --- exercise facts ---

(define workout/exercise-fields
  (lambda (session-id exercise weight unit reps notes detail)
    (dict-set* "{}"
      (list
        "session_id" (as-str session-id)
        "exercise" (workout/normalize-exercise exercise)
        "weight" (as-str weight)
        "unit" (workout/normalize-unit unit)
        "reps" (as-str reps)
        "notes" (as-str notes)
        "detail" (as-str detail))))
  "Build workout-exercise fields.")

(define workout/exercises
  (lambda (session-id)
    (let ((sid (as-str session-id)))
      (list/filter
        (lambda (row)
          (string-eq (workout/field (workout/row-fields row) "session_id") sid))
        (rows 'workout-exercise 500))))
  "Exercise facts for session.")

(define workout/exercise-log
  (lambda (session-id exercise weight unit reps notes detail)
    (let ((name (workout/normalize-exercise exercise)))
      (if (blank? name)
          ()
          (create 'workout-exercise
            (workout/exercise-fields
              session-id name weight unit
              (if (blank? (as-str reps)) "0" (as-str reps))
              (if (null? notes) "" notes)
              (if (null? detail) "" detail))))))
  "Append exercise fact; return id.")

(define workout/exercise-delete
  (lambda (id) (delete 'workout-exercise (as-str id)))
  "Delete one exercise fact.")

(define workout/session-delete
  (lambda (id)
    (let ((sid (as-str id)))
      (seq
        (list/map
          (lambda (row) (workout/exercise-delete (workout/row-id row)))
          (workout/exercises sid))
        (delete 'workout-session sid))))
  "Delete session and its facts.")

;; --- queries ---

(define workout/session-date-of
  (lambda (session-id)
    (let ((f (workout/session session-id)))
      (if (null? f) "" (workout/session-date f))))
  "Date of session id.")

(define workout/volume
  (lambda (exercise from-date to-date weight unit)
    (let ((ex (workout/normalize-exercise exercise))
          (w (as-str weight))
          (u (workout/normalize-unit unit)))
      (list/foldl
        (lambda (acc row)
          (let ((f (workout/row-fields row)))
            (if (string-eq (workout/field f "exercise") ex)
                (let ((day (workout/session-date-of
                             (workout/field f "session_id"))))
                  (if (workout/date-in-range? day from-date to-date)
                      (let ((match-w
                              (if (blank? w) #t
                                  (string-eq (workout/field f "weight") w)))
                            (match-u
                              (if (blank? u) #t
                                  (string-eq
                                    (workout/normalize-unit
                                      (workout/field f "unit"))
                                    u))))
                        (if match-w
                            (if match-u
                                (+ acc
                                   (workout/as-int (workout/field f "reps")))
                                acc)
                            acc))
                      acc))
                acc)))
        0
        (rows 'workout-exercise 1000))))
  "Sum reps for exercise with optional date/load filters.")

(define workout/history
  (lambda (from-date to-date limit)
    (let ((lim (if (null? limit) 50 (workout/as-int limit))))
      (list/filter
        (lambda (row)
          (workout/date-in-range?
            (workout/session-date (workout/row-fields row))
            from-date to-date))
        (workout/session-list lim))))
  "Sessions in date range.")

(define workout/session-summary
  (lambda (id)
    (let ((f (workout/session id)))
      (if (null? f) () (list id f (workout/exercises id)))))
  "(id fields exercises) for one session.")

(define workout/name-seen?
  (lambda (lst name)
    (list/foldl
      (lambda (acc x)
        (if acc acc (string-eq (as-str x) (as-str name))))
      #f
      lst))
  "True if name in list.")

(define workout/exercise-names
  (lambda (limit)
    (let ((lim (if (null? limit) 100 (workout/as-int limit))))
      (list/foldl
        (lambda (acc row)
          (let ((n (workout/field (workout/row-fields row) "exercise")))
            (if (blank? n)
                acc
                (if (workout/name-seen? acc n) acc (cons n acc)))))
        ()
        (rows 'workout-exercise lim))))
  "Distinct exercise names.")

(define workout/session-total-reps
  (lambda (session-id)
    (list/foldl
      (lambda (acc row)
        (+ acc
           (workout/as-int
             (workout/field (workout/row-fields row) "reps"))))
      0
      (workout/exercises session-id)))
  "Total reps in a session.")

(define workout/programs
  (lambda () (list "free"))
  "Available programs (v1: free).")

(define workout/insert-session-desc
  (lambda (sorted row)
    (if (null? sorted)
        (list row)
        (let ((d (workout/session-date (workout/row-fields row)))
              (hd (workout/session-date (workout/row-fields (car sorted)))))
          (if (workout/date-ge? d hd)
              (cons row sorted)
              (cons (car sorted)
                    (workout/insert-session-desc (cdr sorted) row))))))
  "Insert into date-desc list.")

(define workout/sessions-sorted
  (lambda ()
    (list/foldl workout/insert-session-desc () (workout/session-list 100)))
  "Sessions newest-date first.")

;; --- HTML ---

(define workout/state-badge
  (lambda (state)
    (html/span
      (html/attr "class" "muted")
      (html/text (as-str state))))
  "State label.")

(define workout/session-title
  (lambda (f)
    (let ((date (workout/session-date f))
          (prog (workout/session-program f))
          (inten (workout/session-intensity f)))
      (if (blank? inten)
          (string-append date " · " prog)
          (string-append date " · " prog " / " inten))))
  "Human title for a session.")

(define workout/session-li
  (lambda (row)
    (let ((id (workout/row-id row))
          (f (workout/row-fields row)))
      (let ((reps (workout/session-total-reps id)))
        (html/li (html/attr "class" "card")
          (html/join
            (list
              (html/div ""
                (html/join
                  (list
                    (html/a (string-append "/workout/session?id=" id)
                      (html/strong (workout/session-title f)))
                    (html/text " ")
                    (workout/state-badge (workout/session-state f)))))
              (html/p* (html/attr "class" "muted")
                (html/text
                  (string-append (as-str reps) " total reps")))))))))
  "Recent session list item.")

(define workout/continue-card
  (lambda ()
    (let ((id (workout/session-in-progress ())))
      (if (null? id)
          ""
          (let ((f (workout/session id)))
            (if (null? f)
                ""
                (html/div (html/attr "class" "card")
                  (html/join
                    (list
                      (html/h2 "Continue")
                      (html/p
                        (string-append
                          (workout/session-title f) " is in progress."))
                      (html/p* ""
                        (html/a (string-append "/workout/session?id=" id)
                          "Open session"))))))))))
  "Continue in_progress card or empty string.")

(define workout/start-bar
  (lambda ()
    (html/div (html/attr "class" "card")
      (html/join
        (list
          (html/h2 "Start workout")
          (html/form "POST" "/workout" ""
            (html/join
              (list
                (html/hidden "action" "start")
                (html/hidden "program" "free")
                (html/label (html/attr "for" "w-inten") "Intensity (optional)")
                (html/text-input "intensity"
                  (html/attrs
                    (list
                      (html/attr "id" "w-inten")
                      (html/attr "placeholder" "easy / medium / hard"))))
                (html/button (html/attr "type" "submit") "Start free session"))))
          (html/p* (html/attr "class" "muted")
            (html/join
              (list
                (html/text "Or ")
                (html/a "/workout/log" "quick log")
                (html/text " a completed free workout."))))))))
  "Start free session controls.")

(define workout/recent-list
  (lambda ()
    (let ((rows (workout/sessions-sorted)))
      (html/div (html/attr "id" "workout-recent")
        (html/join
          (list
            (html/h2 "Recent")
            (if (null? rows)
                (html/p* (html/attr "class" "muted")
                  (html/text "No sessions yet."))
                (html/ul ""
                  (html/join (list/map workout/session-li rows)))))))))
  "Recent sessions.")

(define workout/home-main
  (lambda ()
    (html/div (html/attr "id" "workout-main")
      (html/join
        (list
          (workout/continue-card)
          (workout/start-bar)
          (workout/recent-list)))))
  "Home body.")

(define workout/exercise-li
  (lambda (row)
    (let ((id (workout/row-id row))
          (f (workout/row-fields row)))
      (let ((ex (workout/field f "exercise"))
            (w (workout/field f "weight"))
            (u (workout/field f "unit"))
            (r (workout/field f "reps"))
            (n (workout/field f "notes"))
            (sid (workout/field f "session_id")))
        (let ((load
                (if (blank? w) ""
                    (string-append " @ " w
                      (if (blank? u) "" (string-append " " u))))))
          (html/li (html/attr "class" "card")
            (html/join
              (list
                (html/div ""
                  (html/strong
                    (string-append ex " — " r " reps" load)))
                (if (blank? n) ""
                    (html/p* (html/attr "class" "muted") (html/text n)))
                (html/form "POST" "/workout/session"
                  (html/attr "style" "display:inline")
                  (html/join
                    (list
                      (html/hidden "action" "delete-exercise")
                      (html/hidden "id" sid)
                      (html/hidden "exercise_id" id)
                      (html/button (html/attr "type" "submit")
                        "Delete")))))))))))
  "One exercise fact.")

(define workout/add-exercise-form
  (lambda (session-id)
    (html/div (html/attr "class" "card")
      (html/join
        (list
          (html/h2 "Log exercise")
          (html/form "POST" "/workout/session" ""
            (html/join
              (list
                (html/hidden "action" "log-exercise")
                (html/hidden "id" (as-str session-id))
                (html/label (html/attr "for" "we-name") "Exercise")
                (html/text-input "exercise"
                  (html/attrs
                    (list
                      (html/attr "id" "we-name")
                      (html/attr "required" "required")
                      (html/attr "placeholder" "Swing / Snatch L / Pushup"))))
                (html/label (html/attr "for" "we-reps") "Reps (total)")
                (html/text-input "reps"
                  (html/attrs
                    (list
                      (html/attr "id" "we-reps")
                      (html/attr "required" "required")
                      (html/attr "inputmode" "numeric")
                      (html/attr "placeholder" "75"))))
                (html/label (html/attr "for" "we-w") "Weight")
                (html/text-input "weight"
                  (html/attrs
                    (list
                      (html/attr "id" "we-w")
                      (html/attr "inputmode" "decimal")
                      (html/attr "placeholder" "24"))))
                (html/label (html/attr "for" "we-u") "Unit")
                (html/text-input "unit"
                  (html/attrs
                    (list
                      (html/attr "id" "we-u")
                      (html/attr "placeholder" "kg"))))
                (html/label (html/attr "for" "we-n") "Notes")
                (html/text-input "notes"
                  (html/attrs
                    (list
                      (html/attr "id" "we-n")
                      (html/attr "placeholder" "optional"))))
                (html/button (html/attr "type" "submit") "Add"))))))))
  "Append exercise form.")

(define workout/session-actions
  (lambda (id state)
    (html/div (html/attr "class" "card")
      (html/join
        (list
          (html/h2 "Session")
          (if (string-eq (as-str state) "in_progress")
              (html/join
                (list
                  (html/form "POST" "/workout/session"
                    (html/attr "style" "display:inline")
                    (html/join
                      (list
                        (html/hidden "action" "complete")
                        (html/hidden "id" (as-str id))
                        (html/button (html/attr "type" "submit")
                          "Complete"))))
                  (html/text " ")
                  (html/form "POST" "/workout/session"
                    (html/attr "style" "display:inline")
                    (html/join
                      (list
                        (html/hidden "action" "abandon")
                        (html/hidden "id" (as-str id))
                        (html/button (html/attr "type" "submit")
                          "Abandon"))))))
              "")
          (html/form "POST" "/workout/session"
            (html/attrs
              (list
                (html/attr "style" "margin-top:0.75rem")
                (html/attr "onsubmit"
                  "return confirm('Delete session and its exercises?')")))
            (html/join
              (list
                (html/hidden "action" "delete-session")
                (html/hidden "id" (as-str id))
                (html/button (html/attr "type" "submit")
                  "Delete session"))))
          (html/p* "" (html/a "/workout" "← All workouts"))))))
  "Complete/abandon/delete controls.")

(define workout/session-main
  (lambda (id)
    (let ((f (workout/session id)))
      (if (null? f)
          (html/div (html/attr "id" "workout-session-main")
            (html/join
              (list
                (html/p "Session not found.")
                (html/p* "" (html/a "/workout" "← Back")))))
          (let ((exs (workout/exercises id))
                (st (workout/session-state f))
                (notes (workout/session-notes f)))
            (html/div (html/attr "id" "workout-session-main")
              (html/join
                (list
                  (html/h1 (html/text (workout/session-title f)))
                  (html/p* "" (workout/state-badge st))
                  (if (blank? notes) "" (html/p notes))
                  (workout/session-actions id st)
                  (if (string-eq st "abandoned")
                      ""
                      (workout/add-exercise-form id))
                  (html/h2 "Exercises")
                  (if (null? exs)
                      (html/p* (html/attr "class" "muted")
                        (html/text "No exercises logged."))
                      (html/ul ""
                        (html/join (list/map workout/exercise-li exs))))
                  (html/p* (html/attr "class" "muted")
                    (html/text
                      (string-append
                        "Total reps: "
                        (as-str (workout/session-total-reps id))))))))))))
  "Session detail panel.")

(define workout/log-form
  (lambda ()
    (html/div (html/attr "class" "card")
      (html/join
        (list
          (html/h2 "Quick log")
          (html/p* (html/attr "class" "muted")
            (html/text
              "Log a finished free workout (session + up to 3 exercises)."))
          (html/form "POST" "/workout/log" ""
            (html/join
              (list
                (html/hidden "action" "quick-log")
                (html/label (html/attr "for" "ql-date") "Date")
                (html/text-input "date"
                  (html/attrs
                    (list
                      (html/attr "id" "ql-date")
                      (html/attr "value" (workout/today))
                      (html/attr "placeholder" "YYYY-MM-DD"))))
                (html/label (html/attr "for" "ql-notes") "Session notes")
                (html/textarea "notes"
                  (html/attrs
                    (list
                      (html/attr "id" "ql-notes")
                      (html/attr "rows" "2")
                      (html/attr "placeholder" "optional")))
                  "")
                (html/h3 "Exercise 1")
                (html/label (html/attr "for" "ql-e1") "Name")
                (html/text-input "exercise1"
                  (html/attrs
                    (list
                      (html/attr "id" "ql-e1")
                      (html/attr "required" "required")
                      (html/attr "placeholder" "Swing"))))
                (html/label (html/attr "for" "ql-r1") "Reps")
                (html/text-input "reps1"
                  (html/attrs
                    (list
                      (html/attr "id" "ql-r1")
                      (html/attr "required" "required")
                      (html/attr "inputmode" "numeric"))))
                (html/label (html/attr "for" "ql-w1") "Weight")
                (html/text-input "weight1"
                  (html/attrs (list (html/attr "id" "ql-w1"))))
                (html/label (html/attr "for" "ql-u1") "Unit")
                (html/text-input "unit1"
                  (html/attrs
                    (list
                      (html/attr "id" "ql-u1")
                      (html/attr "placeholder" "kg"))))
                (html/h3 "Exercise 2 (optional)")
                (html/label (html/attr "for" "ql-e2") "Name")
                (html/text-input "exercise2"
                  (html/attrs (list (html/attr "id" "ql-e2"))))
                (html/label (html/attr "for" "ql-r2") "Reps")
                (html/text-input "reps2"
                  (html/attrs
                    (list
                      (html/attr "id" "ql-r2")
                      (html/attr "inputmode" "numeric"))))
                (html/label (html/attr "for" "ql-w2") "Weight")
                (html/text-input "weight2"
                  (html/attrs (list (html/attr "id" "ql-w2"))))
                (html/label (html/attr "for" "ql-u2") "Unit")
                (html/text-input "unit2"
                  (html/attrs
                    (list
                      (html/attr "id" "ql-u2")
                      (html/attr "placeholder" "kg"))))
                (html/h3 "Exercise 3 (optional)")
                (html/label (html/attr "for" "ql-e3") "Name")
                (html/text-input "exercise3"
                  (html/attrs (list (html/attr "id" "ql-e3"))))
                (html/label (html/attr "for" "ql-r3") "Reps")
                (html/text-input "reps3"
                  (html/attrs
                    (list
                      (html/attr "id" "ql-r3")
                      (html/attr "inputmode" "numeric"))))
                (html/label (html/attr "for" "ql-w3") "Weight")
                (html/text-input "weight3"
                  (html/attrs (list (html/attr "id" "ql-w3"))))
                (html/label (html/attr "for" "ql-u3") "Unit")
                (html/text-input "unit3"
                  (html/attrs
                    (list
                      (html/attr "id" "ql-u3")
                      (html/attr "placeholder" "kg"))))
                (html/button (html/attr "type" "submit") "Save workout"))))
          (html/p* "" (html/a "/workout" "← All workouts"))))))
  "Quick-log form.")

;; --- POST handlers ---

(define workout/handle-home-post
  (lambda ()
    (let ((action (as-str (request-param "action"))))
      (cond
        ((string-eq action "start")
         (let ((id (workout/session-create
                     (request-param "program")
                     (request-param "intensity"))))
           (redirect (string-append "/workout/session?id=" (as-str id)))))
        (else (redirect "/workout")))))
  "POST /workout.")

(define workout/handle-session-post
  (lambda ()
    (let ((action (as-str (request-param "action")))
          (id (as-str (request-param "id"))))
      (cond
        ((string-eq action "log-exercise")
         (seq
           (workout/exercise-log
             id
             (request-param "exercise")
             (request-param "weight")
             (request-param "unit")
             (request-param "reps")
             (request-param "notes")
             "")
           (redirect (string-append "/workout/session?id=" id))))
        ((string-eq action "delete-exercise")
         (seq
           (workout/exercise-delete (request-param "exercise_id"))
           (redirect (string-append "/workout/session?id=" id))))
        ((string-eq action "complete")
         (seq
           (workout/session-complete id (request-param "notes"))
           (redirect (string-append "/workout/session?id=" id))))
        ((string-eq action "abandon")
         (seq
           (workout/session-abandon id)
           (redirect (string-append "/workout/session?id=" id))))
        ((string-eq action "delete-session")
         (seq
           (workout/session-delete id)
           (redirect "/workout")))
        (else (redirect "/workout")))))
  "POST /workout/session.")

(define workout/maybe-log
  (lambda (sid name weight unit reps)
    (if (blank? (as-str name))
        ()
        (workout/exercise-log sid name weight unit reps "" "")))
  "Log exercise if name set.")

(define workout/handle-log-post
  (lambda ()
    (let ((action (as-str (request-param "action"))))
      (if (string-eq action "quick-log")
          (let ((sid (workout/session-create-complete
                       "free" ""
                       (request-param "date")
                       (request-param "notes"))))
            (seq
              (workout/maybe-log sid
                (request-param "exercise1")
                (request-param "weight1")
                (request-param "unit1")
                (request-param "reps1"))
              (workout/maybe-log sid
                (request-param "exercise2")
                (request-param "weight2")
                (request-param "unit2")
                (request-param "reps2"))
              (workout/maybe-log sid
                (request-param "exercise3")
                (request-param "weight3")
                (request-param "unit3")
                (request-param "reps3"))
              (redirect
                (string-append "/workout/session?id=" (as-str sid)))))
          (redirect "/workout/log"))))
  "POST /workout/log.")

;; --- pages ---

(define workout/home-page
  (lambda ()
    (if (string-eq (request-method) "POST")
        (workout/handle-home-post)
        (html/page
          (list
            (html/h1 "Workout")
            (html/p* (html/attr "class" "muted")
              (html/text
                "Training journal — free log now; structured programs later."))
            (workout/home-main)))))
  "GET/POST /workout.")

(define workout/session-page
  (lambda ()
    (if (string-eq (request-method) "POST")
        (workout/handle-session-post)
        (let ((id (as-str (request-param "id"))))
          (html/page
            (list
              (if (blank? id)
                  (html/join
                    (list
                      (html/h1 "Session")
                      (html/p "Missing id.")
                      (html/p* "" (html/a "/workout" "← Back"))))
                  (workout/session-main id)))))))
  "GET/POST /workout/session?id=.")

(define workout/log-page
  (lambda ()
    (if (string-eq (request-method) "POST")
        (workout/handle-log-post)
        (html/page
          (list
            (html/h1 "Quick log")
            (workout/log-form)))))
  "GET/POST /workout/log.")

(define workout/run-page
  (lambda ()
    (html/page
      (list
        (html/h1 "Run")
        (html/p* (html/attr "class" "muted")
          (html/text
            "Structured program runners (ksk/qd) are not shipped yet."))
        (html/p* "" (html/a "/workout" "← Workouts"))
        (html/p* "" (html/a "/workout/log" "Quick log instead")))))
  "Placeholder /workout/run.")

(route "/workout" "workout/home-page")
(route "/workout/session" "workout/session-page")
(route "/workout/log" "workout/log-page")
(route "/workout/run" "workout/run-page")
