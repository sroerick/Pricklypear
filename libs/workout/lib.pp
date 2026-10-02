;; package workout -- exported from live branch system/main
;; Sync: scripts/pp_sync.py pull|push

(define workout/add-exercise-form
  (lambda
    (session-id)
    (html/div
      (html/attr "class" "card")
      (html/join
        (list
          (html/h2 "Log exercise")
          (html/form
            "POST"
            "/workout/session"
            ""
            (html/join
              (list
                (html/hidden "action" "log-exercise")
                (html/hidden "id" (as-str session-id))
                (html/label (html/attr "for" "we-name") "Exercise")
                (html/text-input
                  "exercise"
                  (html/attrs
                    (list
                      (html/attr "id" "we-name")
                      (html/attr "required" "required")
                      (html/attr "placeholder" "Swing / Snatch L / Pushup"))))
                (html/label (html/attr "for" "we-reps") "Reps (total)")
                (html/text-input
                  "reps"
                  (html/attrs
                    (list
                      (html/attr "id" "we-reps")
                      (html/attr "required" "required")
                      (html/attr "inputmode" "numeric")
                      (html/attr "placeholder" "75"))))
                (html/label (html/attr "for" "we-w") "Weight")
                (html/text-input
                  "weight"
                  (html/attrs
                    (list
                      (html/attr "id" "we-w")
                      (html/attr "inputmode" "decimal")
                      (html/attr "placeholder" "24"))))
                (html/label (html/attr "for" "we-u") "Unit")
                (html/text-input
                  "unit"
                  (html/attrs
                    (list
                      (html/attr "id" "we-u")
                      (html/attr "placeholder" "kg"))))
                (html/label (html/attr "for" "we-n") "Notes")
                (html/text-input
                  "notes"
                  (html/attrs
                    (list
                      (html/attr "id" "we-n")
                      (html/attr "placeholder" "optional"))))
                (html/button (html/attr "type" "submit") "Add"))))))))
  "HTML card form to log an exercise into a session.")

(define workout/as-int
  (lambda
    (x)
    (let ((s (as-str x))) (if (blank? s) 0 (parse-int s))))
  "Parse x as an integer, 0 when blank.")

(define workout/cal-delete-ksk1-from
  (lambda
    (from-day)
    (list/foldl
      (lambda
        (acc row)
        (let
          ((f (workout/row-fields row))
            (rid (workout/row-id row)))
          (if
            (workout/str-lt? (workout/field f "day") from-day)
            acc
            (if
              (string-eq
                (nth (string-split (workout/field f "title") " ") 0)
                "KSK1")
              (seq (delete (quote calendar-event) rid) acc)
              acc))))
      ()
      (rows (quote calendar-event) 500)))
  "Delete KSK1 calendar events dated from-day or later.")

(define workout/capture-facts
  (lambda
    (program session-id)
    (let*
      ((p (as-str program))
        (doc (workout/parse-session-doc session-id)))
      (cond
        ((null? doc) ())
        ((string-eq p "run5k")
          (workout/run5k-capture-facts session-id doc))
        ((string-eq p "sct") (workout/sct-facts session-id doc))
        (else ()))))
  "Project the prescription doc into facts (runs before gates).")

(define workout/car-or-null
  (lambda (xs) (if (null? xs) () (car xs)))
  "First element or ().")

(define workout/char-rank
  (lambda
    (c)
    (let
      ((s (as-str c)))
      (cond
        ((string-eq s "0") 0)
        ((string-eq s "1") 1)
        ((string-eq s "2") 2)
        ((string-eq s "3") 3)
        ((string-eq s "4") 4)
        ((string-eq s "5") 5)
        ((string-eq s "6") 6)
        ((string-eq s "7") 7)
        ((string-eq s "8") 8)
        ((string-eq s "9") 9)
        ((string-eq s "-") 10)
        (else 50))))
  "Rank of a single char for custom string ordering.")

(define workout/chart-data-js
  (lambda
    ()
    (let*
      ((all (workout/session-list ""))
        (ksk (list/filter workout/ksk1? all))
        (points (list/map workout/chart-point-js ksk)))
      (string-append "[" (workout/join-csv points) "]"))))

(define workout/chart-page
  (lambda
    ()
    (html/page
      (list
        (html/h1 "Workout Progress")
        (html/p*
          (html/attr "class" "muted")
          "KSK1 Snatch - reps per hand per session")
        (html/a "/workout" "<- Back to Workout")
        (html/div
          (html/attr "style" "max-width:760px;margin:1em 0")
          (html/tag "canvas" (html/attr "id" "workoutChart") ""))
        (html/div
          (html/attr "style" "margin:1em 0;font-size:14px")
          "<span style=\"color:#3b82f6\">&#9632;</span> Day 1 &nbsp; <span style=\"color:#10b981\">&#9632;</span> Day 2 &nbsp; <span style=\"color:#f59e0b\">&#9632;</span> Day 3")
        (html/tag
          "script"
          (html/attr
            "src"
            "https://cdn.jsdelivr.net/npm/chart.js@4.4.1")
          "")
        (html/tag
          "script"
          ""
          (string-append
            "const sessions="
            (workout/chart-data-js)
            ";"
            "sessions.sort(function(a,b){return a.date.localeCompare(b.date);});"
            "const dc={1:'#3b82f6',2:'#10b981',3:'#f59e0b'};"
            "const labels=sessions.map(function(s){const d=new Date(s.date);return d.toLocaleDateString('en-US',{month:'short',day:'numeric'});});"
            "const colors=sessions.map(function(s){return dc[s.day]||'#999';});"
            "new Chart(document.getElementById('workoutChart'),{"
            "type:'bar',"
            "data:{labels:labels,datasets:[{label:'Reps per hand',data:sessions.map(function(s){return s.reps;}),backgroundColor:colors,borderRadius:4}]},"
            "options:{responsive:true,plugins:{legend:{display:false},tooltip:{callbacks:{afterLabel:function(c){const s=sessions[c.dataIndex];return ['Day '+s.day+' ('+s.pattern+')',s.notes];}}}},scales:{y:{beginAtZero:true,title:{display:true,text:'Reps per hand'}},x:{title:{display:true,text:'Session date'}}}}"
            "});"))))))

(define workout/chart-point-js
  (lambda
    (row)
    (let*
      ((f (workout/row-fields row))
        (doc (dict-get f "doc"))
        (prog (dict-get doc "progress"))
        (params (dict-get doc "params"))
        (date (as-str (dict-get f "date")))
        (reps
          (as-str (workout/as-int (dict-get prog "reps_l"))))
        (phase
          (as-str (workout/as-int (dict-get params "phase"))))
        (day (as-str (workout/as-int (dict-get params "day"))))
        (pattern
          (workout/js-escape (dict-get params "pattern")))
        (notes (workout/js-escape (dict-get f "notes"))))
      (string-append
        "{date:\""
        date
        "\","
        "reps:"
        reps
        ","
        "phase:"
        phase
        ","
        "day:"
        day
        ","
        "pattern:\""
        pattern
        "\","
        "notes:\""
        notes
        "\"}"))))

(define workout/col-lower
  (lambda
    (col)
    (let*
      ((s (as-str col))
        (pair (string-split s "x"))
        (set-part (if (null? pair) "0" (nth pair 0)))
        (rep-part (if (null? pair) "0" (nth pair 1)))
        (set-tok (string-split set-part "-"))
        (sets (workout/as-int (nth set-tok 0)))
        (rep-clean
          (if
            (string-eq
              "s"
              (substring
                rep-part
                (- (string-length rep-part) 1)
                (string-length rep-part)))
            (substring rep-part 0 (- (string-length rep-part) 1))
            rep-part))
        (rep-tok (string-split rep-clean "-"))
        (reps (workout/as-int (nth rep-tok 0))))
      (* sets reps)))
  "Lower-bound total of a column like 2-3x5-10 or 3x30s.")

(define workout/completed-count
  (lambda
    (program)
    (list/foldl
      (lambda
        (acc row)
        (if
          (string-eq
            (workout/field (workout/row-fields row) "state")
            "complete")
          (+ acc 1)
          acc))
      0
      (find-rows
        (quote workout-session)
        "program"
        (as-str program)
        500)))
  "Number of completed sessions for a program.")

(define workout/continue-card
  (lambda
    ()
    (let
      ((id (workout/session-in-progress ())))
      (if
        (null? id)
        ""
        (let
          ((f (workout/session id)))
          (if
            (null? f)
            ""
            (let
              ((prog (workout/session-program f))
                (run-href (string-append "/workout/run?id=" id))
                (sess-href (string-append "/workout/session?id=" id)))
              (html/div
                (html/attr "class" "card")
                (html/join
                  (list
                    (html/h2 "Continue")
                    (html/p
                      (string-append
                        (workout/session-title f)
                        " is in progress."))
                    (html/p*
                      ""
                      (html/join
                        (list
                          (if
                            (workout/ksk-program? prog)
                            (html/a run-href "Resume run")
                            "")
                          (if (workout/ksk-program? prog) (html/text " · ") "")
                          (html/a sess-href "Open session")))))))))))))
  "HTML card to resume any in-progress session.")

(define workout/date-ge?
  (lambda (a b) (string>=? (as-str a) (as-str b)))
  "True when date a is on/after date b.")

(define workout/date-in-range?
  (lambda
    (day from to)
    (let
      ((d (as-str day)))
      (if
        (blank? d)
        #f
        (let
          ((ok-from
              (if (blank? (as-str from)) #t (workout/date-ge? d from)))
            (ok-to
              (if (blank? (as-str to)) #t (workout/date-le? d to))))
          (if ok-from ok-to #f)))))
  "True when day is within the optional from/to range.")

(define workout/date-le?
  (lambda (a b) (string<=? (as-str a) (as-str b)))
  "True when date a is on/before date b.")

(define workout/doc-params
  (lambda (doc) (dict-get doc "params"))
  "Params sub-dict of a session doc.")

(define workout/doc-progress
  (lambda (doc) (dict-get doc "progress"))
  "Progress sub-dict of a session doc.")

(define workout/empty-doc
  (lambda
    (program intensity)
    (dict-set*
      "{}"
      (list
        "program"
        (as-str program)
        "intensity"
        (as-str intensity)
        "params"
        "{}"
        "progress"
        "{}"
        "blocks"
        "[]")))
  "Fresh empty session doc for a program and intensity.")

(define workout/exercise-delete
  (lambda
    (id)
    (delete (quote workout-exercise) (as-str id)))
  "Delete a workout-exercise row by id.")

(define workout/exercise-fields
  (lambda
    (session-id
      exercise
      weight
      unit
      reps
      notes
      detail)
    (dict-set*
      "{}"
      (list
        "session_id"
        (as-str session-id)
        "exercise"
        (workout/normalize-exercise exercise)
        "weight"
        (as-str weight)
        "unit"
        (workout/normalize-unit unit)
        "reps"
        (as-str reps)
        "notes"
        (as-str notes)
        "detail"
        (as-str detail))))
  "Dict of fields for a workout-exercise row.")

(define workout/exercise-li
  (lambda
    (row)
    (let
      ((id (workout/row-id row)) (f (workout/row-fields row)))
      (let
        ((ex (workout/field f "exercise"))
          (w (workout/field f "weight"))
          (u (workout/field f "unit"))
          (r (workout/field f "reps"))
          (n (workout/field f "notes"))
          (sid (workout/field f "session_id")))
        (let
          ((load
            (if
              (blank? w)
              ""
              (string-append
                " @ "
                w
                (if (blank? u) "" (string-append " " u))))))
          (html/li
            (html/attr "class" "card")
            (html/join
              (list
                (html/div
                  ""
                  (html/strong (string-append ex " — " r " reps" load)))
                (if
                  (blank? n)
                  ""
                  (html/p* (html/attr "class" "muted") (html/text n)))
                (html/form
                  "POST"
                  "/workout/session"
                  (html/attr "style" "display:inline")
                  (html/join
                    (list
                      (html/hidden "action" "delete-exercise")
                      (html/hidden "id" sid)
                      (html/hidden "exercise_id" id)
                      (html/button (html/attr "type" "submit") "Delete")))))))))))
  "HTML list item for an exercise row, with delete form.")

(define workout/exercise-log
  (lambda
    (session-id
      exercise
      weight
      unit
      reps
      notes
      detail)
    (let
      ((name (workout/normalize-exercise exercise)))
      (if
        (blank? name)
        ()
        (create
          (quote workout-exercise)
          (workout/exercise-fields
            session-id
            name
            weight
            unit
            (if (blank? (as-str reps)) "0" (as-str reps))
            (if (null? notes) "" notes)
            (if (null? detail) "" detail))))))
  "Create a workout-exercise row; () when the name is blank.")

(define workout/exercise-names
  (lambda
    (limit)
    (let
      ((lim (if (null? limit) 100 (workout/as-int limit))))
      (list/foldl
        (lambda
          (acc row)
          (let
            ((n (workout/field (workout/row-fields row) "exercise")))
            (if
              (blank? n)
              acc
              (if (workout/name-seen? acc n) acc (cons n acc)))))
        ()
        (rows (quote workout-exercise) lim))))
  "Distinct normalized exercise names, up to limit.")

(define workout/exercises
  (lambda
    (session-id)
    (let
      ((sid (as-str session-id)))
      (list/filter
        (lambda
          (row)
          (string-eq
            (workout/field (workout/row-fields row) "session_id")
            sid))
        (rows (quote workout-exercise) 500))))
  "Exercise rows belonging to a session.")

(define workout/field
  (lambda
    (fields key)
    (let
      ((v (dict-get fields (as-str key))))
      (if (null? v) "" (as-str v))))
  "Field value from a fields dict, \"\" when absent.")

(define workout/gate-assert
  (lambda
    (program key passed session_id notes)
    (create
      (quote workout-gate)
      (workout/gate-fields
        program
        key
        passed
        session_id
        ""
        notes)))
  "Record an asserted gate in the ledger. Returns the new row id.")

(define workout/gate-eval
  (lambda
    (program session-id)
    (cond
      ((string-eq (as-str program) "toy")
        (workout/toy-gates session-id (workout/state "toy")))
      ((string-eq (as-str program) "ksf")
        (workout/ksf-gates session-id (workout/state "ksf")))
      ((string-eq (as-str program) "run5k")
        (workout/run5k-gates session-id (workout/state "run5k")))
      ((string-eq (as-str program) "sct")
        (workout/sct-gates session-id (workout/state "sct")))
      ((string-eq (as-str program) "stretch")
        (workout/stretch-gates
          session-id
          (workout/state "stretch")))
      (else ())))
  "Evaluate program gates after a completed session.")

(define workout/gate-fields
  (lambda
    (program key passed session_id date notes)
    (dict-set*
      "{}"
      (list
        "program"
        (as-str program)
        "key"
        (as-str key)
        "passed"
        (as-str passed)
        "session_id"
        (if (null? session_id) "" (as-str session_id))
        "date"
        (if (null? date) (workout/today) (as-str date))
        "notes"
        (if (null? notes) "" (as-str notes)))))
  "Dict of fields for a workout-gate row.")

(define workout/gate-status
  (lambda
    (program)
    (dict-set*
      "{}"
      (list
        "program"
        (as-str program)
        "state"
        (workout/state program)
        "gates"
        (workout/gates-summary program))))
  "Cursor + gate truth for an agent or the UI (flat dict).")

(define workout/gates
  (lambda
    (program)
    (find-rows
      (quote workout-gate)
      "program"
      (as-str program)
      100))
  "Gate rows for a program (max 100).")

(define workout/gates-summary
  (lambda
    (program)
    (list/foldl
      (lambda
        (acc row)
        (let
          ((f (workout/row-fields row)))
          (let
            ((entry
              (string-append
                (workout/field f "key")
                "="
                (workout/field f "passed")
                (if
                  (blank? (workout/field f "date"))
                  ""
                  (string-append " " (workout/field f "date"))))))
            (if (blank? acc) entry (string-append entry "; " acc)))))
      ""
      (workout/gates program)))
  "Gate ledger rendered as one string (key=passed @date).")

(define workout/handle-home-post
  (lambda
    ()
    (let
      ((action (as-str (request-param "action"))))
      (cond
        ((string-eq action "start")
          (let
            ((id
              (workout/session-create
                (request-param "program")
                (request-param "intensity"))))
            (redirect
              (string-append "/workout/session?id=" (as-str id)))))
        ((string-eq action "start-ksk")
          (let
            ((id
              (workout/ksk-start
                (request-param "program")
                (request-param "phase")
                (request-param "day")
                (request-param "week")
                (request-param "load")
                (request-param "unit")
                (request-param "rm_total"))))
            (if
              (null? id)
              (redirect "/workout")
              (redirect
                (string-append "/workout/run?id=" (as-str id))))))
        (else (redirect "/workout")))))
  "Route the home page POST to start or start-ksk.")

(define workout/handle-ksf-run-post
  (lambda
    (id action)
    (let
      ((sets (workout/as-int (workout/ksf-sets id))))
      (cond
        ((string-eq action "ksf-step")
          (let
            ((next (+ sets 1)))
            (seq
              (workout/ksf-progress-put id next)
              (redirect
                (string-append "/workout/run?id=" (as-str id))))))
        ((string-eq action "ksf-end")
          (seq
            (workout/ksf-progress-put id sets)
            (workout/session-complete id "ended by runner")))
        (else
          (redirect
            (string-append "/workout/run?id=" (as-str id)))))))
  "KSF runner POST routing.")

(define workout/handle-log-post
  (lambda
    ()
    (let
      ((action (as-str (request-param "action"))))
      (cond
        ((string-eq action "quick-log")
          (let
            ((sid
              (workout/session-create-complete
                "free"
                ""
                (request-param "date")
                (request-param "notes"))))
            (seq
              (workout/maybe-log
                sid
                (request-param "exercise1")
                (request-param "weight1")
                (request-param "unit1")
                (request-param "reps1"))
              (workout/maybe-log
                sid
                (request-param "exercise2")
                (request-param "weight2")
                (request-param "unit2")
                (request-param "reps2"))
              (workout/maybe-log
                sid
                (request-param "exercise3")
                (request-param "weight3")
                (request-param "unit3")
                (request-param "reps3"))
              (redirect
                (string-append "/workout/session?id=" (as-str sid))))))
        ((string-eq action "quick-walk")
          (let
            ((sid
              (workout/walk-create
                (request-param "date")
                (request-param "distance")
                (request-param "unit")
                (request-param "duration")
                (request-param "location")
                (request-param "notes"))))
            (redirect
              (string-append "/workout/session?id=" (as-str sid)))))
        (else (redirect "/workout/log")))))
  "Route the quick-log POST to quick-log or quick-walk.")

(define workout/handle-run-post
  (lambda
    ()
    (let
      ((action (as-str (request-param "action")))
        (id (as-str (request-param "id"))))
      (cond
        ((string-eq action "ksk-step")
          (seq
            (workout/ksk-step id)
            (redirect (string-append "/workout/run?id=" id))))
        ((string-eq action "ksk-capture")
          (seq
            (workout/ksk-capture id)
            (redirect (string-append "/workout/session?id=" id))))
        ((string-eq action "abandon")
          (seq
            (workout/session-abandon id)
            (redirect (string-append "/workout/session?id=" id))))
        (else (redirect "/workout")))))
  "Route the run page POST to ksk-step/capture/abandon.")

(define workout/handle-session-post
  (lambda
    ()
    (let
      ((action (as-str (request-param "action")))
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
            (if
              (workout/ksk-program?
                (workout/session-program (workout/session id)))
              (workout/ksk-capture id)
              (workout/session-complete id (request-param "notes")))
            (redirect (string-append "/workout/session?id=" id))))
        ((string-eq action "abandon")
          (seq
            (workout/session-abandon id)
            (redirect (string-append "/workout/session?id=" id))))
        ((string-eq action "ksk-step")
          (seq
            (workout/ksk-step id)
            (redirect (string-append "/workout/run?id=" id))))
        ((string-eq action "ksk-capture")
          (seq
            (workout/ksk-capture id)
            (redirect (string-append "/workout/session?id=" id))))
        ((string-eq action "delete-session")
          (seq (workout/session-delete id) (redirect "/workout")))
        (else (redirect "/workout")))))
  "Route the session page POST to the matching action.")

(define workout/history
  (lambda
    (from-date to-date limit)
    (let
      ((lim (if (null? limit) 50 (workout/as-int limit))))
      (list/filter
        (lambda
          (row)
          (workout/date-in-range?
            (workout/session-date (workout/row-fields row))
            from-date
            to-date))
        (workout/session-list lim))))
  "Sessions whose date falls in the range, newest first.")

(define workout/home-main
  (lambda
    ()
    (html/div
      (html/attr "id" "workout-main")
      (html/join
        (list
          (workout/continue-card)
          (workout/start-bar)
          (workout/recent-list)))))
  "HTML main content of the workout home page.")

(define workout/home-page
  (lambda
    ()
    (if
      (string-eq (request-method) "POST")
      (workout/handle-home-post)
      (html/page
        (list
          (html/h1 "Workout")
          (html/p*
            (html/attr "class" "muted")
            (html/text
              "Training journal — free log + King-Sized Killer (snatch)."))
          (workout/home-main)))))
  "Home page, handling POST or rendering the HTML.")

(define workout/insert-session-desc
  (lambda
    (sorted row)
    (if
      (null? sorted)
      (list row)
      (let
        ((d (workout/session-date (workout/row-fields row)))
          (hd
            (workout/session-date (workout/row-fields (car sorted)))))
        (if
          (workout/date-ge? d hd)
          (cons row sorted)
          (cons
            (car sorted)
            (workout/insert-session-desc (cdr sorted) row))))))
  "Insert a session row into a date-descending sorted list.")

(define workout/join-csv
  (lambda
    (xs)
    (if
      (null? xs)
      ""
      (if
        (null? (cdr xs))
        (car xs)
        (string-append (car xs) "," (workout/join-csv (cdr xs)))))))

(define workout/join-nums
  (lambda
    (xs)
    (list/foldl
      (lambda
        (acc x)
        (if
          (blank? acc)
          (as-str x)
          (string-append acc " " (as-str x))))
      ""
      xs))
  "Join a list of numbers into a space-separated string.")

(define workout/js-escape
  (lambda
    (s)
    (string-replace
      (string-replace (as-str s) "\\" "\\\\")
      "\""
      "\\\"")))

(define workout/ksf-bell-for
  (lambda
    (st deload)
    (let
      ((b (workout/field st "bell")))
      (if
        (blank? b)
        b
        (if
          (string-eq (as-str deload) "true")
          (as-str
            (-
              (workout/as-int b)
              (if (string-eq "lb" (workout/field st "unit")) 9 4)))
          b))))
  "Bell: one size lighter on deloads.")

(define workout/ksf-cleared?
  (lambda
    (session-id)
    (>= (workout/ksf-duration-min session-id) 30))
  "Clears at >= 30 min.")

(define workout/ksf-configure
  (lambda
    (bell unit)
    (workout/state-put
      "ksf"
      (dict-set*
        "{}"
        (list
          "phase"
          "A"
          "pattern"
          "CJ"
          "bell"
          (as-str bell)
          "unit"
          (workout/normalize-unit unit)
          "done"
          "0"
          "stalls"
          "0"
          "since_deload"
          "0"
          "deload_left"
          "0"))
      "ksf configured after RM test"))
  "Initial ksf state after the LC RM test (weaker arm 6-12RM).")

(define workout/ksf-doc
  (lambda
    (phase pattern day bell unit deload)
    (dict-set*
      "{}"
      (list
        "program"
        "ksf"
        "phase"
        (as-str phase)
        "pattern"
        (as-str pattern)
        "day"
        (as-str day)
        "params"
        (dict-set*
          "{}"
          (list
            "bell"
            (as-str bell)
            "unit"
            (as-str unit)
            "cadence_sec"
            "30"
            "target_min"
            "30"
            "deload"
            (if deload "true" "false")))
        "progress"
        (dict-set* "{}" (list "sets_done" "0" "ended" ""))
        "blocks"
        "[]")))
  "Session doc. Day: LCCJ (A) / C / J (B).")

(define workout/ksf-doc-elapsed-min
  (lambda
    (session-id)
    (let
      ((doc (workout/parse-session-doc session-id)))
      (if
        (null? doc)
        0
        (workout/as-int
          (dict-get (workout/doc-progress doc) "elapsed_min")))))
  "Progress.elapsed_min from the session doc, 0 when absent.")

(define workout/ksf-duration-min
  (lambda
    (session-id)
    (let
      ((doc-min (workout/ksf-doc-elapsed-min session-id)))
      (if
        (> doc-min 0)
        doc-min
        (list/foldl
          (lambda
            (acc row)
            (let
              ((f (workout/row-fields row)))
              (if
                (string-eq (workout/field f "exercise") "Duration")
                (+ acc (workout/as-int (workout/field f "reps")))
                acc)))
          0
          (workout/exercises session-id)))))
  "Session duration in minutes: the session doc's progress.elapsed_min
     when present and nonzero (the normal KSF logger writes it —
     ksf-progress-put), else the legacy Duration exercise-row sum.

     OBITUARY (2026-10-02, BUG ksf-cleared?): the old body ONLY summed
     Duration rows. The KSF run logger never emits a Duration row, so
     every session scored short and the pattern never advanced (stalls
     incremented forever). Verified live 2026-09-27: no session had a
     Duration row. The doc was already the authority; read it first.")

(define workout/ksf-duration-sum
  (lambda
    (session-id)
    (list/foldl
      (lambda
        (acc row)
        (let
          ((f (workout/row-fields row)))
          (if
            (string-eq (workout/field f "exercise") "Duration")
            (+ acc (workout/as-int (workout/field f "reps")))
            acc)))
      0
      (workout/exercises session-id)))
  "Duration in minutes from any Duration exercise rows (legacy convention).")

(define workout/ksf-gates
  (lambda
    (session-id state)
    (let*
      ((st
          (workout/or-dict state (workout/ksf-state-or-default)))
        (doc (workout/parse-session-doc session-id))
        (was-deload
          (string-eq
            "true"
            (workout/pget (workout/doc-params doc) "deload")))
        (cleared (workout/ksf-cleared? session-id))
        (phase (workout/field st "phase"))
        (pattern (workout/field st "pattern"))
        (idx (workout/ksf-index pattern (workout/ksf-ladder)))
        (done (+ 1 (workout/as-int (workout/field st "done")))))
      (let*
        ((cleared-next
            (cond
              (was-deload pattern)
              ((string-eq "A" phase)
                (cond
                  ((not cleared) pattern)
                  ((< (+ idx 1) (workout/len (workout/ksf-ladder)))
                    (nth (workout/ksf-ladder) (+ idx 1)))
                  (else "B:P3")))
              ((string-eq "B" phase)
                (if
                  cleared
                  (let
                    ((n (workout/ksf-section-reps pattern)))
                    (if
                      (< n 5)
                      (string-append "P" (as-str (+ n 1)))
                      pattern))
                  pattern))
              (else pattern)))
          (flip-to-b
            (and
              (not cleared)
              (not (string-eq "B" phase))
              (>=
                (+ 1 (workout/as-int (workout/field st "stalls")))
                6)))
          (next-phase
            (if
              (string-eq "A" phase)
              (cond
                (flip-to-b "B")
                ((string-eq "B:P3" cleared-next) "B")
                (else "A"))
              phase))
          (next-pattern
            (cond
              (flip-to-b "P3")
              ((string-eq "B:P3" cleared-next) "P3")
              (else cleared-next)))
          (next-stalls
            (if
              cleared
              0
              (+ 1 (workout/as-int (workout/field st "stalls")))))
          (since-d
            (workout/as-int (workout/field st "since_deload")))
          (deload-left
            (workout/as-int (workout/field st "deload_left")))
          (sched
            (if
              was-deload
              (list since-d deload-left)
              (if
                (>= (+ since-d 1) 8)
                (list 0 2)
                (list (+ since-d 1) deload-left))))
          (note
            (cond
              (flip-to-b "stalled: retest LC RM, switch to Plan B")
              ((string-eq "B:P3" cleared-next)
                "ladder complete: Plan A yields to Plan B")
              ((and cleared (not (string-eq pattern next-pattern)))
                "cleared: 30 min, pattern advanced")
              (cleared "cleared: 30 min")
              (else "short session: talk test ended the day")))
          (state2
            (dict-set*
              "{}"
              (list
                "phase"
                next-phase
                "pattern"
                next-pattern
                "bell"
                (workout/field st "bell")
                "unit"
                (workout/field st "unit")
                "done"
                (as-str done)
                "stalls"
                (as-str next-stalls)
                "since_deload"
                (as-str (nth sched 0))
                "deload_left"
                (as-str (nth sched 1))))))
        (seq (workout/state-put "ksf" state2 note) state2))))
  "ksf gates: clear (>=30min) advances Plan A; 6 shorts flip to B.")

(define workout/ksf-index
  (lambda
    (needle haystack)
    (workout/ksf-index-from needle haystack 0))
  "0-based index, or -1.")

(define workout/ksf-index-from
  (lambda
    (needle haystack i)
    (cond
      ((null? haystack) -1)
      ((string-eq (as-str (car haystack)) (as-str needle)) i)
      (else
        (workout/ksf-index-from needle (cdr haystack) (+ i 1))))))

(define workout/ksf-ladder
  (lambda () (list "CJ" "CJC" "CJCJ" "CJCJC" "CJCJCJ"))
  "Plan A rep patterns per 30s set, in advance order.")

(define workout/ksf-progress-put
  (lambda
    (id sets)
    (let
      ((doc (workout/parse-session-doc id)))
      (unless
        (null? doc)
        (let*
          ((old-progress (workout/doc-progress doc))
            (new-progress
              (dict-set*
                old-progress
                (list
                  "sets_done"
                  (as-str sets)
                  "elapsed_min"
                  (as-str (div (* (workout/as-int sets) 30) 60)))))
            (doc2 (dict-set* doc (list "progress" new-progress))))
          (workout/ksk-save-doc id doc2)))))
  "Persist sets_done into doc.progress.")

(define workout/ksf-run-main
  (lambda
    (id)
    (let
      ((f (workout/session id)))
      (if
        (null? f)
        (html/div "" (html/p "Session not found."))
        (let*
          ((doc (workout/parse-session-doc id))
            (day (dict-get doc "day"))
            (pattern (dict-get doc "pattern"))
            (params (workout/doc-params doc))
            (bell (workout/pget params "bell"))
            (unit (workout/pget params "unit"))
            (deload (workout/pget params "deload"))
            (sets
              (workout/as-int
                (dict-get (workout/doc-progress doc) "sets_done")))
            (target 60)
            (arm (if (= 0 (mod sets 2)) "L" "R")))
          (html/div
            (html/attr "class" "ksk-run")
            (html/join
              (list
                (html/h2 "KSF run")
                (html/p*
                  ""
                  (html/text
                    (string-append
                      "Plan "
                      (as-str (dict-get doc "phase"))
                      " - pattern "
                      (as-str pattern)
                      " - "
                      bell
                      unit
                      (if
                        (string-eq "true" (as-str deload))
                        " - DELOAD (one bell lighter)"
                        ""))))
                (html/p*
                  ""
                  (html/text
                    (string-append
                      "Weak arm (L) on the minute, strong (R) 30s in. Sets done: "
                      (as-str sets)
                      " / "
                      (as-str target))))
                (html/div
                  (html/attrs
                    (list
                      (html/attr "id" "ksf-timer")
                      (html/attr "class" "ksk-timer")
                      (html/attr "data-sid" (as-str id))))
                  "")
                (html/p*
                  ""
                  (html/text (string-append "This set: " arm " arm")))
                (html/form
                  "POST"
                  "/workout/run"
                  (html/attr "style" "display:inline")
                  (html/join
                    (list
                      (html/hidden "action" "ksf-step")
                      (html/hidden "id" (as-str id))
                      (html/button
                        (html/attr "type" "submit")
                        "Set done (+1)"))))
                (html/text " ")
                (html/form
                  "POST"
                  "/workout/run"
                  (html/attr "style" "display:inline")
                  (html/join
                    (list
                      (html/hidden "action" "ksf-end")
                      (html/hidden "id" (as-str id))
                      (html/button
                        (html/attr "type" "submit")
                        "End - talk test done"))))
                (workout/timer-script 0.5)
                (html/p*
                  ""
                  (html/a
                    (string-append "/workout/session?id=" (as-str id))
                    "Back to session")))))))))
  "Live KSF runner.")

(define workout/ksf-section-reps
  (lambda
    (pattern)
    (if
      (blank? pattern)
      -1
      (if
        (string-eq "P" (substring (as-str pattern) 0 1))
        (parse-int (substring (as-str pattern) 1 2))
        -1)))
  "P3/P4/P5 -> 3/4/5; -1 otherwise.")

(define workout/ksf-sets
  (lambda
    (id)
    (dict-get
      (workout/doc-progress (workout/parse-session-doc id))
      "sets_done"))
  "Current sets_done (0 when unset).")

(define workout/ksf-state-or-default
  (lambda
    ()
    (workout/or-dict
      (workout/state "ksf")
      (dict-set*
        "{}"
        (list
          "phase"
          "A"
          "pattern"
          "CJ"
          "bell"
          ""
          "unit"
          "kg"
          "done"
          "0"
          "stalls"
          "0"
          "since_deload"
          "0"
          "deload_left"
          "0"))))
  "ksf state dict, or fresh default.")

(define workout/ksf-suggest
  (lambda
    (date)
    (let*
      ((st (workout/ksf-state-or-default))
        (parts (string-split (as-str date) "-"))
        (wd
          (if
            (null? parts)
            -1
            (date-weekday
              (parse-int (nth parts 0))
              (parse-int (nth parts 1))
              (parse-int (nth parts 2)))))
        (on-day (workout/member? wd (workout/ksf-weekdays)))
        (deload
          (not (string-eq "0" (workout/field st "deload_left"))))
        (phase (if on-day (workout/field st "phase") "OFF"))
        (day
          (cond
            ((not on-day) "OFF")
            ((string-eq "A" phase) "LCCJ")
            ((= wd 5) "C")
            (else "J"))))
      (workout/ksf-doc
        phase
        (workout/field st "pattern")
        day
        (workout/ksf-bell-for st deload)
        (workout/field st "unit")
        deload)))
  "ksf doc from state. Pure.")

(define workout/ksf-weekdays
  (lambda () (list 5 0))
  "2-day variant: Friday and Sunday.")

(define workout/ksk-build-doc
  (lambda
    (program
      phase
      day
      week
      load
      unit
      pattern
      rm-total
      pct
      per-hand)
    (dict-set*
      "{}"
      (list
        "program"
        (as-str program)
        "exercise"
        "Snatch"
        "params"
        (dict-set*
          "{}"
          (list
            "phase"
            (as-str phase)
            "day"
            (as-str day)
            "week"
            (as-str week)
            "time_cap_min"
            (as-str (workout/ksk-time-cap program))
            "load"
            (as-str load)
            "unit"
            (workout/normalize-unit unit)
            "pattern"
            (as-str pattern)
            "kind"
            (workout/pattern-kind pattern)
            "rm_total"
            (as-str rm-total)
            "pct"
            (as-str pct)
            "per_hand"
            (as-str per-hand)
            "hand_policy"
            "per-rung-both"))
        "progress"
        (dict-set*
          "{}"
          (list
            "ladder"
            "0"
            "rung_index"
            "0"
            "side"
            "L"
            "reps_l"
            "0"
            "reps_r"
            "0"
            "sets_done"
            "0"
            "started"
            ""
            "elapsed_sec"
            "0"))
        "blocks"
        "[]")))
  "Build the KSK session document dict from plan params.")

(define workout/ksk-capture
  (lambda
    (session-id)
    (let
      ((doc (workout/parse-session-doc session-id)))
      (seq
        (unless
          (null? doc)
          (workout/ksk-upsert-facts session-id doc))
        (workout/session-complete session-id ""))))
  "Write KSK facts then complete the session.")

(define workout/ksk-current-target
  (lambda
    (doc)
    (let*
      ((params (workout/doc-params doc))
        (prog (workout/doc-progress doc))
        (kind (workout/pget params "kind"))
        (pat (workout/pget params "pattern"))
        (ri (workout/as-int (workout/pget prog "rung_index"))))
      (if
        (string-eq kind "sets")
        (let
          ((ph (workout/pget params "per_hand")))
          (if
            (blank? ph)
            (workout/pattern-set-size pat)
            (workout/as-int ph)))
        (let*
          ((rungs (workout/pattern-rungs pat))
            (n (workout/len rungs))
            (i (if (< ri 0) 0 (if (>= ri n) 0 ri))))
          (if (null? rungs) 0 (workout/as-int (nth rungs i)))))))
  "Current per-hand snatch target from a session doc.")

(define workout/ksk-label
  (lambda
    (doc)
    (let
      ((params (workout/doc-params doc))
        (prog (as-str (dict-get doc "program"))))
      (let
        ((phase (workout/pget params "phase"))
          (day (workout/pget params "day"))
          (week (workout/pget params "week"))
          (pat (workout/pget params "pattern"))
          (load (workout/pget params "load"))
          (unit (workout/pget params "unit"))
          (cap (workout/pget params "time_cap_min")))
        (string-append
          (as-str prog)
          (if (blank? phase) "" (string-append " P" phase))
          (if (blank? week) "" (string-append " W" week))
          " D"
          day
          " · "
          pat
          " · "
          load
          (if (blank? unit) "" (string-append " " unit))
          " · "
          cap
          "m"))))
  "Human-readable title for a KSK session doc.")

(define workout/ksk-program?
  (lambda
    (p)
    (let
      ((s (as-str p)))
      (if
        (string-eq s "ksk1")
        #t
        (if (string-eq s "ksk2") #t (string-eq s "ksk3")))))
  "True when p names a KSK program (ksk1/2/3).")

(define workout/ksk-replace-side
  (lambda
    (sid name load unit reps)
    (seq
      (list/map
        (lambda
          (row)
          (let
            ((f (workout/row-fields row)))
            (if
              (string-eq (workout/field f "exercise") (as-str name))
              (workout/exercise-delete (workout/row-id row))
              ())))
        (workout/exercises sid))
      (unless
        (or
          (blank? (as-str reps))
          (string-eq (as-str reps) "0"))
        (workout/exercise-log
          sid
          name
          load
          unit
          reps
          ""
          ""))))
  "Replace the exercise rows for one side with the given totals.")

(define workout/ksk-save-doc
  (lambda
    (id doc)
    (let
      ((cur (workout/session id)))
      (if
        (null? cur)
        ()
        (workout/session-update
          id
          (workout/session-fields
            (workout/session-date cur)
            (workout/session-program cur)
            (workout/session-intensity cur)
            (workout/session-state cur)
            (workout/session-notes cur)
            doc
            (workout/field cur "duration_sec")
            (workout/field cur "author"))))))
  "Persist doc back onto the session, preserving other fields.")

(define workout/ksk-start
  (lambda
    (program
      phase
      day
      week
      load
      unit
      rm-total)
    (let*
      ((p (as-str program))
        (doc
          (cond
            ((string-eq p "ksk1")
              (workout/ksk1-plan phase day load unit))
            ((string-eq p "ksk2")
              (workout/ksk2-plan phase day load unit))
            ((string-eq p "ksk3")
              (workout/ksk3-plan week day load unit rm-total))
            (else ())))
        (inten
          (if
            (string-eq p "ksk3")
            (string-append "w" (as-str week) "d" (as-str day))
            (string-append "p" (as-str phase) "d" (as-str day)))))
      (if
        (null? doc)
        ()
        (workout/session-create-with-doc p inten doc))))
  "Create an in-progress KSK session; () when the plan is unknown.")

(define workout/ksk-step
  (lambda
    (session-id)
    (let
      ((doc (workout/parse-session-doc session-id)))
      (if
        (null? doc)
        ()
        (let*
          ((params (workout/doc-params doc))
            (prog (workout/doc-progress doc))
            (target (workout/ksk-current-target doc))
            (side (workout/pget prog "side"))
            (kind (workout/pget params "kind"))
            (pat (workout/pget params "pattern"))
            (rl (workout/as-int (workout/pget prog "reps_l")))
            (rr (workout/as-int (workout/pget prog "reps_r")))
            (ri (workout/as-int (workout/pget prog "rung_index")))
            (lad (workout/as-int (workout/pget prog "ladder")))
            (sd (workout/as-int (workout/pget prog "sets_done")))
            (advance-rung (string-eq side "R"))
            (is-sets (string-eq kind "sets"))
            (n-rungs (workout/len (workout/pattern-rungs pat)))
            (rl2 (if (string-eq side "L") (+ rl target) rl))
            (rr2 (if advance-rung (+ rr target) rr))
            (next-side (if advance-rung "L" "R"))
            (ri2
              (if
                is-sets
                ri
                (if
                  advance-rung
                  (if (>= (+ ri 1) n-rungs) 0 (+ ri 1))
                  ri)))
            (lad2
              (if
                is-sets
                lad
                (if
                  advance-rung
                  (if (>= (+ ri 1) n-rungs) (+ lad 1) lad)
                  lad)))
            (sd2 (if is-sets (if advance-rung (+ sd 1) sd) sd))
            (prog2
              (dict-set*
                "{}"
                (list
                  "ladder"
                  (as-str lad2)
                  "rung_index"
                  (as-str ri2)
                  "side"
                  next-side
                  "reps_l"
                  (as-str rl2)
                  "reps_r"
                  (as-str rr2)
                  "sets_done"
                  (as-str sd2)
                  "started"
                  (workout/pget prog "started")
                  "elapsed_sec"
                  (workout/pget prog "elapsed_sec"))))
            (doc2 (dict-set doc "progress" prog2)))
          (seq
            (workout/ksk-save-doc session-id doc2)
            (workout/ksk-upsert-facts session-id doc2)
            doc2)))))
  "Advance one side of a KSK run and persist the new progress.")

(define workout/ksk-time-cap
  (lambda
    (program)
    (if (string-eq (as-str program) "ksk3") 15 20))
  "Time cap in minutes for a KSK program (15 for ksk3, else 20).")

(define workout/ksk-upsert-facts
  (lambda
    (session-id doc)
    (let
      ((params (workout/doc-params doc))
        (prog (workout/doc-progress doc)))
      (let
        ((load (workout/pget params "load"))
          (unit (workout/pget params "unit"))
          (rl (workout/pget prog "reps_l"))
          (rr (workout/pget prog "reps_r"))
          (sid (as-str session-id)))
        (seq
          (workout/ksk-replace-side sid "Snatch L" load unit rl)
          (workout/ksk-replace-side sid "Snatch R" load unit rr)))))
  "Write the current L/R snatch totals as exercise rows.")

(define workout/ksk1-next
  (lambda
    (phase week day)
    (cond
      ((< day 3) (list phase week (+ day 1)))
      ((< week 3) (list phase (+ week 1) 1))
      ((< phase 3) (list (+ phase 1) 1 1))
      (else (list))))
  "Next (phase week day) tuple, or () when the program completes.")

(define workout/ksk1-pattern
  (lambda
    (phase day)
    (let
      ((p (workout/as-int phase)) (d (workout/as-int day)))
      (cond
        ((= p 1)
          (cond
            ((= d 1) "1,2,3,4,5")
            ((= d 2) "1,2,3")
            (else "1,2,3,4")))
        ((= p 2)
          (cond
            ((= d 1) "3,6,9")
            ((= d 2) "3,6")
            (else "3,6,9,12")))
        (else
          (cond ((= d 1) "S:9") ((= d 2) "S:15") (else "S:12"))))))
  "Pattern string for a KSK1 phase/day.")

(define workout/ksk1-plan
  (lambda
    (phase day load unit)
    (let
      ((p (workout/as-int phase))
        (d (workout/as-int day))
        (pat (workout/ksk1-pattern phase day)))
      (workout/ksk-build-doc
        "ksk1"
        p
        d
        ""
        load
        unit
        pat
        ""
        ""
        "")))
  "Build a KSK1 session doc from phase/day/load/unit.")

(define workout/ksk1-replan
  (lambda
    (start-date weekdays load unit skip notes)
    (seq
      (workout/cal-delete-ksk1-from (workout/today))
      (workout/ksk1-schedule
        start-date
        weekdays
        load
        unit
        skip
        notes
        true)))
  "Replace the KSK1 plan: deletes KSK1 calendar events dated today or later, then regenerates the full schedule from start-date. Past events and logged sessions are untouched.")

(define workout/ksk1-sched-from
  (lambda
    (date
      weekdays
      load
      unit
      skip
      phase
      week
      day
      notes
      create?
      acc)
    (if
      (> phase 3)
      (if create? (count acc) acc)
      (let*
        ((pat (workout/ksk1-pattern phase day))
          (title
            (string-append
              "KSK1 P"
              (as-str phase)
              " W"
              (as-str week)
              " D"
              (as-str day)
              " · "
              pat
              " · "
              (as-str load)
              (as-str unit)
              " · "
              (as-str (workout/ksk-time-cap "ksk1"))
              "m"))
          (nxt (workout/ksk1-next phase week day)))
        (if
          (> skip 0)
          (if
            (null? nxt)
            (if create? (count acc) acc)
            (workout/ksk1-sched-from
              date
              weekdays
              load
              unit
              (- skip 1)
              (nth nxt 0)
              (nth nxt 1)
              (nth nxt 2)
              notes
              create?
              acc))
          (let*
            ((acc2 (append acc (list (list date title))))
              (next-date
                (workout/next-training-day
                  (date-add-days date 1)
                  weekdays)))
            (seq
              (if create? (calendar/create title date "" notes) ())
              (if
                (null? nxt)
                (if create? (count acc2) acc2)
                (workout/ksk1-sched-from
                  next-date
                  weekdays
                  load
                  unit
                  0
                  (nth nxt 0)
                  (nth nxt 1)
                  (nth nxt 2)
                  notes
                  create?
                  acc2))))))))
  "Accumulate KSK1 scheduled dates, creating calendar rows when asked.")

(define workout/ksk1-schedule
  (lambda
    (start-date
      weekdays
      load
      unit
      skip
      notes
      create?)
    (workout/ksk1-sched-from
      (workout/next-training-day start-date weekdays)
      weekdays
      load
      unit
      skip
      1
      1
      1
      notes
      create?
      (list)))
  "Plan the full KSK1 schedule of dates and titles.")

(define workout/ksk1-title
  (lambda
    (phase week day load unit)
    (string-append
      "KSK1 P"
      (as-str phase)
      " W"
      (as-str week)
      " D"
      (as-str day)
      " · "
      (workout/ksk1-pattern phase day)
      " · "
      (as-str load)
      (as-str unit)
      " · "
      (as-str (workout/ksk-time-cap "ksk1"))
      "m")))

(define workout/ksk1?
  (lambda
    (row)
    (string-eq
      (workout/field (workout/row-fields row) "program")
      "ksk1")))

(define workout/ksk2-plan
  (lambda
    (phase day load unit)
    (let
      ((p (workout/as-int phase))
        (d (workout/as-int day))
        (n (workout/ksk2-set-size phase day)))
      (workout/ksk-build-doc
        "ksk2"
        p
        d
        ""
        load
        unit
        (string-append "S:" (as-str n))
        ""
        ""
        "")))
  "Build a KSK2 session doc from phase/day/load/unit.")

(define workout/ksk2-set-size
  (lambda
    (phase day)
    (let
      ((p (workout/as-int phase)) (d (workout/as-int day)))
      (cond
        ((= p 1) (cond ((= d 1) 10) ((= d 2) 20) (else 15)))
        ((= p 2) (cond ((= d 1) 12) ((= d 2) 24) (else 18)))
        (else (cond ((= d 1) 15) ((= d 2) 30) (else 20))))))
  "Reps per set for a KSK2 phase/day.")

(define workout/ksk3-pct
  (lambda
    (week day)
    (let
      ((w (workout/as-int week)) (d (workout/as-int day)))
      (cond
        ((= d 1) (if (or (= w 1) (= w 3)) 50 60))
        ((= d 2) (if (or (= w 1) (= w 3)) 65 75))
        (else (if (or (= w 1) (= w 3)) 70 80)))))
  "Percentage of RM for a KSK3 week/day.")

(define workout/ksk3-per-hand
  (lambda
    (rm-total pct)
    (workout/max0
      (floor
        (div
          (* (workout/as-int rm-total) (workout/as-int pct))
          200))))
  "Per-hand snatch target given RM total and percentage.")

(define workout/ksk3-plan
  (lambda
    (week day load unit rm-total)
    (let
      ((w (workout/as-int week))
        (d (workout/as-int day))
        (rm (workout/as-int rm-total))
        (pct (workout/ksk3-pct week day)))
      (let
        ((ph (workout/ksk3-per-hand rm pct)))
        (workout/ksk-build-doc
          "ksk3"
          ""
          d
          w
          load
          unit
          (string-append "S:" (as-str ph))
          rm
          pct
          ph))))
  "Build a KSK3 session doc from week/day/load/unit/rm.")

(define workout/len
  (lambda
    (xs)
    (list/foldl (lambda (acc _) (+ acc 1)) 0 xs))
  "Number of elements in a list.")

(define workout/log-form
  (lambda
    ()
    (html/div
      (html/attr "class" "card")
      (html/join
        (list
          (html/h2 "Quick log")
          (html/p*
            (html/attr "class" "muted")
            (html/text
              "Log a finished free workout (session + up to 3 exercises)."))
          (html/form
            "POST"
            "/workout/log"
            ""
            (html/join
              (list
                (html/hidden "action" "quick-log")
                (html/label (html/attr "for" "ql-date") "Date")
                (html/text-input
                  "date"
                  (html/attrs
                    (list
                      (html/attr "id" "ql-date")
                      (html/attr "value" (workout/today))
                      (html/attr "placeholder" "YYYY-MM-DD"))))
                (html/label
                  (html/attr "for" "ql-notes")
                  "Session notes")
                (html/textarea
                  "notes"
                  (html/attrs
                    (list
                      (html/attr "id" "ql-notes")
                      (html/attr "rows" "2")
                      (html/attr "placeholder" "optional")))
                  "")
                (html/h3 "Exercise 1")
                (html/label (html/attr "for" "ql-e1") "Name")
                (html/text-input
                  "exercise1"
                  (html/attrs
                    (list
                      (html/attr "id" "ql-e1")
                      (html/attr "required" "required")
                      (html/attr "placeholder" "Swing"))))
                (html/label (html/attr "for" "ql-r1") "Reps")
                (html/text-input
                  "reps1"
                  (html/attrs
                    (list
                      (html/attr "id" "ql-r1")
                      (html/attr "required" "required")
                      (html/attr "inputmode" "numeric"))))
                (html/label (html/attr "for" "ql-w1") "Weight")
                (html/text-input
                  "weight1"
                  (html/attrs (list (html/attr "id" "ql-w1"))))
                (html/label (html/attr "for" "ql-u1") "Unit")
                (html/text-input
                  "unit1"
                  (html/attrs
                    (list
                      (html/attr "id" "ql-u1")
                      (html/attr "placeholder" "kg"))))
                (html/h3 "Exercise 2 (optional)")
                (html/label (html/attr "for" "ql-e2") "Name")
                (html/text-input
                  "exercise2"
                  (html/attrs (list (html/attr "id" "ql-e2"))))
                (html/label (html/attr "for" "ql-r2") "Reps")
                (html/text-input
                  "reps2"
                  (html/attrs
                    (list
                      (html/attr "id" "ql-r2")
                      (html/attr "inputmode" "numeric"))))
                (html/label (html/attr "for" "ql-w2") "Weight")
                (html/text-input
                  "weight2"
                  (html/attrs (list (html/attr "id" "ql-w2"))))
                (html/label (html/attr "for" "ql-u2") "Unit")
                (html/text-input
                  "unit2"
                  (html/attrs
                    (list
                      (html/attr "id" "ql-u2")
                      (html/attr "placeholder" "kg"))))
                (html/h3 "Exercise 3 (optional)")
                (html/label (html/attr "for" "ql-e3") "Name")
                (html/text-input
                  "exercise3"
                  (html/attrs (list (html/attr "id" "ql-e3"))))
                (html/label (html/attr "for" "ql-r3") "Reps")
                (html/text-input
                  "reps3"
                  (html/attrs
                    (list
                      (html/attr "id" "ql-r3")
                      (html/attr "inputmode" "numeric"))))
                (html/label (html/attr "for" "ql-w3") "Weight")
                (html/text-input
                  "weight3"
                  (html/attrs (list (html/attr "id" "ql-w3"))))
                (html/label (html/attr "for" "ql-u3") "Unit")
                (html/text-input
                  "unit3"
                  (html/attrs
                    (list
                      (html/attr "id" "ql-u3")
                      (html/attr "placeholder" "kg"))))
                (html/button (html/attr "type" "submit") "Save workout"))))
          (html/p* "" (html/a "/workout" "← All workouts"))))))
  "HTML card form for the quick-log of a finished workout.")

(define workout/log-page
  (lambda
    ()
    (if
      (string-eq (request-method) "POST")
      (workout/handle-log-post)
      (html/page
        (list
          (html/h1 "Quick log")
          (workout/log-form)
          (workout/walk-form)))))
  "Quick-log page, handling POST or rendering the form.")

(define workout/log/ksk-write
  (lambda
    (date
      program
      intensity
      doc
      load
      unit
      total-l
      total-r
      notes
      rungs-l-str
      rungs-r-str)
    (let*
      ((tl (workout/as-int total-l))
        (tr (workout/as-int total-r))
        (rsl (as-str rungs-l-str))
        (rsr (as-str rungs-r-str))
        (prog0 (workout/doc-progress doc))
        (prog
          (dict-set*
            "{}"
            (list
              "ladder"
              (workout/pget prog0 "ladder")
              "rung_index"
              (workout/pget prog0 "rung_index")
              "side"
              "L"
              "reps_l"
              (as-str tl)
              "reps_r"
              (as-str tr)
              "sets_done"
              "0"
              "started"
              ""
              "elapsed_sec"
              ""
              "logged"
              "true"
              "rungs_l"
              rsl
              "rungs_r"
              rsr)))
        (n
          (if
            (blank? (as-str notes))
            (if
              (string-eq (as-str tl) (as-str tr))
              (string-append
                (as-str program)
                " log: "
                rsl
                " @ "
                (as-str load)
                (as-str unit)
                " ("
                (as-str tl)
                "/hand)")
              (string-append
                (as-str program)
                " log L:"
                rsl
                " R:"
                rsr
                " @ "
                (as-str load)
                (as-str unit)
                " ("
                (as-str tl)
                "/"
                (as-str tr)
                ")"))
            (as-str notes)))
        (doc2 (dict-set doc "progress" prog))
        (sid
          (create
            "workout-session"
            (workout/session-fields
              (if
                (blank? (as-str date))
                (workout/today)
                (as-str date))
              (as-str program)
              (as-str intensity)
              "complete"
              n
              doc2
              ""
              (as-str (whoami))))))
      (seq
        (unless
          (< tl 1)
          (workout/exercise-log
            sid
            "Snatch L"
            load
            unit
            tl
            ""
            rsl))
        (unless
          (< tr 1)
          (workout/exercise-log
            sid
            "Snatch R"
            load
            unit
            tr
            ""
            rsr))
        sid)))
  "Create a completed KSK session from logged totals and rungs.")

(define workout/log/ksk1
  (lambda
    (date
      phase
      day
      load
      unit
      rungs
      notes)
    (let
      ((doc (workout/ksk1-plan phase day load unit))
        (total (workout/sum rungs))
        (rs (workout/join-nums rungs))
        (inten
          (string-append "p" (as-str phase) "d" (as-str day)))
        (n (if (null? notes) "" notes)))
      (workout/log/ksk-write
        date
        "ksk1"
        inten
        doc
        load
        unit
        total
        total
        n
        rs
        rs)))
  "Log a finished KSK1 session from a rungs list.")

(define workout/log/ksk1-sides
  (lambda
    (date
      phase
      day
      load
      unit
      rungs-l
      rungs-r
      notes)
    (let
      ((doc (workout/ksk1-plan phase day load unit))
        (tl (workout/sum rungs-l))
        (tr (workout/sum rungs-r))
        (rsl (workout/join-nums rungs-l))
        (rsr (workout/join-nums rungs-r))
        (inten
          (string-append "p" (as-str phase) "d" (as-str day)))
        (n (if (null? notes) "" notes)))
      (workout/log/ksk-write
        date
        "ksk1"
        inten
        doc
        load
        unit
        tl
        tr
        n
        rsl
        rsr)))
  "Log a finished KSK1 session with separate L/R rung lists.")

(define workout/log/ksk2
  (lambda
    (date
      phase
      day
      load
      unit
      sets
      notes)
    (let
      ((doc (workout/ksk2-plan phase day load unit))
        (total (workout/sum sets))
        (rs (workout/join-nums sets))
        (inten
          (string-append "p" (as-str phase) "d" (as-str day)))
        (n (if (null? notes) "" notes)))
      (workout/log/ksk-write
        date
        "ksk2"
        inten
        doc
        load
        unit
        total
        total
        n
        rs
        rs)))
  "Log a finished KSK2 session from a sets list.")

(define workout/log/ksk3
  (lambda
    (date
      week
      day
      load
      unit
      rm-total
      sets
      notes)
    (let
      ((doc (workout/ksk3-plan week day load unit rm-total))
        (total (workout/sum sets))
        (rs (workout/join-nums sets))
        (inten
          (string-append "w" (as-str week) "d" (as-str day)))
        (n (if (null? notes) "" notes)))
      (workout/log/ksk-write
        date
        "ksk3"
        inten
        doc
        load
        unit
        total
        total
        n
        rs
        rs)))
  "Log a finished KSK3 session from a sets list.")

(define workout/max0
  (lambda (n) (if (< n 1) 1 n))
  "At least 1 (bottom floor for positive counts).")

(define workout/maybe-log
  (lambda
    (sid name weight unit reps)
    (if
      (blank? (as-str name))
      ()
      (workout/exercise-log
        sid
        name
        weight
        unit
        reps
        ""
        "")))
  "Log an exercise row, or () when the name is blank.")

(define workout/member?
  (lambda
    (x xs)
    (if
      (null? xs)
      #f
      (if (= (car xs) x) #t (workout/member? x (cdr xs)))))
  "True when x is in list xs (numeric equality).")

(define workout/name-seen?
  (lambda
    (lst name)
    (list/foldl
      (lambda
        (acc x)
        (if acc acc (string-eq (as-str x) (as-str name))))
      #f
      lst))
  "True when an exercise name is already in lst.")

(define workout/next-training-day
  (lambda
    (date weekdays)
    (let*
      ((parts (string-split (as-str date) "-"))
        (y (parse-int (nth parts 0)))
        (m (parse-int (nth parts 1)))
        (d (parse-int (nth parts 2)))
        (wd (date-weekday y m d)))
      (if
        (workout/member? wd weekdays)
        (as-str date)
        (workout/next-training-day
          (date-add-days (as-str date) 1)
          weekdays))))
  "Next date (from date) that falls on a training weekday.")

(define workout/normalize-exercise
  (lambda (name) (string-trim (as-str name)))
  "Trimmed exercise name.")

(define workout/normalize-unit
  (lambda
    (u)
    (let
      ((s (as-str u)))
      (cond
        ((blank? s) "")
        ((string-eq s "KG") "kg")
        ((string-eq s "Kg") "kg")
        ((string-eq s "LB") "lb")
        ((string-eq s "Lb") "lb")
        ((string-eq s "lbs") "lb")
        ((string-eq s "LBS") "lb")
        (else s))))
  "Normalize a weight unit string to kg or lb, else as-is.")

(define workout/or-dict
  (lambda
    (maybe-dict fallback)
    (if (null? maybe-dict) fallback maybe-dict))
  "maybe-dict when non-null, else fallback.")

(define workout/parse-session-doc
  (lambda
    (id)
    (let
      ((f (workout/session id)))
      (if
        (null? f)
        ()
        (let
          ((raw (workout/session-doc f)))
          (if (blank? raw) () raw)))))
  "Parsed doc dict for a session, or ().")

(define workout/pattern-kind
  (lambda
    (pat)
    (let
      ((s (as-str pat)))
      (if
        (string-eq
          (substring s 0 (if (< (string-length s) 2) 0 2))
          "S:")
        "sets"
        "ladder")))
  "Kind of a pattern string: sets or ladder.")

(define workout/pattern-rungs
  (lambda
    (pat)
    (let
      ((s (as-str pat)))
      (if
        (string-eq (workout/pattern-kind s) "sets")
        (list)
        (string-split s ","))))
  "Rung list of a ladder pattern, () for a sets pattern.")

(define workout/pattern-set-size
  (lambda
    (pat)
    (let
      ((s (as-str pat)))
      (if
        (string-eq (workout/pattern-kind s) "sets")
        (workout/as-int (substring s 2 (- (string-length s) 2)))
        0)))
  "Set size of a sets pattern, 0 for a ladder pattern.")

(define workout/pget
  (lambda
    (dict key)
    (if
      (null? dict)
      ""
      (let
        ((v (dict-get dict (as-str key))))
        (if (null? v) "" (as-str v)))))
  "dict-get as a string, \"\" when absent.")

(define workout/program-advance
  (lambda
    (doc)
    (cond
      ((null? doc) ())
      ((blank? (workout/pget (workout/doc-params doc) "week"))
        ())
      (else
        (let*
          ((params (workout/doc-params doc))
            (prog (as-str (dict-get doc "program")))
            (phase (workout/pget params "phase"))
            (week (workout/pget params "week"))
            (day (workout/pget params "day"))
            (r (workout/program-row)))
          (if
            (null? r)
            ()
            (let
              ((f (workout/row-fields r)))
              (cond
                ((string-eq (workout/field f "done") "1") ())
                ((not (string-eq (workout/field f "program") prog)) ())
                ((not (string-eq (workout/field f "phase") phase)) ())
                ((not (string-eq (workout/field f "week") week)) ())
                ((not (string-eq (workout/field f "day") day)) ())
                (else
                  (workout/program-apply-next
                    r
                    (workout/program-next-state f)))))))))))

(define workout/program-apply-next
  (lambda
    (r nxt)
    (let
      ((f (workout/row-fields r)) (id (workout/row-id r)))
      (if
        (null? nxt)
        (workout/program-write
          id
          (workout/field f "program")
          ""
          ""
          ""
          (workout/field f "load")
          (workout/field f "unit")
          "1")
        (workout/program-write
          id
          (workout/field f "program")
          (nth nxt 0)
          (nth nxt 1)
          (nth nxt 2)
          (workout/field f "load")
          (workout/field f "unit")
          "0")))))

(define workout/program-doc
  (lambda
    (f)
    (workout/ksk-build-doc
      "ksk1"
      (workout/field f "phase")
      (workout/field f "day")
      (workout/field f "week")
      (workout/field f "load")
      (workout/field f "unit")
      (workout/ksk1-pattern
        (workout/field f "phase")
        (workout/field f "day"))
      ""
      ""
      "")))

(define workout/program-fields
  (lambda
    (program
      phase
      week
      day
      load
      unit
      done)
    (dict-set*
      "{}"
      (list
        "program"
        (as-str program)
        "phase"
        (as-str phase)
        "week"
        (as-str week)
        "day"
        (as-str day)
        "load"
        (as-str load)
        "unit"
        (as-str unit)
        "done"
        (as-str done)
        "author"
        (as-str (whoami))))))

(define workout/program-get
  (lambda
    ()
    (let
      ((r (workout/program-row)))
      (if (null? r) () (workout/row-fields r)))))

(define workout/program-intensity
  (lambda
    (f)
    (string-append
      "p"
      (workout/field f "phase")
      "d"
      (workout/field f "day"))))

(define workout/program-next-state
  (lambda
    (f)
    (if
      (string-eq (workout/field f "program") "ksk1")
      (workout/ksk1-next
        (workout/as-int (workout/field f "phase"))
        (workout/as-int (workout/field f "week"))
        (workout/as-int (workout/field f "day")))
      ())))

(define workout/program-row
  (lambda
    ()
    (let
      ((rs
        (find-rows
          (quote workout-program)
          "author"
          (as-str (whoami))
          1)))
      (if (null? rs) () (car rs)))))

(define workout/program-set
  (lambda
    (program phase week day load unit)
    (let
      ((r (workout/program-row)))
      (workout/program-write
        (if (null? r) "" (workout/row-id r))
        program
        phase
        week
        day
        load
        unit
        "0"))))

(define workout/program-skip
  (lambda
    ()
    (let
      ((r (workout/program-row)))
      (if
        (null? r)
        ()
        (if
          (string-eq
            (workout/field (workout/row-fields r) "done")
            "1")
          ()
          (workout/program-apply-next
            r
            (workout/program-next-state (workout/row-fields r))))))))

(define workout/program-title
  (lambda
    (f)
    (if
      (null? f)
      ""
      (if
        (string-eq (workout/field f "program") "ksk1")
        (workout/ksk1-title
          (workout/field f "phase")
          (workout/field f "week")
          (workout/field f "day")
          (workout/field f "load")
          (workout/field f "unit"))
        (workout/field f "program")))))

(define workout/program-write
  (lambda
    (id
      program
      phase
      week
      day
      load
      unit
      done)
    (let
      ((f
        (workout/program-fields
          program
          phase
          week
          day
          load
          unit
          done)))
      (if
        (blank? (as-str id))
        (create (quote workout-program) f)
        (update (quote workout-program) (as-str id) f)))))

(define workout/programs
  (lambda () (list "free" "ksk1" "ksk2" "ksk3"))
  "Supported workout program names.")

(define workout/recent-list
  (lambda
    ()
    (let
      ((rows (workout/sessions-sorted)))
      (html/div
        (html/attr "id" "workout-recent")
        (html/join
          (list
            (html/h2 "Recent")
            (if
              (null? rows)
              (html/p*
                (html/attr "class" "muted")
                (html/text "No sessions yet."))
              (html/ul
                ""
                (html/join (list/map workout/session-li rows))))
            (workout/walk-form))))))
  "HTML list of the most recent sessions plus walk form.")

(define workout/replace-fact
  (lambda
    (sid name unit reps)
    (seq
      (list/map
        (lambda
          (row)
          (let
            ((f (workout/row-fields row)))
            (if
              (string-eq (workout/field f "exercise") (as-str name))
              (workout/exercise-delete (workout/row-id row))
              ())))
        (workout/exercises sid))
      (unless
        (or
          (blank? (as-str reps))
          (string-eq (as-str reps) "0"))
        (workout/exercise-log
          sid
          name
          ""
          unit
          reps
          ""
          ""))))
  "Upsert one exercise fact (replace by session+exercise).")

(define workout/row-fields
  (lambda (row) (cdr row))
  "Fields portion of a data row.")

(define workout/row-id
  (lambda (row) (as-str (car row)))
  "Id string of a data row.")

(define workout/run-main
  (lambda
    (id)
    (let
      ((f (workout/session id)))
      (if
        (null? f)
        (html/div
          ""
          (html/join
            (list
              (html/p "Session not found.")
              (html/p* "" (html/a "/workout" "← Back")))))
        (let
          ((doc (workout/parse-session-doc id))
            (st (workout/session-state f))
            (prog-name (workout/session-program f)))
          (if
            (not (workout/ksk-program? prog-name))
            (html/div
              ""
              (html/join
                (list
                  (html/p "Not a KSK session.")
                  (html/p*
                    ""
                    (html/a
                      (string-append "/workout/session?id=" id)
                      "Open session")))))
            (let
              ((params (workout/doc-params doc))
                (prog (workout/doc-progress doc))
                (target (workout/ksk-current-target doc))
                (side (workout/pget (workout/doc-progress doc) "side"))
                (cap
                  (workout/pget (workout/doc-params doc) "time_cap_min"))
                (kind (workout/pget (workout/doc-params doc) "kind"))
                (pat (workout/pget (workout/doc-params doc) "pattern"))
                (rl (workout/pget (workout/doc-progress doc) "reps_l"))
                (rr (workout/pget (workout/doc-progress doc) "reps_r"))
                (lad (workout/pget (workout/doc-progress doc) "ladder"))
                (sd
                  (workout/pget (workout/doc-progress doc) "sets_done"))
                (ri
                  (workout/pget (workout/doc-progress doc) "rung_index")))
              (html/div
                (html/attr "class" "ksk-run")
                (html/join
                  (list
                    (workout/run-style)
                    (html/h1 (html/text (workout/ksk-label doc)))
                    (html/p*
                      (html/attr "class" "muted")
                      (html/text (string-append "state: " st)))
                    (html/div
                      (html/attrs
                        (list
                          (html/attr "id" "ksk-timer")
                          (html/attr "class" "ksk-timer")
                          (html/attr "data-sid" id)))
                      (html/text "--:--"))
                    (workout/timer-script cap)
                    (html/p*
                      (html/attr "class" "ksk-side")
                      (html/text
                        (string-append
                          "Hand: "
                          side
                          (if
                            (string-eq kind "ladder")
                            (string-append
                              " · rung #"
                              (as-str (+ (workout/as-int ri) 1))
                              " · ladder "
                              lad)
                            (string-append " · sets done " sd)))))
                    (html/p*
                      (html/attr "class" "ksk-target")
                      (html/text
                        (string-append (as-str target) " snatches (" side ")")))
                    (html/p*
                      (html/attr "class" "muted")
                      (html/text
                        (string-append "pattern " pat " · per-rung both hands")))
                    (if
                      (string-eq st "in_progress")
                      (html/join
                        (list
                          (html/form
                            "POST"
                            "/workout/run"
                            ""
                            (html/join
                              (list
                                (html/hidden "action" "ksk-step")
                                (html/hidden "id" id)
                                (html/button
                                  (html/attr "type" "submit")
                                  (string-append "Done " (as-str target) " " side)))))
                          (html/form
                            "POST"
                            "/workout/run"
                            ""
                            (html/join
                              (list
                                (html/hidden "action" "ksk-capture")
                                (html/hidden "id" id)
                                (html/button
                                  (html/attr "type" "submit")
                                  "Finish session"))))
                          (html/form
                            "POST"
                            "/workout/run"
                            (html/attr "style" "display:inline")
                            (html/join
                              (list
                                (html/hidden "action" "abandon")
                                (html/hidden "id" id)
                                (html/button (html/attr "type" "submit") "Abandon"))))))
                      "")
                    (html/p*
                      (html/attr "class" "ksk-totals")
                      (html/text
                        (string-append
                          "Totals — L: "
                          rl
                          " · R: "
                          rr
                          " · all: "
                          (as-str (+ (workout/as-int rl) (workout/as-int rr))))))
                    (html/p*
                      ""
                      (html/join
                        (list
                          (html/a
                            (string-append "/workout/session?id=" id)
                            "Session detail")
                          (html/text " · ")
                          (html/a "/workout" "All workouts")))))))))))))
  "Main HTML of the KSK run UI for a session.")

(define workout/run-page
  (lambda
    ()
    (if
      (string-eq (request-method) "POST")
      (workout/handle-run-post)
      (let
        ((id (as-str (request-param "id"))))
        (html/page
          (list
            (if
              (blank? id)
              (html/join
                (list
                  (html/h1 "Run")
                  (html/p "Missing session id.")
                  (html/p* "" (html/a "/workout" "← Back"))))
              (workout/run-main id)))))))
  "Run page, handling POST or rendering the run UI.")

(define workout/run-style
  (lambda
    ()
    (html/tag
      "style"
      ""
      (html/join
        (list
          ".ksk-run .ksk-timer{font-size:2.5rem;font-weight:700;letter-spacing:.05em}"
          ".ksk-run .ksk-target{font-size:2rem;font-weight:700;margin:.5rem 0}"
          ".ksk-run .ksk-side{font-size:1.4rem}"
          ".ksk-run form button{font-size:1.2rem;padding:.75rem 1.25rem;margin:.35rem .35rem .35rem 0}"
          ".ksk-run .ksk-totals{font-size:1.1rem;margin-top:.75rem}"))))
  "Inline CSS for the KSK run page.")

(define workout/run5k-a-goal
  (lambda
    (week)
    (workout/run5k-ref
      (list
        "30/90/6"
        "30/90/6"
        "45/75/6"
        "45/75/6"
        "60/60/5"
        "60/60/5"
        "90/30/4"
        "90/30/4"
        "120/30/4"
        "120/30/4"
        "180/60/3"
        "180/60/3"
        "180/30/4"
        "180/30/4"
        "240/45/3"
        "240/45/3"
        "300/45/3"
        "300/45/3")
      week))
  "Fri ratio session: jog/walk/sets.")

(define workout/run5k-b-goal
  (lambda
    (week)
    (workout/run5k-ref
      (list
        "30/90/8"
        "30/90/8"
        "45/75/8"
        "45/75/8"
        "60/60/9"
        "60/60/9"
        "90/30/7"
        "90/30/7"
        "120/60/5"
        "120/60/5"
        "120/45/7"
        "120/45/7"
        "180/60/6"
        "180/60/6"
        "180/45/7"
        "180/45/7"
        "240/45/5"
        "RACE")
      week))
  "Sun long session; w18 = RACE.")

(define workout/run5k-capture-facts
  (lambda
    (session-id doc)
    (let
      ((params (workout/doc-params doc))
        (goal (workout/pget (workout/doc-params doc) "goal")))
      (unless
        (or (blank? goal) (string-eq "RACE" goal))
        (let*
          ((g (string-split goal "/"))
            (jog (workout/as-int (nth g 0)))
            (walk (workout/as-int (nth g 1)))
            (sets (workout/as-int (nth g 2)))
            (mins (div (+ (* jog sets) (* walk sets)) 60)))
          (workout/replace-fact
            session-id
            "Run/Walk"
            "min"
            (as-str mins))))))
  "run5k: prescribed box in minutes.")

(define workout/run5k-configure
  (lambda
    ()
    (workout/state-put
      "run5k"
      (dict-set* "{}" (list "week" "1" "in_week" "0"))
      "run5k configured")))

(define workout/run5k-day
  (lambda
    (date)
    (let
      ((parts (string-split (as-str date) "-")))
      (if
        (null? parts)
        "OFF"
        (let
          ((wd
            (date-weekday
              (parse-int (nth parts 0))
              (parse-int (nth parts 1))
              (parse-int (nth parts 2)))))
          (cond ((= wd 5) "A") ((= wd 0) "B") (else "OFF"))))))
  "A Fri, B Sun, OFF otherwise.")

(define workout/run5k-gates
  (lambda
    (session-id state)
    (let*
      ((st
          (workout/or-dict
            state
            (dict-set* "{}" (list "week" "1" "in_week" "0"))))
        (in-week
          (+ 1 (workout/as-int (workout/field st "in_week"))))
        (week (workout/as-int (workout/field st "week")))
        (sore-mark (workout/field st "sore_mark"))
        (week-turn (= in-week 2))
        (sore
          (and week-turn (workout/run5k-sore-unseen? sore-mark)))
        (week-next
          (if
            (and week-turn (not sore))
            (if (< week 18) (+ week 1) 18)
            week))
        (next-mark
          (if
            week-turn
            (workout/run5k-sore-count)
            (workout/as-int (workout/field st "sore_mark"))))
        (sore-note
          (if sore "; sore: repeating the week this cycle" ""))
        (state2
          (dict-set*
            "{}"
            (list
              "week"
              (as-str week-next)
              "in_week"
              (as-str (if week-turn 0 in-week))
              "sore_mark"
              (as-str next-mark)))))
      (seq
        (workout/state-put
          "run5k"
          state2
          (if
            week-turn
            (string-append
              "week "
              (as-str week)
              " complete"
              sore-note)
            "session logged in current week"))
        state2)))
  "Week turns over after 2 sessions; sore repeats it.")

(define workout/run5k-ref
  (lambda
    (xs week)
    (let
      ((n (- (workout/as-int week) 1)))
      (let
        ((lim (workout/len xs)))
        (cond
          ((< n 0) (nth xs 0))
          ((< n lim) (nth xs n))
          (else (nth xs (- lim 1)))))))
  "Week n (1-based) -> element n-1; out of range repeats last.")

(define workout/run5k-sore-count
  (lambda
    ()
    (list/foldl
      (lambda
        (acc row)
        (let
          ((f (workout/row-fields row)))
          (if
            (string-eq (workout/field f "key") "sore")
            (+ acc 1)
            acc)))
      0
      (workout/gates "run5k"))))

(define workout/run5k-sore-unseen?
  (lambda
    (seen)
    (>
      (workout/run5k-sore-count)
      (workout/as-int (as-str seen)))))

(define workout/run5k-suggest
  (lambda
    (date)
    (let*
      ((st
          (workout/or-dict
            (workout/state "run5k")
            (dict-set* "{}" (list "week" "1" "in_week" "0"))))
        (week (workout/as-int (workout/field st "week")))
        (day (workout/run5k-day date))
        (goal
          (if
            (string-eq day "B")
            (workout/run5k-b-goal week)
            (workout/run5k-a-goal week))))
      (dict-set*
        "{}"
        (list
          "program"
          "run5k"
          "week"
          (as-str week)
          "day"
          day
          "params"
          (dict-set*
            "{}"
            (list
              "goal"
              goal
              "warmup"
              "3min brisk walk"
              "flush"
              "2min easy walk"
              "talk_test"
              "conversational effort only"))
          "blocks"
          "[]"))))
  "run doc from state. Pure.")

(define workout/run5k-weekdays
  (lambda () (list 5 0)))

(define workout/sct-carry-kind
  (lambda
    (n)
    (workout/pget (workout/sct-carry-session n) "carry")))

(define workout/sct-carry-note
  (lambda
    (n)
    (string-append
      "carry block: "
      (workout/sct-carry-kind n)
      " - accumulate max distance in "
      (workout/pget (workout/sct-carry-session n) "minutes")
      " min, RPE "
      (workout/pget (workout/sct-carry-session n) "rpe"))))

(define workout/sct-carry-session
  (lambda
    (n)
    (let
      ((min-l
          (list
            7
            7
            7
            8
            8
            8
            9
            9
            9
            10
            10
            10))
        (rpe-l
          (list
            7
            7
            7
            8
            8
            8
            9
            9
            9
            7
            7
            7))
        (carry-l
          (list
            "Suitcase 24kg"
            "Rack 24kg"
            "Overhead 24kg"
            "Bottoms-up 24kg"
            "Farmers 2x24kg"
            "Double Rack 2x24kg"
            "Double Overhead 2x24kg")))
      (dict-set*
        "{}"
        (list
          "minutes"
          (as-str (nth min-l (- n 1)))
          "rpe"
          (as-str (nth rpe-l (- n 1)))
          "carry"
          (nth carry-l (mod (- n 1) 7))))))
  "Power session: minutes, RPE, rotating carry (24kg / 2x24kg).")

(define workout/sct-configure
  (lambda
    ()
    (workout/state-put
      "sct"
      (dict-set*
        "{}"
        (list
          "phase"
          "stability"
          "session"
          "1"
          "exits"
          "0"))
      "sct configured"))
  "Initial state: stability s1.")

(define workout/sct-content
  (lambda
    (phase n day)
    (cond
      ((string-eq "OFF" day) (workout/sct-microdose phase))
      ((string-eq "stability" phase)
        (workout/sct-suggest-stability n))
      ((string-eq "strength" phase)
        (if
          (string-eq "A" day)
          (workout/sct-suggest-strength-a n)
          (workout/sct-suggest-strength-b n)))
      ((string-eq "power" phase)
        (if
          (string-eq "B" day)
          (workout/sct-carry-note n)
          "strength maintenance: pick 2-3 drills, stay ownable"))
      (else ""))))

(define workout/sct-dl-column
  (lambda
    (n)
    (nth
      (list
        "3-5x1-3"
        "3-5x2-4"
        "3-5x3-5"
        "3-5x4-6"
        "3-5x5-7"
        "2-4x6-8"
        "2-4x7-9"
        "2-3x8-10"
        "1x10")
      (- n 1))))

(define workout/sct-facts
  (lambda
    (session-id doc)
    (let*
      ((phase (workout/pget doc "phase"))
        (n (workout/as-int (workout/pget doc "session")))
        (day (workout/pget doc "day")))
      (cond
        ((string-eq "stability" phase)
          (workout/sct-stability-facts session-id n))
        ((string-eq "strength" phase)
          (workout/sct-strength-facts session-id n day))
        ((string-eq "power" phase)
          (if
            (string-eq "B" day)
            (workout/replace-fact
              session-id
              (workout/sct-carry-kind n)
              "min"
              (workout/pget (workout/sct-carry-session n) "minutes"))
            ()))
        (else ()))))
  "sct: one fact per drill, lower-bound totals.")

(define workout/sct-gates
  (lambda
    (session-id state)
    (let*
      ((st
          (workout/or-dict state (workout/sct-state-or-default)))
        (phase (workout/field st "phase"))
        (n (workout/as-int (workout/field st "session")))
        (last (workout/sct-phase-last phase))
        (next-n (if (< n last) (+ n 1) last))
        (state2
          (dict-set*
            "{}"
            (list
              "phase"
              phase
              "session"
              (as-str next-n)
              "exits"
              (workout/field st "exits")))))
      (seq
        (workout/state-put
          "sct"
          state2
          "sct: session column advanced")
        state2)))
  "Session column advances, capped at the benchmark.")

(define workout/sct-hang-column
  (lambda
    (n)
    (nth
      (list
        "3-5x1-3"
        "3-6x1-4"
        "3-5x1-5"
        "3-6x1-5"
        "3-7x2-5"
        "3-5x3-5"
        "3-6x3-5"
        "3-5x4-6"
        "3x4-5")
      (- n 1))))

(define workout/sct-hold-column
  (lambda
    (n)
    (nth
      (list
        "3-5x10s"
        "3-5x12-15s"
        "3-5x15-18s"
        "3-5x18-20s"
        "3-5x20-22s"
        "3-5x22-25s"
        "3-5x24-27s"
        "3-4x26-28s"
        "3x30s")
      (- n 1))))

(define workout/sct-hollow-column
  (lambda
    (n)
    (nth
      (list
        "6-10x10s"
        "6-10x15s"
        "5-8x20s"
        "4-7x25s"
        "3-7x30s"
        "3-6x35s"
        "3-5x40s"
        "2-4x45s"
        "2-3x50s")
      (- n 1))))

(define workout/sct-microdose
  (lambda
    (phase)
    (if
      (string-eq "strength" phase)
      "MICRO-DOSE: breathing 1max/position | dead bugs 2x8 | neck drills 1min | 1-2 submax floor drills"
      "MICRO-DOSE: breathing 1max/position | dead bugs 2x8 | neck drills 1min")))

(define workout/sct-phase-last
  (lambda
    (phase)
    (cond
      ((string-eq phase "stability") 12)
      ((string-eq phase "strength") 9)
      (else 12)))
  "Sessions per phase.")

(define workout/sct-promote
  (lambda
    ()
    (let*
      ((st (workout/sct-state-or-default))
        (phase (workout/field st "phase"))
        (exits (workout/as-int (workout/field st "exits"))))
      (cond
        ((string-eq "stability" phase)
          (seq
            (workout/gate-assert
              "sct"
              "stability-exit"
              "true"
              ""
              "benchmarks passed")
            (workout/state-put
              "sct"
              (dict-set*
                "{}"
                (list
                  "phase"
                  "strength"
                  "session"
                  "1"
                  "exits"
                  (as-str (+ exits 1))))
              "promoted: stability -> strength")))
        ((string-eq "strength" phase)
          (seq
            (workout/gate-assert
              "sct"
              "strength-exit"
              "true"
              ""
              "goal column reached")
            (workout/state-put
              "sct"
              (dict-set*
                "{}"
                (list
                  "phase"
                  "power"
                  "session"
                  "1"
                  "exits"
                  (as-str (+ exits 1))))
              "promoted: strength -> power")))
        (else (workout/state "sct")))))
  "Explicit promotion after benchmark tests.")

(define workout/sct-raised-column
  (lambda
    (n)
    (nth
      (list
        "3-6x3-5"
        "3-6x4-7"
        "3-6x5-8"
        "3-5x6-9"
        "3-5x7-10"
        "3-5x8-12"
        "3-4x12-15"
        "2-3x15"
        "3x15")
      (- n 1))))

(define workout/sct-sit-column
  (lambda
    (n)
    (nth
      (list
        "6-10x5s"
        "6-9x7-10s"
        "5-8x8-12s"
        "4-6x10-15s"
        "3-5x15-20s"
        "3-5x18-22s"
        "3-5x20-25s"
        "1-2x22-28s"
        "1x30s")
      (- n 1))))

(define workout/sct-stab-column
  (lambda
    (n)
    (nth
      (list
        "2-3x5-10"
        "3-5x6-12"
        "3-5x6-12"
        "3-5x8-15"
        "3-5x8-15"
        "3-5x15-20"
        "3-5x15-20"
        "3-4x20-25"
        "3-5x20-25"
        "1-2x25-30"
        "2-3x25-30"
        "2-3x25-30")
      (- n 1))))

(define workout/sct-stability-facts
  (lambda
    (session-id n)
    (let
      ((total (workout/col-lower (workout/sct-stab-column n))))
      (seq
        (workout/replace-fact
          session-id
          "Dead Bug"
          "reps"
          (as-str total))
        (workout/replace-fact
          session-id
          "Bird Dog"
          "reps"
          (as-str total))))))

(define workout/sct-state-or-default
  (lambda
    ()
    (workout/or-dict
      (workout/state "sct")
      (dict-set*
        "{}"
        (list
          "phase"
          "stability"
          "session"
          "1"
          "exits"
          "0")))))

(define workout/sct-strength-facts
  (lambda
    (session-id n day)
    (seq
      (workout/replace-fact
        session-id
        "Hollow Hold"
        "sec"
        (as-str
          (workout/col-lower (workout/sct-hollow-column n))))
      (workout/replace-fact
        session-id
        "Leg Raise Short Lever"
        "reps"
        (as-str
          (workout/col-lower (workout/sct-raised-column n))))
      (if
        (string-eq "B" day)
        (seq
          (workout/replace-fact
            session-id
            "Leg Raise Long Lever"
            "reps"
            (as-str
              (workout/col-lower (workout/sct-raised-column n))))
          (workout/replace-fact
            session-id
            "Tuck Sit"
            "sec"
            (as-str (workout/col-lower (workout/sct-sit-column n))))
          (workout/replace-fact
            session-id
            "Hanging Knee Raise"
            "reps"
            (as-str (workout/col-lower (workout/sct-hang-column n)))))
        ())
      (if
        (and (string-eq "B" day) (>= n 7))
        (seq
          (workout/replace-fact
            session-id
            "Ground Hold"
            "sec"
            (as-str (workout/col-lower (workout/sct-hold-column n))))
          (workout/replace-fact
            session-id
            "SL Deadlift"
            "reps"
            (as-str (workout/col-lower (workout/sct-dl-column n)))))
        ())))
  "Strength phase: hollow + short lever always; B >=7 adds ground work.")

(define workout/sct-suggest
  (lambda
    (date)
    (let*
      ((st (workout/sct-state-or-default))
        (phase (workout/field st "phase"))
        (n (workout/as-int (workout/field st "session")))
        (last (workout/sct-phase-last phase))
        (day (workout/run5k-day date))
        (content (workout/sct-content phase n day))
        (bench (if (= n last) "true" "false")))
      (dict-set*
        "{}"
        (list
          "program"
          "sct"
          "phase"
          phase
          "session"
          (as-str n)
          "day"
          day
          "params"
          (dict-set*
            "{}"
            (list
              "content"
              content
              "benchmark_due"
              bench
              "promote_with"
              "(workout/sct-promote)"))))))
  "sct doc from state. Pure.")

(define workout/sct-suggest-stability
  (lambda
    (n)
    (let
      ((col (workout/sct-stab-column n)))
      (string-append
        "breathing 1max/position (goal 30) | dead bugs "
        col
        " | bird dogs "
        col
        " | neck nods/rotations 30 | optional rocking 30"))))

(define workout/sct-suggest-strength-a
  (lambda
    (n)
    (string-append
      "hollow hold "
      (workout/sct-hollow-column n)
      " | short-lever leg raises "
      (workout/sct-raised-column n)
      " | long-lever leg raises "
      (workout/sct-raised-column n))))

(define workout/sct-suggest-strength-b
  (lambda
    (n)
    (let
      ((base
        (string-append
          "hollow hold "
          (workout/sct-hollow-column n)
          " | leg raises "
          (workout/sct-raised-column n)
          " | tuck sit "
          (workout/sct-sit-column n)
          " | hanging knee raises "
          (workout/sct-hang-column n))))
      (if
        (>= n 7)
        (string-append
          base
          " | holds "
          (workout/sct-hold-column n)
          " | kickstand->SL deadlifts "
          (workout/sct-dl-column n))
        base))))

(define workout/session
  (lambda
    (id)
    (let
      ((f (get (quote workout-session) (as-str id))))
      (if (null? f) () f)))
  "Session fields for id, or ().")

(define workout/session-abandon
  (lambda
    (id)
    (let
      ((cur (workout/session id)))
      (if
        (null? cur)
        ()
        (workout/session-update
          id
          (workout/session-fields
            (workout/session-date cur)
            (workout/session-program cur)
            (workout/session-intensity cur)
            "abandoned"
            (workout/session-notes cur)
            (workout/session-doc cur)
            (workout/field cur "duration_sec")
            (workout/field cur "author"))))))
  "Mark a session abandoned.")

(define workout/session-actions
  (lambda
    (id state)
    (let
      ((f (workout/session id))
        (prog
          (if
            (null? (workout/session id))
            ""
            (workout/session-program (workout/session id)))))
      (html/div
        (html/attr "class" "card")
        (html/join
          (list
            (html/h2 "Session")
            (if
              (and
                (string-eq (as-str state) "in_progress")
                (workout/ksk-program? prog))
              (html/p*
                ""
                (html/a
                  (string-append "/workout/run?id=" (as-str id))
                  "→ Open run UI"))
              "")
            (if
              (string-eq (as-str state) "in_progress")
              (html/join
                (list
                  (html/form
                    "POST"
                    "/workout/session"
                    (html/attr "style" "display:inline")
                    (html/join
                      (list
                        (html/hidden "action" "complete")
                        (html/hidden "id" (as-str id))
                        (html/button (html/attr "type" "submit") "Complete"))))
                  (html/text " ")
                  (html/form
                    "POST"
                    "/workout/session"
                    (html/attr "style" "display:inline")
                    (html/join
                      (list
                        (html/hidden "action" "abandon")
                        (html/hidden "id" (as-str id))
                        (html/button (html/attr "type" "submit") "Abandon"))))))
              "")
            (html/form
              "POST"
              "/workout/session"
              (html/attrs
                (list
                  (html/attr "style" "margin-top:0.75rem")
                  (html/attr
                    "onsubmit"
                    "return confirm('Delete session and its exercises?')")))
              (html/join
                (list
                  (html/hidden "action" "delete-session")
                  (html/hidden "id" (as-str id))
                  (html/button
                    (html/attr "type" "submit")
                    "Delete session"))))
            (html/p* "" (html/a "/workout" "← All workouts")))))))
  "HTML card of session actions (complete/abandon/run/delete).")

(define workout/session-complete
  (lambda
    (id notes)
    (let
      ((cur (workout/session id)))
      (if
        (null? cur)
        ()
        (let
          ((n
            (if
              (blank? (as-str notes))
              (workout/session-notes cur)
              (as-str notes))))
          (workout/session-update
            id
            (workout/session-fields
              (workout/session-date cur)
              (workout/session-program cur)
              (workout/session-intensity cur)
              "complete"
              n
              (workout/session-doc cur)
              (workout/field cur "duration_sec")
              (workout/field cur "author")))))))
  "Mark a session complete, keeping existing notes when blank.")

(define workout/session-create
  (lambda
    (program intensity)
    (let
      ((prog
          (if (blank? (as-str program)) "free" (as-str program)))
        (inten (if (null? intensity) "" (as-str intensity))))
      (create
        (quote workout-session)
        (workout/session-fields
          (workout/today)
          prog
          inten
          "in_progress"
          ""
          (workout/empty-doc prog inten)
          ""
          (as-str (whoami))))))
  "Create an in-progress session using today's date.")

(define workout/session-create-complete
  (lambda
    (program intensity date notes)
    (let
      ((prog
          (if (blank? (as-str program)) "free" (as-str program)))
        (inten (if (null? intensity) "" (as-str intensity)))
        (d
          (if
            (blank? (as-str date))
            (workout/today)
            (as-str date)))
        (n (if (null? notes) "" (as-str notes))))
      (create
        (quote workout-session)
        (workout/session-fields
          d
          prog
          inten
          "complete"
          n
          (workout/empty-doc prog inten)
          ""
          (as-str (whoami))))))
  "Create an already-complete session with the given date and notes.")

(define workout/session-create-with-doc
  (lambda
    (program intensity doc)
    (let*
      ((prog
          (if (blank? (as-str program)) "free" (as-str program)))
        (inten (if (null? intensity) "" (as-str intensity)))
        (d
          (if
            (blank? (as-str doc))
            (workout/empty-doc prog inten)
            (as-str doc))))
      (create
        (quote workout-session)
        (workout/session-fields
          (workout/today)
          prog
          inten
          "in_progress"
          ""
          d
          ""
          (as-str (whoami))))))
  "Create an in-progress session carrying the given doc.")

(define workout/session-date
  (lambda (f) (workout/field f "date"))
  "Date field of a session.")

(define workout/session-date-of
  (lambda
    (session-id)
    (let
      ((f (workout/session session-id)))
      (if (null? f) "" (workout/session-date f))))
  "Date of a session by id, or \"\" when absent.")

(define workout/session-delete
  (lambda
    (id)
    (let
      ((sid (as-str id)))
      (seq
        (list/map
          (lambda
            (row)
            (workout/exercise-delete (workout/row-id row)))
          (workout/exercises sid))
        (delete (quote workout-session) sid))))
  "Delete a session and all of its exercise rows.")

(define workout/session-doc
  (lambda (f) (workout/field f "doc"))
  "Doc field of a session.")

(define workout/session-fields
  (lambda
    (date
      program
      intensity
      state
      notes
      doc
      duration_sec
      author)
    (dict-set*
      "{}"
      (list
        "date"
        (as-str date)
        "program"
        (as-str program)
        "intensity"
        (as-str intensity)
        "state"
        (as-str state)
        "notes"
        (as-str notes)
        "doc"
        (as-str doc)
        "duration_sec"
        (as-str duration_sec)
        "author"
        (as-str author))))
  "Dict of fields for a workout-session row.")

(define workout/session-in-progress
  (lambda
    (program)
    (let
      ((want (if (null? program) "" (as-str program))))
      (list/foldl
        (lambda
          (acc row)
          (if
            (null? acc)
            (let
              ((f (workout/row-fields row)))
              (if
                (string-eq (workout/session-state f) "in_progress")
                (if
                  (blank? want)
                  (workout/row-id row)
                  (if
                    (string-eq (workout/session-program f) want)
                    (workout/row-id row)
                    acc))
                acc))
            acc))
        ()
        (workout/session-list 100))))
  "Id of an in-progress session (optionally matching program), or ().")

(define workout/session-intensity
  (lambda (f) (workout/field f "intensity"))
  "Intensity field of a session.")

(define workout/session-li
  (lambda
    (row)
    (let
      ((id (workout/row-id row)) (f (workout/row-fields row)))
      (let
        ((reps (workout/session-total-reps id)))
        (html/li
          (html/attr "class" "card")
          (html/join
            (list
              (html/div
                ""
                (html/join
                  (list
                    (html/a
                      (string-append "/workout/session?id=" id)
                      (html/strong (workout/session-title f)))
                    (html/text " ")
                    (workout/state-badge (workout/session-state f)))))
              (if
                (string-eq (workout/session-program f) "walk")
                (html/p*
                  (html/attr "class" "muted")
                  (html/text (workout/walk-li-summary f)))
                (html/p*
                  (html/attr "class" "muted")
                  (html/text (string-append (as-str reps) " total reps"))))))))))
  "HTML list item for a session row.")

(define workout/session-list
  (lambda
    (limit)
    (let
      ((n (if (null? limit) 50 (workout/as-int limit))))
      (rows (quote workout-session) (if (< n 1) 50 n))))
  "Session rows, up to limit (default 50).")

(define workout/session-main
  (lambda
    (id)
    (let
      ((f (workout/session id)))
      (if
        (null? f)
        (html/div
          (html/attr "id" "workout-session-main")
          (html/join
            (list
              (html/p "Session not found.")
              (html/p* "" (html/a "/workout" "Back")))))
        (let
          ((exs (workout/exercises id))
            (st (workout/session-state f))
            (notes (workout/session-notes f)))
          (let
            ((is-walk (string-eq (workout/session-program f) "walk")))
            (html/div
              (html/attr "id" "workout-session-main")
              (html/join
                (list
                  (html/h1 (html/text (workout/session-title f)))
                  (html/p* "" (workout/state-badge st))
                  (if (blank? notes) "" (html/p notes))
                  (workout/session-actions id st)
                  (if
                    is-walk
                    (workout/walk-detail f)
                    (html/join
                      (list
                        (if
                          (string-eq st "abandoned")
                          ""
                          (workout/add-exercise-form id))
                        (html/h2 "Exercises")
                        (if
                          (null? exs)
                          (html/p*
                            (html/attr "class" "muted")
                            (html/text "No exercises logged."))
                          (html/ul
                            ""
                            (html/join (list/map workout/exercise-li exs))))
                        (html/p*
                          (html/attr "class" "muted")
                          (html/text
                            (string-append
                              "Total reps: "
                              (as-str (workout/session-total-reps id))))))))))))))))
  "Main HTML of a session page (title, actions, exercises).")

(define workout/session-notes
  (lambda (f) (workout/field f "notes"))
  "Notes field of a session.")

(define workout/session-page
  (lambda
    ()
    (if
      (string-eq (request-method) "POST")
      (workout/handle-session-post)
      (let
        ((id (as-str (request-param "id"))))
        (html/page
          (list
            (if
              (blank? id)
              (html/join
                (list
                  (html/h1 "Session")
                  (html/p "Missing id.")
                  (html/p* "" (html/a "/workout" "← Back"))))
              (workout/session-main id)))))))
  "Session page, handling POST or rendering the session.")

(define workout/session-program
  (lambda (f) (workout/field f "program"))
  "Program field of a session.")

(define workout/session-state
  (lambda (f) (workout/field f "state"))
  "State field of a session.")

(define workout/session-summary
  (lambda
    (id)
    (let
      ((f (workout/session id)))
      (if (null? f) () (list id f (workout/exercises id)))))
  "Summary list (id fields exercises) for a session.")

(define workout/session-title
  (lambda
    (f)
    (let
      ((date (workout/session-date f))
        (prog (workout/session-program f))
        (inten (workout/session-intensity f)))
      (if
        (blank? inten)
        (string-append date " · " prog)
        (string-append date " · " prog " / " inten))))
  "Title string for a session: date, program, and intensity.")

(define workout/session-total-reps
  (lambda
    (session-id)
    (list/foldl
      (lambda
        (acc row)
        (+
          acc
          (workout/as-int
            (workout/field (workout/row-fields row) "reps"))))
      0
      (workout/exercises session-id)))
  "Sum of reps across a session's exercises.")

(define workout/session-update
  (lambda
    (id fields)
    (update (quote workout-session) (as-str id) fields))
  "Update a session row with new fields.")

(define workout/sessions-sorted
  (lambda
    ()
    (list/foldl
      workout/insert-session-desc
      ()
      (workout/session-list 100)))
  "Sessions sorted newest first.")

(define workout/start-bar
  (lambda
    ()
    (html/div
      (html/attr "class" "card")
      (html/join
        (list
          (html/h2 "Start workout")
          (html/form
            "POST"
            "/workout"
            ""
            (html/join
              (list
                (html/hidden "action" "start")
                (html/hidden "program" "free")
                (html/label
                  (html/attr "for" "w-inten")
                  "Note (optional)")
                (html/text-input
                  "intensity"
                  (html/attrs
                    (list
                      (html/attr "id" "w-inten")
                      (html/attr "placeholder" "optional tag"))))
                (html/button
                  (html/attr "type" "submit")
                  "Start free session"))))
          (html/h3 "King-Sized Killer")
          (html/p*
            (html/attr "class" "muted")
            (html/text
              "Single-bell snatch programs (1.0 ladders, 2.0 high sets, 3.0 %RM)."))
          (html/form
            "POST"
            "/workout"
            ""
            (html/join
              (list
                (html/hidden "action" "start-ksk")
                (html/label (html/attr "for" "ksk-prog") "Program")
                (html/select
                  "program"
                  (html/attr "id" "ksk-prog")
                  (html/join
                    (list
                      (html/option "ksk1" "KSK 1.0" #t)
                      (html/option "ksk2" "KSK 2.0" #f)
                      (html/option "ksk3" "KSK 3.0" #f))))
                (html/label
                  (html/attr "for" "ksk-phase")
                  "Phase (1.0/2.0)")
                (html/text-input
                  "phase"
                  (html/attrs
                    (list
                      (html/attr "id" "ksk-phase")
                      (html/attr "value" "1")
                      (html/attr "inputmode" "numeric"))))
                (html/label (html/attr "for" "ksk-week") "Week (3.0)")
                (html/text-input
                  "week"
                  (html/attrs
                    (list
                      (html/attr "id" "ksk-week")
                      (html/attr "value" "1")
                      (html/attr "inputmode" "numeric"))))
                (html/label (html/attr "for" "ksk-day") "Day 1–3")
                (html/text-input
                  "day"
                  (html/attrs
                    (list
                      (html/attr "id" "ksk-day")
                      (html/attr "value" "1")
                      (html/attr "inputmode" "numeric"))))
                (html/label (html/attr "for" "ksk-load") "Load")
                (html/text-input
                  "load"
                  (html/attrs
                    (list
                      (html/attr "id" "ksk-load")
                      (html/attr "placeholder" "24")
                      (html/attr "required" "required"))))
                (html/label (html/attr "for" "ksk-unit") "Unit")
                (html/text-input
                  "unit"
                  (html/attrs
                    (list
                      (html/attr "id" "ksk-unit")
                      (html/attr "value" "kg"))))
                (html/label
                  (html/attr "for" "ksk-rm")
                  "RM total (3.0 — L+R max test)")
                (html/text-input
                  "rm_total"
                  (html/attrs
                    (list
                      (html/attr "id" "ksk-rm")
                      (html/attr "placeholder" "50")
                      (html/attr "inputmode" "numeric"))))
                (html/button
                  (html/attr "type" "submit")
                  "Start KSK run"))))
          (html/p*
            (html/attr "class" "muted")
            (html/join
              (list
                (html/text "Or ")
                (html/a "/workout/log" "quick log")
                (html/text " a completed free workout."))))))))
  "HTML card to start a free session or a KSK run.")

(define workout/start-today
  (lambda
    ()
    (let
      ((f (workout/program-get)))
      (if
        (null? f)
        ()
        (if
          (string-eq (workout/field f "done") "1")
          ()
          (let
            ((ip (workout/session-in-progress "")))
            (if
              (null? ip)
              (if
                (string-eq (workout/field f "program") "ksk1")
                (workout/session-create-with-doc
                  "ksk1"
                  (workout/program-intensity f)
                  (workout/program-doc f))
                ())
              ip)))))))

(define workout/state
  (lambda
    (program)
    (let
      ((row (workout/state-row program)))
      (if
        (null? row)
        ()
        (workout/field (workout/row-fields row) "state"))))
  "Active state dict for a program, or () when fresh.")

(define workout/state-badge
  (lambda
    (state)
    (html/span
      (html/attr "class" "muted")
      (html/text (as-str state))))
  "Muted badge showing a session state.")

(define workout/state-fields
  (lambda
    (program state active updated notes)
    (dict-set*
      "{}"
      (list
        "program"
        (as-str program)
        "state"
        (as-str state)
        "active"
        (as-str active)
        "updated"
        (as-str updated)
        "notes"
        (if (null? notes) "" (as-str notes)))))
  "Dict of fields for a workout-program-state row.")

(define workout/state-put
  (lambda
    (program state notes)
    (let
      ((old (workout/state-row program)))
      (seq
        (unless
          (null? old)
          (update
            (quote workout-program-state)
            (workout/row-id old)
            (workout/state-fields
              (workout/field (workout/row-fields old) "program")
              (workout/field (workout/row-fields old) "state")
              ""
              (workout/field (workout/row-fields old) "updated")
              "superseded")))
        (create
          (quote workout-program-state)
          (workout/state-fields
            program
            state
            "true"
            (workout/today)
            notes)))))
  "Write a new active state row; supersedes (deactivates) the old one.")

(define workout/state-row
  (lambda
    (program)
    (let
      ((rs
        (find-rows
          (quote workout-program-state)
          "program"
          (as-str program)
          "active"
          "true"
          5)))
      (if (null? rs) () (car rs))))
  "Active workout-program-state row (id . fields), or ().")

(define workout/str-lt-loop
  (lambda
    (a b i)
    (let
      ((la (string-length a)) (lb (string-length b)))
      (if
        (>= i la)
        (if (>= i lb) #f #t)
        (if
          (>= i lb)
          #f
          (let
            ((ca (substring a i 1)) (cb (substring b i 1)))
            (if
              (string-eq ca cb)
              (workout/str-lt-loop a b (+ i 1))
              (< (workout/char-rank ca) (workout/char-rank cb))))))))
  "Compare strings char-by-char from index i.")

(define workout/str-lt?
  (lambda
    (a b)
    (let
      ((sa (as-str a)) (sb (as-str b)))
      (if (string-eq sa sb) #f (workout/str-lt-loop sa sb 0))))
  "True when string a sorts before string b.")

(define workout/stretch-configure
  (lambda
    ()
    (workout/state-put
      "stretch"
      (dict-set*
        "{}"
        (list
          "cycle"
          "A"
          "sun_count"
          "0"
          "iso_unlocked"
          "false"
          "mark"
          "0"))
      "stretch configured"))
  "Initial state: cycle A (front split advancing).")

(define workout/stretch-day
  (lambda
    (date)
    (let
      ((parts (string-split (as-str date) "-")))
      (if
        (null? parts)
        "OFF"
        (let
          ((wd
            (date-weekday
              (parse-int (nth parts 0))
              (parse-int (nth parts 1))
              (parse-int (nth parts 2)))))
          (cond ((= wd 0) "SUN") ((= wd 3) "WED") (else "LIGHT"))))))
  "SUN heavy slot, WED medium, LIGHT otherwise.")

(define workout/stretch-flip
  (lambda
    ()
    (let
      ((st (workout/state "stretch")))
      (if
        (null? st)
        ()
        (let
          ((cycle (workout/field st "cycle")))
          (let
            ((next-cycle (if (string-eq "A" cycle) "B" "A")))
            (seq
              (workout/gate-assert
                "stretch"
                "cycle-achieved"
                "true"
                ""
                (string-append "cycle " cycle " goal achieved"))
              (workout/state-put
                "stretch"
                (dict-set*
                  "{}"
                  (list
                    "cycle"
                    next-cycle
                    "sun_count"
                    (workout/field st "sun_count")
                    "iso_unlocked"
                    (workout/field st "iso_unlocked")
                    "mark"
                    (workout/field st "mark")))
                (string-append "leapfrog: " cycle " -> " next-cycle))
              (workout/state "stretch")))))))
  "Explicit leapfrog flip when the advancing goal is achieved.")

(define workout/stretch-gates
  (lambda
    (session-id state)
    (let*
      ((st
          (workout/or-dict
            state
            (dict-set*
              "{}"
              (list
                "cycle"
                "A"
                "sun_count"
                "0"
                "iso_unlocked"
                "false"
                "mark"
                "0"))))
        (doc (workout/parse-session-doc session-id))
        (was-sun
          (string-eq "SUN" (as-str (dict-get doc "day"))))
        (counted
          (if
            was-sun
            (+ 1 (workout/as-int (workout/field st "sun_count")))
            (workout/as-int (workout/field st "sun_count")))))
      (let*
        ((ledger (workout/stretch-ledger-count))
          (unlocked
            (workout/stretch-or-str
              (workout/field st "iso_unlocked")
              (if
                (> ledger (workout/as-int (workout/field st "mark")))
                "true"
                "false")))
          (mark-next
            (if
              was-sun
              ledger
              (workout/as-int (workout/field st "mark")))))
        (seq
          (workout/state-put
            "stretch"
            (dict-set*
              "{}"
              (list
                "cycle"
                (workout/field st "cycle")
                "sun_count"
                (as-str counted)
                "iso_unlocked"
                unlocked
                "mark"
                (as-str mark-next)))
            "stretch: sunday slot counted")
          (workout/state "stretch")))))
  "Consumes own-the-range asserts from the marker.")

(define workout/stretch-goals
  (lambda
    (cycle)
    (cond
      ((string-eq "A" cycle)
        (dict-set*
          "{}"
          (list
            "advancing"
            "Front Split"
            "light"
            "Reverse Hamstring Stretch + Kneeling Lunge - prying"
            "medium"
            "Front Split - Pulses+Holds (10 pulses/15s -> 10/20s -> 10/30s)"
            "heavy"
            "Front Split - Extended Isometric (3-5 contractions 30-45s, 3-5min rests)"
            "maintain"
            "Loaded Straddle: Horizontal Cossack - Pulses+Holds"
            "light_other"
            "Frog Series - prying")))
      (else
        (dict-set*
          "{}"
          (list
            "advancing"
            "Loaded Straddle"
            "light"
            "Frog Series - prying"
            "medium"
            "Loaded Straddle - Pulses+Holds"
            "heavy"
            "Loaded Straddle - Extended Isometric"
            "maintain"
            "Front Split - Pulses+Holds"
            "light_other"
            "Reverse Hamstring Stretch + Kneeling Lunge - prying")))))
  "Advancing + maintaining slot content per cycle.")

(define workout/stretch-ledger-count
  (lambda
    ()
    (list/foldl
      (lambda
        (acc row)
        (let
          ((f (workout/row-fields row)))
          (if
            (string-eq (workout/field f "key") "own-the-range")
            (+ acc 1)
            acc)))
      0
      (workout/gates "stretch")))
  "own-the-range asserts, total.")

(define workout/stretch-or-str
  (lambda
    (cur next)
    (cond
      ((string-eq "true" (as-str cur)) "true")
      ((string-eq "true" (as-str next)) "true")
      (else
        (if (blank? (as-str cur)) (as-str next) (as-str cur)))))
  "true wins: never downgrades.")

(define workout/stretch-suggest
  (lambda
    (date)
    (let*
      ((st
          (workout/or-dict
            (workout/state "stretch")
            (dict-set*
              "{}"
              (list
                "cycle"
                "A"
                "sun_count"
                "0"
                "iso_unlocked"
                "false"
                "mark"
                "0"))))
        (cycle (workout/field st "cycle"))
        (goals (workout/stretch-goals cycle))
        (day (workout/stretch-day date))
        (iso
          (string-eq "true" (workout/field st "iso_unlocked")))
        (sun-even
          (=
            0
            (mod (workout/as-int (workout/field st "sun_count")) 2)))
        (slot
          (cond
            ((string-eq "SUN" day)
              (if
                (and iso sun-even)
                (workout/field goals "heavy")
                (workout/field goals "medium")))
            ((string-eq "WED" day) (workout/field goals "maintain"))
            (else
              (string-append
                "LIGHT - "
                (workout/field goals "light_other")
                " + "
                (workout/field goals "light")
                " (prying only)")))))
      (dict-set*
        "{}"
        (list
          "program"
          "stretch"
          "cycle"
          cycle
          "day"
          day
          "params"
          (dict-set*
            "{}"
            (list
              "slot"
              slot
              "cap"
              (if
                iso
                "heavy iso 3x per 2 weeks max"
                "no deep isometrics until the range is owned")
              "promote_with"
              "(workout/stretch-flip) when the advancing goal is achieved"))))))
  "stretch doc from state. Pure.")

(define workout/suggest
  (lambda
    (program date)
    (cond
      ((string-eq (as-str program) "toy")
        (workout/toy-suggest date))
      ((string-eq (as-str program) "ksf")
        (workout/ksf-suggest date))
      ((string-eq (as-str program) "run5k")
        (workout/run5k-suggest date))
      ((string-eq (as-str program) "sct")
        (workout/sct-suggest date))
      ((string-eq (as-str program) "stretch")
        (workout/stretch-suggest date))
      (else ())))
  "Generate today's session doc for a gated program. Pure.")

(define workout/sum
  (lambda
    (xs)
    (list/foldl
      (lambda (acc x) (+ acc (workout/as-int x)))
      0
      xs))
  "Sum of a list of numbers.")

(define workout/timer-script
  (lambda
    (cap-min)
    (html/tag
      "script"
      ""
      (string-append
        "(function(){"
        "var el=document.getElementById('ksk-timer');"
        "if(!el)return;"
        "var cap="
        (as-str (workout/as-int cap-min))
        "*60;"
        "var key='pp.ksk.t.'+ (el.getAttribute('data-sid')||'');"
        "var start=0;"
        "try{start=parseInt(sessionStorage.getItem(key)||'0',10)||0;}catch(e){}"
        "if(!start){start=Date.now();try{sessionStorage.setItem(key,String(start));}catch(e){}}"
        "function tick(){"
        "var s=Math.floor((Date.now()-start)/1000);"
        "var left=Math.max(0,cap-s);"
        "var m=Math.floor(left/60), r=left%60;"
        "el.textContent=(m<10?'0':'')+m+':'+(r<10?'0':'')+r"
        "+(left===0?' DONE':'');"
        "if(left>0)setTimeout(tick,250);"
        "}"
        "tick();"
        "})();")))
  "Inline JS implementing the countdown timer for a KSK run.")

(define workout/today
  (lambda () (date-today))
  "Today's date string.")

(define workout/today-card
  (lambda
    ()
    (let
      ((r (workout/program-row)))
      (if
        (null? r)
        (html/div
          (html/attr "class" "card")
          (html/join
            (list
              (html/h2 "Start a program")
              (html/p*
                (html/attr "class" "muted")
                (html/text
                  "Track where you are: one click to start, auto-advanced when you finish."))
              (html/form
                "POST"
                "/workout"
                ""
                (html/join
                  (list
                    (html/hidden "action" "program-init")
                    (html/hidden "program" "ksk1")
                    (html/label (html/attr "for" "pi-load") "Load")
                    (html/text-input
                      "load"
                      (html/attrs
                        (list
                          (html/attr "id" "pi-load")
                          (html/attr "placeholder" "24")
                          (html/attr "required" "required"))))
                    (html/label (html/attr "for" "pi-unit") "Unit")
                    (html/text-input
                      "unit"
                      (html/attrs
                        (list
                          (html/attr "id" "pi-unit")
                          (html/attr "value" "kg"))))
                    (html/button
                      (html/attr "type" "submit")
                      "Begin KSK 1.0")))))))
        (let
          ((f (workout/row-fields r)))
          (if
            (string-eq (workout/field f "done") "1")
            (html/div
              (html/attr "class" "card")
              (html/join
                (list
                  (html/h2 "Program complete")
                  (html/p*
                    (html/attr "class" "muted")
                    (html/text "KSK 1.0 finished — all 3 phases."))
                  (html/form
                    "POST"
                    "/workout"
                    ""
                    (html/join
                      (list
                        (html/hidden "action" "program-init")
                        (html/hidden "program" "ksk1")
                        (html/label (html/attr "for" "pi-load-r") "Load")
                        (html/text-input
                          "load"
                          (html/attrs
                            (list
                              (html/attr "id" "pi-load-r")
                              (html/attr "placeholder" "24")
                              (html/attr "required" "required"))))
                        (html/label (html/attr "for" "pi-unit-r") "Unit")
                        (html/text-input
                          "unit"
                          (html/attrs
                            (list
                              (html/attr "id" "pi-unit-r")
                              (html/attr "value" "kg"))))
                        (html/button
                          (html/attr "type" "submit")
                          "Start a new cycle")))))))
            (html/div
              (html/attr "class" "card")
              (html/join
                (list
                  (html/h2 "Next session")
                  (html/p (workout/program-title f))
                  (if
                    (null? (workout/session-in-progress ""))
                    (html/form
                      "POST"
                      "/workout"
                      ""
                      (html/join
                        (list
                          (html/hidden "action" "start-today")
                          (html/button
                            (html/attr "type" "submit")
                            "Start session"))))
                    (html/p*
                      (html/attr "class" "muted")
                      (html/text "Session in progress — resume above.")))
                  (html/form
                    "POST"
                    "/workout"
                    ""
                    (html/join
                      (list
                        (html/hidden "action" "program-skip")
                        (html/button
                          (html/attr "type" "submit")
                          "Skip this session")))))))))))))

(define workout/today-card-for
  (lambda
    (program)
    (html/div
      (html/attr "class" "card")
      (html/join
        (list
          (html/h2 (as-str program))
          (html/p*
            (html/attr "class" "muted")
            (html/text (as-str (workout/gate-status program))))
          (html/pre
            (html/text
              (as-str (workout/suggest program (workout/today)))))))))
  "One program block: name, state summary, suggestion doc.")

(define workout/today-page
  (lambda
    ()
    (html/page
      (list
        (html/h1 "Today")
        (html/p*
          (html/attr "class" "muted")
          (html/text (workout/today)))
        (workout/today-card-for "ksf")
        (workout/today-card-for "run5k")
        (workout/today-card-for "sct")
        (workout/today-card-for "stretch")
        (html/a "/workout" "All workouts"))))
  "Agenda view: suggest docs + state for the four gated programs.")

(define workout/toy-count
  (lambda () (workout/completed-count "toy")))

(define workout/toy-gates
  (lambda
    (session-id state)
    (let
      ((done (workout/toy-count)))
      (let
        ((per (workout/toy-sessions-per-level)))
        (let
          ((new-st (workout/toy-state (+ 1 (div done per)) done)))
          (let
            ((old-done
              (if
                (null? state)
                "-1"
                (as-str (dict-get state "sessions_done")))))
            (if
              (string-eq old-done (as-str done))
              state
              (seq
                (workout/state-put
                  "toy"
                  new-st
                  "toy gate: sessions advanced")
                new-st)))))))
  "Toy gate: fold completed sessions into state.")

(define workout/toy-sessions-per-level
  (lambda () 3)
  "Completed sessions required to advance a toy level.")

(define workout/toy-state
  (lambda
    (level done)
    (dict-set*
      "{}"
      (list
        "level"
        (as-str level)
        "sessions_done"
        (as-str done))))
  "Toy state dict.")

(define workout/toy-suggest
  (lambda
    (date)
    (let
      ((st (workout/state "toy")))
      (dict-set*
        "{}"
        (list
          "program"
          "toy"
          "date"
          (as-str date)
          "params"
          (if
            (null? st)
            (dict-set*
              "{}"
              (list
                "level"
                "1"
                "note"
                "fresh: 3 completions level up"))
            st)
          "blocks"
          "[]"))))
  "Toy doc: derived, writes nothing.")

(define workout/volume
  (lambda
    (exercise from-date to-date weight unit)
    (let
      ((ex (workout/normalize-exercise exercise))
        (w (as-str weight))
        (u (workout/normalize-unit unit)))
      (list/foldl
        (lambda
          (acc row)
          (let
            ((f (workout/row-fields row)))
            (if
              (string-eq (workout/field f "exercise") ex)
              (let
                ((day
                  (workout/session-date-of (workout/field f "session_id"))))
                (if
                  (workout/date-in-range? day from-date to-date)
                  (let
                    ((match-w
                        (if
                          (blank? w)
                          #t
                          (string-eq (workout/field f "weight") w)))
                      (match-u
                        (if
                          (blank? u)
                          #t
                          (string-eq
                            (workout/normalize-unit (workout/field f "unit"))
                            u))))
                    (if
                      match-w
                      (if
                        match-u
                        (+ acc (workout/as-int (workout/field f "reps")))
                        acc)
                      acc))
                  acc))
              acc)))
        0
        (rows (quote workout-exercise) 1000))))
  "Total reps of an exercise in the range, filtered by weight/unit.")

(define workout/walk-create
  (lambda
    (date distance unit duration-min location notes)
    (let
      ((d
          (if
            (blank? (as-str date))
            (workout/today)
            (as-str date)))
        (dur
          (if
            (blank? (as-str duration-min))
            "0"
            (as-str duration-min))))
      (create
        (quote workout-session)
        (workout/session-fields
          d
          "walk"
          ""
          "complete"
          (if (null? notes) "" (as-str notes))
          (workout/walk-doc distance unit dur location)
          (as-str (* (workout/as-int dur) 60))
          (as-str (whoami))))))
  "Create a completed walk session.")

(define workout/walk-detail
  (lambda
    (f)
    (let
      ((doc (workout/session-doc f))
        (dist (workout/pget (workout/session-doc f) "distance"))
        (unit (workout/pget (workout/session-doc f) "unit"))
        (dur
          (workout/pget (workout/session-doc f) "duration_min"))
        (loc (workout/pget (workout/session-doc f) "location"))
        (pace (workout/walk-pace-str (workout/session-doc f))))
      (html/div
        (html/attr "class" "card")
        (html/join
          (list
            (html/h2 "Walk")
            (html/ul
              ""
              (html/join
                (list
                  (html/li "" (html/text (string-append dist " " unit)))
                  (html/li "" (html/text (string-append dur " min")))
                  (if
                    (blank? pace)
                    ""
                    (html/li "" (html/text (string-append pace " /" unit))))
                  (if (blank? loc) "" (html/li "" (html/text loc)))))))))))
  "HTML detail card for a walk session.")

(define workout/walk-doc
  (lambda
    (distance unit duration-min location)
    (dict-set*
      "{}"
      (list
        "kind"
        "walk"
        "distance"
        (as-str distance)
        "unit"
        (as-str unit)
        "duration_min"
        (as-str duration-min)
        "location"
        (as-str location))))
  "Session doc dict for a walk.")

(define workout/walk-form
  (lambda
    ()
    (html/div
      (html/attr "class" "card")
      (html/join
        (list
          (html/h2 "Log a walk")
          (html/p*
            (html/attr "class" "muted")
            (html/text "Distance, duration, and pace."))
          (html/form
            "POST"
            "/workout/log"
            ""
            (html/join
              (list
                (html/hidden "action" "quick-walk")
                (html/label (html/attr "for" "wk-date") "Date")
                (html/text-input
                  "date"
                  (html/attrs
                    (list
                      (html/attr "id" "wk-date")
                      (html/attr "value" (workout/today))
                      (html/attr "placeholder" "YYYY-MM-DD"))))
                (html/label (html/attr "for" "wk-dist") "Distance")
                (html/text-input
                  "distance"
                  (html/attrs
                    (list
                      (html/attr "id" "wk-dist")
                      (html/attr "required" "required")
                      (html/attr "inputmode" "decimal")
                      (html/attr "placeholder" "3.2"))))
                (html/label (html/attr "for" "wk-unit") "Unit")
                (html/select
                  "unit"
                  (html/attr "id" "wk-unit")
                  (html/join
                    (list
                      (html/option "mi" "mi" #t)
                      (html/option "km" "km" #f))))
                (html/label (html/attr "for" "wk-dur") "Duration (min)")
                (html/text-input
                  "duration"
                  (html/attrs
                    (list
                      (html/attr "id" "wk-dur")
                      (html/attr "required" "required")
                      (html/attr "inputmode" "numeric")
                      (html/attr "placeholder" "45"))))
                (html/label
                  (html/attr "for" "wk-loc")
                  "Location (optional)")
                (html/text-input
                  "location"
                  (html/attrs
                    (list
                      (html/attr "id" "wk-loc")
                      (html/attr "placeholder" "neighborhood / trail"))))
                (html/label
                  (html/attr "for" "wk-notes")
                  "Notes (optional)")
                (html/textarea
                  "notes"
                  (html/attrs
                    (list
                      (html/attr "id" "wk-notes")
                      (html/attr "rows" "2")))
                  "")
                (html/button (html/attr "type" "submit") "Save walk"))))))))
  "HTML card form to log a walk.")

(define workout/walk-li-summary
  (lambda
    (f)
    (let
      ((dist (workout/pget (workout/session-doc f) "distance"))
        (unit (workout/pget (workout/session-doc f) "unit"))
        (dur
          (workout/pget (workout/session-doc f) "duration_min"))
        (pace (workout/walk-pace-str (workout/session-doc f))))
      (if
        (blank? pace)
        (string-append
          dist
          " "
          unit
          " - "
          dur
          " min")
        (string-append
          dist
          " "
          unit
          " - "
          dur
          " min - "
          pace
          " /"
          unit))))
  "One-line summary of a walk session.")

(define workout/walk-pace-str
  (lambda
    (doc)
    (let
      ((dist-str (workout/pget doc "distance"))
        (dur-str (workout/pget doc "duration_min")))
      (if
        (or (blank? dist-str) (blank? dur-str))
        ""
        (let
          ((dist (parse-float dist-str))
            (dur (parse-float dur-str)))
          (if
            (= dist 0.0)
            ""
            (let
              ((pace-sec (/ (* dur 60.0) dist)))
              (let
                ((pace-min (floor (/ pace-sec 60.0))))
                (let
                  ((sec-rem (round (- pace-sec (* pace-min 60.0)))))
                  (string-append
                    (as-str pace-min)
                    ":"
                    (if
                      (< sec-rem 10)
                      (string-append "0" (as-str sec-rem))
                      (as-str sec-rem)))))))))))
  "Pace string (mm:ss per unit) for a walk doc, \"\" when no distance.")
