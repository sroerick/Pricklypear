;; Workout journal — fluid session.doc + standardized exercise facts.
;; Authority: borge/workout.borg. Free-log MVP (structured ksk/qd later).
;; Install: (load-library "workout") — seeded on system/main after calendar.
;; Depends: std (html/*, blank?, list/*, cond, when/unless).

(deftype 'workout-session
  'date 'program 'intensity 'state 'notes 'doc 'duration_sec 'author)

(deftype 'workout-exercise
  'session_id 'exercise 'weight 'unit 'reps 'notes 'detail)

;; --- small helpers ---

(define (workout/today)
  "Today YYYY-MM-DD."
  (date-today))

(define (workout/row-id row)
  "Id from (id . fields)."
  (as-str (car row)))

(define (workout/row-fields row)
  "Fields from (id . fields)."
  (cdr row))

(define (workout/field fields key)
  "String field or empty."
  (let ((v (dict-get fields (as-str key))))
    (if (null? v) "" (as-str v))))

(define (workout/as-int x)
  "Parse int; blank -> 0."
  (let ((s (as-str x)))
    (if (blank? s) 0 (parse-int s))))

(define (workout/normalize-unit u)
  "Normalize unit to kg|lb when obvious."
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

(define (workout/normalize-exercise name)
  "Trim exercise name."
  (string-trim (as-str name)))

(define (workout/empty-doc program intensity)
  "Minimal fluid doc envelope (JSON tree)."
  (dict-set* "{}"
    (list
      "program" (as-str program)
      "intensity" (as-str intensity)
      "params" "{}"
      "progress" "{}"
      "blocks" "[]")))

;; Date ordering via the string<? prim family (byte-wise; correct for
;; zero-padded ISO YYYY-MM-DD from date-format/date-today). Replaces the
;; hand-rolled char-rank/str-lt loop deleted when string<? landed.
(define (workout/date-ge? a b)
  "a >= b (ISO date strings)."
  (string>=? (as-str a) (as-str b)))

(define (workout/date-le? a b)
  "a <= b (ISO date strings)."
  (string<=? (as-str a) (as-str b)))

(define (workout/date-in-range? day from to)
  "True if day in [from,to] inclusive; blank bounds open."
  (let ((d (as-str day)))
    (if (blank? d)
        #f
        (let ((ok-from
                (if (blank? (as-str from)) #t (workout/date-ge? d from)))
              (ok-to
                (if (blank? (as-str to)) #t (workout/date-le? d to))))
          (if ok-from ok-to #f)))))

;; --- session CRUD ---

(define (workout/session-fields date program intensity state notes doc duration_sec author)
  "Build workout-session fields."
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

(define (workout/session-create program intensity)
  "Create in_progress session; return id."
  (let ((prog (if (blank? (as-str program)) "free" (as-str program)))
        (inten (if (null? intensity) "" (as-str intensity))))
    (create 'workout-session
      (workout/session-fields
        (workout/today) prog inten "in_progress" ""
        (workout/empty-doc prog inten) "" (as-str (whoami))))))

(define (workout/session-create-with-doc program intensity doc)
  "Create in_progress session with full fluid doc; return id."
  (let ((prog (if (blank? (as-str program)) "free" (as-str program)))
        (inten (if (null? intensity) "" (as-str intensity)))
        (d (if (blank? (as-str doc))
               (workout/empty-doc prog inten)
               (as-str doc))))
    (create 'workout-session
      (workout/session-fields
        (workout/today) prog inten "in_progress" ""
        d "" (as-str (whoami))))))

(define (workout/session-create-complete program intensity date notes)
  "Create complete session (quick log); return id."
  (let ((prog (if (blank? (as-str program)) "free" (as-str program)))
        (inten (if (null? intensity) "" (as-str intensity)))
        (d (if (blank? (as-str date)) (workout/today) (as-str date)))
        (n (if (null? notes) "" (as-str notes))))
    (create 'workout-session
      (workout/session-fields
        d prog inten "complete" n
        (workout/empty-doc prog inten) "" (as-str (whoami))))))

(define (workout/session id)
  "Session fields or ()."
  (let ((f (get 'workout-session (as-str id))))
    (if (null? f) () f)))

(define (workout/session-list limit)
  "Recent workout-session rows."
  (let ((n (if (null? limit) 50 (workout/as-int limit))))
    (rows 'workout-session (if (< n 1) 50 n))))

(define (workout/session-date f)
  (workout/field f "date"))
(define (workout/session-program f)
  (workout/field f "program"))
(define (workout/session-intensity f)
  (workout/field f "intensity"))
(define (workout/session-state f)
  (workout/field f "state"))
(define (workout/session-notes f)
  (workout/field f "notes"))
(define (workout/session-doc f)
  (workout/field f "doc"))

(define (workout/session-update id fields)
  "Replace session fields."
  (update 'workout-session (as-str id) fields))

(define (workout/session-complete id notes)
  "Mark session complete."
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

(define (workout/session-abandon id)
  "Mark session abandoned."
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

(define (workout/session-in-progress program)
  "First in_progress session id (optional program filter)."
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

;; --- exercise facts ---

(define (workout/exercise-fields session-id exercise weight unit reps notes detail)
  "Build workout-exercise fields."
  (dict-set* "{}"
    (list
      "session_id" (as-str session-id)
      "exercise" (workout/normalize-exercise exercise)
      "weight" (as-str weight)
      "unit" (workout/normalize-unit unit)
      "reps" (as-str reps)
      "notes" (as-str notes)
      "detail" (as-str detail))))

(define (workout/exercises session-id)
  "Exercise facts for session."
  (let ((sid (as-str session-id)))
    (list/filter
      (lambda (row)
        (string-eq (workout/field (workout/row-fields row) "session_id") sid))
      (rows 'workout-exercise 500))))

(define (workout/exercise-log session-id exercise weight unit reps notes detail)
  "Append exercise fact; return id."
  (let ((name (workout/normalize-exercise exercise)))
    (if (blank? name)
        ()
        (create 'workout-exercise
          (workout/exercise-fields
            session-id name weight unit
            (if (blank? (as-str reps)) "0" (as-str reps))
            (if (null? notes) "" notes)
            (if (null? detail) "" detail))))))

(define (workout/exercise-delete id)
  "Delete one exercise fact."
  (delete 'workout-exercise (as-str id)))

(define (workout/session-delete id)
  "Delete session and its facts."
  (let ((sid (as-str id)))
    (seq
      (list/map
        (lambda (row) (workout/exercise-delete (workout/row-id row)))
        (workout/exercises sid))
      (delete 'workout-session sid))))

;; --- queries ---

(define (workout/session-date-of session-id)
  "Date of session id."
  (let ((f (workout/session session-id)))
    (if (null? f) "" (workout/session-date f))))

(define (workout/volume exercise from-date to-date weight unit)
  "Sum reps for exercise with optional date/load filters."
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

(define (workout/history from-date to-date limit)
  "Sessions in date range."
  (let ((lim (if (null? limit) 50 (workout/as-int limit))))
    (list/filter
      (lambda (row)
        (workout/date-in-range?
          (workout/session-date (workout/row-fields row))
          from-date to-date))
      (workout/session-list lim))))

(define (workout/session-summary id)
  "(id fields exercises) for one session."
  (let ((f (workout/session id)))
    (if (null? f) () (list id f (workout/exercises id)))))

(define (workout/name-seen? lst name)
  "True if name in list."
  (list/foldl
    (lambda (acc x)
      (if acc acc (string-eq (as-str x) (as-str name))))
    #f
    lst))

(define (workout/exercise-names limit)
  "Distinct exercise names."
  (let ((lim (if (null? limit) 100 (workout/as-int limit))))
    (list/foldl
      (lambda (acc row)
        (let ((n (workout/field (workout/row-fields row) "exercise")))
          (if (blank? n)
              acc
              (if (workout/name-seen? acc n) acc (cons n acc)))))
      ()
      (rows 'workout-exercise lim))))

(define (workout/session-total-reps session-id)
  "Total reps in a session."
  (list/foldl
    (lambda (acc row)
      (+ acc
         (workout/as-int
           (workout/field (workout/row-fields row) "reps"))))
    0
    (workout/exercises session-id)))

(define (workout/programs)
  "Available program ids."
  (list "free" "ksk1" "ksk2" "ksk3"))

(define (workout/ksk-program? p)
  "True if program is a KSK variant."
  (let ((s (as-str p)))
    (if (string-eq s "ksk1") #t
        (if (string-eq s "ksk2") #t
            (string-eq s "ksk3")))))

;; --- integer helpers (for KSK 3.0 %) ---

(define (workout/max0 n)
  "Clamp to at least 1."
  (if (< n 1) 1 n))

(define (workout/len xs)
  "Length of list or pair-chain (count only accepts true lists)."
  (list/foldl (lambda (acc _) (+ acc 1)) 0 xs))

;; --- KSK prescription tables (structure only; personal program use) ---

;; Returns comma-joined rungs for ladder, or "S:N" for straight sets of N.
(define (workout/ksk1-pattern phase day)
  "KSK 1.0 pattern string for phase/day."
  (let ((p (workout/as-int phase))
        (d (workout/as-int day)))
    (cond
      ((= p 1)
       (cond ((= d 1) "1,2,3,4,5")
             ((= d 2) "1,2,3")
             (else "1,2,3,4")))
      ((= p 2)
       (cond ((= d 1) "3,6,9")
             ((= d 2) "3,6")
             (else "3,6,9,12")))
      (else
       (cond ((= d 1) "S:9")
             ((= d 2) "S:15")
             (else "S:12"))))))

(define (workout/ksk2-set-size phase day)
  "KSK 2.0 per-hand set size for phase/day."
  (let ((p (workout/as-int phase))
        (d (workout/as-int day)))
    (cond
      ((= p 1) (cond ((= d 1) 10) ((= d 2) 20) (else 15)))
      ((= p 2) (cond ((= d 1) 12) ((= d 2) 24) (else 18)))
      (else (cond ((= d 1) 15) ((= d 2) 30) (else 20))))))

(define (workout/ksk3-pct week day)
  "KSK 3.0 percentage of RM-total for week/day."
  (let ((w (workout/as-int week))
        (d (workout/as-int day)))
    (cond
      ((= d 1) (if (or (= w 1) (= w 3)) 50 60))
      ((= d 2) (if (or (= w 1) (= w 3)) 65 75))
      (else (if (or (= w 1) (= w 3)) 70 80)))))

(define (workout/ksk3-per-hand rm-total pct)
  "Per-hand reps = floor(rm_total * pct / 200)."
  (workout/max0 (floor (div (* (workout/as-int rm-total) (workout/as-int pct)) 200))))

(define (workout/pattern-kind pat)
  "ladder | sets from pattern string."
  (let ((s (as-str pat)))
    (if (string-eq (substring s 0 (if (< (string-length s) 2) 0 2)) "S:")
        "sets"
        "ladder")))

(define (workout/pattern-rungs pat)
  "List of rung size strings for ladder pattern."
  (let ((s (as-str pat)))
    (if (string-eq (workout/pattern-kind s) "sets")
        (list)
        (string-split s ","))))

(define (workout/pattern-set-size pat)
  "Straight-set size from S:N pattern."
  (let ((s (as-str pat)))
    (if (string-eq (workout/pattern-kind s) "sets")
        (workout/as-int (substring s 2 (- (string-length s) 2)))
        0)))

(define (workout/ksk-time-cap program)
  "Minutes: ksk3=15, else 20."
  (if (string-eq (as-str program) "ksk3") 15 20))

(define (workout/ksk-build-doc program phase day week load unit pattern rm-total pct per-hand)
  "Full KSK session.doc tree."
  (dict-set* "{}"
    (list
      "program" (as-str program)
      "exercise" "Snatch"
      "params"
      (dict-set* "{}"
        (list
          "phase" (as-str phase)
          "day" (as-str day)
          "week" (as-str week)
          "time_cap_min" (as-str (workout/ksk-time-cap program))
          "load" (as-str load)
          "unit" (workout/normalize-unit unit)
          "pattern" (as-str pattern)
          "kind" (workout/pattern-kind pattern)
          "rm_total" (as-str rm-total)
          "pct" (as-str pct)
          "per_hand" (as-str per-hand)
          "hand_policy" "per-rung-both"))
      "progress"
      (dict-set* "{}"
        (list
          "ladder" "0"
          "rung_index" "0"
          "side" "L"
          "reps_l" "0"
          "reps_r" "0"
          "sets_done" "0"
          "started" ""
          "elapsed_sec" "0"))
      "blocks" "[]")))

(define (workout/ksk1-plan phase day load unit)
  "KSK 1.0 plan doc for phase day load unit."
  (let ((p (workout/as-int phase))
        (d (workout/as-int day))
        (pat (workout/ksk1-pattern phase day)))
    (workout/ksk-build-doc "ksk1" p d "" load unit pat "" "" "")))

(define (workout/ksk2-plan phase day load unit)
  "KSK 2.0 plan doc (straight sets)."
  (let ((p (workout/as-int phase))
        (d (workout/as-int day))
        (n (workout/ksk2-set-size phase day)))
    (workout/ksk-build-doc "ksk2" p d "" load unit
      (string-append "S:" (as-str n)) "" "" "")))

(define (workout/ksk3-plan week day load unit rm-total)
  "KSK 3.0 plan doc from week/day/rm-total."
  (let ((w (workout/as-int week))
        (d (workout/as-int day))
        (rm (workout/as-int rm-total))
        (pct (workout/ksk3-pct week day)))
    (let ((ph (workout/ksk3-per-hand rm pct)))
      (workout/ksk-build-doc "ksk3" "" d w load unit
        (string-append "S:" (as-str ph))
        rm pct ph))))

(define (workout/doc-params doc)
  "params sub-dict of session doc."
  (dict-get doc "params"))

(define (workout/doc-progress doc)
  "progress sub-dict of session doc."
  (dict-get doc "progress"))

(define (workout/pget dict key)
  "String get from nested dict."
  (if (null? dict) ""
      (let ((v (dict-get dict (as-str key))))
        (if (null? v) "" (as-str v)))))

(define (workout/parse-session-doc id)
  "Raw doc string for session (JSON tree)."
  (let ((f (workout/session id)))
    (if (null? f) ()
        (let ((raw (workout/session-doc f)))
          (if (blank? raw) () raw)))))

(define (workout/ksk-current-target doc)
  "Reps target for current step (per hand)."
  (let* ((params (workout/doc-params doc))
         (prog (workout/doc-progress doc))
         (kind (workout/pget params "kind"))
         (pat (workout/pget params "pattern"))
         (ri (workout/as-int (workout/pget prog "rung_index"))))
    (if (string-eq kind "sets")
        (let ((ph (workout/pget params "per_hand")))
          (if (blank? ph)
              (workout/pattern-set-size pat)
              (workout/as-int ph)))
        (let* ((rungs (workout/pattern-rungs pat))
               (n (workout/len rungs))
               (i (if (< ri 0) 0 (if (>= ri n) 0 ri))))
          (if (null? rungs) 0
              (workout/as-int (nth rungs i)))))))

(define (workout/ksk-label doc)
  "Short human label for a KSK doc."
  (let ((params (workout/doc-params doc))
        (prog (as-str (dict-get doc "program"))))
    (let ((phase (workout/pget params "phase"))
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
        " D" day
        " · " pat
        " · " load (if (blank? unit) "" (string-append " " unit))
        " · " cap "m"))))

(define (workout/ksk-start program phase day week load unit rm-total)
  "Create in_progress KSK session from plan args; return id."
  (let* ((p (as-str program))
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
           (if (string-eq p "ksk3")
               (string-append "w" (as-str week) "d" (as-str day))
               (string-append "p" (as-str phase) "d" (as-str day)))))
    (if (null? doc)
        ()
        (workout/session-create-with-doc p inten doc))))

(define (workout/ksk-upsert-facts session-id doc)
  "Project L/R totals into exercise facts (idempotent replace)."
  (let ((params (workout/doc-params doc))
        (prog (workout/doc-progress doc)))
    (let ((load (workout/pget params "load"))
          (unit (workout/pget params "unit"))
          (rl (workout/pget prog "reps_l"))
          (rr (workout/pget prog "reps_r"))
          (sid (as-str session-id)))
      (seq
        (workout/ksk-replace-side sid "Snatch L" load unit rl)
        (workout/ksk-replace-side sid "Snatch R" load unit rr)))))

(define (workout/ksk-replace-side sid name load unit reps)
  "Delete existing name rows then log fresh total."
  (list/map
    (lambda (row)
      (let ((f (workout/row-fields row)))
        (if (string-eq (workout/field f "exercise") (as-str name))
            (workout/exercise-delete (workout/row-id row))
            ())))
    (workout/exercises sid))
  (unless (or (blank? (as-str reps)) (string-eq (as-str reps) "0"))
    (workout/exercise-log sid name load unit reps "" "")))

(define (workout/ksk-save-doc id doc)
  "Write doc back onto session row."
  (let ((cur (workout/session id)))
    (if (null? cur)
        ()
        (workout/session-update id
          (workout/session-fields
            (workout/session-date cur)
            (workout/session-program cur)
            (workout/session-intensity cur)
            (workout/session-state cur)
            (workout/session-notes cur)
            doc
            (workout/field cur "duration_sec")
            (workout/field cur "author"))))))

(define (workout/ksk-step session-id)
  "Complete current rung/set side; advance progress; upsert facts."
  (let ((doc (workout/parse-session-doc session-id)))
    (if (null? doc)
        ()
        (let* ((params (workout/doc-params doc))
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
               (ri2 (if is-sets ri
                       (if advance-rung
                           (if (>= (+ ri 1) n-rungs) 0 (+ ri 1))
                           ri)))
               (lad2 (if is-sets lad
                        (if advance-rung
                            (if (>= (+ ri 1) n-rungs) (+ lad 1) lad)
                            lad)))
               (sd2 (if is-sets
                        (if advance-rung (+ sd 1) sd)
                        sd))
               (prog2 (dict-set* "{}"
                        (list
                          "ladder" (as-str lad2)
                          "rung_index" (as-str ri2)
                          "side" next-side
                          "reps_l" (as-str rl2)
                          "reps_r" (as-str rr2)
                          "sets_done" (as-str sd2)
                          "started" (workout/pget prog "started")
                          "elapsed_sec" (workout/pget prog "elapsed_sec"))))
               (doc2 (dict-set doc "progress" prog2)))
          (seq
            (workout/ksk-save-doc session-id doc2)
            (workout/ksk-upsert-facts session-id doc2)
            doc2)))))

(define (workout/ksk-capture session-id)
  "Final project + mark session complete."
  (let ((doc (workout/parse-session-doc session-id)))
    (seq
      (unless (null? doc)
        (workout/ksk-upsert-facts session-id doc))
      (workout/session-complete session-id ""))))

;; --- historic log DSL (backfill / agent journal) ---
;;
;; Prefer program-specific loggers:
;;   (workout/log/ksk1 date phase day load unit rungs [notes])
;;   (workout/log/ksk2 date phase day load unit sets  [notes])
;;   (workout/log/ksk3 date week day load unit rm-total sets [notes])
;; rungs/sets = list of per-hand rep counts in order performed.
;; Both hands assumed equal (Snatch L and Snatch R get the same total).

(define (workout/sum xs)
  "Sum numeric list/pair-chain."
  (list/foldl
    (lambda (acc x) (+ acc (workout/as-int x)))
    0
    xs))

(define (workout/join-nums xs)
  "Space-join numbers for notes/detail."
  (list/foldl
    (lambda (acc x)
      (if (blank? acc)
          (as-str x)
          (string-append acc " " (as-str x))))
    ""
    xs))

(define (workout/log/ksk-write date program intensity doc load unit total-l total-r notes rungs-l-str rungs-r-str)
  "Write complete KSK session + L/R facts (totals may differ); return id."
  (let* ((tl (workout/as-int total-l))
         (tr (workout/as-int total-r))
         (rsl (as-str rungs-l-str))
         (rsr (as-str rungs-r-str))
         (prog0 (workout/doc-progress doc))
         (prog
           (dict-set* "{}"
             (list
               "ladder" (workout/pget prog0 "ladder")
               "rung_index" (workout/pget prog0 "rung_index")
               "side" "L"
               "reps_l" (as-str tl)
               "reps_r" (as-str tr)
               "sets_done" "0"
               "started" ""
               "elapsed_sec" ""
               "logged" "true"
               "rungs_l" rsl
               "rungs_r" rsr)))
         (n (if (blank? (as-str notes))
                (if (string-eq (as-str tl) (as-str tr))
                    (string-append
                      (as-str program) " log: " rsl
                      " @ " (as-str load) (as-str unit)
                      " (" (as-str tl) "/hand)")
                    (string-append
                      (as-str program) " log L:" rsl
                      " R:" rsr
                      " @ " (as-str load) (as-str unit)
                      " (" (as-str tl) "/" (as-str tr) ")"))
                (as-str notes)))
         (doc2 (dict-set doc "progress" prog))
         (sid
           (create "workout-session"
             (workout/session-fields
               (if (blank? (as-str date)) (workout/today) (as-str date))
               (as-str program)
               (as-str intensity)
               "complete"
               n
               doc2
               ""
               (as-str (whoami))))))
    (seq
      (unless (< tl 1)
        (workout/exercise-log sid "Snatch L" load unit tl "" rsl))
      (unless (< tr 1)
        (workout/exercise-log sid "Snatch R" load unit tr "" rsr))
      sid)))

(define (workout/log/ksk1 date phase day load unit rungs notes)
  "Log finished KSK 1.0 (both hands same rung list)."
  (let ((doc (workout/ksk1-plan phase day load unit))
        (total (workout/sum rungs))
        (rs (workout/join-nums rungs))
        (inten (string-append "p" (as-str phase) "d" (as-str day)))
        (n (if (null? notes) "" notes)))
    (workout/log/ksk-write date "ksk1" inten doc load unit
      total total n rs rs)))

(define (workout/log/ksk1-sides date phase day load unit rungs-l rungs-r notes)
  "Log KSK 1.0 with different L/R rung lists (missed reps, fail mid-ladder)."
  (let ((doc (workout/ksk1-plan phase day load unit))
        (tl (workout/sum rungs-l))
        (tr (workout/sum rungs-r))
        (rsl (workout/join-nums rungs-l))
        (rsr (workout/join-nums rungs-r))
        (inten (string-append "p" (as-str phase) "d" (as-str day)))
        (n (if (null? notes) "" notes)))
    (workout/log/ksk-write date "ksk1" inten doc load unit
      tl tr n rsl rsr)))

(define (workout/log/ksk2 date phase day load unit sets notes)
  "Log finished KSK 2.0 session. sets = per-hand set sizes in order (10 10 10 …)."
  (let ((doc (workout/ksk2-plan phase day load unit))
        (total (workout/sum sets))
        (rs (workout/join-nums sets))
        (inten (string-append "p" (as-str phase) "d" (as-str day)))
        (n (if (null? notes) "" notes)))
    (workout/log/ksk-write date "ksk2" inten doc load unit
      total total n rs rs)))

(define (workout/log/ksk3 date week day load unit rm-total sets notes)
  "Log finished KSK 3.0 session. sets = per-hand set sizes; rm-total = L+R test sum."
  (let ((doc (workout/ksk3-plan week day load unit rm-total))
        (total (workout/sum sets))
        (rs (workout/join-nums sets))
        (inten (string-append "w" (as-str week) "d" (as-str day)))
        (n (if (null? notes) "" notes)))
    (workout/log/ksk-write date "ksk3" inten doc load unit
      total total n rs rs)))

(define (workout/insert-session-desc sorted row)
  "Insert into date-desc list."
  (if (null? sorted)
      (list row)
      (let ((d (workout/session-date (workout/row-fields row)))
            (hd (workout/session-date (workout/row-fields (car sorted)))))
        (if (workout/date-ge? d hd)
            (cons row sorted)
            (cons (car sorted)
                  (workout/insert-session-desc (cdr sorted) row))))))

(define (workout/sessions-sorted)
  "Sessions newest-date first."
  (list/foldl workout/insert-session-desc () (workout/session-list 100)))

;; --- HTML ---

(define (workout/state-badge state)
  "State label."
  (html/span
    (html/attr "class" "muted")
    (html/text (as-str state))))

(define (workout/session-title f)
  "Human title for a session."
  (let ((date (workout/session-date f))
        (prog (workout/session-program f))
        (inten (workout/session-intensity f)))
    (if (blank? inten)
        (string-append date " · " prog)
        (string-append date " · " prog " / " inten))))

(define (workout/session-li row)
  "Recent session list item."
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

(define (workout/continue-card)
  "Continue in_progress card or empty string."
  (let ((id (workout/session-in-progress ())))
    (if (null? id)
        ""
        (let ((f (workout/session id)))
          (if (null? f)
              ""
              (let ((prog (workout/session-program f))
                    (run-href (string-append "/workout/run?id=" id))
                    (sess-href (string-append "/workout/session?id=" id)))
                (html/div (html/attr "class" "card")
                  (html/join
                    (list
                      (html/h2 "Continue")
                      (html/p
                        (string-append
                          (workout/session-title f) " is in progress."))
                      (html/p* ""
                        (html/join
                          (list
                            (if (workout/ksk-program? prog)
                                (html/a run-href "Resume run")
                                "")
                            (if (workout/ksk-program? prog)
                                (html/text " · ")
                                "")
                            (html/a sess-href "Open session")))))))))))))

(define (workout/start-bar)
  "Start free or KSK session controls."
  (html/div (html/attr "class" "card")
    (html/join
      (list
        (html/h2 "Start workout")
        (html/form "POST" "/workout" ""
          (html/join
            (list
              (html/hidden "action" "start")
              (html/hidden "program" "free")
              (html/label (html/attr "for" "w-inten") "Note (optional)")
              (html/text-input "intensity"
                (html/attrs
                  (list
                    (html/attr "id" "w-inten")
                    (html/attr "placeholder" "optional tag"))))
              (html/button (html/attr "type" "submit")
                "Start free session"))))
        (html/h3 "King-Sized Killer")
        (html/p* (html/attr "class" "muted")
          (html/text
            "Single-bell snatch programs (1.0 ladders, 2.0 high sets, 3.0 %RM)."))
        (html/form "POST" "/workout" ""
          (html/join
            (list
              (html/hidden "action" "start-ksk")
              (html/label (html/attr "for" "ksk-prog") "Program")
              (html/select "program"
                (html/attr "id" "ksk-prog")
                (html/join
                  (list
                    (html/option "ksk1" "KSK 1.0" #t)
                    (html/option "ksk2" "KSK 2.0" #f)
                    (html/option "ksk3" "KSK 3.0" #f))))
              (html/label (html/attr "for" "ksk-phase") "Phase (1.0/2.0)")
              (html/text-input "phase"
                (html/attrs
                  (list
                    (html/attr "id" "ksk-phase")
                    (html/attr "value" "1")
                    (html/attr "inputmode" "numeric"))))
              (html/label (html/attr "for" "ksk-week") "Week (3.0)")
              (html/text-input "week"
                (html/attrs
                  (list
                    (html/attr "id" "ksk-week")
                    (html/attr "value" "1")
                    (html/attr "inputmode" "numeric"))))
              (html/label (html/attr "for" "ksk-day") "Day 1–3")
              (html/text-input "day"
                (html/attrs
                  (list
                    (html/attr "id" "ksk-day")
                    (html/attr "value" "1")
                    (html/attr "inputmode" "numeric"))))
              (html/label (html/attr "for" "ksk-load") "Load")
              (html/text-input "load"
                (html/attrs
                  (list
                    (html/attr "id" "ksk-load")
                    (html/attr "placeholder" "24")
                    (html/attr "required" "required"))))
              (html/label (html/attr "for" "ksk-unit") "Unit")
              (html/text-input "unit"
                (html/attrs
                  (list
                    (html/attr "id" "ksk-unit")
                    (html/attr "value" "kg"))))
              (html/label (html/attr "for" "ksk-rm")
                "RM total (3.0 — L+R max test)")
              (html/text-input "rm_total"
                (html/attrs
                  (list
                    (html/attr "id" "ksk-rm")
                    (html/attr "placeholder" "50")
                    (html/attr "inputmode" "numeric"))))
              (html/button (html/attr "type" "submit") "Start KSK run"))))
        (html/p* (html/attr "class" "muted")
          (html/join
            (list
              (html/text "Or ")
              (html/a "/workout/log" "quick log")
              (html/text " a completed free workout."))))))))

(define (workout/recent-list)
  "Recent sessions."
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

(define (workout/home-main)
  "Home body."
  (html/div (html/attr "id" "workout-main")
    (html/join
      (list
        (workout/continue-card)
        (workout/start-bar)
        (workout/recent-list)))))

(define (workout/exercise-li row)
  "One exercise fact."
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

(define (workout/add-exercise-form session-id)
  "Append exercise form."
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

(define (workout/session-actions id state)
  "Complete/abandon/delete controls."
  (let ((f (workout/session id))
        (prog (if (null? (workout/session id)) ""
                  (workout/session-program (workout/session id)))))
  (html/div (html/attr "class" "card")
    (html/join
      (list
        (html/h2 "Session")
        (if (and (string-eq (as-str state) "in_progress")
                 (workout/ksk-program? prog))
            (html/p* ""
              (html/a (string-append "/workout/run?id=" (as-str id))
                "→ Open run UI"))
            "")
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
        (html/p* "" (html/a "/workout" "← All workouts")))))))

(define (workout/session-main id)
  "Session detail panel."
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

(define (workout/log-form)
  "Quick-log form."
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

;; --- POST handlers ---

(define (workout/handle-home-post)
  "POST /workout."
  (let ((action (as-str (request-param "action"))))
    (cond
      ((string-eq action "start")
       (let ((id (workout/session-create
                   (request-param "program")
                   (request-param "intensity"))))
         (redirect (string-append "/workout/session?id=" (as-str id)))))
      ((string-eq action "start-ksk")
       (let ((id (workout/ksk-start
                   (request-param "program")
                   (request-param "phase")
                   (request-param "day")
                   (request-param "week")
                   (request-param "load")
                   (request-param "unit")
                   (request-param "rm_total"))))
         (if (null? id)
             (redirect "/workout")
             (redirect (string-append "/workout/run?id=" (as-str id))))))
      (else (redirect "/workout")))))

(define (workout/handle-session-post)
  "POST /workout/session."
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
         (if (workout/ksk-program?
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
       (seq
         (workout/session-delete id)
         (redirect "/workout")))
      (else (redirect "/workout")))))

(define (workout/maybe-log sid name weight unit reps)
  "Log exercise if name set."
  (if (blank? (as-str name))
      ()
      (workout/exercise-log sid name weight unit reps "" "")))

(define (workout/handle-log-post)
  "POST /workout/log."
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

;; --- pages ---

(define (workout/home-page)
  "GET/POST /workout."
  (if (string-eq (request-method) "POST")
      (workout/handle-home-post)
      (html/page
        (list
          (html/h1 "Workout")
          (html/p* (html/attr "class" "muted")
            (html/text
              "Training journal — free log + King-Sized Killer (snatch)."))
          (workout/home-main)))))

(define (workout/session-page)
  "GET/POST /workout/session?id=."
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

(define (workout/log-page)
  "GET/POST /workout/log."
  (if (string-eq (request-method) "POST")
      (workout/handle-log-post)
      (html/page
        (list
          (html/h1 "Quick log")
          (workout/log-form)))))

(define (workout/timer-script cap-min)
  "Client countdown timer for KSK cap minutes."
  (html/tag "script" ""
    (string-append
      "(function(){"
      "var el=document.getElementById('ksk-timer');"
      "if(!el)return;"
      "var cap=" (as-str (workout/as-int cap-min)) "*60;"
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

(define (workout/run-style)
  "Inline CSS for run page."
  (html/tag "style" ""
    (html/join
      (list
        ".ksk-run .ksk-timer{font-size:2.5rem;font-weight:700;letter-spacing:.05em}"
        ".ksk-run .ksk-target{font-size:2rem;font-weight:700;margin:.5rem 0}"
        ".ksk-run .ksk-side{font-size:1.4rem}"
        ".ksk-run form button{font-size:1.2rem;padding:.75rem 1.25rem;margin:.35rem .35rem .35rem 0}"
        ".ksk-run .ksk-totals{font-size:1.1rem;margin-top:.75rem}"))))

(define (workout/run-main id)
  "KSK live run panel."
  (let ((f (workout/session id)))
    (if (null? f)
        (html/div ""
          (html/join
            (list
              (html/p "Session not found.")
              (html/p* "" (html/a "/workout" "← Back")))))
        (let ((doc (workout/parse-session-doc id))
              (st (workout/session-state f))
              (prog-name (workout/session-program f)))
          (if (not (workout/ksk-program? prog-name))
              (html/div ""
                (html/join
                  (list
                    (html/p "Not a KSK session.")
                    (html/p* ""
                      (html/a (string-append "/workout/session?id=" id)
                        "Open session")))))
              (let ((params (workout/doc-params doc))
                    (prog (workout/doc-progress doc))
                    (target (workout/ksk-current-target doc))
                    (side (workout/pget (workout/doc-progress doc) "side"))
                    (cap (workout/pget (workout/doc-params doc) "time_cap_min"))
                    (kind (workout/pget (workout/doc-params doc) "kind"))
                    (pat (workout/pget (workout/doc-params doc) "pattern"))
                    (rl (workout/pget (workout/doc-progress doc) "reps_l"))
                    (rr (workout/pget (workout/doc-progress doc) "reps_r"))
                    (lad (workout/pget (workout/doc-progress doc) "ladder"))
                    (sd (workout/pget (workout/doc-progress doc) "sets_done"))
                    (ri (workout/pget (workout/doc-progress doc) "rung_index")))
                (html/div (html/attr "class" "ksk-run")
                  (html/join
                    (list
                      (workout/run-style)
                      (html/h1 (html/text (workout/ksk-label doc)))
                      (html/p* (html/attr "class" "muted")
                        (html/text (string-append "state: " st)))
                      (html/div
                        (html/attrs
                          (list
                            (html/attr "id" "ksk-timer")
                            (html/attr "class" "ksk-timer")
                            (html/attr "data-sid" id)))
                        (html/text "--:--"))
                      (workout/timer-script cap)
                      (html/p* (html/attr "class" "ksk-side")
                        (html/text
                          (string-append "Hand: " side
                            (if (string-eq kind "ladder")
                                (string-append " · rung #"
                                  (as-str (+ (workout/as-int ri) 1))
                                  " · ladder " lad)
                                (string-append " · sets done " sd)))))
                      (html/p* (html/attr "class" "ksk-target")
                        (html/text
                          (string-append (as-str target)
                            " snatches (" side ")")))
                      (html/p* (html/attr "class" "muted")
                        (html/text
                          (string-append "pattern " pat
                            " · per-rung both hands")))
                      (if (string-eq st "in_progress")
                          (html/join
                            (list
                              (html/form "POST" "/workout/run" ""
                                (html/join
                                  (list
                                    (html/hidden "action" "ksk-step")
                                    (html/hidden "id" id)
                                    (html/button (html/attr "type" "submit")
                                      (string-append "Done "
                                        (as-str target) " " side)))))
                              (html/form "POST" "/workout/run" ""
                                (html/join
                                  (list
                                    (html/hidden "action" "ksk-capture")
                                    (html/hidden "id" id)
                                    (html/button (html/attr "type" "submit")
                                      "Finish session"))))
                              (html/form "POST" "/workout/run"
                                (html/attr "style" "display:inline")
                                (html/join
                                  (list
                                    (html/hidden "action" "abandon")
                                    (html/hidden "id" id)
                                    (html/button (html/attr "type" "submit")
                                      "Abandon"))))))
                          "")
                      (html/p* (html/attr "class" "ksk-totals")
                        (html/text
                          (string-append "Totals — L: " rl " · R: " rr
                            " · all: "
                            (as-str (+ (workout/as-int rl)
                                       (workout/as-int rr))))))
                      (html/p* ""
                        (html/join
                          (list
                            (html/a
                              (string-append "/workout/session?id=" id)
                              "Session detail")
                            (html/text " · ")
                            (html/a "/workout" "All workouts")))))))))))))

(define (workout/handle-run-post)
  "POST /workout/run."
  (let ((action (as-str (request-param "action")))
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

(define (workout/run-page)
  "GET/POST /workout/run?id=."
  (if (string-eq (request-method) "POST")
      (workout/handle-run-post)
      (let ((id (as-str (request-param "id"))))
        (html/page
          (list
            (if (blank? id)
                (html/join
                  (list
                    (html/h1 "Run")
                    (html/p "Missing session id.")
                    (html/p* "" (html/a "/workout" "← Back"))))
                (workout/run-main id)))))))

(route "/workout" "workout/home-page")
(route "/workout/session" "workout/session-page")
(route "/workout/log" "workout/log-page")
(route "/workout/run" "workout/run-page")
