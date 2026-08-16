;; package workout/log -- exported from live branch system/main
;; Sync: scripts/pp_sync.py pull|push

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
    "Log a completed workout session, writing snatch exercise rows."
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
        sid))))

(define workout/log/ksk1
  (lambda
    (date
      phase
      day
      load
      unit
      rungs
      notes)
    "Log a ksk1 ladder workout with equal totals for both hands."
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
        rs))))

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
    "Log a ksk1 workout with separate left and right rung totals."
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
        rsr))))

(define workout/log/ksk2
  (lambda
    (date
      phase
      day
      load
      unit
      sets
      notes)
    "Log a ksk2 workout from its set list."
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
        rs))))

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
    "Log a ksk3 workout from its week/day and set list."
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
        rs))))

