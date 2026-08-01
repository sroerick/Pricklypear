;; package calendar -- exported from live branch system/main
;; Sync: scripts/pp_sync.py pull|push

(define calendar/add-month
  (lambda
    (ym delta)
    (let
      ((y (parse-int (substring (as-str ym) 0 4))))
      (let
        ((m (parse-int (substring (as-str ym) 5 2))))
        (let
          ((m2 (+ m delta)))
          (if
            (> m2 12)
            (string-append
              (as-str (+ y 1))
              "-"
              (calendar/pad2 (- m2 12)))
            (if
              (< m2 1)
              (string-append
                (as-str (- y 1))
                "-"
                (calendar/pad2 (+ m2 12)))
              (string-append (as-str y) "-" (calendar/pad2 m2)))))))))

(define calendar/agenda
  (lambda
    ()
    (let
      ((today (date-today)))
      (let
        ((weekday-names
          (list
            "Sun"
            "Mon"
            "Tue"
            "Wed"
            "Thu"
            "Fri"
            "Sat")))
        (let
          ((days
            (list
              today
              (date-add-days today 1)
              (date-add-days today 2)
              (date-add-days today 3)
              (date-add-days today 4)
              (date-add-days today 5)
              (date-add-days today 6))))
          (list/map
            (lambda
              (d)
              (let
                ((y (parse-int (substring d 0 4)))
                  (m (parse-int (substring d 5 2)))
                  (day (parse-int (substring d 8 2))))
                (let
                  ((w (calendar/weekday y m day)))
                  (list
                    (list-ref weekday-names w)
                    d
                    (calendar/events-for-day d)
                    (calendar/todos-due-on d)))))
            days))))))

(define calendar/agenda-widget
  (lambda
    ()
    (let
      ((month-names
        (list
          "Jan"
          "Feb"
          "Mar"
          "Apr"
          "May"
          "Jun"
          "Jul"
          "Aug"
          "Sep"
          "Oct"
          "Nov"
          "Dec")))
      (let
        ((rows (calendar/agenda)))
        (layout/card
          "Agenda"
          (html/div
            (html/attr "id" "agenda-widget")
            (html/join
              (list/map
                (lambda
                  (row)
                  (let
                    ((day-name (car row))
                      (date (car (cdr row)))
                      (events (car (cdr (cdr row))))
                      (todos (car (cdr (cdr (cdr row))))))
                    (let
                      ((m-str (substring date 5 2))
                        (d-str (substring date 8 2)))
                      (let
                        ((month (nth month-names (- (parse-int m-str) 1))))
                        (html/div
                          (html/attr "class" "agenda-day")
                          (html/join
                            (list
                              (html/div
                                (html/attr "class" "agenda-day-header")
                                (html/strong
                                  (string-append day-name " " month " " d-str)))
                              (if
                                (null? events)
                                (html/p*
                                  (html/attr "class" "muted")
                                  "nothing scheduled")
                                (html/ul
                                  (list)
                                  (html/join
                                    (list/map
                                      (lambda
                                        (ev)
                                        (let
                                          ((f (cdr ev)))
                                          (let
                                            ((t (calendar/event-time-string f)))
                                            (html/li
                                              (list)
                                              (if
                                                (string-eq t "")
                                                (html/text (calendar/event-title f))
                                                (html/join
                                                  (list
                                                    (html/span
                                                      (html/attr
                                                        "style"
                                                        "font-weight: bold; color: gray; margin-right: 0.5em;")
                                                      (html/text t))
                                                    (html/text (calendar/event-title f)))))))))
                                      events)))))))))))
                rows))))))))

(define calendar/all-events
  (lambda () (rows (quote calendar-event) 500)))

(define calendar/build-cells
  (lambda
    (y
      m
      dim
      start-wd
      today
      ym
      d
      acc)
    (if
      (> d dim)
      acc
      (calendar/build-cells
        y
        m
        dim
        start-wd
        today
        ym
        (+ d 1)
        (string-append acc (calendar/day-cell y m d today ym))))))

(define calendar/cell-marks
  (lambda
    (day)
    (let
      ((ne (calendar/count-events-on day)))
      (let
        ((nt (calendar/count-todos-on day)))
        (let
          ((daily (calendar/has-daily? day)))
          (html/div
            (html/attr "class" "cal-marks")
            (html/join
              (list
                (if
                  (> ne 0)
                  (html/span
                    (html/attr "class" "cal-dot cal-dot-ev")
                    (html/text (as-str ne)))
                  "")
                (if
                  (> nt 0)
                  (html/span
                    (html/attr "class" "cal-dot cal-dot-todo")
                    (html/text (as-str nt)))
                  "")
                (if
                  daily
                  (html/span
                    (html/attr "class" "cal-dot cal-dot-note")
                    (html/text "n"))
                  "")))))))))

(define calendar/count-events-on
  (lambda
    (day)
    (list/foldl
      (lambda (acc _) (+ acc 1))
      0
      (calendar/events-on-day day))))

(define calendar/count-todos-on
  (lambda
    (day)
    (list/foldl
      (lambda (acc _) (+ acc 1))
      0
      (calendar/todos-due-on day))))

(define calendar/create
  (lambda
    (title day start notes)
    (let
      ((st (if (null? start) "" (as-str start))))
      (let
        ((all (if (string-eq st "") "true" "false")))
        (create
          (quote calendar-event)
          (dict-set*
            "{}"
            (list
              "title"
              (as-str title)
              "day"
              (as-str day)
              "start_time"
              st
              "end_time"
              ""
              "all_day"
              all
              "notes"
              (as-str notes)
              "location"
              ""
              "color"
              ""
              "source"
              "calendar")))))))

(define calendar/cron-field-matches?
  (lambda
    (field val)
    (if
      (string-eq field "*")
      #t
      (if
        (string-contains? field ",")
        (let
          ((vals (string-split field ",")))
          (list/foldl
            (lambda (acc v) (or acc (= val (parse-int v))))
            #f
            vals))
        (if
          (string-contains? field "-")
          (let*
            ((parts (string-split field "-"))
              (lo (parse-int (car parts)))
              (hi (parse-int (car (cdr parts)))))
            (and (>= val lo) (<= val hi)))
          (let ((field-val (parse-int field))) (= val field-val)))))))

(define calendar/cron-matches?
  (lambda
    (date-str cron-expr)
    (let*
      ((parts (string-split cron-expr " "))
        (min-field (car parts))
        (hr-field (car (cdr parts)))
        (dom-field (car (cdr (cdr parts))))
        (mon-field (car (cdr (cdr (cdr parts)))))
        (dow-field (car (cdr (cdr (cdr (cdr parts)))))))
      (let*
        ((y (parse-int (substring date-str 0 4)))
          (m (parse-int (substring date-str 5 2)))
          (d (parse-int (substring date-str 8 2)))
          (dow (calendar/weekday y m d)))
        (and
          (calendar/cron-field-matches? mon-field m)
          (let
            ((dom-specified (not (string-eq dom-field "*")))
              (dow-specified (not (string-eq dow-field "*"))))
            (if
              (and dom-specified dow-specified)
              (or
                (calendar/cron-field-matches? dom-field d)
                (calendar/cron-field-matches? dow-field dow))
              (if
                dom-specified
                (calendar/cron-field-matches? dom-field d)
                (if
                  dow-specified
                  (calendar/cron-field-matches? dow-field dow)
                  #t)))))))))

(define calendar/cron-to-event
  (lambda
    (rec)
    (let
      ((cron (car rec))
        (title (car (cdr rec)))
        (time (car (cdr (cdr rec)))))
      (cons
        ""
        (string-append
          "{\"title\": \""
          title
          "\", \"start_time\": \""
          time
          "\", \"cron\": \""
          cron
          "\", \"all_day\": \"false\"}")))))

(define calendar/day-cell
  (lambda
    (y m d today ym)
    (if
      (= d 0)
      (html/div (html/attr "class" "cal-cell cal-empty") "")
      (let
        ((day (calendar/ymd y m d)))
        (html/a
          (string-append "/calendar?day=" day)
          (html/div
            (html/attr
              "class"
              (if
                (calendar/day-eq? day today)
                "cal-cell cal-today"
                "cal-cell"))
            (html/join
              (list
                (html/span
                  (html/attr "class" "cal-dom")
                  (html/text (as-str d)))
                (calendar/cell-marks day)))))))))

(define calendar/day-eq?
  (lambda (a b) (string-eq (as-str a) (as-str b))))

(define calendar/day-html
  (lambda
    (day)
    (html/div
      (html/attr "class" "cal-day-panel")
      (html/join
        (list
          (html/h2 (html/text day))
          (html/p*
            (html/attr "class" "muted")
            (html/join
              (list
                (html/a
                  (string-append "/calendar?ym=" (calendar/ym day))
                  "← month")
                " · "
                (html/a
                  (string-append "/notes?slug=daily-" day)
                  "daily note"))))
          (html/h3 (html/text "Events"))
          (html/ul
            (html/attr "class" "card cal-list")
            (list/foldl
              (lambda
                (acc row)
                (string-append acc (calendar/event-li row)))
              ""
              (calendar/events-on-day day)))
          (html/h3 (html/text "Due todos"))
          (html/ul
            (html/attr "class" "card cal-list")
            (list/foldl
              (lambda
                (acc row)
                (string-append acc (calendar/todo-due-li row)))
              ""
              (calendar/todos-due-on day)))
          (html/div
            (html/attr "class" "card")
            (html/join
              (list
                (html/h3 (html/text "Add event"))
                (html/form
                  "POST"
                  "/calendar"
                  (html/attrs
                    (list
                      (html/attr "hx-post" "/calendar")
                      (html/attr "hx-target" "#cal-main")
                      (html/attr "hx-swap" "innerHTML")
                      (html/attr
                        "hx-on::after-request"
                        "if(event.detail.successful){this.reset()}")))
                  (html/join
                    (list
                      (html/hidden "action" "create")
                      (html/hidden "day" day)
                      (html/label (html/attr "for" "title") "Title")
                      (html/text-input
                        "title"
                        (html/attrs
                          (list
                            (html/attr "id" "title")
                            (html/attr "required" "required")
                            (html/attr "placeholder" "standup"))))
                      (html/label
                        (html/attr "for" "start_time")
                        "Start (HH:MM, blank=all day)")
                      (html/text-input
                        "start_time"
                        (html/attrs
                          (list
                            (html/attr "id" "start_time")
                            (html/attr "placeholder" "10:00"))))
                      (html/label (html/attr "for" "notes") "Notes")
                      (html/text-input
                        "notes"
                        (html/attrs
                          (list
                            (html/attr "id" "notes")
                            (html/attr "placeholder" ""))))
                      (html/button (html/attr "type" "submit") "Add event"))))))))))))

(define calendar/day-prefix?
  (lambda
    (day ym)
    (string-eq (substring (as-str day) 0 7) (as-str ym))))

(define calendar/days-in-month
  (lambda
    (y m)
    (if
      (= m 2)
      (if (calendar/leap? y) 29 28)
      (if
        (= m 4)
        30
        (if (= m 6) 30 (if (= m 9) 30 (if (= m 11) 30 31)))))))

(define calendar/delete
  (lambda (id) (delete (quote calendar-event) id)))

(define calendar/dom
  (lambda (day) (parse-int (substring (as-str day) 8 2))))

(define calendar/event-all-day?
  (lambda
    (f)
    (let
      ((a (dict-get f "all_day")))
      (if
        (null? a)
        (string-eq (calendar/event-start f) "")
        (string-eq (as-str a) "true")))))

(define calendar/event-day
  (lambda (f) (as-str (dict-get f "day"))))

(define calendar/event-li
  (lambda
    (row)
    (let
      ((id (car row)))
      (let
        ((f (cdr row)))
        (html/li
          (html/attr "class" "cal-event")
          (html/join
            (list
              (html/span
                (html/attr "class" "cal-time")
                (html/text
                  (if
                    (calendar/event-all-day? f)
                    "all day"
                    (calendar/event-start f))))
              (html/span
                (html/attr "class" "cal-ev-title")
                (html/text (calendar/event-title f)))
              (html/form
                "POST"
                "/calendar"
                (html/attrs
                  (list
                    (html/attr "class" "inline-form")
                    (html/attr "hx-post" "/calendar")
                    (html/attr "hx-target" "#cal-main")
                    (html/attr "hx-swap" "innerHTML")))
                (html/join
                  (list
                    (html/hidden "action" "delete")
                    (html/hidden "id" id)
                    (html/hidden "day" (calendar/event-day f))
                    (html/button
                      (html/attrs
                        (list
                          (html/attr "type" "submit")
                          (html/attr "class" "secondary ol-btn")))
                      "del")))))))))))

(define calendar/event-start
  (lambda
    (f)
    (let
      ((s (dict-get f "start_time")))
      (if (null? s) "" (as-str s)))))

(define calendar/event-time-string
  (lambda
    (f)
    (let
      ((s (as-str (dict-get f "start_time")))
        (e (as-str (dict-get f "end_time"))))
      (if
        (string-eq s "")
        ""
        (if
          (string-contains? s "-")
          s
          (if (string-eq e "") s (string-append s " - " e)))))))

(define calendar/event-title
  (lambda (f) (as-str (dict-get f "title"))))

(define calendar/events-for-day
  (lambda
    (d)
    (let
      ((regular (calendar/events-on-day d))
        (recurring (calendar/recurring-events)))
      (let
        ((cron-evs
          (list/filter
            (lambda (rec) (calendar/cron-matches? d (car rec)))
            recurring)))
        (append
          regular
          (list/map calendar/cron-to-event cron-evs))))))

(define calendar/events-in-month
  (lambda
    (ym)
    (list/filter
      (lambda
        (row)
        (calendar/day-prefix? (calendar/event-day (cdr row)) ym))
      (calendar/all-events))))

(define calendar/events-on-day
  (lambda
    (day)
    (list/filter
      (lambda
        (row)
        (calendar/day-eq? (calendar/event-day (cdr row)) day))
      (calendar/all-events))))

(define calendar/foo
  (lambda () (date-today)))

(define calendar/handle-post
  (lambda
    ()
    (let
      ((action (as-str (request-param "action"))))
      (cond
        ((string-eq action "create")
          (calendar/create
            (request-param "title")
            (request-param "day")
            (request-param "start_time")
            (request-param "notes")))
        ((string-eq action "delete")
          (calendar/delete (request-param "id")))
        (else ())))))

(define calendar/has-daily?
  (lambda
    (day)
    (let
      ((slug (string-append "daily-" (as-str day))))
      (if (null? (notes/doc/get slug)) #f #t))))

(define calendar/leap?
  (lambda
    (y)
    (if
      (= (mod y 400) 0)
      #t
      (if (= (mod y 100) 0) #f (= (mod y 4) 0)))))

(define calendar/main-html
  (lambda
    ()
    (let
      ((day (request-param "day")) (ym (request-param "ym")))
      (html/div
        (html/attr "id" "cal-main")
        (if
          (null? day)
          (calendar/month-html
            (if
              (null? ym)
              (calendar/ym (calendar/today))
              (as-str ym)))
          (calendar/day-html (as-str day)))))))

(define calendar/month
  (lambda (day) (parse-int (substring (as-str day) 5 2))))

(define calendar/month-grid
  (lambda
    (ym)
    (let
      ((y (parse-int (substring (as-str ym) 0 4))))
      (let
        ((m (parse-int (substring (as-str ym) 5 2))))
        (let
          ((today (calendar/today)))
          (let
            ((dim (calendar/days-in-month y m)))
            (let
              ((wd (calendar/weekday y m 1)))
              (html/div
                (html/attr "class" "cal-grid")
                (html/join
                  (list
                    (html/div
                      (html/attr "class" "cal-dow")
                      (html/text "Su"))
                    (html/div
                      (html/attr "class" "cal-dow")
                      (html/text "Mo"))
                    (html/div
                      (html/attr "class" "cal-dow")
                      (html/text "Tu"))
                    (html/div
                      (html/attr "class" "cal-dow")
                      (html/text "We"))
                    (html/div
                      (html/attr "class" "cal-dow")
                      (html/text "Th"))
                    (html/div
                      (html/attr "class" "cal-dow")
                      (html/text "Fr"))
                    (html/div
                      (html/attr "class" "cal-dow")
                      (html/text "Sa"))
                    (calendar/pad-start wd "")
                    (calendar/build-cells
                      y
                      m
                      dim
                      wd
                      today
                      ym
                      1
                      "")))))))))))

(define calendar/month-html
  (lambda
    (ym)
    (html/div
      (html/attr "class" "cal-month")
      (html/join
        (list
          (calendar/month-nav ym)
          (calendar/month-grid ym)
          (html/p*
            (html/attr "class" "muted")
            "Dots: events · todos due · daily note. Click a day for agenda."))))))

(define calendar/month-nav
  (lambda
    (ym)
    (let
      ((prev (calendar/add-month ym -1)))
      (let
        ((next (calendar/add-month ym 1)))
        (html/div
          (html/attr "class" "cal-nav")
          (html/join
            (list
              (html/a
                (string-append "/calendar?ym=" prev)
                "◀")
              (html/strong ym)
              (html/a
                (string-append "/calendar?ym=" next)
                "▶")
              (html/a "/calendar" "Today"))))))))

(define calendar/pad-start
  (lambda
    (n acc)
    (if
      (<= n 0)
      acc
      (calendar/pad-start
        (- n 1)
        (string-append
          acc
          (html/div (html/attr "class" "cal-cell cal-empty") ""))))))

(define calendar/pad2
  (lambda
    (n)
    (if (< n 10) (string-append "0" (as-str n)) (as-str n))))

(define calendar/page
  (lambda
    ()
    (if
      (string-eq (request-method) "POST")
      (seq
        (calendar/handle-post)
        (if
          (hx-request?)
          (render-html
            (let
              ((day (request-param "day")))
              (if
                (null? day)
                (calendar/main-html)
                (html/div
                  (html/attr "id" "cal-main")
                  (calendar/day-html (as-str day))))))
          (let
            ((day (request-param "day")))
            (if
              (null? day)
              (redirect "/calendar")
              (redirect (string-append "/calendar?day=" (as-str day)))))))
      (html/page
        (list
          (html/h1 "Calendar")
          (html/p*
            (html/attr "class" "muted")
            "Month grid · day agenda · events + due todos + daily notes")
          (calendar/main-html))))))

(define calendar/recurring-events
  (lambda
    ()
    (list
      (list "30 7 * * 1-4" "Taylor Dental" "07:30-17:00")
      (list "0 17 * * 2-4" "Cassie's" "17:00-23:00"))))

(define calendar/today
  (lambda () (date-today)))

(define calendar/todo-due-li
  (lambda
    (row)
    (let
      ((id (car row)))
      (let
        ((f (cdr row)))
        (html/li
          (html/attr "class" "cal-todo")
          (html/join
            (list
              (html/span
                (html/attr "class" "cal-time")
                (html/text "todo"))
              (html/a
                "/todos"
                (html/text (as-str (dict-get f "title")))))))))))

(define calendar/todos-due-on
  (lambda
    (day)
    (list/filter
      (lambda
        (row)
        (let
          ((due (dict-get (cdr row) "due")))
          (if (null? due) #f (calendar/day-eq? due day))))
      (todo/item/all))))

(define calendar/weekday
  (lambda (y m d) (date-weekday y m d)))

(define calendar/year
  (lambda (day) (parse-int (substring (as-str day) 0 4))))

(define calendar/ym
  (lambda (day) (substring (as-str day) 0 7)))

(define calendar/ymd
  (lambda (y m d) (date-format y m d)))

