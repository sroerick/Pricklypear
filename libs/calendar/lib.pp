;; Calendar — month + day views; events + todo dues + daily notes.
;; Naive local dates (YYYY-MM-DD). Seeded after todo.
;; (load-library "calendar")

(deftype 'calendar-event
  'title 'day 'start_time 'end_time 'all_day 'notes 'location 'color 'source)

;; --- date helpers ---

(define calendar/today (lambda () (date-today))
  "Today date string YYYY-MM-DD.")

(define calendar/year
  (lambda (day) (parse-int (substring (as-str day) 0 4)))
  "Year int from YYYY-MM-DD string.")

(define calendar/month
  (lambda (day) (parse-int (substring (as-str day) 5 2)))
  "Month int from YYYY-MM-DD string.")

(define calendar/dom
  (lambda (day) (parse-int (substring (as-str day) 8 2)))
  "Day-of-month int from YYYY-MM-DD string.")

(define calendar/ym
  (lambda (day) (substring (as-str day) 0 7))
  "YYYY-MM prefix string from date.")

(define calendar/leap?
  (lambda (y)
    (if (= (mod y 400) 0)
        #t
        (if (= (mod y 100) 0)
            #f
            (= (mod y 4) 0))))
  "True if year is leap year.")

(define calendar/days-in-month
  (lambda (y m)
    (if (= m 2)
        (if (calendar/leap? y) 29 28)
        (if (= m 4) 30
          (if (= m 6) 30
            (if (= m 9) 30
              (if (= m 11) 30 31))))))
  "Number of days in year/month.")

(define calendar/pad2
  (lambda (n)
    (if (< n 10)
        (string-append "0" (as-str n))
        (as-str n)))
  "Zero-pad int to two digits.")

(define calendar/ymd
  (lambda (y m d) (date-format y m d))
  "Build YYYY-MM-DD from year month day.")

(define calendar/add-month
  (lambda (ym delta)
    (let ((y (parse-int (substring (as-str ym) 0 4))))
      (let ((m (parse-int (substring (as-str ym) 5 2))))
        (let ((m2 (+ m delta)))
          (if (> m2 12)
              (string-append (as-str (+ y 1)) "-" (calendar/pad2 (- m2 12)))
              (if (< m2 1)
                  (string-append (as-str (- y 1)) "-" (calendar/pad2 (+ m2 12)))
                  (string-append (as-str y) "-" (calendar/pad2 m2))))))))
  "Add delta months to YYYY-MM-DD; return YMD in month.")

(define calendar/weekday
  (lambda (y m d) (date-weekday y m d))
  "Weekday index for date (via host prim if any).")

(define calendar/day-eq?
  (lambda (a b) (string-eq (as-str a) (as-str b)))
  "True if two date strings equal.")

(define calendar/day-prefix?
  (lambda (day ym)
    (string-eq (substring (as-str day) 0 7) (as-str ym)))
  "True if date starts with prefix (month filter).")

;; --- events CRUD ---

(define calendar/all-events
  (lambda () (rows 'calendar-event 500))
  "Up to N calendar-event rows.")

(define calendar/event-title
  (lambda (f) (as-str (dict-get f "title")))
  "Event title from fields.")

(define calendar/event-day
  (lambda (f) (as-str (dict-get f "day")))
  "Event day YYYY-MM-DD from fields.")

(define calendar/event-start
  (lambda (f)
    (let ((s (dict-get f "start_time")))
      (if (null? s) "" (as-str s))))
  "Event start time string from fields.")

(define calendar/event-all-day?
  (lambda (f)
    (let ((a (dict-get f "all_day")))
      (if (null? a)
          (string-eq (calendar/event-start f) "")
          (string-eq (as-str a) "true"))))
  "True if event is all-day.")

(define calendar/events-on-day
  (lambda (day)
    (list/filter
      (lambda (row)
        (calendar/day-eq? (calendar/event-day (cdr row)) day))
      (calendar/all-events)))
  "Events whose day equals day string.")

(define calendar/events-in-month
  (lambda (ym)
    (list/filter
      (lambda (row)
        (calendar/day-prefix? (calendar/event-day (cdr row)) ym))
      (calendar/all-events)))
  "Events in YYYY-MM month prefix.")

(define calendar/create
  (lambda (title day start notes)
    (let ((st (if (null? start) "" (as-str start))))
      (let ((all (if (string-eq st "") "true" "false")))
      (create 'calendar-event
        (dict-set* "{}"
          (list
            "title" (as-str title)
            "day" (as-str day)
            "start_time" st
            "end_time" ""
            "all_day" all
            "notes" (as-str notes)
            "location" ""
            "color" ""
            "source" "calendar"))))))
  "Create calendar-event row.")

(define calendar/delete
  (lambda (id) (delete 'calendar-event id))
  "Delete calendar-event by id.")

;; --- overlays: todos with due, daily notes ---

(define calendar/todos-due-on
  (lambda (day)
    (list/filter
      (lambda (row)
        (let ((due (dict-get (cdr row) "due")))
          (if (null? due)
              #f
              (calendar/day-eq? due day))))
      (todo/item/all)))
  "Open todos with due date equal to day.")

(define calendar/count-events-on
  (lambda (day)
    (list/foldl (lambda (acc _) (+ acc 1)) 0 (calendar/events-on-day day)))
  "Count events on day.")

(define calendar/count-todos-on
  (lambda (day)
    (list/foldl (lambda (acc _) (+ acc 1)) 0 (calendar/todos-due-on day)))
  "Count todos due on day.")

(define calendar/has-daily?
  (lambda (day)
    (let ((slug (string-append "daily-" (as-str day))))
      (if (null? (notes/doc/get slug)) #f #t)))
  "True if a daily note exists for day (notes hook).")

;; --- HTML: month cell ---

(define calendar/cell-marks
  (lambda (day)
    (let ((ne (calendar/count-events-on day)))
      (let ((nt (calendar/count-todos-on day)))
        (let ((daily (calendar/has-daily? day)))
      (html/div (html/attr "class" "cal-marks")
        (html/join
          (list
            (if (> ne 0)
                (html/span (html/attr "class" "cal-dot cal-dot-ev")
                  (html/text (as-str ne)))
                "")
            (if (> nt 0)
                (html/span (html/attr "class" "cal-dot cal-dot-todo")
                  (html/text (as-str nt)))
                "")
            (if daily
                (html/span (html/attr "class" "cal-dot cal-dot-note")
                  (html/text "n"))
                ""))))))))
  "HTML marks string for month cell (events/todos/daily).")

(define calendar/day-cell
  (lambda (y m d today ym)
    (if (= d 0)
        (html/div (html/attr "class" "cal-cell cal-empty") "")
        (let ((day (calendar/ymd y m d)))
          (html/a
            (string-append "/calendar?day=" day)
            (html/div
              (html/attr "class"
                (if (calendar/day-eq? day today)
                    "cal-cell cal-today"
                    "cal-cell"))
              (html/join
                (list
                  (html/span (html/attr "class" "cal-dom")
                    (html/text (as-str d)))
                  (calendar/cell-marks day))))))))
  "One month-grid day cell HTML.")

(define calendar/build-cells
  (lambda (y m dim start-wd today ym d acc)
    (if (> d dim)
        acc
        (calendar/build-cells y m dim start-wd today ym (+ d 1)
          (string-append acc
            (calendar/day-cell y m d today ym)))))
  "Build day cell list for month starting at day.")

(define calendar/pad-start
  (lambda (n acc)
    (if (<= n 0)
        acc
        (calendar/pad-start (- n 1)
          (string-append acc
            (html/div (html/attr "class" "cal-cell cal-empty") "")))))
  "Leading empty cells before first weekday.")

(define calendar/month-grid
  (lambda (ym)
    (let ((y (parse-int (substring (as-str ym) 0 4))))
      (let ((m (parse-int (substring (as-str ym) 5 2))))
        (let ((today (calendar/today)))
      (let ((dim (calendar/days-in-month y m)))
        (let ((wd (calendar/weekday y m 1)))
        (html/div (html/attr "class" "cal-grid")
          (html/join
            (list
              (html/div (html/attr "class" "cal-dow") (html/text "Su"))
              (html/div (html/attr "class" "cal-dow") (html/text "Mo"))
              (html/div (html/attr "class" "cal-dow") (html/text "Tu"))
              (html/div (html/attr "class" "cal-dow") (html/text "We"))
              (html/div (html/attr "class" "cal-dow") (html/text "Th"))
              (html/div (html/attr "class" "cal-dow") (html/text "Fr"))
              (html/div (html/attr "class" "cal-dow") (html/text "Sa"))
              (calendar/pad-start wd "")
              (calendar/build-cells y m dim wd today ym 1 ""))))))))))
  "Full month grid HTML for year/month.")

(define calendar/month-nav
  (lambda (ym)
    (let ((prev (calendar/add-month ym -1)))
      (let ((next (calendar/add-month ym 1)))
      (html/div (html/attr "class" "cal-nav")
        (html/join
          (list
            (html/a (string-append "/calendar?ym=" prev) "◀")
            (html/strong ym)
            (html/a (string-append "/calendar?ym=" next) "▶")
            (html/a "/calendar" "Today")))))))
  "Prev/next month navigation HTML.")

;; --- day agenda ---

(define calendar/event-li
  (lambda (row)
    (let ((id (car row)))
      (let ((f (cdr row)))
      (html/li (html/attr "class" "cal-event")
        (html/join
          (list
            (html/span (html/attr "class" "cal-time")
              (html/text
                (if (calendar/event-all-day? f)
                    "all day"
                    (calendar/event-start f))))
            (html/span (html/attr "class" "cal-ev-title")
              (html/text (calendar/event-title f)))
            (html/form "POST" "/calendar"
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
                    "del"))))))))))
  "Agenda li for one event with delete.")

(define calendar/todo-due-li
  (lambda (row)
    (let ((id (car row)))
      (let ((f (cdr row)))
      (html/li (html/attr "class" "cal-todo")
        (html/join
          (list
            (html/span (html/attr "class" "cal-time") (html/text "todo"))
            (html/a "/todos"
              (html/text (as-str (dict-get f "title"))))))))))
  "Agenda li for a todo due that day.")

(define calendar/day-html
  (lambda (day)
    (html/div (html/attr "class" "cal-day-panel")
      (html/join
        (list
          (html/h2 (html/text day))
          (html/p*
            (html/attr "class" "muted")
            (html/join
              (list
                (html/a (string-append "/calendar?ym=" (calendar/ym day))
                  "← month")
                " · "
                (html/a
                  (string-append "/notes?slug=daily-" day)
                  "daily note"))))
          (html/h3 (html/text "Events"))
          (html/ul (html/attr "class" "card cal-list")
            (list/foldl
              (lambda (acc row)
                (string-append acc (calendar/event-li row)))
              ""
              (calendar/events-on-day day)))
          (html/h3 (html/text "Due todos"))
          (html/ul (html/attr "class" "card cal-list")
            (list/foldl
              (lambda (acc row)
                (string-append acc (calendar/todo-due-li row)))
              ""
              (calendar/todos-due-on day)))
          (html/div (html/attr "class" "card")
            (html/join
              (list
                (html/h3 (html/text "Add event"))
                (html/form "POST" "/calendar"
                  (html/attrs
                    (list
                      (html/attr "hx-post" "/calendar")
                      (html/attr "hx-target" "#cal-main")
                      (html/attr "hx-swap" "innerHTML")
                      (html/attr "hx-on::after-request"
                        "if(event.detail.successful){this.reset()}")))
                  (html/join
                    (list
                      (html/hidden "action" "create")
                      (html/hidden "day" day)
                      (html/label (html/attr "for" "title") "Title")
                      (html/text-input "title"
                        (html/attrs
                          (list
                            (html/attr "id" "title")
                            (html/attr "required" "required")
                            (html/attr "placeholder" "standup"))))
                      (html/label (html/attr "for" "start_time")
                        "Start (HH:MM, blank=all day)")
                      (html/text-input "start_time"
                        (html/attrs
                          (list
                            (html/attr "id" "start_time")
                            (html/attr "placeholder" "10:00"))))
                      (html/label (html/attr "for" "notes") "Notes")
                      (html/text-input "notes"
                        (html/attrs
                          (list
                            (html/attr "id" "notes")
                            (html/attr "placeholder" ""))))
                      (html/button (html/attr "type" "submit")
                        "Add event")))))))))))
  "Day agenda panel: events, todos, add form.")

(define calendar/month-html
  (lambda (ym)
    (html/div (html/attr "class" "cal-month")
      (html/join
        (list
          (calendar/month-nav ym)
          (calendar/month-grid ym)
          (html/p* (html/attr "class" "muted")
            "Dots: events · todos due · daily note. Click a day for agenda.")))))
  "Month view: nav plus grid.")

(define calendar/main-html
  (lambda ()
    (let ((day (request-param "day"))
          (ym (request-param "ym")))
      (html/div (html/attr "id" "cal-main")
        (if (null? day)
            (calendar/month-html
              (if (null? ym)
                  (calendar/ym (calendar/today))
                  (as-str ym)))
            (calendar/day-html (as-str day))))))
  "Main calendar panel for month and optional day.")

(define calendar/handle-post
  (lambda ()
    (let ((action (as-str (request-param "action"))))
      (cond
        ((string-eq action "create")
         (calendar/create
           (request-param "title")
           (request-param "day")
           (request-param "start_time")
           (request-param "notes")))
        ((string-eq action "delete")
         (calendar/delete (request-param "id")))
        (else ()))))
  "Dispatch create/delete POST actions.")

(define calendar/page
  (lambda ()
    (if (string-eq (request-method) "POST")
        (seq
          (calendar/handle-post)
          (if (hx-request?)
              (render-html
                (let ((day (request-param "day")))
                  (if (null? day)
                      (calendar/main-html)
                      (html/div (html/attr "id" "cal-main")
                        (calendar/day-html (as-str day))))))
              (let ((day (request-param "day")))
                (if (null? day)
                    (redirect "/calendar")
                    (redirect (string-append "/calendar?day=" (as-str day)))))))
        (html/page
          (list
            (html/h1 "Calendar")
            (html/p* (html/attr "class" "muted")
              "Month grid · day agenda · events + due todos + daily notes")
            (calendar/main-html)))))
  "GET/POST /calendar page handler.")

(route "/calendar" "calendar/page")
(route "/partials/calendar" "calendar/main-html")
