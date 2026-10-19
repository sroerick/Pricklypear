;; package calendar -- exported from live branch system/main
;; Sync: scripts/pp-sync.sh status|pull|snapshot

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
              (string-append (as-str y) "-" (calendar/pad2 m2))))))))
  "Add delta months to a YYYY-MM string, wrapping year boundaries.")

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
            days)))))
  "Build a 7-day agenda list with events and due todos per day.")

(define calendar/agenda-day-html
  (lambda
    (row month-names)
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
                  (and (null? events) (null? todos))
                  (html/p*
                    (html/attr "class" "muted")
                    "nothing scheduled")
                  (html/join
                    (list
                      (if
                        (null? events)
                        ""
                        (html/ul
                          (list)
                          (html/join (list/map calendar/agenda-event-li events))))
                      (if
                        (null? todos)
                        ""
                        (html/ul
                          (html/attr "class" "cal-list")
                          (html/join (list/map calendar/agenda-todo-li todos))))))))))))))
  "Render a single agenda day row as HTML with events and todos.")

(define calendar/agenda-event-li
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
  "Render an agenda event as a list item with optional time prefix.")

(define calendar/agenda-todo-li
  (lambda
    (td)
    (let
      ((tf (cdr td)))
      (html/li
        (list)
        (html/join
          (list
            (html/span
              (html/attr
                "style"
                "font-weight: bold; color: #c92a2a; margin-right: 0.5em;")
              (html/text "todo"))
            (html/a
              "/todos"
              (html/text (as-str (dict-get tf "title")))))))))
  "Render an agenda todo as a list item linking to /todos.")

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
                rows)))))))
  "Render the 7-day agenda as an HTML card widget.")

(define calendar/all-events
  (lambda () (rows (quote calendar-event) 500))
  "Return up to 500 calendar-event rows from the store.")

(define calendar/browse
  (lambda
    (path)
    (filmstrip/browse-walk
      calendar/browse-children
      (list (calendar/browse-years))
      path))
  "calendar cascade: year -> month -> day. Day entries leaf to\n/calendar?day=YYYY-MM-DD (the day agenda route).")

(define calendar/browse-children
  (lambda
    (entry)
    (let
      ((e (as-str entry)))
      (if
        (= (string-length e) 4)
        (calendar/browse-months e)
        (if
          (= (string-length e) 7)
          (calendar/browse-days e)
          (list)))))
  "Children of a calendar crumb: year -> months, YYYY-MM -> days.")

(define calendar/browse-contains
  (lambda
    (xs s)
    (if
      (null? xs)
      #f
      (if
        (string-eq (as-str (car xs)) s)
        #t
        (calendar/browse-contains (cdr xs) s))))
  "Membership by string equality.")

(define calendar/browse-days
  (lambda
    (ym)
    (let
      ((y (substring ym 0 4))
        (m (parse-int (substring ym 5 2))))
      (calendar/browse-days-acc
        y
        m
        1
        (calendar/days-in-month y m)
        (list))))
  "Day entries for a month, each a leaf to the day agenda.")

(define calendar/browse-days-acc
  (lambda
    (y m d dim acc)
    (if
      (> d dim)
      acc
      (calendar/browse-days-acc
        y
        m
        (+ d 1)
        dim
        (append
          acc
          (list
            (list
              (as-str d)
              (string-append
                "/calendar?day="
                y
                "-"
                (calendar/pad2 m)
                "-"
                (calendar/pad2 d))))))))
  "Accumulate day entries (label D, target /calendar?day=YYYY-MM-DD).")

(define calendar/browse-event-years
  (lambda
    ()
    (calendar/browse-year-acc (calendar/all-events) (list)))
  "Distinct years across stored events.")

(define calendar/browse-insert-sorted
  (lambda
    (s xs)
    (if
      (null? xs)
      (list s)
      (if
        (string<? s (as-str (car xs)))
        (cons s xs)
        (cons
          (car xs)
          (calendar/browse-insert-sorted s (cdr xs))))))
  "Insert into an ascending string list.")

(define calendar/browse-months
  (lambda (y) (calendar/browse-months-acc y 1 (list)))
  "Twelve YYYY-MM entries for a year.")

(define calendar/browse-months-acc
  (lambda
    (y m acc)
    (if
      (> m 12)
      acc
      (calendar/browse-months-acc
        y
        (+ m 1)
        (append
          acc
          (list
            (list
              (string-append
                (calendar/pad2 m)
                " "
                (calendar/month-name m))
              (string-append y "-" (calendar/pad2 m))))))))
  "Accumulate month entries (label DD Mon, target YYYY-MM).")

(define calendar/browse-numsort
  (lambda (xs) xs)
  "Lists are built ascending already; identity for clarity.")

(define calendar/browse-year-acc
  (lambda
    (rows acc)
    (if
      (null? rows)
      acc
      (let
        ((d (as-str (dict-get (cdr (car rows)) "day"))))
        (if
          (< (string-length d) 4)
          (calendar/browse-year-acc (cdr rows) acc)
          (let
            ((y (substring d 0 4)))
            (calendar/browse-year-acc
              (cdr rows)
              (if
                (calendar/browse-contains acc y)
                acc
                (calendar/browse-insert-sorted y acc))))))))
  "Accumulate distinct YYYY prefixes from event rows.")

(define calendar/browse-years
  (lambda
    ()
    (let
      ((today (calendar/today)))
      (let
        ((y (substring today 0 4)))
        (let
          ((ev-years (calendar/browse-event-years)))
          (calendar/browse-numsort
            (if
              (calendar/browse-contains ev-years y)
              ev-years
              (calendar/browse-insert-sorted y ev-years)))))))
  "Years with events, plus the current year; sorted ascending.")

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
        (string-append acc (calendar/day-cell y m d today ym)))))
  "Recursively build calendar day cells from day 1 to days-in-month.")

(define calendar/build-cells-fast
  (lambda
    (y
      m
      dim
      start-wd
      today
      ym
      d
      acc
      all-evs
      all-todos)
    (if
      (> d dim)
      acc
      (calendar/build-cells-fast
        y
        m
        dim
        start-wd
        today
        ym
        (+ d 1)
        (string-append
          acc
          (calendar/day-cell-fast
            y
            m
            d
            today
            ym
            all-evs
            all-todos))
        all-evs
        all-todos))))

(define calendar/build-cells-fast2
  (lambda
    (y
      m
      dim
      start-wd
      today
      ym
      d
      acc
      all-evs
      all-todos)
    (if
      (> d dim)
      acc
      (calendar/build-cells-fast2
        y
        m
        dim
        start-wd
        today
        ym
        (+ d 1)
        (string-append
          acc
          (calendar/day-cell-fast2
            y
            m
            d
            today
            ym
            all-evs
            all-todos))
        all-evs
        all-todos))))

(define calendar/build-cells-no-daily
  (lambda
    (y
      m
      dim
      start-wd
      today
      ym
      d
      acc
      all-evs
      all-todos)
    (if
      (> d dim)
      acc
      (let
        ((day (calendar/ymd y m d)))
        (let
          ((ne
            (+
              (calendar/count-events-in day all-evs)
              (calendar/count-recurring-on day))))
          (let
            ((nt (calendar/count-todos-in day all-todos)))
            (calendar/build-cells-no-daily
              y
              m
              dim
              start-wd
              today
              ym
              (+ d 1)
              (string-append
                acc
                (calendar/day-cell-str
                  d
                  day
                  today
                  ne
                  nt
                  #f))
              all-evs
              all-todos)))))))

(define calendar/build-cells-str
  (lambda
    (y
      m
      dim
      today
      ym
      d
      acc
      ev-str
      todo-str
      daily-str)
    (if
      (> d dim)
      acc
      (calendar/build-cells-str
        y
        m
        dim
        today
        ym
        (+ d 1)
        (string-append
          acc
          (calendar/day-cell-str
            d
            y
            m
            today
            ev-str
            todo-str
            daily-str))
        ev-str
        todo-str
        daily-str))))

(define calendar/cell-marks
  (lambda
    (day)
    (let
      ((ne
        (+
          (calendar/count-events-on day)
          (calendar/count-recurring-on day))))
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

(define calendar/count-events-in
  (lambda
    (day all-evs)
    (list/foldl
      (lambda
        (acc row)
        (if
          (calendar/day-eq? (calendar/event-day (cdr row)) day)
          (+ acc 1)
          acc))
      0
      all-evs)))

(define calendar/count-events-on
  (lambda
    (day)
    (list/foldl
      (lambda (acc _) (+ acc 1))
      0
      (calendar/events-on-day day)))
  "Count events on a specific YYYY-MM-DD day.")

(define calendar/count-recurring-on
  (lambda
    (day)
    (list/foldl
      (lambda
        (acc rec)
        (if
          (calendar/cron-matches? day (car rec))
          (+ acc 1)
          acc))
      0
      (calendar/recurring-events))))

(define calendar/count-todos-in
  (lambda
    (day all-todos)
    (list/foldl
      (lambda
        (acc row)
        (let
          ((due (dict-get (cdr row) "due")))
          (if
            (null? due)
            acc
            (if (calendar/day-eq? due day) (+ acc 1) acc))))
      0
      all-todos)))

(define calendar/count-todos-on
  (lambda
    (day)
    (list/foldl
      (lambda (acc _) (+ acc 1))
      0
      (calendar/todos-due-on day)))
  "Count todos due on a specific YYYY-MM-DD day.")

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
              "calendar"))))))
  "Create a new calendar-event with title, day, start time, and notes.")

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
          (let ((field-val (parse-int field))) (= val field-val))))))
  "Test if a cron field value matches a given integer.")

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
                  #t))))))))
  "Test if a cron expression matches a YYYY-MM-DD date string.")

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
          "\", \"all_day\": \"false\"}"))))
  "Convert a recurring cron rule to a calendar-event JSON row.")

(define calendar/css
  (lambda
    ()
    (css/sheet
      (list
        (css/rule
          ".cal-nav"
          (list
            "display"
            "flex"
            "align-items"
            "center"
            "gap"
            ".75rem"
            "margin"
            ".5rem 0"
            "flex-wrap"
            "wrap"))
        (css/rule
          ".cal-nav a"
          (list "text-decoration" "none" "font-weight" "700"))
        (css/rule
          ".cal-grid"
          (list
            "display"
            "grid"
            "grid-template-columns"
            "repeat(7,1fr)"
            "gap"
            "2px"
            "background"
            "var(--shadow)"
            "border"
            "2px solid #000"
            "max-width"
            "42rem"))
        (css/rule
          ".cal-dow"
          (list
            "background"
            "var(--chrome)"
            "text-align"
            "center"
            "font-weight"
            "700"
            "padding"
            ".25rem"
            "font-size"
            ".8rem"))
        (css/rule
          ".cal-cell"
          (list
            "background"
            "var(--panel)"
            "min-height"
            "3.5rem"
            "padding"
            ".2rem"
            "display"
            "block"
            "color"
            "var(--ink)"
            "text-decoration"
            "none"))
        (css/rule
          ".cal-empty"
          (list "background" "var(--bg)" "min-height" "3.5rem"))
        (css/rule
          ".cal-today"
          (list
            "outline"
            "2px solid #000"
            "background"
            "var(--today-bg)"))
        (css/rule
          ".cal-dom"
          (list "font-weight" "700" "font-size" ".85rem"))
        (css/rule
          ".cal-marks"
          (list
            "display"
            "flex"
            "flex-wrap"
            "wrap"
            "gap"
            "2px"
            "margin-top"
            ".15rem"))
        (css/rule
          ".cal-dot"
          (list
            "font-size"
            ".65rem"
            "padding"
            "0 .2rem"
            "border"
            "1px solid #000"
            "background"
            "var(--chrome)"
            "line-height"
            "1.2"))
        (css/rule
          ".cal-dot-ev"
          (list "background" "var(--done-bg)"))
        (css/rule
          ".cal-dot-todo"
          (list "background" "var(--pending-bg)"))
        (css/rule
          ".cal-dot-note"
          (list "background" "var(--waiting-bg)"))
        (css/rule
          ".cal-list"
          (list
            "list-style"
            "none"
            "padding"
            ".25rem"
            "margin"
            "0"))
        (css/rule
          ".cal-event,.cal-todo"
          (list
            "display"
            "flex"
            "gap"
            ".5rem"
            "align-items"
            "center"
            "padding"
            ".35rem 0"
            "border-bottom"
            "1px solid #ccc"))
        (css/rule
          ".cal-time"
          (list
            "font-size"
            ".8rem"
            "min-width"
            "3.5rem"
            "color"
            "var(--muted)"))
        (css/rule
          ".cal-day-panel label"
          (list "display" "block" "margin-top" ".4rem"))
        (css/rule
          "a .cal-cell:hover"
          (list "background" "var(--sel-bg)")))))
  "Render the calendar component stylesheet (extracted from ui/theme).")

(define calendar/daily-day-str
  (lambda
    (ym nodes)
    (let
      ((needle
        (string-append "daily_" (string-replace ym "-" "_") "_")))
      (list/foldl
        (lambda
          (acc node)
          (let
            ((path (as-str (dict-get node "path"))))
            (if
              (string-contains? path needle)
              (string-append
                acc
                (substring
                  path
                  (- (string-length path) 2)
                  (string-length path))
                ",")
              acc)))
        ","
        nodes))))

(define calendar/day-cell
  (lambda
    (y m d today ym)
    (if
      (= d 0)
      (html/div (html/attr "class" "cal-cell cal-empty") "")
      (let
        ((day (calendar/ymd y m d)))
          (calendar/nav-a
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
                (calendar/cell-marks day))))))))
  "Render a single calendar day cell as a pane-local link (hx-get swaps
#cal-main in place) with day number and event marks.")

(define calendar/day-cell-fast
  (lambda
    (y
      m
      d
      today
      ym
      all-evs
      all-todos)
    (if
      (= d 0)
      (html/div (html/attr "class" "cal-cell cal-empty") "")
      (let
        ((day (calendar/ymd y m d)))
        (let
          ((ne
            (+
              (calendar/count-events-in day all-evs)
              (calendar/count-recurring-on day))))
          (let
            ((nt (calendar/count-todos-in day all-todos)))
            (let
              ((daily (calendar/has-daily? day)))
                (calendar/nav-a
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
                              "")))))))))))))))

(define calendar/day-cell-fast2
  (lambda
    (y
      m
      d
      today
      ym
      all-evs
      all-todos)
    (if
      (= d 0)
      (html/div (html/attr "class" "cal-cell cal-empty") "")
      (let
        ((day (calendar/ymd y m d)))
        (let
          ((ne
            (+
              (calendar/count-events-in day all-evs)
              (calendar/count-recurring-on day))))
          (let
            ((nt (calendar/count-todos-in day all-todos)))
              (calendar/nav-a
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
                            ""))))))))))))))

(define calendar/day-cell-str
  (lambda
    (d
      y
      m
      today
      ev-str
      todo-str
      daily-str)
    (if
      (= d 0)
      "<div class=\"cal-cell cal-empty\"></div>"
      (let
        ((day (calendar/ymd y m d)))
        (string-append
            "<a href=\"/calendar?day="
            day
            "\" hx-get=\"/calendar?day="
            day
            "\" hx-target=\"#cal-main\" hx-swap=\"outerHTML\"><div class=\""
          (if
            (calendar/day-eq? day today)
            "cal-cell cal-today"
            "cal-cell")
          "\"><span class=\"cal-dom\">"
          (as-str d)
          "</span>"
          "<div class=\"cal-marks\">"
          (if
            (calendar/day-in-str? d ev-str)
            "<span class=\"cal-dot cal-dot-ev\">&middot;</span>"
            "")
          (if
            (calendar/day-in-str? d todo-str)
            "<span class=\"cal-dot cal-dot-todo\">&middot;</span>"
            "")
          (if
            (calendar/day-in-str? d daily-str)
            "<span class=\"cal-dot cal-dot-note\">n</span>"
            "")
          "</div></div></a>")))))

(define calendar/day-eq?
  (lambda (a b) (string-eq (as-str a) (as-str b)))
  "Check if two day strings are equal (as strings).")

(define calendar/nav-a
  (lambda
    (href label)
    (html/tag
      "a"
      (html/attrs
        (list
          (html/attr "href" href)
          (html/attr "hx-get" href)
          (html/attr "hx-target" "#cal-main")
          (html/attr "hx-swap" "outerHTML")))
      label))
  "Pane-local calendar link: hx-get swaps #cal-main in place (filmstrip
pane or classic page); plain href fallback without JS.")

(define calendar/nav-a/examples
  (lambda ()
    (lb/obj
      (list
        "name" "calendar/nav-a"
        "doc" "One pane-local calendar link: hx-get swaps #cal-main in place (filmstrip pane or classic page); plain href fallback without JS."
        "file" "libs/calendar/lib.pp"
        "formals" (json-emit (list "href" "label"))
        "variants" (json-emit
                     (list
                       (lb/obj (list "href" "/calendar?ym=2026-10" "label" "<"))
                       (lb/obj (list "href" "/calendar?day=2026-10-18" "label" "18"))))
        "source"
        "(calendar/nav-a href label) ->\n  (html/tag \"a\"\n    (html/attrs\n      (list (html/attr \"href\" href)\n            (html/attr \"hx-get\" href)\n            (html/attr \"hx-target\" \"#cal-main\")\n            (html/attr \"hx-swap\" \"outerHTML\")))\n    label)))))")))
  "The calendar/nav-a card: this def IS the element's declaration.")

(define lookbook/elements
  (lambda () (append (lookbook/base-elements) (list "calendar/nav-a")))
  "Calendar package registration on the lookbook seam (libs/lookbook):
the package vocabulary appends to lookbook/base-elements -- never a
captured snapshot of the seam itself (the cyclic-thunk-force rule).
Every entry answers a <name>/examples card.")

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
                  (calendar/nav-a
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
              (calendar/events-for-day day)))
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

(define calendar/day-in-str?
  (lambda
    (d day-str)
    (string-contains?
      day-str
      (string-append (calendar/pad2 d) ","))))

(define calendar/day-prefix?
  (lambda
    (day ym)
    (string-eq (substring (as-str day) 0 7) (as-str ym)))
  "Check if a day string starts with a given YYYY-MM prefix.")

(define calendar/days-in-month
  (lambda
    (y m)
    (if
      (= m 2)
      (if (calendar/leap? y) 29 28)
      (if
        (= m 4)
        30
        (if (= m 6) 30 (if (= m 9) 30 (if (= m 11) 30 31))))))
  "Return number of days in month m of year y, handling leap years.")

(define calendar/delete
  (lambda (id) (delete (quote calendar-event) id))
  "Delete a calendar-event by id.")

(define calendar/dom
  (lambda (day) (parse-int (substring (as-str day) 8 2)))
  "Extract day-of-month integer from a YYYY-MM-DD string.")

(define calendar/event-all-day?
  (lambda
    (f)
    (let
      ((a (dict-get f "all_day")))
      (if
        (null? a)
        (string-eq (calendar/event-start f) "")
        (string-eq (as-str a) "true"))))
  "Check if an event fields dict is marked all-day.")

(define calendar/event-day
  (lambda (f) (as-str (dict-get f "day")))
  "Extract the day field from an event fields dict.")

(define calendar/event-day-str
  (lambda
    (ym all-evs)
    (list/foldl
      (lambda
        (acc row)
        (let
          ((day-str (calendar/event-day (cdr row))))
          (if
            (string-eq (substring day-str 0 7) ym)
            (string-append acc (substring day-str 8 2) ",")
            acc)))
      ","
      all-evs)))

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
                      "del"))))))))))
  "Render a calendar event as an HTML list item with delete button.")

(define calendar/event-sort-key
  (lambda
    (row)
    (let
      ((f (cdr row)))
      (string-append
        (calendar/event-day f)
        "|"
        (calendar/event-start f))))
  "Build a sort key string from event day + start_time for ordering.")

(define calendar/event-start
  (lambda
    (f)
    (let
      ((s (dict-get f "start_time")))
      (if (null? s) "" (as-str s))))
  "Extract start_time from event fields dict, empty if missing.")

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
          (if (string-eq e "") s (string-append s " - " e))))))
  "Format event time range from start_time and end_time fields.")

(define calendar/event-title
  (lambda (f) (as-str (dict-get f "title")))
  "Extract the title from an event fields dict.")

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
      (calendar/all-events)))
  "Filter all events to those in a given YYYY-MM month.")

(define calendar/events-on-day
  (lambda
    (day)
    (list/filter
      (lambda
        (row)
        (calendar/day-eq? (calendar/event-day (cdr row)) day))
      (calendar/all-events))))

(define calendar/foo
  (lambda () (date-today))
  "Returns todays date. Debug/test stub.")

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
        (else ()))))
  "Handle POST requests for create and delete calendar actions.")

(define calendar/has-daily?
  (lambda
    (day)
    (let
      ((slug (string-append "daily-" (as-str day))))
      (if (null? (notes/doc/get slug)) #f #t)))
  "Check if a daily note exists for a given YYYY-MM-DD day.")

(define calendar/insert-sorted-event
  (lambda
    (item lst)
    (if
      (null? lst)
      (list item)
      (if
        (string<=?
          (calendar/event-sort-key item)
          (calendar/event-sort-key (car lst)))
        (cons item lst)
        (cons
          (car lst)
          (calendar/insert-sorted-event item (cdr lst))))))
  "Insert an event row into a list sorted by event-sort-key.")

(define calendar/leap?
  (lambda
    (y)
    (if
      (= (mod y 400) 0)
      #t
      (if (= (mod y 100) 0) #f (= (mod y 4) 0))))
  "Check if year y is a leap year.")

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
          (calendar/day-html (as-str day))))))
  "Render the main calendar panel (month grid or day view).")

(define calendar/month
  (lambda (day) (parse-int (substring (as-str day) 5 2)))
  "Extract month integer from a YYYY-MM-DD string.")

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
              (let
                ((all-evs (calendar/all-events)))
                (let
                  ((all-todos (todo/item/all)))
                  (let
                    ((all-notes
                      (tree/list-kind "data.note.u.admin" "note" 200)))
                    (let
                      ((ev-str
                        (string-append
                          (calendar/event-day-str ym all-evs)
                          (calendar/recurring-day-str y m dim))))
                      (let
                        ((todo-str (calendar/todo-day-str ym all-todos)))
                        (let
                          ((daily-str (calendar/daily-day-str ym all-notes)))
                          (string-append
                            "<div class=\"cal-grid\">"
                            "<div class=\"cal-dow\">Su</div>"
                            "<div class=\"cal-dow\">Mo</div>"
                            "<div class=\"cal-dow\">Tu</div>"
                            "<div class=\"cal-dow\">We</div>"
                            "<div class=\"cal-dow\">Th</div>"
                            "<div class=\"cal-dow\">Fr</div>"
                            "<div class=\"cal-dow\">Sa</div>"
                            (calendar/pad-start wd "")
                            (calendar/build-cells-str
                              y
                              m
                              dim
                              today
                              ym
                              1
                              ""
                              ev-str
                              todo-str
                              daily-str)
                            "</div>"))))))))))))))

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
            "Dots: events · todos due · daily note. Click a day for agenda.")))))
  "Render the full month view with navigation, grid, and legend.")

(define calendar/month-name
  (lambda
    (m)
    (nth
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
        "Dec")
      (- m 1)))
  "Month abbreviation for 1..12.")

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
                (calendar/nav-a (string-append "/calendar?ym=" prev) "◀")
                (html/strong ym)
                (calendar/nav-a (string-append "/calendar?ym=" next) "▶")
                (calendar/nav-a "/calendar" "Today")))))))
  "Render previous/next/today navigation links for a YYYY-MM month.")

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
          (html/div (html/attr "class" "cal-cell cal-empty") "")))))
  "Recursively prepend n empty cells to pad the grid start.")

(define calendar/pad2
  (lambda
    (n)
    (if (< n 10) (string-append "0" (as-str n)) (as-str n)))
  "Zero-pad a number to 2 digits (e.g. 3 -> 03).")

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
        (if
          (hx-request?)
          (render-html (calendar/main-html))
          (html/page
            (list
              (html/h1 "Calendar")
              (html/p*
                (html/attr "class" "muted")
                "Month grid · day agenda · events + due todos + daily notes")
              (calendar/main-html))))))
  "Route handler for /calendar: POST actions or GET page render.")

(define calendar/rd-str-loop
  (lambda
    (y m d dim acc)
    (if
      (> d dim)
      (string-append "," acc)
      (calendar/rd-str-loop
        y
        m
        (+ d 1)
        dim
        (if
          (> (calendar/count-recurring-on (calendar/ymd y m d)) 0)
          (string-append acc (calendar/pad2 d) ",")
          acc)))))

(define calendar/recurring-day-str
  (lambda (y m dim) (calendar/rd-str-loop y m 1 dim "")))

(define calendar/recurring-events
  (lambda
    ()
    (list
      (list "30 7 * * 1-4" "Taylor Dental" "07:30-17:00")
      (list "0 17 * * 2-4" "Cassie's" "17:00-23:00")))
  "Return hardcoded list of recurring cron-based events.")

(define calendar/recurring-for-day
  (lambda
    (d)
    (let
      ((recurring (calendar/recurring-events)))
      (let
        ((matched
          (list/filter
            (lambda (rec) (calendar/cron-matches? d (car rec)))
            recurring)))
        (list/map
          (lambda
            (rec)
            (let
              ((cron (car rec))
                (title (car (cdr rec)))
                (time (car (cdr (cdr rec)))))
              (cons
                ""
                (dict-set*
                  "{}"
                  (list
                    "title"
                    title
                    "start_time"
                    time
                    "day"
                    d
                    "cron"
                    cron
                    "all_day"
                    "false")))))
          matched))))
  "Generate recurring cron event rows matching day d, with day field set.")

(define calendar/sort-events
  (lambda
    (events)
    (list/foldl
      (lambda (acc ev) (calendar/insert-sorted-event ev acc))
      (list)
      events))
  "Sort a list of event rows by day then start_time via insertion sort.")

(define calendar/today
  (lambda () (date-today))
  "Returns todays date as YYYY-MM-DD via date-today.")

(define calendar/todo-day-str
  (lambda
    (ym all-todos)
    (list/foldl
      (lambda
        (acc row)
        (let
          ((due (dict-get (cdr row) "due")))
          (if
            (null? due)
            acc
            (let
              ((ds (as-str due)))
              (if
                (string-eq (substring ds 0 7) ym)
                (string-append acc (substring ds 8 2) ",")
                acc)))))
      ","
      all-todos)))

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
                (html/text (as-str (dict-get f "title"))))))))))
  "Render a due todo as an HTML list item linking to /todos.")

(define calendar/todos-due-on
  (lambda
    (day)
    (list/filter
      (lambda
        (row)
        (let
          ((due (dict-get (cdr row) "due")))
          (if (null? due) #f (calendar/day-eq? due day))))
      (todo/item/all)))
  "Filter all todo items to those due on a specific YYYY-MM-DD day.")

(define calendar/upcoming-events
  (lambda
    (limit)
    (let
      ((today (date-today)))
      (let
        ((db-future
          (list/filter
            (lambda
              (row)
              (let
                ((d (calendar/event-day (cdr row))))
                (if (string-eq d "") #f (string>=? d today))))
            (calendar/all-events))))
        (let
          ((recurring-future
            (calendar/upcoming-recurring today 14)))
          (let
            ((all (append db-future recurring-future)))
            (list/take (calendar/sort-events all) limit))))))
  "Return upcoming events (DB + recurring) from today forward, sorted, capped at limit.")

(define calendar/upcoming-li
  (lambda
    (row)
    (let
      ((f (cdr row)))
      (let
        ((day (calendar/event-day f))
          (t (calendar/event-time-string f)))
        (let
          ((m-str (substring day 5 2))
            (d-str (substring day 8 2)))
          (html/li
            (html/attr "class" "cal-event")
            (html/join
              (list
                (html/span
                  (html/attr "class" "cal-time")
                  (html/text
                    (if
                      (string-eq t "")
                      (string-append m-str "/" d-str)
                      (string-append m-str "/" d-str " " t))))
                (html/span
                  (html/attr "class" "cal-ev-title")
                  (html/text (calendar/event-title f))))))))))
  "Render an upcoming event as an HTML li with date, time, and title.")

(define calendar/upcoming-recurring
  (lambda
    (start horizon)
    (calendar/upcoming-recurring-acc start 0 horizon (list)))
  "Generate recurring event instances for the next horizon days from start.")

(define calendar/upcoming-recurring-acc
  (lambda
    (start i horizon acc)
    (if
      (>= i horizon)
      acc
      (let
        ((d (date-add-days start i)))
        (let
          ((evs (calendar/recurring-for-day d)))
          (calendar/upcoming-recurring-acc
            start
            (+ i 1)
            horizon
            (append acc evs))))))
  "Accumulator helper: generate recurring events for days i..horizon.")

(define calendar/upcoming-widget
  (lambda
    ()
    (layout/card
      "Upcoming Events"
      (let
        ((events (calendar/upcoming-events 20)))
        (if
          (null? events)
          (html/p*
            (html/attr "class" "muted")
            "no upcoming events")
          (html/ul
            (list)
            (html/join (list/map calendar/upcoming-li events)))))))
  "Render upcoming events as an HTML card widget (max 20 items).")

(define calendar/weekday
  (lambda (y m d) (date-weekday y m d))
  "Return day-of-week (0=Sun..6=Sat) for year y, month m, day d.")

(define calendar/year
  (lambda (day) (parse-int (substring (as-str day) 0 4)))
  "Extract year integer from a YYYY-MM-DD string.")

(define calendar/ym
  (lambda (day) (substring (as-str day) 0 7))
  "Extract YYYY-MM prefix from a YYYY-MM-DD string.")

(define calendar/ymd
  (lambda (y m d) (date-format y m d))
  "Format year y, month m, day d as a YYYY-MM-DD string.")
