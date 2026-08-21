;; package food -- exported from live branch system/main
;; Sync: scripts/pp_sync.py pull|push

(define food/a
  (lambda () 1))

(define food/add-template
  (lambda
    (day-type
      meal
      food
      calories
      protein
      carbs
      fat)
    (create
      (quote food-template)
      (dict-set*
        "{}"
        (list
          "day-type"
          (as-str day-type)
          "meal"
          (as-str meal)
          "food"
          (as-str food)
          "calories"
          (as-str calories)
          "protein"
          (as-str protein)
          "carbs"
          (as-str carbs)
          "fat"
          (as-str fat)
          "author"
          (as-str (whoami)))))))

(define food/all
  (lambda () (rows (quote food-log) 500)))

(define food/apply-templates
  (lambda
    (date day-type)
    (food/foldl
      (lambda
        (acc entry)
        (food/log
          date
          (food/entry-str entry "meal")
          (food/entry-str entry "food")
          (food/entry-val entry "calories")
          (food/entry-val entry "protein")
          (food/entry-val entry "carbs")
          (food/entry-val entry "fat")
          ""))
      0
      (food/templates day-type))))

(define food/b
  (lambda () 2))

(define food/cal-of
  (lambda
    (row)
    (parse-int (food/field (cdr row) "calories"))))

(define food/dates-back
  (lambda
    (n)
    (if
      (< n 0)
      ()
      (cons
        (date-add-days (date-today) (- 0 n))
        (food/dates-back (- n 1))))))

(define food/day-calories
  (lambda
    (day)
    (food/sum-calories
      (find-rows (quote food-log) "date" day))))

(define food/day-summary
  (lambda
    (date)
    (let*
      ((dtype (food/day-type date))
        (tgt (food/target dtype))
        (tot (food/day-total date))
        (tot-cal (food/safe-num (dict-get tot "calories")))
        (tot-pro (food/safe-num (dict-get tot "protein")))
        (tot-car (food/safe-num (dict-get tot "carbs")))
        (tot-fat (food/safe-num (dict-get tot "fat")))
        (tgt-cal (food/safe-num (dict-get tgt "calories")))
        (tgt-pro (food/safe-num (dict-get tgt "protein")))
        (tgt-car (food/safe-num (dict-get tgt "carbs")))
        (tgt-fat (food/safe-num (dict-get tgt "fat"))))
      (dict-set*
        "{}"
        (list
          "date"
          (as-str date)
          "day-type"
          dtype
          "calories"
          (as-str tot-cal)
          "calories-target"
          (as-str tgt-cal)
          "calories-remaining"
          (as-str (- tgt-cal tot-cal))
          "protein"
          (as-str tot-pro)
          "protein-target"
          (as-str tgt-pro)
          "protein-remaining"
          (as-str (- tgt-pro tot-pro))
          "carbs"
          (as-str tot-car)
          "carbs-target"
          (as-str tgt-car)
          "carbs-remaining"
          (as-str (- tgt-car tot-car))
          "fat"
          (as-str tot-fat)
          "fat-target"
          (as-str tgt-fat)
          "fat-remaining"
          (as-str (- tgt-fat tot-fat))
          "entries"
          (dict-get tot "entries"))))))

(define food/day-total
  (lambda
    (date)
    (let*
      ((es (food/entries date))
        (cal (food/sum-field es "calories"))
        (pro (food/sum-field es "protein"))
        (car (food/sum-field es "carbs"))
        (fat (food/sum-field es "fat")))
      (dict-set*
        "{}"
        (list
          "date"
          (as-str date)
          "calories"
          (as-str cal)
          "protein"
          (as-str pro)
          "carbs"
          (as-str car)
          "fat"
          (as-str fat)
          "entries"
          (as-str (length es)))))))

(define food/day-type
  (lambda
    (date)
    (let
      ((row (find-rows (quote food-day) "date" (as-str date))))
      (if
        (null? row)
        "off"
        (as-str (dict-get (cdr (car row)) "day-type"))))))

(define food/delete
  (lambda (id) (delete (quote food-log) id)))

(define food/entries
  (lambda
    (date)
    (find-rows (quote food-log) "date" (as-str date))))

(define food/entry-li
  (lambda
    (row)
    (html/li
      ""
      (html/join
        (list
          (html/strong (food/name-of row))
          (html/text
            (string-append " - " (as-str (food/cal-of row)) " cal")))))))

(define food/entry-str
  (lambda
    (entry field)
    (as-str (dict-get (cdr entry) field))))

(define food/entry-val
  (lambda
    (entry field)
    (food/safe-num (dict-get (cdr entry) field))))

(define food/field
  (lambda
    (fields key)
    (let
      ((v (dict-get fields key)))
      (if (null? v) "" (as-str v)))))

(define food/fields
  (lambda
    (food calories date)
    (dict-set*
      "{}"
      (list
        "food"
        food
        "calories"
        (as-str calories)
        "date"
        date
        "author"
        (as-str (whoami))))))

(define food/foldl
  (lambda
    (f init xs)
    (if
      (null? xs)
      init
      (food/foldl f (f init (car xs)) (cdr xs)))))

(define food/form
  (lambda
    ()
    (html/div
      (html/attr "class" "card")
      (html/join
        (list
          (html/h2 "Log food")
          (html/form
            "POST"
            "/food"
            ""
            (html/join
              (list
                (html/text-input
                  "food"
                  (html/attr "placeholder" "food name"))
                (html/text-input
                  "calories"
                  (html/attr "placeholder" "cal"))
                (html/button (html/attr "type" "submit") "Log")))))))))

(define food/log
  (lambda
    (date
      meal
      food
      calories
      protein
      carbs
      fat
      notes)
    (create
      (quote food-log)
      (dict-set*
        "{}"
        (list
          "date"
          date
          "meal"
          meal
          "food"
          food
          "calories"
          (as-str calories)
          "protein"
          (as-str protein)
          "carbs"
          (as-str carbs)
          "fat"
          (as-str fat)
          "notes"
          notes
          "author"
          (as-str (whoami)))))))

(define food/name-of
  (lambda (row) (food/field (cdr row) "food")))

(define food/page
  (lambda
    ()
    (if
      (string-eq (request-method) "POST")
      (let
        ((fv (request-param "food"))
          (cv (request-param "calories")))
        (seq
          (food/log (as-str fv) (parse-int (as-str cv)))
          (redirect "/food")))
      (let*
        ((entries (food/today))
          (total (food/sum-calories entries)))
        (html/page
          (list
            (html/h1 "Food Log")
            (food/form)
            (html/div
              (html/attr "class" "card")
              (html/join
                (list
                  (html/h2
                    (string-append "Today - " (as-str total) " cal"))
                  (html/ul
                    ""
                    (html/join (list/map food/entry-li entries))))))))))))

(define food/quick
  (lambda
    (date meal food calories)
    (food/log
      date
      meal
      food
      calories
      ""
      ""
      ""
      "")))

(define food/safe-num
  (lambda (s) (parse-float (as-str s))))

(define food/set-day
  (lambda
    (date day-type)
    (let
      ((existing
        (find-rows (quote food-day) "date" (as-str date))))
      (if
        (null? existing)
        (create
          (quote food-day)
          (dict-set*
            "{}"
            (list
              "date"
              (as-str date)
              "day-type"
              (as-str day-type)
              "author"
              (as-str (whoami)))))
        (update
          (quote food-day)
          (car (car existing))
          (dict-set*
            "{}"
            (list
              "date"
              (as-str date)
              "day-type"
              (as-str day-type)
              "author"
              (as-str (whoami)))))))))

(define food/set-target
  (lambda
    (day-type calories protein carbs fat)
    (let
      ((existing
        (find-rows
          (quote food-target)
          "day-type"
          (as-str day-type))))
      (if
        (null? existing)
        (create
          (quote food-target)
          (dict-set*
            "{}"
            (list
              "day-type"
              (as-str day-type)
              "calories"
              (as-str calories)
              "protein"
              (as-str protein)
              "carbs"
              (as-str carbs)
              "fat"
              (as-str fat)
              "author"
              (as-str (whoami)))))
        (update
          (quote food-target)
          (car (car existing))
          (dict-set*
            "{}"
            (list
              "day-type"
              (as-str day-type)
              "calories"
              (as-str calories)
              "protein"
              (as-str protein)
              "carbs"
              (as-str carbs)
              "fat"
              (as-str fat)
              "author"
              (as-str (whoami)))))))))

(define food/sum-calories
  (lambda
    (rows)
    (list/foldl
      (lambda (acc row) (+ acc (food/cal-of row)))
      0
      rows)))

(define food/sum-field
  (lambda
    (entries field)
    (food/foldl
      (lambda (acc e) (+ acc (food/entry-val e field)))
      0
      entries)))

(define food/target
  (lambda
    (day-type)
    (let
      ((row
        (find-rows
          (quote food-target)
          "day-type"
          (as-str day-type))))
      (if (null? row) "{}" (cdr (car row))))))

(define food/templates
  (lambda
    (day-type)
    (find-rows
      (quote food-template)
      "day-type"
      (as-str day-type))))

(define food/test
  (lambda () "hello from food"))

(define food/test2
  (lambda () (dict-set* "{}" (list "a" "1" "b" "2"))))

(define food/today
  (lambda
    ()
    (find-rows (quote food-log) "date" (date-today))))

(define food/today-total
  (lambda () (food/sum-calories (food/today))))

(define food/week-dates
  (lambda () (food/dates-back 6)))

(define food/week-summary
  (lambda
    ()
    (food/foldl
      (lambda
        (acc date)
        (cons (cons date (food/day-summary date)) acc))
      ()
      (food/week-dates))))
