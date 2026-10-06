;; package food -- exported from live branch system/main
;; Sync: scripts/pp-sync.sh status|pull|snapshot

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

(define food/apply-one
  (lambda
    (i meal)
    (let
      ((suf (string-append "_" (as-str i))))
      (let
        ((nm (request-param (string-append "name" suf)))
          (sv (request-param (string-append "servings" suf)))
          (ex (request-param (string-append "ex" suf))))
        (if
          (string-eq ex "1")
          (food/log-item (date-today) meal nm sv)
          (let
            ((cal (request-param (string-append "calories" suf)))
              (pro (request-param (string-append "protein" suf)))
              (cbr (request-param (string-append "carbs" suf)))
              (ft (request-param (string-append "fat" suf)))
              (sgr (request-param (string-append "sugar" suf)))
              (svng (request-param (string-append "serving" suf))))
            (if
              (string-eq
                (request-param (string-append "addreg" suf))
                "1")
              (seq
                (food/register
                  nm
                  svng
                  cal
                  pro
                  cbr
                  ft
                  sgr)
                (food/log-item (date-today) meal nm sv))
              (food/log
                (date-today)
                meal
                nm
                (as-str (* (food/safe-num cal) (food/safe-num sv)))
                (as-str (* (food/safe-num pro) (food/safe-num sv)))
                (as-str (* (food/safe-num cbr) (food/safe-num sv)))
                (as-str (* (food/safe-num ft) (food/safe-num sv)))
                (as-str (* (food/safe-num sgr) (food/safe-num sv)))
                (string-append sv " x " svng)))))))))

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
          ""
          ""))
      0
      (food/templates day-type)))
  "Apply all food-template entries for a given day-type to a date.")

(define food/b
  (lambda () 2))

(define food/cal-of
  (lambda
    (row)
    (parse-int (food/field (cdr row) "calories"))))

(define food/confirm-apply
  (lambda
    (i count meal)
    (if
      (>= i count)
      "done"
      (seq
        (food/apply-one i meal)
        (food/confirm-apply (+ i 1) count meal)))))

(define food/confirm-ex-row
  (lambda
    (it i)
    (let
      ((suf (string-append "_" (as-str i))))
      (let
        ((nm (as-str (nth it 1))) (sv (as-str (nth it 2))))
        (let
          ((r (food/item nm)))
          (html/div
            (html/attr
              "style"
              "display:flex;gap:.5em;align-items:center;flex-wrap:wrap;padding:.5em 0;border-bottom:1px solid rgba(128,128,128,.25)")
            (html/join
              (list
                (html/hidden (string-append "ex" suf) "1")
                (food/txt-input (string-append "name" suf) nm "16em")
                (html/span (html/attr "style" "opacity:.6") "x")
                (food/num-input
                  (string-append "servings" suf)
                  sv
                  "4.5em")
                (html/span
                  (html/attr "style" "font-size:.85em;opacity:.65")
                  (if
                    (null? r)
                    "not found in registry - fix the name before logging"
                    (food/registry-line r)))))))))))

(define food/confirm-new-row
  (lambda
    (it i)
    (let
      ((suf (string-append "_" (as-str i))))
      (html/div
        (html/attr
          "style"
          "display:flex;gap:.5em;align-items:center;flex-wrap:wrap;padding:.5em 0;border-bottom:1px solid rgba(128,128,128,.25)")
        (html/join
          (list
            (html/hidden (string-append "ex" suf) "0")
            (food/txt-input
              (string-append "name" suf)
              (as-str (nth it 1))
              "14em")
            (html/span (html/attr "style" "opacity:.6") "x")
            (food/num-input
              (string-append "servings" suf)
              (as-str (nth it 2))
              "4em")
            (html/span (html/attr "style" "opacity:.6") "per")
            (food/txt-input
              (string-append "serving" suf)
              (as-str (nth it 3))
              "11em")
            (html/span
              (html/attr "style" "font-size:.8em;opacity:.6")
              "cal")
            (food/num-input
              (string-append "calories" suf)
              (as-str (nth it 4))
              "4.5em")
            (html/span
              (html/attr "style" "font-size:.8em;opacity:.6")
              "P")
            (food/num-input
              (string-append "protein" suf)
              (as-str (nth it 5))
              "4em")
            (html/span
              (html/attr "style" "font-size:.8em;opacity:.6")
              "C")
            (food/num-input
              (string-append "carbs" suf)
              (as-str (nth it 6))
              "4em")
            (html/span
              (html/attr "style" "font-size:.8em;opacity:.6")
              "F")
            (food/num-input
              (string-append "fat" suf)
              (as-str (nth it 7))
              "4em")
            (html/span
              (html/attr "style" "font-size:.8em;opacity:.6")
              "S")
            (food/num-input
              (string-append "sugar" suf)
              (as-str (nth it 8))
              "4em")
            (html/tag
              "label"
              (html/attr
                "style"
                "font-size:.8em;opacity:.75;display:flex;gap:.3em;align-items:center")
              (string-append
                "<input type=\"checkbox\" name=\"addreg"
                suf
                "\" value=\"1\" checked>save to registry"))))))))

(define food/confirm-page
  (lambda
    (description items)
    (html/page
      (list
        (html/h1 "Confirm Meal")
        (html/div
          (html/attr
            "style"
            "font-size:.9em;opacity:.7;margin-bottom:.8em")
          (string-append "From: \"" description "\""))
        (html/form
          "POST"
          "/food/smart"
          ""
          (html/join
            (append
              (list
                (html/hidden "action" "confirm")
                (html/hidden "count" (as-str (length items)))
                (html/div
                  (html/attr "style" "margin-bottom:.6em")
                  (html/join
                    (list
                      (html/span
                        (html/attr "style" "margin-right:.5em;opacity:.7")
                        "Log all as:")
                      (html/select
                        "meal"
                        ""
                        (html/join
                          (list
                            (html/option "breakfast" "Breakfast" #f)
                            (html/option "lunch" "Lunch" #f)
                            (html/option "dinner" "Dinner" #f)
                            (html/option "snack" "Snack" #f))))))))
              (food/confirm-rows items 0 (list))
              (list
                (html/div
                  (html/attr "style" "margin:.8em 0;font-weight:600")
                  (food/confirm-totals-line items))
                (html/button (html/attr "type" "submit") "Log meal")))))
        (html/div
          (html/attr "style" "margin-top:1em;font-size:.9em")
          (html/a "/food/smart" "<- Back (discard)"))))))

(define food/confirm-row
  (lambda
    (it i)
    (if
      (food/it-kind? it)
      (food/confirm-ex-row it i)
      (food/confirm-new-row it i))))

(define food/confirm-rows
  (lambda
    (items i acc)
    (if
      (null? items)
      acc
      (food/confirm-rows
        (cdr items)
        (+ i 1)
        (append acc (list (food/confirm-row (car items) i)))))))

(define food/confirm-totals-line
  (lambda
    (items)
    (string-append
      "Total: "
      (as-str (round (food/sum-macro items "calories" 0)))
      " cal - P "
      (as-str (round (food/sum-macro items "protein" 0)))
      " - C "
      (as-str (round (food/sum-macro items "carbs" 0)))
      " - F "
      (as-str (round (food/sum-macro items "fat" 0)))
      " - S "
      (as-str (round (food/sum-macro items "sugar" 0))))))

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
        (tot-sgr (food/safe-num (dict-get tot "sugar")))
        (tgt-cal (food/safe-num (dict-get tgt "calories")))
        (tgt-pro (food/safe-num (dict-get tgt "protein")))
        (tgt-car (food/safe-num (dict-get tgt "carbs")))
        (tgt-fat (food/safe-num (dict-get tgt "fat")))
        (tgt-sgr (food/safe-num (dict-get tgt "sugar"))))
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
          "sugar"
          (as-str tot-sgr)
          "sugar-target"
          (as-str tgt-sgr)
          "sugar-remaining"
          (as-str (- tgt-sgr tot-sgr))
          "entries"
          (dict-get tot "entries")))))
  "Day summary with macro totals, targets, and remaining — now including sugar.")

(define food/day-total
  (lambda
    (date)
    (let*
      ((es (food/entries date))
        (cal (food/sum-field es "calories"))
        (pro (food/sum-field es "protein"))
        (car (food/sum-field es "carbs"))
        (fat (food/sum-field es "fat"))
        (sgr (food/sum-field es "sugar")))
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
          "sugar"
          (as-str sgr)
          "entries"
          (as-str (length es))))))
  "Sum all macros for a date including sugar.")

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
    (let
      ((f (cdr row)))
      (html/li
        ""
        (html/join
          (list
            (html/strong (html/text (food/field f "food")))
            (html/text
              (string-append
                " — "
                (food/field f "calories")
                " cal"
                " | P:"
                (food/field f "protein")
                " C:"
                (food/field f "carbs")
                " F:"
                (food/field f "fat")
                " S:"
                (food/field f "sugar")
                (if
                  (string-eq (food/field f "meal") "")
                  ""
                  (string-append " [" (food/field f "meal") "]")))))))))
  "Render a food-log entry as a list item with macros + meal.")

(define food/entry-str
  (lambda
    (entry field)
    (as-str (dict-get (cdr entry) field))))

(define food/entry-val
  (lambda
    (entry field)
    (food/safe-num (dict-get (cdr entry) field))))

(define food/fail-card
  (lambda
    (description raw)
    (html/page
      (list
        (html/h1 "Could not parse that")
        (html/div
          (html/attr "style" "margin-bottom:.8em")
          "The AI reply did not contain a JSON object. Try rephrasing, or paste the raw reply into the Agora chat.")
        (html/tag
          "pre"
          (html/attr
            "style"
            "white-space:pre-wrap;font-size:.8em;opacity:.75;border:1px solid rgba(128,128,128,.3);padding:.6em;max-height:20em;overflow:auto")
          raw)
        (html/div
          (html/attr "style" "margin-top:1em")
          (html/a "/food/smart" "<- Back"))))))

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
          (html/h2 "Quick Add")
          (html/form
            "POST"
            "/food"
            ""
            (html/join
              (list
                (html/hidden "action" "quickadd")
                (html/select
                  "meal"
                  ""
                  (html/join
                    (list
                      (html/option "breakfast" "Breakfast" #f)
                      (html/option "lunch" "Lunch" #f)
                      (html/option "dinner" "Dinner" #f)
                      (html/option "snack" "Snack" #f))))
                (html/text-input
                  "food"
                  (html/attr "placeholder" "food name"))
                (html/text-input
                  "calories"
                  (html/attr "placeholder" "cal"))
                (html/text-input
                  "protein"
                  (html/attr "placeholder" "protein (g)"))
                (html/text-input
                  "carbs"
                  (html/attr "placeholder" "carbs (g)"))
                (html/text-input
                  "fat"
                  (html/attr "placeholder" "fat (g)"))
                (html/text-input
                  "sugar"
                  (html/attr "placeholder" "sugar (g)"))
                (html/button (html/attr "type" "submit") "Log"))))))))
  "Render the quick-add form with sugar field.")

(define food/it-kind?
  (lambda (it) (string-eq (as-str (nth it 0)) "existing")))

(define food/item
  (lambda
    (name)
    (let
      ((rows
        (find-rows (quote food-item) "name" (as-str name))))
      (if (null? rows) () (car rows))))
  "Find a food item by exact name. Returns (id . fields) or ().")

(define food/item-by-id
  (lambda (id) (get (quote food-item) id))
  "Get a food item by its row id. Returns fields-json or ().")

(define food/item-delete
  (lambda (id) (delete (quote food-item) id))
  "Delete a food item by id.")

(define food/item-macro
  (lambda
    (it key)
    (let
      ((sv (food/safe-num (nth it 2))))
      (if
        (food/it-kind? it)
        (let
          ((r (food/item (nth it 1))))
          (if
            (null? r)
            0
            (* (food/safe-num (dict-get (cdr r) key)) sv)))
        (let
          ((idx (food/macro-idx key)))
          (* (food/safe-num (nth it idx)) sv))))))

(define food/item-options
  (lambda
    (items)
    (if
      (null? items)
      ""
      (let
        ((f (cdr (car items))))
        (string-append
          (html/option
            (food/field f "name")
            (string-append
              (food/field f "name")
              " ("
              (food/field f "calories")
              " cal)")
            #f)
          (food/item-options (cdr items))))))
  "Build option tags for a select dropdown from food items.")

(define food/item-row
  (lambda
    (row)
    (let
      ((f (cdr row)))
      (html/tag
        "tr"
        ""
        (html/join
          (list
            (html/tag "td" "" (html/strong (food/field f "name")))
            (html/tag "td" "" (html/text (food/field f "serving")))
            (html/tag "td" "" (html/text (food/field f "calories")))
            (html/tag "td" "" (html/text (food/field f "protein")))
            (html/tag "td" "" (html/text (food/field f "carbs")))
            (html/tag "td" "" (html/text (food/field f "fat")))
            (html/tag "td" "" (html/text (food/field f "sugar"))))))))
  "Render a food-item row as a table row with sugar column.")

(define food/item-search
  (lambda
    (needle)
    (find-text (quote food-item) "name" (as-str needle)))
  "Full-text search food items by name. Returns list of (id . fields).")

(define food/item-update
  (lambda
    (id
      name
      serving
      calories
      protein
      carbs
      fat
      sugar)
    (update
      (quote food-item)
      id
      (dict-set*
        "{}"
        (list
          "name"
          (as-str name)
          "serving"
          (as-str serving)
          "calories"
          (as-str calories)
          "protein"
          (as-str protein)
          "carbs"
          (as-str carbs)
          "fat"
          (as-str fat)
          "sugar"
          (as-str sugar)
          "author"
          (as-str (whoami))))))
  "Update all fields of a food item including sugar.")

(define food/item-valid?
  (lambda
    (it)
    (if
      (pair? it)
      (let
        ((n (length it)))
        (if
          (string-eq (as-str (nth it 0)) "existing")
          (if
            (string-eq (as-str n) "3")
            (food/numeric? (nth it 2))
            #f)
          (if
            (string-eq (as-str (nth it 0)) "new")
            (if
              (string-eq (as-str n) "9")
              (if
                (food/numeric? (nth it 2))
                (if
                  (food/numeric? (nth it 4))
                  (if
                    (food/numeric? (nth it 5))
                    (if
                      (food/numeric? (nth it 6))
                      (if
                        (food/numeric? (nth it 7))
                        (food/numeric? (nth it 8))
                        #f)
                      #f)
                    #f)
                  #f)
                #f)
              #f)
            #f)))
      #f)))

(define food/items
  (lambda () (rows (quote food-item) 500))
  "Return all registered food items as (id . fields) pairs.")

(define food/items-page
  (lambda
    ()
    (if
      (string-eq (request-method) "POST")
      (let
        ((action (request-param "action")))
        (if
          (string-eq action "register")
          (seq
            (food/register
              (request-param "name")
              (request-param "serving")
              (request-param "calories")
              (request-param "protein")
              (request-param "carbs")
              (request-param "fat")
              (request-param "sugar"))
            (redirect "/food/items"))
          (if
            (string-eq action "quicklog")
            (seq
              (food/log-item
                (date-today)
                (request-param "meal")
                (request-param "name")
                (request-param "servings"))
              (redirect "/food"))
            (redirect "/food/items"))))
      (html/page
        (list
          (html/h1 "Food Items")
          (html/a
            (html/attr "href" "/food")
            (html/text "← Back to Food Log"))
          (food/quick-log-form)
          (food/register-form)
          (food/items-table)))))
  "Handler for /food/items — register food items, browse registry, quick-log.")

(define food/items-table
  (lambda
    ()
    (let
      ((items (food/items)))
      (if
        (null? items)
        (html/p
          (html/attr "class" "muted")
          (html/text "No food items registered yet."))
        (html/tag
          "table"
          (html/attr "class" "food-items-table")
          (html/join
            (list
              (html/tag
                "thead"
                ""
                (html/tag
                  "tr"
                  ""
                  (html/join
                    (list
                      (html/tag "th" "" (html/text "Food"))
                      (html/tag "th" "" (html/text "Serving"))
                      (html/tag "th" "" (html/text "Cal"))
                      (html/tag "th" "" (html/text "Protein"))
                      (html/tag "th" "" (html/text "Carbs"))
                      (html/tag "th" "" (html/text "Fat"))
                      (html/tag "th" "" (html/text "Sugar"))))))
              (html/tag
                "tbody"
                ""
                (html/join (list/map food/item-row items)))))))))
  "Render all registered food items as an HTML table with sugar column.")

(define food/items-valid?
  (lambda
    (items)
    (if
      (pair? items)
      (if
        (food/item-valid? (car items))
        (if
          (null? (cdr items))
          #t
          (food/items-valid? (cdr items)))
        #f)
      #f)))

(define food/js-quote
  (lambda
    (s)
    (string-append
      "\""
      (string-replace
        (string-replace (as-str s) "\\" "\\\\")
        "\""
        "\\\"")
      "\"")))

(define food/log
  (lambda
    (date
      meal
      food
      calories
      protein
      carbs
      fat
      sugar
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
          "sugar"
          (as-str sugar)
          "notes"
          notes
          "author"
          (as-str (whoami))))))
  "Create a food-log row. sugar is grams of sugar (string or number).")

(define food/log-item
  (lambda
    (date meal name servings)
    (let
      ((row (food/item name)))
      (if
        (null? row)
        (string-append "food item not found: " (as-str name))
        (let*
          ((item (cdr row))
            (cal (food/safe-num (dict-get item "calories")))
            (pro (food/safe-num (dict-get item "protein")))
            (cbr (food/safe-num (dict-get item "carbs")))
            (ft (food/safe-num (dict-get item "fat")))
            (sgr (food/safe-num (dict-get item "sugar")))
            (sv (food/safe-num servings)))
          (food/log
            date
            meal
            (as-str (dict-get item "name"))
            (as-str (* cal sv))
            (as-str (* pro sv))
            (as-str (* cbr sv))
            (as-str (* ft sv))
            (as-str (* sgr sv))
            (string-append
              (as-str sv)
              " x "
              (as-str (dict-get item "serving"))))))))
  "Log a registered food item by name. Multiplies all macros (including sugar) by servings count.")

(define food/macro-idx
  (lambda
    (key)
    (if
      (string-eq key "calories")
      4
      (if
        (string-eq key "protein")
        5
        (if
          (string-eq key "carbs")
          6
          (if (string-eq key "fat") 7 8))))))

(define food/name-of
  (lambda (row) (food/field (cdr row) "food")))

(define food/num-input
  (lambda
    (name val width)
    (html/tag
      "input"
      (string-append
        (html/attr "name" name)
        " "
        (html/attr "value" val)
        " "
        "type=\"number\" step=\"0.1\" min=\"0\" "
        (html/attr "style" (string-append "width:" width)))
      "")))

(define food/numeric-chk
  (lambda
    (s i)
    (if
      (>= i (string-length s))
      (if (> i 0) #t #f)
      (if
        (string-contains? "0123456789.-" (substring s i 1))
        (food/numeric-chk s (+ i 1))
        #f))))

(define food/numeric?
  (lambda (v) (food/numeric-chk (as-str v) 0)))

(define food/page
  (lambda
    ()
    (if
      (string-eq (request-method) "POST")
      (let
        ((action (request-param "action")))
        (if
          (string-eq action "quickadd")
          (seq
            (food/log
              (date-today)
              (request-param "meal")
              (request-param "food")
              (request-param "calories")
              (request-param "protein")
              (request-param "carbs")
              (request-param "fat")
              (request-param "sugar")
              "")
            (redirect "/food"))
          (redirect "/food")))
      (let*
        ((summary (food/day-summary (date-today)))
          (entries (food/entries (date-today))))
        (html/page
          (list
            (html/h1 "Food Log")
            (html/a
              (html/attr "href" "/food/items")
              (html/text "Food Item Registry →"))
            (html/div
              (html/attr "class" "card")
              (html/join
                (list
                  (html/h2
                    (string-append
                      "Today — "
                      (food/field summary "calories")
                      " cal"))
                  (html/p*
                    ""
                    (html/text
                      (string-append
                        "Target: "
                        (food/field summary "calories-target")
                        " cal"
                        " | Remaining: "
                        (food/field summary "calories-remaining")
                        " cal")))
                  (html/p*
                    ""
                    (html/text
                      (string-append
                        "Protein: "
                        (food/field summary "protein")
                        "/"
                        (food/field summary "protein-target")
                        "g"
                        " | Carbs: "
                        (food/field summary "carbs")
                        "/"
                        (food/field summary "carbs-target")
                        "g"
                        " | Fat: "
                        (food/field summary "fat")
                        "/"
                        (food/field summary "fat-target")
                        "g"
                        " | Sugar: "
                        (food/field summary "sugar")
                        "/"
                        (food/field summary "sugar-target")
                        "g"))))))
            (food/form)
            (html/div
              (html/attr "class" "card")
              (html/join
                (list
                  (html/h2 "Today's Entries")
                  (if
                    (null? entries)
                    (html/p*
                      (html/attr "class" "muted")
                      (html/text "No entries yet."))
                    (html/ul
                      ""
                      (html/join (list/map food/entry-li entries))))))))))))
  "Food log page handler — POST logs food, GET shows day summary with sugar.")

(define food/parse-description
  (lambda
    (description)
    (let
      ((raw (llm-chat (food/system-prompt) description)))
      (let
        ((cand (sexp/extract raw)))
        (if
          (null? cand)
          (food/fail-card description raw)
          (let
            ((items (sexp/parse cand)))
            (if
              (food/items-valid? items)
              (food/confirm-page description items)
              (food/fail-card description raw))))))))

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
      ""
      ""))
  "Quick-log with just calories; other macros blank.")

(define food/quick-log-form
  (lambda
    ()
    (let
      ((items (food/items)))
      (html/div
        (html/attr "class" "card")
        (html/join
          (list
            (html/h2 "Quick Log")
            (html/form
              "POST"
              "/food/items"
              ""
              (html/join
                (list
                  (html/hidden "action" "quicklog")
                  (html/select
                    "name"
                    (html/attr "placeholder" "Select food")
                    (food/item-options items))
                  (html/select
                    "meal"
                    ""
                    (html/join
                      (list
                        (html/option "breakfast" "Breakfast" #f)
                        (html/option "lunch" "Lunch" #f)
                        (html/option "dinner" "Dinner" #f)
                        (html/option "snack" "Snack" #f))))
                  (html/text-input
                    "servings"
                    (html/attr "placeholder" "servings (e.g. 1, 1.5, 2)"))
                  (html/button (html/attr "type" "submit") "Log")))))))))
  "Render the quick-log form — pick a registered food, meal, servings, and log it.")

(define food/register
  (lambda
    (name
      serving
      calories
      protein
      carbs
      fat
      sugar)
    (create
      (quote food-item)
      (dict-set*
        "{}"
        (list
          "name"
          (as-str name)
          "serving"
          (as-str serving)
          "calories"
          (as-str calories)
          "protein"
          (as-str protein)
          "carbs"
          (as-str carbs)
          "fat"
          (as-str fat)
          "sugar"
          (as-str sugar)
          "author"
          (as-str (whoami))))))
  "Register a food item with macros including sugar. Returns the new row id.")

(define food/register-form
  (lambda
    ()
    (html/div
      (html/attr "class" "card")
      (html/join
        (list
          (html/h2 "Register Food Item")
          (html/form
            "POST"
            "/food/items"
            ""
            (html/join
              (list
                (html/hidden "action" "register")
                (html/text-input
                  "name"
                  (html/attr "placeholder" "Food name"))
                (html/text-input
                  "serving"
                  (html/attr
                    "placeholder"
                    "serving size e.g. 100g, 1 cup"))
                (html/text-input
                  "calories"
                  (html/attr "placeholder" "calories"))
                (html/text-input
                  "protein"
                  (html/attr "placeholder" "protein (g)"))
                (html/text-input
                  "carbs"
                  (html/attr "placeholder" "carbs (g)"))
                (html/text-input
                  "fat"
                  (html/attr "placeholder" "fat (g)"))
                (html/text-input
                  "sugar"
                  (html/attr "placeholder" "sugar (g)"))
                (html/button (html/attr "type" "submit") "Register"))))))))
  "Render the food item registration form with sugar field.")

(define food/registry-line
  (lambda
    (row)
    (let
      ((it (cdr row)))
      (string-append
        (as-str (dict-get it "name"))
        " ("
        (as-str (dict-get it "serving"))
        "): "
        (as-str (dict-get it "calories"))
        "cal "
        (as-str (dict-get it "protein"))
        "p "
        (as-str (dict-get it "carbs"))
        "c "
        (as-str (dict-get it "fat"))
        "f "
        (as-str (dict-get it "sugar"))
        "s"))))

(define food/registry-lines
  (lambda () (food/registry-lines-j (food/items) "")))

(define food/registry-lines-j
  (lambda
    (rows acc)
    (if
      (null? rows)
      acc
      (food/registry-lines-j
        (cdr rows)
        (if
          (string-eq acc "")
          (food/registry-line (car rows))
          (string-append acc "; " (food/registry-line (car rows))))))))

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
    (day-type calories protein carbs fat sugar)
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
              "sugar"
              (as-str sugar)
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
              "sugar"
              (as-str sugar)
              "author"
              (as-str (whoami))))))))
  "Set macro targets for a day-type including sugar.")

(define food/smart-item-js
  (lambda
    (row)
    (let
      ((it (cdr row)))
      (string-append
        "{\"name\":"
        (food/js-quote (dict-get it "name"))
        ",\"serving\":"
        (food/js-quote (dict-get it "serving"))
        ",\"cal\":"
        (as-str (dict-get it "calories"))
        ",\"pro\":"
        (as-str (dict-get it "protein"))
        ",\"carb\":"
        (as-str (dict-get it "carbs"))
        ",\"fat\":"
        (as-str (dict-get it "fat"))
        ",\"sugar\":"
        (as-str (dict-get it "sugar"))
        "}"))))

(define food/smart-items-js
  (lambda
    ()
    (string-append
      "["
      (food/smart-join-js (food/items) "")
      "]")))

(define food/smart-join-js
  (lambda
    (rows acc)
    (if
      (null? rows)
      acc
      (food/smart-join-js
        (cdr rows)
        (if
          (string-eq acc "")
          (food/smart-item-js (car rows))
          (string-append acc "," (food/smart-item-js (car rows))))))))

(define food/smart-page
  (lambda
    ()
    (if
      (string-eq (request-method) "POST")
      (let
        ((action (request-param "action")))
        (if
          (string-eq action "quicklog")
          (seq
            (food/log-item
              (date-today)
              (request-param "meal")
              (request-param "name")
              (request-param "servings"))
            (redirect "/food"))
          (if
            (string-eq action "parse")
            (let
              ((d (string-trim (request-param "description"))))
              (if
                (string-eq d "")
                (redirect "/food/smart")
                (food/parse-description d)))
            (if
              (string-eq action "confirm")
              (seq
                (food/confirm-apply
                  0
                  (parse-int (request-param "count"))
                  (request-param "meal"))
                (redirect "/food"))
              (redirect "/food/smart")))))
      (html/page
        (list
          (html/h1 "Log Food")
          (html/a "/food" "<- Back to Food Log")
          (html/div
            (html/attr "style" "margin:1em 0")
            (html/tag
              "input"
              "id=\"fq\" type=\"search\" placeholder=\"What did you eat?\" autocomplete=\"off\" style=\"width:100%;padding:.6em;font-size:1.05em;box-sizing:border-box\""
              ""))
          (html/div
            (html/attr
              "style"
              "font-size:.85em;opacity:.65;margin-bottom:.4em")
            "Type to filter the registry.")
          (html/tag "div" (html/attr "id" "fres") "")
          (html/form
            "POST"
            "/food/smart"
            (html/attr
              "style"
              "margin-top:1.5em;border-top:1px solid rgba(128,128,128,.3);padding-top:1em")
            (html/join
              (list
                (html/hidden "action" "parse")
                (html/div
                  (html/attr "style" "font-weight:600")
                  "Not in the registry? Describe the food or meal")
                (html/tag
                  "input"
                  "name=\"description\" type=\"text\" placeholder=\"e.g. two eggs, a banana and a handful of peanuts\" autocomplete=\"off\" style=\"width:100%;padding:.6em;box-sizing:border-box;margin:.5em 0\""
                  "")
                (html/button
                  (html/attr "type" "submit")
                  "Parse with AI")
                (html/div
                  (html/attr
                    "style"
                    "font-size:.8em;opacity:.6;margin-top:.3em")
                  "Takes ~20s. You review and adjust everything before anything is logged."))))
          (html/tag
            "script"
            ""
            (string-append
              "const foodItems="
              (food/smart-items-js)
              ";const box=document.getElementById('fq');const res=document.getElementById('fres');function esc(s){return String(s).replace(/[&<>\"']/g,function(c){return {'&':'&amp;','<':'&lt;','>':'&gt;','\"':'&quot;',\"'\":'&#39;'}[c];});}function defaultMeal(){const h=new Date().getHours();if(h<11)return 'breakfast';if(h<15)return 'lunch';if(h<21)return 'dinner';return 'snack';}function mealOpts(){return ['breakfast','lunch','dinner','snack'].map(function(m){return '<option value=\"'+m+'\"'+(m===defaultMeal()?' selected':'')+'>'+m+'</option>';}).join('');}function rowHTML(it){return '<div style=\"display:flex;gap:.6em;align-items:center;flex-wrap:wrap;padding:.55em 0;border-bottom:1px solid rgba(128,128,128,.25)\">'+'<div style=\"min-width:230px;flex:1\"><strong>'+esc(it.name)+'</strong><br>'+'<span style=\"font-size:.85em;opacity:.65\">'+esc(it.serving)+' &middot; '+it.cal+' cal &middot; P'+it.pro+' C'+it.carb+' F'+it.fat+' S'+it.sugar+'</span></div>'+'<form method=\"POST\" action=\"/food/smart\" style=\"display:flex;gap:.4em;align-items:center;margin:0\">'+'<input type=\"hidden\" name=\"action\" value=\"quicklog\">'+'<input type=\"hidden\" name=\"name\" value=\"'+esc(it.name)+'\">'+'<input type=\"number\" name=\"servings\" value=\"1\" min=\"0\" step=\"0.5\" style=\"width:4.5em\">'+'<select name=\"meal\">'+mealOpts()+'</select>'+'<button type=\"submit\">Log</button></form></div>';}function render(list){if(!list.length){res.innerHTML='<p style=\"opacity:.7\">No match in the registry. Describe the food or meal in the Agora chat and it can be added.</p>';return;}res.innerHTML=list.map(rowHTML).join('');}function doFilter(){const q=box.value.trim().toLowerCase();render(q?foodItems.filter(function(i){return i.name.toLowerCase().indexOf(q)>=0;}):foodItems);}box.addEventListener('input',doFilter);render(foodItems);")))))))

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

(define food/sum-macro
  (lambda
    (items key acc)
    (if
      (null? items)
      acc
      (food/sum-macro
        (cdr items)
        key
        (+ acc (food/item-macro (car items) key))))))

(define food/system-prompt
  (lambda
    ()
    (string-append
      "You convert a meal description into a Lisp-style list for a food tracker. "
      "Your reply MUST BEGIN with the list: the very first character of your reply must be ( . "
      "No preamble, no analysis before the list. "
      "Structure: a list of item lists. Each item is either "
      "(\"existing\" \"EXACT NAME FROM REGISTRY\" SERVINGS) for foods in the registry, or "
      "(\"new\" \"FOOD NAME\" SERVINGS \"ONE SERVING e.g. 1 oz (28g)\" CALORIES PROTEIN CARBS FAT SUGAR) for foods not in the registry. "
      "SERVINGS and the macro fields are plain numbers, no units. serving and names are quoted strings. "
      "SERVINGS means how many of those serving sizes were eaten (two eggs = servings 2). "
      "Always prefer a registry item when it reasonably matches. "
      "For non-registry foods, estimate typical nutrition per serving using your nutrition knowledge; do not leave macros at 0 unless truly negligible. "
      "Example reply: ((\"existing\" \"Egg\" 2) (\"new\" \"Peanuts\" 1 \"1 oz (28g)\" 160 7 6 14 1)) "
      "Registry: "
      (food/registry-lines))))

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

(define food/txt-input
  (lambda
    (name val width)
    (html/tag
      "input"
      (string-append
        (html/attr "name" name)
        " "
        (html/attr "value" val)
        " "
        "type=\"text\" "
        (html/attr "style" (string-append "width:" width)))
      "")))

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
