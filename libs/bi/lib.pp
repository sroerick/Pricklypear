;; bi/ — governed metrics + Vega-Lite dashboards (MVP)
;; Seeded on system/main. Incremental graph deferred; metrics are plain
;; Lisp over data_rows (find-rows / rows). Agent tools may READ metrics
;; and edit dashboard layout; they must not redefine metric semantics.

(deftype 'bi-expense 'day 'category 'amount 'note)
(deftype 'bi-revenue 'day 'source 'amount 'note)
(deftype 'bi-metric-cache 'name 'value 'updated_at)
(deftype 'dashboard-revision 'name 'source 'prompt 'author 'created_at)

;; ---------- sample mart (idempotent-ish seed) ----------

(define bi/seed-sample-data
  (lambda ()
    (seq
      (if (null? (rows 'bi-expense 1))
          (seq
            (create 'bi-expense
              (dict-set* "{}"
                (list "day" "2026-07-01" "category" "hosting"
                      "amount" "42.00" "note" "Vultr")))
            (create 'bi-expense
              (dict-set* "{}"
                (list "day" "2026-07-05" "category" "software"
                      "amount" "20.00" "note" "domain")))
            (create 'bi-expense
              (dict-set* "{}"
                (list "day" "2026-07-12" "category" "hosting"
                      "amount" "42.00" "note" "Vultr")))
            (create 'bi-expense
              (dict-set* "{}"
                (list "day" "2026-07-18" "category" "tools"
                      "amount" "15.50" "note" "CDN")))
            0)
          0)
      (if (null? (rows 'bi-revenue 1))
          (seq
            (create 'bi-revenue
              (dict-set* "{}"
                (list "day" "2026-07-03" "source" "consulting"
                      "amount" "500.00" "note" "week 1")))
            (create 'bi-revenue
              (dict-set* "{}"
                (list "day" "2026-07-10" "source" "product"
                      "amount" "120.00" "note" "subs")))
            (create 'bi-revenue
              (dict-set* "{}"
                (list "day" "2026-07-17" "source" "consulting"
                      "amount" "350.00" "note" "week 3")))
            0)
          0)
      "seeded")))

;; ---------- helpers ----------

(define bi/parse-amount
  (lambda (s)
    (let ((f (parse-float (as-str s))))
      (if (null? f) 0.0 f))))

(define bi/row-amount
  (lambda (row)
    (bi/parse-amount (dict-get (cdr row) "amount"))))

(define bi/sum-amounts
  (lambda (rows)
    (list/foldl
      (lambda (acc row) (+ acc (bi/row-amount row)))
      0.0
      rows)))

;; ---------- governed metrics (read-only contract for agents) ----------

(define bi/metric/expense-total
  (lambda ()
    (bi/sum-amounts (rows 'bi-expense 500))))

(define bi/metric/revenue-total
  (lambda ()
    (bi/sum-amounts (rows 'bi-revenue 500))))

(define bi/metric/profit
  (lambda ()
    (- (bi/metric/revenue-total) (bi/metric/expense-total))))

(define bi/metric/profit-margin
  (lambda ()
    (let ((rev (bi/metric/revenue-total)))
      (if (= rev 0.0)
          0.0
          (div (bi/metric/profit) rev)))))

(define bi/metric/expense-by-category
  (lambda ()
    (list/foldl
      (lambda (acc row)
        (let ((cat (as-str (dict-get (cdr row) "category")))
              (amt (bi/row-amount row)))
          (let ((prev (dict-get acc cat)))
            (dict-set acc cat
              (as-str
                (if (null? prev) amt (+ (bi/parse-amount prev) amt)))))))
      "{}"
      (rows 'bi-expense 500))))

(define bi/metrics-snapshot
  (lambda ()
    (dict-set*
      "{}"
      (list
        "revenue" (as-str (bi/metric/revenue-total))
        "expenses" (as-str (bi/metric/expense-total))
        "profit" (as-str (bi/metric/profit))
        "margin" (as-str (bi/metric/profit-margin))
        "by_category" (bi/metric/expense-by-category)))))

;; Optional MVP cache: write computed snapshot into data_rows.
(define bi/cache-metrics!
  (lambda ()
    (let ((snap (bi/metrics-snapshot))
          (now (date-today)))
      (let ((existing (find-row 'bi-metric-cache "name" "snapshot")))
        (if (null? existing)
            (create 'bi-metric-cache
              (dict-set* "{}"
                (list "name" "snapshot" "value" snap "updated_at" now)))
            (update 'bi-metric-cache (car existing)
              (dict-set* "{}"
                (list "name" "snapshot" "value" snap "updated_at" now))))))))

;; ---------- Vega-Lite helper ----------

;; (bi/vega-lite spec-json) → HTML string with embed + CDN script.
;; Spec is a JSON string (agent or hand-written).
(define bi/vega-lite
  (lambda (spec-json)
    (let ((id (string-append "vl-" (as-str (gensym "c"))))
          (spec (as-str spec-json)))
      (html/join
        (list
          (html/tag "div" (html/attr "id" id) "")
          (html/tag "script" ""
            (string-append
              "(function(){var el=document.getElementById('" id "');"
              "if(!el)return;"
              "function go(){if(window.vegaEmbed){vegaEmbed('#" id "',"
              spec
              ",{actions:false}).catch(function(e){el.textContent=String(e);});}"
              "else{setTimeout(go,50);}}go();})();")))))))

(define bi/vega-cdn
  (lambda ()
    (html/join
      (list
        (html/tag "script"
          (html/attr "src" "https://cdn.jsdelivr.net/npm/vega@5")
          "")
        (html/tag "script"
          (html/attr "src" "https://cdn.jsdelivr.net/npm/vega-lite@5")
          "")
        (html/tag "script"
          (html/attr "src" "https://cdn.jsdelivr.net/npm/vega-embed@6")
          "")))))

(define bi/join-json-array
  (lambda (xs)
    (list/foldl
      (lambda (acc x)
        (if (string-eq acc "")
            x
            (string-append acc "," x)))
      ""
      xs)))

(define bi/spec-expense-bars
  (lambda ()
    (let ((by (bi/metric/expense-by-category)))
      (let ((keys (dict-keys by)))
        (let ((values
                (list/map
                  (lambda (k)
                    (string-append
                      "{\"category\":"
                      (json-emit (as-str k))
                      ",\"amount\":"
                      (as-str (bi/parse-amount (dict-get by k)))
                      "}"))
                  keys)))
          (string-append
            "{\"$schema\":\"https://vega.github.io/schema/vega-lite/v5.json\","
            "\"description\":\"Expenses by category\","
            "\"width\":320,\"height\":200,"
            "\"data\":{\"values\":["
            (bi/join-json-array values)
            "]},"
            "\"mark\":\"bar\","
            "\"encoding\":{"
            "\"x\":{\"field\":\"category\",\"type\":\"nominal\",\"title\":\"Category\"},"
            "\"y\":{\"field\":\"amount\",\"type\":\"quantitative\",\"title\":\"Amount\"}"
            "}}"))))))

;; ---------- reference dashboard ----------

(define bi/fmt-money
  (lambda (n)
    (string-append "$" (as-str n))))

(define bi/metric-card
  (lambda (label value)
    (html/div (html/attr "class" "bi-card")
      (html/join
        (list
          (html/div (html/attr "class" "bi-card-label") (html/text label))
          (html/div (html/attr "class" "bi-card-value")
            (html/text (as-str value))))))))

(define bi/dashboard-html
  (lambda ()
    (let ((snap (bi/metrics-snapshot)))
      (html/page
        (list
          (bi/vega-cdn)
          (html/tag "style" ""
            (string-append
              ".bi-grid{display:grid;grid-template-columns:repeat(auto-fit,minmax(10rem,1fr));gap:.75rem;margin:1rem 0;}"
              ".bi-card{border:2px solid #000;padding:.75rem;background:#fff;}"
              ".bi-card-label{font-size:.8rem;text-transform:uppercase;letter-spacing:.04em;}"
              ".bi-card-value{font-size:1.4rem;font-weight:700;margin-top:.25rem;}"
              ".bi-chart{margin:1.25rem 0;border:2px solid #000;padding:1rem;background:#fff;}"))
          (html/h1 "P&amp;L Dashboard")
          (html/p* (html/attr "class" "muted")
            "Governed metrics over bi-expense / bi-revenue. Agents may edit layout; metrics stay read-only.")
          (html/div (html/attr "class" "bi-grid")
            (html/join
              (list
                (bi/metric-card "Revenue" (dict-get snap "revenue"))
                (bi/metric-card "Expenses" (dict-get snap "expenses"))
                (bi/metric-card "Profit" (dict-get snap "profit"))
                (bi/metric-card "Margin" (dict-get snap "margin")))))
          (html/div (html/attr "class" "bi-chart")
            (html/join
              (list
                (html/h2 "Expenses by category")
                (bi/vega-lite (bi/spec-expense-bars)))))
          (html/p*
            (html/attr "class" "muted")
            (html/join
              (list
                (html/text "API: ")
                (html/code "(bi/metrics-snapshot)")
                (html/text " · ")
                (html/code "(bi/dashboard/source)")
                (html/text " · ")
                (html/a "/bi" (html/text "refresh"))))))))))

(define bi/dashboard
  (lambda ()
    (seq
      (bi/seed-sample-data)
      (bi/dashboard-html))))

;; ---------- agent tools (layout only; metrics read-only) ----------

(define bi/dashboard/source
  (lambda ()
    (function-source "bi/dashboard-html")))

(define bi/dashboard/read
  (lambda ()
    (dict-set*
      "{}"
      (list
        "metrics" (bi/metrics-snapshot)
        "source" (bi/dashboard/source)
        "route" "/bi"))))

;; Install new dashboard HTML function body (string) and record revision.
;; Does NOT redefine bi/metric/* — trust boundary.
(define bi/dashboard/publish
  (lambda (source-body prompt)
    (seq
      (install-function "bi/dashboard-html" (as-str source-body))
      (create 'dashboard-revision
        (dict-set* "{}"
          (list
            "name" "bi/dashboard-html"
            "source" (as-str source-body)
            "prompt" (as-str prompt)
            "author" (whoami)
            "created_at" (date-today))))
      "published")))

;; Preview helper: eval source string to HTML without installing.
;; (Agents draft on a branch via fork/checkout/merge for true preview.)
(define bi/dashboard/preview-note
  (lambda ()
    "Branch preview: (fork \"bi-preview\") (checkout \"…/bi-preview\") then edit bi/dashboard-html; merge when ready. Metrics functions stay on main and are read-only to layout agents."))

;; ---------- routes ----------

(route "/bi" "bi/dashboard")
(route "/bi/api/metrics" "bi/metrics-snapshot")
