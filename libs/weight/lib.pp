;; package weight -- exported from live branch system/main
;; Sync: scripts/pp-sync.sh status|pull|snapshot

(define weight/all
  (lambda
    ()
    (list/foldl
      weight/insert-sorted
      ()
      (rows (quote weight-log) 500)))
  "Return all weight-log rows sorted by date.")

(define weight/avg-r
  (lambda
    (pairs from to s n)
    (if
      (null? pairs)
      (if (= n 0) 0.0 (/ s (+ 0.0 n)))
      (if
        (string>=? (car (car pairs)) from)
        (if
          (string<=? (car (car pairs)) to)
          (weight/avg-r
            (cdr pairs)
            from
            to
            (+ s (cadr (car pairs)))
            (+ n 1))
          (weight/avg-r (cdr pairs) from to s n))
        (weight/avg-r (cdr pairs) from to s n))))
  "Sum/count day-pairs whose date falls in [from, to] (ISO strings).")

(define weight/avg-window
  (lambda
    (pairs d)
    (weight/avg-r pairs (date-add-days d -6) d 0.0 0))
  "Trailing 7-day average of day-pairs up to and including date d.")

(define weight/chart-svg
  (lambda
    (rows)
    (let
      ((n (weight/len rows)))
      (if
        (< n 1)
        "<div class=\"muted\">No weight entries yet - log one below.</div>"
        (let
          ((rawmin (weight/min-w rows))
            (rawmax (weight/max-w rows)))
          (let
            ((wmin (- rawmin 2.0)) (wmax (+ rawmax 2.0)))
            (let
              ((coords (weight/coords rows 0 n wmin wmax)))
              (string-append
                "<svg viewBox=\"0 0 760 360\" width=\"100%\" height=\"360\" "
                "style=\"max-width:760px\" preserveAspectRatio=\"xMidYMid meet\">"
                "<rect x=\"60\" y=\"20\" width=\"680\" height=\"300\" "
                "fill=\"#f7f7f9\" stroke=\"#e2e2e8\"/>"
                "<line x1=\"60\" y1=\"20\" x2=\"60\" y2=\"320\" stroke=\"#ccc\"/>"
                "<line x1=\"60\" y1=\"320\" x2=\"740\" y2=\"320\" stroke=\"#ccc\"/>"
                "<text x=\"54\" y=\"24\" text-anchor=\"end\" font-size=\"11\" fill=\"#666\">"
                (as-str (round wmax))
                "</text>"
                "<text x=\"54\" y=\"320\" text-anchor=\"end\" font-size=\"11\" fill=\"#666\">"
                (as-str (round wmin))
                "</text>"
                "<polyline points=\""
                (weight/pts-str coords)
                "\" fill=\"none\" stroke=\"#2a7\" stroke-width=\"2\"/>"
                (weight/dots coords)
                "</svg>")))))))
  "Render an SVG line chart of the given weight rows.")

(define weight/coords
  (lambda
    (rows i n wmin wmax)
    (if
      (null? rows)
      (list)
      (let
        ((w (weight/wt-num (car rows))))
        (cons
          (list (weight/x-of i n) (weight/y-of w wmin wmax))
          (weight/coords (cdr rows) (+ i 1) n wmin wmax)))))
  "Map weight rows to (x y) chart coordinates.")

(define weight/day-pairs
  (lambda
    (rows)
    (weight/day-pairs-r rows (list) "" 0.0 0))
  "Collapse date-sorted weight rows into (date avg-weight) pairs, one per date.")

(define weight/day-pairs-r
  (lambda
    (rows acc d s n)
    (if
      (null? rows)
      (if
        (= n 0)
        acc
        (append acc (list (list d (/ s (+ 0.0 n))))))
      (if
        (string-eq (weight/ts-of (car rows)) d)
        (weight/day-pairs-r
          (cdr rows)
          acc
          d
          (+ s (weight/wt-num (car rows)))
          (+ n 1))
        (if
          (= n 0)
          (weight/day-pairs-r
            (cdr rows)
            acc
            (weight/ts-of (car rows))
            (weight/wt-num (car rows))
            1)
          (weight/day-pairs-r
            (cdr rows)
            (append acc (list (list d (/ s (+ 0.0 n)))))
            (weight/ts-of (car rows))
            (weight/wt-num (car rows))
            1))))))

(define weight/delete
  (lambda (id) (delete (quote weight-log) id))
  "Delete a weight-log row by id.")

(define weight/dots
  (lambda
    (coords)
    (list/foldl
      (lambda
        (acc p)
        (string-append
          acc
          "<circle cx=\""
          (as-str (car p))
          "\" cy=\""
          (as-str (car (cdr p)))
          "\" r=\"3\" fill=\"#2a7\" stroke=\"#fff\" stroke-width=\"1\"/>"))
      ""
      coords))
  "Emit SVG circle dots for each chart coordinate.")

(define weight/field
  (lambda
    (fields key)
    (let
      ((v (dict-get fields key)))
      (if (null? v) "" (as-str v))))
  "dict-get over a fields JSON string; empty string when absent.")

(define weight/fields
  (lambda
    (ts weight)
    (dict-set*
      "{}"
      (list
        "ts"
        (as-str ts)
        "weight"
        (as-str weight)
        "author"
        (as-str (whoami)))))
  "Build a weight-log fields JSON string.")

(define weight/fmt
  (lambda
    (w)
    (let
      ((s (as-str w)))
      (if (blank? s) s (as-str (parse-float s)))))
  "Format a weight value as a trimmed number.")

(define weight/fmt1
  (lambda (x) (as-str (/ (round (* x 10.0)) 10.0)))
  "Format a number rounded to one decimal place.")

(define weight/form
  (lambda
    ()
    (html/div
      (html/attr "class" "card")
      (html/join
        (list
          (html/h2 "Log weight")
          (html/form
            "POST"
            "/weight"
            ""
            (html/join
              (list
                (html/label (html/attr "for" "w-date") "Date")
                (html/text-input
                  "date"
                  (html/attr "value" (date-today)))
                (html/label (html/attr "for" "w-weight") "Weight (lbs)")
                (html/text-input
                  "weight"
                  (html/attr "placeholder" "185.4"))
                (html/button (html/attr "type" "submit") "Save")
                (html/hidden "action" "log"))))))))
  "Render the log-weight HTML form.")

(define weight/insert-sorted
  (lambda
    (sorted row)
    (if
      (null? sorted)
      (list row)
      (let
        ((d (weight/ts-of row))
          (hd (weight/ts-of (car sorted))))
        (if
          (string<=? d hd)
          (cons row sorted)
          (cons
            (car sorted)
            (weight/insert-sorted (cdr sorted) row))))))
  "Insert a row into a date-sorted list.")

(define weight/last
  (lambda
    (xs)
    (if (null? (cdr xs)) (car xs) (weight/last (cdr xs))))
  "Return the last element of a non-empty list.")

(define weight/len
  (lambda
    (xs)
    (list/foldl (lambda (acc x) (+ acc 1)) 0 xs))
  "Return the number of elements in xs.")

(define weight/log
  (lambda
    (date weight)
    (create (quote weight-log) (weight/fields date weight)))
  "Create a weight-log row from a date and weight value.")

(define weight/max-w
  (lambda
    (rows)
    (if
      (null? rows)
      0.0
      (list/foldl
        (lambda
          (acc row)
          (let ((w (weight/wt-num row))) (if (> w acc) w acc)))
        (weight/wt-num (car rows))
        (cdr rows))))
  "Return the maximum weight value across rows; 0.0 when empty.")

(define weight/min-w
  (lambda
    (rows)
    (if
      (null? rows)
      0.0
      (list/foldl
        (lambda
          (acc row)
          (let ((w (weight/wt-num row))) (if (< w acc) w acc)))
        (weight/wt-num (car rows))
        (cdr rows))))
  "Return the minimum weight value across rows; 0.0 when empty.")

(define weight/page
  (lambda
    ()
    (if
      (string-eq (request-method) "POST")
      (let
        ((dv (request-param "date"))
          (wv (request-param "weight")))
        (let
          ((d
              (let
                ((ds (as-str dv)))
                (if (blank? ds) (date-today) ds)))
            (w (as-str wv)))
          (seq (weight/log d w) (redirect "/weight"))))
      (let
        ((pts
          (weight/with-trend (weight/day-pairs (weight/all)))))
        (html/page
          (list
            (html/h1 "Weight")
            (weight/form)
            (weight/stats-html pts)
            (weight/plot-chart pts)
            (html/div
              (html/attr
                "style"
                "max-width:760px;display:flex;gap:24px;font-size:13px;color:#666;margin-bottom:16px")
              (html/join
                (list
                  (html/span
                    (html/attr
                      "style"
                      "display:flex;align-items:center;gap:6px")
                    (string-append
                      (html/tag
                        "span"
                        (html/attr
                          "style"
                          "display:inline-block;width:24px;height:3px;background:#94a3b8;border-radius:2px")
                        "")
                      " Daily weight"))
                  (html/span
                    (html/attr
                      "style"
                      "display:flex;align-items:center;gap:6px")
                    (string-append
                      (html/tag
                        "span"
                        (html/attr
                          "style"
                          "display:inline-block;width:24px;height:3px;background:#6366f1;border-radius:2px")
                        "")
                      " 7-day average")))))
            (weight/recent-table pts)))))))

(define weight/plot-all-ys
  (lambda (pts acc)
    (if
      (null? pts)
      acc
      (let
        ((p (car pts)))
        (weight/plot-all-ys
          (cdr pts)
          (cons (nth p 1) (cons (nth p 2) acc)))))))

(define weight/plot-chart
  (lambda (pts)
    (if
      (null? pts)
      "<div style=\"color:#666;margin:16px 0;\">No weight entries yet.</div>"
      (let*
        ((n (length pts))
          (daily-data (weight/plot-daily-data pts 0 (list)))
          (trend-data (weight/plot-trend-data pts 0 (list)))
          (x-labels (weight/plot-x-labels pts (list)))
          (tooltips (weight/plot-tooltips pts (list)))
          (all-ys (weight/plot-all-ys pts (list)))
          (ymin (plot/list-min all-ys))
          (ymax (plot/list-max all-ys))
          (yt (plot/nice-ticks ymin ymax 5))
          (ynice-min (nth yt 0))
          (ynice-max (nth yt 1))
          (xt (plot/nice-ticks 0 (- n 1) 6))
          (xnice-min (nth xt 0))
          (xnice-max (nth xt 1))
          (f (plot/frame 760 360 (list 56 16 20 36)))
          (y-scale
            (plot/scale-linear
              ynice-min
              ynice-max
              (plot/frame-bottom f)
              (plot/frame-top f)))
          (x-scale
            (plot/scale-linear
              xnice-min
              xnice-max
              (plot/frame-left f)
              (plot/frame-right f)))
          (daily-marks
            (plot/mark-line
              x-scale
              y-scale
              daily-data
              (list
                (list "color" "#94a3b8")
                (list "width" 1)
                (list "radius" 3)
                (list "show-dots" #t)
                (list "tooltips" tooltips))))
          (trend-marks
            (plot/mark-line
              x-scale
              y-scale
              trend-data
              (list
                (list "color" "#6366f1")
                (list "width" 3)
                (list "show-dots" #f))))
          (marks (string-append daily-marks trend-marks))
          (y-axis (plot/axis-y y-scale f (list)))
          (x-tick-labels (plot/pick-labels daily-data x-labels 6))
          (x-axis (plot/axis-x x-scale f x-tick-labels (list))))
        (plot/chart-svg f y-axis x-axis marks)))))

(define weight/plot-daily-data
  (lambda (pts i acc)
    (if
      (null? pts)
      (plot/list-reverse acc)
      (weight/plot-daily-data
        (cdr pts)
        (+ i 1)
        (cons (list i (nth (car pts) 1)) acc)))))

(define weight/plot-tooltips
  (lambda (pts acc)
    (if
      (null? pts)
      (plot/list-reverse acc)
      (let
        ((p (car pts)))
        (weight/plot-tooltips
          (cdr pts)
          (cons
            (string-append
              (nth p 0)
              ": "
              (as-str (nth p 1))
              " lbs (7-day avg: "
              (as-str (nth p 2))
              ")")
            acc))))))

(define weight/plot-trend-data
  (lambda (pts i acc)
    (if
      (null? pts)
      (plot/list-reverse acc)
      (weight/plot-trend-data
        (cdr pts)
        (+ i 1)
        (cons (list i (nth (car pts) 2)) acc)))))

(define weight/plot-x-labels
  (lambda (pts acc)
    (if
      (null? pts)
      (plot/list-reverse acc)
      (weight/plot-x-labels
        (cdr pts)
        (cons
          (string-replace (substring (nth (car pts) 0) 5) "-" "/")
          acc)))))

(define weight/points-js
  (lambda (pts) (weight/points-js-r pts "["))
  "Emit (date w trend) triples as a JS array literal.")

(define weight/points-js-r
  (lambda
    (pts acc)
    (if
      (null? pts)
      (string-append acc "]")
      (weight/points-js-r
        (cdr pts)
        (string-append
          acc
          (if (string-eq acc "[") "" ",")
          "{d:'"
          (car (car pts))
          "',w:"
          (weight/fmt1 (cadr (car pts)))
          ",t:"
          (weight/fmt1 (caddr (car pts)))
          "}")))))

(define weight/pts-str
  (lambda
    (coords)
    (list/foldl
      (lambda
        (acc p)
        (string-append
          acc
          (as-str (car p))
          ","
          (as-str (car (cdr p)))
          " "))
      ""
      coords))
  "Render coordinates as an SVG polyline points string.")

(define weight/rate-label
  (lambda
    (s)
    (if
      (string-eq s "n/a")
      "n/a"
      (if (string-starts-with? s "-") s (string-append "+" s))))
  "Prefix a trend-rate string with + when positive.")

(define weight/recent-rows
  (lambda
    (pts)
    (if
      (null? pts)
      ""
      (string-append
        "<tr style=\"border-top:1px solid #eee;\"><td style=\"padding:6px 12px 6px 0;\">"
        (car (car pts))
        "</td>"
        "<td style=\"padding:6px 12px;\">"
        (weight/fmt1 (cadr (car pts)))
        " lbs</td>"
        "<td style=\"padding:6px 12px;color:#6366f1;\">"
        (weight/fmt1 (caddr (car pts)))
        " lbs</td></tr>"
        (weight/recent-rows (cdr pts)))))
  "Table rows for (date w trend) triples.")

(define weight/recent-table
  (lambda
    (pts)
    (if
      (null? pts)
      ""
      (html/div
        (html/attr "style" "margin:16px 0;")
        (string-append
          "<h3>Recent entries</h3>"
          "<table style=\"border-collapse:collapse;font-size:14px;min-width:280px;\">"
          "<tr style=\"text-align:left;color:#666;font-size:12px;\">"
          "<th style=\"padding:6px 12px 6px 0;\">Date</th><th style=\"padding:6px 12px;\">Weight</th>"
          "<th style=\"padding:6px 12px;\">7-day avg</th></tr>"
          (weight/recent-rows (weight/take 8 (reverse pts)))
          "</table>"))))
  "Recent-entries table, newest first, max 8.")

(define weight/stat-card
  (lambda
    (label value sub)
    (string-append
      "<div style=\"flex:1;min-width:150px;padding:14px 16px;border:1px solid #e2e2e8;border-radius:10px;background:#fff;\">"
      "<div style=\"font-size:12px;color:#666;text-transform:uppercase;letter-spacing:0.05em;\">"
      label
      "</div>"
      "<div style=\"font-size:24px;font-weight:600;margin:4px 0;\">"
      value
      "</div>"
      "<div style=\"font-size:12px;color:#999;\">"
      sub
      "</div></div>"))
  "One stat card for the summary strip.")

(define weight/stats-html
  (lambda
    (pts)
    (if
      (null? pts)
      ""
      (let
        ((last (weight/last pts)))
        (string-append
          "<div style=\"display:flex;gap:12px;flex-wrap:wrap;margin:16px 0;\">"
          (weight/stat-card
            "Latest"
            (string-append (weight/fmt1 (cadr last)) " lbs")
            (car last))
          (weight/stat-card
            "7-day average"
            (string-append (weight/fmt1 (caddr last)) " lbs")
            "smooths daily fluctuation")
          (weight/stat-card
            "Trend rate"
            (string-append
              (weight/rate-label (weight/trend-rate pts))
              " lb/wk")
            "avg change per week")
          "</div>"))))
  "Summary strip: latest weight, current 7-day average, weekly trend rate.")

(define weight/summary
  (lambda
    (rows)
    (let
      ((n (weight/len rows)))
      (if
        (< n 1)
        ""
        (let
          ((last (weight/last rows)))
          (html/p*
            (html/attr "class" "muted")
            (html/text
              (string-append
                (as-str n)
                " entries  -  latest "
                (weight/ts-of last)
                "  "
                (weight/fmt (weight/wt-of last))
                " lbs")))))))
  "Render a one-line summary of weight entries and latest value.")

(define weight/take
  (lambda
    (n xs)
    (if
      (<= n 0)
      (list)
      (if
        (null? xs)
        (list)
        (cons (car xs) (weight/take (- n 1) (cdr xs))))))
  "First n elements of xs.")

(define weight/test-fn
  (lambda (x) (+ x 1))
  "Increment x by one (test helper).")

(define weight/trend-chart
  (lambda
    (pts)
    (if
      (null? pts)
      "<div style=\"color:#666;margin:16px 0;\">No weight entries yet - log one above.</div>"
      (string-append
        "<div style=\"max-width:760px;height:360px;position:relative;margin:16px 0;\"><canvas id=\"weightChart\"></canvas></div>"
        "<script src=\"https://cdn.jsdelivr.net/npm/chart.js@4.4.1\"></script>"
        "<script>"
        "const pts="
        (weight/points-js pts)
        ";"
        "const labels=pts.map(function(p){const d=new Date(p.d+'T00:00:00');return d.toLocaleDateString('en-US',{month:'short',day:'numeric'});});"
        "new Chart(document.getElementById('weightChart'),{type:'line',data:{labels:labels,datasets:["
        "{label:'Daily weight',data:pts.map(function(p){return p.w;}),borderColor:'#94a3b8',backgroundColor:'#94a3b8',borderWidth:1.5,borderDash:[5,4],pointRadius:4,pointHoverRadius:6,tension:0.2},"
        "{label:'7-day average',data:pts.map(function(p){return p.t;}),borderColor:'#6366f1',backgroundColor:'rgba(99,102,241,0.07)',borderWidth:3,pointRadius:0,pointHoverRadius:5,tension:0.35,fill:true}"
        "]},options:{responsive:true,maintainAspectRatio:false,interaction:{mode:'index',intersect:false},"
        "plugins:{legend:{display:true,position:'bottom'},tooltip:{callbacks:{label:function(c){return c.dataset.label+': '+c.parsed.y.toFixed(1)+' lbs';}}}},"
        "scales:{y:{title:{display:true,text:'lbs'},grid:{color:'rgba(0,0,0,0.05)'}},x:{grid:{display:false}}}}});"
        "</script>")))
  "Render Chart.js line chart: daily weights vs 7-day moving average.")

(define weight/trend-rate
  (lambda
    (pts)
    (if
      (< (weight/len pts) 2)
      "n/a"
      (let
        ((days
          (date-diff-days (car (car pts)) (car (weight/last pts)))))
        (if
          (< days 1)
          "n/a"
          (weight/fmt1
            (/
              (* (- (caddr (weight/last pts)) (caddr (car pts))) 7.0)
              (+ 0.0 days)))))))
  "Average weekly rate of change of the trend line, or \"n/a\".")

(define weight/ts-of
  (lambda (row) (weight/field (cdr row) "ts"))
  "Return the timestamp field of a weight row.")

(define weight/with-trend
  (lambda
    (pairs)
    (weight/with-trend-r pairs pairs (list)))
  "Annotate (date w) pairs into (date w trend) triples.")

(define weight/with-trend-r
  (lambda
    (all rest acc)
    (if
      (null? rest)
      acc
      (weight/with-trend-r
        all
        (cdr rest)
        (append
          acc
          (list
            (list
              (car (car rest))
              (cadr (car rest))
              (weight/avg-window all (car (car rest))))))))))

(define weight/wt-num
  (lambda
    (row)
    (let
      ((s (weight/wt-of row)))
      (if (blank? s) 0.0 (parse-float s))))
  "Return a weight row's numeric weight, 0.0 when blank.")

(define weight/wt-of
  (lambda (row) (weight/field (cdr row) "weight"))
  "Return the weight field of a weight row.")

(define weight/x-of
  (lambda
    (i n)
    (if
      (<= n 1)
      (+ 60.0 (/ 680.0 2.0))
      (+ 60.0 (* (+ 0.0 i) (/ 680.0 (+ 0.0 (- n 1)))))))
  "Map a zero-based index to an SVG x position across n points.")

(define weight/y-of
  (lambda
    (w wmin wmax)
    (+ 20.0 (* (- 1.0 (/ (- w wmin) (- wmax wmin))) 300.0)))
  "Map a weight value to an SVG y position within min/max range.")
