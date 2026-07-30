;; package weight -- exported from live branch system/main
;; Sync: scripts/pp_sync.py pull|push

(define weight/all
  (lambda
    ()
    (list/foldl
      weight/insert-sorted
      ()
      (rows (quote weight-log) 500))))

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
                "</svg>"))))))))

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
          (weight/coords (cdr rows) (+ i 1) n wmin wmax))))))

(define weight/delete
  (lambda (id) (delete (quote weight-log) id)))

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
      coords)))

(define weight/field
  (lambda
    (fields key)
    (let
      ((v (dict-get fields key)))
      (if (null? v) "" (as-str v)))))

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
        (as-str (whoami))))))

(define weight/fmt
  (lambda
    (w)
    (let
      ((s (as-str w)))
      (if (blank? s) s (as-str (parse-float s))))))

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
                (html/hidden "action" "log")))))))))

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
            (weight/insert-sorted (cdr sorted) row)))))))

(define weight/last
  (lambda
    (xs)
    (if (null? (cdr xs)) (car xs) (weight/last (cdr xs)))))

(define weight/len
  (lambda
    (xs)
    (list/foldl (lambda (acc x) (+ acc 1)) 0 xs)))

(define weight/log
  (lambda
    (date weight)
    (create (quote weight-log) (weight/fields date weight))))

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
        (cdr rows)))))

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
        (cdr rows)))))

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
        ((rows (weight/all)))
        (html/page
          (list
            (html/h1 "Weight")
            (weight/form)
            (weight/chart-svg rows)
            (weight/summary rows)))))))

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
      coords)))

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
                " lbs"))))))))

(define weight/test-fn
  (lambda (x) (+ x 1)))

(define weight/ts-of
  (lambda (row) (weight/field (cdr row) "ts")))

(define weight/wt-num
  (lambda
    (row)
    (let
      ((s (weight/wt-of row)))
      (if (blank? s) 0.0 (parse-float s)))))

(define weight/wt-of
  (lambda (row) (weight/field (cdr row) "weight")))

(define weight/x-of
  (lambda
    (i n)
    (if
      (<= n 1)
      (+ 60.0 (/ 680.0 2.0))
      (+ 60.0 (* (+ 0.0 i) (/ 680.0 (+ 0.0 (- n 1))))))))

(define weight/y-of
  (lambda
    (w wmin wmax)
    (+ 20.0 (* (- 1.0 (/ (- w wmin) (- wmax wmin))) 300.0))))

