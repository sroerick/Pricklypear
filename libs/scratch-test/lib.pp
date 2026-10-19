;; scratch-test round 4 - bar-svg verbatim shape
(package scratch-test

(define st/vals
  (quote
    (("a" 1)
     ("b" 2))))

(define st/bar-svg
  (lambda (pairs title unit)
    (let* ((w 920)
           (h 320)
           (padl 46)
           (padb 34)
           (n (length pairs))
           (maxv (list/foldl (lambda (acc p) (if (> (cdr p) acc) (cdr p) acc)) 1 pairs))
           (gapw (div (- w padl 10) n))
           (barw (div (* gapw 7) 10))
           (bars
             (list/map
               (lambda (i)
                 (let* ((p (list-ref pairs i))
                        (label (as-str (car p)))
                        (v (cdr p))
                        (bh (div (* v (- h padb 20)) maxv))
                        (x (+ padl (* i gapw) (div (- gapw barw) 2)))
                        (y (- h padb bh))
                        (bar (string-append
                               "<rect x='" (as-str x) "' y='" (as-str y)
                               "' width='" (as-str barw) "' height='" (as-str bh)
                               "' fill='#7aa2f7'><title>" label ": " (as-str v)
                               " " unit "</title></rect>"))
                        (tick
                          (if (= (mod i 5) 0)
                            (string-append
                              "<text x='" (as-str x) "' y='" (as-str (- h 10))
                              "' font-size='9' fill='#6b7080' text-anchor='middle'>"
                              label "</text>")
                            "")))
                   (string-append bar tick)))
               (list/range n))))
      (string-append
        "<svg viewBox='0 0 " (as-str w) " " (as-str h)
        "' width='100%' role='img'><text x='10' y='16' font-size='12'>"
        title "</text>"
        (html/join bars)
        "<line x1='" (as-str padl) "' y1='" (as-str (- h padb))
        "' x2='" (as-str w) "' y2='" (as-str (- h padb)) "' stroke='#2a2e3a'/></svg>")))
  "Server-rendered SVG bar chart from (label . count) pairs.")

(route "/scratch-test" "st/bar-svg"))
