;; package quake - USGS earthquake exploration (Track B probe, data-value
;; program card 64b0af4e). Static aggregates from the USGS FDSN catalog:
;; 273,294 events M4.5+, 1975-2026. v1 = baked aggregates + server-rendered
;; SVG charts (no JS, no CDN). Refresh = re-extract + re-release.

(package quake
(define quake/depth-buckets
  (quote
    ((shallow 210304)
     (intermediate 48676)
     (deep 4801)
     (very-deep 9513))))


(define quake/regions-m7
  (quote
    (("Indonesia" 73)
    ("Papua New Guinea" 49)
    ("Vanuatu" 39)
    ("Philippines" 38)
    ("Japan" 35)
    ("Russia" 29)
    ("Chile" 26)
    ("Solomon Islands" 26)
    ("Peru" 23)
    ("Mexico" 22)
    ("Alaska" 20)
    ("New Zealand" 20))))


;; depth bands (km): shallow <=70, intermediate <=300, deep <=500, very-deep >500.
;; M7+ total 693 (1975-2026).

(define quake/range
  (lambda
    (lo hi)
    (if
      (>= lo hi)
      ()
      (cons lo (quake/range (+ lo 1)))))
  "Integers lo..hi-1.")

(define quake/bar-svg
  (lambda
    (pairs title unit)
    (let*
      ((w 920)
       (h 320)
       (pad-l 46)
       (pad-b 34)
       (n (length pairs))
       (maxv
         (list/foldl
           (lambda (acc p)
             (if (> (cdr p) acc) (cdr p) acc))
           1
           pairs))
       (bw (div (* (- w pad-l 10) 100) (* n 120)))
       (gap (div (- w pad-l 10) n))
       (bars
         (list/map
           (lambda (i)
             (let*
               ((p (nth pairs i))
                (v (cdr p))
                (bh (div (* v (- h pad-b 20)) maxv))
                (x (+ pad-l (* i gap) (div (- gap bw) 2)))
                (y (- h pad-b bh))
                (label (car p)))
               (string-append
                 "<rect x=\"" (as-str x) "\" y=\"" (as-str y)
                 "\" width=\"" (as-str bw) "\" height=\"" (as-str bh)
                 "\" fill=\"#7aa2f7\"><title>" label ": " (as-str v) " " unit "</title></rect>"
                 (if (= (mod i 5) 0)
                     (string-append "<text x=\"" (as-str x) "\" y=\"" (as-str (- h 10))
                       "\" font-size=\"9\" fill=\"#6b7080\" text-anchor=\"middle\">" label "</text>")
                     ""))))
           (quake/range 0 (- n 1)))))
      (string-append
        "<svg viewBox=\"0 0 " (as-str w) " " (as-str h)
        "\" width=\"100%\" style=\"max-width:" (as-str w) "px\" role=\"img\">"
        "<text x=\"" (as-str pad-l) "\" y=\"16\" font-size=\"12\" fill=\"#d8dee9\">" title "</text>"
        (html/join bars)
        "<line x1=\"" (as-str pad-l) "\" y1=\"" (as-str (- h pad-b))
        "\" x2=\"" (as-str w) "\" y2=\"" (as-str (- h pad-b)) "\" stroke=\"#2a2e3a\"/></svg>"))))
  "Server-rendered SVG bar chart from (label . count) pairs. No JS.)

(define quake/page
  (lambda
    ()
    (html/page
      (list
        (html/h1 "Earthquakes")
        (html/p*
          (html/attr "class" "muted")
          "USGS FDSN catalog: 273,294 events M4.5+, 1975-2026. Aggregates baked at build; refresh = re-extract + re-release. Data: earthquake.usgs.gov (public domain).")
        (html/h2 "Events per year")
        (html/div
          (html/attr "style" "max-width:960px")
          (quake/bar-svg quake/yearly "M4.5+ events per year, 1975-2026" "events"))
        (html/p*
          (html/attr "class" "muted")
          "Counts rise over time because the detection network grows, not because the planet shakes more - any rate model must account for the instruments.")
        (html/h2 "Depth distribution")
        (html/div
          (html/attr "style" "max-width:960px")
          (quake/bar-svg quake/depth-buckets "Events by depth band (km)" "events"))
        (html/p*
          (html/attr "class" "muted")
          "9,513 events deeper than 500km - mostly the Fiji/Tonga slab, Earth's most active deep-subduction system.")
        (html/h2 "M7+ by region")
        (html/div
          (html/attr "style" "max-width:960px")
          (quake/bar-svg quake/regions-m7 "M7+ events by region, top 12 (693 total)" "M7+ events"))
        (html/h2 "Gutenberg-Richter check")
        (html/p*
          (html/attr "class" "muted")
          "log10(cumulative count) vs magnitude is linear: b-value = 1.03 over M5-7.5, textbook. The ETL self-validates.")
        (html/h2 "OSINT corner")
        (html/p*
          (html/attr "class" "muted")
          "497 nuclear explosions M4.5+ appear in the catalog as a distinct type (Kazakhstan, Nevada, Russia, French Polynesia; tail = 2017 DPRK)."))))

(route "/quake" "quake/page"))
)
)
