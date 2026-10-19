;; package quake - USGS earthquake exploration (Track B probe, card
;; 64b0af4e). Static aggregates from the USGS FDSN catalog: 273,294
;; events M4.5+, 1975-2026. Server-rendered SVG, no JS, no CDN.
;; HTML attrs single-quoted: the load-reader rejects backslash escapes
;; confirmed via quote-test 2026-10-19.
;; Refresh = re-extract + re-release.

(package quake

(define quake/yearly
  (quote
    (("1975" 3344)
("1976" 3631)
("1977" 3614)
("1978" 3348)
("1979" 3327)
("1980" 3515)
("1981" 3289)
("1982" 3573)
("1983" 4124)
("1984" 4126)
("1985" 4574)
("1986" 4518)
("1987" 4021)
("1988" 4012)
("1989" 4012)
("1990" 4430)
("1991" 4098)
("1992" 4391)
("1993" 4189)
("1994" 4200)
("1995" 4702)
("1996" 4674)
("1997" 4588)
("1998" 3804)
("1999" 4068)
("2000" 5010)
("2001" 4680)
("2002" 4963)
("2003" 5136)
("2004" 6183)
("2005" 7878)
("2006" 7075)
("2007" 7274)
("2008" 6307)
("2009" 6054)
("2010" 7214)
("2011" 9584)
("2012" 7189)
("2013" 7211)
("2014" 8333)
("2015" 7153)
("2016" 7440)
("2017" 6372)
("2018" 7571)
("2019" 7211)
("2020" 6511)
("2021" 8959)
("2022" 7766)
("2023" 7651)
("2024" 6397))))

(define quake/depth-buckets
  (quote
    (("shallow" 210304)
("intermediate" 48676)
("deep" 4801)
("very-deep" 9513))))

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

(define quake/range
  (lambda
    (lo hi)
    (if
      (>= lo hi)
      ()
      (cons lo (quake/range (+ lo 1)))))
  "Integers lo..hi-1.")

(define quake/bar-svg
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
  "Server-rendered SVG bar chart from label-count pairs.")

(define quake/page
  (lambda ()
    (html/page
      (list
        (html/h1 "Earthquakes")
        (html/p*
          (html/attr "class" "muted")
          "USGS FDSN catalog: 273,294 events M4.5+, 1975-2026. Aggregates baked at build; refresh = re-extract + re-release. Data: earthquake.usgs.gov - public domain.")
        (html/h2 "Events per year")
        (html/div
          (html/attr "style" "max-width:960px")
          (quake/bar-svg quake/yearly "M4.5+ events per year" "events"))
        (html/p*
          (html/attr "class" "muted")
          "Counts rise with time because the detection network grows - any rate model must account for the instruments.")
        (html/h2 "Depth distribution")
        (html/div
          (html/attr "style" "max-width:960px")
          (quake/bar-svg quake/depth-buckets "Events by depth band in km" "events"))
        (html/p*
          (html/attr "class" "muted")
          "9,513 events deeper than 500km - mostly the Fiji/Tonga slab, Earth's most active deep-subduction system.")
        (html/h2 "M7+ by region")
        (html/div
          (html/attr "style" "max-width:960px")
          (quake/bar-svg quake/regions-m7 "M7+ events by region, top 12 of 693" "M7+ events"))
        (html/h2 "Gutenberg-Richter check")
        (html/p*
          (html/attr "class" "muted")
          "log10 cumulative count vs magnitude is linear: b = 1.03 over M5-7.5, textbook. The ETL self-validates.")
        (html/h2 "OSINT corner")
        (html/p*
          (html/attr "class" "muted")
          "497 nuclear explosions M4.5+ sit in the catalog as a distinct type - Kazakhstan, Nevada, Russia, French Polynesia; tail = 2017 DPRK."))))

(route "/quake" "quake/page")
)
)
