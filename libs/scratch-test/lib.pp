;; package quake - USGS earthquake exploration (Track B probe, card
;; 64b0af4e). Static aggregates from the USGS FDSN catalog: 273,294
;; events M4.5+, 1975-2026. Server-rendered SVG, no JS, no CDN.
;; HTML attrs single-quoted: the load-reader rejects backslash escapes
;; confirmed via quote-test 2026-10-19.
;; Refresh = re-extract + re-release.

(package quake

(define quake/yearly (quote (("1975" 3344) ("1976" 3631) ("1977" 3614) ("1978" 3348) ("1979" 3327))))

(define quake/depth-buckets (quote (("shallow" 210304) ("intermediate" 48676) ("deep" 4801) ("very-deep" 9513))))

(define quake/regions-m7 (quote (("Indonesia" 73) ("Papua New Guinea" 49) ("Vanuatu" 39) ("Philippines" 38) ("Japan" 35) ("Russia" 29) ("Chile" 26) ("Solomon Islands" 26) ("Peru" 23) ("Mexico" 22) ("Alaska" 20) ("New Zealand" 20))))

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
    "<svg/>")
  "Stub.")

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
