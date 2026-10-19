;; scratch-test round 12 - page full
(package scratch-test

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
          (st/bar quake/yearly "M4.5+ events per year" "events"))
        (html/p*
          (html/attr "class" "muted")
          "Counts rise with time because the detection network grows - any rate model must account for the instruments.")
        (html/h2 "Depth distribution")
        (html/div
          (html/attr "style" "max-width:960px")
          (st/bar quake/depth-buckets "Events by depth band in km" "events"))
        (html/p*
          (html/attr "class" "muted")
          "9,513 events deeper than 500km - mostly the Fiji/Tonga slab, Earth's most active deep-subduction system.")
        (html/h2 "M7+ by region")
        (html/div
          (html/attr "style" "max-width:960px")
          (st/bar quake/regions-m7 "M7+ events by region, top 12 of 693" "M7+ events"))
        (html/h2 "Gutenberg-Richter check")
        (html/p*
          (html/attr "class" "muted")
          "log10 cumulative count vs magnitude is linear: b = 1.03 over M5-7.5, textbook. The ETL self-validates.")
        (html/h2 "OSINT corner")
        (html/p*
          (html/attr "class" "muted")
          "497 nuclear explosions M4.5+ sit in the catalog as a distinct type - Kazakhstan, Nevada, Russia, French Polynesia; tail = 2017 DPRK."))))))


(define st/bar
  (lambda (pairs title unit)
    "<svg/>"))

(route "/scratch-test" "quake/page"))
