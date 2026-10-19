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
          (st/bar quake/depth-buckets "Events by depth band in km" "events"))))))

(define st/bar
  (lambda (pairs title unit)
    "<svg/>"))

(route "/scratch-test" "quake/page"))
