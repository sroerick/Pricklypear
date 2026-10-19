;; r22: yearly-5 + page-6-elements
(package scratch-test

(define quake/yearly
  (quote
    (("1975" 3344)
("1976" 3631)
("1977" 3614)
("1978" 3348)
("1979" 3327)
("1980" 3515))))

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
          "Counts rise with time because the detection network grows - any rate model must account for the instruments."))))))


(define st/bar
  (lambda (pairs title unit)
    "<svg/>"))

(route "/scratch-test" "quake/page"))
