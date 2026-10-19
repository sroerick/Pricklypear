;; scratch-test r17: data defines + range + page(half) + stub
(package scratch-test

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
          (st/bar quake/yearly "M4.5+ events per year" "events"))))))


(define st/bar
  (lambda (pairs title unit)
    "<svg/>"))

(route "/scratch-test" "quake/page"))
