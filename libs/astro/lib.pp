;; Astro — sunrise/sunset + moon phase for a location.
;;
;; Sunrise/sunset: client-side Open-Meteo fetch (no API key, CORS-friendly,
;;   same pattern as the weather lib). The image has no server-side trig, so
;;   sun times are fetched by the browser and rendered into server-emitted
;;   placeholder cells (ids sr-<date>, ss-<date>, dl-<date>).
;; Moon phase: pure-Lisp arithmetic on the image. Mean synodic-month lunation
;;   reckoned from the 2000-01-06 18:14 UTC new moon. Illuminated fraction is
;;   sin^2(pi * phase); sin on [0,pi] via the Bhaskara I approximation (valid
;;   because phase in [0,1) => theta = pi*phase in [0,pi], no arg reduction).
;;   Accuracy: phase name exact at quarter/new/full boundaries; illumination
;;   within ~0.3%; date boundary cases near a phase edge may differ by a day
;;   from a true-ephemeris source.
;;
;; Depends: html (html/page, html/*), list (list-ref), core prims (date-*,
;;   string-split, parse-int, div, mod, floor, round, abs, +, -, *, <, ...).
;; Install: (load-library "astro").  Live eval also works (defines persist).
;; Public routes: /astro (full page). Widget: (astro/widget).
;; REPL API: (astro/moon (date-today)), (astro/phase-name (date-today)),
;;   (astro/illum-percent (date-today)), (astro/phase-glyph (date-today)), ...

;; ---------------------------------------------------------------------------
;; constants
;; ---------------------------------------------------------------------------

(define astro/pi 3.141592653589793
  "pi (Bhaskara sine uses this and pi^2).")
(define astro/pi-sq 9.869604401089358
  "pi^2 (precomputed; 5*pi^2 base of the Bhaskara denominator).")
(define astro/synodic-month 29.530588853
  "Mean synodic month in days.")
(define astro/new-moon-epoch "2000-01-06"
  "Reference new moon, 2000-01-06 18:14 UTC (date part).")
(define astro/epoch-frac 0.7597
  "Fraction of day at 18:14 UTC past midnight (1094/1440).")

(define astro/default-lat "44.52634"
  "Default latitude (Cody, WY — matches weather lib).")
(define astro/default-lon "-109.05653"
  "Default longitude (Cody, WY).")
(define astro/default-label "Cody, WY"
  "Default location label.")

;; ---------------------------------------------------------------------------
;; date helpers (YYYY-MM-DD string -> ints / pretty)
;; ---------------------------------------------------------------------------

(define astro/date-parts
  (lambda (d)
    (let ((ps (string-split d "-")))
      (list (parse-int (list-ref ps 0))
            (parse-int (list-ref ps 1))
            (parse-int (list-ref ps 2)))))
  "Split YYYY-MM-DD into (year month day) ints.")

(define astro/weekday-names
  (list "Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday")
  "Weekday names; date-weekday returns 0=Sunday..6=Saturday.")
(define astro/weekday-short-names
  (list "Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat")
  "Short weekday names (0=Sun).")
(define astro/month-names
  (list "January" "February" "March" "April" "May" "June"
        "July" "August" "September" "October" "November" "December")
  "Month names 1..12.")

(define astro/weekday-name
  (lambda (i) (list-ref astro/weekday-names i))
  "Full weekday name from date-weekday int (0=Sun).")
(define astro/weekday-short
  (lambda (i) (list-ref astro/weekday-short-names i))
  "Short weekday name from date-weekday int (0=Sun).")
(define astro/month-name
  (lambda (m) (list-ref astro/month-names (- m 1)))
  "Month name from 1..12.")

(define astro/date-seq
  (lambda (start n)
    (if (<= n 0)
        (list)
        (cons start (astro/date-seq (date-add-days start 1) (- n 1)))))
  "List of n date strings starting at start (inclusive), forward.")

;; ---------------------------------------------------------------------------
;; moon math (pure arithmetic; no trig primitive needed)
;; ---------------------------------------------------------------------------

(define astro/days-since-epoch
  (lambda (d)
    (- (date-diff-days astro/new-moon-epoch d) astro/epoch-frac))
  "Days from the 2000-01-06 new moon to date d (midnights + 18:14 correction). Negative before the epoch; works for any historical date.")

(define astro/phase-fraction
  (lambda (d)
    (let ((p (mod (div (astro/days-since-epoch d) astro/synodic-month) 1.0)))
      (if (< p 0) (+ p 1.0) p)))
  "Cycle phase 0..1 (0 = new, 0.25 = first quarter, 0.5 = full, 0.75 = last quarter). Handles negative mod for pre-epoch dates.")

(define astro/phase-age
  (lambda (d)
    (let ((a (mod (astro/days-since-epoch d) astro/synodic-month)))
      (if (< a 0) (+ a astro/synodic-month) a)))
  "Age within the current cycle, 0..29.53 days (positive for pre-epoch dates).")

(define astro/sin-0-pi
  (lambda (theta)
    (let ((u (- astro/pi theta)))
      (let ((q (* theta u)))
        (let ((num (* 16 q))
              (den (- (* 5 astro/pi-sq) (* 4 q))))
          (div num den)))))
  "sin(theta) for theta in [0,pi] via Bhaskara I (max error ~0.16%).")

(define astro/illum-fraction
  (lambda (d)
    (let ((theta (* astro/pi (astro/phase-fraction d))))
      (let ((s (astro/sin-0-pi theta)))
        (* s s))))
  "Illuminated fraction 0..1 = sin^2(pi*phase).")

(define astro/illum-percent
  (lambda (d)
    (round (* 100 (astro/illum-fraction d))))
  "Illuminated percent 0..100 (rounded).")

(define astro/phase-waxing?
  (lambda (d) (< (astro/phase-fraction d) 0.5))
  "True if waxing (cycle phase < 0.5).")

(define astro/phase-names
  (list "New Moon" "Waxing Crescent" "First Quarter" "Waxing Gibbous"
        "Full Moon" "Waning Gibbous" "Last Quarter" "Waning Crescent")
  "Eight phase names indexed by the centered bucket (see astro/phase-index).")

(define astro/phase-glyphs
  (list "🌑" "🌒" "🌓" "🌔"
        "🌕" "🌖" "🌗" "🌘")
  "Eight phase emoji glyphs (new..waning-crescent) indexed by the centered bucket.")

(define astro/phase-index
  (lambda (d)
    (mod (floor (* (+ (astro/phase-fraction d) 0.0625) 8)) 8))
  "Centered 8-bucket index 0..7 (0=New). Buckets are 1/8 wide centered on the exact phase points (new=0, first quarter=0.25, full=0.5, last quarter=0.75).")

(define astro/phase-name
  (lambda (d) (list-ref astro/phase-names (astro/phase-index d)))
  "Moon phase name (8 phases).")

(define astro/phase-glyph
  (lambda (d) (list-ref astro/phase-glyphs (astro/phase-index d)))
  "Moon phase emoji glyph.")

;; ---------------------------------------------------------------------------
;; public moon API (REPL-callable)
;; ---------------------------------------------------------------------------

(define astro/moon
  (lambda (d)
    (string-append (astro/phase-glyph d) " " (astro/phase-name d)
      " - " (as-str (astro/illum-percent d)) "% illuminated - age "
      (as-str (round (astro/phase-age d))) " days"))
  "One-line moon summary for date d.")

(define astro/help
  (lambda ()
    (string-append
      "astro - sunrise/sunset + moon phase. "
      "Sun times are fetched client-side (Open-Meteo); moon is computed on the image. "
      "REPL: (astro/moon (date-today)) returns glyph+name+illum+age. "
      "Also: (astro/phase-name d) (astro/phase-glyph d) (astro/illum-percent d) "
      "(astro/phase-fraction d) (astro/phase-age d). "
      "Web: /astro  -  embed: (astro/widget)."))
  "Short help string for the astro library.")

;; ---------------------------------------------------------------------------
;; HTML pieces
;; ---------------------------------------------------------------------------

(define astro/td
  (lambda (content) (html/tag "td" "" content))
  "Plain table cell.")
(define astro/td-id
  (lambda (id content) (html/tag "td" (html/attrs (list (html/attr "id" id))) content))
  "Table cell with id (JS fills these for sun columns).")
(define astro/th
  (lambda (content) (html/tag "th" "" content))
  "Table header cell.")

(define astro/style
  (lambda ()
    (html/tag "style" ""
      (html/join
        (list
          ".astro-card .astro-today{display:flex;gap:1.5rem;flex-wrap:wrap;align-items:center;margin:.5rem 0}"
          ".astro-card .astro-moon-big{line-height:1.4}"
          ".astro-card .astro-glyph{font-size:3.5rem;line-height:1;display:block}"
          ".astro-card .astro-mname{font-weight:700;margin:.15rem 0}"
          ".astro-card .astro-illum{color:#444}"
          ".astro-card .astro-age{color:#666;font-size:.85rem}"
          ".astro-card .astro-sun-today .astro-sun-line{margin:.15rem 0}"
          ".astro-card .astro-sun-today .lbl{color:#666;display:inline-block;min-width:4.5rem}"
          ".astro-card table.astro-table{width:100%;border-collapse:collapse;margin-top:1rem;font-size:.92rem}"
          ".astro-card table.astro-table th,.astro-card table.astro-table td{text-align:left;padding:.3rem .5rem;border-bottom:1px solid #eee}"
          ".astro-card table.astro-table th{background:#f6f6f6}"
          ".astro-card .astro-form{display:flex;gap:.35rem;flex-wrap:wrap;margin:.5rem 0}"
          ".astro-card .astro-form input[type=text]{flex:1 1 8rem;min-width:0}"
          ".astro-card .astro-form button{width:auto;min-width:4.5rem}"
          ".astro-card .astro-loc{font-weight:700;margin:.25rem 0}"
          ".astro-card .astro-err{color:#800000;font-weight:700}"
          ".astro-card .astro-updated{font-size:.8rem;color:#555;margin-top:.35rem}"
          ".astro-card .astro-historical{margin-top:1.5rem;padding-top:1rem;border-top:1px solid #eee}"
          ".astro-card .astro-historical h3{margin:.25rem 0}"
          ".astro-card .astro-hist-card{display:flex;gap:1.5rem;flex-wrap:wrap;align-items:center;margin-top:.5rem}"
          ".astro-card .astro-hist-date{font-weight:700;font-size:1.1rem}"
          ".astro-card .astro-hist-sun .lbl{color:#666;display:inline-block;min-width:4.5rem}"
          ".astro-card .astro-hist-sun div{margin:.15rem 0}"
          ".astro-card .astro-form input[type=date]{flex:0 0 auto}"
          ".astro-card .astro-hist-moon .astro-glyph{font-size:2.5rem}"))))
  "Inline CSS for the astro card.")

(define astro/moon-card-html
  (lambda (d)
    (html/div (html/attr "class" "astro-moon-big")
      (html/join
        (list
          (html/span (html/attr "class" "astro-glyph") (astro/phase-glyph d))
          (html/div (html/attr "class" "astro-mname") (astro/phase-name d))
          (html/div (html/attr "class" "astro-illum")
            (string-append (as-str (astro/illum-percent d)) "% illuminated"))
          (html/div (html/attr "class" "astro-age")
            (string-append "Age " (as-str (round (astro/phase-age d))) " days"))))))
  "Reusable moon card (big glyph + name + illum% + age) for any date. Used by today card, historical lookup, and /astro/moon-for.")

(define astro/today-card-html
  (lambda ()
    (let ((d (date-today)))
      (html/div (html/attr "class" "astro-today")
        (html/join
          (list
            (astro/moon-card-html d)
            (html/div (html/attr "class" "astro-sun-today")
              (html/join
                (list
                  (html/div (html/attr "class" "astro-sun-line")
                    (html/join (list
                      (html/span (html/attr "class" "lbl") "Sunrise ")
                      (html/span (html/attr "id" "today-sr") "-"))))
                  (html/div (html/attr "class" "astro-sun-line")
                    (html/join (list
                      (html/span (html/attr "class" "lbl") "Sunset ")
                      (html/span (html/attr "id" "today-ss") "-"))))
                  (html/div (html/attr "class" "astro-sun-line")
                    (html/join (list
                      (html/span (html/attr "class" "lbl") "Daylight ")
                      (html/span (html/attr "id" "today-dl") "-"))))))))))))
  "Today's card: big moon (server-rendered) + sun placeholders (JS-filled).")

(define astro/forecast-row-html
  (lambda (d)
    (let ((parts (astro/date-parts d)))
      (let ((y (list-ref parts 0)) (m (list-ref parts 1)) (day (list-ref parts 2)))
        (let ((wd (astro/weekday-short (date-weekday y m day))))
          (html/tag "tr" (html/attrs (list (html/attr "data-date" d)))
            (html/join
              (list
                (astro/td (string-append wd " " d))
                (astro/td-id (string-append "sr-" d) "-")
                (astro/td-id (string-append "ss-" d) "-")
                (astro/td-id (string-append "dl-" d) "-")
                (astro/td (string-append (astro/phase-glyph d) " " (astro/phase-name d)))
                (astro/td (string-append (as-str (astro/illum-percent d)) "%")))))))))
  "Forecast row: date, sunrise/sunset/daylight (JS-filled), moon glyph+name, illum%.")

(define astro/forecast-rows-html
  (lambda (dates)
    (if (null? dates)
        ""
        (html/join
          (list (astro/forecast-row-html (car dates))
                (astro/forecast-rows-html (cdr dates))))))
  "All forecast table rows for a list of date strings.")

(define astro/forecast-table-html
  (lambda ()
    (let ((dates (astro/date-seq (date-today) 7)))
      (html/tag "table" (html/attrs (list (html/attr "class" "astro-table")))
        (html/join
          (list
            (html/tag "thead" ""
              (html/tag "tr" ""
                (html/join
                  (list (astro/th "Date") (astro/th "Sunrise") (astro/th "Sunset")
                        (astro/th "Daylight") (astro/th "Moon Phase") (astro/th "Illum")))))
            (html/tag "tbody" "" (astro/forecast-rows-html dates)))))))
  "7-day forecast table: date + sun columns (JS-filled) + moon columns.")

(define astro/script
  (lambda (lat lon label)
    (html/tag "script" ""
      (string-append
        "(function(){"
        "var root=document.getElementById('astro-widget');"
        "if(!root)return;"
        "var locEl=document.getElementById('astro-loc-label');"
        "var form=document.getElementById('astro-loc-form');"
        "var input=document.getElementById('astro-city');"
        "var todaySr=document.getElementById('today-sr');"
        "var todaySs=document.getElementById('today-ss');"
        "var todayDl=document.getElementById('today-dl');"
        "var errEl=document.getElementById('astro-err');"
        "var upEl=document.getElementById('astro-updated');"
        "var KEY='pp.astro.loc';"
        "var def={lat:" (as-str lat) ",lon:" (as-str lon)
        ",label:" (json-emit (as-str label)) "};"
        "function loadLoc(){try{var s=localStorage.getItem(KEY);if(s)return JSON.parse(s);}catch(e){}return def;}"
        "function saveLoc(loc){try{localStorage.setItem(KEY,JSON.stringify(loc));}catch(e){}}"
        "function fmtTime(iso){if(!iso)return '-';var t=String(iso).split('T')[1]||'';return t.slice(0,5);}"
        "function fmtDur(sec){if(sec==null)return '-';sec=Math.round(sec);var h=Math.floor(sec/3600),m=Math.floor((sec%3600)/60);return h+'h '+m+'m';}"
        "function fillRow(date,sr,ss,dl){"
        "var a=document.getElementById('sr-'+date);var b=document.getElementById('ss-'+date);var c=document.getElementById('dl-'+date);"
        "if(a)a.textContent=fmtTime(sr);if(b)b.textContent=fmtTime(ss);if(c)c.textContent=fmtDur(dl);"
        "}"
        "function renderToday(sr,ss,dl){if(todaySr)todaySr.textContent=fmtTime(sr);if(todaySs)todaySs.textContent=fmtTime(ss);if(todayDl)todayDl.textContent=fmtDur(dl);}"
        "function fail(msg){if(errEl)errEl.textContent=msg;}"
        "function fetchAstro(loc){"
        "if(locEl)locEl.textContent=loc.label||(loc.lat+', '+loc.lon);"
        "var url='https://api.open-meteo.com/v1/forecast?latitude='+encodeURIComponent(loc.lat)"
        "+'&longitude='+encodeURIComponent(loc.lon)"
        "+'&daily=sunrise,sunset,daylight_duration&timezone=auto&forecast_days=7';"
        "fetch(url).then(function(r){if(!r.ok)throw new Error('HTTP '+r.status);return r.json();})"
        ".then(function(j){var d=j.daily||{};var ts=d.time||[];var sr=d.sunrise||[];var ss=d.sunset||[];var dl=d.daylight_duration||[];"
        "if(ts.length){renderToday(sr[0],ss[0],dl[0]);}"
        "for(var i=0;i<ts.length;i++){fillRow(ts[i],sr[i],ss[i],dl[i]);}"
        "if(upEl)upEl.textContent='via Open-Meteo - local browser fetch';"
        "})"
        ".catch(function(e){fail('Sun data fetch failed: '+(e&&e.message?e.message:e));});"
        "}"
        "function geocode(q){"
        "var url='https://geocoding-api.open-meteo.com/v1/search?name='+encodeURIComponent(q)+'&count=1&language=en&format=json';"
        "return fetch(url).then(function(r){if(!r.ok)throw new Error('HTTP '+r.status);return r.json();})"
        ".then(function(j){if(!j.results||!j.results.length)throw new Error('place not found');"
        "var p=j.results[0];var bits=[p.name,p.admin1,p.country_code].filter(Boolean);"
        "return{lat:p.latitude,lon:p.longitude,label:bits.join(', ')};});"
        "}"
        "if(form){form.addEventListener('submit',function(e){"
        "e.preventDefault();var q=(input&&input.value||'').trim();if(!q)return;"
        "geocode(q).then(function(loc){saveLoc(loc);fetchAstro(loc);})"
        ".catch(function(err){fail(err&&err.message?err.message:String(err));});"
        "});}"
        "fetchAstro(loadLoc());"
        "})();")))
  "Client JS: Open-Meteo daily sunrise/sunset/daylight fetch + geocode + localStorage.")

(define astro/historical-panel-html
  (lambda ()
    (html/div (html/attr "class" "astro-historical")
      (html/join
        (list
          (html/h3 "Look up a date")
          (html/p* (html/attr "class" "muted")
            "Enter any past date to see its sunrise/sunset (Open-Meteo Archive) and moon phase (computed on the image). Uses the location set above.")
          (html/tag "form"
            (html/attrs
              (list
                (html/attr "id" "astro-hist-form")
                (html/attr "class" "astro-form")
                (html/attr "action" "#")
                (html/attr "method" "get")))
            (html/join
              (list
                (html/input
                  (html/attrs
                    (list
                      (html/attr "type" "date")
                      (html/attr "id" "astro-hist-date")
                      (html/attr "name" "date")
                      (html/attr "value" (date-today)))))
                (html/button (html/attr "type" "submit") "Look up"))))
          (html/div (html/attr "id" "astro-hist-result") "")))))
  "Historical date lookup panel: date input + Look up button + result container. Placed on the /astro page (not the home widget).")

(define astro/historical-script
  (lambda ()
    (html/tag "script" ""
      (string-append
        "(function(){"
        "var root=document.getElementById('astro-widget');"
        "if(!root)return;"
        "var form=document.getElementById('astro-hist-form');"
        "var dateEl=document.getElementById('astro-hist-date');"
        "var result=document.getElementById('astro-hist-result');"
        "var errEl=document.getElementById('astro-err');"
        "var KEY='pp.astro.loc';"
        "var defLoc={lat:" (as-str astro/default-lat) ",lon:" (as-str astro/default-lon) "};"
        "function loadLoc(){try{var s=localStorage.getItem(KEY);if(s)return JSON.parse(s);}catch(e){}return defLoc;}"
        "function fmtTime(iso){if(!iso)return '-';var t=String(iso).split('T')[1]||'';return t.slice(0,5);}"
        "function fmtDur(sec){if(sec==null)return '-';sec=Math.round(sec);var h=Math.floor(sec/3600),m=Math.floor((sec%3600)/60);return h+'h '+m+'m';}"
        "function fail(msg){if(errEl)errEl.textContent=msg;result.innerHTML='';}"
        "function lookup(date,loc){"
        "result.innerHTML='<p class=\"muted\">Loading...</p>';"
        "if(errEl)errEl.textContent='';"
        "var sunUrl='https://archive-api.open-meteo.com/v1/archive?latitude='+encodeURIComponent(loc.lat)"
        "+'&longitude='+encodeURIComponent(loc.lon)"
        "+'&start_date='+encodeURIComponent(date)+'&end_date='+encodeURIComponent(date)"
        "+'&daily=sunrise,sunset,daylight_duration&timezone=auto';"
        "var moonUrl='/partials/astro/moon-for?date='+encodeURIComponent(date);"
        "Promise.all([fetch(sunUrl).then(function(r){if(!r.ok)throw new Error('Archive HTTP '+r.status);return r.json();}),fetch(moonUrl).then(function(r){if(!r.ok)throw new Error('Moon HTTP '+r.status);return r.text();})])"
        ".then(function(res){var j=res[0];var moonHtml=res[1];"
        "var d=(j.daily||{});var sr=(d.sunrise||[])[0];var ss=(d.sunset||[])[0];var dl=(d.daylight_duration||[])[0];"
        "result.innerHTML="
        "'<div class=\"astro-hist-card\">'"
        "+'<div class=\"astro-hist-date\">'+date+'</div>'"
        "+'<div class=\"astro-hist-sun\">'"
        "+'<div><span class=\"lbl\">Sunrise </span><strong>'+(fmtTime(sr)||'-')+'</strong></div>'"
        "+'<div><span class=\"lbl\">Sunset </span><strong>'+(fmtTime(ss)||'-')+'</strong></div>'"
        "+'<div><span class=\"lbl\">Daylight </span><strong>'+fmtDur(dl)+'</strong></div>'"
        "+'</div>'"
        "+'<div class=\"astro-hist-moon\">'+moonHtml+'</div>'"
        "+'</div>';"
        "})"
        ".catch(function(e){fail('Lookup failed: '+(e&&e.message?e.message:e));});"
        "}"
        "if(form){form.addEventListener('submit',function(e){"
        "e.preventDefault();var q=(dateEl&&dateEl.value||'').trim();if(!q)return;lookup(q,loadLoc());"
        "});}"
        "})();")))
  "Client JS: historical date lookup. Fetches Open-Meteo Archive API for sunrise/sunset + /astro/moon-for for moon phase, renders into #astro-hist-result.")

(define astro/widget
  (lambda ()
    (html/div
      (html/attrs
        (list
          (html/attr "class" "card astro-card")
          (html/attr "id" "astro-widget")))
      (html/join
        (list
          (html/h2 "Sun & Moon")
          (html/div (html/attrs (list (html/attr "class" "astro-loc") (html/attr "id" "astro-loc-label")))
            astro/default-label)
          (html/div (html/attrs (list (html/attr "id" "astro-err") (html/attr "class" "astro-err"))) "")
          (html/tag "form"
            (html/attrs
              (list
                (html/attr "id" "astro-loc-form")
                (html/attr "class" "astro-form")
                (html/attr "action" "#")
                (html/attr "method" "get")))
            (html/join
              (list
                (html/input
                  (html/attrs
                    (list
                      (html/attr "type" "text")
                      (html/attr "id" "astro-city")
                      (html/attr "name" "city")
                      (html/attr "placeholder" "City...")
                      (html/attr "autocomplete" "off"))))
                (html/button (html/attr "type" "submit") "Set"))))
          (astro/today-card-html)
          (astro/forecast-table-html)
          (html/div (html/attrs (list (html/attr "class" "astro-updated") (html/attr "id" "astro-updated"))) "")
          (astro/style)
          (astro/script astro/default-lat astro/default-lon astro/default-label)))))
  "Embeddable Sun & Moon card (form + today card + 7-day table + script).")

(define astro/moon-for
  (lambda ()
    (let ((raw (request-param "date")))
      (let ((d (if (or (null? raw) (string-eq (as-str raw) "")) (date-today) (as-str raw))))
        (astro/moon-card-html d))))
  "GET /astro/moon-for?date=YYYY-MM-DD - moon card HTML partial for any date (defaults to today). Reuses the on-image moon math.")

(define astro/page
  (lambda ()
    (html/page
      (list
        (html/h1 "Sun & Moon")
        (html/p* (html/attr "class" "muted")
          "Sunrise & sunset via Open-Meteo (browser fetch). Moon phase computed on the image.")
        (astro/widget)
        (astro/historical-panel-html)
        (astro/historical-script))))
  "GET /astro - full Sun & Moon page (today + 7-day forecast + historical date lookup).")

(route "/astro" "astro/page")
(route "/astro/moon-for" "astro/moon-for")
(route "/partials/astro/moon-for" "astro/moon-for")
