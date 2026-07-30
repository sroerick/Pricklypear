;; package weather -- exported from live branch system/main
;; Sync: scripts/pp_sync.py pull|push

(define weather/default-label
  "Cody, WY")

(define weather/default-lat
  "44.52634")

(define weather/default-lon
  "-109.05653")

(define weather/page
  (lambda
    ()
    (html/page
      (list
        (html/h1 "Weather")
        (html/p*
          (html/attr "class" "muted")
          "Browser fetch via Open-Meteo. Location is stored in this browser only.")
        (weather/widget)))))

(define weather/script
  (lambda
    (lat lon label)
    (html/tag
      "script"
      ""
      (string-append
        "(function(){"
        "var root=document.getElementById('weather-widget');"
        "if(!root)return;"
        "var body=document.getElementById('weather-body');"
        "var locEl=document.getElementById('weather-loc-label');"
        "var form=document.getElementById('weather-loc-form');"
        "var input=document.getElementById('weather-city');"
        "var KEY='pp.weather.loc';"
        "var def={lat:"
        (as-str lat)
        ",lon:"
        (as-str lon)
        ",label:"
        (json-emit (as-str label))
        "};"
        "function codes(c){"
        "c=String(c);"
        "return({0:'Clear',1:'Mainly clear',2:'Partly cloudy',3:'Overcast',"
        "45:'Fog',48:'Rime fog',51:'Light drizzle',53:'Drizzle',55:'Heavy drizzle',"
        "61:'Light rain',63:'Rain',65:'Heavy rain',71:'Light snow',73:'Snow',"
        "75:'Heavy snow',80:'Rain showers',81:'Rain showers',82:'Heavy showers',"
        "95:'Thunderstorm'}[c]||'\226\128\148');"
        "}"
        "function loadLoc(){"
        "try{var s=localStorage.getItem(KEY);if(s)return JSON.parse(s);}catch(e){}"
        "return def;"
        "}"
        "function saveLoc(loc){"
        "try{localStorage.setItem(KEY,JSON.stringify(loc));}catch(e){}"
        "}"
        "function render(loc,cur){"
        "if(locEl)locEl.textContent=loc.label||(loc.lat+', '+loc.lon);"
        "if(!cur){body.innerHTML='<p class=\"muted\">No data</p>';return;}"
        "var t=Math.round(cur.temperature_2m);"
        "var u=cur.temperature_2m!=null?'\194\176F':'';"
        "var w=codes(cur.weather_code);"
        "var hum=cur.relative_humidity_2m!=null?(cur.relative_humidity_2m+'% rh'):'';"
        "var wind=cur.wind_speed_10m!=null?(Math.round(cur.wind_speed_10m)+' mph'):'';"
        "body.innerHTML="
        "'<div class=\"wx-row\">'"
        "+'<div class=\"wx-temp\">'+t+u+'</div>'"
        "+'<div><div><strong>'+w+'</strong></div>'"
        "+'<div class=\"wx-meta\">'+[hum,wind].filter(Boolean).join(' \194\183 ')+'</div></div>'"
        "+'</div>'"
        "+'<div class=\"wx-updated muted\">via Open-Meteo \194\183 local browser fetch</div>';"
        "}"
        "function fail(msg){"
        "body.innerHTML='<p class=\"wx-err\">'+msg+'</p>';"
        "}"
        "function fetchWx(loc){"
        "body.innerHTML='<p class=\"muted\">Loading\226\128\166</p>';"
        "if(locEl)locEl.textContent=loc.label||'\226\128\166';"
        "var url='https://api.open-meteo.com/v1/forecast'"
        "+'?latitude='+encodeURIComponent(loc.lat)"
        "+'&longitude='+encodeURIComponent(loc.lon)"
        "+'&current=temperature_2m,relative_humidity_2m,weather_code,wind_speed_10m'"
        "+'&temperature_unit=fahrenheit&wind_speed_unit=mph&timezone=auto';"
        "fetch(url).then(function(r){if(!r.ok)throw new Error('HTTP '+r.status);return r.json();})"
        ".then(function(j){render(loc,j.current||null);})"
        ".catch(function(e){fail('Weather fetch failed: '+(e&&e.message?e.message:e));});"
        "}"
        "function geocode(q){"
        "var url='https://geocoding-api.open-meteo.com/v1/search?name='"
        "+encodeURIComponent(q)+'&count=1&language=en&format=json';"
        "return fetch(url).then(function(r){if(!r.ok)throw new Error('HTTP '+r.status);return r.json();})"
        ".then(function(j){"
        "if(!j.results||!j.results.length)throw new Error('place not found');"
        "var p=j.results[0];"
        "var bits=[p.name,p.admin1,p.country_code].filter(Boolean);"
        "return{lat:p.latitude,lon:p.longitude,label:bits.join(', ')};"
        "});"
        "}"
        "if(form){"
        "form.addEventListener('submit',function(e){"
        "e.preventDefault();"
        "var q=(input&&input.value||'').trim();"
        "if(!q)return;"
        "body.innerHTML='<p class=\"muted\">Looking up\226\128\166</p>';"
        "geocode(q).then(function(loc){saveLoc(loc);fetchWx(loc);})"
        ".catch(function(err){fail(err&&err.message?err.message:String(err));});"
        "});"
        "}"
        "fetchWx(loadLoc());"
        "})();"))))

(define weather/style
  (lambda
    ()
    (html/tag
      "style"
      ""
      (html/join
        (list
          ".weather-card .wx-row{display:flex;align-items:baseline;gap:.75rem;flex-wrap:wrap}"
          ".weather-card .wx-temp{font-size:2rem;font-weight:700;line-height:1}"
          ".weather-card .wx-meta{font-size:.9rem;color:#333}"
          ".weather-card .wx-loc{font-weight:700;margin:.25rem 0}"
          ".weather-card .wx-form{display:flex;gap:.35rem;flex-wrap:wrap;margin-top:.5rem}"
          ".weather-card .wx-form input[type=text]{flex:1 1 8rem;min-width:0}"
          ".weather-card .wx-form button{width:auto;min-width:4.5rem}"
          ".weather-card .wx-err{color:#800000;font-weight:700}"
          ".weather-card .wx-updated{font-size:.8rem;color:#555;margin-top:.35rem}")))))

(define weather/widget
  (lambda
    ()
    (html/div
      (html/attrs
        (list
          (html/attr "class" "card weather-card")
          (html/attr "id" "weather-widget")))
      (html/join
        (list
          (html/h2 "Weather")
          (html/div
            (html/attr "class" "wx-loc")
            (html/span
              (html/attr "id" "weather-loc-label")
              (html/text weather/default-label)))
          (html/div
            (html/attr "id" "weather-body")
            (html/p*
              (html/attr "class" "muted")
              "Loading\226\128\166"))
          (html/tag
            "form"
            (html/attrs
              (list
                (html/attr "id" "weather-loc-form")
                (html/attr "class" "wx-form")
                (html/attr "action" "#")
                (html/attr "method" "get")))
            (html/join
              (list
                (html/input
                  (html/attrs
                    (list
                      (html/attr "type" "text")
                      (html/attr "id" "weather-city")
                      (html/attr "name" "city")
                      (html/attr "placeholder" "City\226\128\166")
                      (html/attr "autocomplete" "off"))))
                (html/button (html/attr "type" "submit") "Set"))))
          (weather/style)
          (weather/script
            weather/default-lat
            weather/default-lon
            weather/default-label))))))

