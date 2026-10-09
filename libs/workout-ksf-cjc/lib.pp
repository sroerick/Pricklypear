;; package workout-ksf-cjc -- the first program-as-sub-module.
;; Spec: specs/workout-ksf-cjc.md. Composes with the core workout lib.
;; Uses defines instead of nested lets for clarity.

(package workout-ksf-cjc
  (export agenda is-ksf get-sets))

(define workout-ksf-cjc/is-ksf
  (lambda (pair)
    (workout/field (workout/car-or-null (cdr pair)) "program")))

(define workout-ksf-cjc/get-sets
  (lambda (session-id)
    (dict-get
      (workout/doc-progress (workout/parse-session-doc session-id))
      "sets_done")))

(define workout-ksf-cjc/session-li
  (lambda (pair)
    (html/li "" (html/text
      (string-append
        (workout/session-date-of (workout/car-or-null (cdr pair)))
        " - "
        (as-str (workout-ksf-cjc/get-sets (car pair)))
        " rounds @ 20kg")))))

(define workout-ksf-cjc/recent
  (lambda (n)
    (list/filter workout-ksf-cjc/is-ksf (workout/sessions-sorted))))

(define workout-ksf-cjc/cjc-data-js
  (lambda ()
    (workout/join-csv
      (list/map
        (lambda (pair)
          (string-append
            (workout/session-date-of (workout/car-or-null (cdr pair)))
            ":"
            (as-str (workout-ksf-cjc/get-sets (car pair)))))
        (workout-ksf-cjc/recent 999)))))

(define workout-ksf-cjc/agenda
  (lambda ()
    (html/page
      (list
        (html/h2 "KSF - CJC block @ 20kg")
        (html/p* (html/attr "class" "muted")
          (html/text "Current program. Long-cycle clean-jerk-clean."))
        (html/h3 "Recent sessions")
        (html/tag "ul" (html/attr "class" "cjc-recent")
          (html/join (list/map workout-ksf-cjc/session-li
            (workout-ksf-cjc/recent 8))))
        (html/h3 "Rounds per session")
        (html/div (html/attr "style" "max-width:720px;margin:1em 0")
          (html/tag "canvas" (html/attr "id" "cjcChart") ""))
        (html/tag "script" ""
          (string-append
            "var raw=" (workout-ksf-cjc/cjc-data-js) ";"
            "var pts=raw.split(',').map(s=>{var p=s.split(':');return[p[0],parseInt(p[1])]});"
            "var c=document.getElementById('cjcChart');"
            "new Chart(c,{type:'line',data:{labels:pts.map(x=>x[0]),"
            "datasets:[{label:'CJC rounds',data:pts.map(x=>x[1]),"
            "borderColor:'#2a7',tension:.3}]},options:{scales:{y:{beginAtZero:true}}}});"))
        (html/p (html/a "/workout" "<- Workout home"))))))
