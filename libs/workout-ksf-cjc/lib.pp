;; package workout-ksf-cjc -- the first program-as-sub-module.
;; Spec: specs/workout-ksf-cjc.md. Composes with the core workout lib.
;; NOTE: this kernel has no let* special form - use nested let.

(package workout-ksf-cjc
  (export agenda))

(define workout-ksf-cjc/recent
  (lambda (n)
    (let ((ksf (list/filter
                 (lambda (pair)
                   (let ((f (workout/car-or-null (cdr pair))))
                     (string-eq (workout/field f "program") "ksf")))
                 (workout/sessions-sorted))))
      (if (> (workout/len ksf) n)
        (list/take ksf n)
        ksf))))

(define workout-ksf-cjc/session-li
  (lambda (pair)
    (let ((id (car pair)))
      (let ((f (workout/car-or-null (cdr pair))))
        (let ((doc (workout/parse-session-doc id)))
          (let ((p (workout/doc-params doc)))
            (let ((sets (dict-get (workout/doc-progress doc) "sets_done")))
              (let ((bell (workout/pget p "bell")))
                (let ((unit (workout/pget p "unit")))
                  (html/li ""
                    (html/join (list
                      (html/text (string-append
                        (workout/session-date-of f)
                        " - "
                        (as-str sets) " rounds @ "
                        (as-str bell) (as-str unit)
                        (if (string-eq (workout/field f "state") "done")
                          " done"
                          "")))))))))))))))

(define workout-ksf-cjc/cjc-data-js
  (lambda ()
    (let ((ksf (list/filter
                 (lambda (pair)
                   (let ((f (workout/car-or-null (cdr pair))))
                     (string-eq (workout/field f "program") "ksf")))
                 (workout/sessions-sorted))))
      (let ((points (list/map
                      (lambda (pair)
                        (let ((id (car pair)))
                          (let ((f (workout/car-or-null (cdr pair))))
                            (let ((doc (workout/parse-session-doc id)))
                              (let ((d (dict-get (workout/doc-progress doc) "sets_done")))
                                (string-append
                                  (workout/session-date-of f)
                                  ":"
                                  (as-str d))))))
                      ksf))))
        (workout/join-csv points))))

(define workout-ksf-cjc/agenda
  (lambda ()
    (html/page
      (list
        (html/h2 "KSF - CJC block @ 20kg")
        (html/p* (html/attr "class" "muted")
          (html/text "Current program. Long-cycle clean-jerk-clean, left/right alternation."))
        (html/h3 "What's next")
        (html/p* "" (html/text "No session logged today - train when ready, log after."))
        (html/h3 "Recent sessions")
        (html/tag "ul" (html/attr "class" "cjc-recent")
          (html/join (list/map workout-ksf-cjc/session-li (workout-ksf-cjc/recent 8))))
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
)
