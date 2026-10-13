;; package ops -- exported from live branch system/main
;; Sync: scripts/pp-sync.sh status|pull|snapshot

(define ops/age-html
  (lambda
    (secs)
    "A human age from seconds: '12s', '4m', '2h', '3d'."
    (let
      ((n (parse-int (as-str secs))))
      (if
        (< n 60)
        (string-append (as-str n) "s")
        (if
          (< n 3600)
          (string-append (as-str (div n 60)) "m")
          (if
            (< n 86400)
            (string-append (as-str (div n 3600)) "h")
            (string-append (as-str (div n 86400)) "d"))))))
  "Compact age glyph for the console's last-seen column.");; package ops -- exported from live branch system/main
;; Sync: scripts/pp_sync.py pull|push

(define ops/code/__t
  (lambda () 1))

(define ops/code/apply-pairs
  (lambda
    (src pairs)
    (if
      (null? pairs)
      src
      (let
        ((pair (car pairs)))
        (ops/code/apply-pairs
          (string-replace
            src
            (as-str (car pair))
            (as-str (car (cdr pair))))
          (cdr pairs))))))

(define ops/code/default-pkgs
  (list
    "bible"
    "workout"
    "welcome"
    "layout"
    "todo"
    "dabar"
    "outline"
    "notes"
    "roam"
    "calendar"
    "flash"
    "chat"
    "signup"
    "bi"
    "contacts"
    "plan"
    "strongs"
    "weather"
    "ref"
    "ai"
    "ui"
    "menu"
    "html"
    "css"
    "ops"))

(define ops/code/define-src
  (lambda
    (name body-src)
    (load-string
      (string-append
        "(define "
        (as-str name)
        "\n  "
        (as-str body-src)
        "\n)"))))

(define ops/code/fix-mangled
  (lambda
    ()
    (list/filter
      (lambda
        (row)
        (not (string-eq (as-str (car (cdr row))) "clean")))
      (list/map ops/code/fix-name (ops/code/mangled-hits)))))

(define ops/code/fix-name
  (lambda
    (name)
    (if
      (or
        (string-eq (as-str name) "ops/code/mangled-pairs")
        (string-eq (as-str name) "ops/code/mangled-needles"))
      (list name "skip")
      (let
        ((src0 (function-source name)))
        (if
          (null? src0)
          (list name "missing")
          (let
            ((src (as-str src0)))
            (let
              ((new (ops/code/apply-pairs src ops/code/mangled-pairs)))
              (if
                (string-eq src new)
                (list name "clean")
                (begin
                  (ops/code/define-src name new)
                  (list name "fixed"))))))))))

(define ops/code/fix-names
  (lambda (names) (list/map ops/code/fix-name names)))

(define ops/code/fix-pkg
  (lambda
    (pkg)
    (list/filter
      (lambda
        (row)
        (not (string-eq (as-str (car (cdr row))) "clean")))
      (list/map
        ops/code/fix-name
        (ops/code/uniq
          (list/foldl
            (lambda
              (acc needle)
              (append acc (ops/code/scan-pkg pkg needle)))
            ()
            ops/code/mangled-needles))))))

(define ops/code/has-find-code?
  (lambda () (not (null? (function-source "find-code")))))

(define ops/code/mangled-hits
  (lambda
    ()
    (ops/code/uniq
      (list/foldl
        (lambda
          (acc needle)
          (append acc (ops/code/search-limit needle 200)))
        ()
        ops/code/mangled-needles))))

(define ops/code/mangled-needles
  (list
    "226134"
    "194183"
    "226128"
    "226151"
    "226150"
    "194171"
    "194187"
    "194176"
    "u2190"
    "u2192"
    "u00b7"
    "u2014"
    "u2013"
    "u2026"
    "u2019"
    "u2018"
    "u201c"
    "u201d"
    "u25c0"
    "u25b6"
    "u00ab"
    "u00bb"
    "u00b0"
    "u2191"
    "u2193"))

(define ops/code/mangled-pairs
  (list
    (list "226134144" "&larr;")
    (list "226134146" "&rarr;")
    (list "226134145" "&uarr;")
    (list "226134147" "&darr;")
    (list "194183" "&middot;")
    (list "226128148" "&mdash;")
    (list "226128147" "&ndash;")
    (list "226128166" "&hellip;")
    (list "226128153" "&rsquo;")
    (list "226128152" "&lsquo;")
    (list "226128156" "&ldquo;")
    (list "226128157" "&rdquo;")
    (list "226151128" "&#9664;")
    (list "226150182" "&#9654;")
    (list "194171" "&laquo;")
    (list "194187" "&raquo;")
    (list "194176" "&deg;")
    (list "u2190" "&larr;")
    (list "u2192" "&rarr;")
    (list "u2191" "&uarr;")
    (list "u2193" "&darr;")
    (list "u00b7" "&middot;")
    (list "u2014" "&mdash;")
    (list "u2013" "&ndash;")
    (list "u2026" "&hellip;")
    (list "u2019" "&rsquo;")
    (list "u2018" "&lsquo;")
    (list "u201c" "&ldquo;")
    (list "u201d" "&rdquo;")
    (list "u25c0" "&#9664;")
    (list "u25b6" "&#9654;")
    (list "u00ab" "&laquo;")
    (list "u00bb" "&raquo;")
    (list "u00b0" "&deg;")))

(define ops/code/scan-pkg
  (lambda
    (pkg needle)
    (list/filter
      (lambda
        (name)
        (let
          ((src (function-source name)))
          (if
            (null? src)
            #f
            (string-contains? (as-str src) (as-str needle)))))
      (package-bindings pkg))))

(define ops/code/scan-pkgs
  (lambda
    (pkgs needle limit)
    (let
      ((lim (parse-int (as-str limit))))
      (list/foldl
        (lambda
          (acc pkg)
          (if
            (>= (count acc) lim)
            acc
            (append acc (ops/code/scan-pkg pkg needle))))
        ()
        pkgs))))

(define ops/code/search
  (lambda (needle) (ops/code/search-limit needle 500)))

(define ops/code/search-limit
  (lambda
    (needle limit)
    (ops/code/scan-pkgs ops/code/default-pkgs needle limit)))

(define ops/code/uniq
  (lambda
    (xs)
    (list/foldl
      (lambda
        (acc x)
        (if (null? (member x acc)) (cons x acc) acc))
      ()
      xs)))

(define ops/console-page
  (lambda
    ()
    "GET/POST /ops: the operator console. POST action=hold dispatches
     (ops/hold) and re-renders the strip inline (htmx swaps it into
     #ops-console); any GET renders it. Admin gate is the kernel's."
    (if
      (string-eq (request-method) "POST")
      (let
        ((action (as-str (request-param "action"))))
        (if
          (string-eq action "hold")
          (seq
            (ops/hold
              (as-str (request-param "kind"))
              (as-str (request-param "target")))
            (ops/console-strip))
          (ops/console-strip)))
      (ops/console-strip)))
  "The /ops operator console face (filmstrip pane; admin only at the kernel).")

(define ops/console-strip
  (lambda
    ()
    "The whole operator strip: live sessions + holds, spend rollups, knocks,
     incidents, and the ledger tail. Rendered inline after every hold."
    (let*
      ((presence-raw (ops/presence))
        (denied (ops/denied? presence-raw))
        (presence
          (if
            denied
            (json-parse "{}")
            (if
              (string? presence-raw)
              (json-parse presence-raw)
              presence-raw)))
        (sessions (dict-get presence "sessions"))
        (rollups (dict-get presence "rollups"))
        (activity (if denied (list) (ops/activity "" "" "" 60)))
        (incidents
          (if denied (list) (json-parse (ops/incidents))))
        (knock
          (if
            denied
            (json-parse "{}")
            (let
              ((k (ops/knock)))
              (if (string? k) (json-parse k) k)))))
      (html/div
        (html/attr "id" "ops-console")
        (html/join
          (list
            (html/h1 (html/text "Operator console"))
            (if
              denied
              (html/p*
                (html/attr "class" "muted")
                (as-str presence-raw))
              "")
            (html/p*
              (html/attr "class" "muted")
              "Live sessions, per-identity spend, deterministic knocks, and the ledger tail. Every hold is reversible and writes admin.hold — the watched machine watches its watchers.")
            (html/h2 (html/text "On now"))
            (if
              (null? sessions)
              (html/p* (html/attr "class" "muted") "no live sessions in the window")
              (html/join (list/map ops/session-row-html sessions)))
            (html/join
              (list/map
                (lambda (s)
                  (if
                    (null? (dict-get s "last_auth"))
                    ""
                    (ops/last-auth-html s)))
                sessions))
            (html/h2 (html/text "Spend (day rollups, kept forever)"))
            (ops/rollup-table-html rollups)
            (ops/knocks-html knock)
            (html/h2 (html/text "Open incidents"))
            (if
              (null? incidents)
              (html/p* (html/attr "class" "muted") "no open incidents")
              (html/ul
                (html/attr "class" "ops-incidents")
                (html/join (list/map ops/incident-html incidents))))
            (html/h2 (html/text "Ledger tail"))
            (ops/ledger-table-html activity)
            (html/p*
              (html/attr "class" "muted")
              (html/join
                (list
                  (html/a "/issues" "issues register")
                  " · "
                  (html/a "/workstreams" "workstreams")
                  " · "
                  (html/a "/timing" "timing")))))))))
  "The complete operator console strip (the filmstrip pane body).")

(define ops/denied?
  (lambda
    (v)
    "True when an ops read returned a denial string rather than JSON."
    (if
      (string? v)
      (string-starts-with? (as-str v) "ops/presence: denied")
      #f))
  "Guard for the operator console: a non-admin's (ops/presence) denial is
   rendered as text, not parsed as JSON.")

(define ops/field-int
  (lambda
    (obj key)
    "The integer value of a JSON field, 0 when absent/null."
    (let
      ((v (dict-get obj key)))
      (if
        (null? v)
        0
        (if
          (number? v)
          (parse-int (as-str v))
          0))))
  "Integer coercion of a parsed JSON field.")

(define ops/field-str
  (lambda
    (obj key)
    "The string value of a JSON field, \"\" when absent/null."
    (let
      ((v (dict-get obj key)))
      (if
        (null? v)
        ""
        (as-str v))))
  "String coercion of a parsed JSON field; () and JSON null both become \"\".")

(define ops/hold-form
  (lambda
    (kind target label)
    "A one-click hold verb: a POST form carrying kind + target."
    (html/form
      "POST"
      "/ops"
      (html/attrs
        (list
          (html/attr "hx-post" "/ops")
          (html/attr "hx-target" "#ops-console")
          (html/attr "hx-swap" "innerHTML")
          (html/attr "class" "ops-hold")))
      (html/join
        (list
          (html/hidden "action" "hold")
          (html/hidden "kind" kind)
          (html/hidden "target" target)
          (html/button
            (html/attrs
              (list (html/attr "type" "submit")
                    (html/attr "title"
                      (string-append
                        "reversible containment: " kind " " target))))
            (html/text label))))))
  "A HOLD verb button (revoke-token/revoke-sessions/cancel-job/disable-package).")

(define ops/incident-html
  (lambda
    (i)
    "One open incident row: severity, component, count, sample."
    (html/li
      (html/attrs
        (list
          (html/attr "class"
            (string-append "ops-incident ops-sev-"
              (let
                ((s (ops/field-str i "severity")))
                (if (string-eq s "") "reliability" s))))))
      (html/join
        (list
          (html/strong
            (html/text
              (string-append
                "[" (ops/field-str i "severity") "] "
                (ops/field-str i "component"))))
          (html/span
            (html/attr "class" "muted")
            (html/text
              (string-append " ×" (as-str (ops/field-int i "count"))
                " · " (ops/field-str i "last_seen"))))
          (html/span
            (html/attr "class" "ops-fingerprint")
            (html/text (ops/field-str i "fingerprint")))))))
  "One incident row from (ops/incidents).")

;; ============================================================
;; OPERATOR CONSOLE — the filmstrip strip, live (product-experience.borg
;; §activity-ledger READ; security.borg §the-watched-machine).
;;
;; borge/filmstrip.borg names the console: "The console ships as a
;; filmstrip strip: live sessions from Session_registry, a live ledger
;; tail, the EXISTING views ..., per-identity spend/burst from day
;; rollups, and the HOLD verbs (revoke token / cancel job / disable
;; package) one click from the feed." The kernel side exists:
;; (ops/presence) (live sessions + forever-kept rollups + token/job/auth
;; provenance), (ops/activity) (ledger tail), (ops/knock) (deterministic
;; security rows), (ops/incidents) (the incident queue), and (ops/hold)
;; (the reversible containment verbs). This face is the one click.
;;
;; It is a PANE (an ordinary route with an HX-Request partial mode): the
;; shell opens it at /ops like any other pane. Zero JS: the verbs are
;; htmx POSTs to /ops that dispatch (ops/hold), re-read (ops/presence),
;; and answer the whole strip inline (the strip IS the response, so the
;; operator never sees a stale live list after a hold).
;;
;; ADMIN ONLY. A non-admin hitting /ops gets the denial text from
;; (ops/presence) (the kernel gate is authoritative — this face never
;; re-implements it). Structure only: no content, no token material.
;; ============================================================

(define ops/json-field
  (lambda
    (obj key)
    "A JSON object's field by key, or () — works on alists and strings."
    (dict-get obj key))
  "Read one field from a parsed JSON object (alist-oriented).")

(define ops/knocks-html
  (lambda
    (knock)
    "The knock scan summary: scanned window, detected knocks, trifecta."
    (let
      ((knocks (dict-get knock "knocks"))
        (tri (dict-get knock "trifecta")))
      (html/div
        (html/attr "class" "ops-knocks")
        (html/join
          (list
            (html/h3 (html/text "Knocks (deterministic)"))
            (html/p*
              (html/attr "class" "muted")
              (html/text
                (string-append
                  "scanned " (as-str (ops/field-int knock "scanned"))
                  " events in the last "
                  (as-str (ops/field-int knock "window")) "s; "
                  (as-str (length knocks)) " knock(s) fired")))
            (if
              (null? knocks)
              ""
              (html/ul
                ""
                (html/join
                  (list/map
                    (lambda (k)
                      (html/li
                        ""
                        (html/join
                          (list
                            (html/strong (html/text (ops/field-str k "kind")))
                            (html/span
                              (html/attr "class" "muted")
                              (html/text
                                (string-append " " (ops/field-str k "actor"))))))))
                    knocks))))
            (if
              (null? tri)
              ""
              (html/ul
                (html/attr "class" "ops-trifecta")
                (html/join
                  (list/map
                    (lambda (t)
                      (html/li
                        ""
                        (html/text
                          (string-append
                            (ops/field-str t "actor") ": "
                            (if (ops/field-str t "flagged") "TRIFECTA FLAGGED" "partial")))))
                    tri)))))))))
  "Knock scan + trifecta summary (from (ops/knock)).")

(define ops/last-auth-html
  (lambda
    (s)
    "Login provenance for the identity (auth_log read face): kind, ok,
     source ip, at."
    (let
      ((auth (dict-get s "last_auth")))
      (if
        (null? auth)
        ""
        (html/div
          (html/attr "class" "ops-auth")
          (html/join
            (list
              (html/strong (html/text "last auth "))
              (html/join
                (list/map
                  (lambda (a)
                    (html/span
                      (html/attr "class" "ops-auth-row")
                      (html/text
                        (string-append
                          (ops/field-str a "kind")
                          (if (ops/field-str a "ok") " ok" " denied")
                          (let
                            ((ip (ops/field-str a "source_ip")))
                            (if (string-eq ip "") "" (string-append " from " ip)))
                          (string-append " · " (ops/field-str a "at"))))))
                  auth))))))))
  "The last-auth provenance line for one live identity.")

(define ops/ledger-row-html
  (lambda
    (r)
    "One ledger-table row: at, actor, class, target, outcome, cost steps."
    (let
      ((cost (dict-get r "cost"))
        (cls (ops/field-str r "class")))
      (html/tag
        "tr"
        ""
        (html/join
          (list
            (html/tag "td" "" (html/text (ops/field-str r "at")))
            (html/tag "td" "" (html/text (ops/field-str r "actor")))
            (html/tag
              "td"
              ""
              (html/span
                (html/attr "class"
                  (string-append "ops-cls ops-cls-"
                    (string-replace cls "." "-")))
                (html/text cls)))
            (html/tag "td" "" (html/text (ops/field-str r "target")))
            (html/tag "td" "" (html/text (ops/field-str r "outcome")))
            (html/tag "td" ""
              (html/text
                (if (null? cost)
                  ""
                  (as-str (ops/field-int cost "steps")))))))))))

(define ops/ledger-table-html
  (lambda
    (rows)
    "The live ledger tail: at, actor, class, target, outcome, cost steps."
    (if
      (null? rows)
      (html/p* (html/attr "class" "muted") "ledger empty (or still in the ring)")
      (html/tag
        "table"
        (html/attr "class" "ops-ledger")
        (html/join
          (list
            (html/tag
              "thead"
              ""
              (html/tag
                "tr"
                ""
                (html/join
                  (list
                    (html/tag "th" "" "at")
                    (html/tag "th" "" "actor")
                    (html/tag "th" "" "class")
                    (html/tag "th" "" "target")
                    (html/tag "th" "" "outcome")
                    (html/tag "th" "" "steps")))))
            (html/tag
              "tbody"
              ""
              (html/join (list/map ops/ledger-row-html rows))))))))
  "The ledger tail table (structure only, latest first).")

(define ops/rollup-table-html
  (lambda
    (rollups)
    "Per-identity spend/burst from the forever-kept day rollups."
    (if
      (null? rollups)
      (html/p* (html/attr "class" "muted")
        "no rollups recorded yet (the ledger flush fiber accumulates them)")
      (html/tag
        "table"
        (html/attr "class" "ops-rollups")
        (html/join
          (list
            (html/tag
              "thead"
              ""
              (html/tag
                "tr"
                ""
                (html/join
                  (list
                    (html/tag "th" "" "actor")
                    (html/tag "th" "" "days")
                    (html/tag "th" "" "events")
                    (html/tag "th" "" "steps")
                    (html/tag "th" "" "db ms")))))
            (html/tag
              "tbody"
              ""
              (html/join
                (list/map
                  (lambda (r)
                    (html/tag
                      "tr"
                      ""
                      (html/join
                        (list
                          (html/tag "td" ""
                            (html/text (ops/field-str r "actor")))
                          (html/tag "td" "" (html/text (as-str (ops/field-int r "days"))))
                          (html/tag "td" "" (html/text (as-str (ops/field-int r "events"))))
                          (html/tag "td" "" (html/text (as-str (ops/field-int r "steps"))))
                          (html/tag "td" "" (html/text (as-str (ops/field-int r "db_ms"))))))))
                  rollups))))))))
  "The per-identity spend table: day rollups summed by actor.")

(define ops/session-jobs-html
  (lambda
    (s)
    "Recent/in-flight jobs for one identity, each with a cancel hold."
    (let
      ((jobs (dict-get s "jobs")))
      (if
        (null? jobs)
        (html/span (html/attr "class" "muted") "no jobs")
        (html/ul
          (html/attr "class" "ops-jobs")
          (html/join
            (list/map
              (lambda (j)
                (html/li
                  ""
                  (html/join
                    (list
                      (html/span
                        (html/attr "class" "ops-job-fn")
                        (html/text (ops/field-str j "fn")))
                      (html/span
                        (html/attr "class" "muted")
                        (html/text
                          (string-append " "
                            (ops/field-str j "status"))))
                      " "
                      (ops/hold-form
                        "cancel-job"
                        (ops/field-str j "id")
                        "cancel")))))
              jobs))))))
  "Per-identity job list with the cancel-job hold one click away.")

(define ops/session-row-html
  (lambda
    (s)
    "One live-session row: identity, branch, age, provenance, and the
     per-identity spend/burst plus the holds (tokens, jobs, sessions)."
    (let
      ((actor (ops/field-str s "actor"))
        (uid (ops/field-str s "user_id"))
        (branch (ops/field-str s "branch")))
      (html/div
        (html/attr "class" "ops-session card")
        (html/join
          (list
            (html/h3
              (html/join
                (list
                  (html/text actor)
                  (if
                    (ops/field-str s "is_admin")
                    (html/span
                      (html/attr "class" "ops-admin-badge")
                      (html/text "admin"))
                    "")
                  (html/span
                    (html/attr "class" "muted")
                    (html/text
                      (string-append " · " branch
                        " · seen " (ops/age-html (ops/field-int s "age_seconds"))))))))
            (html/p*
              (html/attr "class" "muted ops-uid")
              (html/text uid))
            (html/tag
              "div"
              (html/attr "class" "ops-holds")
              (html/join
                (list
                  (html/strong (html/text "holds "))
                  (ops/hold-form "revoke-sessions" uid "revoke sessions"))))
            (ops/session-tokens-html s)
            (ops/session-jobs-html s))))))
  "One live-session card with all four hold verbs in reach.")

(define ops/session-tokens-html
  (lambda
    (s)
    "The live token rows for one session's identity, each with a revoke hold."
    (let
      ((tokens (dict-get s "tokens")))
      (if
        (null? tokens)
        (html/span (html/attr "class" "muted") "no api tokens")
        (html/ul
          (html/attr "class" "ops-tokens")
          (html/join
            (list/map
              (lambda (t)
                (html/li
                  ""
                  (html/join
                    (list
                      (html/span
                        (html/attr "class" "ops-token-label")
                        (html/text
                          (let
                            ((l (ops/field-str t "label")))
                            (if
                              (string-eq l "")
                              (ops/field-str t "id")
                              l))))
                      (html/span
                        (html/attr "class" "muted")
                        (html/text
                          (string-append " …" (ops/field-str t "last4"))))
                      " "
                      (ops/hold-form
                        "revoke-token"
                        (ops/field-str t "id")
                        "revoke")))))
              tokens))))))
  "Per-identity API-token list with the revoke-token hold one click away.")
