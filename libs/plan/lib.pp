;; package plan -- exported from live branch system/main
;; Sync: scripts/pp_sync.py pull|push

(define plan
  (lambda (body) (plan/set! body)))

(define plan/all-rows
  (lambda () (rows (quote user-plan) 200)))

(define plan/clear!
  (lambda () (plan/set! "")))

(define plan/directory-html
  (lambda
    ()
    (let
      ((xs (plan-list)))
      (if
        (null? xs)
        (html/p*
          (html/attr "class" "muted")
          "No one has set a plan yet.")
        (html/ul "" (html/join (list/map plan/directory-li xs)))))))

(define plan/directory-li
  (lambda
    (pair)
    (let
      ((user (as-str (car pair))) (body (as-str (cdr pair))))
      (html/li
        (html/attr "class" "card")
        (html/join
          (list
            (html/p*
              ""
              (html/a
                (string-append "/profile?user=" user)
                (html/strong user)))
            (html/p (html/text body))))))))

(define plan/fields-body
  (lambda (fields) (as-str (dict-get fields "body"))))

(define plan/fields-updated
  (lambda
    (fields)
    (let
      ((u (dict-get fields "updated_at")))
      (if (null? u) "" (as-str u)))))

(define plan/fields-username
  (lambda (fields) (as-str (dict-get fields "username"))))

(define plan/find-row
  (lambda
    (username)
    (let
      ((want (as-str username)))
      (list/foldl
        (lambda
          (acc row)
          (if
            (null? acc)
            (if
              (string-eq
                (plan/fields-username (plan/row-fields row))
                want)
              row
              acc)
            acc))
        ()
        (plan/all-rows)))))

(define plan/form-html
  (lambda
    (body)
    (html/div
      (html/attr "class" "card")
      (html/join
        (list
          (html/h2 "Your plan")
          (html/p*
            (html/attr "class" "muted")
            "Like a Unix .plan file \226\128\148 short note other users on this box can read.")
          (html/form
            "POST"
            "/plan"
            (html/attrs
              (list
                (html/attr "hx-post" "/plan")
                (html/attr "hx-target" "#plan-main")
                (html/attr "hx-swap" "innerHTML")))
            (html/join
              (list
                (html/label
                  (html/attr "for" "plan-body")
                  "What are you working on?")
                (html/textarea
                  "body"
                  (html/attrs
                    (list
                      (html/attr "id" "plan-body")
                      (html/attr "rows" "4")
                      (html/attr
                        "placeholder"
                        "shipping macros, out Thursday, \226\128\166")))
                  (as-str body))
                (html/button (html/attr "type" "submit") "Save plan")))))))))

(define plan/main-html
  (lambda
    ()
    (let
      ((body (plan/mine)))
      (html/div
        (html/attr "id" "plan-main")
        (html/join
          (list (plan/mine-display body) (plan/form-html body)))))))

(define plan/mine
  (lambda () (plan/of (whoami))))

(define plan/mine-display
  (lambda
    (body)
    (html/div
      (html/attr "class" "card")
      (if
        (blank? body)
        (html/p*
          (html/attr "class" "muted")
          "No plan yet. What are you working on?")
        (html/p (html/text body))))))

(define plan/of
  (lambda
    (username)
    (let
      ((row (plan/find-row username)))
      (if
        (null? row)
        ""
        (plan/fields-body (plan/row-fields row))))))

(define plan/page
  (lambda
    ()
    (if
      (string-eq (request-method) "POST")
      (seq
        (plan/set! (request-param "body"))
        (if
          (hx-request?)
          (render-html (plan/main-html))
          (redirect "/plan")))
      (html/page
        (list
          (html/h1 "Plan")
          (html/p*
            (html/attr "class" "muted")
            (string-append "Signed in as " (as-str (whoami))))
          (plan/main-html)
          (html/p* "" (html/a "/plans" "Everyone's plans")))))))

(define plan/plans-page
  (lambda
    ()
    (html/page
      (list
        (html/h1 "Plans")
        (html/p*
          (html/attr "class" "muted")
          "Who is working on what (Unix .plan directory).")
        (html/p* "" (html/a "/plan" "Edit my plan"))
        (plan/directory-html)))))

(define plan/profile-page
  (lambda
    ()
    (let
      ((user (as-str (request-param "user"))))
      (if
        (blank? user)
        (html/page
          (list
            (html/h1 "Profile")
            (html/p*
              (html/attr "class" "muted")
              "Pass ?user=username (v1; path /u/:user deferred).")))
        (let
          ((body (plan/of user)))
          (html/page
            (list
              (html/h1 (html/text user))
              (html/p* (html/attr "class" "muted") "Plan (finger)")
              (if
                (blank? body)
                (html/p*
                  (html/attr "class" "muted")
                  "No plan yet. What are you working on?")
                (html/div
                  (html/attr "class" "card")
                  (html/p (html/text body))))
              (html/p* "" (html/a "/plans" "All plans"))
              (html/p* "" (html/a "/plan" "My plan")))))))))

(define plan/row-fields
  (lambda (row) (cdr row)))

(define plan/row-id
  (lambda (row) (as-str (car row))))

(define plan/set!
  (lambda
    (body)
    (let
      ((user (as-str (whoami)))
        (text (as-str body))
        (row (plan/find-row (whoami))))
      (if
        (null? row)
        (seq
          (create
            (quote user-plan)
            (dict-set*
              "{}"
              (list
                "username"
                user
                "body"
                text
                "updated_at"
                "")))
          text)
        (seq
          (update
            (quote user-plan)
            (plan/row-id row)
            (dict-set*
              "{}"
              (list
                "username"
                user
                "body"
                text
                "updated_at"
                "")))
          text)))))

