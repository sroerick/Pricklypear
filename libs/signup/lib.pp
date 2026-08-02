;; package signup -- exported from live branch system/main
;; Sync: scripts/pp_sync.py pull|push

(define signup/blank?
  (lambda (s) (blank? (string-trim (as-str s)))))

(define signup/fields
  (lambda
    (email note source ua)
    (dict-set*
      "{}"
      (list
        "email"
        (string-trim (as-str email))
        "note"
        (as-str note)
        "source"
        (as-str source)
        "user_agent"
        (as-str ua)))))

(define signup/form
  (lambda
    (msg err)
    (layout/page
      (list
        (layout/hero
          "Stay in the loop"
          "Drop your email if you want updates when Pricklypear grows. No spam, no list-sold nonsense — just a note in my inbox."
          (html/join
            (list
              (layout/cta "/welcome" "Welcome")
              (layout/cta "/background" "Background")
              (layout/cta "/login" "Log in"))))
        (if
          (signup/blank? err)
          ""
          (html/p* (html/attr "class" "err") (html/text err)))
        (if
          (signup/blank? msg)
          ""
          (html/p* (html/attr "class" "ok") (html/text msg)))
        (layout/section
          "Email signup"
          (html/form
            "POST"
            "/signup"
            (html/attr "class" "signup-form")
            (html/join
              (list
                (html/p "Email")
                (html/text-input
                  "email"
                  (html/attrs
                    (list
                      (html/attr "required" "required")
                      (html/attr "placeholder" "you@example.com")
                      (html/attr "autocomplete" "email"))))
                (html/p "Optional note")
                (html/textarea
                  "note"
                  (html/attrs
                    (list
                      (html/attr "rows" "3")
                      (html/attr "placeholder" "What brought you here?")))
                  "")
                (html/hidden "source" "web")
                (html/div
                  ""
                  (html/button
                    (html/attrs (list (html/attr "type" "submit")))
                    "Sign up"))))))
        (html/div
          (html/attr "class" "layout-foot")
          (html/join
            (list
              (html/a "/welcome" (html/text "welcome"))
              (html/text " · ")
              (html/a "/roadmap" (html/text "roadmap")))))))))

(define signup/looks-like-email?
  (lambda
    (email)
    (let
      ((e (string-trim (as-str email))))
      (if
        (signup/blank? e)
        #f
        (if
          (string-contains? e "@")
          (if (> (string-length e) 3) #t #f)
          #f)))))

(define signup/notify-inbox
  (lambda
    (email note)
    (let
      ((title
          (string-append "Signup: " (string-trim (as-str email))))
        (body
          (string-append
            "New public signup\n"
            "email: "
            (string-trim (as-str email))
            "\n"
            "note: "
            (as-str note)
            "\n"
            "source: web")))
      (if
        (string-eq (type-of notes/capture/add) "lambda")
        (notes/capture/add title body)
        "no-notes"))))

(define signup/page
  (lambda
    ()
    (if
      (string-eq (request-method) "POST")
      (let
        ((email (request-param "email"))
          (note (request-param "note")))
        (if
          (signup/looks-like-email? email)
          (seq
            (with-public-write
              (lambda () (signup/record email note)))
            (signup/thanks email))
          (signup/form "" "Need a real-looking email (with @).")))
      (signup/form "" ""))))

(define signup/record
  (lambda
    (email note)
    (let
      ((fields (signup/fields email note "web" "")))
      (seq
        (create (quote signup) fields)
        (seq (signup/notify-inbox email note) email)))))

(define signup/thanks
  (lambda
    (email)
    (layout/page
      (list
        (layout/hero
          "You're on the list"
          (string-append
            "Thanks — "
            (as-str email)
            ".")
          (html/join
            (list
              (layout/cta "/welcome" "Back to welcome")
              (layout/cta "/code" "Browse the source")
              (layout/cta "/login" "Log in"))))
        (html/div
          (html/attr "class" "layout-foot")
          (html/a "/welcome" (html/text "welcome")))))))

;; Public signup form (hard-public path in http.ml).
(public-route "/signup" "signup/page")
