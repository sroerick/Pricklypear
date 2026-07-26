;; Dabar — unified Bible study layer:
;;   /dabar?book=X&chapter=N           → chapter reader
;;   /dabar?book=X&chapter=N&verse=V   → verse study (BSB + interlinear + Strong's)
;;   /dabar?ref=BSB:John:3:16          → verse study
;;   /dabar?random=1                   → random verse, chapter shown with focus
;;   /dabar?q=love                     → search
;;   /strongs?n=H430                    → Strong's entry
;;
;; Types only here:
;;   strongs-entry     — scripts/seed_strongs.py
;;   bible-interlinear  — scripts/seed_interlinear_dogfood.py
;;
;; Data layer: libs/bible/lib.pp (bible/* functions, no routes).
;; Boot loads bible then dabar in session.ml seed_stdlib.

(deftype 'strongs-entry 'number 'lemma 'xlit 'pronounce 'description)
(deftype 'bible-interlinear 'ref 'tokens 'heading 'crossref)

(define strongs/get
  (lambda (number)
    (find-row 'strongs-entry "number" (as-str number)))
  "Fetch strongs-entry row by number field.")

(define strongs/lemma
  (lambda (number)
    (let ((row (strongs/get number)))
      (if (null? row) () (dict-get row "lemma"))))
  "Lemma string for Strong number, or empty.")

(define strongs/description
  (lambda (number)
    (let ((row (strongs/get number)))
      (if (null? row) () (dict-get row "description"))))
  "Description string for Strong number, or empty.")

(define strongs/entry-html
  (lambda (row)
    (if (null? row)
        (html/p* (html/attr "class" "err") "not found")
        (html/div (html/attr "class" "card strongs-entry")
          (html/join
            (list
              (html/h2 (html/text (dict-get row "number")))
              (html/p (dict-get row "lemma"))
              (html/p* (html/attr "class" "muted")
                (html/text
                  (string-append
                    (as-str (dict-get row "xlit"))
                    " / "
                    (as-str (dict-get row "pronounce")))))
              (html/p (dict-get row "description")))))))
  "HTML card for one Strong entry row.")

(define strongs/lookup-form
  (lambda (n)
    (html/form "GET" "/strongs"
      (html/attrs
        (list
          (html/attr "class" "card")
          (html/attr "hx-get" "/strongs")
          (html/attr "hx-target" "#strongs-panel")
          (html/attr "hx-swap" "innerHTML")
          (html/attr "hx-push-url" "true")))
      (html/join
        (list
          (html/label ""
            (html/join
              (list
                "Number "
                (html/text-input "n"
                  (html/attrs
                    (list
                      (html/attr "value" (if (null? n) "" (as-str n)))
                      (html/attr "placeholder" "H430 or G26")))))))
          " "
          (html/button (html/attr "type" "submit") "Look up")))))
  "Strong lookup form prefilled with n.")

(define strongs/panel
  (lambda ()
    (let ((n (request-param "n")))
      (html/join
        (list
          (html/h1 "Strong's")
          (strongs/lookup-form n)
          (if (null? n)
              (html/p* (html/attr "class" "muted")
                "Enter a Strong's number (H for Hebrew, G for Greek). Seed: scripts/seed_strongs.py")
              (strongs/entry-html (strongs/get n)))
          (html/p* (html/attr "class" "muted")
            (html/join
              (list
                (html/a "/dabar?book=John&chapter=3" "Bible")
                " · "
                (html/a "/dabar?book=John&chapter=3&verse=16" "Dabar study"))))))))
  "Strong panel from request param n.")

(define strongs/page
  (lambda ()
    (if (hx-request?)
        (strongs/panel)
        (html/page
          (list
            (html/div (html/attr "id" "strongs-panel")
              (strongs/panel))))))
  "GET /strongs page or htmx panel.")

(define dabar/inter-row
  (lambda (book chapter verse)
    (find-row 'bible-interlinear "ref"
      (bible/ref book chapter verse)))
  "bible-interlinear row for book/chapter/verse ref.")

(define dabar/inter-html
  (lambda (book chapter verse)
    (let ((row (dabar/inter-row book chapter verse)))
      (if (null? row)
          (html/p* (html/attr "class" "muted")
            (string-append
              "No interlinear for "
              (bible/ref book chapter verse)
              " — plain: "
              (as-str (bible/verse book chapter verse))))
          (let ((pre (dict-get row "html")))
            (if (null? pre)
                (html/p* (html/attr "class" "muted")
                  "interlinear row has no html field")
                pre)))))
  "Interlinear HTML or muted fallback for verse.")

(define dabar/verse-study
  (lambda (book chapter verse)
    (let ((ab (bible/abbrev-of book))
          (ch (bible/safe-chapter book chapter)))
      (html/join
        (list
          (html/h1
            (string-append
              (as-str book) " " (as-str ch) ":" (as-str verse)))
          (bible/nav-links ab ch)
          (html/div (html/attr "class" "card")
            (html/join
              (list
                (html/h2 "BSB")
                (html/p (bible/verse book ch verse)))))
          (html/div (html/attr "class" "card dabar-inter")
            (html/join
              (list
                (html/h2 "Interlinear")
                (dabar/inter-html book ch verse))))
          (html/p* (html/attr "class" "muted")
            (html/join
              (list
                (html/a
                  (string-append
                    "/dabar?book="
                    (bible/abbrev-of book)
                    "&chapter="
                    (as-str ch))
                  "open in reader")
                " · "
                (html/a "/strongs" "Strong's"))))))))
  "Verse study block: BSB, interlinear, nav links.")

(define dabar/verse-study-from-params
  (lambda (book chapter verse)
    (let ((b (if (null? book) "John" (as-str book)))
          (c (if (null? chapter) "3" (as-str chapter)))
          (v (as-str verse)))
      (dabar/verse-study
        b
        (bible/safe-chapter b c)
        (bible/parse-int v))))
  "Verse study from book/chapter/verse params with defaults.")

(define dabar/verse-study-from-ref
  (lambda (ref)
    (let ((row (bible/parse-ref ref)))
      (if (null? row)
          (html/join
            (list
              (bible/go-form "John" "3" ())
              (html/p* (html/attr "class" "err")
                (string-append "Unknown ref " (as-str ref)))))
          (let ((book (dict-get row "book"))
                (chapter (dict-get row "chapter"))
                (verse (dict-get row "verse")))
            (html/join
              (list
                (bible/go-form book chapter verse)
                (dabar/verse-study book chapter verse)))))))
  "Verse study from BSB ref string, or error.")

;; Dispatch: ref > verse study, otherwise chapter reader (handles random/search).
(define dabar/panel-for-request
  (lambda ()
    (let ((ref (request-param "ref"))
          (verse (request-param "verse")))
      (let ((has-ref
              (if (null? ref) #f
                  (not (string-eq (as-str ref) ""))))
            (has-verse
              (if (null? verse) #f
                  (not (string-eq (as-str verse) "")))))
        (if has-ref
            (dabar/verse-study-from-ref ref)
            (if has-verse
                (dabar/verse-study-from-params
                  (request-param "book")
                  (request-param "chapter")
                  verse)
                (bible/chapter-reader))))))
  "Dispatch dabar panel: ref, verse study, or chapter reader.")

(define dabar/page
  (lambda ()
    (if (string-eq (request-method) "POST")
        (let ((book (request-param "book"))
              (chapter (request-param "chapter"))
              (verse (request-param "verse")))
          (redirect
            (string-append
              "/dabar?book="
              (if (null? book) "John" (as-str book))
              "&chapter="
              (if (null? chapter) "3" (as-str chapter))
              (if (null? verse)
                  ""
                  (if (string-eq (as-str verse) "")
                      ""
                      (string-append "&verse=" (as-str verse)))))))
        (if (hx-request?)
            (dabar/panel-for-request)
            (html/page
              (list
                (html/div (html/attr "id" "dabar-panel")
                  (dabar/panel-for-request)))))))
  "GET/POST /dabar handler (POST redirects to query).")

(route "/dabar" "dabar/page")
(route "/strongs" "strongs/page")
