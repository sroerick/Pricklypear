;; Dabar — study layer on top of bible/ (Strong's + interlinear dogfood).
;; Install: (load-library "dabar") or POST /api/recipe/dabar
;; Requires: bible/* already loaded.
;;
;; Strong's: type strongs-entry — seed scripts/seed_strongs.py
;; Interlinear: type bible-interlinear (ref + html dogfood) —
;;   seed scripts/seed_interlinear_dogfood.py

(deftype 'strongs-entry 'number 'lemma 'xlit 'pronounce 'description)
(deftype 'bible-interlinear 'ref 'tokens 'heading 'crossref)

(define strongs/get
  (lambda (number)
    (find-row 'strongs-entry "number" (as-str number))))

(define strongs/lemma
  (lambda (number)
    (let ((row (strongs/get number)))
      (if (null? row) () (dict-get row "lemma")))))

(define strongs/description
  (lambda (number)
    (let ((row (strongs/get number)))
      (if (null? row) () (dict-get row "description")))))

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
              (html/p (dict-get row "description"))))))))

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
          (html/button (html/attr "type" "submit") "Look up"))))))

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
                (html/a "/bible" "Bible")
                " · "
                (html/a "/dabar" "Dabar study")))))))))

(define strongs/page
  (lambda ()
    (if (hx-request?)
        (strongs/panel)
        (html/page
          (list
            (html/div (html/attr "id" "strongs-panel")
              (strongs/panel)))))))

(define dabar/inter-row
  (lambda (book chapter verse)
    (find-row 'bible-interlinear "ref"
      (bible/ref book chapter verse))))

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
                pre))))))

(define dabar/verse-study
  (lambda (book chapter verse)
    (html/join
      (list
        (html/h1
          (string-append
            (as-str book) " " (as-str chapter) ":" (as-str verse)))
        (bible/nav-links (bible/abbrev-of book)
          (bible/safe-chapter book chapter))
        (html/div (html/attr "class" "card")
          (html/join
            (list
              (html/h2 "BSB")
              (html/p (bible/verse book chapter verse)))))
        (html/div (html/attr "class" "card dabar-inter")
          (html/join
            (list
              (html/h2 "Interlinear")
              (dabar/inter-html book chapter verse))))
        (html/p* (html/attr "class" "muted")
          (html/join
            (list
              (html/a
                (string-append
                  "/bible?book="
                  (bible/abbrev-of book)
                  "&chapter="
                  (as-str chapter)
                  "&verse="
                  (as-str verse))
                "open in reader")
              " · "
              (html/a "/strongs" "Strong's"))))))))

(define dabar/panel
  (lambda ()
    (let ((book (request-param "book"))
          (chapter (request-param "chapter"))
          (verse (request-param "verse"))
          (ref (request-param "ref")))
      (if (null? ref)
          (let ((b (if (null? book) "John" (as-str book)))
                (c (if (null? chapter) "3" (as-str chapter)))
                (v (if (null? verse) "16" (as-str verse))))
            (dabar/verse-study b c v))
          (let ((row (bible/parse-ref ref)))
            (if (null? row)
                (html/p* (html/attr "class" "err")
                  (string-append "unknown ref " (as-str ref)))
                (dabar/verse-study
                  (dict-get row "abbrev")
                  (dict-get row "chapter")
                  (dict-get row "verse"))))))))

(define dabar/page
  (lambda ()
    (if (hx-request?)
        (dabar/panel)
        (html/page
          (list
            (html/h1 "Dabar")
            (html/div (html/attr "id" "dabar-panel")
              (dabar/panel)))))))

(route "/strongs" "strongs/page")
(route "/dabar" "dabar/page")
