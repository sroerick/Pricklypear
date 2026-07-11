;; Bible library (BSB) — functions + /bible route.
;; Canon text lives in data_rows (seed: scripts/seed_bible_bsb.py).
;; Install: (load-library "bible") or POST /api/recipe/bible
;;
;; Ref format: BSB:<Abbrev>:<chapter>:<verse>  e.g. BSB:John:3:16
;; URLs: /bible?book=John&chapter=3&verse=16  or  ?ref=BSB:John:3:16
;;
;; Text: Berean Standard Bible (see seed script NOTICE / attribution).

(deftype 'bible-book 'name 'abbrev 'ordinal 'testament)
(deftype 'bible-verse 'translation 'book 'abbrev 'chapter 'verse 'text 'ref)

(define bible/book-row
  (lambda (book)
    (let ((b (as-str book)))
      (let ((by-ab (find-row 'bible-book "abbrev" b)))
        (if (null? by-ab)
            (find-row 'bible-book "name" b)
            by-ab)))))

(define bible/abbrev-of
  (lambda (book)
    (let ((row (bible/book-row book)))
      (if (null? row)
          (as-str book)
          (dict-get row "abbrev")))))

(define bible/ref
  (lambda (book chapter verse)
    (string-append
      "BSB:"
      (bible/abbrev-of book)
      ":"
      (as-str chapter)
      ":"
      (as-str verse))))

(define bible/verse-row
  (lambda (book chapter verse)
    (find-row 'bible-verse "ref" (bible/ref book chapter verse))))

(define bible/verse
  (lambda (book chapter verse)
    (let ((row (bible/verse-row book chapter verse)))
      (if (null? row)
          ()
          (dict-get row "text")))))

(define bible/chapter
  (lambda (book chapter)
    (find-rows 'bible-verse
      "abbrev" (bible/abbrev-of book)
      "chapter" (as-str chapter)
      200)))

(define bible/books
  (lambda ()
    (rows 'bible-book 100)))

(define bible/search
  (lambda (q)
    (find-text 'bible-verse "text" (as-str q) 40)))

(define bible/parse-ref
  (lambda (ref)
    ;; "BSB:John:3:16" -> uses find-row directly via ref
    (find-row 'bible-verse "ref" (as-str ref))))

(define bible/verse-li
  (lambda (row)
    (let ((fields (cdr row)))
      (let ((n (dict-get fields "verse"))
            (text (dict-get fields "text"))
            (ref (dict-get fields "ref")))
        (string-append
          "<li id=\"" (as-str ref) "\"><sup>"
          (as-str n)
          "</sup> "
          (as-str text)
          "</li>")))))

(define bible/chapter-html
  (lambda (book chapter focus-verse)
    (let ((rows (bible/chapter book chapter)))
      (string-append
        "<h1>"
        (as-str book)
        " "
        (as-str chapter)
        "</h1>"
        "<ol class=\"bible-chapter\">"
        (list/foldl
          (lambda (acc row) (string-append acc (bible/verse-li row)))
          ""
          rows)
        "</ol>"
        (if (null? focus-verse)
            ""
            (string-append
              "<p class=\"muted\">Focus verse "
              (as-str focus-verse)
              ": "
              (as-str (bible/verse book chapter focus-verse))
              "</p>"))))))

(define bible/page
  (lambda ()
    (if (string-eq (request-method) "POST")
        (let ((book (request-param "book"))
              (chapter (request-param "chapter"))
              (verse (request-param "verse")))
          (redirect
            (string-append
              "/bible?book="
              (if (null? book) "John" (as-str book))
              "&chapter="
              (if (null? chapter) "3" (as-str chapter))
              (if (null? verse)
                  ""
                  (string-append "&verse=" (as-str verse))))))
        (let ((ref (request-param "ref"))
              (q (request-param "q")))
          (if (null? q)
              (if (null? ref)
                  (let ((book (request-param "book"))
                        (chapter (request-param "chapter"))
                        (verse (request-param "verse")))
                    (let ((b (if (null? book) "John" (as-str book)))
                          (c (if (null? chapter) "3" (as-str chapter))))
                      (render-html
                        (string-append
                          "<form method=\"GET\" action=\"/bible\" class=\"card\">"
                          "<label>Book <input name=\"book\" value=\"" b "\"></label> "
                          "<label>Chapter <input name=\"chapter\" value=\"" c "\"></label> "
                          "<label>Verse <input name=\"verse\" placeholder=\"optional\" value=\""
                          (if (null? verse) "" (as-str verse))
                          "\"></label> "
                          "<button type=\"submit\">Go</button></form>"
                          (bible/chapter-html b c verse)
                          "<p class=\"muted\">Berean Standard Bible · ref "
                          (bible/ref b c (if (null? verse) 1 verse))
                          " · <a href=\"/bible?q=love\">search love</a></p>"))))
                  (let ((row (bible/parse-ref ref)))
                    (if (null? row)
                        (render-html
                          (string-append "<p class=\"err\">Unknown ref "
                            (as-str ref) "</p>"))
                        (let ((book (dict-get row "book"))
                              (chapter (dict-get row "chapter"))
                              (verse (dict-get row "verse")))
                          (render-html
                            (string-append
                              (bible/chapter-html book chapter verse)
                              "<p class=\"muted\">ref " (as-str ref)
                              " · Berean Standard Bible</p>"))))))
              (let ((hits (bible/search q)))
                (render-html
                  (string-append
                    "<h1>Search: " (as-str q) "</h1>"
                    "<ul>"
                    (list/foldl
                      (lambda (acc row)
                        (let ((f (cdr row)))
                          (string-append acc
                            "<li><a href=\"/bible?ref="
                            (as-str (dict-get f "ref"))
                            "\">"
                            (as-str (dict-get f "ref"))
                            "</a> "
                            (as-str (dict-get f "text"))
                            "</li>")))
                      ""
                      hits)
                    "</ul>"
                    "<p class=\"muted\">Berean Standard Bible</p>"))))))))

(route "/bible" "bible/page")
