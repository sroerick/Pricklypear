;; Bible library (BSB) — functions + /bible route.
;; Canon text lives in data_rows (seed: scripts/seed_bible_bsb.py).
;; Install: (load-library "bible") or POST /api/recipe/bible
;;
;; Ref format: BSB:<Abbrev>:<chapter>:<verse>  e.g. BSB:John:3:16
;; URLs: /bible?book=John&chapter=3&verse=16  or  ?ref=BSB:John:3:16
;;
;; Text: Berean Standard Bible (see seed script NOTICE / attribution).
;; HTML via std html/* helpers.

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
    (find-row 'bible-verse "ref" (as-str ref))))

(define bible/verse-li
  (lambda (row)
    (let ((fields (cdr row)))
      (let ((n (dict-get fields "verse"))
            (text (dict-get fields "text"))
            (ref (dict-get fields "ref")))
        (html/li (html/attr "id" (as-str ref))
          (html/join
            (list
              (html/tag "sup" "" (html/text (as-str n)))
              " "
              (html/text text))))))))

(define bible/chapter-html
  (lambda (book chapter focus-verse)
    (let ((rows (bible/chapter book chapter)))
      (html/join
        (list
          (html/h1
            (string-append (as-str book) " " (as-str chapter)))
          (html/tag "ol" (html/attr "class" "bible-chapter")
            (list/foldl
              (lambda (acc row) (string-append acc (bible/verse-li row)))
              ""
              rows))
          (if (null? focus-verse)
              ""
              (html/p*
                (html/attr "class" "muted")
                (string-append
                  "Focus verse "
                  (as-str focus-verse)
                  ": "
                  (as-str (bible/verse book chapter focus-verse))))))))))

(define bible/go-form
  (lambda (b c verse)
    (html/form "GET" "/bible" (html/attr "class" "card")
      (html/join
        (list
          (html/label ""
            (html/join
              (list
                "Book "
                (html/text-input "book"
                  (html/attr "value" b)))))
          " "
          (html/label ""
            (html/join
              (list
                "Chapter "
                (html/text-input "chapter"
                  (html/attr "value" c)))))
          " "
          (html/label ""
            (html/join
              (list
                "Verse "
                (html/text-input "verse"
                  (html/attrs
                    (list
                      (html/attr "placeholder" "optional")
                      (html/attr "value"
                        (if (null? verse) "" (as-str verse)))))))))
          " "
          (html/button (html/attr "type" "submit") "Go"))))))

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
                      (html/page
                        (list
                          (bible/go-form b c verse)
                          (bible/chapter-html b c verse)
                          (html/p*
                            (html/attr "class" "muted")
                            (html/join
                              (list
                                "Berean Standard Bible · ref "
                                (bible/ref b c (if (null? verse) 1 verse))
                                " · "
                                (html/a "/bible?q=love" "search love"))))))))
                  (let ((row (bible/parse-ref ref)))
                    (if (null? row)
                        (html/page
                          (list
                            (html/p*
                              (html/attr "class" "err")
                              (string-append "Unknown ref " (as-str ref)))))
                        (let ((book (dict-get row "book"))
                              (chapter (dict-get row "chapter"))
                              (verse (dict-get row "verse")))
                          (html/page
                            (list
                              (bible/chapter-html book chapter verse)
                              (html/p*
                                (html/attr "class" "muted")
                                (string-append
                                  "ref "
                                  (as-str ref)
                                  " · Berean Standard Bible"))))))))
              (let ((hits (bible/search q)))
                (html/page
                  (list
                    (html/h1 (string-append "Search: " (as-str q)))
                    (html/ul ""
                      (list/foldl
                        (lambda (acc row)
                          (let ((f (cdr row)))
                            (string-append acc
                              (html/li ""
                                (html/join
                                  (list
                                    (html/a
                                      (string-append
                                        "/bible?ref="
                                        (as-str (dict-get f "ref")))
                                      (as-str (dict-get f "ref")))
                                    " "
                                    (html/text (dict-get f "text"))))))))
                        ""
                        hits))
                    (html/p*
                      (html/attr "class" "muted")
                      "Berean Standard Bible")))))))))

(route "/bible" "bible/page")
