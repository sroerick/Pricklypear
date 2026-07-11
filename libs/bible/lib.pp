;; Bible library (BSB) — functions + /bible route.
;; Canon text lives in data_rows (seed: scripts/seed_bible_bsb.py).
;; Install: (load-library "bible") or POST /api/recipe/bible
;;
;; Ref format: BSB:<Abbrev>:<chapter>:<verse>  e.g. BSB:John:3:16
;; URLs: /bible?book=John&chapter=3&verse=16  or  ?ref=BSB:John:3:16
;;
;; Partials (htmx — same pattern as todo):
;;   bible/chapter-panel  → chapter HTML fragment
;;   route /partials/bible-chapter
;; Full page: /bible (chrome + form + #bible-panel).
;; GET with HX-Request returns panel fragment only (swap into #bible-panel).
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

;; Fragment: chapter body only (htmx #bible-panel / /partials/bible-chapter).
(define bible/chapter-panel
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

;; Alias kept for older call sites.
(define bible/chapter-html bible/chapter-panel)

(define bible/search-panel
  (lambda (q)
    (let ((hits (bible/search q)))
      (html/join
        (list
          (html/h1 (string-append "Search: " (as-str q)))
          (html/ul (html/attr "class" "bible-hits")
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
            "Berean Standard Bible"))))))

(define bible/go-form
  (lambda (b c verse)
    (html/form "GET" "/bible"
      (html/attrs
        (list
          (html/attr "class" "card bible-go")
          (html/attr "hx-get" "/bible")
          (html/attr "hx-target" "#bible-panel")
          (html/attr "hx-swap" "innerHTML")
          (html/attr "hx-push-url" "true")))
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

(define bible/search-form
  (lambda (q)
    (html/form "GET" "/bible"
      (html/attrs
        (list
          (html/attr "class" "card bible-search")
          (html/attr "hx-get" "/bible")
          (html/attr "hx-target" "#bible-panel")
          (html/attr "hx-swap" "innerHTML")
          (html/attr "hx-push-url" "true")))
      (html/join
        (list
          (html/label ""
            (html/join
              (list
                "Search "
                (html/text-input "q"
                  (html/attrs
                    (list
                      (html/attr "value" (if (null? q) "" (as-str q)))
                      (html/attr "placeholder" "love")))))))
          " "
          (html/button (html/attr "type" "submit") "Find"))))))

;; Resolve current GET view into a panel fragment (no chrome).
(define bible/panel-for-request
  (lambda ()
    (let ((ref (request-param "ref"))
          (q (request-param "q")))
      (if (null? q)
          (if (null? ref)
              (let ((book (request-param "book"))
                    (chapter (request-param "chapter"))
                    (verse (request-param "verse")))
                (let ((b (if (null? book) "John" (as-str book)))
                      (c (if (null? chapter) "3" (as-str chapter))))
                  (bible/chapter-panel b c verse)))
              (let ((row (bible/parse-ref ref)))
                (if (null? row)
                    (html/p*
                      (html/attr "class" "err")
                      (string-append "Unknown ref " (as-str ref)))
                    (let ((book (dict-get row "book"))
                          (chapter (dict-get row "chapter"))
                          (verse (dict-get row "verse")))
                      (bible/chapter-panel book chapter verse)))))
          (bible/search-panel q)))))

;; Raw partial route handler (query: book, chapter, verse | ref | q).
(define bible/partial
  (lambda ()
    (bible/panel-for-request)))

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
        ;; GET: htmx wants fragment only; full page wraps #bible-panel.
        (if (hx-request?)
            (bible/panel-for-request)
            (let ((ref (request-param "ref"))
                  (q (request-param "q"))
                  (book (request-param "book"))
                  (chapter (request-param "chapter"))
                  (verse (request-param "verse")))
              (let ((b (if (null? book) "John" (as-str book)))
                    (c (if (null? chapter) "3" (as-str chapter))))
                (html/page
                  (list
                    (html/h1 "Bible")
                    (bible/go-form b c verse)
                    (bible/search-form q)
                    (html/div (html/attr "id" "bible-panel")
                      (bible/panel-for-request))
                    (html/p*
                      (html/attr "class" "muted")
                      (html/join
                        (list
                          "Berean Standard Bible · "
                          (if (null? ref)
                              (bible/ref b c (if (null? verse) 1 verse))
                              (as-str ref))
                          " · htmx swaps #bible-panel · "
                          (html/a "/partials/bible-chapter" "raw partial"))))))))))))

(route "/bible" "bible/page")
(route "/partials/bible-chapter" "bible/partial")
