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
          ;; list-style:none — <ol> markers + <sup> verse nums was double-numbering
          (html/tag "ol"
            (html/attrs
              (list
                (html/attr "class" "bible-chapter")
                (html/attr "style"
                  "list-style:none;padding-left:0;margin:.25rem 0")))
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

;; Protestant BSB chapter counts (abbrev . n). Used for chapter <select>.
(define bible/chapter-counts
  (list
    (list "Gen" 50) (list "Exod" 40) (list "Lev" 27) (list "Num" 36)
    (list "Deut" 34) (list "Josh" 24) (list "Judg" 21) (list "Ruth" 4)
    (list "1Sam" 31) (list "2Sam" 24) (list "1Kgs" 22) (list "2Kgs" 25)
    (list "1Chr" 29) (list "2Chr" 36) (list "Ezra" 10) (list "Neh" 13)
    (list "Esth" 10) (list "Job" 42) (list "Ps" 150) (list "Prov" 31)
    (list "Eccl" 12) (list "Song" 8) (list "Isa" 66) (list "Jer" 52)
    (list "Lam" 5) (list "Ezek" 48) (list "Dan" 12) (list "Hos" 14)
    (list "Joel" 3) (list "Amos" 9) (list "Obad" 1) (list "Jonah" 4)
    (list "Mic" 7) (list "Nah" 3) (list "Hab" 3) (list "Zeph" 3)
    (list "Hag" 2) (list "Zech" 14) (list "Mal" 4)
    (list "Matt" 28) (list "Mark" 16) (list "Luke" 24) (list "John" 21)
    (list "Acts" 28) (list "Rom" 16) (list "1Cor" 16) (list "2Cor" 13)
    (list "Gal" 6) (list "Eph" 6) (list "Phil" 4) (list "Col" 4)
    (list "1Thess" 5) (list "2Thess" 3) (list "1Tim" 6) (list "2Tim" 4)
    (list "Titus" 3) (list "Phlm" 1) (list "Heb" 13) (list "Jas" 5)
    (list "1Pet" 5) (list "2Pet" 3) (list "1John" 5) (list "2John" 1)
    (list "3John" 1) (list "Jude" 1) (list "Rev" 22)))

(define bible/chapter-count
  (lambda (abbrev)
    (list/foldl
      (lambda (acc pair)
        (if (string-eq (car pair) (as-str abbrev))
            (car (cdr pair))
            acc))
      1
      bible/chapter-counts)))

(define bible/int-in-range?
  (lambda (s lo hi)
    (bible/int-in-range-from? s lo hi lo)))

(define bible/int-in-range-from?
  (lambda (s lo hi i)
    (if (> i hi)
        #f
        (if (string-eq (as-str s) (as-str i))
            #t
            (bible/int-in-range-from? s lo hi (+ i 1))))))

(define bible/safe-chapter
  (lambda (book chapter)
    (let ((max (bible/chapter-count (bible/abbrev-of book)))
          (c (if (null? chapter) "1" (as-str chapter))))
      (if (bible/int-in-range? c 1 max) c "1"))))

(define bible/option-range
  (lambda (n selected)
    (bible/option-range-from 1 n selected)))

(define bible/option-range-from
  (lambda (i n selected)
    (if (> i n)
        ""
        (string-append
          (html/option (as-str i) (as-str i)
            (string-eq (as-str selected) (as-str i)))
          (bible/option-range-from (+ i 1) n selected)))))

(define bible/book-options
  (lambda (current)
    (list/foldl
      (lambda (acc row)
        (let ((f (cdr row)))
          (let ((name (dict-get f "name"))
                (abbrev (dict-get f "abbrev")))
            (string-append acc
              (html/option (as-str abbrev) (as-str name)
                (if (string-eq (as-str current) (as-str abbrev))
                    #t
                    (string-eq (as-str current) (as-str name))))))))
      ""
      (bible/books))))

(define bible/verse-options
  (lambda (book chapter selected)
    (string-append
      (html/option "" "—"
        (if (null? selected) #t (string-eq (as-str selected) "")))
      (list/foldl
        (lambda (acc row)
          (let ((n (dict-get (cdr row) "verse")))
            (string-append acc
              (html/option (as-str n) (as-str n)
                (string-eq (as-str selected) (as-str n))))))
        ""
        (bible/chapter book chapter)))))

;; Dropdown navigator: book / chapter / verse.
;; change from:select reloads #bible-panel so option lists stay valid.
(define bible/go-form
  (lambda (b c verse)
    (let ((ab (bible/abbrev-of b))
          (ch (bible/safe-chapter b c))
          (vs (if (null? verse) "" (as-str verse))))
      (html/form "GET" "/bible"
        (html/attrs
          (list
            (html/attr "class" "card bible-go")
            (html/attr "hx-get" "/bible")
            (html/attr "hx-target" "#bible-panel")
            (html/attr "hx-swap" "innerHTML")
            (html/attr "hx-push-url" "true")
            (html/attr "hx-trigger" "submit, change from:select")))
        (html/join
          (list
            (html/label ""
              (html/join
                (list
                  "Book "
                  (html/select "book" "" (bible/book-options ab)))))
            " "
            (html/label ""
              (html/join
                (list
                  "Chapter "
                  (html/select "chapter" ""
                    (bible/option-range
                      (bible/chapter-count ab) ch)))))
            " "
            (html/label ""
              (html/join
                (list
                  "Verse "
                  (html/select "verse" ""
                    (bible/verse-options ab ch vs)))))
            " "
            (html/button (html/attr "type" "submit") "Go")))))))

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

;; Full panel body: forms + reading (swapped by htmx into #bible-panel).
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
                      (c (bible/safe-chapter
                            (if (null? book) "John" book)
                            (if (null? chapter) "3" chapter)))
                      (v (if (null? verse) () verse)))
                  (html/join
                    (list
                      (bible/go-form b c v)
                      (bible/search-form ())
                      (bible/chapter-panel b c v)
                      (html/p*
                        (html/attr "class" "muted")
                        (html/join
                          (list
                            "Berean Standard Bible · "
                            (bible/ref b c (if (null? v) 1 v))
                            " · "
                            (html/a "/partials/bible-chapter" "raw partial"))))))))
              (let ((row (bible/parse-ref ref)))
                (if (null? row)
                    (html/join
                      (list
                        (bible/go-form "John" "3" ())
                        (html/p*
                          (html/attr "class" "err")
                          (string-append "Unknown ref " (as-str ref)))))
                    (let ((book (dict-get row "book"))
                          (chapter (dict-get row "chapter"))
                          (verse (dict-get row "verse")))
                      (html/join
                        (list
                          (bible/go-form book chapter verse)
                          (bible/search-form ())
                          (bible/chapter-panel book chapter verse)
                          (html/p*
                            (html/attr "class" "muted")
                            (string-append
                              "ref "
                              (as-str ref)
                              " · Berean Standard Bible"))))))))
          (html/join
            (list
              (bible/go-form "John" "3" ())
              (bible/search-form q)
              (bible/search-panel q)))))))

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
        (if (hx-request?)
            (bible/panel-for-request)
            (html/page
              (list
                (html/h1 "Bible")
                (html/div (html/attr "id" "bible-panel")
                  (bible/panel-for-request))))))))

(route "/bible" "bible/page")
(route "/partials/bible-chapter" "bible/partial")
