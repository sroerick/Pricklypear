;; package bible -- exported from live branch system/main
;; Sync: scripts/pp_sync.py pull|push

(define bible/abbrev-of
  (lambda
    (book)
    (let
      ((row (bible/book-row book)))
      (if (null? row) (as-str book) (dict-get row "abbrev")))))

(define bible/book-by-ordinal
  (lambda
    (ord)
    (list/foldl
      (lambda
        (acc row)
        (let
          ((f (cdr row)))
          (if
            (string-eq (as-str (dict-get f "ordinal")) (as-str ord))
            f
            acc)))
      ()
      (bible/books))))

(define bible/book-options
  (lambda
    (current)
    (list/foldl
      (lambda
        (acc row)
        (let
          ((f (cdr row)))
          (let
            ((name (dict-get f "name"))
              (abbrev (dict-get f "abbrev")))
            (string-append
              acc
              (html/option
                (as-str abbrev)
                (as-str name)
                (if
                  (string-eq (as-str current) (as-str abbrev))
                  #t
                  (string-eq (as-str current) (as-str name))))))))
      ""
      (bible/books))))

(define bible/book-ordinal
  (lambda
    (book)
    (let
      ((row (bible/book-row book)))
      (if
        (null? row)
        0
        (bible/parse-int (dict-get row "ordinal"))))))

(define bible/book-row
  (lambda
    (book)
    (let
      ((b (as-str book)))
      (let
        ((by-ab (find-row (quote bible-book) "abbrev" b)))
        (if
          (null? by-ab)
          (find-row (quote bible-book) "name" b)
          by-ab)))))

(define bible/books
  (lambda () (rows (quote bible-book) 100)))

(define bible/chapter
  (lambda
    (book chapter)
    (find-rows
      (quote bible-verse)
      "abbrev"
      (bible/abbrev-of book)
      "chapter"
      (as-str chapter)
      200)))

(define bible/chapter-count
  (lambda
    (abbrev)
    (list/foldl
      (lambda
        (acc pair)
        (if
          (string-eq (car pair) (as-str abbrev))
          (car (cdr pair))
          acc))
      1
      bible/chapter-counts)))

(define bible/chapter-counts
  (list
    (list "Gen" 50)
    (list "Exod" 40)
    (list "Lev" 27)
    (list "Num" 36)
    (list "Deut" 34)
    (list "Josh" 24)
    (list "Judg" 21)
    (list "Ruth" 4)
    (list "1Sam" 31)
    (list "2Sam" 24)
    (list "1Kgs" 22)
    (list "2Kgs" 25)
    (list "1Chr" 29)
    (list "2Chr" 36)
    (list "Ezra" 10)
    (list "Neh" 13)
    (list "Esth" 10)
    (list "Job" 42)
    (list "Ps" 150)
    (list "Prov" 31)
    (list "Eccl" 12)
    (list "Song" 8)
    (list "Isa" 66)
    (list "Jer" 52)
    (list "Lam" 5)
    (list "Ezek" 48)
    (list "Dan" 12)
    (list "Hos" 14)
    (list "Joel" 3)
    (list "Amos" 9)
    (list "Obad" 1)
    (list "Jonah" 4)
    (list "Mic" 7)
    (list "Nah" 3)
    (list "Hab" 3)
    (list "Zeph" 3)
    (list "Hag" 2)
    (list "Zech" 14)
    (list "Mal" 4)
    (list "Matt" 28)
    (list "Mark" 16)
    (list "Luke" 24)
    (list "John" 21)
    (list "Acts" 28)
    (list "Rom" 16)
    (list "1Cor" 16)
    (list "2Cor" 13)
    (list "Gal" 6)
    (list "Eph" 6)
    (list "Phil" 4)
    (list "Col" 4)
    (list "1Thess" 5)
    (list "2Thess" 3)
    (list "1Tim" 6)
    (list "2Tim" 4)
    (list "Titus" 3)
    (list "Phlm" 1)
    (list "Heb" 13)
    (list "Jas" 5)
    (list "1Pet" 5)
    (list "2Pet" 3)
    (list "1John" 5)
    (list "2John" 1)
    (list "3John" 1)
    (list "Jude" 1)
    (list "Rev" 22)))

(define bible/chapter-html
  (lambda
    (book chapter focus-verse)
    (bible/chapter-panel book chapter focus-verse)))

(define bible/chapter-panel
  (lambda
    (book chapter focus-verse)
    (let
      ((rows (bible/chapter book chapter))
        (ab (bible/abbrev-of book))
        (ch (bible/safe-chapter book chapter)))
      (html/join
        (list
          (html/h1
            (string-append (as-str book) " " (as-str chapter)))
          (bible/nav-links ab ch)
          (html/tag
            "ol"
            (html/attrs
              (list
                (html/attr "class" "bible-chapter")
                (html/attr
                  "style"
                  "list-style:none;padding-left:0;margin:.25rem 0")))
            (list/foldl
              (lambda
                (acc row)
                (string-append acc (bible/verse-li row)))
              ""
              rows))
          (bible/nav-links ab ch)
          (if
            (null? focus-verse)
            ""
            (html/p*
              (html/attr "class" "muted")
              (string-append
                "Focus verse "
                (as-str focus-verse)
                ": "
                (as-str (bible/verse book chapter focus-verse))))))))))

(define bible/chapter-reader
  (lambda
    ()
    (let
      ((ref (request-param "ref"))
        (q (request-param "q"))
        (rnd (request-param "random")))
      (if
        (null? rnd)
        (if
          (null? q)
          (if
            (null? ref)
            (let
              ((book (request-param "book"))
                (chapter (request-param "chapter"))
                (verse (request-param "verse")))
              (let
                ((b (if (null? book) "John" (as-str book)))
                  (c
                    (bible/safe-chapter
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
                          (html/a "/strongs" "Strong's"))))))))
            (let
              ((row (bible/parse-ref ref)))
              (if
                (null? row)
                (html/join
                  (list
                    (bible/go-form "John" "3" ())
                    (html/p*
                      (html/attr "class" "err")
                      (string-append "Unknown ref " (as-str ref)))))
                (let
                  ((book (dict-get row "book"))
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
              (bible/search-panel q))))
        (let
          ((row (bible/random-row)))
          (if
            (null? row)
            (html/p* (html/attr "class" "err") "no verses seeded")
            (let
              ((f (cdr row)))
              (let
                ((book (dict-get f "abbrev"))
                  (chapter (dict-get f "chapter"))
                  (verse (dict-get f "verse")))
                (html/join
                  (list
                    (bible/go-form book chapter verse)
                    (bible/search-form ())
                    (bible/chapter-panel book chapter verse)
                    (html/p*
                      (html/attr "class" "muted")
                      (string-append
                        "random · "
                        (as-str (dict-get f "ref"))))))))))))))

(define bible/go-form
  (lambda
    (b c verse)
    (let
      ((ab (bible/abbrev-of b))
        (ch (bible/safe-chapter b c))
        (vs (if (null? verse) "" (as-str verse))))
      (html/form
        "GET"
        "/dabar"
        (html/attrs
          (list
            (html/attr "class" "card bible-go")
            (html/attr "hx-get" "/dabar")
            (html/attr "hx-target" "#dabar-panel")
            (html/attr "hx-swap" "innerHTML")
            (html/attr "hx-push-url" "true")
            (html/attr "hx-trigger" "submit, change from:select")))
        (html/join
          (list
            (html/label
              ""
              (html/join
                (list
                  "Book "
                  (html/select "book" "" (bible/book-options ab)))))
            " "
            (html/label
              ""
              (html/join
                (list
                  "Chapter "
                  (html/select
                    "chapter"
                    ""
                    (bible/option-range (bible/chapter-count ab) ch)))))
            " "
            (html/label
              ""
              (html/join
                (list
                  "Verse "
                  (html/select "verse" "" (bible/verse-options ab ch vs)))))
            " "
            (html/button (html/attr "type" "submit") "Go")))))))

(define bible/href-chapter
  (lambda
    (pair)
    (if
      (null? pair)
      "#"
      (string-append
        "/dabar?book="
        (as-str (car pair))
        "&chapter="
        (as-str (car (cdr pair)))))))

;; notes.links-and-roam / dabar.note-bible-links: sexp bible refs → /dabar
(define bible/link-href
  (lambda
    (book chapter verse)
    (let
      ((ab (bible/abbrev-of book))
        (ch (bible/safe-chapter book chapter))
        (v (if (null? verse) "" (as-str verse))))
      (if
        (string-eq v "")
        (string-append "/dabar?book=" ab "&chapter=" ch)
        (string-append
          "/dabar?book="
          ab
          "&chapter="
          ch
          "&verse="
          v)))))

(define bible/link-html
  (lambda
    (book chapter verse label)
    (let
      ((ab (bible/abbrev-of book))
        (ch (bible/safe-chapter book chapter))
        (v (if (null? verse) "" (as-str verse)))
        (lab
          (if
            (or (null? label) (string-eq (as-str label) ""))
            (if
              (or (null? verse) (string-eq (as-str verse) ""))
              (string-append (as-str book) " " (as-str chapter))
              (string-append
                (as-str book)
                " "
                (as-str chapter)
                ":"
                (as-str verse)))
            (as-str label))))
      (html/a
        (bible/link-href ab ch (if (string-eq v "") () v))
        (html/text lab)))))

(define bible/int-in-range-from?
  (lambda
    (s lo hi i)
    (if
      (> i hi)
      #f
      (if
        (string-eq (as-str s) (as-str i))
        #t
        (bible/int-in-range-from? s lo hi (+ i 1))))))

(define bible/int-in-range?
  (lambda (s lo hi) (bible/int-in-range-from? s lo hi lo)))

(define bible/nav-links
  (lambda
    (book chapter)
    (let
      ((prev (bible/prev-ref book chapter))
        (next (bible/next-ref book chapter)))
      (html/p*
        (html/attr "class" "bible-nav")
        (html/join
          (list
            (if
              (null? prev)
              (html/span
                (html/attr "class" "muted")
                "← prev")
              (html/a (bible/href-chapter prev) "← prev"))
            " · "
            (html/a "/dabar?random=1" "random")
            " · "
            (if
              (null? next)
              (html/span
                (html/attr "class" "muted")
                "next →")
              (html/a (bible/href-chapter next) "next →"))
            " · "
            (html/a
              (string-append
                "/dabar?book="
                (bible/abbrev-of book)
                "&chapter="
                (bible/safe-chapter book chapter)
                "&verse=1")
              "study")))))))

(define bible/next-ref
  (lambda
    (book chapter)
    (let
      ((ab (bible/abbrev-of book))
        (c (bible/parse-int (bible/safe-chapter book chapter)))
        (maxc (bible/chapter-count (bible/abbrev-of book)))
        (ord (bible/book-ordinal book)))
      (if
        (< c maxc)
        (list ab (as-str (+ c 1)))
        (if
          (< ord 66)
          (let
            ((nxt (bible/book-by-ordinal (+ ord 1))))
            (if (null? nxt) () (list (dict-get nxt "abbrev") "1")))
          ())))))

(define bible/option-range
  (lambda
    (n selected)
    (bible/option-range-from 1 n selected)))

(define bible/option-range-from
  (lambda
    (i n selected)
    (if
      (> i n)
      ""
      (string-append
        (html/option
          (as-str i)
          (as-str i)
          (string-eq (as-str selected) (as-str i)))
        (bible/option-range-from (+ i 1) n selected)))))

(define bible/parse-int
  (lambda (s) (bible/parse-int-from s 1 200)))

(define bible/parse-int-from
  (lambda
    (s i max)
    (if
      (> i max)
      0
      (if
        (string-eq (as-str s) (as-str i))
        i
        (bible/parse-int-from s (+ i 1) max)))))

(define bible/parse-ref
  (lambda
    (ref)
    (find-row (quote bible-verse) "ref" (as-str ref))))

(define bible/prev-ref
  (lambda
    (book chapter)
    (let
      ((ab (bible/abbrev-of book))
        (c (bible/parse-int (bible/safe-chapter book chapter)))
        (ord (bible/book-ordinal book)))
      (if
        (> c 1)
        (list ab (as-str (- c 1)))
        (if
          (> ord 1)
          (let
            ((prev (bible/book-by-ordinal (- ord 1))))
            (if
              (null? prev)
              ()
              (list
                (dict-get prev "abbrev")
                (as-str (bible/chapter-count (dict-get prev "abbrev"))))))
          ())))))

(define bible/random-row
  (lambda () (find-random (quote bible-verse))))

(define bible/ref
  (lambda
    (book chapter verse)
    (string-append
      "BSB:"
      (bible/abbrev-of book)
      ":"
      (as-str chapter)
      ":"
      (as-str verse))))

(define bible/safe-chapter
  (lambda
    (book chapter)
    (let
      ((max (bible/chapter-count (bible/abbrev-of book)))
        (c (if (null? chapter) "1" (as-str chapter))))
      (if (bible/int-in-range? c 1 max) c "1"))))

(define bible/search
  (lambda
    (q)
    (find-text (quote bible-verse) "text" (as-str q) 40)))

(define bible/search-form
  (lambda
    (q)
    (html/form
      "GET"
      "/dabar"
      (html/attrs
        (list
          (html/attr "class" "card bible-search")
          (html/attr "hx-get" "/dabar")
          (html/attr "hx-target" "#dabar-panel")
          (html/attr "hx-swap" "innerHTML")
          (html/attr "hx-push-url" "true")))
      (html/join
        (list
          (html/label
            ""
            (html/join
              (list
                "Search "
                (html/text-input
                  "q"
                  (html/attrs
                    (list
                      (html/attr "value" (if (null? q) "" (as-str q)))
                      (html/attr "placeholder" "love")))))))
          " "
          (html/button (html/attr "type" "submit") "Find"))))))

(define bible/search-panel
  (lambda
    (q)
    (let
      ((hits (bible/search q)))
      (html/join
        (list
          (html/h1 (string-append "Search: " (as-str q)))
          (html/ul
            (html/attr "class" "bible-hits")
            (list/foldl
              (lambda
                (acc row)
                (let
                  ((f (cdr row)))
                  (string-append
                    acc
                    (html/li
                      ""
                      (html/join
                        (list
                          (html/a
                            (string-append
                              "/dabar?ref="
                              (as-str (dict-get f "ref")))
                            (as-str (dict-get f "ref")))
                          " "
                          (html/text (dict-get f "text"))))))))
              ""
              hits))
          (html/p*
            (html/attr "class" "muted")
            "Berean Standard Bible"))))))

(define bible/verse
  (lambda
    (book chapter verse)
    (let
      ((row (bible/verse-row book chapter verse)))
      (if (null? row) () (dict-get row "text")))))

(define bible/verse-li
  (lambda
    (row)
    (let
      ((fields (cdr row)))
      (let
        ((n (dict-get fields "verse"))
          (text (dict-get fields "text"))
          (ref (dict-get fields "ref")))
        (html/li
          (html/attr "id" (as-str ref))
          (html/join
            (list
              (html/tag "sup" "" (html/text (as-str n)))
              " "
              (html/text text))))))))

(define bible/verse-options
  (lambda
    (book chapter selected)
    (string-append
      (html/option
        ""
        "—"
        (if
          (null? selected)
          #t
          (string-eq (as-str selected) "")))
      (list/foldl
        (lambda
          (acc row)
          (let
            ((n (dict-get (cdr row) "verse")))
            (string-append
              acc
              (html/option
                (as-str n)
                (as-str n)
                (string-eq (as-str selected) (as-str n))))))
        ""
        (bible/chapter book chapter)))))

(define bible/verse-row
  (lambda
    (book chapter verse)
    (find-row
      (quote bible-verse)
      "ref"
      (bible/ref book chapter verse))))


;; --- outline body (bible-ref ...) rendering ---

(define bible/nth
  (lambda (lst n)
    (if (= n 0) (car lst) (bible/nth (cdr lst) (- n 1))))
)

(define bible/ref-body?
  (lambda (body)
    (let
      ((s (as-str body)))
      (string-starts-with? s "(bible-ref")))
)

(define bible/ref-parse
  (lambda (body)
    (let
      ((form (read-string (as-str body))))
      (if
        (pair? form)
        (if
          (string-eq (symbol->string (car form)) "bible-ref")
          form
          ())
        ())))
)

(define bible/render-body
  (lambda (body)
    (if
      (bible/ref-body? body)
      (let
        ((form (bible/ref-parse body)))
        (if
          (null? form)
          (roam/render-body body)
          (let
            ((book (bible/nth form 1))
              (chapter (bible/nth form 2))
              (vstart (bible/nth form 3))
              (vend (bible/nth form 4))
              (mode (bible/nth form 5)))
            (bible/render-ref book chapter vstart vend mode))))
      ()))
)

(define bible/render-ref
  (lambda (book chapter vstart vend mode)
    (if
      (string-eq (as-str mode) "link")
      (bible/verse-link-html book chapter vstart vend)
      (bible/verse-inline-html book chapter vstart vend)))
)

(define bible/cite-label
  (lambda (book chapter vstart vend)
    (if
      (= vstart vend)
      (string-append
        (as-str book)
        " "
        (as-str chapter)
        ":"
        (as-str vstart))
      (string-append
        (as-str book)
        " "
        (as-str chapter)
        ":"
        (as-str vstart)
        "-"
        (as-str vend))))
)

(define bible/verse-body
  (lambda (book chapter vstart vend mode)
    (string-append
      "(bible-ref \""
      (as-str book)
      "\" "
      (as-str chapter)
      " "
      (as-str vstart)
      " "
      (as-str vend)
      " \""
      (as-str mode)
      "\")"))
)

(define bible/verse-inline-html
  (lambda (book chapter vstart vend)
    (if
      (= vstart vend)
      (bible/verse-inline-one book chapter vstart)
      (bible/verse-inline-range book chapter vstart vend)))
)

(define bible/verse-inline-one
  (lambda (book chapter verse)
    (let
      ((row (bible/verse-row book chapter verse))
        (ab (bible/abbrev-of book))
        (ch (bible/safe-chapter book chapter))
        (label (bible/cite-label book chapter verse verse)))
      (if
        (null? row)
        (html/p*
          (html/attr "class" "err")
          (string-append "Verse not found: " label))
        (html/tag
          "blockquote"
          (html/attr "class" "bible-verse")
          (html/join
            (list
              (html/tag "sup" "" (as-str verse))
              " "
              (html/text (dict-get row "text"))
              " "
              (html/p*
                (html/attr "class" "bible-cite")
                (html/a (bible/link-href ab ch verse) label))))))))
)

(define bible/verse-inline-range
  (lambda (book chapter vstart vend)
    (let
      ((rows (bible/verses-in-range book chapter vstart vend))
        (ab (bible/abbrev-of book))
        (ch (bible/safe-chapter book chapter))
        (label (bible/cite-label book chapter vstart vend)))
      (if
        (null? rows)
        (html/p*
          (html/attr "class" "err")
          (string-append "No verses found: " label))
        (html/tag
          "blockquote"
          (html/attr "class" "bible-verse")
          (html/join
            (list
              (list/foldl
                (lambda
                  (acc row)
                  (let
                    ((fields (cdr row)))
                    (string-append
                      acc
                      (html/tag "sup" "" (as-str (dict-get fields "verse")))
                      " "
                      (html/text (dict-get fields "text"))
                      " ")))
                ""
                rows)
              (html/p*
                (html/attr "class" "bible-cite")
                (html/a (bible/link-href ab ch vstart) label))))))))
)

(define bible/verse-link-html
  (lambda (book chapter vstart vend)
    (let
      ((ab (bible/abbrev-of book))
        (ch (bible/safe-chapter book chapter))
        (label (bible/cite-label book chapter vstart vend)))
      (html/a (bible/link-href ab ch vstart) label)))
)

(define bible/verses-in-range
  (lambda (book chapter vstart vend)
    (let
      ((rows (bible/chapter book chapter)))
      (list/filter
        (lambda
          (row)
          (let
            ((v (parse-int (dict-get (cdr row) "verse"))))
            (and (>= v vstart) (<= v vend))))
        rows)))
)

(define bible/insert-verse
  (lambda (book chapter vstart vend mode)
    (outline/node*
      (gensym "n")
      (bible/cite-label book chapter vstart vend)
      (bible/verse-body book chapter vstart vend mode)
      ""
      (list)))
)

