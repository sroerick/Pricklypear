;; package bible -- exported from live branch system/main
;; Sync: scripts/pp_sync.py pull|push

(define bible/abbrev-of
  (lambda
    (book)
    (let
      ((row (bible/book-row book)))
      (if (null? row) (as-str book) (dict-get row "abbrev"))))  "Canonical BSB abbreviation for a book (accepts a full name or an abbrev).")

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
      (bible/books)))  "Look up a bible-book row by its canonical ordinal (1-66); () if out of range.")

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
      (bible/books)))  "HTML <option> list of all books, with `current` selected (empty string selects none).")

(define bible/book-ordinal
  (lambda
    (book)
    (let
      ((row (bible/book-row book)))
      (if
        (null? row)
        0
        (bible/parse-int (dict-get row "ordinal")))))  "Canonical 1-66 ordinal of a book; 0 if the book is unknown.")

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
          by-ab))))  "Look up a bible-book row by name or abbreviation; () if not found.")

(define bible/books
  (lambda () (rows (quote bible-book) 100))  "All bible-book rows as (id . fields) pairs, one per book.")

(define bible/chapter
  (lambda
    (book chapter)
    (find-rows
      (quote bible-verse)
      "abbrev"
      (bible/abbrev-of book)
      "chapter"
      (as-str chapter)
      200
      "COALESCE((fields->>'verse')::int, 0) ASC, created_at ASC"))  "Return a chapter's bible-verse rows as (id . fields) pairs, ordered by verse number.")

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
      bible/chapter-counts))  "Number of chapters a book contains; 0 if the book is unknown.")

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
    (list "Rev" 22))  "Map of abbrev -> chapter count for every book.")

(define bible/chapter-html
  (lambda
    (book chapter focus-verse)
    (bible/chapter-panel book chapter focus-verse))  "Chapter view HTML focusing a verse; alias of chapter-panel.")

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
                (as-str (bible/verse book chapter focus-verse)))))))))  "Render a chapter: title, nav links, ordered verse <li>s (each verse number links to its study), focus note.")

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
                        (as-str (dict-get f "ref")))))))))))))  "Request handler for the /dabar chapter reader (ref, q, or random params). Returns an HTML panel.")

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
            (html/button (html/attr "type" "submit") "Go"))))))  "Book/chapter/verse jump form HTML targeting #dabar-panel.")

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
        (as-str (car (cdr pair))))))  "/dabar URL for a (book-abbrev chapter) pair from prev-ref/next-ref; '#' when the pair is null.")

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
          v))))  "/dabar URL for a book (optionally with chapter and verse).")

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
        (html/text lab))))  "<a> element linking to a verse or chapter; derives a label from book chapter(:verse) when none is given.")

(define bible/int-in-range-from?
  (lambda
    (s lo hi i)
    (if
      (> i hi)
      #f
      (if
        (string-eq (as-str s) (as-str i))
        #t
        (bible/int-in-range-from? s lo hi (+ i 1)))))  "Worker for int-in-range?: true if int-string s equals a value in [lo,hi], scanning i upward.")

(define bible/int-in-range?
  (lambda (s lo hi) (bible/int-in-range-from? s lo hi lo))  "True if int-string s equals an integer in [lo, hi].")

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
              "study"))))))  "Prev/next/random/study chapter navigation links HTML for a book and chapter.")

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
          ()))))  "(book-abbrev next-chapter) pair after a chapter, crossing into the next book; () at the last book.")

(define bible/option-range
  (lambda
    (n selected)
    (bible/option-range-from 1 n selected))  "HTML <option>s for chapter numbers 1..n, with `selected` highlighted.")

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
        (bible/option-range-from (+ i 1) n selected))))  "Worker for option-range: emit options i..n, selected highlighted.")

(define bible/parse-int
  (lambda (s) (bible/parse-int-from s 1 200))  "Parse an int-string in the default 1..200 range; 0 when not a valid number.")

(define bible/parse-int-from
  (lambda
    (s i max)
    (if
      (> i max)
      0
      (if
        (string-eq (as-str s) (as-str i))
        i
        (bible/parse-int-from s (+ i 1) max))))  "Worker for parse-int: 1 if s equals i scanning 1..max, else 0.")

(define bible/parse-ref
  (lambda
    (ref)
    (find-row (quote bible-verse) "ref" (as-str ref)))  "Parse a BSB ref string into a bible-verse fields JSON, or () if unknown.")

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
          ()))))  "(book-abbrev prev-chapter) pair before a chapter, crossing into the previous book; () at the first book.")

(define bible/random-row
  (lambda () (find-random (quote bible-verse)))  "A random bible-verse row as (id . fields), or () if none.")

(define bible/ref
  (lambda
    (book chapter verse)
    (string-append
      "BSB:"
      (bible/abbrev-of book)
      ":"
      (as-str chapter)
      ":"
      (as-str verse)))  "Canonical BSB ref string, e.g. 'BSB:John:3:16'.")

(define bible/safe-chapter
  (lambda
    (book chapter)
    (let
      ((max (bible/chapter-count (bible/abbrev-of book)))
        (c (if (null? chapter) "1" (as-str chapter))))
      (if (bible/int-in-range? c 1 max) c "1")))  "Clamp a chapter to a book's valid range, defaulting to 1.")

(define bible/search
  (lambda
    (q)
    (find-text (quote bible-verse) "text" (as-str q) 40))  "Find up to 40 bible-verse rows whose text contains q.")

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
          (html/button (html/attr "type" "submit") "Find")))))  "Search form HTML posting q to /dabar.")

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
            "Berean Standard Bible")))))  "Search results panel HTML for a query.")

(define bible/verse
  (lambda
    (book chapter verse)
    (let
      ((row (bible/verse-row book chapter verse)))
      (if (null? row) () (dict-get (cdr row) "text"))))
  "Return the BSB text of one verse, or () if missing.")

(define bible/verse-li
  (lambda
    (row)
    (let
      ((fields (cdr row)))
      (let
        ((n (dict-get fields "verse"))
          (text (dict-get fields "text"))
          (ref (dict-get fields "ref")))
        (let
          ((href
            (string-append
              "/dabar?book=" (as-str (dict-get fields "abbrev"))
              "&chapter=" (as-str (dict-get fields "chapter"))
              "&verse=" (as-str n))))
          (html/li
            (html/attr "id" (as-str ref))
            (html/join
              (list
                (html/tag "sup" "" (html/a href (html/text (as-str n))))
                " "
                (html/text text))))))))  "One <li> for a chapter row: superscript verse-number study link plus the text.")

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
        (bible/chapter book chapter))))  "HTML <option>s for every verse in a chapter with `selected` highlighted (plus a blank '—' entry).")

(define bible/verse-row
  (lambda
    (book chapter verse)
    (let
      ((rows
        (find-rows
          (quote bible-verse)
          "ref"
          (bible/ref book chapter verse)
          1)))
      (if (null? rows) () (car rows))))
  "Fetch one bible-verse row as (id . fields) pair, or () if not found. Matches find-rows shape; composes with bible/verse-li.")


;; --- outline body (bible-ref ...) rendering ---

(define bible/nth
  (lambda (lst n)
    (if (= n 0) (car lst) (bible/nth (cdr lst) (- n 1))))
  "Nth element of a list (0-indexed).")

(define bible/ref-body?
  (lambda (body)
    (let
      ((s (as-str body)))
      (string-starts-with? s "(bible-ref")))
  "True if an outline body string starts with '(bible-ref'.")

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
  "Parse a (bible-ref B ch vstart vend MODE) body string into its list form, or ().")

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
  "Render an outline body string: bible-ref forms render as link/inline HTML, otherwise roam/render-body.")

(define bible/render-ref
  (lambda (book chapter vstart vend mode)
    (if
      (string-eq (as-str mode) "link")
      (bible/verse-link-html book chapter vstart vend)
      (bible/verse-inline-html book chapter vstart vend)))
  "Dispatch a parsed (bible-ref ...) to link or inline HTML according to mode.")

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
  "Human citation label, e.g. 'John 3:16' or 'John 3:16-18'.")

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
  "Outline body string for a (bible-ref B ch vstart vend MODE) range.")

(define bible/verse-inline-html
  (lambda (book chapter vstart vend)
    (if
      (= vstart vend)
      (bible/verse-inline-one book chapter vstart)
      (bible/verse-inline-range book chapter vstart vend)))
  "Blockquote HTML for a single verse or a contiguous verse range.")

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
              (html/text (dict-get (cdr row) "text"))
              " "
              (html/p*
                (html/attr "class" "bible-cite")
                (html/a (bible/link-href ab ch verse) label))))))))
  "Render one verse as a blockquote with cite link, or 'Verse not found: <label>' if missing.")
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
  "Blockquote HTML rendering a contiguous verse range.")

(define bible/verse-link-html
  (lambda (book chapter vstart vend)
    (let
      ((ab (bible/abbrev-of book))
        (ch (bible/safe-chapter book chapter))
        (label (bible/cite-label book chapter vstart vend)))
      (html/a (bible/link-href ab ch vstart) label)))
  "<a> link HTML to a verse or chapter with a citation label.")

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
  "Rows of a chapter filtered to verse numbers in [vstart, vend].")

(define bible/insert-verse
  (lambda (book chapter vstart vend mode)
    (outline/node*
      (gensym "n")
      (bible/cite-label book chapter vstart vend)
      (bible/verse-body book chapter vstart vend mode)
      ""
      (list)))
  "Emit a (bible-ref ...) outline node for a verse range with the given render mode.")

