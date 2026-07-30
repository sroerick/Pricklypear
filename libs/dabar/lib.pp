;; package dabar -- exported from live branch system/main
;; Sync: scripts/pp_sync.py pull|push

(define dabar/inter-html
  (lambda
    (book chapter verse)
    (let
      ((row (dabar/inter-row book chapter verse)))
      (if
        (null? row)
        (html/p*
          (html/attr "class" "muted")
          (string-append
            "No interlinear for "
            (bible/ref book chapter verse)
            " \226\128\148 plain: "
            (as-str (bible/verse book chapter verse))))
        (let
          ((pre (dict-get row "html")))
          (if
            (null? pre)
            (html/p*
              (html/attr "class" "muted")
              "interlinear row has no html field")
            pre))))))

(define dabar/inter-row
  (lambda
    (book chapter verse)
    (find-row
      (quote bible-interlinear)
      "ref"
      (bible/ref book chapter verse))))

(define dabar/page
  (lambda
    ()
    (if
      (string-eq (request-method) "POST")
      (let
        ((book (request-param "book"))
          (chapter (request-param "chapter"))
          (verse (request-param "verse")))
        (redirect
          (string-append
            "/dabar?book="
            (if (null? book) "John" (as-str book))
            "&chapter="
            (if (null? chapter) "3" (as-str chapter))
            (if
              (null? verse)
              ""
              (if
                (string-eq (as-str verse) "")
                ""
                (string-append "&verse=" (as-str verse)))))))
      (if
        (hx-request?)
        (dabar/panel-for-request)
        (html/page
          (list
            (html/div
              (html/attr "id" "dabar-panel")
              (dabar/panel-for-request))))))))

(define dabar/panel
  (lambda
    ()
    (let
      ((book (request-param "book"))
        (chapter (request-param "chapter"))
        (verse (request-param "verse"))
        (ref (request-param "ref")))
      (if
        (null? ref)
        (let
          ((b (if (null? book) "John" (as-str book)))
            (c (if (null? chapter) "3" (as-str chapter)))
            (v (if (null? verse) "16" (as-str verse))))
          (dabar/verse-study b c v))
        (let
          ((row (bible/parse-ref ref)))
          (if
            (null? row)
            (html/p*
              (html/attr "class" "err")
              (string-append "unknown ref " (as-str ref)))
            (dabar/verse-study
              (dict-get row "abbrev")
              (dict-get row "chapter")
              (dict-get row "verse"))))))))

(define dabar/panel-for-request
  (lambda
    ()
    (let
      ((ref (request-param "ref"))
        (verse (request-param "verse")))
      (let
        ((has-ref
            (if (null? ref) #f (not (string-eq (as-str ref) ""))))
          (has-verse
            (if
              (null? verse)
              #f
              (not (string-eq (as-str verse) "")))))
        (if
          has-ref
          (dabar/verse-study-from-ref ref)
          (if
            has-verse
            (dabar/verse-study-from-params
              (request-param "book")
              (request-param "chapter")
              verse)
            (bible/chapter-reader)))))))

(define dabar/verse-study
  (lambda
    (book chapter verse)
    (let
      ((ab (bible/abbrev-of book))
        (ch (bible/safe-chapter book chapter)))
      (html/join
        (list
          (html/h1
            (string-append
              (as-str book)
              " "
              (as-str ch)
              ":"
              (as-str verse)))
          (bible/nav-links ab ch)
          (html/div
            (html/attr "class" "card")
            (html/join
              (list
                (html/h2 "BSB")
                (html/p (bible/verse book ch verse)))))
          (html/div
            (html/attr "class" "card dabar-inter")
            (html/join
              (list
                (html/h2 "Interlinear")
                (dabar/inter-html book ch verse))))
          (html/p*
            (html/attr "class" "muted")
            (html/join
              (list
                (html/a
                  (string-append
                    "/dabar?book="
                    (bible/abbrev-of book)
                    "&chapter="
                    (as-str ch))
                  "open in reader")
                " \194\183 "
                (html/a "/strongs" "Strong's")))))))))

(define dabar/verse-study-from-params
  (lambda
    (book chapter verse)
    (let
      ((b (if (null? book) "John" (as-str book)))
        (c (if (null? chapter) "3" (as-str chapter)))
        (v (as-str verse)))
      (dabar/verse-study
        b
        (bible/safe-chapter b c)
        (bible/parse-int v)))))

(define dabar/verse-study-from-ref
  (lambda
    (ref)
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
              (dabar/verse-study book chapter verse))))))))

