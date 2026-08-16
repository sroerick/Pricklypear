;; package dabar -- exported from live branch system/main
;; Sync: scripts/pp_sync.py pull|push
;;
;; Study layer on top of bible/*: Strong's-linked interlinear + /dabar chrome.
;; Interlinear rows: type bible-interlinear (seed: scripts/seed_interlinear.py).
;; Toggle: request param inter=1 shows token HTML; default plain (inter=0).
;; Chapter-reader decision (v1): chapter view stays bible-owned; each study
;; path is /dabar?book=&chapter=&verse=&inter=1 (no inline chapter tokens).

(define dabar/inter-html
  (lambda
    (book chapter verse)
    "Render interlinear token HTML for a verse, or a muted no-interlinear notice."
    (let
      ((row (dabar/inter-row book chapter verse)))
      (if
        (null? row)
        (html/p*
          (html/attr "class" "muted")
          (string-append
            "No interlinear for "
            (bible/ref book chapter verse)
            " — plain: "
            (as-str (bible/verse book chapter verse))))
        (let
          ((pre (dict-get row "html")))
          (if
            (null? pre)
            (html/p*
              (html/attr "class" "muted")
              "interlinear row has no html field")
            pre))))))

;; Canonical alias — spec smoke (dabar/inter "John" 1 1).
(define dabar/inter
  (lambda
    (book chapter verse)
    "Canonical alias: render interlinear HTML for a verse."
    (dabar/inter-html book chapter verse)))

(define dabar/inter-row
  (lambda
    (book chapter verse)
    "Fetch the interlinear row for a verse ref; () when absent."
    (find-row
      (quote bible-interlinear)
      "ref"
      (bible/ref book chapter verse))))

;; Truthy when inter=1 (default off / plain).
(define dabar/inter-on?
  (lambda
    ()
    "True when the request asks for interlinear view (inter=1)."
    (let
      ((p (request-param "inter")))
      (if
        (null? p)
        #f
        (string-eq (as-str p) "1")))))

(define dabar/study-href
  (lambda
    (book chapter verse inter-on)
    "Build a /dabar study URL for a verse with the given inter flag."
    (string-append
      "/dabar?book="
      (bible/abbrev-of book)
      "&chapter="
      (as-str chapter)
      "&verse="
      (as-str verse)
      "&inter="
      (if inter-on "1" "0"))))

(define dabar/toggle-link
  (lambda
    (book chapter verse inter-on)
    "Render the plain/interlinear toggle link for a verse."
    (let
      ((next (not inter-on))
        (label (if inter-on "plain" "interlinear")))
      (html/tag
        "a"
        (html/attrs
          (list
            (html/attr "href" (dabar/study-href book chapter verse next))
            (html/attr "hx-get" (dabar/study-href book chapter verse next))
            (html/attr "hx-target" "#dabar-panel")
            (html/attr "hx-swap" "innerHTML")
            (html/attr "hx-push-url" "true")
            (html/attr "class" "dabar-inter-toggle")))
        label))))

(define dabar/page
  (lambda
    ()
    "Route handler for /dabar: redirect POSTs, render panel otherwise."
    (if
      (string-eq (request-method) "POST")
      (let
        ((book (request-param "book"))
          (chapter (request-param "chapter"))
          (verse (request-param "verse"))
          (inter (request-param "inter")))
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
                (string-append "&verse=" (as-str verse))))
            (if
              (null? inter)
              ""
              (if
                (string-eq (as-str inter) "")
                ""
                (string-append "&inter=" (as-str inter)))))))
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
    "Render the dabar study panel for the requested book/chapter/verse or ref."
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
    "Render the panel from request params, falling back to the chapter reader."
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
    "Render the full verse study view (BSB text plus interlinear toggle)."
    (let
      ((ab (bible/abbrev-of book))
        (ch (bible/safe-chapter book chapter))
        (inter-on (dabar/inter-on?)))
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
                (html/h2
                  (html/join
                    (list
                      "Interlinear · "
                      (dabar/toggle-link book ch verse inter-on))))
                (if
                  inter-on
                  (dabar/inter-html book ch verse)
                  (html/p*
                    (html/attr "class" "muted")
                    (html/join
                      (list
                        "Plain view. "
                        (dabar/toggle-link book ch verse #f)
                        " for original-language tokens + Strong's.")))))))
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
                " · "
                (html/a "/strongs" "Strong's")))))))))

(define dabar/verse-study-from-params
  (lambda
    (book chapter verse)
    "Render a verse study from raw book/chapter/verse params."
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
    "Render a verse study from a parsed bible ref, or an error form."
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

(route "/dabar" "dabar/page")
