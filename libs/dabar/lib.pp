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
          ((pre (dict-get (cdr row) "html")))
          (if
            (null? pre)
            (html/p*
              (html/attr "class" "muted")
              "interlinear row has no html field")
            pre)))))
  "Return the interlinear HTML for one verse, or a 'No interlinear for <ref>' fallback.")

;; Canonical alias — spec smoke (dabar/inter "John" 1 1).
(define dabar/inter
  (lambda
    (book chapter verse)
    (dabar/inter-html book chapter verse))  "Canonical alias for interlinear HTML; e.g. (dabar/inter book chapter verse).")

(define dabar/inter-row
  (lambda
    (book chapter verse)
    (let
      ((rows
        (find-rows
          (quote bible-interlinear)
          "ref"
          (bible/ref book chapter verse)
          1)))
      (if (null? rows) () (car rows))))
  "Fetch one bible-interlinear row as (id . fields) pair, or () if missing. Matches find-rows shape.")

;; Truthy when inter=1 (default off / plain).
(define dabar/inter-on?
  (lambda
    ()
    (let
      ((p (request-param "inter")))
      (if
        (null? p)
        #f
        (string-eq (as-str p) "1"))))  "Truthy when the inter=1 request param is set (plain view is the default).")

(define dabar/study-href
  (lambda
    (book chapter verse inter-on)
    (string-append
      "/dabar?book="
      (bible/abbrev-of book)
      "&chapter="
      (as-str chapter)
      "&verse="
      (as-str verse)
      "&inter="
      (if inter-on "1" "0")))  "/dabar URL for a verse study, including an intercross-toggling inter= param.")

(define dabar/toggle-link
  (lambda
    (book chapter verse inter-on)
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
        label)))  "Interlinear<->plain toggle link targeting #dabar-panel.")

(define dabar/page
  (lambda
    ()
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
              (dabar/panel-for-request)))))))  "/dabar route handler: redirects POSTs, else renders the panel (htmx request or full page).")

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
              (dict-get row "verse")))))))  "Resolve request params (book/chapter/verse or ref=) into a verse-study panel.")

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
            (bible/chapter-reader))))))  "Dispatch a request by ref=, verse=, or fall back to the chapter reader.")

(define dabar/verse-study
  (lambda
    (book chapter verse)
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
                (html/a "/strongs" "Strong's"))))))))  "Full verse-study panel: heading, BSB card, interlinear card, and reader/Strong's links.")

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
        (bible/parse-int v))))  "Render verse-study from request book/chapter/verse params, applying defaults.")

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
              (dabar/verse-study book chapter verse)))))))  "Render verse-study from a plain ref string, or an unknown-ref error with a go-form.")

(route "/dabar" "dabar/page")

;; ---- daily-scripture (borge/dabar.borg daily-scripture) ------------
;; Verse-of-the-day, pushed to yourself: deterministic day-indexed pick,
;; delivered into the calendar's daily note and pinged via ntfy.
;; Requires: bible/*, notes/doc/*, outline/*, ntfy/* (seeded libs).

(define dabar/day-index
  (lambda
    ()
    "Days since 1970-01-01 in image-local time — the daily cycle."
    (date-diff-days "1970-01-01" (date-today))))

(define dabar/daily-ref
  (lambda
    ()
    "Verse-of-the-day as (abbrev chapter verse). Deterministic and
stateless: book = day mod 66 (canonical ordinal), chapter = day within
the book's chapters, verse = day within the chapter's verse count.
Same verse for every user on the same day; survives re-fire; cycles
annually-ish. bible/random-row stays the Surprise control."
    (let
      ((n (dabar/day-index)))
      (let
        ((book-fields (bible/book-by-ordinal (+ 1 (mod n 66)))))
        (let
          ((book
            (if
              (null? book-fields)
              (error "dabar/daily-ref: bible corpus empty — run scripts/seed_bible_bsb.py")
              (as-str (dict-get book-fields "abbrev")))))
          (let
            ((c (+ 1 (mod (quotient n 66) (bible/chapter-count book)))))
            (let
              ((vs (bible/chapter book c)))
              (let
                ((vc (count vs)))
                (list
                  book
                  c
                  (if (<= vc 0) 1 (+ 1 (mod n vc))))))))))))

(define dabar/starts-with?
  (lambda
    (s prefix)
    (if
      (< (string-length s) (string-length prefix))
      #f
      (string-eq (substring s 0 (string-length prefix)) prefix))))

(define dabar/daily-has?
  (lambda
    (tree marker)
    "True when a root-level kid body already starts with marker (today's ref was delivered — idempotency for job re-fires)."
    (dabar/daily-has-loop (outline/kids tree) marker)))

(define dabar/daily-has-loop
  (lambda
    (kids marker)
    (if
      (null? kids)
      #f
      (if
        (dabar/starts-with? (outline/body (car kids)) marker)
        #t
        (dabar/daily-has-loop (cdr kids) marker)))))

(define dabar/daily-click
  (lambda
    (book c v)
    "Absolute study URL for the ntfy Click: header (empty when no site configured)."
    (let
      ((cfg (guard (lambda (e) ()) (ntfy/config))))
      (let
        ((site-f (if (null? cfg) () (dict-get cfg "site"))))
        (let
          ((site (if (null? site-f) "" (as-str site-f))))
          (if
            (string-eq site "")
            ""
            (string-append
              site
              "/dabar?book="
              (as-str book)
              "&chapter="
              (as-str c)
              "&verse="
              (as-str v))))))))

(define dabar/daily-deliver
  (lambda
    ()
    "Verse-of-the-day -> append to the daily note (daily-YYYY-MM-DD,
the calendar package's slug convention) as a (bible …) ref node, then
ntfy-push with a Click: straight to the dabar study view. Idempotent
per day; returns a short status string."
    (let
      ((r (dabar/daily-ref)))
      (let
        ((book (car r))
          (c (cadr r))
          (v (caddr r)))
        (let
          ((ref-str
            (string-append
              (as-str book) " " (as-str c) ":" (as-str v)))
            (marker
              (string-append "(bible " (as-str book) " " (as-str c) " " (as-str v) ")"))
            (text (as-str (bible/verse book c v)))
            (day (date-today)))
          (let
            ((slug (string-append "daily-" day)))
            (let
              ((prev (notes/doc/get slug)))
              (let
                ((tree
                  (if
                    (null? prev)
                    (notes/doc/empty-outline day)
                    (notes/doc/outline prev))))
                (if
                  (dabar/daily-has? tree marker)
                  (string-append "already: " ref-str)
                  (let
                    ((node
                      (outline/set-body
                        (outline/node
                          (string-append "dv-" (as-str (now)))
                          (string-append "Daily scripture — " ref-str))
                        (string-append marker "\n\n" text "\n"))))
                    (let
                      ((new-tree (outline/add-child-by-id tree "root" node)))
                      (let
                        ((title (string-append "Daily scripture — " ref-str)))
                        (let
                          ((payload
                            (guard
                              (lambda (e) (str e))
                              (ntfy/send/click title text (dabar/daily-click book c v)))))
                          (seq
                            (notes/doc/upsert slug day new-tree "daily" "note" day)
                            (string-append
                              "delivered: "
                              ref-str
                              (if (string-eq (as-str payload) "")
                                  ""
                                  (string-append " (ntfy: " (as-str payload) ")")))))))))))))))))
