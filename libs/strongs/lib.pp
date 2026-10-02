;; package strongs -- exported from live branch system/main
;; Sync: scripts/pp_sync.py pull|push

(define strongs/description
  (lambda
    (number)
    "The description of a Strong's entry, or () when absent."
    (let
      ((row (strongs/get number)))
      (if (null? row) () (dict-get row "description")))))

(define strongs/entry-html
  (lambda
    (row)
    "Render a Strong's entry row as HTML, or a not-found message."
    (if
      (null? row)
      (html/p* (html/attr "class" "err") "not found")
      (html/div
        (html/attr "class" "card strongs-entry")
        (html/join
          (list
            (html/h2 (html/text (dict-get row "number")))
            (html/p (dict-get row "lemma"))
            (html/p*
              (html/attr "class" "muted")
              (html/text
                (string-append
                  (as-str (dict-get row "xlit"))
                  " / "
                  (as-str (dict-get row "pronounce")))))
            (html/p (dict-get row "description"))))))))

(define strongs/get
  (lambda
    (number)
    "Fetch a Strong's entry row by number, or () when absent."
    (find-row
      (quote strongs-entry)
      "number"
      (as-str number))))

(define strongs/lemma
  (lambda
    (number)
    "The lemma of a Strong's entry, or () when absent."
    (let
      ((row (strongs/get number)))
      (if (null? row) () (dict-get row "lemma")))))

(define strongs/lookup-form
  (lambda
    (n)
    "An htmx lookup form for a Strong's number."
    (html/form
      "GET"
      "/strongs"
      (html/attrs
        (list
          (html/attr "class" "card")
          (html/attr "hx-get" "/strongs")
          (html/attr "hx-target" "#strongs-panel")
          (html/attr "hx-swap" "innerHTML")
          (html/attr "hx-push-url" "true")))
      (html/join
        (list
          (html/label
            ""
            (html/join
              (list
                "Number "
                (html/text-input
                  "n"
                  (html/attrs
                    (list
                      (html/attr "value" (if (null? n) "" (as-str n)))
                      (html/attr "placeholder" "H430 or G26")))))))
          " "
          (html/button (html/attr "type" "submit") "Look up"))))))

(define strongs/page
  (lambda
    ()
    "Serve the Strong's page, htmx fragment or full page."
    (if
      (hx-request?)
      (strongs/panel)
      (html/page
        (list
          (html/div
            (html/attr "id" "strongs-panel")
            (strongs/panel)))))))

(define strongs/panel
  (lambda
    ()
    "Render the Strong's lookup panel for the request number."
    (let
      ((n (request-param "n")))
      (html/join
        (list
          (html/h1 "Strong's")
          (strongs/lookup-form n)
          (if
            (null? n)
            (html/p*
              (html/attr "class" "muted")
              "Enter a Strong's number (H for Hebrew, G for Greek). Seed: scripts/seed_strongs.py")
            (strongs/entry-html (strongs/get n)))
          (html/p*
            (html/attr "class" "muted")
            (html/join
              (list
                (html/a "/dabar?book=John&chapter=3" "Bible")
                " · "
                (html/a
                  "/dabar?book=John&chapter=3&verse=16"
                  "Dabar study")))))))))
