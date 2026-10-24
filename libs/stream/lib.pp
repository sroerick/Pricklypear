;; package stream -- manna: the daily portion. Verses, RSS, tasks; star = SRS.
;; Exodus 16 rule: the stream ends daily. Star = keep (2/6/15/35/70 days), fade = let go.
;; Sync: scripts/load_forms.py libs/stream/lib.pp "package stream"  (PP_URL+creds in env)

(package stream
  (export
    stream/item/add
    stream/items-all
    stream/items-today
    stream/star
    stream/unstar
    stream/done
    stream/card-html
    stream/portion-html
    stream/css
    stream/page)

  (deftype 'stream-item 'kind 'source 'title 'ref 'body 'served)
  (deftype 'stream-star 'item_id 'interval 'reviews 'due)

  (define stream/intervals (list 2 6 15 35 70))

  (define (stream/item/add kind source title ref body)
    "Add a stream item (verse | rss | task). Returns the row id."
    (todo/tree/create "stream-item"
      (dict-set* "{}" (list
        "kind" (as-str kind) "source" (as-str source)
        "title" (as-str title) "ref" (as-str ref)
        "body" (as-str body) "served" "0"))))

  (define (stream/items-all)
    "All stream items as (id . fields) pairs."
    (let ((rows (todo/tree/rows "stream-item" 200)))
      (list/map (lambda (id) (cons id (dict-get rows id)))
                (dict-keys rows))))

  (define (stream/items-today)
    "The day's portion: stream items not yet served."
    (list/filter (lambda (p)
                   (string-eq (as-str (dict-get (cdr p) "served")) "0"))
                 (stream/items-all)))

  (define (stream/star-row-for item-id)
    "The star row (id . fields) for an item, or ()."
    (let ((rows (todo/tree/rows "stream-star" 500)))
      (let ((mine (list/filter
                    (lambda (id)
                      (string-eq (as-str (dict-get (dict-get rows id) "item_id"))
                                 (as-str item-id)))
                    (dict-keys rows))))
        (if (null? mine) () (cons (car mine) (dict-get rows (car mine)))))))

  (define (stream/next-interval cur)
    "SM-2 ladder: 2 -> 6 -> 15 -> 35 -> 70 (days), capped at 70."
    (if (string-eq cur "2") "6"
      (if (string-eq cur "6") "15"
        (if (string-eq cur "15") "35"
          (if (string-eq cur "35") "70" "70")))))

  (define (stream/star item-id)
    "Star an item: create its star row at interval 2, or advance the ladder."
    (let ((existing (stream/star-row-for item-id)))
      (if (null? existing)
          (seq (todo/tree/create "stream-star"
                 (dict-set* "{}" (list "item_id" (as-str item-id)
                                       "interval" "2" "reviews" "1" "due" "")))
               "2")
          (let* ((sid (car existing))
                 (cur (as-str (dict-get (cdr existing) "interval")))
                 (nxt (stream/next-interval cur)))
            (seq (todo/db/update-field "stream-star" sid "interval" nxt)
                 nxt)))))

  (define (stream/unstar item-id)
    "Let it fade: delete the star row. Grace in the data model."
    (let ((existing (stream/star-row-for item-id)))
      (if (null? existing)
          "no star"
          (seq (todo/tree/del "stream-star" (car existing)) "faded"))))

  (define (stream/done item-id)
    "Mark an item served (task consumed, verse read)."
    (seq (todo/db/update-field "stream-item" item-id "served" "1") "done"))

  (define (stream/card-html id fields)
    (html/div (html/attr "class" "stream-card")
      (html/join (list
        (html/span (html/attr "class" "stream-kind")
          (string-append (as-str (dict-get fields "kind"))
                         " - "
                         (as-str (dict-get fields "ref"))))
        (html/div (html/attr "class" "stream-title")
          (as-str (dict-get fields "title")))
        (html/div (html/attr "class" "stream-body")
          (as-str (dict-get fields "body")))))))

  (define (stream/portion-html)
    "Today's portion as HTML."
    (let ((items (stream/items-today)))
      (if (null? items)
          (html/div (html/attr "class" "stream-end")
            (html/h2 "Today's portion is finished."))
          (html/join (list/map (lambda (p) (stream/card-html (car p) (cdr p)))
                               items)))))

  (define (stream/css)
    ".stream-card{border:1px solid #d8d2c4;border-radius:8px;padding:18px;margin:0 0 14px;background:#faf8f2}
.stream-kind{font:700 .72rem monospace;letter-spacing:.08em;text-transform:uppercase;color:#8a7d5c}
.stream-title{font-weight:700;margin:6px 0 4px}
.stream-body{white-space:pre-wrap;line-height:1.5}
.stream-end{text-align:center;padding:60px 0;color:#777}")

  (define (stream/page)
    (html/page (list
      (html/h1 "manna")
      (html/p* (html/attr "class" "muted")
        "The daily portion. Star what you want to keep; the rest fades.")
      (stream/portion-html))))
)
