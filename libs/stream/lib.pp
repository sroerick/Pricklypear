;; package stream -- manna: the daily portion. Verses, RSS, tasks; star = SRS.
;; Exodus 16 rule: one day's gather. deal-today deals stream/daily-count unserved
;; items into a stream-deal ledger for the date; portion = today's deals not yet
;; served; when the portion is consumed the day is finished. Star = keep
;; (2/6/15/35/70 days), fade = let go, done = served.
;; Sync: scripts/load_forms.py libs/stream/lib.pp "package stream"  (PP_URL+creds in env)
;; Routes: /stream (page), /stream/star|fade|done (action handlers; need SHIP to bind).

(package stream
  (export
    stream/item/add
    stream/items-all
    stream/items-today
    stream/star
    stream/unstar
    stream/done
    stream/deal-today
    stream/todays-deals
    stream/portion
    stream/portion-html
    stream/action-star
    stream/action-fade
    stream/action-done
    stream/card-html
    stream/css
    stream/page)

  (deftype 'stream-item 'kind 'source 'title 'ref 'body 'served)
  (deftype 'stream-star 'item_id 'interval 'reviews 'due)
  (deftype 'stream-deal 'date 'item_id 'served)

  (define stream/daily-count 5)

  ;; ---------- data ----------

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

  (define (stream/id-in-list id lst)
    (if (null? lst)
        (string-eq "1" "0")
        (if (string-eq (as-str id) (as-str (car lst)))
            (string-eq "1" "1")
            (stream/id-in-list id (cdr lst)))))

  (define (stream/served-item-ids)
    "Item ids consumed via the deal ledger."
    (list/map (lambda (p) (as-str (dict-get (cdr p) "item_id")))
              (list/filter (lambda (p)
                             (string-eq (as-str (dict-get (cdr p) "served")) "1"))
                           (stream/deal-rows))))

  (define (stream/items-today)
    "The gather pool: items neither served-flagged nor ledger-consumed."
    (let ((eaten (stream/served-item-ids)))
      (list/filter (lambda (p)
                     (if (string-eq (as-str (dict-get (cdr p) "served")) "1")
                         (string-eq "1" "0")
                         (if (stream/id-in-list (car p) eaten)
                             (string-eq "1" "0")
                             (string-eq "1" "1"))))
                   (stream/items-all))))

  ;; ---------- daily portion (Exodus 16) ----------

  (define (stream/today-key)
    (date-today))

  (define (stream/deal-rows)
    "All deal rows as (id . fields) pairs."
    (let ((rows (todo/tree/rows "stream-deal" 500)))
      (list/map (lambda (id) (cons id (dict-get rows id)))
                (dict-keys rows))))

  (define (stream/todays-deals)
    "Deal rows dated today."
    (let ((today (stream/today-key)))
      (list/filter (lambda (p)
                     (string-eq (as-str (dict-get (cdr p) "date")) today))
                   (stream/deal-rows))))

  (define (stream/first-n lst n)
    "First n pairs of lst (n as number; 2-arg minus only)."
    (if (null? lst)
        ()
        (if (string-eq (as-str n) "0")
            ()
            (cons (car lst) (stream/first-n (cdr lst) (- n 1))))))

  (define (stream/deal-today)
    "Deal today's portion if today has none. Returns the dealt count."
    (if (null? (stream/todays-deals))
        (let ((pool (stream/items-today)))
          (let ((take (min stream/daily-count (length pool))))
            (let ((portion (stream/first-n pool take)))
              (seq
                (list/map (lambda (p)
                            (todo/tree/create "stream-deal"
                              (dict-set* "{}" (list
                                "date" (stream/today-key)
                                "item_id" (as-str (car p))
                                "served" "0"))))
                          portion)
                (length portion)))))
        (length (stream/todays-deals))))

  (define (stream/portion)
    "Today's dealt, unconsumed items as (id . fields) pairs."
    (list/map
      (lambda (p)
        (let ((iid (as-str (dict-get (cdr p) "item_id"))))
          (cons iid (todo/tree/get "stream-item" iid))))
      (list/filter
        (lambda (p)
          (if (string-eq (as-str (dict-get (cdr p) "served")) "1")
              (string-eq "1" "0")
              (string-eq "1" "1")))
        (stream/todays-deals))))

  ;; ---------- SRS ----------

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
                                       "interval" "2" "reviews" "1"
                                       "due" (date-add-days (date-today) "2"))))
               "2")
          (let* ((sid (car existing))
                 (cur (as-str (dict-get (cdr existing) "interval")))
                 (nxt (stream/next-interval cur)))
            (seq (todo/db/update-field "stream-star" sid "interval" nxt)
                 (todo/db/update-field "stream-star" sid "due"
                   (date-add-days (date-today) nxt))
                 nxt)))))

  (define (stream/unstar item-id)
    "Let it fade: delete the star row. Grace in the data model."
    (let ((existing (stream/star-row-for item-id)))
      (if (null? existing)
          "no star"
          (seq (todo/tree/del "stream-star" (car existing)) "faded"))))

  (define (stream/done item-id)
    "Consume today's deal for item-id (deal-ledger write; stream-item
     update-field is unreliable under request context)."
    (let ((today (stream/today-key)))
      (let ((deal (list/filter
                    (lambda (p)
                      (if (string-eq (as-str (dict-get (cdr p) "date")) today)
                          (if (string-eq (as-str (dict-get (cdr p) "item_id"))
                                         (as-str item-id))
                              (string-eq "1" "1")
                              (string-eq "1" "0"))
                          (string-eq "1" "0")))
                    (stream/deal-rows))))
        (if (null? deal)
            (seq (todo/tree/create "stream-deal"
                   (dict-set* "{}" (list "date" today
                                         "item_id" (as-str item-id)
                                         "served" "1")))
                 "done")
            (seq (todo/db/update-field "stream-deal" (car (car deal)) "served" "1")
                 "done")))))

  ;; ---------- request handlers ----------

  (define (stream/action-star)
    (let ((id (request-param "id")))
      (seq (if (null? id) "no-op"
             (if (string-eq (as-str id) "") "no-op"
               (stream/star id)))
           (redirect "/stream"))))

  (define (stream/action-fade)
    (let ((id (request-param "id")))
      (seq (if (null? id) "no-op"
             (if (string-eq (as-str id) "") "no-op"
               (stream/unstar id)))
           (redirect "/stream"))))

  (define (stream/action-done)
    (let ((id (request-param "id")))
      (seq (if (null? id) "no-op"
             (if (string-eq (as-str id) "") "no-op"
               (stream/done id)))
           (redirect "/stream"))))

  ;; ---------- html ----------

  (define (stream/card-html id fields)
    (html/div (html/attr "class" "card stream-card")
      (html/join (list
        (html/div (html/attr "class" "stream-kind")
          (string-append (as-str (dict-get fields "kind"))
                         " - "
                         (as-str (dict-get fields "ref"))))
        (html/div (html/attr "class" "stream-title")
          (as-str (dict-get fields "title")))
        (html/div (html/attr "class" "stream-body")
          (as-str (dict-get fields "body")))
        (html/div (html/attr "class" "stream-actions")
          (html/join (list
            (html/a (string-append "/stream/star?id=" (as-str id)) "star")
            (html/span (html/attr "class" "stream-sep") " - ")
            (html/a (string-append "/stream/fade?id=" (as-str id)) "fade")
            (html/span (html/attr "class" "stream-sep") " - ")
            (html/a (string-append "/stream/done?id=" (as-str id)) "done"))))))))

  (define (stream/portion-html)
    "Deal (idempotent) then render today's portion."
    (seq (stream/deal-today)
         (let ((items (stream/portion)))
           (if (null? items)
               (html/div (html/attr "class" "card stream-end")
                 (html/h2 "Today's portion is finished."))
               (html/join (list/map (lambda (p) (stream/card-html (car p) (cdr p)))
                                    items))))))

  (define (stream/css)
    ".stream-card{text-align:center;max-width:34rem;margin:0 auto 14px}
.stream-kind{font:700 .72rem var(--font-mono);letter-spacing:.08em;text-transform:uppercase;color:var(--muted)}
.stream-title{font-weight:700;margin:8px 0 6px;font-size:1.08rem}
.stream-body{white-space:pre-wrap;line-height:1.7;margin:0 auto;max-width:30rem}
.stream-actions{margin-top:12px;font-size:.85rem}
.stream-actions a{color:var(--muted);text-decoration:none}
.stream-sep{color:var(--muted);opacity:.5}
.stream-end{text-align:center;padding:60px 0;color:var(--muted)}")

  (define (stream/page)
    (html/page (list
      (html/join (list "<style>" (stream/css) "</style>"))
      (html/h1 "manna")
      (html/p* (html/attr "class" "muted")
        "The daily portion. Star what you want to keep; the rest fades.")
      (stream/portion-html))))
)

;; vault-declared routes (workout pattern 04ba7a1): route rows ride ships ONLY from here.
(route "/stream" "stream/page")
(route "/stream/star" "stream/action-star")
(route "/stream/fade" "stream/action-fade")
(route "/stream/done" "stream/action-done")
