;; package streams -- the daily portion. Verses, RSS, tasks.
;; Star levels (1-5, tally "*" .. "*****"): 5 stars = every 3 days, 1 = every 60.
;; Fade steps a star DOWN; star rows are never deleted. Exposures are a dated
;; ledger (stream-seen). Deal priority: most-starred first. Exodus 16: one
;; day's gather, consumed = finished.
;; Sync: scripts/load_forms.py libs/stream/lib.pp "package stream"

(package stream
  (export
    stream/item/add
    stream/items-all
    stream/interval-for
    stream/stars-display
    stream/star-row-for
    stream/star
    stream/unstar
    stream/seen-rows
    stream/seen-count
    stream/id-in-list
    stream/served-item-ids
    stream/gather-pool
    stream/as-num0
    stream/eligible
    stream/today-key
    stream/deal-rows
    stream/todays-deals
    stream/first-n
    stream/pick-level
    stream/list-merge2
    stream/list-merge5
    stream/mark-seen
    stream/deal-today
    stream/portion
    stream/done
    stream/action-star
    stream/action-fade
    stream/action-done
    stream/card-html
    stream/portion-html
    stream/css
    stream/page)

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

(define (stream/interval-for stars)
    "Days between appearances, by star tally. More stars = more frequent."
    (if (string-eq stars "*****") "3"
      (if (string-eq stars "****") "7"
        (if (string-eq stars "***") "14"
          (if (string-eq stars "**") "30"
            "60")))))

(define (stream/stars-display stars)
    "Tally to display stars."
    (if (string-eq stars "*****") "*****"
      (if (string-eq stars "****") "****"
        (if (string-eq stars "***") "***"
          (if (string-eq stars "**") "**"
            (if (string-eq stars "*") "*"
              ""))))))

(define (stream/star-row-for item-id)
    "The star row (id . fields) for an item, or ()."
    (let ((rows (todo/tree/rows "stream-star" 500)))
      (let ((mine (list/filter
                    (lambda (id)
                      (string-eq (as-str (dict-get (dict-get rows id) "item_id"))
                                 (as-str item-id)))
                    (dict-keys rows))))
        (if (null? mine) () (cons (car mine) (dict-get rows (car mine)))))))

(define (stream/star item-id)
  (let ((existing (stream/star-row-for item-id)))
    (if (null? existing)
      (seq
        (todo/tree/create "stream-star"
          (dict-set* "{}" (list "item_id" (as-str item-id) "stars" "*" "last_seen" "")))
        "*")
      (let ((sid (car existing)) (cur (as-str (dict-get (cdr existing) "stars"))))
        (if (string-eq cur "*****")
          "*****"
          (let ((nxt (string-append cur "*")))
            (seq
              (todo/db/update-field "stream-star" sid "stars" nxt)
              (todo/db/update-field "stream-star" sid "last_seen" (date-today))
              nxt)))))))

(define (stream/unstar item-id)
  (let ((existing (stream/star-row-for item-id)))
    (if (null? existing)
      "no star"
      (let ((sid (car existing)) (cur (as-str (dict-get (cdr existing) "stars"))))
        (if (string-eq cur "")
          "already zero"
          (let ((down (if (string-eq cur "*") ""
            (if (string-eq cur "**") "*"
              (if (string-eq cur "***") "**"
                (if (string-eq cur "****") "***"
                  "****"))))))
            (seq
              (todo/db/update-field "stream-star" sid "stars" down)
              down)))))))

(define (stream/list-append lst x) (if (null? lst) (list x) (cons (car lst) (stream/list-append (cdr lst) x))))
(define (stream/reverse lst) (if (null? lst) () (stream/list-append (stream/reverse (cdr lst)) (car lst))))
(define (stream/seen-rows)
    "All exposure rows as (id . fields) pairs."
    (let ((rows (todo/tree/rows "stream-seen" 2000)))
      (list/map (lambda (id) (cons id (dict-get rows id)))
                (dict-keys rows))))

(define (stream/seen-count item-id)
    "How many times this item has appeared in a portion."
    (length (list/filter
              (lambda (p)
                (string-eq (as-str (dict-get (cdr p) "item_id")) (as-str item-id)))
              (stream/seen-rows))))

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

(define (stream/gather-pool)
    "Items neither served-flagged nor ledger-consumed."
    (let ((eaten (stream/served-item-ids)))
      (list/filter (lambda (p)
                     (if (string-eq (as-str (dict-get (cdr p) "served")) "1")
                         (string-eq "1" "0")
                         (if (stream/id-in-list (car p) eaten)
                             (string-eq "1" "0")
                             (string-eq "1" "1"))))
                   (stream/items-all))))

(define (stream/as-num0 s)
    "Interval string to number (60 for unknown/empty)."
    (if (string-eq s "3") 3
      (if (string-eq s "7") 7
        (if (string-eq s "14") 14
          (if (string-eq s "30") 30
            60)))))

(define (stream/eligible p)
    "Due for appearance: no star row, or last_seen aged past the interval."
    (let ((srow (stream/star-row-for (car p))))
      (if (null? srow)
          (string-eq "1" "1")
          (let* ((stars (as-str (dict-get (cdr srow) "stars")))
                 (seen (as-str (dict-get (cdr srow) "last_seen"))))
            (if (string-eq seen "")
                (string-eq "1" "1")
                (if (< (date-diff-days (date-today) seen)
                       (stream/as-num0 (stream/interval-for stars)))
                    (string-eq "1" "0")
                    (string-eq "1" "1")))))))

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

(define (stream/drop-n lst n)
  "Skip the first n items."
  (if (null? lst)
    ()
    (if (string-eq (as-str n) "0")
      lst
      (stream/drop-n (cdr lst) (- n 1)))))

(define (stream/first-n lst n)
    "First n pairs of lst."
    (if (null? lst)
        ()
        (if (string-eq (as-str n) "0")
            ()
            (cons (car lst) (stream/first-n (cdr lst) (- n 1))))))

(define (stream/pick-level pool stars)
    "Eligible pool items whose star tally matches exactly."
    (list/filter
      (lambda (p)
        (let ((srow (stream/star-row-for (car p))))
          (if (null? srow)
              (string-eq stars "")
              (string-eq (as-str (dict-get (cdr srow) "stars")) stars))))
      (list/filter stream/eligible pool)))

(define (stream/list-merge2 a b)
    (if (null? a)
        b
        (cons (car a) (stream/list-merge2 (cdr a) b))))

(define (stream/list-append lst x)
    "Append one item x at the end of lst."
    (if (null? lst)
        (list x)
        (cons (car lst) (stream/list-append (cdr lst) x))))
(define (stream/list-merge5 a b c d e f)
    (stream/list-merge2 a
      (stream/list-merge2 b
        (stream/list-merge2 c
          (stream/list-merge2 d
            (stream/list-merge2 e f))))))

(define (stream/mark-seen item-id)
    "Ledger one exposure + refresh last_seen."
    (let ((srow (stream/star-row-for item-id)))
      (seq (todo/tree/create "stream-seen"
             (dict-set* "{}" (list "item_id" (as-str item-id)
                                   "date" (date-today))))
           (if (null? srow)
               "seen"
               (todo/db/update-field "stream-star" (car srow) "last_seen"
                 (date-today))))))

(define (stream/dealt-item-ids)
  "Item ids that have ever been dealt."
  (list/map (lambda (p) (as-str (dict-get (cdr p) "item_id")))
            (stream/deal-rows)))

(define (stream/fresh-pool)
  "Never-dealt items: they appear once; star saves them for resurfacing."
  (let ((dealt (stream/dealt-item-ids)))
    (list/filter (lambda (p)
                   (if (stream/id-in-list (car p) dealt)
                       (string-eq "1" "0")
                       (string-eq "1" "1")))
                 (stream/gather-pool))))

(define (stream/resurfacing)
  "Starred items whose interval has elapsed - the save-and-resurface lane."
  (let ((srows (todo/tree/rows "stream-star" 500)))
    (let ((candidates
            (list/map (lambda (id) (cons id (dict-get srows id)))
                      (dict-keys srows))))
      (list/filter
        (lambda (p)
          (let ((iid (as-str (dict-get (cdr p) "item_id"))))
            (let ((item (todo/tree/get "stream-item" iid)))
              (if (null? item)
                (string-eq "1" "0")
                (stream/eligible (cons iid item))))))
        candidates))))(define (stream/deal-today)
  "Deal today's portion if today has none: starred-due first, then fresh."
  (if (null? (stream/todays-deals))
    (let ((res (stream/resurfacing)))
      (let ((res-ids (list/map car res)))
        (let ((fresh (list/filter
                       (lambda (p)
                         (if (stream/id-in-list (car p) res-ids)
                           (string-eq "1" "0")
                           (string-eq "1" "1")))
                       (stream/fresh-pool))))
          (let ((ordered (stream/list-merge2 res fresh)))
            (let ((take (min stream/daily-count (length ordered))))
              (let ((portion (stream/first-n ordered take)))
                (seq
                  (list/map (lambda (p)
                              (seq
                                (todo/tree/create "stream-deal"
                                  (dict-set* "{}" (list
                                    "date" (stream/today-key)
                                    "item_id" (as-str (car p))
                                    "served" "0")))
                                (stream/mark-seen (car p))))
                            portion)
                  (length portion))))))))
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

(define (stream/done item-id)
    "Consume today's deal for item-id (deal-ledger write)."
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

(define (stream/rows-pairs rows)
  "Normalize tree/rows output (dict when non-empty, () when empty) to pairs."
  (if (null? rows)
      ()
      (list/map (lambda (id) (cons id (dict-get rows id)))
                (dict-keys rows))))

(define (stream/star-row-for-in item-id rows)
  "Star row lookup against a pre-read star table."
  (let ((pairs (stream/rows-pairs rows)))
    (let ((mine (list/filter
                  (lambda (p)
                    (string-eq (as-str (dict-get (cdr p) "item_id"))
                               (as-str item-id)))
                  pairs)))
      (if (null? mine) () (car mine)))))

(define (stream/card-html id fields stars seens)
  (let ((srow (stream/star-row-for-in id stars)))
    (let ((tally (if (null? srow) "" (as-str (dict-get (cdr srow) "stars")))))
      (let ((seencount (length (list/filter
                                 (lambda (p)
                                   (string-eq (as-str (dict-get (cdr p) "item_id"))
                                              (as-str id)))
                                 (stream/rows-pairs seens)))))
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
            (html/div (html/attr "class" "stream-meta")
              (string-append "seen "
                             (as-str seencount)
                             " times"))
            (html/div (html/attr "class" "stream-stars")
              (stream/stars-display tally))
            (html/div (html/attr "class" "stream-actions")
              (html/a (string-append "/stream/star?id=" (as-str id)) "star")))))))))


(define stream/page-size 12)
(define (stream/page-html p) (let ((all (stream/reverse (stream/items-all))))(let ((start (* (- p 1) stream/page-size)))(let ((page (stream/first-n (stream/drop-n all start) stream/page-size)))(let ((items (todo/tree/rows "stream-item" 200)))(let ((stars (todo/tree/rows "stream-star" 500)))(let ((seens (todo/tree/rows "stream-seen" 2000)))(let ((res (if (= p 1) (stream/resurfacing) ()))) (html/join (list (if (null? res) "" (html/join (list/map (lambda (rp) (let ((iid (as-str (dict-get (cdr rp) "item_id")))) (let ((fields (dict-get items iid))) (if (null? fields) "" (stream/card-html iid fields stars seens))))) res))) (if (null? page) (html/div (html/attr "class" "card stream-end") (html/p "caught up.")) (html/join (list/map (lambda (pair) (let ((iid (as-str (car pair)))) (let ((fields (dict-get items iid))) (if (null? fields) "" (stream/card-html iid fields stars seens))))) page))) (html/div (html/attr "class" "stream-more") (html/a (string-append "/stream?p=" (as-str (+ p 1))) "more"))))))))))))

(define (stream/portion-html)
  (stream/page-html 1))

(define (stream/css)
    ".stream-card{text-align:center;max-width:34rem;margin:0 auto 14px}
.stream-kind{font:700 .72rem var(--font-mono);letter-spacing:.08em;text-transform:uppercase;color:var(--muted)}
.stream-title{font-weight:700;margin:8px 0 6px;font-size:1.08rem}
.stream-body{white-space:pre-wrap;line-height:1.7;margin:0 auto;max-width:30rem}
.stream-meta{margin-top:10px;font-size:.78rem;color:var(--muted)}
.stream-stars{font-size:1rem;letter-spacing:.2em;color:var(--ink)}
.stream-actions{margin-top:8px;font-size:.85rem}
.stream-actions a{color:var(--muted);text-decoration:none}
.stream-sep{color:var(--muted);opacity:.5}
.stream-end{text-align:center;padding:60px 0;color:var(--muted)}")

(define (stream/page)
    (html/page (list
      (html/join (list "<style>" (stream/css) "</style>"))
      (html/h1 "streams")
      (html/p* (html/attr "class" "muted")
        "The daily portion. Star what you want to keep - it comes back around.")
      (stream/portion-html)))))

(route "/stream" "stream/page")
(route "/stream/star" "stream/action-star")
(route "/stream/fade" "stream/action-fade")
(route "/stream/done" "stream/action-done")
