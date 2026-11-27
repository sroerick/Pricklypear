;; package upsets -- ingest pollers (borge/upsets.borg, subsection ingest).
;; Three Nopales jobs over the existing http-get prim, no new kernel code:
;;   (upsets/poll)       gamma /markets -> upsert pm-market rows
;;   (upsets/poll-odds)  clob /prices-history -> pm-odds (watched only)
;;   (upsets/poll-flow)  data-api /trades -> wallet-flow aggregates
;; Idempotency is structural: markets upsert on condition-id, odds points
;; are watermarked by last-odds-ts, wallet flows upsert on
;; (market wallet side hour-bucket). The promoted package copy is
;; pp/libs/upsets/lib.pp; this source is the spikes-side artifact that the
;; ingest alcotest vectors (test/) check.

(define upsets/gamma
  "https://gamma-api.polymarket.com")

(define upsets/clob
  "https://clob.polymarket.com")

(define upsets/data-api
  "https://data-api.polymarket.com")

;; caps: a tick must fit the interpreter floors.
(define upsets/market-cap
  500)

(define upsets/poll-pages
  2)

(define upsets/odds-chunk
  500)

(define upsets/flow-chunk
  500)

(define upsets/watch-max
  12)

(define upsets/flow-max
  12)

(define upsets/watch-threshold
  0.4)

(define upsets/min-trade-usd
  10)

;; schedules ship OFF; the operator turns them on with (schedule name cron fn).
(define upsets/schedules-enabled
  0)

(define upsets/schedules
  (list
    (list "upsets-poll" "*/5 * * * *" "upsets/poll")
    (list "upsets-odds" "0 * * * *" "upsets/poll-odds")
    (list "upsets-flow" "*/15 * * * *" "upsets/poll-flow"))
  "Shipped-off schedules: (name cron fn). Enabling is upsets/schedules-enabled.")

(define upsets/fetch-json
  (lambda
    (url)
    (let
      ((r (http-get url)))
      (if
        (string-eq (as-str (dict-get r "ok")) "true")
        (json-parse (dict-get r "body"))
        (error
          (string-append
            "upsets/fetch-json: "
            (as-str (dict-get r "err"))
            " "
            (as-str (dict-get r "status")))))))
  "GET url as the operator identity (the identity gate on http-get) and json-parse the body; raises on transport / non-ok, so a failed tick records a job error and leaves no partial row state.")

(define upsets/str-or
  (lambda (v d) (if (null? v) d (as-str v))))

(define upsets/num-or
  (lambda (v d) (if (null? v) d (parse-float (as-str v)))))

(define upsets/max2
  (lambda (a b) (if (> a b) a b)))

(define upsets/min2
  (lambda (a b) (if (< a b) a b)))

(define upsets/market-json
  (lambda
    (question slug cid tok-yes tok-no vol end status last-odds observed-at watched last-odds-ts last-flow-ts)
    (string-append
      "{\"question\":\""
      (json-escape question)
      "\",\"slug\":\""
      (json-escape slug)
      "\",\"condition-id\":\""
      (json-escape cid)
      "\",\"token-yes\":\""
      (json-escape tok-yes)
      "\",\"token-no\":\""
      (json-escape tok-no)
      "\",\"vol\":"
      (as-str vol)
      ",\"end\":\""
      (json-escape end)
      "\",\"status\":\""
      status
      "\",\"last-odds\":"
      (as-str last-odds)
      ",\"observed-at\":"
      (as-str observed-at)
      ",\"watched\":"
      (if watched "true" "false")
      ",\"last-odds-ts\":"
      (as-str last-odds-ts)
      ",\"last-flow-ts\":"
      (as-str last-flow-ts)
      "}"))
  "Canonical pm-market row JSON; watched is the ingest-set flag.")

(define upsets/upsert-market
  (lambda
    (m)
    (let
      ((cid (dict-get m "conditionId"))
        (toks-raw (dict-get m "clobTokenIds")))
      (if
        (or (null? cid) (null? toks-raw))
        "skip"
        (let
          ((toks (json-parse toks-raw)))
          (if
            (not (= (count toks) 2))
            "skip"
            (let
              ((question (upsets/str-or (dict-get m "question") ""))
                (slug (upsets/str-or (dict-get m "slug") ""))
                (vol (upsets/num-or (dict-get m "volumeNum") 0))
                (end (upsets/str-or (dict-get m "endDate") ""))
                (prices-raw (dict-get m "outcomePrices")))
              (let
                ((prices
                  (if (null? prices-raw) (list) (json-parse prices-raw))))
                (let
                  ((last-odds
                    (if
                      (< (count prices) 2)
                      1
                      (upsets/min2
                        (parse-float (car prices))
                        (parse-float (car (cdr prices)))))))
                  (let
                    ((watched (< last-odds upsets/watch-threshold))
                      (now-ts (now))
                      (existing
                        (find-rows
                          (quote pm-market)
                          "condition-id"
                          (as-str cid)
                          1)))
                    (if
                      (null? existing)
                      (begin
                        (create
                          (quote pm-market)
                          (upsets/market-json
                            question
                            slug
                            (as-str cid)
                            (as-str (car toks))
                            (as-str (car (cdr toks)))
                            vol
                            end
                            "open"
                            last-odds
                            now-ts
                            watched
                            0
                            0))
                        "ok")
                      (begin
                        (update
                          (quote pm-market)
                          (car (car existing))
                          (upsets/market-json
                            question
                            slug
                            (as-str cid)
                            (as-str (car toks))
                            (as-str (car (cdr toks)))
                            vol
                            end
                            "open"
                            last-odds
                            now-ts
                            watched
                            (upsets/num-or
                              (dict-get (cdr (car existing)) "last-odds-ts")
                              0)
                            (upsets/num-or
                              (dict-get (cdr (car existing)) "last-flow-ts")
                              0)))
                        "ok"))))))))))))
  "Upsert one gamma market dict; returns ok | skip. Re-running the same page writes no duplicate pm-market row.")

(define upsets/poll
  (lambda () (upsets/poll-page 0 0))
  "Discovery tick: gamma open markets (poll-pages x 100 by 24h volume) -> upsert pm-market rows; marks the watched set. Idempotent. Returns {markets}.")

(define upsets/poll-page
  (lambda
    (page seen)
    (if
      (>= page upsets/poll-pages)
      (string-append "{\"markets\":" (as-str seen) "}")
      (if
        (>= seen upsets/market-cap)
        (string-append "{\"markets\":" (as-str seen) "}")
        (let
          ((ms
            (upsets/fetch-json
              (string-append
                upsets/gamma
                "/markets?closed=false&limit=100&offset="
                (as-str (* page 100))
                "&order=volume24hr&ascending=false"))))
          (upsets/poll-loop ms page seen)))))
  "Fetch one gamma page and upsert; recurse to poll-pages / market-cap.")

(define upsets/poll-loop
  (lambda
    (ms page seen)
    (if
      (null? ms)
      (upsets/poll-page (+ page 1) seen)
      (upsets/poll-loop
        (cdr ms)
        page
        (if
          (string-eq (upsets/upsert-market (car ms)) "ok")
          (+ seen 1)
          seen)))))

(define upsets/odds-row-json
  (lambda
    (mid ts p)
    (string-append
      "{\"market-id\":\""
      (json-escape mid)
      "\",\"ts\":"
      (as-str ts)
      ",\"p\":"
      (as-str p)
      "}")))

(define upsets/odds-scan-max
  (lambda
    (pts acc)
    (if
      (null? pts)
      acc
      (upsets/odds-scan-max
        (cdr pts)
        (upsets/max2 acc (floor (parse-float (dict-get (car pts) "t"))))))))

(define upsets/odds-write
  (lambda
    (mid pts last-ts written budget)
    (if
      (or (null? pts) (<= budget 0))
      written
      (let
        ((pt (car pts)) (rest (cdr pts)))
        (let
          ((ts (floor (parse-float (dict-get pt "t"))))
            (p (parse-float (dict-get pt "p"))))
          (if
            (> ts last-ts)
            (begin
              (create (quote pm-odds) (upsets/odds-row-json mid ts p))
              (upsets/odds-write mid rest ts (+ written 1) (- budget 1)))
            (upsets/odds-write mid rest last-ts written budget))))))
  "Write pm-odds rows for points with ts > last-ts (capped by budget); the last-odds-ts watermark is what makes a re-run write nothing.")

(define upsets/poll-odds
  (lambda () (upsets/odds-loop (upsets/watched-rows) 0))
  "Odds tick: hourly YES prices for watched markets (capped per market). Idempotent via the last-odds-ts watermark.")

(define upsets/odds-loop
  (lambda
    (watched total)
    (if
      (null? watched)
      (string-append "{\"points\":" (as-str total) "}")
      (let
        ((row (car watched))
          (fields (cdr (car watched)))
          (res (upsets/poll-odds-market (car watched)))
          (n (car res))
          (max-ts (cdr res)))
        (begin
          (if
            (> n 0)
            (update
              (quote pm-market)
              (car row)
              (upsets/market-json
                (upsets/str-or (dict-get fields "question") "")
                (upsets/str-or (dict-get fields "slug") "")
                (upsets/str-or (dict-get fields "condition-id") "")
                (upsets/str-or (dict-get fields "token-yes") "")
                (upsets/str-or (dict-get fields "token-no") "")
                (upsets/num-or (dict-get fields "vol") 0)
                (upsets/str-or (dict-get fields "end") "")
                "open"
                (upsets/num-or (dict-get fields "last-odds") 1)
                (upsets/num-or (dict-get fields "observed-at") 0)
                (string-eq (as-str (dict-get fields "watched")) "true")
                max-ts
                (upsets/num-or (dict-get fields "last-flow-ts") 0)))
            ())
          (upsets/odds-loop (cdr watched) (+ total n)))))))

(define upsets/poll-odds-market
  (lambda
    (row)
    (let
      ((fields (cdr row))
        (mid (as-str (dict-get (cdr row) "condition-id")))
        (tok (as-str (dict-get (cdr row) "token-yes")))
        (last-ts
          (upsets/num-or (dict-get (cdr row) "last-odds-ts") 0)))
      (let
        ((hist
          (upsets/fetch-json
            (string-append
              upsets/clob
              "/prices-history?market="
              tok
              "&interval=max&fidelity=60"))))
        (let
          ((raw (dict-get hist "history"))
            (pts
              (if
                (null? (dict-get hist "history"))
                (list)
                (json-parse (dict-get hist "history")))))
          (if
            (null? pts)
            (cons 0 last-ts)
            (let
              ((n
                (upsets/odds-write mid pts last-ts 0 upsets/odds-chunk)))
              (cons n (upsets/odds-scan-max pts last-ts))))))))
  "Fetch hourly history for one watched market; write new points; return (written . max-ts-seen).")

(define upsets/flow-bucket
  (lambda (ts) (quotient (floor ts) 3600))
  "Hour bucket for wallet-flow aggregation.")

(define upsets/flow-agg-add
  (lambda
    (agg key usd ts bucket)
    (cond
      ((null? agg) (list (list key usd 1 bucket ts ts)))
      ((string-eq (car (car agg)) key)
        (let
          ((e (car agg)))
          (cons
            (list
              key
              (+ (car (cdr e)) usd)
              (+ (car (cdr (cdr e))) 1)
              bucket
              (car (cdr (cdr (cdr e))))
              ts)
            (cdr agg))))
      (else
        (cons (car agg) (upsets/flow-agg-add (cdr agg) key usd ts bucket))))))

(define upsets/flow-agg
  (lambda
    (trades tok-yes agg)
    (if
      (null? trades)
      agg
      (let
        ((t (car trades)) (rest (cdr trades)))
        (let
          ((side-raw (upsets/str-or (dict-get t "side") ""))
            (usd
              (*
                (upsets/num-or (dict-get t "size") 0)
                (upsets/num-or (dict-get t "price") 0))))
          (if
            (or
              (not (string-eq side-raw "BUY"))
              (< usd upsets/min-trade-usd))
            (upsets/flow-agg rest tok-yes agg)
            (let
              ((oside
                  (if
                    (string-eq
                      (upsets/str-or (dict-get t "asset") "")
                      tok-yes)
                    "YES"
                    "NO"))
                (wallet
                  (upsets/str-or
                    (dict-get t "pseudonym")
                    (upsets/str-or (dict-get t "proxyWallet") "?")))
                (ts (floor (upsets/num-or (dict-get t "timestamp") 0))))
              (upsets/flow-agg
                rest
                tok-yes
                (upsets/flow-agg-add
                  agg
                  (string-append wallet "|" oside)
                  usd
                  ts
                  (upsets/flow-bucket ts)))))))))
  "Fold BUY prints into per (wallet|side|hour-bucket) aggregates; prints are never stored raw.")

(define upsets/flow-row-json
  (lambda
    (mid e)
    (let
      ((key (car e))
        (usd (car (cdr e)))
        (n (car (cdr (cdr e))))
        (bucket (car (cdr (cdr (cdr e)))))
        (first-ts (car (cdr (cdr (cdr (cdr e))))))
        (last-ts (car (cdr (cdr (cdr (cdr (cdr e))))))))
      (string-append
        "{\"market-id\":\""
        (json-escape mid)
        "\",\"wallet\":\""
        (json-escape key)
        "\",\"usd\":"
        (as-str usd)
        ",\"n\":"
        (as-str n)
        ",\"bucket\":"
        (as-str bucket)
        ",\"first-ts\":"
        (as-str first-ts)
        ",\"last-ts\":"
        (as-str last-ts)
        "}"))))

(define upsets/flow-write
  (lambda
    (mid agg written)
    (if
      (null? agg)
      written
      (begin
        (create (quote wallet-flow) (upsets/flow-row-json mid (car agg)))
        (upsets/flow-write mid (cdr agg) (+ written 1))))))

(define upsets/flow-market
  (lambda
    (row)
    (let
      ((mid (as-str (dict-get (cdr row) "condition-id")))
        (tok (as-str (dict-get (cdr row) "token-yes"))))
      (let
        ((trades
          (upsets/fetch-json
            (string-append
              upsets/data-api
              "/trades?market="
              mid
              "&limit="
              (as-str upsets/flow-chunk)))))
        (upsets/flow-write
          mid
          (upsets/flow-agg trades tok (list))
          0)))))

(define upsets/poll-flow
  (lambda () (upsets/flow-loop (upsets/watched-rows) 0 0))
  "Flow tick: data-api trade prints for the first flow-max watched markets -> hourly wallet-flow rows. Idempotent via (market,wallet,side,bucket) upsert.")

(define upsets/flow-loop
  (lambda
    (watched total n)
    (if
      (or (null? watched) (>= n upsets/flow-max))
      (string-append "{\"flows\":" (as-str total) "}")
      (upsets/flow-loop
        (cdr watched)
        (+ total (upsets/flow-market (car watched)))
        (+ n 1)))))

(define upsets/watched-loop
  (lambda
    (rs acc n)
    (if
      (null? rs)
      acc
      (let
        ((fields (cdr (car rs))) (rest (cdr rs)))
        (if
          (and
            (string-eq (as-str (dict-get fields "watched")) "true")
            (string-eq (as-str (dict-get fields "status")) "open")
            (< n upsets/watch-max))
          (upsets/watched-loop
            rest
            (append acc (list (car rs)))
            (+ n 1))
          (upsets/watched-loop rest acc n))))))

(define upsets/watched-rows
  (lambda
    ()
    (upsets/watched-loop
      (rows (quote pm-market) 500)
      (list)
      0))
  "Open watched pm-market rows, capped at watch-max; the odds and flow ticks touch nothing else.")
