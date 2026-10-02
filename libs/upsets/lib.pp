;; package upsets -- exported from live branch system/main
;; Sync: scripts/pp_sync.py pull|push

(define upsets/clob
  "https://clob.polymarket.com")

(define upsets/data-api
  "https://data-api.polymarket.com")

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
  "GET url and json-parse the body; raises on transport / non-ok.")

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
  "Fold BUY prints into per (wallet|side|hour-bucket) aggregates.")

(define upsets/flow-agg-add
  (lambda
    (agg key usd ts bucket)
    (cond
      ((null? agg) (list (list key usd 1 bucket ts)))
      ((string-eq (car (car agg)) key)
        (let
          ((e (car agg)))
          (cons
            (list
              key
              (+ (car (cdr e)) usd)
              (+ 1 (car (cdr (cdr e))))
              (upsets/min2 (car (cdr (cdr (cdr e)))) bucket)
              (upsets/max2 (car (cdr (cdr (cdr (cdr e))))) ts))
            (cdr agg))))
      (else
        (cons
          (car agg)
          (upsets/flow-agg-add (cdr agg) key usd ts bucket)))))
  "Functionally merge one trade into the aggregate list [key usd n first last].")

(define upsets/flow-bucket
  (lambda (ts) (* 3600 (quotient (floor ts) 3600)))
  "Hour bucket start; canonical first-ts for dedupe. (floor: API ts may parse as float; quotient needs ints.)")

(define upsets/flow-loop
  (lambda
    (watched idx total)
    (if
      (or (null? watched) (>= idx upsets/flow-max))
      (string-append "{\"flows\":" (as-str total) "}")
      (let*
        ((row (car watched))
          (fields (cdr row))
          (cid
            (upsets/str-or (dict-get fields "condition-id") ""))
          (tok (upsets/str-or (dict-get fields "token-yes") ""))
          (trades
            (upsets/fetch-json
              (string-append
                upsets/data-api
                "/trades?market="
                cid
                "&limit=500")))
          (agg (upsets/flow-agg trades tok (list)))
          (n (upsets/flow-write cid agg 0)))
        (upsets/flow-loop (cdr watched) (+ idx 1) (+ total n))))))

(define upsets/flow-max
  5)

(define upsets/flow-row-json
  (lambda
    (mid
      wallet
      side
      usd
      n
      first-ts
      last-ts)
    (string-append
      "{\"market-id\":\""
      (upsets/json-esc mid)
      "\",\"wallet\":\""
      (upsets/json-esc wallet)
      "\",\"side\":\""
      (upsets/json-esc side)
      "\",\"usd\":"
      (as-str usd)
      ",\"n\":"
      (as-str n)
      ",\"first-ts\":"
      (as-str first-ts)
      ",\"last-ts\":"
      (as-str last-ts)
      "}")))

(define upsets/flow-upsert
  (lambda
    (mid
      wallet
      oside
      usd
      n
      first-ts
      last-ts)
    (let
      ((existing
        (find-rows
          (quote wallet-flow)
          "market-id"
          mid
          "wallet"
          wallet
          "side"
          oside
          "first-ts"
          (as-str first-ts)
          1)))
      (if
        (null? existing)
        (create
          (quote wallet-flow)
          (upsets/flow-row-json
            mid
            wallet
            oside
            usd
            n
            first-ts
            last-ts))
        (let
          ((f (cdr (car existing))))
          (update
            (quote wallet-flow)
            (car (car existing))
            (upsets/flow-row-json
              mid
              wallet
              oside
              (+ usd (upsets/num-or (dict-get f "usd") 0))
              (+ n (upsets/num-or (dict-get f "n") 0))
              first-ts
              (upsets/max2
                last-ts
                (upsets/num-or (dict-get f "last-ts") 0))))))))
  "Upsert one hourly wallet-flow row keyed (market, wallet, side, bucket).")

(define upsets/flow-write
  (lambda
    (mid agg written)
    (if
      (null? agg)
      written
      (let
        ((e (car agg)) (rest (cdr agg)))
        (let
          ((key (car e))
            (usd (car (cdr e)))
            (n (car (cdr (cdr e))))
            (first-ts (car (cdr (cdr (cdr e)))))
            (last-ts (car (cdr (cdr (cdr (cdr e)))))))
          (begin
            (upsets/flow-upsert
              mid
              (upsets/key-wallet key)
              (upsets/key-side key)
              usd
              n
              first-ts
              last-ts)
            (upsets/flow-write mid rest (+ written 1))))))))

(define upsets/gamma
  "https://gamma-api.polymarket.com")

(define upsets/install
  (lambda
    ()
    (begin
      (schedule "upsets-poll" "*/5 * * * *" "upsets/poll")
      (schedule "upsets-odds" "3 * * * *" "upsets/poll-odds")
      (schedule
        "upsets-flow"
        "*/15 * * * *"
        "upsets/poll-flow")
      (schedules)))
  "Register the three ingest schedules (chapter: shipped off; the operator turns them on). Run ONCE on the branch that owns these fns.")

(define upsets/json-esc
  (lambda (s) (upsets/json-esc-loop (as-str s) 0 ""))
  "Escape a string for embedding in a JSON document.")

(define upsets/json-esc-loop
  (lambda
    (s i acc)
    (if
      (>= i (string-length s))
      acc
      (let
        ((c (substring s i 1)))
        (upsets/json-esc-loop
          s
          (+ i 1)
          (string-append
            acc
            (cond
              ((string-eq c "\\") "\\\\")
              ((string-eq c "\"") "\\\"")
              (else c))))))))

(define upsets/key-side
  (lambda (key) (nth (string-split key "|") 1)))

(define upsets/key-wallet
  (lambda (key) (nth (string-split key "|") 0)))

(define upsets/market-json
  (lambda
    (question
      slug
      cid
      tok-yes
      tok-no
      vol
      end
      status
      last-odds
      observed
      watched
      lots
      flos)
    (string-append
      "{\"question\":\""
      (upsets/json-esc question)
      "\",\"slug\":\""
      (upsets/json-esc slug)
      "\",\"condition-id\":\""
      (upsets/json-esc cid)
      "\",\"token-yes\":\""
      (upsets/json-esc tok-yes)
      "\",\"token-no\":\""
      (upsets/json-esc tok-no)
      "\",\"vol\":"
      (as-str vol)
      ",\"end\":\""
      (upsets/json-esc end)
      "\",\"status\":\""
      (upsets/json-esc status)
      "\",\"last-odds\":"
      (as-str last-odds)
      ",\"observed-at\":"
      (as-str observed)
      ",\"watched\":"
      (if watched "true" "false")
      ",\"last-odds-ts\":"
      (as-str lots)
      ",\"last-flow-ts\":"
      (as-str flos)
      ",\"cluster\":\"\"}")))

(define upsets/market-json-of
  (lambda
    (fields lots)
    (upsets/market-json
      (upsets/str-or (dict-get fields "question") "")
      (upsets/str-or (dict-get fields "slug") "")
      (upsets/str-or (dict-get fields "condition-id") "")
      (upsets/str-or (dict-get fields "token-yes") "")
      (upsets/str-or (dict-get fields "token-no") "")
      (upsets/num-or (dict-get fields "vol") 0)
      (upsets/str-or (dict-get fields "end") "")
      (upsets/str-or (dict-get fields "status") "open")
      (upsets/num-or (dict-get fields "last-odds") 1)
      (upsets/num-or (dict-get fields "observed-at") 0)
      (string-eq (as-str (dict-get fields "watched")) "true")
      lots
      (upsets/num-or (dict-get fields "last-flow-ts") 0)))
  "Rebuild a pm-market payload from an existing row's fields, overriding last-odds-ts.")

(define upsets/max2
  (lambda (a b) (if (> a b) a b)))

(define upsets/min-trade-usd
  1)

(define upsets/min2
  (lambda (a b) (if (< a b) a b)))

(define upsets/num-or
  (lambda (v d) (if (null? v) d (parse-float v))))

(define upsets/odds-chunk
  200)

(define upsets/odds-loop
  (lambda
    (watched total)
    (if
      (null? watched)
      (string-append "{\"points\":" (as-str total) "}")
      (let*
        ((row (car watched))
          (fields (cdr row))
          (res (upsets/poll-odds-market row))
          (n (car res))
          (max-ts (cdr res)))
        (begin
          (if
            (> n 0)
            (update
              (quote pm-market)
              (car row)
              (upsets/market-json-of fields max-ts))
            ())
          (upsets/odds-loop (cdr watched) (+ total n)))))))

(define upsets/odds-row-json
  (lambda
    (mid ts p)
    (string-append
      "{\"market-id\":\""
      (upsets/json-esc mid)
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
        (upsets/max2 acc (parse-float (dict-get (car pts) "t")))))))

(define upsets/odds-write
  (lambda
    (mid pts last-ts written budget)
    (if
      (or (null? pts) (<= budget 0))
      written
      (let
        ((pt (car pts)) (rest (cdr pts)))
        (let
          ((ts (parse-float (dict-get pt "t")))
            (p (parse-float (dict-get pt "p"))))
          (if
            (> ts last-ts)
            (begin
              (create (quote pm-odds) (upsets/odds-row-json mid ts p))
              (upsets/odds-write
                mid
                rest
                ts
                (+ written 1)
                (- budget 1)))
            (upsets/odds-write mid rest last-ts written budget))))))
  "Write pm-odds rows for points with ts > last-ts (capped by budget).")

(define upsets/poll
  (lambda () (upsets/poll-page 0 0))
  "Discovery tick: gamma open markets (poll-pages x 100 by 24h volume) -> upsert pm-market rows. Idempotent. Returns {markets}.")

(define upsets/poll-flow
  (lambda () (upsets/flow-loop (upsets/watched-rows) 0 0))
  "Flow tick: data-api trade prints for the first flow-max watched markets -> hourly wallet-flow rows. Idempotent via (market,wallet,side,bucket) upsert.")

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

(define upsets/poll-odds
  (lambda () (upsets/odds-loop (upsets/watched-rows) 0))
  "Odds tick: hourly YES prices for watched markets (capped per market). Idempotent via the last-odds-ts watermark.")

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
  "Fetch full hourly history for one watched market; write new points; return (points-written . max-ts-seen).")

(define upsets/poll-page
  (lambda
    (page seen)
    (if
      (>= page upsets/poll-pages)
      (string-append "{\"markets\":" (as-str seen) "}")
      (let
        ((ms
          (upsets/fetch-json
            (string-append
              upsets/gamma
              "/markets?closed=false&limit=100&offset="
              (as-str (* page 100))
              "&order=volume24hr&ascending=false"))))
        (upsets/poll-loop ms page seen))))
  "Fetch gamma pages of open markets and upsert; recurse to poll-pages.")

(define upsets/poll-pages
  2)

(define upsets/str-or
  (lambda (v d) (if (null? v) d (as-str v))))

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
                        "ok")))))))))))
  "Upsert one gamma market dict; returns ok | skip (non-2-token / no clob ids).")

(define upsets/watch-max
  12)

(define upsets/watch-threshold
  0.4)

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
  "Open watched pm-market rows, capped at watch-max (row order; no ranking at ingest).")
