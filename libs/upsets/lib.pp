;; package upsets -- prediction-market informed-flow ingest
;; Spec: borge/upsets.borg (upsets.data-model + upsets.ingest)
;; Branch: develop on <user>/dev; promote is the gate. Schedules are
;;   INSTALLED SEPARATELY by the operator via (upsets/install) — the
;;   package ships them off (chapter law).
;;
;; Kernel deps (all shipped on prod today): http-get (admin/agent),
;;   json-parse, now, deftype/create/update/find-rows/rows, schedule,
;;   quotient, parse-float, nth, assoc, begin/let*. No kernel sort
;;   needed (watched set taken in row order, capped).
;;
;; Ingest law (chapter): AGGREGATE, DON'T HOARD — trade prints reduce
;;   to hourly wallet-flow rows at fetch time; pm-odds only for watched
;;   markets (watermarked by last-odds-ts); every tick is idempotent.

;; ---- types (chapter data-model) ----
(deftype 'pm-market 'question 'slug 'condition-id 'token-yes 'token-no 'vol 'end 'status 'last-odds 'observed-at 'watched 'last-odds-ts 'last-flow-ts 'cluster)
(deftype 'pm-odds 'market-id 'ts 'p)
(deftype 'wallet-flow 'market-id 'wallet 'side 'usd 'n 'first-ts 'last-ts)
(deftype 'signal 'market-id 'kind 'score 'evidence 'fired-at 'rule-id 'status)
(deftype 'paper-order 'market-id 'side 'entry-odds 'size 'opened-at 'settled-at 'resolved-outcome 'pnl 'rule-id 'manifest-id)
(deftype 'sim-manifest 'dataset-hash 'rule-hash 'params 'window-kind 'from-ts 'to-ts 'metrics 'created-at)

;; ---- config ----
(define upsets/gamma "https://gamma-api.polymarket.com")
(define upsets/clob "https://clob.polymarket.com")
(define upsets/data-api "https://data-api.polymarket.com")
(define upsets/poll-pages 2)
(define upsets/watch-max 12)
(define upsets/flow-max 5)
(define upsets/watch-threshold 0.40)
(define upsets/odds-chunk 200)
(define upsets/min-trade-usd 1)

;; ---- small helpers ----
;; json-escape comes from libs/std (shared escaper, i-1788670711);
;; the old upsets/json-esc only covered backslash+quote, so a newline
;; in a question/market string produced invalid JSON payloads.

(define upsets/min2
  (lambda (a b) (if (< a b) a b)))

(define upsets/max2
  (lambda (a b) (if (> a b) a b)))

(define upsets/fetch-json
  (lambda
    (url)
    "GET url and json-parse the body; raises on transport / non-ok."
    (let
      ((r (http-get url)))
      (if
        (string-eq (as-str (dict-get r "ok")) "true")
        (json-parse (dict-get r "body"))
        (error (string-append "upsets/fetch-json: "
                (as-str (dict-get r "err")) " "
                (as-str (dict-get r "status"))))))))

(define upsets/num-or
  (lambda (v d) (if (null? v) d (parse-float v))))

(define upsets/str-or
  (lambda (v d) (if (null? v) d (as-str v))))

;; ---- pm-market ----
(define upsets/market-json
  (lambda
    (question slug cid tok-yes tok-no vol end status last-odds observed watched lots flos)
    (string-append
      "{\"question\":\"" (json-escape question)
      "\",\"slug\":\"" (json-escape slug)
      "\",\"condition-id\":\"" (json-escape cid)
      "\",\"token-yes\":\"" (json-escape tok-yes)
      "\",\"token-no\":\"" (json-escape tok-no)
      "\",\"vol\":" (as-str vol)
      ",\"end\":\"" (json-escape end)
      "\",\"status\":\"" (json-escape status)
      "\",\"last-odds\":" (as-str last-odds)
      ",\"observed-at\":" (as-str observed)
      ",\"watched\":" (if watched "true" "false")
      ",\"last-odds-ts\":" (as-str lots)
      ",\"last-flow-ts\":" (as-str flos)
      ",\"cluster\":\"\"}")))

(define upsets/market-json-of
  (lambda
    (fields lots)
    "Rebuild a pm-market payload from an existing row's fields, overriding last-odds-ts."
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
      (upsets/num-or (dict-get fields "last-flow-ts") 0))))

(define upsets/upsert-market
  (lambda
    (m)
    "Upsert one gamma market dict; returns ok | skip (non-2-token / no clob ids)."
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
                ((prices (if (null? prices-raw) (list) (json-parse prices-raw))))
                (let
                  ((last-odds
                     (if (< (count prices) 2)
                         1
                         (upsets/min2 (parse-float (car prices))
                                      (parse-float (car (cdr prices)))))))
                  (let
                    ((watched (< last-odds upsets/watch-threshold))
                     (now-ts (now))
                     (existing (find-rows 'pm-market "condition-id" (as-str cid) 1)))
                    (if
                      (null? existing)
                      (begin
                        (create 'pm-market
                                (upsets/market-json question slug (as-str cid)
                                                    (as-str (car toks)) (as-str (car (cdr toks)))
                                                    vol end "open" last-odds now-ts watched 0 0))
                        "ok")
                      (begin
                        (update 'pm-market (car (car existing))
                                (upsets/market-json question slug (as-str cid)
                                                    (as-str (car toks)) (as-str (car (cdr toks)))
                                                    vol end "open" last-odds now-ts watched
                                                    (upsets/num-or (dict-get (cdr (car existing)) "last-odds-ts") 0)
                                                    (upsets/num-or (dict-get (cdr (car existing)) "last-flow-ts") 0)))
                        "ok"))))))))))))

(define upsets/poll-page
  (lambda
    (page seen)
    "Fetch gamma pages of open markets and upsert; recurse to poll-pages."
    (if
      (>= page upsets/poll-pages)
      (string-append "{\"markets\":" (as-str seen) "}")
      (let
        ((ms (upsets/fetch-json
               (string-append upsets/gamma "/markets?closed=false&limit=100&offset="
                              (as-str (* page 100))
                              "&order=volume24hr&ascending=false"))))
        (upsets/poll-loop ms page seen)))))

(define upsets/poll-loop
  (lambda
    (ms page seen)
    (if
      (null? ms)
      (upsets/poll-page (+ page 1) seen)
      (upsets/poll-loop (cdr ms) page
                        (if (string-eq (upsets/upsert-market (car ms)) "ok")
                            (+ seen 1)
                            seen)))))

(define upsets/poll
  (lambda
    ()
    "Discovery tick: gamma open markets (poll-pages x 100 by 24h volume) -> upsert pm-market rows. Idempotent. Returns {markets}."
    (upsets/poll-page 0 0)))

;; ---- watched set ----
(define upsets/watched-rows
  (lambda
    ()
    "Open watched pm-market rows, capped at watch-max (row order; no ranking at ingest)."
    (upsets/watched-loop (rows 'pm-market 500) (list) 0)))

(define upsets/watched-loop
  (lambda
    (rs acc n)
    (if
      (null? rs)
      acc
      (let
        ((fields (cdr (car rs)))
         (rest (cdr rs)))
        (if
          (and (string-eq (as-str (dict-get fields "watched")) "true")
               (string-eq (as-str (dict-get fields "status")) "open")
               (< n upsets/watch-max))
          (upsets/watched-loop rest (append acc (list (car rs))) (+ n 1))
          (upsets/watched-loop rest acc n))))))

;; ---- odds ----
(define upsets/odds-row-json
  (lambda
    (mid ts p)
    (string-append
      "{\"market-id\":\"" (json-escape mid)
      "\",\"ts\":" (as-str ts)
      ",\"p\":" (as-str p) "}")))

(define upsets/odds-write
  (lambda
    (mid pts last-ts written budget)
    "Write pm-odds rows for points with ts > last-ts (capped by budget)."
    (if
      (or (null? pts) (<= budget 0))
      written
      (let
        ((pt (car pts))
         (rest (cdr pts)))
        (let
          ((ts (parse-float (dict-get pt "t")))
           (p (parse-float (dict-get pt "p"))))
          (if
            (> ts last-ts)
            (begin
              (create 'pm-odds (upsets/odds-row-json mid ts p))
              (upsets/odds-write mid rest ts (+ written 1) (- budget 1)))
            (upsets/odds-write mid rest last-ts written budget)))))))

(define upsets/poll-odds-market
  (lambda
    (row)
    "Fetch full hourly history for one watched market; write new points; return (points-written . max-ts-seen)."
    (let
      ((fields (cdr row))
       (mid (as-str (dict-get (cdr row) "condition-id")))
       (tok (as-str (dict-get (cdr row) "token-yes")))
       (last-ts (upsets/num-or (dict-get (cdr row) "last-odds-ts") 0)))
      (let
        ((hist (upsets/fetch-json
                 (string-append upsets/clob "/prices-history?market=" tok
                                "&interval=max&fidelity=60"))))
        (let
          ((raw (dict-get hist "history"))
           ;; dict-get returns complex sub-values as JSON strings (the
           ;; VStr contract) -- arrays need an explicit json-parse.
           (pts (if (null? (dict-get hist "history"))
                    (list)
                    (json-parse (dict-get hist "history")))))
          (if
            (null? pts)
            (cons 0 last-ts)
            (let
              ((n (upsets/odds-write mid pts last-ts 0 upsets/odds-chunk)))
              (cons n (upsets/odds-scan-max pts last-ts)))))))))

(define upsets/odds-scan-max
  (lambda
    (pts acc)
    (if
      (null? pts)
      acc
      (upsets/odds-scan-max (cdr pts)
                            (upsets/max2 acc (parse-float (dict-get (car pts) "t")))))))

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
            (update 'pm-market (car row) (upsets/market-json-of fields max-ts))
            ())
          (upsets/odds-loop (cdr watched) (+ total n)))))))

(define upsets/poll-odds
  (lambda
    ()
    "Odds tick: hourly YES prices for watched markets (capped per market). Idempotent via the last-odds-ts watermark."
    (upsets/odds-loop (upsets/watched-rows) 0)))

;; ---- wallet flows ----
(define upsets/flow-bucket
  (lambda
    (ts)
    "Hour bucket start; canonical first-ts for dedupe. (floor: API ts may parse as float; quotient needs ints.)"
    (* 3600 (quotient (floor ts) 3600))))

(define upsets/flow-agg-add
  (lambda
    (agg key usd ts bucket)
    "Functionally merge one trade into the aggregate list [key usd n first last]."
    (cond
      ((null? agg) (list (list key usd 1 bucket ts)))
      ((string-eq (car (car agg)) key)
       (let
         ((e (car agg)))
         (cons
           (list key
                 (+ (car (cdr e)) usd)
                 (+ 1 (car (cdr (cdr e))))
                 (upsets/min2 (car (cdr (cdr (cdr e)))) bucket)
                 (upsets/max2 (car (cdr (cdr (cdr (cdr e))))) ts))
           (cdr agg))))
      (else (cons (car agg) (upsets/flow-agg-add (cdr agg) key usd ts bucket))))))

(define upsets/flow-agg
  (lambda
    (trades tok-yes agg)
    "Fold BUY prints into per (wallet|side|hour-bucket) aggregates."
    (if
      (null? trades)
      agg
      (let
        ((t (car trades))
         (rest (cdr trades)))
        (let
          ((side-raw (upsets/str-or (dict-get t "side") ""))
           (usd (* (upsets/num-or (dict-get t "size") 0)
                   (upsets/num-or (dict-get t "price") 0))))
          (if
            (or (not (string-eq side-raw "BUY")) (< usd upsets/min-trade-usd))
            (upsets/flow-agg rest tok-yes agg)
            (let
              ((oside (if (string-eq (upsets/str-or (dict-get t "asset") "") tok-yes)
                          "YES"
                          "NO"))
               (wallet (upsets/str-or (dict-get t "pseudonym")
                                      (upsets/str-or (dict-get t "proxyWallet") "?")))
               (ts (floor (upsets/num-or (dict-get t "timestamp") 0))))
              (upsets/flow-agg rest tok-yes
                               (upsets/flow-agg-add agg
                                                    (string-append wallet "|" oside)
                                                    usd ts
                                                    (upsets/flow-bucket ts))))))))))

(define upsets/flow-upsert
  (lambda
    (mid wallet oside usd n first-ts last-ts)
    "Upsert one hourly wallet-flow row keyed (market, wallet, side, bucket)."
    (let
      ((existing (find-rows 'wallet-flow "market-id" mid "wallet" wallet
                            "side" oside "first-ts" (as-str first-ts) 1)))
      (if
        (null? existing)
        (create 'wallet-flow
                (upsets/flow-row-json mid wallet oside usd n first-ts last-ts))
        (let
          ((f (cdr (car existing))))
          (update 'wallet-flow (car (car existing))
                  (upsets/flow-row-json mid wallet oside
                                        (+ usd (upsets/num-or (dict-get f "usd") 0))
                                        (+ n (upsets/num-or (dict-get f "n") 0))
                                        first-ts
                                        (upsets/max2 last-ts (upsets/num-or (dict-get f "last-ts") 0)))))))))

(define upsets/flow-row-json
  (lambda
    (mid wallet side usd n first-ts last-ts)
    (string-append
      "{\"market-id\":\"" (json-escape mid)
      "\",\"wallet\":\"" (json-escape wallet)
      "\",\"side\":\"" (json-escape side)
      "\",\"usd\":" (as-str usd)
      ",\"n\":" (as-str n)
      ",\"first-ts\":" (as-str first-ts)
      ",\"last-ts\":" (as-str last-ts) "}")))

(define upsets/flow-write
  (lambda
    (mid agg written)
    (if
      (null? agg)
      written
      (let
        ((e (car agg))
         (rest (cdr agg)))
        (let
          ((key (car e))
           (usd (car (cdr e)))
           (n (car (cdr (cdr e))))
           (first-ts (car (cdr (cdr (cdr e)))))
           (last-ts (car (cdr (cdr (cdr (cdr e)))))))
          (begin
            (upsets/flow-upsert mid (upsets/key-wallet key) (upsets/key-side key)
                                usd n first-ts last-ts)
            (upsets/flow-write mid rest (+ written 1))))))))

(define upsets/key-wallet
  (lambda
    (key)
    (nth (string-split key "|") 0)))

(define upsets/key-side
  (lambda
    (key)
    (nth (string-split key "|") 1)))

(define upsets/flow-loop
  (lambda
    (watched idx total)
    (if
      (or (null? watched) (>= idx upsets/flow-max))
      (string-append "{\"flows\":" (as-str total) "}")
      (let*
        ((row (car watched))
         (fields (cdr row))
         (cid (upsets/str-or (dict-get fields "condition-id") ""))
         (tok (upsets/str-or (dict-get fields "token-yes") ""))
         (trades (upsets/fetch-json
                   (string-append upsets/data-api "/trades?market=" cid "&limit=500")))
         (agg (upsets/flow-agg trades tok (list)))
         (n (upsets/flow-write cid agg 0)))
        (upsets/flow-loop (cdr watched) (+ idx 1) (+ total n))))))

(define upsets/poll-flow
  (lambda
    ()
    "Flow tick: data-api trade prints for the first flow-max watched markets -> hourly wallet-flow rows. Idempotent via (market,wallet,side,bucket) upsert."
    (upsets/flow-loop (upsets/watched-rows) 0 0)))

;; ---- operator entry point ----
(define upsets/install
  (lambda
    ()
    "Register the three ingest schedules (chapter: shipped off; the operator turns them on). Run ONCE on the branch that owns these fns."
    (begin
      (schedule "upsets-poll" "*/5 * * * *" "upsets/poll")
      (schedule "upsets-odds" "3 * * * *" "upsets/poll-odds")
      (schedule "upsets-flow" "*/15 * * * *" "upsets/poll-flow")
      (schedules))))
