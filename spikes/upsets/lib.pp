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
    (list "upsets-flow" "*/15 * * * *" "upsets/poll-flow")
    (list "upsets-settle" "*/10 * * * *" "upsets/settle"))
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

;; ---------------------------------------------------------------------------
;; control surface (borge/upsets.borg, subsection control-surface).
;; Five private faces over the bench's artifacts, plus the paper ledger that
;; settles against resolutions:
;;   /upsets            signal board: ranked by score, evidence expander
;;   /upsets/m/<mid>    market drill: pm-odds sparkline, wallet-flow table,
;;                      related markets via the co-occurrence cluster
;;   /upsets/wallets    wallet x market co-occurrence, cluster colour,
;;                      win-rate per cluster
;;   /upsets/rules      rule rows + manifest trail (old manifests read-only)
;;   /upsets/positions  paper ledger: open orders, settled history, equity
;;                      curve, and the base-rate band beside every metric
;; L-1 rides in the UI: a metric cell is never rendered without its band.
;; Alerts ride the existing ntfy package (borge/notify.borg); a signal pushes
;; only for a rule that graduated to live, and the push names the signal id,
;; because phone-in-hand execution is the sanctioned v1 mode until
;; upsets.execution clears its gate.
;; Tables: upsets-signal (sweep output), upsets-resolution (settle input),
;; upsets-rule (rule record + live flag), upsets-tax (base-rate bands),
;; paper-order (the ledger).
;; ---------------------------------------------------------------------------

(define upsets/cs/market-href
  (lambda (mid) (string-append "/upsets/m/" mid))
  "Canonical drill-down href: the /upsets/m/<mid> face, mid as the trailing route param.")

(define upsets/cs/routes
  (list
    (list "/upsets" "upsets/board-page")
    (list "/upsets/m/<mid>" "upsets/market-page")
    (list "/upsets/wallets" "upsets/wallets-page")
    (list "/upsets/rules" "upsets/rules-page")
    (list "/upsets/positions" "upsets/positions-page"))
  "The five control-surface faces; every one is declared private at the foot of this section.")

(define upsets/cs/num
  (lambda (v) (parse-float (as-str v))))

(define upsets/cs/max1
  (lambda (n) (upsets/max2 n 1)))

(define upsets/cs/band
  (lambda (lo lost won)
    (string-append
      "base "
      (as-str lo)
      "-"
      (as-str (+ lo 0.05))
      ": "
      (as-str lost)
      " lost / "
      (as-str won)
      " won"))
  "L-1 band label: the observed lost/won split of one 0.05 odds band.")

(define upsets/cs/cell
  (lambda (value band)
    (html/span
      (html/attr "class" "metric")
      (html/join
        (list
          (html/span (html/attr "class" "value") value)
          (html/span (html/attr "class" "band muted") band))))
  "A metric cell is value + base-rate band: no number ships without its band.")

;; ---- sparkline (shared by the market drill and the equity curve) ----

(define upsets/cs/series-lo
  (lambda (pts acc)
    (if
      (null? pts)
      acc
      (upsets/cs/series-lo
        (cdr pts)
        (upsets/min2 acc (upsets/cs/num (car (cdr (car pts)))))))))

(define upsets/cs/series-hi
  (lambda (pts acc)
    (if
      (null? pts)
      acc
      (upsets/cs/series-hi
        (cdr pts)
        (upsets/max2 acc (upsets/cs/num (car (cdr (car pts)))))))))

(define upsets/cs/spark-points
  (lambda (pts i n lo hi w h)
    (if
      (null? pts)
      ""
      (string-append
        (as-str (floor (* (/ (* i w) (upsets/cs/max1 (- n 1))) 1)))
        ","
        (as-str
          (floor
            (*
              (- 1 (/ (- (upsets/cs/num (car (cdr (car pts)))) lo) (upsets/max2 0.000001 (- hi lo))))
              h)))
        " "
        (upsets/cs/spark-points (cdr pts) (+ i 1) n lo hi w h)))))

(define upsets/cs/sparkline
  (lambda (pts label w h)
    (if
      (< (count pts) 2)
      (html/p* (html/attr "class" "muted") (string-append label ": no history yet"))
      (html/join
        (list
          (html/span (html/attr "class" "title") label)
          (string-append
            "<svg class=\"spark\" width=\""
            (as-str w)
            "\" height=\""
            (as-str h)
            "\" viewBox=\"0 0 "
            (as-str w)
            " "
            (as-str h)
            "\"><polyline fill=\"none\" stroke=\"currentColor\" points=\""
            (upsets/cs/spark-points pts 0 (count pts) (upsets/cs/series-lo pts 1) (upsets/cs/series-hi pts 0) w h)
            "\"/></svg>"))))
  "Inline SVG sparkline over (ts . value) points; fewer than two points is a note, never a fake line.")

(define upsets/cs/points
  (lambda (rows acc)
    (if
      (null? rows)
      acc
      (upsets/cs/points
        (cdr rows)
        (append acc
          (list
            (list
              (as-str (dict-get (cdr (car rows)) "ts"))
              (as-str (dict-get (cdr (car rows)) "p"))))))))
  "pm-odds rows -> (ts . p) points, ascending (the ingest watermark keeps them ordered).")

;; ---- 1. signal board ----

(define upsets/cs/score
  (lambda (row) (upsets/cs/num (dict-get (cdr row) "score"))))

(define upsets/cs/insert
  (lambda (row sorted)
    (cond
      ((null? sorted) (list row))
      ((> (upsets/cs/score row) (upsets/cs/score (car sorted))) (cons row sorted))
      (else (cons (car sorted) (upsets/cs/insert row (cdr sorted)))))))

(define upsets/cs/rank
  (lambda (rs sorted)
    (if
      (null? rs)
      sorted
      (upsets/cs/rank (cdr rs) (upsets/cs/insert (car rs) sorted))))
  "Rank signal rows by score, descending; ties keep sweep order.")

(define upsets/cs/signal-row
  (lambda (row)
    (let
      ((f (cdr row)) (mid (as-str (dict-get (cdr row) "market-id"))))
      (string-append
        "<tr><td>"
        (html/code mid)
        "</td><td>"
        (as-str (dict-get f "side"))
        "</td><td>"
        (as-str (dict-get f "kind"))
        "</td><td>"
        (as-str (dict-get f "score"))
        "</td><td><details><summary>evidence</summary>"
        (html/code (as-str (dict-get f "evidence")))
        "</details></td><td>"
        (html/a (upsets/cs/market-href mid) "drill")
        "</td><td>"
        (html/a
          (string-append "/upsets/take?signal-id=" (as-str (dict-get f "id")) "&size=100")
          "take 100")
        "</td></tr>"))))

(define upsets/cs/join-rows
  (lambda (rs acc)
    (if
      (null? rs)
      acc
      (upsets/cs/join-rows (cdr rs) (string-append acc (upsets/cs/signal-row (car rs)))))))

(define upsets/cs/board-table
  (lambda (signals)
    (if
      (null? signals)
      (html/p* (html/attr "class" "muted") "no signals on this branch yet")
      (string-append
        "<table class=\"grid\"><thead><tr><th>market</th><th>side</th><th>kind</th><th>score</th><th>evidence</th><th></th><th></th></tr></thead><tbody>"
        (upsets/cs/join-rows signals "")
        "</tbody></table>"))))

(define upsets/cs/signals
  (lambda () (upsets/cs/rank (rows (quote upsets-signal) 200) (list)))
  "The board's ranked signal set: the sweep's newest 200 rows.")

(define upsets/board-partial
  (lambda () (upsets/cs/board-table (upsets/cs/signals)))
  "htmx partial: the board's ranked table, refreshed in place.")

(define upsets/board-page
  (lambda
    ()
    (html/page
      (list
        (html/h1 "Upsets: signal board")
        (html/p*
          (html/attr "class" "muted")
          "Ranked by score. Evidence expands in place; drill opens the market face. A push alert fires only for a live rule.")
        (html/div
          (html/attr "id" "upsets-board")
          (html/attr "hx-get" "/upsets/partial")
          (html/attr "hx-trigger" "every 30s")
          (html/attr "hx-swap" "innerHTML")
          (upsets/cs/board-table (upsets/cs/signals))))))
  "Signal board face (/upsets): ranked rows, evidence expander, htmx partial refresh.")

;; ---- 2. market drill-down ----

(define upsets/cs/flow-table
  (lambda (mid)
    (upsets/cs/flow-loop (find-rows (quote wallet-flow) "market-id" mid 200) ""))
  "Wallet-flow table for one market (aggregates only — the chapter never stores raw prints).")

(define upsets/cs/flow-loop
  (lambda (rows acc)
    (if
      (null? rows)
      (if
        (string-eq acc "")
        (html/p* (html/attr "class" "muted") "no wallet flow yet")
        (string-append
          "<table class=\"grid\"><thead><tr><th>wallet</th><th>usd</th><th>n</th><th>last ts</th></tr></thead><tbody>"
          acc
          "</tbody></table>"))
      (let ((f (cdr (car rows))))
        (upsets/cs/flow-loop
          (cdr rows)
          (string-append
            acc
            "<tr><td>"
            (html/code (as-str (dict-get (cdr (car rows)) "wallet")))
            "</td><td>"
            (as-str (dict-get f "usd"))
            "</td><td>"
            (as-str (dict-get f "n"))
            "</td><td>"
            (as-str (dict-get f "last-ts"))
            "</td></tr>"))))))

(define upsets/cs/link-loop
  (lambda (rows acc)
    (if
      (null? rows)
      acc
      (let ((cid (as-str (dict-get (cdr (car rows)) "condition-id"))))
        (upsets/cs/link-loop
          (cdr rows)
          (string-append acc (html/a (upsets/cs/market-href cid) cid) " "))))))

(define upsets/cs/related
  (lambda (mid)
    (let ((hits (find-rows (quote pm-market) "condition-id" mid 1)))
      (if
        (null? hits)
        (html/p* (html/attr "class" "muted") "market not ingested on this branch")
        (let ((cluster (as-str (dict-get (cdr (car hits)) "cluster"))))
          (if
            (string-eq cluster "")
            (html/p* (html/attr "class" "muted") "no co-occurrence cluster yet")
            (html/join
              (list
                (html/p*
                  (html/attr "class" "title")
                  (string-append "related markets (cluster " cluster ")"))
                (upsets/cs/link-loop (find-rows (quote pm-market) "cluster" cluster 20) ""))))))))
  "Related markets: neighbours that share this market's co-occurrence cluster tag.")

(define upsets/market-page
  (lambda
    ()
    (let ((mid (as-str (request-param "mid"))))
      (html/page
        (list
          (html/h1 (string-append "Upsets: " mid))
          (upsets/cs/sparkline
            (upsets/cs/points (find-rows (quote pm-odds) "market-id" mid 500) (list))
            "pm-odds (YES)"
            320
            64)
          (upsets/cs/flow-table mid)
          (upsets/cs/related mid)))))
  "Market drill-down face (/upsets/m/<mid>): odds sparkline, wallet-flow table, co-occurrence neighbours.")

;; ---- 3. wallet co-occurrence ----

(define upsets/cs/palette
  (list "#c2410c" "#1d4ed8" "#047857" "#7c3aed" "#b45309" "#0f766e"))

(define upsets/cs/palette-at
  (lambda (i ps)
    (if (= i 0) (car ps) (upsets/cs/palette-at (- i 1) (cdr ps)))))

(define upsets/cs/color
  (lambda (i) (upsets/cs/palette-at (mod i (count upsets/cs/palette)) upsets/cs/palette))
  "Cluster colour: palette slot by first-appearance index (stable within a render).")

(define upsets/cs/mem?
  (lambda (x xs)
    (cond
      ((null? xs) #f)
      ((string-eq x (car xs)) #t)
      (else (upsets/cs/mem? x (cdr xs))))))

(define upsets/cs/add-uniq
  (lambda (x xs) (if (upsets/cs/mem? x xs) xs (cons x xs))))

(define upsets/cs/index
  (lambda (rows field acc)
    (if
      (null? rows)
      acc
      (upsets/cs/index
        (cdr rows)
        field
        (cons
          (list
            (as-str (dict-get (cdr (car rows)) "condition-id"))
            (as-str (dict-get (cdr (car rows)) field)))
          acc))))
  "mid -> field index over pm-market rows (small, in-render only).")

(define upsets/cs/assoc-or
  (lambda (key al dflt)
    (cond
      ((null? al) dflt)
      ((string-eq (car (car al)) key) (car (cdr (car al))))
      (else (upsets/cs/assoc-or key (cdr al) dflt)))))

(define upsets/cs/cluster-add
  (lambda (clusters cluster wallet mid usd)
    (cond
      ((null? clusters) (list (list cluster usd (list wallet) (list mid))))
      ((string-eq (car (car clusters)) cluster)
        (let ((e (car clusters)))
          (cons
            (list
              cluster
              (+ (car (cdr e)) usd)
              (upsets/cs/add-uniq wallet (car (cdr (cdr e))))
              (upsets/cs/add-uniq mid (car (cdr (cdr (cdr e))))))
            (cdr clusters))))
      (else
        (cons
          (car clusters)
          (upsets/cs/cluster-add (cdr clusters) cluster wallet mid usd))))))

(define upsets/cs/clusters
  (lambda (flows index acc)
    (if
      (null? flows)
      acc
      (let
        ((f (cdr (car flows)))
          (mid (as-str (dict-get (cdr (car flows)) "market-id")))
          (cluster (upsets/cs/assoc-or mid index "")))
        (upsets/cs/clusters
          (cdr flows)
          index
          (if
            (string-eq cluster "")
            acc
            (upsets/cs/cluster-add
              acc
              cluster
              (as-str (dict-get f "wallet"))
              mid
              (upsets/cs/num (dict-get f "usd"))))))))
  "Fold wallet-flow rows into (cluster, usd, wallets, markets); one cluster per pm-market tag.")

(define upsets/cs/cluster-winrate
  (lambda (mids wins n)
    (if
      (null? mids)
      (if (= n 0) "n/a" (string-append (as-str wins) "/" (as-str n)))
      (let ((res (find-rows (quote upsets-resolution) "market-id" (car mids) 1)))
        (if
          (null? res)
          (upsets/cs/cluster-winrate (cdr mids) wins n)
          (upsets/cs/cluster-winrate
            (cdr mids)
            (if
              (string-eq (as-str (dict-get (cdr (car res)) "outcome")) "YES")
              (+ wins 1)
              wins)
            (+ n 1))))))
  "Resolution win share inside a cluster: wins / resolved markets, 'n/a' until one resolves.")

(define upsets/cs/cluster-table
  (lambda (clusters i)
    (if
      (null? clusters)
      ""
      (let
        ((e (car clusters))
          (cluster (car (car clusters)))
          (usd (car (cdr (car clusters))))
          (wallets (car (cdr (cdr (car clusters)))))
          (mids (car (cdr (cdr (cdr (car clusters)))))))
        (string-append
          "<tr><td><span class=\"chip\" style=\"background:"
          (upsets/cs/color i)
          "\"></span> "
          (html/code cluster)
          "</td><td>"
          (as-str (count mids))
          "</td><td>"
          (as-str (count wallets))
          "</td><td>"
          (as-str usd)
          "</td><td>"
          (upsets/cs/cluster-winrate mids 0 0)
          "</td></tr>"
          (upsets/cs/cluster-table (cdr clusters) (+ i 1)))))))

(define upsets/wallets-page
  (lambda
    ()
    (let ((index (upsets/cs/index (rows (quote pm-market) 500) "cluster" (list))))
      (html/page
        (list
          (html/h1 "Upsets: wallet co-occurrence")
          (html/p*
            (html/attr "class" "muted")
            "Wallets sharing a cluster, by market; win-rate is the resolution share inside the cluster.")
          (string-append
            "<table class=\"grid\"><thead><tr><th>cluster</th><th>markets</th><th>wallets</th><th>usd</th><th>win rate</th></tr></thead><tbody>"
            (upsets/cs/cluster-table
              (upsets/cs/clusters (rows (quote wallet-flow) 500) index (list))
              0)
            "</tbody></table>"))))
  "Wallet x market co-occurrence face (/upsets/wallets): cluster rows, cluster colour, win-rate per cluster.")

;; ---- 4. rules + manifest trail ----

(define upsets/cs/rule-row
  (lambda (row)
    (let ((f (cdr row)))
      (string-append
        "<tr><td>"
        (html/code (as-str (dict-get f "rule-hash")))
        "</td><td>"
        (as-str (dict-get f "window-kind"))
        "</td><td>"
        (as-str (dict-get f "live"))
        "</td><td>"
        (as-str (dict-get f "manifest"))
        "</td><td>"
        (if
          (string-eq (as-str (dict-get f "live")) "true")
          "live: an edit is a new manifest"
          "read-only manifest")
        "</td></tr>"))))

(define upsets/cs/rule-rows
  (lambda (rows acc)
    (if
      (null? rows)
      acc
      (upsets/cs/rule-rows (cdr rows) (string-append acc (upsets/cs/rule-row (car rows)))))))

(define upsets/rules-page
  (lambda
    ()
    (html/page
      (list
        (html/h1 "Upsets: rules")
        (html/p*
          (html/attr "class" "muted")
          "Every rule edit is a new manifest; old manifests are read-only (the obituary surface).")
        (string-append
          "<table class=\"grid\"><thead><tr><th>rule hash</th><th>window</th><th>live</th><th>manifest</th><th></th></tr></thead><tbody>"
          (upsets/cs/rule-rows (rows (quote upsets-rule) 200) "")
          "</tbody></table>"))))
  "Rule rows + manifest trail face (/upsets/rules); a manifest is never rewritten in place.")

;; ---- 5. paper ledger: take, settle, positions ----

(define upsets/paper-fill
  (lambda (odds size depth coef max-slip)
    (upsets/min2
      0.98
      (* odds (+ 1 (upsets/min2 max-slip (/ (* coef size) (upsets/max2 depth 500)))))))
  "Paper fill price: the signal print adjusted by the depth haircut named in the rule record — the same formula fills.ml applies, so the ledger and the bench agree (upsets.parity-contract).")

(define upsets/settle-pnl
  (lambda (entry size won fees-bps)
    (let ((fee (/ (* fees-bps size) 10000)))
      (if won (- (/ size entry) size fee) (- 0 size fee))))
  "Settled pnl: fees on entry notional; a loser returns -size - fee (fills.ml settle).")

(define upsets/paper-order-json
  (lambda (sig mid side kind size entry ts)
    (string-append
      "{\"signal-id\":\""
      (json-escape sig)
      "\",\"market-id\":\""
      (json-escape mid)
      "\",\"side\":\""
      (json-escape side)
      "\",\"kind\":\""
      (json-escape kind)
      "\",\"size\":"
      (as-str size)
      ",\"entry\":"
      (as-str entry)
      ",\"status\":\"open\",\"created-ts\":"
      (as-str ts)
      "}"))
  "paper-order row: one signal, one size, one fill price; open until the settle job runs.")

(define upsets/paper-settled-json
  (lambda (f won pnl ts)
    (string-append
      "{\"signal-id\":\""
      (json-escape (as-str (dict-get f "signal-id")))
      "\",\"market-id\":\""
      (json-escape (as-str (dict-get f "market-id")))
      "\",\"side\":\""
      (json-escape (as-str (dict-get f "side")))
      "\",\"kind\":\""
      (json-escape (as-str (dict-get f "kind")))
      "\",\"size\":"
      (as-str (dict-get f "size"))
      ",\"entry\":"
      (as-str (dict-get f "entry"))
      ",\"status\":\"settled\",\"created-ts\":"
      (as-str (dict-get f "created-ts"))
      ",\"won\":"
      (if won "true" "false")
      ",\"pnl\":"
      (as-str pnl)
      ",\"settled-ts\":"
      (as-str ts)
      "}"))
  "Settled paper-order row: the open fields plus won/pnl/settled-ts — the manifest-linked result the ledger shows.")

(define upsets/alert
  (lambda (sig mid side size)
    (if
      (null? (find-rows (quote upsets-rule) "live" "true" 1))
      ""
      (begin
        (ntfy/send
          (string-append "upsets " side " " mid)
          (string-append
            "signal "
            sig
            " · "
            (as-str size)
            " usd · phone-in-hand: (upsets/take \""
            sig
            "\" "
            (as-str size)
            ")"))
        "")))
  "Push a live signal over the notify chapter's package (borge/notify.borg); silent while no rule has graduated.")

(define upsets/take
  (lambda
    ()
    (let
      ((sig-id (as-str (request-param "signal-id")))
        (size (upsets/cs/num (request-param "size")))
        (hits (find-rows (quote upsets-signal) "id" (as-str (request-param "signal-id")) 1)))
      (if
        (null? hits)
        (html/page
          (list
            (html/h1 "no such signal")
            (html/p*
              (html/attr "class" "muted")
              (string-append "signal " sig-id " is not on this branch"))))
        (let
          ((f (cdr (car hits)))
            (mid (as-str (dict-get (cdr (car hits)) "market-id")))
            (side (as-str (dict-get (cdr (car hits)) "side")))
            (entry
              (upsets/paper-fill
                (upsets/cs/num (dict-get (cdr (car hits)) "odds"))
                size
                500
                0.25
                0.15)))
          (begin
            (create
              (quote paper-order)
              (upsets/paper-order-json
                sig-id mid side (as-str (dict-get f "kind")) size entry (now)))
            (upsets/alert sig-id mid side size)
            (html/page
              (list
                (html/h1 "paper order taken")
                (html/p*
                  (html/attr "class" "muted")
                  "one paper order; the human is the executor until upsets.execution clears its gate")
                (html/code (upsets/paper-order-json sig-id mid side (as-str (dict-get f "kind")) size entry (now))))))))))
  "(upsets/take signal-id size): materialise one paper order at the signal print and push the ntfy alert.")

(define upsets/settle-loop
  (lambda (orders n pnl)
    (if
      (null? orders)
      (string-append "{\"settled\":" (as-str n) ",\"pnl\":" (as-str pnl) "}")
      (let
        ((f (cdr (car orders)))
          (res
            (find-rows
              (quote upsets-resolution)
              "market-id"
              (as-str (dict-get (cdr (car orders)) "market-id"))
              1)))
        (if
          (null? res)
          (upsets/settle-loop (cdr orders) n pnl)
          (let
            ((won
              (string-eq
                (as-str (dict-get (cdr (car res)) "outcome"))
                (as-str (dict-get f "side"))))
              (p
                (upsets/settle-pnl
                  (upsets/cs/num (dict-get f "entry"))
                  (upsets/cs/num (dict-get f "size"))
                  (string-eq
                    (as-str (dict-get (cdr (car res)) "outcome"))
                    (as-str (dict-get f "side")))
                  (upsets/cs/num (dict-get (cdr (car res)) "fees-bps")))))
            (begin
              (update (quote paper-order) (car (car orders)) (upsets/paper-settled-json f won p (now)))
              (upsets/settle-loop (cdr orders) (+ n 1) (+ pnl p)))))))))

(define upsets/settle
  (lambda
    ()
    (upsets/settle-loop (find-rows (quote paper-order) "status" "open" 200) 0 0))
  "Settle job (jobs-scheduled): every open order whose market has a resolution gets pnl + settled status; returns {settled,pnl} and the row keeps the manifest trail.")

(define upsets/cs/equity
  (lambda (settled acc pnl)
    (if
      (null? settled)
      acc
      (let ((p (+ pnl (upsets/cs/num (dict-get (cdr (car settled)) "pnl")))))
        (upsets/cs/equity
          (cdr settled)
          (append acc
            (list
              (list
                (as-str (dict-get (cdr (car settled)) "settled-ts"))
                (as-str p))))
          p))))
  "Equity curve: (settled-ts . cumulative pnl) at each settle event.")

(define upsets/cs/orders-loop
  (lambda (rows label acc)
    (if
      (null? rows)
      (if
        (string-eq acc "")
        (html/p* (html/attr "class" "muted") (string-append label ": none"))
        (html/join
          (list
            (html/p* (html/attr "class" "title") label)
            (string-append
              "<table class=\"grid\"><thead><tr><th>market</th><th>side</th><th>kind</th><th>size</th><th>entry</th><th>status</th><th>pnl</th></tr></thead><tbody>"
              acc
              "</tbody></table>"))))
      (let ((f (cdr (car rows))))
        (upsets/cs/orders-loop
          (cdr rows)
          label
          (string-append
            acc
            "<tr><td>"
            (html/code (as-str (dict-get (cdr (car rows)) "market-id")))
            "</td><td>"
            (as-str (dict-get f "side"))
            "</td><td>"
            (as-str (dict-get f "kind"))
            "</td><td>"
            (as-str (dict-get f "size"))
            "</td><td>"
            (as-str (dict-get f "entry"))
            "</td><td>"
            (as-str (dict-get f "status"))
            "</td><td>"
            (as-str (dict-get f "pnl"))
            "</td></tr>"))))))

(define upsets/cs/orders-table
  (lambda (rows label) (upsets/cs/orders-loop rows label "")))

(define upsets/cs/band-strip
  (lambda (rows acc)
    (if
      (null? rows)
      acc
      (upsets/cs/band-strip
        (cdr rows)
        (string-append
          acc
          (html/span
            (html/attr "class" "band muted")
            (upsets/cs/band
              (upsets/cs/num (dict-get (cdr (car rows)) "band"))
              (upsets/cs/num (dict-get (cdr (car rows)) "lost"))
              (upsets/cs/num (dict-get (cdr (car rows)) "won"))))))))
  "Base-rate tax rows rendered as the band strip that sits beside every ledger metric (L-1 in the UI).")

(define upsets/positions-page
  (lambda
    ()
    (let
      ((open-orders (find-rows (quote paper-order) "status" "open" 200))
        (settled (find-rows (quote paper-order) "status" "settled" 500)))
      (html/page
        (list
          (html/h1 "Upsets: paper ledger")
          (html/p*
            (html/attr "class" "muted")
            "Human is the executor until upsets.execution clears its gate; every metric shows its base-rate band.")
          (upsets/cs/cell
            (string-append "settled orders " (as-str (count settled)))
            (upsets/cs/band-strip (rows (quote upsets-tax) 20) ""))
          (upsets/cs/sparkline
            (upsets/cs/equity settled (list) 0)
            "equity curve (cumulative pnl)"
            360
            72)
          (upsets/cs/orders-table open-orders "open orders")
          (upsets/cs/orders-table settled "settled history"))))
  "Paper ledger face (/upsets/positions): open orders, settled history, equity curve, base-rate band beside every metric.")

;; ---------------------------------------------------------------------------
;; Routes (private by default; anonymous traffic stays on system/main)
;; ---------------------------------------------------------------------------
(route "/upsets" "upsets/board-page" "private")
(route "/upsets/partial" "upsets/board-partial" "private")
(route "/upsets/m" "upsets/market-page" "private")
(route "/upsets/wallets" "upsets/wallets-page" "private")
(route "/upsets/rules" "upsets/rules-page" "private")
(route "/upsets/positions" "upsets/positions-page" "private")
(route "/upsets/take" "upsets/take" "private")
