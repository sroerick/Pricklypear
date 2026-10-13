

(define standup/brief
  (lambda ()
    (string-append
      "Standup:\n"
      (guard (lambda (e) "lobby: n/a") (standup/lobby-line))
      "\n"
      (guard (lambda (e) "open cards: n/a") (standup/todo-line))
      (guard (lambda (e) "workstreams: n/a") (standup/workstreams-line))
      (guard (lambda (e) "human review: n/a") (standup/review-line))
      (guard (lambda (e) "moved: n/a") (standup/moved-line))
      "\nhttps://pricklypear.rocks/chat")));; package standup -- daily 09:00 Mountain standup brief (gregor)
;; Delivery: multi-line brief to the lobby + one ntfy push. Data: lobby
;; chat history + gregor-visible todo-item rows (calendar and full board
;; coverage wait on shared-data-spaces S1/S2 grants).
;; Scheduling: cron is UTC-only in the kernel, so 09:00 MDT = "0 15 * * *".
;; After DST ends (first Sunday of November) flip the spec to "0 16 * * *".
;; The schedule row self-heals: standup/run re-upserts itself each run.
;;
;; Push ordering: the ntfy push runs FIRST and raises on non-ok so the
;; jobs runner retries transient failures (observed: ntfy.sh returned a
;; one-off 404 on 09-08); the lobby post follows, guarded. The job result
;; is the brief plus a final "push ok" line for self-diagnostics.
;;
;; Topic: this lib reads the topic from a caller-garden ntfy-config row
;; (see libs/ntfy/lib.pp). gregor keeps one in his own garden; nothing
;; capability-shaped is stored in git here. Eval-installed copies on
;; gregor/personal carry the topic inline until this lib deploys.
;; After a deploy: (load-library "standup") once restores the defines.
;;
;; v2 (orchestration task 10, borge/workstreams.borg standup-v2): the brief
;; now also reads the LIVE store for per-workstream column counts, the
;; 'ready for human review' rollup, and yesterday's todo-event transitions
;; (so it says where each stream is, what moved, what's ready for gregor).
;; The new sections are guarded: before the workstreams package deploys on a
;; host they silently render nothing; after it does they light up. The
;; delivery shape (lobby + ntfy + the self-healing schedule row) is unchanged.
;; Workstreams/card data is read through the todo + workstreams families
;; (lazy bindings), and `rows 'todo-item' stays the gregor-visible read.

(define standup/count
  (lambda (l)
    (if (null? l) 0 (+ 1 (standup/count (cdr l))))))

;; ---- v2: workstreams + human-review + yesterday's transitions -----------
;; borge/workstreams.borg standup-v2 (orchestration task 10). Reads the LIVE
;; store; the workstreams-family sections are load-guarded so a host without
;; the workstreams package (pre-promote deployment) degrades to nothing
;; rather than breaking the brief.

(define standup/date-prefix
  (lambda (s)
    (let ((t (as-str s)))
      (if (>= (string-length t) 10) (substring t 0 10) t))))

(define standup/ensure-workstreams
  (lambda ()
    "Bind the workstreams family in this session (job context; safe)."
    (load-library "workstreams")))

(define standup/filter-open
  (lambda (l)
    (if (null? l)
        ()
        (let ((st (as-str (dict-get (cdr (car l)) "state"))))
          (if (if (string-eq st "DONE") #f (string-eq st "PARKED"))
              (standup/filter-open (cdr l))
              (cons (car l) (standup/filter-open (cdr l))))))))

(define standup/item-index
  (lambda ()
    "All todo-items as (id . title), id from the PATH tail. For resolving
     rollup/event card ids to titles in the brief."
    (list/map
      (lambda (n)
        (cons
          (todo/tree/redash (todo/tree/tail (dict-get n "path")))
          (as-str (dict-get (dict-get n "body") "title"))))
      (tree/list (todo/tree/prefix "todo-item") 1 500))))

(define standup/join-lines
  (lambda (parts)
    (if
      (null? parts)
      ""
      (list/foldl
        (lambda (acc p) (string-append acc (string-append "\n" p)))
        (car parts)
        (cdr parts)))))

(define standup/last-other
  (lambda (l)
    (if (null? l)
        ()
        (let ((rest (standup/last-other (cdr l))))
          (if (null? rest)
              (if (string-eq (as-str (dict-get (cdr (car l)) "from")) "gregor")
                  ()
                  (car l))
              rest)))))

(define standup/lobby-line
  (lambda ()
    (let ((h (chat/history "lobby")))
      (let ((lastf (standup/last-other h)))
        (if (null? lastf)
            "Lobby: quiet"
            (string-append
              "Lobby: "
              (as-str (standup/count h))
              " msgs; last "
              (as-str (dict-get (cdr lastf) "from"))
              ": "
              (standup/short (as-str (dict-get (cdr lastf) "body")) 80)))))))

(define standup/moved-line
  (lambda ()
    (guard
      (lambda (e) "")
      (let*
        ((y (as-str (date-add-days -1)))
          (evs (tree/list (todo/tree/prefix "todo-event") 1 500))
          (dim
            (list/filter
              (lambda (n)
                (string-eq (standup/date-prefix (dict-get n "created_at")) y))
              evs))
          (index (standup/item-index)))
        (if
          (null? dim)
          ""
          (string-append
            (string-append "\nMoved yesterday (" y "):\n")
            (standup/join-lines
              (list/map
                (lambda (n)
                  (let*
                    ((b (dict-get n "body"))
                      (id (as-str (dict-get b "item_id"))))
                    (string-append
                      "- " (standup/title-of id index)
                      ": " (as-str (dict-get b "from_state"))
                      " -> " (as-str (dict-get b "to_state")))))
                dim))))))))

(define standup/ntfy-push
  (lambda (body)
    (seq
      (load-library "ntfy")
      (ntfy/send "Standup" body))))

(define standup/review-line
  (lambda ()
    (guard
      (lambda (e) "")
      (seq
        (standup/ensure-workstreams)
        (let*
          ((index (standup/item-index))
            (cards (workstreams/rollup "HUMAN-REVIEW")))
          (if
            (null? cards)
            ""
            (string-append
              "\nReady for human review:\n"
              (standup/join-lines
                (list/map
                  (lambda (c)
                    (string-append
                      "- " (standup/title-of (car c) index)
                      " (" (cdr c) ")"))
                  cards)))))))))

(define standup/run
  (lambda ()
    (seq
      (schedule "gregor-standup" "0 15 * * *" "standup/run" "[]")
      (let ((b (standup/brief)))
        (seq
          (let ((resp (standup/ntfy-push b)))
            (if (dict-get resp "ok") resp (error "ntfy push failed")))
          (guard (lambda (e) e) (chat/say-in "lobby" b))
          (string-append b "\npush ok"))))))

(define standup/short
  (lambda (s n)
    (if (> (string-length s) n)
        (string-append (substring s 0 n) "...")
        s)))

(define standup/stream-line
  (lambda (stream)
    "One stream's summary: title, non-zero column counts, open total, and a
     STALE n flag when any open card is >3d old."
    (let*
      ((lid (car stream))
        (title (cdr stream))
        (counts (workstreams/column-counts lid))
        (open (workstreams/len (workstreams/open-items lid)))
        (stale (workstreams/len (workstreams/stale-items lid))))
      (string-append
        "• " title ": "
        (if (null? counts) "no open work" (workstreams/counts-line counts))
        " (" (as-str open) " open)"
        (if (> stale 0) (string-append " · STALE " (as-str stale)) "")))))

(define standup/title-of
  (lambda (id index)
    "Title for a card id from the (id . title) index, else the id itself."
    (if
      (null? index)
      (as-str id)
      (if
        (string-eq (as-str (car (car index))) (as-str id))
        (as-str (cdr (car index)))
        (standup/title-of id (cdr index))))))

(define standup/titles
  (lambda (l n)
    (if (null? l)
        ""
        (if (< n 1)
            ""
            (let ((t (string-append
                       "- "
                       (standup/short
                         (as-str (dict-get (cdr (car l)) "title")) 46))))
              (if (if (null? (cdr l)) #t (< n 2))
                  t
                  (string-append t "\n" (standup/titles (cdr l) (- n 1)))))))))

(define standup/todo-line
  (lambda ()
    (let ((open (standup/filter-open (rows 'todo-item 200))))
      (string-append
        "Open cards: "
        (as-str (standup/count open))
        (if (> (standup/count open) 0)
            (string-append "\n" (standup/titles open 3))
            "")))))

(define standup/workstreams-line
  (lambda ()
    (guard
      (lambda (e) "")
      (seq
        (standup/ensure-workstreams)
        (let ((streams (workstreams/stream-rows)))
          (if
            (null? streams)
            ""
            (string-append
              "\nWorkstreams:\n"
              (standup/join-lines (list/map standup/stream-line streams)))))))))
