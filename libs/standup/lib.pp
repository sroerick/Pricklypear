;; package standup -- daily 09:00 Mountain standup brief (gregor)
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

(define standup/count
  (lambda (l)
    (if (null? l) 0 (+ 1 (standup/count (cdr l))))))

(define standup/short
  (lambda (s n)
    (if (> (string-length s) n)
        (string-append (substring s 0 n) "...")
        s)))

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

(define standup/filter-open
  (lambda (l)
    (if (null? l)
        ()
        (let ((st (as-str (dict-get (cdr (car l)) "state"))))
          (if (if (string-eq st "DONE") #f (string-eq st "PARKED"))
              (standup/filter-open (cdr l))
              (cons (car l) (standup/filter-open (cdr l))))))))

(define standup/todo-line
  (lambda ()
    (let ((open (standup/filter-open (rows 'todo-item 200))))
      (string-append
        "Open cards: "
        (as-str (standup/count open))
        (if (> (standup/count open) 0)
            (string-append "\n" (standup/titles open 3))
            "")))))

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

(define standup/brief
  (lambda ()
    (string-append
      "Standup:\n"
      (guard (lambda (e) "lobby: n/a") (standup/lobby-line))
      "\n"
      (guard (lambda (e) "open cards: n/a") (standup/todo-line))
      "\nhttps://pricklypear.rocks/chat")))

(define standup/ntfy-push
  (lambda (body)
    (seq
      (load-library "ntfy")
      (ntfy/send "Standup" body))))

(define standup/run
  (lambda ()
    (seq
      (schedule "gregor-standup" "0 15 * * *" "standup/run" "[]")
      (let ((b (standup/brief)))
        (seq
          (let ((resp (standup/ntfy-push b)))
            (if (dict-get resp "ok") resp (error "ntfy push failed")))
          (guard (lambda (e) e) (chat/say-in "lobby" b))
          (string-append b "\npush ok")))))))
