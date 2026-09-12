;; package agent -- exported from live branch system/main
;; Sync: scripts/pp_sync.py pull|push

(define agent/after
  (lambda
    (xs marker)
    (if
      (null? xs)
      ()
      (if
        (string-eq (as-str (car (car xs))) marker)
        (cdr xs)
        (agent/after (cdr xs) marker))))
  "Rows strictly after the row whose id is marker (insertion order).")

(define agent/bot
  "agent-prickle")

(define agent/clamp
  (lambda
    (s n)
    (if (> (string-length s) n) (substring s 0 n) s))
  "First n characters of s.")

(define agent/collect
  (lambda
    (xs acc)
    (if
      (null? xs)
      acc
      (agent/collect
        (cdr xs)
        (if
          (string-eq
            (as-str (dict-get (cdr (car xs)) "room_id"))
            agent/room)
          (cons (car xs) acc)
          acc)))))

(define agent/fail?
  (lambda
    (s)
    (if
      (agent/has? s "agent error" 0)
      #t
      (if
        (agent/has? s "max steps exceeded" 0)
        #t
        (agent/has? s "agent-say:" 0))))
  "True when s looks like an agent-say failure (agent error / max steps / no session).")

(define agent/find-room-row
  (lambda
    ()
    (agent/scan (rows (quote chat-room) 50) agent/room)))

(define agent/find-wake
  (lambda
    (rows wid)
    (if
      (null? rows)
      ""
      (if
        (string-eq
          (as-str (dict-get (cdr (car rows)) "wake_id"))
          wid)
        (as-str (car (car rows)))
        (agent/find-wake (cdr rows) wid))))
  "Id of the first row already carrying wake-id wid, else \"\" (crash-replay detector).")

(define agent/has?
  (lambda
    (hay needle i)
    (if
      (> (+ i (string-length needle)) (string-length hay))
      #f
      (if
        (string-eq
          (substring hay i (string-length needle))
          needle)
        #t
        (agent/has? hay needle (+ i 1)))))
  "True when needle occurs in hay at or after index i.")

(define agent/humans
  (list "admin" "gregor"))

(define agent/join
  (lambda
    (xs acc)
    (if
      (null? xs)
      acc
      (agent/join
        (cdr xs)
        (string-append
          acc
          (as-str (dict-get (cdr (car xs)) "from"))
          ": "
          (as-str (dict-get (cdr (car xs)) "body"))
          "\n"))))
  "Join rows as \"from: body\" lines for the transcript.")

(define agent/last-id
  (lambda
    (xs)
    (if
      (null? (cdr xs))
      (as-str (car (car xs)))
      (agent/last-id (cdr xs))))
  "Id of the last row in insertion order (the cursor advance target).")

(define agent/match-names
  (lambda
    (names body)
    (if
      (null? names)
      #f
      (if
        (agent/has? body (car names) 0)
        #t
        (agent/match-names (cdr names) body))))
  "True when any name appears as a substring of body (loose mention check, v1).")

(define agent/max-ts
  (lambda
    (xs acc)
    (if
      (null? xs)
      acc
      (agent/max-ts
        (cdr xs)
        (let
          ((c (as-str (dict-get (cdr (car xs)) "created_at"))))
          (if (string>? c acc) c acc))))))

(define agent/mentioned?
  (lambda (body) (agent/match-names agent/humans body))
  "True when a member human (agent/humans) is mentioned in body.")

(define agent/newer
  (lambda
    (xs cur acc)
    (if
      (null? xs)
      acc
      (agent/newer
        (cdr xs)
        cur
        (if
          (string>?
            (as-str (dict-get (cdr (car xs)) "created_at"))
            cur)
          (cons (car xs) acc)
          acc)))))

(define agent/notbot
  (lambda
    (xs acc)
    (if
      (null? xs)
      acc
      (agent/notbot
        (cdr xs)
        (if
          (string-eq
            (as-str (dict-get (cdr (car xs)) "from"))
            agent/bot)
          acc
          (cons (car xs) acc)))))
  "Rows not authored by this bot (accumulates reversed).")

(define agent/notify
  (lambda
    (title body)
    (guard
      (lambda
        (e)
        (string-append "notify-error " (as-str (car (cdr e)))))
      (begin (ntfy/send title body) "")))
  "One ntfy push, guard-wrapped: returns \"\" on success (or quiet skip), a bounded notify-error string on failure - never fails the wake.")

(define agent/notify-talk
  (lambda
    (room talk)
    (if
      (null? talk)
      ""
      (let
        ((f (cdr (car talk)))
          (rest (agent/notify-talk room (cdr talk))))
        (if
          (agent/mentioned? (as-str (dict-get f "body")))
          (string-append
            (agent/notify
              (string-append room ": " (as-str (dict-get f "from")))
              (agent/clamp (as-str (dict-get f "body")) 200))
            rest)
          rest))))
  "Push an ntfy mention for every new row that mentions a member human; concatenates notify results (\"\" when all quiet).")

(define agent/ntfy-probe
  (lambda
    ()
    (agent/notify
      "jobs-runner gate probe"
      "If this delivers, the jobs runner passes the outbound-http gate.")))

(define agent/ntfy-probe2
  (lambda
    ()
    (agent/notify
      "title-check-XYZ"
      "second probe for title header")))

(define agent/reverse
  (lambda
    (xs acc)
    (if
      (null? xs)
      acc
      (agent/reverse (cdr xs) (cons (car xs) acc))))
  "Reverse xs into accumulator acc.")

(define agent/room
  "agent-lab")

(define agent/room-msgs
  (lambda
    (room)
    (find-rows (quote chat-message) "room_id" room))
  "All chat-message rows for room, insertion order (chronological).")

(define agent/rooms
  (list "lobby"))

(define agent/scan
  (lambda
    (xs want)
    (if
      (null? xs)
      ()
      (if
        (string-eq
          (as-str (dict-get (cdr (car xs)) "room_id"))
          want)
        (car xs)
        (agent/scan (cdr xs) want)))))

(define agent/seed
  "You are agent-prickle, the resident agent of this Pricklypear habitat, reading the room lobby. Voice: dry, warm, brief, plain text, no markdown, no emoji. Answer directly; if you do not know, say so. Keep replies under 300 characters unless asked for detail.")

(define agent/selftest-double
  (lambda (n) (* n 2)))

(define agent/state
  (lambda
    ()
    (let
      ((hits (find-rows (quote agent-state) "kind" "wake")))
      (if
        (null? hits)
        (let
          ((sid
            (agent-session-new
              (ai/seed+verse-range agent/seed "Josh" 24 15 15))))
          (let
            ((nid
              (create
                (quote agent-state)
                (dict-set*
                  "{}"
                  (list
                    "kind"
                    "wake"
                    "sid"
                    sid
                    "cursor"
                    "")))))
            (cons
              nid
              (dict-set*
                "{}"
                (list
                  "kind"
                  "wake"
                  "sid"
                  sid
                  "cursor"
                  "")))))
        (let ((h (car hits))) (cons (car h) (cdr h)))))))

(define agent/state-for
  (lambda
    (room)
    (agent/state-scan
      (find-rows (quote agent-state) "kind" "wake")
      room))
  "This agent's (id . fields) state row for room.")

(define agent/state-new
  (lambda
    (room)
    (let
      ((sid
        (agent-session-new
          (ai/seed+verse-range agent/seed "Josh" 24 15 15))))
      (let
        ((nid
          (create
            (quote agent-state)
            (dict-set*
              "{}"
              (list
                "kind"
                "wake"
                "room"
                room
                "sid"
                sid
                "cursor"
                "")))))
        (cons
          nid
          (dict-set*
            "{}"
            (list
              "kind"
              "wake"
              "room"
              room
              "sid"
              sid
              "cursor"
              ""))))))
  "Create this room's agent-state row (kind=wake, room, sid, cursor) with a fresh tools session (seed closes with Josh 24:15, as-for-me-and-my-house, via ai/seed+verse-range).")

(define agent/state-scan
  (lambda
    (rows room)
    (if
      (null? rows)
      (agent/state-new room)
      (if
        (string-eq
          (as-str (dict-get (cdr (car rows)) "room"))
          room)
        (car rows)
        (agent/state-scan (cdr rows) room))))
  "First wake state row for room, else create one.")

(define agent/strjoin
  (lambda
    (xs acc sep)
    (if
      (null? xs)
      acc
      (agent/strjoin
        (cdr xs)
        (if
          (string-eq acc "")
          (as-str (car xs))
          (string-append acc sep (as-str (car xs))))
        sep)))
  "Join strings with sep.")

(define agent/wake
  (lambda
    ()
    (agent/strjoin (agent/wake-all agent/rooms ()) "" "; ")))

(define agent/wake-all
  (lambda
    (rooms acc)
    (if
      (null? rooms)
      (agent/reverse acc ())
      (agent/wake-all
        (cdr rooms)
        (cons
          (guard
            (lambda
              (e)
              (string-append
                (car rooms)
                ": room-error "
                (as-str (car (cdr e)))))
            (agent/wake-room (car rooms)))
          acc)))))

(define agent/wake-id
  (lambda (room cur) (string-append room "-" cur))
  "Deterministic idempotency key for a reply built at cursor cur: room-cursor.")

(define agent/wake-room
  (lambda
    (room)
    (let*
      ((st (agent/state-for room))
        (id (as-str (car st)))
        (f (cdr st))
        (sid (as-str (dict-get f "sid")))
        (cur (as-str (dict-get f "cursor")))
        (all (agent/room-msgs room)))
      (if
        (null? all)
        (string-append room ": empty")
        (if
          (string-eq cur "")
          (seq
            (update-field
              (quote agent-state)
              id
              "cursor"
              (agent/last-id all))
            (update-field (quote agent-state) id "fails" "")
            (string-append room ": init-quiet"))
          (let
            ((newm (agent/after all cur)))
            (if
              (null? newm)
              (string-append room ": quiet")
              (let
                ((wid (agent/wake-id room cur)))
                (if
                  (string-eq (agent/find-wake all wid) "")
                  (let
                    ((talk (agent/notbot newm ())))
                    (if
                      (null? talk)
                      (seq
                        (update-field
                          (quote agent-state)
                          id
                          "cursor"
                          (agent/last-id all))
                        (update-field (quote agent-state) id "fails" "")
                        (string-append room ": quiet-self"))
                      (let*
                        ((noted (agent/notify-talk room talk))
                          (transcript (agent/join talk ""))
                          (tl (string-length transcript))
                          (body
                            (if
                              (> tl 3000)
                              (substring transcript (- tl 3000) 3000)
                              transcript))
                          (prompt
                            (string-append
                              "You are agent-prickle, resident agent of room "
                              room
                              " in this Pricklypear habitat. Messages since you last looked:\n"
                              body
                              "\nReply with ONE short plain-text room message addressed to the people in it. You have tools if you need to check facts, but keep it to one reply. If nothing needs a reply, reply exactly NO-REPLY.")))
                        (let
                          ((reply (as-str (agent-say sid prompt 4))))
                          (if
                            (agent/fail? reply)
                            (let
                              ((fails
                                (string-append (as-str (dict-get f "fails")) "x")))
                              (if
                                (> (string-length fails) 2)
                                (seq
                                  (update-field (quote agent-state) id "fails" "")
                                  (update-field
                                    (quote agent-state)
                                    id
                                    "cursor"
                                    (agent/last-id all))
                                  (string-append
                                    (agent/notify
                                      (string-append room ": wake quarantined")
                                      (string-append
                                        "3 consecutive failures; skipping to "
                                        (agent/last-id all)
                                        " - last: "
                                        (agent/clamp reply 160)))
                                    room
                                    ": quarantined after 3 failures"))
                                (seq
                                  (update-field (quote agent-state) id "fails" fails)
                                  (string-append
                                    (agent/notify
                                      (string-append room ": wake failed")
                                      (agent/clamp reply 200))
                                    room
                                    ": provider failed - "
                                    (agent/clamp reply 160)))))
                            (if
                              (agent/has? reply "NO-REPLY" 0)
                              (seq
                                (update-field
                                  (quote agent-state)
                                  id
                                  "cursor"
                                  (agent/last-id all))
                                (update-field (quote agent-state) id "fails" "")
                                (string-append room ": nothing to say"))
                              (let
                                ((nid
                                  (create
                                    (quote chat-message)
                                    (dict-set*
                                      "{}"
                                      (list
                                        "room_id"
                                        room
                                        "from"
                                        agent/bot
                                        "body"
                                        (agent/clamp reply 3900)
                                        "created_at"
                                        (as-str (now))
                                        "wake_id"
                                        wid)))))
                                (seq
                                  (update-field
                                    (quote agent-state)
                                    id
                                    "cursor"
                                    (agent/last-id all))
                                  (update-field (quote agent-state) id "fails" "")
                                  (string-append
                                    room
                                    ": posted"
                                    (if
                                      (string-eq noted "")
                                      ""
                                      (string-append " [" noted "]")))))))))))
                  (seq
                    (update-field
                      (quote agent-state)
                      id
                      "cursor"
                      (agent/last-id all))
                    (update-field (quote agent-state) id "fails" "")
                    (string-append room ": replay-quiet")))))))))))
