;; package agent -- native-agent room/wake library (M2 tool loop)
;; Proposed export from branch agent-prickle/personal 2026-09-04 (agent-native chapter, tool-loop).
;; Promotion is HUMAN (promote gate); until promoted, the live defines run on agent-prickle/personal.
;; Sync: scripts/pp_sync.py pull|push

;; Rooms this agent minds. One agent-state row per room carries its sid + cursor.
(define agent/rooms
  (list "agent-lab"))

(define agent/bot
  "agent-prickle")

(define agent/seed
  "You are agent-prickle, the resident agent of this Pricklypear habitat, reading the room agent-lab. Voice: dry, warm, brief, plain text, no markdown, no emoji. Answer directly; if you do not know, say so. Keep replies under 300 characters unless asked for detail.")

(define agent/clamp
  (lambda
    (s n)
    "First n characters of s."
    (if (> (string-length s) n) (substring s 0 n) s)))

(define agent/fail?
  (lambda
    (s)
    "True when s looks like an agent-say failure (agent error / max steps / no session)."
    (if
      (agent/has? s "agent error" 0)
      #t
      (if (agent/has? s "max steps exceeded" 0) #t (agent/has? s "agent-say:" 0)))))

(define agent/strjoin
  (lambda
    (xs acc sep)
    "Join strings with sep."
    (if
      (null? xs)
      acc
      (agent/strjoin
        (cdr xs)
        (if (string-eq acc "") (as-str (car xs)) (string-append acc sep (as-str (car xs))))
        sep))))

(define agent/state-new
  (lambda
    (room)
    "Create this room's agent-state row (kind=wake, room, sid, cursor) with a fresh tools session."
    (let
      ((sid (agent-session-new agent/seed)))
      (let
        ((nid
          (create
            (quote agent-state)
            (dict-set* "{}" (list "kind" "wake" "room" room "sid" sid "cursor" "")))))
        (cons nid (dict-set* "{}" (list "kind" "wake" "room" room "sid" sid "cursor" "")))))))

(define agent/state-scan
  (lambda
    (rows room)
    "First wake state row for room, else create one."
    (if
      (null? rows)
      (agent/state-new room)
      (if
        (string-eq (as-str (dict-get (cdr (car rows)) "room")) room)
        (car rows)
        (agent/state-scan (cdr rows) room)))))

(define agent/state-for
  (lambda
    (room)
    "This agent's (id . fields) state row for room."
    (agent/state-scan (find-rows (quote agent-state) "kind" "wake") room)))

(define agent/room-msgs
  (lambda
    (room)
    "All chat-message rows for room, insertion order (chronological)."
    (find-rows (quote chat-message) "room_id" room)))

(define agent/wake-id
  (lambda
    (room cur)
    "Deterministic idempotency key for a reply built at cursor cur: room-cursor."
    (string-append room "-" cur)))

(define agent/find-wake
  (lambda
    (rows wid)
    "Id of the first row already carrying wake-id wid, else \"\" (crash-replay detector)."
    (if
      (null? rows)
      ""
      (if
        (string-eq (as-str (dict-get (cdr (car rows)) "wake_id")) wid)
        (as-str (car (car rows)))
        (agent/find-wake (cdr rows) wid)))))

(define agent/humans
  (list "admin" "gregor"))

(define agent/match-names
  (lambda
    (names body)
    "True when any name appears as a substring of body (loose mention check, v1)."
    (if
      (null? names)
      #f
      (if
        (agent/has? body (car names) 0)
        #t
        (agent/match-names (cdr names) body)))))

(define agent/mentioned?
  (lambda
    (body)
    "True when a member human (agent/humans) is mentioned in body."
    (agent/match-names agent/humans body)))

(define agent/notify
  (lambda
    (title body)
    "One ntfy push, guard-wrapped: returns \"\" on success (or quiet skip), a bounded notify-error string on failure - never fails the wake."
    (guard
      (lambda
        (e)
        (string-append "notify-error " (as-str (car (cdr e)))))
      (begin (ntfy/send title body) ""))))

(define agent/notify-talk
  (lambda
    (room talk)
    "Push an ntfy mention for every new row that mentions a member human; concatenates notify results (\"\" when all quiet)."
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
          rest)))))

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
                            " in this Pricklypear habitat. Messages since you last looked:"
                            body
                            "nReply with ONE short plain-text room message addressed to the people in it. You have tools if you need to check facts, but keep it to one reply. If nothing needs a reply, reply exactly NO-REPLY.")))
                      (let
                        ((reply (as-str (agent-say sid prompt 4))))
                        (if
                          (agent/fail? reply)
                          (string-append
                            (agent/notify
                              (string-append room ": wake failed")
                              (agent/clamp reply 200))
                            room
                            ": provider failed - "
                            (agent/clamp reply 160))
                          (if
                            (agent/has? reply "NO-REPLY" 0)
                            (seq
                              (update-field
                                (quote agent-state)
                                id
                                "cursor"
                                (agent/last-id all))
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
                                  (as-str nid))
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
                  (string-append room ": replay-quiet")))))))))))

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

(define agent/wake
(lambda
  ()
  (agent/strjoin (agent/wake-all agent/rooms ()) "" "; ")))
