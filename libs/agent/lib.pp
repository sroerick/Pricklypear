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

(define agent/wake-room
  (lambda
    (room)
    "One room's tick: quiet (advance nothing, no provider call) or loud (observe, decide, post one wake-id-stamped reply, advance cursor to the reply's own id). Provider failure returns a bounded note WITHOUT advancing - the next tick retries."
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
            (update-field (quote agent-state) id "cursor" (agent/last-id all))
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
                        (update-field (quote agent-state) id "cursor" (agent/last-id all))
                        (string-append room ": quiet-self"))
                      (let*
                        ((transcript (agent/join talk ""))
                          (tl (string-length transcript))
                          (body
                            (if (> tl 3000) (substring transcript (- tl 3000) 3000) transcript))
                          (prompt
                            (string-append
                              "You are agent-prickle, resident agent of room "
                              room
                              " in this Pricklypear habitat. Messages since you last looked:"
                              body
                              "\nReply with ONE short plain-text room message addressed to the people in it. You have tools if you need to check facts, but keep it to one reply. If nothing needs a reply, reply exactly NO-REPLY.")))
                        (let
                          ((reply (as-str (agent-say sid prompt 4))))
                          (if
                            (agent/fail? reply)
                            (string-append room ": provider failed - " (agent/clamp reply 160))
                            (if
                              (agent/has? reply "NO-REPLY" 0)
                              (seq
                                (update-field (quote agent-state) id "cursor" (agent/last-id all))
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
                                  (update-field (quote agent-state) id "cursor" (as-str nid))
                                  (string-append room ": posted")))))))))
                  (seq
                    (update-field (quote agent-state) id "cursor" (agent/last-id all))
                    (string-append room ": replay-quiet"))))))))))))

(define agent/wake-all
  (lambda
    (rooms acc)
    "Run every room's tick under a per-room guard; budget exceeded still propagates (transactional abort)."
    (if
      (null? rooms)
      (agent/reverse acc ())
      (agent/wake-all
        (cdr rooms)
        (cons
          (guard
            (lambda
              (e)
              (string-append (car rooms) ": room-error " (as-str (car (cdr e)))))
            (agent/wake-room (car rooms)))
          acc)))))

(define agent/wake
  (lambda
    ()
    "The cron heartbeat: tick every room in agent/rooms, results joined with \"; \"."
    (agent/strjoin (agent/wake-all agent/rooms ()) "" "; ")))
