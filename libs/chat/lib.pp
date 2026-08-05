;; package chat -- exported from live branch system/main
;; Sync: scripts/pp_sync.py pull|push

(define chat/all-messages
  (lambda
    ()
    "Return up to 200 chat-message rows."
    (rows (quote chat-message) 200)))

(define chat/alphabet
  " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~")

(define chat/can-access?
  (lambda
    (room-id)
    "Check if current user can access a chat room."
    (let
      ((rid (as-str room-id)) (me (as-str (whoami))))
      (if
        (string-eq rid chat/lobby-id)
        #t
        (let
          ((row (chat/find-room rid)))
          (if
            (null? row)
            #f
            (let
              ((f (chat/row-fields row)))
              (if
                (string-eq (as-str (dict-get f "kind")) "lobby")
                #t
                (chat/member? (dict-get f "members") me)))))))))

(define chat/char-rank
  (lambda
    (c)
    "Get the rank (index) of a character in the alphabet."
    (chat/char-rank-loop
      c
      chat/alphabet
      0
      (string-length chat/alphabet))))

(define chat/char-rank-loop
  (lambda
    (c alphabet i n)
    "Tail-recursive helper for char-rank."
    (if
      (>= i n)
      999
      (if
        (string-eq (substring alphabet i 1) c)
        i
        (chat/char-rank-loop c alphabet (+ i 1) n)))))

(define chat/dm
  (lambda
    (peer)
    "Get or create a DM room id for a peer."
    (let
      ((them (as-str peer)) (me (as-str (whoami))))
      (if
        (blank? them)
        ""
        (if
          (string-eq them me)
          ""
          (let
            ((rid (chat/dm-room-id them))
              (found (chat/find-room (chat/dm-room-id them))))
            (if
              (null? found)
              (seq
                (create
                  (quote chat-room)
                  (dict-set*
                    "{}"
                    (list
                      "kind"
                      "dm"
                      "room_id"
                      rid
                      "title"
                      (string-append "DM " me "/" them)
                      "members"
                      (chat/members-json me them)
                      "created_by"
                      me
                      "created_at"
                      "")))
                rid)
              rid)))))))

(define chat/dm-list-li
  (lambda
    (row)
    "Render a DM thread as an HTML list item."
    (let
      ((f (chat/row-fields row)) (me (as-str (whoami))))
      (let
        ((title (as-str (dict-get f "title")))
          (rid (as-str (dict-get f "room_id"))))
        (let
          ((peer (chat/dm-peer-from-title title me)))
          (html/li
            (html/attr "class" "card")
            (html/p*
              ""
              (html/a
                (string-append "/chat/dm?peer=" peer)
                (html/text peer)))))))))

(define chat/dm-list-page
  (lambda
    ()
    "Render the DM list page with start-DM form."
    (html/page
      (list
        (html/h1 "Direct messages")
        (html/p* "" (html/a "/chat" "Lobby"))
        (html/h2 "Start DM")
        (html/form
          "GET"
          "/chat/dm"
          ""
          (html/join
            (list
              (html/label (html/attr "for" "dm-peer") "Peer username")
              (html/text-input
                "peer"
                (html/attrs
                  (list
                    (html/attr "id" "dm-peer")
                    (html/attr "required" "required")
                    (html/attr "placeholder" "alice"))))
              (html/button (html/attr "type" "submit") "Open"))))
        (html/h2 "Your threads")
        (let
          ((xs (chat/dms)))
          (if
            (null? xs)
            (html/p* (html/attr "class" "muted") "No DMs yet.")
            (html/ul "" (html/join (list/map chat/dm-list-li xs)))))))))

(define chat/dm-main-html
  (lambda
    (peer)
    "Render the DM main area with message form and history."
    (html/div
      (html/attr "id" "dm-main")
      (html/join
        (list
          (html/div
            (html/attr "class" "card")
            (html/form
              "POST"
              (string-append "/chat/dm?peer=" peer)
              (html/attrs
                (list
                  (html/attr
                    "hx-post"
                    (string-append "/chat/dm?peer=" peer))
                  (html/attr "hx-target" "#dm-main")
                  (html/attr "hx-swap" "innerHTML")
                  (html/attr
                    "hx-on::after-request"
                    "if(event.detail.successful){this.reset()}")))
              (html/join
                (list
                  (html/hidden "peer" peer)
                  (html/label (html/attr "for" "dm-body") "Message")
                  (html/textarea
                    "body"
                    (html/attrs
                      (list
                        (html/attr "id" "dm-body")
                        (html/attr "rows" "2")
                        (html/attr "required" "required")
                        (html/attr "maxlength" "4000")))
                    "")
                  (html/button (html/attr "type" "submit") "Send")))))
          (chat/dm-messages-html peer))))))

(define chat/dm-messages-html
  (lambda
    (peer)
    "Render DM message history for a peer."
    (let
      ((rid (chat/dm peer))
        (xs (chat/history (chat/dm peer))))
      (html/div
        (html/attr "id" "dm-messages")
        (if
          (null? xs)
          (html/p* (html/attr "class" "muted") "No messages yet.")
          (html/ul
            (html/attr "class" "chat-list")
            (html/join (list/map chat/msg-li xs))))))))

(define chat/dm-page
  (lambda
    ()
    "DM route handler: list, open, or post to a DM thread."
    (let
      ((peer (as-str (request-param "peer"))))
      (if
        (blank? peer)
        (chat/dm-list-page)
        (if
          (string-eq (request-method) "POST")
          (seq
            (chat/say-dm peer (request-param "body"))
            (if
              (hx-request?)
              (render-html (chat/dm-main-html peer))
              (redirect (string-append "/chat/dm?peer=" peer))))
          (html/page
            (list
              (html/h1 (string-append "DM with " peer))
              (html/p*
                ""
                (html/join
                  (list
                    (html/a "/chat" "Lobby")
                    (html/span "" " · ")
                    (html/a "/chat/dm" "All DMs"))))
              (chat/dm-main-html peer))))))))

(define chat/dm-peer-from-title
  (lambda
    (title me)
    "Extract peer username from a DM room title."
    (let
      ((t (as-str title)) (prefix "DM "))
      (if
        (string-contains? t prefix)
        (let
          ((rest (substring t 3)))
          (if
            (string-contains? rest (string-append me "/"))
            (substring rest (+ (string-length me) 1))
            (if
              (string-contains? rest (string-append "/" me))
              (substring
                rest
                0
                (- (string-length rest) (+ (string-length me) 1)))
              rest)))
        t))))

(define chat/dm-room-id
  (lambda
    (peer)
    "Compute the canonical DM room id for two users."
    (let
      ((me (as-str (whoami))) (them (as-str peer)))
      (if
        (chat/str-le? me them)
        (string-append "dm:" me ":" them)
        (string-append "dm:" them ":" me)))))

(define chat/dms
  (lambda
    ()
    "List all DM rooms the current user is a member of."
    (let
      ((me (as-str (whoami))))
      (list/filter
        (lambda
          (row)
          (let
            ((f (chat/row-fields row)))
            (if
              (string-eq (as-str (dict-get f "kind")) "dm")
              (chat/member? (dict-get f "members") me)
              #f)))
        (rows (quote chat-room) 100)))))

(define chat/ensure-lobby
  (lambda
    ()
    "Create the lobby chat-room if it does not exist."
    (let
      ((found (chat/find-room chat/lobby-id)))
      (if
        (null? found)
        (create
          (quote chat-room)
          (dict-set*
            "{}"
            (list
              "kind"
              "lobby"
              "room_id"
              chat/lobby-id
              "title"
              "Lobby"
              "members"
              "[]"
              "created_by"
              (as-str (whoami))
              "created_at"
              "")))
        (chat/row-id found)))))

(define chat/find-room
  (lambda
    (room-id)
    "Find a chat-room row by room-id (scan up to 50)."
    (let
      ((want (as-str room-id)))
      (list/foldl
        (lambda
          (acc row)
          (if
            (null? acc)
            (if
              (string-eq (chat/room-id-of (chat/row-fields row)) want)
              row
              acc)
            acc))
        ()
        (rows (quote chat-room) 50)))))

(define chat/form-html
  (lambda
    ()
    "Render the lobby message form with htmx submit."
    (html/div
      (html/attr "class" "card")
      (html/form
        "POST"
        "/chat"
        (html/attrs
          (list
            (html/attr "hx-post" "/chat")
            (html/attr "hx-target" "#chat-main")
            (html/attr "hx-swap" "innerHTML")
            (html/attr
              "hx-on::after-request"
              "if(event.detail.successful){this.reset()}")))
        (html/join
          (list
            (html/label (html/attr "for" "chat-body") "Message")
            (html/textarea
              "body"
              (html/attrs
                (list
                  (html/attr "id" "chat-body")
                  (html/attr "rows" "2")
                  (html/attr "required" "required")
                  (html/attr
                    "placeholder"
                    "Hello from the lobby…")
                  (html/attr "maxlength" "4000")))
              "")
            (html/button (html/attr "type" "submit") "Send")))))))

(define chat/history
  (lambda
    (room-id)
    "Get messages for a room the user can access."
    (let
      ((want (as-str room-id)))
      (if
        (chat/can-access? want)
        (list/filter
          (lambda
            (row)
            (string-eq (chat/msg-room (chat/row-fields row)) want))
          (chat/all-messages))
        (list)))))

(define chat/lobby
  (lambda
    ()
    "Ensure the lobby exists and return its message history."
    (seq (chat/ensure-lobby) (chat/history chat/lobby-id))))

(define chat/lobby-id
  "lobby")

(define chat/main-html
  (lambda
    ()
    "Render the chat main area: form plus lobby messages."
    (html/div
      (html/attr "id" "chat-main")
      (html/join
        (list
          (chat/form-html)
          (html/h2 "Lobby")
          (chat/messages-html))))))

(define chat/member?
  (lambda
    (members-json user)
    "Check if a user is in a members JSON array string."
    (string-contains? (as-str members-json) (as-str user))))

(define chat/members-json
  (lambda
    (a b)
    "Build a 2-member JSON array string for a DM room."
    (string-append
      "[\""
      (as-str a)
      "\",\""
      (as-str b)
      "\"]")))

(define chat/messages-html
  (lambda
    ()
    "Render lobby messages with auto-refresh htmx polling."
    (let
      ((xs (chat/lobby)))
      (html/div
        (html/attrs
          (list
            (html/attr "id" "chat-messages")
            (html/attr "hx-get" "/chat/partial")
            (html/attr "hx-trigger" "every 4s")
            (html/attr "hx-swap" "innerHTML")))
        (if
          (null? xs)
          (html/p*
            (html/attr "class" "muted")
            "Say hi to the box.")
          (html/ul
            (html/attr "class" "chat-list")
            (html/join (list/map chat/msg-li xs))))))))

(define chat/msg-body
  (lambda
    (fields)
    "Extract body field from a message fields dict."
    (as-str (dict-get fields "body"))))

(define chat/msg-from
  (lambda
    (fields)
    "Extract from field from a message fields dict."
    (as-str (dict-get fields "from"))))

(define chat/msg-li
  (lambda
    (row)
    "Render a chat message as an HTML list item."
    (let
      ((f (chat/row-fields row)))
      (html/li
        (html/attr "class" "card chat-msg")
        (html/join
          (list
            (html/p*
              ""
              (html/join
                (list
                  (html/strong (chat/msg-from f))
                  (html/span (html/attr "class" "muted") " · "))))
            (html/p (html/text (chat/msg-body f)))))))))

(define chat/msg-room
  (lambda
    (fields)
    "Extract room_id field from a message fields dict."
    (as-str (dict-get fields "room_id"))))

(define chat/page
  (lambda
    ()
    "Chat lobby route handler: GET renders page, POST sends message."
    (if
      (string-eq (request-method) "POST")
      (seq
        (chat/say (request-param "body"))
        (if
          (hx-request?)
          (render-html (chat/main-html))
          (redirect "/chat")))
      (html/page
        (list
          (html/h1 "Chat")
          (html/p*
            (html/attr "class" "muted")
            (string-append
              "Lobby for everyone on this box. You are "
              (as-str (whoami))
              "."))
          (chat/main-html))))))

(define chat/partial
  (lambda
    ()
    "Render lobby messages as an htmx partial for polling."
    (render-html
      (let
        ((xs (chat/lobby)))
        (if
          (null? xs)
          (html/p*
            (html/attr "class" "muted")
            "Say hi to the box.")
          (html/ul
            (html/attr "class" "chat-list")
            (html/join (list/map chat/msg-li xs))))))))

(define chat/room-id-of
  (lambda
    (fields)
    "Extract room_id field from a chat-room fields dict."
    (as-str (dict-get fields "room_id"))))

(define chat/row-fields
  (lambda
    (row)
    "Get fields dict from a chat row."
    (cdr row)))

(define chat/row-id
  (lambda
    (row)
    "Get the id from a chat row."
    (as-str (car row))))

(define chat/say
  (lambda
    (body)
    "Say a message in the lobby."
    (chat/say-in chat/lobby-id body)))

(define chat/say-dm
  (lambda
    (peer body)
    "Say a direct message to a peer user."
    (let
      ((rid (chat/dm peer)))
      (if
        (blank? rid)
        "chat: bad peer"
        (chat/say-in rid body)))))

(define chat/say-in
  (lambda
    (room-id body)
    "Say a message in a specific chat room (with access check)."
    (let
      ((text (as-str body))
        (user (as-str (whoami)))
        (rid (as-str room-id)))
      (if
        (blank? text)
        ""
        (if
          (> (string-length text) 4000)
          "chat: body too long (max 4000)"
          (if
            (chat/can-access? rid)
            (seq
              (if
                (string-eq rid chat/lobby-id)
                (chat/ensure-lobby)
                ())
              (create
                (quote chat-message)
                (dict-set*
                  "{}"
                  (list
                    "room_id"
                    rid
                    "from"
                    user
                    "body"
                    text
                    "created_at"
                    "")))
              text)
            "chat: not a member of room"))))))

(define chat/str-le?
  (lambda
    (a b)
    "Lexicographic string less-than-or-equal via char ranks."
    (if
      (string-eq a b)
      #t
      (if
        (string-eq a "")
        #t
        (if
          (string-eq b "")
          #f
          (let
            ((ca (substring a 0 1)) (cb (substring b 0 1)))
            (if
              (string-eq ca cb)
              (chat/str-le? (substring a 1) (substring b 1))
              (<= (chat/char-rank ca) (chat/char-rank cb)))))))))

(define chat/users
  (lambda
    ()
    "List all usernames on the image."
    (list-usernames)))

