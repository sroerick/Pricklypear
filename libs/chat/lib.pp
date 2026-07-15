;; Multi-user chat — site lobby (multi-user.chat phase 1).
;; Install: (load-library "chat") — also seeded on system/main.
;; DMs (phase 2) live alongside; lobby is enough for first smoke.

(deftype 'chat-room 'kind 'room_id 'title 'members 'created_by 'created_at)
(deftype 'chat-message 'room_id 'from 'body 'created_at)

(define chat/blank?
  (lambda (s) (if (null? s) #t (string-eq (as-str s) ""))))

(define chat/row-id (lambda (row) (as-str (car row))))
(define chat/row-fields (lambda (row) (cdr row)))

(define chat/msg-from
  (lambda (fields) (as-str (dict-get fields "from"))))
(define chat/msg-body
  (lambda (fields) (as-str (dict-get fields "body"))))
(define chat/msg-room
  (lambda (fields) (as-str (dict-get fields "room_id"))))
(define chat/room-id-of
  (lambda (fields) (as-str (dict-get fields "room_id"))))

(define chat/lobby-id "lobby")

(define chat/find-room
  (lambda (room-id)
    (let ((want (as-str room-id)))
      (list/foldl
        (lambda (acc row)
          (if (null? acc)
              (if (string-eq (chat/room-id-of (chat/row-fields row)) want)
                  row
                  acc)
              acc))
        ()
        (rows 'chat-room 50)))))

(define chat/ensure-lobby
  (lambda ()
    (let ((found (chat/find-room chat/lobby-id)))
      (if (null? found)
          (create 'chat-room
            (dict-set* "{}"
              (list
                "kind" "lobby"
                "room_id" chat/lobby-id
                "title" "Lobby"
                "members" "[]"
                "created_by" (as-str (whoami))
                "created_at" "")))
          (chat/row-id found)))))

(define chat/all-messages
  (lambda () (rows 'chat-message 200)))

(define chat/can-access?
  (lambda (room-id)
    (let ((rid (as-str room-id))
          (me (as-str (whoami))))
      (if (string-eq rid chat/lobby-id)
          #t
          (let ((row (chat/find-room rid)))
            (if (null? row)
                #f
                (let ((f (chat/row-fields row)))
                  (if (string-eq (as-str (dict-get f "kind")) "lobby")
                      #t
                      (chat/member? (dict-get f "members") me)))))))))

(define chat/history
  (lambda (room-id)
    (let ((want (as-str room-id)))
      (if (chat/can-access? want)
          (list/filter
            (lambda (row)
              (string-eq (chat/msg-room (chat/row-fields row)) want))
            (chat/all-messages))
          (list)))))

(define chat/lobby
  (lambda ()
    (seq
      (chat/ensure-lobby)
      (chat/history chat/lobby-id))))

(define chat/say-in
  (lambda (room-id body)
    (let ((text (as-str body))
          (user (as-str (whoami)))
          (rid (as-str room-id)))
      (if (chat/blank? text)
          ""
          (if (> (string-length text) 4000)
              "chat: body too long (max 4000)"
              (if (chat/can-access? rid)
                  (seq
                    (if (string-eq rid chat/lobby-id)
                        (chat/ensure-lobby)
                        ())
                    (create 'chat-message
                      (dict-set* "{}"
                        (list
                          "room_id" rid
                          "from" user
                          "body" text
                          "created_at" "")))
                    text)
                  "chat: not a member of room"))))))

(define chat/say
  (lambda (body) (chat/say-in chat/lobby-id body)))

;; --- DMs (phase 2) ---
;; Room id: dm:user1:user2 with user1 <= user2 (lexicographic via char ranks).

(define chat/alphabet
  " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~")

(define chat/char-rank-loop
  (lambda (c alphabet i n)
    (if (>= i n)
        999
        (if (string-eq (substring alphabet i 1) c)
            i
            (chat/char-rank-loop c alphabet (+ i 1) n)))))

(define chat/char-rank
  (lambda (c)
    (chat/char-rank-loop c chat/alphabet 0 (string-length chat/alphabet))))

(define chat/str-le?
  (lambda (a b)
    (if (string-eq a b)
        #t
        (if (string-eq a "")
            #t
            (if (string-eq b "")
                #f
                (let ((ca (substring a 0 1))
                      (cb (substring b 0 1)))
                  (if (string-eq ca cb)
                      (chat/str-le? (substring a 1) (substring b 1))
                      (<= (chat/char-rank ca) (chat/char-rank cb)))))))))

(define chat/dm-room-id
  (lambda (peer)
    (let ((me (as-str (whoami)))
          (them (as-str peer)))
      (if (chat/str-le? me them)
          (string-append "dm:" me ":" them)
          (string-append "dm:" them ":" me)))))

(define chat/members-json
  (lambda (a b)
    (string-append "[\"" (as-str a) "\",\"" (as-str b) "\"]")))

(define chat/dm
  (lambda (peer)
    (let ((them (as-str peer))
          (me (as-str (whoami))))
      (if (chat/blank? them)
          ""
          (if (string-eq them me)
          ""
          (let ((rid (chat/dm-room-id them))
                (found (chat/find-room (chat/dm-room-id them))))
            (if (null? found)
                (seq
                  (create 'chat-room
                    (dict-set* "{}"
                      (list
                        "kind" "dm"
                        "room_id" rid
                        "title" (string-append "DM " me "/" them)
                        "members" (chat/members-json me them)
                        "created_by" me
                        "created_at" "")))
                  rid)
                rid)))))))

(define chat/member?
  (lambda (members-json user)
    (string-contains? (as-str members-json) (as-str user))))

(define chat/dms
  (lambda ()
    (let ((me (as-str (whoami))))
      (list/filter
        (lambda (row)
          (let ((f (chat/row-fields row)))
            (if (string-eq (as-str (dict-get f "kind")) "dm")
                (chat/member? (dict-get f "members") me)
                #f)))
        (rows 'chat-room 100)))))

(define chat/users
  (lambda () (list-usernames)))

(define chat/say-dm
  (lambda (peer body)
    (let ((rid (chat/dm peer)))
      (if (chat/blank? rid)
          "chat: bad peer"
          (chat/say-in rid body)))))

;; --- HTML ---

(define chat/msg-li
  (lambda (row)
    (let ((f (chat/row-fields row)))
      (html/li (html/attr "class" "card chat-msg")
        (html/join
          (list
            (html/p* ""
              (html/join
                (list
                  (html/strong (chat/msg-from f))
                  (html/span (html/attr "class" "muted") " · "))))
            (html/p (html/text (chat/msg-body f)))))))))

(define chat/messages-html
  (lambda ()
    (let ((xs (chat/lobby)))
      (html/div
        (html/attrs
          (list
            (html/attr "id" "chat-messages")
            (html/attr "hx-get" "/chat/partial")
            (html/attr "hx-trigger" "every 4s")
            (html/attr "hx-swap" "innerHTML")))
        (if (null? xs)
            (html/p* (html/attr "class" "muted") "Say hi to the box.")
            (html/ul (html/attr "class" "chat-list")
              (html/join (list/map chat/msg-li xs))))))))

(define chat/form-html
  (lambda ()
    (html/div (html/attr "class" "card")
      (html/form "POST" "/chat"
        (html/attrs
          (list
            (html/attr "hx-post" "/chat")
            (html/attr "hx-target" "#chat-main")
            (html/attr "hx-swap" "innerHTML")
            (html/attr "hx-on::after-request"
              "if(event.detail.successful){this.reset()}")))
        (html/join
          (list
            (html/label (html/attr "for" "chat-body") "Message")
            (html/textarea "body"
              (html/attrs
                (list
                  (html/attr "id" "chat-body")
                  (html/attr "rows" "2")
                  (html/attr "required" "required")
                  (html/attr "placeholder" "Hello from the lobby…")
                  (html/attr "maxlength" "4000")))
              "")
            (html/button (html/attr "type" "submit") "Send")))))))

(define chat/main-html
  (lambda ()
    (html/div (html/attr "id" "chat-main")
      (html/join
        (list
          (chat/form-html)
          (html/h2 "Lobby")
          (chat/messages-html))))))

(define chat/partial
  (lambda ()
    (render-html
      (let ((xs (chat/lobby)))
        (if (null? xs)
            (html/p* (html/attr "class" "muted") "Say hi to the box.")
            (html/ul (html/attr "class" "chat-list")
              (html/join (list/map chat/msg-li xs))))))))

(define chat/page
  (lambda ()
    (if (string-eq (request-method) "POST")
        (seq
          (chat/say (request-param "body"))
          (if (hx-request?)
              (render-html (chat/main-html))
              (redirect "/chat")))
        (html/page
          (list
            (html/h1 "Chat")
            (html/p* (html/attr "class" "muted")
              (string-append
                "Lobby for everyone on this box. You are "
                (as-str (whoami))
                "."))
            (chat/main-html))))))

(define chat/dm-peer-from-title
  (lambda (title me)
    ;; title is "DM a/b" — pick the other username
    (let ((t (as-str title))
          (prefix "DM "))
      (if (string-contains? t prefix)
          (let ((rest (substring t 3)))
            (if (string-contains? rest (string-append me "/"))
                (substring rest (+ (string-length me) 1))
                (if (string-contains? rest (string-append "/" me))
                    (substring rest 0 (- (string-length rest) (+ (string-length me) 1)))
                    rest)))
          t))))

(define chat/dm-list-li
  (lambda (row)
    (let ((f (chat/row-fields row))
          (me (as-str (whoami))))
      (let ((title (as-str (dict-get f "title")))
            (rid (as-str (dict-get f "room_id"))))
        (let ((peer (chat/dm-peer-from-title title me)))
          (html/li (html/attr "class" "card")
            (html/p* ""
              (html/a (string-append "/chat/dm?peer=" peer)
                (html/text peer)))))))))

(define chat/dm-list-page
  (lambda ()
    (html/page
      (list
        (html/h1 "Direct messages")
        (html/p* "" (html/a "/chat" "Lobby"))
        (html/h2 "Start DM")
        (html/form "GET" "/chat/dm" ""
          (html/join
            (list
              (html/label (html/attr "for" "dm-peer") "Peer username")
              (html/text-input "peer"
                (html/attrs
                  (list
                    (html/attr "id" "dm-peer")
                    (html/attr "required" "required")
                    (html/attr "placeholder" "alice"))))
              (html/button (html/attr "type" "submit") "Open"))))
        (html/h2 "Your threads")
        (let ((xs (chat/dms)))
          (if (null? xs)
              (html/p* (html/attr "class" "muted") "No DMs yet.")
              (html/ul ""
                (html/join (list/map chat/dm-list-li xs)))))))))

(define chat/dm-messages-html
  (lambda (peer)
    (let ((rid (chat/dm peer))
          (xs (chat/history (chat/dm peer))))
      (html/div (html/attr "id" "dm-messages")
        (if (null? xs)
            (html/p* (html/attr "class" "muted") "No messages yet.")
            (html/ul (html/attr "class" "chat-list")
              (html/join (list/map chat/msg-li xs))))))))

(define chat/dm-main-html
  (lambda (peer)
    (html/div (html/attr "id" "dm-main")
      (html/join
        (list
          (html/div (html/attr "class" "card")
            (html/form "POST" (string-append "/chat/dm?peer=" peer)
              (html/attrs
                (list
                  (html/attr "hx-post" (string-append "/chat/dm?peer=" peer))
                  (html/attr "hx-target" "#dm-main")
                  (html/attr "hx-swap" "innerHTML")
                  (html/attr "hx-on::after-request"
                    "if(event.detail.successful){this.reset()}")))
              (html/join
                (list
                  (html/hidden "peer" peer)
                  (html/label (html/attr "for" "dm-body") "Message")
                  (html/textarea "body"
                    (html/attrs
                      (list
                        (html/attr "id" "dm-body")
                        (html/attr "rows" "2")
                        (html/attr "required" "required")
                        (html/attr "maxlength" "4000")))
                    "")
                  (html/button (html/attr "type" "submit") "Send")))))
          (chat/dm-messages-html peer))))))

(define chat/dm-page
  (lambda ()
    (let ((peer (as-str (request-param "peer"))))
      (if (chat/blank? peer)
          (chat/dm-list-page)
          (if (string-eq (request-method) "POST")
              (seq
                (chat/say-dm peer (request-param "body"))
                (if (hx-request?)
                    (render-html (chat/dm-main-html peer))
                    (redirect (string-append "/chat/dm?peer=" peer))))
              (html/page
                (list
                  (html/h1 (string-append "DM with " peer))
                  (html/p* ""
                    (html/join
                      (list
                        (html/a "/chat" "Lobby")
                        (html/span "" " · ")
                        (html/a "/chat/dm" "All DMs"))))
                  (chat/dm-main-html peer))))))))

(route "/chat" "chat/page")
(route "/chat/partial" "chat/partial")
(route "/chat/dm" "chat/dm-page")
