;; package chat -- exported from live branch system/main
;; Sync: scripts/pp_sync.py pull|push

(define chat/all-messages
  (lambda () (rows (quote chat-message) 200))
  "Return up to 200 chat-message rows.")

(define chat/alphabet
  " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~")

(define chat/box
  (lambda
    (kind size class style-suffix extra)
    (append
      (list
        "class"
        class
        "style"
        (string-append
          (pads/box-style kind (list (cons (quote size) size)))
          style-suffix))
      extra))
  "Attr pairs for one declared box.")

(define chat/bubble
  (lambda
    (row)
    (let*
      ((f (chat/row-fields row)) (from (chat/msg-from f)))
      (ui/bubble
        (if (string-eq from (as-str (whoami))) "user" "llm")
        (string-append
          from
          " · "
          (chat/fmt-ts (dict-get f "created_at")))
        (chat/msg-body f))))
  "One message row -> a declared ui/bubble value.")

(define chat/can-access?
  (lambda
    (room-id)
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
                (chat/member? (dict-get f "members") me))))))))
  "Check if current user can access a chat room.")

(define chat/char-rank
  (lambda
    (c)
    (chat/char-rank-loop
      c
      chat/alphabet
      0
      (string-length chat/alphabet)))
  "Get the rank (index) of a character in the alphabet.")

(define chat/char-rank-loop
  (lambda
    (c alphabet i n)
    (if
      (>= i n)
      999
      (if
        (string-eq (substring alphabet i 1) c)
        i
        (chat/char-rank-loop c alphabet (+ i 1) n))))
  "Tail-recursive helper for char-rank.")

(define chat/css
  (lambda
    ()
    (css/sheet
      (list
        (css/rule
          ".chatpage"
          (list "max-height" "72vh" "gap" ".5rem"))
        (css/rule ".chat-log" (list "padding" ".5rem"))
        (css/rule
          ".chat-composer"
          (list "gap" ".5rem" "align-items" "flex-end"))
        (css/rule
          ".chat-composer textarea"
          (list "resize" "vertical"))
        (css/rule
          ".bubble"
          (list
            "background"
            "var(--panel)"
            "border-radius"
            "6px"
            "padding"
            ".35rem .6rem"
            "margin"
            ".35rem 0"))
        (css/rule
          ".bubble.user"
          (list "border-left" "3px solid var(--ok)"))
        (css/rule
          ".bubble.llm"
          (list "border-left" "3px solid var(--link)"))
        (css/rule
          ".bubble-meta"
          (list "font-size" ".8rem" "color" "var(--muted)"))
        (css/rule
          ".bubble-body"
          (list "white-space" "pre-wrap")))))
  "Chat paint (borge/filmstrip lookbook): the structural layout is DECLARED\n   on the elements through the pads size grammar; this sheet carries paint\n   only.")

(define chat/dm
  (lambda
    (peer)
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
                      (chat/timestamp)))))
              rid))))))
  "Get or create a DM room id for a peer. Room rows stamp created_at\n     with (chat/timestamp) — was hardcoded \"\".")

(define chat/dm-kids
  (lambda
    (peer)
    (let
      ((xs (chat/history (chat/dm peer))))
      (if
        (null? xs)
        (list
          (ui/elm
            "p"
            (list "class" "muted")
            (list (ui/text "No messages yet."))))
        (list/map chat/bubble xs))))
  "DM transcript kid values.")

(define chat/dm-list-li
  (lambda
    (row)
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
                (html/text peer))))))))
  "Render a DM thread as an HTML list item.")

(define chat/dm-list-page
  (lambda
    ()
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
            (html/ul "" (html/join (list/map chat/dm-list-li xs))))))))
  "Render the DM list page with start-DM form.")

(define chat/dm-main-html
  (lambda
    (peer)
    (lookbook/html
      (ui/chatpage
        "dm-main"
        ""
        (string-append "/chat/dm?peer=" peer)
        (chat/dm-kids peer))))
  "Render the DM main area with message form and history.")

(define chat/dm-messages-html
  (lambda
    (peer)
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
            (html/join (list/map chat/msg-li xs)))))))
  "Render DM message history for a peer.")

(define chat/dm-page
  (lambda
    ()
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
              (render-html (chat/kids-html (chat/dm-kids peer)))
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
              (chat/dm-main-html peer)))))))
  "DM route handler: list, open, or post to a DM thread.")

(define chat/dm-peer-from-title
  (lambda
    (title me)
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
        t)))
  "Extract peer username from a DM room title.")

(define chat/dm-room-id
  (lambda
    (peer)
    (let
      ((me (as-str (whoami))) (them (as-str peer)))
      (if
        (chat/str-le? me them)
        (string-append "dm:" me ":" them)
        (string-append "dm:" them ":" me))))
  "Compute the canonical DM room id for two users.")

(define chat/dms
  (lambda
    ()
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
        (rows (quote chat-room) 100))))
  "List all DM rooms the current user is a member of.")

(define chat/ensure-lobby
  (lambda
    ()
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
              (chat/timestamp))))
        (chat/row-id found))))
  "Create the lobby chat-room if it does not exist. Room rows stamp\n     created_at with (chat/timestamp) — was hardcoded \"\".")

(define chat/ensure-space
  (lambda
    ()
    (if
      (space-exists? chat/open-space)
      #t
      (guard
        (lambda (e) #f)
        (seq
          (space-create chat/open-space (quote open))
          (space-admit (quote chat-room) chat/open-space)
          (space-admit (quote chat-message) chat/open-space)
          #t))))
  "Create the shared open chat space and admit chat types (idempotent,\n      best-effort; the first poster races safely). Repair of the post-013\n      confinement break: chat rows live in data.s.chat-lobby.*, visible\n      to every authenticated identity (open space).")

(define chat/epoch->iso
  (lambda
    (s)
    (let*
      ((sec (read-string s))
        (days (floor (/ sec 86400)))
        (rem (remainder sec 86400))
        (hh (floor (/ rem 3600)))
        (mm (floor (/ (remainder rem 3600) 60)))
        (z (+ days 719468))
        (era (floor (/ z 146097)))
        (doe (- z (* era 146097)))
        (yoe-num
          (-
            (+ doe (floor (/ doe 36524)))
            (+ (floor (/ doe 1460)) (if (>= doe 146096) 1 0))))
        (yoe (floor (/ yoe-num 365)))
        (y (+ yoe (* era 400)))
        (doy
          (-
            doe
            (+
              (* 365 yoe)
              (floor (/ yoe 4))
              (- (floor (/ yoe 100))))))
        (mp (floor (/ (+ (* 5 doy) 2) 153)))
        (d (+ (- doy (floor (/ (+ (* 153 mp) 2) 5))) 1))
        (m (+ mp (if (< mp 10) 3 -9)))
        (y2 (+ y (if (<= m 2) 1 0))))
      (string-append
        (chat/pad4 y2)
        "-"
        (chat/pad2 m)
        "-"
        (chat/pad2 d)
        " "
        (chat/pad2 hh)
        ":"
        (chat/pad2 mm)))))

(define chat/find-room
  (lambda
    (room-id)
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
        (rows (quote chat-room) 50))))
  "Find a chat-room row by room-id (scan up to 50).")

(define chat/fmt-ts
  (lambda
    (ts)
    (let
      ((s (as-str ts)))
      (cond
        ((string=? s "") "unknown")
        ((and
            (= (string-length s) 10)
            (guard
              (lambda (e) #f)
              (string=? (as-str (read-string s)) s)))
          (chat/epoch->iso s))
        ((>= (string-length s) 16)
          (string-append
            (substring s 0 10)
            " "
            (substring s 11 5)))
        (else s))))
  "created_at stamp -> short YYYY-MM-DD HH:MM display. TOTAL (i-1790569346): epoch seconds convert to UTC display, empty renders 'unknown' (never a blank meta line, never a raw epoch), ISO slices as before. Nopales substring is (s pos len) — OCaml String.sub convention.")

(define chat/form-html
  (lambda
    ()
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
                  (html/attr "placeholder" "Hello from the lobby…")
                  (html/attr "maxlength" "4000")))
              "")
            (html/button (html/attr "type" "submit") "Send"))))))
  "Render the lobby message form with htmx submit.")

(define chat/history
  (lambda
    (room-id)
    (let
      ((want (as-str room-id)))
      (if
        (chat/can-access? want)
        (list/filter
          (lambda
            (row)
            (string-eq (chat/msg-room (chat/row-fields row)) want))
          (chat/all-messages))
        (list))))
  "Get messages for a room the user can access.")

(define chat/kid
  (lambda
    (x)
    (cond
      ((null? x) (ui/text ""))
      ((string? x)
        (if (string-starts-with? x "{") x (ui/text x)))
      (else (ui/text (as-str x)))))
  "One kid param -> a kid wrapper.")

(define chat/kid-list
  (lambda
    (body)
    (cond
      ((null? body) (list))
      ((pair? body) body)
      ((string? body)
        (if
          (string-starts-with? body "{")
          (list body)
          (list (ui/text body))))
      (else (list (ui/text (as-str body))))))
  "A kids param -> a list of kid wrappers.")

(define chat/kids-html
  (lambda
    (kids)
    (html/join (list/map lookbook/html kids)))
  "Declared transcript kids -> one html fragment (the ONE markup site for\n   transcript content).")

(define chat/lb-elements
  (list
    "ui/bubble"
    "ui/chat-log"
    "ui/chat-composer"
    "ui/chatpage")
  "This host's declared chat vocabulary (literal list, NOT a snapshot of\n   lookbook/elements: a captured snapshot would make lookbook/elements\n   refer to itself after a reload -> cyclic thunk force).")

(define chat/lobby
  (lambda
    ()
    (seq (chat/ensure-lobby) (chat/history chat/lobby-id)))
  "Ensure the lobby exists and return its message history.")

(define chat/lobby-id
  "lobby")

(define chat/log-kids
  (lambda
    ()
    (let
      ((xs (chat/lobby)))
      (if
        (null? xs)
        (list
          (ui/elm
            "p"
            (list "class" "muted")
            (list (ui/text "Say hi to the box."))))
        (list/map chat/bubble xs))))
  "Lobby transcript kid values.")

(define chat/main-html
  (lambda
    ()
    (lookbook/html
      (ui/chatpage
        "chat-main"
        "/chat/partial"
        "/chat"
        (chat/log-kids))))
  "Render the chat main area: the declared chatpage shell (transcript grows,\n     composer row inside) projected to html. The log id is chat-main-log\n     (the POST/partial swap target).")

(define chat/member?
  (lambda
    (members-json user)
    (string-contains? (as-str members-json) (as-str user)))
  "Check if a user is in a members JSON array string.")

(define chat/members-json
  (lambda
    (a b)
    (string-append
      "[\""
      (as-str a)
      "\",\""
      (as-str b)
      "\"]"))
  "Build a 2-member JSON array string for a DM room.")

(define chat/messages-html
  (lambda
    ()
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
            (html/join (list/map chat/msg-li xs)))))))
  "Render lobby messages with auto-refresh htmx polling.")

(define chat/msg-body
  (lambda (fields) (as-str (dict-get fields "body")))
  "Extract body field from a message fields dict.")

(define chat/msg-from
  (lambda (fields) (as-str (dict-get fields "from")))
  "Extract from field from a message fields dict.")

(define chat/msg-li
  (lambda
    (row)
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
                  (html/span
                    (html/attr "class" "muted")
                    (string-append
                      " · "
                      (chat/fmt-ts (dict-get f "created_at")))))))
            (html/p (html/text (chat/msg-body f))))))))
  "Render a chat message as an HTML list item (from · timestamp · body).")

(define chat/msg-room
  (lambda (fields) (as-str (dict-get fields "room_id")))
  "Extract room_id field from a message fields dict.")

(define chat/open-space
  "chat-lobby")

(define chat/pad2
  (lambda
    (n)
    (let
      ((s (as-str (floor n))))
      (if (< (string-length s) 2) (string-append "0" s) s))))

(define chat/pad4
  (lambda
    (n)
    (let
      ((s (as-str (floor n))))
      (string-append
        (if
          (< (string-length s) 4)
          (substring "0000" 0 (- 4 (string-length s)))
          "")
        s))))

(define chat/page
  (lambda
    ()
    (if
      (string-eq (request-method) "POST")
      (seq
        (chat/say (request-param "body"))
        (if
          (hx-request?)
          (render-html (chat/kids-html (chat/log-kids)))
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
          (chat/main-html)))))
  "Chat lobby route handler: GET renders the page; POST sends a message and,\n     under the HX-Request HEADER, answers the TRANSCRIPT FRAGMENT that swaps\n     into #chat-main-log (a non-htmx POST redirects).")

(define chat/partial
  (lambda
    ()
    (render-html (chat/kids-html (chat/log-kids))))
  "Render lobby transcript content as an htmx partial for polling (swaps\n     #chat-main-log).")

(define chat/room-id-of
  (lambda (fields) (as-str (dict-get fields "room_id")))
  "Extract room_id field from a chat-room fields dict.")

(define chat/row-fields
  (lambda (row) (cdr row))
  "Get fields dict from a chat row.")

(define chat/row-id
  (lambda (row) (as-str (car row)))
  "Get the id from a chat row.")

(define chat/say
  (lambda (body) (chat/say-in chat/lobby-id body))
  "Say a message in the lobby.")

(define chat/say-dm
  (lambda
    (peer body)
    (let
      ((rid (chat/dm peer)))
      (if
        (blank? rid)
        "chat: bad peer"
        (chat/say-in rid body))))
  "Say a direct message to a peer user.")

(define chat/say-in
  (lambda
    (room-id body)
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
              (chat/ensure-space)
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
                    (chat/timestamp)))))
            "chat: not a member of room")))))
  "Say a message in a specific chat room (with access check). Stamps\n     created_at with (chat/timestamp) — was hardcoded \"\" (blank DM\n     timestamps, BORGE-1790367502).")

(define chat/str-le?
  (lambda
    (a b)
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
              (<= (chat/char-rank ca) (chat/char-rank cb))))))))
  "Lexicographic string less-than-or-equal via char ranks.")

(define chat/timestamp
  (lambda () (guard (lambda (e) "") (now-iso)))
  "ISO-8601 UTC stamp (YYYY-MM-DDTHH:MM:SSZ) via the kernel now-iso prim.\n     Empty string until the image carrying the prim deploys (guard-catch),\n     so this pin is safe to land before the kernel patch. Uniform 20-char\n     shape: lexicographic compare is chronological (PG timestamptz convention).")

(define chat/users
  (lambda () (list-usernames))
  "List all usernames on the image.")
