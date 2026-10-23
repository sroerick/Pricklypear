;; package chat -- exported from live branch system/main
;; Sync: scripts/pp-sync.sh status|pull|snapshot

(define chat/all-messages
  (lambda () (rows (quote chat-message) 2000))
  "Return up to 2000 chat-message rows. 10-23: was 200 - the scan window
silently truncated the feed once the dataset crossed 200 rows (onset 10-21
10:30Z): every later write returned ok+id but vanished from history reads
while by-id reads still saw the rows (incident mem 716f53e3). Proper fix =
newest-first ordering or pagination in the scan; this buys headroom.")

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
            ".chat-older"
            (list "max-height" "40vh" "overflow-y" "auto"))
          (css/rule
            ".chat-older-btn"
            (list
              "margin"
              ".35rem 0"
              "font"
              "inherit"
              "padding"
              ".25rem .6rem"
              "border"
              "1px solid var(--border)"
              "border-radius"
              "4px"
              "background"
              "var(--panel)"
              "color"
              "inherit"
              "cursor"
              "pointer"))
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
        ((xs (chat/latest (chat/dm peer))))
      (if
        (null? xs)
        (list
          (ui/elm
            "p"
            (list "class" "muted")
            (list (ui/text "No messages yet."))))
        (list/map chat/bubble xs))))
  "DM transcript kid values (live window: latest chat/window-size; older\n     pages lazy-load into the #chat-older region).")

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
          (string-append "/chat/dm?peer=" peer)
          (string-append "/chat/dm?peer=" peer)
          (chat/dm-kids peer))))
  "Render the DM main area with message form and history. The DM page is
     its own poll: an HX-Request GET returns the windowed transcript
     fragment, so DMs get autoscroll + catch-up like the lobby.")

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
          (if
            (blank? (as-str (request-param "before")))
            (if
              (hx-request?)
              (render-html (chat/kids-html (chat/dm-kids peer)))
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
                  (chat/dm-older-initial peer)
                  (chat/dm-main-html peer))))
            (render-html (chat/dm-older-html peer (request-param "before"))))))))
  "DM route handler: list, open, or post to a DM thread. Under HX-Request a
     GET is the windowed transcript fragment (the log poll); a GET with
     ?before=<id> returns one older history page for #chat-older.")

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
      (seq (chat/ensure-lobby) (chat/latest chat/lobby-id)))
  "Ensure the lobby exists and return its live window (latest
     chat/window-size messages) — the lobby render no longer pulls the
     entire chat.")

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
      (if
        (blank? (as-str (request-param "before")))
        (html/page
          (list
            (html/h1 "Chat")
            (html/p*
              (html/attr "class" "muted")
              (string-append
                "Lobby for everyone on this box. You are "
                (as-str (whoami))
                "."))
            (chat/older-initial)
            (chat/main-html)))
        (if
          (hx-request?)
          (render-html (chat/older-html (request-param "before")))
          (redirect "/chat")))))
  "Chat lobby route handler: GET renders the page (live window of the
     latest chat/window-size messages + the lazy-history region); a GET
     with ?before=<id> under HX-Request returns one OLDER page for
     #chat-older; POST sends a message and, under HX-Request, answers the
     windowed transcript fragment that swaps into #chat-main-log (a
     non-htmx POST redirects).")

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

;; --- windowing + lazy history (card 70015a74, 2026-10-18) ---
;; The live surface (initial render, 4s poll, POST response) renders only
;; the latest chat/window-size messages; older history lazy-loads one page
;; at a time into the #chat-older region above the log. New helpers live at
;; the end of the file: define order is irrelevant in Nopales (bodies are
;; lazy recipes; chat/char-rank already forward-references
;; chat/char-rank-loop).

(define chat/window-size 50)

(define chat/id-le?
  (lambda (a b)
    (let ((sa (as-str a)) (sb (as-str b)))
      (if (string-eq sa sb) #t (string<? sa sb))))
  "Row-id <= (tie-break only: lexicographic via the native string<?; chat/
   row-le? carries the chronological order via created_at, so any stored
   id shape works. Was char-rank-walking chat/str-le? — O(alphabet) per
   character, blew the eval-step budget on 98 lobby rows.")

(define chat/id-lt?
  (lambda (a b)
    (if (string-eq (as-str a) (as-str b)) #f (chat/id-le? a b)))
  "Row-id < (chronological).")

(define chat/row-when
  (lambda (row) (as-str (dict-get (chat/row-fields row) "created_at")))
  "Row sort stamp: created_at. ISO stamps (chat/timestamp, uniform 20
   chars) sort chronologically; blank and legacy epoch stamps sort before
   them, which stays chronological for every shape chat/fmt-ts knows.")

(define chat/row-le?
  (lambda (a b)
    (let ((ta (chat/row-when a)) (tb (chat/row-when b)))
      (if (string-eq ta tb)
        (chat/id-le? (chat/row-id a) (chat/row-id b))
        (string<? ta tb))))
  "Row <= (chronological): created_at first (native string order; ISO stamps
   are uniform 20 chars, blank sorts first), row id as tie-break.")

(define chat/row-index
  (lambda (rows id i)
    (if (null? rows)
      -1
      (if (string-eq (chat/row-id (car rows)) (as-str id))
        i
        (chat/row-index (cdr rows) id (+ i 1)))))
  "Position of the row with this id in an ordered row list, -1 when the
   id is absent (pruned or beyond the 200-row fetch cap).")

(define chat/half
  (lambda (n) (if (< n 2) 0 (+ 1 (chat/half (- n 2)))))
  "n / 2 rounded down (avoids relying on a quotient prim).")

(define chat/split-at
  (lambda (xs n)
    (if (or (<= n 0) (null? xs))
      (cons () xs)
      (let ((rest (chat/split-at (cdr xs) (- n 1))))
        (cons (cons (car xs) (car rest)) (cdr rest)))))
  "Split xs into (first-n . rest) pair.")

(define chat/merge-rows
  (lambda (a b)
    (if (null? a)
      b
      (if (null? b)
        a
        (if (chat/row-le? (car a) (car b))
          (cons (car a) (chat/merge-rows (cdr a) b))
          (cons (car b) (chat/merge-rows a (cdr b)))))))
  "Merge two row-le?-ascending row lists.")

(define chat/sort-rows
  (lambda (rows)
    (if (or (null? rows) (null? (cdr rows)))
      rows
      (let ((parts (chat/split-at rows (chat/half (length rows)))))
        (chat/merge-rows
          (chat/sort-rows (car parts))
          (chat/sort-rows (cdr parts))))))
  "Sort rows (created_at,id)-ascending (merge sort; the live fetch is
   capped at 200 rows by chat/all-messages).")

(define chat/take-n
  (lambda (n xs)
    (if (or (<= n 0) (null? xs))
      ()
      (cons (car xs) (chat/take-n (- n 1) (cdr xs)))))
  "First n elements of xs.")

(define chat/take-last
  (lambda (n xs) (reverse (chat/take-n n (reverse xs))))
  "Last n elements of xs, original order.")

(define chat/room-rows
  (lambda (room-id) (chat/sort-rows (chat/history room-id)))
   "All messages for an accessible room, (created_at,id)-ascending ((rows)
   gives no order promise). Bounded by chat/all-messages' 200-row cap.")

(define chat/latest
  (lambda (room-id)
    (chat/take-last chat/window-size (chat/room-rows room-id)))
    "The live window: latest chat/window-size messages for a room,
   chronological. The live surface never renders more than this.")

(define chat/more-older?
  (lambda (rows before)
    (< 0 (chat/row-index rows before 0)))
  "True when the cursor row sits after the head of the ordered list, i.e.
   older rows exist before its position.")

(define chat/no-older-kid
  (lambda () (ui/elm "p" (list "class" "muted") (list (ui/text "No older messages."))))
  "Muted note when a lazy-history fetch has nothing older.")

(define chat/older-bar
  (lambda (href)
    (html/button
      (html/attrs
        (list
          (html/attr "hx-get" href)
          (html/attr "hx-target" "#chat-older")
          (html/attr "hx-swap" "outerHTML")))
      "Load older messages"))
  "Lazy-history control: fetches one older page and replaces the
   #chat-older region with it plus the next control.")

(define chat/older-region
  (lambda (kids href)
    (html/div
      (html/attrs
        (list (html/attr "id" "chat-older") (html/attr "class" "chat-older")))
      (html/join
        (append
          (list (chat/kids-html kids))
          (if (string-eq href "") (list) (list (chat/older-bar href)))))))
  "The #chat-older region: accumulated older pages + the load-older control.
   It sits ABOVE the polled log as a page sibling, so the 4s poll (which
   re-swaps the log window) never clobbers loaded history.")

(define chat/older-rows
  (lambda (room-id before)
    (let* ((rows (chat/room-rows room-id))
           (i (chat/row-index rows before 0)))
      (if (< i 0)
        ()
        (chat/take-last chat/window-size (chat/take-n i rows)))))
  "One older page: the window-size ordered rows immediately before the
   cursor row's position (position-based, so any stored id shape works).
   Cursor not found -> ().")

(define chat/older-html
  (lambda (before)
    (let ((rows (chat/older-rows chat/lobby-id before)))
      (if
        (null? rows)
        (chat/older-region (list (chat/no-older-kid)) "")
        (chat/older-region
          (list/map chat/bubble rows)
          (string-append "/chat?before=" (chat/row-id (car rows)))))))
  "Lobby older-history fragment (GET /chat?before=<id> under HX-Request).")

(define chat/older-initial
  (lambda ()
    (let* ((rows (chat/room-rows chat/lobby-id))
           (window (chat/take-last chat/window-size rows)))
      (if
        (null? window)
        ""
        (if
          (chat/more-older? rows (chat/row-id (car window)))
          (chat/older-region
            (list)
            (string-append "/chat?before=" (chat/row-id (car window))))
          ""))))
  "Initial lobby #chat-older region: the control only when history exists
   beyond the live window, empty otherwise.")

(define chat/dm-older-html
  (lambda (peer before)
    (let ((rows (chat/older-rows (chat/dm peer) before)))
      (if
        (null? rows)
        (chat/older-region (list (chat/no-older-kid)) "")
        (chat/older-region
          (list/map chat/bubble rows)
          (string-append "/chat/dm?peer=" (as-str peer) "&before=" (chat/row-id (car rows)))))))
  "DM older-history fragment (GET /chat/dm?peer=..&before=<id> under
   HX-Request).")

(define chat/dm-older-initial
  (lambda (peer)
    (let* ((rows (chat/room-rows (chat/dm peer)))
           (window (chat/take-last chat/window-size rows)))
      (if
        (null? window)
        ""
        (if
          (chat/more-older? rows (chat/row-id (car window)))
          (chat/older-region
            (list)
            (string-append "/chat/dm?peer=" (as-str peer) "&before=" (chat/row-id (car window))))
          ""))))
  "Initial DM #chat-older region: the control only when history exists
   beyond the live window, empty otherwise.")
