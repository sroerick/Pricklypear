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

(define chat/bad-peer?
  (lambda
    (peer)
    (let
      ((p (as-str peer)))
      (if
        (string-contains? p ":")
        #t
        (if
          (string-eq p chat/lobby-id)
          #t
          #f))))
  "Whether a supposed peer name is really a room id or a reserved token.
   chat/dm CREATES a room on first message, so feeding it a room id mints a
   junk DM whose title embeds the id - observed live as the malformed room
   'dm:admin:dm:admin:gregor', created when a room id was passed where a
   peer belonged. A username never contains ':' and never equals the lobby
   id, so both are rejected before any row is written.")

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
          (if
            (chat/bad-peer? them)
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
                        (chat/timestamp)
                        "protocol"
                        "addressed"))))
                rid)))))))
  "Get or create a DM room id for a peer. Room rows stamp created_at\n     with (chat/timestamp) — was hardcoded \"\". New DM rooms declare\n     protocol 'addressed': a DM is recorded, but it summons a turn only when\n     a row carries turn:true plus an explicit to:.")

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
          (string-append "/chat/dm?peer=" peer "&partial=1")
          (string-append "/chat/dm?peer=" peer)
        (chat/dm-kids peer))))
  "Render the DM main area with message form and history. The composer
   posts to this peer only; the log polls the ?partial=1 transcript
   fragment, so an HX-Request GET WITHOUT partial serves the pane body
   (shell + composer) when the thread opens as a filmstrip pane.")

  (define chat/dm-messages-html
  (lambda
    (peer)
    (let
      ((rid (chat/dm peer))
        (xs (chat/history rid chat/window-size)))
      (html/div
        (html/attr "id" "dm-messages")
        (if
          (null? xs)
          (html/p* (html/attr "class" "muted") "No messages yet.")
          (html/ul
            (html/attr "class" "chat-list")
            (html/join (list/map chat/msg-li xs)))))))
  "Render the DM live window: the newest chat/window-size messages for a peer.
   The read is bounded in SQL (chat/history), so this legacy list render no
   longer pulls the whole thread; older pages come from chat/dm-older-html.")

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
              (if
                (blank? (as-str (request-param "partial")))
                (render-html (chat/dm-main-html peer))
                (render-html (chat/kids-html (chat/dm-kids peer))))
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

(define chat/id-seen?
  (lambda
    (rid rows)
    (pair?
      (filter
        (lambda (r) (string-eq (chat/room-id-of (chat/row-fields r)) rid))
        rows)))
  "Whether rid already appears among rows' room_id fields.")

(define chat/dedupe-rooms
  (lambda
    (rows)
    (reverse
      (foldl
        (lambda
          (acc row)
          (let
            ((rid (chat/room-id-of (chat/row-fields row))))
            (if
              (chat/id-seen? rid acc)
              acc
              (cons row acc))))
        (list)
        rows)))
  "Keep the FIRST row per room_id, preserving order.

     chat-room is not a seed type, so rows are author-scoped and the same
     logical room can exist more than once - prod carries TWO rows with
     kind=lobby (one authored by admin, one by gregor, both pre-dating the
     open space). Returning both would make a bridge poll and answer the
     same room twice. Discovery surfaces therefore collapse by room_id;
     anything that needs the canonical row asks chat/find-room, which
     resolves one row on its own.")

(define chat/dms
  (lambda
    ()
    (let
      ((me (as-str (whoami))))
      (chat/dedupe-rooms
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
  "List all DM rooms the current user is a member of. Collapsed by room_id
     (see chat/dedupe-rooms): the same DM can exist once per author.")

(define chat/rooms
  (lambda
    ()
    (let
      ((me (as-str (whoami))))
      (chat/dedupe-rooms
        (list/filter
          (lambda
            (row)
            (let*
              ((f (chat/row-fields row)) (kind (as-str (dict-get f "kind"))))
              (if
                (string-eq kind "dm")
                #f
                (if
                  (string-eq kind "lobby")
                  #t
                  (chat/member? (dict-get f "members") me)))))
          (rows (quote chat-room) 100)))))
  "List non-DM chat rooms the current user can see: open lobby rooms plus\n     every kind=room the user is an EXPLICIT member of.

     The read-side counterpart of chat/dms, and the discovery step an agent\n     or bridge needs before it can poll a room at all - nothing else\n     enumerates joined rooms (chat/find-room wants an id up front and\n     chat/room-rows wants one too, so 'which rooms am I in' had no answer).\n     Membership is tested with chat/member? (whole-element compare, never a\n     substring), and kind=dm is excluded so DMs keep their own surface.\n     Collapsed by room_id (chat/dedupe-rooms): prod holds two lobby rows,\n     and returning both would have a bridge answer the lobby twice.")

(define chat/room-protocol
  (lambda
    (room-id)
    (let
      ((rid (as-str room-id)))
      (if
        (blank? rid)
        "addressed"
        (let
          ((row (chat/find-room rid)))
          (if
            (null? row)
            "addressed"
            (let*
              ((f (chat/row-fields row))
                (declared (as-str (dict-get f "protocol"))))
              (if
                (blank? declared)
                (if
                  (string-eq (as-str (dict-get f "kind")) "lobby")
                  "legacy"
                  "addressed")
                declared)))))))
  "How a room decides when a message becomes an inference turn.

     'addressed' - a row wakes a reader only when it carries turn:true plus\n     an explicit to: (Tier 1). Arrival alone is Tier 0: recorded, no turn.\n     'legacy' - wake on any non-self message (pre-addressing behaviour).\n
     The declaration is a FIELD on the room, not a guess. Deriving the mode\n     by scanning whether a room has 'ever' carried an addressed row reads a\n     quiet room as legacy forever, which is exactly the wrong answer for a\n     freshly created DM: it would make every DM wake the other party.\n     Absent field: lobby rooms stay 'legacy' so deploying this does not\n     silence an agent mid-conversation, and everything else - new rooms and\n     DMs - defaults to 'addressed'.")

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
  (lambda (room-id limit)
    (let ((want (as-str room-id)))
      (if (chat/can-access? want)
          (reverse
            (find-rows (quote chat-message) "room_id" want limit "created_at DESC"))
          (list))))
  "The newest LIMIT messages for a room the user can access, oldest-first.
   The limit and the order are pushed into SQL (ORDER BY created_at DESC LIMIT
   n on the data_rows column), so a live window or one history page never pulls
   the whole room. Filtered in SQL, not in Lisp: fetching (chat/all-messages)
   and dropping non-matching rows cost a full-table scan per room read and
   silently truncated once the store passed the scan cap (10-21 read-fork).
   find-rows pushes room_id into the WHERE clause under the same visible_sql
   floor that (rows ...) applies, so access semantics are unchanged.")

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
    "ui/chatpage"
    "chat/channel-row"
    "chat/dm-starter"
      "chat/older-bar"
      "chat/older-more"
      "chat/older-oob"
    "chat/older-gone"
    "chat/older-region")
  "This host's declared chat vocabulary (literal list, NOT a snapshot of
   lookbook/elements: a captured snapshot would make lookbook/elements
   refer to itself after a reload -> cyclic thunk force).")

    ;; ---- lookbook registration ----
    ;; ONE composer owns lookbook/base-elements, so no package can erase the
    ;; inventory by loading last. Every package declares its vocabulary as a
    ;; literal VALUE (chat/lb-elements here, lookbook/todo-elements in
    ;; libs/todo); this def composes the vocabularies seeded into every
    ;; habitat image. Membership-checked appends keep a reload from
    ;; duplicating names.
(define lookbook/base-elements
  (lambda ()
    (chat/lookbook-add
      (chat/lookbook-add chat/lookbook-seed chat/lb-elements)
      lookbook/todo-elements))
  "The lib seed inventory composed with the declared package vocabularies:
chat's transcript elements + todo's assignment control. libs/todo declares
lookbook/todo-elements and must NOT redefine this name -- a package-level
definition is clobbered by the next package to load (todo seeds before chat,
so todo's own extension was silently dropped).")

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
    (let
      ((raw (as-str members-json)) (me (as-str user)))
      (if
        (blank? raw)
        #f
        (let
          ((xs (guard (lambda (e) ()) (json-parse raw))))
          (if (pair? (member me xs)) #t #f)))))
  "Check whether a user is an EXACT member of a room's members JSON array.
   Compares whole array elements, never a substring: a string-contains? test
   let user 'ho' match 'hollis', an access check satisfiable by a prefix of
   someone else's name. Malformed or empty members JSON is 'not a member',
   never an error.")

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
      ((xs (chat/latest chat/lobby-id)))
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
  "Render the lobby live window with auto-refresh htmx polling. Reads
   chat/latest: only the newest chat/window-size messages, bounded in SQL, so
   this legacy poll render never pulls the whole lobby (card 70015a74).")

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
            (chat/channels-html)
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

(define chat/mirror-in
  (lambda
    (room-id body from to)
    (let
      ((text (as-str body))
        (rid (as-str room-id))
        (speaker (as-str from))
        (dest (as-str to))
        (me (as-str (whoami))))
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
                  (if
                    (blank? dest)
                    (list
                      "room_id"
                      rid
                      "from"
                      speaker
                      "body"
                      text
                      "created_at"
                      (chat/timestamp)
                      "mirrored_by"
                      me)
                    (list
                      "room_id"
                      rid
                      "from"
                      speaker
                      "body"
                      text
                      "created_at"
                      (chat/timestamp)
                      "mirrored_by"
                      me
                      "turn"
                      "true"
                      "to"
                      dest)))))
            "chat: not a member of room")))))
  "Record a mirrored message, keeping the SPEAKER and the WRITER distinct.
   A bridge authenticates as one identity but relays messages from many
   people; writing (whoami) into 'from' would launder every author into the
   bridge's own name and make the log lie about who said what. So 'from' is
   the attested speaker and 'mirrored_by' is the identity that actually
   wrote the row (also the crud 'author' column). Blank 'to' records Tier 0
   (no turn); a non-blank 'to' requests one Tier-1 turn from that addressee.
   The bridge attests authorship - it does not thereby gain authority: the
   access check below still runs as the writing identity.")

(define chat/say-in-addressed
  (lambda
    (room-id body to)
    (let
      ((text (as-str body))
        (user (as-str (whoami)))
        (rid (as-str room-id))
        (dest (as-str to)))
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
                    (chat/timestamp)
                    "turn"
                    "true"
                    "to"
                    dest))))
            "chat: not a member of room")))))
  "Say a message that REQUESTS A TURN from its addressee (Tier 1).
   'to' is a FIELD, not an @mention scraped out of the body: the record says
   who it is for, and each reader decides whether that is itself or 'room'.
   This is the transport/inference split - posting writes the log and wakes
   nobody by itself; only the addressee's own wake scan turns the row into a
   model call. Use chat/say-in for Tier 0 (recorded, no turn).")

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
  (lambda (room-id) (chat/history room-id chat/window-size))
  "A room's live window: the newest chat/window-size messages, chronological.
   The read is bounded in SQL (chat/history), so a room read never pulls the
   whole transcript.")

(define chat/latest
  (lambda (room-id) (chat/room-rows room-id))
  "The live window: latest chat/window-size messages for a room, chronological.
   The live surface never renders more than this, and the 4s poll re-reads only
   this bounded window instead of the whole room.")

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
    (ui/elm
      "button"
      (list
        "type" "button"
        "class" "chat-older-btn"
        "hx-get" href
        "hx-target" "#chat-older"
        "hx-swap" "afterbegin")
      (list (ui/text "Load older messages"))))
  "Lazy-history control: fetches one older page and PREPENDS it above the pages
   already loaded (afterbegin into #chat-older), so history accumulates instead
   of being replaced page by page. The id lives on the chat/older-more carrier,
   not here: an OOB refresh replaces that carrier wholesale.")

(define chat/older-bar/examples
  (lambda ()
    (lb/obj
      (list
        "name" "chat/older-bar"
        "doc" "Load-older button for the chat history region: hx-get href, hx-target #chat-older, hx-swap afterbegin (accumulates older pages)."
        "file" "libs/chat/lib.pp"
        "formals" (json-emit (list "href"))
        "variants" (json-emit (list (lb/obj (list "href" "/chat?before=abc"))))
        "source"
        "(chat/older-bar href) ->\n  (ui/elm \"button\"\n    (list \"type\" \"button\" \"class\" \"chat-older-btn\"\n          \"hx-get\" href \"hx-target\" \"#chat-older\" \"hx-swap\" \"afterbegin\")\n    (list (ui/text \"Load older messages\")))")))
  "The chat/older-bar card: this def IS the element's declaration.")

(define chat/older-more
  (lambda (href)
    (ui/elm
      "div"
      (list "id" "chat-older-more")
      (list (chat/older-bar href))))
  "The load-older carrier in the page: a stable #chat-older-more element around
   the button, so the OOB refresh has one swap target and ids stay unique.")

(define chat/older-more/examples
  (lambda ()
    (lb/obj
      (list
        "name" "chat/older-more"
        "doc" "Load-older carrier: div#chat-older-more around the chat/older-bar button."
        "file" "libs/chat/lib.pp"
        "formals" (json-emit (list "href"))
        "variants" (json-emit (list (lb/obj (list "href" "/chat?before=abc"))))
        "source"
        "(chat/older-more href) ->\n  (ui/elm \"div\" (list \"id\" \"chat-older-more\")\n    (list (chat/older-bar href)))")))
  "The chat/older-more card.")

(define chat/older-oob
  (lambda (href)
    (ui/elm
      "div"
      (list "id" "chat-older-more" "hx-swap-oob" "outerHTML")
      (list (chat/older-bar href))))
  "Out-of-band carrier for the NEXT load-older button: a history fragment
   replaces #chat-older-more with it, keeping the control out of the afterbegin
   row insert.")

(define chat/older-oob/examples
  (lambda ()
    (lb/obj
      (list
        "name" "chat/older-oob"
        "doc" "OOB wrapper replacing #chat-older-more with the next load-older button (hx-swap-oob outerHTML)."
        "file" "libs/chat/lib.pp"
        "formals" (json-emit (list "href"))
        "variants" (json-emit (list (lb/obj (list "href" "/chat?before=abc"))))
        "source"
        "(chat/older-oob href) ->\n  (ui/elm \"div\"\n    (list \"id\" \"chat-older-more\" \"hx-swap-oob\" \"outerHTML\")\n    (list (chat/older-bar href)))")))
  "The chat/older-oob card.")

(define chat/older-gone
  (lambda ()
    (ui/elm "div" (list "id" "chat-older-more" "hx-swap-oob" "delete") (list)))
  "Out-of-band removal of the load-older control once the oldest page loads.")

(define chat/older-gone/examples
  (lambda ()
    (lb/obj
      (list
        "name" "chat/older-gone"
        "doc" "OOB delete of #chat-older-more at the oldest message."
        "file" "libs/chat/lib.pp"
        "formals" (json-emit (list))
        "variants" (json-emit (list (lb/obj (list))))
        "source"
        "(chat/older-gone) ->\n  (ui/elm \"div\" (list \"id\" \"chat-older-more\" \"hx-swap-oob\" \"delete\") (list))")))
  "The chat/older-gone card.")

(define chat/older-region
  (lambda (kids href)
    (ui/elm
      "div"
      (list "id" "chat-older" "class" "chat-older")
      (append
        (chat/kid-list kids)
          (if (string-eq href "") (list) (list (chat/older-more href))))))
  "The #chat-older region: accumulated older pages + the load-older control. It
   sits ABOVE the polled log as a page sibling, so the 4s poll (which re-swaps
   the log window) never clobbers loaded history.")

(define chat/older-region/examples
  (lambda ()
    (lb/obj
      (list
        "name" "chat/older-region"
        "doc" "The #chat-older region: accumulated history pages plus the load-older control; composed from declared kids by def call."
        "file" "libs/chat/lib.pp"
        "formals" (json-emit (list "kids" "href"))
        "variants" (json-emit (list (lb/obj (list "kids" "()" "href" "/chat?before=abc"))))
        "source"
          "(chat/older-region kids href) ->\n  (ui/elm \"div\" (list \"id\" \"chat-older\" \"class\" \"chat-older\")\n    (append (chat/kid-list kids)\n            (if (string-eq href \"\") (list) (list (chat/older-more href)))))")))
  "The chat/older-region card.")

(define chat/older-page-html
  (lambda (rows next-href)
    (chat/kids-html
      (append
        (list/map chat/bubble rows)
        (list (if (null? rows) (chat/older-gone) (chat/older-oob next-href))))))
  "One accumulated history response: the page rows (chronological, prepended by
   afterbegin) plus an OOB refresh (or removal) of the load-older control.")

(define chat/older-rows
  (lambda (room-id before)
    (chat/older-probe room-id before (* 2 chat/window-size)))
  "One older page: the window-size rows immediately before the cursor row, or ()
   when the cursor is already the oldest. Reads bounded newest-first pages (100,
   200, 400 ... rows) until the cursor is inside the window, so a page click
   never pulls the whole room unless the reader is that deep.")

(define chat/older-probe
  (lambda (room-id before n)
    (let* ((rows (chat/history room-id n))
           (i (chat/row-index rows before 0)))
      (if (< i 0)
          (if (< (length rows) n) () (chat/older-probe room-id before (* n 2)))
          (chat/take-last chat/window-size (chat/take-n i rows)))))
  "Bounded cursor probe for chat/older-rows: double the newest-first window
   until the cursor row is inside it, or the room is exhausted.")

(define chat/older-html
  (lambda (before)
    (let ((rows (chat/older-rows chat/lobby-id before)))
      (chat/older-page-html
        rows
        (if (null? rows) "" (string-append "/chat?before=" (chat/row-id (car rows)))))))
  "Lobby older-history fragment (GET /chat?before=<id> under HX-Request).")

(define chat/older-initial
  (lambda ()
    (let* ((rows (chat/history chat/lobby-id (+ chat/window-size 1)))
           (window (chat/take-last chat/window-size rows)))
      (if
        (null? window)
        ""
        (if
          (chat/more-older? rows (chat/row-id (car window)))
          (lookbook/html
            (chat/older-region
              (list)
              (string-append "/chat?before=" (chat/row-id (car window)))))
          ""))))
  "Initial lobby #chat-older region: the control only when history exists
   beyond the live window, empty otherwise (one extra row tells us).")

(define chat/dm-older-html
  (lambda (peer before)
    (let ((rows (chat/older-rows (chat/dm peer) before)))
      (chat/older-page-html
        rows
        (if (null? rows) "" (string-append "/chat/dm?peer=" (as-str peer) "&before=" (chat/row-id (car rows)))))))
  "DM older-history fragment (GET /chat/dm?peer=..&before=<id> under
   HX-Request).")

(define chat/dm-older-initial
  (lambda (peer)
    (let* ((rows (chat/history (chat/dm peer) (+ chat/window-size 1)))
           (window (chat/take-last chat/window-size rows)))
      (if
        (null? window)
        ""
        (if
          (chat/more-older? rows (chat/row-id (car window)))
          (lookbook/html
            (chat/older-region
              (list)
              (string-append "/chat/dm?peer=" (as-str peer) "&before=" (chat/row-id (car window)))))
          ""))))
  "Initial DM #chat-older region: the control only when history exists
   beyond the live window, empty otherwise.")

;; ---- channel picker (card a7fbbaab, 2026-10-25) ----
;; One row per surface the user can talk in -- the lobby, every room they
;; are an explicit member of, every DM thread -- listed at /chat. Rows are
;; DECLARED UI VALUES (libs/lookbook discipline: declare, never render;
;; the <name>/examples card IS the declaration), projected through
;; lookbook/html at the one markup site. A row is an in-shell app-link
;; (POST /filmstrip/open, open-or-focus), never an anchor: an anchor
;; navigates the whole window out of the shell.

(define chat/channel-row
  (lambda (label route)
    (ui/elm
      "button"
      (list
        "type" "button"
        "class" "fs-applink"
        "hx-post" "/filmstrip/open"
        "hx-vals" (string-append "{\"route\":\"" route "\"}")
        "hx-target" "#fs-sink"
        "hx-swap" "innerHTML"
        "data-optimistic" "open")
      (list (ui/text label))))
  "One channel-picker row: a declared in-shell app-link posting
/filmstrip/open with the surface's route (open-or-focus as a filmstrip
pane; never an anchor, which navigates the whole window out of the
shell). Declared, never string-built.")

(define chat/channel-row/examples
  (lambda ()
    (lb/obj
      (list
        "name" "chat/channel-row"
        "doc" "One channel row: an in-shell app-link posting /filmstrip/open with the surface's route -- open-or-focus as a filmstrip pane, never a window navigation."
        "file" "libs/chat/lib.pp"
        "formals" (json-emit (list "label" "route"))
        "variants" (json-emit
                     (list
                       (lb/obj (list "label" "Lobby" "route" "/chat"))
                       (lb/obj (list "label" "alice" "route" "/chat/dm?peer=alice"))))
        "source"
        "(chat/channel-row label route) ->\n  (ui/elm \"button\" (list \"type\" \"button\" \"class\" \"fs-applink\"\n          \"hx-post\" \"/filmstrip/open\"\n          \"hx-vals\" (string-append \"{\\\"route\\\":\\\"\" route \"\\\"}\")\n          \"hx-target\" \"#fs-sink\" \"hx-swap\" \"innerHTML\"\n          \"data-optimistic\" \"open\")\n    (list (ui/text label)))")))
  "The chat/channel-row card: this def IS the element's declaration.")

(define chat/dm-starter
  (lambda ()
    (ui/elm
      "form"
      (list
        "class" "chat-dm-start"
        "hx-post" "/chat/dm/open"
        "hx-target" "#fs-sink"
        "hx-swap" "innerHTML")
      (list
        (ui/elm
          "input"
          (list
            "name" "peer"
            "placeholder" "peer username"
            "aria-label" "peer username"
            "required" "required")
          (list))
        (ui/elm
          "button"
          (list "type" "submit")
          (list (ui/text "DM"))))))
  "The start-DM control: a peer input posting /chat/dm/open (htmx
includes the form field), which open-or-focuses the /chat/dm?peer=<peer>
pane. Declared, never string-built.")

(define chat/dm-starter/examples
  (lambda ()
    (lb/obj
      (list
        "name" "chat/dm-starter"
        "doc" "The start-DM control: a peer input whose submit posts /chat/dm/open, opening the /chat/dm?peer=<peer> pane."
        "file" "libs/chat/lib.pp"
        "formals" (json-emit (list))
        "variants" (json-emit (list (lb/obj (list))))
        "source"
          "(chat/dm-starter) ->\n  (ui/elm \"form\" (list \"class\" \"chat-dm-start\"\n          \"hx-post\" \"/chat/dm/open\" \"hx-target\" \"#fs-sink\"\n          \"hx-swap\" \"innerHTML\")\n    (list (ui/elm \"input\" (list \"name\" \"peer\" \"placeholder\" \"peer username\"\n                  \"aria-label\" \"peer username\" \"required\" \"required\") (list))\n          (ui/elm \"button\" (list \"type\" \"submit\") (list (ui/text \"DM\"))))")))
  "The chat/dm-starter card: this def IS the element's declaration.")

(define chat/channel-entries
  (lambda ()
    (let ((me (as-str (whoami))))
      (append
        (list (list "Lobby" "/chat" "lobby"))
        (list/map
          (lambda (row)
            (let* ((f (chat/row-fields row))
                   (rid (chat/room-id-of f))
                   (title (as-str (dict-get f "title"))))
              (list
                (if (string-eq title "") rid title)
                (string-append "/chat/room?id=" rid)
                "room")))
          (list/filter
            (lambda (row)
              (if (string-eq (as-str (dict-get (chat/row-fields row) "kind")) "lobby")
                #f
                #t))
            (chat/rooms)))
        (list/map
          (lambda (row)
            (let* ((f (chat/row-fields row))
                   (title (as-str (dict-get f "title")))
                   (peer (chat/dm-peer-from-title title me)))
              (list
                peer
                (string-append "/chat/dm?peer=" peer)
                "dm")))
          (chat/dms)))))
  "The surfaces the user can talk in, one (label route kind) entry each:
the lobby, every kind=room they are an EXPLICIT member of (chat/rooms
with kind=lobby rows dropped -- the Lobby entry IS that surface), and
every DM thread (chat/dms, peer label via chat/dm-peer-from-title).")

(define chat/channels-html
  (lambda ()
    (html/div
      (html/attr "class" "card chat-channels")
      (html/join
        (append
          (list (html/h2 "Channels"))
          (list/map
            (lambda (e)
              (lookbook/html (chat/channel-row (nth e 0) (nth e 1))))
            (chat/channel-entries))
          (list
            (html/p* (html/attr "class" "muted") "Start a DM:")
            (lookbook/html (chat/dm-starter)))))))
  "The chats list at /chat (card a7fbbaab): one app-link row per surface
the user can talk in, each open-or-focusing its route as a filmstrip
pane, plus the start-DM control. Rows are declared chat/channel-row
values projected through lookbook/html -- never string-built markup.")

(define chat/dm-open
  (lambda ()
    (let ((peer (string-trim (as-str (request-param "peer")))))
      (if (blank? peer)
          (render-html (filmstrip/oob-msg "chat: peer required"))
          (let* ((r (string-append "/chat/dm?peer=" (filmstrip/url-enc peer)))
                 (row (filmstrip/row))
                 (cascade (nth row 0))
                 (panes (nth row 1))
                 (focus (nth row 2))
                 (res (filmstrip/open-apply panes r))
                 (panes2 (nth res 0))
                 (focus2 (nth res 1))
                 (appended? (nth res 2)))
            (seq
              (filmstrip/save-all cascade panes2 focus2)
              (render-html
                (html/join
                  (list
                    (filmstrip/oob-msg "")
                    (filmstrip/open-response panes2 focus focus2 appended?)
                    (filmstrip/oob-coltabs)))))))))
  "POST /chat/dm/open: peer param -> open-or-focus the
/chat/dm?peer=<peer> pane (the filmstrip/open flow with a route the
server builds from the peer input). A blank peer answers the launcher
status line only.")

;; ---- room threads (card a7fbbaab: rooms open as filmstrip panes) ----

(define chat/room-title
  (lambda (room-id)
    (let ((row (chat/find-room room-id)))
      (if (null? row)
          (as-str room-id)
          (let ((t (as-str (dict-get (chat/row-fields row) "title"))))
            (if (string-eq t "") (as-str room-id) t)))))
  "Display title of a room: its title field, the room id as fallback.")

(define chat/room-kids
  (lambda (room-id)
    (let ((xs (chat/latest room-id)))
      (if (null? xs)
          (list (ui/elm "p" (list "class" "muted") (list (ui/text "No messages yet."))))
          (list/map chat/bubble xs))))
  "Room transcript kid values (live window: latest chat/window-size
messages).")

(define chat/room-main-html
  (lambda (room-id)
    (let ((r (string-append "/chat/room?id=" room-id)))
      (lookbook/html
        (ui/chatpage
          "room-main"
          (string-append r "&partial=1")
          r
          (chat/room-kids room-id)))))
  "Room main area: the declared chatpage shell. The composer posts to
/chat/room?id=<this room> only; the log polls the ?partial=1 transcript
fragment, so an HX-Request GET WITHOUT partial serves the pane body
(shell + composer) when the room opens as a filmstrip pane.")

(define chat/room-older-html
  (lambda (room-id before)
    (let ((rows (chat/older-rows room-id before)))
      (chat/older-page-html
        rows
        (if (null? rows) "" (string-append "/chat/room?id=" room-id "&before=" (chat/row-id (car rows)))))))
  "Room older-history fragment (GET /chat/room?id=..&before=<id> under
   HX-Request).")

(define chat/room-older-initial
  (lambda (room-id)
    (let* ((rows (chat/history room-id (+ chat/window-size 1)))
           (window (chat/take-last chat/window-size rows)))
      (if (null? window)
          ""
          (if (chat/more-older? rows (chat/row-id (car window)))
              (lookbook/html
                (chat/older-region
                  (list)
                  (string-append "/chat/room?id=" room-id "&before=" (chat/row-id (car window)))))
              ""))))
  "Initial room #chat-older region: the control only when history exists
   beyond the live window, empty otherwise.")

(define chat/room-page
  (lambda ()
    (let ((rid (string-trim (as-str (request-param "id")))))
      (if (blank? rid)
          (html/page
            (list
              (html/h1 "Chat")
              (html/p* (html/attr "class" "muted") "Missing room id.")))
          (if (chat/can-access? rid)
              (if (string-eq (request-method) "POST")
                  (seq
                    (chat/say-in rid (request-param "body"))
                    (if (hx-request?)
                        (render-html (chat/kids-html (chat/room-kids rid)))
                        (redirect (string-append "/chat/room?id=" rid))))
                  (if (blank? (as-str (request-param "before")))
                      (if (hx-request?)
                          (if (blank? (as-str (request-param "partial")))
                              (render-html (chat/room-main-html rid))
                              (render-html (chat/kids-html (chat/room-kids rid))))
                          (html/page
                            (list
                              (html/h1 (chat/room-title rid))
                              (chat/room-older-initial rid)
                              (chat/room-main-html rid))))
                      (render-html (chat/room-older-html rid (request-param "before")))))
              (html/page
                (list
                  (html/h1 "Chat")
                  (html/p* (html/attr "class" "muted")
                           "You are not a member of this room.")))))))
  "Room route handler (GET /chat/room?id=<rid>): the room thread (title,
lazy-history region, declared chatpage whose composer posts to this
room only). POST says into the room; a plain HX-Request GET is the pane
body (shell + composer) or, with ?partial=1, the transcript fragment
(the log poll); a GET with ?before=<id> under HX-Request returns one
older history page. Access-checked with chat/can-access?.")

;; ---- route bindings (vault, additive at load/boot-seed) ----
;; multi-user.borg chat routes + the channel-picker surfaces (card
;; a7fbbaab). Handlers stay PRIVATE (login required; the multi-user.borg
;; chat stanza). CAUTION (2026-10-16, filmstrip footer): pp-sync
;; pull/snapshot mirrors STORE content only, so a vault sync silently
;; drops these load-time forms (the 4381766 wipe class) -- keep this
;; footer in any future snapshot.
(route "/chat" "chat/page" "private")
(route "/chat/partial" "chat/partial" "private")
(route "/chat/dm" "chat/dm-page" "private")
(route "/chat/room" "chat/room-page" "private")
(route "/chat/dm/open" "chat/dm-open" "private")

