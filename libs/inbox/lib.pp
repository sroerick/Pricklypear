;; package inbox -- roerick <-> gregor DM inbox (log/inbox, issues placement)
;; Built 2026-09-14 by gregor. One shared doc per habitat, cloned from the
;; log/issues placement: a single note doc (slug log/inbox), children as
;; per-node single writes, discovered GLOBALLY via tree/find-by so every
;; session (any garden) resolves the SAME doc. Multi-writer: node author
;; records the writer; the habitat grants tree writes to the doc garden.
;;
;; Children of the doc:
;;   m-<epoch>-<uuid>   one message. body dict: id, title
;;                      "<HH:MM> <from>: <first line>", body (full text),
;;                      todo = kind (dm | reply | announce | digest | test),
;;                      from = display author. Ids sort chronologically.
;;   cursor-<user>      per-user read cursor; body.title = last-read id.
;; Unread(user) = rows (todo != cursor) with from != user and id > cursor.
;;
;; Entry points:
;;   (inbox/add from kind body)        file one row (no notifications)
;;   (inbox/to-human peer kind body)   chat/say-dm + file + ntfy ping
;;   (inbox/announce body)             file kind announce + ntfy ping
;;   (inbox/unread user)               count unread for user
;;   (inbox/mark-read user [upto])     advance user's cursor
;;   (inbox/page)                      /inbox route (registered below)
;;
;; Consumers:
;;   - saguaro bridge files agent DMs (kind dm) and mirrors admin chat
;;     replies (kind reply); heartbeat files announces + pings ntfy.
;;   - the web reply form sends chat/say-dm to the agent peer, so the
;;     bridge watcher wakes the full agent (same path as XMPP replies).
;;
;; Lib deps: chat (say-dm), ntfy (send/click) -- callers load them; the
;; page reply path needs chat bound in the session (seeded lib).
;; Clock: US Mountain HH:MM, ported from chat/local-clock (chat-ui-p1).

(define inbox/slug "log/inbox")
(define inbox/agent-peer "gregor")

;; ---- clock (ported from chat-ui-p1 libs/chat: civil-from-days,
;; days-from-civil, weekday-of, nth-sunday, dst-offset, p2) ----

(define inbox/civil-from-days
  (lambda (z)
    "Days since 1970-01-01 -> (year month day), proleptic Gregorian."
    (let ((z2 (+ z 719468)))
      (let ((era (quotient z2 146097)))
        (let ((doe (- z2 (* era 146097))))
          (let ((yoe (quotient
                      (+ (- doe (quotient doe 1460))
                         (- (quotient doe 36524) (quotient doe 146096)))
                      365)))
            (let ((y (+ yoe (* era 400))))
              (let ((doy (- doe (+ (* 365 yoe)
                                   (- (quotient yoe 4) (quotient yoe 100))))))
                (let ((mp (quotient (+ (* 5 doy) 2) 153)))
                  (let ((d (+ (- doy (quotient (+ (* 153 mp) 2) 5)) 1)))
                    (let ((m (+ mp (if (< mp 10) 3 -9))))
                      (let ((y2 (+ y (if (<= m 2) 1 0))))
                        (list y2 m d)))))))))))))

(define inbox/days-from-civil
  (lambda (y m d)
    "(year month day) -> days since 1970-01-01."
    (let ((m2 (if (> m 2) (- m 3) (+ m 9))))
      (let ((yy (if (> m 2) y (- y 1))))
        (let ((era (quotient yy 400)))
          (let ((yoe (- yy (* era 400))))
            (let ((doy (+ (quotient (+ (* 153 m2) 2) 5) (- d 1))))
              (let ((doe (+ (* yoe 365)
                            (- (quotient yoe 4) (quotient yoe 100))
                            doy)))
                (- (+ (* era 146097) doe) 719468)))))))))

(define inbox/weekday-of
  (lambda (days)
    "Days since epoch -> 0=Sunday..6=Saturday (1970-01-01 Thursday)."
    (remainder (+ days 4) 7)))

(define inbox/nth-sunday
  (lambda (y m n)
    "Day-of-month of the nth Sunday of y/m."
    (+ 1
       (+ (* (- n 1) 7)
          (remainder (- 7 (inbox/weekday-of (inbox/days-from-civil y m 1))) 7)))))

(define inbox/dst-offset
  (lambda (e)
    "Seconds east of UTC for US Mountain Time at epoch e (MST -7h,
    MDT -6h). DST spans second Sunday of March 2:00 local through first
    Sunday of November 2:00 local (2007 rules)."
    (let ((y (car (inbox/civil-from-days (quotient e 86400)))))
      (let ((start (+ (* (inbox/days-from-civil y 3 (inbox/nth-sunday y 3 2)) 86400)
                      (* 9 3600)))
            (end (+ (* (inbox/days-from-civil y 11 (inbox/nth-sunday y 11 1)) 86400)
                    (* 8 3600))))
        (if (>= e start)
            (if (< e end) -21600 -25200)
            -25200)))))

(define inbox/p2
  (lambda (n)
    "Zero-pad a small integer to two digits."
    (let ((s (as-str n)))
      (if (< (string-length s) 2)
          (string-append "0" s)
          s))))

(define inbox/clock
  (lambda (e)
    "UTC epoch seconds -> HH:MM in US Mountain local time."
    (let ((local (+ e (inbox/dst-offset e))))
      (let ((ls (remainder local 86400)))
        (string-append (inbox/p2 (quotient ls 3600)) ":"
                       (inbox/p2 (quotient (remainder ls 3600) 60)))))))

;; ---- small helpers ----

(define inbox/g
  (lambda (d k)
    "dict-get as string, empty when absent or null."
    (let ((v (dict-get d k)))
      (if (null? v) "" (as-str v)))))

(define inbox/first-line
  (lambda (s)
    "First line of s, trimmed, capped at 80 chars."
    (let ((t (string-trim (as-str s))))
      (let ((nl (string-find-pos t "\n" 0 1)))
        (let ((line (if (< nl 0) t (substring t 0 nl))))
          (if (> (string-length line) 80)
              (string-append (substring line 0 80) "...")
              line))))))

(define inbox/title
  (lambda (from body)
    "Row title: <HH:MM> <from>: <first line>."
    (string-append (inbox/clock (now)) " " from ": " (inbox/first-line body))))

;; ---- doc discovery / creation (issues placement, one shared doc) ----

(define inbox/doc-path
  (lambda ()
    "Absolute tree path of the shared inbox doc, or empty when absent.
    find-by is global: every garden resolves the same doc."
    (let ((hits (tree/find-by "note" "slug" inbox/slug 1)))
      (if (null? hits)
          ""
          (inbox/g (json-parse (as-str (car hits))) "path")))))

(define inbox/ensure
  (lambda ()
    "Create the doc in the caller's garden when no doc exists anywhere;
    then return its absolute path. First creator wins (gregor on prod)."
    (let ((p (inbox/doc-path)))
      (if (string-eq p "")
          (seq
            (notes/doc/upsert inbox/slug inbox/slug
                              (notes/empty-outline inbox/slug)
                              "log" "inbox" "")
            (inbox/doc-path))
          p))))

;; ---- writes (single per-node upserts, issues/add pattern) ----

(define inbox/node-add
  (lambda (path id from kind title body)
    "Upsert one message node at path.h.<id>. Returns id."
    (seq
      (tree/upsert
        (string-append path ".h." (tree/path-label id))
        "heading" "note"
        (dict-set* "{}"
                   (list "id" id
                         "title" title
                         "body" body
                         "todo" kind
                         "from" from)))
      id)))

(define inbox/add
  (lambda (from kind body)
    "File one message row. Returns the new row id."
    (let ((path (inbox/ensure)))
      (let ((id (string-append "m-" (as-str (now)) "-" (random-uuid))))
        (inbox/node-add path id (as-str from) (as-str kind)
                        (inbox/title (as-str from) body) (as-str body))))))

(define inbox/mark-read
  (lambda (user)
    "Advance user's cursor to the newest row (mark everything read)."
    (inbox/mark-read-to user (inbox/newest-id (inbox/rows)))))

(define inbox/mark-read-to
  (lambda (user upto-id)
    "Advance user's read cursor to upto-id (single node upsert)."
    (let ((path (inbox/doc-path)))
      (let* ((u (as-str user))
             (label (tree/path-label (string-append "cursor-" u))))
        (if (string-eq path "")
            ""
            (seq
              (tree/upsert
                (string-append path ".h." label)
                "heading" "note"
                (dict-set* "{}"
                           (list "id" (string-append "cursor-" u)
                                 "title" (as-str upto-id)
                                 "body" ""
                                 "todo" "cursor"
                                 "from" u)))
              (as-str upto-id)))))))

;; ---- reads ----

(define inbox/nodes-under
  (lambda (path)
    "Parsed child nodes of the doc, path order (oldest first), cap 200."
    (list/map json-parse (tree/list path 1 200))))

(define inbox/rows
  (lambda ()
    "Message rows only (cursor nodes filtered), newest first."
    (let ((path (inbox/doc-path)))
      (if (string-eq path "")
          (list)
          (list/foldl
           (lambda (acc n) (cons n acc))
           (list)
           (list/filter
            (lambda (n)
              (let ((b (dict-get n "body")))
                (not (string-eq (inbox/g b "todo") "cursor"))))
            (inbox/nodes-under path)))))))

(define inbox/row-id
  (lambda (n)
    (inbox/g (dict-get n "body") "id")))

(define inbox/row-from
  (lambda (n)
    "Display author: body from key, falling back to the node author."
    (let ((f (inbox/g (dict-get n "body") "from")))
      (if (string-eq f "")
          (inbox/g n "author")
          f))))

(define inbox/cursor-get
  (lambda (user)
    "user's last-read row id, or empty."
    (let ((path (inbox/doc-path)))
      (if (string-eq path "")
          ""
          (let ((cur (tree/get
                      (string-append path ".h."
                                     (tree/path-label (string-append "cursor-" (as-str user)))))))
            (if (null? cur)
                ""
                (let ((b (dict-get (json-parse (as-str cur)) "body")))
                  (inbox/g b "title"))))))))

(define inbox/row-unread?
  (lambda (n user cur)
    "Row is unread for user given cursor cur."
    (if (string-eq (inbox/row-from n) (as-str user))
        #f
        (if (string-eq cur "")
            #t
            (string>? (inbox/row-id n) cur)))))

(define inbox/unread
  (lambda (user)
    "Count of unread rows for user."
    (let ((cur (inbox/cursor-get user)))
      (length
       (list/filter
        (lambda (n) (inbox/row-unread? n user cur))
        (inbox/rows))))))

(define inbox/newest-id
  (lambda (rows)
    "Highest row id (rows are newest first), or empty."
    (if (null? rows) "" (inbox/row-id (car rows)))))

;; ---- notifications (ntfy lib does the wire) ----

(define inbox/click-url
  (lambda ()
    "Habitat /inbox click target from the ntfy-config site field."
    (let ((site (inbox/g (ntfy/config) "site")))
      (if (string-eq site "") "" (string-append site "/inbox")))))

(define inbox/notify
  (lambda (body)
    "One ntfy push for a send-to-human row. Requires the ntfy lib loaded."
    (ntfy/send/click
     (string-append "inbox: " (inbox/first-line body))
     (as-str body)
     (inbox/click-url))))

(define inbox/announce
  (lambda (body)
    "File kind announce + ntfy ping (no say-dm: XMPP push is separate)."
    (let ((id (inbox/add (as-str (whoami)) "announce" body)))
      (seq (inbox/notify body) id))))

(define inbox/to-human
  (lambda (peer kind body)
    "The send-to-human path: DM thread + inbox row + ntfy ping."
    (seq
      (chat/say-dm peer (as-str body))
      (inbox/add (as-str (whoami)) kind body)
      (inbox/notify body))))

(define inbox/file-reply
  (lambda (peer body)
    "Conversational reply: DM thread + inbox row, no ntfy ping."
    (seq
      (chat/say-dm peer (as-str body))
      (inbox/add (as-str (whoami)) "dm" body))))

;; ---- /inbox page ----

(define inbox/row-html
  (lambda (n unread?)
    "One feed row: title line (bullet, author, kind) + full body."
    (let ((b (dict-get n "body")))
      (let ((title (inbox/g b "title"))
            (text (inbox/g b "body"))
            (kind (inbox/g b "todo")))
        (html/li
         (html/attr "class" (if unread? "card inbox-unread" "card"))
         (html/join
          (list
           (html/p*
            ""
            (html/join
             (list
              (if unread?
                  (html/strong (html/text "* "))
                  (html/span (html/attr "class" "muted") "  "))
              (html/strong (html/text title))
              (html/span (html/attr "class" "muted")
                         (string-append " - " kind)))))
           (html/p (html/text text)))))))))

(define inbox/rows-html
  (lambda (user cur)
    "Feed, newest first, unread rows marked against the pre-view cursor."
    (let ((rows (inbox/rows)))
      (if (null? rows)
          (html/p* (html/attr "class" "muted") "Inbox is empty.")
          (html/ul
           (html/attr "class" "card")
           (html/join
            (list/map
             (lambda (n)
               (inbox/row-html n (inbox/row-unread? n user cur)))
             rows)))))))

(define inbox/form
  (lambda ()
    "Reply form: sends to the agent peer via chat/say-dm and files kind reply."
    (html/form
     "POST"
     "/inbox"
     (html/attr "class" "card")
     (html/join
      (list
       (html/h2 "Reply to gregor")
       (html/hidden "action" "inbox-reply")
       (html/textarea
        "body"
        (html/attrs
         (list
          (html/attr "rows" "3")
          (html/attr "required" "required")
          (html/attr "maxlength" "4000")))
        "")
       " "
       (html/button (html/attr "type" "submit") (html/text "Send")))))))

(define inbox/body-html
  (lambda ()
    "Page body: unread count, feed, reply form. Marks read on view
    (cursor advances to the newest id the render showed)."
    (let ((me (as-str (whoami))))
      (let ((rows (inbox/rows)) (cur (inbox/cursor-get me)))
        (let ((unread (length
                       (list/filter
                        (lambda (n) (inbox/row-unread? n me cur))
                        rows))))
          (let ((newest (inbox/newest-id rows)))
            (seq
              (if (string-eq newest cur)
                  ()
                  (inbox/mark-read-to me newest))
              (html/join
               (list
                (html/h1 "Inbox")
                (html/p*
                 (html/attr "class" "muted")
                 (html/join
                  (list
                   (html/a "/notes?slug=log/inbox" "Raw note")
                   " - "
                   (html/a "/chat/dm?peer=gregor" "DM thread"))))
                (html/h2
                 (if (> unread 0)
                     (string-append "Unread: " (as-str unread))
                     "All read"))
                (inbox/form)
                (inbox/rows-html me cur))))))))))

(define inbox/handle-post
  (lambda ()
    "Reply action: say-dm to the agent peer + file kind reply, then back."
    (let ((action (as-str (request-param "action"))))
      (if (string-eq action "inbox-reply")
          (let ((text (string-trim (as-str (request-param "body")))))
            (if (blank? text)
                (redirect "/inbox")
                (seq
                  (chat/say-dm inbox/agent-peer text)
                  (inbox/add (as-str (whoami)) "reply" text)
                  (redirect "/inbox"))))
          (redirect "/inbox")))))

(define inbox/page
  (lambda ()
    (if (string-eq (request-method) "POST")
        (inbox/handle-post)
        (html/page (list (inbox/body-html))))))

(route "/inbox" "inbox/page")

;; ---- weekly workstream triage trigger (gregor-triage schedule) ----
;; Files a row into log/inbox for gregor; the heartbeat sweep reads
;; inbox/unread and gregor does the actual triage live (kanban placement,
;; stale flags, decision list for roerick). Defined here so the bridge's
;; inbox_lib reload re-arms it after image restarts.

(define triage/weekly
  (lambda ()
    (let* ((rows (tree/list "data.note.u.gregor.log_issues" 1 100))
           (titles (list/map (lambda (n) (dict-get (dict-get n "body") "title")) rows))
           (n (length titles)))
      (inbox/add "triage" "weekly"
                 (string-append "weekly workstream triage: "
                                (as-str n)
                                " tracked issues in log/issues. Sweep kanban + stuck cards, place them, hand roerick a decision list."))
      (string-append "triage queued: " (as-str n) " issues"))))
