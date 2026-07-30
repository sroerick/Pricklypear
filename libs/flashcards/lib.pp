;; Flashcards — decks + FSRS-4.5 spaced repetition on data_rows.
;; Prefix: flash/   Package dir: flashcards
;; Install: (load-library "flashcards") — seeded after calendar.
;;
;; Types: flash-deck, flash-card
;; Scheduler: kernel prim (fsrs-schedule S D elapsed rating)
;;   -> (S' D' interval_days). New cards use S=0 D=0.
;; due is YYYY-MM-DD; empty or due <= today means due now.

(deftype 'flash-deck 'title 'description 'color 'archived)
(deftype 'flash-card
  'deck_id 'front 'back 'tags 'suspended
  'stability 'difficulty 'interval 'reps 'lapses
  'due 'last_reviewed 'created)

;; --- helpers ---

(define flash/blank? blank?
  "True if value is blank after as-str trim.")

(define flash/today (lambda () (date-today))
  "Today date string YYYY-MM-DD.")

(define flash/as-int
  (lambda (v)
    (if (null? v) 0 (parse-int (as-str v))))
  "Coerce value to int.")

(define flash/as-float
  (lambda (v)
    (if (null? v) 0.0 (parse-float (as-str v))))
  "Coerce value to float.")

(define flash/true?
  (lambda (v) (string-eq (as-str v) "true"))
  "Truthy check for bool-ish field strings.")

;; Empty due is always due; else ISO YYYY-MM-DD string order.
(define flash/due-now?
  (lambda (due today)
    (let ((d (as-str due)))
      (if (flash/blank? d)
          #t
          (if (string-eq d today)
              #t
              (flash/str-le? d today)))))
  "True if due date empty or on/before today (ISO order).")

(define flash/str-le?
  (lambda (a b)
    (let ((sa (as-str a))
          (sb (as-str b)))
      (if (string-eq sa sb)
          #t
          (flash/str-le-loop sa sb 0 (string-length sa) (string-length sb)))))
  "Lexicographic string <= comparison.")

(define flash/str-le-loop
  (lambda (a b i na nb)
    (if (>= i na)
        #t
        (if (>= i nb)
            #f
            (let ((ca (substring a i 1))
                  (cb (substring b i 1)))
              (if (string-eq ca cb)
                  (flash/str-le-loop a b (+ i 1) na nb)
                  (flash/char-le? ca cb))))))
  "Internal loop for flash/str-le?.")

(define flash/digit-rank
  (lambda (c)
    (if (string-eq c "-") 0
      (if (string-eq c "0") 1
        (if (string-eq c "1") 2
          (if (string-eq c "2") 3
            (if (string-eq c "3") 4
              (if (string-eq c "4") 5
                (if (string-eq c "5") 6
                  (if (string-eq c "6") 7
                    (if (string-eq c "7") 8
                      (if (string-eq c "8") 9
                        (if (string-eq c "9") 10 11))))))))))))
  "Rank of digit character for comparisons.")

(define flash/char-le?
  (lambda (ca cb)
    (<= (flash/digit-rank ca) (flash/digit-rank cb)))
  "Character <= comparison helper.")

(define flash/tr (lambda (body) (html/tag "tr" "" body))
  "HTML tr tag helper.")
(define flash/td (lambda (body) (html/tag "td" "" body))
  "HTML td tag helper.")
(define flash/th (lambda (body) (html/tag "th" "" body))
  "HTML th tag helper.")
(define flash/table (lambda (body) (html/tag "table" "" body))
  "HTML table tag helper.")
(define flash/thead (lambda (body) (html/tag "thead" "" body))
  "HTML thead tag helper.")
(define flash/tbody (lambda (body) (html/tag "tbody" "" body))
  "HTML tbody tag helper.")

;; --- decks ---

(define flash/decks
  (lambda ()
    (list/filter
      (lambda (row)
        (not (flash/true? (dict-get (cdr row) "archived"))))
      (rows 'flash-deck 200)))
  "List flashcard deck rows.")

(define flash/deck
  (lambda (id)
    (get 'flash-deck (as-str id)))
  "Fetch deck row by id.")

(define flash/deck-title
  (lambda (fields) (as-str (dict-get fields "title")))
  "Deck title from fields.")

(define flash/deck-create
  (lambda (title description)
    (create 'flash-deck
      (dict-set* "{}"
        (list
          "title" (as-str title)
          "description" (as-str description)
          "color" ""
          "archived" ""))))
  "Create a new deck with title.")

(define flash/deck-archive
  (lambda (id)
    (update-field 'flash-deck (as-str id) "archived" "true"))
  "Archive deck by id.")

;; --- cards ---

(define flash/default-fields
  (lambda (deck-id front back tags)
    (dict-set* "{}"
      (list
        "deck_id" (as-str deck-id)
        "front" (as-str front)
        "back" (as-str back)
        "tags" (as-str tags)
        "suspended" ""
        "stability" "0"
        "difficulty" "0"
        "interval" "0"
        "reps" "0"
        "lapses" "0"
        "due" ""
        "last_reviewed" ""
        "created" (flash/today))))
  "Default card scheduling fields dict.")

(define flash/add
  (lambda (deck-id front back tags)
    (create 'flash-card (flash/default-fields deck-id front back tags)))
  "Add card to deck with front/back.")

(define flash/card
  (lambda (id) (get 'flash-card (as-str id)))
  "Fetch card row by id.")

(define flash/delete
  (lambda (id) (delete 'flash-card (as-str id)))
  "Delete card by id.")

(define flash/suspend
  (lambda (id) (update-field 'flash-card (as-str id) "suspended" "true"))
  "Suspend card from review.")

(define flash/unsuspend
  (lambda (id) (update-field 'flash-card (as-str id) "suspended" ""))
  "Unsuspend card for review.")

(define flash/cards
  (lambda (deck-id)
    (let ((did (as-str deck-id)))
      (list/filter
        (lambda (row)
          (string-eq (as-str (dict-get (cdr row) "deck_id")) did))
        (rows 'flash-card 500))))
  "Cards belonging to deck id.")

(define flash/card-front
  (lambda (fields) (as-str (dict-get fields "front")))
  "Card front text from fields.")

(define flash/card-back
  (lambda (fields) (as-str (dict-get fields "back")))
  "Card back text from fields.")

(define flash/card-deck-id
  (lambda (fields) (as-str (dict-get fields "deck_id")))
  "Deck id from card fields.")

(define flash/suspended?
  (lambda (fields) (flash/true? (dict-get fields "suspended")))
  "True if card is suspended.")

;; --- FSRS-4.5 via kernel (fsrs-schedule) ---

(define flash/elapsed-days
  (lambda (last today)
    (if (flash/blank? last)
        0
        (date-diff-days (as-str last) (as-str today))))
  "Days elapsed since last review field.")

(define flash/rating-again?
  (lambda (r)
    (let ((s (as-str r)))
      (if (string-eq s "again") #t (string-eq s "1"))))
  "True if rating means Again.")

(define flash/apply-rating
  (lambda (fields rating today)
    (let ((s (flash/as-float (dict-get fields "stability")))
          (d (flash/as-float (dict-get fields "difficulty")))
          (last (as-str (dict-get fields "last_reviewed")))
          (reps (flash/as-int (dict-get fields "reps")))
          (lapses (flash/as-int (dict-get fields "lapses")))
          (r (as-str rating)))
      (let ((elapsed (flash/elapsed-days last today)))
        (let ((res (fsrs-schedule s d elapsed r)))
          (let ((s2 (nth res 0))
                (d2 (nth res 1))
                (iv (flash/as-int (nth res 2))))
            (dict-set* fields
              (list
                "stability" (as-str s2)
                "difficulty" (as-str d2)
                "interval" (str iv)
                "reps"
                  (str (if (flash/rating-again? r) 0 (+ reps 1)))
                "lapses"
                  (str (if (flash/rating-again? r) (+ lapses 1) lapses))
                "due"
                  (if (= iv 0) today (date-add-days today iv))
                "last_reviewed" today)))))))
  "Apply FSRS rating to card fields via kernel.")

(define flash/review
  (lambda (id rating)
    (let ((fields (flash/card id)))
      (if (null? fields)
          "review: no such card"
          (let ((updated (flash/apply-rating fields rating (flash/today))))
            (seq
              (update 'flash-card (as-str id) updated)
              updated)))))
  "Record review rating for card id.")

;; --- explain (LLM) ---

(define flash/explain-seed
  "You are a patient tutor helping someone understand a flashcard. Explain the concept clearly and concisely in plain language. Use short paragraphs separated by blank lines. Do not use markdown headings or code fences unless the card itself is about code. If the card is about a specific system (e.g. Pricklypear, OCaml), assume the reader is learning that system."
  "System seed for card-explanation LLM sessions (no tools).")

(define flash/explain
  (lambda (front back)
    (let ((sid (agent-session-new flash/explain-seed #f)))
      (agent-say sid
        (string-append
          "I don't understand this flashcard. Can you explain it to me?\n\n"
          "Front:\n" front "\n\n"
          "Back:\n" back "\n\n"
          "Please explain what this means in plain language."))))
  "Ask the LLM to explain a card's front/back in plain language; returns explanation text.")

(define flash/explain-html
  (lambda (text)
    (string-append
      "<div class=\"card\" style=\"margin-top:1rem\">"
      "<h3>Explanation</h3>"
      "<div style=\"white-space:pre-wrap\">" (flash/esc text) "</div>"
      "</div>"))
  "HTML for the explanation panel. Escapes LLM text; preserves newlines via CSS.")

(define flash/explain-page
  (lambda ()
    (let ((id (as-str (request-param "id"))))
      (let ((fields (flash/card id)))
        (if (null? fields)
            (render-html (flash/explain-html "Card not found."))
            (render-html
              (flash/explain-html
                (flash/explain (flash/card-front fields) (flash/card-back fields))))))))
  "GET /flashcards/explain?id= — LLM explanation of a card as a raw HTML div.")

(define flash/row-due?
  (lambda (row today)
    (let ((fields (cdr row)))
      (if (flash/suspended? fields)
          #f
          (flash/due-now? (dict-get fields "due") today))))
  "True if card row is due for review.")

(define flash/due
  (lambda (deck-id limit)
    (let ((today (flash/today))
          (lim (if (null? limit) 20 (flash/as-int limit)))
          (did (as-str deck-id)))
      (let ((xs
              (list/filter
                (lambda (row)
                  (if (flash/row-due? row today)
                      (if (flash/blank? did)
                          #t
                          (string-eq (flash/card-deck-id (cdr row)) did))
                      #f))
                (rows 'flash-card 500))))
        ;; Session-fresh cards (last_reviewed < today or blank) come before
        ;; cards already reviewed today, so a card rated Again sinks to the
        ;; bottom of the stack and is not re-shown until the rest of the due
        ;; batch is cleared. Created/id order preserved within each part.
        (let ((seen
                (list/filter
                  (lambda (row)
                    (string-eq (as-str (dict-get (cdr row) "last_reviewed")) today))
                  xs))
              (fresh
                (list/filter
                  (lambda (row)
                    (not (string-eq (as-str (dict-get (cdr row) "last_reviewed")) today)))
                  xs)))
          (flash/take (append fresh seen) lim)))))
  "Due cards for deck (or all), unsuspended. Session-fresh cards (not yet
  reviewed today) come before cards already reviewed today, so a card rated
  Again sinks to the bottom of the stack and is not re-shown until the rest
  of the due batch is cleared.")

(define flash/take
  (lambda (xs n)
    (if (null? xs)
        (list)
        (if (<= n 0)
            (list)
            (append (list (car xs))
                    (flash/take (cdr xs) (- n 1))))))
  "Take first n items from list.")

(define flash/due-count
  (lambda (deck-id)
    (count (flash/due deck-id 500)))
  "Count of due cards for deck.")

(define flash/stats
  (lambda (deck-id)
    (let ((today (flash/today))
          (did (as-str deck-id))
          (all (rows 'flash-card 500)))
      (let ((scoped
              (if (flash/blank? did)
                  all
                  (list/filter
                    (lambda (row)
                      (string-eq (flash/card-deck-id (cdr row)) did))
                    all))))
        (list
          (count scoped)
          (count
            (list/filter
              (lambda (row) (flash/row-due? row today))
              scoped))
          (count
            (list/filter
              (lambda (row) (flash/suspended? (cdr row)))
              scoped))))))
  "Deck stats dict (due, new, suspended counts).")

;; --- HTML ---

(define flash/esc (lambda (s) (html-escape (as-str s)))
  "HTML-escape helper. Use ONLY with raw-body builders (html/h1, flash/td
  WITHOUT an inner html/text) — those do not escape. The auto-escaping
  builders html/p and html/text already call html-escape, so wrapping their
  args with flash/esc DOUBLE-escapes (\" -> &quot; -> &amp;quot;, which the
  browser shows as the literal text '&quot;').")

(define flash/deck-row-html
  (lambda (row)
    (let ((id (as-str (car row)))
          (fields (cdr row)))
      (let ((title (flash/deck-title fields))
            (n (flash/due-count id)))
        (flash/tr
          (html/join
            (list
              (flash/td
                (html/a
                  (string-append "/flashcards/deck?id=" id)
                  (html/text title)))
              (flash/td (html/text (str n)))
              (flash/td
                (html/a
                  (string-append "/flashcards/study?deck=" id)
                  "Study"))))))))
  "Table row HTML for one deck.")

(define flash/new-deck-form
  (lambda ()
    (html/div (html/attr "class" "card")
      (html/join
        (list
          (html/h2 "New deck")
          (html/form "POST" "/flashcards"
            (html/attrs (list))
            (html/join
              (list
                (html/hidden "action" "create-deck")
                (html/label "" "Title")
                (html/text-input "title" (html/attr "required" "required"))
                (html/label "" "Description")
                (html/text-input "description" "")
                (html/button (html/attr "type" "submit") "Create"))))))))
  "Form HTML to create a deck.")

(define flash/decks-table
  (lambda (ds)
    (if (null? ds)
        (html/p* (html/attr "class" "muted") "No decks yet.")
        (flash/table
          (html/join
            (list
              (flash/thead
                (flash/tr
                  (html/join
                    (list
                      (flash/th "Deck")
                      (flash/th "Due")
                      (flash/th "")))))
              (flash/tbody
                (html/join (list/map flash/deck-row-html ds))))))))
  "Table of all decks.")

(define flash/home-html
  (lambda ()
    (let ((ds (flash/decks)))
      (html/join
        (list
          (html/h1 "Flashcards")
          (html/p* (html/attr "class" "meta")
            "Spaced repetition on shared data_rows. Data is shared across branches.")
          (html/p* "" (html/a "/flashcards/study" "Study all due"))
          (flash/new-deck-form)
          (html/div (html/attr "class" "card")
            (html/join
              (list
                (html/h2 "Decks")
                (flash/decks-table ds))))))))
  "Flashcards home panel HTML.")

(define flash/card-row-html
  (lambda (row)
    (let ((id (as-str (car row)))
          (fields (cdr row)))
      (flash/tr
        (html/join
          (list
            (flash/td (html/text (flash/card-front fields)))
            (flash/td (html/text (as-str (dict-get fields "due"))))
            (flash/td (html/text (as-str (dict-get fields "interval"))))
            (flash/td
              (html/form "POST"
                (string-append "/flashcards/deck?id="
                  (flash/card-deck-id fields))
                (html/attrs (list))
                (html/join
                  (list
                    (html/hidden "action" "delete-card")
                    (html/hidden "id" id)
                    (html/button (html/attr "type" "submit") "Delete"))))))))))
  "Table row HTML for one card in deck view.")

(define flash/deck-page-html
  (lambda (deck-id)
    (let ((fields (flash/deck deck-id))
          (cards (flash/cards deck-id)))
      (if (null? fields)
          (html/p* (html/attr "class" "err") "Deck not found")
          (html/join
            (list
              (html/p* "" (html/a "/flashcards" "← All decks"))
              (html/h1 (flash/esc (flash/deck-title fields)))
              (html/p* (html/attr "class" "meta")
                (as-str (dict-get fields "description")))
              (html/p* ""
                (html/a
                  (string-append "/flashcards/study?deck=" (as-str deck-id))
                  "Study this deck"))
              (html/div (html/attr "class" "card")
                (html/join
                  (list
                    (html/h2 "Add card")
                    (html/form "POST"
                      (string-append "/flashcards/deck?id=" (as-str deck-id))
                      (html/attrs (list))
                      (html/join
                        (list
                          (html/hidden "action" "add-card")
                          (html/label "" "Front")
                          (html/text-input "front" (html/attr "required" "required"))
                          (html/label "" "Back")
                          (html/text-input "back" (html/attr "required" "required"))
                          (html/label "" "Tags")
                          (html/text-input "tags" "")
                          (html/button (html/attr "type" "submit") "Add")))))))
              (html/div (html/attr "class" "card")
                (html/join
                  (list
                    (html/h2 "Cards")
                    (if (null? cards)
                        (html/p* (html/attr "class" "muted") "No cards yet.")
                        (flash/table
                          (html/join
                            (list
                              (flash/thead
                                (flash/tr
                                  (html/join
                                    (list
                                      (flash/th "Front")
                                      (flash/th "Due")
                                      (flash/th "Interval")
                                      (flash/th "")))))
                              (flash/tbody
                                (html/join
                                  (list/map flash/card-row-html cards)))))))))))))))
  "Deck detail page HTML with cards list.")

(define flash/rating-form
  (lambda (card-id deck rating label)
    (html/form "POST" "/flashcards/study"
      (html/attrs (list (html/attr "style" "display:inline;margin-right:.35rem")))
      (html/join
        (list
          (html/hidden "action" "review")
          (html/hidden "id" (as-str card-id))
          (html/hidden "rating" rating)
          (html/hidden "deck" (as-str deck))
          (html/button (html/attr "type" "submit") label)))))
  "Study rating buttons form for card.")

(define flash/study-card-html
  (lambda (row deck reveal)
    (let ((id (as-str (car row)))
          (fields (cdr row)))
      (html/div (html/attr "class" "card")
        (html/join
          (list
            (html/h2 "Front")
            (html/p (flash/card-front fields))
            (if (flash/true? reveal)
                (html/join
                  (list
                    (html/h2 "Back")
                    (html/p (flash/card-back fields))
                    (html/p* ""
                      (html/join
                        (list
                          (flash/rating-form id deck "again" "Again")
                          (flash/rating-form id deck "hard" "Hard")
                          (flash/rating-form id deck "good" "Good")
                          (flash/rating-form id deck "easy" "Easy"))))
                    (html/p* ""
                      (html/join
                        (list
                          (html/button
                            (html/attrs
                              (list
                                (html/attr "type" "button")
                                (html/attr "class" "secondary")
                                (html/attr "hx-get"
                                  (string-append "/flashcards/explain?id=" id))
                                (html/attr "hx-target" "#flash-explain")
                                (html/attr "hx-swap" "innerHTML")
                                (html/attr "hx-indicator" "#explain-spinner")
                                (html/attr "hx-disabled-elt" "this")))
                            "Explain")
                          (html/span
                            (html/attrs
                              (list
                                (html/attr "id" "explain-spinner")
                                (html/attr "class" "htmx-indicator")))
                            "Thinking…"))))
                    (html/div
                      (html/attrs
                        (list
                          (html/attr "id" "flash-explain")))
                      "")))
                (html/form "GET" "/flashcards/study"
                  (html/attrs (list))
                  (html/join
                    (list
                      (html/hidden "deck" (as-str deck))
                      (html/hidden "id" id)
                      (html/hidden "reveal" "true")
                      (html/button (html/attr "type" "submit") "Reveal"))))))))))
  "Single study card face HTML.")

(define flash/study-html
  (lambda (deck-id card-id reveal)
    (let ((due (flash/due deck-id 1)))
      (html/join
        (list
          (html/p* "" (html/a "/flashcards" "← Decks"))
          (html/h1 "Study")
          (if (null? due)
              (html/div (html/attr "class" "card")
                (html/join
                  (list
                    (html/p "No cards due. Nice work.")
                    (html/p* "" (html/a "/flashcards" "Back to decks")))))
              (let ((row
                      (if (flash/blank? card-id)
                          (car due)
                          (let ((found
                                  (list/filter
                                    (lambda (r)
                                      (string-eq (as-str (car r)) (as-str card-id)))
                                    due)))
                            (if (null? found) (car due) (car found))))))
                (flash/study-card-html row deck-id reveal)))))))
  "Study session panel HTML for deck.")

;; --- routes ---

(define flash/home-page
  (lambda ()
    (if (string-eq (request-method) "POST")
        (seq
          (let ((action (as-str (request-param "action"))))
            (when (string-eq action "create-deck")
              (flash/deck-create
                (request-param "title")
                (request-param "description"))))
          (redirect "/flashcards"))
        (html/page (list (flash/home-html)))))
  "GET/POST /flashcards home handler.")

(define flash/deck-page
  (lambda ()
    (let ((id (as-str (request-param "id"))))
      (if (string-eq (request-method) "POST")
          (seq
            (let ((action (as-str (request-param "action"))))
              (cond
                ((string-eq action "add-card")
                 (flash/add id
                   (request-param "front")
                   (request-param "back")
                   (request-param "tags")))
                ((string-eq action "delete-card")
                 (flash/delete (request-param "id")))
                (else ())))
            (redirect (string-append "/flashcards/deck?id=" id)))
          (html/page (list (flash/deck-page-html id))))))
  "GET/POST deck detail handler.")

(define flash/study-page
  (lambda ()
    (let ((deck (as-str (request-param "deck")))
          (id (as-str (request-param "id")))
          (reveal (as-str (request-param "reveal"))))
      (if (string-eq (request-method) "POST")
          (seq
            (let ((action (as-str (request-param "action"))))
              (when (string-eq action "review")
                (flash/review
                  (request-param "id")
                  (request-param "rating"))))
            (redirect
              (if (flash/blank? deck)
                  "/flashcards/study"
                  (string-append "/flashcards/study?deck=" deck))))
          (html/page
            (list
              (flash/study-html deck id
                (if (string-eq reveal "true") "true" "")))))))
  "GET/POST study session handler.")

(route "/flashcards" "flash/home-page")
(route "/flashcards/deck" "flash/deck-page")
(route "/flashcards/study" "flash/study-page")
(route "/flashcards/explain" "flash/explain-page")
