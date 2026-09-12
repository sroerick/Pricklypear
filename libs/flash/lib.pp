;; package flash -- exported from live branch system/main
;; Sync: scripts/pp_sync.py pull|push

(define flash/add
  (lambda
    (deck-id front back tags)
    (create
      (quote flash-card)
      (flash/default-fields deck-id front back tags)))
  "Create a flash-card in a deck with default fields.")

(define flash/apply-rating
  (lambda
    (fields rating today)
    (let
      ((s (flash/as-float (dict-get fields "stability")))
        (d (flash/as-float (dict-get fields "difficulty")))
        (last (as-str (dict-get fields "last_reviewed")))
        (reps (flash/as-int (dict-get fields "reps")))
        (lapses (flash/as-int (dict-get fields "lapses")))
        (r (as-str rating)))
      (let
        ((elapsed (flash/elapsed-days last today)))
        (let
          ((res (fsrs-schedule s d elapsed r)))
          (let
            ((s2 (nth res 0))
              (d2 (nth res 1))
              (iv (flash/as-int (nth res 2))))
            (dict-set*
              fields
              (list
                "stability"
                (as-str s2)
                "difficulty"
                (as-str d2)
                "interval"
                (str iv)
                "reps"
                (str (if (flash/rating-again? r) 0 (+ reps 1)))
                "lapses"
                (str (if (flash/rating-again? r) (+ lapses 1) lapses))
                "due"
                (if (= iv 0) today (date-add-days today iv))
                "last_reviewed"
                today)))))))
  "Apply an FSRS review rating to card fields; return updated dict.")

(define flash/as-float
  (lambda (v) (if (null? v) 0.0 (parse-float (as-str v))))
  "Numeric as float; 0.0 when absent.")

(define flash/as-int
  (lambda (v) (if (null? v) 0 (parse-int (as-str v))))
  "Numeric as int; 0 when absent.")

(define flash/blank?
  blank?
  "Alias for the kernel blank? predicate.")

(define flash/card
  (lambda (id) (get (quote flash-card) (as-str id)))
  "Fetch a flash-card row by id.")

(define flash/card-back
  (lambda (fields) (as-str (dict-get fields "back")))
  "Card back text.")

(define flash/card-deck-id
  (lambda (fields) (as-str (dict-get fields "deck_id")))
  "Card's owning deck id.")

(define flash/card-front
  (lambda (fields) (as-str (dict-get fields "front")))
  "Card front text.")

(define flash/card-row-html
  (lambda
    (row)
    (let
      ((id (as-str (car row))) (fields (cdr row)))
      (flash/tr
        (html/join
          (list
            (flash/td (html/text (flash/card-front fields)))
            (flash/td (html/text (as-str (dict-get fields "due"))))
            (flash/td
              (html/text (as-str (dict-get fields "interval"))))
            (flash/td
              (html/form
                "POST"
                (string-append
                  "/flashcards/deck?id="
                  (flash/card-deck-id fields))
                (html/attrs (list))
                (html/join
                  (list
                    (html/hidden "action" "delete-card")
                    (html/hidden "id" id)
                    (html/button (html/attr "type" "submit") "Delete"))))))))))
  "HTML table row for one flash-card with delete action.")

(define flash/cards
  (lambda
    (deck-id)
    (let
      ((did (as-str deck-id)))
      (list/filter
        (lambda
          (row)
          (string-eq (as-str (dict-get (cdr row) "deck_id")) did))
        (rows (quote flash-card) 500))))
  "All flash-cards belonging to a deck.")

(define flash/char-le?
  (lambda
    (ca cb)
    (<= (flash/digit-rank ca) (flash/digit-rank cb)))
  "True when char ca sorts at-or-before char cb.")

(define flash/deck
  (lambda (id) (get (quote flash-deck) (as-str id)))
  "Fetch a flash-deck row by id.")

(define flash/deck-archive
  (lambda
    (id)
    (update-field
      (quote flash-deck)
      (as-str id)
      "archived"
      "true"))
  "Mark a deck archived.")

(define flash/deck-create
  (lambda
    (title description)
    (create
      (quote flash-deck)
      (dict-set*
        "{}"
        (list
          "title"
          (as-str title)
          "description"
          (as-str description)
          "color"
          ""
          "archived"
          ""))))
  "Create a new deck from title and description.")

(define flash/deck-page
  (lambda
    ()
    (let
      ((id (as-str (request-param "id"))))
      (if
        (string-eq (request-method) "POST")
        (seq
          (let
            ((action (as-str (request-param "action"))))
            (cond
              ((string-eq action "add-card")
                (flash/add
                  id
                  (request-param "front")
                  (request-param "back")
                  (request-param "tags")))
              ((string-eq action "delete-card")
                (flash/delete (request-param "id")))
              (else ())))
          (redirect (string-append "/flashcards/deck?id=" id)))
        (html/page (list (flash/deck-page-html id))))))
  "Deck route handler: GET renders, POST handles card actions.")

(define flash/deck-page-html
  (lambda
    (deck-id)
    (let
      ((fields (flash/deck deck-id))
        (cards (flash/cards deck-id)))
      (if
        (null? fields)
        (html/p* (html/attr "class" "err") "Deck not found")
        (html/join
          (list
            (html/p* "" (html/a "/flashcards" "← All decks"))
            (html/h1 (flash/esc (flash/deck-title fields)))
            (html/p*
              (html/attr "class" "meta")
              (as-str (dict-get fields "description")))
            (html/p*
              ""
              (html/a
                (string-append
                  "/flashcards/study?deck="
                  (as-str deck-id))
                "Study this deck"))
            (html/div
              (html/attr "class" "card")
              (html/join
                (list
                  (html/h2 "Add card")
                  (html/form
                    "POST"
                    (string-append "/flashcards/deck?id=" (as-str deck-id))
                    (html/attrs (list))
                    (html/join
                      (list
                        (html/hidden "action" "add-card")
                        (html/label "" "Front")
                        (html/text-input
                          "front"
                          (html/attr "required" "required"))
                        (html/label "" "Back")
                        (html/text-input
                          "back"
                          (html/attr "required" "required"))
                        (html/label "" "Tags")
                        (html/text-input "tags" "")
                        (html/button (html/attr "type" "submit") "Add")))))))
            (html/div
              (html/attr "class" "card")
              (html/join
                (list
                  (html/h2 "Cards")
                  (if
                    (null? cards)
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
                            (html/join (list/map flash/card-row-html cards)))))))))))))))
  "Full deck page HTML (header, add-card form, card table).")

(define flash/deck-row-html
  (lambda
    (row)
    (let
      ((id (as-str (car row))) (fields (cdr row)))
      (let
        ((title (flash/deck-title fields))
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
  "HTML table row for one deck with due count and study link.")

(define flash/deck-row-with-count
  (lambda
    (row n)
    (let
      ((id (as-str (car row))) (fields (cdr row)))
      (let
        ((title (flash/deck-title fields)))
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
                  "Study")))))))))

(define flash/deck-title
  (lambda (fields) (as-str (dict-get fields "title")))
  "Deck title text.")

(define flash/decks
  (lambda
    ()
    (list/filter
      (lambda
        (row)
        (not (flash/true? (dict-get (cdr row) "archived"))))
      (rows (quote flash-deck) 200)))
  "All non-archived decks.")

(define flash/decks-table
  (lambda
    (ds)
    (if
      (null? ds)
      (html/p* (html/attr "class" "muted") "No decks yet.")
      (flash/table
        (html/join
          (list
            (flash/thead
              (flash/tr
                (html/join
                  (list (flash/th "Deck") (flash/th "Due") (flash/th "")))))
            (flash/tbody
              (html/join (list/map flash/deck-row-html ds))))))))
  "HTML table of decks, or a muted empty note.")

(define flash/default-fields
  (lambda
    (deck-id front back tags)
    (dict-set*
      "{}"
      (list
        "deck_id"
        (as-str deck-id)
        "front"
        (as-str front)
        "back"
        (as-str back)
        "tags"
        (as-str tags)
        "suspended"
        ""
        "stability"
        "0"
        "difficulty"
        "0"
        "interval"
        "0"
        "reps"
        "0"
        "lapses"
        "0"
        "due"
        ""
        "last_reviewed"
        ""
        "created"
        (flash/today))))
  "Initial fields dict for a new flash-card.")

(define flash/delete
  (lambda (id) (delete (quote flash-card) (as-str id)))
  "Delete a flash-card by id.")

(define flash/digit-rank
  (lambda
    (c)
    (if
      (string-eq c "-")
      0
      (if
        (string-eq c "0")
        1
        (if
          (string-eq c "1")
          2
          (if
            (string-eq c "2")
            3
            (if
              (string-eq c "3")
              4
              (if
                (string-eq c "4")
                5
                (if
                  (string-eq c "5")
                  6
                  (if
                    (string-eq c "6")
                    7
                    (if
                      (string-eq c "7")
                      8
                      (if (string-eq c "8") 9 (if (string-eq c "9") 10 11))))))))))))
  "Sort rank for a digit character (- < 0..9 < others).")

(define flash/due
  (lambda
    (deck-id limit)
    (let
      ((today (flash/today))
        (lim (if (null? limit) 20 (flash/as-int limit)))
        (did (as-str deck-id)))
      (let
        ((xs
          (list/filter
            (lambda
              (row)
              (if
                (flash/row-due? row today)
                (if
                  (flash/blank? did)
                  #t
                  (string-eq (flash/card-deck-id (cdr row)) did))
                #f))
            (rows (quote flash-card) 500))))
        (flash/take xs lim))))
  "Due cards in a deck (all when deck blank), up to limit.")

(define flash/due-count
  (lambda (deck-id) (count (flash/due deck-id 500)))
  "Number of cards due now in a deck.")

(define flash/due-now?
  (lambda
    (due today)
    (let
      ((d (as-str due)))
      (if
        (flash/blank? d)
        #t
        (if (string-eq d today) #t (flash/str-le? d today)))))
  "True when a due date is today or earlier.")

(define flash/elapsed-days
  (lambda
    (last today)
    (if
      (flash/blank? last)
      0
      (date-diff-days (as-str last) (as-str today))))
  "Days between last review and today; 0 when never reviewed.")

(define flash/esc
  (lambda (s) (html-escape (as-str s)))
  "HTML-escape a string.")

(define flash/explain
  (lambda
    (front back)
    (let
      ((sid
        (agent-session-new
          (ai/seed+verse-range flash/explain-seed "Neh" 8 8 8)
          #f)))
      (agent-say
        sid
        (string-append
          "I don't understand this flashcard. Can you explain it to me?\n\n"
          "Front:\n"
          front
          "\n\n"
          "Back:\n"
          back
          "\n\n"
          "Please explain what this means in plain language."))))
  "Ask the agent to explain a flashcard in plain language. Session seed closes with Neh 8:8 via ai/seed+verse-range.")

(define flash/explain-html
  (lambda
    (text)
    (string-append
      "<div class=\"card\" style=\"margin-top:1rem\">"
      "<h3>Explanation</h3>"
      "<div style=\"white-space:pre-wrap\">"
      (flash/esc text)
      "</div>"
      "</div>"))
  "HTML card wrapping an agent explanation.")

(define flash/explain-page
  (lambda
    ()
    (let
      ((id (as-str (request-param "id"))))
      (let
        ((fields (flash/card id)))
        (if
          (null? fields)
          (render-html (flash/explain-html "Card not found."))
          (render-html
            (flash/explain-html
              (flash/explain
                (flash/card-front fields)
                (flash/card-back fields))))))))
  "Agent explanation route handler for a card id.")

(define flash/explain-seed
  "You are a patient tutor helping someone understand a flashcard. Explain the concept clearly and concisely in plain language. Use short paragraphs separated by blank lines. Do not use markdown headings or code fences unless the card itself is about code. If the card is about a specific system (e.g. Pricklypear, OCaml), assume the reader is learning that system.")

(define flash/graduate
  (lambda
    (id)
    (let
      ((fields (flash/card id)))
      (if
        (null? fields)
        "graduate: no such card"
        (let
          ((updated
            (dict-set* fields (list "status" "active" "due" ""))))
          (seq
            (update (quote flash-card) (as-str id) updated)
            updated))))))

(define flash/graduate-all
  (lambda
    (deck-id)
    (let
      ((qs (flash/queued deck-id)))
      (if
        (null? qs)
        0
        (seq
          (flash/graduate (car (car qs)))
          (+ 1 (flash/graduate-all-rest (cdr qs))))))))

(define flash/graduate-all-rest
  (lambda
    (qs)
    (if
      (null? qs)
      0
      (seq
        (flash/graduate (car (car qs)))
        (+ 1 (flash/graduate-all-rest (cdr qs)))))))

(define flash/home-html
  (lambda
    ()
    (let
      ((ds (flash/decks)))
      (html/join
        (list
          (html/h1 "Flashcards")
          (html/p*
            (html/attr "class" "meta")
            "Spaced repetition on shared data_rows. Data is shared across branches.")
          (html/p*
            ""
            (html/a "/flashcards/study" "Study all due"))
          (flash/new-deck-form)
          (html/div
            (html/attr "class" "card")
            (html/join
              (list (html/h2 "Decks") (flash/decks-table ds))))))))
  "Flashcards home body HTML (decks + new-deck form).")

(define flash/home-page
  (lambda
    ()
    (if
      (string-eq (request-method) "POST")
      (seq
        (let
          ((action (as-str (request-param "action"))))
          (when
            (string-eq action "create-deck")
            (flash/deck-create
              (request-param "title")
              (request-param "description"))))
        (redirect "/flashcards"))
      (html/page (list (flash/home-html)))))
  "Flashcards home route handler (GET render / POST create).")

(define flash/home-widget
  (lambda
    ()
    (let
      ((due-cards (flash/due "" 1))
        (n-due (flash/due-count "")))
      (html/div
        (html/attr "class" "card flash-home-card")
        (html/join
          (list
            (html/h2 "Flashcards")
            (if
              (null? due-cards)
              (html/join
                (list
                  (html/p* (html/attr "class" "muted") "All caught up!")
                  (html/p* "" (html/a "/flashcards" "Manage decks"))))
              (let
                ((row (car due-cards)) (fields (cdr (car due-cards))))
                (html/join
                  (list
                    (html/div
                      (html/attr "class" "flash-hw-front")
                      (html/text (flash/card-front fields)))
                    (html/div
                      (html/attr "class" "flash-hw-meta")
                      (let
                        ((deck-fields (flash/deck (flash/card-deck-id fields))))
                        (if
                          (null? deck-fields)
                          ""
                          (flash/esc (flash/deck-title deck-fields)))))
                    (html/p*
                      ""
                      (html/a
                        (string-append
                          "/flashcards/study?deck="
                          (flash/card-deck-id fields)
                          "&id="
                          (as-str (car row))
                          "&reveal=true")
                        "Reveal & Study"))
                    (html/div
                      (html/attr "class" "flash-hw-count")
                      (string-append (as-str n-due) " cards due"))))))
            (html/tag
              "style"
              ""
              (html/join
                (list
                  ".flash-home-card .flash-hw-front{font-size:1.05rem;font-weight:600;margin:.35rem 0 .15rem}"
                  ".flash-home-card .flash-hw-meta{font-size:.85rem;color:#666;margin-bottom:.35rem}"
                  ".flash-home-card .flash-hw-count{font-size:.8rem;color:#888;margin-top:.35rem}")))))))))

(define flash/junk-decks
  (lambda () (list))
  "removed helper")

(define flash/new-deck-form
  (lambda
    ()
    (html/div
      (html/attr "class" "card")
      (html/join
        (list
          (html/h2 "New deck")
          (html/form
            "POST"
            "/flashcards"
            (html/attrs (list))
            (html/join
              (list
                (html/hidden "action" "create-deck")
                (html/label "" "Title")
                (html/text-input
                  "title"
                  (html/attr "required" "required"))
                (html/label "" "Description")
                (html/text-input "description" "")
                (html/button (html/attr "type" "submit") "Create"))))))))
  "HTML form for creating a new deck.")

(define flash/purge-junk
  (lambda () (list))
  "removed helper")

(define flash/queue-row-html
  (lambda
    (row deck-id)
    (let
      ((id (as-str (car row))) (fields (cdr row)))
      (flash/tr
        (html/join
          (list
            (flash/td (html/text (flash/card-front fields)))
            (flash/td
              (html/text (as-str (dict-get fields "created"))))
            (flash/td
              (html/join
                (list
                  (html/form
                    "POST"
                    (string-append "/flashcards/deck?id=" deck-id)
                    (html/attrs (list (html/attr "style" "display:inline")))
                    (html/join
                      (list
                        (html/hidden "action" "graduate")
                        (html/hidden "id" id)
                        (html/button (html/attr "type" "submit") "Graduate"))))
                  " "
                  (html/form
                    "POST"
                    (string-append "/flashcards/deck?id=" deck-id)
                    (html/attrs (list (html/attr "style" "display:inline")))
                    (html/join
                      (list
                        (html/hidden "action" "delete-card")
                        (html/hidden "id" id)
                        (html/button (html/attr "type" "submit") "Delete")))))))))))))

(define flash/queued
  (lambda
    (deck-id)
    (let
      ((did (as-str deck-id)))
      (list/filter
        (lambda
          (row)
          (if
            (string-eq (flash/card-deck-id (cdr row)) did)
            (flash/queued? (cdr row))
            #f))
        (rows (quote flash-card) 500)))))

(define flash/queued?
  (lambda
    (fields)
    (string-eq (as-str (dict-get fields "status")) "queued")))

(define flash/rating-again?
  (lambda
    (r)
    (let
      ((s (as-str r)))
      (if (string-eq s "again") #t (string-eq s "1"))))
  "True for the \"again\" rating (or 1).")

(define flash/rating-form
  (lambda
    (card-id deck rating label)
    (html/form
      "POST"
      "/flashcards/study"
      (html/attrs
        (list
          (html/attr "style" "display:inline;margin-right:.35rem")))
      (html/join
        (list
          (html/hidden "action" "review")
          (html/hidden "id" (as-str card-id))
          (html/hidden "rating" rating)
          (html/hidden "deck" (as-str deck))
          (html/button (html/attr "type" "submit") label)))))
  "Inline study rating submit form.")

(define flash/review
  (lambda
    (id rating)
    (let
      ((fields (flash/card id)))
      (if
        (null? fields)
        "review: no such card"
        (let
          ((updated
            (flash/apply-rating fields rating (flash/today))))
          (seq
            (update (quote flash-card) (as-str id) updated)
            updated)))))
  "Apply a rating to a card and persist it.")

(define flash/row-due?
  (lambda
    (row today)
    (let
      ((fields (cdr row)))
      (if
        (flash/suspended? fields)
        #f
        (flash/due-now? (dict-get fields "due") today))))
  "True when a row is unsuspended and due today or earlier.")

(define flash/stats
  (lambda
    (deck-id)
    (let
      ((today (flash/today))
        (did (as-str deck-id))
        (all (rows (quote flash-card) 500)))
      (let
        ((scoped
          (if
            (flash/blank? did)
            all
            (list/filter
              (lambda
                (row)
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
  "(total due suspended) counts for a deck (all when blank).")

(define flash/str-le-loop
  (lambda
    (a b i na nb)
    (if
      (>= i na)
      #t
      (if
        (>= i nb)
        #f
        (let
          ((ca (substring a i 1)) (cb (substring b i 1)))
          (if
            (string-eq ca cb)
            (flash/str-le-loop a b (+ i 1) na nb)
            (flash/char-le? ca cb))))))
  "Tail-recursive char-wise string comparison loop.")

(define flash/str-le?
  (lambda
    (a b)
    (let
      ((sa (as-str a)) (sb (as-str b)))
      (if
        (string-eq sa sb)
        #t
        (flash/str-le-loop
          sa
          sb
          0
          (string-length sa)
          (string-length sb)))))
  "Lexicographic string less-than-or-equal.")

(define flash/study-card-html
  (lambda
    (row deck reveal)
    (let
      ((id (as-str (car row))) (fields (cdr row)))
      (html/div
        (html/attr "class" "card")
        (html/join
          (list
            (html/h2 "Front")
            (html/p (flash/card-front fields))
            (if
              (flash/true? reveal)
              (html/join
                (list
                  (html/h2 "Back")
                  (html/p (flash/card-back fields))
                  (html/p*
                    ""
                    (html/join
                      (list
                        (flash/rating-form id deck "again" "Again")
                        (flash/rating-form id deck "hard" "Hard")
                        (flash/rating-form id deck "good" "Good")
                        (flash/rating-form id deck "easy" "Easy"))))))
              (html/form
                "GET"
                "/flashcards/study"
                (html/attrs (list))
                (html/join
                  (list
                    (html/hidden "deck" (as-str deck))
                    (html/hidden "id" id)
                    (html/hidden "reveal" "true")
                    (html/button (html/attr "type" "submit") "Reveal"))))))))))
  "HTML for one study card with reveal + rating buttons.")

(define flash/study-html
  (lambda
    (deck-id card-id reveal)
    (let
      ((due (flash/due deck-id 1)))
      (html/join
        (list
          (html/p* "" (html/a "/flashcards" "← Decks"))
          (html/h1 "Study")
          (if
            (null? due)
            (html/div
              (html/attr "class" "card")
              (html/join
                (list
                  (html/p "No cards due. Nice work.")
                  (html/p* "" (html/a "/flashcards" "Back to decks")))))
            (let
              ((row
                (if
                  (flash/blank? card-id)
                  (car due)
                  (let
                    ((found
                      (list/filter
                        (lambda
                          (r)
                          (string-eq (as-str (car r)) (as-str card-id)))
                        due)))
                    (if (null? found) (car due) (car found))))))
              (flash/study-card-html row deck-id reveal)))))))
  "Study page body: next due card with reveal/rating controls.")

(define flash/study-page
  (lambda
    ()
    (let
      ((deck (as-str (request-param "deck")))
        (id (as-str (request-param "id")))
        (reveal (as-str (request-param "reveal"))))
      (if
        (string-eq (request-method) "POST")
        (seq
          (let
            ((action (as-str (request-param "action"))))
            (when
              (string-eq action "review")
              (flash/review
                (request-param "id")
                (request-param "rating"))))
          (redirect
            (if
              (flash/blank? deck)
              "/flashcards/study"
              (string-append "/flashcards/study?deck=" deck))))
        (html/page
          (list
            (flash/study-html
              deck
              id
              (if (string-eq reveal "true") "true" "")))))))
  "Study route handler: GET renders, POST handles review rating.")

(define flash/suspend
  (lambda
    (id)
    (update-field
      (quote flash-card)
      (as-str id)
      "suspended"
      "true"))
  "Mark a flash-card suspended.")

(define flash/suspended?
  (lambda
    (fields)
    (flash/true? (dict-get fields "suspended")))
  "True when card fields are suspended.")

(define flash/table
  (lambda (body) (html/tag "table" "" body))
  "Wrap content in a <table>.")

(define flash/take
  (lambda
    (xs n)
    (if
      (null? xs)
      (list)
      (if
        (<= n 0)
        (list)
        (append (list (car xs)) (flash/take (cdr xs) (- n 1))))))
  "First n elements of a list (fewer when shorter).")

(define flash/tbody
  (lambda (body) (html/tag "tbody" "" body))
  "Wrap content in a <tbody>.")

(define flash/td
  (lambda (body) (html/tag "td" "" body))
  "Wrap content in a <td>.")

(define flash/th
  (lambda (body) (html/tag "th" "" body))
  "Wrap content in a <th>.")

(define flash/thead
  (lambda (body) (html/tag "thead" "" body))
  "Wrap content in a <thead>.")

(define flash/today
  (lambda () (date-today))
  "Today's date string.")

(define flash/tr
  (lambda (body) (html/tag "tr" "" body))
  "Wrap content in a <tr>.")

(define flash/true?
  (lambda (v) (string-eq (as-str v) "true"))
  "True when a value stringifies to \"true\".")

(define flash/unsuspend
  (lambda
    (id)
    (update-field
      (quote flash-card)
      (as-str id)
      "suspended"
      ""))
  "Clear the suspended flag on a flash-card.")

(define flash/widget
  (lambda
    ()
    (flash/widget-stats
      (flash/due-count "")
      (count (rows (quote flash-card) 500))))
  "Dashboard widget with due/total card counts.")

(define flash/widget-stats
  (lambda
    (due total)
    (layout/card
      "Flashcards"
      (html/div
        (list)
        (html/join
          (list
            (html/p*
              (html/attr "class" "flash-widget-due")
              (html/join
                (list
                  (html/strong (str due))
                  (html/text " cards due today"))))
            (html/p*
              (list)
              (html/text (string-append (str total) " total cards")))
            (if
              (> due 0)
              (layout/cta "/flashcards/study" "Study now")
              (html/p*
                (html/attr "class" "muted")
                "Nothing due — you're all caught up!"))
            (html/p* (list) (html/a "/flashcards" "Manage decks")))))))
  "Dashboard card rendering due and total counts.")

(define flash/zz-main-sentinel
  (lambda () "MAIN-SENTINEL"))

(define flash/zz-sentinel
  (lambda () "CHANGED-99"))
