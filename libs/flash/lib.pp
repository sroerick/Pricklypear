;; package flash -- exported from live branch system/main
;; Sync: scripts/pp_sync.py pull|push

(define flash/add
  (lambda
    (deck-id front back tags)
    "Create a flash-card in a deck with default fields."
    (create
      (quote flash-card)
      (flash/default-fields deck-id front back tags))))

(define flash/apply-rating
  (lambda
    (fields rating today)
    "Apply an FSRS review rating to card fields; return updated dict."
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
                today))))))))

(define flash/as-float
  (lambda (v) "Numeric as float; 0.0 when absent." (if (null? v) 0.0 (parse-float (as-str v)))))

(define flash/as-int
  (lambda (v) "Numeric as int; 0 when absent." (if (null? v) 0 (parse-int (as-str v)))))

(define flash/blank?
  blank?
  "Alias for the kernel blank? predicate.")

(define flash/card
  (lambda (id) "Fetch a flash-card row by id." (get (quote flash-card) (as-str id))))

(define flash/card-back
  (lambda (fields) "Card back text." (as-str (dict-get fields "back"))))

(define flash/card-deck-id
  (lambda (fields) "Card's owning deck id." (as-str (dict-get fields "deck_id"))))

(define flash/card-front
  (lambda (fields) "Card front text." (as-str (dict-get fields "front"))))

(define flash/card-row-html
  (lambda
    (row)
    "HTML table row for one flash-card with delete action."
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
                    (html/button (html/attr "type" "submit") "Delete")))))))))))

(define flash/cards
  (lambda
    (deck-id)
    "All flash-cards belonging to a deck."
    (let
      ((did (as-str deck-id)))
      (list/filter
        (lambda
          (row)
          (string-eq (as-str (dict-get (cdr row) "deck_id")) did))
        (rows (quote flash-card) 500)))))

(define flash/char-le?
  (lambda
    (ca cb)
    "True when char ca sorts at-or-before char cb."
    (<= (flash/digit-rank ca) (flash/digit-rank cb))))

(define flash/deck
  (lambda (id) "Fetch a flash-deck row by id." (get (quote flash-deck) (as-str id))))

(define flash/deck-archive
  (lambda
    (id)
    "Mark a deck archived."
    (update-field
      (quote flash-deck)
      (as-str id)
      "archived"
      "true")))

(define flash/deck-create
  (lambda
    (title description)
    "Create a new deck from title and description."
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
          "")))))

(define flash/deck-page
  (lambda
    ()
    "Deck route handler: GET renders, POST handles card actions."
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
        (html/page (list (flash/deck-page-html id)))))))

(define flash/deck-page-html
  (lambda
    (deck-id)
    "Full deck page HTML (header, add-card form, card table)."
    (let
      ((fields (flash/deck deck-id))
        (cards (flash/cards deck-id)))
      (if
        (null? fields)
        (html/p* (html/attr "class" "err") "Deck not found")
        (html/join
          (list
            (html/p*
              ""
              (html/a "/flashcards" "← All decks"))
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
                            (html/join (list/map flash/card-row-html cards))))))))))))))))

(define flash/deck-row-html
  (lambda
    (row)
    "HTML table row for one deck with due count and study link."
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
                  "Study")))))))))

(define flash/deck-title
  (lambda (fields) "Deck title text." (as-str (dict-get fields "title"))))

(define flash/decks
  (lambda
    ()
    "All non-archived decks."
    (list/filter
      (lambda
        (row)
        (not (flash/true? (dict-get (cdr row) "archived"))))
      (rows (quote flash-deck) 200))))

(define flash/decks-table
  (lambda
    (ds)
    "HTML table of decks, or a muted empty note."
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
              (html/join (list/map flash/deck-row-html ds)))))))))

(define flash/default-fields
  (lambda
    (deck-id front back tags)
    "Initial fields dict for a new flash-card."
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
        (flash/today)))))

(define flash/delete
  (lambda (id) "Delete a flash-card by id." (delete (quote flash-card) (as-str id))))

(define flash/digit-rank
  (lambda
    (c)
    "Sort rank for a digit character (- < 0..9 < others)."
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
                      (if (string-eq c "8") 9 (if (string-eq c "9") 10 11)))))))))))))

(define flash/due
  (lambda
    (deck-id limit)
    "Due cards in a deck (all when deck blank), up to limit."
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
        (flash/take xs lim)))))

(define flash/due-count
  (lambda (deck-id) "Number of cards due now in a deck." (count (flash/due deck-id 500))))

(define flash/due-now?
  (lambda
    (due today)
    "True when a due date is today or earlier."
    (let
      ((d (as-str due)))
      (if
        (flash/blank? d)
        #t
        (if (string-eq d today) #t (flash/str-le? d today))))))

(define flash/elapsed-days
  (lambda
    (last today)
    "Days between last review and today; 0 when never reviewed."
    (if
      (flash/blank? last)
      0
      (date-diff-days (as-str last) (as-str today)))))

(define flash/esc
  (lambda (s) "HTML-escape a string." (html-escape (as-str s))))

(define flash/explain
  (lambda
    (front back)
    "Ask the agent to explain a flashcard in plain language."
    (let
      ((sid (agent-session-new flash/explain-seed #f)))
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
          "Please explain what this means in plain language.")))))

(define flash/explain-html
  (lambda
    (text)
    "HTML card wrapping an agent explanation."
    (string-append
      "<div class=\"card\" style=\"margin-top:1rem\">"
      "<h3>Explanation</h3>"
      "<div style=\"white-space:pre-wrap\">"
      (flash/esc text)
      "</div>"
      "</div>")))

(define flash/explain-page
  (lambda
    ()
    "Agent explanation route handler for a card id."
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
                (flash/card-back fields)))))))))

(define flash/explain-seed
  "You are a patient tutor helping someone understand a flashcard. Explain the concept clearly and concisely in plain language. Use short paragraphs separated by blank lines. Do not use markdown headings or code fences unless the card itself is about code. If the card is about a specific system (e.g. Pricklypear, OCaml), assume the reader is learning that system.")

(define flash/home-html
  (lambda
    ()
    "Flashcards home body HTML (decks + new-deck form)."
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
              (list (html/h2 "Decks") (flash/decks-table ds)))))))))

(define flash/home-page
  (lambda
    ()
    "Flashcards home route handler (GET render / POST create)."
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
      (html/page (list (flash/home-html))))))

(define flash/new-deck-form
  (lambda
    ()
    "HTML form for creating a new deck."
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
                (html/button (html/attr "type" "submit") "Create")))))))))

(define flash/rating-again?
  (lambda
    (r)
    "True for the \"again\" rating (or 1)."
    (let
      ((s (as-str r)))
      (if (string-eq s "again") #t (string-eq s "1")))))

(define flash/rating-form
  (lambda
    (card-id deck rating label)
    "Inline study rating submit form."
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
          (html/button (html/attr "type" "submit") label))))))

(define flash/review
  (lambda
    (id rating)
    "Apply a rating to a card and persist it."
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
            updated))))))

(define flash/row-due?
  (lambda
    (row today)
    "True when a row is unsuspended and due today or earlier."
    (let
      ((fields (cdr row)))
      (if
        (flash/suspended? fields)
        #f
        (flash/due-now? (dict-get fields "due") today)))))

(define flash/stats
  (lambda
    (deck-id)
    "(total due suspended) counts for a deck (all when blank)."
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
              scoped)))))))

(define flash/str-le-loop
  (lambda
    (a b i na nb)
    "Tail-recursive char-wise string comparison loop."
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
            (flash/char-le? ca cb)))))))

(define flash/str-le?
  (lambda
    (a b)
    "Lexicographic string less-than-or-equal."
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
          (string-length sb))))))

(define flash/study-card-html
  (lambda
    (row deck reveal)
    "HTML for one study card with reveal + rating buttons."
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
                    (html/button (html/attr "type" "submit") "Reveal")))))))))))

(define flash/study-html
  (lambda
    (deck-id card-id reveal)
    "Study page body: next due card with reveal/rating controls."
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
              (flash/study-card-html row deck-id reveal))))))))

(define flash/study-page
  (lambda
    ()
    "Study route handler: GET renders, POST handles review rating."
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
              (if (string-eq reveal "true") "true" ""))))))))

(define flash/suspend
  (lambda
    (id)
    "Mark a flash-card suspended."
    (update-field
      (quote flash-card)
      (as-str id)
      "suspended"
      "true")))

(define flash/suspended?
  (lambda
    (fields)
    "True when card fields are suspended."
    (flash/true? (dict-get fields "suspended"))))

(define flash/table
  (lambda (body) "Wrap content in a <table>." (html/tag "table" "" body)))

(define flash/take
  (lambda
    (xs n)
    "First n elements of a list (fewer when shorter)."
    (if
      (null? xs)
      (list)
      (if
        (<= n 0)
        (list)
        (append (list (car xs)) (flash/take (cdr xs) (- n 1)))))))

(define flash/tbody
  (lambda (body) "Wrap content in a <tbody>." (html/tag "tbody" "" body)))

(define flash/td
  (lambda (body) "Wrap content in a <td>." (html/tag "td" "" body)))

(define flash/th
  (lambda (body) "Wrap content in a <th>." (html/tag "th" "" body)))

(define flash/thead
  (lambda (body) "Wrap content in a <thead>." (html/tag "thead" "" body)))

(define flash/today
  (lambda () "Today's date string." (date-today)))

(define flash/tr
  (lambda (body) "Wrap content in a <tr>." (html/tag "tr" "" body)))

(define flash/true?
  (lambda (v) "True when a value stringifies to \"true\"." (string-eq (as-str v) "true")))

(define flash/unsuspend
  (lambda
    (id)
    "Clear the suspended flag on a flash-card."
    (update-field
      (quote flash-card)
      (as-str id)
      "suspended"
      "")))

(define flash/widget
  (lambda
    ()
    "Dashboard widget with due/total card counts."
    (flash/widget-stats
      (flash/due-count "")
      (count (rows (quote flash-card) 500)))))

(define flash/widget-stats
  (lambda
    (due total)
    "Dashboard card rendering due and total counts."
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
            (html/p* (list) (html/a "/flashcards" "Manage decks"))))))))

(define flash/zz-main-sentinel
  (lambda () "MAIN-SENTINEL"))

(define flash/zz-sentinel
  (lambda () "CHANGED-99"))

