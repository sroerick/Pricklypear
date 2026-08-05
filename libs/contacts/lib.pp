;; package contacts -- exported from live branch system/main
;; Sync: scripts/pp_sync.py pull|push

(define contacts/add-name
  (lambda
    (name)
    "Create a contact with just the name provided"
    (create
      (quote contact)
      (dict-set*
        "{}"
        (list
          "name"
          (as-str name)
          "email"
          ""
          "phone"
          ""
          "notes"
          "")))))

(define contacts/all
  (lambda
    ()
    "Return up to 100 contact rows"
    (rows (quote contact) 100)))

(define contacts/blank?
  (lambda
    (s)
    "Check if string is null or empty"
    (if (null? s) #t (string-eq (as-str s) ""))))

(define contacts/create
  (lambda
    (name email phone notes)
    "Create a new contact with name, email, phone, notes"
    (create
      (quote contact)
      (contacts/fields name email phone notes))))

(define contacts/delete
  (lambda
    (id)
    "Delete a single contact by id"
    (delete (quote contact) (as-str id))))

(define contacts/email
  (lambda
    (fields)
    "Extract email from contact fields object"
    (as-str (dict-get fields "email"))))

(define contacts/fields
  (lambda
    (name email phone notes)
    "Build contact fields dict from name, email, phone, notes"
    (dict-set*
      "{}"
      (list
        "name"
        (as-str name)
        "email"
        (as-str email)
        "phone"
        (as-str phone)
        "notes"
        (as-str notes)))))

(define contacts/form-html
  (lambda
    ()
    "Render the HTML contact form"
    (html/div
      (html/attr "class" "card")
      (html/join
        (list
          (html/h2 "Add contact")
          (html/form
            "POST"
            "/contacts"
            (html/attrs
              (list
                (html/attr "hx-post" "/contacts")
                (html/attr "hx-target" "#contacts-main")
                (html/attr "hx-swap" "innerHTML")
                (html/attr
                  "hx-on::after-request"
                  "if(event.detail.successful){this.reset()}")))
            (html/join
              (list
                (html/hidden "action" "create")
                (html/label (html/attr "for" "c-name") "Name")
                (html/text-input
                  "name"
                  (html/attrs
                    (list
                      (html/attr "id" "c-name")
                      (html/attr "required" "required")
                      (html/attr "placeholder" "Ada Lovelace")
                      (html/attr "autocomplete" "name"))))
                (html/label (html/attr "for" "c-email") "Email")
                (html/text-input
                  "email"
                  (html/attrs
                    (list
                      (html/attr "id" "c-email")
                      (html/attr "placeholder" "ada@example.com")
                      (html/attr "autocomplete" "email"))))
                (html/label (html/attr "for" "c-phone") "Phone")
                (html/text-input
                  "phone"
                  (html/attrs
                    (list
                      (html/attr "id" "c-phone")
                      (html/attr "placeholder" "+1 …")
                      (html/attr "autocomplete" "tel"))))
                (html/label (html/attr "for" "c-notes") "Notes")
                (html/textarea
                  "notes"
                  (html/attrs
                    (list
                      (html/attr "id" "c-notes")
                      (html/attr "rows" "2")
                      (html/attr "placeholder" "optional")))
                  "")
                (html/button (html/attr "type" "submit") "Add")))))))))

(define contacts/get
  (lambda
    (id)
    "Get a specific contact by id (returns id and fields)"
    (let
      ((row (get (quote contact) (as-str id))))
      (if (null? row) () (cons (as-str id) row)))))

(define contacts/handle-post
  (lambda
    ()
    "Haitch-post handler for create/delete contacts"
    (let
      ((action (as-str (request-param "action"))))
      (cond
        ((string-eq action "delete")
          (contacts/delete (request-param "id")))
        ((string-eq action "create")
          (let
            ((name (request-param "name")))
            (unless
              (blank? name)
              (contacts/create
                name
                (request-param "email")
                (request-param "phone")
                (request-param "notes")))))
        (else ())))))

(define contacts/index-html
  (lambda
    ()
    "Render the contacts page with form and list"
    (html/page
      (list
        (html/h1 "Contacts")
        (html/p*
          (html/attr "class" "muted")
          "Manage your contacts")
        (contacts/form-html)
        (contacts/list-html)))))

(define contacts/item-li
  (lambda
    (row)
    "Render a single contact as HTML list item"
    (let
      ((id (contacts/row-id row))
        (fields (contacts/row-fields row)))
      (let
        ((name (contacts/name fields))
          (email (contacts/email fields))
          (phone (contacts/phone fields))
          (notes (contacts/notes fields)))
        (html/li
          (html/attr "class" "card")
          (html/join
            (list
              (html/div
                (html/attr "class" "contact-name")
                (html/strong (html/text name)))
              (unless
                (blank? email)
                (html/p*
                  (html/attr "class" "muted")
                  (html/a (string-append "mailto:" email) email)))
              (unless
                (blank? phone)
                (html/p* (html/attr "class" "muted") (html/text phone)))
              (unless (blank? notes) (html/p (html/text notes)))
              (html/form
                "POST"
                "/contacts"
                (html/attrs
                  (list
                    (html/attr "style" "display:inline")
                    (html/attr "hx-post" "/contacts")
                    (html/attr "hx-target" "#contacts-main")
                    (html/attr "hx-swap" "innerHTML")))
                (html/join
                  (list
                    (html/hidden "action" "delete")
                    (html/hidden "id" id)
                    (html/button (html/attr "type" "submit") "Delete")))))))))))

(define contacts/list-html
  (lambda
    ()
    "Render the list of contacts using the item-li view"
    (html/div
      (html/attr "id" "contacts-list")
      (list-or-empty
        (contacts/all)
        "No contacts yet."
        contacts/item-li))))

(define contacts/main-html
  (lambda
    ()
    "Render main contacts display with form and list"
    (html/div
      (html/attr "id" "contacts-main")
      (html/join
        (list
          (contacts/form-html)
          (html/h2 "All contacts")
          (contacts/list-html))))))

(define contacts/name
  (lambda
    (fields)
    "name field accessor for contact data"
    (as-str (dict-get fields "name"))))

(define contacts/notes
  (lambda
    (fields)
    "notes field accessor for contact data"
    (as-str (dict-get fields "notes"))))

(define contacts/page
  (lambda
    ()
    "contacts route handler (GET/POST)"
    (if
      (string-eq (request-method) "POST")
      (seq
        (contacts/handle-post)
        (if
          (hx-request?)
          (render-html (contacts/main-html))
          (redirect "/contacts")))
      (html/page
        (list
          (html/h1 "Contacts")
          (html/p*
            (html/attr "class" "muted")
            "Simple address book on data_rows (type contact).")
          (contacts/main-html))))))

(define contacts/phone
  (lambda
    (fields)
    "phone field accessor for contact data"
    (as-str (dict-get fields "phone"))))

(define contacts/row-fields
  (lambda
    (row)
    "Get fields from a contact row"
    (cdr row)))

(define contacts/row-id
  (lambda
    (row)
    "Get id from a contact row"
    (as-str (car row))))

(define contacts/update
  (lambda
    (id name email phone notes)
    "update a contact with new name, email, phone, notes"
    (update
      (quote contact)
      (as-str id)
      (contacts/fields name email phone notes))))

