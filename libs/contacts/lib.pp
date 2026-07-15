;; Contacts — name/email/phone/notes on data_rows.
;; Install: (load-library "contacts") — also seeded on system/main.
;; Uses std blank?/when/unless/cond/list-or-empty.

(deftype 'contact 'name 'email 'phone 'notes)

(define contacts/fields
  (lambda (name email phone notes)
    (dict-set* "{}"
      (list
        "name" (as-str name)
        "email" (as-str email)
        "phone" (as-str phone)
        "notes" (as-str notes)))))

(define contacts/all
  (lambda () (rows 'contact 100)))

(define contacts/create
  (lambda (name email phone notes)
    (create 'contact (contacts/fields name email phone notes))))

(define contacts/delete
  (lambda (id) (delete 'contact (as-str id))))

(define contacts/row-id (lambda (row) (as-str (car row))))
(define contacts/row-fields (lambda (row) (cdr row)))
(define contacts/name (lambda (fields) (as-str (dict-get fields "name"))))
(define contacts/email (lambda (fields) (as-str (dict-get fields "email"))))
(define contacts/phone (lambda (fields) (as-str (dict-get fields "phone"))))
(define contacts/notes (lambda (fields) (as-str (dict-get fields "notes"))))

(define contacts/item-li
  (lambda (row)
    (let ((id (contacts/row-id row))
          (fields (contacts/row-fields row)))
      (let ((name (contacts/name fields))
            (email (contacts/email fields))
            (phone (contacts/phone fields))
            (notes (contacts/notes fields)))
        (html/li (html/attr "class" "card")
          (html/join
            (list
              (html/div (html/attr "class" "contact-name")
                (html/strong (html/text name)))
              (unless (blank? email)
                (html/p* (html/attr "class" "muted")
                  (html/a (string-append "mailto:" email) email)))
              (unless (blank? phone)
                (html/p* (html/attr "class" "muted") (html/text phone)))
              (unless (blank? notes)
                (html/p (html/text notes)))
              (html/form "POST" "/contacts"
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
  (lambda ()
    (html/div (html/attr "id" "contacts-list")
      (list-or-empty (contacts/all) "No contacts yet." contacts/item-li))))

(define contacts/form-html
  (lambda ()
    (html/div (html/attr "class" "card")
      (html/join
        (list
          (html/h2 "Add contact")
          (html/form "POST" "/contacts"
            (html/attrs
              (list
                (html/attr "hx-post" "/contacts")
                (html/attr "hx-target" "#contacts-main")
                (html/attr "hx-swap" "innerHTML")
                (html/attr "hx-on::after-request"
                  "if(event.detail.successful){this.reset()}")))
            (html/join
              (list
                (html/hidden "action" "create")
                (html/label (html/attr "for" "c-name") "Name")
                (html/text-input "name"
                  (html/attrs
                    (list
                      (html/attr "id" "c-name")
                      (html/attr "required" "required")
                      (html/attr "placeholder" "Ada Lovelace")
                      (html/attr "autocomplete" "name"))))
                (html/label (html/attr "for" "c-email") "Email")
                (html/text-input "email"
                  (html/attrs
                    (list
                      (html/attr "id" "c-email")
                      (html/attr "placeholder" "ada@example.com")
                      (html/attr "autocomplete" "email"))))
                (html/label (html/attr "for" "c-phone") "Phone")
                (html/text-input "phone"
                  (html/attrs
                    (list
                      (html/attr "id" "c-phone")
                      (html/attr "placeholder" "+1 …")
                      (html/attr "autocomplete" "tel"))))
                (html/label (html/attr "for" "c-notes") "Notes")
                (html/textarea "notes"
                  (html/attrs
                    (list
                      (html/attr "id" "c-notes")
                      (html/attr "rows" "2")
                      (html/attr "placeholder" "optional")))
                  "")
                (html/button (html/attr "type" "submit") "Add")))))))))

(define contacts/main-html
  (lambda ()
    (html/div (html/attr "id" "contacts-main")
      (html/join
        (list
          (contacts/form-html)
          (html/h2 "All contacts")
          (contacts/list-html))))))

(define contacts/handle-post
  (lambda ()
    (let ((action (as-str (request-param "action"))))
      (cond
        ((string-eq action "delete")
         (contacts/delete (request-param "id")))
        ((string-eq action "create")
         (let ((name (request-param "name")))
           (unless (blank? name)
             (contacts/create
               name
               (request-param "email")
               (request-param "phone")
               (request-param "notes")))))
        (else ())))))

(define contacts/page
  (lambda ()
    (if (string-eq (request-method) "POST")
        (seq
          (contacts/handle-post)
          (if (hx-request?)
              (render-html (contacts/main-html))
              (redirect "/contacts")))
        (html/page
          (list
            (html/h1 "Contacts")
            (html/p* (html/attr "class" "muted")
              "Simple address book on data_rows (type contact).")
            (contacts/main-html))))))

(route "/contacts" "contacts/page")
