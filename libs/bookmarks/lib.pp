;; Bookmarks — url + title on data_rows.
;; Install: (load-library "bookmarks") — also seeded on system/main.
;; Dogfood for agent install/test loop (smoke_route + html/*).

(deftype 'bookmark 'title 'url)

(define bookmarks/blank?
  (lambda (s) (if (null? s) #t (string-eq (as-str s) ""))))

(define bookmarks/fields
  (lambda (title url)
    (dict-set* "{}"
      (list "title" (as-str title) "url" (as-str url)))))

(define bookmarks/all
  (lambda () (rows 'bookmark 100)))

(define bookmarks/create
  (lambda (title url)
    (create 'bookmark (bookmarks/fields title url))))

(define bookmarks/delete
  (lambda (id) (delete 'bookmark (as-str id))))

(define bookmarks/item-li
  (lambda (row)
    (let ((id (as-str (car row)))
          (fields (cdr row)))
      (let ((title (as-str (dict-get fields "title")))
            (url (as-str (dict-get fields "url"))))
        (html/li (html/attr "class" "card")
          (html/join
            (list
              (html/a url (html/text title))
              (html/p* (html/attr "class" "muted") (html/text url))
              (html/form "POST" "/bookmarks"
                (html/attrs
                  (list
                    (html/attr "style" "display:inline")
                    (html/attr "hx-post" "/bookmarks")
                    (html/attr "hx-target" "#bookmarks-main")
                    (html/attr "hx-swap" "innerHTML")))
                (html/join
                  (list
                    (html/hidden "action" "delete")
                    (html/hidden "id" id)
                    (html/button (html/attr "type" "submit") "Delete")))))))))))

(define bookmarks/list-html
  (lambda ()
    (let ((xs (bookmarks/all)))
      (html/div (html/attr "id" "bookmarks-list")
        (if (null? xs)
            (html/p* (html/attr "class" "muted") "No bookmarks yet.")
            (html/ul ""
              (html/join (list/map bookmarks/item-li xs))))))))

(define bookmarks/form-html
  (lambda ()
    (html/div (html/attr "class" "card")
      (html/join
        (list
          (html/h2 "Add bookmark")
          (html/form "POST" "/bookmarks"
            (html/attrs
              (list
                (html/attr "hx-post" "/bookmarks")
                (html/attr "hx-target" "#bookmarks-main")
                (html/attr "hx-swap" "innerHTML")
                (html/attr "hx-on::after-request"
                  "if(event.detail.successful){this.reset()}")))
            (html/join
              (list
                (html/hidden "action" "create")
                (html/label (html/attr "for" "bm-title") "Title")
                (html/text-input "title"
                  (html/attrs
                    (list
                      (html/attr "id" "bm-title")
                      (html/attr "required" "required")
                      (html/attr "placeholder" "Example"))))
                (html/label (html/attr "for" "bm-url") "URL")
                (html/text-input "url"
                  (html/attrs
                    (list
                      (html/attr "id" "bm-url")
                      (html/attr "required" "required")
                      (html/attr "placeholder" "https://…"))))
                (html/button (html/attr "type" "submit") "Add")))))))))

(define bookmarks/main-html
  (lambda ()
    (html/div (html/attr "id" "bookmarks-main")
      (html/join
        (list
          (bookmarks/form-html)
          (html/h2 "All bookmarks")
          (bookmarks/list-html))))))

(define bookmarks/handle-post
  (lambda ()
    (let ((action (as-str (request-param "action"))))
      (if (string-eq action "delete")
          (bookmarks/delete (request-param "id"))
          (if (string-eq action "create")
              (let ((title (request-param "title"))
                    (url (request-param "url")))
                (if (bookmarks/blank? title)
                    ()
                    (if (bookmarks/blank? url)
                        ()
                        (bookmarks/create title url))))
              ())))))

(define bookmarks/page
  (lambda ()
    (if (string-eq (request-method) "POST")
        (seq
          (bookmarks/handle-post)
          (if (hx-request?)
              (render-html (bookmarks/main-html))
              (redirect "/bookmarks")))
        (html/page
          (list
            (html/h1 "Bookmarks")
            (html/p* (html/attr "class" "muted")
              "Save links on data_rows (type bookmark).")
            (bookmarks/main-html))))))

(route "/bookmarks" "bookmarks/page")
