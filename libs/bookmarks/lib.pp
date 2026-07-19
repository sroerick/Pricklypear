;; Bookmarks — url + title on data_rows.
;; Install: (load-library "bookmarks") — also seeded on system/main.
;; Dogfood for agent install/test loop (smoke_route + html/*).
;;
;; Clip + bookmarklet:
;;   GET /bookmarks/clip?url=...&title=...  — save current page (auth)
;;   Install link hosted on /bookmarks (drag to bookmarks bar).

(deftype 'bookmark 'title 'url)

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

;; --- helpers -----------------------------------------------------------

(define bookmarks/starts-with?
  (lambda (s prefix)
    (let ((ss (as-str s))
          (p (as-str prefix)))
      (let ((n (string-length p)))
        (and (>= (string-length ss) n)
             (string-eq (substring ss 0 n) p))))))

(define bookmarks/url-ok?
  (lambda (url)
    (let ((u (as-str url)))
      (or (bookmarks/starts-with? u "https://")
          (bookmarks/starts-with? u "http://")))))

(define bookmarks/clip-title
  (lambda (title url)
    (let ((t (string-trim (as-str title))))
      (if (blank? t) (as-str url) t))))

(define bookmarks/find-by-url
  (lambda (url)
    (bookmarks/find-by-url-in (bookmarks/all) (as-str url))))

(define bookmarks/find-by-url-in
  (lambda (rows-list url)
    (if (null? rows-list)
        ()
        (let ((row (car rows-list)))
          (let ((fields (cdr row)))
            (if (string-eq (as-str (dict-get fields "url")) url)
                row
                (bookmarks/find-by-url-in (cdr rows-list) url)))))))

;; --- list / form UI ----------------------------------------------------

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
    (html/div (html/attr "id" "bookmarks-list")
      (let ((xs (bookmarks/all)))
        (if (null? xs)
            (html/p* (html/attr "class" "muted")
              (html/text "No bookmarks yet."))
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
                      (html/attr "placeholder" "https://..."))))
                (html/button (html/attr "type" "submit") "Add")))))))))

;; Bookmarklet origin from Host (+ X-Forwarded-Proto when present).
(define bookmarks/public-origin
  (lambda ()
    (let ((host (as-str (request-header "host")))
          (xfwd (as-str (request-header "x-forwarded-proto"))))
      (let ((scheme
              (if (or (string-eq xfwd "https")
                      (string-contains? host "pricklypear.rocks"))
                  "https"
                  "http")))
        (if (blank? host)
            ""
            (string-append scheme "://" host))))))

;; javascript: URL — navigates to /bookmarks/clip with page url+title.
(define bookmarks/bookmarklet-js
  (lambda ()
    (let ((origin (bookmarks/public-origin)))
      (string-append
        "javascript:(function(){"
        "var o="
        (json-emit origin)
        ";"
        "var u=encodeURIComponent(location.href);"
        "var t=encodeURIComponent(document.title||'');"
        "location.href=o+'/bookmarks/clip?url='+u+'&title='+t;"
        "})();"))))

(define bookmarks/install-html
  (lambda ()
    (html/div (html/attr "class" "card")
      (html/join
        (list
          (html/h2 "Browser bookmarklet")
          (html/p* (html/attr "class" "muted")
            (html/text
              "Drag the link below to your bookmarks bar. Click it on any page while signed in to save title and URL."))
          (html/tag "p" ""
            (html/tag "a"
              (html/attrs
                (list
                  (html/attr "class" "button")
                  (html/attr "href" (bookmarks/bookmarklet-js))))
              (html/text "Save to Pricklypear")))
          (html/p* (html/attr "class" "muted")
            (html/text
              "If logged out, you will sign in and return to finish the save.")))))))

(define bookmarks/main-html
  (lambda ()
    (html/div (html/attr "id" "bookmarks-main")
      (html/join
        (list
          (bookmarks/install-html)
          (bookmarks/form-html)
          (html/h2 "All bookmarks")
          (bookmarks/list-html))))))

(define bookmarks/handle-post
  (lambda ()
    (let ((action (as-str (request-param "action"))))
      (cond
        ((string-eq action "delete")
         (bookmarks/delete (request-param "id")))
        ((string-eq action "create")
         (let ((title (request-param "title"))
               (url (request-param "url")))
           (when (and (not (blank? title)) (not (blank? url)))
             (bookmarks/create title url))))
        (else ())))))

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
              (html/text
                "Save links on data_rows (type bookmark). Hosted bookmarklet clips the current tab."))
            (bookmarks/main-html))))))

;; GET /bookmarks/clip?url=&title= — bookmarklet target (auth required).
(define bookmarks/clip-page
  (lambda ()
    (let ((url (as-str (request-param "url")))
          (title (request-param "title")))
      (cond
        ((blank? url)
         (html/page
           (list
             (html/h1 "Clip bookmark")
             (html/p "Missing url query parameter.")
             (html/tag "p" ""
               (html/a "/bookmarks" (html/text "Back to bookmarks"))))))
        ((not (bookmarks/url-ok? url))
         (html/page
           (list
             (html/h1 "Clip bookmark")
             (html/p "URL must start with http:// or https://")
             (html/tag "p" ""
               (html/a "/bookmarks" (html/text "Back to bookmarks"))))))
        (else
         (let ((t (bookmarks/clip-title title url))
               (existing (bookmarks/find-by-url url)))
           (seq
             (when (null? existing)
               (bookmarks/create t url))
             (html/page
               (list
                 (html/h1
                   (if (null? existing) "Saved" "Already saved"))
                 (html/div (html/attr "class" "card")
                   (html/join
                     (list
                       (html/tag "p" "" (html/strong t))
                       (html/p* (html/attr "class" "muted")
                         (html/a url (html/text url)))
                       (html/tag "p" ""
                         (html/a "/bookmarks"
                           (html/text "View all bookmarks")))))))))))))))

(route "/bookmarks" "bookmarks/page")
(route "/bookmarks/clip" "bookmarks/clip-page")
