;; Hello page recipe — pure Lisp.

(define hi-page
  (lambda ()
    (render-html
      "<h1>Hello</h1><p>Your first Pricklypear page. Edit me at /edit/hi-page.</p>"))
  "GET /hello — minimal sample page.")

(route "/hello" "hi-page")
