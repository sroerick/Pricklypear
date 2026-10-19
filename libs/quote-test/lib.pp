;; quote-test - does the load-library reader support backslash-escaped
;; quotes inside strings? (api/eval accepts them; the file reader may not)
(package quake-test
(define qt/escaped
  (lambda ()
    "<a x=\"1\">y</a>")
  "String containing a backslash-escaped quote."))

(route "/quote-test" "qt/escaped"))
