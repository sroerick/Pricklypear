;; package ntfy -- exported from live branch system/main
;; Sync: scripts/pp_sync.py pull|push

(define ntfy/config
  (lambda
    ()
    (let
      ((rs (rows (quote ntfy-config) 5)))
      (if
        (null? rs)
        (error
          "ntfy/send: no ntfy-config row — create it (admin): (create 'ntfy-config \"{\\\"url\\\":\\\"https://ntfy.sh\\\",\\\"topic\\\":\\\"<topic>\\\",\\\"site\\\":\\\"https://pricklypear.rocks\\\"}\")")
        (cdr (car rs)))))
  "Load the ntfy-config row (first row of the type — there is exactly one); User_error if absent.")

(define ntfy/headers-json
  (lambda
    (title click token)
    (string-append
      "{\"Title\": \""
      (ntfy/json-escape title)
      "\""
      (if
        (string-eq (as-str click) "")
        ""
        (string-append
          ", \"Click\": \""
          (ntfy/json-escape click)
          "\""))
      (if
        (string-eq (as-str token) "")
        ""
        (string-append
          ", \"Authorization\": \"Bearer "
          (ntfy/json-escape token)
          "\""))
      "}"))
  "ntfy wire headers: Title, optional Click, optional Authorization.")

(define ntfy/json-esc
  (lambda
    (s i acc)
    (if
      (>= i (string-length s))
      acc
      (let
        ((c (substring s i 1)))
        (ntfy/json-esc
          s
          (+ i 1)
          (string-append
            acc
            (cond
              ((string-eq c "\\") "\\\\")
              ((string-eq c "\"") "\\\"")
              ((string-eq c "\n") "\\n")
              ((string-eq c "\r") "\\r")
              ((string-eq c "\t") "\\t")
              (else c))))))))

(define ntfy/json-escape
  (lambda (s) (ntfy/json-esc (as-str s) 0 ""))
  "Escape a string for embedding in a JSON header value.")

(define ntfy/send
  (lambda (title body) (ntfy/send/click title body ""))
  "Push a notification with no click target (see ntfy/send/click).")

(define ntfy/send/click
  (lambda
    (title body click-url)
    (let
      ((cfg (ntfy/config)))
      (let
        ((url (as-str (dict-get cfg "url")))
          (topic (as-str (dict-get cfg "topic")))
          (token-f (dict-get cfg "token")))
        (let
          ((token (if (null? token-f) "" (as-str token-f))))
          (http-post
            (string-append url "/" topic)
            (ntfy/headers-json title click-url token)
            (as-str body))))))
  "POST one ntfy message; returns the http-post payload dict (dict-get it for ok/status/err/kind). Transient network failures RAISE — inside jobs the runner's attempts/backoff owns retries.")
