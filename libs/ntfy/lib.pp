;; package ntfy -- exported from live branch system/main
;; Sync: scripts/pp-sync.sh status|pull|snapshot

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
      (json-escape title)
      "\""
      (if
        (string-eq (as-str click) "")
        ""
        (string-append
          ", \"Click\": \""
          (json-escape click)
          "\""))
      (if
        (string-eq (as-str token) "")
        ""
        (string-append
          ", \"Authorization\": \"Bearer "
          (json-escape token)
          "\""))
      "}"))
  "ntfy wire headers: Title, optional Click, optional Authorization.")

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
