;; package ntfy -- push notifications via ntfy topics over http-post
;; Spec: borge/notify.borg (notify.ntfy-delivery)
;; Kernel dep: (http-post url [headers-json [body]]) -- image.outbound-http
;; First consumer: dabar/daily-deliver (borge/dabar.borg daily-scripture)
;;
;; CONFIG: one data row of type ntfy-config (fields dict, read via rows):
;;   url    ntfy base, no trailing slash (e.g. "https://ntfy.sh")
;;   topic  the topic path (a capability: treat like a password)
;;   token  optional bearer for protected topics ("")
;;   site   optional public habitat base used to build Click: targets
;; Create it (admin):
;;   (deftype 'ntfy-config 'url 'topic 'token 'site)
;;   (create 'ntfy-config "{\"url\":\"https://ntfy.sh\",\"topic\":\"<topic>\",\"token\":\"\",\"site\":\"https://pricklypear.rocks\"}")
;; Phone side: subscribe the ntfy app (or a UnifiedPush distributor) to
;;   <url>/<topic>. Self-hosting ntfy is a config change, not code.

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
  (lambda
    (s)
    "Escape a string for embedding in a JSON header value."
    (ntfy/json-esc (as-str s) 0 "")))

(define ntfy/config
  (lambda
    ()
    "Load the ntfy-config row (first row of the type — there is exactly one); User_error if absent."
    (let
      ((rs (rows 'ntfy-config 5)))
      (if
        (null? rs)
        (error "ntfy/send: no ntfy-config row — create it (admin): (create 'ntfy-config \"{\\\"url\\\":\\\"https://ntfy.sh\\\",\\\"topic\\\":\\\"<topic>\\\",\\\"site\\\":\\\"https://pricklypear.rocks\\\"}\")")
        (cdr (car rs))))))

(define ntfy/headers-json
  (lambda
    (title click token)
    "ntfy wire headers: Title, optional Click, optional Authorization."
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
      "}")))

(define ntfy/send/click
  (lambda
    (title body click-url)
    "POST one ntfy message; returns the http-post payload dict (dict-get it for ok/status/err/kind). Transient network failures RAISE — inside jobs the runner's attempts/backoff owns retries."
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
            (as-str body)))))))

(define ntfy/send
  (lambda
    (title body)
    "Push a notification with no click target (see ntfy/send/click)."
    (ntfy/send/click title body "")))
