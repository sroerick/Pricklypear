;; Advanced browser REPL (multipart.repl-v2) — one input, multipart transcript.
;; Dogfood for multipart/mixed. Load: (load-library "repl")
;; Optionally notes/ai. Provides a tiny dict-set* fallback when std seed
;; skipped it (list/map special-form shadow aborts mid-std load).

(package repl
  (export
    repl/page
    repl/send
    repl/answer
    repl/command
    repl/help-text))

(define repl/help-text
  (lambda ()
    "Commands: /note /eval /describe /branch /help — bare (… evals Nopales; plain text asks the LLM. Extend via (define (repl/cmd-NAME args) …) and (repl/command \"NAME\").")
  "Help blurb for /help and empty input.")

(define repl/part-html
  (lambda (html)
    (part html :target "#repl-transcript" :swap "beforeend"))
  "One transcript-append HTML part.")

(define repl/bubble
  (lambda (kind body)
    (string-append
      "<div class=\"repl-line repl-" (as-str kind) "\">"
      "<span class=\"repl-kind\">" (html-escape (as-str kind)) "</span> "
      body
      "</div>"))
  "Transcript bubble HTML.")

(define repl/text-bubble
  (lambda (kind text)
    (repl/bubble kind (html-escape (as-str text))))
  "Escaped text bubble.")

(define repl/starts-with?
  (lambda (s prefix)
    (let ((s (as-str s)) (p (as-str prefix)))
      (if (< (string-length s) (string-length p))
          #f
          (string-eq (substring s 0 (string-length p)) p))))
  "Prefix check.")

(define repl/split1
  (lambda (s)
    (let ((s (as-str s))
          (slen (string-length (as-str s))))
      (let ((rec
              (lambda (self i)
                (if (>= i slen)
                    -1
                    (if (string-eq (substring s i 1) " ")
                        i
                        (self self (+ i 1)))))))
        (let ((pos (rec rec 0)))
          (if (< pos 0)
              (list s "")
              (list
                (substring s 0 pos)
                (string-trim
                  (substring s (+ pos 1) slen))))))))
  "Split on first space → (head rest).")

;; e is (error msg kind) list from guard, or any value
(define repl/err-msg
  (lambda (e)
    (if (pair? e)
        (if (null? (cdr e))
            (str e)
            (as-str (car (cdr e))))
        (str e)))
  "Extract message from guard error value.")

;; Fallback if std dict-set* missing (notes/capture needs it).
(define dict-set*
  (lambda (j pairs)
    (if (null? pairs)
        j
        (dict-set*
          (dict-set j (car pairs) (car (cdr pairs)))
          (cdr (cdr pairs)))))
  "Multi dict-set: (dict-set* json (list k1 v1 k2 v2 …)).")

;; --- built-in commands ---

(define repl/cmd-help
  (lambda (args)
    (list (repl/part-html (repl/text-bubble "help" (repl/help-text)))))
  "/help")

(define repl/cmd-branch
  (lambda (args)
    (list
      (repl/part-html
        (repl/text-bubble "branch"
          (string-append (whoami) " @ " (current-branch))))))
  "/branch")

(define repl/cmd-describe
  (lambda (args)
    (let ((pkg (string-trim (as-str args))))
      (if (string-eq pkg "")
          (list (repl/part-html (repl/text-bubble "error" "/describe needs a package name")))
          (guard
            (lambda (e)
              (list (repl/part-html
                      (repl/text-bubble "error"
                        (string-append "describe failed: " (repl/err-msg e))))))
            (load-library pkg)
            (list
              (repl/part-html
                (repl/bubble "describe"
                  (string-append "<pre>" (html-escape (str (describe pkg)))
                                 "</pre>"))))))))
  "/describe <pkg>")

(define repl/cmd-eval
  (lambda (args)
    (repl/eval-expr (as-str args)))
  "/eval <expr>")

(define repl/cmd-note
  (lambda (args)
    (let ((text (string-trim (as-str args))))
      (if (string-eq text "")
          (list (repl/part-html (repl/text-bubble "error" "/note needs text")))
          (guard
            (lambda (e)
              (list (repl/part-html
                      (repl/text-bubble "error"
                        (string-append "note failed: " (repl/err-msg e))))))
            ;; Prefer already-loaded notes; avoid nested load-library under the
            ;; request rim (can hang on heavy package reload).
            (if (null?
                  (list/filter
                    (lambda (b) (string-eq (as-str b) "notes/capture"))
                    (list-bindings)))
                (load-library "notes")
                #t)
            (notes/capture "" text)
            (list
              (repl/part-html
                (repl/text-bubble "note"
                  (string-append "captured: " text))))))))
  "/note <text> — quick capture into notes inbox")

(define repl/command
  (lambda (name)
    (as-str name))
  "Declare a custom slash command name. Implement as (define (repl/cmd-NAME args) …) returning a list of parts.")

(define repl/run-command
  (lambda (name args)
    (let ((n (as-str name)))
      (cond
        ((string-eq n "help") (repl/cmd-help args))
        ((string-eq n "branch") (repl/cmd-branch args))
        ((string-eq n "describe") (repl/cmd-describe args))
        ((string-eq n "eval") (repl/cmd-eval args))
        ((string-eq n "note") (repl/cmd-note args))
        (else
          (list
            (repl/part-html
              (repl/text-bubble "error"
                (string-append "unknown command /" n " — try /help"))))))))
  "Dispatch slash command.")

(define repl/eval-expr
  (lambda (src)
    (guard
      (lambda (e)
        (list
          (repl/part-html
            (repl/bubble "error"
              (string-append "<pre class=\"repl-err\">"
                             (html-escape (repl/err-msg e))
                             "</pre>")))))
      (let ((v (eval-string (as-str src))))
        (list
          (repl/part-html
            (repl/bubble "value"
              (string-append "<pre>" (html-escape (str v)) "</pre>")))))))
  "Eval Nopales source; return value or error parts.")

(define repl/llm-answer
  (lambda (q)
    (guard
      (lambda (e)
        (list
          (repl/part-html
            (repl/text-bubble "error"
              (string-append "LLM failed: " (repl/err-msg e))))))
      (load-library "ai")
      (let ((ans (ai/ask q)))
        (list
          (repl/part-html
            (repl/bubble "llm"
              (string-append "<div class=\"repl-llm\">"
                             (html-escape (as-str ans))
                             "</div>")))))))
  "LLM question via libs/ai (eager part; token-delta stream deferred).")

(define repl/answer
  (lambda (raw)
    (let ((input (string-trim (as-str raw))))
      (cond
        ((string-eq input "")
          (list (repl/part-html (repl/text-bubble "help" (repl/help-text)))))
        ((repl/starts-with? input "/")
          (let ((pair (repl/split1 (substring input 1 (string-length input)))))
            (repl/run-command (car pair) (car (cdr pair)))))
        ((repl/starts-with? input "(")
          (repl/eval-expr input))
        (else (repl/llm-answer input)))))
  "Dispatch one input line → list of parts.")

(define repl/user-echo
  (lambda (input)
    (if (string-eq input "")
        '()
        (list (repl/part-html
               (repl/bubble "user"
                 (string-append "<div class=\"repl-user\">"
                                (html-escape input)
                                "</div>"))))))
  "User input echo bubble; empty for blank input (help path).")

(define repl/send
  (lambda ()
    (let ((raw (as-str (request-param "q"))))
      (let ((input (string-trim raw)))
        (let ((echo (repl/user-echo input))
              (resp (repl/answer input)))
          (multipart :target "#repl-transcript" :swap "beforeend"
            (append echo resp))))))
  "POST handler — user echo + multipart transcript parts.")

(define repl/page
  (lambda ()
    (render-html
      (string-append
        "<div class=\"repl-v2 card\">"
        "<h1>REPL</h1>"
        "<p class=\"muted\">Eval, slash commands, or ask the LLM. "
        "Responses append to the transcript (multipart).</p>"
        "<div id=\"repl-transcript\" class=\"repl-transcript\" "
        "aria-live=\"polite\"></div>"
        "<form class=\"repl-form\""
        " hx-post=\"/repl-v2/send\""
        " hx-ext=\"hx-multipart\""
        " hx-target=\"#repl-transcript\""
        " hx-swap=\"beforeend\""
        " hx-on::htmx:config-request=\"let q=(event.detail.parameters.q||'');if(q[0]!='/'&&q[0]!='('){event.detail.path='/repl-v2/llm';}\""
        " hx-on::after-request=\"this.reset()\">"
        "<label class=\"sr-only\" for=\"repl-q\">Input</label>"
        "<input id=\"repl-q\" name=\"q\" type=\"text\" "
        "placeholder=\"(+ 20 22)  ·  /note hi  ·  what packages?\" "
        "autocomplete=\"off\" autofocus />"
        "<button type=\"submit\">Send</button>"
        "</form>"
        "<p class=\"muted small\">" (html-escape (repl/help-text)) "</p>"
        "<script src=\"/assets/hx-multipart.js\"></script>"
        "<style>"
        ".repl-transcript{min-height:12rem;max-height:50vh;overflow:auto;"
        "border:1px solid #ccc;padding:.5rem;margin:.5rem 0;background:#fafafa}"
        ".repl-line{margin:.35rem 0;padding:.35rem .5rem;border-left:3px solid #888}"
        ".repl-value{border-color:#2a7}.repl-error{border-color:#a22;background:#fff5f5}"
        ".repl-llm{border-color:#26a}.repl-note{border-color:#a62}.repl-user{border-color:#555;background:#f0f0f0;font-style:italic}"
        ".repl-kind{font-size:.75rem;text-transform:uppercase;color:#666;margin-right:.35rem}"
        ".repl-form{display:flex;gap:.5rem}.repl-form input{flex:1}"
        ".sr-only{position:absolute;width:1px;height:1px;overflow:hidden;clip:rect(0,0,0,0)}"
        "</style>"
        "</div>")))
  "GET /repl-v2 page.")

(route "/repl-v2" "repl/page")
(route "/repl-v2/send" "repl/send")
