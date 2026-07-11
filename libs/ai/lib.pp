;; AI helpers on top of the kernel (agent seed prompt) portal.
;; Install: (load-library "ai")
;;
;; Write path (generate + store on current branch):
;;   (ai/write "demo/double" "lambda of int x that returns (* x 2)")
;; Draft only (no store):
;;   (ai/draft-fn "demo/double" "…")
;; Then install a body you already have:
;;   (install-function "demo/double" "(lambda (x) (* x 2))")

(define ai/copilot-seed
  "You are the co-pilot for a running Pricklypear IMAGE (a multi-user Lisp OS), NOT the cactus genus Opuntia/Pricklypear plant.

Pricklypear is software: users have live images; Lisp functions are durable data in Postgres; branches version code; routes bind functions to HTTP; the UI is htmx. Kernel prims include list-bindings, get, rows, create, update, define, route, render-html, agent, propose-function, install-function.

Rules:
- Prefer facts from the prompt CONTEXT block (user, branch, bindings, etc.).
- If CONTEXT does not contain the answer, say you do not know from this image — do not invent store contents or pretend to be a botany article.
- Answer clearly and briefly unless asked for detail.
- Never describe Pricklypear as a cactus unless the user is clearly asking about the plant in nature (then you may clarify the name collision).")

(define ai/author-seed
  "You are a Pricklypear Lisp author on a live multi-user Lisp OS (not a cactus). Respond ONLY with a single Lisp s-expression that is a function BODY (usually a lambda). No markdown fences, no define wrapper, no commentary. Prefer pure functions. Available prims: + - * = < > list car cdr cons null? string-append str dict-get dict-set get create update delete rows render-html list-bindings whoami current-branch.")

(define ai/context
  (lambda ()
    (string-append
      "=== IMAGE CONTEXT ===\n"
      "system: Pricklypear multi-user Lisp OS image (software, not a cactus)\n"
      "user=" (whoami) "\n"
      "branch=" (current-branch) "\n"
      "bindings=" (str (list-bindings)) "\n"
      "=== END CONTEXT ===")))

(define ai/ask
  (lambda (q)
    (agent ai/copilot-seed
      (string-append (ai/context) "\n\nUser question:\n" q))))

;; Draft only — returns body text, does not write to the store.
(define ai/draft-fn
  (lambda (name spec)
    (agent ai/author-seed
      (string-append "Function name: " name "\nRequirement:\n" spec))))

;; Generate + install on the current branch (author = agent:<model>).
;; Kernel: propose-function (agent draft → effect gate → define + persist).
(define ai/write
  (lambda (name spec)
    (propose-function name spec)))

;; Install a body you already drafted (or edited).
(define ai/install
  (lambda (name body)
    (install-function name body)))

;; Multi-turn tool agent: may list bindings, inspect, and install many functions.
;; Example:
;;   (ai/do "Create demo/inc and demo/dec pure lambdas, then summarize.")
(define ai/do
  (lambda (task)
    (agent-task
      (string-append (ai/context) "\n\nTask:\n" task))))
