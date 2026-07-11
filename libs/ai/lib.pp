;; AI helpers on top of the kernel (agent seed prompt) portal.
;; Install: (load-library "ai")

(define ai/copilot-seed
  "You are the Pricklypear image co-pilot. Answer clearly and briefly. Use only facts present in the prompt context (bindings, notes). If unknown, say so. Do not invent store contents.")

(define ai/author-seed
  "You are a Pricklypear Lisp author. Respond ONLY with a single Lisp s-expression that is a function BODY (usually a lambda). No markdown fences, no define wrapper, no commentary. Prefer pure functions. Available prims: + - * = < > list car cdr cons null? string-append str dict-get dict-set get create update delete rows render-html.")

(define ai/context
  (lambda ()
    (string-append
      "user=" (whoami)
      " branch=" (current-branch)
      " bindings=" (str (list-bindings)))))

(define ai/ask
  (lambda (q)
    (agent ai/copilot-seed
      (string-append (ai/context) "\n\nQuestion:\n" q))))

(define ai/draft-fn
  (lambda (name spec)
    (agent ai/author-seed
      (string-append "Function name: " name "\nRequirement:\n" spec))))
