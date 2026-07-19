;; AI helpers on top of durable agent sessions.
;; Install: (load-library "ai")
;;
;; Conversations:
;;   (define sid (agent-session-new ai/copilot-seed))
;;   (agent-say sid "what branch am I on?")
;;   (agent-history sid)
;;
;; Write path (generate + store on current branch):
;;   (ai/write "demo/double" "lambda of int x that returns (* x 2)")
;; Draft only (no store):
;;   (ai/draft-fn "demo/double" "…")
;; Then install a body you already have:
;;   (install-function "demo/double" "(lambda (x) (* x 2))")

(define ai/copilot-seed
  "You are the co-pilot for a running Pricklypear IMAGE (a multi-user Lisp OS), NOT the cactus genus Opuntia/Pricklypear plant.

Pricklypear is software: users have live images; Lisp functions are durable data in Postgres; branches version code; routes bind functions to HTTP; the UI is htmx. Kernel prims include list-bindings, get, rows, create, update, define, route, render-html, agent-session-new, agent-say, propose-function, install-function.

Rules:
- Prefer facts from the prompt CONTEXT block (user, branch, bindings, etc.).
- If CONTEXT does not contain the answer, say you do not know from this image — do not invent store contents or pretend to be a botany article.
- Answer clearly and briefly unless asked for detail.
- Never describe Pricklypear as a cactus unless the user is clearly asking about the plant in nature (then you may clarify the name collision).
- When tools are available, use them to inspect the live image rather than guessing.
- After (load-library \"notes\"), the note slug agents is this habitat's living AGENTS.md. Read it with (notes/doc/get \"agents\"). Update it with notes/doc/upsert when durable agent guidance should change.
- Knowledge wiki: (wiki/ingest-from-source …), (wiki/query …), (wiki/lint); links are [[slug]] in outline bodies.")

(define ai/author-seed
  "You are a Pricklypear Lisp author on a live multi-user Lisp OS (not a cactus). Respond ONLY with a single Lisp s-expression that is a function BODY (usually a lambda). No markdown fences, no define wrapper, no commentary. Prefer pure functions. Available prims: + - * = < > list car cdr cons null? string-append str dict-get dict-set get create update delete rows render-html list-bindings whoami current-branch.")

(define ai/task-seed
  "You are the agent for a running Pricklypear multi-user Lisp OS image (software, not a cactus).
Tools: image_context, list_bindings, list_routes, inspect_function, install_function.
Install only lambda bodies. Prefer package/name. Finish with a short text summary and no tool calls.")

;; True if a binding name is present (avoids unbound errors).
(define ai/bound?
  (lambda (name)
    (not
      (null?
        (list/filter
          (lambda (b) (string-eq (as-str b) (as-str name)))
          (list-bindings))))))

;; Body text of the live agents note (habitat AGENTS.md), or "" if missing.
;; Requires notes library already loaded in this env.
(define ai/agents-note-body
  (lambda ()
    (if (not (ai/bound? "notes/doc/get"))
        ""
        (let ((n (notes/doc/get "agents")))
          (if (null? n)
              ""
              (outline/body (notes/doc/outline n)))))))

(define ai/context
  (lambda ()
    (let ((agents-body (ai/agents-note-body)))
      (string-append
        "=== IMAGE CONTEXT ===\n"
        "system: Pricklypear multi-user Lisp OS image (software, not a cactus)\n"
        "user=" (whoami) "\n"
        "branch=" (current-branch) "\n"
        "bindings=" (str (list-bindings)) "\n"
        (if (string-eq agents-body "")
            "agents-note: missing — (load-library \"notes\") then create slug agents\n"
            (string-append
              "=== AGENTS NOTE (live AGENTS.md; edit with notes/doc/upsert) ===\n"
              agents-body
              "\n=== END AGENTS NOTE ===\n"))
        "=== END CONTEXT ==="))))

;; New session + one turn (tools on). Returns final text.
(define ai/ask
  (lambda (q)
    (let ((sid (agent-session-new ai/copilot-seed)))
      (agent-say sid
        (string-append (ai/context) "\n\nUser question:\n" q)))))

;; Continue an existing conversation.
(define ai/say
  (lambda (sid q)
    (agent-say sid
      (string-append (ai/context) "\n\nUser question:\n" q))))

;; Draft only — returns body text, does not write to the store.
(define ai/draft-fn
  (lambda (name spec)
    (let ((sid (agent-session-new ai/author-seed #f)))
      (agent-say sid
        (string-append "Function name: " name "\nRequirement:\n" spec)))))

;; Generate + install on the current branch (author = agent:<model>).
(define ai/write
  (lambda (name spec)
    (propose-function name spec)))

;; Install a body you already drafted (or edited).
(define ai/install
  (lambda (name body)
    (install-function name body)))

;; Multi-turn tool agent on a durable session.
(define ai/do
  (lambda (task)
    (let ((sid (agent-session-new ai/task-seed)))
      (agent-say sid
        (string-append (ai/context) "\n\nTask:\n" task)))))
