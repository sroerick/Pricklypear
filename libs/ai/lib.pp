;; package ai -- exported from live branch system/main
;; Sync: scripts/pp_sync.py pull|push

(define ai/agents-note-body
  (lambda
    ()
    "Body of the live agents notes doc, or empty string when notes is unloaded."
    (if
      (not (ai/bound? "notes/doc/get"))
      ""
      (let
        ((n (notes/doc/get "agents")))
        (if (null? n) "" (outline/body (notes/doc/outline n)))))))

(define ai/ask
  (lambda
    (q)
    "New copilot session seeded with the image context; returns the model reply."
    (let
      ((sid (agent-session-new ai/copilot-seed)))
      (agent-say
        sid
        (string-append (ai/context) "\n\nUser question:\n" q)))))

(define ai/author-seed
  "You are a Pricklypear Lisp author on a live multi-user Lisp OS (not a cactus). Respond ONLY with a single Lisp s-expression that is a function BODY (usually a lambda). No markdown fences, no define wrapper, no commentary. Prefer pure functions. Available prims: + - * = < > list car cdr cons null? string-append str dict-get dict-set get create update delete rows render-html list-bindings whoami current-branch.")

(define ai/bound?
  (lambda
    (name)
    "True if a binding named name is visible on the current branch."
    (not
      (null?
        (list/filter
          (lambda (b) (string-eq (as-str b) (as-str name)))
          (list-bindings))))))

(define ai/context
  (lambda
    ()
    "Multi-line image context string (user, branch, bindings, agents note) for the model."
    (let
      ((agents-body (ai/agents-note-body))
        (pkgs (list-packages))
        (nbind (count (list-bindings))))
      (string-append
        "=== IMAGE CONTEXT ===\n"
        "system: Pricklypear multi-user Lisp OS image (software, not a cactus)\n"
        "user="
        (whoami)
        "\n"
        "branch="
        (current-branch)
        "\n"
        "bindings-count="
        (as-str nbind)
        "\n"
        "packages="
        (str pkgs)
        "\n"
        "explore: (describe \"pkg\") (package-bindings \"pkg\") (apropos-docs \"pat\") (function-doc \"name\")\n"
        "reference: (notes/doc/get \"platform-ref\") for the Lisp API quick-ref\n"
        "bootstrap: GET /agent.txt and /.well-known/agent.json then POST /api/eval\n"
        (if
          (string-eq agents-body "")
          "agents-note: missing — (load-library \"notes\") then (notes/doc/get \"agents\")\n"
          (string-append
            "=== AGENTS NOTE (live AGENTS.md; edit with notes/doc/upsert) ===\n"
            agents-body
            "\n=== END AGENTS NOTE ===\n"))
        "=== END CONTEXT ==="))))

(define ai/copilot-seed
  "You are the co-pilot for a running Pricklypear IMAGE (a multi-user Lisp OS), NOT the cactus genus Opuntia/Pricklypear plant.\n\nPricklypear is software: users have live images; Lisp functions are durable data in Postgres; branches version code; routes bind functions to HTTP; the UI is htmx. Kernel prims include list-bindings, get, rows, create, update, define, route, render-html, agent-session-new, agent-say, propose-function, install-function.\n\nRules:\n- Prefer facts from the prompt CONTEXT block (user, branch, bindings, etc.).\n- If CONTEXT does not contain the answer, say you do not know from this image — do not invent store contents or pretend to be a botany article.\n- Answer clearly and briefly unless asked for detail.\n- Never describe Pricklypear as a cactus unless the user is clearly asking about the plant in nature (then you may clarify the name collision).\n- When tools are available, use them to inspect the live image rather than guessing.\n- After (load-library \"notes\"), the note slug agents is this habitat's living AGENTS.md. Read it with (notes/doc/get \"agents\"). Update it with notes/doc/upsert when durable agent guidance should change.\n- Knowledge wiki: (wiki/ingest-from-source …), (wiki/query …), (wiki/lint); links are [[slug]] in outline bodies.")

(define ai/do
  (lambda
    (task)
    "New task-seeded agent session with the image context; returns the result."
    (let
      ((sid (agent-session-new ai/task-seed)))
      (agent-say
        sid
        (string-append (ai/context) "\n\nTask:\n" task)))))

(define ai/draft-fn
  (lambda
    (name spec)
    "Author session that drafts a function body from name and spec; returns the draft."
    (let
      ((sid (agent-session-new ai/author-seed #f)))
      (agent-say
        sid
        (string-append
          "Function name: "
          name
          "\nRequirement:\n"
          spec)))))

(define ai/install
  (lambda (name body) (install-function name body))
  "Install a lambda body as a function named name.")

(define ai/say
  (lambda
    (sid q)
    "Send q to an existing session sid with the image context; returns the reply."
    (agent-say
      sid
      (string-append (ai/context) "\n\nUser question:\n" q))))

(define ai/task-seed
  "You are the agent for a running Pricklypear multi-user Lisp OS image (software, not a cactus).\nTools: image_context, list_bindings, list_routes, inspect_function, install_function.\nInstall only lambda bodies. Prefer package/name. Finish with a short text summary and no tool calls.")

(define ai/write
  (lambda (name spec) (propose-function name spec))
  "Propose (draft) a function from name and spec without installing.")

