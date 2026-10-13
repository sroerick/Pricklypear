;; package doasq -- the queue's own surface (roerick, 2026-10-07: the
;; queue needs to SHOW details — the full command bytes, the sha, the
;; state trail — a kanban card cannot carry an approval decision).
;; The requests live in the SHARED space (data.s, the doasq-requests
;; list) so every identity sees the same queue. The approve action is
;; gated to roerick's session username AND re-verified by the runner
;; (the event author must be roerick — a delegated admin token writes
;; as "admin" and is refused at execution time). The sha covers the
;; exact command bytes; the runner re-verifies at run time.

(package doasq
  (export doasq/page doasq/request-page doasq/items doasq/approve!))

(define doasq/list-id
  (lambda () "19eba8e4-87c8-48a8-b04f-4da6f499a180")
  "The doasq-requests list uuid (shared space).")

(define doasq/items
  (lambda
    ()
    "The queue's request cards: the shared-space todo-items whose
     list_id is the doasq-requests list. Each is (id . fields)."
    (list/filter
      (lambda
        (row)
        (string-eq
          (as-str (dict-get (cdr row) "list_id"))
          (doasq/list-id)))
      (list/map
        todo/tree/node->pair
        (list/filter
          (lambda (nj) (string-eq (as-str (dict-get nj "type")) "todo-item"))
          (tree/list (todo/tree/shared-prefix) 3 5000))))))

(define doasq/path-for
  (lambda
    (id)
    "The shared-space path of request id: the scan rows whose path
     tail (32 chars, the undashed uuid) redashes to the id."
    (let
      ((paths
        (list/map
          (lambda (n) (as-str (dict-get n "path")))
          (list/filter
            (lambda (nj) (string-eq (as-str (dict-get nj "type")) "todo-item"))
            (tree/list (todo/tree/shared-prefix) 3 5000)))))
      (let
        ((plen 32)
         (hit
          (list/filter
            (lambda
              (p)
              (string-eq
                (todo/tree/redash
                  (substring p (- (string-length p) plen) plen))
                (as-str id)))
            paths)))
        (if (null? hit) () (car hit))))))

(define doasq/approve!
  (lambda
    (id)
    "The approve action: roerick's session only (the username gate —
     a delegated admin token writes as admin and is refused HERE, and
     the runner re-refuses at execution time: the event author must be
     roerick). Writes the APPROVED state on the shared row and records
     the transition event."
    (if
      (not (string-eq (as-str (whoami)) "roerick"))
      "the approvals are roerick's to make"
      (let
        ((path (doasq/path-for id)))
        (if
          (null? path)
          "no such request"
          (let
            ((node (tree/get path))
             (fields (todo/tree/get "todo-item" id)))
            (if
              (null? fields)
              "no such request"
              (seq
                (tree/upsert
                  path
                  "datum"
                  "todo-item"
                  (dict-set fields "state" "APPROVED"))
                (todo/event/record id "SUBMITTED" "APPROVED")
                (string-append "approved " id)))))))))

(define doasq/page
    (lambda
      ()
      (let
        ((items (doasq/items)))
        (html/page
          (list
            (html/h1 (html/text "doasq — the privileged-request queue"))
            (html/p*
              (html/attr "class" "muted")
              "Nothing runs at submission. roerick approves with the full command in view; the runner re-verifies the sha and the approval attribution before anything executes.")
            (html/join
              (list/map
                (lambda
                  (row)
                  (let
                    ((fields (cdr row))
                     (id (as-str (car row))))
                    (html/p*
                      (html/attr "class" "ws-board-link")
                      (string-append
                        "["
                        (as-str (dict-get fields "state"))
                        "] "
                        (as-str (dict-get fields "title"))
                        " — "
                        (html/a
                          (string-append "/doasq/request?id=" id)
                          "review")))))
                items))))))
  "GET /doasq — the queue: every request, its state, and the review link.")

(define doasq/request-page
    (lambda
      ()
      (let
        ((id (as-str (request-param "id")))
         (action (as-str (request-param "action"))))
        (let
          ((post-result
            (if
              (string-eq action "approve")
              (doasq/approve! id)
              "")))
          (let
            ((row
              (list/filter
                (lambda (r) (string-eq (as-str (car r)) id))
                (doasq/items))))
            (if
              (null? row)
              (html/page
                (list
                  (html/h1 (html/text "doasq — no such request"))
                  (html/p* (html/attr "class" "muted") id)))
              (let
                ((fields (cdr (car row))))
                (html/page
                  (list
                    (html/h1
                      (html/text
                        (string-append "doasq — " (as-str (dict-get fields "title")))))
                    (html/p*
                      (html/attr "class" "muted")
                      (string-append
                        "state: "
                        (as-str (dict-get fields "state"))
                        " | sha: "
                        (as-str (dict-get fields "sha"))))
                    (html/p*
                      (html/attr "class" "muted")
                      (string-append
                        "requested by "
                        (as-str (dict-get fields "requester"))
                        " — "
                        (as-str (dict-get fields "why"))))
                    (html/h2 (html/text "the command, verbatim"))
                    (html/pre
                      (html/text
                        (as-str (dict-get fields "notes"))))
                    (html/p*
                      (html/attr "class" "muted")
                      post-result)
                    (if
                      (string-eq (as-str (dict-get fields "state")) "SUBMITTED")
                      (if
                        (string-eq (as-str (whoami)) "roerick")
                        (html/form
                          "post"
                          "/doasq/request"
                          ()
                          (list
                            (html/hidden "id" id)
                            (html/hidden "action" "approve")
                            (html/tag
                              "button"
                              (html/attr "type" "submit")
                              (html/text
                                "APPROVE — let hollis run this"))))
                        (html/p*
                          (html/attr "class" "muted")
                          "the approvals are roerick's to make"))
                      (html/p*
                        (html/attr "class" "muted")
                        (html/a "/doasq" "back to the queue")))))))))))
  "GET/POST /doasq/request?id=ID — the review surface: the command
   verbatim, the sha, the requester, the state, and (roerick's session
   only) the approve action. The runner re-verifies everything at
   execution time.")

(route "/doasq" "doasq/page" "private")
(route "/doasq/request" "doasq/request-page" "private")
