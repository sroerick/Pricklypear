;; package memory -- durable per-caller memory rows over the CRUD prims
;;
;; Consumers: the nopalito graft-drain (agent memory store) and anyone
;; who wants remember/replace/forget semantics keyed by a caller-chosen
;; stable string id ("mid") -- e.g. desktop coding sessions.
;;
;; CONTRACT
;;   * TYPE is explicit in every call (default install: "memory"; the
;;     agent's shared store: "gregor-memory" in cactus-ops);
;;   * author = caller (rows are RLS-scoped; in a shared space, members
;;     see the space's rows);
;;   * all args are strings; TAGS is a JSON array string such as
;;     "[\"ops\",\"goals\"]", spliced verbatim into the fields;
;;   * fields: mid, ver, cat (created, unix seconds), uat (updated),
;;     scope ("workspace"), ws, title, content, tags, src.
;;
;; DIALECT: lambda/let/let* bodies are single-expression on PP; wrap
;; multi-step bodies in (begin ...).
;;
;; SETUP (admin, once per type):
;;   (deftype 'memory 'mid 'ver 'cat 'uat 'scope 'ws 'title 'content 'tags 'src)
;;   ; the shared agent store additionally needs its space:
;;   (space-admit "gregor-memory" "cactus-ops")
;;
;; DESKTOP USAGE
;;   (mem/remember "memory" "m-1789" "goal" "ship P1" "[\"town\"]")
;;   (dict-get (mem/get "memory" "m-1789") "content")
;;   (mem/replace "memory" "m-1789" "goal" "ship P1 + P2" "[\"town\"]")
;;   (mem/search "memory" "P1")
;;   (mem/forget "memory" "m-1789")
;;
;; json-escape comes from libs/std (loaded at image boot).

(define mem/row-json
  (lambda (id title content tags cat ver)
    "Fields JSON for one memory row (all-string fields)."
    (string-append
      "{\"mid\":\"" (json-escape id)
      "\",\"ver\":\"" ver
      "\",\"cat\":\"" cat
      "\",\"uat\":\"" (as-str (now))
      "\",\"scope\":\"workspace\",\"ws\":\"\",\"title\":\"" (json-escape title)
      "\",\"content\":\"" (json-escape content)
      "\",\"tags\":" (if (string-eq (as-str tags) "") "[]" tags)
      ",\"src\":\"\"}")))

(define mem/field-or
  (lambda (dict key default)
    "dict-get with a default for missing or empty values."
    (let ((v (dict-get dict key)))
      (if (or (null? v) (string-eq (as-str v) "")) default v))))

(define mem/remember
  (lambda (type id title content tags)
    "Create a new memory row. Errors if a row with that mid already
exists (use mem/replace to overwrite). Returns id."
    (if (null? (find-rows type "mid" id))
        (seq
          (create type (mem/row-json id title content tags (as-str (now)) "1"))
          id)
        (error "mem/remember: row with that mid already exists; use mem/replace"))))

(define mem/replace
  (lambda (type id title content tags)
    "Create or overwrite the memory row with stable id MID. Preserves
the row's original created-at (cat) and ver on update. Returns id."
    (let* ((rs (find-rows type "mid" id))
           (existing (if (null? rs) #f (car rs)))
           (cat (if existing (mem/field-or (cdr existing) "cat" (as-str (now))) (as-str (now))))
           (ver (if existing (mem/field-or (cdr existing) "ver" "1") "1")))
        (begin
          (if existing
              (update type (car existing) (mem/row-json id title content tags cat ver))
              (create type (mem/row-json id title content tags cat ver)))
          id))))

(define mem/upsert
  (lambda (type id fields-json)
    "Create-or-replace a row by mid with caller-built fields JSON
(graft-drain primitive: the caller owns the complete field set).
Returns id."
    (let ((rs (find-rows type "mid" id)))
      (begin
        (if (null? rs)
            (create type fields-json)
            (update type (car (car rs)) fields-json))
        id))))

(define mem/forget
  (lambda (type id)
    "Delete every row whose mid = id. Returns the count deleted."
    (let ((rs (find-rows type "mid" id)))
      (begin
        (map (lambda (row) (delete type (car row))) rs)
        (length rs)))))

(define mem/get
  (lambda (type id)
    "The fields dict of the row with mid = id, or #f when absent."
    (let ((rs (find-rows type "mid" id)))
      (if (null? rs) #f (cdr (car rs))))))

(define mem/list
  (lambda (type n)
    "Up to N rows of TYPE (created-ascending), as (id fields) pairs."
    (rows type n)))

(define mem/find-by
  (lambda (type field value)
    "All rows of TYPE whose FIELD equals VALUE (exact match)."
    (find-rows type field value)))

(define mem/search
  (lambda (type needle)
    "Rows of TYPE whose content contains NEEDLE (case-insensitive
substring), capped at 50."
    (find-text type "content" needle 50)))
