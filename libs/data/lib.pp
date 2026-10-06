;; package data -- exported from live branch system/main
;; Sync: scripts/pp-sync.sh status|pull|snapshot

(define data/get-field
  (lambda
    (type-name id field)
    (dict-get (get type-name id) field)))
