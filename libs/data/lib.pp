;; package data -- exported from live branch system/main
;; Sync: scripts/pp_sync.py pull|push

(define data/get-field
  (lambda
    (type-name id field)
    (dict-get (get type-name id) field)))
  "dict-get one field of a data row as a JSON string; () when the row is absent."

