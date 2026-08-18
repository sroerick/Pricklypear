;; package notes/legacy -- exported from live branch system/main
;; Sync: scripts/pp_sync.py pull|push

(define notes/legacy/tree-render
  (lambda
    ()
    (html/p*
      (html/attr "class" "muted")
      "notes/tree/render retired — use notes/ui/folders-html (flat index).")))

(define notes/legacy/tree-render-folder
  (lambda
    (path names depth)
    (html/p*
      (html/attr "class" "muted")
      (string-append
        "notes/tree/render-folder retired ("
        (as-str path)
        ")"))))
