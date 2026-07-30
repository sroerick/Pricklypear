;; package menu -- exported from live branch system/main
;; Sync: scripts/pp_sync.py pull|push

(define menu/bar
  (lambda
    (items)
    (html/join (list/map menu/render items))))

(define menu/group
  (lambda (label kids) (list "group" (as-str label) kids)))

(define menu/link
  (lambda
    (label href)
    (list "link" (as-str label) (as-str href))))

(define menu/raw
  (lambda (html-str) (list "raw" (as-str html-str) "")))

(define menu/render
  (lambda
    (item)
    (let
      ((kind (car item))
        (label (car (cdr item)))
        (rest (car (cdr (cdr item)))))
      (if
        (string-eq kind "link")
        (html/a rest (html/text label))
        (if
          (string-eq kind "group")
          (html/tag
            "details"
            (html/attr "class" "menu-drop")
            (html/join
              (list
                (html/tag
                  "summary"
                  (html/attr "class" "menu-drop-btn")
                  (html/text label))
                (html/div
                  (html/attr "class" "menu-drop-panel")
                  (html/join (list/map menu/render rest))))))
          (if (string-eq kind "raw") label ""))))))

