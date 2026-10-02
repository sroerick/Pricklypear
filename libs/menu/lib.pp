;; package menu -- exported from live branch system/main
;; Sync: scripts/pp_sync.py pull|push

(define menu/bar
  (lambda
    (items)
    "Render a top-level menu bar from a list of menu items."
    (html/join
      (list/map (lambda (i) (menu/render-d i 0)) items))))

(define menu/group
  (lambda (label kids) "A nested-menu item: label plus child items." (list "group" (as-str label) kids)))

(define menu/link
  (lambda
    (label href)
    "A menu link item with label and href."
    (list "link" (as-str label) (as-str href))))

(define menu/raw
  (lambda (html-str) "A menu item that renders raw HTML." (list "raw" (as-str html-str) "")))

(define menu/render
  (lambda (item) "Render a menu item at top level." (menu/render-d item 0)))

;; depth-aware renderer: d=0 top-level nav, d>0 nested fly-out.
;; Top-level groups share name="pp-nav" so the browser's native
;; <details name> accordion auto-closes siblings (replaces the old
;; chrome toggle JS). Nested groups get name="pp-sub-<label>" per
;; fly-out column, so opening one submenu closes its siblings.
(define menu/render-d
  (lambda
    (item d)
    "Depth-aware renderer: d=0 top-level nav, d>0 nested fly-out."
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
            (html/attrs
              (list
                (html/attr "class" "menu-drop")
                (html/attr
                  "name"
                  (if
                    (= d 0)
                    "pp-nav"
                    (string-append "pp-sub-" (as-str label))))))
            (html/join
              (list
                (html/tag
                  "summary"
                  (html/attr "class" "menu-drop-btn")
                  (html/text label))
                (html/div
                  (html/attr "class" "menu-drop-panel")
                  (html/join
                    (list/map
                      (lambda (k) (menu/render-d k (+ d 1)))
                      rest))))))
          (if (string-eq kind "raw") label ""))))))
