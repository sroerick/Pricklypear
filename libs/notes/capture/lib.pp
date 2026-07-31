;; package notes/capture -- exported from live branch admin/personal
;; Sync: scripts/pp_sync.py pull|push

(define notes/capture
  (lambda
    (title body)
    (let
      ((prev (notes/ensure-inbox)))
      (let
        ((tree (notes/get-outline prev))
          (t (notes/capture-title title body))
          (b (as-str body)))
        (let
          ((child
            (outline/set-body
              (outline/node (notes/capture-node-id tree) t)
              b)))
          (let
            ((new-tree (outline/add-child-by-id tree "root" child)))
            (seq
              (notes/upsert
                "inbox"
                "Inbox"
                new-tree
                "inbox"
                "inbox"
                "")
              "inbox")))))))

(define notes/capture/add
  (lambda
    (title body)
    (let
      ((prev (notes/doc/ensure-inbox)))
      (let
        ((tree (notes/doc/outline prev))
          (t (notes/capture/title title body))
          (b (as-str body)))
        (let
          ((child
            (outline/set-body
              (outline/node (notes/capture/node-id tree) t)
              b)))
          (let
            ((new-tree (outline/add-child-by-id tree "root" child)))
            (seq
              (notes/doc/upsert
                "inbox"
                "Inbox"
                new-tree
                "inbox"
                "inbox"
                "")
              "inbox")))))))

(define notes/capture/form
  (lambda
    ()
    (html/form
      "POST"
      "/notes"
      (html/attrs
        (list
          (html/attr "class" "card capture-card")
          (html/attr "hx-post" "/notes")
          (html/attr "hx-target" "#capture-status")
          (html/attr "hx-swap" "innerHTML")
          (html/attr
            "hx-on::after-request"
            "if(event.detail.successful){this.reset()}")))
      (html/join
        (list
          (html/hidden "action" "capture")
          (html/h2 "Capture")
          (html/p*
            (html/attr "class" "muted")
            "Drops a node into the Inbox note.")
          (html/label (html/attr "for" "capture-title") "Title")
          (html/text-input
            "title"
            (html/attrs
              (list
                (html/attr "id" "capture-title")
                (html/attr "placeholder" "optional")
                (html/attr "autocomplete" "off"))))
          (html/label (html/attr "for" "capture-body") "Note")
          (html/textarea
            "body"
            (html/attrs
              (list
                (html/attr "id" "capture-body")
                (html/attr "rows" "3")
                (html/attr "class" "capture-body")
                (html/attr "placeholder" "Quick capture226128166")
                (html/attr "required" "required")))
            "")
          (html/div
            (html/attr "class" "capture-row")
            (html/join
              (list
                (html/button (html/attr "type" "submit") "Capture")
                (html/a "/notes?slug=inbox" "Open inbox"))))
          (html/div (html/attr "id" "capture-status") ""))))))

(define notes/capture/handle
  (lambda
    ()
    (let
      ((title (request-param "title"))
        (body (request-param "body")))
      (let
        ((b (if (null? body) "" (as-str body))))
        (if
          (string-eq (string-trim b) "")
          (if
            (hx-request?)
            (render-html
              (notes/capture/status #f "Write something first."))
            (redirect "/"))
          (seq
            (notes/capture/add (if (null? title) "" title) b)
            (if
              (hx-request?)
              (render-html
                (notes/capture/status #t "Saved to Inbox."))
              (redirect "/notes?slug=inbox"))))))))

(define notes/capture/nav
  (lambda
    ()
    (html/tag
      "details"
      (html/attr "class" "menu-drop capture-drop")
      (html/join
        (list
          (html/tag
            "summary"
            (html/attr "class" "menu-drop-btn")
            "capture")
          (html/div
            (html/attr "class" "menu-drop-panel capture-panel")
            (html/form
              "POST"
              "/notes"
              (html/attrs
                (list
                  (html/attr "class" "capture-nav-form")
                  (html/attr "hx-post" "/notes")
                  (html/attr "hx-target" "#capture-status")
                  (html/attr "hx-swap" "innerHTML")
                  (html/attr
                    "hx-on::after-request"
                    "if(event.detail.successful){this.reset()}")))
              (html/join
                (list
                  (html/hidden "action" "capture")
                  (html/text-input
                    "title"
                    (html/attrs
                      (list
                        (html/attr "placeholder" "title (optional)")
                        (html/attr "autocomplete" "off")
                        (html/attr "class" "capture-nav-title"))))
                  (html/textarea
                    "body"
                    (html/attrs
                      (list
                        (html/attr "rows" "3")
                        (html/attr "class" "capture-body")
                        (html/attr "placeholder" "Quick capture226128166")
                        (html/attr "required" "required")))
                    "")
                  (html/div
                    (html/attr "class" "capture-row")
                    (html/join
                      (list
                        (html/button (html/attr "type" "submit") "Save")
                        (html/a "/notes?slug=inbox" "inbox"))))
                  (html/div (html/attr "id" "capture-status") ""))))))))))

(define notes/capture/node-id
  (lambda
    (tree)
    (string-append
      "c"
      (as-str (count (outline/kids tree)))
      "-"
      (as-str (string-length (outline/encode tree))))))

(define notes/capture/page
  (lambda
    ()
    (if
      (string-eq (request-method) "POST")
      (notes/capture/handle)
      (html/page
        (list (html/h1 "Capture") (notes/capture/form))))))

(define notes/capture/status
  (lambda
    (ok msg)
    (html/p* (html/attr "class" (if ok "ok" "err")) msg)))

(define notes/capture/title
  (lambda
    (title body)
    (let
      ((t (string-trim (as-str title))))
      (if
        (not (string-eq t ""))
        t
        (let
          ((b (string-trim (as-str body))))
          (if
            (string-eq b "")
            "capture"
            (if
              (<= (string-length b) 48)
              b
              (string-append (substring b 0 45) "..."))))))))

