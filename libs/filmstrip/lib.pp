;; package filmstrip -- exported from live branch system/main
;; Sync: scripts/pp_sync.py pull|push

(package filmstrip
  (export
    filmstrip/page
    filmstrip/css
    filmstrip/ws-fields
    filmstrip/browse
    filmstrip/scene
    filmstrip/native-state
    filmstrip/native-open
    filmstrip/native-close
    filmstrip/native-focus
    filmstrip/native-move
    filmstrip/pane-stamp
    filmstrip/hint-routes
    filmstrip/launcher-hint
))

(define filmstrip/apps
  (lambda
    ()
    (list
      (list "todos" "todo")
      (list "notes" "notes")
      (list "calendar" "calendar")))
  "Column 0: the app chooser. Entries are (label . target); target is the\npackage key that names the browse below.")

(define filmstrip/browse
  (lambda
    (path)
    (let
      ((apps (filmstrip/apps)))
      (if
        (null? path)
        (cons (list apps) #f)
        (let
          ((r
            (filmstrip/pkg-browse (as-str (car path)) (cdr path))))
          (cons (cons apps (car r)) (cdr r))))))
  "Shell cascade: column 0 is the app chooser; deeper columns come from\nthe selected package's browse. Returns (columns . leaf?).")

(define filmstrip/browse-walk
  (lambda
    (childfn cols path)
    (if
      (null? path)
      (cons
        cols
        (filmstrip/col-leaf? (filmstrip/last-col cols)))
      (if
        (filmstrip/route? (car path))
        (cons
          cols
          (filmstrip/col-leaf? (filmstrip/last-col cols)))
        (filmstrip/browse-walk
          childfn
          (append cols (list (childfn (as-str (car path)))))
          (cdr path)))))
  "Shared Miller walk: column i lists the children of path[i-1]. Entries\nwhose target is a route are leaves; the walk stops there. Returns\n(columns . leaf?).")

(define filmstrip/cascade
  (lambda
    ()
    (let*
      ((d (parse-int (request-param "d")))
        (t (as-str (request-param "t")))
        (row (filmstrip/row))
        (cascade (nth row 0))
        (panes (nth row 1))
        (focus (nth row 2))
        (path2 (append (filmstrip/take cascade d) (list t))))
      (if
        (filmstrip/route? t)
        (let*
          ((res (filmstrip/open-apply panes t))
            (panes2 (nth res 0))
            (focus2 (nth res 1))
            (appended? (nth res 2))
            (unshelved? (nth res 3)))
          (seq
            (filmstrip/save-all path2 panes2 focus2)
            (render-html
              (html/join
                (list
                  (filmstrip/oob-msg "")
                  (if unshelved? (filmstrip/shelf-oob) "")
                  (filmstrip/winbox-oob)
                  (filmstrip/oob-cascade path2)
                  (filmstrip/open-response panes2 focus focus2 appended?))))))
        (seq
          (filmstrip/save-all path2 panes focus)
          (render-html
            (html/join
              (list
                (filmstrip/oob-msg "")
                (filmstrip/oob-cascade path2))))))))
  "POST /filmstrip/cascade: d (depth) + t (target) params -> truncate the\ncrumb path to d, append t (V2 entry semantics). A crumb target re-walks\nthe columns; a route target (leaf) also open-or-focuses its pane.")

(define filmstrip/cascade-html
  (lambda
    (path)
    (let
      ((br (filmstrip/browse path)))
      (html/div
        (html/attrs
          (list
            (html/attr "id" "fs-cascade")
            (html/attr "class" "fs-cascade")))
        (html/join
          (filmstrip/map-indexed
            (lambda (col i) (filmstrip/col-html i col path))
            (car br)
            0)))))
  "The cascade strip: one column per path level over the app chooser.")

(define filmstrip/close
  (lambda
    ()
    (let*
      ((id (as-str (request-param "id")))
        (row (filmstrip/row))
        (cascade (nth row 0))
        (panes (nth row 1))
        (focus (nth row 2))
        (i (filmstrip/find-pane-index panes id)))
      (if
        (< i 0)
        (render-html
          (filmstrip/oob-msg "filmstrip: no such pane"))
        (let*
          ((p2 (dict-set (nth panes i) "state" "shelved"))
            (panes2 (filmstrip/replace-at panes i p2))
            (focus2
              (if (= i focus)
                  (filmstrip/first-live-index panes2)
                  focus)))
          (seq
            (filmstrip/save-all cascade panes2 focus2)
            (render-html
              (html/join
                (list
                  (filmstrip/oob-msg "")
                  (filmstrip/shelf-oob)
                  (filmstrip/winbox-oob)
                  (filmstrip/oob-section p2 (= i focus2))
                  (if
                    (if (>= focus2 0) (not (= focus2 focus)) #f)
                    (filmstrip/oob-section
                      (filmstrip/pane-at panes2 focus2)
                      #t)
                    "")))))))))
  "POST /filmstrip/close: id param -> v2 SHELF = the default close:\nstate=shelved, the pane record SURVIVES (its hx-preserve body keeps\nlive DOM), OOB class swap adds .shelved; focus heals to the first\nlive pane when the shelved pane held it. Row removal is destroy.")

(define filmstrip/destroy
  (lambda
    ()
    (let*
      ((id (as-str (request-param "id")))
        (row (filmstrip/row))
        (cascade (nth row 0))
        (panes (nth row 1))
        (focus (nth row 2))
        (i (filmstrip/find-pane-index panes id)))
      (if
        (< i 0)
        (render-html
          (filmstrip/oob-msg "filmstrip: no such pane"))
        (let*
          ((panes2 (filmstrip/remove-at panes i))
            (focus2 (filmstrip/close-focus i focus (length panes2))))
          (seq
            (filmstrip/save-all cascade panes2 focus2)
            (render-html
              (html/join
                (list
                  (filmstrip/oob-msg "")
                  (filmstrip/shelf-oob)
                  (filmstrip/winbox-oob)
                  (filmstrip/oob-delete-pane id)
                  (if
                    (if (>= focus2 0) (not (= focus2 focus)) #f)
                    (filmstrip/oob-section
                      (filmstrip/pane-at panes2 focus2)
                      #t)
                    "")))))))))
  "POST /filmstrip/destroy: id param -> REMOVE the pane record (the\npre-v2 close): OOB delete of its section + focus hand-off per\nclose-focus. The shelf row for the pane goes with it.")

(define filmstrip/restore
  (lambda
    ()
    (let*
      ((id (as-str (request-param "id")))
        (row (filmstrip/row))
        (cascade (nth row 0))
        (panes (nth row 1))
        (focus (nth row 2))
        (i (filmstrip/find-pane-index panes id)))
      (if
        (< i 0)
        (render-html
          (filmstrip/oob-msg "filmstrip: no such pane"))
        (let*
          ((p2 (dict-set (nth panes i) "state" "live"))
            (panes2 (filmstrip/replace-at panes i p2))
            (focus2
              (if
                (or
                  (< focus 0)
                  (string-eq
                    (filmstrip/pane-state (filmstrip/pane-at panes focus))
                    "shelved"))
                i
                focus)))
          (seq
            (filmstrip/save-all cascade panes2 focus2)
            (render-html
              (html/join
                (list
                  (filmstrip/oob-msg "")
                  (filmstrip/shelf-oob)
                  (filmstrip/winbox-oob)
                  (filmstrip/oob-section p2 (= i focus2))
                  (if
                    (if (>= focus 0) (not (= focus2 focus)) #f)
                    (filmstrip/oob-section
                      (filmstrip/pane-at panes2 focus)
                      #f)
                    "")))))))))
  "POST /filmstrip/restore: id param -> state=live, the pane returns to\nits position; OOB class swap brings the section back; focus heals onto\nthe restored pane when nothing live holds focus (a shelved pane's\n.focused class is cleared by the same swap).")

(define filmstrip/shelf-oob
  (lambda () "")
  "OOB payload re-rendering the shelf controlbox after\nclose/restore/destroy. Default empty -- the plain habitat shell has\nno shelf rail yet; glochid redefines it to swap its #fs-shelf rows.")

(define filmstrip/winbox-oob
  (lambda () "")
  "OOB payload re-rendering the windows controlbox after\nclose/restore/destroy (a pane leaving or rejoining the live set\nchanges its rows). Default empty -- the plain habitat shell has no\nwindows rail yet; glochid redefines it to swap its #fs-winbox rows.")

(define filmstrip/close-focus
  (lambda
    (i focus m)
    (cond
      ((< focus 0) -1)
      ((= focus i)
        (if (<= m 0) -1 (min (max (- i 1) 0) (- m 1))))
      ((> focus i) (- focus 1))
      (#t focus)))
  "Focus index after removing pane i (m = pane count after removal): the\nclosed pane's focus moves to its left neighbor (or the new head), later\npanes shift left with it.")

(define filmstrip/col-html
  (lambda
    (i col path)
    (html/div
      (html/attr "class" "fs-col")
      (html/join
        (list/map
          (lambda (e) (filmstrip/entry-html i e path))
          col))))
  "One Miller column of entries.")

(define filmstrip/col-leaf?
  (lambda
    (col)
    (if
      (null? col)
      #f
      (if
        (filmstrip/route? (car (cdr (car col))))
        #t
        (filmstrip/col-leaf? (cdr col)))))
  "A column is a leaf column when any entry targets a route.")

(define filmstrip/css
  (lambda
    ()
    (css/sheet
      (list
        (css/rule
          ".fs-shell"
          (list
            "display"
            "flex"
            "flex-direction"
            "column"
            "gap"
            ".4rem"
            "height"
            "calc(100vh - 6rem)"
            "min-height"
            "28rem"))
        (css/rule
          ".fs-cascade"
          (list
            "display"
            "flex"
            "flex-direction"
            "row"
            "gap"
            ".35rem"
            "flex"
            "0 0 28vh"
            "min-height"
            "8rem"
            "overflow-x"
            "auto"
            "overflow-y"
            "hidden"))
        (css/rule
          ".fs-col"
          (list
            "flex"
            "0 0 auto"
            "min-width"
            "10rem"
            "max-width"
            "16rem"
            "display"
            "flex"
            "flex-direction"
            "column"
            "border"
            "1px solid var(--border)"
            "background"
            "var(--panel)"
            "overflow-y"
            "auto"))
        (css/rule
          ".fs-entry"
          (list
            "text-align"
            "left"
            "padding"
            ".22rem .5rem"
            "background"
            "none"
            "border"
            "none"
            "border-bottom"
            "1px solid var(--border)"
            "color"
            "var(--ink)"
            "cursor"
            "pointer"
            "font"
            "inherit"
            "white-space"
            "nowrap"
            "overflow"
            "hidden"
            "text-overflow"
            "ellipsis"
            "max-width"
            "16rem"))
        (css/rule
          ".fs-entry:hover"
          (list "background" "var(--sel-bg)"))
        (css/rule
          ".fs-entry.fs-sel"
          (list
            "background"
            "var(--sel-accent)"
            "color"
            "#fff"
            "font-weight"
            "700"))
        (css/rule
          ".fs-content"
          (list
            "display"
            "flex"
            "flex-direction"
            "row"
            "gap"
            ".5rem"
            "flex"
            "1 1 auto"
            "overflow-x"
            "auto"
            "overflow-y"
            "hidden"
            "align-items"
            "stretch"))
        (css/rule
          ".fs-pane"
          (list
            "display"
            "flex"
            "flex-direction"
            "column"
            "flex"
            "0 0 33.333%"
            "min-width"
            "15rem"
            "border"
            "1px solid var(--border)"
            "background"
            "var(--panel)"
            "overflow"
            "hidden"))
        (css/rule
          ".fs-pane.fs-third"
          (list "flex-basis" "33.333%"))
        (css/rule ".fs-pane.fs-half" (list "flex-basis" "50%"))
        (css/rule ".fs-pane.fs-full" (list "flex-basis" "100%"))
        (css/rule
          ".fs-pane.focused"
          (list
            "outline"
            "2px solid var(--sel-accent)"
            "outline-offset"
            "-2px"))
        (css/rule
          ".fs-pane.shelved"
          (list "display" "none"))
        (css/rule
          ".fs-head"
          (list
            "display"
            "flex"
            "gap"
            ".3rem"
            "align-items"
            "center"
            "padding"
            ".2rem .35rem"
            "border-bottom"
            "1px solid var(--border)"
            "background"
            "var(--chrome)"
            "flex"
            "0 0 auto"))
        (css/rule
          ".fs-title"
          (list
            "flex"
            "1 1 auto"
            "overflow"
            "hidden"
            "text-overflow"
            "ellipsis"
            "white-space"
            "nowrap"
            "font-weight"
            "700"
            "font-size"
            ".85rem"))
        (css/rule
          ".fs-btn"
          (list
            "width"
            "auto"
            "min-width"
            "0"
            "padding"
            ".08rem .4rem"
            "font-size"
            ".8rem"
            "flex"
            "0 0 auto"))
        (css/rule
          ".fs-body"
          (list
            "flex"
            "1 1 auto"
            "overflow"
            "auto"
            "padding"
            ".4rem"
            "min-height"
            "0"))
        (css/rule
          ".fs-launch"
          (list
            "display"
            "flex"
            "gap"
            ".35rem"
            "align-items"
            "center"
            "flex"
            "0 0 auto"
            "margin"
            "0"))
        (css/rule
          ".fs-launch input[type=text]"
          (list "width" "16rem" "padding" ".2rem .35rem"))
        (css/rule
          ".fs-msg"
          (list "color" "var(--muted)" "font-size" ".85rem"))
        (css/rule ".fs-sentinel" (list "display" "none"))
        (css/rule ".fs-sink" (list "display" "none"))
        (css/rule ".fs-oob-delete" (list "display" "none")))))
  "Strip + pane + launcher stylesheet (~50 lines of rules). Widths are\nthe three presets only; .focused is the server-owned ring.")

(define filmstrip/default-width
  "third")

(define filmstrip/entry-html
  (lambda
    (i e path)
    (let
      ((label (car e))
        (target (car (cdr e)))
        (sel
          (and
            (< i (length path))
            (string-eq (as-str (nth path i)) (car (cdr e))))))
      (html/tag
        "button"
        (html/attrs
          (list
            (html/attr
              "class"
              (if sel "fs-entry fs-sel" "fs-entry"))
            (html/attr "hx-post" "/filmstrip/cascade")
            (html/attr
              "hx-vals"
              (string-append
                "{\"d\":\""
                (as-str i)
                "\",\"t\":\""
                target
                "\"}"))
            (html/attr "hx-target" "#fs-sink")
            (html/attr "hx-swap" "innerHTML")))
        (html/text label))))
  "One cascade entry: clicking POSTs (depth i, target). The selected entry\nof each column (matching the crumb path) wears fs-sel.")

(define filmstrip/find-index
  (lambda
    (pred xs i)
    (if
      (null? xs)
      -1
      (if
        (pred (car xs))
        i
        (filmstrip/find-index pred (cdr xs) (+ i 1)))))
  "Index of the first element satisfying pred, else -1.")

(define filmstrip/find-pane-index
  (lambda
    (panes id)
    (filmstrip/find-index
      (lambda (p) (string-eq (filmstrip/pane-id p) id))
      panes
      0))
  "Index of the pane with id, else -1.")

(define filmstrip/find-route-index
  (lambda
    (panes r)
    (filmstrip/find-index
      (lambda (p) (string-eq (filmstrip/pane-route p) r))
      panes
      0))
  "Index of the pane already showing route r, else -1 (open-or-focus).")

(define filmstrip/focus
  (lambda
    ()
    (let*
      ((dir (as-str (request-param "dir")))
        (row (filmstrip/row))
        (cascade (nth row 0))
        (panes (nth row 1))
        (focus (nth row 2))
        (focus2 (filmstrip/focus-step (length panes) focus dir)))
      (if
        (= focus2 focus)
        (render-html (filmstrip/oob-msg ""))
        (seq
          (filmstrip/save-all cascade panes focus2)
          (render-html
            (html/join
              (list
                (filmstrip/oob-msg "")
                (filmstrip/oob-section
                  (filmstrip/pane-at panes focus)
                  #f)
                (filmstrip/oob-section
                  (filmstrip/pane-at panes focus2)
                  #t))))))))
  "POST /filmstrip/focus: dir param -> move .focused one pane over\n(clamped, no wrap); OOB class swap on exactly two panes (old, new).")

(define filmstrip/focus-id
  (lambda
    ()
    (let*
      ((id (as-str (request-param "id")))
        (row (filmstrip/row))
        (cascade (nth row 0))
        (panes (nth row 1))
        (focus (nth row 2))
        (i (filmstrip/find-pane-index panes id)))
      (if
        (if (< i 0) #t (not (filmstrip/pane-live? (filmstrip/pane-at panes i))))
        (render-html (filmstrip/oob-msg "filmstrip: no such live pane"))
        (if (= i focus)
            (render-html (filmstrip/oob-msg ""))
            (if (< focus 0)
                (seq
                  (filmstrip/save-all cascade panes i)
                  (render-html
                    (html/join
                      (list
                        (filmstrip/oob-msg "")
                        (filmstrip/oob-section (filmstrip/pane-at panes i) #t)))))
                (seq
                  (filmstrip/save-all cascade panes i)
                  (render-html
                    (html/join
                      (list
                        (filmstrip/oob-msg "")
                        (filmstrip/oob-section
                          (filmstrip/pane-at panes focus)
                          #f)
                        (filmstrip/oob-section
                          (filmstrip/pane-at panes i)
                          #t))))))))))
  "POST /filmstrip/focus-id: id param -> point .focused at that pane's\nindex (persisted + same OOB two-section class swap as focus).\nShelved/unknown ids answer the status line only; an already-focused\nid answers empty.")

(define filmstrip/focus-step
  (lambda
    (n i dir)
    (if
      (or (< i 0) (>= i n))
      (if (string-eq dir "left") -1 (if (> n 0) 0 -1))
      (let
        ((j (if (string-eq dir "left") (- i 1) (+ i 1))))
        (if (< j 0) 0 (if (>= j n) (- n 1) j)))))
  "Next focus index for dir (left/right): clamped to the pane count, no\nwrap (niri discipline). An unset/out-of-range focus heals to pane 0 on\nright, none on left.")

(define filmstrip/key-sentinels
  (lambda
    ()
    (html/join
      (list
        (filmstrip/sentinel
          "/filmstrip/focus"
          "{\"dir\":\"left\"}"
          "keydown[altKey&&!shiftKey&&key=='ArrowLeft'] from:body")
        (filmstrip/sentinel
          "/filmstrip/focus"
          "{\"dir\":\"right\"}"
          "keydown[altKey&&!shiftKey&&key=='ArrowRight'] from:body")
        (filmstrip/sentinel
          "/filmstrip/move"
          "{\"dir\":\"left\"}"
          "keydown[altKey&&shiftKey&&key=='ArrowLeft'] from:body")
        (filmstrip/sentinel
          "/filmstrip/move"
          "{\"dir\":\"right\"}"
          "keydown[altKey&&shiftKey&&key=='ArrowRight'] from:body"))))
  "alt+arrows move focus; alt+shift+arrows move pane order. The server\nowns focus and answers with OOB class swaps (borge pane-protocol).")

(define filmstrip/last-col
  (lambda
    (cols)
    (if
      (null? cols)
      (list)
      (if
        (null? (cdr cols))
        (car cols)
        (filmstrip/last-col (cdr cols)))))
  "The deepest column of a column list.")

(define filmstrip/app-routes
  (lambda
    (apps)
    (if
      (null? apps)
      (list)
      (if
        (string-starts-with? (as-str (car (cdr (car apps)))) "/")
        (cons (as-str (car (cdr (car apps))))
          (filmstrip/app-routes (cdr apps)))
        (filmstrip/app-routes (cdr apps)))))
  "The route-leaf targets of the app list ((label target) entries whose\ntarget starts with /).")

(define filmstrip/hint-routes
  (lambda ()
    (let
      ((rs (filmstrip/app-routes (filmstrip/apps))))
      (if
        (null? rs)
        (list/map
          (lambda (a) (string-append "/" (as-str (car a))))
          (filmstrip/apps))
        rs)))
  "Route names the launcher may honestly suggest: the apps' route-leaf\ntargets when the host has them (glochid: /chat /code /sessions\n/settings), else /+label for package-key apps (habitat: /todos\n/notes /calendar). Env late binding picks up host redefinitions of\nfilmstrip/apps — no host seam needed.")

(define filmstrip/routes-str
  (lambda
    (rs)
    (if
      (null? rs)
      ""
      (string-append
        (car rs)
        (if (null? (cdr rs)) "" " ")
        (filmstrip/routes-str (cdr rs)))))
  "Space-joined routes string.")

(define filmstrip/launcher-hint
  (lambda ()
    (let
      ((rs (filmstrip/hint-routes)))
      (if (null? rs) "/" (car rs))))
  "The launcher's example route: the first real route of THIS host's\napp list (was the habitat-specific /todos — the miss that made glochid's\nlauncher a dead end).")

(define filmstrip/pane-stamp
  (lambda
    (rec)
    rec)
  "Host hook on freshly-stamped pane records (default identity). A host\nmay add its own fields — glochid stamps the pane's desktop from the ws\nrow so an app-link open on desk>0 lands VISIBLE there (the lib must not\nread glochid row fields itself; same seam discipline as save-all).")

(define filmstrip/launcher-html
  (lambda
    ()
    (html/tag
      "form"
      (html/attrs
        (list
          (html/attr "id" "fs-launch")
          (html/attr "class" "fs-launch")
          (html/attr "hx-post" "/filmstrip/open")
          (html/attr "hx-target" "#fs-sink")
          (html/attr "hx-swap" "innerHTML")))
      (html/join
        (list
          (html/label
            (html/attrs
              (list
                (html/attr "class" "fs-launch-label")
                (html/attr "for" "fs-launch-route")))
            "open pane")
          (html/text-input
            "route"
            (html/attrs
              (list
                (html/attr "id" "fs-launch-route")
                (html/attr "placeholder" (filmstrip/launcher-hint))
                (html/attr "autocomplete" "off")
                (html/attr "accesskey" "l")
                (html/attr
                  "title"
                  "route to open (accesskey: browser chord for l)"))))
          (html/button
            (html/attrs
              (list
                (html/attr "type" "submit")
                (html/attr "class" "fs-btn")))
            "open")
          (html/span
            (html/attrs
              (list
                (html/attr "id" "fs-launch-msg")
                (html/attr "class" "fs-msg")))
            "")))))
  "Inline launcher row at the head of the content strip: text input for a\nroute + open button. The input's accesskey is the zero-JS 'a key opens\nthe launcher' affordance (browser chord, e.g. Alt+Shift+L on Firefox).")

(define filmstrip/map-indexed
  (lambda
    (fn xs i)
    (if
      (null? xs)
      (list)
      (cons
        (fn (car xs) i)
        (filmstrip/map-indexed fn (cdr xs) (+ i 1)))))
  "Map fn over xs with a 0-based index.")

(define filmstrip/move
  (lambda
    ()
    (let*
      ((dir (as-str (request-param "dir")))
        (row (filmstrip/row))
        (cascade (nth row 0))
        (panes (nth row 1))
        (focus (nth row 2))
        (j (if (string-eq dir "left") (- focus 1) (+ focus 1))))
      (if
        (if (< focus 0) #t (or (< j 0) (>= j (length panes))))
        (render-html (filmstrip/oob-msg ""))
        (let*
          ((panes2
            (if
              (string-eq dir "left")
              (filmstrip/swap-at panes j)
              (filmstrip/swap-at panes focus))))
          (seq
            (filmstrip/save-all cascade panes2 j)
            (render-html
              (html/join
                (list
                  (filmstrip/oob-msg "")
                  (filmstrip/oob-content-strip panes2 j)))))))))
  "POST /filmstrip/move: dir param -> adjacent-transpose the focused pane\nwith its neighbor; focus follows the pane. OOB re-render of the whole\ncontent strip (bodies preserved).")

(define filmstrip/native-close
  (lambda
    (view-key)
    (let*
      ((row (filmstrip/row))
        (cascade (nth row 0))
        (panes (nth row 1))
        (focus (nth row 2))
        (i (filmstrip/native-find-view panes view-key)))
      (if
        (< i 0)
        (filmstrip/native-state)
        (let*
          ((panes2 (filmstrip/remove-at panes i))
            (focus2 (filmstrip/close-focus i focus (length panes2))))
          (seq
            (filmstrip/save-all cascade panes2 focus2)
            (filmstrip/native-state))))))
  "A foreign river window closed: remove its pane, focus hand-off per\nclose-focus. Returns native-state.")

(define filmstrip/native-find-view
  (lambda
    (panes view-key)
    (filmstrip/find-index
      (lambda
        (p)
        (string-eq (filmstrip/pane-view p) view-key))
      panes
      0))
  "Index of the pane bound to a river window key, else -1.")

(define filmstrip/native-focus
  (lambda
    (dir)
    (let*
      ((row (filmstrip/row))
        (cascade (nth row 0))
        (panes (nth row 1))
        (focus (nth row 2))
        (focus2 (filmstrip/focus-step (length panes) focus dir)))
      (seq
        (filmstrip/save-all cascade panes focus2)
        (filmstrip/native-state))))
  "alt+arrows policy: the focus handler's row mutation, clamped no\nwrap. Returns native-state.")

(define filmstrip/native-move
  (lambda
    (dir)
    (let*
      ((row (filmstrip/row))
        (cascade (nth row 0))
        (panes (nth row 1))
        (focus (nth row 2))
        (j (if (string-eq dir "left") (- focus 1) (+ focus 1))))
      (if
        (if (< focus 0) #t (or (< j 0) (>= j (length panes))))
        (filmstrip/native-state)
        (let
          ((panes2
            (if
              (string-eq dir "left")
              (filmstrip/swap-at panes j)
              (filmstrip/swap-at panes focus))))
          (seq
            (filmstrip/save-all cascade panes2 j)
            (filmstrip/native-state))))))
  "alt+shift+arrows policy: adjacent transpose of the focused pane;\nfocus follows. Returns native-state.")

(define filmstrip/native-open
  (lambda
    (app-id view-key)
    (let*
      ((row (filmstrip/row))
        (cascade (nth row 0))
        (panes (nth row 1))
        (i (filmstrip/native-find-view panes view-key)))
      (if
        (>= i 0)
        (seq
          (filmstrip/save-all cascade panes i)
          (filmstrip/native-state))
        (let
          ((panes2
            (append
              panes
              (list
                (dict-set*
                  "{}"
                  (list
                    "id"
                    (filmstrip/new-id)
                    "route"
                    (string-append "app:" app-id)
                    "width"
                    filmstrip/default-width
                    "view"
                    view-key
                    "state"
                    "live"))))))
          (seq
            (filmstrip/save-all cascade panes2 (length panes))
            (filmstrip/native-state))))))
  "A foreign river window mapped: open-or-focus the pane bound to\nview-key (route app:<app-id>, default width preset). Returns\nnative-state.")

(define filmstrip/native-pane-json
  (lambda
    (p focused?)
    (dict-set*
      "{}"
      (list
        "id"
        (filmstrip/pane-id p)
        "route"
        (filmstrip/pane-route p)
        "width"
        (filmstrip/pane-width p)
        "region"
        (filmstrip/pane-region p)
        "view"
        (filmstrip/pane-view p)
        "focused"
        (if focused? "1" "0"))))
  "One pane as the policy-state JSON object.")

(define filmstrip/native-state
  (lambda
    ()
    (let*
      ((row (filmstrip/row))
        (cascade (nth row 0))
        (panes (nth row 1))
        (focus (nth row 2)))
      (json-emit
        (dict-set*
          "{}"
          (list
            "panes"
            (json-emit
              (filmstrip/map-indexed
                (lambda
                  (p i)
                  (filmstrip/native-pane-json p (= i focus)))
                panes
                0))
            "cascade"
            (json-emit cascade)
            "focus"
            (as-str focus))))))
  "Policy state for soil-strip: panes (with view keys + focused flags),\ncascade, focus index — the layout source of the next manage sequence.")

(define filmstrip/new-id
  (lambda () (random-uuid))
  "Fresh pane id (UUID -- the todo/notes row id discipline).")

(define filmstrip/oob-append-pane
  (lambda
    (p focused?)
    (html/tag
      "div"
      (html/attr "hx-swap-oob" "beforeend:#fs-content")
      (filmstrip/pane-html
        p
        focused?
        (filmstrip/pane-content (filmstrip/pane-route p)))))
  "OOB beforeend into the content strip: a freshly opened pane with its\nreal dispatched content (no preserve -- nothing to preserve yet).")

(define filmstrip/oob-cascade
  (lambda
    (path)
    (let
      ((br (filmstrip/browse path)))
      (html/tag
        "div"
        (html/attrs
          (list
            (html/attr "id" "fs-cascade")
            (html/attr "class" "fs-cascade")
            (html/attr "hx-swap-oob" "true")))
        (html/join
          (filmstrip/map-indexed
            (lambda (col i) (filmstrip/col-html i col path))
            (car br)
            0)))))
  "OOB re-render of the cascade strip after a crumb click.")

(define filmstrip/oob-content-strip
  (lambda
    (panes focus)
    (html/tag
      "div"
      (html/attrs
        (list
          (html/attr "id" "fs-content")
          (html/attr "class" "fs-content")
          (html/attr "hx-swap-oob" "true")))
      (html/join (filmstrip/panes-html panes focus))))
  "OOB re-render of the whole content strip (pane order changes; every\nbody is an hx-preserve placeholder, so live pane DOM survives).")

(define filmstrip/oob-delete-pane
  (lambda
    (id)
    (html/tag
      "div"
      (html/attrs
        (list
          (html/attr
            "hx-swap-oob"
            (string-append "delete:#fs-pane-" id))
          (html/attr "class" "fs-oob-delete")))
      ""))
  "OOB delete of a closed pane's section.")

(define filmstrip/oob-msg
  (lambda
    (s)
    (html/span
      (html/attrs
        (list
          (html/attr "id" "fs-launch-msg")
          (html/attr "class" "fs-msg")
          (html/attr "hx-swap-oob" "true")))
      (html/text s)))
  "OOB status line under the launcher.")

(define filmstrip/oob-section
  (lambda
    (p focused?)
    (let
      ((id (filmstrip/pane-id p)))
      (html/tag
        "section"
        (html/attrs
          (list
            (html/attr
              "class"
              (string-append
                "fs-pane"
                " fs-"
                (filmstrip/pane-width p)
                (if focused? " focused" "")
                (if (string-eq (filmstrip/pane-state p) "shelved")
                    " shelved"
                    "")))
            (html/attr "id" (string-append "fs-pane-" id))
            (html/attr "data-pane" id)
            (html/attr
              "data-desktop"
              (as-str (filmstrip/pane-desk p)))
            (html/attr
              "data-region"
              (filmstrip/pane-region p))
            (html/attr "hx-swap-oob" "true")))
        (html/join
          (list
            (filmstrip/pane-head p)
            (html/tag
              "div"
              (html/attrs
                (list
                  (html/attr "class" "fs-body")
                  (html/attr "id" (string-append "fs-body-" id))
                  (html/attr "hx-preserve" "true")))
              ""))))))
  "OOB outerHTML swap carrying only shell state: width + .focused classes.\nThe body div is an hx-preserve placeholder -- htmx re-inserts the live\npane DOM, so focus/width changes never clobber in-pane state.")

(define filmstrip/app-link
  (lambda
    (r label)
    (html/button
      (html/attrs
        (list
          (html/attr "type" "button")
          (html/attr "class" "fs-applink")
          (html/attr "hx-post" "/filmstrip/open")
          (html/attr "hx-vals" (string-append "{\"route\":\"" r "\"}"))
          (html/attr "hx-target" "#fs-sink")
          (html/attr "hx-swap" "innerHTML")))
      (html/text label)))
  "In-shell app link: a link-looking button POSTing /filmstrip/open\n(hx-vals carries the route; open-or-focus semantics) with the OOB pane\nswaps landed in #fs-sink. Pane chrome uses these INSTEAD of <a href> --\nan anchor navigates the WHOLE WINDOW out of the shell; an app-link\nnever leaves it. Returns a PLAIN string (no render-html: the sentinel\nwould leak into any string-append embed like chat/body's h1 nav).")

(define filmstrip/open
  (lambda
    ()
    (let*
      ((r (string-trim (as-str (request-param "route"))))
        (row (filmstrip/row))
        (cascade (nth row 0))
        (panes (nth row 1))
        (focus (nth row 2)))
      (if
        (or
          (< (string-length r) 1)
          (not (string-starts-with? r "/")))
        (render-html
          (filmstrip/oob-msg
            (string-append "filmstrip: routes start with / (try "
              (filmstrip/launcher-hint) ")")))
        (let
          ((path (car (filmstrip/split-query r))))
          (if
            (null? (route/lookup path))
            (render-html
              (filmstrip/oob-msg
                (string-append "filmstrip: no route " path
                  "  (routes on this host: "
                  (filmstrip/routes-str (filmstrip/hint-routes))
                  ")")))
            (let*
              ((res (filmstrip/open-apply panes r))
                (panes2 (nth res 0))
                (focus2 (nth res 1))
                (appended? (nth res 2))
                (unshelved? (nth res 3)))
              (seq
                (filmstrip/save-all cascade panes2 focus2)
                (render-html
                  (html/join
                    (list
                      (filmstrip/oob-msg "")
                      (if unshelved? (filmstrip/shelf-oob) "")
                      (filmstrip/winbox-oob)
                      (filmstrip/open-response panes2 focus focus2 appended?)))))))))))
  "POST /filmstrip/open: launcher route param -> open-or-focus a pane.\nBad/missing routes answer with the launcher status line only; valid ones\npersist the row and answer with OOB pane swaps + winbox/shelf\nrefreshes (a win that unshelves drops its shelf row in the same\nresponse).")

(define filmstrip/open-apply
  (lambda
    (panes r)
    (let
      ((i (filmstrip/find-route-index panes r)))
      (if
        (>= i 0)
        (let
          ((p (filmstrip/pane-at panes i)))
          (if
            (string-eq (filmstrip/pane-state p) "shelved")
            (list
              (filmstrip/replace-at panes i (dict-set p "state" "live"))
              i
              #f
              #t)
            (list panes i #f #f)))
          (list
            (append
              panes
              (list
                (filmstrip/pane-stamp
                  (dict-set*
                    "{}"
                    (list
                      "id"
                      (filmstrip/new-id)
                      "route"
                      r
                      "width"
                      filmstrip/default-width
                      "state"
                      "live")))))
          (length panes)
          #t))))
  "Open-or-focus for route r: a fresh pane record (UUID id, default\nwidth preset, state=live, passed through filmstrip/pane-stamp) appended,\nor the existing pane's index. A HIT ON A SHELVED PANE UNSHELVES it\n(acme Get semantics: naming a hidden window re-shows it). Returns\n(panes focus appended? unshelved?) -- appended? false means an existing\npane won; unshelved? true means the caller owes a shelf OOB swap.")

(define filmstrip/open-response
  (lambda
    (panes2 old-focus focus2 appended?)
    (html/join
      (list
        (if
          (if (>= old-focus 0) (not (= old-focus focus2)) #f)
          (filmstrip/oob-section
            (filmstrip/pane-at panes2 old-focus)
            #f)
          "")
        (if
          appended?
          (filmstrip/oob-append-pane
            (filmstrip/pane-at panes2 focus2)
            #t)
          (filmstrip/oob-section
            (filmstrip/pane-at panes2 focus2)
            #t)))))
  "OOB response body for an open/leaf click: OOB class swap on the\npreviously focused pane, then the opened pane -- appended with its real\ndispatched content, or re-swapped focused (open-or-focus).")

(define filmstrip/page
  (lambda
    ()
    (let*
      ((row (filmstrip/row))
        (cascade (nth row 0))
        (panes (nth row 1))
        (focus (nth row 2)))
      (render-html (filmstrip/shell-html cascade panes focus))))
  "GET /filmstrip: the habitat shell. Reads the workspace row and renders\nthe cascade strip (crumb path) over the content strip (panes with real\ndispatched content). Empty state is valid: no panes, launcher visible.")

(define filmstrip/pane-at
  (lambda
    (panes i)
    (if
      (or (< i 0) (>= i (length panes)))
      (list)
      (nth panes i)))
  "Pane record at index, or () out of range.")

(define filmstrip/pane-content
  (lambda
    (route)
    (let*
      ((parsed (filmstrip/split-query route))
        (path (car parsed))
        (handler (route/lookup path)))
      (if
        (null? handler)
        (html/p*
          (html/attr "class" "muted")
          (string-append "filmstrip: no route for " path))
        (route/dispatch
          handler
          "GET"
          path
          (cdr parsed)
          (list (cons "hx-request" "true"))))))
  "In-process partial dispatch of a pane route (HX-Request header on --\nthe route gate's existing fragment path, the hyper/visit seam).")

(define filmstrip/pane-head
  (lambda
    (p)
    (let
      ((id (filmstrip/pane-id p))
        (route (filmstrip/pane-route p)))
      (html/tag
        "header"
        (html/attr "class" "fs-head")
        (html/join
          (list
            (html/span
              (html/attr "class" "fs-title")
              (html/text route))
            (html/tag
              "button"
              (html/attrs
                (list
                  (html/attr "class" "fs-btn")
                  (html/attr
                    "title"
                    (if
                      (string-eq (filmstrip/pane-region p) "rail")
                      "return the pane to the main strip"
                      "pin the pane to the rail (dock; body stays live here)"))
                  (html/attr "hx-post" "/filmstrip/pin")
                  (html/attr
                    "hx-vals"
                    (string-append "{\"id\":\"" id "\"}"))
                  (html/attr "hx-target" "#fs-sink")
                  (html/attr "hx-swap" "innerHTML")))
              (if
                (string-eq (filmstrip/pane-region p) "rail")
                "unpin"
                "pin"))
            (html/tag
              "button"
              (html/attrs
                (list
                  (html/attr "class" "fs-btn")
                  (html/attr "title" "cycle width (third/half/full)")
                  (html/attr "hx-post" "/filmstrip/width")
                  (html/attr
                    "hx-vals"
                    (string-append "{\"id\":\"" id "\"}"))
                  (html/attr "hx-target" "#fs-sink")
                  (html/attr "hx-swap" "innerHTML")))
              "width")
            (html/tag
              "button"
              (html/attrs
                (list
                  (html/attr "class" "fs-btn")
                  (html/attr "title" "close pane")
                  (html/attr "hx-post" "/filmstrip/close")
                  (html/attr
                    "hx-vals"
                    (string-append "{\"id\":\"" id "\"}"))
                  (html/attr "hx-target" "#fs-sink")
                  (html/attr "hx-swap" "innerHTML")))
              "close"))))))
  "Pane title bar: route label + pin (region flip, v3) + width cycle +
close, all htmx POSTs. The pin word labels by the pane's CURRENT
region (pin/unpin).")

(define filmstrip/pane-html
  (lambda
    (p focused? content)
    (let
      ((id (filmstrip/pane-id p)))
      (html/tag
        "section"
        (html/attrs
          (list
            (html/attr
              "class"
              (string-append
                "fs-pane"
                " fs-"
                (filmstrip/pane-width p)
                (if focused? " focused" "")
                (if (string-eq (filmstrip/pane-state p) "shelved")
                    " shelved"
                    "")))
            (html/attr "id" (string-append "fs-pane-" id))
            (html/attr "data-pane" id)
            (html/attr
              "data-desktop"
              (as-str (filmstrip/pane-desk p)))
            (html/attr
              "data-region"
              (filmstrip/pane-region p))))
        (html/join
          (list
            (filmstrip/pane-head p)
            (html/tag
              "div"
              (html/attrs
                (list
                  (html/attr "class" "fs-body")
                  (html/attr "id" (string-append "fs-body-" id))
                  (html/attr "hx-preserve" "true")))
              content))))))
  "One pane section. content is the dispatched fragment (full page) or an\nempty hx-preserve placeholder (OOB class/width swaps keep the live DOM).\ndata-desktop feeds the shell's :has(#ws-desk.dsk-N) hiding.")

(define filmstrip/pane-id
  (lambda (p) (as-str (dict-get p "id")))
  "Pane record id.")

(define filmstrip/pane-route
  (lambda (p) (as-str (dict-get p "route")))
  "Pane record route.")

(define filmstrip/pane-set-width
  (lambda (p w) (dict-set p "width" w))
  "Pane record with a new width preset.")

(define filmstrip/pane-set-region
  (lambda (p r) (dict-set p "region" r))
  "Pane record with a new region (main | rail) — the field persists on
the record like width/state, so it rides every writer below save-all.")

(define filmstrip/pane-view
  (lambda (p) (as-str (dict-get p "view")))
  "Pane record's river window key (empty for habitat panes).")

(define filmstrip/pane-state
  (lambda (p) (as-str (dict-get p "state")))
  "Pane record state: live | shelved (v2 shelf). Empty = live — records\nfrom before the state field exist and stay live.")

(define filmstrip/pane-region
  (lambda (p)
    (let
      ((r (as-str (dict-get p "region"))))
      (if (string-eq r "") "main" r)))
  "Pane record region (v3 pin): main | rail. Empty = main — records\nfrom before the region field exist and stay main (main keeps the v2\nmodel: strip + one lifted focused pane). The ACCESSOR defaults, so\nselectors + JSON see \"main\" on legacy records, never an empty value.")

(define filmstrip/pane-live?
  (lambda (p)
    (not (string-eq (filmstrip/pane-state p) "shelved")))
  "True when the pane is not shelved (missing state counts live).")

(define filmstrip/pane-desk
  (lambda (p)
    (let
      ((v (dict-get p "desktop")))
      (if (null? v) 0 (parse-int (as-str v)))))
  "Pane record desktop index (0 when unset — pre-v2 records ride
desktop 0).")

(define filmstrip/first-live-index
  (lambda (panes)
    (filmstrip/find-index filmstrip/pane-live? panes 0))
  "Index of the first live (not shelved) pane, else -1.")

(define filmstrip/pane-width
  (lambda
    (p)
    (let
      ((w (as-str (dict-get p "width"))))
      (if (string-eq w "") filmstrip/default-width w)))
  "Pane record width preset (default third).")

(define filmstrip/panes-emit
  (lambda
    (panes)
    (json-emit (list/map (lambda (p) p) panes)))
  "Emit pane records back to the row's content JSON array.")

(define filmstrip/panes-html
  (lambda
    (panes focus)
    (filmstrip/map-indexed
      (lambda
        (p i)
        (filmstrip/pane-html
          p
          (= i focus)
          (filmstrip/pane-content (filmstrip/pane-route p))))
      panes
      0))
  "Every pane section with its in-process dispatched content; the focused\nindex wears .focused.")

(define filmstrip/parse-pairs
  (lambda
    (pairs)
    (if
      (null? pairs)
      (list)
      (let*
        ((kv (string-split (car pairs) "="))
          (k (car kv))
          (v (if (null? (cdr kv)) "" (car (cdr kv)))))
        (cons (cons k v) (filmstrip/parse-pairs (cdr pairs))))))
  "Parse kv strings into a dotted-pair alist.")

(define filmstrip/pkg-browse
  (lambda
    (pkg pkg-path)
    (if
      (string-eq pkg "todo")
      (todo/browse pkg-path)
      (if
        (string-eq pkg "notes")
        (notes/browse pkg-path)
        (if
          (string-eq pkg "calendar")
          (calendar/browse pkg-path)
          (cons (list (list)) #f)))))
  "Dispatch the package browse; unknown packages get one empty column.")

(define filmstrip/remove-at
  (lambda
    (xs i)
    (if
      (null? xs)
      (list)
      (if
        (<= i 0)
        (cdr xs)
        (cons (car xs) (filmstrip/remove-at (cdr xs) (- i 1))))))
  "Copy of xs without element at index i.")

(define filmstrip/replace-at
  (lambda
    (xs i x)
    (if
      (null? xs)
      (list)
      (if
        (<= i 0)
        (cons x (cdr xs))
        (cons
          (car xs)
          (filmstrip/replace-at (cdr xs) (- i 1) x)))))
  "Copy of xs with the element at index i replaced by x.")

(define filmstrip/route?
  (lambda (s) (string-starts-with? (as-str s) "/"))
  "True when a cascade target is a route (leaf) rather than a crumb.")

(define filmstrip/row
  (lambda
    ()
    (let
      ((f (filmstrip/ws-fields)))
      (list
        (filmstrip/ws-cascade f)
        (filmstrip/ws-panes f)
        (filmstrip/ws-focus f))))
  "The workspace triple (cascade panes focus) from this user's row.")

(define filmstrip/save-all
  (lambda
    (cascade panes focus)
    (filmstrip/ws-save
      (dict-set*
        "{}"
        (list
          "cascade"
          (json-emit (list/map (lambda (c) c) cascade))
          "content"
          (filmstrip/panes-emit panes)
          "focus"
          (string-append "content:" (as-str focus))))))
  "Write all three workspace fields in one row upsert.")

(define filmstrip/scene
  (lambda
    ()
    (let*
      ((row (filmstrip/row))
        (cascade (nth row 0))
        (panes (nth row 1))
        (focus (nth row 2))
        (br (filmstrip/browse cascade)))
      (pads/render-scene
        (pad
          (quote column)
          (list (cons (quote gap) "4") (cons (quote padding) "8"))
          (filmstrip/scene-cascade (car br) cascade)
          (pad
            (quote text)
            (list (cons (quote dim) "1"))
            "kiosk view -- edit at /filmstrip")
          (filmstrip/scene-strip panes focus)))))
  "SCENE v0 kiosk render of the workspace (soil M-A): cascade strip\n(Miller columns, selected entry wears sel) over a dim launcher hint over\nthe content strip (pane columns at niri weights, focused pane wears\nsel). Pure data: pads/render-scene output; eval-only (no route), the\nnative shim consumes it via loopback eval --eval \"(filmstrip/scene)\".")

(define filmstrip/scene-cascade
  (lambda
    (cols path)
    (pad
      (quote row)
      (list (cons (quote gap) "6"))
      (filmstrip/map-indexed
        (lambda (col i) (filmstrip/scene-col i col path))
        cols
        0)))
  "The cascade strip: one column per browse level over the app\nchooser (cols from filmstrip/browse).")

(define filmstrip/scene-col
  (lambda
    (i col path)
    (pad
      (quote column)
      (list (cons (quote gap) "2") (cons (quote padding) "6"))
      (append
        (if
          (= i 0)
          (list
            (pad
              (quote text)
              (list (cons (quote level) "1"))
              "filmstrip"))
          (list))
        (let
          ((capped (filmstrip/take col 12)))
          (append
            (list/map
              (lambda
                (e)
                (pad
                  (quote text)
                  (if
                    (filmstrip/scene-entry-sel? i e path)
                    (list (cons (quote sel) "1"))
                    (list))
                  (as-str (car e))))
              capped)
            (if
              (> (length col) 12)
              (list
                (pad
                  (quote text)
                  (list (cons (quote dim) "1"))
                  (string-append
                    "+ "
                    (as-str (- (length col) 12))
                    " more")))
              (list)))))))
  "One Miller column: entries as text (selected wears sel), capped at\n12 with a dim +N more tail; column 0 carries the shell header.")

(define filmstrip/scene-entry-sel?
  (lambda
    (i e path)
    (and
      (< i (length path))
      (string-eq (as-str (nth path i)) (as-str (car (cdr e))))))
  "Entry selected when the crumb path at depth i names its target.")

(define filmstrip/scene-lines
  (lambda
    (labels)
    (let
      ((capped (filmstrip/take labels 12)))
      (append
        (list/map
          (lambda (s) (pad (quote text) (list) s))
          capped)
        (if
          (> (length labels) 12)
          (list
            (pad
              (quote text)
              (list (cons (quote dim) "1"))
              (string-append
                "+ "
                (as-str (- (length labels) 12))
                " more")))
          (list)))))
  "Label strings -> text pad nodes, capped at 12 with a dim +N more\ntail (the shim clips rather than scrolls, so columns cap instead).")

(define filmstrip/scene-pane
  (lambda
    (p focused?)
    (let
      ((route (filmstrip/pane-route p))
        (attrs
          (list
            (cons
              (quote size)
              (list
                (quote grow)
                (filmstrip/scene-w (filmstrip/pane-width p))))
            (cons (quote gap) "2")
            (cons (quote padding) "6"))))
      (pad
        (quote column)
        attrs
        (pad
          (quote text)
          (if
            focused?
            (list (cons (quote level) "1") (cons (quote sel) "1"))
            (list (cons (quote level) "1")))
          route)
        (filmstrip/scene-pane-body route))))
  "One content pane as a scene column: niri weight from the width\npreset, route title (level 1) wearing sel when focused, summary body.")

(define filmstrip/scene-pane-body
  (lambda
    (route)
    (cond
      ((string-starts-with? route "/todos")
        (filmstrip/scene-lines
          (list/map
            (lambda
              (row)
              (string-append
                (as-str (dict-get (cdr row) "title"))
                " ("
                (as-str (filmstrip/scene-todo-count (as-str (car row))))
                ")"))
            (todo/list/all))))
      ((string-starts-with? route "/notes")
        (filmstrip/scene-lines
          (list/map
            (lambda (r) (as-str (nth r 2)))
            (notes/list-light))))
      ((string-starts-with? route "/calendar")
        (list
          (pad
            (quote text)
            (list)
            (string-append "today: " (calendar/today)))
          (pad
            (quote text)
            (list)
            (string-append
              (as-str (length (calendar/all-events)))
              " events"))))
      (#t
        (list
          (pad
            (quote text)
            (list (cons (quote dim) "1"))
            "open in browser /filmstrip")))))
  "Text summary lines for a pane route (D4: summaries from existing\nprims — the shell is the M-A subject, not live pane interiors).")

(define filmstrip/scene-strip
  (lambda
    (panes focus)
    (let
      ((pane-nodes
          (filmstrip/map-indexed
            (lambda (p i) (filmstrip/scene-pane p (= i focus)))
            panes
            0))
        (wsum (filmstrip/scene-weights-sum panes)))
      (pad
        (quote row)
        (list
          (cons (quote gap) "8")
          (cons (quote size) (list (quote grow) 1)))
        (if
          (null? panes)
          (list
            (pad
              (quote text)
              (list (cons (quote dim) "1"))
              "no panes -- open /todos from the browser"))
          (append
            pane-nodes
            (if
              (< wsum 6)
              (list
                (pad
                  (quote text)
                  (list
                    (cons (quote size) (list (quote grow) (- 6 wsum))))
                  ""))
              (list)))))))
  "The content strip: pane columns at niri weights + a trailing\nspacer absorbing unused fraction (third+half -> 2:3 + 1/6 empty);\nempty state is a dim hint.")

(define filmstrip/scene-todo-count
  (lambda
    (lid)
    (length
      (list/filter
        (lambda
          (row)
          (string-eq (as-str (dict-get (cdr row) "list_id")) lid))
        (todo/item/all))))
  "Item count of one todo list (the browse data, counted).")

(define filmstrip/scene-w
  (lambda
    (w)
    (cond
      ((string-eq w "half") 3)
      ((string-eq w "full") 6)
      (#t 2)))
  "niri width preset -> grow weight (denominator 6): third=2, half=3,\nfull=6. The strip splits grow space by weight, so 2/3/6 against a\ntrailing spacer reproduces exact niri fractions; full+others shrink\nproportionally (niri scrolls; the shim has no scroll in v1).")

(define filmstrip/scene-weights-sum
  (lambda
    (panes)
    (if
      (null? panes)
      0
      (+
        (filmstrip/scene-w (filmstrip/pane-width (car panes)))
        (filmstrip/scene-weights-sum (cdr panes)))))
  "Sum of pane grow weights (for the spacer's remainder).")

(define filmstrip/sentinel
  (lambda
    (url vals trigger)
    (html/div
      (html/attrs
        (list
          (html/attr "class" "fs-sentinel")
          (html/attr "hx-post" url)
          (html/attr "hx-vals" vals)
          (html/attr "hx-trigger" trigger)
          (html/attr "hx-target" "#fs-sink")
          (html/attr "hx-swap" "innerHTML")))
      ""))
  "Invisible trigger element: an htmx key filter on the body that POSTs a\nshell event (vals carries the dir param). Zero javascript files (D3) --\nthe filter expression is htmx.")

(define filmstrip/shell-html
  (lambda
    (cascade panes focus)
    (html/join
      (list
        (html/tag "style" "" (css/render (filmstrip/css)))
        (html/tag
          "div"
          (html/attrs
            (list
              (html/attr "class" "fs-shell")
              (html/attr "id" "fs-shell")))
          (html/join
            (list
              (filmstrip/cascade-html cascade)
              (filmstrip/launcher-html)
              (html/tag
                "div"
                (html/attrs
                  (list
                    (html/attr "id" "fs-content")
                    (html/attr "class" "fs-content")))
                (html/join (filmstrip/panes-html panes focus)))
              (filmstrip/key-sentinels)
              (html/tag
                "div"
                (html/attrs
                  (list
                    (html/attr "id" "fs-sink")
                    (html/attr "class" "fs-sink")))
                "")))))))
  "The whole shell: style + cascade strip over launcher + content strip,\nkey sentinels, and the invisible OOB sink. The HTTP gate wraps this\nfragment in chrome on full-page requests.")

(define filmstrip/split-query
  (lambda
    (url)
    (let*
      ((u (as-str url))
        (parts (string-split u "?"))
        (path (car parts))
        (rest (if (null? (cdr parts)) "" (car (cdr parts)))))
      (cons
        path
        (filmstrip/parse-pairs
          (if (string-eq rest "") (list) (string-split rest "&"))))))
  "Split a route string into (path . params) where params is an alist of\nDOTTED (k . v) pairs -- the shape route/dispatch's assoc_of accepts (2-element\nlists are silently dropped, which would lose the query).")

(define filmstrip/swap-at
  (lambda
    (xs i)
    (let
      ((n (length xs)))
      (if
        (or (< i 0) (>= (+ i 1) n))
        xs
        (filmstrip/swap-pair xs i))))
  "Swap element at index i with its right neighbor (no-op at the end).")

(define filmstrip/swap-pair
  (lambda
    (xs i)
    (if
      (<= i 0)
      (cons (car (cdr xs)) (cons (car xs) (cdr (cdr xs))))
      (cons (car xs) (filmstrip/swap-pair (cdr xs) (- i 1)))))
  "Adjacent transposition at index i (assumes a right neighbor exists).")

(define filmstrip/take
  (lambda
    (xs n)
    (if
      (or (null? xs) (<= n 0))
      (list)
      (cons (car xs) (filmstrip/take (cdr xs) (- n 1)))))
  "First n elements of a list.")

(define filmstrip/pin
  (lambda
    ()
    (let*
      ((id (as-str (request-param "id")))
        (row (filmstrip/row))
        (cascade (nth row 0))
        (panes (nth row 1))
        (focus (nth row 2))
        (i (filmstrip/find-pane-index panes id)))
      (if
        (< i 0)
        (render-html
          (filmstrip/oob-msg "filmstrip: no such pane"))
        (let*
          ((p2
              (filmstrip/pane-set-region
                (nth panes i)
                (if
                  (string-eq (filmstrip/pane-region (nth panes i)) "rail")
                  "main"
                  "rail")))
            (panes2 (filmstrip/replace-at panes i p2)))
          (seq
            (filmstrip/save-all cascade panes2 focus)
            (render-html
              (html/join
                (list
                  (filmstrip/oob-msg "")
                  (filmstrip/shelf-oob)
                  (filmstrip/winbox-oob)
                  (filmstrip/oob-section p2 (= i focus))))))))))
  "POST /filmstrip/pin: id param -> trade the pane's region (main/rail\nv3 pin). Rail = dock semantics: the body stays LIVE inside the rail\n(glochid CSS shows [data-region=rail] bodies) beside the focused main\npane. Same grammar as filmstrip/width: OOB section swap (body DOM\npreserved via hx-preserve), plus winbox/shelf OOB refreshes (the win\nrow's pin word re-labels by region).")

(define filmstrip/width
  (lambda
    ()
    (let*
      ((id (as-str (request-param "id")))
        (row (filmstrip/row))
        (cascade (nth row 0))
        (panes (nth row 1))
        (focus (nth row 2))
        (i (filmstrip/find-pane-index panes id)))
      (if
        (< i 0)
        (render-html
          (filmstrip/oob-msg "filmstrip: no such pane"))
        (let*
          ((p2
              (filmstrip/pane-set-width
                (nth panes i)
                (filmstrip/width-next
                  (filmstrip/pane-width (nth panes i)))))
            (panes2 (filmstrip/replace-at panes i p2)))
          (seq
            (filmstrip/save-all cascade panes2 focus)
            (render-html
              (html/join
                (list
                  (filmstrip/oob-msg "")
                  (filmstrip/oob-section p2 (= i focus))))))))))
  "POST /filmstrip/width: id param -> cycle the pane's width preset\n(third/half/full); OOB section swap, pane body preserved.")

(define filmstrip/width-cycle
  (lambda
    (ws w)
    (if
      (null? ws)
      filmstrip/default-width
      (if
        (string-eq (as-str (car ws)) (as-str w))
        (if
          (null? (cdr ws))
          (car filmstrip/widths)
          (car (cdr ws)))
        (filmstrip/width-cycle (cdr ws) w))))
  "Walk the preset list; a hit returns its successor, the tail wraps to\nthe head, and an unknown width falls back to the default.")

(define filmstrip/width-next
  (lambda (w) (filmstrip/width-cycle filmstrip/widths w))
  "Next width preset after w -- the third/half/full cycle, tail wraps.")

(define filmstrip/widths
  (list "third" "half" "full"))

(define filmstrip/ws-cascade
  (lambda
    (fields)
    (let
      ((v (dict-get fields "cascade")))
      (if
        (null? v)
        (list)
        (let
          ((p (json-parse (as-str v))))
          (if (null? p) (list) p)))))
  "Crumb path from the row: list of entry strings (labels/ids/routes).")

(define filmstrip/ws-default
  (lambda
    ()
    (dict-set*
      "{}"
      (list
        "cascade"
        (json-emit (list))
        "content"
        (json-emit (list))
        "focus"
        "content:-1")))
  "Empty workspace fields: no crumbs, no panes, no focus.")

(define filmstrip/ws-fields
  (lambda
    ()
    (let
      ((n (filmstrip/ws-node)))
      (if
        (null? n)
        (filmstrip/ws-default)
        (dict-get n "body"))))
  "Workspace row fields (defaults when the row doesn't exist yet).")

(define filmstrip/ws-focus
  (lambda
    (fields)
    (let*
      ((v (as-str (dict-get fields "focus")))
        (n (string-length v)))
      (if (< n 9) -1 (parse-int (substring v 8 n)))))
  "Focused content index from the row's focus value (content:N); -1 = none.")

(define filmstrip/ws-node
  (lambda
    ()
    (tree/get (node-path filmstrip/ws-type (whoami))))
  "This user's workspace node, or () when never saved.")

(define filmstrip/ws-panes
  (lambda
    (fields)
    (let
      ((v (dict-get fields "content")))
      (if
        (null? v)
        (list)
        (let
          ((ps (json-parse (as-str v))))
          (if (null? ps) (list) ps)))))
  "Pane records from the row: list of JSON object strings {id route width}.")

(define filmstrip/ws-save
  (lambda
    (fields)
    (seq
      (tree/upsert
        (node-path filmstrip/ws-type (whoami))
        "datum"
        filmstrip/ws-type
        fields)
      fields))
  "Persist workspace fields under the user's stable row id.")

(define filmstrip/ws-type
  "filmstrip-ws")
