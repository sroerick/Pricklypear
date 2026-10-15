;; package filmstrip -- exported from live branch system/main
;; Sync: scripts/pp-sync.sh status|pull|snapshot

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

(define calendar/browse
  (lambda
    (path)
    (filmstrip/browse-walk
      calendar/browse-children
      (list (calendar/browse-years))
      path))
  "calendar cascade: year -> month -> day. Day entries leaf to\n/calendar?day=YYYY-MM-DD (the day agenda route).")

(define calendar/browse-children
  (lambda
    (entry)
    (let
      ((e (as-str entry)))
      (if
        (= (string-length e) 4)
        (calendar/browse-months e)
        (if
          (= (string-length e) 7)
          (calendar/browse-days e)
          (list)))))
  "Children of a calendar crumb: year -> months, YYYY-MM -> days.")

(define calendar/browse-contains
  (lambda
    (xs s)
    (if
      (null? xs)
      #f
      (if
        (string-eq (as-str (car xs)) s)
        #t
        (calendar/browse-contains (cdr xs) s))))
  "Membership by string equality.")

(define calendar/browse-days
  (lambda
    (ym)
    (let
      ((y (substring ym 0 4))
        (m (parse-int (substring ym 5 2))))
      (calendar/browse-days-acc
        y
        m
        1
        (calendar/days-in-month y m)
        (list))))
  "Day entries for a month, each a leaf to the day agenda.")

(define calendar/browse-days-acc
  (lambda
    (y m d dim acc)
    (if
      (> d dim)
      acc
      (calendar/browse-days-acc
        y
        m
        (+ d 1)
        dim
        (append
          acc
          (list
            (list
              (as-str d)
              (string-append
                "/calendar?day="
                y
                "-"
                (calendar/pad2 m)
                "-"
                (calendar/pad2 d))))))))
  "Accumulate day entries (label D, target /calendar?day=YYYY-MM-DD).")

(define calendar/browse-event-years
  (lambda
    ()
    (calendar/browse-year-acc (calendar/all-events) (list)))
  "Distinct years across stored events.")

(define calendar/browse-insert-sorted
  (lambda
    (s xs)
    (if
      (null? xs)
      (list s)
      (if
        (string<? s (as-str (car xs)))
        (cons s xs)
        (cons
          (car xs)
          (calendar/browse-insert-sorted s (cdr xs))))))
  "Insert into an ascending string list.")

(define calendar/browse-months
  (lambda (y) (calendar/browse-months-acc y 1 (list)))
  "Twelve YYYY-MM entries for a year.")

(define calendar/browse-months-acc
  (lambda
    (y m acc)
    (if
      (> m 12)
      acc
      (calendar/browse-months-acc
        y
        (+ m 1)
        (append
          acc
          (list
            (list
              (string-append
                (calendar/pad2 m)
                " "
                (calendar/month-name m))
              (string-append y "-" (calendar/pad2 m))))))))
  "Accumulate month entries (label DD Mon, target YYYY-MM).")

(define calendar/browse-numsort
  (lambda (xs) xs)
  "Lists are built ascending already; identity for clarity.")

(define calendar/browse-year-acc
  (lambda
    (rows acc)
    (if
      (null? rows)
      acc
      (let
        ((d (as-str (dict-get (cdr (car rows)) "day"))))
        (if
          (< (string-length d) 4)
          (calendar/browse-year-acc (cdr rows) acc)
          (let
            ((y (substring d 0 4)))
            (calendar/browse-year-acc
              (cdr rows)
              (if
                (calendar/browse-contains acc y)
                acc
                (calendar/browse-insert-sorted y acc))))))))
  "Accumulate distinct YYYY prefixes from event rows.")

(define calendar/browse-years
  (lambda
    ()
    (let
      ((today (calendar/today)))
      (let
        ((y (substring today 0 4)))
        (let
          ((ev-years (calendar/browse-event-years)))
          (let
            ((ys
               (calendar/browse-numsort
                 (if
                   (calendar/browse-contains ev-years y)
                   ev-years
                   (calendar/browse-insert-sorted y ev-years)))))
            (list/map
              (lambda (row) (list row row))
              ys))))))
  "Years with events, plus the current year; sorted ascending.")

(define calendar/month-name
  (lambda
    (m)
    (nth
      (list
        "Jan"
        "Feb"
        "Mar"
        "Apr"
        "May"
        "Jun"
        "Jul"
        "Aug"
        "Sep"
        "Oct"
        "Nov"
        "Dec")
      (- m 1)))
  "Month abbreviation for 1..12.")

(define filmstrip/act-glyph
  (lambda (word)
    (let ((g (filmstrip/act-glyph-lookup (as-str word) filmstrip/act-glyphs)))
      (if (string-eq g "") (as-str word) g)))
  "Action glyph for a verb word; the WORD when the table has no glyph
(so an unknown verb renders as itself -- never an empty button).")

(define filmstrip/act-glyph-lookup
  (lambda (word table)
    (if (null? table)
        ""
        (if (string-eq (car (car table)) word)
            (as-str (car (cdr (car table))))
            (filmstrip/act-glyph-lookup word (cdr table)))))
  "The glyph for word in table, or \"\" when the table has no entry.")

;; ---- action glyphs (UI vocabulary, ONE table) ----
;; borge/filmstrip.borg action-glyphs: verbs are GLYPHS in the row; the
;; word survives as title + aria-label + data-verb (the queryable
;; identity), never as the label. The default table lives HERE because
;; the LIB renders the pane head too -- a host that wants another glyph
;; set redefines this one function (same seam as filmstrip/apps), no
;; renderer fork. Narrow viewports and a missing/invalid glyph both
;; fall back to the word, so the shell degrades to the old behavior
;; instead of rendering a blank button.
(define filmstrip/act-glyphs
  (list
    (list "focus" "⦿")
    (list "pin" "⤒")
    (list "unpin" "⤓")
    (list "width" "⇆")
    (list "close" "⌄")
    (list "restore" "⌃")
    (list "destroy" "✕")
    (list "switch" "⇥")
    (list "collapse" "⇱")
    (list "single" "⛶"))
  "Verb word -> glyph pairs (the UI vocabulary table). A verb with no\nentry (or an entry whose glyph is empty) renders its WORD -- glyphs\nare an improvement over words, never a hard requirement.")

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

(define filmstrip/apps
  (lambda
    ()
    (list
      (list "todos" "todo")
      (list "notes" "notes")
      (list "calendar" "calendar")))
  "Column 0: the app chooser. Entries are (label . target); target is the\npackage key that names the browse below.")

(define filmstrip/axis-sentinel
  (lambda (axis ?oob)
    (html/tag
      "div"
      (html/attrs
        (append
          (list
            (html/attr "id" "fs-axis")
            (html/attr "class" (string-append "fs-sh-axis " axis)))
          (if ?oob
              (list (html/attr "hx-swap-oob" "outerHTML:#fs-axis"))
              (list))))
      ""))
  "Inert axis sentinel (axis PINNED col -- BORGE-1789879934); CSS
:has(#fs-axis.col) lays the shell out as the left-rail sidebar; no OOB
mutator remains for it.")

;; ---- v5 shell furniture (ported from the glochid app surface, 10-14:
;; the desktops/rail-mode back-migration moved the DEFS; this completes
;; the COMPOSITION + theme so the framework shell IS the v5 shell --
;; sidebar-only (BORGE-1789879934), start menu on the railtabs row) ----

(define filmstrip/boxrows
  (lambda ()
    (let ((v (dict-get (filmstrip/ws-fields) "boxrows")))
      (if
        (if (null? v) #t (string-eq (as-str v) ""))
        (list)
        (json-parse (as-str v)))))
  "Visited-rail-box MRU: JSON array of root ids, newest first (ws field
boxrows; absent/empty = none visited this session lineage -- save-all
carries the field as \"\" when unset, so the empty string answers the
empty set, never a parse error).")

(define filmstrip/boxrows-has
  (lambda (root rows)
    (if (null? rows) #f
        (if (string-eq (as-str (car rows)) root) #t
            (filmstrip/boxrows-has root (cdr rows)))))
  "Membership probe for the MRU set (string-eq, order-irrelevant).")

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
            (appended? (nth res 2)))
          (seq
            (filmstrip/save-all path2 panes2 focus2)
            (render-html
              (html/join
                (list
                  (filmstrip/oob-msg "")
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
          ((panes2 (filmstrip/remove-at panes i))
            (focus2 (filmstrip/close-focus i focus (length panes2))))
          (seq
            (filmstrip/save-all cascade panes2 focus2)
            (render-html
              (html/join
                (list
                  (filmstrip/oob-msg "")
                  (filmstrip/oob-delete-pane id)
                  (if
                    (if (>= focus2 0) (not (= focus2 focus)) #f)
                    (filmstrip/oob-section
                      (filmstrip/pane-at panes2 focus2)
                      #t)
                    "")))))))))
  "POST /filmstrip/close: id param -> remove the pane record; OOB delete\nof its section + focus hand-off per close-focus.")

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

(define filmstrip/close-heal
  (lambda
    (panes)
    (let*
      ((d (filmstrip/ws-desktop))
        (m (filmstrip/first-region-live panes 0 "main" d))
        (r (filmstrip/first-region-live panes 0 "rail" d)))
      (cond
        ((>= m 0) m)
        ((>= r 0) r)
        (#t (filmstrip/first-live-index panes)))))
  "Focus target after a close SHELVED the focused pane (v3): first a\nlive MAIN pane on the CURRENT desktop (the v2 lift continues, on a\npane the operator can actually see), else the first live RAIL pane\n(dock semantics: a rail pane is visible on every desktop, so a\nfocused rail pane satisfies I1 with no main lift), else the first\nlive pane anywhere (v2 behavior for region-less hosts). Never a\nshelved pane. Without this, closing the last focused pane while\npinned panes showed beside it healed focus onto a hidden 0x0 main\npane on a foreign desktop — the exact I1 violation the v3 spec\nbans (no 0x0 focused pane).")

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

(define filmstrip/content-area
  (lambda (panes focus)
    (html/tag
      "div"
      (html/attrs
        (list
          (html/attr "id" "fs-surface")
          (html/attr "class" "fs-surface")))
      (html/join
        (list
          (html/tag
            "div"
            (html/attrs
              (list
                (html/attr "id" "fs-content")
                (html/attr "class" "fs-content")))
            (html/join (filmstrip/panes-region panes focus "main")))
          (filmstrip/rail-box panes focus)))))
  "The surface area (v5): the main strip FIRST, then the rail box.
Wrapping both under #fs-surface gives the region-move OOB ONE swap
target (filmstrip/oob-content-strip re-renders this wrapper; every
pane body is an hx-preserve placeholder, so live DOM survives the move
between strip and rail).")

(define filmstrip/css
  (lambda
    ()
    (css/sheet
      (list
          ;; THE PALETTE IS THE THEME (1a ledger): every token resolves
          ;; live from the omarchy theme's colors.toml via glochid/tg
          ;; where that host prim is bound (the app); omarchy-less boxes
          ;; never resolve past tg's own fail-soft defaults.
          (css/rule
            ":root"
            (list
              "color-scheme" (guard (lambda (e) "dark")
                (glochid/tg "mode" "dark"))
              "--border" (guard (lambda (e) "#2a2e3a")
                (glochid/tg "selection" "#2a2e3a"))
              "--panel" (guard (lambda (e) "#1b1f2a")
                (glochid/tg "lighter_background" "#1b1f2a"))
              "--ink" (guard (lambda (e) "#d8dee9")
                (glochid/tg "foreground" "#d8dee9"))
              "--muted" (guard (lambda (e) "#6b7080")
                (glochid/tg "muted" "#6b7080"))
              "--chrome" (guard (lambda (e) "#12141c")
                (glochid/tg "dark_background" "#12141c"))
              "--sel-bg" (guard (lambda (e) "#2a2e3a")
                (glochid/tg "selection" "#2a2e3a"))
              "--sel-accent" (guard (lambda (e) "#7aa2f7")
                (glochid/tg "accent" "#7aa2f7"))
              "--pp-bg" (guard (lambda (e) "#0f1117")
                (glochid/tg "darker_background" "#0f1117"))
              "--pp-repl" (guard (lambda (e) "#bb9af7")
                (glochid/tg "magenta" "#bb9af7"))
              "--pp-ok" (guard (lambda (e) "#2f9e44")
                (glochid/tg "green" "#2f9e44"))
              "--pp-warn" (guard (lambda (e) "#e8590c")
                (glochid/tg "orange" (glochid/tg "yellow" "#e8590c")))
              "--pp-danger" (guard (lambda (e) "#c92a2a")
                (glochid/tg "bright_red" "#c92a2a"))
              "--pp-btn" (guard (lambda (e) "#2a6fdb")
                (glochid/tg "blue" "#2a6fdb"))
              "--pp-btn-hi" (guard (lambda (e) "#3b5bdb")
                (glochid/tg "accent" "#3b5bdb"))
              ))
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
            "hidden"
            ;; v5 focus-scroll: when the OOB swap autofocuses the focused
            ;; pane, the browser scrolls every scrollable ancestor so the
            ;; section is on-stage; these inline margins reserve peek
            ;; space so the NEIGHBOR columns stay visible at the edges
            ;; (niri semantics; the margin shrinks as the column grows).
            "scroll-margin-inline"
            "3rem"))
        (css/rule
          ".fs-pane.fs-third"
          (list "flex-basis" "33.333%" "scroll-margin-inline" "3rem"))
        (css/rule ".fs-pane.fs-half" (list "flex-basis" "50%" "scroll-margin-inline" "1.5rem"))
        (css/rule ".fs-pane.fs-full" (list "flex-basis" "100%" "scroll-margin-inline" "0"))
        ;; v5 one-up: while ANY LIVE single pane exists it takes the
        ;; whole strip and its strip siblings step aside (shelved and
        ;; cross-desktop panes are already display:none; a shelved
        ;; single releases the strip back). Rail-region panes are not
        ;; strip siblings (glochid renders them in its rail box; a bare
        ;; host shell shows pinned panes as columns) - the region guard
        ;; keeps those visible.
        (css/rule ".fs-pane.fs-single"
          (list "flex-basis" "100%" "scroll-margin-inline" "0"))
        (css/rule
          ".fs-shell:has(.fs-pane.fs-single:not(.shelved)) .fs-pane[data-region=\"main\"]:not(.fs-single)"
          (list "display" "none"))
        (css/rule
          ".fs-pane.focused"
          (list
            "outline"
            "2px solid var(--sel-accent)"
            "outline-offset"
            "-2px"))
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
          ;; inert sentinels carry the axis/desk/railmode class to CSS;
          ;; they must not take layout space (back-migrated, phase 2).
          (css/rule ".fs-sh-axis" (list "display" "none"))
          ;; v2 desktops: the #ws-desk sentinel carries the current
          ;; desktop to CSS -- one :has pair per desktop (0..9) hides
          ;; panes of the others (data-desktop is on every pane section;
          ;; no JS, no re-render).
          (css/rule ".fs-shell:has(#ws-desk.dsk-0) .fs-pane[data-desktop]:not([data-desktop=\"0\"]):not([data-region=\"rail\"])" (list "display" "none"))
          (css/rule ".fs-shell:has(#ws-desk.dsk-1) .fs-pane[data-desktop]:not([data-desktop=\"1\"]):not([data-region=\"rail\"])" (list "display" "none"))
          (css/rule ".fs-shell:has(#ws-desk.dsk-2) .fs-pane[data-desktop]:not([data-desktop=\"2\"]):not([data-region=\"rail\"])" (list "display" "none"))
          (css/rule ".fs-shell:has(#ws-desk.dsk-3) .fs-pane[data-desktop]:not([data-desktop=\"3\"]):not([data-region=\"rail\"])" (list "display" "none"))
          (css/rule ".fs-shell:has(#ws-desk.dsk-4) .fs-pane[data-desktop]:not([data-desktop=\"4\"]):not([data-region=\"rail\"])" (list "display" "none"))
          (css/rule ".fs-shell:has(#ws-desk.dsk-5) .fs-pane[data-desktop]:not([data-desktop=\"5\"]):not([data-region=\"rail\"])" (list "display" "none"))
          (css/rule ".fs-shell:has(#ws-desk.dsk-6) .fs-pane[data-desktop]:not([data-desktop=\"6\"]):not([data-region=\"rail\"])" (list "display" "none"))
          (css/rule ".fs-shell:has(#ws-desk.dsk-7) .fs-pane[data-desktop]:not([data-desktop=\"7\"]):not([data-region=\"rail\"])" (list "display" "none"))
          (css/rule ".fs-shell:has(#ws-desk.dsk-8) .fs-pane[data-desktop]:not([data-desktop=\"8\"]):not([data-region=\"rail\"])" (list "display" "none"))
          (css/rule ".fs-shell:has(#ws-desk.dsk-9) .fs-pane[data-desktop]:not([data-desktop=\"9\"]):not([data-region=\"rail\"])" (list "display" "none"))
          ;; v4 rail modes: the SAME hiding trick desktops use, so a
          ;; mode flip never re-renders (expansion state is DOM).
          (css/rule ".fs-shell:has(#fs-railmode.rm-windows) .fs-rmode:not([data-rmode=\"windows\"])" (list "display" "none"))
          (css/rule ".fs-shell:has(#fs-railmode.rm-code) .fs-rmode:not([data-rmode=\"code\"])" (list "display" "none"))
          (css/rule ".fs-shell:has(#fs-railmode.rm-prims) .fs-rmode:not([data-rmode=\"prims\"])" (list "display" "none"))
          (css/rule ".fs-shell:has(#fs-railmode.rm-ui) .fs-rmode:not([data-rmode=\"ui\"])" (list "display" "none"))
        (css/rule ".fs-sentinel" (list "display" "none"))
        (css/rule ".fs-sink" (list "display" "none"))
          (css/rule ".fs-oob-delete" (list "display" "none"))
          ;; v5 theme + layout (ported from the glochid app surface's
          ;; base-css, 10-14): the :root palette ABOVE declares the theme;
          ;; these rules APPLY it (dark shell, sidebar row layout, rail
          ;; box, start menu, lookbook + CM pane chrome).
          (css/raw "html,body{height:100%}body{background:var(--pp-bg);color:var(--ink);font-family:ui-monospace,Menlo,monospace;margin:0;padding:1rem;box-sizing:border-box}a{color:var(--sel-accent);text-decoration:none}a:hover{text-decoration:underline}.muted{color:var(--muted)}h1{margin:.25rem 0 .75rem 0;font-size:1.1rem}table{border-collapse:collapse;width:100%;max-width:60rem}td,th{padding:.35rem .6rem;border-bottom:1px solid var(--border);text-align:left;white-space:nowrap}pre{background:var(--chrome);padding:1rem;border-radius:6px;overflow:auto}input{background:var(--panel);color:var(--ink);border:1px solid var(--border);border-radius:6px;padding:.5rem}select{background:var(--panel);color:var(--ink);border:1px solid var(--border);border-radius:4px}label{color:var(--muted)}button{background:var(--pp-btn);color:#fff;border:0;border-radius:6px;padding:.5rem 1rem;cursor:pointer}")
          (css/raw ".fs-shell{height:calc(100vh - 2rem)}.fs-shell:has(#fs-axis.col){flex-direction:row}.fs-shell:has(#fs-axis.col) .fs-content{order:0;flex:1 1 auto;min-height:0;min-width:0}#fs-surface{display:contents}#fs-rail{display:contents}.fs-shell:has(#fs-axis.col) #fs-rail{order:-3;display:flex;flex-direction:column;flex:0 0 20rem;max-width:20rem;min-width:0;min-height:0;overflow-y:auto;overflow-x:hidden;gap:.25rem;padding:.25rem .25rem .25rem 0}")
          (css/raw ".fs-shell:not(:has(#fs-axis.col)) .fs-pane[data-region=\\\"rail\\\"]{flex:0 0 auto;max-height:20rem;min-width:20rem;min-height:0;overflow:hidden}.fs-shell:not(:has(#fs-axis.col)) .fs-pane[data-region=\\\"rail\\\"] .fs-body{display:block;flex:1 1 auto;min-height:0;overflow:auto}.fs-shell:has(#fs-axis.col) .fs-pane[data-region=\\\"rail\\\"]{flex:1 1 auto;width:100%;min-height:0}.fs-shell:has(#fs-axis.col) .fs-pane[data-region=\\\"rail\\\"] .fs-body{display:block;flex:1 1 auto;min-height:0;overflow:auto}.fs-shell:has(#fs-axis.col) .chatpage{height:100%}")
          (css/raw ".bubble{background:var(--panel);border-radius:6px;padding:.5rem .75rem;margin:.5rem 0;white-space:pre-wrap}.bubble.user{color:var(--pp-ok)}.bubble.llm{color:var(--ink)}.bubble.repl{color:var(--pp-repl)}.trace{color:var(--muted);font-size:.85em;white-space:pre-wrap}.think{color:var(--muted);font-size:.9em;margin:.4rem 0}.think summary{cursor:pointer}.think>div{white-space:pre-wrap}.reason{color:var(--muted);font-style:italic}.think .tk,.think .tool{margin:.25rem 0 .25rem .25rem}.think .tk>summary,.think .tool>summary{color:var(--muted)}.think .tk>div,.think .tool>div{margin-left:.9rem;padding-left:.5rem;border-left:1px solid var(--border)}.think:not(:has(.reason,.trace)){display:none}.chatpage{display:flex;flex-direction:column;height:calc(100vh - 6rem)}.chatpage #log{flex:1;min-height:0;overflow-y:auto;border:1px solid var(--border);border-radius:6px;padding:.75rem;margin-bottom:.75rem}.chatpage form{display:flex;gap:.5rem;flex-shrink:0}.chatpage #msg{flex:1}.chatpage form button{flex-shrink:0}")
          (css/raw "*{scrollbar-width:thin;scrollbar-color:var(--sel-bg) transparent}::-webkit-scrollbar{width:10px;height:10px}::-webkit-scrollbar-track{background:transparent}::-webkit-scrollbar-thumb{background:var(--sel-bg);border-radius:6px}::-webkit-scrollbar-thumb:hover{background:var(--muted)}::-webkit-scrollbar-corner{background:transparent}.fs-applink{background:none;border:0;color:var(--sel-accent);padding:0;margin:0;font:inherit;cursor:pointer;text-decoration:none}.fs-applink:hover{text-decoration:underline}.fs-modelsel{font-size:.6em;padding:.1rem .3rem}")
          (css/raw ".lb-btn{font:inherit;padding:.25rem .7rem;border:1px solid var(--border);border-radius:4px;background:var(--panel);color:inherit;cursor:pointer}.lb-btn-primary{background:var(--pp-btn-hi);border-color:var(--pp-btn-hi);color:#fff}.lb-btn-danger{background:var(--pp-danger);border-color:var(--pp-danger);color:#fff}.lb-badge{display:inline-block;font-size:.72em;padding:.05rem .45rem;border-radius:999px;border:1px solid var(--border);color:var(--muted)}.lb-badge-ok{color:var(--pp-ok);border-color:var(--pp-ok)}.lb-badge-warn{color:var(--pp-warn);border-color:var(--pp-warn)}.lb-card{border:1px solid var(--border);border-radius:6px;padding:.75rem;background:var(--panel)}.lb-card h3{margin:0 0 .5rem;display:flex;gap:.5rem;align-items:center}.lb-card p{margin:.25rem 0 0}.lb-stage{border:1px dashed var(--border);padding:.9rem;border-radius:6px;display:flex;gap:.6rem;align-items:center;flex-wrap:wrap}.lb-form{display:flex;gap:.4rem;flex-wrap:wrap;align-items:center;margin:.6rem 0}.lb-form input{font:inherit;padding:.2rem .4rem;background:var(--panel);color:inherit;border:1px solid var(--border);border-radius:4px}.lb-form label{color:var(--muted,#9aa3b2);font-size:.85em}.lb-folds{margin-top:.6rem}.lb-folds details{margin:.3rem 0}.lb-folds summary{cursor:pointer;color:var(--muted,#9aa3b2)}.lb-folds pre{overflow:auto;font-size:.85em;background:var(--panel);padding:.5rem;border-radius:4px;border:1px solid var(--border)}.lb-vtabs{display:flex;gap:.3rem;flex-wrap:wrap;margin:.4rem 0}.lb-vtab{cursor:pointer}.lb-vtab.fs-sel{outline:1px solid var(--sel-accent)}.lb-swatch{display:inline-flex;align-items:center;gap:.4rem;padding:.15rem .55rem;border:1px solid var(--border);border-radius:4px;font-size:.85em}.lb-swatch b{width:.95em;height:.95em;display:inline-block;border-radius:3px;border:1px solid var(--border)}.lb-palette{display:flex;gap:.35rem;flex-wrap:wrap}.lb-palette .lb-swatch{border:none;padding-left:0}.lb-palette .lb-swatch b{width:1.6em;height:1.6em}.lb-palette .lb-swatch span{color:var(--muted)}")
          (css/raw ".fs-railtabs{display:flex;gap:.3rem;align-items:center;border:1px solid var(--border);background:var(--panel);padding:.3rem .35rem}.fs-start{position:relative;flex:0 0 auto}.fs-start-word{cursor:pointer;list-style:none;font-weight:700}.fs-start-word::-webkit-details-marker{display:none}.fs-start-menu{position:absolute;left:0;top:100%;z-index:30;display:flex;flex-direction:column;min-width:10rem;background:var(--panel);border:1px solid var(--border);box-shadow:0 4px 14px rgba(0,0,0,.5);padding:.25rem}.fs-start-entry{width:100%;text-align:left;padding:.3rem .5rem;white-space:nowrap}.fs-start-entry:hover{background:var(--sel-bg)}.fs-railtab.fs-btn{background:none;border:1px solid transparent;color:var(--sel-accent)}.fs-keys{margin-left:auto;color:var(--muted,#6b7080);cursor:help;font-size:1.1em;user-select:none}")
          (css/raw ".pp-cm-host .CodeMirror{height:80vh;font-size:13px;line-height:1.4;background:var(--chrome);color:var(--ink)}.pp-cm-host .CodeMirror-gutters{background:var(--chrome);border-right:1px solid var(--border)}.pp-cm-host .CodeMirror-linenumber{color:var(--muted)}.pp-cm-host .CodeMirror-cursor{border-left:1px solid var(--sel-accent)}.pp-cm-bar{display:flex;gap:.6rem;align-items:center;margin:.35rem 0}.pp-cm-save{cursor:pointer}.pp-cm-status::after{content:none}"))))
  "Strip + pane + launcher stylesheet (~50 lines of rules). Widths are\nthe three presets only; .focused is the server-owned ring.")

(define filmstrip/default-width
  "third")

(define filmstrip/desk-count 3
  "Desktops the pane-head move verb CYCLES (0 .. N-1, wraps). Glochid\ndeclares three (the windows tree's desk/0..2 rows); a host modeling a\nbigger ring redefines this. The :has CSS may cover 0..9 -- the cycle\nwalks the DECLARED ring, not the CSS ceiling.")

;; ---- 2d. DESKTOPS (back-migrated from the glochid embedded copy,
;; phase 2 of the cut-over; the app copies retire) ----
;; The ws row carries "desktop":N (0 default) and each pane record a
;; "desktop" field. An inert #ws-desk sentinel beside #fs-axis carries
;; the current desktop to CSS (:has hides the other desktops' panes --
;; ten pairs in filmstrip/css, no JS); the windows tree's desktop rows
;; carry the switch verb. Focus heals onto the first live pane of the
;; target desktop when the focused pane lives elsewhere (a live MAIN
;; pane is preferred -- the lift; a rail pane satisfies I1 when no
;; main exists). The desktop READ was already framework-side
;; (filmstrip/ws-desktop); these are the shell pieces that lived only
;; in the copy.
(define filmstrip/desk-sentinel
  (lambda
    (d ?oob)
    (html/tag
      "div"
      (html/attrs
        (append
          (list
            (html/attr "id" "ws-desk")
            (html/attr "class" (string-append "fs-sh-axis dsk-" (as-str d))))
          (if
            ?oob
            (list (html/attr "hx-swap-oob" "outerHTML:#ws-desk"))
            (list))))
      ""))
  "Inert desktop sentinel beside the axis one; CSS :has(#ws-desk.dsk-N)
hides the other desktops' panes. ?oob renders it as an OOB outerHTML
swap (the desktop response).")

(define filmstrip/desktop-set
  (lambda
    ()
    (let*
      ((dv (request-param "d"))
        (d (if (null? dv) 0 (parse-int (as-str dv))))
        (row (filmstrip/row))
        (cascade (nth row 0))
        (panes (nth row 1))
        (focus (nth row 2))
        (focus2
          (if
            (and
              (>= focus 0)
              (= (filmstrip/pane-desk (filmstrip/pane-at panes focus)) d))
            focus
            (let
              ((f (filmstrip/first-on-desk panes d 0)))
              (if (< f 0) focus f)))))
      (seq
        (filmstrip/ws-save
          (dict-set* (filmstrip/ws-fields) (list "desktop" (as-str d))))
        (filmstrip/save-all cascade panes focus2)
        (render-html
          (html/join
            (list
              (filmstrip/oob-msg "")
              (filmstrip/desk-sentinel d #t)
              (filmstrip/oob-content-strip panes focus2)))))))
  "POST /filmstrip/desktop: d param -> set + persist the shell desktop;
heal focus onto the first live pane of that desktop when the focused
pane lives elsewhere; answer with OOB swaps of the desk sentinel + a
re-render of the content strip (panes of the new desktop keep their
live DOM via hx-preserve; panes of the others re-render inert -- the
workspace row keeps every record, only the swapped DOM shrinks).")

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

(define filmstrip/first-live-index
  (lambda (panes)
    (filmstrip/find-index filmstrip/pane-live? panes 0))
  "Index of the first live (not shelved) pane, else -1.")

(define filmstrip/first-on-desk
  (lambda
    (panes d i)
    (let
      ((m (filmstrip/first-region-on-desk panes d i "main")))
      (if (< m 0) (filmstrip/first-region-on-desk panes d i "rail") m)))
  "Index of the first LIVE pane on desktop d, else -1 (v3 heal
discipline: a live MAIN pane is preferred -- it takes the lift; rail
panes are valid focus targets too and satisfy I1 when the desktop has
no live main pane. Never a shelved pane: pane-live? gates both passes.")

(define filmstrip/first-region-live
  (lambda (panes i region d)
    (if
      (null? panes)
      -1
      (if
        (and
          (filmstrip/pane-live? (car panes))
          (string-eq (filmstrip/pane-region (car panes)) region)
          (if (string-eq region "rail")
              #t
              (= (filmstrip/pane-desk (car panes)) d)))
        i
        (filmstrip/first-region-live (cdr panes) (+ i 1) region d))))
  "Index of the first LIVE pane of one region (else -1). MAIN panes
are desktop-scoped (must sit on desktop d); RAIL panes are
desktop-global (dock semantics — visible on every desktop, so the d
check is skipped). Never a shelved pane.")

;; Desktops heal/lookup furniture (back-migrated from the glochid
;; embedded copy, phase 2 of the cut-over): region- and desktop-scoped
;; live-pane indexes for heal-focus and the desktop switch.
(define filmstrip/first-region-on-desk
  (lambda
    (panes d i region)
    (if
      (null? panes)
      -1
      (if
        (and
          (= (filmstrip/pane-desk (car panes)) d)
          (string-eq (filmstrip/pane-region (car panes)) region)
          (filmstrip/pane-live? (car panes)))
        i
        (filmstrip/first-region-on-desk (cdr panes) d (+ i 1) region))))
  "Index of the first LIVE pane of one REGION on desktop d, else -1.")
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
            (let
              ((focus2 (filmstrip/rail-focus-onto panes focus i)))
              (seq
                (filmstrip/save-all cascade panes focus2)
                (render-html
                  (html/join
                    (list
                      (filmstrip/oob-msg "")
                      (if
                        (if (>= focus 0) (not (= focus focus2)) #f)
                        (filmstrip/oob-section
                          (filmstrip/pane-at panes focus)
                          #f)
                        "")
                      (filmstrip/oob-section
                        (filmstrip/pane-at panes i)
                        #t))))))))))
  "POST /filmstrip/focus-id: id param -> point .focused at that pane's\nindex (persisted + same OOB two-section class swap as focus).\nShelved/unknown ids answer the status line only; an already-focused\nid answers empty. v3: a rail-region target never takes the lift — a\nlifted live main pane keeps focus and only the ring lands in the rail.")

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

(define filmstrip/heal-focus
  (lambda
    (panes focus)
    (if
      (if
        (< focus 0)
        #t
        (string-eq
          (filmstrip/pane-state (filmstrip/pane-at panes focus))
          "shelved"))
      (let
        ((f (filmstrip/first-on-desk panes (filmstrip/ws-desktop) 0)))
        (if (< f 0) (filmstrip/first-live-index panes) f))
      focus))
  "Focus that always has a live pane behind it: unset (-1) and
shelved-pointing both heal to the first live pane on the CURRENT
desktop -- v3: a live MAIN pane first (the lift), the first live RAIL
pane as the fallback (a focused rail pane in the rail satisfies I1
when the desktop has no live main pane); -1 only when the desktop has
no live pane at all (falls back to the first live pane anywhere).
Never a shelved pane (I2). Not persisted -- alt+right heals it for
real on the next focus step.")

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

(define filmstrip/json-arr
  (lambda
    (xs)
    (if
      (null? xs)
      "[]"
      (string-append "[" (filmstrip/json-arr-tail xs) "]")))
  "Join already-encoded JSON object strings into one array literal.")

(define filmstrip/json-arr-tail
  (lambda
    (xs)
    (if
      (null? (cdr xs))
      (car xs)
      (string-append (car xs) "," (filmstrip/json-arr-tail (cdr xs)))))
  "Comma join (filmstrip/json-arr's inner walk).")

(define filmstrip/keep-nodes
  (lambda
    (nodes)
    (if
      (null? nodes)
      (list)
      (if
        (string-eq (car nodes) "")
        (filmstrip/keep-nodes (cdr nodes))
        (cons (car nodes) (filmstrip/keep-nodes (cdr nodes))))))
  "Drop the empty-string markers map-indexed leaves in place (the tree
renderer takes only real node dicts).")

(define filmstrip/key-rows
  (list
    (list "/filmstrip/focus" "{\"dir\":\"left\"}" "keydown[altKey&&!shiftKey&&key=='ArrowLeft'] from:body")
    (list "/filmstrip/focus" "{\"dir\":\"right\"}" "keydown[altKey&&!shiftKey&&key=='ArrowRight'] from:body")
    (list "/filmstrip/move" "{\"dir\":\"left\"}" "keydown[altKey&&shiftKey&&key=='ArrowLeft'] from:body")
    (list "/filmstrip/move" "{\"dir\":\"right\"}" "keydown[altKey&&shiftKey&&key=='ArrowRight'] from:body"))
  "Framework keyboard registry rows (url vals trigger): the four
alt+arrow defaults. Host apps APPEND extra rows at boot through
filmstrip/register-keys! instead of copying this registry.")

(define filmstrip/key-sentinels
  (lambda
    ()
    (html/join
      (list/map
        (lambda (row)
          (filmstrip/sentinel (car row) (car (cdr row)) (car (cdr (cdr row)))))
        filmstrip/key-rows)))
  "alt+arrows move focus; alt+shift+arrows move pane order. The server\nowns focus and answers with OOB class swaps (borge pane-protocol).\nSentinels render from the keyboard registry (filmstrip/key-rows):\nframework defaults, then host-appended rows.")

(define filmstrip/keys-hint
  (lambda ()
    (html/span
      (html/attrs
        (list
          (html/attr "class" "fs-keys")
          (html/attr "title"
            (string-append
              "alt+←/→ focus · alt+shift+←/→ move "
              "· alt+1/2/3 desktop · alt+n new session "
              "· alt+w close · alt+p pin · alt+s single"))))
      (html/text "⌨")))
  "The keyboard map as a muted glyph in the rail tabs row: the map is
the title tooltip (hover affordance), zero JS.")

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

(define filmstrip/launcher-hint
  (lambda ()
    (let
      ((rs (filmstrip/hint-routes)))
      (if (null? rs) "/" (car rs))))
  "The launcher's example route: the first real route of THIS host's\napp list (was the habitat-specific /todos — the miss that made glochid's\nlauncher a dead end).")

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
                (html/attr "placeholder" "/todos")
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

(define filmstrip/lift-holds-live-main?
  (lambda
    (panes old)
    (if
      (< old 0)
      #f
      (let
        ((p (filmstrip/pane-at panes old)))
        (and
          (filmstrip/pane-live? p)
          (string-eq (filmstrip/pane-region p) "main")))))
  "True when the persisted focus points at a LIVE main-region pane (that
pane owns the fixed lift). A focused rail pane is a ring, not a window
— it never counts as the lift.")

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

(define filmstrip/move-to
  (lambda
    ()
    (let*
      ((id (as-str (request-param "id")))
        (dp (request-param "d"))
        (row (filmstrip/row))
        (cascade (nth row 0))
        (panes (nth row 1))
        (focus (nth row 2))
        (i (filmstrip/find-pane-index panes id)))
      (if
        (< i 0)
        (render-html (filmstrip/oob-msg "filmstrip: no such pane"))
        (if
          (string-eq (filmstrip/pane-region (nth panes i)) "rail")
          (render-html
            (filmstrip/oob-msg
              "filmstrip: rail panes dock on every desktop"))
          (let*
            ((cur (filmstrip/pane-desk (nth panes i)))
              (d
                (if (null? dp)
                    (as-str
                      (if (< cur (- filmstrip/desk-count 1))
                          (+ cur 1)
                          0))
                    (as-str dp)))
              (p2 (filmstrip/pane-set-desk (nth panes i) d))
              (panes2 (filmstrip/replace-at panes i p2))
              (leaves?
                (and
                  (= i focus)
                  (not (string-eq d (as-str (filmstrip/ws-desktop))))))
              ;; a FOCUSED pane leaving the operator's desktop goes
              ;; hidden under the :has sentinel -- exactly the close
              ;; case: heal onto the nearest strip-visible pane
              ;; (close-heal's rule; never a hidden/shelved pane, I1).
              (focus2
                (if leaves?
                    (filmstrip/close-heal panes2 i)
                    focus)))
          (seq
            (filmstrip/save-all cascade panes2 focus2)
            (render-html
              (html/join
                (list
                  (filmstrip/oob-msg
                    (string-append "moved to desktop " d))
                  ;; the windows tree's desk dirs are the pane
                  ;; inventory -- the moved row changed subtree.
                  (filmstrip/winbox-oob)
                  ;; ALWAYS re-render the strip: a move onto the
                  ;; current desktop must SHOW the pane (data-desktop
                  ;; attr refresh), a leave must refresh the focus
                  ;; classes onto the healed pane. Bodies preserved.
                  (filmstrip/oob-content-strip panes2 focus2))))))))))
  "POST /filmstrip/move-to: id + OPTIONAL d params -> move a pane to\nanother desktop. Without d the pane-head verb's semantics: cycle to\nthe next desktop in the declared ring (filmstrip/desk-count, wraps).\nRail-region panes refuse (dock semantics: they are desktop-global --\nnothing to move). A FOCUSED pane leaving the current desktop heals\nfocus first (close-heal's nearest-visible rule -- the moved pane is\nnow :has-hidden, and a hidden pane can never hold the ring, I1). The\nanswer is the windows-tree OOB (the row moved desk subtrees) + the\nwhole strip OOB (show on arrival / refresh focus classes on leave),\nbodies preserved -- the pin grammar.")

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
                    view-key))))))
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
            (html/attr "id" "fs-surface")
            (html/attr "class" "fs-surface")
            (html/attr "hx-swap-oob" "true")))
        (html/join
          (list
            (html/tag
              "div"
              (html/attrs
                (list
                  (html/attr "id" "fs-content")
                  (html/attr "class" "fs-content")))
              (html/join (filmstrip/panes-region panes focus "main")))
            (filmstrip/rail-box panes focus)))))
    "OOB re-render of the whole surface area (the main strip + the rail\nbox; every body is an hx-preserve placeholder, so live pane DOM\nsurvives the move between strip and rail on pin/unpin).")

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
                (if focused? " focused" "")))
            (html/attr "id" (string-append "fs-pane-" id))
            (html/attr "data-pane" id)
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
            "filmstrip: routes start with / (try /todos)"))
        (let
          ((path (car (filmstrip/split-query r))))
          (if
            (null? (route/lookup path))
            (render-html
              (filmstrip/oob-msg
                (string-append "filmstrip: no route " path)))
            (let*
              ((res (filmstrip/open-apply panes r))
                (panes2 (nth res 0))
                (focus2 (nth res 1))
                (appended? (nth res 2)))
              (seq
                (filmstrip/save-all cascade panes2 focus2)
                (render-html
                  (html/join
                    (list
                      (filmstrip/oob-msg "")
                      (filmstrip/open-response panes2 focus focus2 appended?)))))))))))
  "POST /filmstrip/open: launcher route param -> open-or-focus a pane.\nBad/missing routes answer with the launcher status line only; valid ones\npersist the row and answer with OOB pane swaps.")

(define filmstrip/open-apply
  (lambda
    (panes r)
    (let
      ((i (filmstrip/find-route-index panes r)))
      (if
        (>= i 0)
        (list panes i #f)
        (list
          (append
            panes
            (list
              (dict-set*
                "{}"
                (list
                  "id"
                  (filmstrip/new-id)
                  "route"
                  r
                  "width"
                  filmstrip/default-width))))
          (length panes)
          #t))))
  "Open-or-focus for route r: a fresh pane record (UUID id, default\nwidth preset) appended, or the existing pane's index. Returns\n(panes focus appended?) -- appended? false means an existing pane won.")

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

(define filmstrip/pane-desk
  (lambda (p)
    (let
      ((v (dict-get p "desktop")))
      (if (null? v) 0 (parse-int (as-str v)))))
  "Pane record desktop index (0 when unset — pre-v2 records ride
desktop 0).")

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
  "Pane title bar: route label + width cycle + close, all htmx POSTs.")

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
                (if focused? " focused" "")))
            (html/attr "id" (string-append "fs-pane-" id))
            (html/attr "data-pane" id)))
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
  "One pane section. content is the dispatched fragment (full page) or an\nempty hx-preserve placeholder (OOB class/width swaps keep the live DOM).")

(define filmstrip/pane-id
  (lambda (p) (as-str (dict-get p "id")))
  "Pane record id.")

(define filmstrip/pane-live?
  (lambda (p)
    (not (string-eq (filmstrip/pane-state p) "shelved")))
  "True when the pane is not shelved (missing state counts live).")

(define filmstrip/pane-region
  (lambda (p)
    (let
      ((r (as-str (dict-get p "region"))))
      (if (string-eq r "") "main" r)))
  "Pane record region (v3 pin): main | rail. Empty = main — records\nfrom before the region field exist and stay main (main keeps the v2\nmodel: strip + one lifted focused pane). The ACCESSOR defaults, so\nselectors + JSON see \"main\" on legacy records, never an empty value.")

(define filmstrip/pane-route
  (lambda (p) (as-str (dict-get p "route")))
  "Pane record route.")

(define filmstrip/pane-sclass
  (lambda
    (p focused?)
    (string-append
      "fs-pane"
      " fs-"
      (filmstrip/pane-width p)
      (if focused? " focused" "")
      (if (filmstrip/pane-single p) " fs-single" "")
      (if (string-eq (filmstrip/pane-state p) "shelved")
          " shelved"
          "")))
  "The pane section's class assembly — ONE place, so the natural render
and the OOB swap cannot drift (v5 adds fs-single to width + focused +
shelved).")

(define filmstrip/pane-set-desk
  (lambda (p d) (dict-set p "desktop" d))
  "Pane record with a new desktop (AS A STRING -- dict values ride\nJSON/records as strings everywhere; filmstrip/pane-desk parse-ints on\nread, so both field shapes read identically).")

(define filmstrip/pane-set-region
  (lambda (p r) (dict-set p "region" r))
  "Pane record with a new region (main | rail) — the field persists on
the record like width/state, so it rides every writer below save-all.")

(define filmstrip/pane-set-single
  (lambda (p s) (dict-set p "single" s))
  "Pane record with a new single value — the flag persists on the
record like region/state and rides every writer below save-all.")

(define filmstrip/pane-set-width
  (lambda (p w) (dict-set p "width" w))
  "Pane record with a new width preset.")

(define filmstrip/pane-single
  (lambda (p)
    (let
      ((v (dict-get p "single")))
      (if (string-eq (as-str v) "false")
          #f
          (not (null? v)))))
  "Pane record single flag (v5 one-up): true unless the key is absent
or explicitly false — the flag READS through, so staging then
unstaging (dict-set … #f) really turns it off (as-str #f = \"false\").")

(define filmstrip/pane-stamp
  (lambda
    (rec)
    rec)
  "Host hook on freshly-stamped pane records (default identity). A host\nmay add its own fields — glochid stamps the pane's desktop from the ws\nrow so an app-link open on desk>0 lands VISIBLE there (the lib must not\nread glochid row fields itself; same seam discipline as save-all).")

(define filmstrip/pane-state
  (lambda (p) (as-str (dict-get p "state")))
  "Pane record state: live | shelved (v2 shelf). Empty = live — records\nfrom before the state field exist and stay live.")

;; ---- 2b2. THE WINDOWS TREE (back-migrated with the desktops, phase 2
;; rail-trees-v4: ONE tree root renders the DECLARED ws state --
;; rail-trees-v4: ONE tree root renders the DECLARED ws state --
;; desktops 0..2 as dirs (DEFAULT-OPEN: eager kids + the switch verb;
;; the lazy level pull stays for the shelf) + a shelf subtree of shelved
;; panes. Pane rows carry focus/pin/width/close or restore/destroy and
;; acme-explicit to<N> move words. Declared state only -- no DOM
;; scraping.
(define filmstrip/pane-verb
  (lambda
    (word route vals)
    (list word route
      (string-append "{" (list/foldl
        (lambda
          (acc kv)
          (string-append acc
            (if (string-eq acc "") "" ",")
            "\"" (car kv) "\":\"" (filmstrip/url-enc (car (cdr kv))) "\""))
        "" vals)
        "}")
      ;; 4th element = the GLYPH. Rows carry it explicitly (the renderer
      ;; falls back to the word when it is empty), so a host edit or a
      ;; lib table change cannot silently re-label the rail.
      (filmstrip/act-glyph word)))
  "One tree verb row spec: (word route vals-json glyph) from (key val)
pairs; values ride filmstrip/url-enc (route/path vals carry ?&). The
glyph is filmstrip/act-glyph's answer for the word.")

(define filmstrip/pane-view
  (lambda (p) (as-str (dict-get p "view")))
  "Pane record's river window key (empty for habitat panes).")

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

(define filmstrip/panes-region
  (lambda (panes focus region)
    (filmstrip/map-indexed
      (lambda (p i)
        (if (string-eq (filmstrip/pane-region p) region)
            (filmstrip/pane-html p (= i focus)
              (filmstrip/pane-content (filmstrip/pane-route p)))
            ""))
      panes 0))
  "Pane sections of ONE region only (v5 split): main panes render into
the #fs-content strip and rail (pinned) panes into #fs-rail -- the
strip is the reading surface, the rail box the dock. Empty strings for
the other region keep map-indexed's shape (html/join skips them).")

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

(define filmstrip/rail-box
  (lambda (panes focus)
    (html/tag
      "div"
      (html/attrs
        (list
          (html/attr "id" "fs-rail")
          (html/attr "class" "fs-rail")))
      (html/join
        (append
          (filmstrip/panes-region panes focus "rail")
          (list
            (filmstrip/railtabs-html)
            (html/join (filmstrip/tree-boxes)))))))
  "The rail BOX (v5): docked panes + railtabs + tree boxes in one
flex-column side panel. Row-axis CSS turns #fs-rail into
display:contents (the furniture flows into the shell column as
before); col-axis CSS makes it a real 20rem left panel.")

(define filmstrip/rail-focus-onto
  (lambda
    (panes old new)
    (if
      (= new old)
      old
      (if
        (not
          (string-eq
            (filmstrip/pane-region (filmstrip/pane-at panes new))
            "rail"))
        new
        (if
          (filmstrip/lift-holds-live-main? panes old)
          old
          new))))
  "Focused index for a focus attempt onto pane `new` (v3 region
discipline): a rail-region target NEVER takes the fixed main slot. If
a live main pane holds the lift, focus stays on it (only the caller's
OOB ring moves onto the rail pane); when no live main pane is lifted,
the rail pane becomes the persisted focus itself — a focused rail pane
visible in the rail satisfies I1 (no forced main pane). Main-region
targets behave exactly as before (take the focus).")

;; ---- 2d2. RAIL MODE (back-migrated, 1a ledger) ----
;; One #fs-railmode sentinel over data-rmode windows/code/prims/ui (the
;; SAME :has hiding trick desktops use). rm-* classes ride fs-sh-axis;
;; a mode flip is the host's POST /filmstrip/railmode handler OOBing the
;; sentinel through filmstrip/railmode-sentinel.
(define filmstrip/railmode
  (lambda
    ()
    (let
      ((m (as-str (dict-get (filmstrip/ws-fields) "railmode"))))
      (if (string-eq m "") "windows" m)))
  "Rail mode read (ws fields; empty = the first root, never an error).")

(define filmstrip/railmode-sentinel
  (lambda
    (m ?oob)
    (html/tag
      "div"
      (html/attrs
        (append
          (list
            (html/attr "id" "fs-railmode")
            (html/attr "class" (string-append "fs-sh-axis rm-" m)))
          (if
            ?oob
            (list (html/attr "hx-swap-oob" "outerHTML:#fs-railmode"))
            (list))))
      ""))
  "Inert rail-mode sentinel (like the axis/desk ones); CSS
:has(#fs-railmode.rm-<id>) hides every other tree box.")

(define filmstrip/railtabs
  (lambda ()
    (list/map
      (lambda (r)
        (lookbook/html (ui/railtab (as-str (car r)))))
      (filmstrip/tree-roots)))
  "The rail mode tabs: one word per tree root (mode names are CONTENT
labels, not verbs -- they stay words, like the cascade's app names).
POST /filmstrip/railmode flips the #fs-railmode sentinel -- the SAME
:has hiding desktops use, so mode flips keep every tree's expansion
DOM alive.")

(define filmstrip/railtabs-html
  (lambda ()
    (html/tag
      "div"
      (html/attrs
        (list
          (html/attr "id" "fs-railtabs")
          (html/attr "class" "fs-cbox fs-railtabs")
          (html/attr "data-cbox" "railtabs")))
      (html/join
        (append
          (list (filmstrip/start-menu))
          (list (html/span (html/attr "class" "fs-title") (html/text "rail")))
          (filmstrip/railtabs)
          (list (filmstrip/keys-hint))))))
  "The rail mode controlbox (one word per tree root, per
filmstrip.borg sidebar-main-dichotomy -- the axis toggle word was
retired, BORGE-1789879934: sidebar-only shell) + the keyboard-hint
glyph (the shortcut map is the glyph's title tooltip).")

(define filmstrip/register-keys!
  (__macro__
    (rows)
    (quasiquote
      (seq
        (define
          filmstrip/key-rows
          (append filmstrip/key-rows (unquote rows)))))))

(define filmstrip/register-store!
  (__macro__
    (read write)
    (quasiquote
      (seq
        (define filmstrip/store-read (unquote read))
        (define filmstrip/store-write (unquote write))))))

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

(define filmstrip/route?
  (lambda (s) (string-starts-with? (as-str s) "/"))
  "True when a cascade target is a route (leaf) rather than a crumb.")

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
    (let*
      ;; CARRY-FORWARD: save-all REBUILDS the dict from "{}" -- whatever
      ;; is not listed here is dropped from the row. The desktops /
      ;; rail-mode fields therefore ride every write: railmode/boxrows/
      ;; axis read-modify-write (same discipline the app copy used) and
      ;; desktop rides the CURRENT shell desktop (0 when unset; the app
      ;; copy wrote it as glochid/deskt before the back-migration).
      ;; (Not moved, recorded: the copy's axis write was the app pin
      ;; glochid/axis "col" -- BORGE-1789879934; the render-side pin
      ;; still governs the shell, so the stored axis field only carries
      ;; forward here instead of being re-asserted to "col".)
      ((ws (filmstrip/ws-fields))
        (rm (dict-get ws "railmode"))
        (br (dict-get ws "boxrows"))
        (ax (dict-get ws "axis")))
      (filmstrip/ws-save
        (dict-set*
          "{}"
          (list
            "cascade"
            (json-emit (list/map (lambda (c) c) cascade))
            "content"
            (filmstrip/panes-emit panes)
            "focus"
            (string-append "content:" (as-str focus))
            "axis"
            (if (null? ax) "" (as-str ax))
            "desktop"
            (as-str (filmstrip/ws-desktop))
            "railmode"
            (if (null? rm) "" (as-str rm))
            "boxrows"
            (if (null? br) "" (as-str br)))))))
  "Write all workspace fields (cascade/content/focus + carried
axis/desktop/railmode/boxrows) in one row upsert. save-all REBUILDS
the dict -- every field added to the ws row must be carried HERE
through every write path (phase 2: the app copy's redefinition
retires; this framework body is the one residence).")

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

(define filmstrip/shelf-oob
  (lambda () "")
  "OOB payload re-rendering the shelf controlbox after\nclose/restore/destroy. Default empty -- the plain habitat shell has\nno shelf rail yet; glochid redefines it to swap its #fs-shelf rows.")

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
              (filmstrip/content-area panes focus)
              (filmstrip/railmode-sentinel (filmstrip/railmode) #f)
              (filmstrip/key-sentinels)
              (html/tag
                "div"
                (html/attrs
                  (list
                    (html/attr "id" "fs-sink")
                    (html/attr "class" "fs-sink")))
                "")
              (html/span
                (html/attrs
                  (list
                    (html/attr "id" "fs-launch-msg")
                    (html/attr "class" "fs-msg")
                    (html/attr "hidden" "hidden")))
                "")
              (filmstrip/axis-sentinel "col" #f)
              (filmstrip/desk-sentinel (filmstrip/ws-desktop) #f)))))))
  "The whole shell (v5, sidebar-only -- BORGE-1789879934): style +
surface area (main strip + rail box: docked panes, railtabs, start
menu, trees), sentinels, the OOB sink + a status-line anchor. The
cascade column and the launcher are retired (the start menu rides the
railtabs row); the axis is pinned col; the cascade arg is kept for the
route signature. Zero scripts (CSS :has flips the layout from the axis
sentinel's class and hides off-desktop panes from the desk one).")

(define filmstrip/single
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
          ((p2 (filmstrip/pane-set-single (nth panes i)
                 (if (filmstrip/pane-single (nth panes i)) #f #t)))
            (panes2 (filmstrip/replace-at panes i p2)))
          (seq
            (filmstrip/save-all cascade panes2 focus)
            (render-html
              (html/join
                (list
                  (filmstrip/oob-msg "")
                  (filmstrip/oob-section p2 (= i focus))))))))))
  "POST /filmstrip/single: id param -> v5 ONE-UP: toggle the pane's
single flag. Staged single, the pane rides the strip alone (full
width, neighbors hidden by CSS while a focused single exists); off,
it rejoins the strip at its preset width. Same grammar as width: OOB
section swap, pane body preserved. The flag persists in the workspace
row (niri singling as declared state).")

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

(define filmstrip/start-menu
  (lambda ()
    (html/tag
      "details"
      (html/attrs
        (list
          (html/attr "id" "fs-start")
          (html/attr "class" "fs-start")))
      (html/join
        (list
          (html/tag
            "summary"
            (html/attrs
              (list
                (html/attr "class" "fs-btn fs-start-word")
                (html/attr "title" "start: open one of the surfaces")
                (html/attr "aria-label" "start menu")))
            (html/text "start"))
          (html/tag
            "div"
            (html/attrs
              (list (html/attr "class" "fs-start-menu")))
            (html/join
              (list/map
                (lambda (app)
                  (lookbook/html
                    (ui/start-entry (car app) (car (cdr app)))))
                  (filmstrip/apps))))))))
  "The start menu lives on the rail (B: replaced the cascade/launchbar
column, 2026-09-20 operator decision). A zero-JS <details> fold; the
cascade column is gone from the shell; /filmstrip/cascade stays routed
for the lib contract, filmstrip/apps stays HOST SEAM 2.")

(define filmstrip/store-backend
  (lambda
    ()
    (list filmstrip/store-read filmstrip/store-write))
  "GET the knob: the currently registered (read write) storage pair.
The names late-bind and register-store! rebinds them at host boot;
every ws verb routes through the pair.")

;; filmstrip/store-backend: the storage knob -- the ONE store seam
;; (seam-map section 3). GET = (filmstrip/store-backend); SET = the
;; filmstrip/register-store! macro. Default read/write = the habitat
;; floor (tree prims under the user's stable node id), so the habitat
;; works byte-for-byte unchanged; a host app registers its own pair
;; once at boot through the setter (glochid: the local fs pair) and
;; the three ws verbs route through it -- no verb body ever copied.

(define filmstrip/store-read
  (lambda
    ()
    (tree/get (node-path filmstrip/ws-type (whoami))))
  "Store-backend default read (habitat floor): this user's workspace
node, or () when never saved. Swappable per boot through
filmstrip/register-store! -- never by copying a verb body.")

(define filmstrip/store-write
  (lambda
    (fields)
    (seq
      (tree/upsert
        (node-path filmstrip/ws-type (whoami))
        "datum"
        filmstrip/ws-type
        fields)
      fields))
  "Store-backend default write (habitat floor): one workspace row
upsert under the user's stable id; returns fields. Swappable through
filmstrip/register-store!.")

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

(define filmstrip/tree
  (lambda ()
    (let*
      ((id (as-str (request-param "root")))
        (node (filmstrip/url-dec (as-str (request-param "node"))))
        (root (filmstrip/tree-find-root (filmstrip/tree-roots) id)))
      (if (null? root)
          (render-html (filmstrip/oob-msg (string-append "filmstrip: no tree root " id)))
          (render-html
            (filmstrip/tree-level id ((car (cdr (cdr root))) node))))))
  "GET /filmstrip/tree?root=<id>&node=<path>: ONE level of the named\nroot, as the li splice payload (native 200 fragment; a missing root\nanswers the status line only).")

(define filmstrip/tree-box
  (lambda (root)
    (let*
      ((id (as-str (car root)))
        (title (as-str (car (cdr root))))
        (levelfn (car (cdr (cdr root)))))
      (html/tag
        "div"
        (html/attrs
          (list
            (html/attr "id" (string-append "fs-tree-" id))
            (html/attr "class" "fs-cbox fs-rmode")
            (html/attr "data-rmode" id)))
        (html/join
          (list
            (html/span (html/attr "class" "fs-title") (html/text title))
            (html/tag
              "button"
              (html/attrs
                (list
                  (html/attr "class" "fs-btn fs-act")
                  (html/attr "title" "fold everything back (one POST; state is DOM)")
                  (html/attr "aria-label" "collapse all")
                  (html/attr "data-verb" "collapse")
                  (html/attr "hx-post" "/filmstrip/tree/collapse")
                  (html/attr "hx-vals" (string-append "{\"root\":\"" id "\"}"))
                  (html/attr "hx-target" "#fs-sink")
                  (html/attr "hx-swap" "innerHTML")))
              (html/text (filmstrip/act-glyph "collapse")))
            (html/tag
              "ul"
              (html/attrs
                (list
                  (html/attr "id" (string-append "fs-tree-" id "-root"))
                  (html/attr "class" "fs-tr-kids fs-tr-root")))
              (filmstrip/tree-level id (filmstrip/tree-lazy (levelfn "")))))))))
  "One rail tree controlbox: title + collapse-all word + level 0. All
geometry from classes; the level-0 UL id (fs-tree-<id>-root) is the
mutation OOB seam -- the head never changes, so only rows re-render.")

(define filmstrip/tree-boxes
  (lambda ()
    (let*
      ((active (filmstrip/railmode))
        (visited (filmstrip/boxrows)))
      (list/map
        (lambda (r)
          (let ((id (as-str (car r))))
            (if
              (if (string-eq id active) #t
                  (filmstrip/boxrows-has id visited))
              (filmstrip/tree-box r)
              (filmstrip/tree-stub r))))
        (filmstrip/tree-roots))))
  "Lazy-first-reveal (BORGE-1789950734): only the ACTIVE railmode's box
(+ the MRU-2 recently visited ones) materializes rows on a doc render;
the remaining hidden boxes answer empty chrome. The sentinel :has
hiding is unchanged -- the mode flip OOB fills stubs in place.")

(define filmstrip/tree-closed
  (lambda (nodes)
    (list/map
      (lambda (n)
        (dict-set (dict-set n "kids" "") "expand" ""))
      nodes))
  "The FULLY-CLOSED projection (collapse-all only): kids emptied AND
expand cleared, so the pressure valve folds the default-open dirs too
-- the one sanctioned way the server touches expansion state (v4).
Everything else in the shell keeps tree-lazy semantics: expand nodes
arrive open with their eager kids.")

(define filmstrip/tree-collapse
  (lambda
    ()
    (let*
      ((id (as-str (request-param "root")))
        (root
          (filmstrip/tree-find-root (filmstrip/tree-roots) id)))
      (if
        (null? root)
        (render-html
          (filmstrip/oob-msg
            (string-append "filmstrip: no tree root " id)))
        (render-html
          (html/join
            (list (filmstrip/oob-msg "") (filmstrip/tree-oob root)))))))
  "POST /filmstrip/tree/collapse: the DOM pressure valve -- one POST\nOOB-swaps the box back to head + level 0 (the ONLY sanctioned way the\nserver touches expansion state).")

(define filmstrip/tree-find-root
  (lambda (roots id)
    (if (null? roots)
        (list)
        (if (string-eq (as-str (car (car roots))) id)
            (car roots)
            (filmstrip/tree-find-root (cdr roots) id))))
  "The root row (id title levelfn) matching id, or the empty list.")

(define filmstrip/tree-lazy
  (lambda
    (nodes)
    (list/map (lambda (n) (dict-set n "kids" "")) nodes))
  "Boot-lazy projection of a level: eager kids are EMPTIED so the head\nrender stays level-0-only (the request budget is a whole shared shell\n-- an eager head over the fs made the boot shell bust the 400k step\nceiling). The DOM rule still holds at boot: folding pulls the level\nfresh via toggle-once, and the SPLICE renders whatever eager kids the\nhost chose (a GET answers a small subtree in one response). Pane faces\nrender the level directly and honor kids -- one template, two\nmaterialization points. Node dicts ride JSON strings (v4 make-note:\ndict-set* refuses non-strings), so dict-set applies cleanly.")

(define filmstrip/tree-level
  (lambda (root-id nodes)
    (html/join (list/map (lambda (n) (filmstrip/tree-node-li root-id n)) nodes)))
  "One LEVEL of a tree as a joined li fragment (the splice payload).")

(define filmstrip/tree-node-li
  (lambda
    (root-id n)
    (let*
      ((label (as-str (dict-get n "label")))
        (kind (as-str (dict-get n "kind")))
        (path (as-str (dict-get n "node")))
        (open
          (let
            ((o (dict-get n "open")))
            (if (null? o) "" (as-str o))))
        (verbs
          (let
            ((raw (dict-get n "verbs")))
            (if
              (null? raw)
              (list)
              (let
                ((s (as-str raw)))
                (if (string-eq s "") (list) (json-parse s))))))
        (is-dir (string-eq kind "dir"))
        (kids
          (let
            ((k (dict-get n "kids")))
            (if (null? k) "" (as-str k))))
        (eager (and is-dir (not (string-eq kids ""))))
        (label-word
          (if
            (string-eq open "")
            (html/span
              (html/attr "class" "fs-tr-open")
              (html/text label))
            (html/tag
              "button"
              (html/attrs
                (list
                  (html/attr "class" "fs-btn fs-applink fs-tr-open")
                  (html/attr
                    "title"
                    (string-append "open-or-focus " label))
                  (html/attr "hx-post" "/filmstrip/open")
                  (html/attr
                    "hx-vals"
                    (string-append "{\"route\":\"" open "\"}"))
                  (html/attr "hx-target" "#fs-sink")
                  (html/attr "hx-swap" "innerHTML")))
              (html/text label))))
        (verb-text
          (lambda (v) (filmstrip/verb-hint (as-str (car v)))))
        (verb-glyph
          (lambda
            (v)
            (let
              ((g
                (if
                  (< (count v) 4)
                  ""
                  (as-str (car (cdr (cdr (cdr v))))))))
              (if (string-eq g "") (as-str (car v)) g))))
        (verb-glyph-or
          (lambda
            (v)
            (let
              ((g (verb-glyph v)))
              (if
                (string-eq g (as-str (car v)))
                (filmstrip/act-glyph (as-str (car v)))
                g))))
        (verb-btn
          (lambda
            (v ?act)
            (html/tag
              "button"
              (html/attrs
                (list
                  (html/attr
                    "class"
                    (if ?act "fs-btn fs-act" "fs-btn fs-tr-verb"))
                  (html/attr "title" (verb-text v))
                  (html/attr "aria-label" (as-str (car v)))
                  (html/attr "data-verb" (as-str (car v)))
                  (html/attr "hx-post" (as-str (car (cdr v))))
                  (html/attr "hx-vals" (as-str (car (cdr (cdr v)))))
                  (html/attr "hx-target" "#fs-sink")
                  (html/attr "hx-swap" "innerHTML")))
              (html/text (verb-glyph-or v)))))
        (verb-word (lambda (v) (verb-btn v #f)))
        (verb-act (lambda (v) (verb-btn v #t)))
        (action-li
          (lambda
            (v)
            (html/tag
              "li"
              (html/attr "class" "fs-tr-item fs-tr-actrow")
              (verb-act v))))
        (actions-fold
          (if
            (null? verbs)
            ""
            (html/tag
              "ul"
              (html/attr "class" "fs-tr-acts-ul")
              (html/join (list/map action-li verbs)))))
        (details-attrs
          (append
            (list (html/attr "class" "fs-tr-node"))
            (if
              (or (not is-dir) eager)
              (list)
              (list
                (html/attr
                  "hx-get"
                  (string-append
                    "/filmstrip/tree?root="
                    root-id
                    "&node="
                    (filmstrip/url-enc path)))
                (html/attr "hx-trigger" "toggle once")
                (html/attr "hx-target" "find .fs-tr-kids")
                (html/attr "hx-swap" "innerHTML")))))
        (row-els
          (list
            (html/span
              (html/attr "class" "fs-tr-fold")
              (html/text "▸"))
            label-word
            (html/join (list/map verb-word verbs)))))
      (html/tag
        "li"
        (html/attrs
          (list
            (html/attr "class" "fs-tr-item")
            (html/attr "data-node" path)))
        (html/tag
          "details"
          (html/attrs details-attrs)
          (html/join
            (append
              (list
                (html/tag
                  "summary"
                  (html/attr "class" "fs-tr-row")
                  (html/join row-els)))
              (if
                (not is-dir)
                (list)
                (list
                  (html/tag
                    "ul"
                    (html/attr "class" "fs-tr-kids")
                    (if
                      eager
                      (filmstrip/tree-level root-id (json-parse kids))
                      ""))))
              (list actions-fold)))))))
  "One node row: li[data-node] > details > summary(fold arrow + label\n+ verb GLYPHS) [+ kids ul for dirs: EAGER (pre-walked level rendered\ninline, no pull attrs) when the node carries a non-empty kids JSON\nstring, else the lazy toggle-once level GET] [+ a narrow-only actions\nul when the node carries verbs -- the SAME glyphs as children, never\na second summary or a duplicate list]. Leaf details are fold-only.\nZero JS beyond the htmx attrs.")

(define filmstrip/tree-oob
  (lambda (root)
    (let*
      ((id (as-str (car root)))
        (levelfn (car (cdr (cdr root)))))
      (html/tag
        "ul"
        (html/attrs
          (list
            (html/attr "id" (string-append "fs-tree-" id "-root"))
            (html/attr "class" "fs-tr-kids fs-tr-root")
            (html/attr "hx-swap-oob" "innerHTML")))
        (filmstrip/tree-level id (filmstrip/tree-lazy (levelfn ""))))))
  "OOB (hx-swap-oob innerHTML on the level-0 UL) re-render of one
tree's ROWS -- the mutation refresh seam. The box head never changes,
so the div itself is never swapped (swapping it made htmx drop
listeners for subsequently spliced levels); expansion state above the
mutated rows now survives mutations, which is the truer v4 story.")

;; ---- 5. RAIL TREES (v4: ONE tree renderer, N roots) ----
;; borge/filmstrip.borg rail-trees-v4: a tree is a GEOMETRY RULE over
;; controlboxes, not a third primitive. A node row is an acme tag-line:
;; leading fold word (its own hitbox), LABEL word (open-or-focus via
;; POST /filmstrip/open), trailing VERB words (mutation POSTs). The
;; host supplies EVERYTHING tree-shaped: filmstrip/tree-roots is a list
;; of (id title levelfn) where levelfn: path -> list of node dicts
;; (label kind node open verbs kids). The lib renders ONE LEVEL per
;; response, EXCEPT where the host pre-walked a small subtree (the
;; optional kids field renders inline with no pull attrs -- the
;; threshold is host policy, never lib state); expansion state is
;; NATIVE <details> DOM state -- splice never swap; the server never
;; knows which nodes are open.
(define filmstrip/tree-roots
  (lambda () (list))
  "Host seam: list of (id title levelfn). A levelfn takes a node path
string (\"\" = the root level) and answers a list of node dicts:
(label kind node open verbs kids) -- kind dir|leaf, node the lazy-kids
key, open a route string or empty (label verb, open-or-focus), verbs a
list of (word route vals-json) mutation words, kids an OPTIONAL JSON
string of a pre-walked child level (eager dir: fold pulls nothing),
expand the OPTIONAL default-open seed (\"true\" renders the <details>
open -- pair it WITH kids: a parsed-open dir never fires its
\"toggle once\" pull).
Hosts redefine; default empty (a rootless shell renders no rail trees).")

(define filmstrip/tree-stub
  (lambda (root)
    (let*
      ((id (as-str (car root)))
        (title (as-str (car (cdr root)))))
      (html/tag
        "div"
        (html/attrs
          (list
            (html/attr "id" (string-append "fs-tree-" id))
            (html/attr "class" "fs-cbox fs-rmode")
            (html/attr "data-rmode" id)))
        (html/join
          (list
            (html/span (html/attr "class" "fs-title") (html/text title))
            (html/tag
              "ul"
              (html/attrs
                (list
                  (html/attr "id" (string-append "fs-tree-" id "-root"))
                  (html/attr "class" "fs-tr-kids fs-tr-root")))
              ""))))))
  "Lazy-first-reveal stub (BORGE-1789950734): the rail box chrome with
an EMPTY level-0 UL -- no rows materialized while the box is hidden.
The UL id is the mutation OOB seam, so a first-visit flip fills it in
place.")

(define filmstrip/url-dec
  (lambda (s)
    (filmstrip/url-dec-go s 0 ""))
  "Inverse of filmstrip/url-enc for the SAME six escapees (unknown %xx
passes through verbatim as %xx -- never throw inside a renderer).")

(define filmstrip/url-dec-ch
  (lambda (hex)
    (if (string-eq hex "20") " "
    (if (string-eq hex "25") "%"
    (if (string-eq hex "3F") "?"
    (if (string-eq hex "26") "&"
    (if (string-eq hex "23") "#"
    (if (string-eq hex "2B") "+"
    (string-append "%" hex))))))))
  "One filmstrip/url-dec escapee, or verbatim %xx passthrough.")

(define filmstrip/url-dec-go
  (lambda (s i acc)
    (if (>= i (string-length s))
        acc
        (if (string-eq (substring s i 1) "%")
            (if (< (+ i 3) (string-length s))
                (filmstrip/url-dec-go s (+ i 3)
                  (string-append acc
                    (filmstrip/url-dec-ch (substring s (+ i 1) 2))))
                (filmstrip/url-dec-go s (+ i 1)
                  (string-append acc (substring s i 1))))
            (filmstrip/url-dec-go s (+ i 1)
              (string-append acc (substring s i 1))))))
  "filmstrip/url-dec's accumulator walk (named define, not letrec).")

(define filmstrip/url-enc
  (lambda (s)
    (if (or (string-contains? s "%")
            (string-contains? s " ")
            (string-contains? s "?")
            (string-contains? s "&")
            (string-contains? s "#")
            (string-contains? s "+"))
        (filmstrip/url-enc-go s 0 "")
        s))
  "Minimal percent-encoding for a tree node path riding a query param
(same six escapees filmstrip/url-dec inverts; everything else passes).
FAST PATH: a per-char Nopales walk costs ~10 steps/char, which the
eager trees multiplied into whole budget levels; the OCaml
string-contains? probes settle the no-escapee case in one prim and
the walk only runs when an escapee is present.")

(define filmstrip/url-enc-ch
  (lambda (c)
    (if (string-eq c "%") "%25"
    (if (string-eq c " ") "%20"
    (if (string-eq c "?") "%3F"
    (if (string-eq c "&") "%26"
    (if (string-eq c "#") "%23"
    (if (string-eq c "+") "%2B" c)))))))
  "One char of filmstrip/url-enc: the six escapees, else identity.")

(define filmstrip/url-enc-go
  (lambda (s i acc)
    (if (>= i (string-length s))
        acc
        (filmstrip/url-enc-go s (+ i 1)
          (string-append acc
            (filmstrip/url-enc-ch (substring s i 1))))))
  "filmstrip/url-enc's accumulator walk (named define, not letrec).")

(define filmstrip/verb-hint
  (lambda
    (w)
    (if
      (string-eq w "focus")
      "focus this pane"
      (if
        (string-eq w "pin")
        "pin to the rail (dock; body stays live)"
        (if
          (string-eq w "unpin")
          "unpin (back to the main strip)"
          (if
            (string-eq w "width")
            "width: cycle third / half / full"
            (if
              (string-eq w "close")
              "close: shelve the pane (restore it from the shelf)"
              (if
                (string-eq w "restore")
                "restore from the shelf"
                (if
                  (string-eq w "destroy")
                  "destroy the pane for good"
                  (if
                    (string-eq w "switch")
                    "switch this desktop"
                    (if (string-eq w "collapse") "fold everything back" w))))))))))
  "The TOOLTIP text for a verb word (title attr). A glyph is not\nself-explanatory -- every action button carries its words here and its\nword in aria-label/data-verb, so the glyph is purely visual chrome.")

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

(define filmstrip/winbox-oob
  (lambda () "")
  "OOB payload re-rendering the windows controlbox after\nclose/restore/destroy (a pane leaving or rejoining the live set\nchanges its rows). Default empty -- the plain habitat shell has no\nwindows rail yet; glochid redefines it to swap its #fs-winbox rows.")

(define filmstrip/windows-pane-node
  (lambda
    (p shelved?)
    (let*
      ((id (filmstrip/pane-id p))
        (route (filmstrip/pane-route p))
        (v-verbs
          (lambda
            (pairs)
            (list/map
              (lambda
                (pr)
                (filmstrip/pane-verb (car pr) (car (cdr pr))
                  (list (list "id" id))))
              pairs)))
        (live-verbs
          (v-verbs
            (list
              (list "focus" "/filmstrip/focus-id")
              (if
                (string-eq (filmstrip/pane-region p) "rail")
                (list "unpin" "/filmstrip/pin")
                (list "pin" "/filmstrip/pin"))
              (list "width" "/filmstrip/width")
              (list "single" "/filmstrip/single")
              (list "close" "/filmstrip/close"))))
        (desk (as-str (filmstrip/pane-desk p)))
        ;; cross-desktop MOVE words: an explicit destination verb per
        ;; desktop EXCEPT the pane's current one (rail panes carry none
        ;; -- dock semantics mean nothing to move). The pane-head move
        ;; glyph cycles; the tree row words are acme-explicit.
        (m-verbs
          (if
            (string-eq (filmstrip/pane-region p) "rail")
            (list)
            (list/map
              (lambda
                (d)
                (filmstrip/pane-verb
                  (string-append "to" d)
                  "/filmstrip/move-to"
                  (list (list "id" id) (list "d" d))))
              (list/filter
                (lambda
                  (d)
                  (not (string-eq d desk)))
                (list "0" "1" "2")))))
        (live-verbs2 (append live-verbs m-verbs))
        (shelved-verbs
          (v-verbs
            (list
              (list "restore" "/filmstrip/restore")
              (list "destroy" "/filmstrip/destroy")))))
      (dict-set*
        "{}"
        (list
          "label" route
          "kind" "leaf"
          "node" (string-append "pane/" id)
          "open" route
          "verbs" (json-emit (if shelved? shelved-verbs live-verbs2))))))
  "One pane row: label = route (open-or-focus, acme Get on a shelved
row), verbs by CURRENT state (live: focus/pin/width/close + to<N>
move words for every OTHER desktop; shelved: restore/destroy). Rail
panes carry no move words (dock semantics -- desktop-global). Declared
state only -- no DOM scraping.")

(define filmstrip/windows-tree
  (lambda
    (path)
    (let*
      ((panes (nth (filmstrip/row) 1))
        (live-on-desk
          (lambda
            (d)
            (filmstrip/keep-nodes
             (filmstrip/map-indexed
                (lambda
                  (p i)
                  (if
                    (if
                      (string-eq (filmstrip/pane-state p) "shelved")
                      #f
                      (string-eq (as-str (filmstrip/pane-desk p)) d))
                    (filmstrip/windows-pane-node p #f)
                    ""))
                panes 0))))
        (desk-dir
          (lambda
            (d)
            ;; DEFAULT-OPEN (rail-trees-v4 expand seed + operator note:
            ;; the desktop tree reads expanded): eager kids (this
            ;; desktop's live panes, pre-walked -- a parsed-open details
            ;; never fires its toggle-once pull, so expand RIDES kids)
            ;; + the switch verb. The windows subtree is tiny, so the
            ;; eager head cost is host-trivial (the budget gate is the
            ;; FS tree, not this one).
            (dict-set*
              "{}"
              (list
                "label" (string-append "desktop " d)
                "kind" "dir"
                "node" (string-append "desk/" d)
                "open" ""
                "verbs"
                (json-emit
                  (list
                    (list "switch" "/filmstrip/desktop"
                      (string-append "{\"d\":\"" d "\"}"))))
                "kids" (filmstrip/json-arr (live-on-desk d))
                "expand" "true"))))
        (shelf-dir
          (dict-set*
            "{}"
            (list
              "label" "shelf"
              "kind" "dir"
              "node" "shelf"
              "open" ""
              "verbs" ""))))
      (if
        (string-eq path "")
        (list (desk-dir "0") (desk-dir "1") (desk-dir "2") shelf-dir)
        (if
          (string-eq path "shelf")
          (filmstrip/keep-nodes
            (filmstrip/map-indexed
              (lambda
                (p i)
                (if
                  (string-eq (filmstrip/pane-state p) "shelved")
                  (filmstrip/windows-pane-node p #t)
                  ""))
              panes 0))
          (live-on-desk (substring path 5 (string-length path)))))))
  "levelfn for the windows tree (node 'desk/<n>' -> that desktop's
panes; 'shelf' -> the shelved panes): DESKTOP-GLOBAL data from the
one workspace row, never from live DOM. Desk dirs 0..2 render
DEFAULT-OPEN: their level answer pre-walks the pane rows as eager
kids + the expand seed (the lazy level pull stays for the shelf).")

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

(define filmstrip/ws-desktop
  (lambda ()
    (let
      ((v (dict-get (filmstrip/ws-fields) "desktop")))
      (if (null? v) 0 (parse-int (as-str v)))))
  "Current desktop from the workspace row's desktop field (0 when
unset — hosts that don't model desktops ride 0). Read through THIS
lib's own ws-fields seam, the same read the habitat shell's deskt
wrapper does; the lib never touches host pane-row fields.")
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

;; filmstrip-ws workspace row type: declared up top because the kernel's
;; typed-boundary rule rejects writes to an undeclared datum type
;; (crud.check_type_fields) and returns that rejection as a plain string
;; VALUE which ws-save silently discards -- an undeclared type made every
;; pane open fail to persist across reload (prod 10-12, UI gate da8e0223:
;; GET /filmstrip rendered fs-content empty after every open).
;; Guarded like todo/comments-init!. Zero fields = the declared
;; schemaless case (crud.ml: [Some []] vs [None] are distinct): the row
;; body is lib-managed JSON that gains keys across versions and is also
;; written by the habitat shell's deskt wrapper, so the type stays
;; free-form. The toplevel call seeds the declaration at boot/reseed
;; (boot-admin context); delegated sessions no-op by design (the
;; denial lands as the value, not an error).

(define filmstrip/ws-init!
  (lambda
      ()
      (if
        (null? (type-fields (quote filmstrip-ws)))
        (deftype (quote filmstrip-ws))
        "filmstrip-ws ok"))
  "Declare the filmstrip-ws workspace row type (guarded, idempotent). A
zero-field type reports (type-fields) as null, so the guard re-fires on
every load/reseed and the re-declare lands as an ON CONFLICT upsert. A
delegated session no-ops by design: the deftype denial lands as this
call's return value, not an error.")

(define filmstrip/ws-node
  (lambda
    ()
    (filmstrip/store-read))
  "This user's workspace node, or () when never saved. Routes through
the store-backend knob's current read (filmstrip/store-read).")

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
    (filmstrip/store-write fields))
  "Persist workspace fields under the user's stable row id. Routes
through the store-backend knob's current write (filmstrip/store-write).")

(define filmstrip/ws-type
  "filmstrip-ws")

(define notes/browse
  (lambda
    (path)
    (filmstrip/browse-walk
      notes/browse-headings
      (list (notes/browse-docs))
      path))
  "notes cascade: docs -> outline headings. Heading entries leaf to\n/notes?slug=<doc> (headings have no route of their own).")

(define notes/browse-docs
  (lambda
    ()
    (list/map
      (lambda (r) (list (nth r 2) (nth r 3)))
      (notes/list-light)))
  "Column of docs: (title . slug).")

(define notes/browse-headings
  (lambda
    (slug)
    (let
      ((n (notes/get slug)))
      (if
        (null? n)
        (list)
        (list/map
          (lambda
            (kid)
            (list
              (as-str (outline/body kid))
              (string-append "/notes?slug=" slug)))
          (outline/kids (notes/get-outline n))))))
  "Top-level outline headings of one doc, each a leaf to the doc view.")

(define todo/browse
  (lambda
    (path)
    (filmstrip/browse-walk
      todo/browse-items
      (list (todo/browse-lists))
      path))
  "todo cascade: lists -> items. Item entries leaf to /todos?id=<item>\n(the only granularity /todos serves; lists drill to the item column).")

(define todo/browse-items
  (lambda
    (list-id)
    (list/map
      (lambda
        (row)
        (let
          ((f (cdr row)))
          (list
            (as-str (dict-get f "title"))
            (string-append "/todos?id=" (as-str (car row))))))
      (list/filter
        (lambda
          (row)
          (string-eq
            (as-str (dict-get (cdr row) "list_id"))
            list-id))
        (todo/item/all))))
  "Items of one list; each entry is a leaf to the item's /todos detail.")

(define todo/browse-lists
  (lambda
    ()
    (list/map
      (lambda
        (row)
        (let
          ((f (cdr row)))
          (list (as-str (dict-get f "title")) (as-str (car row)))))
      (todo/list/all)))
  "Column of todo lists: (title . list-id).")

(define ui/railtab
  (lambda (label)
    (ui/elm
      "button"
      (list
        "class" "fs-btn fs-railtab"
        "title" (string-append "show the " label " tree")
        "hx-post" "/filmstrip/railmode"
        "hx-vals" (string-append "{\"root\":\"" label "\"}")
        "hx-target" "#fs-sink"
        "hx-swap" "innerHTML")
      (list (ui/text label))))
  "One rail mode tab: POSTs /filmstrip/railmode with this root (the
sentinel :has flip -- expansion DOM stays alive). Declared behavior
data: the hx-* verbs ride attrs verbatim through the projection.")

(define ui/start-entry
  (lambda (label route)
    (ui/elm
      "button"
      (list
        "class" "fs-btn fs-start-entry"
        "title" (string-append "open " label)
        "hx-post" "/filmstrip/open"
        "hx-vals" (string-append "{\"route\":\"" route "\"}")
        "hx-target" "#fs-sink"
        "hx-swap" "innerHTML")
      (list (ui/text label))))
  "One start-menu entry: opens `route` as a pane via /filmstrip/open.")
