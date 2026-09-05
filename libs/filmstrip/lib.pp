;; package filmstrip -- the habitat shell (two-strip Miller-cascade desktop).
;; Spec: borge/filmstrip.borg (filmstrip-shell). Handoff:
;; docs/plans/filmstrip-v1.md.
;;
;; Screen model: a cascade strip (short, Miller columns) over a content
;; strip (niri-style row of panes). Panes are ordinary routes dispatched
;; in-process with the HX-Request header on (the hyper/visit seam) -- they
;; don't know they're windowed. Workspace state is a per-user row in the
;; object store (tree datum type filmstrip-ws, stable id = username), so
;; the desktop persists in the same store as the todos it displays.
;;
;; Zero-JS discipline: htmx key filters on shell sentinels POST to
;; /filmstrip/focus and /filmstrip/move with a dir param; the server owns
;; focus and answers with OOB class swaps on exactly the affected pane
;; sections. Pane bodies carry hx-preserve, so class/width swaps never
;; disturb live pane DOM. Launcher input focus is the native accesskey
;; attribute -- no JS file.
;; NOTE: routes are exact-match in the route table, so the plan's
;; /filmstrip/width/<id> shape is served as /filmstrip/<action> routes
;; carrying id/dir/d/t params (same semantics).
;;
;; browse: the cascade protocol lives here as <pkg>/browse definitions
;; (todo/browse, notes/browse, calendar/browse) until promoted into their
;; packages -- one-file make constraint, protocol shape unchanged:
;; (pkg/browse path) -> (columns . leaf?).

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
    filmstrip/native-move))

;; ---------- constants ----------

(define filmstrip/ws-type
  "filmstrip-ws")
  "Tree datum type holding the per-user workspace row."

(define filmstrip/widths
  (list "third" "half" "full"))
  "niri set-column-width presets, in cycle order. Never pixels (D2)."

(define filmstrip/default-width
  "third")
  "Width preset given to freshly opened panes."

;; ---------- small helpers ----------

(define filmstrip/route?
  (lambda (s)
    (string-starts-with? (as-str s) "/"))
  "True when a cascade target is a route (leaf) rather than a crumb.")

(define filmstrip/new-id
  (lambda () (random-uuid))
  "Fresh pane id (UUID -- the todo/notes row id discipline).")

(define filmstrip/split-query
  (lambda (url)
    (let* ((u (as-str url))
           (parts (string-split u "?"))
           (path (car parts))
           (rest (if (null? (cdr parts)) "" (car (cdr parts)))))
      (cons path (filmstrip/parse-pairs (if (string-eq rest "") (list) (string-split rest "&"))))))
  "Split a route string into (path . params) where params is an alist of
DOTTED (k . v) pairs -- the shape route/dispatch's assoc_of accepts (2-element
lists are silently dropped, which would lose the query).")

(define filmstrip/parse-pairs
  (lambda (pairs)
    (if (null? pairs)
        (list)
        (let* ((kv (string-split (car pairs) "="))
               (k (car kv))
               (v (if (null? (cdr kv)) "" (car (cdr kv)))))
          (cons (cons k v) (filmstrip/parse-pairs (cdr pairs))))))
  "Parse kv strings into a dotted-pair alist.")

(define filmstrip/take
  (lambda (xs n)
    (if (or (null? xs) (<= n 0))
        (list)
        (cons (car xs) (filmstrip/take (cdr xs) (- n 1)))))
  "First n elements of a list.")

(define filmstrip/remove-at
  (lambda (xs i)
    (if (null? xs)
        (list)
        (if (<= i 0)
            (cdr xs)
            (cons (car xs) (filmstrip/remove-at (cdr xs) (- i 1))))))
  "Copy of xs without element at index i.")

(define filmstrip/swap-at
  (lambda (xs i)
    (let ((n (length xs)))
      (if (or (< i 0) (>= (+ i 1) n))
          xs
          (filmstrip/swap-pair xs i))))
  "Swap element at index i with its right neighbor (no-op at the end).")

(define filmstrip/swap-pair
  (lambda (xs i)
    (if (<= i 0)
        (cons (car (cdr xs)) (cons (car xs) (cdr (cdr xs))))
        (cons (car xs) (filmstrip/swap-pair (cdr xs) (- i 1)))))
  "Adjacent transposition at index i (assumes a right neighbor exists).")

(define filmstrip/last-col
  (lambda (cols)
    (if (null? cols)
        (list)
        (if (null? (cdr cols))
            (car cols)
            (filmstrip/last-col (cdr cols)))))
  "The deepest column of a column list.")

(define filmstrip/map-indexed
  (lambda (fn xs i)
    (if (null? xs)
        (list)
        (cons (fn (car xs) i) (filmstrip/map-indexed fn (cdr xs) (+ i 1)))))
  "Map fn over xs with a 0-based index.")

(define filmstrip/find-index
  (lambda (pred xs i)
    (if (null? xs)
        -1
        (if (pred (car xs))
            i
            (filmstrip/find-index pred (cdr xs) (+ i 1)))))
  "Index of the first element satisfying pred, else -1.")

;; ---------- workspace row (W1) ----------

(define filmstrip/ws-node
  (lambda ()
    (tree/get (node-path filmstrip/ws-type (whoami))))
  "This user's workspace node, or () when never saved.")

(define filmstrip/ws-default
  (lambda ()
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
  (lambda ()
    (let ((n (filmstrip/ws-node)))
      (if (null? n) (filmstrip/ws-default) (dict-get n "body"))))
  "Workspace row fields (defaults when the row doesn't exist yet).")

(define filmstrip/ws-save
  (lambda (fields)
    (seq
      (tree/upsert
        (node-path filmstrip/ws-type (whoami))
        "datum"
        filmstrip/ws-type
        fields)
      fields))
  "Persist workspace fields under the user's stable row id.")

(define filmstrip/ws-cascade
  (lambda (fields)
    (let ((v (dict-get fields "cascade")))
      (if (null? v)
          (list)
          (let ((p (json-parse (as-str v))))
            (if (null? p) (list) p)))))
  "Crumb path from the row: list of entry strings (labels/ids/routes).")

(define filmstrip/ws-panes
  (lambda (fields)
    (let ((v (dict-get fields "content")))
      (if (null? v)
          (list)
          (let ((ps (json-parse (as-str v))))
            (if (null? ps) (list) ps)))))
  "Pane records from the row: list of JSON object strings {id route width}.")

(define filmstrip/ws-focus
  (lambda (fields)
    (let* ((v (as-str (dict-get fields "focus")))
           (n (string-length v)))
      (if (< n 9)
          -1
          (parse-int (substring v 8 n)))))
  "Focused content index from the row's focus value (content:N); -1 = none.")

(define filmstrip/panes-emit
  (lambda (panes)
    (json-emit (list/map (lambda (p) p) panes)))
  "Emit pane records back to the row's content JSON array.")

(define filmstrip/save-all
  (lambda (cascade panes focus)
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

(define filmstrip/pane-id
  (lambda (p) (as-str (dict-get p "id")))
  "Pane record id.")

(define filmstrip/pane-route
  (lambda (p) (as-str (dict-get p "route")))
  "Pane record route.")

(define filmstrip/pane-width
  (lambda (p)
    (let ((w (as-str (dict-get p "width"))))
      (if (string-eq w "") filmstrip/default-width w)))
  "Pane record width preset (default third).")

(define filmstrip/pane-set-width
  (lambda (p w)
    (dict-set p "width" w))
  "Pane record with a new width preset.")

(define filmstrip/find-pane-index
  (lambda (panes id)
    (filmstrip/find-index
      (lambda (p) (string-eq (filmstrip/pane-id p) id))
      panes
      0))
  "Index of the pane with id, else -1.")

(define filmstrip/pane-at
  (lambda (panes i)
    (if (or (< i 0) (>= i (length panes)))
        (list)
        (nth panes i)))
  "Pane record at index, or () out of range.")

;; ---------- pane rendering (W2/W3) ----------

(define filmstrip/pane-head
  (lambda (p)
    "Pane title bar: route label + width cycle + close, all htmx POSTs."
    (let ((id (filmstrip/pane-id p))
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
                  (html/attr "hx-vals" (string-append "{\"id\":\"" id "\"}"))
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
                  (html/attr "hx-vals" (string-append "{\"id\":\"" id "\"}"))
                  (html/attr "hx-target" "#fs-sink")
                  (html/attr "hx-swap" "innerHTML")))
              "close")))))))

(define filmstrip/pane-content
  (lambda (route)
    (let* ((parsed (filmstrip/split-query route))
           (path (car parsed))
           (handler (route/lookup path)))
      (if (null? handler)
          (html/p*
            (html/attr "class" "muted")
            (string-append "filmstrip: no route for " path))
          (route/dispatch
            handler
            "GET"
            path
            (cdr parsed)
            (list (cons "hx-request" "true"))))))
  "In-process partial dispatch of a pane route (HX-Request header on --
the route gate's existing fragment path, the hyper/visit seam).")

(define filmstrip/pane-html
  (lambda (p focused? content)
    (let ((id (filmstrip/pane-id p)))
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
  "One pane section. content is the dispatched fragment (full page) or an
empty hx-preserve placeholder (OOB class/width swaps keep the live DOM).")

(define filmstrip/oob-section
  (lambda (p focused?)
    (let ((id (filmstrip/pane-id p)))
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
  "OOB outerHTML swap carrying only shell state: width + .focused classes.
The body div is an hx-preserve placeholder -- htmx re-inserts the live
pane DOM, so focus/width changes never clobber in-pane state.")

(define filmstrip/oob-append-pane
  (lambda (p focused?)
    (html/tag
      "div"
      (html/attr "hx-swap-oob" "beforeend:#fs-content")
      (filmstrip/pane-html p focused? (filmstrip/pane-content (filmstrip/pane-route p)))))
  "OOB beforeend into the content strip: a freshly opened pane with its
real dispatched content (no preserve -- nothing to preserve yet).")

(define filmstrip/oob-delete-pane
  (lambda (id)
    (html/tag
      "div"
      (html/attrs
        (list
          (html/attr "hx-swap-oob" (string-append "delete:#fs-pane-" id))
          (html/attr "class" "fs-oob-delete")))
      ""))
  "OOB delete of a closed pane's section.")

(define filmstrip/oob-content-strip
  (lambda (panes focus)
    (html/tag
      "div"
      (html/attrs
        (list
          (html/attr "id" "fs-content")
          (html/attr "class" "fs-content")
          (html/attr "hx-swap-oob" "true")))
      (html/join (filmstrip/panes-html panes focus))))
  "OOB re-render of the whole content strip (pane order changes; every
body is an hx-preserve placeholder, so live pane DOM survives).")

(define filmstrip/oob-msg
  (lambda (s)
    (html/span
      (html/attrs
        (list
          (html/attr "id" "fs-launch-msg")
          (html/attr "class" "fs-msg")
          (html/attr "hx-swap-oob" "true")))
      (html/text s)))
  "OOB status line under the launcher.")

;; ---------- browse protocol (W4) ----------

(define filmstrip/apps
  (lambda ()
    (list
      (list "todos" "todo")
      (list "notes" "notes")
      (list "calendar" "calendar")))
  "Column 0: the app chooser. Entries are (label . target); target is the
package key that names the browse below.")

(define filmstrip/col-leaf?
  (lambda (col)
    (if (null? col)
        #f
        (if (filmstrip/route? (car (cdr (car col))))
            #t
            (filmstrip/col-leaf? (cdr col)))))
  "A column is a leaf column when any entry targets a route.")

(define filmstrip/browse-walk
  (lambda (childfn cols path)
    (if (null? path)
        (cons cols (filmstrip/col-leaf? (filmstrip/last-col cols)))
        (if (filmstrip/route? (car path))
            (cons cols (filmstrip/col-leaf? (filmstrip/last-col cols)))
            (filmstrip/browse-walk
              childfn
              (append cols (list (childfn (as-str (car path)))))
              (cdr path)))))
  "Shared Miller walk: column i lists the children of path[i-1]. Entries
whose target is a route are leaves; the walk stops there. Returns
(columns . leaf?).")

(define todo/browse
  (lambda (path)
    (filmstrip/browse-walk
      todo/browse-items
      (list (todo/browse-lists))
      path))
  "todo cascade: lists -> items. Item entries leaf to /todos?id=<item>
(the only granularity /todos serves; lists drill to the item column).")

(define todo/browse-lists
  (lambda ()
    (list/map
      (lambda (row)
        (let ((f (cdr row)))
          (list (as-str (dict-get f "title")) (as-str (car row)))))
      (todo/list/all)))
  "Column of todo lists: (title . list-id).")

(define todo/browse-items
  (lambda (list-id)
    (list/map
      (lambda (row)
        (let ((f (cdr row)))
          (list
            (as-str (dict-get f "title"))
            (string-append "/todos?id=" (as-str (car row))))))
      (list/filter
        (lambda (row)
          (string-eq (as-str (dict-get (cdr row) "list_id")) list-id))
        (todo/item/all))))
  "Items of one list; each entry is a leaf to the item's /todos detail.")

(define notes/browse
  (lambda (path)
    (filmstrip/browse-walk
      notes/browse-headings
      (list (notes/browse-docs))
      path))
  "notes cascade: docs -> outline headings. Heading entries leaf to
/notes?slug=<doc> (headings have no route of their own).")

(define notes/browse-docs
  (lambda ()
    (list/map
      (lambda (r)
        (list (nth r 2) (nth r 3)))
      (notes/list-light)))
  "Column of docs: (title . slug).")

(define notes/browse-headings
  (lambda (slug)
    (let ((n (notes/get slug)))
      (if (null? n)
          (list)
          (list/map
            (lambda (kid)
              (list
                (as-str (outline/body kid))
                (string-append "/notes?slug=" slug)))
            (outline/kids (notes/get-outline n))))))
  "Top-level outline headings of one doc, each a leaf to the doc view.")

(define calendar/browse
  (lambda (path)
    (filmstrip/browse-walk
      calendar/browse-children
      (list (calendar/browse-years))
      path))
  "calendar cascade: year -> month -> day. Day entries leaf to
/calendar?day=YYYY-MM-DD (the day agenda route).")

(define calendar/browse-years
  (lambda ()
    (let ((today (calendar/today)))
      (let ((y (substring today 0 4)))
        (let ((ev-years (calendar/browse-event-years)))
          (calendar/browse-numsort
            (if (calendar/browse-contains ev-years y)
                ev-years
                (calendar/browse-insert-sorted y ev-years)))))))
  "Years with events, plus the current year; sorted ascending.")

(define calendar/browse-event-years
  (lambda ()
    (calendar/browse-year-acc (calendar/all-events) (list)))
  "Distinct years across stored events.")

(define calendar/browse-year-acc
  (lambda (rows acc)
    (if (null? rows)
        acc
        (let ((d (as-str (dict-get (cdr (car rows)) "day"))))
          (if (< (string-length d) 4)
              (calendar/browse-year-acc (cdr rows) acc)
              (let ((y (substring d 0 4)))
                (calendar/browse-year-acc
                  (cdr rows)
                  (if (calendar/browse-contains acc y) acc (calendar/browse-insert-sorted y acc))))))))
  "Accumulate distinct YYYY prefixes from event rows.")

(define calendar/browse-contains
  (lambda (xs s)
    (if (null? xs)
        #f
        (if (string-eq (as-str (car xs)) s) #t (calendar/browse-contains (cdr xs) s))))
  "Membership by string equality.")

(define calendar/browse-insert-sorted
  (lambda (s xs)
    (if (null? xs)
        (list s)
        (if (string<? s (as-str (car xs)))
            (cons s xs)
            (cons (car xs) (calendar/browse-insert-sorted s (cdr xs))))))
  "Insert into an ascending string list.")

(define calendar/browse-numsort
  (lambda (xs) xs)
  "Lists are built ascending already; identity for clarity.")

(define calendar/browse-children
  (lambda (entry)
    (let ((e (as-str entry)))
      (if (= (string-length e) 4)
          (calendar/browse-months e)
          (if (= (string-length e) 7)
              (calendar/browse-days e)
              (list)))))
  "Children of a calendar crumb: year -> months, YYYY-MM -> days.")

(define calendar/browse-months
  (lambda (y)
    (calendar/browse-months-acc y 1 (list)))
  "Twelve YYYY-MM entries for a year.")

(define calendar/browse-months-acc
  (lambda (y m acc)
    (if (> m 12)
        acc
        (calendar/browse-months-acc
          y
          (+ m 1)
          (append
            acc
            (list
              (list
                (string-append (calendar/pad2 m) " " (calendar/month-name m))
                (string-append y "-" (calendar/pad2 m))))))))
  "Accumulate month entries (label DD Mon, target YYYY-MM).")

(define calendar/month-name
  (lambda (m)
    (nth
      (list
        "Jan" "Feb" "Mar" "Apr" "May" "Jun"
        "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")
      (- m 1)))
  "Month abbreviation for 1..12.")

(define calendar/browse-days
  (lambda (ym)
    (let ((y (substring ym 0 4))
          (m (parse-int (substring ym 5 2))))
      (calendar/browse-days-acc y m 1 (calendar/days-in-month y m) (list))))
  "Day entries for a month, each a leaf to the day agenda.")

(define calendar/browse-days-acc
  (lambda (y m d dim acc)
    (if (> d dim)
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

(define filmstrip/pkg-browse
  (lambda (pkg pkg-path)
    (if (string-eq pkg "todo")
        (todo/browse pkg-path)
        (if (string-eq pkg "notes")
            (notes/browse pkg-path)
            (if (string-eq pkg "calendar")
                (calendar/browse pkg-path)
                (cons (list (list)) #f)))))
  "Dispatch the package browse; unknown packages get one empty column.")

(define filmstrip/browse
  (lambda (path)
    (let ((apps (filmstrip/apps)))
      (if (null? path)
          (cons (list apps) #f)
          (let ((r (filmstrip/pkg-browse (as-str (car path)) (cdr path))))
            (cons (cons apps (car r)) (cdr r))))))
  "Shell cascade: column 0 is the app chooser; deeper columns come from
the selected package's browse. Returns (columns . leaf?).")

;; ---------- cascade strip rendering ----------

(define filmstrip/entry-html
  (lambda (i e path)
    (let ((label (car e))
          (target (car (cdr e)))
          (sel (and (< i (length path))
                    (string-eq (as-str (nth path i)) (car (cdr e))))))
      (html/tag
        "button"
        (html/attrs
          (list
            (html/attr "class" (if sel "fs-entry fs-sel" "fs-entry"))
            (html/attr "hx-post" "/filmstrip/cascade")
            (html/attr
              "hx-vals"
              (string-append "{\"d\":\"" (as-str i) "\",\"t\":\"" target "\"}"))
            (html/attr "hx-target" "#fs-sink")
            (html/attr "hx-swap" "innerHTML")))
          (html/text label))))
  "One cascade entry: clicking POSTs (depth i, target). The selected entry
of each column (matching the crumb path) wears fs-sel.")

(define filmstrip/col-html
  (lambda (i col path)
    (html/div
      (html/attr "class" "fs-col")
      (html/join
        (list/map (lambda (e) (filmstrip/entry-html i e path)) col))))
  "One Miller column of entries.")

(define filmstrip/cascade-html
  (lambda (path)
    (let ((br (filmstrip/browse path)))
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

(define filmstrip/oob-cascade
  (lambda (path)
    (let ((br (filmstrip/browse path)))
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

;; ---------- launcher + sentinels ----------

(define filmstrip/launcher-html
  (lambda ()
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
  "Inline launcher row at the head of the content strip: text input for a
route + open button. The input's accesskey is the zero-JS 'a key opens
the launcher' affordance (browser chord, e.g. Alt+Shift+L on Firefox).")

(define filmstrip/sentinel
  (lambda (url vals trigger)
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
  "Invisible trigger element: an htmx key filter on the body that POSTs a
shell event (vals carries the dir param). Zero javascript files (D3) --
the filter expression is htmx.")

(define filmstrip/key-sentinels
  (lambda ()
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
  "alt+arrows move focus; alt+shift+arrows move pane order. The server
owns focus and answers with OOB class swaps (borge pane-protocol).")

;; ---------- row access + mutation helpers (W1/W3) ----------

(define filmstrip/row
  (lambda ()
    (let ((f (filmstrip/ws-fields)))
      (list (filmstrip/ws-cascade f) (filmstrip/ws-panes f) (filmstrip/ws-focus f))))
  "The workspace triple (cascade panes focus) from this user's row.")

(define filmstrip/find-route-index
  (lambda (panes r)
    (filmstrip/find-index
      (lambda (p) (string-eq (filmstrip/pane-route p) r))
      panes
      0))
  "Index of the pane already showing route r, else -1 (open-or-focus).")

(define filmstrip/replace-at
  (lambda (xs i x)
    (if (null? xs)
        (list)
        (if (<= i 0)
            (cons x (cdr xs))
            (cons (car xs) (filmstrip/replace-at (cdr xs) (- i 1) x)))))
  "Copy of xs with the element at index i replaced by x.")

(define filmstrip/width-next
  (lambda (w)
    (filmstrip/width-cycle filmstrip/widths w))
  "Next width preset after w -- the third/half/full cycle, tail wraps.")

(define filmstrip/width-cycle
  (lambda (ws w)
    (if (null? ws)
        filmstrip/default-width
        (if (string-eq (as-str (car ws)) (as-str w))
            (if (null? (cdr ws)) (car filmstrip/widths) (car (cdr ws)))
            (filmstrip/width-cycle (cdr ws) w))))
  "Walk the preset list; a hit returns its successor, the tail wraps to
the head, and an unknown width falls back to the default.")

(define filmstrip/focus-step
  (lambda (n i dir)
    (if (or (< i 0) (>= i n))
        (if (string-eq dir "left") -1 (if (> n 0) 0 -1))
        (let ((j (if (string-eq dir "left") (- i 1) (+ i 1))))
          (if (< j 0)
              0
              (if (>= j n) (- n 1) j)))))
  "Next focus index for dir (left/right): clamped to the pane count, no
wrap (niri discipline). An unset/out-of-range focus heals to pane 0 on
right, none on left.")

(define filmstrip/close-focus
  (lambda (i focus m)
    (cond
      ((< focus 0) -1)
      ((= focus i) (if (<= m 0) -1 (min (max (- i 1) 0) (- m 1))))
      ((> focus i) (- focus 1))
      (#t focus)))
  "Focus index after removing pane i (m = pane count after removal): the
closed pane's focus moves to its left neighbor (or the new head), later
panes shift left with it.")

(define filmstrip/open-apply
  (lambda (panes r)
    "Open-or-focus for route r: a fresh pane record (UUID id, default
width preset) appended, or the existing pane's index. Returns
(panes focus appended?) -- appended? false means an existing pane won."
    (let ((i (filmstrip/find-route-index panes r)))
      (if (>= i 0)
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
            #t)))))

(define filmstrip/open-response
  (lambda (panes2 old-focus focus2 appended?)
    "OOB response body for an open/leaf click: OOB class swap on the
previously focused pane, then the opened pane -- appended with its real
dispatched content, or re-swapped focused (open-or-focus)."
    (html/join
      (list
        (if (if (>= old-focus 0) (not (= old-focus focus2)) #f)
            (filmstrip/oob-section (filmstrip/pane-at panes2 old-focus) #f)
            "")
        (if appended?
            (filmstrip/oob-append-pane (filmstrip/pane-at panes2 focus2) #t)
            (filmstrip/oob-section (filmstrip/pane-at panes2 focus2) #t))))))

;; ---------- shell assembly (W2) ----------

(define filmstrip/panes-html
  (lambda (panes focus)
    (filmstrip/map-indexed
      (lambda (p i)
        (filmstrip/pane-html
          p
          (= i focus)
          (filmstrip/pane-content (filmstrip/pane-route p))))
      panes
      0))
  "Every pane section with its in-process dispatched content; the focused
index wears .focused.")

(define filmstrip/shell-html
  (lambda (cascade panes focus)
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
  "The whole shell: style + cascade strip over launcher + content strip,
key sentinels, and the invisible OOB sink. The HTTP gate wraps this
fragment in chrome on full-page requests.")

(define filmstrip/page
  (lambda ()
    "GET /filmstrip: the habitat shell. Reads the workspace row and renders
the cascade strip (crumb path) over the content strip (panes with real
dispatched content). Empty state is valid: no panes, launcher visible."
    (let* ((row (filmstrip/row))
           (cascade (nth row 0))
           (panes (nth row 1))
           (focus (nth row 2)))
      (render-html (filmstrip/shell-html cascade panes focus)))))

;; ---------- shell events (W3): POST handlers ----------

(define filmstrip/open
  (lambda ()
    "POST /filmstrip/open: launcher route param -> open-or-focus a pane.
Bad/missing routes answer with the launcher status line only; valid ones
persist the row and answer with OOB pane swaps."
    (let* ((r (string-trim (as-str (request-param "route"))))
           (row (filmstrip/row))
           (cascade (nth row 0))
           (panes (nth row 1))
           (focus (nth row 2)))
      (if (or (< (string-length r) 1) (not (string-starts-with? r "/")))
          (render-html (filmstrip/oob-msg "filmstrip: routes start with / (try /todos)"))
          (let ((path (car (filmstrip/split-query r))))
            (if (null? (route/lookup path))
                (render-html
                  (filmstrip/oob-msg (string-append "filmstrip: no route " path)))
                (let* ((res (filmstrip/open-apply panes r))
                       (panes2 (nth res 0))
                       (focus2 (nth res 1))
                       (appended? (nth res 2)))
                  (seq
                    (filmstrip/save-all cascade panes2 focus2)
                    (render-html
                      (html/join
                        (list
                          (filmstrip/oob-msg "")
                          (filmstrip/open-response panes2 focus focus2 appended?))))))))))))

(define filmstrip/close
  (lambda ()
    "POST /filmstrip/close: id param -> remove the pane record; OOB delete
of its section + focus hand-off per close-focus."
    (let* ((id (as-str (request-param "id")))
           (row (filmstrip/row))
           (cascade (nth row 0))
           (panes (nth row 1))
           (focus (nth row 2))
           (i (filmstrip/find-pane-index panes id)))
      (if (< i 0)
          (render-html (filmstrip/oob-msg "filmstrip: no such pane"))
          (let* ((panes2 (filmstrip/remove-at panes i))
                 (focus2 (filmstrip/close-focus i focus (length panes2))))
            (seq
              (filmstrip/save-all cascade panes2 focus2)
              (render-html
                (html/join
                  (list
                    (filmstrip/oob-msg "")
                    (filmstrip/oob-delete-pane id)
                    (if (if (>= focus2 0) (not (= focus2 focus)) #f)
                        (filmstrip/oob-section (filmstrip/pane-at panes2 focus2) #t)
                        ""))))))))))

(define filmstrip/width
  (lambda ()
    "POST /filmstrip/width: id param -> cycle the pane's width preset
(third/half/full); OOB section swap, pane body preserved."
    (let* ((id (as-str (request-param "id")))
           (row (filmstrip/row))
           (cascade (nth row 0))
           (panes (nth row 1))
           (focus (nth row 2))
           (i (filmstrip/find-pane-index panes id)))
      (if (< i 0)
          (render-html (filmstrip/oob-msg "filmstrip: no such pane"))
          (let* ((p2 (filmstrip/pane-set-width
                       (nth panes i)
                       (filmstrip/width-next (filmstrip/pane-width (nth panes i)))))
                 (panes2 (filmstrip/replace-at panes i p2)))
            (seq
              (filmstrip/save-all cascade panes2 focus)
              (render-html
                (html/join
                  (list
                    (filmstrip/oob-msg "")
                    (filmstrip/oob-section p2 (= i focus)))))))))))

(define filmstrip/focus
  (lambda ()
    "POST /filmstrip/focus: dir param -> move .focused one pane over
(clamped, no wrap); OOB class swap on exactly two panes (old, new)."
    (let* ((dir (as-str (request-param "dir")))
           (row (filmstrip/row))
           (cascade (nth row 0))
           (panes (nth row 1))
           (focus (nth row 2))
           (focus2 (filmstrip/focus-step (length panes) focus dir)))
      (if (= focus2 focus)
          (render-html (filmstrip/oob-msg ""))
          (seq
            (filmstrip/save-all cascade panes focus2)
            (render-html
              (html/join
                (list
                  (filmstrip/oob-msg "")
                  (filmstrip/oob-section (filmstrip/pane-at panes focus) #f)
                  (filmstrip/oob-section (filmstrip/pane-at panes focus2) #t)))))))))

(define filmstrip/move
  (lambda ()
    "POST /filmstrip/move: dir param -> adjacent-transpose the focused pane
with its neighbor; focus follows the pane. OOB re-render of the whole
content strip (bodies preserved)."
    (let* ((dir (as-str (request-param "dir")))
           (row (filmstrip/row))
           (cascade (nth row 0))
           (panes (nth row 1))
           (focus (nth row 2))
           (j (if (string-eq dir "left") (- focus 1) (+ focus 1))))
      (if (if (< focus 0) #t (or (< j 0) (>= j (length panes))))
          (render-html (filmstrip/oob-msg ""))
          (let* ((panes2 (if (string-eq dir "left")
                             (filmstrip/swap-at panes j)
                             (filmstrip/swap-at panes focus))))
            (seq
              (filmstrip/save-all cascade panes2 j)
              (render-html
                (html/join
                  (list
                    (filmstrip/oob-msg "")
                    (filmstrip/oob-content-strip panes2 j))))))))))

(define filmstrip/cascade
  (lambda ()
    "POST /filmstrip/cascade: d (depth) + t (target) params -> truncate the
crumb path to d, append t (V2 entry semantics). A crumb target re-walks
the columns; a route target (leaf) also open-or-focuses its pane."
    (let* ((d (parse-int (request-param "d")))
           (t (as-str (request-param "t")))
           (row (filmstrip/row))
           (cascade (nth row 0))
           (panes (nth row 1))
           (focus (nth row 2))
           (path2 (append (filmstrip/take cascade d) (list t))))
      (if (filmstrip/route? t)
          (let* ((res (filmstrip/open-apply panes t))
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
                  (filmstrip/oob-cascade path2)))))))))

;; ---------- routes ----------

(route "/filmstrip" "filmstrip/page")
(route "/filmstrip/open" "filmstrip/open")
(route "/filmstrip/close" "filmstrip/close")
(route "/filmstrip/width" "filmstrip/width")
(route "/filmstrip/focus" "filmstrip/focus")
(route "/filmstrip/move" "filmstrip/move")
(route "/filmstrip/cascade" "filmstrip/cascade")

;; ---------- shell CSS (W2) ----------

(define filmstrip/css
  (lambda ()
    (css/sheet
      (list
        (css/rule
          ".fs-shell"
          (list
            "display" "flex"
            "flex-direction" "column"
            "gap" ".4rem"
            "height" "calc(100vh - 6rem)"
            "min-height" "28rem"))
        (css/rule
          ".fs-cascade"
          (list
            "display" "flex"
            "flex-direction" "row"
            "gap" ".35rem"
            "flex" "0 0 28vh"
            "min-height" "8rem"
            "overflow-x" "auto"
            "overflow-y" "hidden"))
        (css/rule
          ".fs-col"
          (list
            "flex" "0 0 auto"
            "min-width" "10rem"
            "max-width" "16rem"
            "display" "flex"
            "flex-direction" "column"
            "border" "1px solid var(--border)"
            "background" "var(--panel)"
            "overflow-y" "auto"))
        (css/rule
          ".fs-entry"
          (list
            "text-align" "left"
            "padding" ".22rem .5rem"
            "background" "none"
            "border" "none"
            "border-bottom" "1px solid var(--border)"
            "color" "var(--ink)"
            "cursor" "pointer"
            "font" "inherit"
            "white-space" "nowrap"
            "overflow" "hidden"
            "text-overflow" "ellipsis"
            "max-width" "16rem"))
        (css/rule
          ".fs-entry:hover"
          (list "background" "var(--sel-bg)"))
        (css/rule
          ".fs-entry.fs-sel"
          (list
            "background" "var(--sel-accent)"
            "color" "#fff"
            "font-weight" "700"))
        (css/rule
          ".fs-content"
          (list
            "display" "flex"
            "flex-direction" "row"
            "gap" ".5rem"
            "flex" "1 1 auto"
            "overflow-x" "auto"
            "overflow-y" "hidden"
            "align-items" "stretch"))
        (css/rule
          ".fs-pane"
          (list
            "display" "flex"
            "flex-direction" "column"
            "flex" "0 0 33.333%"
            "min-width" "15rem"
            "border" "1px solid var(--border)"
            "background" "var(--panel)"
            "overflow" "hidden"))
        (css/rule
          ".fs-pane.fs-third"
          (list "flex-basis" "33.333%"))
        (css/rule
          ".fs-pane.fs-half"
          (list "flex-basis" "50%"))
        (css/rule
          ".fs-pane.fs-full"
          (list "flex-basis" "100%"))
        (css/rule
          ".fs-pane.focused"
          (list
            "outline" "2px solid var(--sel-accent)"
            "outline-offset" "-2px"))
        (css/rule
          ".fs-head"
          (list
            "display" "flex"
            "gap" ".3rem"
            "align-items" "center"
            "padding" ".2rem .35rem"
            "border-bottom" "1px solid var(--border)"
            "background" "var(--chrome)"
            "flex" "0 0 auto"))
        (css/rule
          ".fs-title"
          (list
            "flex" "1 1 auto"
            "overflow" "hidden"
            "text-overflow" "ellipsis"
            "white-space" "nowrap"
            "font-weight" "700"
            "font-size" ".85rem"))
        (css/rule
          ".fs-btn"
          (list
            "width" "auto"
            "min-width" "0"
            "padding" ".08rem .4rem"
            "font-size" ".8rem"
            "flex" "0 0 auto"))
        (css/rule
          ".fs-body"
          (list
            "flex" "1 1 auto"
            "overflow" "auto"
            "padding" ".4rem"
            "min-height" "0"))
        (css/rule
          ".fs-launch"
          (list
            "display" "flex"
            "gap" ".35rem"
            "align-items" "center"
            "flex" "0 0 auto"
            "margin" "0"))
        (css/rule
          ".fs-launch input[type=text]"
          (list
            "width" "16rem"
            "padding" ".2rem .35rem"))
        (css/rule
          ".fs-msg"
          (list "color" "var(--muted)" "font-size" ".85rem"))
        (css/rule
          ".fs-sentinel"
          (list "display" "none"))
        (css/rule
          ".fs-sink"
          (list "display" "none"))
        (css/rule
          ".fs-oob-delete"
          (list "display" "none")))))
  "Strip + pane + launcher stylesheet (~50 lines of rules). Widths are
the three presets only; .focused is the server-owned ring.")

;; ---------- scene (soil M-A): the workspace as pads/render-scene ------
;; The kiosk render: same workspace row, same Miller walk, lowered to a
;; scene-dialect-v0 pad tree instead of HTML (docs/plans/soil-m-a.md).
;; Pure data — no eval on any consumer frame path (soil contract).

(define filmstrip/scene-w
  (lambda (w)
    "niri width preset -> grow weight (denominator 6): third=2, half=3,
full=6. The strip splits grow space by weight, so 2/3/6 against a
trailing spacer reproduces exact niri fractions; full+others shrink
proportionally (niri scrolls; the shim has no scroll in v1)."
    (cond
      ((string-eq w "half") 3)
      ((string-eq w "full") 6)
      (#t 2))))

(define filmstrip/scene-weights-sum
  (lambda (panes)
    "Sum of pane grow weights (for the spacer's remainder)."
    (if (null? panes)
        0
        (+ (filmstrip/scene-w (filmstrip/pane-width (car panes)))
           (filmstrip/scene-weights-sum (cdr panes))))))

(define filmstrip/scene-lines
  (lambda (labels)
    "Label strings -> text pad nodes, capped at 12 with a dim +N more
tail (the shim clips rather than scrolls, so columns cap instead)."
    (let ((capped (filmstrip/take labels 12)))
      (append
        (list/map (lambda (s) (pad 'text (list) s)) capped)
        (if (> (length labels) 12)
            (list (pad 'text (list (cons 'dim "1"))
                    (string-append "+ " (as-str (- (length labels) 12)) " more")))
            (list))))))

(define filmstrip/scene-todo-count
  (lambda (lid)
    "Item count of one todo list (the browse data, counted)."
    (length
      (list/filter
        (lambda (row) (string-eq (as-str (dict-get (cdr row) "list_id")) lid))
        (todo/item/all)))))

(define filmstrip/scene-pane-body
  (lambda (route)
    "Text summary lines for a pane route (D4: summaries from existing
prims — the shell is the M-A subject, not live pane interiors)."
    (cond
      ((string-starts-with? route "/todos")
       (filmstrip/scene-lines
         (list/map
           (lambda (row)
             (string-append
               (as-str (dict-get (cdr row) "title"))
               " ("
               (as-str (filmstrip/scene-todo-count (as-str (car row))))
               ")"))
           (todo/list/all))))
      ((string-starts-with? route "/notes")
       (filmstrip/scene-lines
         (list/map (lambda (r) (as-str (nth r 2))) (notes/list-light))))
      ((string-starts-with? route "/calendar")
       (list
         (pad 'text (list) (string-append "today: " (calendar/today)))
         (pad 'text (list)
           (string-append (as-str (length (calendar/all-events))) " events"))))
      (#t
       (list (pad 'text (list (cons 'dim "1")) "open in browser /filmstrip"))))))

(define filmstrip/scene-pane
  (lambda (p focused?)
    "One content pane as a scene column: niri weight from the width
preset, route title (level 1) wearing sel when focused, summary body."
    (let ((route (filmstrip/pane-route p))
          (attrs (list
                   (cons 'size
                     (list 'grow (filmstrip/scene-w (filmstrip/pane-width p))))
                   (cons 'gap "2")
                   (cons 'padding "6"))))
      (pad 'column attrs
        (pad 'text
          (if focused?
              (list (cons 'level "1") (cons 'sel "1"))
              (list (cons 'level "1")))
          route)
        (filmstrip/scene-pane-body route)))))

(define filmstrip/scene-strip
  (lambda (panes focus)
    "The content strip: pane columns at niri weights + a trailing
spacer absorbing unused fraction (third+half -> 2:3 + 1/6 empty);
empty state is a dim hint."
    (let ((pane-nodes
            (filmstrip/map-indexed
              (lambda (p i) (filmstrip/scene-pane p (= i focus)))
              panes
              0))
          (wsum (filmstrip/scene-weights-sum panes)))
      (pad 'row (list (cons 'gap "8") (cons 'size (list 'grow 1)))
        (if (null? panes)
            (list (pad 'text (list (cons 'dim "1"))
                    "no panes -- open /todos from the browser"))
            (append
              pane-nodes
              (if (< wsum 6)
                  (list (pad 'text
                          (list (cons 'size (list 'grow (- 6 wsum))))
                          ""))
                  (list))))))))

(define filmstrip/scene-entry-sel?
  (lambda (i e path)
    "Entry selected when the crumb path at depth i names its target."
    (and (< i (length path))
         (string-eq (as-str (nth path i)) (as-str (car (cdr e)))))))

(define filmstrip/scene-col
  (lambda (i col path)
    "One Miller column: entries as text (selected wears sel), capped at
12 with a dim +N more tail; column 0 carries the shell header."
    (pad 'column (list (cons 'gap "2") (cons 'padding "6"))
      (append
        (if (= i 0)
            (list (pad 'text (list (cons 'level "1")) "filmstrip"))
            (list))
        (let ((capped (filmstrip/take col 12)))
          (append
            (list/map
              (lambda (e)
                (pad 'text
                  (if (filmstrip/scene-entry-sel? i e path)
                      (list (cons 'sel "1"))
                      (list))
                  (as-str (car e))))
              capped)
            (if (> (length col) 12)
                (list (pad 'text (list (cons 'dim "1"))
                        (string-append "+ " (as-str (- (length col) 12)) " more")))
                (list))))))))

(define filmstrip/scene-cascade
  (lambda (cols path)
    "The cascade strip: one column per browse level over the app
chooser (cols from filmstrip/browse)."
    (pad 'row (list (cons 'gap "6"))
      (filmstrip/map-indexed
        (lambda (col i) (filmstrip/scene-col i col path))
        cols
        0))))

(define filmstrip/scene
  (lambda ()
    "SCENE v0 kiosk render of the workspace (soil M-A): cascade strip
(Miller columns, selected entry wears sel) over a dim launcher hint over
the content strip (pane columns at niri weights, focused pane wears
sel). Pure data: pads/render-scene output; eval-only (no route), the
native shim consumes it via loopback eval --eval \"(filmstrip/scene)\"."
    (let* ((row (filmstrip/row))
           (cascade (nth row 0))
           (panes (nth row 1))
           (focus (nth row 2))
           (br (filmstrip/browse cascade)))
      (pads/render-scene
        (pad 'column (list (cons 'gap "4") (cons 'padding "8"))
          (filmstrip/scene-cascade (car br) cascade)
          (pad 'text (list (cons 'dim "1"))
            "kiosk view -- edit at /filmstrip")
          (filmstrip/scene-strip panes focus))))))

;; ---------- native policy surface (soil M-B) --------------------------
;; soil-strip (the river window-manager client) drives the SAME workspace
;; row the browser shell serves: a mapped river window becomes a pane
;; record carrying a `view` key (its river window identity); habitat
;; panes have no view field. All evals return native-state so one round
;; trip drives the next manage sequence.

(define filmstrip/pane-view
  (lambda (p)
    (as-str (dict-get p "view")))
  "Pane record's river window key (empty for habitat panes).")

(define filmstrip/native-pane-json
  (lambda (p focused?)
    (dict-set* "{}"
      (list
        "id" (filmstrip/pane-id p)
        "route" (filmstrip/pane-route p)
        "width" (filmstrip/pane-width p)
        "view" (filmstrip/pane-view p)
        "focused" (if focused? "1" "0"))))
  "One pane as the policy-state JSON object.")

(define filmstrip/native-state
  (lambda ()
    "Policy state for soil-strip: panes (with view keys + focused flags),
cascade, focus index — the layout source of the next manage sequence."
    (let* ((row (filmstrip/row))
           (cascade (nth row 0))
           (panes (nth row 1))
           (focus (nth row 2)))
      (json-emit
        (dict-set* "{}"
          (list
            "panes"
            (json-emit
              (filmstrip/map-indexed
                (lambda (p i) (filmstrip/native-pane-json p (= i focus)))
                panes
                0))
            "cascade" (json-emit cascade)
            "focus" (as-str focus)))))))

(define filmstrip/native-find-view
  (lambda (panes view-key)
    (filmstrip/find-index
      (lambda (p) (string-eq (filmstrip/pane-view p) view-key))
      panes
      0))
  "Index of the pane bound to a river window key, else -1.")

(define filmstrip/native-open
  (lambda (app-id view-key)
    "A foreign river window mapped: open-or-focus the pane bound to
view-key (route app:<app-id>, default width preset). Returns
native-state."
    (let* ((row (filmstrip/row))
           (cascade (nth row 0))
           (panes (nth row 1))
           (i (filmstrip/native-find-view panes view-key)))
      (if (>= i 0)
          (seq
            (filmstrip/save-all cascade panes i)
            (filmstrip/native-state))
          (let ((panes2
                  (append
                    panes
                    (list
                      (dict-set* "{}"
                        (list
                          "id" (filmstrip/new-id)
                          "route" (string-append "app:" app-id)
                          "width" filmstrip/default-width
                          "view" view-key))))))
            (seq
              (filmstrip/save-all cascade panes2 (length panes))
              (filmstrip/native-state)))))))

(define filmstrip/native-close
  (lambda (view-key)
    "A foreign river window closed: remove its pane, focus hand-off per
close-focus. Returns native-state."
    (let* ((row (filmstrip/row))
           (cascade (nth row 0))
           (panes (nth row 1))
           (focus (nth row 2))
           (i (filmstrip/native-find-view panes view-key)))
      (if (< i 0)
          (filmstrip/native-state)
          (let* ((panes2 (filmstrip/remove-at panes i))
                 (focus2 (filmstrip/close-focus i focus (length panes2))))
            (seq
              (filmstrip/save-all cascade panes2 focus2)
              (filmstrip/native-state)))))))

(define filmstrip/native-focus
  (lambda (dir)
    "alt+arrows policy: the focus handler's row mutation, clamped no
wrap. Returns native-state."
    (let* ((row (filmstrip/row))
           (cascade (nth row 0))
           (panes (nth row 1))
           (focus (nth row 2))
           (focus2 (filmstrip/focus-step (length panes) focus dir)))
      (seq
        (filmstrip/save-all cascade panes focus2)
        (filmstrip/native-state)))))

(define filmstrip/native-move
  (lambda (dir)
    "alt+shift+arrows policy: adjacent transpose of the focused pane;
focus follows. Returns native-state."
    (let* ((row (filmstrip/row))
           (cascade (nth row 0))
           (panes (nth row 1))
           (focus (nth row 2))
           (j (if (string-eq dir "left") (- focus 1) (+ focus 1))))
      (if (if (< focus 0) #t (or (< j 0) (>= j (length panes))))
          (filmstrip/native-state)
          (let ((panes2 (if (string-eq dir "left")
                            (filmstrip/swap-at panes j)
                            (filmstrip/swap-at panes focus))))
            (seq
              (filmstrip/save-all cascade panes2 j)
              (filmstrip/native-state)))))))
