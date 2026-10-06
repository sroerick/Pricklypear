;; package book -- exported from live branch system/main
;; Sync: scripts/pp-sync.sh status|pull|snapshot

(define book/binding-classes
  (lambda
    (prose)
    (let
      ((toks (prose-tokens (as-str prose))))
      (list
        (list/filter spec/binding-resolves? toks)
        (list/filter
          (lambda (n) (not (spec/binding-resolves? n)))
          toks))))
  "(resolved holes) over prose's UNIQUE token-shaped cores: one kernel\n   scan + O(unique tokens) function-source lookups, never O(words).")

(define book/chapter-html
  (lambda
    (label path b mode)
    (let
      ((title (book/f b "title"))
        (prose (book/f b "prose"))
        (raw (book/f b "raw")))
      (html/page
        (list
          (book/style-html)
          (html/p*
            (html/attr "class" "muted")
            (html/join (list (html/a "/book" "book") " · chapter")))
          (book/toggle-html
            (string-append "/book/chapter/" label)
            mode)
          (html/h1 (html-escape (as-str title)))
          (book/workshop-html b mode)
          (book/raw-html raw)
          (let
            ((link? (book/fn-bound? "spec/token-binding?")))
            (book/prose-html-cls
              prose
              (book/row-classes b prose link?)
              link?))
          (book/children-html path mode)
          (html/p*
            (html/attr "class" "muted")
            (html-escape
              (string-append
                "chapter "
                (as-str (book/f b "chapter"))
                " · seeded rows (git is authoritative for .borg)")))))))
  "One borg chapter page from tree rows.")

(define book/chapter-li
  (lambda
    (node mode)
    (let
      ((b (dict-get node "body")))
      (let
        ((title (book/f b "title"))
          (status (book/f b "status")))
        (html/li
          ""
          (html/join
            (list
              (html/a
                (string-append
                  "/book/chapter/"
                  (tree/leaf (book/row-path node)))
                (html-escape (as-str title)))
              (if
                (string-eq mode "workshop")
                (string-append " " (book/status-badge status))
                "")))))))
  "Index row for one chapter (status badge in workshop register only).")

(define book/chapter-page
  (lambda
    ()
    (let
      ((label
        (book/path-suffix "/book/chapter/" (request-path))))
      (if
        (not (book/label-safe? label))
        (book/not-found "chapter")
        (let
          ((path (string-append "data.book.ch." label)))
          (let
            ((b (book/row path)))
            (if
              (null? b)
              (book/not-found (string-append "chapter " label))
              (book/chapter-html label path b (book/mode))))))))
  "GET /book/chapter/<label> — headings/prose/code/examples from rows.")

(define book/children
  (lambda (parent-path) (tree/children-of parent-path))
  "Document-order children of a book row (rank order; tree/children-of's\n   literal-h covering reaches chapter.h.<section> and section.h.<sub>).")

(define book/children-html
  (lambda
    (parent-path mode)
    (html/join
      (list/map
        (lambda
          (node)
          (let
            ((b (dict-get node "body")))
            (if
              (string-eq (as-str (dict-get b "node")) "example")
              (book/example-html node mode)
              (book/section-html node mode))))
        (book/children parent-path))))
  "Document-order children of a book row: sections (recursive) + examples.")

(define book/classes-compute
  (lambda
    (prose)
    (book/partition-toks
      (prose-tokens (as-str prose))
      ()
      ()))
  "(resolved holes) over prose's UNIQUE token-shaped cores: one kernel\n   scan + O(unique tokens) function-source lookups, never O(words).\n   PAGE PATHS MUST NOT RUN THIS against the seeded book: on prod each\n   function-source is a DB round trip, and a chapter costs hundreds,\n   so pages blow the 100k-step budget. Page paths use book/row-classes\n   (stored fields); this compute path is for book/precompute1\n   (seed-time) and the small course/outline surfaces.")

(define book/code-index-html
  (lambda
    ()
    (let
      ((code (book/children "data.book.code")))
      (if
        (null? code)
        (html/p*
          (html/attr "class" "muted")
          "No book rows seeded yet — run scripts/load-book.sh <export.book.json>.")
        (html/join
          (list
            (html/h2
              (string-append
                "Code appendix ("
                (as-str (count code))
                ")"))
            (html/tag
              "details"
              ""
              (html/join
                (list
                  (html/tag
                    "summary"
                    ""
                    "all code chapters (.pp / .sql / .ml)")
                  (html/ul
                    ""
                    (html/join
                      (list/map
                        (lambda
                          (node)
                          (let
                            ((b (dict-get node "body")))
                            (html/li
                              ""
                              (html/a
                                (string-append
                                  "/book/code/"
                                  (tree/leaf (book/row-path node)))
                                (html-escape (as-str (book/f b "path")))))))
                        code)))))))))))
  "Collapsed index of every code chapter (the export's appendix order).")

(define book/code-page
  (lambda
    ()
    (let
      ((label (book/path-suffix "/book/code/" (request-path))))
      (if
        (not (book/label-safe? label))
        (book/not-found "code chapter")
        (let
          ((b (book/row (string-append "data.book.code." label))))
          (if
            (null? b)
            (book/not-found (string-append "code chapter " label))
            (let
              ((p (book/f b "path"))
                (lang (book/f b "lang"))
                (src (book/f b "source")))
              (html/page
                (list
                  (book/style-html)
                  (html/p*
                    (html/attr "class" "muted")
                    (html/join
                      (list (html/a "/book" "book") " · code appendix")))
                  (html/h1 (html-escape (as-str p)))
                  (html/p*
                    (html/attr "class" "muted")
                    (string-append "lang: " (as-str lang)))
                  (html/pre (html-escape (as-str src)))))))))))
  "GET /book/code/<label> — one code-chapter listing from its row.")

(define book/collect
  (lambda
    (path)
    (book/collect-list (book/children path) (list path))))

(define book/collect-list
  (lambda
    (paths acc)
    (if
      (null? paths)
      acc
      (book/collect-list
        (cdr paths)
        (book/collect (car paths) acc)))))

(define book/course-chapter-ref
  (lambda
    (body)
    (let
      ((lines (string-split (as-str body) "\n")))
      (if
        (null? lines)
        ""
        (let
          ((first (as-str (car lines))))
          (if
            (string-starts-with? first "chapter: ")
            (substring first 9 (string-length first))
            "")))))
  "Chapter label referenced by an outline node — the `chapter: <label>`\n   first line of its body — or \"\" when the node is not a reading.\n   Labels are book/label-safe? (the loader's ltree shape); validated\n   before any link or row lookup.")

(define book/course-desc
  (lambda
    (body)
    (list/foldl
      (lambda
        (acc line)
        (if
          (string-eq acc "")
          (as-str line)
          (string-append acc "\n" (as-str line))))
      ""
      (cdr (string-split (as-str body) "\n"))))
  "Node body minus its first (chapter-reference) line, rejoined —\n   prose-html re-paragraphs on blank lines.")

(define book/course-doc-slug
  (lambda
    (suffix)
    (string-append "course/" (as-str suffix)))
  "Notes-doc slug for a course URL suffix.")

(define book/course-drop
  (lambda
    (n xs)
    (if (= n 0) xs (book/course-drop (- n 1) (cdr xs))))
  "Tail after n elements.")

(define book/course-html
  (lambda
    (suffix doc mode link?)
    (let
      ((title (as-str (notes/doc/title doc)))
        (outline (notes/get-outline doc)))
      (html/page
        (list
          (book/style-html)
          (html/p*
            (html/attr "class" "muted")
            (html/join (list (html/a "/book" "book") " · course")))
          (book/toggle-html
            (string-append "/book/course/" suffix)
            mode)
          (html/h1 (html-escape title))
          (book/prose-html (outline/body outline) link?)
          (book/course-level-html (outline/kids outline) 1 link?)
          (html/p*
            (html/attr "class" "muted")
            (html-escape
              (string-append
                "course "
                suffix
                " · a composition over the book's chapters, never a copy — "
                "paper edition: borge book print --root course/"
                suffix
                ".borg")))))))
  "Course page from a notes doc: title, blurb, ordered reading list\n   with chapter status badges, labs.")

(define book/course-index-li
  (lambda
    (node)
    (let
      ((n (json-parse (as-str node))))
      (if
        (null? n)
        ""
        (let
          ((b (dict-get n "body")))
          (let
            ((slug (if (null? b) "" (as-str (dict-get b "slug")))))
            (let
              ((suffix (book/path-suffix "course/" slug)))
              (if
                (string-eq suffix "")
                ""
                (html/li
                  ""
                  (html/a
                    (string-append "/book/course/" suffix)
                    (html-escape
                      (if (null? b) suffix (as-str (dict-get b "title")))))))))))))
  "Index row for one course doc (a tree/find-by hit); empty for\n   non-course/ slugs — kind=course is course-facing only when the slug\n   carries the course/ prefix.")

(define book/course-len
  (lambda
    (xs)
    (if (null? xs) 0 (+ 1 (book/course-len (cdr xs)))))
  "Length of a cons-built (pair) or kernel list. Kernel count is\n   VList-strict — it refuses pair chains (the cons-recursion shape\n   book/course-readers builds; spec/append produces them too), while\n   list/map, list/filter and html/join all accept pairs. Pair-friendly\n   length here; kernel count relaxation filed in log/issues.")

(define book/course-level-html
  (lambda
    (nodes depth link?)
    (if
      (null? nodes)
      ""
      (if
        (string-eq
          (book/course-chapter-ref (outline/body (car nodes)))
          "")
        (html/join
          (list
            (book/course-section-html (car nodes) depth link?)
            (book/course-level-html (cdr nodes) depth link?)))
        (let
          ((run (book/course-readers nodes)))
          (html/join
            (list
              (html/tag
                "ol"
                ""
                (html/join (list/map book/course-reading-html run)))
              (book/course-level-html
                (book/course-drop (book/course-len run) nodes)
                depth
                link?)))))))
  "One outline level: consecutive reading nodes group into a single\n   ordered list (the course's reading order); other nodes render as\n   sections with their children.")

(define book/course-page
  (lambda
    ()
    (let
      ((suffix
        (book/path-suffix "/book/course/" (request-path))))
      (if
        (not (book/course-suffix-safe? suffix))
        (book/not-found "course")
        (if
          (not (book/fn-bound? "notes/doc/get"))
          (book/not-found
            (string-append
              "course "
              suffix
              " (notes not loaded — (load-library \"notes\"))"))
          (let
            ((doc (notes/doc/get (book/course-doc-slug suffix))))
            (if
              (null? doc)
              (book/not-found (string-append "course " suffix))
              (book/course-html
                suffix
                doc
                (book/mode)
                (book/fn-bound? "spec/token-binding?"))))))))
  "GET /book/course/<suffix> — assemble the notes doc course/<suffix>\n   (kind course) over the seeded chapter rows.")

(define book/course-readers
  (lambda
    (nodes)
    (if
      (null? nodes)
      ()
      (if
        (string-eq
          (book/course-chapter-ref (outline/body (car nodes)))
          "")
        ()
        (cons (car nodes) (book/course-readers (cdr nodes))))))
  "The leading run of reading nodes (consecutive chapter refs).")

(define book/course-reading-html
  (lambda
    (node)
    (let
      ((ref (book/course-chapter-ref (outline/body node))))
      (let
        ((safe (book/label-safe? ref)))
        (let
          ((ch-row
            (if
              safe
              (book/row (string-append "data.book.ch." ref))
              ())))
          (html/li
            ""
            (html/join
              (list
                (if
                  safe
                  (html/a
                    (string-append "/book/chapter/" ref)
                    (html-escape (as-str (outline/title node))))
                  (html/span
                    (html/attr "class" "book-hole")
                    (html-escape
                      (string-append
                        (as-str (outline/title node))
                        " — unresolvable chapter ref"))))
                (if
                  (null? ch-row)
                  ""
                  (string-append
                    " "
                    (book/status-badge (book/f ch-row "status"))))
                (book/prose-html
                  (book/course-desc (outline/body node))
                  (book/fn-bound? "spec/token-binding?")))))))))
  "One ordered reading-list entry: chapter link (node title), the\n   chapter's status badge from its seeded row (always shown — the\n   reading list is a status view), then the description prose. An\n   unresolvable ref renders as a hole, never silently.")

(define book/course-section-html
  (lambda
    (node depth link?)
    (let
      ((title (as-str (outline/title node))))
      (html/join
        (list
          (if
            (string-eq title "")
            ""
            ((if (= depth 1) html/h2 html/h3) (html-escape title)))
          (book/prose-html (outline/body node) link?)
          (book/course-level-html
            (outline/kids node)
            (+ depth 1)
            link?)))))
  "Non-reading outline node: heading (h2 at depth 1, h3 below), body\n   prose (binding tokens link when the spec machinery is loaded),\n   children.")

(define book/course-suffix-chars?
  (lambda
    (s i n)
    (if
      (= i n)
      (> n 0)
      (let
        ((c (substring s i 1)))
        (if
          (or
            (string-eq c "_")
            (string-eq c "-")
            (and (string>=? c "a") (string<=? c "z"))
            (and (string>=? c "0") (string<=? c "9")))
          (book/course-suffix-chars? s (+ i 1) n)
          #f))))
  "Char walk for course-suffix-safe?.")

(define book/course-suffix-safe?
  (lambda
    (s)
    (book/course-suffix-chars? s 0 (string-length s)))
  "Nonempty [a-z0-9_-] course URL suffix (notes slugs carry hyphens;\n   the doc slug gets the course/ prefix before any lookup, so the URL\n   suffix alone can never traverse the notes tree).")

(define book/courses-index-html
  (lambda
    ()
    (let
      ((hits (tree/find-by "note" "kind" "course" 50)))
      (let
        ((lis
          (list/filter
            (lambda (li) (not (string-eq li "")))
            (list/map book/course-index-li hits))))
        (if
          (null? lis)
          ""
          (html/join
            (list
              (html/h2 "Courses")
              (html/p*
                (html/attr "class" "muted")
                "compositions over the chapters (habitat-book course-documents) — ordered readings, never copies")
              (html/ul "" (html/join lis))))))))
  "Index of course docs (notes kind=course via tree/find-by — the same\n   read-only lookup notes/doc/row uses for slugs); empty section when\n   none are seeded.")

(define book/example-html
  (lambda
    (node mode)
    (let
      ((b (dict-get node "body")))
      (let
        ((lang (book/f b "lang"))
          (src (book/f b "src"))
          (expected (book/f b "expected")))
        (html/div
          (html/attr "class" "book-example")
          (html/join
            (list
              (html/pre (html/code (as-str src)))
              (if
                (string-eq (as-str lang) "nopales")
                (html/button
                  (html/attrs
                    (list
                      (html/attr "class" "book-run")
                      (html/attr "hx-post" "/book/eval-example")
                      (html/attr
                        "hx-vals"
                        (string-append
                          "{\"path\":\""
                          (book/row-path node)
                          "\"}"))
                      (html/attr "hx-target" "closest div")
                      (html/attr "hx-swap" "outerHTML")))
                  "run")
                (html/span
                  (html/attr "class" "muted")
                  "captured, not live"))
              (if
                (null? expected)
                ""
                (html/p*
                  (html/attr "class" "book-expected")
                  (html-escape
                    (string-append "expect => " (as-str expected)))))))))))
  "Example block: nopales gets a run button (POST /book/eval-example,\n   live with the example-eval prim); ocaml is captured output only\n   (habitat-book eval-examples: no live OCaml evaluator in v1).")

(define book/f
  (lambda
    (body field)
    (if (null? body) () (dict-get body field)))
  "Field of a row body JSON, or nil. dict-get maps JSON arrays/objects\n   back to VStr JSON — wrap with json-parse for lists.")

(define book/fn-bound?
  (lambda (name) (not (null? (function-source name))))
  "True when a library binding has a function_def on the visible lens.")

(define book/guard-msg
  (lambda (e) (if (pair? e) (as-str (nth e 1)) (str e)))
  "Message slot of a guard error value ((error msg kind)); anything\n   else renders via str.")

(define book/hole-html
  (lambda
    (h)
    (let
      ((path (as-str (car h)))
        (src (as-str (car (cdr h))))
        (expected (as-str (car (cdr (cdr h)))))
        (got (as-str (car (cdr (cdr (cdr h)))))))
      (html/div
        (html/attr "class" "book-hole-row")
        (html/join
          (list
            (html/p* "" (html-escape path))
            (html/pre (html-escape src))
            (html/p*
              ""
              (html/join
                (list
                  "expect => "
                  (html/code expected)
                  " · got "
                  (html/code got)))))))))
  "One verify hole row (drift-as-rendering): row path, the example\n   source, expected trailer vs actual render/refusal.")

(define book/json-array
  (lambda
    (xs)
    (string-append "[" (book/json-array-join xs "" #t) "]")))

(define book/json-array-join
  (lambda
    (xs acc first?)
    (if
      (null? xs)
      acc
      (book/json-array-join
        (cdr xs)
        (string-append
          acc
          (if first? "" ",")
          "\""
          (book/json-esc (as-str (car xs)) 0 "")
          "\"")
        #f))))

(define book/json-esc
  (lambda
    (s i acc)
    (if
      (>= i (string-length s))
      acc
      (let
        ((c (substring s i 1)))
        (book/json-esc
          s
          (+ i 1)
          (string-append
            acc
            (if
              (string-eq c "\\")
              "\\\\"
              (if (string-eq c "\"") "\\\"" c)))))))
  "Escape one string for embedding in a JSON array literal.")

(define book/label-chars?
  (lambda
    (s i n)
    (if
      (= i n)
      (> n 0)
      (let
        ((c (substring s i 1)))
        (if
          (or
            (string-eq c "_")
            (and (string>=? c "a") (string<=? c "z"))
            (and (string>=? c "0") (string<=? c "9")))
          (book/label-chars? s (+ i 1) n)
          #f))))
  "Char walk for label-safe?.")

(define book/label-safe?
  (lambda (s) (book/label-chars? s 0 (string-length s)))
  "Nonempty [a-z0-9_]-only string (ltree label shape the loader writes).")

(define book/mode
  (lambda
    ()
    (let
      ((m (request-param "mode")))
      (if
        (and (not (null? m)) (string-eq (as-str m) "workshop"))
        "workshop"
        "reader")))
  "Register of the active request: workshop when ?mode=workshop, else reader.")

(define book/not-found
  (lambda
    (what)
    (status-response
      404
      (string-append
        "<h1>not found</h1><p>No "
        (html-escape (as-str what))
        " in the seeded book.</p>"
        "<p><a href=\"/book\">back to /book</a></p>")))
  "404 status-response for unknown book paths.")

(define book/notes-html
  (lambda
    (notes)
    (if
      (null? notes)
      ""
      (html/join
        (list/map
          (lambda
            (n)
            (html/tag
              "blockquote"
              (html/attr "class" "book-note")
              (html/join
                (list
                  (html/p*
                    (html/attr "class" "muted")
                    (string-append
                      "agent note ("
                      (html-escape (as-str (dict-get n "by")))
                      ")"))
                  (html/p (as-str (dict-get n "body")))))))
          (json-parse (as-str notes))))))
  "Workshop: agent notes as blockquotes (html/p escapes its own body —\n   no pre-escape, that would double-escape).")

(define book/other-html
  (lambda
    (other)
    (if
      (null? other)
      ""
      (html/join
        (list/map
          (lambda
            (o)
            (html/div
              (html/attr "class" "book-other")
              (html/join
                (list
                  (html/p*
                    (html/attr "class" "muted")
                    (string-append
                      "unknown node kind: "
                      (html-escape (as-str (dict-get o "kind")))))
                  (html/pre (html-escape (as-str (dict-get o "body"))))))))
          (json-parse (as-str other))))))
  "Workshop: unknown node kinds the loader could not classify (never\n   silently dropped — the row body is the source of truth).")

(define book/page
  (lambda
    ()
    (let
      ((mode (book/mode)))
      (let
        ((root-b (book/row "data.book"))
          (chapters (book/children "data.book.ch")))
        (let
          ((rev
              (if (null? root-b) "" (as-str (book/f root-b "rev"))))
            (book-root
              (if (null? root-b) "" (as-str (book/f root-b "root")))))
          (html/page
            (list
              (book/style-html)
              (html/h1 "The Pricklypear Book")
              (html/p*
                (html/attr "class" "muted")
                (html-escape
                  (string-append
                    "The spec IS the book — borge chapters rendered live from"
                    " seeded tree rows (scripts/load-book.sh"
                    (if
                      (string-eq rev "")
                      ""
                      (string-append
                        ", rev "
                        (substring rev 0 (min 8 (string-length rev)))))
                    (if
                      (string-eq book-root "")
                      ""
                      (string-append ", root " book-root))
                    ").")))
              (book/toggle-html "/book" mode)
              (html/p*
                (html/attr "class" "muted")
                (html/join
                  (list
                    (html/a "/book/verify" "verify")
                    " — re-run every expect trailer (the book's dune runtest)")))
              (if
                (null? chapters)
                (html/p*
                  (html/attr "class" "muted")
                  "No chapters seeded yet — run scripts/load-book.sh <export.book.json>.")
                (html/join
                  (list
                    (html/h2
                      (string-append
                        "Chapters ("
                        (as-str (count chapters))
                        ")"))
                    (html/ul
                      ""
                      (html/join
                        (list/map
                          (lambda (node) (book/chapter-li node mode))
                          chapters))))))
              (book/courses-index-html)
              (book/spec-index-html)
              (book/code-index-html)))))))
  "GET /book — merged index: borg chapters (export order) + course\n   compositions + package spec docs + the code appendix.")

(define book/para-html
  (lambda
    (para link? classes)
    (html/p*
      ""
      (if
        link?
        (render-tokens
          (as-str para)
          (car classes)
          (car (cdr classes)))
        (html-escape (as-str para)))))
  "One prose paragraph: kernel render-tokens (links + holes) when the\n   spec machinery is loaded; plain escaping otherwise. html/p* keeps\n   the kernel's HTML raw; the plain branch escapes manually (html/p\n   would double-escape the render-tokens output).")

(define book/partition-toks
  (lambda
    (toks resolved holes)
    (if
      (null? toks)
      (list resolved holes)
      (if
        (spec/binding-resolves? (car toks))
        (book/partition-toks
          (cdr toks)
          (cons (car toks) resolved)
          holes)
        (book/partition-toks
          (cdr toks)
          resolved
          (cons (car toks) holes)))))
  "Single pass over unique token cores: ONE spec/binding-resolves? per\n   token (the old double filter called it twice per token).")

(define book/path-suffix
  (lambda
    (prefix path)
    (let
      ((p (as-str path)))
      (let
        ((pre (as-str prefix)))
        (if
          (string-starts-with? p pre)
          (substring
            p
            (string-length pre)
            (- (string-length p) (string-length pre)))
          ""))))
  "Text after prefix, or empty when path lacks the prefix.")

(define book/precompute
  (lambda
    (label)
    (book/precompute-rows
      (book/children (string-append "data.book.ch." label))
      0)))

(define book/precompute-rows
  (lambda
    (rows acc)
    (if
      (null? rows)
      acc
      (book/precompute-rows
        (cdr rows)
        (+
          acc
          (book/precompute1 (book/row-path (car rows)))
          (book/precompute-rows
            (book/children (book/row-path (car rows)))
            0))))))

(define book/precompute1
  (lambda
    (path)
    (let
      ((row (tree/get path)))
      (if
        (null? row)
        0
        (let
          ((b (dict-get row "body")))
          (let
            ((prose (as-str (book/f b "prose"))))
            (if
              (string-eq prose "")
              0
              (let
                ((cls (book/classes-compute prose)))
                (seq
                  (tree/upsert
                    path
                    (dict-get row "kind")
                    (dict-get row "type")
                    (dict-set*
                      (as-str b)
                      (list
                        "resolved"
                        (book/json-array (car cls))
                        "holes"
                        (book/json-array (car (cdr cls))))))
                  1))))))))
  "Resolve + store resolved/holes on ONE section row. Returns 1 when\n   the row had prose. Budget-safe per call (one section's tokens).")

(define book/prose-html
  (lambda
    (prose link?)
    (book/prose-html-cls
      prose
      (if link? (book/classes-compute prose) (list () ()))
      link?))
  "Course/outline surfaces (no book row in hand): compute live. Book\n   section/chapter pages call book/prose-html-cls with row classes.")

(define book/prose-html-cls
  (lambda
    (prose classes link?)
    (let
      ((p (as-str prose)))
      (if
        (string-eq p "")
        ""
        (html/join
          (list/map
            (lambda (para) (book/para-html para link? classes))
            (string-split p "\n\n"))))))
  "Prose paragraphs with caller-supplied (resolved holes) classes:\n   the stored-fields fast path (book/row-classes) or fresh compute.")

(define book/raw-html
  (lambda
    (raw)
    (if
      (null? raw)
      ""
      (html/div
        (html/attr "class" "book-raw")
        (html/join
          (list
            (html/p*
              (html/attr "class" "muted")
              "unparsed source (borge could not structure this — drift hole)")
            (html/pre (html-escape (as-str raw))))))))
  "BOTH registers: the raw fallback is a drift hole, never stripped.")

(define book/row
  (lambda
    (path)
    (let
      ((n (tree/get path)))
      (if (null? n) () (dict-get n "body"))))
  "Body JSON (VStr) of the book tree row at path, or nil when absent.")

(define book/row-classes
  (lambda
    (b prose link?)
    (if
      (not link?)
      (list () ())
      (let
        ((r (book/f b "resolved")) (h (book/f b "holes")))
        (if
          (and (not (null? r)) (not (null? h)))
          (list (json-parse (as-str r)) (json-parse (as-str h)))
          (book/classes-compute prose)))))
  "Prefer the seed-time resolved/holes stored on the row (fast path:\n   json-parse only); fall back to live compute for rows that predate\n   precompute (dev fixtures, course notes).")

(define book/row-path
  (lambda (node) (as-str (dict-get node "path")))
  "Tree path of one node-JSON row (tree/children-of element).")

(define book/section-html
  (lambda
    (node mode)
    (let
      ((b (dict-get node "body")))
      (let
        ((title (book/f b "title"))
          (level (as-str (book/f b "level")))
          (prose (book/f b "prose"))
          (raw (book/f b "raw")))
        (let
          ((tag
            (if
              (string-eq level "2")
              "h2"
              (if (string-eq level "3") "h3" "h4"))))
          (html/join
            (list
              (html/tag tag "" (html-escape (as-str title)))
              (book/workshop-html b mode)
              (book/raw-html raw)
              (let
                ((link? (book/fn-bound? "spec/token-binding?")))
                (book/prose-html-cls
                  prose
                  (book/row-classes b prose link?)
                  link?))
              (book/children-html (book/row-path node) mode)))))))
  "One section/subsection: heading, register scaffolding, prose, then\n   nested children (document order via tree rank).")

(define book/spec-index-html
  (lambda
    ()
    (html/join
      (list
        (html/h2 "Package specs")
        (html/p*
          (html/attr "class" "muted")
          (html/join
            (list
              "spec/<pkg> intent docs -- the other corpus this index merges "
              "(habitat-workflow.book-view). Live listing at "
              (html/a "/spec" "/spec health")
              " (an inline listing here decoded every note outline and blew "
              "the page budget -- DECISION 2026-08-30: point, don't list; a "
              "kernel-side bulk listing prim is the v2 fix)."))))))
  "Index of package spec docs: a pointer to /spec, not a listing\n   (notes/list decodes every outline; blew the page budget on\n   prod's 3361 docs -- DECISION 2026-08-30).")

(define book/spec-packages
  (lambda
    ()
    (list/map
      (lambda
        (row)
        (let
          ((slug (as-str (dict-get (car (cdr row)) "slug"))))
          (substring slug 5 (string-length slug))))
      (list/filter
        (lambda
          (row)
          (spec/starts-with?
            (as-str (dict-get (car (cdr row)) "slug"))
            "spec/"))
        (notes/doc/list))))
  "Package slugs with spec docs, via notes/doc/list (cheap listing;\n   spec/packages-with-docs builds on notes/list, which decodes every\n   outline and blew the 100k-step page budget on prod's 3361 docs).")

(define book/status-badge
  (lambda
    (st)
    (if
      (null? st)
      ""
      (html/span
        (html/attrs
          (list
            (html/attr "class" "book-status")
            (html/attr
              "style"
              (string-append
                "font-size:.7rem;border:1px solid #666;"
                "border-radius:999px;padding:.05rem .5rem;color:"
                (book/status-color st)))))
        (as-str st))))
  "Small status pill (workshop register only).")

(define book/status-color
  (lambda
    (st)
    (let
      ((s (as-str st)))
      (if
        (string-eq s "implemented")
        "#4a9"
        (if
          (string-eq s "planned")
          "#c93"
          (if (string-eq s "deprecated") "#a66" "#888")))))
  "Badge color for a borge status.")

(define book/style-html
  (lambda
    ()
    (html/tag
      "style"
      ""
      (string-append
        ".book-example{border:1px solid #3a3f44;border-radius:.5rem;"
        "padding:.5rem .6rem;margin:.7rem 0;background:#101418}"
        ".book-example pre{margin:.2rem 0 .4rem 0;overflow-x:auto}"
        ".book-expected{color:#8a8f98;font-size:.85rem;margin:.1rem 0}"
        "details.book-verify{border:1px dashed #555;border-radius:.4rem;"
        "padding:.3rem .5rem;margin:.4rem 0}"
        "details.book-verify summary{cursor:pointer;color:#9aa0a6}"
        "blockquote.book-note{border-left:3px solid #666;margin:.5rem 0;"
        "padding:.1rem .8rem;color:#b9bdc4}"
        ".book-hole{color:#f88;border-bottom:2px dashed #c44}"
        ".book-result{border-top:1px dashed #333;margin-top:.4rem;"
        "padding-top:.3rem;color:#cde;font-size:.9rem}"
        ".book-elapsed{color:#667;font-size:.75rem}"
        ".book-error{color:#f88;margin-top:.3rem;font-size:.85rem}"
        ".book-hole-row{border:1px solid #c44;border-radius:.4rem;"
        "padding:.5rem .6rem;margin:.6rem 0;color:#f88}"
        ".book-hole-row pre{color:#cde;margin:.3rem 0}")))
  "Book-page styles (phone-honest; print-considerate by restraint).")

(define book/toggle-html
  (lambda
    (base mode)
    (if
      (string-eq mode "workshop")
      (html/p*
        (html/attr "class" "muted")
        (html/join
          (list
            "workshop register · "
            (html/a
              (string-append base "?mode=reader")
              "reader view"))))
      (html/p*
        (html/attr "class" "muted")
        (html/join
          (list
            "reader register · "
            (html/a
              (string-append base "?mode=workshop")
              "workshop view"))))))
  "The register toggle — the 'spec vs textbook' switch made tangible.")

(define book/untracked-html
  (lambda
    (untracked)
    (if
      (null? untracked)
      ""
      (html/tag
        "p"
        (html/attr "class" "muted")
        (html/tag
          "em"
          ""
          (html/join
            (list/map
              (lambda
                (u)
                (html-escape
                  (string-append
                    "[untracked "
                    (as-str (dict-get u "path"))
                    ": "
                    (as-str (dict-get u "reason"))
                    "] ")))
              (json-parse (as-str untracked))))))))
  "Workshop: untracked-path notes as italic lines.")

(define book/verify
  (lambda
    (&optional slug)
    (let
      ((root
        (if
          (string-eq (as-str slug) "")
          "data.book.ch"
          (string-append "data.book.ch." (as-str slug)))))
      (book/verify-holes (tree/subtree root 20000))))
  "(book/verify [slug]) — the book's self-test: re-eval every nopales\n   example with an expect trailer under data.book.ch[.slug]; returns\n   hole records (path src expected got); empty list = all green.\n   Author-time verify half of habitat-book eval-examples (the run\n   button is the read-time half).")

(define book/verify-candidates
  (lambda
    (rows)
    (list/filter
      (lambda
        (n)
        (not (book/verify-skip? (dict-get n "body"))))
      rows))
  "Example rows verify will actually run (nopales + expect trailer).")

(define book/verify-holes
  (lambda
    (rows)
    (list/filter
      (lambda (h) (not (null? h)))
      (list/map book/verify-one rows)))
  "Hole records over a row set, document order (tree/subtree order).")

(define book/verify-html
  (lambda
    (verify)
    (if
      (null? verify)
      ""
      (let
        ((items (json-parse (as-str verify))))
        (html/tag
          "details"
          (html/attr "class" "book-verify")
          (html/join
            (list
              (html/tag
                "summary"
                ""
                (string-append "verify (" (as-str (count items)) ")"))
              (html/pre
                (html-escape
                  (html/join
                    (list/map
                      (lambda (v) (string-append (as-str v) "\n"))
                      items))))))))))
  "Workshop: verify entries as a collapsed listing.")

(define book/verify-one
  (lambda
    (node)
    (let
      ((b (dict-get node "body")))
      (if
        (book/verify-skip? b)
        ()
        (let
          ((path (book/row-path node))
            (src (as-str (book/f b "src")))
            (expected (string-trim (as-str (book/f b "expected")))))
          (guard
            (lambda (e) (list path src expected (book/guard-msg e)))
            (let
              ((got (string-trim (as-str (example-eval src)))))
              (if
                (string-eq got expected)
                ()
                (list path src expected got))))))))
  "One example row -> () (skip or pass), else a hole record (path src\n   expected got). Refusals/eval errors are holes too: an example that\n   cannot run in the reader's restricted env is drift — the got slot\n   then carries the message verbatim (\"example-eval: refused: …\" for\n   gate/budget refusals, \"unbound: …\" for eval errors).")

(define book/verify-page
  (lambda
    ()
    (let
      ((m (request-param "chapter")))
      (let
        ((slug (if (null? m) "" (string-trim (as-str m)))))
        (let
          ((root
            (if
              (string-eq slug "")
              "data.book.ch"
              (string-append "data.book.ch." slug))))
          (let
            ((rows (tree/subtree root 20000)))
            (if
              (null? rows)
              (if
                (string-eq slug "")
                (html/page
                  (list
                    (book/style-html)
                    (html/h1 "book verify")
                    (html/p*
                      (html/attr "class" "muted")
                      "No book rows seeded yet — run scripts/load-book.sh <export.book.json>.")))
                (book/not-found (string-append "chapter " slug)))
              (book/verify-page-html
                slug
                (count (book/verify-candidates rows))
                (book/verify-holes rows))))))))
  "GET /book/verify[?chapter=slug] — the verify report: every nopales\n   example with an expect trailer re-evaluated; mismatches/refusals\n   render as hole rows (habitat-book eval-examples).")

(define book/verify-page-html
  (lambda
    (slug n holes)
    (html/page
      (list
        (book/style-html)
        (html/p*
          (html/attr "class" "muted")
          (html/join (list (html/a "/book" "book") " · verify")))
        (html/h1 "book verify")
        (html/p*
          (html/attr "class" "muted")
          (html-escape
            (string-append
              (if
                (string-eq slug "")
                "the whole book · "
                (string-append "chapter " slug " · "))
              "checked "
              (as-str n)
              (if (= n 1) " example" " examples")
              " with expect trailers · holes "
              (as-str (count holes)))))
        (if
          (= (count holes) 0)
          (html/p*
            ""
            "all green — every expect trailer matches its render.")
          (html/join (list/map book/hole-html holes))))))
  "Report body for GET /book/verify: counts + one hole row per\n   mismatched/refused example.")

(define book/verify-skip?
  (lambda
    (b)
    (or
      (null? (book/f b "expected"))
      (not (string-eq (as-str (book/f b "lang")) "nopales"))))
  "Out of verify scope: no expect trailer, or an ocaml block\n   (captured, not live — habitat-book eval-examples v1).")

(define book/workshop-html
  (lambda
    (b mode)
    (if
      (string-eq mode "workshop")
      (html/join
        (list
          " "
          (book/status-badge (book/f b "status"))
          (book/verify-html (book/f b "verify"))
          (book/notes-html (book/f b "notes"))
          (book/untracked-html (book/f b "untracked"))
          (book/other-html (book/f b "other"))))
      ""))
  "Workshop-register scaffolding block for a row body (status/verify/\n   notes/untracked/other). Reader strips all of it.")
