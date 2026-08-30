;; package book — the spec IS the book: borg chapters as live habitat pages.
;;
;; Spec: borge/habitat-book.borg (habitat-loader rows in; web-renderer out).
;; Rows are seeded by scripts/load-book.sh from `borge book export` under
;; data.book (type book-chapter, branch-free data.* namespace — git stays
;; authoritative for .borg, habitat-book decision 2). This package RENDERS
;; rows; it never parses .borg (one parser, two projections — borge stays
;; the only parser, decision 3).
;;
;; Kernel-dispatched routes (image/lib/http.ml, no route pins needed):
;;   GET /book                  -> book/page        merged index: borg chapters + package specs
;;   GET /book/chapter/<label>  -> book/chapter-page
;;   GET /book/code/<label>     -> book/code-page
;;   GET /book/<pkg>            -> spec/book-page   (kernel fallthrough, unchanged)
;;   GET /book/pkg/<pkg>        -> alias of /book/<pkg>
;;
;; Registers (decision 1 — mode is a render flag, never a document fork):
;;   reader (default): prose/headings/code/examples only.
;;   ?mode=workshop:   + status badges, verify blocks, agent notes,
;;                     untracked lines, unknown-node listings.
;;   Drift holes stay visible in BOTH registers (raw parse-failure
;;   listings, unresolved binding tokens).
;;
;; Depends (boot-seeded): std (string-split), html, list. The binding-token
;; machinery is REUSED from spec when loaded (spec/token-binding?,
;; spec/binding-resolves?) — the proven drift renderer is inherited, not
;; forked; without spec, prose renders plain (escaped) and everything else
;; works. Example blocks post to /book/eval-example — the run button goes
;; live with the example-eval prim (habitat-book eval-examples, P2.3).

;; ---------------------------------------------------------------- rows

(define book/row
  (lambda (path)
    (let ((n (tree/get path)))
      (if (null? n) () (dict-get n "body"))))
  "Body JSON (VStr) of the book tree row at path, or nil when absent.")

(define book/f
  (lambda (body field)
    (if (null? body) () (dict-get body field)))
  "Field of a row body JSON, or nil. dict-get maps JSON arrays/objects
   back to VStr JSON — wrap with json-parse for lists.")

(define book/row-path
  (lambda (node)
    (as-str (dict-get node "path")))
  "Tree path of one node-JSON row (tree/children-of element).")

(define book/children
  (lambda (parent-path)
    (tree/children-of parent-path))
  "Document-order children of a book row (rank order; tree/children-of's
   literal-h covering reaches chapter.h.<section> and section.h.<sub>).")

(define book/label-safe?
  (lambda (s)
    (book/label-chars? s 0 (string-length s)))
  "Nonempty [a-z0-9_]-only string (ltree label shape the loader writes).")

(define book/label-chars?
  (lambda (s i n)
    (if (= i n)
        (> n 0)
        (let ((c (substring s i 1)))
          (if (or (string-eq c "_")
                  (and (string>=? c "a") (string<=? c "z"))
                  (and (string>=? c "0") (string<=? c "9")))
              (book/label-chars? s (+ i 1) n)
              #f))))
  "Char walk for label-safe?.")

;; ---------------------------------------------------------------- request

(define book/mode
  (lambda ()
    (let ((m (request-param "mode")))
      (if (and (not (null? m)) (string-eq (as-str m) "workshop"))
          "workshop"
          "reader")))
  "Register of the active request: workshop when ?mode=workshop, else reader.")

(define book/path-suffix
  (lambda (prefix path)
    (let ((p (as-str path)))
      (let ((pre (as-str prefix)))
        (if (string-starts-with? p pre)
            (substring p (string-length pre) (- (string-length p) (string-length pre)))
            ""))))
  "Text after prefix, or empty when path lacks the prefix.")

(define book/fn-bound?
  (lambda (name)
    (not (null? (function-source name))))
  "True when a library binding has a function_def on the visible lens.")

;; ---------------------------------------------------------------- html bits

(define book/style-html
  (lambda ()
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
        ".book-error{color:#f88;margin-top:.3rem;font-size:.85rem}")))
  "Book-page styles (phone-honest; print-considerate by restraint).")

(define book/status-color
  (lambda (st)
    (let ((s (as-str st)))
      (if (string-eq s "implemented") "#4a9"
          (if (string-eq s "planned") "#c93"
              (if (string-eq s "deprecated") "#a66" "#888")))))
  "Badge color for a borge status.")

(define book/status-badge
  (lambda (st)
    (if (null? st)
        ""
        (html/span
          (html/attrs
            (list
              (html/attr "class" "book-status")
              (html/attr "style"
                         (string-append
                           "font-size:.7rem;border:1px solid #666;"
                           "border-radius:999px;padding:.05rem .5rem;color:"
                           (book/status-color st)))))
          (as-str st))))
  "Small status pill (workshop register only).")

(define book/toggle-html
  (lambda (base mode)
    (if (string-eq mode "workshop")
        (html/p*
          (html/attr "class" "muted")
          (html/join
            (list "workshop register · "
                  (html/a (string-append base "?mode=reader") "reader view"))))
        (html/p*
          (html/attr "class" "muted")
          (html/join
            (list "reader register · "
                  (html/a (string-append base "?mode=workshop") "workshop view"))))))
  "The register toggle — the 'spec vs textbook' switch made tangible.")

(define book/not-found
  (lambda (what)
    (status-response
      404
      (string-append
        "<h1>not found</h1><p>No " (html-escape (as-str what))
        " in the seeded book.</p>"
        "<p><a href=\"/book\">back to /book</a></p>")))
  "404 status-response for unknown book paths.")

;; ---------------------------------------------------------------- prose
;;
;; DECISION (2026-08-30, P2.2 dogfood — obituary): this section first
;; rendered prose with an interpreted per-word pipeline (book/split-piece
;; + book/piece-html via list/map, calling spec/token-binding? and
;; spec/binding-resolves? per word). That cost ~300 interpreter steps per
;; word — a 12.7k-word chapter needed ~4M steps against the 100k-step
;; page budget, so every chapter page died with "budget exceeded". The
;; per-word work now lives in TWO kernel scans (pure/prims_pure.ml):
;; (prose-tokens s) collects UNIQUE token-shaped cores in one OCaml pass;
;; (render-tokens s resolved holes) renders one paragraph in one pass.
;; Resolution STAYS in Nopales via spec/binding-resolves? (function-source)
;; at O(unique tokens), not O(words) — the drift decision is still made
;; by the proven spec machinery. The kernel shape test mirrors
;; spec/token-binding?; spec/ keeps the authoritative definition — keep
;; the two in lockstep (parity-tested in image/test/primitives_test.ml).
;; book/split-piece + book/piece-html + the has-prefix?/has-suffix?
;; helpers are retired with this change.

(define book/binding-classes
  (lambda (prose)
    (let ((toks (prose-tokens (as-str prose))))
      (list
        (list/filter spec/binding-resolves? toks)
        (list/filter (lambda (n) (not (spec/binding-resolves? n))) toks))))
  "(resolved holes) over prose's UNIQUE token-shaped cores: one kernel
   scan + O(unique tokens) function-source lookups, never O(words).")

(define book/para-html
  (lambda (para link? classes)
    (html/p*
      ""
      (if link?
          (render-tokens
            (as-str para) (car classes) (car (cdr classes)))
          (html-escape (as-str para)))))
  "One prose paragraph: kernel render-tokens (links + holes) when the
   spec machinery is loaded; plain escaping otherwise. html/p* keeps
   the kernel's HTML raw; the plain branch escapes manually (html/p
   would double-escape the render-tokens output).")

(define book/prose-html
  (lambda (prose link?)
    (let ((p (as-str prose)))
      (if (string-eq p "")
          ""
          (let ((classes (if link? (book/binding-classes p) (list () ()))))
            (html/join
              (list/map
                (lambda (para) (book/para-html para link? classes))
                (string-split p "\n\n")))))))
  "Prose text as paragraphs; binding tokens link/hole via the kernel
   render-tokens pass (drift-as-rendering inherited from spec).")

;; ---------------------------------------------------------------- scaffolding

(define book/verify-html
  (lambda (verify)
    (if (null? verify)
        ""
        (let ((items (json-parse (as-str verify))))
          (html/tag
            "details"
            (html/attr "class" "book-verify")
            (html/join
              (list
                (html/tag "summary" ""
                          (string-append "verify (" (as-str (count items)) ")"))
                (html/pre
                  (html-escape
                    (html/join
                      (list/map
                        (lambda (v) (string-append (as-str v) "\n"))
                        items))))))))))
  "Workshop: verify entries as a collapsed listing.")

(define book/notes-html
  (lambda (notes)
    (if (null? notes)
        ""
        (html/join
          (list/map
            (lambda (n)
              (html/tag
                "blockquote"
                (html/attr "class" "book-note")
                (html/join
                  (list
                    (html/p*
                      (html/attr "class" "muted")
                      (string-append
                        "agent note ("
                        (html-escape (as-str (dict-get n "by"))) ")"))
                    (html/p (as-str (dict-get n "body")))))))
            (json-parse (as-str notes))))))
  "Workshop: agent notes as blockquotes (html/p escapes its own body —
   no pre-escape, that would double-escape).")

(define book/untracked-html
  (lambda (untracked)
    (if (null? untracked)
        ""
        (html/tag
          "p"
          (html/attr "class" "muted")
          (html/tag
            "em"
            ""
            (html/join
              (list/map
                (lambda (u)
                  (html-escape
                    (string-append
                      "[untracked " (as-str (dict-get u "path")) ": "
                      (as-str (dict-get u "reason")) "] ")))
                (json-parse (as-str untracked))))))))
  "Workshop: untracked-path notes as italic lines.")

(define book/other-html
  (lambda (other)
    (if (null? other)
        ""
        (html/join
          (list/map
            (lambda (o)
              (html/div
                (html/attr "class" "book-other")
                (html/join
                  (list
                    (html/p*
                      (html/attr "class" "muted")
                      (string-append "unknown node kind: "
                                     (html-escape (as-str (dict-get o "kind")))))
                    (html/pre (html-escape (as-str (dict-get o "body"))))))))
            (json-parse (as-str other))))))
  "Workshop: unknown node kinds the loader could not classify (never
   silently dropped — the row body is the source of truth).")

(define book/raw-html
  (lambda (raw)
    (if (null? raw)
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

(define book/workshop-html
  (lambda (b mode)
    (if (string-eq mode "workshop")
        (html/join
          (list
            " "
            (book/status-badge (book/f b "status"))
            (book/verify-html (book/f b "verify"))
            (book/notes-html (book/f b "notes"))
            (book/untracked-html (book/f b "untracked"))
            (book/other-html (book/f b "other"))))
        "")
    )
  "Workshop-register scaffolding block for a row body (status/verify/
   notes/untracked/other). Reader strips all of it.")

;; ---------------------------------------------------------------- chapters

(define book/example-html
  (lambda (node mode)
    (let ((b (dict-get node "body")))
      (let ((lang (book/f b "lang"))
            (src (book/f b "src"))
            (expected (book/f b "expected")))
        (html/div
          (html/attr "class" "book-example")
          (html/join
            (list
              (html/pre (html/code (as-str src)))
              (if (string-eq (as-str lang) "nopales")
                  (html/button
                    (html/attrs
                      (list
                        (html/attr "class" "book-run")
                        (html/attr "hx-post" "/book/eval-example")
                        (html/attr "hx-vals"
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
              (if (null? expected)
                  ""
                  (html/p*
                    (html/attr "class" "book-expected")
                    (html-escape
                      (string-append "expect => " (as-str expected)))))))))))
  "Example block: nopales gets a run button (POST /book/eval-example,
   live with the example-eval prim); ocaml is captured output only
   (habitat-book eval-examples: no live OCaml evaluator in v1).")

(define book/section-html
  (lambda (node mode)
    (let ((b (dict-get node "body")))
      (let ((title (book/f b "title"))
            (level (as-str (book/f b "level")))
            (prose (book/f b "prose"))
            (raw (book/f b "raw")))
        (let ((tag (if (string-eq level "2") "h2"
                       (if (string-eq level "3") "h3" "h4"))))
          (html/join
            (list
              (html/tag tag "" (html-escape (as-str title)))
              (book/workshop-html b mode)
              (book/raw-html raw)
              (book/prose-html prose (book/fn-bound? "spec/token-binding?"))
              (book/children-html (book/row-path node) mode)))))))
  "One section/subsection: heading, register scaffolding, prose, then
   nested children (document order via tree rank).")

(define book/children-html
  (lambda (parent-path mode)
    (html/join
      (list/map
        (lambda (node)
          (let ((b (dict-get node "body")))
            (if (string-eq (as-str (dict-get b "node")) "example")
                (book/example-html node mode)
                (book/section-html node mode))))
        (book/children parent-path))))
  "Document-order children of a book row: sections (recursive) + examples.")

(define book/chapter-html
  (lambda (label path b mode)
    (let ((title (book/f b "title"))
          (prose (book/f b "prose"))
          (raw (book/f b "raw")))
      (html/page
        (list
          (book/style-html)
          (html/p*
            (html/attr "class" "muted")
            (html/join (list (html/a "/book" "book") " · chapter")))
          (book/toggle-html (string-append "/book/chapter/" label) mode)
          (html/h1 (html-escape (as-str title)))
          (book/workshop-html b mode)
          (book/raw-html raw)
          (book/prose-html prose (book/fn-bound? "spec/token-binding?"))
          (book/children-html path mode)
          (html/p*
            (html/attr "class" "muted")
            (html-escape
              (string-append
                "chapter " (as-str (book/f b "chapter"))
                " · seeded rows (git is authoritative for .borg)")))))))
  "One borg chapter page from tree rows.")

(define book/chapter-page
  (lambda ()
    (let ((label (book/path-suffix "/book/chapter/" (request-path))))
      (if (not (book/label-safe? label))
          (book/not-found "chapter")
          (let ((path (string-append "data.book.ch." label)))
            (let ((b (book/row path)))
              (if (null? b)
                  (book/not-found (string-append "chapter " label))
                  (book/chapter-html
                    label path b (book/mode))))))))
  "GET /book/chapter/<label> — headings/prose/code/examples from rows.")

;; ---------------------------------------------------------------- code

(define book/code-page
  (lambda ()
    (let ((label (book/path-suffix "/book/code/" (request-path))))
      (if (not (book/label-safe? label))
          (book/not-found "code chapter")
          (let ((b (book/row (string-append "data.book.code." label))))
            (if (null? b)
                (book/not-found (string-append "code chapter " label))
                (let ((p (book/f b "path"))
                      (lang (book/f b "lang"))
                      (src (book/f b "source")))
                  (html/page
                    (list
                      (book/style-html)
                      (html/p*
                        (html/attr "class" "muted")
                        (html/join
                          (list (html/a "/book" "book")
                                " · code appendix")))
                      (html/h1 (html-escape (as-str p)))
                      (html/p*
                        (html/attr "class" "muted")
                        (string-append "lang: " (as-str lang)))
                      (html/pre (html-escape (as-str src)))))))))))
  "GET /book/code/<label> — one code-chapter listing from its row.")

;; ---------------------------------------------------------------- index

(define book/chapter-li
  (lambda (node mode)
    (let ((b (dict-get node "body")))
      (let ((title (book/f b "title"))
            (status (book/f b "status")))
        (html/li
          ""
          (html/join
            (list
              (html/a
                (string-append "/book/chapter/" (tree/leaf (book/row-path node)))
                (html-escape (as-str title)))
              (if (string-eq mode "workshop")
                  (string-append " " (book/status-badge status))
                  "")))))))
  "Index row for one chapter (status badge in workshop register only).")

(define book/spec-index-html
  (lambda ()
    (if (book/fn-bound? "spec/packages-with-docs")
        (let ((pkgs (spec/packages-with-docs)))
          (if (null? pkgs)
              ""
              (html/join
                (list
                  (html/h2 (string-append "Package specs (" (as-str (count pkgs)) ")"))
                  (html/p*
                    (html/attr "class" "muted")
                    "spec/<pkg> intent docs — the other corpus this index merges (habitat-workflow.book-view).")
                  (html/ul
                    ""
                    (html/join
                      (list/map
                        (lambda (p)
                          (html/li
                            ""
                            (html/a (string-append "/book/" (as-str p)) (as-str p))))
                        pkgs)))))))
        ""))
  "Index of package spec docs (reuses spec/packages-with-docs; empty
   section when the spec package is not loaded).")

(define book/code-index-html
  (lambda ()
    (let ((code (book/children "data.book.code")))
      (if (null? code)
          (html/p*
            (html/attr "class" "muted")
            "No book rows seeded yet — run scripts/load-book.sh <export.book.json>.")
          (html/join
            (list
              (html/h2 (string-append "Code appendix (" (as-str (count code)) ")"))
              (html/tag
                "details"
                ""
                (html/join
                  (list
                    (html/tag "summary" "" "all code chapters (.pp / .sql / .ml)")
                    (html/ul
                      ""
                      (html/join
                        (list/map
                          (lambda (node)
                            (let ((b (dict-get node "body")))
                              (html/li
                                ""
                                (html/a
                                  (string-append
                                    "/book/code/"
                                    (tree/leaf (book/row-path node)))
                                  (html-escape (as-str (book/f b "path")))))))
                          code)))))))))))
  "Collapsed index of every code chapter (the export's appendix order).")

(define book/page
  (lambda ()
    (let ((mode (book/mode)))
      (let ((root-b (book/row "data.book"))
            (chapters (book/children "data.book.ch")))
        (let ((rev (if (null? root-b) "" (as-str (book/f root-b "rev"))))
              (book-root (if (null? root-b) "" (as-str (book/f root-b "root")))))
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
                    (if (string-eq rev "")
                        ""
                        (string-append ", rev " (substring rev 0 (min 8 (string-length rev)))))
                    (if (string-eq book-root "")
                        ""
                        (string-append ", root " book-root))
                    ").")))
              (book/toggle-html "/book" mode)
              (if (null? chapters)
                  (html/p*
                    (html/attr "class" "muted")
                    "No chapters seeded yet — run scripts/load-book.sh <export.book.json>.")
                  (html/join
                    (list
                      (html/h2 (string-append "Chapters (" (as-str (count chapters)) ")"))
                      (html/ul
                        ""
                        (html/join
                          (list/map
                            (lambda (node) (book/chapter-li node mode))
                            chapters))))))
              (book/spec-index-html)
              (book/code-index-html)))))))
  "GET /book — merged index: borg chapters (export order) + package
   spec docs + the code appendix.")
