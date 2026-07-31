;; package spec — habitat-workflow lint queries over notes docs + bindings.
;; Spec: borge/habitat-workflow.borg (spec-checks / session-ritual).
;; Load: (load-library "spec") after vault/seed, or load-string this file.
;; Depends on notes, outline, list (and string-split / string-downcase prims).
;;
;; Note: (package …) form is omitted so load-string works on images whose
;; eval path already has package intercept only via load-library; exports
;; are prefix-convention (spec/*). Re-add (package spec (export …)) when
;; seeding via load-library on a current image.


(define spec/doc-slug
  (lambda (pkg)
    (string-append "spec/" (as-str pkg)))
  "Slug of the package intent notes doc.")

(define spec/doc
  (lambda (pkg)
    (notes/doc/get (spec/doc-slug pkg)))
  "notes/doc/get for spec/<pkg>, or ().")

(define spec/member?
  (lambda (x xs)
    (if (null? xs)
        #f
        (if (string-eq (as-str (car xs)) (as-str x))
            #t
            (spec/member? x (cdr xs)))))
  "Membership test on a list of strings.")

(define spec/unique
  (lambda (xs)
    (list/foldl
      (lambda (acc x)
        (if (spec/member? x acc) acc (cons (as-str x) acc)))
      (list)
      xs))
  "Dedup preserving reverse encounter order.")

(define spec/append
  (lambda (a b)
    (if (null? a) b (cons (car a) (spec/append (cdr a) b))))
  "Concatenate two lists.")

(define spec/outline-text
  (lambda (n)
    (let ((here
            (string-append
              (as-str (outline/title n))
              " "
              (as-str (outline/body n))
              " "))
          (kids (outline/kids n)))
      (list/foldl
        (lambda (acc k) (string-append acc (spec/outline-text k)))
        here
        kids)))
  "Flatten outline titles+bodies into one searchable string.")

(define spec/find-section
  (lambda (ol needle)
    (let ((want (string-downcase (as-str needle))))
      (car
        (list/filter
          (lambda (k)
            (or (string-eq (string-downcase (as-str (outline/id k))) want)
                (string-eq (string-downcase (as-str (outline/title k))) want)
                (string-contains?
                  (string-downcase (as-str (outline/title k)))
                  want)))
          (outline/kids ol)))))
  "First child of outline whose id/title matches needle (case-insensitive).")

(define spec/strip-punct
  (lambda (tok)
    (let ((s0 (as-str tok)))
      (if (= (string-length s0) 0)
          ""
          (let ((n0 (string-length s0))
                (a (substring s0 0 1)))
            (let ((s1
                    (if (or (string-eq a "(")
                            (string-eq a "[")
                            (string-eq a "'")
                            (string-eq a ","))
                        (substring s0 1 (- n0 1))
                        s0)))
              (let ((n1 (string-length s1)))
                (if (= n1 0)
                    ""
                    (let ((z1 (substring s1 (- n1 1) 1)))
                      (if (or (string-eq z1 ")")
                              (string-eq z1 "]")
                              (string-eq z1 ",")
                              (string-eq z1 ".")
                              (string-eq z1 ";")
                              (string-eq z1 ":"))
                          (substring s1 0 (- n1 1))
                          s1)))))))))
  "Trim one layer of common punctuation from a token.")

(define spec/token-binding?
  (lambda (tok)
    (let ((s (as-str tok)))
      (and (> (string-length s) 2)
           (string-contains? s "/")
           (not (string-eq s "->"))
           (not (string-eq s "/"))
           (not (string-contains? s " "))
           (not (string-eq (substring s 0 1) "/")))))
  "Heuristic: tokens that look like pkg/name bindings.")

(define spec/tokens-from-text
  (lambda (text)
    (list/filter
      spec/token-binding?
      (list/map
        spec/strip-punct
        (string-split (as-str text) " "))))
  "Split text on spaces; strip punct; keep pkg/name-looking tokens.")

(define spec/flow-entries
  (lambda (pkg)
    (let ((d (spec/doc pkg)))
      (if (null? d)
          (list)
          (let ((ol (car (cdr (cdr (cdr d)))))
                (sec (spec/find-section
                       (car (cdr (cdr (cdr d))))
                       "flows")))
            (if (null? sec)
                (list)
                (list/map
                  (lambda (c)
                    (list
                      (as-str (outline/title c))
                      (as-str (outline/body c))
                      (spec/unique
                        (spec/append
                          (spec/tokens-from-text (outline/title c))
                          (spec/tokens-from-text (outline/body c))))))
                  (outline/kids sec)))))))
  "List of (flow-title flow-body binding-tokens) from FLOWS section.")

(define spec/binding-resolves?
  (lambda (name)
    (not (null? (function-source (as-str name)))))
  "True when function-source returns a body.")

(define spec/flows-resolve
  (lambda (pkg)
    (let ((entries (spec/flow-entries pkg)))
      (if (null? (spec/doc pkg))
          (list
            (list "ERROR" (string-append "missing notes doc " (spec/doc-slug pkg)) (list)))
          (list/map
            (lambda (e)
              (let ((title (car e))
                    (body (car (cdr e)))
                    (toks (car (cdr (cdr e)))))
                (let ((missing
                        (list/filter
                          (lambda (n) (not (spec/binding-resolves? n)))
                          toks))
                      (ok
                        (list/filter spec/binding-resolves? toks)))
                  (list title body ok missing))))
            entries))))
  "Parse spec/<pkg> FLOWS; each entry → (title body resolved missing).")

(define spec/public-names
  (lambda (pkg)
    (let ((exports
            (package-exports (as-str pkg)))
          (all
            (package-bindings (as-str pkg))))
      ;; Prefer declared exports; fall back to all package bindings.
      (if (null? exports) all exports)))
  "Public surface: package-exports, else package-bindings.")

(define spec/doc-text
  (lambda (pkg)
    (let ((d (spec/doc pkg)))
      (if (null? d)
          ""
          (spec/outline-text (car (cdr (cdr (cdr d))))))))
  "Full searchable text of the package spec doc.")

(define spec/mentioned?
  (lambda (name text)
    (string-contains? (as-str text) (as-str name)))
  "True if name appears as substring in text.")

(define spec/doc-coverage
  (lambda (pkg)
    (list/filter
      (lambda (n) (null? (function-doc (as-str n))))
      (spec/public-names pkg)))
  "Public bindings with empty function-doc.")

(define spec/source-sig
  (lambda (name)
    (let ((s (function-source (as-str name))))
      (if (null? s) "" (as-str s))))
  "function-source text or empty.")

(define spec/names-redefined
  (lambda (pkg branch)
    ;; Compare public binding sources on BRANCH vs system/main.
    ;; Restores the prior current-branch afterward.
    (let ((prev (current-branch))
          (names (spec/public-names pkg))
          (br (as-str branch)))
      (let ((main-pairs
              (begin
                (checkout "system/main")
                (list/map
                  (lambda (n) (list n (spec/source-sig n)))
                  names)))
            (branch-pairs
              (begin
                (checkout br)
                (list/map
                  (lambda (n) (list n (spec/source-sig n)))
                  names))))
        (begin
          (if (string-eq (as-str prev) "")
              #t
              (checkout prev))
          (list/filter
            (lambda (n)
              (let ((m
                      (car
                        (list/filter
                          (lambda (p) (string-eq (car p) n))
                          main-pairs)))
                    (b
                      (car
                        (list/filter
                          (lambda (p) (string-eq (car p) n))
                          branch-pairs))))
                (and (not (null? m))
                     (not (null? b))
                     (not (string-eq (car (cdr m)) ""))
                     (not (string-eq (car (cdr b)) ""))
                     (not (string-eq (car (cdr m)) (car (cdr b)))))))
            names)))))
  "Public names whose function-source differs between system/main and branch.")

(define spec/unacknowledged
  (lambda (pkg branch)
    (let ((text (spec/doc-text pkg))
          (br (as-str branch))
          (redef
            (if (or (string-eq (as-str branch) "")
                    (string-eq (as-str branch) "system/main"))
                ;; No branch lens: treat all public names as candidates
                ;; that must at least be mentioned in the spec doc.
                (spec/public-names pkg)
                (spec/names-redefined pkg branch))))
      (list/filter
        (lambda (n) (not (spec/mentioned? n text)))
        redef)))
  "Public bindings redefined on branch (vs main) with no mention in spec/<pkg> text. If branch is empty/main, all unmentioned public names.")

(define spec/flows-missing-count
  (lambda (rows)
    (list/foldl
      (lambda (n row)
        (+ n (count (car (cdr (cdr (cdr row)))))))
      0
      rows))
  "Count missing bindings across flows-resolve rows.")

(define spec/lint
  (lambda (pkg branch)
    (let ((flows (spec/flows-resolve pkg))
          (undoc (spec/doc-coverage pkg))
          (unack (spec/unacknowledged pkg branch))
          (miss (spec/flows-missing-count (spec/flows-resolve pkg))))
      (list
        (list "pkg" (as-str pkg))
        (list "branch" (as-str branch))
        (list "spec-doc" (spec/doc-slug pkg))
        (list "flows" flows)
        (list "flow-missing-count" miss)
        (list "doc-coverage-empty" undoc)
        (list "doc-coverage-count" (count undoc))
        (list "unacknowledged" unack)
        (list "unacknowledged-count" (count unack)))))
  "Aggregate lint report for pkg on branch (soft warnings shape for promote).")

;; ---- /spec health page (habitat-workflow.spec-health) ----

(define spec/starts-with?
  (lambda (s p)
    (let ((ss (as-str s)) (pp (as-str p))
          (n (string-length (as-str p))))
      (and (>= (string-length ss) n)
           (string-eq (substring ss 0 n) pp))))
  "Prefix test.")

(define spec/packages-with-docs
  (lambda ()
    (list/map
      (lambda (row)
        ;; notes/list row: (id . fields-json-str)
        (let ((slug (as-str (dict-get (cdr row) "slug"))))
          (substring slug 5 (- (string-length slug) 5))))
      (list/filter
        (lambda (row)
          (spec/starts-with?
            (as-str (dict-get (cdr row) "slug"))
            "spec/"))
        (notes/list))))
  "Package names that have a notes doc slug spec/<pkg>.")

(define spec/assoc-get
  (lambda (report key)
    (let ((hit
            (list/filter
              (lambda (pair) (string-eq (as-str (car pair)) (as-str key)))
              report)))
      (if (null? hit) () (car (cdr (car hit))))))
  "Lookup key in (spec/lint) alist report.")

(define spec/lint-summary
  (lambda (pkg branch)
    (if (null? (spec/doc pkg))
        (string-append (as-str pkg) ": (no spec doc)")
        (let ((r (spec/lint pkg branch)))
          (string-append
            (as-str pkg) ": flow-holes="
            (as-str (spec/assoc-get r "flow-missing-count"))
            " undoc="
            (as-str (spec/assoc-get r "doc-coverage-count"))
            " unack="
            (as-str (spec/assoc-get r "unacknowledged-count"))))))
  "One-line soft warning string for promote checklist / UI.")

(define spec/promote-checklist
  (lambda (branch names)
    ;; Soft warnings for packages touched by the promote name set.
    (let ((pkgs
            (spec/unique
              (list/map
                (lambda (n)
                  (let ((s (as-str n)))
                    (if (string-contains? s "/")
                        (car (string-split s "/"))
                        "")))
                names))))
      (list/map
        (lambda (p)
          (if (string-eq (as-str p) "")
              "(root): skipped"
              (spec/lint-summary p branch)))
        (list/filter (lambda (p) (not (string-eq (as-str p) ""))) pkgs))))
  "List of soft-warning lines for promote of names on branch.")

(define spec/row-summary
  (lambda (pkg branch)
    (let ((r (spec/lint pkg branch)))
      (list
        (as-str pkg)
        (spec/assoc-get r "flow-missing-count")
        (spec/assoc-get r "doc-coverage-count")
        (spec/assoc-get r "unacknowledged-count"))))
  "(pkg flow-missing doc-empty unack) for one package.")

(define spec/td
  (lambda (s)
    (html/tag "td" "" (html-escape (as-str s))))
  "Table cell.")

(define spec/th
  (lambda (s)
    (html/tag "th" "" (html-escape (as-str s))))
  "Header cell.")

(define spec/tr
  (lambda (cells)
    (html/tag "tr" "" (html/join cells)))
  "Table row from cell html list.")

(define spec/status-emoji
  (lambda (miss undoc unack)
    (if (and (= miss 0) (= undoc 0) (= unack 0))
        "ok"
        (if (> miss 0) "flows!" "warn")))
  "Coarse phone-readable status label.")

(define spec/health-row-html
  (lambda (sum)
    (let ((pkg (car sum))
          (miss (car (cdr sum)))
          (undoc (car (cdr (cdr sum))))
          (unack (car (cdr (cdr (cdr sum))))))
      (spec/tr
        (list
          (spec/td pkg)
          (spec/td (spec/status-emoji miss undoc unack))
          (spec/td miss)
          (spec/td undoc)
          (spec/td unack)
          (html/tag
            "td" ""
            (html/a
              (string-append "/notes?slug=spec/" (as-str pkg))
              (string-append "spec/" (as-str pkg))))))))
  "One health table row.")

(define spec/health-table
  (lambda (branch)
    (let ((pkgs (spec/packages-with-docs))
          (br (as-str branch)))
      (if (null? pkgs)
          (html/p* (html/attr "class" "muted")
                   "No spec/<pkg> notes docs yet. Seed with B1 ritual.")
          (html/tag
            "table"
            (html/attr "class" "spec-health")
            (html/join
              (cons
                (spec/tr
                  (list
                    (spec/th "pkg")
                    (spec/th "status")
                    (spec/th "flow holes")
                    (spec/th "undoc")
                    (spec/th "unack")
                    (spec/th "spec")))
                (list/map
                  (lambda (p)
                    (spec/health-row-html (spec/row-summary p br)))
                  pkgs)))))))
  "Full health table HTML for packages with spec docs.")

(define spec/page
  (lambda ()
    (let ((br (current-branch)))
      (html/page
        (list
          (html/h1 "/spec health")
          (html/p*
            (html/attr "class" "muted")
            (string-append
              "Package intent lint (soft). Branch: "
              (as-str br)
              ". flow holes = unresolved FLOWS bindings; undoc = public "
              "bindings missing function-doc; unack = public names not "
              "mentioned in spec doc (or redefined vs main when branch set)."))
          (spec/health-table "")
          (html/p*
            (html/attr "class" "muted")
            "API: (spec/lint \"todo\" \"\") · (spec/flows-resolve \"todo\")")))))
  "Private GET /spec handler — phone-readable package health.")

(route "/spec" "spec/page")
