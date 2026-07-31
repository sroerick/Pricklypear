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
