;; verify/ — habitat self-tests (run ON the image via eval)
;; Prefer this over growing Python smoke frameworks.
;;
;; Each suite is a zero-arg thunk returning a list of
;; ("PASS" name) | ("FAIL" name). The ksh driver runs ONE suite
;; per eval so the 100k step budget is not shared across suites.
;;
;;   (load-library "verify")
;;   (verify/suite-extract-links)
;;   (verify/summary-of (verify/suite-list-hof))

(define verify/pass
  (lambda (name) (list "PASS" name))
  "Result tuple (PASS name).")

(define verify/fail
  (lambda (name) (list "FAIL" name))
  "Result tuple (FAIL name).")

(define verify/check
  (lambda (name pred)
    (if pred (verify/pass name) (verify/fail name)))
  "PASS name if pred truthy else FAIL name.")

(define verify/failed?
  (lambda (r) (string-eq (as-str (car r)) "FAIL"))
  "True if result car is FAIL.")

(define verify/count-fails
  (lambda (rs)
    (list/foldl
      (lambda (n r) (if (verify/failed? r) (+ n 1) n))
      0 rs))
  "Count FAIL results in rs.")

(define verify/fail-names
  (lambda (rs)
    (list/foldl
      (lambda (acc r)
        (if (verify/failed? r) (cons (car (cdr r)) acc) acc))
      (list)
      rs))
  "List of names from FAIL results.")

(define verify/summary-of
  (lambda (rs)
    (let ((nfails (verify/count-fails rs)))
      (if (= nfails 0)
          (string-append "ALL PASSED (" (as-str (count rs)) " checks)")
          (string-append "FAILED: " (str (verify/fail-names rs))))))
  "Human summary string: ALL PASSED or FAILED: names.")

;; ---------- suites (each intended as one eval call) ----------

(define verify/suite-extract-links
  (lambda ()
    (list
      (verify/check "extract-links three"
        (= (count (extract-links "see [[a]] and [[b|Bee]] [[media:u|x]]")) 3))
      (verify/check "extract-links zero"
        (= (count (extract-links "no links")) 0))
      (verify/check "extract-links unclosed"
        (= (count (extract-links "text [[oops")) 0))
      (verify/check "extract-links empty brackets"
        (= (count (extract-links "[[]]")) 0))
      (verify/check "render-links wiki"
        (string-contains?
          (render-links "hi [[a|A]]")
          "/notes?slug=a"))
      (verify/check "render-links media"
        (string-contains?
          (render-links "[[media:uuid-1|alt]]")
          "note-media"))))
  "Suite: notes link extraction checks.")

(define verify/suite-list-hof
  (lambda ()
    (list
      (verify/check "list/map"
        (string-eq
          (str (list/map (lambda (x) (* x 2)) (list 1 2 3)))
          "(2 4 6)"))
      (verify/check "list/filter"
        (string-eq
          (str (list/filter (lambda (x) (= x 2)) (list 1 2 3 2)))
          "(2 2)"))
      (verify/check "list/foldl sum"
        (= (list/foldl (lambda (a x) (+ a x)) 0 (list 1 2 3 4)) 10))))
  "Suite: list higher-order helper checks.")

(define verify/suite-budget-parse
  (lambda ()
    (let ((body
            (list/foldl
              (lambda (a i)
                (string-append a "lorem [[p" (as-str i) "]] x "))
              ""
              (list/range 500))))
      (let ((n (count (roam/parse-links body)))
            (left (budget-remaining)))
        (list
          (verify/check "parse-links 500" (= n 500))
          (verify/check "parse budget floor" (> left 20000))))))
  "Suite: notes budget parse checks.")

(define verify/suite-outline-render
  (lambda ()
    (let ((kids
            (list/map
              (lambda (i)
                (list
                  (string-append "n" (as-str i))
                  (string-append "T" (as-str i))
                  (string-append "body [[x" (as-str i) "]] text")
                  ""
                  (list)))
              (list/range 25))))
      (let ((tree (list "root" "R" "" "" kids)))
        (let ((html (outline/html "verify-render" tree #f))
              (left (budget-remaining)))
          (list
            (verify/check "outline/html 25 nodes length"
              (> (string-length (as-str html)) 1000))
            (verify/check "outline/html wiki rendered"
              (string-contains? (as-str html) "/notes?slug=x0"))
            (verify/check "outline/html budget floor"
              (> left 5000))
            (verify/check "extra-actions no per-node refile/options"
              (not
                (string-contains?
                  (function-source "outline/extra-actions")
                  "refile/options"))))))))
  "Suite: outline render checks.")

(define verify/suite-upsert-reindex
  (lambda ()
    (let ((slug "verify-lib-upsert-tmp")
          (pad
            (list/foldl
              (lambda (a i)
                (string-append a "abcdefghijklmnopqrstuvwxyz0123 "))
              ""
              (list/range 100))))
      (let ((mk
              (lambda (i)
                (list
                  (string-append "n" (as-str i))
                  "t"
                  (string-append pad " [[L" (as-str i) "]] " pad)
                  ""
                  (list)))))
        (let ((tree
                (list "root" "V" "" ""
                  (list (mk 0) (mk 1) (mk 2) (mk 3) (mk 4)
                        (mk 5) (mk 6) (mk 7) (mk 8) (mk 9)))))
          (let ((up (notes/doc/upsert slug "V" tree "verify" "note" ""))
                (nlinks
                  (count
                    (find-rows (quote note-link) "from_slug" slug 100)))
                (left (budget-remaining)))
            (seq
              (notes/doc/delete slug)
              (list
                (verify/check "upsert ok"
                  (string-eq (as-str up) slug))
                (verify/check "reindex 10 links" (= nlinks 10))
                (verify/check "upsert budget floor"
                  (> left 10000)))))))))
  "Suite: notes upsert and reindex checks.")

(define verify/suite-extra-actions-contract
  (lambda ()
    (list
      (verify/check "extra-actions is extract-only"
        (let ((src (function-source "outline/extra-actions")))
          (if (string-contains? src "refile/form")
              #f
              (string-contains? src "extract"))))))
  "Suite: outline extra-actions contract checks.")

;; Named suite table for drivers.
(define verify/suite-names
  (list
    "extract-links"
    "list-hof"
    "budget-parse"
    "outline-render"
    "upsert-reindex"
    "extra-actions-contract")
  "List of suite name strings for drivers.")

(define verify/run-suite
  (lambda (name)
    (cond
      ((string-eq name "extract-links") (verify/suite-extract-links))
      ((string-eq name "list-hof") (verify/suite-list-hof))
      ((string-eq name "budget-parse")
       (seq (load-library "notes") (verify/suite-budget-parse)))
      ((string-eq name "outline-render")
       (seq (load-library "outline") (load-library "notes")
            (verify/suite-outline-render)))
      ((string-eq name "upsert-reindex")
       (seq (load-library "notes") (verify/suite-upsert-reindex)))
      ((string-eq name "extra-actions-contract")
       (seq (load-library "notes") (verify/suite-extra-actions-contract)))
      (else (list (verify/fail (string-append "unknown-suite:" name))))))
  "Dispatch suite by name; loads deps as needed.")
