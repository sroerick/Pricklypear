;; package ops -- exported from live branch system/main
;; Sync: scripts/pp_sync.py pull|push

(define ops/code/__t
  (lambda () 1))

(define ops/code/apply-pairs
  (lambda
    (src pairs)
    (if
      (null? pairs)
      src
      (let
        ((pair (car pairs)))
        (ops/code/apply-pairs
          (string-replace
            src
            (as-str (car pair))
            (as-str (car (cdr pair))))
          (cdr pairs))))))

(define ops/code/default-pkgs
  (list
    "bible"
    "workout"
    "welcome"
    "layout"
    "todo"
    "dabar"
    "outline"
    "notes"
    "roam"
    "calendar"
    "flash"
    "chat"
    "signup"
    "bi"
    "contacts"
    "plan"
    "strongs"
    "weather"
    "ref"
    "ai"
    "ui"
    "menu"
    "html"
    "css"
    "ops"))

(define ops/code/define-src
  (lambda
    (name body-src)
    (load-string
      (string-append
        "(define "
        (as-str name)
        "\n  "
        (as-str body-src)
        "\n)"))))

(define ops/code/fix-mangled
  (lambda
    ()
    (list/filter
      (lambda
        (row)
        (not (string-eq (as-str (car (cdr row))) "clean")))
      (list/map ops/code/fix-name (ops/code/mangled-hits)))))

(define ops/code/fix-name
  (lambda
    (name)
    (if
      (or
        (string-eq (as-str name) "ops/code/mangled-pairs")
        (string-eq (as-str name) "ops/code/mangled-needles"))
      (list name "skip")
      (let
        ((src0 (function-source name)))
        (if
          (null? src0)
          (list name "missing")
          (let
            ((src (as-str src0)))
            (let
              ((new (ops/code/apply-pairs src ops/code/mangled-pairs)))
              (if
                (string-eq src new)
                (list name "clean")
                (begin
                  (ops/code/define-src name new)
                  (list name "fixed"))))))))))

(define ops/code/fix-names
  (lambda (names) (list/map ops/code/fix-name names)))

(define ops/code/fix-pkg
  (lambda
    (pkg)
    (list/filter
      (lambda
        (row)
        (not (string-eq (as-str (car (cdr row))) "clean")))
      (list/map
        ops/code/fix-name
        (ops/code/uniq
          (list/foldl
            (lambda
              (acc needle)
              (append acc (ops/code/scan-pkg pkg needle)))
            ()
            ops/code/mangled-needles))))))

(define ops/code/has-find-code?
  (lambda () (not (null? (function-source "find-code")))))

(define ops/code/mangled-hits
  (lambda
    ()
    (ops/code/uniq
      (list/foldl
        (lambda
          (acc needle)
          (append acc (ops/code/search-limit needle 200)))
        ()
        ops/code/mangled-needles))))

(define ops/code/mangled-needles
  (list
    "226134"
    "194183"
    "226128"
    "226151"
    "226150"
    "194171"
    "194187"
    "194176"
    "u2190"
    "u2192"
    "u00b7"
    "u2014"
    "u2013"
    "u2026"
    "u2019"
    "u2018"
    "u201c"
    "u201d"
    "u25c0"
    "u25b6"
    "u00ab"
    "u00bb"
    "u00b0"
    "u2191"
    "u2193"))

(define ops/code/mangled-pairs
  (list
    (list "226134144" "&larr;")
    (list "226134146" "&rarr;")
    (list "226134145" "&uarr;")
    (list "226134147" "&darr;")
    (list "194183" "&middot;")
    (list "226128148" "&mdash;")
    (list "226128147" "&ndash;")
    (list "226128166" "&hellip;")
    (list "226128153" "&rsquo;")
    (list "226128152" "&lsquo;")
    (list "226128156" "&ldquo;")
    (list "226128157" "&rdquo;")
    (list "226151128" "&#9664;")
    (list "226150182" "&#9654;")
    (list "194171" "&laquo;")
    (list "194187" "&raquo;")
    (list "194176" "&deg;")
    (list "u2190" "&larr;")
    (list "u2192" "&rarr;")
    (list "u2191" "&uarr;")
    (list "u2193" "&darr;")
    (list "u00b7" "&middot;")
    (list "u2014" "&mdash;")
    (list "u2013" "&ndash;")
    (list "u2026" "&hellip;")
    (list "u2019" "&rsquo;")
    (list "u2018" "&lsquo;")
    (list "u201c" "&ldquo;")
    (list "u201d" "&rdquo;")
    (list "u25c0" "&#9664;")
    (list "u25b6" "&#9654;")
    (list "u00ab" "&laquo;")
    (list "u00bb" "&raquo;")
    (list "u00b0" "&deg;")))

(define ops/code/scan-pkg
  (lambda
    (pkg needle)
    (list/filter
      (lambda
        (name)
        (let
          ((src (function-source name)))
          (if
            (null? src)
            #f
            (string-contains? (as-str src) (as-str needle)))))
      (package-bindings pkg))))

(define ops/code/scan-pkgs
  (lambda
    (pkgs needle limit)
    (let
      ((lim (parse-int (as-str limit))))
      (list/foldl
        (lambda
          (acc pkg)
          (if
            (>= (count acc) lim)
            acc
            (append acc (ops/code/scan-pkg pkg needle))))
        ()
        pkgs))))

(define ops/code/search
  (lambda (needle) (ops/code/search-limit needle 500)))

(define ops/code/search-limit
  (lambda
    (needle limit)
    (ops/code/scan-pkgs ops/code/default-pkgs needle limit)))

(define ops/code/uniq
  (lambda
    (xs)
    (list/foldl
      (lambda
        (acc x)
        (if (null? (member x acc)) (cons x acc) acc))
      ()
      xs)))
