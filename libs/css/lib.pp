;; package css -- exported from live branch system/main
;; Sync: scripts/pp_sync.py pull|push

(define css/media
  (lambda
    (query items)
    (list "media" (as-str query) items)))

(define css/prop-name
  (lambda (k) (as-str k)))

(define css/prop-value
  (lambda (v) (as-str v)))

(define css/raw
  (lambda (s) (list "raw" (as-str s))))

(define css/render
  (lambda
    (sheet)
    (if
      (string-eq (type-of sheet) "lambda")
      (css/render (sheet))
      (if
        (null? sheet)
        ""
        (if
          (string-eq (as-str (car sheet)) "sheet")
          (css/render-items (car (cdr sheet)))
          (css/render-item sheet))))))

(define css/render-decls
  (lambda
    (props)
    (if
      (null? props)
      ""
      (if
        (null? (cdr props))
        ""
        (string-append
          (css/prop-name (car props))
          ":"
          (css/prop-value (car (cdr props)))
          ";"
          (css/render-decls (cdr (cdr props))))))))

(define css/render-item
  (lambda
    (item)
    (if
      (null? item)
      ""
      (let
        ((kind (as-str (car item))))
        (if
          (string-eq kind "rule")
          (let
            ((sel (car (cdr item))) (props (car (cdr (cdr item)))))
            (string-append
              (as-str sel)
              "{"
              (css/render-decls props)
              "}\n"))
          (if
            (string-eq kind "media")
            (let
              ((q (car (cdr item))) (kids (car (cdr (cdr item)))))
              (string-append
                "@media("
                (as-str q)
                "){\n"
                (css/render-items kids)
                "}\n"))
            (if
              (string-eq kind "raw")
              (string-append (as-str (car (cdr item))) "\n")
              (if
                (string-eq kind "use")
                (css/render (car (cdr item)))
                (if
                  (string-eq kind "sheet")
                  (css/render-items (car (cdr item)))
                  "")))))))))

(define css/render-items
  (lambda
    (items)
    (list/foldl
      (lambda
        (acc it)
        (string-append acc (css/render-item it)))
      ""
      items)))

(define css/rule
  (lambda
    (selector props)
    (list "rule" (as-str selector) props)))

(define css/sheet
  (lambda (items) (list "sheet" items)))

(define css/use
  (lambda (sheet) (list "use" sheet)))
