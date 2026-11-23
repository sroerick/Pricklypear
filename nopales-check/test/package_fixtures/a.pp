(package a
  (version "1.0.0")
  (requires b)
  (export a/x))

(define (a/x) (b/y))
