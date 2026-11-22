(define (k xs)
  (let* ((a (car xs))
         (b (cdr xs)))
    (append a b)))

(define (c x)
  (cond ((null? x) 0)
        (else 1)))

(define (g2 x)
  (guard (lambda (e) 0) x))

(defmacro with-name (name body)
  body)
