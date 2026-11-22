;; A macro that introduces a binding. Expansion must bind x for the walker;
;; without expansion this file would report x as unbound.
(defmacro with-bound (body)
  (list (quote let) (list (list (quote x) 1)) body))

(define (m)
  (with-bound (list x)))
