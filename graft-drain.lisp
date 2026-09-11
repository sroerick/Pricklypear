;;;; graft-drain.lisp -- one-shot cron drain: graft queue -> Pricklypear.
;;;; Portable: honor $NOPALES_DATA for the queue dir and $NOPALES_HOME for
;;;; the nopales.lisp location. Cron example:
;;;;   */10 * * * * sbcl --script $NOPALES_HOME/graft-drain.lisp
;;;; Drains pending graft entries in batched evals (idempotent by mid) and
;;;; appends one result line per run to <queue>/drain.log. Exit 1 on error.

(load (or (and (sb-ext:posix-getenv "NOPALES_HOME")
               (concatenate 'string (sb-ext:posix-getenv "NOPALES_HOME") "/nopales.lisp"))
          "/home/al/nopales/nopales.lisp"))

(defparameter *drain-log* (merge-pathnames "drain.log" nopales:*graft-dir*))

(handler-case
    (let ((result (nopales:graft-drain)))
      (with-open-file (f *drain-log* :direction :output
                                     :if-exists :append :if-does-not-exist :create)
        (format f "[~d] ~s~%" (get-universal-time) result)))
  (error (condition)
    (with-open-file (f *drain-log* :direction :output
                                   :if-exists :append :if-does-not-exist :create)
      (format f "[~d] ERROR ~a~%" (get-universal-time) condition))
    (sb-ext:exit :code 1)))