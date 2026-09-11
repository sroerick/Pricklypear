;;;; graft-drain.lisp -- one-shot cron drain: graft queue -> Pricklypear.
;;;; Crontab (user al):
;;;;   */10 * * * * /usr/local/bin/sbcl --script /home/al/nopales/graft-drain.lisp
;;;; Loads ~/nopales/nopales.lisp, drains pending graft entries in batched
;;;; evals (idempotent by mid), appends one result line per run to
;;;; ~/.local/share/nopales/drain.log. Exit 1 on error (visible in cron.log).

(load "/home/al/nopales/nopales.lisp")

(defparameter *drain-log* #P"/home/al/.local/share/nopales/drain.log")

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