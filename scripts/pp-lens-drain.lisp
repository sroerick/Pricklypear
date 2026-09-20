;;;; pp-lens-drain.lisp -- one-shot cron drain: tombstone queue -> Pricklypear.
;;;;
;;;; The lens sync pushes local node WRITES but does not propagate local
;;;; DELETIONS (verified 2026-09-20: a removed agenda item's server row
;;;; survived `pp-local sync`). Autolith therefore enqueues one tombstone file
;;;; per deletion under $PP_LENS_TOMBSTONES (default
;;;; ~/.local/share/autolith/pp-lens-tombstones), named
;;;; <type>.<dashed-node-id>.tombstone, and this script turns each into an
;;;; idempotent wire delete through pp-local.
;;;;
;;;; GOTCHAS baked in (both verified 2026-09-20):
;;;;   - the wire delete resolves the DASHED node id (2cd5dd5e-8ad5-...),
;;;;     not the dash-stripped form that names the node file;
;;;;   - pp-local has no `progn` (PP's `let*` = one body; use `seq`), so each
;;;;     deletion is its own pp-local call.
;;;;
;;;; Cron example (paired with the */15 pp-local sync):
;;;;   */10 * * * * /usr/local/bin/sbcl --script /home/al/pp-lens/drain.lisp
;;;;          >> /home/al/pp-lens/drain-cron.log 2>&1
;;;;
;;;; A deleted tombstone file is consumed; a failed delete leaves its
;;;; tombstone queued (deleting an already-gone address returns (), so
;;;; retries converge). Malformed names are skipped and logged. Pure Common
;;;; Lisp: no ASDF/UIOP under `sbcl --script`. One result line per run to
;;;; drain.log; exit 1 when any wire delete failed.

(defparameter *tombstone-dir*
  (or (let ((env (sb-ext:posix-getenv "PP_LENS_TOMBSTONES")))
        (and env (plusp (length env))
             (pathname (concatenate 'string env "/"))))
      (merge-pathnames ".local/share/autolith/pp-lens-tombstones/"
                       (user-homedir-pathname))))

(defparameter *pp-local*
  (or (let ((env (sb-ext:posix-getenv "PP_LOCAL")))
        (and env (plusp (length env)) env))
      "/home/al/.local/bin/pp-local"))

(defparameter *drain-log* "/home/al/pp-lens/drain.log")

(defun log-line (control &rest args)
  (with-open-file (f *drain-log* :direction :output
                                 :if-exists :append
                                 :if-does-not-exist :create)
    (format f "[~d] ~?~%" (get-universal-time) control args)))

(defun dashed-address-p (string)
  "A dash-stripped-free dashed uuid shape: 8-4-4-4-12 lowercase hex."
  (and (= (length string) 36)
       (char= (char string 8) #\-) (char= (char string 13) #\-)
       (char= (char string 18) #\-) (char= (char string 23) #\-)
       (every (lambda (c)
                (or (digit-char-p c) (find c "abcdef")))
              (remove #\- string))))

(defun tombstone-type-and-address (pathname)
  "Parse agent-agenda.2cd5dd5e-....tombstone into (values type address), or
NIL when the name is not a well-formed tombstone of a dashed node id."
  (let ((name (pathname-name pathname)))
    (when name
      (let ((dot (position #\. name)))
        (when dot
          (let ((type (subseq name 0 dot))
                (address (subseq name (1+ dot))))
            (when (and (plusp (length type))
                       (dashed-address-p address))
              (values type address))))))))

(handler-case
    (let ((files (sort (directory (merge-pathnames "*.tombstone"
                                                   *tombstone-dir*))
                       #'string< :key #'namestring)))
      (cond
        ((null files)
         (log-line "nothing queued"))
        (t
         (let ((failures 0) (drained 0))
           (dolist (file files)
             (multiple-value-bind (type address)
                 (tombstone-type-and-address file)
               (cond
                 ((null type)
                  (log-line "SKIP malformed tombstone ~a" (namestring file)))
                 (t
                  (let* ((process (sb-ext:run-program
                                  *pp-local*
                                  (list "-e"
                                        (format nil "(delete (quote ~A) \"~A\")"
                                                type address))
                                  :output nil :error nil :wait t))
                        (code (and process (sb-ext:process-exit-code process))))
                    (if (and code (= code 0))
                        (progn
                          (ignore-errors (delete-file file))
                          (incf drained))
                        (progn
                          (incf failures)
                          (log-line "KEEP ~a: wire delete exit ~s"
                                    (pathname-name file) code))))))))
           (log-line "drained ~d tombstone(s), ~d failure(s)"
                     drained failures)
           (when (plusp failures)
             (sb-ext:exit :code 1))))))
  (error (condition)
    (log-line "ERROR ~a" condition)
    (sb-ext:exit :code 1)))
