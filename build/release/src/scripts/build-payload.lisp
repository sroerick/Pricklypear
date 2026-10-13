#!/usr/local/bin/sbcl --script
;; build-payload.lisp SRC.expr DST.json
;; Wrap a Nopales expression in a PP /api/eval POST body. Quote and
;; backslash characters are built at run time from code-char, so this
;; source file carries no escape sequences to mangle in transit.
;; Non-ASCII and control characters become uXXXX escapes (ASCII-safe).

(defparameter *q* (code-char 34))
(defparameter *b* (code-char 92))
(defparameter *nl* (code-char 10))
(defparameter *cr* (code-char 13))
(defparameter *tb* (code-char 9))

(defun two (a c) (concatenate 'string (string a) (string c)))

(defun esc-out (code out)
  (write-char *b* out)
  (format out "u~4,'0x" code))

(defun json-escape (s out)
  (loop for ch across s
        for code = (char-code ch)
        do (cond ((char= ch *q*) (write-string (two *b* *q*) out))
                 ((char= ch *b*) (write-string (two *b* *b*) out))
                 ((char= ch *nl*) (write-string (two *b* (code-char 110)) out))
                 ((char= ch *cr*) (write-string (two *b* (code-char 114)) out))
                 ((char= ch *tb*) (write-string (two *b* (code-char 116)) out))
                 ((or (< code 32) (> code 126)) (esc-out code out))
                 (t (write-char ch out)))))

(defun read-file (path)
  (with-open-file (in path)
    (let ((s (make-string (file-length in))))
      (read-sequence s in)
      s)))

(defun main ()
  (let* ((a sb-ext:*posix-argv*)
         (n (length a))
         (src (nth (- n 2) a))
         (dst (nth (- n 1) a))
         (expr (read-file src))
         (prefix (concatenate 'string "{" (string *q*) "expr"
                              (string *q*) ":" (string *q*)))
         (suffix (concatenate 'string (string *q*) "}")))
    (with-open-file (out dst :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create)
      (write-string prefix out)
      (json-escape expr out)
      (write-string suffix out))))

(main)
