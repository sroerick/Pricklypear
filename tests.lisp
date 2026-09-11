;;;; tests.lisp -- equivalence battery: local interpreter vs live PP.
;;;; Run in the worker: (load "~/nopales/nopales.lisp") (load "~/nopales/tests.lisp") (battery) (store-battery)
;;;; Uses type gregor-memory in cactus-ops; all test rows are deleted at the end.

(in-package :nopales)

(defparameter *pass* 0)
(defparameter *fail* 0)

(defun report (name ok remote local)
  (if ok
      (progn (incf *pass*) (format t "PASS ~a~%" name))
      (progn (incf *fail*)
             (format t "DIFF ~a~%  remote: ~s~%  local:  ~s~%" name remote local))))

(defun fields-only (rows) (mapcar #'second rows))

(defun normalize-ids (value)
  (typecase value
    (string
     (if (and (= (length value) 36)
              (eql (char value 8) #\-)
              (every (lambda (c) (or (digit-char-p c) (find c "abcdefABCDEF-"))) value))
         "ID"
         value))
    (cons (cons (normalize-ids (car value)) (normalize-ids (cdr value))))
    (t value)))

(defun sorted (list) (sort (copy-list list) #'string< :key (lambda (x) (princ-to-string x))))

(defun battery ()
  (setf *pass* 0 *fail* 0)
  (load-env)
  (local-reset)
  ;; clean both planes
  (wipe-remote "gregor-memory")
  (pp-memory-ensure-space)
  (let ((content (format nil "line1~%line2 \"quoted\" back\\slash ~a ünïcode" (code-char 233))))
    ;; 1. create + find-rows + numeric-string + tricky content
    (run `(create "gregor-memory" ,(json-object (list (cons "k" "one") (cons "n" "080") (cons "content" content)))) :local)
    (run `(create "gregor-memory" ,(json-object (list (cons "k" "one") (cons "n" "080") (cons "content" content)))) :remote)
    (run `(create "gregor-memory" ,(json-object (list (cons "k" "two")))) :local)
    (run `(create "gregor-memory" ,(json-object (list (cons "k" "two")))) :remote)
    (let ((l (fields-only (normalize-ids (run `(find-rows "gregor-memory" "k" "one") :local))))
          (r (fields-only (normalize-ids (run `(find-rows "gregor-memory" "k" "one") :remote)))))
      (report "find-rows one row (content/080 round-trip)" (equal (normalize l) (normalize r)) r l))
    ;; 2. quote-style expr
    (let ((l (fields-only (normalize-ids (run '(find-rows (quote gregor-memory) (quote k) "one") :local))))
          (r (fields-only (normalize-ids (run '(find-rows (quote gregor-memory) (quote k) "one") :remote)))))
      (report "find-rows quote-style" (equal (normalize l) (normalize r)) r l))
    ;; 3. count
    (let ((l (run '(count (find-rows (quote gregor-memory) (quote k) "one")) :local))
          (r (run '(count (find-rows (quote gregor-memory) (quote k) "one")) :remote)))
      (report "count" (equal (princ-to-string l) (princ-to-string r)) r l))
    ;; 4. find-rows-author triples
    (let ((l (mapcar #'cdr (normalize-ids (run '(find-rows-author (quote gregor-memory) (quote k) "one") :local))))
          (r (mapcar #'cdr (normalize-ids (run '(find-rows-author (quote gregor-memory) (quote k) "one") :remote)))))
      (report "find-rows-author" (equal (normalize l) (normalize r)) r l))
    ;; 5. find-row + missing row
    (let ((l (ignore-errors (json-decode (run '(find-row (quote gregor-memory) (quote k) "one") :local))))
          (r (run '(find-row (quote gregor-memory) (quote k) "one") :remote)))
      (report "find-row" (equal (normalize l) (normalize r)) r l))
    (let ((l (run '(find-row (quote gregor-memory) (quote k) "nope") :local))
          (r (run '(find-row (quote gregor-memory) (quote k) "nope") :remote)))
      (report "find-row missing" (equal (normalize l) (normalize r)) r l))
    ;; 6. get existing + missing
    (let* ((lid (first (first (run '(find-rows (quote gregor-memory) (quote k) "one") :local))))
           (rid (first (first (run '(find-rows (quote gregor-memory) (quote k) "one") :remote)))))
      (let ((l (ignore-errors (json-decode (run `(get "gregor-memory" ,lid) :local))))
            (r (run `(get "gregor-memory" ,rid) :remote)))
        (report "get" (equal (normalize l) (normalize r)) r l)))
    ;; 7. update-field via per-backend script
    (let ((lid (first (first (run '(find-rows (quote gregor-memory) (quote k) "two") :local))))
          (rid (first (first (run '(find-rows (quote gregor-memory) (quote k) "two") :remote)))))
      (run `(update-field "gregor-memory" ,lid "extra" "090") :local)
      (run `(update-field "gregor-memory" ,rid "extra" "090") :remote)
      (let ((l (fields-only (normalize-ids (run '(find-rows (quote gregor-memory) (quote k) "two") :local))))
            (r (fields-only (normalize-ids (run '(find-rows (quote gregor-memory) (quote k) "two") :remote)))))
        (report "update-field + read back (090)" (equal (normalize l) (normalize r)) r l))
      ;; 8. update (full replace)
      (run `(update "gregor-memory" ,lid ,(json-object (list (cons "k" "two") (cons "v" "replaced")))) :local)
      (run `(update "gregor-memory" ,rid ,(json-object (list (cons "k" "two") (cons "v" "replaced")))) :remote)
      (let ((l (fields-only (normalize-ids (run '(find-rows (quote gregor-memory) (quote k) "two") :local))))
            (r (fields-only (normalize-ids (run '(find-rows (quote gregor-memory) (quote k) "two") :remote)))))
        (report "update full replace" (equal (normalize l) (normalize r)) r l))
      ;; 9. delete + gone
      (let ((l (run `(delete "gregor-memory" ,lid) :local))
            (r (run `(delete "gregor-memory" ,rid) :remote)))
        (report "delete returns id" (and (stringp l) (stringp r)) r l))
      (let ((l (run '(count (find-rows (quote gregor-memory) (quote k) "two")) :local))
            (r (run '(count (find-rows (quote gregor-memory) (quote k) "two")) :remote)))
        (report "after delete count=0" (equal (princ-to-string l) (princ-to-string r)) r l)))
    ;; 10. delete missing
    (let ((l (run '(delete (quote gregor-memory) "00000000-0000-0000-0000-000000000000") :local))
          (r (run '(delete (quote gregor-memory) "00000000-0000-0000-0000-000000000000") :remote)))
      (report "delete missing" (equal (normalize l) (normalize r)) r l))
    ;; 11. spaces (battery-owned space; production spaces differ by design)
    (run '(space-create (quote nopales-battery)) :local)
    (ignore-errors (run '(space-create (quote nopales-battery)) :remote))
    (let ((l (run '(space-exists? "nopales-battery") :local))
          (r (run '(space-exists? "nopales-battery") :remote)))
      (report "space-exists?" (equal (normalize l) (normalize r)) r l))
    (let ((l (member "nopales-battery" (normalize (run '(space-list) :local)) :test #'string=))
          (r (member "nopales-battery" (normalize (run '(space-list) :remote)) :test #'string=)))
      (report "space-list contains battery space" (and l r) r l))
    ;; cleanup remote test rows
    (wipe-remote "gregor-memory")
    (format t "~%battery: ~a pass, ~a fail~%" *pass* *fail*)
    (values *pass* *fail*)))

(defun wipe-remote (&optional (type "gregor-memory"))
  (ignore-errors
   (dolist (row (pp-eval (format nil "(rows (quote ~a) 500)" type)))
     (pp-eval (format nil "(delete (quote ~a) ~s)" type (first row))))))

;;;; -- memory store round-trip

(defun fake-record (id updated-at)
  (list :memory
        :version 1
        :id id
        :created-at 3997000000
        :updated-at updated-at
        :scope :workspace
        :workspace "/home/al/"
        :title (format nil "test memory ~a" id)
        :content (format nil "body of ~a~%second line \"quoted\" ~a" id (code-char 233))
        :tags (list "ops" "glochid")
        :source-conversation "T-test"))

(defun rec-get (record key)
  "GETF over a tagged (:memory ...) record's properties."
  (getf (if (eq (first record) :memory) (cdr record) record) key))

(defun store-battery ()
  (setf *pass* 0 *fail* 0)
  (load-env)
  (let* ((rec1 (fake-record "TESTMEM-AAAA" 3997100000))
         (rec2 (fake-record "TESTMEM-BBBB" 3997200000)))
    (wipe-remote "gregor-memory")
    (pp-memory-ensure-space)
    (pp-memory-upsert rec1)
    (pp-memory-upsert rec2)
    ;; replace rec1 (newer)
    (setf (getf (cdr rec1) :updated-at) 3997300000)
    (pp-memory-upsert rec1)
    (let* ((pulled (pp-memory-pull))
             (by-id (lambda (mid) (find-if (lambda (e) (string= (rec-get (getf e :record) :id) mid)) pulled)))
           (p1 (funcall by-id "TESTMEM-AAAA"))
           (p2 (funcall by-id "TESTMEM-BBBB")))
      (report "pull count" (= (length pulled) 2) (length pulled) 2)
        (report "rec1 round-trip"
                (and p1
                     (string= (rec-get (getf p1 :record) :content) (rec-get rec1 :content))
                     (equal (rec-get (getf p1 :record) :tags) (rec-get rec1 :tags))
                     (= (rec-get (getf p1 :record) :updated-at) 3997300000)
                     (eq (rec-get (getf p1 :record) :scope) :workspace)
                     (string= (rec-get (getf p1 :record) :workspace) "/home/al/"))
                (getf p1 :record) rec1)
        (let ((r (rec-get (getf p2 :record) :title))
              (l (rec-get rec2 :title)))
        (report "rec2 round-trip" (and p2 (string= r l)) r l))
      ;; forget rec2
      (pp-memory-delete "TESTMEM-BBBB")
      (let ((after (pp-memory-pull)))
        (report "after delete count=1" (= (length after) 1) (length after) 1))
      (wipe-remote "gregor-memory")
      (format t "~%store: ~a pass, ~a fail~%" *pass* *fail*)
      (values *pass* *fail*))))
