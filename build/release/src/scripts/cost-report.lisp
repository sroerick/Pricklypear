#!/usr/local/bin/sbcl --script
;; cost-report.lisp -- weekly wire cost profile (pp/4, card 99979a7a).
;;
;; 1. POSTs the bench battery (cost-bench.expr) to /api/eval; bench
;;    returns avg microseconds per eval. Budget rule (roerick 09-24):
;;    every function under 100000 us.
;; 2. GETs /timing -- the server's page-timing / budget ledger (per
;;    route: eval steps, limit 400000, wall clock, DB share and per-prim
;;    costs; budget-share flags at >= 25 percent; each load probes up to
;;    15 unprobed GET routes).
;; 3. Upserts a "cost-profile" page note into both gardens (gregor token
;;    + admin deleg token). The board is the authority; the page is a
;;    read-only mirror.
;;
;; Cron: refresh-cost.sh, Mondays 16:03Z. Tokens are read from
;; ~/.config/pricklypear and never printed.

(defparameter *base* "https://pricklypear.rocks")
(defparameter *dir* "/home/al/pp-share")
(defparameter *budget-us* 100000)
(defparameter *step-budget* 400000)
(defparameter *top-routes* 8)

(defun trim (s) (string-trim '(#\Space #\Tab #\Newline #\Return) s))

(defun cat (&rest parts) (apply #'concatenate 'string parts))

(defun trunc (s n)
  (if (> (length s) n) (cat (subseq s 0 n) "...") s))

(defun read-first-line (path)
  (with-open-file (f path)
    (trim (read-line f))))

(defparameter *tok-gregor*
  (read-first-line "/home/al/.config/pricklypear/pp.token"))
(defparameter *tok-admin*
  (read-first-line "/home/al/.config/pricklypear/deleg-workouts.token"))

(defun run-curl (args)
  (let ((out (make-string-output-stream)))
    (let ((p (sb-ext:run-program "/usr/local/bin/curl" args
                                 :output out :error :output :wait t)))
      (values (get-output-stream-string out)
              (sb-ext:process-exit-code p)))))

(defun api-eval (token payload-path)
  (run-curl (list "-sS" "--max-time" "180"
                  "-H" (cat "Authorization: Bearer " token)
                  "-H" "Content-Type: application/json"
                  "-d" (cat "@" payload-path)
                  (cat *base* "/api/eval"))))

(defun api-get (token path)
  (run-curl (list "-sS" "--max-time" "120"
                  "-H" (cat "Authorization: Bearer " token)
                  (cat *base* path))))

(defun build-payload (src dst)
  (let ((p (sb-ext:run-program "/usr/local/bin/sbcl"
                               (list "--script"
                                     (cat *dir* "/build-payload.lisp")
                                     src dst)
                               :wait t)))
    (= 0 (sb-ext:process-exit-code p))))

;; ---- minimal JSON response reading -------------------------------

(defun read-json-string (s start)
  ;; s[start] is a double quote; return (values string index-after).
  (let ((out (make-string-output-stream))
        (i (1+ start))
        (n (length s)))
    (loop
      (when (>= i n) (error "unterminated JSON string"))
      (let ((c (char s i)))
        (cond
          ((char= c #\\)
           (incf i)
           (when (>= i n) (error "bad JSON escape"))
           (let ((e (char s i)))
             (case e
               (#\n (write-char #\Newline out))
               (#\t (write-char #\Tab out))
               (#\r (write-char #\Return out))
               (#\b (write-char (code-char 8) out))
               (#\f (write-char (code-char 12) out))
               (#\/ (write-char #\/ out))
               ((#\" #\\) (write-char e out))
               (#\u
                (let ((cp (parse-integer s :start (1+ i)
                                          :end (min n (+ i 5))
                                          :radix 16 :junk-allowed t)))
                  (when (and cp (>= cp 32) (< cp 65536))
                    (ignore-errors (write-char (code-char cp) out)))
                  (incf i 4)))
               (t (write-char e out))))
           (incf i))
          ((char= c #\")
           (return (values (get-output-stream-string out) (1+ i))))
          (t (write-char c out) (incf i)))))))

(defun json-string-member (s name)
  ;; Return the JSON string value of member NAME, or NIL.
  (let* ((key (cat "\"" name "\":"))
         (p (search key s)))
    (when p
      (let ((i (+ p (length key))))
        (loop while (and (< i (length s))
                         (member (char s i)
                                 '(#\Space #\Tab #\Newline #\Return)))
              do (incf i))
        (when (and (< i (length s)) (char= (char s i) #\"))
          (read-json-string s i))))))

(defun json-ok-p (s) (not (null (search "\"ok\":true" s))))

;; ---- /timing HTML scraping ----------------------------------------

(defun decode-entities (s)
  (let ((out (make-string-output-stream)) (i 0) (n (length s)))
    (loop while (< i n)
          do (let ((c (char s i)))
               (if (char= c #\&)
                   (let ((semi (position #\; s :start i
                                             :end (min n (+ i 12)))))
                     (if (null semi)
                         (progn (write-char c out) (incf i))
                         (let ((name (subseq s (1+ i) semi)))
                           (cond
                             ((string= name "amp") (write-char #\& out))
                             ((string= name "lt") (write-char #\< out))
                             ((string= name "gt") (write-char #\> out))
                             ((string= name "quot") (write-char #\" out))
                             ((string= name "nbsp") (write-char #\Space out))
                             ((string= name "mdash") (write-string "--" out))
                             ((string= name "ndash") (write-string "-" out))
                             ((string= name "times") (write-char #\x out))
                             ((and (> (length name) 1)
                                   (char= (char name 0) #\#))
                              (let ((cp (parse-integer name :start 1
                                                       :junk-allowed t)))
                                (when (and cp (>= cp 32) (< cp 65536))
                                  (ignore-errors
                                    (write-char (code-char cp) out)))))
                             (t nil))
                           (setf i (1+ semi)))))
                   (progn (write-char c out) (incf i)))))
    (get-output-stream-string out)))

(defun ascii-clean (s)
  (map 'string
       (lambda (c)
         (cond ((char= c (code-char 215)) #\x)     ; multiplication sign
               ((char= c (code-char 8212)) #\-)    ; em dash
               ((or (char= c (code-char 181))     ; micro
                    (char= c (code-char 956)))    ; greek mu
                #\u)
               (t (if (and (>= (char-code c) 32)
                           (<= (char-code c) 126))
                      c #\Space))))
       s))

(defun strip-tags (s)
  (let ((out (make-string-output-stream)) (i 0) (n (length s)) in-tag)
    (loop while (< i n)
          do (let ((c (char s i)))
               (cond ((char= c #\<) (setf in-tag t))
                     ((char= c #\>) (setf in-tag nil))
                     ((not in-tag) (write-char c out)))
               (incf i)))
    (decode-entities (get-output-stream-string out))))

(defun extract-cells (row)
  (let ((cells nil) (pos 0) (n (length row)))
    (loop
      (let ((td (search "<td" row :start2 pos)))
        (unless td (return))
        (let ((gt (position #\> row :start td)))
          (unless gt (return))
          (let ((end (search "</td>" row :start2 gt)))
            (unless end (return))
            (push (ascii-clean (trim (strip-tags
                                      (subseq row (1+ gt) end))))
                  cells)
            (setf pos (+ end 5))))))
    (nreverse cells)))

(defun extract-rows (html)
  (let ((rows nil) (pos 0) (n (length html)))
    (loop
      (let ((tr (search "<tr" html :start2 pos)))
        (unless tr (return))
        (let ((tre (search "</tr>" html :start2 tr)))
          (unless tre (return))
          (let ((row (subseq html tr (+ tre 5))))
            (when (search "<td" row)
              (push (extract-cells row) rows)))
          (setf pos (+ tre 5)))))
    (nreverse rows)))

(defun parse-timing (html)
  (let ((sampled nil) (unprobed nil))
    (dolist (cells (extract-rows html))
      (cond ((and (= (length cells) 11)
                  (ignore-errors
                    (parse-integer (nth 2 cells) :junk-allowed t)))
             (push cells sampled))
            ((and (= (length cells) 6)
                  (member (nth 3 cells)
                          (list "-" "--" (string (code-char 8212)))
                          :test #'string=))
             (push cells unprobed))))
    (list (nreverse sampled) (nreverse unprobed))))

(defun row-max-ms (cells)
  (or (ignore-errors (read-from-string (nth 6 cells))) 0))

;; ---- report sections ----------------------------------------------

(defun join (list sep)
  (with-output-to-string (o)
    (loop for (x . more) on list
          do (write-string x o)
             (when more (write-string sep o)))))

(defun utc-now ()
  (multiple-value-bind (s m h d mo y)
      (decode-universal-time (get-universal-time) 0)
    (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0dZ"
            y mo d h m s)))

(defun battery-section (value)
  (let ((lines nil))
    (dolist (seg (split-on value " | "))
      (let* ((seg (trim seg))
             (p (position #\= seg)))
        (when p
          (let* ((label (subseq seg 0 p))
                 (numstr (trim (subseq seg (1+ p))))
                 (num (ignore-errors (read-from-string numstr))))
            (push
             (if (and (numberp num) (> num 0))
                 (format nil "* ~a: ~a us -- ~a"
                          label (format nil "~d" (round num))
                         (if (> num *budget-us*)
                             (format nil "OVER budget (~ax the ~a us limit)"
                                     (max 1 (round num *budget-us*))
                                     *budget-us*)
                             (format nil "under the ~a us budget" *budget-us*)))
                 (format nil "* ~a: ~a" label numstr))
             lines)))))
    (cat "Wire bench battery via /api/eval (gregor garden). bench returns
average microseconds per eval; budget rule: every function under
100000 us.

"
         (join (nreverse lines)
               "
")
         "

The pure-call floor (~2 us) is interpreter overhead; judge the
DB lines by their margin over it.")))

(defun split-on (s sep)
  (let ((parts nil) (start 0) (n (length s)) (m (length sep)))
    (loop
      (let ((p (search sep s :start2 start)))
        (if p
            (progn (push (subseq s start p) parts)
                   (setf start (+ p m)))
            (progn (push (subseq s start) parts) (return)))))
    (nreverse parts)))

(defun route-line (cells)
  (let* ((route (trunc (nth 0 cells) 48))
         (steps (nth 3 cells))
         (budget (nth 4 cells))
         (avg (nth 5 cells))
         (maxms (nth 6 cells))
         (dbshare (nth 9 cells))
         (prims (nth 10 cells))
         (bnum (ignore-errors (parse-integer budget :junk-allowed t)))
         (flag (and bnum (>= bnum 25))))
    (cat "* " route ": steps " steps " (" budget " of step budget"
         (if flag " -- FLAG >= 25%" "")
         ") | avg " avg " ms, max " maxms " ms | db " dbshare
         (if (or (string= prims "") (string= prims "--"))
             ""
             (cat " | prims: " (trunc prims 140))))))

(defun timing-section (sampled unprobed)
  (let ((sorted (sort (copy-list sampled) #'> :key #'row-max-ms)))
    (let ((top (subseq sorted 0 (min *top-routes* (length sorted)))))
      (cat
       "Server /timing ledger (slowest-first by max wall clock). Steps are
the handler's own eval-budget counter (limit 400000); the budget
column is that share, flagged at >= 25%. Each /timing load probes up
to 15 unprobed GET routes, so coverage accumulates across runs.

"
       (if (null top)
           "(no sampled routes yet)
"
           (join (mapcar #'route-line top)
                 "
"))
       "

Unprobed routes this pass:
"
       (if (null unprobed)
           "(none)
"
           (join (mapcar (lambda (c)
                           (cat "* " (nth 0 c) " -- " (nth 1 c)
                                " (" (nth 2 c) ")"))
                         unprobed)
                 "
"))))))

(defun nopales-escape (s)
  ;; Real newline bytes pass through (the Nopales reader accepts them
  ;; inside string literals); backslash and quote get Nopales escapes,
  ;; built from code-char so this source carries no backslash maze.
  (let ((bs (code-char 92)) (q (code-char 34)))
    (with-output-to-string (o)
      (loop for c across s
            do (cond ((char= c bs)
                      (write-char bs o) (write-char bs o))
                     ((char= c q)
                      (write-char bs o) (write-char q o))
                     ((char= c #\Newline) (write-char c o))
                     ((char= c #\Return) nil)
                     (t (write-char c o)))))))

(defun note-body (battery-value sampled unprobed)
  (cat
   "Weekly wire cost profile, generated by cost-report.lisp on vultr.guest
(pp/4, card 99979a7a). Budget rule (roerick 09-24): every function
under 100000 us bench time; route handlers under 25% of the 400000-step
eval budget. Generated: "
   (utc-now) ".

-- Wire bench battery --

"
   (battery-section battery-value)
   "

-- Route timing ledger (/timing) --

"
   (timing-section sampled unprobed)
   "

Method notes: bench numbers are warm per-call averages from the
gregor garden via /api/eval; a cold first call reads 1.4-1.6x higher.
The kanban board is the authority; this page is a read-only mirror.
"))

(defun write-file (path s)
  (with-open-file (o path :direction :output :if-exists :supersede
                          :if-does-not-exist :create)
    (write-string s o)))

(defun main ()
  ;; 1. bench battery on the wire
  (unless (build-payload (cat *dir* "/cost-bench.expr") "/tmp/cost-bench.json")
    (error "build-payload failed for cost-bench.expr"))
  (multiple-value-bind (body bcode)
      (api-eval *tok-gregor* "/tmp/cost-bench.json")
    (unless (= bcode 0) (error "curl failed for the bench battery"))
    (unless (json-ok-p body)
      (error "battery eval failed: ~a" (trunc body 300)))
    (let ((battery-value (json-string-member body "value")))
      (unless battery-value (setf battery-value "(battery value missing)"))
      ;; 2. route timing ledger
      (multiple-value-bind (html tcode) (api-get *tok-gregor* "/timing")
        (unless (= tcode 0) (error "curl failed for /timing"))
          (when (search "timing panel is admin-only" html)
            (error "/timing returned 403 (timing panel is admin-only): ledger scrape blocked, lock landed 09-28..10-02; refusing to post a silently-empty timing section - read the 09-28 snapshot in the cost-profile note instead"))
        (let ((parsed (parse-timing html)))
          (let ((sampled (first parsed)) (unprobed (second parsed)))
            ;; 3. page upsert, both gardens
            (let* ((body-text (note-body battery-value sampled unprobed))
                   (expr (cat
                          "(seq (load-library \"notes\")"
                          " (notes/upsert \"cost-profile\" \"Cost profile\""
                          " (list \"cproot\" \"Cost profile\" \""
                          (nopales-escape body-text)
                          "\" \"\" (list))"
                          " \"cost-profile\" \"page\" \"\"))"))
                   (failures 0))
              (write-file "/tmp/cost-note.expr" expr)
              (unless (build-payload "/tmp/cost-note.expr" "/tmp/cost-note.json")
                (error "build-payload failed for the cost note"))
              (dolist (garden (list (list "gregor" *tok-gregor*)
                                    (list "admin" *tok-admin*)))
                (multiple-value-bind (gbody gcode)
                    (api-eval (second garden) "/tmp/cost-note.json")
                  (write-file (cat *dir* "/last-cost-" (first garden) ".json")
                              gbody)
                  (if (and (= gcode 0) (json-ok-p gbody))
                      (format t "~a: ok~%" (first garden))
                      (progn
                        (format t "~a: FAILED ~a~%"
                                (first garden) (trunc gbody 200))
                        (incf failures)))))
              (format t "routes sampled: ~a, unprobed: ~a~%"
                      (length sampled) (length unprobed))
              (if (> failures 0)
                  (sb-ext:exit :code 1)
                  (sb-ext:exit :code 0)))))))))

(main)
