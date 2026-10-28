;; package treecalc -- tree calculus interpreter (Bader 2024 triage rules)
;; for PricklyPear. Spec: swarm-demo/treecalc/PLAN.md. Reduction oracle:
;; Johannes Bader's eager-stacks reference evaluator (vendored read-only
;; under tuna-fix/reference/tree-calculus); every vector in
;; libs/treecalc/test/ was cross-checked against it.
;;
;; Representation (plan section 3): a tree is the PP list of its children
;; stored in REVERSE order -- head = leftmost child -- mirroring the
;; eager-stacks Tree[] convention:
;;   leaf  = ()            bare triangle
;;   stem  = (u)           one child
;;   fork  = (a b)         two children (car = leftmost)
;;   spine = node with >= 3 children (implicit application; children 4..
;;           are extra arguments a rule result keeps)
;; Applying E to F attaches F as the right-most child of E's root:
;;   (tc/app e f) = (append e (list f)).
;;
;; Rules (x y z = the leftmost three children of a spine, r = the rest):
;;   (1)   x = leaf                 -> y's children ++ r    (z discarded)
;;   (2)   x = stem(w)              -> w's children ++ (z (y z) ++ r);
;;          the new redex (y z) reduces fully before the parent re-fires
;;   (3a)  x = fork(w x2), z = leaf      -> w's children ++ r
;;   (3b)  x = fork(w x2), z = stem(u)   -> x2's children ++ (u ++ r)
;;   (3c)  x = fork(w x2), z = fork(u v) -> y's children ++ (u v ++ r)
;;
;; The oracle mutates spine arrays in place and queues sub-redexes on a
;; todo stack; this port reduces those sub-redexes by RECURSION instead --
;; the same eager order (a queued (y z) redex reduces fully before the
;; parent re-fires) with no worklist bookkeeping. Open children (length
;; >= 3: nested redexes from the parser) reduce on demand exactly where
;; the plan's pseudocode says "no rule fires now; z reduces via its own
;; todo entry". On contract-conforming input (all non-top-level subterms
;; already values, which is what tc/from-ternary always produces) the
;; machine agrees with the oracle rule for rule and step for step.
;;
;; Step budget: the PP eval budget is 400k steps (lisp/lib/budget.ml) and
;; PP's Budget_exceeded deliberately propagates through guard, so tc/eval
;; carries its OWN tc-step counter and raises (error "tc-out-of-steps")
;; at exactly its limit; tc/eval's guard turns that into
;; {status "out-of-steps", steps = limit}. MEASURED on the 400k build
;; (receipts slice S3): the omega term (S I I)(S I I) trips the PP budget
;; between 4700 and 4800 TC steps (~84 PP steps per TC step for
;; rule-2-heavy work), so tc-default-limit is 4000 -- a default tc/eval
;; returns out-of-steps cleanly instead of leaking PP's budget error.
;; The canonical size program needs 25096 TC steps (oracle count), about
;; 2.1M PP steps at that ratio: it exceeds ANY tc-limit under the 400k
;; build and completes only on a build with a >= ~2.2M budget; the
;; pending 400k->2M bump (factory/todos-budget-1028) is marginal for it.
;; Its oracle-verified output is recorded in the test receipts.
;;
;; NOTE: PP has unrelated OCaml prims named tree/*; every entry here is
;; tc/*. Never define a bare "tree" entry.

(package treecalc (export tc/app tc/from-text tc/to-text tc/from-ternary tc/to-ternary tc/from-minbin tc/to-minbin tc/size tc/nat tc/true tc/false tc/k tc/s tc/i tc/is-zero tc/eval tc/eval-arg tc/eval-limit tc/step tc/trace tc/trace-arg))

;; ---------- representation ----------

(define tc/app
  (lambda (e f) (append e (list f))))

;; real booleans without relying on reader boolean literals
(define tc-true (equal? '() '()))
(define tc-false (not (equal? '() '())))

;; ---------- human-readable text ----------
;; Grammar: atom+ left-assoc application; atom = t | triangle | (expr).
;; "t t y z" is the spine (t y z): the leading triangle is the root, the
;; second token is the spine's first child (= the machine's x).

(define tc-ws?
  (lambda (c)
    (or (string-eq c " ") (string-eq c "\t") (string-eq c "\n") (string-eq c "\r"))))

;; char-walk tokenizer -> token string list, left to right
(define tc-tok
  (lambda (s i n cur acc)
    (if (< i n)
        (let ((c (substring s i 1)))
          (if (tc-ws? c)
              (tc-tok s (+ i 1) n "" (if (string-eq cur "") acc (append acc (list cur))))
              (if (or (string-eq c "(") (string-eq c ")"))
                  (tc-tok s (+ i 1) n ""
                          (append (if (string-eq cur "") acc (append acc (list cur)))
                                  (list c)))
                  (tc-tok s (+ i 1) n (string-append cur c) acc))))
        (if (string-eq cur "") acc (append acc (list cur))))))

(define tc/tokenize
  (lambda (s) (tc-tok s 0 (string-length s) "" '())))

(define tc-tri?
  (lambda (tok)
    (let ((t (string-downcase tok)))
      (or (string-eq t "t") (string-eq t "triangle")))))

;; parse-expr: tokens -> (tree . rest); bad shapes raise via error
(define tc/parse-expr
  (lambda (toks)
    (let ((first (tc/parse-atom toks)))
      (tc/parse-app (car first) (cdr first)))))

(define tc/parse-app
  (lambda (acc toks)
    (if (null? toks)
        (cons acc '())
        (if (string-eq (car toks) ")")
            (cons acc toks)
            (let ((next (tc/parse-atom toks)))
              (tc/parse-app (append acc (list (car next))) (cdr next)))))))

(define tc/parse-atom
  (lambda (toks)
    (if (null? toks)
        (error "tc: unexpected end of input")
        (if (string-eq (car toks) "(")
            (let ((inner (tc/parse-expr (cdr toks))))
              (if (or (null? (cdr inner)) (not (string-eq (car (cdr inner)) ")")))
                  (error "tc: expected )")
                  (cons (car inner) (cdr (cdr inner)))))
            (if (tc-tri? (car toks))
                (cons '() (cdr toks))
                (error "tc: bad token"))))))

(define tc/from-text
  (lambda (s)
    (let ((r (tc/parse-expr (tc/tokenize s))))
      (if (null? (cdr r))
          (car r)
          (error "tc: trailing tokens")))))

;; render: leaf -> "t"; node -> "t" then " " + wrap(child) per child, where
;; a non-leaf child is parenthesized (paren-free stems would re-associate).
(define tc/wrap
  (lambda (c)
    (if (null? c) "t" (string-append "(" (tc/to-text c) ")"))))

(define tc-kids-text
  (lambda (kids)
    (if (null? kids)
        ""
        (string-append " " (tc/wrap (car kids)) (tc-kids-text (cdr kids))))))

(define tc/to-text
  (lambda (v)
    (if (null? v) "t" (string-append "t" (tc-kids-text v)))))

;; ---------- ternary (values only) ----------
;; preorder arity encoding: leaf=0, stem=1<child>, fork=2<left><right>.

;; keep only 0/1/2 chars (whitespace-tolerant like the reference encodings)
(define tc-tern-clean
  (lambda (s i n acc)
    (if (< i n)
        (let ((c (substring s i 1)))
          (if (or (string-eq c "0") (string-eq c "1") (string-eq c "2"))
              (tc-tern-clean s (+ i 1) n (string-append acc c))
              (tc-tern-clean s (+ i 1) n acc)))
        acc)))

;; parse: s i n -> (tree . next-index)
(define tc-tern-parse
  (lambda (s i n)
    (if (< i n)
        (let ((c (substring s i 1)))
          (if (string-eq c "0")
              (cons '() (+ i 1))
              (if (string-eq c "1")
                  (let ((r (tc-tern-parse s (+ i 1) n)))
                    (cons (list (car r)) (cdr r)))
                  (if (string-eq c "2")
                      (let ((r1 (tc-tern-parse s (+ i 1) n)))
                        (let ((r2 (tc-tern-parse s (cdr r1) n)))
                          (cons (append (list (car r1)) (list (car r2))) (cdr r2))))
                      (error "tc: bad ternary char")))))
        (error "tc: truncated ternary"))))

(define tc/from-ternary
  (lambda (s)
    (let ((clean (tc-tern-clean s 0 (string-length s) "")))
      (let ((r (tc-tern-parse clean 0 (string-length clean))))
        (if (= (cdr r) (string-length clean))
            (car r)
            (error "tc: trailing ternary chars"))))))

(define tc/to-ternary
  (lambda (v)
    (if (null? v)
        "0"
        (if (= (length v) 1)
            (string-append "1" (tc/to-ternary (car v)))
            (if (= (length v) 2)
                (string-append "2" (tc/to-ternary (car v)) (tc/to-ternary (car (cdr v))))
                (error "tc/to-ternary: not a value (arity > 2)"))))))

;; ---------- minbin (expressions) ----------
;; '1' = leaf, '0' = application node (a b): explicit-application binary
;; tree, preorder. Implicit node (c1..ck) = left-assoc application chain;
;; a bare stem (c) is t applied to c.

(define tc-mb-parse
  (lambda (s i n)
    (if (< i n)
        (let ((c (substring s i 1)))
          (if (string-eq c "1")
              (cons '() (+ i 1))
              (if (string-eq c "0")
                  (let ((a (tc-mb-parse s (+ i 1) n)))
                    (let ((b (tc-mb-parse s (cdr a) n)))
                        (cons (append (car a) (list (car b))) (cdr b))))
                  (error "tc: bad minbin char"))))
        (error "tc: truncated minbin"))))

(define tc/from-minbin
  (lambda (s)
    (let ((r (tc-mb-parse s 0 (string-length s))))
      (if (= (cdr r) (string-length s))
          (car r)
          (error "tc: trailing minbin chars")))))

;; left-assoc fold from the implicit root t: acc = the encoding of the
;; application chain so far; each child ci -> "0" ++ acc ++ enc(ci). The
;; spine's root t is the initial acc ("1"), so a k-child spine encodes
;; the explicit binary tree t c1 c2 ... ck -- reference-checked against
;; the conventions' strong round-trips ('1', identity '001010111',
;; '00111' = t t t = fork(t,t)).
(define tc-mb-fold
  (lambda (rest acc)
    (if (null? rest)
        acc
        (tc-mb-fold (cdr rest) (string-append "0" acc (tc/to-minbin (car rest)))))))

(define tc/to-minbin
  (lambda (v)
    (if (null? v)
        "1"
        (tc-mb-fold v "1"))))

;; ---------- single-rule fire (used by tc/step) ----------
;; fire one rule on a spine; -> (rule new-spine); rule "1"|"2"|"3a"|"3b"|"3c"
(define tc/fire
  (lambda (s)
    (let ((x (car s)) (y (car (cdr s))) (z (car (cdr (cdr s)))))
      (let ((r (cdr (cdr (cdr s)))))
        (if (null? x)
            (list "1" (append y r))
            (if (= (length x) 1)
                (list "2" (append (car x) (append (list z) (append (list (append y (list z))) r))))
                (if (null? z)
                    (list "3a" (append (car x) r))
                    (if (= (length z) 1)
                        (list "3b" (append (car (cdr x)) (append (list (car z)) r)))
                        (list "3c" (append y (append (list (car z)) (append (list (car (cdr z))) r))))))))))))

;; ---------- reduction engine (eager-stacks port) ----------
;; tc/eval-spine reduces one spine to a value: -> (value . steps).
;; Out of steps raises (error "tc-out-of-steps"); tc/eval's guard turns
;; that into {status "out-of-steps"} with steps = the limit exactly.

(define tc/eval-spine
  (lambda (s steps limit)
    (if (< (length s) 3)
        (cons s steps)
        (if (< steps limit)
            (let ((x (car s)) (y (car (cdr s))) (z (car (cdr (cdr s)))))
              (let ((r (cdr (cdr (cdr s)))))
                (if (< (length x) 3)
                    (if (null? x)
                        ;; (1)
                        (tc/eval-spine (append y r) (+ steps 1) limit)
                        (if (= (length x) 1)
                            ;; (2): the (y z) redex reduces fully first
                            (let ((red (tc/eval-spine (append y (list z)) (+ steps 1) limit)))
                              (tc/eval-spine (append (car x)
                                                     (append (list z) (append (list (car red)) r)))
                                             (cdr red) limit))
                            ;; (3): x is a fork; triage on z
                            (if (< (length z) 3)
                                (if (null? z)
                                    ;; (3a)
                                    (tc/eval-spine (append (car x) r) (+ steps 1) limit)
                                    (if (= (length z) 1)
                                        ;; (3b)
                                        (tc/eval-spine (append (car (cdr x)) (append (list (car z)) r)) (+ steps 1) limit)
                                        ;; (3c)
                                        (tc/eval-spine (append y (append (list (car z)) (append (list (car (cdr z))) r))) (+ steps 1) limit)))
                                ;; z open: reduce it first, then re-dispatch
                                (let ((rz (tc/eval-spine z steps limit)))
                                  (tc/eval-spine (append (list x) (append (list y) (append (list (car rz)) r)))
                                                 (cdr rz) limit)))))
                    ;; x open: reduce it first, then re-dispatch
                    (let ((rx (tc/eval-spine x steps limit)))
                      (tc/eval-spine (append (list (car rx)) (cdr s)) (cdr rx) limit)))))
            (error "tc-out-of-steps")))))

;; ---------- public eval surface ----------

(define tc-default-limit 4000)

;; (tc/eval program)             eval program alone
;; (tc/eval-arg program arg)     apply program to arg
;; (tc/eval-limit program limit) explicit tc-step limit (budget tests)
;; -> alist ((status . "ok"|"out-of-steps") (steps . N) (value . tree))
;; PP's reader has no variadic lambda syntax (no dotted params), so the
;; optional forms are separate fixed-arity entries over one core.
(define tc-eval-core
  (lambda (program arg limit)
    (let ((expr (if (equal? arg 'tc-none) program (append program (list arg)))))
      (guard
       (lambda (e)
         (if (string-eq (car (cdr e)) "tc-out-of-steps")
             (list (cons 'status "out-of-steps")
                   (cons 'steps limit)
                   (cons 'value '()))
             (error (car (cdr e)))))
       (let ((r (tc/eval-spine expr 0 limit)))
         (list (cons 'status "ok")
               (cons 'steps (cdr r))
               (cons 'value (car r))))))))

(define tc/eval
  (lambda (program) (tc-eval-core program 'tc-none tc-default-limit)))

(define tc/eval-arg
  (lambda (program arg) (tc-eval-core program arg tc-default-limit)))

(define tc/eval-limit
  (lambda (program limit) (tc-eval-core program 'tc-none limit)))

;; (tc/step state) -> alist {rule, tree, done}: one reduce-one on a spine
;; whose children are values (the visualizer's contract); a value reports
;; done. rule is "1" "2" "3a" "3b" "3c" or "" when already done.
(define tc/step
  (lambda (state)
    (if (< (length state) 3)
        (list (cons 'rule "") (cons 'tree state) (cons 'done tc-true))
        (let ((r (tc/fire state)))
          (list (cons 'rule (car r))
                (cons 'tree (car (cdr r)))
                (cons 'done tc-false))))))

(define tc-trace-cap 200)

;; trace-mode fire: record (rule snew), stop at the cap, else continue
(define tc/t-fire
  (lambda (rule snew steps limit trace)
    (let ((trace2 (append trace (list (list rule snew)))))
        (if (not (< (length trace2) tc-trace-cap))
          (list 'capped snew steps trace2)
          (tc/trace-spine snew steps limit trace2)))))

;; trace-mode engine: -> (status value steps trace), status 'ok|'out|'capped
(define tc/trace-spine
  (lambda (s steps limit trace)
    (if (< (length s) 3)
        (list 'ok s steps trace)
        (if (< steps limit)
            (let ((x (car s)) (y (car (cdr s))) (z (car (cdr (cdr s)))))
              (let ((r (cdr (cdr (cdr s)))))
                (if (< (length x) 3)
                    (if (null? x)
                        (tc/t-fire "1" (append y r) (+ steps 1) limit trace)
                        (if (= (length x) 1)
                            (let ((red (tc/trace-spine (append y (list z)) (+ steps 1) limit trace)))
                              (if (equal? (car red) 'ok)
                                  (tc/t-fire "2"
                                             (append (car x) (append (list z) (append (list (car (car (cdr red))) r))))
                                             (car (cdr (cdr red))) limit (car (cdr (cdr (cdr red)))))
                                  red))
                            (if (< (length z) 3)
                                (if (null? z)
                                    (tc/t-fire "3a" (append (car x) r) (+ steps 1) limit trace)
                                    (if (= (length z) 1)
                                        (tc/t-fire "3b" (append (car (cdr x)) (append (list (car z)) r)) (+ steps 1) limit trace)
                                        (tc/t-fire "3c" (append y (append (list (car z)) (append (list (car (cdr z))) r))) (+ steps 1) limit trace)))
                                (let ((rz (tc/trace-spine z steps limit trace)))
                                  (if (equal? (car rz) 'ok)
                                      (tc/trace-spine (append (list x) (append (list y) (append (list (car (car (cdr rz))) r))))
                                                      (car (cdr (cdr rz))) limit (car (cdr (cdr (cdr rz)))))
                                        rz)))))
                    (let ((rx (tc/trace-spine x steps limit trace)))
                      (if (equal? (car rx) 'ok)
                          (tc/trace-spine (append (list (car (car (cdr rx)))) (cdr s))
                                          (car (cdr (cdr rx))) limit (car (cdr (cdr (cdr rx)))))
                          rx)))))
            (list 'out s steps trace)))))

;; (tc/trace program) | (tc/trace-arg program arg)
;; -> PP list of (rule tree) snapshots, oldest first, capped at 200
(define tc-trace-core
  (lambda (program arg limit)
    (let ((expr (if (equal? arg 'tc-none) program (append program (list arg)))))
      (car (cdr (cdr (cdr (tc/trace-spine expr 0 limit '()))))))))

(define tc/trace
  (lambda (program) (tc-trace-core program 'tc-none tc-default-limit)))

(define tc/trace-arg
  (lambda (program arg) (tc-trace-core program arg tc-default-limit)))

;; ---------- helpers ----------

;; node count (1 per node)
(define tc-size-kids
  (lambda (kids)
    (if (null? kids) 0 (+ (tc/size (car kids)) (tc-size-kids (cdr kids))))))

(define tc/size
  (lambda (v)
    (if (null? v) 1 (+ 1 (tc-size-kids v)))))

;; natural n as a chain of n stems (0 = leaf), per the conventions
(define tc/nat
  (lambda (n)
    (if (= n 0) '() (list (tc/nat (- n 1))))))

;; ---------- canonical values + is-zero ----------

(define tc/true (list '()))              ; stem(tri) = "10"
(define tc/false '())                    ; leaf = "0"
(define tc/k (tc/from-ternary "10"))     ; K  = triangle triangle
(define tc/i (tc/from-ternary "21100"))  ; identity (oracle-verified)
(define tc/s (tc/from-ternary "212000")) ; S x y z = x z (y z) (oracle-verified)

;; triage{l,s,f} = t (t l s) f: applied to n it case-analyzes n
;; (leaf -> l, stem(u) -> s u, fork(u v) -> f u v)
(define tc/triage
  (lambda (l s f)
    (tc/app (tc/app '() (tc/app (tc/app '() l) s)) f)))

(define tc-church-false (tc/app tc/k tc/i))       ; K I
(define tc-kfalse (tc/app tc/k tc-church-false))  ; K (K I)
(define tc-k2false (tc/app tc/k tc-kfalse))       ; K (K (K I))

;; is-zero via triage{true, K false, K2 false} (plan S2)
(define tc/is-zero
  (lambda (n)
    (tc/app (tc/triage tc/true tc-kfalse tc-k2false) n)))
