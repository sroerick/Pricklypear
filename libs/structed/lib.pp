;; structed/ — pure higher-order tree-surgery algebra (nopales).
;; Spec: borge/structural-editing.borg → structed-algebra.
;; No DB, no I/O. Load: (load-library "structed")
;;
;; One op implementation serves any tree whose domain supplies child
;; accessors: (structed/slurp-forward get-kids set-kids tree target).
;; Outline passes (outline/kids, outline/set-kids); the AST editor
;; passes expr-children accessors. Do NOT duplicate op logic per domain.
;;
;; Every op returns (list tree' selection') and is TOTAL: an unknown
;; target, missing sibling, or wrong shape returns the input tree and
;; unchanged selection. A structure key must never produce an error.

;; ---------------------------------------------------------------------------
;; list helpers (pure; the kernel has cons/car/cdr/count/append/reverse)
;; ---------------------------------------------------------------------------

(define structed/list/take
  (lambda (xs n)
    (if (or (null? xs) (<= n 0))
        (list)
        (cons (car xs) (structed/list/take (cdr xs) (- n 1)))))
  "First n elements of xs.")

(define structed/list/drop
  (lambda (xs n)
    (if (or (null? xs) (<= n 0))
        xs
        (structed/list/drop (cdr xs) (- n 1))))
  "xs without the first n elements.")

(define structed/list/nth
  (lambda (xs i)
    (if (null? xs)
        ()
        (if (<= i 0)
            (car xs)
            (structed/list/nth (cdr xs) (- i 1)))))
  "nth element (0-based) or () if out of range.")

(define structed/list/index-of
  (lambda (xs x eq?)
    (structed/list/index-of-acc xs x eq? 0))
  "0-based index of first element equal? to x, or -1.")

(define structed/list/index-of-acc
  (lambda (xs x eq? i)
    (if (null? xs)
        -1
        (if (eq? (car xs) x)
            i
            (structed/list/index-of-acc (cdr xs) x eq? (+ i 1)))))
  "index-of accumulator.")

(define structed/list/replace-at
  (lambda (xs i v)
    (if (null? xs)
        (list)
        (if (<= i 0)
            (cons v (cdr xs))
            (cons (car xs) (structed/list/replace-at (cdr xs) (- i 1) v)))))
  "xs with element at i replaced by v (no-op if i out of range).")

(define structed/list/insert-at
  (lambda (xs i v)
    (if (or (null? xs) (<= i 0))
        (cons v xs)
        (cons (car xs) (structed/list/insert-at (cdr xs) (- i 1) v))))
  "xs with v inserted before index i (clamped to end).")

(define structed/list/remove-at
  (lambda (xs i)
    (if (null? xs)
        (list)
        (if (<= i 0)
            (cdr xs)
            (cons (car xs) (structed/list/remove-at (cdr xs) (- i 1))))))
  "xs with element at i removed (no-op if i out of range).")

(define structed/list/split-at
  (lambda (xs i)
    (list (structed/list/take xs i) (structed/list/drop xs i)))
  "(list before after): xs split at index i.")

;; ---------------------------------------------------------------------------
;; tree helpers over (get-kids set-kids)
;; ---------------------------------------------------------------------------

;; find a node by id within a tree; returns the node or ()
(define structed/find
  (lambda (get-kids tree id)
    (if (string-eq (structed/id tree) id)
        tree
        (structed/find-in-kids get-kids (get-kids tree) id)))
  "Find node by id in tree (get-kids accessor).")

(define structed/find-in-kids
  (lambda (get-kids kids id)
    (if (null? kids)
        ()
        (let ((hit (structed/find get-kids (car kids) id)))
          (if hit hit (structed/find-in-kids get-kids (cdr kids) id)))))
  "find across a kids list.")

;; default id accessor: every domain node carries an id as its first
;; element (outline node = (id title body todo kids); AST path-node uses
;; the path string as id). Domains may pass their own id-of if needed.
(define structed/id
  (lambda (n) (as-str (car n)))
  "Node id = first element (car n). Domains may override.")

;; find the parent of the node with id; returns () for root/unknown.
(define structed/find-parent
  (lambda (get-kids tree id)
    (if (string-eq (structed/id tree) id)
        ()
        (structed/find-parent-in-kids get-kids tree (get-kids tree) id)))
  "Parent of node id, or () if id is root/unknown.")

(define structed/find-parent-in-kids
  (lambda (get-kids parent kids id)
    (if (null? kids)
        ()
        (let ((k (car kids)))
          (if (string-eq (structed/id k) id)
              parent
              (let ((hit (structed/find-parent get-kids k id)))
                (if hit hit (structed/find-parent-in-kids get-kids parent (cdr kids) id)))))))
  "find-parent across a kids list.")

;; replace the node with id by applying f to it, in place.
(define structed/update
  (lambda (get-kids set-kids tree id f)
    (if (string-eq (structed/id tree) id)
        (f tree)
        (set-kids tree
          (structed/update-kids get-kids set-kids (get-kids tree) id f))))
  "Apply f to the node with id, returning a new tree.")

(define structed/update-kids
  (lambda (get-kids set-kids kids id f)
    (if (null? kids)
        (list)
        (cons (structed/update get-kids set-kids (car kids) id f)
              (structed/update-kids get-kids set-kids (cdr kids) id f))))
  "update across a kids list.")

;; replace the node with id by a new node value.
(define structed/replace
  (lambda (get-kids set-kids tree id node)
    (structed/update get-kids set-kids tree id (lambda (_) node)))
  "Replace node id with node.")

;; ---------------------------------------------------------------------------
;; sibling + position helpers
;; ---------------------------------------------------------------------------

;; siblings of node id (the kids list of its parent), or () if root.
(define structed/siblings
  (lambda (get-kids tree id)
    (let ((parent (structed/find-parent get-kids tree id)))
      (if parent (get-kids parent) (list))))
  "Kids list of id's parent; () if id is root.")

;; index of node id among its siblings, or 0 if root/unknown.
(define structed/sibling-index
  (lambda (get-kids tree id)
    (let ((sibs (structed/siblings get-kids tree id)))
      (let ((i (structed/list/index-of sibs (structed/find get-kids tree id)
                  (lambda (a b) (string-eq (structed/id a) (structed/id b))))))
        (if (< i 0) 0 i))))
  "0-based index of id among siblings (0 if root/unknown).")

;; ---------------------------------------------------------------------------
;; the ops. Each returns (list tree' selection'). selection = id string.
;; ---------------------------------------------------------------------------

;; slurp-forward: target absorbs its NEXT sibling as a new last child.
;; (a (b) c) with target (b) -> (a (b c)). selection stays on target.
(define structed/slurp-forward
  (lambda (get-kids set-kids tree id)
    (let ((parent (structed/find-parent get-kids tree id)))
      (if (or (not parent) (string-eq id (structed/id tree)))
          (list tree id)
          (let ((sibs (get-kids parent)))
            (let ((i (structed/sibling-index get-kids tree id)))
              (if (>= (+ i 1) (count sibs))
                  (list tree id)
                  (let ((next (structed/list/nth sibs (+ i 1))))
                    (let ((absorbed (set-kids (structed/list/nth sibs i)
                                    (append (get-kids (structed/list/nth sibs i))
                                            (list next)))))
                      (let ((new-sibs (structed/list/remove-at
                                       (structed/list/replace-at sibs i absorbed)
                                       (+ i 1))))
                        (let ((new-parent (set-kids parent new-sibs)))
                          (let ((new-tree (structed/replace get-kids set-kids tree
                                            (structed/id parent) new-parent)))
                            (list new-tree id))))))))))))
  "Slurp next sibling into target as last child; selection stays on target.")

;; slurp-backward: target absorbs its PREV sibling as a new first child.
;; (a (b) c) with target (b) -> ((a b) c). selection stays on target.
(define structed/slurp-backward
  (lambda (get-kids set-kids tree id)
    (let ((parent (structed/find-parent get-kids tree id)))
      (if (or (not parent) (string-eq id (structed/id tree)))
          (list tree id)
          (let ((sibs (get-kids parent)))
            (let ((i (structed/sibling-index get-kids tree id)))
              (if (<= i 0)
                  (list tree id)
                  (let ((prev (structed/list/nth sibs (- i 1))))
                    (let ((absorbed (set-kids (structed/list/nth sibs i)
                                    (cons prev (get-kids (structed/list/nth sibs i))))))
                      (let ((new-sibs (structed/list/remove-at
                                       (structed/list/replace-at sibs i absorbed)
                                       (- i 1))))
                        (let ((new-parent (set-kids parent new-sibs)))
                          (let ((new-tree (structed/replace get-kids set-kids tree
                                            (structed/id parent) new-parent)))
                            (list new-tree id))))))))))))
  "Slurp prev sibling into target as first child; selection stays on target.")

;; barf-forward: target's LAST child is pushed out to become its next sibling.
;; (a (b c)) with target (a)... wait target is the form: ((b c) d) target (b c)
;; -> (b c d)? No: barf pushes a CHILD out. (a (b c) d) target (b c) last child c
;; -> (a (b) c d). selection stays on target.
(define structed/barf-forward
  (lambda (get-kids set-kids tree id)
    (let ((target (structed/find get-kids tree id)))
      (if (or (not target) (string-eq id (structed/id tree)))
          (list tree id)
          (let ((kids (get-kids target)))
            (if (<= (count kids) 0)
                (list tree id)
                (let ((last-kid (structed/list/nth kids (- (count kids) 1))))
                  (let ((shrunk (set-kids target (structed/list/take kids (- (count kids) 1)))))
                    (let ((parent (structed/find-parent get-kids tree id)))
                      (if (not parent)
                          ;; target is root: barf is a no-op (no sibling to push to)
                          (list tree id)
                          (let ((sibs (get-kids parent)))
                          (let ((i (structed/sibling-index get-kids tree id)))
                            (let ((new-sibs (structed/list/insert-at
                                             (structed/list/replace-at sibs i shrunk)
                                             (+ i 1) last-kid)))
                              (let ((new-parent (set-kids parent new-sibs)))
                                (let ((new-tree (structed/replace get-kids set-kids tree
                                                  (structed/id parent) new-parent)))
                                  (list new-tree id)))))))))))))))
  "Barf last child of target out as next sibling; selection stays on target.")

;; barf-backward: target's FIRST child is pushed out to become its prev sibling.
;; (a (b c) d) target (b c) first child b -> (a b (c) d). selection stays on target.
(define structed/barf-backward
  (lambda (get-kids set-kids tree id)
    (let ((target (structed/find get-kids tree id)))
      (if (or (not target) (string-eq id (structed/id tree)))
          (list tree id)
          (let ((kids (get-kids target)))
            (if (<= (count kids) 0)
                (list tree id)
                (let ((first-kid (structed/list/nth kids 0)))
                  (let ((shrunk (set-kids target (structed/list/drop kids 1))))
                    (let ((parent (structed/find-parent get-kids tree id)))
                      (if (not parent)
                          (list tree id)
                          (let ((sibs (get-kids parent)))
                          (let ((i (structed/sibling-index get-kids tree id)))
                            (let ((new-sibs (structed/list/insert-at
                                             (structed/list/replace-at sibs i shrunk)
                                             i first-kid)))
                              (let ((new-parent (set-kids parent new-sibs)))
                                (let ((new-tree (structed/replace get-kids set-kids tree
                                                  (structed/id parent) new-parent)))
                                  (list new-tree id)))))))))))))))
  "Barf first child of target out as prev sibling; selection stays on target.")

;; wrap: target becomes the sole child of a fresh node inserted in its
;; place. The fresh node shape is domain-supplied via make-wrap-node:
;;   (wrap get-kids set-kids make-wrap-node tree id [head-atom])
;; For outlines, make-wrap-node = (lambda (kid) (outline/node fresh-id "")).
;; For exprs with a head-atom: (lambda (kid head) (list head kid)) → (f x).
;; Without a head arg, make-wrap-node takes one arg (kid). selection stays.
(define structed/wrap
  (lambda (get-kids set-kids make-wrap-node tree id head)
    (let ((target (structed/find get-kids tree id)))
      (if (not target)
          (list tree id)
          (let ((wrapped (if (null? head)
                             (make-wrap-node target)
                             (make-wrap-node target head))))
              (let ((parent (structed/find-parent get-kids tree id)))
                (if (not parent)
                    (list tree id)
                    (let ((sibs (get-kids parent)))
                    (let ((i (structed/sibling-index get-kids tree id)))
                      (let ((new-sibs (structed/list/replace-at sibs i wrapped)))
                        (let ((new-parent (set-kids parent new-sibs)))
                          (let ((new-tree (structed/replace get-kids set-kids tree
                                            (structed/id parent) new-parent)))
                            (list new-tree (structed/id wrapped)))))))))))))
  "Wrap target in a fresh node (head arg: () for none, or an atom for exprs); selection moves to the new node.")

;; splice: target is removed; its children take its place in the parent,
;; in order. selection -> first lifted child, else parent.
;; reattach: given tree, target id, and a function (new-sibs-fn parent sibs i)
;; producing a new siblings list, return (list new-tree id). Centralizes the
;; "find parent -> rebuild sibs -> replace parent in tree" pattern so ops stay flat.
(define structed/reattach
  (lambda (get-kids set-kids tree id new-sibs-fn)
    (let ((parent (structed/find-parent get-kids tree id)))
      (if (not parent)
          (list tree id)
          (let ((sibs (get-kids parent)))
            (let ((i (structed/sibling-index get-kids tree id)))
              (let ((new-sibs (new-sibs-fn parent sibs i)))
                (let ((new-parent (set-kids parent new-sibs)))
                  (let ((new-tree (structed/replace get-kids set-kids tree
                                    (structed/id parent) new-parent)))
                    (list new-tree id)))))))))
  "Rebuild target's siblings via new-sibs-fn and reattach parent; returns (list tree' id).")

;; splice: target is removed; its children take its place in the parent,
;; in order. selection -> first lifted child, else parent.
(define structed/splice
  (lambda (get-kids set-kids tree id)
    (let ((target (structed/find get-kids tree id)))
      (if (or (not target) (string-eq id (structed/id tree)))
          (list tree id)
          (let ((kids (get-kids target)))
            (let ((parent (structed/find-parent get-kids tree id)))
              (let ((parent-id (if (null? parent) id (structed/id parent))))
                (let ((res (structed/reattach get-kids set-kids tree id
                            (lambda (parent sibs i)
                              (structed/list/insert-list-at
                                (structed/list/remove-at sibs i) i kids)))))
                  (let ((new-tree (car res)))
                    (let ((sel (if (null? kids) parent-id (structed/id (car kids)))))
                      (list new-tree sel))))))))))
  "Splice target's children into parent; selection -> first child else parent.")

;; insert a whole list xs at index i (helper for splice)
(define structed/list/insert-list-at
  (lambda (xs i ys)
    (if (null? ys)
        xs
        (structed/list/insert-list-at
          (structed/list/insert-at xs i (car ys)) (+ i 1) (cdr ys))))
  "Insert all of ys into xs starting at index i.")

;; raise: target replaces its parent. selection stays on target (now at
;; parent's place). (a (b c) d) target (b c) with parent (a ...) ... if
;; target is the whole parent this is a no-op; raise replaces parent with
;; the target node itself.
(define structed/raise
  (lambda (get-kids set-kids tree id)
    (let ((parent (structed/find-parent get-kids tree id)))
      (if (not parent)
          (list tree id)
          (let ((grand (structed/find-parent get-kids tree (structed/id parent))))
            (if (not grand)
                ;; parent is root: raise target to root (target becomes tree)
                (let ((target (structed/find get-kids tree id)))
                  (list target id))
                (let ((sibs (get-kids grand)))
                (let ((i (structed/list/index-of sibs parent
                          (lambda (a b) (string-eq (structed/id a) (structed/id b))))))
                  (let ((target (structed/find get-kids tree id)))
                    (let ((new-sibs (structed/list/replace-at sibs i target)))
                      (let ((new-grand (set-kids grand new-sibs)))
                        (let ((new-tree (structed/replace get-kids set-kids tree
                                          (structed/id grand) new-grand)))
                          (list new-tree id))))))))))))
  "Replace parent with target; selection stays on target.")

;; split: target divides at position into two adjacent siblings.
;; pos defaults to 0 (split before all kids → empty + full). Caller passes
;; a position in `args` (rest). selection -> first resulting node.
(define structed/split
  (lambda (get-kids set-kids make-node tree id pos)
      (let ((target (structed/find get-kids tree id)))
        (if (or (not target) (string-eq id (structed/id tree)))
            (list tree id)
            (let ((kids (get-kids target)))
              (let ((parts (structed/list/split-at kids pos)))
                (let ((before (car parts)) (after (car (cdr parts))))
                  (let ((n1 (make-node before)) (n2 (make-node after)))
                    (let ((parent (structed/find-parent get-kids tree id)))
                      (if (not parent)
                          (list tree id)
                          (let ((sibs (get-kids parent)))
                          (let ((i (structed/sibling-index get-kids tree id)))
                            (let ((new-sibs
                                    (structed/list/insert-at
                                      (structed/list/insert-at
                                        (structed/list/remove-at sibs i) i n1)
                                      (+ i 1) n2)))
                              (let ((new-parent (set-kids parent new-sibs)))
                                (let ((new-tree (structed/replace get-kids set-kids tree
                                                  (structed/id parent) new-parent)))
                                  (list new-tree (structed/id n1))))))))))))))))
  "Split target at pos into two siblings; selection -> first half. Pass pos (0-based).")

;; join: target merges with its NEXT sibling (children concatenated).
;; selection -> merged target. (a (b) (c)) target (b) -> (a (b c)).
(define structed/join
  (lambda (get-kids set-kids tree id)
    (let ((parent (structed/find-parent get-kids tree id)))
      (if (or (not parent) (string-eq id (structed/id tree)))
          (list tree id)
          (let ((sibs (get-kids parent)))
            (let ((i (structed/sibling-index get-kids tree id)))
              (if (>= (+ i 1) (count sibs))
                  (list tree id)
                  (let ((me (structed/list/nth sibs i))
                        (next (structed/list/nth sibs (+ i 1))))
                    (let ((merged (set-kids me (append (get-kids me) (get-kids next)))))
                      (let ((new-sibs (structed/list/remove-at
                                       (structed/list/replace-at sibs i merged)
                                       (+ i 1))))
                        (let ((new-parent (set-kids parent new-sibs)))
                          (let ((new-tree (structed/replace get-kids set-kids tree
                                            (structed/id parent) new-parent)))
                            (list new-tree id))))))))))))
  "Merge target with next sibling (kids concat); selection -> merged target.")

;; transpose: target swaps with its NEXT sibling (or PREV if last).
;; selection stays on target.
(define structed/transpose
  (lambda (get-kids set-kids tree id)
    (let ((parent (structed/find-parent get-kids tree id)))
      (if (or (not parent) (string-eq id (structed/id tree)))
          (list tree id)
          (let ((sibs (get-kids parent)))
            (let ((i (structed/sibling-index get-kids tree id)))
              (if (< (count sibs) 2)
                  (list tree id)
                  (let ((j (if (>= (+ i 1) (count sibs)) (- i 1) (+ i 1))))
                    (let ((a (structed/list/nth sibs i))
                          (b (structed/list/nth sibs j)))
                      (let ((new-sibs (structed/list/replace-at
                                       (structed/list/replace-at sibs i b) j a)))
                        (let ((new-parent (set-kids parent new-sibs)))
                          (let ((new-tree (structed/replace get-kids set-kids tree
                                            (structed/id parent) new-parent)))
                            (list new-tree id))))))))))))
  "Swap target with next (or prev if last) sibling; selection stays on target.")

;; ---------------------------------------------------------------------------
;; property-test helpers (used by eval-level smokes, not the UI)
;; ---------------------------------------------------------------------------

;; round-trip check: slurp-forward then barf-forward is identity on the
;; tree (when the op actually did something). Returns the tree or () if
;; not identity. Caller wires get-kids/set-kids and an id-eq.
(define structed/roundtrip-slurp-barf
  (lambda (get-kids set-kids tree id)
    (let ((r1 (structed/slurp-forward get-kids set-kids tree id)))
      (let ((t1 (car r1)))
        (let ((r2 (structed/barf-forward get-kids set-kids t1 id)))
          (let ((t2 (car r2)))
            (if (string-eq (json-emit t2) (json-emit tree)) t2 ()))))))
  "slurp-forward then barf-forward; tree if identity else ().")

(define structed/roundtrip-wrap-splice
  (lambda (get-kids set-kids make-wrap-node tree id)
    (let ((r1 (structed/wrap get-kids set-kids make-wrap-node tree id ())))
      (let ((t1 (car r1)) (s1 (car (cdr r1))))
        (let ((r2 (structed/splice get-kids set-kids t1 s1)))
          (let ((t2 (car r2)))
            (if (string-eq (json-emit t2) (json-emit tree)) t2 ()))))))
  "wrap then splice; tree if identity else ().")
