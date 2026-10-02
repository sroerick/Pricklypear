;; package structed -- exported from live branch system/main
;; Sync: scripts/pp_sync.py pull|push

(define structed/barf-backward
  (lambda
    (get-kids set-kids tree id)
    "Move a target's first child up to become its preceding sibling; returns (tree id)."
    (let
      ((target (structed/find get-kids tree id)))
      (if
        (or (not target) (string-eq id (structed/id tree)))
        (list tree id)
        (let
          ((kids (get-kids target)))
          (if
            (<= (structed/count kids) 0)
            (list tree id)
            (let
              ((first-kid (structed/list/nth kids 0)))
              (let
                ((shrunk (set-kids target (structed/list/drop kids 1))))
                (let
                  ((parent (structed/find-parent get-kids tree id)))
                  (if
                    (not parent)
                    (list tree id)
                    (let
                      ((sibs (get-kids parent)))
                      (let
                        ((i (structed/sibling-index get-kids tree id)))
                        (let
                          ((new-sibs
                            (structed/list/insert-at
                              (structed/list/replace-at sibs i shrunk)
                              i
                              first-kid)))
                          (let
                            ((new-parent (set-kids parent new-sibs)))
                            (let
                              ((new-tree
                                (structed/replace
                                  get-kids
                                  set-kids
                                  tree
                                  (structed/id parent)
                                  new-parent)))
                              (list new-tree id))))))))))))))))

(define structed/barf-forward
  (lambda
    (get-kids set-kids tree id)
    "Move a target's last child down to become its next sibling; returns (tree id)."
    (let
      ((target (structed/find get-kids tree id)))
      (if
        (or (not target) (string-eq id (structed/id tree)))
        (list tree id)
        (let
          ((kids (get-kids target)))
          (if
            (<= (structed/count kids) 0)
            (list tree id)
            (let
              ((last-kid
                (structed/list/nth kids (- (structed/count kids) 1))))
              (let
                ((shrunk
                  (set-kids
                    target
                    (structed/list/take kids (- (structed/count kids) 1)))))
                (let
                  ((parent (structed/find-parent get-kids tree id)))
                  (if
                    (not parent)
                    (list tree id)
                    (let
                      ((sibs (get-kids parent)))
                      (let
                        ((i (structed/sibling-index get-kids tree id)))
                        (let
                          ((new-sibs
                            (structed/list/insert-at
                              (structed/list/replace-at sibs i shrunk)
                              (+ i 1)
                              last-kid)))
                          (let
                            ((new-parent (set-kids parent new-sibs)))
                            (let
                              ((new-tree
                                (structed/replace
                                  get-kids
                                  set-kids
                                  tree
                                  (structed/id parent)
                                  new-parent)))
                              (list new-tree id))))))))))))))))

(define structed/count
  (lambda
    (xs)
    "Length of the list xs."
    (if (null? xs) 0 (+ 1 (structed/count (cdr xs))))))

(define structed/find
  (lambda
    (get-kids tree id)
    "Node in tree whose id matches, or ()."
    (if
      (string-eq (structed/id tree) id)
      tree
      (structed/find-in-kids get-kids (get-kids tree) id))))

(define structed/find-in-kids
  (lambda
    (get-kids kids id)
    "Depth-first search for id across the sibling list kids."
    (if
      (null? kids)
      ()
      (let
        ((hit (structed/find get-kids (car kids) id)))
        (if
          hit
          hit
          (structed/find-in-kids get-kids (cdr kids) id))))))

(define structed/find-parent
  (lambda
    (get-kids tree id)
    "Parent node of the node with id, or ()."
    (if
      (string-eq (structed/id tree) id)
      ()
      (structed/find-parent-in-kids
        get-kids
        tree
        (get-kids tree)
        id))))

(define structed/find-parent-in-kids
  (lambda
    (get-kids parent kids id)
    "Search kids for id, returning the enclosing parent node."
    (if
      (null? kids)
      ()
      (let
        ((k (car kids)))
        (if
          (string-eq (structed/id k) id)
          parent
          (let
            ((hit (structed/find-parent get-kids k id)))
            (if
              hit
              hit
              (structed/find-parent-in-kids
                get-kids
                parent
                (cdr kids)
                id))))))))

(define structed/id
  (lambda (n) "Node id as a string." (as-str (car n))))

(define structed/join
  (lambda
    (get-kids set-kids tree id)
    "Merge a target with its next sibling (children merged, next removed); returns (tree id)."
    (let
      ((parent (structed/find-parent get-kids tree id)))
      (if
        (or (not parent) (string-eq id (structed/id tree)))
        (list tree id)
        (let
          ((sibs (get-kids parent)))
          (let
            ((i (structed/sibling-index get-kids tree id)))
            (if
              (>= (+ i 1) (structed/count sibs))
              (list tree id)
              (let
                ((me (structed/list/nth sibs i))
                  (next (structed/list/nth sibs (+ i 1))))
                (let
                  ((merged
                    (set-kids me (append (get-kids me) (get-kids next)))))
                  (let
                    ((new-sibs
                      (structed/list/remove-at
                        (structed/list/replace-at sibs i merged)
                        (+ i 1))))
                    (let
                      ((new-parent (set-kids parent new-sibs)))
                      (let
                        ((new-tree
                          (structed/replace
                            get-kids
                            set-kids
                            tree
                            (structed/id parent)
                            new-parent)))
                        (list new-tree id)))))))))))))

(define structed/list/drop
  (lambda
    (xs n)
    "Element list with the first n removed."
    (if
      (or (null? xs) (<= n 0))
      xs
      (structed/list/drop (cdr xs) (- n 1)))))

(define structed/list/index-of
  (lambda
    (xs x eq?)
    "First index of x in xs per eq?, or -1."
    (structed/list/index-of-acc xs x eq? 0)))

(define structed/list/index-of-acc
  (lambda
    (xs x eq? i)
    "Accumulating index search helper."
    (if
      (null? xs)
      -1
      (if
        (eq? (car xs) x)
        i
        (structed/list/index-of-acc (cdr xs) x eq? (+ i 1))))))

(define structed/list/insert-at
  (lambda
    (xs i v)
    "Insert v into xs at index i (clamped to head)."
    (if
      (or (null? xs) (<= i 0))
      (cons v xs)
      (cons
        (car xs)
        (structed/list/insert-at (cdr xs) (- i 1) v)))))

(define structed/list/insert-list-at
  (lambda
    (xs i ys)
    "Insert every element of ys into xs starting at index i."
    (if
      (null? ys)
      xs
      (structed/list/insert-list-at
        (structed/list/insert-at xs i (car ys))
        (+ i 1)
        (cdr ys)))))

(define structed/list/nth
  (lambda
    (xs i)
    "Element at index i of xs, or ()."
    (if
      (null? xs)
      ()
      (if
        (<= i 0)
        (car xs)
        (structed/list/nth (cdr xs) (- i 1))))))

(define structed/list/remove-at
  (lambda
    (xs i)
    "xs with the element at index i removed."
    (if
      (null? xs)
      (list)
      (if
        (<= i 0)
        (cdr xs)
        (cons
          (car xs)
          (structed/list/remove-at (cdr xs) (- i 1)))))))

(define structed/list/replace-at
  (lambda
    (xs i v)
    "xs with index i replaced by v."
    (if
      (null? xs)
      (list)
      (if
        (<= i 0)
        (cons v (cdr xs))
        (cons
          (car xs)
          (structed/list/replace-at (cdr xs) (- i 1) v))))))

(define structed/list/split-at
  (lambda
    (xs i)
    "Split xs at index i into (before after)."
    (list
      (structed/list/take xs i)
      (structed/list/drop xs i))))

(define structed/list/take
  (lambda
    (xs n)
    "First n elements of xs."
    (if
      (or (null? xs) (<= n 0))
      (list)
      (cons (car xs) (structed/list/take (cdr xs) (- n 1))))))

(define structed/raise
  (lambda
    (get-kids set-kids tree id)
    "Lift a target up to replace its grandparent; returns (tree id)."
    (let
      ((parent (structed/find-parent get-kids tree id)))
      (if
        (not parent)
        (list tree id)
        (let
          ((grand
            (structed/find-parent
              get-kids
              tree
              (structed/id parent))))
          (if
            (not grand)
            (let
              ((target (structed/find get-kids tree id)))
              (list target id))
            (let
              ((sibs (get-kids grand)))
              (let
                ((i
                  (structed/list/index-of
                    sibs
                    parent
                    (lambda
                      (a b)
                      (string-eq (structed/id a) (structed/id b))))))
                (let
                  ((target (structed/find get-kids tree id)))
                  (let
                    ((new-sibs (structed/list/replace-at sibs i target)))
                    (let
                      ((new-grand (set-kids grand new-sibs)))
                      (let
                        ((new-tree
                          (structed/replace
                            get-kids
                            set-kids
                            tree
                            (structed/id grand)
                            new-grand)))
                        (list new-tree id)))))))))))))

(define structed/reattach
  (lambda
    (get-kids set-kids tree id new-sibs-fn)
    "Replace the target's sibling slot with new-sibs-fn applied to its parent; returns (tree id)."
    (let
      ((parent (structed/find-parent get-kids tree id)))
      (if
        (not parent)
        (list tree id)
        (let
          ((sibs (get-kids parent)))
          (let
            ((i (structed/sibling-index get-kids tree id)))
            (let
              ((new-sibs (new-sibs-fn parent sibs i)))
              (let
                ((new-parent (set-kids parent new-sibs)))
                (let
                  ((new-tree
                    (structed/replace
                      get-kids
                      set-kids
                      tree
                      (structed/id parent)
                      new-parent)))
                  (list new-tree id))))))))))

(define structed/replace
  (lambda
    (get-kids set-kids tree id node)
    "Substitute node for the node currently at id."
    (structed/update
      get-kids
      set-kids
      tree
      id
      (lambda (_) node))))

(define structed/roundtrip-slurp-barf
  (lambda
    (get-kids set-kids tree id)
    "Check that slurp-forward then barf-forward restores the tree (or () on mismatch)."
    (let
      ((r1 (structed/slurp-forward get-kids set-kids tree id)))
      (let
        ((t1 (car r1)))
        (let
          ((r2 (structed/barf-forward get-kids set-kids t1 id)))
          (let
            ((t2 (car r2)))
            (if (string-eq (json-emit t2) (json-emit tree)) t2 ())))))))

(define structed/roundtrip-wrap-splice
  (lambda
    (get-kids set-kids make-wrap-node tree id)
    "Check that wrap then splice restores the tree (or () on mismatch)."
    (let
      ((r1
        (structed/wrap
          get-kids
          set-kids
          make-wrap-node
          tree
          id
          ())))
      (let
        ((t1 (car r1)) (s1 (car (cdr r1))))
        (let
          ((r2 (structed/splice get-kids set-kids t1 s1)))
          (let
            ((t2 (car r2)))
            (if (string-eq (json-emit t2) (json-emit tree)) t2 ())))))))

(define structed/sibling-index
  (lambda
    (get-kids tree id)
    "0-based index of a target within its parent's kids."
    (let
      ((sibs (structed/siblings get-kids tree id)))
      (let
        ((i
          (structed/list/index-of
            sibs
            (structed/find get-kids tree id)
            (lambda
              (a b)
              (string-eq (structed/id a) (structed/id b))))))
        (if (< i 0) 0 i)))))

(define structed/siblings
  (lambda
    (get-kids tree id)
    "Sibling list of the node with id, or () for the root."
    (let
      ((parent (structed/find-parent get-kids tree id)))
      (if parent (get-kids parent) (list)))))

(define structed/slurp-backward
  (lambda
    (get-kids set-kids tree id)
    "Absorb a target's previous sibling into it; returns (tree id)."
    (let
      ((parent (structed/find-parent get-kids tree id)))
      (if
        (or (not parent) (string-eq id (structed/id tree)))
        (list tree id)
        (let
          ((sibs (get-kids parent)))
          (let
            ((i (structed/sibling-index get-kids tree id)))
            (if
              (<= i 0)
              (list tree id)
              (let
                ((prev (structed/list/nth sibs (- i 1))))
                (let
                  ((absorbed
                    (set-kids
                      (structed/list/nth sibs i)
                      (cons prev (get-kids (structed/list/nth sibs i))))))
                  (let
                    ((new-sibs
                      (structed/list/remove-at
                        (structed/list/replace-at sibs i absorbed)
                        (- i 1))))
                    (let
                      ((new-parent (set-kids parent new-sibs)))
                      (let
                        ((new-tree
                          (structed/replace
                            get-kids
                            set-kids
                            tree
                            (structed/id parent)
                            new-parent)))
                        (list new-tree id)))))))))))))

(define structed/slurp-forward
  (lambda
    (get-kids set-kids tree id)
    "Absorb a target's next sibling into it; returns (tree id)."
    (let
      ((parent (structed/find-parent get-kids tree id)))
      (if
        (or (not parent) (string-eq id (structed/id tree)))
        (list tree id)
        (let
          ((sibs (get-kids parent)))
          (let
            ((i (structed/sibling-index get-kids tree id)))
            (if
              (>= (+ i 1) (structed/count sibs))
              (list tree id)
              (let
                ((next (structed/list/nth sibs (+ i 1))))
                (let
                  ((absorbed
                    (set-kids
                      (structed/list/nth sibs i)
                      (append
                        (get-kids (structed/list/nth sibs i))
                        (list next)))))
                  (let
                    ((new-sibs
                      (structed/list/remove-at
                        (structed/list/replace-at sibs i absorbed)
                        (+ i 1))))
                    (let
                      ((new-parent (set-kids parent new-sibs)))
                      (let
                        ((new-tree
                          (structed/replace
                            get-kids
                            set-kids
                            tree
                            (structed/id parent)
                            new-parent)))
                        (list new-tree id)))))))))))))

(define structed/splice
  (lambda
    (get-kids set-kids tree id)
    "Lift a target's children up into its slot (removing it); returns (tree sel)."
    (let
      ((target (structed/find get-kids tree id)))
      (if
        (or (not target) (string-eq id (structed/id tree)))
        (list tree id)
        (let
          ((kids (get-kids target)))
          (let
            ((parent (structed/find-parent get-kids tree id)))
            (let
              ((parent-id (if (null? parent) id (structed/id parent))))
              (let
                ((res
                  (structed/reattach
                    get-kids
                    set-kids
                    tree
                    id
                    (lambda
                      (parent sibs i)
                      (structed/list/insert-list-at
                        (structed/list/remove-at sibs i)
                        i
                        kids)))))
                (let
                  ((new-tree (car res)))
                  (let
                    ((sel
                      (if (null? kids) parent-id (structed/id (car kids)))))
                    (list new-tree sel)))))))))))

(define structed/split
  (lambda
    (get-kids set-kids make-node tree id pos)
    "Split a target's children at pos into two sibling nodes; returns (tree first-id)."
    (let
      ((target (structed/find get-kids tree id)))
      (if
        (or (not target) (string-eq id (structed/id tree)))
        (list tree id)
        (let
          ((kids (get-kids target)))
          (let
            ((parts (structed/list/split-at kids pos)))
            (let
              ((before (car parts)) (after (car (cdr parts))))
              (let
                ((n1 (make-node before)) (n2 (make-node after)))
                (let
                  ((parent (structed/find-parent get-kids tree id)))
                  (if
                    (not parent)
                    (list tree id)
                    (let
                      ((sibs (get-kids parent)))
                      (let
                        ((i (structed/sibling-index get-kids tree id)))
                        (let
                          ((new-sibs
                            (structed/list/insert-at
                              (structed/list/insert-at
                                (structed/list/remove-at sibs i)
                                i
                                n1)
                              (+ i 1)
                              n2)))
                          (let
                            ((new-parent (set-kids parent new-sibs)))
                            (let
                              ((new-tree
                                (structed/replace
                                  get-kids
                                  set-kids
                                  tree
                                  (structed/id parent)
                                  new-parent)))
                              (list new-tree (structed/id n1)))))))))))))))))

(define structed/transpose
  (lambda
    (get-kids set-kids tree id)
    "Swap a target with its next sibling (or previous at the end); returns (tree id)."
    (let
      ((parent (structed/find-parent get-kids tree id)))
      (if
        (or (not parent) (string-eq id (structed/id tree)))
        (list tree id)
        (let
          ((sibs (get-kids parent)))
          (let
            ((i (structed/sibling-index get-kids tree id)))
            (if
              (< (structed/count sibs) 2)
              (list tree id)
              (let
                ((j
                  (if (>= (+ i 1) (structed/count sibs)) (- i 1) (+ i 1))))
                (let
                  ((a (structed/list/nth sibs i))
                    (b (structed/list/nth sibs j)))
                  (let
                    ((new-sibs
                      (structed/list/replace-at
                        (structed/list/replace-at sibs i b)
                        j
                        a)))
                    (let
                      ((new-parent (set-kids parent new-sibs)))
                      (let
                        ((new-tree
                          (structed/replace
                            get-kids
                            set-kids
                            tree
                            (structed/id parent)
                            new-parent)))
                        (list new-tree id)))))))))))))

(define structed/update
  (lambda
    (get-kids set-kids tree id f)
    "Apply f to the node at id, rebuilding the tree along the way."
    (if
      (string-eq (structed/id tree) id)
      (f tree)
      (set-kids
        tree
        (structed/update-kids
          get-kids
          set-kids
          (get-kids tree)
          id
          f)))))

(define structed/update-kids
  (lambda
    (get-kids set-kids kids id f)
    "Apply update across a sibling list, mapping each child."
    (if
      (null? kids)
      (list)
      (cons
        (structed/update get-kids set-kids (car kids) id f)
        (structed/update-kids get-kids set-kids (cdr kids) id f)))))

(define structed/wrap
  (lambda
    (get-kids set-kids make-wrap-node tree id head)
    "Wrap a target in a new node (from make-wrap-node); returns (tree wrap-id)."
    (let
      ((target (structed/find get-kids tree id)))
      (if
        (not target)
        (list tree id)
        (let
          ((wrapped
            (if
              (null? head)
              (make-wrap-node target)
              (make-wrap-node target head))))
          (let
            ((parent (structed/find-parent get-kids tree id)))
            (if
              (not parent)
              (list tree id)
              (let
                ((sibs (get-kids parent)))
                (let
                  ((i (structed/sibling-index get-kids tree id)))
                  (let
                    ((new-sibs (structed/list/replace-at sibs i wrapped)))
                    (let
                      ((new-parent (set-kids parent new-sibs)))
                      (let
                        ((new-tree
                          (structed/replace
                            get-kids
                            set-kids
                            tree
                            (structed/id parent)
                            new-parent)))
                        (list new-tree (structed/id wrapped))))))))))))))
