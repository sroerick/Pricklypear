;; package forge — executable spec graphs as habitat data.
;; Spec: borge/forge.borg. Load: (load-library "forge") or load-string.
;; Depends on: list (foldl/filter/map), notes (log/issues), and kernel prims
;; (read-string, forge-effects, deftype, create/update/get/rows, dict-*).
;;
;; The borge discipline as a living graph: a spec is a tree of typed nodes,
;; each node carries a machine-checkable victory condition, LLM drafters fill
;; leaves bottom-up along a computed frontier, mechanical checkers flag
;; anomalies, and the human approves each level before the descent continues.
;;
;; Exports are prefix-convention (forge/*). See borge/forge.borg for the
;; design rationale: victory conditions are the artifact, code is a cache;
;; planning is a computation (free-vars), not an LLM judgment; mechanical
;; flags run before any human is asked to judge.

;; ============================================================
;; TYPES (graph-model)
;; ============================================================
;; Placement: data.shared.forge-* (outside branch lenses) per one-tree
;; branching-interaction — same argument as jobs: the loop (host identity)
;; mutates node rows regardless of who drafted.

(deftype 'forge-graph
  'name 'root_path 'status 'created_by 'created_at 'max_depth)

(deftype 'forge-node
  'graph 'path 'kind 'title 'body 'verify 'effects 'status
  'constraints 'parent 'children 'branch 'author)

(deftype 'forge-flag
  'node_path 'check 'detail 'resolved 'resolution)

;; ============================================================
;; SMALL HELPERS (pure)
;; ============================================================

(define forge/member?
  (lambda (x xs)
    (if (null? xs)
        #f
        (if (string-eq (as-str (car xs)) (as-str x))
            #t
            (forge/member? x (cdr xs)))))
  "String membership over a list.")

(define forge/append
  (lambda (a b)
    (if (null? a) b (cons (car a) (forge/append (cdr a) b))))
  "Concatenate two lists.")

(define forge/str-join
  (lambda (sep xs)
    (if (null? xs)
        ""
        (if (null? (cdr xs))
            (as-str (car xs))
            (string-append (as-str (car xs))
              (string-append sep (forge/str-join sep (cdr xs)))))))
  "Join a list of strings with sep.")

;; field access on a data row (id . fields-json-string) pair.
;; rows returns (id . fields) pairs where fields is a JSON string;
;; get returns the bare fields JSON string. Both normalize here.
(define forge/field
  (lambda (fields-json key)
    (dict-get fields-json key))
  "dict-get over a fields JSON string; () when absent.")

(define forge/field-str
  (lambda (fields-json key)
    (let ((v (forge/field fields-json key)))
      (if (null? v) "" (as-str v))))
  "Field as a string; \"\" when absent.")

;; ============================================================
;; GRAPH CRUD (graph-model)
;; ============================================================

(define forge/new-graph
  (lambda (name root-title max-depth)
    (let ((gid (create 'forge-graph
                 (string-append
                   "{\"name\":\"" name
                   "\",\"root_path\":\"" name
                   "\",\"status\":\"planning"
                   "\",\"created_by\":\"" (whoami)
                   "\",\"created_at\":\"" (as-str (now-ms))
                   "\",\"max_depth\":\"" (as-str max-depth)
                   "\"}"))))
      gid))
  "Create a graph row; returns the new id. Root node is added separately
   via forge/add-node so the caller controls the root requirement.")

(define forge/graph-by-name
  (lambda (name)
    (let ((all (rows 'forge-graph 100)))
      (forge/find-row
        (lambda (pair) (string-eq (forge/field-str (cdr pair) "name") name))
        all)))
  "Find a forge-graph row by its name field; () when absent.")

(define forge/find-row
  (lambda (pred xs)
    (if (null? xs)
        (list)
        (if (pred (car xs))
            (car xs)
            (forge/find-row pred (cdr xs)))))
  "First element of xs satisfying pred, or ().")

;; ---- nodes ----

(define forge/node-by-path
  (lambda (graph path)
    (forge/find-row
      (lambda (pair)
        (let ((f (cdr pair)))
          (if (string-eq (forge/field-str f "graph") graph)
              (string-eq (forge/field-str f "path") path)
              #f)))
      (rows 'forge-node 500)))
  "Find a node (id . fields) pair by graph+path; () when absent.")

(define forge/node-exists?
  (lambda (graph path)
    (not (null? (forge/node-by-path graph path))))
  "True when a node row exists for graph+path.")

;; children is a denormalized JSON list of child paths maintained in the
;; same mutation as the child's parent link (one logical write).
(define forge/add-node
  (lambda (graph path kind title parent)
    (if (forge/node-exists? graph path)
        (forge/node-id graph path)
        (let ((id (create 'forge-node
                    (string-append
                      "{\"graph\":\"" graph
                      "\",\"path\":\"" path
                      "\",\"kind\":\"" kind
                      "\",\"title\":\"" title
                      "\",\"body\":\""
                      "\",\"verify\":\""
                      "\",\"effects\":\"pure"
                      "\",\"status\":\"planned"
                      "\",\"constraints\":\"[]"
                      "\",\"parent\":\"" parent
                      "\",\"children\":\"[]"
                      "\",\"branch\":\"" (current-branch)
                      "\",\"author\":\"" (whoami)
                      "\"}"))))
          (if (string-eq parent "")
              id
              (forge/link-child graph parent path)))))
  "Create a planned node and link it under parent (when parent non-empty).")

(define forge/node-id
  (lambda (graph path)
    (let ((pair (forge/node-by-path graph path)))
      (if (null? pair) "" (car pair))))
  "Id of the node at graph+path, or \"\".")

(define forge/link-child
  (lambda (graph parent-path child-path)
    (let ((pair (forge/node-by-path graph parent-path)))
      (if (null? pair)
          ""
          (let ((f (cdr pair)))
            (let ((kids (json-parse (forge/field-str f "children"))))
              (if (forge/member? child-path kids)
                  (car pair)
                  (seq
                    (update 'forge-node (car pair)
                      (dict-set f "children"
                        (json-emit (forge/append kids (list child-path)))))
                    (car pair))))))))
  "Append child-path to parent's children index (idempotent).")

(define forge/children-of
  (lambda (graph path)
    (let ((pair (forge/node-by-path graph path)))
      (if (null? pair)
          (list)
          (json-parse (forge/field-str (cdr pair) "children")))))
  "List of child paths for a node.")

(define forge/nodes-of-graph
  (lambda (graph)
    (list/filter
      (lambda (pair)
        (string-eq (forge/field-str (cdr pair) "graph") graph))
      (rows 'forge-node 500)))
  "All node (id . fields) pairs belonging to a graph.")

(define forge/node-depth
  (lambda (path)
    (forge/count-slashes path 0 0))
  "Depth of a node = number of '/' separators in its path. Root has 0.")

(define forge/count-slashes
  (lambda (s i n)
    (if (>= i (string-length s))
        n
        (forge/count-slashes s (+ i 1)
          (if (string-eq (substring s i 1) "/") (+ n 1) n))))
  "Count '/' characters in s (recursive over index).")

;; ============================================================
;; STATUS TRANSITIONS (graph-model)
;; ============================================================

(define forge/set-status
  (lambda (graph path status)
    (let ((pair (forge/node-by-path graph path)))
      (if (null? pair)
          ""
          (update 'forge-node (car pair)
            (dict-set (cdr pair) "status" status)))))
  "Set a node's status; \"\" when the node is absent.")

(define forge/set-body-verify
  (lambda (graph path body verify)
    (let ((pair (forge/node-by-path graph path)))
      (if (null? pair)
          ""
          (let ((f (cdr pair)))
            (update 'forge-node (car pair)
              (dict-set (dict-set f "body" body) "verify" verify))))))
  "Write a drafted body + verify source onto a node.")

(define forge/add-constraint
  (lambda (graph path note)
    (let ((pair (forge/node-by-path graph path)))
      (if (null? pair)
          ""
          (let ((f (cdr pair)))
            (let ((cs (json-parse (forge/field-str f "constraints"))))
              (seq
                (update 'forge-node (car pair)
                  (dict-set f "constraints"
                    (json-emit (forge/append cs (list note)))))
                (car pair)))))))
  "Append a reroll-with-note constraint; the next draft prompt includes it.")

;; ============================================================
;; FLAGS (graph-model writes; mechanical-flags computes)
;; ============================================================

(define forge/flag
  (lambda (node-path check detail)
    (create 'forge-flag
      (string-append
        "{\"node_path\":\"" node-path
        "\",\"check\":\"" check
        "\",\"detail\":\"" detail
        "\",\"resolved\":false"
        ",\"resolution\":\"\""
        "}")))
  "Raise a flag on a node. check ∈ capability|victory|dangling|cycle|orphan|size|judgment.")

(define forge/flags-for
  (lambda (node-path)
    (list/filter
      (lambda (pair)
        (string-eq (forge/field-str (cdr pair) "node_path") node-path))
      (rows 'forge-flag 500)))
  "All flag rows for a node path.")

(define forge/open-flags
  (lambda (graph)
    (list/filter
      (lambda (pair)
        (let ((f (cdr pair)))
          (if (string-eq (forge/field-str f "resolved") "true")
              #f
              (forge/path-in-graph? graph (forge/field-str f "node_path")))))
      (rows 'forge-flag 500)))
  "Unresolved flags whose node_path belongs to the graph.")

(define forge/path-in-graph?
  (lambda (graph node-path)
    (not (null? (forge/node-by-path graph node-path))))
  "True when node_path is a node of the graph.")

(define forge/resolve-flag
  (lambda (flag-id resolution)
    (let ((row (get 'forge-flag flag-id)))
      (if (null? row)
          ""
          (update 'forge-flag flag-id
            (dict-set (dict-set row "resolved" "true")
                      "resolution" resolution)))))
  "Close a flag with a resolution note.")

;; ============================================================
;; THE FRONTIER (planning is a computation, not a judgment)
;; ============================================================
;; free-vars walks a parsed body and collects symbols not bound
;; by any enclosing lambda/let. unresolved filters those against
;; image bindings (function-doc oracle) + sibling node names.
;; frontier = planned nodes whose unresolved set is empty.

;; ---- set helpers on lists of strings ----

(define forge/union
  (lambda (a b)
    (forge/union-into a b (list)))
  "Set union of two lists of strings (dedup, order-preserving).")

(define forge/union-into
  (lambda (a b acc)
    (if (null? a)
        (forge/finish-union b acc)
        (if (forge/member? (car a) acc)
            (forge/union-into (cdr a) b acc)
            (forge/union-into (cdr a) b (cons (car a) acc)))))
  "Fold a into acc, then fold b.")

(define forge/finish-union
  (lambda (b acc)
    (if (null? b)
        (forge/reverse acc)
        (if (forge/member? (car b) acc)
            (forge/finish-union (cdr b) acc)
            (forge/finish-union (cdr b) (cons (car b) acc)))))
  "Fold remaining b, then reverse.")

(define forge/reverse
  (lambda (xs)
    (forge/reverse-into xs (list)))
  "Reverse a list.")

(define forge/reverse-into
  (lambda (xs acc)
    (if (null? xs)
        acc
        (forge/reverse-into (cdr xs) (cons (car xs) acc))))
  "Tail-recursive reverse.")

(define forge/difference
  (lambda (xs exclude)
    (if (null? xs)
        (list)
        (if (forge/member? (car xs) exclude)
            (forge/difference (cdr xs) exclude)
            (cons (car xs) (forge/difference (cdr xs) exclude)))))
  "xs with any element in exclude removed.")

;; ---- special forms (excluded from free-var collection) ----

(define forge/constant?
  (lambda (name)
    (forge/member? name (list "nil" "#t" "#f" "true" "false" "else")))
  "True for literal constants that are not free variables.")

(define forge/special-form?
  (lambda (name)
    (forge/member? name (list
      "lambda" "let" "letrec" "if" "define" "quote" "seq" "begin"
      "match" "guard" "error" "else" "=>" "cond")))
  "Is name a Nopales special form / keyword?")

;; ---- image binding oracle ----

(define forge/image-bound?
  (lambda (name)
    (if (forge/special-form? name)
        #t
        (not (null? (function-doc name)))))
  "True when name resolves to a kernel prim, special form, or
   user binding. function-doc covers prims + user defs; special
   forms return () from function-doc so they are handled here.")

;; ---- bound name collection from lambda params / let bindings ----

(define forge/lambda-params
  (lambda (params)
    (if (null? params)
        (list)
        (cons (as-str (car params)) (forge/lambda-params (cdr params)))))
  "List of param name strings from a lambda formals list.")

(define forge/let-names
  (lambda (bindings)
    (if (null? bindings)
        (list)
        (let ((b (car bindings)))
          (let ((name (car b)))
            (cons (as-str name) (forge/let-names (cdr bindings)))))))
  "List of bound name strings from a let bindings list.")

;; ---- free-vars over a list of expressions ----

(define forge/free-vars-list
  (lambda (exprs bound)
    (if (null? exprs)
        (list)
        (forge/union
          (forge/free-vars (car exprs) bound)
          (forge/free-vars-list (cdr exprs) bound))))
  "Union of free-vars over each expr in a list.")

;; ---- free-vars over one expression ----

(define forge/free-vars
  (lambda (expr bound)
    (cond
      ((null? expr) (list))
      ((not (pair? expr))
       (if (string-eq (type-of expr) "symbol")
           (let ((name (as-str expr)))
             (if (or (forge/member? name bound)
                     (forge/constant? name))
                 (list)
                 (list name)))
           (list)))
      ((string-eq (as-str (car expr)) "quote") (list))
      ((string-eq (as-str (car expr)) "lambda")
       (forge/free-vars (caddr expr)
         (forge/append bound (forge/lambda-params (cadr expr)))))
      ((string-eq (as-str (car expr)) "let")
       (forge/free-vars (caddr expr)
         (forge/append bound (forge/let-names (cadr expr)))))
      ((string-eq (as-str (car expr)) "define") (list))
      ((string-eq (as-str (car expr)) "if")
       (forge/union
         (forge/free-vars (cadr expr) bound)
         (forge/union
           (forge/free-vars (caddr expr) bound)
           (forge/free-vars (car (cdr (cdr (cdr expr)))) bound))))
      (#t
       (forge/free-vars-list expr bound))))
  "Free variables of a parsed expression: symbols not bound by an
   enclosing lambda/let and not a special form. quote is a literal;
   define is top-level (handled by the caller).")

;; ---- unresolved: free-vars that reference nothing real ----

(define forge/sibling-names
  (lambda (graph path)
    (let ((siblings (forge/children-of graph (forge/parent-of path))))
      (forge/node-titles->names graph siblings)))
  "Names offered by sibling nodes (their label, derived from path).")

(define forge/parent-of
  (lambda (path)
    (forge/parent-of-acc path ""))
  "Parent path: drop the last /segment.")

(define forge/parent-of-acc
  (lambda (path acc)
    (if (string-eq path "")
        acc
        (forge/parent-of-acc (forge/tail-str path)
          (if (string-eq (forge/head-char path) "/") path acc))))
  "Scan to last '/', return everything before it (or \"\" for root).")

(define forge/node-titles->names
  (lambda (graph paths)
    (if (null? paths)
        (list)
        (let ((p (car paths)))
          (let ((node (forge/node-by-path graph p)))
            (if (null? node)
                (forge/node-titles->names graph (cdr paths))
                (cons (forge/field-str (cdr node) "title")
                      (forge/node-titles->names graph (cdr paths))))))))
  "Titles of the nodes at the given paths.")

(define forge/unresolved
  (lambda (graph path body-src)
    (let ((fvs (forge/free-vars (forge/first-form body-src) (list))))
      (forge/difference fvs
        (forge/append (list) (forge/image-bound-names fvs)))))
  "Free vars of body that resolve to nothing: not a prim, not a
   binding, not a special form. These are the stub-children candidates.
   Uses first-form to extract the lambda robustly (the drafter may
   append prose; read-string would error on trailing input).")

(define forge/image-bound-names
  (lambda (names)
    (if (null? names)
        (list)
        (if (forge/image-bound? (car names))
            (cons (car names) (forge/image-bound-names (cdr names)))
            (forge/image-bound-names (cdr names)))))
  "Subset of names that ARE bound in the image (prims + defs + specials).")

(define forge/frontier
  (lambda (graph)
    (forge/frontier-scan graph (forge/nodes-of-graph graph) (list)))
  "Planned nodes whose unresolved set is empty — leaves whose
   dependencies all exist. Bottom-up build order.")

(define forge/frontier-scan
  (lambda (graph nodes acc)
    (if (null? nodes)
        (forge/reverse acc)
        (let ((pair (car nodes)))
          (let ((f (cdr pair)))
            (let ((status (forge/field-str f "status"))
                  (path (forge/field-str f "path"))
                  (kind (forge/field-str f "kind"))
                  (body (forge/field-str f "body")))
              (let ((ready
                      (if (string-eq status "planned")
                          (if (string-eq kind "requirement")
                              #f
                              (if (string-eq body "")
                                  #t
                                  (forge/ready? graph path body)))
                          #f)))
                (forge/frontier-scan graph (cdr nodes)
                  (if ready (cons path acc) acc))))))))
  "Scan nodes; a planned node with empty body is trivially ready
   (no deps), otherwise check unresolved against the image.
   Requirements are never drafted (human-authored) so they are
   excluded from the frontier; the human approves them at the
   level gate, then propose-descent stubs their children.")

(define forge/ready?
  (lambda (graph path body)
    (let ((unres (forge/unresolved graph path body)))
      (null? unres)))
  "True when a drafted body has no unresolved references.")

;; ============================================================
;; MECHANICAL FLAGS (checks that run before the human wakes)
;; ============================================================
;; Each flagger is a small function over the graph. They run after
;; every draft and after every level-descent proposal. The approval
;; surface shows FLAGS, not nodes.

;; ---- capability escalation: child effects exceed parent declared ----

(define forge/flag-capability
  (lambda (graph path)
    (let ((node (forge/node-by-path graph path)))
      (if (null? node) (list)
      (let ((f (cdr node)))
        (let ((parent-path (forge/field-str f "parent")))
          (if (string-eq parent-path "") (list)
          (let ((parent (forge/node-by-path graph parent-path)))
            (if (null? parent) (list)
            (let ((body (forge/field-str f "body"))
                  (declared (forge/field-str (cdr parent) "effects")))
              (if (string-eq body "") (list)
              (let ((actual (forge-effects body)))
                (if (forge/effect-escalates? declared actual)
                    (list (forge/make-flag-spec path "capability"
                            (string-append "declared " declared
                              " but body is " actual)))
                    (list)))))))))))))
  "Raise a capability flag when a child's inferred effect exceeds the
   parent's declared badge.")

(define forge/effect-escalates?
  (lambda (declared actual)
    (not (forge/effect-subset? declared actual)))
  "True when actual is NOT a subset of declared — i.e. the child does
   more than the parent promised.")

(define forge/effect-subset?
  (lambda (declared actual)
    (cond
      ((string-eq actual "pure") #t)
      ((string-eq declared "pure") #f)
      ((string-eq declared actual) #t)
      ((string-eq declared "db-read")
       (string-eq actual "db-write"))
      (#t #f)))
  "Coarse effect lattice: pure < db-read < db-write < branch/publish/ai/net.
   A read-only parent allows a read child; a pure parent forbids any effect.")

;; ---- missing victory condition: empty verify source ----

(define forge/flag-victory
  (lambda (graph path)
    (let ((node (forge/node-by-path graph path)))
      (if (null? node) (list)
      (let ((f (cdr node)))
        (let ((verify (forge/field-str f "verify"))
              (kind (forge/field-str f "kind")))
          (if (string-eq kind "requirement")
              (list)
              (if (string-eq verify "")
                  (list (forge/make-flag-spec path "victory"
                          "node has no verify stanza — unapprovable"))
                  (list))))))))
  "Flag nodes (non-requirement) with no verify source.")

;; ---- dangling refs: free-vars referencing nothing ----

(define forge/flag-dangling
  (lambda (graph path)
    (let ((node (forge/node-by-path graph path)))
      (if (null? node) (list)
      (let ((f (cdr node)))
        (let ((body (forge/field-str f "body")))
          (if (string-eq body "") (list)
          (let ((unres (forge/unresolved graph path body)))
            (if (null? unres) (list)
            (list (forge/make-flag-spec path "dangling"
                    (string-append "unresolved references: "
                      (forge/str-join ", " unres))))))))))))
  "Flag drafted bodies with references that resolve to nothing.")

;; ---- orphan: node traces to no requirement ancestor ----

(define forge/flag-orphan
  (lambda (graph path)
    (if (forge/has-requirement-ancestor? graph path)
        (list)
        (list (forge/make-flag-spec path "orphan"
                "traces to no requirement ancestor"))))
  "Flag nodes with no requirement node in their ancestor chain.")

(define forge/has-requirement-ancestor?
  (lambda (graph path)
    (let ((node (forge/node-by-path graph path)))
      (if (null? node) #t
      (let ((f (cdr node)))
        (let ((kind (forge/field-str f "kind"))
              (parent (forge/field-str f "parent")))
          (cond
            ((string-eq kind "requirement") #t)
            ((string-eq parent "") #f)
            (#t (forge/has-requirement-ancestor? graph parent))))))))
  "Walk parent chain; true when a requirement kind node is found.")

;; ---- size heuristic: body too big to review in one glance ----

(define forge/flag-size
  (lambda (graph path)
    (let ((node (forge/node-by-path graph path)))
      (if (null? node) (list)
      (let ((f (cdr node)))
        (let ((body (forge/field-str f "body")))
          (if (forge/too-big? body 200)
              (list (forge/make-flag-spec path "size"
                      "body exceeds review-in-one-glance threshold"))
              (list)))))))
  "Flag bodies longer than the threshold (default ~200 chars).")

(define forge/too-big?
  (lambda (s limit)
    (> (forge/strlen s) limit))
  "True when string length exceeds limit.")

(define forge/strlen
  (lambda (s)
    (string-length s))
  "String length (wrapper for readability).")

;; ---- run all mechanical flaggers on a node ----

(define forge/mechanical-flags
  (lambda (graph path)
    (forge/append
      (forge/append
        (forge/append
          (forge/flag-capability graph path)
          (forge/flag-victory graph path))
        (forge/append
          (forge/flag-dangling graph path)
          (forge/flag-orphan graph path)))
      (forge/flag-size graph path)))
  "All mechanical flags for one node. Returns a list of flag-spec pairs:
   (check . detail). The caller raises them as forge-flag rows.")

(define forge/make-flag-spec
  (lambda (path check detail)
    (cons check detail))
  "A flag spec is (check . detail); raised as a row by forge/raise-flags.")

(define forge/raise-flags
  (lambda (graph path specs)
    (if (null? specs) (list)
    (let ((spec (car specs)))
      (seq
        (forge/flag path (car spec) (cdr spec))
        (forge/raise-flags graph path (cdr specs))))))
  "Raise each flag-spec as a forge-flag row on the node.")

(define forge/scan-mechanical-flags
  (lambda (graph)
    (forge/scan-flags-nodes graph (forge/nodes-of-graph graph)))
  "Run mechanical flaggers over every node in the graph and raise flags.")

(define forge/scan-flags-nodes
  (lambda (graph nodes)
    (if (null? nodes) (list)
    (let ((pair (car nodes)))
      (let ((path (forge/field-str (cdr pair) "path")))
        (seq
          (forge/raise-flags graph path
            (forge/mechanical-flags graph path))
          (forge/scan-flags-nodes graph (cdr nodes)))))))
  "Scan all nodes, raise their mechanical flags.")

;; ============================================================
;; THE LOOP (the interpreter that replaces plan/make/review)
;; ============================================================
;; forge/step is the whole state machine. It is deliberately boring:
;;   1. open judgment flags → do nothing; BLOCKED on the human
;;   2. current level has unapproved nodes → run flags, present, stop
;;   3. frontier non-empty → pick one, draft it (LLM), write, flag
;;   4. else → propose descent (stub children), stop (human gate)

;; ---- depth helpers ----

(define forge/levels
  (lambda (graph)
    (forge/levels-scan (forge/nodes-of-graph graph) (list)))
  "Distinct depths present in the graph, ascending.")

(define forge/levels-scan
  (lambda (nodes acc)
    (if (null? nodes)
        (forge/sort-numbers acc)
        (let ((d (forge/node-depth (forge/field-str (cdr (car nodes)) "path"))))
          (forge/levels-scan (cdr nodes)
            (if (forge/member? d acc) acc (cons d acc))))))
  "Collect distinct depths.")

(define forge/sort-numbers
  (lambda (xs)
    (if (null? xs) (list)
    (forge/insert-sorted (car xs) (forge/sort-numbers (cdr xs)))))
  "Insertion sort for small lists of numbers.")

(define forge/insert-sorted
  (lambda (x xs)
    (if (null? xs) (list x)
    (if (<= x (car xs))
        (cons x xs)
        (cons (car xs) (forge/insert-sorted x (cdr xs))))))
  "Insert x into sorted xs.")

(define forge/nodes-at-depth
  (lambda (graph d)
    (list/filter
      (lambda (pair)
        (string-eq (as-str (forge/node-depth (forge/field-str (cdr pair) "path")))
                   (as-str d)))
      (forge/nodes-of-graph graph)))
  "All node pairs at a given depth.")

(define forge/status>=?
  (lambda (status target)
    (forge/status-rank status >= (forge/status-rank target 0)))
  "Status ordering: planned < drafted < verified < approved < implemented.")

(define forge/status-rank
  (lambda (status fallback)
    (cond
      ((string-eq status "planned") 0)
      ((string-eq status "drafted") 1)
      ((string-eq status "verified") 2)
      ((string-eq status "approved") 3)
      ((string-eq status "implemented") 4)
      (#t fallback)))
  "Numeric rank of a status string.")

;; ---- the draft prompt (context assembly) ----

(define forge/draft-prompt
  (lambda (graph path)
    (let ((node (forge/node-by-path graph path)))
      (if (null? node) ""
      (let ((f (cdr node)))
        (let ((title (forge/field-str f "title"))
              (kind (forge/field-str f "kind"))
              (constraints (json-parse (forge/field-str f "constraints"))))
          (string-append
            "You are drafting a Nopales (Lisp-1) function for a Pricklypear spec graph.\n"
            "Node: " path "\n"
            "Kind: " kind "\n"
            "Purpose: " title "\n"
            "Respond with ONLY a single Lisp s-expression: a (lambda ...) body.\n"
            "No markdown, no commentary.\n"
            "Available prims: + - * = < > list car cdr cons null? string-append str\n"
            "  get create update delete rows dict-get dict-set render-html\n"
            (forge/constraints-text constraints)))))))
  "Assemble the draft prompt from a node's context + accumulated constraints.")

(define forge/constraints-text
  (lambda (cs)
    (if (null? cs) ""
    (string-append "\nConstraints from prior rerolls:\n"
      (forge/str-join "\n" cs) "\n")))
  "Render constraints as prompt text.")

;; ---- draft a node (call the LLM) ----

(define forge/draft-node
  (lambda (graph path)
    (let ((prompt (forge/draft-prompt graph path)))
      (let ((resp (llm-chat "You are a Pricklypear Lisp author. Respond ONLY with a single (lambda ...) s-expression. No markdown, no prose, no commentary." prompt)))
        (if (forge/llm-error? resp)
            (seq
              (forge/flag path "judgment"
                (string-append "drafter failed: " resp))
              (forge/commit-draft graph path ""))
            (forge/commit-draft graph path (forge/first-form resp))))))
  "Call the LLM drafter for a node; write the parsed body + run flags.
   On LLM error, raise a judgment flag and leave the body empty (no
   error string written as body).")

(define forge/llm-error?
  (lambda (s)
    (string-starts-with? s "llm-chat:"))
  "True when the LLM call returned an error string rather than content.")

(define forge/first-form
  (lambda (s)
    (forge/first-form-at s 0))
  "Extract the first balanced s-expression from the LLM response,
   ignoring trailing prose/reasoning. Scans for the first '(',
   tracks paren depth respecting \" strings, returns the substring
   up to the matching close. Robust to dots and other prose tokens.")

(define forge/first-form-at
  (lambda (s i)
    (let ((n (string-length s)))
      (if (>= i n)
          ""
          (if (string-eq (substring s i 1) "(")
              (let ((end (forge/balanced-end s i 0 #f)))
                (if (<= end i)
                    ""
                    (substring s i (- end i))))
              (forge/first-form-at s (+ i 1))))))
  "Find the first '(' then return substring from there to its match.")

(define forge/balanced-end
  (lambda (s i depth in-str)
    (let ((n (string-length s)))
      (if (>= i n)
          i
          (let ((c (substring s i 1)))
            (cond
              ((string-eq c "\"")
               (if in-str
                   (forge/balanced-end s (+ i 1) depth #f)
                   (forge/balanced-end s (+ i 1) depth #t)))
              (in-str
               (forge/balanced-end s (+ i 1) depth #t))
              ((string-eq c "(")
               (forge/balanced-end s (+ i 1) (+ depth 1) #f))
              ((string-eq c ")")
               (if (<= depth 1)
                   (+ i 1)
                   (forge/balanced-end s (+ i 1) (- depth 1) #f)))
              (#t
               (forge/balanced-end s (+ i 1) depth #f)))))))
  "Return the index just past the close paren matching the open
   paren at the starting depth.")

;; ---- commit a draft: write body, infer effects, set status, flag ----

(define forge/commit-draft
  (lambda (graph path body-src)
    (seq
      (forge/set-body-verify graph path body-src "")
      (forge/set-status graph path "drafted")
      (forge/set-inferred-effects graph path body-src)
      (forge/raise-flags graph path
        (forge/mechanical-flags graph path))
      path))
  "Write the drafted body, mark drafted, set inferred effects, raise flags.")

(define forge/set-inferred-effects
  (lambda (graph path body-src)
    (let ((pair (forge/node-by-path graph path)))
      (if (null? pair) ""
      (let ((f (cdr pair)))
        (let ((badge (forge-effects body-src)))
          (update 'forge-node (car pair)
            (dict-set f "effects" badge)))))))
  "Set the node's effects field to the inferred badge.")

;; ---- stub children: unresolved refs become planned nodes ----

(define forge/stub-children
  (lambda (graph path)
    (let ((node (forge/node-by-path graph path)))
      (if (null? node) (list)
      (let ((f (cdr node)))
        (let ((body (forge/field-str f "body")))
          (if (string-eq body "") (list)
          (let ((unres (forge/unresolved graph path body)))
            (forge/stub-list graph path unres))))))))
  "Create a planned child node for each unresolved reference in the body.")

(define forge/stub-list
  (lambda (graph parent-path names)
    (if (null? names) (list)
    (let ((name (car names)))
      (let ((child-path (string-append parent-path "/" name)))
        (seq
          (forge/add-node graph child-path "function" name parent-path)
          (forge/stub-list graph parent-path (cdr names)))))))
  "Stub each unresolved name as a child node.")

;; ---- the step state machine ----

(define forge/step
  (lambda (graph-name)
    (let ((graph (forge/graph-by-name graph-name)))
      (if (null? graph) "graph not found"
      (let ((judgment (forge/open-judgment-flags graph-name)))
        (cond
          ((not (null? judgment)) "blocked: open judgment flags — see /forge")
          ((forge/has-unapproved-level? graph-name) "pending: level needs approval — see /forge")
          ((forge/frontier-non-empty? graph-name) (forge/draft-frontier graph-name))
          (#t (forge/propose-descent graph-name)))))))
  "One step of the loop. Returns a status string or the drafted path.")

(define forge/open-judgment-flags
  (lambda (graph-name)
    (list/filter
      (lambda (pair)
        (string-eq (forge/field-str (cdr pair) "check") "judgment"))
      (forge/open-flags graph-name)))
  "Open flags with check=judgment (the human escape hatch).")

(define forge/has-unapproved-level?
  (lambda (graph-name)
    (forge/has-unapproved-scan graph-name (forge/nodes-of-graph-by-name graph-name)))
  "True when any node at the current working depth is below approved.")

(define forge/nodes-of-graph-by-name
  (lambda (graph-name)
    (list/filter
      (lambda (pair)
        (string-eq (forge/field-str (cdr pair) "graph") graph-name))
      (rows 'forge-node 500)))
  "All node pairs for a graph name (rows scan).")

(define forge/has-unapproved-scan
  (lambda (graph-name nodes)
    (if (null? nodes) #f
    (let ((f (cdr (car nodes))))
      (let ((status (forge/field-str f "status")))
        (cond
          ((string-eq status "drafted") #t)
          ((string-eq status "verified") #t)
          (#t (forge/has-unapproved-scan graph-name (cdr nodes))))))))
  "A drafted/verified node is unapproved (past planned, not yet approved).
   Planned/approved/implemented nodes are not unapproved — keep scanning.")

(define forge/frontier-non-empty?
  (lambda (graph-name)
    (not (null? (forge/frontier graph-name))))
  "True when the computed frontier has ready nodes.")

(define forge/draft-frontier
  (lambda (graph-name)
    (let ((f (forge/frontier graph-name)))
      (if (null? f) "frontier empty"
      (forge/draft-node graph-name (car f)))))
  "Draft the first ready node on the frontier.")

(define forge/propose-descent
  (lambda (graph-name)
    (forge/descent-scan graph-name (forge/nodes-of-graph-by-name graph-name)))
  "Stub children for every approved node with a body; propose the next level.")

(define forge/descent-scan
  (lambda (graph-name nodes)
    (if (null? nodes) "done: no further descent"
    (let ((f (cdr (car nodes))))
      (let ((status (forge/field-str f "status"))
            (path (forge/field-str f "path")))
        (cond
          ((string-eq status "approved")
           (seq
             (forge/stub-children graph-name path)
             (forge/descent-scan graph-name (cdr nodes))))
          (#t (forge/descent-scan graph-name (cdr nodes))))))))
  "For each approved node, stub its unresolved refs as children (descent).")
