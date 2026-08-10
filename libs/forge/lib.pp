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

(define forge/node-by-path-in
  (lambda (nodes path)
    (forge/find-row
      (lambda (pair) (string-eq (forge/field-str (cdr pair) "path") path))
      nodes))
  "Find a node in a PRE-FETCHED nodes list (the graph's own nodes, not
   all 500 rows). O(nodes-in-graph) vs O(500) for node-by-path. Use this
   in any loop that touches many nodes (flaggers, frontier, scans) to
   avoid re-scanning the whole forge-node table per lookup.")

(define forge/node-in-graph?
  (lambda (graph node-path)
    (let ((g (string-append graph "/")))
      (or (string-eq node-path graph)
          (string-starts-with? node-path g))))
  "True when node-path belongs to graph — by path PREFIX, no row scan.
   Paths are graph/scoped/... so node-path starts with graph/ (or equals
   graph for the root). Replaces path-in-graph? (which called node-by-path
   per flag, O(flags×nodes)).")

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
              (forge/node-in-graph? graph (forge/field-str f "node_path")))))
      (rows 'forge-flag 500)))
  "Unresolved flags whose node_path belongs to the graph. Uses the path
   prefix check (node-in-graph?) — O(flags) with cheap string ops, no
   per-flag node row scan.")

(define forge/path-in-graph?
  (lambda (graph node-path)
    (forge/node-in-graph? graph node-path))
  "Legacy alias for node-in-graph? (prefix check, no row scan).")

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
  (lambda (nodes path)
    (let ((node (forge/node-by-path-in nodes path)))
      (if (null? node) (list)
      (let ((f (cdr node)))
        (let ((parent-path (forge/field-str f "parent")))
          (if (string-eq parent-path "") (list)
          (let ((parent (forge/node-by-path-in nodes parent-path)))
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
   parent's declared badge. Takes the pre-fetched nodes list.")

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
  (lambda (nodes path)
    (let ((node (forge/node-by-path-in nodes path)))
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
  (lambda (nodes path)
    (let ((node (forge/node-by-path-in nodes path)))
      (if (null? node) (list)
      (let ((f (cdr node)))
        (let ((body (forge/field-str f "body")))
          (if (string-eq body "") (list)
          (let ((unres (forge/unresolved "" path body)))
            (if (null? unres) (list)
            (list (forge/make-flag-spec path "dangling"
                    (string-append "unresolved references: "
                      (forge/str-join ", " unres))))))))))))
  "Flag drafted bodies with references that resolve to nothing.")

;; ---- orphan: node traces to no requirement ancestor ----

(define forge/flag-orphan
  (lambda (nodes path)
    (if (forge/has-requirement-ancestor? nodes path)
        (list)
        (list (forge/make-flag-spec path "orphan"
                "traces to no requirement ancestor"))))
  "Flag nodes with no requirement node in their ancestor chain.")

(define forge/has-requirement-ancestor?
  (lambda (nodes path)
    (let ((node (forge/node-by-path-in nodes path)))
      (if (null? node) #t
      (let ((f (cdr node)))
        (let ((kind (forge/field-str f "kind"))
              (parent (forge/field-str f "parent")))
          (cond
            ((string-eq kind "requirement") #t)
            ((string-eq parent "") #f)
            (#t (forge/has-requirement-ancestor? nodes parent))))))))
  "Walk parent chain; true when a requirement kind node is found.")

;; ---- size heuristic: body too big to review in one glance ----

(define forge/flag-size
  (lambda (nodes path)
    (let ((node (forge/node-by-path-in nodes path)))
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
  (lambda (nodes path)
    (forge/append
      (forge/append
        (forge/append
          (forge/flag-capability nodes path)
          (forge/flag-victory nodes path))
        (forge/append
          (forge/flag-dangling nodes path)
          (forge/flag-orphan nodes path)))
      (forge/flag-size nodes path)))
  "All mechanical flags for one node. Returns a list of flag-spec pairs:
   (check . detail). Takes the pre-fetched nodes list so the flaggers
   search the graph's own nodes, not all 500 rows.")

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
            (forge/mechanical-flags nodes path))
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
            "Output TWO s-expressions on separate lines: (1) the (lambda ...) body, (2) a verify expression using `fn` that returns #t if correct.\n"
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

;; ---- draft a node (agentic: eval-and-iterate, then harvest last form) ----

(define forge/drafter-seed
  (string-append
    "You are a Nopales (Lisp-1) drafter for a Pricklypear spec graph. "
    "Your job: produce ONE working (lambda ...) body for a single node.\n"
    "\n"
    "PROCESS (mandatory):\n"
    "1. Use eval_expr to TEST your draft. Call it like: "
    "((lambda (a b) (+ a b)) 1 2) — it returns the value or an error.\n"
    "2. If eval_expr errors, READ the error, FIX the lambda, and re-eval. "
    "Iterate until it evaluates cleanly with realistic sample args.\n"
    "3. Do NOT use install_function, register_route, or smoke_route — "
    "forge manages the body in a graph node, NOT on the branch. "
    "install_function is forbidden; only eval_expr is allowed for testing.\n"
    "4. Your FINAL message must contain EXACTLY TWO s-expression forms,\n"
    "   each on its own line, nothing else:\n"
    "   Line 1: the (lambda ...) body you verified — copy it verbatim.\n"
    "   Line 2: a VERIFY expression that uses `fn` (which will be bound\n"
    "   to your lambda) and returns #t if the function is correct.\n"
    "   Example verify: (string? (fn \"sample\" \"arg\"))\n"
    "   The verify is your test — it must call fn with realistic args\n"
    "   and check the result. Count parens carefully on both lines.\n"
    "\n"
    "NOPALES SYNTAX REMINDERS:\n"
    "  - let takes exactly ONE body form; use (seq a b) for multi-step.\n"
    "  - let* is sequential binding: (let* ((a 1) (b (+ a 1))) b) => 2.\n"
    "  - There is NO named-let (no (let loop ...)); use list/foldl or recursion.\n"
    "  - dict-get returns JSON values as strings; compare with string-eq.\n"
    "  - substring takes (start, length), not (start, end).\n"
    "  - create: (create 'type-name (dict-set* \"{}\" (list \"field1\" val1 \"field2\" val2))) "
    "-> returns an id string. Fields MUST be a JSON dict string built via "
    "dict-set*, NOT an assoc list.\n"
    "  - rows: (rows 'type-name 200) -> list of (id . json-string) pairs; "
    "(cdr row) is the JSON field string; use (dict-get (cdr row) \"field\").\n"
    "  - CRITICAL: prims return error STRINGS (e.g. \"create: bad args\") "
    "instead of throwing. After eval_expr, CHECK the returned value: if it "
    "contains \": bad args\" or \"error\" or starts with the prim name + \":\", "
    "it FAILED — fix and re-test. A returned string is not always success.\n"
    "\n"
    "AVAILABLE PRIMS: + - * = < > list car cdr cons null? count "
    "string-append str type-of whoami list-bindings dict-get dict-set "
    "dict-set* render-html get create update delete rows as-str string-eq "
    "string-length string-starts-with? request-param request-method "
    "list/map list/filter list/foldl with-transaction")
  "Seed for the forge drafter agent session. Tells the LLM to use eval_expr to test-and-iterate, forbids install/route tools, and pins Nopales syntax gotchas. The drafter returns TWO forms: the (lambda ...) body, then a verify expression that uses `fn` as the bound name.")

(define forge/draft-node
  (lambda (graph path)
    (let* ((prompt (forge/draft-prompt graph path))
           (sid (agent-session-new forge/drafter-seed #t))
           (resp (agent-say sid prompt 12))
           (body (forge/first-form resp))
           (verify (forge/second-form resp)))
      (seq
        (agent-session-delete sid)
        (if (forge/drafter-failed? resp)
            (seq
              (forge/flag path "judgment"
                (string-append "drafter failed: " resp))
              (forge/commit-draft graph path ""))
            (if (forge/parse-ok? body)
                (forge/commit-draft graph path body verify)
                (seq
                  (forge/flag path "judgment"
                    "drafter body unparseable (missing parens or syntax error)")
                  (forge/commit-draft graph path "")))))))
  "Agentic draft: spin up a drafter agent session (eval_expr enabled), let it test-and-iterate up to 12 steps, harvest the body (first form) + verify (second form), parse-check the body, then commit-draft. The agent loop runs in OCaml (per-tool budget). On failure or unparseable harvest, raise a judgment flag and leave the body empty.")

(define forge/parse-ok?
  (lambda (s)
    (if (string-eq s "")
        #f
        (guard (lambda (e) #f) (seq (read-string s) #t))))
  "True when s parses as valid Lisp. read-string raises on parse error; guard catches it and returns #f. Empty string is #f.")

(define forge/drafter-failed?
  (lambda (s)
    (or (string-starts-with? s "agent error:")
        (string-starts-with? s "agent: max steps exceeded")))
  "True when the agent session errored or exhausted its steps without a clean final turn. (agent-say returns these prefixes on failure.)")

(define forge/llm-error?
  (lambda (s)
    (string-starts-with? s "llm-chat:"))
  "Legacy: true when a raw llm-chat call returned an error string. Kept for frontier/review paths that still use llm-chat directly.")

;; ---- tri-state review: a SECOND LLM session judges the draft ----
;; Separation of powers: the drafter writes code; the reviewer (a different
;; system prompt, not the drafter's session) returns approve | deny | flag.
;; Runs only when mechanical flags are clean (mechanical checks gate first).

(define forge/reviewer-seed
  (string-append
    "You are a Nopales code reviewer for a Pricklypear spec graph. "
    "A drafter (a different LLM session) produced a function body. "
    "Judge it for correctness and style.\n"
    "\n"
    "Respond with EXACTLY one line, starting with one of:\n"
    "  APPROVE — the body is correct and clean\n"
    "  DENY: <reason> — the body is wrong (bad syntax, wrong prim args, logic error); the drafter will redraft with this reason as a constraint\n"
    "  FLAG: <reason> — you are unsure or it needs a human judgment\n"
    "\n"
    "Check for: balanced parens, valid Nopales prims (create takes dict-set* JSON not assoc lists; let takes one body form; no named-let), correct field access (dict-get on cdr of row), and that the body matches the node purpose. Do not nitpick style; only flag real defects.")
  "Seed for the reviewer LLM. Separation of powers: never the drafter's session.")

(define forge/review-node
  (lambda (graph path)
    (let* ((node (forge/node-by-path graph path)))
      (if (null? node) "review: node not found"
      (let* ((f (cdr node))
             (body (forge/field-str f "body"))
             (title (forge/field-str f "title")))
        (if (string-eq body "") "review: empty body"
        (let* ((all-flags (forge/flags-for path))
               (mflags (list/filter (lambda (p) (not (string-eq (forge/field-str (cdr p) "resolved") "true"))) all-flags)))
          (if (not (null? mflags)) "review: skipped (mechanical flags open)"
          (let* ((prompt (forge/review-prompt path body title))
                 (verdict (llm-chat forge/reviewer-seed prompt)))
            (if (forge/llm-error? verdict)
                (seq
                  (forge/flag path "judgment"
                    (string-append "reviewer call failed: " verdict))
                  "review: reviewer call failed")
                (forge/apply-review verdict graph path))))))))))
  "Tri-state review of a drafted node. Skips if mechanical flags are open
   (mechanical checks gate first). Calls the reviewer LLM (a separate
   session via llm-chat — no tools, pure judgment). Returns a status string.")

(define forge/review-prompt
  (lambda (path body title)
    (string-append
      "Review this Nopales function draft.\n\n"
      "Node: " path "\n"
      "Purpose: " title "\n"
      "Body:\n" body "\n\n"
      "Respond with one line: APPROVE, DENY: <reason>, or FLAG: <reason>."))
  "Assemble the review prompt from the node's body + purpose.")

(define forge/apply-review
  (lambda (verdict graph path)
    (let* ((first-line (car (string-split verdict "\n")))
           (head (forge/trim first-line)))
      (cond
        ((string-starts-with? head "APPROVE") "review: approved")
        ((string-starts-with? head "DENY")
         (let ((reason (forge/review-reason head)))
           (seq
             (forge/add-constraint graph path (string-append "reviewer denied: " reason))
             (forge/flag path "judgment"
               (string-append "reviewer denied: " reason))
             "review: denied (constraint + judgment flag added)")))
        ((string-starts-with? head "FLAG")
         (let ((reason (forge/review-reason head)))
           (seq
             (forge/flag path "judgment"
               (string-append "reviewer flagged: " reason))
             "review: flagged (judgment flag added)")))
        (#t
         (seq
           (forge/flag path "judgment"
             (string-append "reviewer verdict unparseable: " head))
           "review: unparseable verdict")))))
  "Apply the reviewer's verdict. Takes the FIRST line of the verdict, trims it,
   and checks prefixes (APPROVE/DENY/FLAG). APPROVE: no-op (stays drafted,
   human gates the level). DENY: append reason as a constraint + judgment flag.
   FLAG: judgment flag. Unknown: judgment flag. Robust to trailing commentary.")

(define forge/review-reason
  (lambda (head)
    (let ((parts (string-split head ":")))
      (if (null? (cdr parts))
          "no reason given"
          (forge/trim (forge/str-join ":" (cdr parts))))))
  "Extract the reason from a DENY:/FLAG: verdict line — the text after the
   first colon, trimmed. Joins on ':' in case the reason itself has colons.")

(define forge/trim
  (lambda (s)
    (string-trim s))
  "Trim whitespace (wrapper over string-trim).")

;; ---- verify: run the drafter's test, resolve the victory flag ----
;; The verify is a Nopales expression that uses `fn` (bound to the body).
;; verify-node evals (let ((fn body)) verify) — if truthy, the victory
;; flag resolves and the node advances to verified. This is the TDD loop:
;; the drafter writes a test alongside the code; the loop runs it.

(define forge/verify-node
  (lambda (graph path)
    (let ((node (forge/node-by-path graph path)))
      (if (null? node) "verify: node not found"
      (let ((f (cdr node)))
        (let ((body (forge/field-str f "body"))
              (verify (forge/field-str f "verify")))
          (if (string-eq body "") "verify: empty body"
          (if (string-eq verify "") "verify: empty verify (victory flag stays open)"
          (if (not (forge/parse-ok? verify))
              (seq
                (forge/flag path "judgment"
                  "verify expression unparseable")
                "verify: unparseable")
          (let ((test-src
                  (string-append "(let ((fn " body ")) " verify ")")))
            (let ((result (forge/eval-or-error test-src)))
              (if (forge/eval-failed? result)
                  (seq
                    (forge/flag path "judgment"
                      (string-append "verify failed: " result))
                    "verify: failed (judgment flag)")
                  (if (forge/truthy? result)
                      (seq
                        (forge/resolve-victory-flag graph path)
                        (forge/set-status graph path "verified")
                        "verify: passed (victory flag resolved, status verified)")
                      (seq
                        (forge/flag path "judgment"
                          "verify returned falsey — function fails its own test")
                        "verify: returned falsey (judgment flag)")))))))))))))
  "Run the node's verify expression. Evals (let ((fn body)) verify). If it
   returns truthy, resolve the victory flag and set status verified. If it
   errors or returns falsey, raise a judgment flag. This is the TDD gate:
   the drafter's test must pass before review fires.")

(define forge/eval-or-error
  (lambda (src)
    (guard (lambda (e) (string-append "error: " (as-str e)))
      (str (eval-string src))))
  "Eval a string; return the str'd result on success or 'error: ...' on exception.")

(define forge/eval-failed?
  (lambda (s)
    (string-starts-with? s "error:"))
  "True when eval-or-error caught an exception.")

(define forge/truthy?
  (lambda (s)
    (not (or (string-eq s "#f") (string-eq s "()") (string-eq s ""))))
  "True when the eval result string is truthy (not #f, not (), not empty).")

(define forge/resolve-victory-flag
  (lambda (graph path)
    (let ((flags (forge/flags-for path)))
      (forge/resolve-victory-scan path flags)))
  "Resolve all open victory flags on a node (the verify passed).")

(define forge/resolve-victory-scan
  (lambda (path flags)
    (if (null? flags)
        ""
        (let ((pair (car flags)))
          (let ((fid (car pair))
                (check (forge/field-str (cdr pair) "check"))
                (resolved (forge/field-str (cdr pair) "resolved")))
            (seq
              (if (and (string-eq check "victory")
                       (not (string-eq resolved "true")))
                  (forge/resolve-flag fid "verify passed")
                  "")
              (forge/resolve-victory-scan path (cdr flags)))))))
  "Scan flags, resolve each open victory flag.")

(define forge/first-form
  (lambda (s)
    (forge/first-form-at s 0))
  "Extract the first balanced s-expression from the LLM response,
   ignoring trailing prose/reasoning. Scans for the first '(',
   tracks paren depth respecting \" strings, returns the substring
   up to the matching close. Robust to dots and other prose tokens.")

(define forge/second-form
  (lambda (s)
    (let ((first-end (forge/first-form-end s 0)))
      (if (<= first-end 0)
          ""
          (forge/first-form-at s first-end))))
  "Extract the SECOND balanced s-expression (the verify expression).
   Finds the end of the first form, then scans from there for the next '('.
   Returns \"\" if there is no second form.")

(define forge/first-form-end
  (lambda (s i)
    (let ((n (string-length s)))
      (if (>= i n)
          0
          (if (string-eq (substring s i 1) "(")
              (let ((end (forge/balanced-end s i 0 #f)))
                (if (<= end i) 0 end))
              (forge/first-form-end s (+ i 1))))))
  "Return the index just past the end of the first balanced form, or 0.")

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
  (lambda (graph path body-src verify-src)
    (seq
      (forge/set-body-verify graph path body-src verify-src)
      (forge/set-status graph path "drafted")
      (forge/set-inferred-effects graph path body-src)
      (let ((nodes (forge/nodes-of-graph graph)))
        (seq
          (forge/raise-flags graph path
            (forge/mechanical-flags nodes path))
          path))))
  "Write the drafted body, mark drafted, set inferred effects, raise flags.
   Fetches the graph's nodes AFTER the body/effects writes so the flaggers
   see the fresh body (not a stale empty one). The five mechanical flaggers
   search the graph's own nodes (not all 500 rows) — O(nodes-in-graph).")

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
    (forge/nodes-of-graph graph-name))
  "Alias for nodes-of-graph (kept for step/descent callers).")

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
      (let ((path (car f)))
        (seq
          (forge/draft-node graph-name path)
          (forge/verify-node graph-name path)
          (forge/review-node graph-name path))))))
  "Draft the first ready node, run its verify (TDD gate), then tri-state review. The verify must pass before review fires — mechanical flags (including victory) gate first.")

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

;; ---- forge/run: the loop as a job (borge/jobs.borg) ----

(define forge/run
  (lambda (graph-name)
    (submit-job "forge/step-job" (string-append "{\"graph\":\"" graph-name "\"}")))
  "Submit forge/step as a background job. The runner evals (forge/step-job {\"graph\":...})
   on your branch; the loop persists across HTTP requests and survives the
   laptop disconnecting. Poll (job-status id) to watch progress. The step-job
   function below is the job's entry point — it calls forge/step once per run.
   Re-submit (forge/run graph) after each gate (approval) to continue the descent.")

(define forge/step-job
  (lambda (args)
    (let ((graph-name (dict-get args "graph")))
      (if (null? graph-name)
          "no graph in args"
          (forge/step graph-name))))
  "Job entry point for forge/run. Calls forge/step once. The runner records
   the step result; the caller re-submits after approving the level. This is
   the loop-persists-across-requests shape from borge/forge.borg forge/run.")
