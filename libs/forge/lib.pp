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
       (let ((name (as-str expr)))
         (if (forge/member? name bound)
             (list)
             (list name))))
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
    (let ((fvs (forge/free-vars (read-string body-src) (list))))
      (forge/difference fvs
        (forge/append (list) (forge/image-bound-names fvs)))))
  "Free vars of body that resolve to nothing: not a prim, not a
   binding, not a special form. These are the stub-children candidates.")

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
                  (body (forge/field-str f "body")))
              (let ((ready
                      (if (string-eq status "planned")
                          (if (string-eq body "")
                              #t
                              (forge/ready? graph path body))
                          #f)))
                (forge/frontier-scan graph (cdr nodes)
                  (if ready (cons path acc) acc))))))))
  "Scan nodes; a planned node with empty body is trivially ready
   (no deps), otherwise check unresolved against the image.")

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
