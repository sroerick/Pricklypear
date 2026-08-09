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
