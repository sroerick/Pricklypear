(** Pricklypear Image — Primitive registry.

    Primitives are OCaml functions exposed to Lisp. They live in the image
    (not the lisp/ package) because they're I/O-shaped: read from store,
    emit htmx, persist, etc. The lisp/ package is pure I/O-free.

    Spec [lisp-interpreter.primitives] lists the full ~20; for the minimal
    image, ~5 are sufficient to demo the loop. *)
open Pricklypear_lisp.Ast
(* Prim fn: evaluated args -> value (or raise). Direct-style under Eio floor.
   Pure and DB-touching prims share the same ABI; I/O blocks the fiber. *)
type prim_fn = value list -> value

(* Alias: same as prim_fn after decolor. *)
type prim_fn_sync = value list -> value

type t = (string, prim_fn) Hashtbl.t

let make () : t = Hashtbl.create 16

let register (t : t) (name : string) (fn : prim_fn) : unit =
  Hashtbl.replace t name fn

let register_sync (t : t) (name : string) (fn : prim_fn_sync) : unit =
  Hashtbl.replace t name (fun args -> (fn args))

(* --- Server-only locality metadata (borge/remote-eval.borg
   server-endpoint). ---

   Some prims MUST execute server-side to mean anything: route, fork,
   merge, checkout, create-user, is-admin? (auth admin). The
   remote client's /api/prim endpoint refuses these with a structured
   `server_only` error instead of pretending to proxy them.

   The classification is DERIVED from this registry metadata, never a
   hand-maintained list in the endpoint: a prim is server-only iff it
   was registered via [register_server_only]/[register_server_only_sync]
   below. The set is registry-wide (server-only locality is a property
   of the prim NAME, identical for every session's table) and lives in
   this registry module so the endpoint reads the same metadata the
   registration sites wrote. *) 
let server_only : (string, unit) Hashtbl.t = Hashtbl.create 8

(** Register [name] AND mark it server-only (refused by a remote
    client's /api/prim). Metadata lives in this registry; the endpoint
    reads it via [is_server_only]. *) 
let register_server_only (t : t) (name : string) (fn : prim_fn) : unit =
  Hashtbl.replace server_only name ();
  register t name fn

let register_server_only_sync
    (t : t) (name : string) (fn : prim_fn_sync) : unit =
  Hashtbl.replace server_only name ();
  register_sync t name fn

(** Refuse-or-forward classification, derived from registry metadata.
    A prim is server-only iff it was registered via
    [register_server_only*]. *) 
let is_server_only (_t : t) (name : string) : bool =
  Hashtbl.mem server_only name

let lookup (t : t) (name : string) : prim_fn option =
  Hashtbl.find_opt t name

(* --- Refuse-or-forward dispatch for /api/prim (borge/remote-eval.borg
   server-endpoint). ---

   One prim call, dispatched exactly the way the image rim services a
   [Prim_call] during eval (same table lookup, same [fn args]), but
   with the server-only refusal in front of it. Classification is
   DERIVED from registry metadata ([is_server_only]), never a hand
   list. Factored here (rather than inline in http_pages.ml) so the
   endpoint is thin and the refusal/execution logic is unit-testable
   without spinning up an HTTP server. *) 
type prim_result =
  | Prim_ok of value
  | Prim_refused_server_only of string * string (* name * reason *)
  | Prim_error of exn

let dispatch (t : t) (name : string) (args : value list) : prim_result =
  if is_server_only t name then
    Prim_refused_server_only (name, "prim must execute server-side")
  else
    match lookup t name with
    | Some fn ->
        (try Prim_ok (fn args) with exn -> Prim_error exn)
    | None ->
        Prim_error
          (Interp_error
             (Prim_error (name, "unregistered primitive", "prim_error")))

(* Numeric helpers: VInt | VFloat with promotion (int+float → float). *)
(* --- Session-aware primitives ---

   These need access to the running session (env, manifest). They're
   registered as closures at session-make time via
   [register_session_prims]. The signature stays prim_fn (value list
   -> value) — the closure captures the session. *)

(* (whoami) -> VStr. Current session username. [username_fn] is
   re-read on each call so cotenant multi-user can switch identity
   per HTTP request without re-registering prims. *)
let prim_whoami_fn ~(username_fn : unit -> string) (_ : value list) : value =
  VStr (username_fn ())

(* (current-author) -> VStr. Same as whoami for now; reserved for
   the multi-user permission model where (current-author) consults
   the binding being executed rather than the session. *)
let prim_current_author_fn ~(username_fn : unit -> string)
    (_ : value list) : value =
  VStr (username_fn ())

(* (list-bindings) -> VList of binding-name VStrs. Lists all names
   currently visible in the session env. *)
let prim_list_bindings_fn ~(env : Pricklypear_lisp.Ast.env) (_ : value list) : value =
  let names =
    Pricklypear_lisp.Env.fold env
      (fun name _ acc -> name :: acc) []
  in
  VList (List.map (fun s -> VStr s) names)

(* Register session-aware primitives into [t]. Called at session-make
   time with a dynamic username getter + env. *)
let register_session_prims
    (t : t)
    ~(username_fn : unit -> string)
    ~(env : Pricklypear_lisp.Ast.env)
    : unit =
  register_sync t "whoami" (prim_whoami_fn ~username_fn);
  register_sync t "current-author" (prim_current_author_fn ~username_fn);
  register_sync t "list-bindings" (prim_list_bindings_fn ~env)


(* (date-today) -> VStr "YYYY-MM-DD" in local time. *)

(* Re-export pure helpers used by Session / Http. *)
let is_raw_html = Prims_pure.is_raw_html
let is_multipart = Prims_pure.is_multipart
let is_part = Prims_pure.is_part
let is_redirect = Prims_pure.is_redirect
let redirect_value = Prims_pure.redirect_value
let is_status = Prims_pure.is_status
let is_bytes = Prims_html.is_bytes
let status_response = Prims_pure.status_response
let prim_dict_set = Prims_pure.prim_dict_set

(* i-1787971303: image-side crypto-random variant of the pure
   prims_pure.prim_random_uuid — minted identifiers (session-scoped
   Lisp (random-uuid)) must come from Mirage_crypto_rng, never the
   MT19937 Random state. Shadowing the pure prim for this registry;
   the browser client keeps the pure version. *)
let prim_random_uuid (_args : value list) : value =
  let b = Bytes.of_string (Mirage_crypto_rng.generate 16) in
  (* RFC 4122 version 4 + variant 10xx *)
  Bytes.set b 6 (Char.chr ((Char.code (Bytes.get b 6) land 0x0f) lor 0x40));
  Bytes.set b 8 (Char.chr ((Char.code (Bytes.get b 8) land 0x3f) lor 0x80));
  let hex_byte i = Printf.sprintf "%02x" (Char.code (Bytes.get b i)) in
  VStr
    (String.concat ""
       [ hex_byte 0; hex_byte 1; hex_byte 2; hex_byte 3; "-"
       ; hex_byte 4; hex_byte 5; "-"
       ; hex_byte 6; hex_byte 7; "-"
       ; hex_byte 8; hex_byte 9; "-"
       ; hex_byte 10; hex_byte 11; hex_byte 12; hex_byte 13; hex_byte 14;
         hex_byte 15
       ])

(* Register the prim set. Returns the populated table. *)
let make_with_defaults () : t =
  let t = make () in
  (* ONE shared pure catalog (pure/prims_pure.ml [Prims_pure.install])
     — the frozen pure surface every host registers (i-1788670799-f:
     the per-host hand copies here / client / agent / remote had
     already diverged). Image-only extras layer ON TOP
     (Hashtbl.replace semantics): wall-clock prims (prims_time.ml
     reads Unix), sha1 (prims_crypto.ml), and the crypto-grade
     random-uuid shadow below (the browser client keeps the pure
     MT19937 version). *)
  Prims_pure.install t;
  register_sync t "date-today" Prims_time.prim_date_today;
  register_sync t "now" Prims_time.prim_now;
  register_sync t "now-iso" Prims_time.prim_now_iso;
  register_sync t "now-ms" Prims_time.prim_now_ms;
  register_sync t "date-weekday" Prims_time.prim_date_weekday;
  register_sync t "date-format" Prims_time.prim_date_format;
  register_sync t "date-add-days" Prims_time.prim_date_add_days;
  register_sync t "date-diff-days" Prims_time.prim_date_diff_days;
  register_sync t "sha1" Prims_crypto.prim_sha1;
  register_sync t "random-uuid" prim_random_uuid;
  t

(* Short docs for the Functions browser (kernel package). *)
let prim_summary (name : string) : string =
  match name with
  | "+" -> "add numbers"
  | "-" -> "subtract / negate"
  | "*" -> "multiply"
  | "div" | "/" -> "divide (int when both int; else float)"
  | "mod" -> "integer remainder"
  | "abs" -> "absolute value"
  | "min" -> "minimum of VInts"
  | "max" -> "maximum of VInts"
  | "sort" -> "stable ascending: all-number lists numerically, all-string lists lexicographically; mixed kinds error"
  | "=" -> "numeric equality"
  | "<" | ">" | "<=" | ">=" -> "numeric compare"
  | "not" -> "boolean not"
  | "list" -> "build a list"
  | "car" -> "first of pair/list"
  | "cdr" -> "rest of pair/list"
  | "cons" -> "cons cell"
  | "null?" -> "empty list?"
  | "pair?" -> "is pair?"
  | "count" -> "length of list"
  | "length" -> "length of list (accepts proper lists and cons chains)"
  | "equal?" -> "structural deep equality"
  | "err/shape" -> "build the (error msg kind) envelope as data"
  | "cadr" -> "second element (car of cdr)"
  | "caar" -> "car of car"
  | "caddr" -> "third element (car of cdr of cdr)"
  | "cddr" -> "cdr of cdr"
  | "assoc" -> "lookup key in alist; entry or nil"
  | "member" -> "sublist starting at x, or #f"
  | "date-today" -> "local date YYYY-MM-DD"
  | "now" -> "unix epoch seconds (UTC int)"
  | "now-iso" -> "ISO-8601 UTC timestamp YYYY-MM-DDTHH:MM:SSZ"
  | "now-ms" -> "unix epoch milliseconds (UTC int)"
  | "sleep" -> "sleep N seconds (float or int); yields the fiber (Eio)"
  | "http-get" -> "outbound HTTP GET: (http-get url [headers-json]) -> {ok,status,headers,body,err,kind} dict; admin/agent only"
  | "http-post" -> "outbound HTTP POST: (http-post url [headers-json [body]]) -> {ok,status,headers,body,err,kind} dict; admin/agent only"
  | "parse-int" -> "string or int to int"
  | "parse-float" -> "string or number to float"
  | "floor" | "round" -> "float to int"
  | "expt" -> "power: int^nonneg-int stays int; else float"
  | "sqrt" -> "square root (error on negative)"
  | "exp" -> "e^x"
  | "log" -> "natural log, or log base b with 2 args"
  | "sin" | "cos" | "tan" -> "trig in radians"
  | "asin" | "acos" | "atan" -> "inverse trig; atan 2-arg is atan2"
  | "number?" -> "true for int or float"
  | "integer?" -> "true for VInt only (tag-based)"
  | "zero?" | "positive?" | "negative?" -> "numeric sign predicates"
  | "even?" | "odd?" -> "parity; integers (or integral floats)"
  | "quotient" -> "integer division (Scheme name for div int path)"
  | "remainder" -> "integer remainder (Scheme name for mod int path)"
  | "random" -> "(random) float [0,1); (random n) int [0,n)"
  | "random-seed" -> "reseed process PRNG from int"
  | "date-weekday" -> "weekday 0=Sun for y m d"
  | "date-format" -> "format y m d as YYYY-MM-DD"
  | "date-add-days" -> "add n days to YYYY-MM-DD (or today)"
  | "date-diff-days" -> "days between two YYYY-MM-DD strings"
  | "fsrs-schedule" -> "FSRS-4.5: (S D elapsed rating) -> (S' D' interval)"

  | "string-append" -> "concat strings"
  | "string-join" -> "join a list of strings in O(n), optional sep"
  | "string-length" -> "length of string"
  | "substring" -> "substring start [len]"
  | "string-contains?" -> "haystack contains needle?"
  | "string-split" -> "split string on delimiter string → list of pieces"
  | "prose-tokens" -> "unique token-shaped binding cores in prose (scan, budget-safe)"
  | "render-tokens" -> "prose paragraph HTML with binding-token links/holes"
  | "string-eq" -> "string equality"
  | "string<?" -> "string less-than (lexicographic)"
  | "string<=?" -> "string less-or-equal (lexicographic)"
  | "string>?" -> "string greater-than (lexicographic)"
  | "string>=?" -> "string greater-or-equal (lexicographic)"
  | "string-trim" -> "trim whitespace from string"
  | "html-escape" -> "escape text for HTML"
  | "json-parse" -> "parse JSON string to Lisp value"
  | "json-emit" -> "emit Lisp value as JSON string"
  | "read-string" -> "read one s-expr from string → quoted data (no eval)"
  | "read-all-string" -> "read all top-level s-exprs from string → list"
  | "example-eval" ->
      "restricted read-time eval of a book example: (example-eval src) -> rendered value; whitelisted pure + read-only prims, no define/route/write"
  | "with-public-write" ->
      "(with-public-write (lambda () …)) run body with write privs for public forms"
  | "request-method" -> "HTTP method of active request"
  | "request-path" -> "path of active request"
  | "request-param" -> "form/query param of active request"
  | "request-header" -> "HTTP header of active request"
  | "hx-request?" -> "true if HX-Request header present"
  | "request-params" -> "all params as JSON object"
  | "request-body" -> "raw body of active request"
  | "redirect" -> "HTTP 303 redirect response"
  | "bytes-response" -> "serve non-HTML bytes from a route (content-type, max-age, body)"
  | "append" -> "concatenate lists"
  | "reverse" -> "reverse list"
  | "nth" -> "nth element of list (0-based)"
  | "list-ref" -> "alias of nth: i-th element of list (0-based)"
  | "str" -> "value to string"
  | "as-str" -> "plain string coerce (no quotes)"
  | "type-of" -> "runtime type tag"
  | "symbol?" -> "true if value is a symbol"
  | "string?" -> "true if value is a string"
  | "symbol=?" -> "symbol name equality"
  | "symbol->string" -> "symbol name as string"
  | "string->symbol" -> "intern string as symbol"
  | "dict-get" -> "JSON object field get"
  | "dict-set" -> "JSON object field set"
  | "dict-keys" -> "JSON object keys"
  | "render-html" -> "mark string as raw HTML"
  | "part" -> "build one multipart/mixed part (html)"
  | "json-part" -> "build one multipart/mixed JSON part"
  | "multipart" -> "bundle parts into a multipart/mixed response"
  | "hx-get" | "hx-post" -> "htmx attribute helper"
  | "whoami" | "current-author" -> "session identity"
  | "list-bindings" -> "names in env"
  | "list-branches" -> "list branch refs"
  | "list-usernames" -> "list usernames (authenticated; for chat/DM picker)"
  | "is-admin?" -> "true if calling identity is admin"
  | "create-user" -> "admin: create user + password -> user id"
  | "set-password" -> "admin: set password for user by id or username"
  | "delete-user" -> "admin: delete user by id or username -> count (cascades creds/sessions)"
  | "add-ssh-key" -> "register OpenSSH public key for SSH REPL auth"
  | "list-ssh-keys" -> "list SSH key fingerprints for current user"
  | "revoke-ssh-key" -> "revoke SSH key by fingerprint or id"
  | "set-llm-key" -> "store per-user LLM provider key (secret; status shows last4)"
  | "clear-llm-key" -> "remove per-user LLM provider key"
  | "llm-key-status" -> "configured? last4 base model (never raw secret)"
  | "current-branch" -> "current branch name"
  | "fork" -> "fork a branch"
  | "merge" -> "merge branches"
  | "checkout" -> "switch branch"
  | "route" -> "bind URL to function"
  | "deftype" -> "declare a data type"
  | "get" -> "read one data row"
  | "create" -> "insert data row → id"
  | "update" -> "replace row fields JSON"
  | "update-field" -> "set one JSON field"
  | "delete" -> "delete data row"
  | "rows" -> "list data rows"
  | "find-row" -> "first row by fields->>key"
  | "find-rows" -> "rows filtered by fields"
  | "find-text" -> "substring search on a field"
  | "find-code" -> "substring search over function body AST (branch-visible)"
  | "find-random" -> "one random data row of a type"
  | "select-for-update" -> "lock row in transaction (requires with-transaction)"
  | "node-path" -> "path constructor (space-aware: admitted types report data.s.<space>.*; else data.u.<user>.* / data.shared.*)"
  | "space-create" -> "create a shared data space (owner = caller), optional 'open flag = every authenticated identity is implicitly a member"
  | "space-exists?" -> "boolean: does the named space exist"
  | "space-add-member" -> "add a member to a space (owner only)"
  | "space-remove-member" -> "remove a member from a space (owner only)"
  | "space-admit" -> "admit a type into a space; writes route to data.s.<space>.* (owner only)"
  | "space-list" -> "spaces the caller is a member of"
  | "data-scope" -> "read-only JSON of the caller's effective data scope (user root, shared seed, member spaces, data lenses)"
  | "space-members" -> "members (with roles) of a space (caller must be member)"
  | "data-history" -> "version history for a data row (newest-first: gen, author, created_at, delete, body; any compartment)"
  | "backlinks" -> "paths that reference needle (edges first, then DEPRECATED legacy body-string matches)"
  | "type-fields" -> "declared fields for a type"
  | "tree/get" -> "get hierarchical tree node by path → JSON"
  | "tree/list" -> "list subtree (depth 1 = children) under path"
  | "tree/children-of" -> "DIRECT child nodes of path via materialized path index"
  | "tree/subtree" -> "capped subtree payload under path via materialized path index"
  | "tree/lens-get" -> "resolve relative fn/type/route/pkg path across the branch lens chain (closest-wins)"
  | "tree/lens-children" -> "direct children of relative path under the effective branch lens"
  | "tree/lens-subtree" -> "capped subtree of relative path under the effective branch lens"
  | "lens/fork" -> "create a lens branch as a named child of the checked-out branch (local-first lens-model; <owner>/lens-<device>[-<date>])"
  | "replication/unit" -> "materialize the authorized-read subtree (replication unit): data.u.<user>.* + data.shared.* + code lens + branch ancestry → JSON manifest"
  | "sync/plan" -> "server-side sync reconcile: send a client (path,hash) manifest [+ base manifest] → JSON reconciliation report / three-way merge plan (local-first sync)"
  | "sync/push" -> "apply a client two-way push (manifest + bodies) for the caller's data.u.<user>.* subtree; returns {applied, server_only_paths, diverge_paths, generation} (local-first sync v1; diverge reported, not auto-merged)"
  | "tree/upsert" -> "upsert folder|note|heading at path"
  | "tree/delete" -> "delete path [, recursive]"
  | "tree/move" -> "move subtree old-path → new-path"
  | "tree/reorder" -> "set node rank directly (base-36 fractional string; \"\" clears) → node JSON"
  | "tree/move-before" -> "place node immediately before ranked sibling anchor → new rank"
  | "tree/move-after" -> "place node immediately after ranked sibling anchor → new rank"
  | "tree/siblings" -> "ordered children of path's parent (ranked first) → node JSONs"
  | "tree/next" -> "adjacent sibling after path in tree order → node JSON or nil"
  | "tree/prev" -> "adjacent sibling before path in tree order → node JSON or nil"
  | "tree/insert-child" -> "create child of parent with rank after anchor (or appended); hier kinds; body needs slug/title"
  | "link" -> "create an edge node (kind=link) from → to; to may dangle → edge node JSON"
  | "unlink" -> "tombstone an edge node (kind=link only) → edge path"
  | "out-links" -> "outgoing edges of path with resolution status (resolved/dangling) → edge JSONs"
  | "tree/get-uuid" -> "lookup node by payload body.uuid"
  | "tree/find-by" -> "find nodes by kind + body field; garden-scoped by default, optional limit then prefix (\"\" = whole store)"
  | "tree/path-label" -> "sanitize string to ltree-safe label"
  | "tree/parent" -> "strict parent path of path, or nil at root (pure)"
  | "tree/leaf" -> "final label of path (pure)"
  | "tree/segments" -> "labels of path, root-first (pure)"
  | "tree/child" -> "append label to path (encodes; pure)"
  | "tree/ancestors" -> "every strict prefix of path, root-first (pure)"
  | "tree/garden-root" -> "data.note.u.<user> path prefix"
  | "gen-now" -> "current branch generation (int) — reads-as-of-gen clock coordinate"
  | "gen-read" -> "node body at gen (absolute path or lens suffix), crossed over branch lens rules → version JSON"
  | "gen-diff" -> "node-granular diff between gen-a and gen-b (hashes first, walk on mismatch)"
  | "undo" -> "restore value at head-n as a NEW write at head+1 (restore-by-write, datum paths)"
  | "code-restore" -> "restore a code node (fn/type/route) at head-n as a NEW write at head+1 (restore-by-write, gen-read + define)"
  | "tree/folder-index" -> "distinct folder paths under prefix (SQL dedup)"
  | "tree/children-html" -> "budget-safe HTML for notes-tree children under path"
  | "extract-links" -> "parse [[wiki]] / [[slug|label]] from body → list of pairs"
  | "render-links" -> "body with [[wiki]]/[[media:]] → HTML (budget-safe)"
  | "budget-remaining" -> "steps left in current eval budget"
  | "string-replace" -> "replace all occurrences of pat with repl"
  | "string-downcase" -> "lowercase ASCII"
  | "string-upcase" -> "uppercase ASCII"
  | "string-starts-with?" -> "prefix test"
  | "string-ends-with?" -> "suffix test"
  | "url-encode" -> "percent-encode a string"
  | "url-decode" -> "percent-decode a string"
  | "sha1" -> "SHA-1 hex digest of a string"
  | "html-decode" -> "decode HTML entities in a string"
  | "parse-date" -> "parse RFC-822/ISO-8601/date -> UTC epoch int"
  | "xml-parse" -> "lenient XML text -> Lisp tree"
  | "random-uuid" -> "generate a random UUID string"
  | "bench" -> "avg microseconds per eval of expr-string [n]"
  | "agent-session-new" -> "create durable AI session: (agent-session-new seed [tools?])"
  | "agent-say" -> "append user msg + run tool loop: (agent-say sid prompt)"
  | "agent-history" -> "list messages in AI session"
  | "agent-sessions" -> "list recent AI sessions for whoami"
  | "agent-session-delete" -> "delete AI session and messages"
  | "agent" -> "alias: new session + one say (prefer agent-session-new/say)"
  | "agent-task" -> "alias: new tools session + one say"
  | "propose-function" -> "AI draft+install function on branch"
  | "install-function" -> "install AI body string as named function"
  | "function-source" -> "pretty-print stored function body"
  | "function-doc" -> "docstring for a binding (function_defs.doc)"
  | "analyze" -> "LLM/heuristic complexity analysis of a function (cached)"
  | "explain" -> "inside-out structural breakdown of a function"
  | "function-ast" -> "raw stored function body as quoted data (no eval)"
  | "forge-effects" -> "capability badge (pure/db-read/db-write/…) for a parsed source expr or named binding"
  | "llm-chat" -> "raw one-shot LLM completion (system, user) -> content string; not the agentic loop"
  | "submit-job" -> "submit a background job: (submit-job fn args) -> job-id; runner evals (fn args) on your branch"
  | "submit-job-at" -> "submit a scheduled job: (submit-job-at fn args run-at-ms) -> job-id"
  | "job-status" -> "get a job row by id (confined to submitter unless admin): (job-status id)"
  | "jobs" -> "list your recent jobs (admin sees all): (jobs)"
  | "cancel-job" -> "cancel a pending job: (cancel-job id)"
  | "package-exports" -> "shallow public names for a package (std freeze or pkg/local)"
  | "package-bindings" -> "all binding names with package prefix (any depth)"
  | "list-packages" -> "top-level package prefixes from function_defs"
  | "apropos-docs" -> "(name doc) pairs for names containing pattern"
  | "describe" -> "selective reveal: package summary or one binding doc"
  | "export-package" -> "export package as lib.pp text"
  | "llm-inner" | "llm-outer" | "llm-rewrite" | "llm-seed" ->
      "AI rewrite / growth tool"
  | "__with_transaction__" -> "internal transaction helper"
  | "load-string" -> "load Lisp source text (define/route/deftype)"
  | "load-library" -> "load libs/<name>/lib.pp into current branch"
  | "reseed-package" -> "admin: upsert libs/<pkg> onto system/main (clobber)"
  | "callees" -> "static forward callees of a function (App-Var heads)"
  | "callers" -> "static direct callers (senders) of a name"
  | "dependents" -> "reverse-graph dependents of a function"
  | "used-by-routes" -> "routes whose handlers depend on a function"
  | "holes" -> "unbound App-Var names in a function body"
  | "impact-queue" -> "recent define impact events (dependents + routes)"
  | "impact-clear" -> "clear impact queue (all or one name)"
  | "impact-mark" -> "mark impact status smoked|dismissed"
  | "change-transcript" -> "session define/route transcript ring"
  | "branch-changes" -> "own-branch function/route change set"
  | "stub-function" -> "create TODO lambda stub for a missing name"
  | "log-tail" -> "recent structured ops log events"
  | "ops/summary" -> "today analytics JSON (viewers, logins, errors)"
  | "ops/incidents" -> "open reliability incident fingerprints"
  | "ops/activity" -> "activity ledger rows (admin all; non-admin self); JSON strings"
  | "ops/presence" -> "live sessions + spend rollups for the operator console (admin)"
  | "ops/hold" -> "reversible operator containment: revoke-token/sessions, cancel-job, disable-package (admin)"
  | "ops/knock" -> "deterministic knock scan + trifecta rows; files security incidents (admin)"
  | _ -> "built-in primitive"

(** All registered prim names with one-line summaries, sorted. *)
let catalog (t : t) : (string * string) list =
  Hashtbl.fold
    (fun name _ acc -> (name, prim_summary name) :: acc)
    t []
  |> List.sort (fun (a, _) (b, _) -> String.compare a b)

let list_names (t : t) : string list =
  List.map fst (catalog t)

(* H1.7: image stores this table directly for Control_rim; lisp Eval no
   longer carries Lwt prim_fn. Identity helper kept for call-site clarity. *)
let to_eval_table (t : t) : t = t
