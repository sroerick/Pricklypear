(** Pricklypear Image — Primitive registry.

    Primitives are OCaml functions exposed to Lisp. They live in the image
    (not the lisp/ package) because they're I/O-shaped: read from store,
    emit htmx, persist, etc. The lisp/ package is pure I/O-free.

    Spec [lisp-interpreter.primitives] lists the full ~20; for the minimal
    image, ~5 are sufficient to demo the loop. *)
open Lwt
open Pricklypear_lisp.Ast
(* A prim fn takes a per-request context + evaluated args, returns a value
   (or raises) wrapped in a promise. The context carries the stable manifest
   (and later env/routes) for the duration of the page. Pure prims register
   via [register_sync] which wraps the sync prim in [Lwt.return]; DB-touching
   prims register via [register] and return their promise directly. *)
type prim_fn = value list -> value Lwt.t

(* The sync form: pure prims return [value] immediately. They get wrapped
   in [Lwt.return] when registered via [register_sync]. *)
type prim_fn_sync = value list -> value

type t = (string, prim_fn) Hashtbl.t

let make () : t = Hashtbl.create 16

let register (t : t) (name : string) (fn : prim_fn) : unit =
  Hashtbl.replace t name fn

let register_sync (t : t) (name : string) (fn : prim_fn_sync) : unit =
  Hashtbl.replace t name (fun args -> Lwt.return (fn args))

let lookup (t : t) (name : string) : prim_fn option =
  Hashtbl.find_opt t name

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

open Prims_pure

(* Re-export pure helpers used by Session / Http. *)
let is_raw_html = Prims_pure.is_raw_html
let is_redirect = Prims_pure.is_redirect
let redirect_value = Prims_pure.redirect_value
let prim_dict_set = Prims_pure.prim_dict_set

(* Register the minimal set. Returns the populated table. *)
let make_with_defaults () : t =
  let t = make () in
  register_sync t "+" prim_add;
  register_sync t "-" prim_sub;
  register_sync t "*" prim_mul;
  register_sync t "div" prim_div;
  register_sync t "/" prim_div; (* Scheme-style alias of div *)
  register_sync t "mod" prim_mod;
  register_sync t "abs" prim_abs;
  register_sync t "min" prim_min;
  register_sync t "max" prim_max;
  register_sync t "=" prim_num_eq;
  register_sync t "<" prim_lt;
  register_sync t ">" prim_gt;
  register_sync t "<=" prim_le;
  register_sync t ">=" prim_ge;
  register_sync t "not" prim_not;
  register_sync t "list" prim_list;
  register_sync t "car" prim_car;
  register_sync t "cdr" prim_cdr;
  register_sync t "cons" prim_cons;
  register_sync t "null?" prim_null_p;
  register_sync t "pair?" prim_pair_p;
  register_sync t "string-append" prim_string_append;
  register_sync t "string-length" prim_string_length;
  register_sync t "substring" prim_substring;
  register_sync t "string-contains?" prim_string_contains;
  register_sync t "string-eq" prim_string_eq;
  register_sync t "string<?" prim_string_lt;
  register_sync t "string<=?" prim_string_le;
  register_sync t "string>?" prim_string_gt;
  register_sync t "string>=?" prim_string_ge;
  register_sync t "string-trim" prim_string_trim;
  register_sync t "html-escape" prim_html_escape;
  register_sync t "json-parse" prim_json_parse;
  register_sync t "json-emit" prim_json_emit;
  register_sync t "read-string" prim_read_string;
  register_sync t "read-all-string" prim_read_all_string;
  register_sync t "type-of" prim_type_of;
  register_sync t "symbol?" prim_symbol_p;
  register_sync t "string?" prim_string_p;
  register_sync t "symbol=?" prim_symbol_eq;
  register_sync t "symbol->string" prim_symbol_to_string;
  register_sync t "string->symbol" prim_string_to_symbol;
  register_sync t "str" prim_str;
  register_sync t "as-str" prim_as_str;
  register_sync t "append" prim_append;
  register_sync t "reverse" prim_reverse;
  register_sync t "nth" prim_nth;
  (* list-branches is registered as a session DB prim in session.ml *)
  register_sync t "hx-get" prim_hx_get;
  register_sync t "hx-post" prim_hx_post;
  register_sync t "count" prim_count;
  register_sync t "equal?" prim_equal_p;
  register_sync t "cadr" prim_cadr;
  register_sync t "caar" prim_caar;
  register_sync t "caddr" prim_caddr;
  register_sync t "cddr" prim_cddr;
  register_sync t "assoc" prim_assoc;
  register_sync t "member" prim_member;
  register_sync t "dict-get" prim_dict_get;
  register_sync t "dict-set" prim_dict_set;
  register_sync t "date-today" prim_date_today;
  register_sync t "parse-int" prim_parse_int;
  register_sync t "parse-float" prim_parse_float;
  register_sync t "floor" prim_floor;
  register_sync t "round" prim_round;
  register_sync t "date-weekday" prim_date_weekday;
  register_sync t "date-format" prim_date_format;
  register_sync t "date-add-days" prim_date_add_days;
  register_sync t "date-diff-days" prim_date_diff_days;
  register_sync t "fsrs-schedule" prim_fsrs_schedule;
  register_sync t "dict-keys" prim_dict_keys;
  register_sync t "render-html" prim_render_html;
  register_sync t "extract-links" prim_extract_links;
  register_sync t "render-links" prim_render_links;
  register_sync t "budget-remaining" prim_budget_remaining;
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
  | "equal?" -> "structural deep equality"
  | "cadr" -> "second element (car of cdr)"
  | "caar" -> "car of car"
  | "caddr" -> "third element (car of cdr of cdr)"
  | "cddr" -> "cdr of cdr"
  | "assoc" -> "lookup key in alist; entry or nil"
  | "member" -> "sublist starting at x, or #f"
  | "date-today" -> "local date YYYY-MM-DD"
  | "parse-int" -> "string or int to int"
  | "parse-float" -> "string or number to float"
  | "floor" | "round" -> "float to int"
  | "date-weekday" -> "weekday 0=Sun for y m d"
  | "date-format" -> "format y m d as YYYY-MM-DD"
  | "date-add-days" -> "add n days to YYYY-MM-DD (or today)"
  | "date-diff-days" -> "days between two YYYY-MM-DD strings"
  | "fsrs-schedule" -> "FSRS-4.5: (S D elapsed rating) -> (S' D' interval)"

  | "string-append" -> "concat strings"
  | "string-length" -> "length of string"
  | "substring" -> "substring start [len]"
  | "string-contains?" -> "haystack contains needle?"
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
  | "append" -> "concatenate lists"
  | "reverse" -> "reverse list"
  | "nth" -> "nth element of list (0-based)"
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
  | "hx-get" | "hx-post" -> "htmx attribute helper"
  | "whoami" | "current-author" -> "session identity"
  | "list-bindings" -> "names in env"
  | "list-branches" -> "list branch refs"
  | "list-usernames" -> "list usernames (authenticated; for chat/DM picker)"
  | "is-admin?" -> "true if calling identity is admin"
  | "create-user" -> "admin: create user + password -> user id"
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
  | "find-random" -> "one random data row of a type"
  | "select-for-update" -> "lock row in transaction (requires with-transaction)"
  | "extract-links" -> "parse [[wiki]] / [[slug|label]] from body → list of pairs"
  | "render-links" -> "body with [[wiki]]/[[media:]] → HTML (budget-safe)"
  | "budget-remaining" -> "steps left in current eval budget"
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
  | "function-ast" -> "raw stored function body as quoted data (no eval)"
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
