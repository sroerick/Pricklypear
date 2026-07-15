(** Pricklypear Image — Primitive registry.

    Primitives are OCaml functions exposed to Lisp. They live in the image
    (not the lisp/ package) because they're I/O-shaped: read from store,
    emit htmx, persist, etc. The lisp/ package is pure I/O-free.

    Spec [lisp-interpreter.primitives] lists the full ~20; for the minimal
    image, ~5 are sufficient to demo the loop. *)
open Lwt
open Pricklypear_lisp.Ast
(* A prim fn takes evaluated args, returns a value (or raises) wrapped in
   a promise. Pure prims register via [register_sync] which wraps the sync
   prim in [Lwt.return]; DB-touching prims register via [register] and
   return their promise directly. *)
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
  register_sync t "string-trim" prim_string_trim;
  register_sync t "html-escape" prim_html_escape;
  register_sync t "json-parse" prim_json_parse;
  register_sync t "json-emit" prim_json_emit;
  register_sync t "type-of" prim_type_of;
  register_sync t "str" prim_str;
  register_sync t "as-str" prim_as_str;
  register_sync t "append" prim_append;
  register_sync t "reverse" prim_reverse;
  register_sync t "nth" prim_nth;
  (* list-branches is registered as a session DB prim in session.ml *)
  register_sync t "hx-get" prim_hx_get;
  register_sync t "hx-post" prim_hx_post;
  register_sync t "count" prim_count;
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
  t

(* Short docs for the Functions browser (kernel package). *)
let prim_summary (name : string) : string =
  match name with
  | "+" -> "add numbers"
  | "-" -> "subtract / negate"
  | "*" -> "multiply"
  | "div" -> "integer divide"
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
  | "string-trim" -> "trim whitespace from string"
  | "html-escape" -> "escape text for HTML"
  | "json-parse" -> "parse JSON string to Lisp value"
  | "json-emit" -> "emit Lisp value as JSON string"
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
  | "dict-get" -> "JSON object field get"
  | "dict-set" -> "JSON object field set"
  | "dict-keys" -> "JSON object keys"
  | "render-html" -> "mark string as raw HTML"
  | "hx-get" | "hx-post" -> "htmx attribute helper"
  | "whoami" | "current-author" -> "session identity"
  | "list-bindings" -> "names in env"
  | "list-branches" -> "list branch refs"
  | "list-usernames" -> "list usernames (authenticated; for chat/DM picker)"
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
  | "select-for-update" -> "lock row in transaction"
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
  | "package-bindings" -> "list binding names in a package"
  | "export-package" -> "export package as lib.pp text"
  | "llm-inner" | "llm-outer" | "llm-rewrite" | "llm-seed" ->
      "AI rewrite / growth tool"
  | "__with_transaction__" -> "internal transaction helper"
  | "load-string" -> "load Lisp source text (define/route/deftype)"
  | "load-library" -> "load libs/<name>/lib.pp into current branch"
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

(* Convert the table into the shape Pricklypear_lisp.Eval.prim_table
   expects. Delegating to a separate function so the integration with
   the interpreter is explicit. *)
let to_eval_table (t : t) : Pricklypear_lisp.Eval.prim_table =
  let module E = Pricklypear_lisp.Eval in
  let tbl = E.make_prim_table () in
  Hashtbl.iter (fun name fn -> E.register_prim tbl name fn) t;
  tbl
