(** Pricklypear Lisp — AST.

    The AST IS the source (homoiconicity). Function bodies are stored as
    JSONB rows in [function_defs.body], parsed back into [expr] values by
    the reader and evaluated by [Eval].

    Per spec [lisp-interpreter.ast]: this is a Scheme-flavored Lisp-1
    (lexically scoped, first-class closures, primitives dispatched by
    name through a table the IMAGE layer registers at boot).

    The interpreter proper is PURE: it takes an env and an expr and
    returns a value or raises an [Interp_error]. No I/O here. Side
    effects go through [Prim] calls, wired to real Postgres/HTTP by the
    image layer. *)

(** A reference into the persistent store. Carries its target type so
    the interpreter can verify derefs at call time before querying. *)
type type_name = string
type id = string  (* uuid-ish; opaque to the interpreter *)

type expr =
  | Var of string
  | Lit of value
  | App of expr * expr list
  | Let of string * expr * expr
  | Letrec of (string * expr) list * expr
  | If of expr * expr * expr
  | Lambda of string list * expr
  | Prim of string * expr list
  | Seq of expr * expr
  | Quote of expr
  | Define of string * expr   (* top-level binding *)

(** Values are what evaluation produces. The interesting cases are
    [VRef] (a tagged reference into the store, so values can point into
    the persistent object graph without copying it) and [VLambda]
    (closures capture their defining env). *)
and value =
  | VInt of int64
  | VFloat of float                 (* IEEE float; arithmetic promotes int+float *)
  | VStr of string
  | VSymbol of string               (* code names as data; distinct from VStr *)
  | VBool of bool
  | VNil
  | VPair of value * value         (* cons cells *)
  | VLambda of string list * expr * env
  | VMacro of string list * expr * env
      (* defmacro transformer; params bind to unevaluated arg values *)
  | VPrim of string                 (* primitive name; resolved at apply *)
  | VRef of type_name * id
  | VList of value list             (* sugar for nested pairs; reader interns *)
  | VThunk of thunk_cell ref        (* memoized recipe; forced at Env.lookup *)

(** A not-yet-evaluated binding body (lisp-interpreter.lazy-env-thunks).
    The payload carries its OWN evaluator so the pure kernel never
    decides HOW a body runs: pure hosts force under [Eval]'s table,
    the image forces under its prim rim. Memoization covers failure
    too — a body that raises once replays the exception, it does not
    re-run (and re-pay) on every touch. [Forcing] marks an in-flight
    force so self-referential cycles raise instead of diverging. *)
and thunk_cell =
  | Unforced of thunk_payload
  | Forcing
  | Forced of value
  | Failed of exn

and thunk_payload = {
  thunk_body : expr;
  thunk_env : env;
  thunk_force : env -> expr -> value;
}

(** Lexical environment: a local binding table plus an optional parent
    closure chain; lookup walks toward the root. *)
and env = {
  bindings : (string, value) Hashtbl.t;
  parent : env option;
}

(** Raised on interpreter errors. The REPL catches these, prints them
    with location if available, and continues the session. *)
type interp_error =
  | Unbound_var of string
  | Not_a_function of value
  | Arity_mismatch of string * int * int  (* (name, expected, got) *)
  | Wrong_arg_count of string * int * int
  | Type_mismatch of string * value  (* (expected, got) *)
  | Budget_exceeded
  | Prim_error of string * string * string  (* (prim_name, message, kind) *)
  | Parse_error of string * int * int (* (message, line, col) *)
  | User_error of string * string         (* (message, kind) *)

exception Interp_error of interp_error

let _ = Obj.repr (fun (_ : interp_error) -> ())  (* keep type referenced *)

(** Human-readable form of an [interp_error]. Used by logging/REPL so a
    refresh_ui_shell warn names the actual failure (Unbound_var x /
    User_error ... / Prim_error ...) instead of the opaque
    `Interp_error(_)` Printexc produces. *)
let rec pp_value_short (v : value) : string =
  match v with
  | VStr s -> "\"" ^ s ^ "\""
  | VSymbol s -> s
  | VInt i -> Int64.to_string i
  | VNil -> "nil"
  | VBool b -> string_of_bool b
  | VFloat f -> string_of_float f
  | VList xs -> "[" ^ String.concat ", " (List.map pp_value_short xs) ^ "]"
  | VPair (a, b) -> "(" ^ pp_value_short a ^ " . " ^ pp_value_short b ^ ")"
  | VLambda _ -> "<lambda>"
  | VMacro _ -> "<macro>"
  | VPrim n -> "<prim " ^ n ^ ">"
  | VRef (t, _) -> "<ref " ^ t ^ ">"
  | VThunk _ -> "<thunk>"

(** Machine-readable kind tag for an [interp_error] (used by CLI/UI code). *)
let interp_error_kind (e : interp_error) : string =
  match e with
  | Unbound_var _ -> "unbound_var"
  | Not_a_function _ -> "not_a_function"
  | Arity_mismatch _ -> "bad-arity"
  | Wrong_arg_count _ -> "bad-arity"
  | Type_mismatch _ -> "type_mismatch"
  | Budget_exceeded -> "budget_exceeded"
  | Prim_error (_, _, k) -> k
  | Parse_error _ -> "parse_error"
  | User_error (_, k) -> k

(** Source location (line, col) carried by an error, if any ([Parse_error]). *)
let interp_error_loc (e : interp_error) : (int * int) option =
  match e with
  | Parse_error (_, line, col) -> Some (line, col)
  | _ -> None

(** Human-readable rendering of an [interp_error] for logs/REPL output. *)
let pp_interp_error (e : interp_error) : string =
  match e with
  | Unbound_var s -> "unbound: " ^ s
  | Not_a_function v -> "not a function: " ^ pp_value_short v
  | Arity_mismatch (n, exp, got) ->
      Printf.sprintf "arity mismatch in %s: expected %d, got %d" n exp got
  | Wrong_arg_count (n, exp, got) ->
      Printf.sprintf "wrong arg count in %s: expected %d, got %d" n exp got
  | Type_mismatch (exp, v) ->
      "type mismatch: expected " ^ exp ^ ", got " ^ pp_value_short v
  | Budget_exceeded -> "budget exceeded"
  | Prim_error (p, m, _) -> "prim " ^ p ^ ": " ^ m
  | Parse_error (m, l, c) -> Printf.sprintf "parse error: %s (%d:%d)" m l c
  | User_error (m, _) -> m

(** Helper constructors for building AST in tests *)
let var s = Var s
let int n = Lit (VInt (Int64.of_int n))
let float f = Lit (VFloat f)
let str s = Lit (VStr s)
let bool b = Lit (VBool b)
let nil = Lit VNil

(** JSON serialization of [expr].

    Function bodies are stored as JSONB. Round-trips via [of_json]. *)
let rec expr_to_json (e : expr) : Yojson.Safe.t =
  match e with
  | Var s -> `Assoc [ ("var", `String s) ]
  | Lit v -> `Assoc [ ("lit", value_to_json v) ]
  | App (f, args) ->
      `Assoc [ ("app", `List (expr_to_json f :: List.map expr_to_json args)) ]
  | Let (name, val_expr, body) ->
      `Assoc [
        ("let", `List [ `String name; expr_to_json val_expr; expr_to_json body ]);
      ]
  | Letrec (bindings, body) ->
      `Assoc [
        ( "letrec",
          `List [
            `List (List.map (fun (n, e) -> `List [ `String n; expr_to_json e ]) bindings);
            expr_to_json body;
          ] );
      ]
  | If (test, then_e, else_e) ->
      `Assoc [ ("if", `List [ expr_to_json test; expr_to_json then_e; expr_to_json else_e ]) ]
  | Lambda (params, body) ->
      `Assoc [ ("lambda", `List [ `List (List.map (fun s -> `String s) params); expr_to_json body ]) ]
  | Prim (name, args) ->
      `Assoc [ ("prim", `List [ `String name; `List (List.map expr_to_json args) ]) ]
  | Seq (a, b) -> `Assoc [ ("seq", `List [ expr_to_json a; expr_to_json b ]) ]
  | Quote e -> `Assoc [ ("quote", expr_to_json e) ]
  | Define (name, val_expr) ->
      `Assoc [ ("define", `List [ `String name; expr_to_json val_expr ]) ]

and value_to_json (v : value) : Yojson.Safe.t =
  match v with
  | VInt n -> `Assoc [ ("int", `String (Int64.to_string n)) ]
  | VFloat f -> `Assoc [ ("float", `Float f) ]
  | VStr s -> `Assoc [ ("str", `String s) ]
  | VSymbol s -> `Assoc [ ("sym", `String s) ]
  | VBool b -> `Assoc [ ("bool", `Bool b) ]
  | VNil -> `Assoc [ ("nil", `Null) ]
  | VPair (a, b) -> `Assoc [ ("pair", `List [ value_to_json a; value_to_json b ]) ]
  | VLambda _ -> `Assoc [ ("lambda", `String "#<lambda>") ]  (* closures aren't serializable *)
  | VMacro _ -> `Assoc [ ("macro", `String "#<macro>") ]
  | VThunk _ -> `Assoc [ ("thunk", `String "#<thunk>") ]
  | VPrim name -> `Assoc [ ("prim", `String name) ]
  | VRef (t, i) -> `Assoc [ ("ref", `List [ `String t; `String i ]) ]
  | VList vs -> `Assoc [ ("list", `List (List.map value_to_json vs)) ]

(** JSON deserialization of [expr]. Inverse of [expr_to_json]. *)
let rec expr_of_json (j : Yojson.Safe.t) : (expr, string) result =
  match j with
  | `Assoc [ ("var", `String s) ] -> Ok (Var s)
  | `Assoc [ ("lit", v) ] ->
      (match value_of_json v with
       | Ok v -> Ok (Lit v)
       | Error e -> Error e)
  | `Assoc [ ("app", `List (f :: args)) ] ->
      (match expr_of_json f with
       | Ok f ->
           let args' =
             List.fold_left
               (fun acc a ->
                 match acc, expr_of_json a with
                 | Ok lst, Ok e -> Ok (lst @ [ e ])
                 | Error e, _ -> Error e
                 | _, Error e -> Error e)
               (Ok []) args
           in
           (match args' with Ok lst -> Ok (App (f, lst)) | Error e -> Error e)
       | Error e -> Error e)
  | `Assoc [ ("let", `List [ `String name; v; b ]) ] ->
      (match expr_of_json v, expr_of_json b with
       | Ok v, Ok b -> Ok (Let (name, v, b))
       | Error e, _ | _, Error e -> Error e)
  | `Assoc [ ("letrec", `List [ `List bindings; b ]) ] ->
      let bindings' =
        List.fold_left
          (fun acc bj ->
            match acc, bj with
            | Ok lst, `List [ `String n; e ] ->
                (match expr_of_json e with
                 | Ok e -> Ok (lst @ [ (n, e) ])
                 | Error e -> Error e)
            | Ok _, _ -> Error "letrec binding not a [name, expr] pair"
            | Error e, _ -> Error e)
          (Ok []) bindings
      in
      (match bindings', expr_of_json b with
       | Ok bs, Ok b -> Ok (Letrec (bs, b))
       | Error e, _ | _, Error e -> Error e)
  | `Assoc [ ("if", `List [ t; th; el ]) ] ->
      (match expr_of_json t, expr_of_json th, expr_of_json el with
       | Ok t, Ok th, Ok el -> Ok (If (t, th, el))
       | Error e, _, _
       | _, Error e, _
       | _, _, Error e -> Error e)
  | `Assoc [ ("lambda", `List [ `List params; b ]) ] ->
      let params' =
        List.fold_left
          (fun acc p ->
            match acc, p with
            | Ok lst, `String s -> Ok (lst @ [ s ])
            | Ok _, _ -> Error "lambda param not a string"
            | Error e, _ -> Error e)
          (Ok []) params
      in
      (match params', expr_of_json b with
       | Ok ps, Ok b -> Ok (Lambda (ps, b))
       | Error e, _ | _, Error e -> Error e)
  | `Assoc [ ("prim", `List [ `String name; `List args ]) ] ->
      let args' =
        List.fold_left
          (fun acc a ->
            match acc, expr_of_json a with
            | Ok lst, Ok e -> Ok (lst @ [ e ])
            | Error e, _ -> Error e
            | _, Error e -> Error e)
          (Ok []) args
      in
      (match args' with Ok lst -> Ok (Prim (name, lst)) | Error e -> Error e)
  | `Assoc [ ("seq", `List [ a; b ]) ] ->
      (match expr_of_json a, expr_of_json b with
       | Ok a, Ok b -> Ok (Seq (a, b))
       | Error e, _ | _, Error e -> Error e)
  | `Assoc [ ("quote", e) ] ->
      (match expr_of_json e with Ok e -> Ok (Quote e) | Error e -> Error e)
  | `Assoc [ ("define", `List [ `String name; v ]) ] ->
      (match expr_of_json v with
       | Ok v -> Ok (Define (name, v))
       | Error e -> Error e)
  | _ -> Error ("unrecognized expr JSON: " ^ Yojson.Safe.to_string j)

and value_of_json (j : Yojson.Safe.t) : (value, string) result =
  match j with
  | `Assoc [ ("int", `String s) ] ->
      (try Ok (VInt (Int64.of_string s))
       with _ -> Error ("bad int: " ^ s))
  | `Assoc [ ("float", `Float f) ] -> Ok (VFloat f)
  | `Assoc [ ("float", `Int n) ] -> Ok (VFloat (float_of_int n))
  | `Assoc [ ("float", `String s) ] ->
      (try Ok (VFloat (float_of_string s))
       with _ -> Error ("bad float: " ^ s))
  | `Assoc [ ("str", `String s) ] -> Ok (VStr s)
  | `Assoc [ ("sym", `String s) ] -> Ok (VSymbol s)
  | `Assoc [ ("bool", `Bool b) ] -> Ok (VBool b)
  | `Assoc [ ("nil", `Null) ] -> Ok VNil
  | `Assoc [ ("pair", `List [ a; b ]) ] ->
      (match value_of_json a, value_of_json b with
       | Ok a, Ok b -> Ok (VPair (a, b))
       | Error e, _ | _, Error e -> Error e)
  | `Assoc [ ("prim", `String name) ] -> Ok (VPrim name)
  | `Assoc [ ("ref", `List [ `String t; `String i ]) ] -> Ok (VRef (t, i))
  | `Assoc [ ("list", `List lst) ] ->
      let lst' =
        List.fold_left
          (fun acc v ->
            match acc, value_of_json v with
            | Ok l, Ok v -> Ok (l @ [ v ])
            | Error e, _ -> Error e
            | _, Error e -> Error e)
          (Ok []) lst
      in
      (match lst' with Ok lst -> Ok (VList lst) | Error e -> Error e)
  | _ -> Error ("unrecognized value JSON: " ^ Yojson.Safe.to_string j)

(** Opaque store-body codec (i-1788671083-c): the storage layer hands
    function bodies across its boundary as the raw jsonb text it stored;
    parsing to an [expr] happens HERE at the interpreter boundary, not in
    the store (the store no longer links AST types).
    [expr_of_body_json] is [None] on any parse failure — the per-row
    decode-guard policy the store used to own, now owned by the caller.
    [expr_to_body_json] is the exact serialization the store persists. *)
let expr_of_body_json (body : string) : expr option =
  match Yojson.Safe.from_string body with
  | j -> (
      match expr_of_json j with
      | Ok e -> Some e
      | Error _ -> None)
  | exception Yojson.Json_error _ -> None

let expr_to_body_json (e : expr) : string =
  Yojson.Safe.to_string (expr_to_json e)

