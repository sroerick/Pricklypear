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
  | VStr of string
  | VBool of bool
  | VNil
  | VPair of value * value         (* cons cells *)
  | VLambda of string list * expr * env
  | VPrim of string                 (* primitive name; resolved at apply *)
  | VRef of type_name * id
  | VList of value list             (* sugar for nested pairs; reader interns *)

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
  | Prim_error of string * string    (* (prim_name, message) *)
  | Parse_error of string * int * int (* (message, line, col) *)
  | User_error of string

exception Interp_error of interp_error

let _ = Obj.repr (fun (_ : interp_error) -> ())  (* keep type referenced *)

(** Helper constructors for building AST in tests *)
let var s = Var s
let int n = Lit (VInt (Int64.of_int n))
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
  | VStr s -> `Assoc [ ("str", `String s) ]
  | VBool b -> `Assoc [ ("bool", `Bool b) ]
  | VNil -> `Assoc [ ("nil", `Null) ]
  | VPair (a, b) -> `Assoc [ ("pair", `List [ value_to_json a; value_to_json b ]) ]
  | VLambda _ -> `Assoc [ ("lambda", `String "#<lambda>") ]  (* closures aren't serializable *)
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
  | `Assoc [ ("str", `String s) ] -> Ok (VStr s)
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
