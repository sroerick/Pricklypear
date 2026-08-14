(** Pricklypear Lisp — Lexical environment.

    Lexically scoped bindings. Each env has a parent (set at closure
    capture time) and a hashtable of name -> value. Lookup walks up the
    parent chain.

    Per spec [lisp-interpreter.eval]: the evaluator is pure w.r.t. I/O;
    the env is mutable (binding updates), which is fine — it's local to
    a session, not persisted. Persistence happens via [(define ...)]
    which, in the IMAGE layer, ALSO writes to the store. *)
open Ast

type env = Ast.env

(** Create a fresh env. [~parent] (default [None]) is the lexical parent
    captured at closure-creation time; lookups walk up this chain. *)
let create ?(parent = None) () : env =
  { bindings = Hashtbl.create 16; parent }

(** Create a child env whose parent is [e]. Bindings defined in the child
    shadow (not mutate) names in [e]. *)
let child (e : env) : env = create ~parent:(Some e) ()

(** [lookup e name] returns [Some v] if [name] is bound in [e] or any
    ancestor, else [None]. Walks the parent chain to the root. *)
let rec lookup (e : env) (name : string) : value option =
  match Hashtbl.find_opt e.bindings name with
  | Some v -> Some v
  | None ->
      (match e.parent with
       | Some p -> lookup p name
       | None -> None)

(** Bind [name] to [v] in [e], replacing any existing binding locally
    (does not touch parents). Mirrors the image-layer [(define ...)]
    which in the IMAGE also persists to the store. *)
let define (e : env) (name : string) (v : value) : unit =
  Hashtbl.replace e.bindings name v

(** Like [define] but only binds if [name] is already bound somewhere in
    the parent chain. Returns [true] on success, [false] if [name] is
    not bound (leaving the env unchanged). See [lisp-interpreter.eval] for
    the set!/redefinition semantics this mirrors. *)
let set_existing (e : env) (name : string) (v : value) : bool =
  (* Like [define] but only succeeds if the name is already bound
     somewhere in the parent chain. Returns false if not bound. *)
  let rec find_env e =
    match Hashtbl.find_opt e.bindings name with
    | Some _ -> Some e
    | None -> Option.bind e.parent find_env
  in
  match find_env e with
  | Some e' -> Hashtbl.replace e'.bindings name v; true
  | None -> false

(** Fold [f] over every binding in [e] and its ancestor chain, in
    child-to-parent order. Used by reflection primitives like
    [(list-bindings)]. *)
let rec fold (e : env) (f : string -> value -> 'a -> 'a) (acc : 'a) : 'a =
  let local = Hashtbl.fold f e.bindings acc in
  match e.parent with
  | Some p -> fold p f local
  | None -> local

