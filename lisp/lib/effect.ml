(** Capability effects for Pricklypear Lisp programs.

    Separate from Lwt async plumbing and from OCaml 5 algebraic effects.
    See spec [lisp-interpreter.effects].

    Effects label what a term *needs* (capabilities), not when it runs. *)

open Ast

(** [effect] is an OCaml 5 keyword; use [eff]. *)
type eff =
  | Pure
  | ReadDb
  | WriteDb
  | Branch
  | Publish
  | Ai
  | Html
  | Txn

let eff_to_string = function
  | Pure -> "pure"
  | ReadDb -> "read-db"
  | WriteDb -> "write-db"
  | Branch -> "branch"
  | Publish -> "publish"
  | Ai -> "ai"
  | Html -> "html"
  | Txn -> "txn"

let eff_of_string = function
  | "pure" -> Some Pure
  | "read-db" -> Some ReadDb
  | "write-db" -> Some WriteDb
  | "branch" -> Some Branch
  | "publish" -> Some Publish
  | "ai" -> Some Ai
  | "html" -> Some Html
  | "txn" -> Some Txn
  | _ -> None

let eff_rank = function
  | Pure -> 0
  | Html -> 1
  | ReadDb -> 2
  | WriteDb -> 3
  | Txn -> 4
  | Publish -> 5
  | Branch -> 6
  | Ai -> 7

module EffSet = struct
  type t = eff list  (* unique, sorted by rank then name *)

  let empty : t = []

  let normalize (xs : eff list) : t =
    let xs = List.filter (function Pure -> false | _ -> true) xs in
    let xs =
      List.sort_uniq
        (fun a b ->
          let c = compare (eff_rank a) (eff_rank b) in
          if c <> 0 then c else String.compare (eff_to_string a) (eff_to_string b))
        xs
    in
    xs

  let singleton e = normalize [ e ]

  let union a b = normalize (a @ b)

  let unions sets = List.fold_left union empty sets

  let mem e set = List.mem e set

  let is_empty = function [] -> true | _ -> false

  let is_pure = is_empty

  let subset (a : t) (b : t) : bool =
    List.for_all (fun e -> mem e b) a

  let to_list s = s

  let pp (s : t) : string =
    if is_pure s then "pure"
    else String.concat "+" (List.map eff_to_string s)

  let badge (s : t) : string =
    (* short UI badge *)
    if is_pure s then "pure"
    else if mem Ai s then "ai"
    else if mem Publish s then "page"
    else if mem Branch s then "branch"
    else if mem WriteDb s then "db-write"
    else if mem ReadDb s then "db-read"
    else if mem Html s then "html"
    else pp s
end

type effect_set = EffSet.t

(** Default pure prim names (image may add more). *)
let pure_prims = [
  "+"; "-"; "*"; "="; "<"; ">"; "<="; ">="; "not";
  "list"; "car"; "cdr"; "cons"; "null?"; "pair?"; "count";
  "string-append"; "str"; "type-of"; "as-str";
  "string-eq"; "string-length"; "substring"; "string-contains?";
  "html-escape"; "hx-request?"; "request-header";
  "dict-get"; "dict-set"; "dict-keys";
  "hx-get"; "hx-post";
]

let read_db_prims = [
  "get"; "rows"; "select-for-update";
  "list-branches"; "current-branch"; "list-bindings";
  "whoami"; "current-author";
]

let write_db_prims = [
  "create"; "update"; "update-field"; "delete"; "deftype";
]

let branch_prims = [ "fork"; "merge"; "checkout" ]
let publish_prims = [ "route" ]
let html_prims = [ "render-html" ]
(* Author tools that freeze into function_defs (Ai + WriteDb). *)
let ai_write_prims = [
  "propose-function"; "install-function"; "agent-task";
  "llm-inner"; "llm-outer"; "llm-rewrite"; "llm-seed";
]
(* Portal that only talks to the model (Ai only). *)
let ai_prims = "agent" :: ai_write_prims

let effects_for_prim (name : string) : effect_set =
  if List.mem name pure_prims then EffSet.empty
  else if List.mem name read_db_prims then EffSet.singleton ReadDb
  else if List.mem name write_db_prims then EffSet.singleton WriteDb
  else if List.mem name branch_prims then EffSet.singleton Branch
  else if List.mem name publish_prims then EffSet.singleton Publish
  else if List.mem name html_prims then EffSet.singleton Html
  else if name = "agent" then EffSet.singleton Ai
  else if List.mem name ai_write_prims then
    EffSet.union (EffSet.singleton Ai) (EffSet.singleton WriteDb)
  else if name = "with-transaction" || name = "__with_transaction__" then EffSet.singleton Txn
  else
    (* Unknown name: not a registered capability prim. User fns resolved
       via known_fn; bare unknowns contribute nothing (avoid painting every
       call as db-write). *)
    EffSet.empty

type prim_effect_lookup = string -> effect_set
type known_fn_lookup = string -> effect_set option

let default_prim_lookup = effects_for_prim

(** Conservative AST effect inference.
    [known_fn] resolves user function names when known; unknown globals
    contribute no extra effects (overapprox only on prims). Higher-order
    unknown application does not explode to top — callers may wrap. *)
let rec infer
    ?(prims = default_prim_lookup)
    ?(known_fn : known_fn_lookup = fun _ -> None)
    (e : expr) : effect_set =
  let go = infer ~prims ~known_fn in
  match e with
  | Var name ->
      (match known_fn name with
       | Some s -> s
       | None ->
           (* bare var might be a prim bound in env *)
           if List.mem name pure_prims
              || List.mem name read_db_prims
              || List.mem name write_db_prims
              || List.mem name branch_prims
              || List.mem name publish_prims
              || List.mem name html_prims
              || List.mem name ai_prims
           then prims name
           else EffSet.empty)
  | Lit _ -> EffSet.empty
  | Quote _ -> EffSet.empty
  | Lambda (_params, body) -> go body
  | If (t, a, b) -> EffSet.unions [ go t; go a; go b ]
  | Let (_n, v, body) -> EffSet.union (go v) (go body)
  | Letrec (bindings, body) ->
      let bs = List.map (fun (_, e') -> go e') bindings in
      EffSet.unions (go body :: bs)
  | Seq (a, b) -> EffSet.union (go a) (go b)
  | Define (_n, v) -> go v
  | Prim (name, args) ->
      EffSet.union (prims name) (EffSet.unions (List.map go args))
  | App (head, args) ->
      let head_fx =
        match head with
        | Var name ->
            (match known_fn name with
             | Some s -> s
             | None -> prims name)
        | _ -> go head
      in
      let special =
        match head with
        | Var "with-transaction" -> EffSet.singleton Txn
        | _ -> EffSet.empty
      in
      EffSet.unions (head_fx :: special :: List.map go args)

(** Default AI allowlist for generated code. *)
let ai_default_allow : effect_set =
  EffSet.normalize [ ReadDb; Html ]

let policy_allows ~(allow : effect_set) (got : effect_set) : bool =
  (* Pure always allowed; every effect in got must be in allow,
     except Pure which is absent from sets. WriteDb not allowed unless in allow. *)
  EffSet.subset got allow
  || (EffSet.is_pure got)
