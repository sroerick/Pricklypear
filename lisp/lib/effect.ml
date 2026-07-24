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

(** Default pure prim names (image may add more).
    H0.1 audit 2026-07-18: every registered image prim must appear in
    exactly one capability bucket (or the special-case arms in
    [effects_for_prim]). Guest-visible only — not host control effects. *)
let pure_prims = [
  "+"; "-"; "*"; "div"; "mod"; "abs"; "min"; "max";
  "="; "<"; ">"; "<="; ">="; "not";
  "list"; "car"; "cdr"; "cons"; "null?"; "pair?"; "count";
  "append"; "reverse"; "nth";
  "string-append"; "str"; "type-of"; "as-str";
  "string-eq"; "string-trim"; "string-length"; "substring"; "string-contains?";
  "html-escape"; "json-parse"; "json-emit";
  "dict-get"; "dict-set"; "dict-keys";
  "hx-get"; "hx-post"; "hx-request?";
  "request-header"; "request-method"; "request-path";
  "request-param"; "request-params"; "request-body";
  "date-today"; "date-weekday"; "date-format";
  "date-add-days"; "date-diff-days";
  "parse-int"; "parse-float"; "floor"; "round";
  "fsrs-schedule";
  "verify-ssh-key";  (* pure crypto check on args; no store *)
]

let read_db_prims = [
  "get"; "rows"; "select-for-update";
  "find-row"; "find-rows"; "find-text"; "find-random";
  "list-branches"; "current-branch"; "list-bindings";
  "whoami"; "current-author";
  "function-source"; "function-ast"; "package-bindings"; "export-package";
  "route-visibility";
  "list-usernames"; "list-ssh-keys";
  "llm-key-status";
  "log-tail"; "ops/summary"; "ops/incidents";
  "agent-history"; "agent-sessions";
]

let write_db_prims = [
  "create"; "update"; "update-field"; "delete"; "deftype";
  "load-string"; "load-library";  (* persist defs/routes onto branch *)
  "add-ssh-key"; "revoke-ssh-key";
  "set-llm-key"; "clear-llm-key";
  "with-public-write";  (* elevates anon → may write data_rows *)
]

let branch_prims = [ "fork"; "merge"; "checkout" ]
let publish_prims = [ "route"; "public-route" ]
let html_prims = [ "render-html"; "redirect" ]
(* Author tools that freeze into function_defs or drive the model (Ai + WriteDb). *)
let ai_write_prims = [
  "propose-function"; "install-function"; "agent-task";
  "llm-inner"; "llm-outer"; "llm-rewrite"; "llm-seed";
  "agent-session-new"; "agent-say"; "agent-session-delete";
]
(* Portal that only talks to the model (Ai only) — legacy one-shot. *)
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

(** Build a [known_fn] lookup from visible bindings [(name, body)].
    Memoized; cycles break by treating the in-progress name as pure so
    mutual recursion does not diverge. Used by badges, AI install gate,
    and route publish policy (H0.2 / H0.3). *)
let rec known_fn_of_bindings (bindings : (string * expr) list) : known_fn_lookup =
  let bodies : (string, expr) Hashtbl.t =
    Hashtbl.create (max 16 (List.length bindings))
  in
  List.iter (fun (n, e) -> Hashtbl.replace bodies n e) bindings;
  let cache : (string, effect_set) Hashtbl.t =
    Hashtbl.create (max 16 (List.length bindings))
  in
  let rec lookup (name : string) : effect_set option =
    match Hashtbl.find_opt cache name with
    | Some s -> Some s
    | None ->
        (match Hashtbl.find_opt bodies name with
         | None -> None
         | Some body ->
             (* cycle break *)
             Hashtbl.replace cache name EffSet.empty;
             let s = infer ~known_fn:lookup body in
             Hashtbl.replace cache name s;
             Some s)
  in
  lookup

(** Conservative AST effect inference.
    [known_fn] resolves user function names when known; unknown globals
    contribute no extra effects (overapprox only on prims). Higher-order
    unknown application does not explode to top — callers may wrap. *)
and infer
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

(** Default AI allowlist for generated code (install_function bodies).

    WriteDb is allowed so agents can build real apps (create/update/delete/
    deftype on data_rows). Publish (route) stays denied here — use the
    dedicated register_route tool so route registration is explicit and
    not smuggled inside a lambda body. Elevate with PP_AI_ALLOW_WRITE for
    Branch/Publish/Ai extras. *)
let ai_default_allow : effect_set =
  EffSet.normalize [ ReadDb; WriteDb; Html ]

let policy_allows ~(allow : effect_set) (got : effect_set) : bool =
  (* Pure always allowed; every effect in got must be in allow,
     except Pure which is absent from sets. WriteDb not allowed unless in allow. *)
  EffSet.subset got allow
  || (EffSet.is_pure got)

(** Soft transactional hygiene hints (H0.4). Advisory only — does not
    reject eval. Looks for multi-write bodies and [select-for-update]
    without a surrounding [with-transaction] / Txn effect. *)
let count_write_sites (e : expr) : int =
  let rec go = function
    | Prim (name, args) ->
        let here = if List.mem name write_db_prims then 1 else 0 in
        here + List.fold_left (fun n a -> n + go a) 0 args
    | App (Var name, args) when List.mem name write_db_prims ->
        1 + List.fold_left (fun n a -> n + go a) 0 args
    | App (h, args) -> go h + List.fold_left (fun n a -> n + go a) 0 args
    | Lambda (_, body) | Define (_, body) -> go body
    | If (t, a, b) -> go t + go a + go b
    | Let (_, v, body) | Seq (v, body) -> go v + go body
    | Letrec (bs, body) ->
        List.fold_left (fun n (_, e') -> n + go e') (go body) bs
    | Var _ | Lit _ | Quote _ -> 0
  in
  go e

let has_select_for_update (e : expr) : bool =
  let rec go = function
    | Prim ("select-for-update", _) -> true
    | App (Var "select-for-update", _) -> true
    | Prim (_, args) -> List.exists go args
    | App (h, args) -> go h || List.exists go args
    | Lambda (_, body) | Define (_, body) -> go body
    | If (t, a, b) -> go t || go a || go b
    | Let (_, v, body) | Seq (v, body) -> go v || go body
    | Letrec (bs, body) -> go body || List.exists (fun (_, e') -> go e') bs
    | Var _ | Lit _ | Quote _ -> false
  in
  go e

let txn_lint ?(known_fn : known_fn_lookup = fun _ -> None) (e : expr)
  : string list =
  let fx = infer ~known_fn e in
  let has_txn = EffSet.mem Txn fx in
  let writes = count_write_sites e in
  let hints = ref [] in
  if writes >= 2 && not has_txn then
    hints :=
      (Printf.sprintf
         "soft txn: %d write-db sites without with-transaction — consider wrapping multi-step mutations"
         writes)
      :: !hints;
  if has_select_for_update e && not has_txn then
    hints :=
      "soft txn: select-for-update without with-transaction — row lock will not span later prims"
      :: !hints;
  List.rev !hints
