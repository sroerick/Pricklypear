(** Match-pattern walker — split out of eval.ml (i-1788671219 walker
    split). Pure over Ast.expr / Ast.value; eval.ml re-exports
    [match_pat] via alias. *)

(** Match pattern against value; returns Some binding list or None. *)
let rec match_pat (pat : Ast.expr) (v : Ast.value)
  : (string * Ast.value) list option =
  let open Ast in
  match pat, v with
  | Var "_", _ -> Some []
  | Var name, _ -> Some [ (name, v) ]
  | Lit lit, v when lit = v -> Some []
  | Lit (VInt n), VInt m when n = m -> Some []
  | Lit (VFloat f), VFloat g when f = g -> Some []
  | Lit (VStr s), VStr t when s = t -> Some []
  | Lit (VBool b), VBool c when b = c -> Some []
  | Lit VNil, VNil -> Some []
  | Lit VNil, VList [] -> Some []
  | Quote (Var s), VSymbol t when s = t -> Some []
  | Quote (Var s), VStr t when s = t -> Some [] (* compat *)
  | Quote (Lit lit), v when lit = v -> Some []
  | App (Var "quote", [ Var s ]), VSymbol t when s = t -> Some []
  | App (Var "quote", [ Var s ]), VStr t when s = t -> Some []
  | App (Var "list", pats), VList vs -> match_list_pats pats vs
  | App (Var "list", pats), VNil when pats = [] -> Some []
  | App (Var head, pats), VList (vh :: vt) ->
      (* (lambda params body) as pattern => list starting with symbol head *)
      let head_ok =
        match vh with
        | VSymbol s | VStr s -> s = head
        | _ -> false
      in
      if head_ok then match_list_pats pats vt else None
  | App (Var head, []), VSymbol s when s = head -> Some []
  | App (Var head, []), VStr s when s = head -> Some []
  | _ -> None

(** Match a list pattern against a value list (helper for [match_pat]).
    Returns combined bindings or [None] on mismatch. *)
and match_list_pats (pats : Ast.expr list) (vs : Ast.value list)
  : (string * Ast.value) list option =
  match pats, vs with
  | [], [] -> Some []
  | [], _ | _, [] -> None
  | p :: ps, v :: vs ->
      (match match_pat p v with
       | None -> None
       | Some b1 ->
           (match match_list_pats ps vs with
            | None -> None
            | Some b2 -> Some (b1 @ b2)))
