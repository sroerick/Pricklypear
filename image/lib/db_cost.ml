(** Static DB-prim cost tiers for binding-page annotations. *)

type cost_tier =
  | Constant
  | Index_seek
  | Linear
  | Seq_scan
  | Unknown

let badge = function
  | Constant -> "O(1) write"
  | Index_seek -> "index seek"
  | Linear -> "filtered scan"
  | Seq_scan -> "seq scan"
  | Unknown -> "?"

let static_cost (prim_name : string) : cost_tier =
  match prim_name with
  | "get" | "find-row" | "select-for-update" | "node-path" | "type-fields"
  | "tree/get" | "tree/get-uuid" -> Index_seek
  | "create" | "update" | "update-field" | "delete" | "tree/upsert"
  | "tree/delete" | "tree/move"
  | "space-create" | "space-add-member" | "space-remove-member"
  | "space-admit" -> Constant
  | "rows" | "find-random" | "tree/list" -> Seq_scan
  | "find-rows" | "find-text" | "backlinks" | "tree/find-by"
  | "space-list" | "space-members" -> Linear
  | _ -> Unknown

let sql_hint = function
  | "get" -> Some "SELECT fields FROM data_rows WHERE type_name=$1 AND id=$2"
  | "create" -> Some "INSERT INTO data_rows (type_name, id, fields, author) …"
  | "update" | "update-field" ->
      Some "UPDATE data_rows SET fields=… WHERE type_name=$1 AND id=$2"
  | "delete" -> Some "DELETE FROM data_rows WHERE type_name=$1 AND id=$2"
  | "rows" -> Some "SELECT id, fields FROM data_rows WHERE type_name=$1 LIMIT …"
  | "find-row" -> Some "SELECT … FROM data_rows WHERE type_name AND field filter"
  | "find-rows" | "find-text" ->
      Some "SELECT … FROM data_rows WHERE type_name AND text/json filter"
  | _ -> None

module Ast = Pricklypear_lisp.Ast

let rec collect_db_prims (acc : string list) (e : Ast.expr) : string list =
  match e with
  | Ast.Prim (n, args) ->
      let acc =
        match static_cost n with Unknown -> acc | _ -> n :: acc
      in
      List.fold_left collect_db_prims acc args
  | Ast.App (Ast.Var n, args) ->
      let acc =
        match static_cost n with Unknown -> acc | _ -> n :: acc
      in
      List.fold_left collect_db_prims acc args
  | Ast.App (f, args) ->
      List.fold_left collect_db_prims (collect_db_prims acc f) args
  | Ast.Let (_, a, b) | Ast.Seq (a, b) ->
      collect_db_prims (collect_db_prims acc a) b
  | Ast.Letrec (bs, body) ->
      let acc =
        List.fold_left (fun a (_, e) -> collect_db_prims a e) acc bs
      in
      collect_db_prims acc body
  | Ast.If (a, b, c) ->
      collect_db_prims (collect_db_prims (collect_db_prims acc a) b) c
  | Ast.Lambda (_, body) | Ast.Define (_, body) -> collect_db_prims acc body
  | Ast.Var _ | Ast.Lit _ | Ast.Quote _ -> acc

let uniq xs =
  let seen = Hashtbl.create 8 in
  List.fold_left
    (fun acc x ->
      if Hashtbl.mem seen x then acc
      else begin
        Hashtbl.add seen x ();
        x :: acc
      end)
    [] xs
  |> List.rev

let db_prims_of_expr (e : Ast.expr) : string list =
  uniq (collect_db_prims [] e)
