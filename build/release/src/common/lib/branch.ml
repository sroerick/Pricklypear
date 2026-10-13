(** Pricklypear Common — Branch reference.

    A branch is identified by (owner, name); each has a parent (the branch
    it was forked from). The root is system/main (owner="system",
    name="main", no parent).

    Per spec [branching]: branches organize code (keyed by branch_id in
    Postgres). Shared product data lives in data_rows (not branch-scoped). *)
open Yojson.Safe

type t = {
  id : string;            (* uuid, the primary key in branches table *)
  parent_id : string option;
  owner : string;         (* "system" for main, username otherwise *)
  name : string;          (* "main", "alice/main", "alice/experiment", ... *)
  created_at : string;    (* ISO 8601 timestamp *)
  last_synced : string option;  (* retired (branching.branch-status); kept for protocol compat *)
  (* branching.branch-status: the parent's head generation at fork time
     (branch_generations counter). The merge base — advanced when a merge
     lands this branch into its parent; NOT advanced by promote, so
     selective pins stay visible as `synced`. *)
  base_gen : int64;
}

let make ~id ~parent_id ~owner ~name ~created_at ?(last_synced = None)
    ?(base_gen = 0L) () : t =
  { id; parent_id; owner; name; created_at; last_synced; base_gen }

let is_main (b : t) : bool = b.owner = "system" && b.name = "main"

let qualified_name (b : t) : string =
  if is_main b then "main" else b.owner ^ "/" ^ b.name

let to_json (b : t) : Yojson.Safe.t =
  `Assoc [
    ("id", `String b.id);
    ("parent_id", match b.parent_id with
                  | Some p -> `String p
                  | None -> `Null);
    ("owner", `String b.owner);
    ("name", `String b.name);
    ("created_at", `String b.created_at);
    ("last_synced", match b.last_synced with
                    | Some s -> `String s
                    | None -> `Null);
    ("base_gen", `Int (Int64.to_int b.base_gen));
  ]

let of_json (j : Yojson.Safe.t) : (t, string) result =
  try
    let assoc = match j with `Assoc a -> a | _ -> failwith "not an assoc" in
    let find k = List.assoc k assoc in
    let id = match find "id" with `String s -> s | _ -> failwith "id" in
    let parent_id = match find "parent_id" with
                    | `String s -> Some s | `Null -> None
                    | _ -> failwith "parent_id" in
    let owner = match find "owner" with `String s -> s | _ -> failwith "owner" in
    let name = match find "name" with `String s -> s | _ -> failwith "name" in
    let created_at = match find "created_at" with `String s -> s | _ -> failwith "created_at" in
    let last_synced = match find "last_synced" with
                      | `String s -> Some s | `Null -> None
                      | _ -> failwith "last_synced" in
    let base_gen = match find "base_gen" with
                    | `Int i -> Int64.of_int i
                    | _ -> 0L (* tolerant: pre-024 snapshots *) in
    Ok { id; parent_id; owner; name; created_at; last_synced; base_gen }
  with Failure msg -> Error msg
