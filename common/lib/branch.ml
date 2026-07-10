(** Pricklypear Common — Branch reference.

    Shared between supervisor, auth, and image. A branch is identified by
    (owner, name); each has a parent (the branch it was forked from). The
    root branch is [main], which has owner="system", name="main", no parent.

    Per spec [branching]: branches are the fundamental organizational
    primitive. Code lives in branches (keyed by branch_id in Postgres);
    shared data lives in main's data tables. *)
open Yojson.Safe

type t = {
  id : string;            (* uuid, the primary key in branches table *)
  parent_id : string option;
  owner : string;         (* "system" for main, username otherwise *)
  name : string;          (* "main", "alice/main", "alice/experiment", ... *)
  created_at : string;    (* ISO 8601 timestamp *)
  last_synced : string option;  (* for merge diffing *)
}

let make ~id ~parent_id ~owner ~name ~created_at ?(last_synced = None) () : t =
  { id; parent_id; owner; name; created_at; last_synced }

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
    Ok { id; parent_id; owner; name; created_at; last_synced }
  with Failure msg -> Error msg
