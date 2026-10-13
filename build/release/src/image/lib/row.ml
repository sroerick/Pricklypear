(** Row codec — one home for pgx row-cell accessors.

    Previously hand-copied per store module (ai_store, auth_store, jobs,
    media each carried their own [cell_string] and friends with byte-identical
    bodies). i-1788671505. All cells are positional: callers index columns by
    number, matching the SELECT column order of each query. *)

(* nth cell of a pgx row as a string option (SQL NULL / absent -> None). *)
let cell_string row i =
  match List.nth_opt row i with
  | Some v -> Pgx.Value.to_string v
  | None -> None

(* Postgres boolean text spellings; anything else (incl. NULL) -> false. *)
let cell_bool row i =
  match cell_string row i with
  | Some ("t" | "true" | "TRUE" | "1") -> true
  | Some ("f" | "false" | "FALSE" | "0") -> false
  | _ -> false

let cell_int row i =
  match cell_string row i with
  | Some s -> (try Some (int_of_string s) with _ -> None)
  | None -> None

let cell_int64 row i =
  match cell_string row i with
  | Some s -> (try Some (Int64.of_string s) with _ -> None)
  | None -> None

(* Non-empty text (uuid-ish columns use this to keep "" as None). *)
let cell_uuid_opt row i =
  match cell_string row i with
  | Some s when s <> "" -> Some s
  | _ -> None
