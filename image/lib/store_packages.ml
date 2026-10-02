(** Pricklypear Image — Store: package registry (i-1788671083-c split).
    Branch-lensed package_registry reads/writes + provenance stamps. *)
(* ---- package-surface: package_registry (branch-lensed) ---- *)

type package_row = {
  name : string;
  enabled : bool;
  exports : string list;
  author : string;
}

let json_string_list (j : Yojson.Safe.t) : string list =
  match j with
  | `List xs ->
      List.filter_map
        (function `String s -> Some s | _ -> None)
        xs
  | _ -> []

let string_list_to_json (xs : string list) : string =
  Yojson.Safe.to_string
    (`List (List.map (fun s -> `String s) xs))

(** Effective package registry rows for the current branch (closest wins). *)
let list_packages (t : Store_db.t) : package_row list =
  match t.conn with
  | None -> []
  | Some conn ->
      (try
      let sql =
            Store_common.chain_sql ("SELECT pr.name, pr.enabled::text, pr.exports::text, pr.author, " ^
            "       chain.depth " ^
            "FROM " ^ Store_common.pkg_rel () ^ " pr " ^
            "JOIN chain ON pr.branch_id = chain.id " ^
            "ORDER BY chain.depth ASC, pr.name ASC") in
          let rows =
            Store_db.exec conn ~params:[ Pgx.Value.of_string t.branch_id ] sql
          in
          let seen = Hashtbl.create 32 in
          let acc =
            List.filter_map
              (fun row ->
                match row with
                | [ name_c; en_c; exp_c; author_c; _depth_c ] ->
                    (match Pgx.Value.to_string name_c,
                            Pgx.Value.to_string en_c,
                            Pgx.Value.to_string exp_c,
                            Pgx.Value.to_string author_c with
                     | Some name, Some en_s, Some exp_s, Some author ->
                         if Hashtbl.mem seen name then None
                         else begin
                           Hashtbl.replace seen name ();
                           let enabled =
                             en_s = "t" || en_s = "true" || en_s = "1"
                           in
                           let exports =
                             try json_string_list (Yojson.Safe.from_string exp_s)
                             with _ -> []
                           in
                           Some { name; enabled; exports; author }
                         end
                     | _ -> None)
                | _ -> None)
              rows
          in
          acc
    with exn -> Store_db.reraise ~op:"list_packages" exn)

(** Set of package names disabled on the current branch (effective). *)
let disabled_package_names (t : Store_db.t) : string list =
  let rows = list_packages t in
  (List.filter_map
       (fun (r : package_row) -> if r.enabled then None else Some r.name)
       rows)

let lookup_package (t : Store_db.t) (name : string) : package_row option =
  let rows = list_packages t in
  (List.find_opt (fun (r : package_row) -> r.name = name) rows)

(** Upsert registry row. [enabled_preserve]: if true and a row already exists
    on this branch, keep its enabled bit (header reload must not re-enable).
    Exports always replaced when provided. *)
let upsert_package_registry (t : Store_db.t) ~(name : string)
    ~(exports : string list) ~(author : string)
    ?(enabled : bool = true) ?(enabled_preserve : bool = true)
    () : unit =
  match t.conn with
  | None -> ()
  | Some conn ->
      (try
      let exp_json = string_list_to_json exports in
          let sql =
            if enabled_preserve then
              "INSERT INTO package_registry " ^
              "  (branch_id, name, enabled, exports, author) " ^
              "VALUES ($1, $2, $3, $4::jsonb, $5) " ^
              "ON CONFLICT (branch_id, name) DO UPDATE " ^
              "SET exports = EXCLUDED.exports, author = EXCLUDED.author, " ^
              "    updated_at = now()"
            else
              "INSERT INTO package_registry " ^
              "  (branch_id, name, enabled, exports, author) " ^
              "VALUES ($1, $2, $3, $4::jsonb, $5) " ^
              "ON CONFLICT (branch_id, name) DO UPDATE " ^
              "SET enabled = EXCLUDED.enabled, exports = EXCLUDED.exports, " ^
              "    author = EXCLUDED.author, updated_at = now()"
          in
          let _ =
            Store_db.exec conn
              ~params:[ Pgx.Value.of_string t.branch_id;
                        Pgx.Value.of_string name;
                        Pgx.Value.of_bool enabled;
                        Pgx.Value.of_string exp_json;
                        Pgx.Value.of_string author ]
              sql
          in
          ()
    with exn -> Store_db.reraise ~op:"upsert_package_registry" exn)

(** Write enabled bit on the *current* branch (creates a shadowing row if
    the package only exists on an ancestor). Copies exports from effective
    row when inserting a new shadow. *)
let set_package_enabled (t : Store_db.t) ~(name : string) ~(enabled : bool)
    ~(author : string) : unit =
  match t.conn with
  | None -> ()
  | Some conn ->
      (try
      let existing = lookup_package t name in
          let exports =
            match existing with
            | Some r -> r.exports
            | None -> []
          in
          let exp_json = string_list_to_json exports in
          let sql =
            "INSERT INTO package_registry " ^
            "  (branch_id, name, enabled, exports, author) " ^
            "VALUES ($1, $2, $3, $4::jsonb, $5) " ^
            "ON CONFLICT (branch_id, name) DO UPDATE " ^
            "SET enabled = EXCLUDED.enabled, author = EXCLUDED.author, " ^
            "    updated_at = now()"
          in
          let _ =
            Store_db.exec conn
              ~params:[ Pgx.Value.of_string t.branch_id;
                        Pgx.Value.of_string name;
                        Pgx.Value.of_bool enabled;
                        Pgx.Value.of_string exp_json;
                        Pgx.Value.of_string author ]
              sql
          in
          ()
    with exn -> Store_db.reraise ~op:"set_package_enabled" exn)

(** Stamp untagged function_defs on the current branch with [package].
    Matches export list names and [package]/ prefix. Used when a
    (package …) header is seen so provenance lands even if later defines
    in the same load abort (e.g. std kernel special-form fallbacks). *)
let stamp_package_bindings (t : Store_db.t) ~(package : string)
    ~(exports : string list) : unit =
  match t.conn with
  | None -> ()
  | Some conn ->
      (try
      let exp_json = string_list_to_json exports in
          let sql =
            "UPDATE function_defs SET package = $2 " ^
            "WHERE branch_id = $1 AND package IS NULL AND ( " ^
            "  name = $2 " ^
            "  OR name LIKE $2 || '/%' " ^
            "  OR name = ANY (SELECT jsonb_array_elements_text($3::jsonb)) " ^
            ")" in
          let _ =
            Store_db.exec conn
              ~params:[ Pgx.Value.of_string t.branch_id;
                        Pgx.Value.of_string package;
                        Pgx.Value.of_string exp_json ]
              sql
          in
          ()
    with exn -> Store_db.reraise ~op:"stamp_package_bindings" exn)

(** Map name -> package provenance for effective function_defs (closest wins). *)
let function_package_map (t : Store_db.t) : (string, string) Hashtbl.t =
  match t.conn with
  | None -> (Hashtbl.create 1)
  | Some conn ->
      (try
      let sql =
            Store_common.chain_sql ("SELECT fd.name, fd.package, chain.depth " ^
            "FROM " ^ Store_common.fn_rel () ^ " fd " ^
            "JOIN chain ON fd.branch_id = chain.id " ^
            "WHERE fd.package IS NOT NULL " ^
            "ORDER BY chain.depth ASC, fd.name ASC") in
          let rows =
            Store_db.exec conn ~params:[ Pgx.Value.of_string t.branch_id ] sql
          in
          let tbl = Hashtbl.create 128 in
          List.iter
            (fun row ->
              match row with
              | [ name_c; pkg_c; _depth_c ] ->
                  (match Pgx.Value.to_string name_c,
                          Pgx.Value.to_string pkg_c with
                   | Some name, Some pkg when not (Hashtbl.mem tbl name) ->
                       Hashtbl.replace tbl name pkg
                   | _ -> ())
              | _ -> ())
            rows;
          tbl
    with exn -> Store_db.reraise ~op:"function_package_map" exn)

(** Binding names whose PROVENANCE is [package] (the (package …) form's
    stamp on function_defs.package), closest definition wins.

    This is the correct membership source for export/snapshot: a package
    may define names that do NOT share its name as a prefix (std defines
    list/…, html/…, css/…; each theme-* defines theme/…). Name-prefix
    matching silently misses all of them. Returns [] when the package has
    no stamped bindings (caller decides whether to fall back to prefix). *)
let package_names_by_provenance (t : Store_db.t) (package : string)
  : string list =
  let package = String.trim package in
  if package = "" then []
  else
    let map = function_package_map t in
    Hashtbl.fold
      (fun name pkg acc -> if pkg = package then name :: acc else acc)
      map []
    |> List.sort_uniq String.compare

