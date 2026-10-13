(** Pricklypear Image — Store: code history (i-1788671083-c split).
    Code-history (012 table + tree_versions), code-diff, code-restore. *)
(* ---- code-history (branching.code-history) ---- *)

type code_history_entry = {
  rel : string;
  op : string;
  author : string option;
  doc : string option;
  package : string option;
  changed_at : string;
  body : string option; (* jsonb text *)
}

let code_history_entry_to_yojson (e : code_history_entry) : Yojson.Safe.t =
  let opt_str = function None -> `Null | Some s -> `String s in
  let body_json =
    match e.body with
    | None -> `Null
    | Some s ->
        (try Yojson.Safe.from_string s with _ -> `String s)
  in
  `Assoc
    [ ("rel", `String e.rel)
    ; ("op", `String e.op)
    ; ("author", opt_str e.author)
    ; ("doc", opt_str e.doc)
    ; ("package", opt_str e.package)
    ; ("changed_at", `String e.changed_at)
    ; ("body", body_json)
    ]

(** 012-table before-image history for [name] on [branch_id]. Kept as the
    pre-shadow fallback source (tree-time.projections Phase 3): the versioned
    tree only captures writes after migration 019, so pre-019 historical
    depth still lives here until the coexistence window closes. *)
let list_code_history_012 (t : Store_db.t) ~(branch_id : string) ~(name : string)
  : code_history_entry list =
  match t.conn with
  | None -> []
  | Some conn ->
      (try
         let sql =
           "SELECT rel, op, author, doc, package, changed_at::text, " ^
           "       body::text " ^
           "FROM code_history " ^
           "WHERE branch_id = $1 AND name = $2 " ^
           "ORDER BY changed_at DESC, id DESC"
         in
         let rows =
           Store_db.exec conn
             ~params:
               [ Pgx.Value.of_string branch_id; Pgx.Value.of_string name ]
             sql
         in
         List.filter_map
           (fun row ->
             match row with
             | [ rel_c; op_c; author_c; doc_c; pkg_c; at_c; body_c ] ->
                 (match
                    ( Pgx.Value.to_string rel_c
                    , Pgx.Value.to_string op_c
                    , Pgx.Value.to_string at_c )
                  with
                  | Some rel, Some op, Some changed_at ->
                      Some
                        { rel
                        ; op
                        ; author = Pgx.Value.to_string author_c
                        ; doc = Pgx.Value.to_string doc_c
                        ; package = Pgx.Value.to_string pkg_c
                        ; changed_at
                        ; body = Pgx.Value.to_string body_c
                        }
                  | _ -> None)
             | _ -> None)
           rows
       with exn -> Store_db.reraise ~op:"list_code_history_012" exn)

(* ---- code-history over tree_versions (tree-time.projections, Phase 3) ----
   Re-implements branching.code-history / code-diff over the versioned tree
   (tree_versions) instead of the 012 before-image table, KEEPING SIGNATURES
   UNCHANGED.

   SEMANTIC NOTE (recorded in tree-time.coexistence-plan Phase 3): 012 stored
   the OLD before-image per UPDATE/DELETE (never on the first INSERT);
   tree_versions stores the post-write head payload at each gen. A tree-backed
   history read therefore reports ONLY the rows STRICTLY BELOW HEAD — the
   previous states, newest-first — which the projections doc phrases as "read
   the version row at gen-1". The current head row (the live state) is never
   history, so an insert-only fn has no history until it is rewritten or
   deleted, exactly like the 012 table. Delete-then-recreate leaves a
   middle TOMBSTONE row (op=DELETE) below head, which is preserved.

   FALLBACK: when the code path has NO version rows yet (a pre-019 write —
   the 019 shadow only captures writes after installation), fall back to the
   012 table so no pre-shadow historical depth is lost during the coexistence
   window. Post-shadow history is complete in tree_versions. *)

(* The inner definition payload of a code version row. The tree payload for a
   fn/type/route node carries the code under a 'body' key (fn: {body, params};
   type: the fields list; route: the handler JSON) — the same shape 012 stored
   in function_defs.body / type_defs.fields / view_bindings handler. *)
let code_body_of_version (v : Tree_store.version) : string option =
  match v.Tree_store.v_body with
  | `Assoc kv -> (
      match List.assoc_opt "body" kv with
      | Some b -> Some (Yojson.Safe.to_string b)
      | None -> None )
  | _ -> None

let code_doc_of_version (v : Tree_store.version) : string option =
  match v.Tree_store.v_body with
  | `Assoc kv -> (
      match List.assoc_opt "doc" kv with
      | Some (`String s) -> Some s
      | _ -> None )
  | _ -> None

(* Resolve a logical code [name] on [branch_id] to its tree path + rel.
   Tries fn, then type, then route (same priority live_code_body_text uses).
   A candidate path "exists" if it has a version chain (covers the live AND
   the tombstoned/deleted case) OR a live tree node (the pre-019 case that
   falls back to the 012 table). *)
let code_path_for_name (t : Store_db.t) ~(branch_id : string) ~(name : string)
  : (string * string) option =
  match t.conn with
  | None -> None
  | Some conn ->
      let candidates =
        [ (Tree_store.fn_path ~branch_id ~name, "function_defs")
        ; (Tree_store.type_path ~branch_id ~name, "type_defs")
        ; (Tree_store.route_path ~branch_id ~route:name, "view_bindings")
        ]
      in
      List.find_map
        (fun (p, rel) ->
          let has_ver =
            Tree_store.versioned_history conn ~path:p () <> []
          in
          let live = Tree_store.get_by_path conn ~path:p <> None in
          if has_ver || live then Some (p, rel) else None)
        candidates

(* Tree-backed code history for [name] on [branch_id], newest-first.
   Only rows STRICTLY BELOW HEAD are history — the 012 table captured
   before-images on UPDATE/DELETE only (never on the first INSERT), and the
   projections doc defines a history read as "the version row at gen-1".
   So the current head row (the live state) is EXCLUDED; the newest history
   entry is the state just before the latest change. Empty when the name has
   no code path OR only a first-insert head row (no superseded state yet) —
   caller falls back to the 012 table so pre-shadow depth is never lost. *)
let list_code_history_tree (t : Store_db.t) ~(branch_id : string) ~(name : string)
  : code_history_entry list =
  match code_path_for_name t ~branch_id ~name with
  | None -> []
  | Some (path, rel) -> (
      match t.conn with
      | None -> []
      | Some conn ->
          let vers = Tree_store.versioned_history conn ~path () in
          (* newest gen = the head (excluded); anything below it is history *)
          let head_gen =
            List.fold_left
              (fun acc v ->
                match v.Tree_store.v_gen with
                | Some g -> Int64.max acc g
                | None -> acc)
              0L vers
          in
          let below_head =
            List.filter
              (fun v ->
                match v.Tree_store.v_gen with
                | Some g -> Int64.compare g head_gen < 0
                | None -> true)
              vers
          in
          List.rev_map
            (fun v ->
              { rel
              ; op =
                  (if v.Tree_store.v_tombstone then "DELETE" else "UPDATE")
              ; author = v.Tree_store.v_author
              ; doc = code_doc_of_version v
              ; package = None
              ; changed_at =
                  (match v.Tree_store.v_created_at with
                   | Some c -> c
                   | None -> "")
              ; body =
                  (if v.Tree_store.v_tombstone then None
                   else code_body_of_version v)
              })
            below_head)

(** Newest-first history rows for [name] on [branch_id]. Tree-backed (Phase 3)
    with an 012 fallback for pre-shadow names. *)
let list_code_history (t : Store_db.t) ~(branch_id : string) ~(name : string)
  : code_history_entry list =
  let tree_hist = list_code_history_tree t ~branch_id ~name in
  if tree_hist <> [] then tree_hist
  else list_code_history_012 t ~branch_id ~name

let list_code_history_json (t : Store_db.t) ~(branch_id : string) ~(name : string)
  : string =
  let entries = list_code_history t ~branch_id ~name in
  Yojson.Safe.to_string
    (`List (List.map code_history_entry_to_yojson entries))

(** Live body text for a name on a branch: function body preferred, else
    type fields, else view_bindings handler JSON. *)
let live_code_body_text (t : Store_db.t) ~(branch_id : string) ~(name : string)
  : string option =
  match t.conn with
  | None -> None
  | Some conn ->
      (try
         let try_fn () =
           let sql =
             "SELECT body::text FROM function_defs " ^
             "WHERE branch_id = $1 AND name = $2"
           in
           match
             Store_db.exec conn
               ~params:
                 [ Pgx.Value.of_string branch_id; Pgx.Value.of_string name ]
               sql
           with
           | [ [ c ] ] -> Pgx.Value.to_string c
           | _ -> None
         in
         let try_ty () =
           let sql =
             "SELECT fields::text FROM type_defs " ^
             "WHERE branch_id = $1 AND name = $2"
           in
           match
             Store_db.exec conn
               ~params:
                 [ Pgx.Value.of_string branch_id; Pgx.Value.of_string name ]
               sql
           with
           | [ [ c ] ] -> Pgx.Value.to_string c
           | _ -> None
         in
         let try_rt () =
           let sql =
             "SELECT jsonb_build_object('handler', handler, " ^
             "  'visibility', COALESCE(visibility, 'private'))::text " ^
             "FROM view_bindings WHERE branch_id = $1 AND route = $2"
           in
           match
             Store_db.exec conn
               ~params:
                 [ Pgx.Value.of_string branch_id; Pgx.Value.of_string name ]
               sql
           with
           | [ [ c ] ] -> Pgx.Value.to_string c
           | _ -> None
         in
         (match try_fn () with
          | Some s -> Some s
          | None ->
              (match try_ty () with
               | Some s -> Some s
               | None -> try_rt ()))
       with exn -> Store_db.reraise ~op:"live_code_body_text" exn)

(** Minimal line-oriented text diff: lines only in [a], only in [b], or
    shared. Not a full LCS unified diff — good enough for prim smoke. *)
let simple_text_diff ~(a : string) ~(b : string) : string =
  let lines s =
    if s = "" then []
    else String.split_on_char '\n' s
  in
  let la = lines a in
  let lb = lines b in
  let module S = Set.Make (String) in
  let sa = List.fold_left (fun acc x -> S.add x acc) S.empty la in
  let sb = List.fold_left (fun acc x -> S.add x acc) S.empty lb in
  let buf = Buffer.create 256 in
  Buffer.add_string buf "--- history\n+++ current\n";
  List.iter
    (fun line ->
      if not (S.mem line sb) then begin
        Buffer.add_char buf '-';
        Buffer.add_string buf line;
        Buffer.add_char buf '\n'
      end)
    la;
  List.iter
    (fun line ->
      if not (S.mem line sa) then begin
        Buffer.add_char buf '+';
        Buffer.add_string buf line;
        Buffer.add_char buf '\n'
      end)
    lb;
  Buffer.contents buf

(** Current live body vs the nth-newest history body (1-based). *)
let code_diff (t : Store_db.t) ~(branch_id : string) ~(name : string) ~(n : int)
  : (string, string) result =
  if n < 1 then Error "code-diff: n must be >= 1"
  else
    let hist = list_code_history t ~branch_id ~name in
    match List.nth_opt hist (n - 1) with
    | None -> Error (Printf.sprintf "code-diff: no history entry #%d" n)
    | Some entry ->
        let old_body = Option.value entry.body ~default:"" in
        let live =
          Option.value
            (live_code_body_text t ~branch_id ~name)
            ~default:""
        in
        Ok (simple_text_diff ~a:old_body ~b:live)

(* The handler + visibility from a route node's inner body. The route tree
   payload body is {route, handler, visibility} (Tree_store.upsert_route);
   code-restore needs the two writable fields to re-write the route. *)
let route_parts_of_body (body_js : string) : (string * string) option =
  match Yojson.Safe.from_string body_js with
  | `Assoc kv -> (
      match List.assoc_opt "handler" kv with
      | Some (`String h) ->
          let vis =
            match List.assoc_opt "visibility" kv with
            | Some (`String v) -> v
            | _ -> "private"
          in
          Some (h, vis)
      | _ -> None )
  | _ -> None

(** (code-restore name [n] [owner/branch]) — restore a code node (fn/type/
    route) at head-n as a NEW write at head+1 (restore-by-write). This is the
    code-path analogue of (undo) (which is datum-path only): gen-read + write
    the same value back through the code write surface (upsert_fn /
    upsert_type / upsert_route), so the reverse mirror re-populates
    function_defs / type_defs / view_bindings and the restore itself is
    versioned + redoable (the restored value becomes the new head; calling it
    again restores the state before that). The below-head semantic matches
    code-history's "read the version row at gen-1": n=1 restores the state
    just before the latest change; n=2 the state before that. Out-of-range n,
    no history, no body, or an unsupported rel are clean error strings (never
    a crash). Consumers: tree-time.consumers — code-restore = gen-read +
    define, the lossless-redefine pattern the 012 design documented by hand,
    now a real prim. [author] is the session user, the fallback only when the
    source version row carries no author. *)
let code_restore (t : Store_db.t) ~(branch_id : string) ~(name : string)
    ~(author : string) ?(n : int = 1) () : string =
  match code_path_for_name t ~branch_id ~name with
  | None -> Printf.sprintf "code-restore: no such code path: %s" name
  | Some (path, rel) -> (
      match t.conn with
      | None -> "code-restore: no DB"
      | Some conn ->
          let vers = Tree_store.versioned_history conn ~path () in
          if vers = [] then
            Printf.sprintf "code-restore: no version history for %s" name
          else
            (* newest-first; index 0 is the live head (the current state, never
               a restore target), index n is the state n steps back. Row-index,
               NOT gen arithmetic: the branch_generations counter is shared
               across branches and bumped by tombstones, so gens are not
               guaranteed consecutive for a single path — counting version rows
               is the correct "n steps back". *) 
            let newest_first = List.rev vers in
            if n < 1 then Printf.sprintf "code-restore: n must be >= 1"
            else if n >= List.length newest_first then
              Printf.sprintf
                "code-restore: %s has no state %d steps back" name n
            else
              let v = List.nth newest_first n in
              if v.Tree_store.v_tombstone then
                Printf.sprintf
                  "code-restore: %s at %d steps back is a deletion (tombstone); use tree/delete"
                  name n
              else
                let rest_author =
                  match v.Tree_store.v_author with
                  | Some a -> a
                  | None -> author
                in
                (match code_body_of_version v with
                 | None ->
                     Printf.sprintf
                       "code-restore: no body %d steps back for %s" n name
                 | Some body_js -> (
                     match rel with
                     | "function_defs" ->
                         let doc = code_doc_of_version v in
                         ignore
                           (Tree_store.upsert_fn conn ~branch_id ~name
                              ~body_json:body_js ~author:rest_author ~doc);
                         Printf.sprintf
                           "restored fn %s (%d steps back)" name n
                     | "type_defs" ->
                         ignore
                           (Tree_store.upsert_type conn ~branch_id ~name
                              ~fields_json:body_js ~author:rest_author);
                         Printf.sprintf
                           "restored type %s (%d steps back)" name n
                     | "view_bindings" -> (
                         match route_parts_of_body body_js with
                         | Some (handler, visibility) ->
                             ignore
                               (Tree_store.upsert_route conn ~branch_id
                                  ~route:name ~handler ~visibility);
                             Printf.sprintf
                               "restored route %s (%d steps back)" name n
                         | None ->
                             Printf.sprintf
                               "code-restore: route body malformed for %s"
                               name )
                     | _ ->
                         Printf.sprintf "code-restore: unsupported rel %s" rel)))

