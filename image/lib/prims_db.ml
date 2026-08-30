(** Prim registration extracted from session.ml (Eio-floor Phase 4). *)
open Session_types

let promote_to_personal (t : t) ~(names : string list)
  : (int, string) result =
  let me_opt = Auth_store.get_user_by_username t.store t.manifest.username in
  match me_opt with
  | None -> (Error "no such user")
  | Some me when me.user_type <> "Agent_tools.agent" ->
      (Error "promote-to-personal is for Agent_tools.agent users")
  | Some me ->
      (match me.parent_user_id with
       | None -> (Error "Agent_tools.agent has no parent")
       | Some pid ->
           let parent_opt = Auth_store.get_user_by_id t.store pid in
           (match parent_opt with
            | None -> (Error "parent user not found")
            | Some parent ->
                let allowed =
                  Auth_store.get_allow_agent_promote t.store
                    ~user_id:parent.id
                in
                if not allowed then
                  (Error "Agent_tools.agent promote disabled by parent")
                else
                  let final_names =
                    if names <> [] then names
                    else
                      let meta = Store.list_function_names_meta t.store in
                      let own =
                        List.filter (fun (_, _, _, d) -> d = 0) meta
                      in
                      (List.map (fun (n, _, _, _) -> n) own)
                  in
                  if final_names = [] then
                    (Error "no functions defined on this branch")
                  else
                    let source_id = Store.current_branch_id t.store in
                    let tgt_opt =
                      Store.lookup_branch_id_by_name t.store
                        ~owner:parent.username ~name:"personal"
                    in
                    (match tgt_opt with
                     | None ->
                         (Error "parent personal branch not found")
                     | Some target_id ->
                         let n =
                           Store.promote_functions t.store
                             ~source_id ~target_id ~names:final_names
                         in
                         (Ok n))))


let register ?(rehydrate_fn : t -> unit = fun _ -> ()) (t : t) (tbl : Primitives.t) : unit =
  (* Quoted type/field names are VSymbol after Phase 1; CRUD still wants
     string payloads for SQL. Coerce symbols→strings at the CRUD boundary
     only (do not collapse in symbol=? / general prims). *)
  let coerce_sym_args (args : Pricklypear_lisp.Ast.value list) =
    List.map
      (function
        | Pricklypear_lisp.Ast.VSymbol s -> Pricklypear_lisp.Ast.VStr s
        | v -> v)
      args
  in
  let register_crud name fn =
    Primitives.register tbl name (fun args ->
        Pricklypear_lisp.Eval.note_db_hit ();
        let t0 = Unix.gettimeofday () in
        let result = fn (coerce_sym_args args) in
        let elapsed_ms = (Unix.gettimeofday () -. t0) *. 1000. in
        (* accumulate DB wall time into the eval budget for page-timing *)
        Pricklypear_lisp.Eval.note_db_ms elapsed_ms;
        result)
  in
  (* (deftype 'name 'field1 'field2 ...) -> VStr "defined type <name>".
     All args are quoted symbols (now VSymbol; coerced above). Persists
     the declaration into type_defs on the session's branch. *)
  let deny_anon name =
    (* Raise, don't return a VStr: a returned string is silently dropped
       by (seq …), so a multi-write handler could partially succeed past
       an anonymous denial. Raising aborts the whole eval. (BORGE-1785213376) *)
    raise
      (Pricklypear_lisp.Ast.Interp_error
         (Pricklypear_lisp.Ast.User_error
            (name ^ ": denied for anonymous", "bad-args")))
  in
  (* Per-request identity snapshot (avoids the cotenant race where the
     live t.manifest is mid-swap). Falls back to t.manifest outside HTTP. *)
  let actor () =
    match Request_context.get () with
    | Some c -> (Request_context.manifest c).username
    | None -> t.manifest.username
  in
  (* deftype registration is deferred until require_admin is in scope
     (BORGE-1785213343: schema-authority — admin only). Placeholder:
     see register after require_admin below. *)

  (* (get 'type id) -> VStr fields-json | VNil *)
  register_crud "get"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr type_name;
          Pricklypear_lisp.Ast.VStr id ] ->
          Crud.get t.store ~type_name ~id
      | _ -> (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("get", "bad args (need type, id)", "bad-args")))));

  (* (create 'type "{fields-json}") -> VStr new-id; raises Prim_error
     "failed" when the insert is rejected. *)
  register_crud "create"
    (fun args ->
      let uname =
        match Request_context.get () with
        | Some c -> (Request_context.manifest c).username
        | None -> t.manifest.username
      in
      if uname = "anonymous" then deny_anon "create"
      else
        match args with
        | [ Pricklypear_lisp.Ast.VStr type_name;
            Pricklypear_lisp.Ast.VStr fields_json ] ->
            (* Attribute to the per-request identity, not the live manifest
               (which can be mid-swap under the cotenant race). *)
            Crud.create t.store
              ~type_name ~fields_json
              ~author:uname
        | _ ->
            (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("create", "bad args (need type, fields)", "bad-args")))));

  (* (update 'type id "{fields-json}") -> VStr id | VNil *)
  register_crud "update"
    (fun args ->
      let uname = actor () in
      if uname = "anonymous" then deny_anon "update"
      else
        match args with
        | [ Pricklypear_lisp.Ast.VStr type_name;
            Pricklypear_lisp.Ast.VStr id;
            Pricklypear_lisp.Ast.VStr fields_json ] ->
            Crud.update t.store ~type_name ~id ~fields_json
        | _ ->
            ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("update", "bad args (need type, id, fields)", "bad-args"))))));

  (* (update-field 'type id "field" "value") -> VStr id | VNil.
      Ergonomic shorthand: composes get + dict-set + update in one prim.
      Lands it18 (2026-07-06); resolves the it14 "verbose round-trip syntax" TODO. *)
  register_crud "update-field"
    (fun args ->
      let uname = actor () in
      if uname = "anonymous" then deny_anon "update-field"
      else
        match args with
        | [ Pricklypear_lisp.Ast.VStr type_name;
            Pricklypear_lisp.Ast.VStr id;
            Pricklypear_lisp.Ast.VStr field;
            Pricklypear_lisp.Ast.VStr value ] ->
            Crud.update_field t.store ~type_name ~id ~field ~value
        | _ ->
            ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("update-field", "bad args (need type, id, field, value)", "bad-args"))))));

  (* (delete 'type id) -> VStr id | VNil *)
  register_crud "delete"
    (fun args ->
      let uname = actor () in
      if uname = "anonymous" then deny_anon "delete"
      else
        match args with
        | [ Pricklypear_lisp.Ast.VStr type_name;
            Pricklypear_lisp.Ast.VStr id ] ->
            Crud.delete t.store ~type_name ~id
        | _ ->
            (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("delete", "bad args (need type, id)", "bad-args")))));

  (* (select-for-update 'type id) -> VStr fields-json | VNil.
     Acquires an explicit SELECT ... FOR UPDATE row lock on the
     named row, for the duration of the surrounding transaction.
     Per spec [storage.transactions] (prim lands it17, 2026-07-06).
     See Crud.select_for_update doc for transactional-scoping caveat:
     outside a transaction, this is a hinted-get with no real lock. *)
  register_crud "select-for-update"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr type_name;
          Pricklypear_lisp.Ast.VStr id ] ->
          Crud.select_for_update t.store ~type_name ~id
      | _ -> (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("select-for-update", "bad args (need type, id)", "bad-args")))));

  (* (rows 'type [limit]) -> VList of (id . fields) pairs.
     With no limit, defaults to 100. NOTE: prim name is `rows`, NOT
     `list` — the latter is the pure VList-constructor prim. Earlier
     tentative naming collided and broke `(list 1 2 3)`; renamed to
     avoid that. *)
  register_crud "rows"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr type_name ] ->
          Crud.list t.store ~type_name ()
      | [ Pricklypear_lisp.Ast.VStr type_name;
          Pricklypear_lisp.Ast.VInt n ] ->
          Crud.list t.store ~type_name ~limit:(Int64.to_int n) ()
      | [ Pricklypear_lisp.Ast.VStr type_name;
          Pricklypear_lisp.Ast.VStr sort ] ->
          Crud.list t.store ~type_name ~sort:(Some sort) ()
      | [ Pricklypear_lisp.Ast.VStr type_name;
          Pricklypear_lisp.Ast.VInt n;
          Pricklypear_lisp.Ast.VStr sort ] ->
          Crud.list t.store ~type_name ~limit:(Int64.to_int n) ~sort:(Some sort) ()
      | _ -> (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("rows", "bad args (need type[, limit][, sort])", "bad-args")))));

  (* (find-row 'type "field" "value") -> fields JSON | () *)
  register_crud "find-row"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr type_name;
          Pricklypear_lisp.Ast.VStr field;
          Pricklypear_lisp.Ast.VStr value ] ->
          Crud.find_row t.store ~type_name ~field ~value
      | _ ->
          ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("find-row", "need type, field, value", "bad-args"))))));

  (* (find-rows 'type "f" "v" … [limit] [sort]) -> list of (id . fields) *)
  register_crud "find-rows"
    (fun args ->
      let rec parse_filters acc = function
        | [] -> Ok (List.rev acc, 500, None)
        | [ Pricklypear_lisp.Ast.VInt n ] ->
            Ok (List.rev acc, Int64.to_int n, None)
        | [ Pricklypear_lisp.Ast.VStr s ] ->
            Ok (List.rev acc, 500, Some s)
        | [ Pricklypear_lisp.Ast.VInt n; Pricklypear_lisp.Ast.VStr s ] ->
            Ok (List.rev acc, Int64.to_int n, Some s)
        | Pricklypear_lisp.Ast.VStr f :: Pricklypear_lisp.Ast.VStr v :: rest ->
            parse_filters ((f, v) :: acc) rest
        | _ -> Error "find-rows: bad filter args"
      in
      match args with
      | Pricklypear_lisp.Ast.VStr type_name :: rest ->
          (match parse_filters [] rest with
           | Error e ->
               raise (Pricklypear_lisp.Ast.Interp_error
                        (Pricklypear_lisp.Ast.Prim_error
                           ("find-rows", e, "bad-args")))
           | Ok (filters, limit, sort) ->
               Crud.find_rows t.store ~type_name ~filters ~limit ~sort ())
      | _ ->
          ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("find-rows", "need type and field/value pairs", "bad-args"))))));

  (* (find-text 'type "field" "needle" [limit]) *)
  register_crud "find-text"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr type_name;
          Pricklypear_lisp.Ast.VStr field;
          Pricklypear_lisp.Ast.VStr needle ] ->
          Crud.find_text t.store ~type_name ~field ~needle ()
      | [ Pricklypear_lisp.Ast.VStr type_name;
          Pricklypear_lisp.Ast.VStr field;
          Pricklypear_lisp.Ast.VStr needle;
          Pricklypear_lisp.Ast.VInt n ] ->
          Crud.find_text t.store ~type_name ~field ~needle
            ~limit:(Int64.to_int n) ()
      | _ ->
          ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("find-text", "need type, field, needle [, limit]", "bad-args"))))));

  (* (find-random 'type) -> (id . fields) | () *)
  register_crud "find-random"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr type_name ] ->
          Crud.find_random t.store ~type_name
      | _ ->
          (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("find-random", "need type", "bad-args")))));

  (* one-tree P6: typed nodes + path addressing + backlinks *)
  register_crud "node-path"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr type_name;
          Pricklypear_lisp.Ast.VStr id ] ->
          (Crud.node_path ~type_name ~id)
      | _ ->
          (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("node-path", "need type, id", "bad-args")))));

  register_crud "type-fields"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr type_name ] ->
          Crud.type_fields t.store ~type_name
      | _ ->
          (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("type-fields", "need type", "bad-args")))));

  register_crud "backlinks"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr needle ] ->
          Crud.backlinks t.store ~needle ()
      | [ Pricklypear_lisp.Ast.VStr needle;
          Pricklypear_lisp.Ast.VInt n ] ->
          Crud.backlinks t.store ~needle ~limit:(Int64.to_int n) ()
      | _ ->
          (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("backlinks", "need id-or-path [, limit]", "bad-args")))));

  (* Hierarchical tree prims (notes.tree-cutover K0). Direct tree_nodes
     access for folder/note/heading — not kind=datum CRUD. *)
  register_crud "tree/get"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr path ] ->
          Crud.tree_get t.store ~path ()
      | [ Pricklypear_lisp.Ast.VStr path
        ; Pricklypear_lisp.Ast.VInt g
        ] ->
          (* reads-as-of-gen: optional :gen N -> versioned read at that gen
             (tree-time.reads-as-of-gen). Omitted means head. *)
          Crud.tree_get t.store ~path ~at_gen:g ()
      | _ ->
          (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("tree/get", "need path [, :gen]/", "bad-args")))));

  register_crud "tree/list"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr prefix ] ->
          Crud.tree_list t.store ~prefix ()
      | [ Pricklypear_lisp.Ast.VStr prefix; Pricklypear_lisp.Ast.VInt depth ] ->
          Crud.tree_list t.store ~prefix ~depth:(Int64.to_int depth) ()
      | [ Pricklypear_lisp.Ast.VStr prefix
        ; Pricklypear_lisp.Ast.VInt depth
        ; Pricklypear_lisp.Ast.VInt lim
        ] ->
          Crud.tree_list t.store ~prefix ~depth:(Int64.to_int depth)
            ~limit:(Int64.to_int lim) ()
      | _ ->
          ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("tree/list", "need prefix [, depth [, limit]]", "bad-args"))))));

  (* (tree/children-of path [limit]) -> VList of direct-child node JSON.
     one-tree.tree-read-prims: level-predicate children read over
     tree_nodes (never an anti-join; outline covering at +2 under label h).
     Anonymous reads allowed (resolved against system/main, prefix-confined). *)
  register_crud "tree/children-of"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr path ] ->
          Crud.tree_children_of t.store ~path ()
      | [ Pricklypear_lisp.Ast.VStr path; Pricklypear_lisp.Ast.VInt lim ] ->
          Crud.tree_children_of t.store ~path ~limit:(Int64.to_int lim) ()
      | _ ->
          ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("tree/children-of", "need path [, limit]", "bad-args"))))));

  (* (tree/subtree path [limit]) -> VList of all-descendant node JSON under
     path, capped at limit. one-tree.tree-read-prims. Anonymous allowed. *)
  register_crud "tree/subtree"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr path ] ->
          Crud.tree_subtree t.store ~path ()
      | [ Pricklypear_lisp.Ast.VStr path; Pricklypear_lisp.Ast.VInt lim ] ->
          Crud.tree_subtree t.store ~path ~limit:(Int64.to_int lim) ()
      | _ ->
          ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("tree/subtree", "need path [, limit]", "bad-args"))))));

  (* Lens-aware read prims (one-tree.tree-read-prims, BORGE-1785516993).
     Resolve a RELATIVE logical path (fn/type/route/pkg suffixes such as
     "fn.notes/doc/get", "pkg.todo", "route./issues") against the SESSION's
     branch lens chain — self -> parent -> ... -> system/main, closest-wins,
     exactly like function_defs. Lets a session on <user>/personal see the
     EFFECTIVE override chain for tree_nodes (the store-tree kind the
     tree-browser v2 /sys + branch-lens roots need). Anonymous reads
     allowed (resolves against system/main via the anon lens). *)
  register_crud "tree/lens-get"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr suffix ] ->
          Crud.tree_lens_get t.store ~suffix
      | _ ->
          ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("tree/lens-get", "need relative-suffix (e.g. \"fn.notes/doc/get\")", "bad-args"))))));

  register_crud "tree/lens-children"
    (fun args ->
      let uname = actor () in
      ignore uname;
      match args with
      | [ Pricklypear_lisp.Ast.VStr suffix ] ->
          Crud.tree_lens_children t.store ~suffix ()
      | [ Pricklypear_lisp.Ast.VStr suffix
        ; Pricklypear_lisp.Ast.VInt lim ] ->
          Crud.tree_lens_children t.store ~suffix ~limit:(Int64.to_int lim) ()
      | _ ->
          ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("tree/lens-children", "need relative-suffix [, limit]", "bad-args"))))));

  register_crud "tree/lens-subtree"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr suffix ] ->
          Crud.tree_lens_subtree t.store ~suffix ()
      | [ Pricklypear_lisp.Ast.VStr suffix
        ; Pricklypear_lisp.Ast.VInt lim ] ->
          Crud.tree_lens_subtree t.store ~suffix ~limit:(Int64.to_int lim) ()
      | _ ->
          ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("tree/lens-subtree", "need relative-suffix [, limit]", "bad-args"))))));

  (* local-first (borge/local-first.borg lens-model + replication-units,
     campaign 8.1). lens/fork names + creates a lens branch as a NAMED CHILD
     of the checked-out branch per the pinned <owner>/lens-<device>[-<date>]
     contract (single-writer-per-lens, server stays parent). Non-anonymous
     only (an anonymous identity owns no lens).
     (lens/fork device-name [, date]) -> VStr "ok owner/lens-<device>[-<date>] (id)" *)
  register_crud "lens/fork"
    (fun args ->
      let uname = actor () in
      if uname = "anonymous" then deny_anon "lens/fork"
      else
        match args with
        | [ Pricklypear_lisp.Ast.VStr device ] ->
            Crud.lens_fork t.store ~device ~date:None ()
        | [ Pricklypear_lisp.Ast.VStr device;
            Pricklypear_lisp.Ast.VStr date ] ->
            Crud.lens_fork t.store ~device ~date:(Some date) ()
        | _ ->
            ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("lens/fork", "need device-name [, date]", "bad-args"))))));

  (* local-first replication-units (borge/local-first.borg). replication/unit
     materializes the exact authorized-read subtree the current identity
     would replicate: data.u.<user>.* + data.shared.* + the forked code lens
     + the branch ancestry it resolves through. Read-only; safe for any
     identity (resolves against its own lens — anon sees data.u.anonymous +
     shared seed). Returns a JSON manifest with per-prefix node counts. *)
  register_crud "replication/unit"
    (fun args ->
      match args with
      | [] -> Crud.replication_unit t.store ()
      | _ ->
          ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("replication/unit", "no arguments", "bad-args"))))));

  (* local-first sync (borge/local-first.borg sync, campaign 8.3). The
     server-side merge authority. sync/plan takes a CLIENT (path, hash)
     manifest — the exact shape Lens_store.node_manifest produces — plus
     an optional three-way merge base (the (path, hash) manifest the
     lens last synced at), and returns the reconciliation: what the
     server would pull (ServerOnly), push (ClientOnly), or merge
     (Diverge / conflict). Pure classification over the server's own
     subtree hash manifest; read-only, safe for any identity (scoped to
     its own lens replication unit).

       (sync/plan '[("data.u....".x "<hash>") ...])
       (sync/plan client-manifest base-manifest)

     Returns a JSON object {same, client_only, server_only, diverge,
     client_only_paths, server_only_paths, diverge_paths} — or, with a
     base, a three-way plan {rows:[{path, outcome, detail}]}. *)
  let parse_manifest (v : Pricklypear_lisp.Ast.value) : Sync.manifest option =
    let parse_pair = function
      | Pricklypear_lisp.Ast.VList [ Pricklypear_lisp.Ast.VStr p;
                                     Pricklypear_lisp.Ast.VStr h ] -> Some (p, h)
      | _ -> None
    in
    match v with
    | Pricklypear_lisp.Ast.VList pairs ->
        let m = List.filter_map parse_pair pairs in
        if List.length m = List.length pairs then Some m else None
    | _ -> None
  in
  let sync_user_prefix () =
    let uname = actor () in
    "data.u." ^ Tree_store.enc_label uname
  in
  register_crud "sync/plan"
    (fun args ->
      match args with
      | [ cm; b ] ->
          (* three-way: client manifest + base (last-synced) manifest *)
          (match parse_manifest cm, parse_manifest b with
           | Some c, Some base ->
               let prefix = sync_user_prefix () in
               (match Sync.server_manifest t.store ~prefix with
                | None -> ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("sync/plan", "no DB", "failed")))))
                | Some server ->
                    let plans = Sync.plan_of base server c in
                    (Pricklypear_lisp.Ast.VStr (Sync.json_of_plan plans)))
           | _ -> ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("sync/plan", "manifests must be [[path, hash], …]", "bad-args"))))))
      | [ cm ] ->
          (* two-way classify: client manifest vs server, no base *)
          (match parse_manifest cm with
           | Some c ->
               let prefix = sync_user_prefix () in
               (match Sync.server_manifest t.store ~prefix with
                | None -> ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("sync/plan", "no DB", "failed")))))
                | Some server ->
                    let r = Sync.classify server c in
                    (Pricklypear_lisp.Ast.VStr (Sync.json_of_report r)))
           | None -> ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("sync/plan", "manifest must be [[path, hash], …]", "bad-args"))))))
      | _ -> ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("sync/plan", "need client-manifest [, base-manifest]", "bad-args"))))));

  (* sync/push — apply a client push for the caller's data subtree
     (borge/local-first.borg sync, v1 two-way + three-way / data-only).
     Takes the client (path, hash) manifest (same shape as sync/plan)
     plus a list of (path, envelope-json) pairs for the bodies the
     client holds, and an OPTIONAL third argument: the base manifest
     the client recorded at its last successful sync (the common
     ancestor — the __base.json the client pins from the previous
     round's merged_manifest). The server classifies its own
     data.u.<user>.* manifest against the client's, then:

       - ClientOnly paths (server missing the node) are applied through
         the typed-boundary-validated datum write surface
         (Sync.apply_client_node).
       - Diverge/conflict paths (both sides hold different hashes) are
         LWW-ARBITRATED per the-glochids v0: the client envelope wins iff
         its updated_at (epoch, stamped by the lens substrate on every
         local put) is STRICTLY newer than the server node's; otherwise
         the server copy stays. EVERY resolution is surfaced with its
         winner (lww_client_won / lww_server_kept) — the obituary rule
         applies to the losing side; nothing is silently dropped.
       - With a base, the FULL three-way plan (Sync.plan_of) runs:
         clean client edits apply (theirs_add/theirs_mod), client
         DELETIONS propagate (theirs_del), server deletions come back
         as client_delete_paths for the client to mirror locally, and
         conflicts get the same LWW arbitration.
       - ServerOnly paths are returned so the client can pull them.

     Returns a JSON object:
       {mode, applied, client_only, server_only, diverge,
        server_only_paths, diverge_paths, conflicts, lww_client_won,
        lww_server_kept, client_delete_paths, merged_manifest,
        generation}
     where merged_manifest is the server's post-apply subtree manifest —
     the (path, hash) list the client records as its NEXT round's base —
     and generation is the new (branch, generation) coordinate to pin.
     Code-lens sync (the b.<lens> code paths) is NOT in scope here - it
     is the branch-pointer move (promote/merge), a separate path; callers
     filter the manifest to data paths first.
       (sync/push client-manifest client-bodies [base-manifest])
     client-manifest: [[path, hash], ...]  (same as sync/plan)
     client-bodies:    [[path, envelope-json-str], ...]
     base-manifest:    [[path, hash], ...]  (client's last-synced state) *)
  register_crud "sync/push"
    (fun args ->
      (* [cm] client manifest; [bodies] client envelope bodies;
         [base] optional last-synced manifest — three-way when present. *)
      let parse_bodies body_pairs =
        match body_pairs with
        | Pricklypear_lisp.Ast.VList pairs ->
            List.filter_map
              (function
               | Pricklypear_lisp.Ast.VList
                   [ Pricklypear_lisp.Ast.VStr p;
                     Pricklypear_lisp.Ast.VStr env ] ->
                   (try Some (p, Yojson.Safe.from_string env)
                    with _ -> None)
               | _ -> None)
              pairs
        | _ -> []
      in
      let lww_diverge ~server ~cb (diverge : (string * string) list) =
        (* the-glochids v0: LWW-arbitrate diverge pairs. Client strictly
           newer wins (applied); otherwise the server copy stays. Every
           resolution is surfaced with its winner. *)
        let client_paths = List.map fst diverge in
        let epochs = Sync.server_updated_at t.store client_paths in
        List.fold_left
          (fun (won, kept) (p, _) ->
            let cl =
              match List.assoc_opt p cb with
              | Some env -> Sync.envelope_updated_at env
              | None -> None
            in
            let client_wins =
              match List.assoc_opt p epochs with
              | Some sv -> Sync.lww_client_wins cl sv
              | None -> false
            in
            if client_wins then
              match List.assoc_opt p cb with
              | Some env -> (
                  match Sync.apply_client_node t.store ~env with
                  | Ok () -> (p :: won, kept)
                  | Error _ -> (won, p :: kept))
              | None -> (won, p :: kept)
            else (won, p :: kept))
          ([], []) diverge
      in
      match args with
      | [ cm; bodies ] ->
          (match parse_manifest cm, bodies with
           | Some c, _ ->
               let prefix = sync_user_prefix () in
               (match Sync.server_manifest t.store ~prefix with
                | None ->
                    ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("sync/push", "no DB", "failed")))))
                | Some server ->
                    let cb = parse_bodies bodies in
                    let r = Sync.classify server c in
                    (* 1. ClientOnly pushes (server missing the node) —
                       no arbitration needed, the server has no copy. *)
                    let applied_co =
                      List.fold_left
                        (fun n (p, _) ->
                          match List.assoc_opt p cb with
                          | Some env ->
                              (match Sync.apply_client_node t.store ~env with
                               | Ok () -> n + 1
                               | Error _ -> n)
                          | None -> n)
                        0 r.client_only
                    in
                    (* 2. Diverge pairs LWW-arbitrate (the-glochids v0). *)
                    let lww_won, lww_kept =
                      lww_diverge ~server ~cb r.diverge
                    in
                    let applied = applied_co + List.length lww_won in
                    let bid = Store.current_branch_id t.store in
                    let gen =
                      match Store.current_generation t.store ~branch_id:bid with
                      | Some n -> n
                      | None -> 0L
                    in
                    let json =
                      Yojson.Safe.to_string
                        (`Assoc
                           [ ("mode", `String "two-way")
                           ; ("applied", `Int applied)
                           ; ("client_only",
                              `Int (List.length r.client_only))
                           ; ("server_only",
                              `Int (List.length r.server_only))
                           ; ("diverge", `Int (List.length r.diverge))
                           ; ("server_only_paths",
                              `List (List.map
                                       (fun (p, _) -> `String p)
                                       (Sync.normalize r.server_only)))
                           ; ("diverge_paths",
                              `List (List.map
                                       (fun (p, _) -> `String p)
                                       (Sync.normalize r.diverge)))
                           ; ("lww_client_won",
                              `List (List.map (fun p -> `String p)
                                       (List.rev lww_won)))
                           ; ("lww_server_kept",
                              `List (List.map (fun p -> `String p)
                                       (List.rev lww_kept)))
                           ; ("generation",
                              `String (Int64.to_string gen))
                           ])
                    in
                    (Pricklypear_lisp.Ast.VStr json))
           | _ ->
               ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("sync/push", "args must be (client-manifest client-bodies)", "bad-args"))))))
      | [ cm; bodies; base ] ->
          (match parse_manifest cm, bodies, parse_manifest base with
           | Some c, _, Some base ->
               let prefix = sync_user_prefix () in
               (match Sync.server_manifest t.store ~prefix with
                | None ->
                    ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("sync/push", "no DB", "failed")))))
                | Some server ->
                    let cb = parse_bodies bodies in
                    (* the full three-way: base = last-synced, ours =
                       server, theirs = client. Clean client edits and
                       deletions apply; conflicts LWW-arbitrate; server
                       deletions come back as client_delete_paths. *)
                    let plans = Sync.plan_of base server c in
                    let report = Sync.commit t.store ~client_bodies:cb plans in
                    let r = Sync.classify server c in
                    let bid = Store.current_branch_id t.store in
                    let gen =
                      match Store.current_generation t.store ~branch_id:bid with
                      | Some n -> n
                      | None -> 0L
                    in
                    (* post-apply server subtree = the client's next
                       round's base (the merged state both sides now
                       hold — or will, once the client mirrors the
                       deletions and pulls). *)
                    let merged =
                      match Sync.server_manifest t.store ~prefix with
                      | Some m -> m
                      | None -> []
                    in
                    (* pulls = server-only (client missing) + conflicts
                       the LWW kept on the server (client stale copy must
                       converge to the winner or the pair re-conflicts
                       every round). *)
                    let pull =
                      Sync.normalize
                        (r.server_only
                         @ List.map (fun p -> (p, "")) report.lww_server_kept)
                    in
                    let json =
                      Yojson.Safe.to_string
                        (`Assoc
                           [ ("mode", `String "three-way")
                           ; ("applied", `Int report.applied)
                           ; ("client_only",
                              `Int (List.length r.client_only))
                           ; ("server_only",
                              `Int (List.length r.server_only))
                           ; ("diverge", `Int (List.length r.diverge))
                           ; ("server_only_paths",
                              `List (List.map
                                       (fun (p, _) -> `String p)
                                       (Sync.normalize r.server_only)))
                           ; ("diverge_paths",
                              `List (List.map
                                       (fun (p, _) -> `String p)
                                       (Sync.normalize r.diverge)))
                           ; ("conflicts",
                              `List (List.map
                                       (fun (p, _, _) -> `String p)
                                       (List.rev report.conflicts)))
                           ; ("lww_client_won",
                              `List (List.map (fun p -> `String p)
                                       (List.rev report.lww_client_won)))
                           ; ("lww_server_kept",
                              `List (List.map (fun p -> `String p)
                                       (List.rev report.lww_server_kept)))
                           ; ("client_delete_paths",
                              `List (List.map (fun p -> `String p)
                                       (List.rev report.client_deletes)))
                           ; ("pull_paths",
                              `List (List.map (fun (p, _) -> `String p) pull))
                           ; ("merged_manifest",
                              `List (List.map
                                       (fun (p, h) ->
                                         `List [ `String p; `String h ])
                                       (Sync.normalize merged)))
                           ; ("generation",
                              `String (Int64.to_string gen))
                           ])
                    in
                    (Pricklypear_lisp.Ast.VStr json))
           | _ ->
               ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("sync/push", "args must be (client-manifest client-bodies [base-manifest])", "bad-args"))))))
      | _ ->
          ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("sync/push", "need (client-manifest client-bodies [base-manifest])", "bad-args"))))));


  (* data lens operations (one-tree.borg branching-interaction, 2026-08-23
     make: versioned user-keyed data). fork-data creates a deliberate
     fork of the user's live data into data.l.<lens>.<user>; merge-data
     merges a lens back (LWW v0); abandon-data drops a lens;
     list-data-lenses enumerates the user's lenses. *)
  register_crud "fork-data"
    (fun args ->
      let uname = actor () in
      if uname = "anonymous" then deny_anon "fork-data"
      else
        match args with
        | [ Pricklypear_lisp.Ast.VStr lens ] ->
            Crud.fork_data t.store ~lens ()
        | [ Pricklypear_lisp.Ast.VStr lens;
            Pricklypear_lisp.Ast.VStr ty ] ->
            Crud.fork_data t.store ~lens ~type_name:(Some ty) ()
        | _ ->
            ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("fork-data", "need lens-name [, type-name]", "bad-args"))))));

  register_crud "merge-data"
    (fun args ->
      let uname = actor () in
      if uname = "anonymous" then deny_anon "merge-data"
      else
        match args with
        | [ Pricklypear_lisp.Ast.VStr lens ] ->
            Crud.merge_data_lens t.store ~lens ()
        | _ ->
            ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("merge-data", "need lens-name", "bad-args"))))));

  register_crud "abandon-data"
    (fun args ->
      let uname = actor () in
      if uname = "anonymous" then deny_anon "abandon-data"
      else
        match args with
        | [ Pricklypear_lisp.Ast.VStr lens ] ->
            Crud.abandon_data_lens t.store ~lens ()
        | _ ->
            ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("abandon-data", "need lens-name", "bad-args"))))));

  register_crud "list-data-lenses"
    (fun _args ->
      let uname = actor () in
      if uname = "anonymous" then deny_anon "list-data-lenses"
      else Crud.list_data_lenses t.store ());
  register_crud "tree/upsert"
    (fun args ->
      let uname = actor () in
      if uname = "anonymous" then deny_anon "tree/upsert"
      else
        match args with
        | [ Pricklypear_lisp.Ast.VStr path
          ; Pricklypear_lisp.Ast.VStr kind
          ; Pricklypear_lisp.Ast.VStr type_name
          ; Pricklypear_lisp.Ast.VStr body_json
          ] ->
            Crud.tree_upsert t.store ~path ~kind ~type_name ~body_json
              ~author:uname
        | _ ->
            ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("tree/upsert", "need path kind type body-json", "bad-args"))))));

  register_crud "tree/delete"
    (fun args ->
      let uname = actor () in
      if uname = "anonymous" then deny_anon "tree/delete"
      else
        match args with
        | [ Pricklypear_lisp.Ast.VStr path ] ->
            Crud.tree_delete t.store ~path ~recursive:false
        | [ Pricklypear_lisp.Ast.VStr path; Pricklypear_lisp.Ast.VBool rec_ ] ->
            Crud.tree_delete t.store ~path ~recursive:rec_
        | [ Pricklypear_lisp.Ast.VStr path; Pricklypear_lisp.Ast.VStr flag ]
          when flag = "recursive" || flag = "r" || flag = "#t" ->
            Crud.tree_delete t.store ~path ~recursive:true
        | _ ->
            ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("tree/delete", "need path [, recursive]", "bad-args"))))));

  register_crud "tree/move"
    (fun args ->
      let uname = actor () in
      if uname = "anonymous" then deny_anon "tree/move"
      else
        match args with
        | [ Pricklypear_lisp.Ast.VStr old_path
          ; Pricklypear_lisp.Ast.VStr new_path
          ] ->
            Crud.tree_move t.store ~old_path ~new_path
        | _ ->
            (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("tree/move", "need old-path new-path", "bad-args")))));

  (* Sibling-order prims (one-tree.sibling-order). Rank = canonical base-36
     fractional string over [0-9a-z] (not ending in '0'); NULL rank sorts
     last, path-ordered. Mutations deny_anon; reads open. *)
  register_crud "tree/reorder"
    (fun args ->
      let uname = actor () in
      if uname = "anonymous" then deny_anon "tree/reorder"
      else
        match args with
        | [ Pricklypear_lisp.Ast.VStr path; Pricklypear_lisp.Ast.VStr rank ] ->
            Crud.tree_reorder t.store ~path ~rank
        | _ ->
            (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("tree/reorder", "need path rank (\"\" clears)", "bad-args")))));

  register_crud "tree/move-before"
    (fun args ->
      let uname = actor () in
      if uname = "anonymous" then deny_anon "tree/move-before"
      else
        match args with
        | [ Pricklypear_lisp.Ast.VStr path; Pricklypear_lisp.Ast.VStr anchor ] ->
            Crud.tree_move_before t.store ~path ~anchor
        | _ ->
            (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("tree/move-before", "need path anchor (siblings)", "bad-args")))));

  register_crud "tree/move-after"
    (fun args ->
      let uname = actor () in
      if uname = "anonymous" then deny_anon "tree/move-after"
      else
        match args with
        | [ Pricklypear_lisp.Ast.VStr path; Pricklypear_lisp.Ast.VStr anchor ] ->
            Crud.tree_move_after t.store ~path ~anchor
        | _ ->
            (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("tree/move-after", "need path anchor (siblings)", "bad-args")))));

  register_crud "tree/siblings"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr path ] ->
          Crud.tree_siblings t.store ~path
      | _ ->
          (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("tree/siblings", "need path", "bad-args")))));

  register_crud "tree/next"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr path ] -> Crud.tree_next t.store ~path
      | _ -> (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("tree/next", "need path", "bad-args")))));

  register_crud "tree/prev"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr path ] -> Crud.tree_prev t.store ~path
      | _ -> (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("tree/prev", "need path", "bad-args")))));

  register_crud "tree/insert-child"
    (fun args ->
      let uname = actor () in
      if uname = "anonymous" then deny_anon "tree/insert-child"
      else
        match args with
        | [ Pricklypear_lisp.Ast.VStr parent
          ; Pricklypear_lisp.Ast.VStr kind
          ; Pricklypear_lisp.Ast.VStr type_name
          ; Pricklypear_lisp.Ast.VStr body_json
          ] ->
            Crud.tree_insert_child t.store ~parent ~kind ~type_name
              ~body_json ~anchor:None ~author:uname
        | [ Pricklypear_lisp.Ast.VStr parent
          ; Pricklypear_lisp.Ast.VStr kind
          ; Pricklypear_lisp.Ast.VStr type_name
          ; Pricklypear_lisp.Ast.VStr body_json
          ; Pricklypear_lisp.Ast.VStr anchor
          ] ->
            Crud.tree_insert_child t.store ~parent ~kind ~type_name
              ~body_json ~anchor:(Some anchor) ~author:uname
        | _ ->
            (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("tree/insert-child", "need parent kind type body [, after-anchor]", "bad-args")))));

  (* Tree-edges prims (one-tree.tree-edges). An edge IS a node: kind=link
     at <source-path>.lnk.<uuid>, body {to, kind, label?}. "from" is
     positional; to is stored verbatim and MAY dangle (resolution is a
     read-time act against the reader's lens). Mutations deny_anon. *)
  register_crud "link"
    (fun args ->
      let uname = actor () in
      if uname = "anonymous" then deny_anon "link"
      else
        match args with
        | [ Pricklypear_lisp.Ast.VStr from_p; Pricklypear_lisp.Ast.VStr to_p ] ->
            Crud.link_edge t.store ~from_path:from_p ~to_path:to_p
              ~edge_kind:"ref" ~label:None ~author:uname
        | [ Pricklypear_lisp.Ast.VStr from_p
          ; Pricklypear_lisp.Ast.VStr to_p
          ; Pricklypear_lisp.Ast.VStr edge_kind
          ] ->
            Crud.link_edge t.store ~from_path:from_p ~to_path:to_p
              ~edge_kind ~label:None ~author:uname
        | [ Pricklypear_lisp.Ast.VStr from_p
          ; Pricklypear_lisp.Ast.VStr to_p
          ; Pricklypear_lisp.Ast.VStr edge_kind
          ; Pricklypear_lisp.Ast.VStr label
          ] ->
            Crud.link_edge t.store ~from_path:from_p ~to_path:to_p
              ~edge_kind ~label:(Some label) ~author:uname
        | _ ->
            (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("link", "need from to [, kind [, label]]", "bad-args")))));

  register_crud "unlink"
    (fun args ->
      let uname = actor () in
      if uname = "anonymous" then deny_anon "unlink"
      else
        match args with
        | [ Pricklypear_lisp.Ast.VStr edge_path ] ->
            Crud.unlink_edge t.store ~edge_path
        | _ ->
            (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("unlink", "need edge-path", "bad-args")))));

  register_crud "out-links"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr path ] ->
          Crud.out_links t.store ~path ()
      | [ Pricklypear_lisp.Ast.VStr path; Pricklypear_lisp.Ast.VInt lim ] ->
          Crud.out_links t.store ~path ~limit:(Int64.to_int lim) ()
      | _ ->
          (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("out-links", "need path [, limit]", "bad-args")))));

  register_crud "tree/get-uuid"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr uuid ] ->
          Crud.tree_get_uuid t.store ~uuid
      | _ ->
          (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("tree/get-uuid", "need uuid", "bad-args")))));

  register_crud "tree/find-by"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr kind
        ; Pricklypear_lisp.Ast.VStr field
        ; Pricklypear_lisp.Ast.VStr value
        ] ->
          Crud.tree_find_by t.store ~kind:(Some kind) ~field ~value ()
      | [ Pricklypear_lisp.Ast.VStr kind
        ; Pricklypear_lisp.Ast.VStr field
        ; Pricklypear_lisp.Ast.VStr value
        ; Pricklypear_lisp.Ast.VInt n
        ] ->
          Crud.tree_find_by t.store ~kind:(Some kind) ~field ~value
            ~limit:(Int64.to_int n) ()
      | _ ->
          ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("tree/find-by", "need kind field value [, limit]", "bad-args"))))));

  register_crud "tree/list-kind"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr prefix; Pricklypear_lisp.Ast.VStr kind ] ->
          Crud.tree_list_kind t.store ~prefix ~kind ()
      | [ Pricklypear_lisp.Ast.VStr prefix
        ; Pricklypear_lisp.Ast.VStr kind
        ; Pricklypear_lisp.Ast.VInt n
        ] ->
          Crud.tree_list_kind t.store ~prefix ~kind ~limit:(Int64.to_int n) ()
      | _ ->
          ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("tree/list-kind", "need prefix kind [, limit]", "bad-args"))))));

  register_crud "tree/note-index"
    (fun args ->
      match args with
      | [] ->
          Crud.tree_note_index t.store
            ~prefix:(Tree_store.garden_root ~user:(actor ())) ()
      | [ Pricklypear_lisp.Ast.VStr prefix ] ->
          Crud.tree_note_index t.store ~prefix ()
      | [ Pricklypear_lisp.Ast.VStr prefix; Pricklypear_lisp.Ast.VInt n ] ->
          Crud.tree_note_index t.store ~prefix ~limit:(Int64.to_int n) ()
      | _ ->
          ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("tree/note-index", "[] | prefix [, limit]", "bad-args"))))));

  register_crud "tree/folder-index"
    (fun args ->
      match args with
      | [] ->
          Crud.tree_folder_index t.store
            ~prefix:(Tree_store.garden_root ~user:(actor ())) ()
      | [ Pricklypear_lisp.Ast.VStr prefix ] ->
          Crud.tree_folder_index t.store ~prefix ()
      | _ ->
          ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("tree/folder-index", "[] | prefix", "bad-args"))))));

  (* (tree/children-html [prefix]) -> HTML string of notes-tree children.
     OCaml-side so large folders (3k+) do not blow the eval budget.
     Caller wraps with (render-html …) for raw emission. *)
  register_crud "tree/children-html"
    (fun args ->
      let garden = Tree_store.garden_root ~user:(actor ()) in
      let prefix =
        match args with
        | [] -> garden
        | [ Pricklypear_lisp.Ast.VStr p ] -> p
        | _ -> ""
      in
      if prefix = "" then
        ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("tree/children-html", "[] | prefix", "bad-args")))))
      else
        Crud.tree_children_html t.store ~prefix ~garden_root:garden ());

  (* (tree/search-text needle [limit]) or (tree/search-text prefix needle [limit])
     -> list of hit JSON (slug/title/node_id/header_path/matched/rank).
     Default prefix = current garden root. ILIKE over note title/tags/slug and
     heading title/body — notes.search-modal (BORGE-1785605474). *)
  register_crud "tree/search-text"
    (fun args ->
      let run ~prefix ~needle ~limit =
        Crud.tree_search_text t.store ~prefix ~needle ~limit ()
      in
      let garden () = Tree_store.garden_root ~user:(actor ()) in
      match args with
      | [ Pricklypear_lisp.Ast.VStr needle ] ->
          run ~prefix:(garden ()) ~needle ~limit:20
      | [ Pricklypear_lisp.Ast.VStr needle; Pricklypear_lisp.Ast.VInt n ] ->
          run ~prefix:(garden ()) ~needle ~limit:(Int64.to_int n)
      | [ Pricklypear_lisp.Ast.VStr prefix; Pricklypear_lisp.Ast.VStr needle ] ->
          run ~prefix ~needle ~limit:20
      | [ Pricklypear_lisp.Ast.VStr prefix
        ; Pricklypear_lisp.Ast.VStr needle
        ; Pricklypear_lisp.Ast.VInt n
        ] ->
          run ~prefix ~needle ~limit:(Int64.to_int n)
      | _ ->
          ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("tree/search-text", "needle [, limit] | prefix needle [, limit]", "bad-args"))))));

  (* Pure path helpers (no DB). *)
  register_crud "tree/path-label"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr s ] ->
          (Crud.tree_path_label s)
      | _ ->
          (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("tree/path-label", "need string", "bad-args")))));

  (* Kernel path algebra (one-tree.tree-path-prims): one canonical set of
     pure path constructors/accessors, replacing per-package string hacks
     (notes/tree/*, todo/tree/*, tree/breadcrumb-parts). No DB, no auth;
     label encoding on append is identical to tree/path-label except that
     labels which are ALREADY valid ltree labels pass through untouched
     (round-trip: (tree/child (tree/parent p) (tree/leaf p)) = p).
     Invalid paths raise Prim_error; a root path has nil parent and no
     strict ancestors. *)
  register_crud "tree/parent"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr path ] -> Crud.tree_parent path
      | _ ->
          (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("tree/parent", "need path", "bad-args")))));

  register_crud "tree/leaf"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr path ] -> Crud.tree_leaf path
      | _ ->
          (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("tree/leaf", "need path", "bad-args")))));

  register_crud "tree/segments"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr path ] -> Crud.tree_segments path
      | _ ->
          (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("tree/segments", "need path", "bad-args")))));

  register_crud "tree/child"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr path; Pricklypear_lisp.Ast.VStr label ] ->
          Crud.tree_child ~path ~label
      | _ ->
          (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("tree/child", "need path label", "bad-args")))));

  register_crud "tree/ancestors"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr path ] -> Crud.tree_ancestors path
      | _ ->
          (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("tree/ancestors", "need path", "bad-args")))));

  register_crud "tree/garden-root"
    (fun args ->
      match args with
      | [] ->
          (Crud.tree_garden_root ~user:(actor ()))
      | [ Pricklypear_lisp.Ast.VStr user ] ->
          (Crud.tree_garden_root ~user)
      | _ ->
          ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("tree/garden-root", "[] or user", "bad-args"))))));

  (* (list-branches) -> VList of "owner/name" strings *)
  Primitives.register tbl "list-branches"
    (fun _args ->
      let bs = Store.list_branches t.store in
      let names =
        List.map
          (fun (b : Pricklypear_common.Branch.t) ->
            Pricklypear_lisp.Ast.VStr
              (b.Pricklypear_common.Branch.owner ^ "/" ^
               b.Pricklypear_common.Branch.name))
          bs
      in
      (Pricklypear_lisp.Ast.VList names));

  (* (gen-now) -> current tree gen (int). Spec: tree-time.reads-as-of-gen.
     Returns the current branch's generation (migration 017 ancestry-walked
     counter) = the code-path head gen coordinate for this session's branch
     (what a :gen read-as-of coordinate on code paths compares against). A
     write anywhere in the branch's ancestry bumps it, so a descendant sees
     its own writes via inheritance, consistent with how a session resolves
     code-path tree nodes. Unknown/unreachable -> 0 (caller treats as
     "at head / reload"). Pure read, no gen allocation. *)
  Primitives.register tbl "gen-now"
    (fun _args ->
      let bid = Store.current_branch_id t.store in
      let g =
        match Store.current_generation t.store ~branch_id:bid with
        | Some n -> n
        | None -> 0L
      in
      (Pricklypear_lisp.Ast.VInt g));

  (* (gen-read path gen) -> node body at gen, crossed over the same branch
     lens rules head reads use (tree-time.reads-as-of-gen). path may be an
     absolute b.*/data.* path or a RELATIVE logical suffix (kind.name)
     resolved closest-wins through the branch lens chain. gen is the as-of
     coordinate; 0/omitted means head. Returns version JSON (the version_to_json
     surface: id/path/kind/type/body/author/hash/created_at/gen/tombstone) or
     nil when the path is unversioned at that gen. ReadDb cap. *)
  register_crud "gen-read"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr path
        ; Pricklypear_lisp.Ast.VInt g
        ] ->
          Crud.tree_gen_read t.store ~path ~gen:g
      | _ ->
          (raise (Pricklypear_lisp.Ast.Interp_error
                    (Pricklypear_lisp.Ast.Prim_error
                       ("gen-read", "need path gen", "bad-args")))));

  (* (gen-diff path gen-a gen-b) -> node-granular diff: reads both leaves
     via the versioned store (hashes first, walk on mismatch) and returns
     "same" when hash-equal, else a JSON {path,a,b,same,delta}. ReadDb. *)
  register_crud "gen-diff"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr path
        ; Pricklypear_lisp.Ast.VInt ga
        ; Pricklypear_lisp.Ast.VInt gb
        ] ->
          Crud.tree_gen_diff t.store ~path ~gen_a:ga ~gen_b:gb
      | _ ->
          (raise (Pricklypear_lisp.Ast.Interp_error
                    (Pricklypear_lisp.Ast.Prim_error
                       ("gen-diff",
                        "need path gen-a gen-b", "bad-args")))));

  (* (undo path [n]) -> restore value at head-n as a NEW write at head+1
     (restore-by-write, so the undo itself is versioned and redoable).
     Supports datum paths (data-prefixed; the documents' first consumer is notes
     undo). Code paths (b-prefixed) report the code-restore consumer deferral.
     WriteDb cap (it appends a version row). Returns the written path or an
     error string. *)
  register_crud "undo"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr path ] ->
          Crud.tree_undo t.store ~path ()
      | [ Pricklypear_lisp.Ast.VStr path
        ; Pricklypear_lisp.Ast.VInt n
        ] ->
          Crud.tree_undo t.store ~path ~n ()
      | _ ->
          (raise (Pricklypear_lisp.Ast.Interp_error
                    (Pricklypear_lisp.Ast.Prim_error
                       ("undo", "need path [, n]", "bad-args")))));

  (* (code-restore name [n] [owner/branch]) -> restore a code node (fn/type/
     route) at head-n as a NEW write at head+1 (restore-by-write). The
     code-path analogue of (undo) (which is datum-path only): gen-read + write
     the same value back via the code write surface (upsert_fn / upsert_type /
     upsert_route), so the reverse mirror re-populates function_defs /
     type_defs / view_bindings and the restore itself is versioned + redoable
     (the restored value becomes the new head). n default 1 = the state just
     before the latest change. Clean error strings (never a crash). WriteDb
     cap. Consumers: tree-time.consumers — code-restore = gen-read + define. *)
  register_crud "code-restore"
    (fun args ->
      let resolve_branch_id ref_opt =
        match ref_opt with
        | None -> Store.current_branch_id t.store
        | Some branch_ref ->
            let owner, name =
              match String.split_on_char '/' branch_ref with
              | [ o; n ] when o <> "" && n <> "" -> (o, n)
              | _ -> ("", branch_ref)
            in
            (match Store.lookup_branch_id_by_name t.store ~owner ~name with
             | Some id -> id
             | None -> "")
      in
      let err = "code-restore: need name [n] [owner/branch]" in
      let bad_branch ref_opt bid =
        match ref_opt with
        | None -> (Pricklypear_lisp.Ast.VStr err)
        | Some branch_ref ->
            (Pricklypear_lisp.Ast.VStr
               (Printf.sprintf "code-restore: no such branch: %s" branch_ref))
      in
      let default_author = t.manifest.username in
      match args with
      | [ Pricklypear_lisp.Ast.VStr name ] ->
          let bid = resolve_branch_id None in
          if bid = "" then bad_branch None bid
          else
            (Pricklypear_lisp.Ast.VStr
               (Store.code_restore t.store ~branch_id:bid ~name
                  ~author:default_author ()))
      | [ Pricklypear_lisp.Ast.VStr name; Pricklypear_lisp.Ast.VInt n64 ] ->
          let bid = resolve_branch_id None in
          if bid = "" then bad_branch None bid
          else
            (Pricklypear_lisp.Ast.VStr
               (Store.code_restore t.store ~branch_id:bid ~name
                  ~author:default_author ~n:(Int64.to_int n64) ()))
      | [ Pricklypear_lisp.Ast.VStr name; Pricklypear_lisp.Ast.VStr branch_ref
        ] ->
          let bid = resolve_branch_id (Some branch_ref) in
          if bid = "" then bad_branch (Some branch_ref) bid
          else
            (Pricklypear_lisp.Ast.VStr
               (Store.code_restore t.store ~branch_id:bid ~name
                  ~author:default_author ()))
      | [ Pricklypear_lisp.Ast.VStr name; Pricklypear_lisp.Ast.VInt n64
        ; Pricklypear_lisp.Ast.VStr branch_ref
        ] ->
          let bid = resolve_branch_id (Some branch_ref) in
          if bid = "" then bad_branch (Some branch_ref) bid
          else
            (Pricklypear_lisp.Ast.VStr
               (Store.code_restore t.store ~branch_id:bid ~name
                  ~author:default_author ~n:(Int64.to_int n64) ()))
      | _ ->
          (raise (Pricklypear_lisp.Ast.Interp_error
                    (Pricklypear_lisp.Ast.Prim_error
                       ("code-restore", err, "bad-args")))));

  (* (list-usernames) -> VList of VStr; any authenticated user.
     Usernames are public identifiers on the box (multi-user.chat DMs). *)
  Primitives.register tbl "list-usernames"
    (fun _args ->
      if t.manifest.username = "anonymous" then
        (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("list-usernames", "denied for anonymous", "denied"))))
      else
        let users = Auth_store.list_users t.store in
        let names =
          List.map
            (fun (u : Auth_store.user) ->
              Pricklypear_lisp.Ast.VStr u.Auth_store.username)
            users
        in
        (Pricklypear_lisp.Ast.VList names));

  (* Admin gating for Lisp auth surface (auth.user-management /
     auth.bootstrap-admin). Prefer the per-request snapshot username
     and re-check users.is_admin in PG so a concurrent attach cannot
     flip t.is_admin under an in-flight /api/eval. *)
  let caller_username () =
    match Request_context.get () with
    | Some c -> (Request_context.manifest c).username
    | None -> t.manifest.username
  in
  let require_admin (op : string) (k : unit -> Pricklypear_lisp.Ast.value)
    : Pricklypear_lisp.Ast.value =
    let uname = caller_username () in
    if uname = "anonymous" then
      (Pricklypear_lisp.Ast.VStr (op ^ ": denied for anonymous"))
    else
      let u = Auth_store.get_user_by_username t.store uname in
      match u with
      | Some u when u.Auth_store.is_admin -> k ()
      | _ ->
          (Pricklypear_lisp.Ast.VStr (op ^ ": denied: admin only"))
  in

  (* (is-admin?) -> VBool. True iff the calling identity has users.is_admin.
     Option 3 (BORGE-1785213238): reads Auth_store directly — never Env.lookup
     of the name, so a user-defined shadow cannot grant admin. *)
  Primitives.register_server_only tbl "is-admin?"
    (fun _args ->
      let uname = caller_username () in
      if uname = "anonymous" then
        (Pricklypear_lisp.Ast.VBool false)
      else
        let u = Auth_store.get_user_by_username t.store uname in
        match u with
        | Some u -> (Pricklypear_lisp.Ast.VBool u.Auth_store.is_admin)
        | None -> (Pricklypear_lisp.Ast.VBool false));

  (* ---- tree-time.gc (5.3): retention worker prims (admin-gated) ----------
     Policy as HABITAT DATA: retention-policy-set / retention-delete-policy /
     retention-policies manage tree_retention_policy (migration 022);
     retention-run drives the worker (rollup + DROP PARTITION) per path class.
     Administrative surface — it controls whole-habitat storage/retention. *)
  let retention_admin op k =
    if t.manifest.username = "anonymous" then
      (Pricklypear_lisp.Ast.VStr (op ^ ": denied for anonymous"))
    else
      require_admin op (fun () -> k ())
  in
  (* (retention-policy-set path_class keep_windows) — whole-class policy.
     (retention-policy-set path_class type_name keep_windows) — per-type
     (recorded for future finer-grained work; the shared gen-window floor is
     still whole-class). *) 
  Primitives.register_server_only tbl "retention-policy-set"
    (fun args ->
      retention_admin "retention-policy-set" (fun () ->
          match args with
          | [ Pricklypear_lisp.Ast.VStr pc; Pricklypear_lisp.Ast.VInt kw ] ->
              Crud.retention_set_policy t.store ~path_class:pc
                ~type_name:None ~keep_windows:kw ()
          | [ Pricklypear_lisp.Ast.VStr pc; Pricklypear_lisp.Ast.VStr tn
            ; Pricklypear_lisp.Ast.VInt kw
            ] ->
              Crud.retention_set_policy t.store ~path_class:pc
                ~type_name:(Some tn) ~keep_windows:kw ()
          | _ ->
              (raise
                 (Pricklypear_lisp.Ast.Interp_error
                    (Pricklypear_lisp.Ast.Prim_error
                       ("retention-policy-set",
                        "need path_class keep_windows [type_name]",
                        "bad-args"))))));
  (* (retention-delete-policy path_class [type_name]) *) 
  Primitives.register_server_only tbl "retention-delete-policy"
    (fun args ->
      retention_admin "retention-delete-policy" (fun () ->
          match args with
          | [ Pricklypear_lisp.Ast.VStr pc ] ->
              Crud.retention_delete_policy t.store ~path_class:pc
                ~type_name:None ()
          | [ Pricklypear_lisp.Ast.VStr pc; Pricklypear_lisp.Ast.VStr tn ] ->
              Crud.retention_delete_policy t.store ~path_class:pc
                ~type_name:(Some tn) ()
          | _ ->
              (raise
                 (Pricklypear_lisp.Ast.Interp_error
                    (Pricklypear_lisp.Ast.Prim_error
                       ("retention-delete-policy", "need path_class [type_name]",
                        "bad-args"))))));
  (* (retention-policies) -> VList of policy JSON *) 
  Primitives.register_server_only tbl "retention-policies"
    (fun args ->
      retention_admin "retention-policies" (fun () ->
          match args with
          | [] -> Crud.retention_policies t.store ()
          | _ ->
              (raise
                 (Pricklypear_lisp.Ast.Interp_error
                    (Pricklypear_lisp.Ast.Prim_error
                       ("retention-policies", "need no args", "bad-args"))))));
  (* (retention-run [path_class]) -> VList of (path_class, dropped, ckptd) JSON.
     Runs only for path classes with an explicit whole-class policy; otherwise
     a keep-everything no-op. *) 
  Primitives.register_server_only tbl "retention-run"
    (fun args ->
      retention_admin "retention-run" (fun () ->
          match args with
          | [] -> Crud.retention_run t.store ~path_class:None ()
          | [ Pricklypear_lisp.Ast.VStr pc ] ->
              Crud.retention_run t.store ~path_class:(Some pc) ()
          | _ ->
              (raise
                 (Pricklypear_lisp.Ast.Interp_error
                    (Pricklypear_lisp.Ast.Prim_error
                       ("retention-run", "need [path_class]", "bad-args"))))));

  (* (deftype 'name 'field1 ...) — admin only (BORGE-1785213343).
     Schema authority lives with main/admin; garden authors must not
     overwrite type_defs. Anonymous still raises (deny_anon). *)
  register_crud "deftype"
    (fun args ->
      let uname = caller_username () in
      if uname = "anonymous" then deny_anon "deftype"
      else
        require_admin "deftype" (fun () ->
            Crud.deftype t.store ~args ~author:uname));

  (* (create-user username password [is-admin]) -> VStr new-user-id | error VStr.
     Admin only. Mirrors POST /admin/users action=create (create + set password
     + auth_log admin_create_user). Optional third arg: bool / truthy str/int. *)
  Primitives.register_server_only tbl "create-user"
    (fun args ->
      require_admin "create-user" (fun () ->
        let parse_is_admin = function
          | Pricklypear_lisp.Ast.VBool b -> b
          | Pricklypear_lisp.Ast.VStr s ->
              (match String.lowercase_ascii (String.trim s) with
               | "1" | "true" | "yes" | "on" -> true
               | _ -> false)
          | Pricklypear_lisp.Ast.VInt n -> n <> 0L
          | _ -> false
        in
        let run ~username ~password ~is_admin =
          if String.trim username = "" || password = "" then
            ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("create-user", "username and password required", "bad-args")))))
          else
            let created =
              Auth_store.create_user t.store ~username ~is_admin
            in
            match created with
            | Error e ->
                ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("create-user", e, "failed")))))
            | Ok user ->
                let pw =
                  Auth_store.set_password t.store
                    ~user_id:user.Auth_store.id ~password
                in
                (match pw with
                 | Error e ->
                     ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("create-user", "password: " ^ e, "failed")))))
                 | Ok () ->
                     let () =
                       Auth_store.log_auth t.store
                         ~user_id:user.Auth_store.id
                         ~kind:"admin_create_user"
                         ~success:true ()
                     in
                     (Pricklypear_lisp.Ast.VStr user.Auth_store.id))
        in
        match args with
        | [ Pricklypear_lisp.Ast.VStr username;
            Pricklypear_lisp.Ast.VStr password ] ->
            run ~username ~password ~is_admin:false
        | [ Pricklypear_lisp.Ast.VStr username;
            Pricklypear_lisp.Ast.VStr password;
            flag ] ->
            run ~username ~password ~is_admin:(parse_is_admin flag)
        | _ ->
            ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("create-user", "need username password [is-admin]", "bad-args")))))));

  (* (create-Agent_tools.agent parent-username) -> VStr Agent_tools.agent-user-id. Admin only. *)
  Primitives.register tbl "create-Agent_tools.agent"
    (fun args ->
      require_admin "create-Agent_tools.agent" (fun () ->
        match args with
        | [ Pricklypear_lisp.Ast.VStr parent ] ->
            let r = Auth_store.create_agent t.store ~parent_username:parent in
            (match r with
             | Error e ->
                 (Pricklypear_lisp.Ast.VStr ("create-Agent_tools.agent: " ^ e))
             | Ok u ->
                 (Pricklypear_lisp.Ast.VStr u.Auth_store.id))
        | _ ->
            (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("prim", "create-Agent_tools.agent: need parent-username", "bad-args"))))));

  (* (issue-token [label]) -> VStr raw-token (returned ONCE). Caller's own user. *)
  (* (issue-token user-id [label]) -> VStr raw token. Admin only when user-id given. *)
  Primitives.register tbl "issue-token"
    (fun args ->
      let caller = caller_username () in
      let parse_label = function
        | Pricklypear_lisp.Ast.VStr s when s <> "" -> Some s
        | _ -> None
      in
      (match args with
       | [] ->
           (* issue for the caller themselves, no label *)
           let u = Auth_store.get_user_by_username t.store caller in
           (match u with
            | None ->
                raise (Pricklypear_lisp.Ast.Interp_error
                         (Pricklypear_lisp.Ast.Prim_error
                            ("issue-token", "no such user", "not-found")))
            | Some user ->
                let r = Auth_store.issue_token t.store ~user_id:user.id () in
                (match r with
                 | Error e ->
                     raise (Pricklypear_lisp.Ast.Interp_error
                              (Pricklypear_lisp.Ast.Prim_error
                                 ("issue-token", e, "failed")))
                 | Ok raw -> (Pricklypear_lisp.Ast.VStr raw)))
       | [ label ] ->
           (* issue for the caller themselves, with label *)
           let u = Auth_store.get_user_by_username t.store caller in
           (match u with
            | None ->
                raise (Pricklypear_lisp.Ast.Interp_error
                         (Pricklypear_lisp.Ast.Prim_error
                            ("issue-token", "no such user", "not-found")))
            | Some user ->
                let r = Auth_store.issue_token t.store ~user_id:user.id ~label:(parse_label label) () in
                (match r with
                 | Error e ->
                     raise (Pricklypear_lisp.Ast.Interp_error
                              (Pricklypear_lisp.Ast.Prim_error
                                 ("issue-token", e, "failed")))
                 | Ok raw -> (Pricklypear_lisp.Ast.VStr raw)))
       | [ Pricklypear_lisp.Ast.VStr uid; label ] ->
           require_admin "issue-token" (fun () ->
             let r = Auth_store.issue_token t.store ~user_id:uid ~label:(parse_label label) () in
             (match r with
              | Error e ->
                  raise (Pricklypear_lisp.Ast.Interp_error
                           (Pricklypear_lisp.Ast.Prim_error
                              ("issue-token", e, "failed")))
              | Ok raw -> (Pricklypear_lisp.Ast.VStr raw)))
       | _ -> (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("issue-token", "need [label] (own) or user-id label (admin)", "bad-args"))))));

  (* (list-tokens [user-id]) -> VList of (id label last4 created last_used). *)
  Primitives.register tbl "list-tokens"
    (fun args ->
      let uname = caller_username () in
      let caller_opt = Auth_store.get_user_by_username t.store uname in
      let caller_id =
        match caller_opt with Some x -> x.Auth_store.id | None -> ""
      in
      let is_admin =
        match caller_opt with Some x -> x.Auth_store.is_admin | None -> false
      in
      let target_uid =
        match args with
        | [ Pricklypear_lisp.Ast.VStr uid ] when uid <> "" -> uid
        | _ -> caller_id
      in
      if not is_admin && target_uid <> caller_id then
        (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("list-tokens", "denied: admin only", "denied"))))
      else if target_uid = "" then
        (raise (Pricklypear_lisp.Ast.Interp_error
                 (Pricklypear_lisp.Ast.Prim_error
                    ("list-tokens", "no such user", "not-found"))))
      else
        let metas = Auth_store.list_tokens t.store ~user_id:target_uid in
        let rows =
          List.map
            (fun (m : Auth_store.token_meta) ->
              Pricklypear_lisp.Ast.VList
                [ Pricklypear_lisp.Ast.VStr m.id;
                  Pricklypear_lisp.Ast.VStr (Option.value m.label ~default:"");
                  Pricklypear_lisp.Ast.VStr m.token_last4;
                  Pricklypear_lisp.Ast.VStr m.created_at;
                  Pricklypear_lisp.Ast.VStr (Option.value m.last_used_at ~default:"") ])
            metas
        in
        (Pricklypear_lisp.Ast.VList rows));

  (* (revoke-token credential-id) -> VStr ok|error. Caller may revoke own; admin any. *)
  Primitives.register tbl "revoke-token"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr cid ] when cid <> "" ->
          let r = Auth_store.revoke_token t.store ~credential_id:cid in
          (match r with
           | Error e ->
               raise (Pricklypear_lisp.Ast.Interp_error
                        (Pricklypear_lisp.Ast.Prim_error
                           ("revoke-token", e, "failed")))
           | Ok () -> (Pricklypear_lisp.Ast.VStr "ok"))
      | _ -> (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("revoke-token", "need credential-id", "bad-args")))));

  (* (rotate-token credential-id [label]) -> VStr new-raw-token. Admin: rotates any;
     non-admin rotates own (credential must belong to caller). Returns new raw token ONCE. *)
  Primitives.register tbl "rotate-token"
    (fun args ->
      let uname = caller_username () in
      (match args with
       | [ Pricklypear_lisp.Ast.VStr cid ] when cid <> "" ->
           (* Verify the credential belongs to the caller (or admin). *)
           let caller = Auth_store.get_user_by_username t.store uname in
           let caller_id = match caller with Some x -> x.Auth_store.id | None -> "" in
           let is_admin = match caller with Some x -> x.Auth_store.is_admin | None -> false in
           let metas = Auth_store.list_tokens t.store ~user_id:caller_id in
           let owns = List.exists (fun (m : Auth_store.token_meta) -> m.id = cid) metas in
           if not is_admin && not owns then
             (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("rotate-token", "denied: not your token", "denied"))))
           else
             let r = Auth_store.rotate_token t.store ~credential_id:cid ~user_id:caller_id () in
             (match r with
              | Error e ->
                  raise (Pricklypear_lisp.Ast.Interp_error
                           (Pricklypear_lisp.Ast.Prim_error
                              ("rotate-token", e, "failed")))
              | Ok raw -> (Pricklypear_lisp.Ast.VStr raw))
       | _ -> (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("rotate-token", "need credential-id", "bad-args"))))));

  (* (set-allow-Agent_tools.agent-promote bool) -> VStr ok|error. Sets the CALLER's own toggle. *)
  Primitives.register tbl "set-allow-Agent_tools.agent-promote"
    (fun args ->
      let parse_bool = function
        | Pricklypear_lisp.Ast.VBool b -> b
        | Pricklypear_lisp.Ast.VStr s ->
            (match String.lowercase_ascii (String.trim s) with
             | "1" | "true" | "yes" | "on" -> true
             | _ -> false)
        | Pricklypear_lisp.Ast.VInt n -> n <> 0L
        | _ -> false
      in
      let caller = caller_username () in
      if caller = "anonymous" then
        (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("prim", "set-allow-Agent_tools.agent-promote: denied for anonymous", "denied"))))
      else
        (match args with
         | [ flag ] ->
             let u = Auth_store.get_user_by_username t.store caller in
             (match u with
              | None ->
                  raise (Pricklypear_lisp.Ast.Interp_error
                           (Pricklypear_lisp.Ast.Prim_error
                              ("prim", "set-allow-Agent_tools.agent-promote: no such user", "not-found")))
              | Some user ->
                  let v = parse_bool flag in
                  let r = Auth_store.set_allow_agent_promote t.store ~user_id:user.id ~value:v in
                  (match r with
                   | Error e ->
                       raise (Pricklypear_lisp.Ast.Interp_error
                                (Pricklypear_lisp.Ast.Prim_error
                                   ("prim", "set-allow-Agent_tools.agent-promote: " ^ e, "failed")))
                   | Ok () -> (Pricklypear_lisp.Ast.VStr "ok")))
         | _ -> (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("prim", "set-allow-Agent_tools.agent-promote: need bool", "bad-args"))))));

  (* (promote [name...]) -> VStr count|error. Agent: promote own defs (or named)
     into parent's personal. Human: denied (use promote-to-main admin path). *)
  Primitives.register tbl "promote"
    (fun args ->
      let names =
        List.filter_map
          (fun a -> match a with Pricklypear_lisp.Ast.VStr s -> Some s | _ -> None)
          args
      in
      let r = promote_to_personal t ~names in
      (match r with
       | Error e -> ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("promote", e, "failed")))))
       | Ok n -> (Pricklypear_lisp.Ast.VStr (string_of_int n))));

  (* (promote-to-main source-ref name...) | (promote-to-main source-ref (list name...))
     Admin-only selective upsert onto system/main. branching.promote / land. *)
  Primitives.register tbl "promote-to-main"
    (fun args ->
      let names_of_val = function
        | Pricklypear_lisp.Ast.VStr s -> [ s ]
        | Pricklypear_lisp.Ast.VList xs ->
            List.filter_map
              (function
                | Pricklypear_lisp.Ast.VStr s -> Some s
                | Pricklypear_lisp.Ast.VSymbol s -> Some s
                | _ -> None)
              xs
        | Pricklypear_lisp.Ast.VSymbol s -> [ s ]
        | _ -> []
      in
      match args with
      | [] ->
          ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("promote-to-main", "need source-ref and name(s)", "bad-args")))))
      | Pricklypear_lisp.Ast.VStr source_ref :: rest ->
          let names = List.concat_map names_of_val rest in
          if names = [] then
            ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("promote-to-main", "no names selected", "bad-args")))))
          else if
            (not t.is_admin)
            && t.manifest.username <> "admin"
            && t.manifest.username <> "system"
          then
            ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("promote-to-main", "requires admin", "denied")))))
          else begin
            let owner, name =
              match String.split_on_char '/' source_ref with
              | [ o; n ] when o <> "" && n <> "" -> (o, n)
              | _ -> ("", source_ref)
            in
            match Store.lookup_branch_id_by_name t.store ~owner ~name with
            | None ->
                ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("promote-to-main", "no such source branch: " ^ source_ref, "not-found")))))
            | Some source_id ->
                let n =
                  Store.promote_functions t.store ~source_id
                    ~target_id:Store.system_main_id ~names
                in
                (if Store.current_branch_id t.store = Store.system_main_id then
                   rehydrate_fn t);
                (Pricklypear_lisp.Ast.VStr
                   (Printf.sprintf "promoted %d → system/main" n))
          end
      | _ ->
          ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("promote-to-main", "need source-ref string first", "bad-args"))))));

  (* (current-branch) -> VStr *)
  Primitives.register_sync tbl "current-branch"
    (fun _ ->
      let branch =
        match Request_context.get () with
        | Some c -> (Request_context.manifest c).branch_name
        | None -> t.manifest.branch_name
      in
      Pricklypear_lisp.Ast.VStr branch);

  (* Durable agent sessions (ai-authoring.agent-portal). *)
  Primitives.register tbl "agent-session-new"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr seed ] ->
          Agent_tools.agent_session_new t ~seed ~tools:true ()
      | [ Pricklypear_lisp.Ast.VStr seed; Pricklypear_lisp.Ast.VBool tools ] ->
          Agent_tools.agent_session_new t ~seed ~tools ()
      | _ ->
          ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("agent-session-new", "need seed-str [, tools-bool]", "bad-args"))))));

  Primitives.register tbl "agent-say"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr sid; Pricklypear_lisp.Ast.VStr prompt ] ->
          Agent_tools.agent_say t ~session_id:sid ~prompt ()
      | [ Pricklypear_lisp.Ast.VStr sid; Pricklypear_lisp.Ast.VStr prompt;
          Pricklypear_lisp.Ast.VInt n ] ->
          Agent_tools.agent_say t ~session_id:sid ~prompt ~max_steps:(Int64.to_int n) ()
      | _ ->
          ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("agent-say", "need session-id, prompt [, max-steps]", "bad-args"))))));

  Primitives.register tbl "agent-history"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr sid ] -> Agent_tools.agent_history t ~session_id:sid
      | _ ->
          (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("agent-history", "need session-id", "bad-args")))));

  Primitives.register tbl "agent-sessions"
    (fun args ->
      match args with
      | [] -> Agent_tools.agent_sessions t ()
      | [ Pricklypear_lisp.Ast.VInt n ] ->
          Agent_tools.agent_sessions t ~limit:(Int64.to_int n) ()
      | _ ->
          ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("agent-sessions", "optional limit int", "bad-args"))))));

  Primitives.register tbl "agent-session-delete"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr sid ] ->
          Agent_tools.agent_session_delete t ~session_id:sid
      | _ ->
          (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("agent-session-delete", "need session-id", "bad-args")))));

  (* Temporary aliases: new session + one say. Prefer agent-session-new/say. *)
  Primitives.register tbl "agent"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr seed;
          Pricklypear_lisp.Ast.VStr prompt ] ->
          Agent_tools.agent t ~seed ~prompt ()
      | _ ->
          ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("agent", "need 2 strings (seed, prompt)", "bad-args"))))));

  Primitives.register tbl "agent-task"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr prompt ] ->
          Agent_tools.agent_task t ~prompt ()
      | [ Pricklypear_lisp.Ast.VStr prompt; Pricklypear_lisp.Ast.VInt n ] ->
          Agent_tools.agent_task t ~max_steps:(Int64.to_int n) ~prompt ()
      | _ ->
          ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("agent-task", "need prompt-str [, max-steps int]", "bad-args"))))));

  (* (propose-function 'name "spec text") -> VStr name | error string.
     Calls OpenAI-compat LLM, defines the function on the CURRENT branch
     with author = Agent_tools.agent:<model> (AI provenance). Never merges to system. *)
  Primitives.register tbl "propose-function"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr name;
          Pricklypear_lisp.Ast.VStr spec ] ->
          Agent_tools.propose_function t ~name ~spec
      | _ ->
          ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("propose-function", "bad args (need name-str, spec-str)", "bad-args"))))));

  (* (install-function name body-src) -> install AI-authored body (effect gate). *)
  Primitives.register tbl "install-function"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr name;
          Pricklypear_lisp.Ast.VStr body ] ->
          Agent_tools.install_ai_function t ~name ~body_src:body
      | _ ->
          ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("install-function", "need name-str, body-str", "bad-args"))))));

  (* (function-source name) -> VStr pretty body | VNil *)
  Primitives.register tbl "function-source"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr name ] ->
          let src = Agent_tools.function_source t name in
          (match src with
           | Some s -> (Pricklypear_lisp.Ast.VStr s)
           | None -> Pricklypear_lisp.Ast.VNil)
      | _ ->
          (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("function-source", "need name-str", "bad-args")))));

  (* (find-code needle [limit]) -> VList of name strings.
     Postgres ILIKE over effective function_defs.body::text (branch chain). *)
  Primitives.register tbl "find-code"
    (fun args ->
      let names_of needle limit =
        let ns =
          Store.search_function_bodies t.store ~needle ~limit
        in
        Pricklypear_lisp.Ast.VList
          (List.map (fun n -> Pricklypear_lisp.Ast.VStr n) ns)
      in
      match args with
      | [ Pricklypear_lisp.Ast.VStr needle ] -> names_of needle 200
      | [ Pricklypear_lisp.Ast.VStr needle; Pricklypear_lisp.Ast.VInt n ] ->
          names_of needle (Int64.to_int n)
      | [ Pricklypear_lisp.Ast.VStr needle; Pricklypear_lisp.Ast.VStr n_s ] ->
          (match int_of_string_opt n_s with
           | Some n -> names_of needle n
           | None ->
               ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("find-code", "limit must be int", "bad-args"))))))
      | _ ->
          (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("find-code", "need needle-str [, limit]", "bad-args")))));

  (* (function-doc name) -> VStr docstring | VNil
     Store metadata first; special-form / prim blurbs as fallback. *)
  Primitives.register tbl "function-doc"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr name ] ->
          let doc = Store.lookup_function_doc t.store name in
          (match doc with
           | Some s -> (Pricklypear_lisp.Ast.VStr s)
           | None ->
               (match Pricklypear_lisp.Special_forms.find name with
                | Some e ->
                    (Pricklypear_lisp.Ast.VStr e.summary)
                | None ->
                    let blurb = Primitives.prim_summary name in
                    if blurb = "built-in primitive" then
                      Pricklypear_lisp.Ast.VNil
                    else (Pricklypear_lisp.Ast.VStr blurb)))
      | _ ->
          (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("function-doc", "need name-str", "bad-args")))));

  (* (function-ast name) -> quoted body value tree | VNil.
     Does not eval the body; reifies the stored AST via quote_to_value. *)
  Primitives.register tbl "function-ast"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr name ] ->
          let ast = Agent_tools.function_ast t name in
          (match ast with
           | Some v -> v
           | None -> Pricklypear_lisp.Ast.VNil)
      | _ ->
          (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("function-ast", "need name-str", "bad-args")))));

  (* ===== Jobs prims (borge/jobs.borg) ===== *)
  (* Guest surface: submit-job, job-status, jobs, cancel-job, step,
     schedule, unschedule, schedules. The runner fiber (boot.ml) claims
     and evals; prims just read/submit. with-step is a special form (eval.ml)
     routed through the __step__ prim in session.ml. *)

  let job_caller () =
    match Request_context.get () with
    | Some c -> (Request_context.manifest c).username
    | None -> t.manifest.username
  in
  let job_is_admin () =
    let uname = job_caller () in
    if uname = "anonymous" then false
    else
      match Auth_store.get_user_by_username t.store uname with
      | Some u -> u.Auth_store.is_admin
      | None -> false
  in

  Primitives.register tbl "submit-job"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr fn;
          args_val ] ->
        let args_json =
          match args_val with
          | Pricklypear_lisp.Ast.VStr s -> s
          | _ -> Pricklypear_lisp.Reader.pp_value args_val
        in
        let caller = job_caller () in
        if caller = "anonymous" then
          (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("submit-job", "denied for anonymous", "denied"))))
        else
          let branch =
            match Request_context.get () with
            | Some c -> (Request_context.manifest c).branch_name
            | None -> t.manifest.branch_name
          in
          let now_ms =
            Int64.of_float (Unix.gettimeofday () *. 1000.0)
          in
          let id =
            Jobs.submit ~store:t.store ~fn ~args_json ~branch
              ~submitted_by:caller ~run_at_ms:now_ms ()
          in
          (Pricklypear_lisp.Ast.VStr id)
      | _ ->
        (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("submit-job", "need fn-str, args", "bad-args")))));

  Primitives.register tbl "submit-job-at"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr fn;
          args_val;
          Pricklypear_lisp.Ast.VInt run_at_ms ] ->
        let args_json =
          match args_val with
          | Pricklypear_lisp.Ast.VStr s -> s
          | _ -> Pricklypear_lisp.Reader.pp_value args_val
        in
        let caller = job_caller () in
        if caller = "anonymous" then
          (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("submit-job-at", "denied for anonymous", "denied"))))
        else
          let branch =
            match Request_context.get () with
            | Some c -> (Request_context.manifest c).branch_name
            | None -> t.manifest.branch_name
          in
          let id =
            Jobs.submit ~store:t.store ~fn ~args_json ~branch
              ~submitted_by:caller ~run_at_ms ()
          in
          (Pricklypear_lisp.Ast.VStr id)
      | _ ->
        (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("submit-job-at", "need fn-str, args, run-at-ms", "bad-args")))));

  Primitives.register tbl "job-status"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr id ] ->
        Jobs.get ~store:t.store ~id
          ~caller:(job_caller ()) ~is_admin:(job_is_admin ())
      | _ -> (Pricklypear_lisp.Ast.VNil));

  Primitives.register tbl "jobs"
    (fun _ ->
      Jobs.list ~store:t.store
        ~caller:(job_caller ()) ~is_admin:(job_is_admin ()) ~limit:50 ());

  Primitives.register tbl "cancel-job"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr id ] ->
        Jobs.cancel ~store:t.store ~id
          ~caller:(job_caller ()) ~is_admin:(job_is_admin ())
      | _ ->
        (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("cancel-job", "need job-id-str", "bad-args")))));

  (* (schedule "name" "cron-or-ms" "fn" args) -> upsert a schedule,
     caller-scoped identity + branch. spec is a 5-field cron or an
     every-ms integer. Capability: WriteDb (borge/jobs.borg schedules). *)
  Primitives.register tbl "schedule"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr name;
          Pricklypear_lisp.Ast.VStr spec;
          Pricklypear_lisp.Ast.VStr fn;
          args_val ] ->
        let args_json =
          match args_val with
          | Pricklypear_lisp.Ast.VStr s -> s
          | _ -> Pricklypear_lisp.Reader.pp_value args_val
        in
        let caller = job_caller () in
        if caller = "anonymous" then
          (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("schedule", "denied for anonymous", "denied"))))
        else
          let branch =
            match Request_context.get () with
            | Some c -> (Request_context.manifest c).branch_name
            | None -> t.manifest.branch_name
          in
          Jobs.schedule_upsert ~store:t.store ~name ~spec ~fn ~args_json
            ~branch ~submitted_by:caller ()
      | _ ->
        (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("schedule", "need name-str, spec-str, fn-str, args", "bad-args")))));

  Primitives.register tbl "unschedule"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr name ] ->
        Jobs.unschedule ~store:t.store ~name ()
      | _ ->
        (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("unschedule", "need name-str", "bad-args")))));

  Primitives.register tbl "schedules"
    (fun _ -> Jobs.list_schedules ~store:t.store ());

  (* ===== Forge prims ===== *)

  (* (forge-effects "src-or-name") -> VStr badge string.
     Exposes lisp/lib/effect.ml infer over a parsed source expression
     (or a named binding's body). The SAME analysis install-function
     gates on; this prim merely exposes it for the forge mechanical
     flaggers (capability escalation check). Read-only, pure-ish.
     Spec: borge/forge.borg mechanical-flags. *)
  Primitives.register tbl "forge-effects"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr s ] ->
          (try
             let forms = Pricklypear_lisp.Reader.read_all s in
             (match forms with
              | [] -> (Pricklypear_lisp.Ast.VStr "pure")
              | expr :: _ ->
                  let defs = Store.list_function_names t.store in
                  let known_fn =
                    Pricklypear_lisp.Effect.known_fn_of_bindings
                      (List.map (fun (n, e, _) -> (n, e)) defs)
                  in
                  let badge =
                    Pricklypear_lisp.Effect.EffSet.badge
                      (Pricklypear_lisp.Effect.infer ~known_fn expr)
                  in
                  (Pricklypear_lisp.Ast.VStr badge))
           with exn ->
             raise (Pricklypear_lisp.Ast.Interp_error
                      (Pricklypear_lisp.Ast.Prim_error
                         ("forge-effects",
                          "parse/infer failed: " ^ Printexc.to_string exn,
                          "failed"))))
      | _ ->
          (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("forge-effects", "need src-or-name-str", "bad-args")))));

  (* (package-exports pkg) -> VList of public names (package-surface).
     Resolution: declared registry exports -> kernel_std_exports for
     std/kernel -> shallow heuristic. *)
  let kernel_std_exports =
    [ "list/map"; "list/filter"; "list/foldl"; "reverse"; "append"; "nth"
    ; "null?"; "pair?"; "car"; "cdr"; "cons"; "list"; "count"
    ; "string-append"; "string-length"; "substring"; "string-eq"
    ; "string-contains?"; "string-trim"; "html-escape"; "as-str"; "str"
    ; "string-join"
    ; "string<?"; "string<=?"; "string>?"; "string>=?"
    ; "symbol?"; "string?"; "symbol=?"; "symbol->string"; "string->symbol"
    ; "not"; "type-of"; "read-string"; "read-all-string"; "function-ast"
    ; "function-source"; "function-doc"; "apropos"; "apropos-docs"
    ; "Agent_tools.describe"; "list-packages"; "package-bindings"; "package-exports"
    ; "packages"; "package-enable"; "package-disable"; "prefix-of"
    ]
  in
  Primitives.register tbl "package-exports"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr pkg ] ->
          let pkg = String.lowercase_ascii (String.trim pkg) in
          if pkg = "std" || pkg = "kernel" || pkg = "(kernel)" then
            (Pricklypear_lisp.Ast.VList
                 (List.map
                    (fun n -> Pricklypear_lisp.Ast.VStr n)
                    kernel_std_exports))
          else begin
            let reg = Store.lookup_package t.store pkg in
            match reg with
            | Some r when r.exports <> [] ->
                (Pricklypear_lisp.Ast.VList
                     (List.map
                        (fun n -> Pricklypear_lisp.Ast.VStr n)
                        r.exports))
            | _ ->
                let names = Agent_tools.package_export_names t pkg in
                (Pricklypear_lisp.Ast.VList
                     (List.map (fun n -> Pricklypear_lisp.Ast.VStr n) names))
          end
      | _ ->
          (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("package-exports", "need package-str", "bad-args")))));

  (* (package-bindings pkg) -> VList of name strings (prefix pkg/ any depth) *)
  Primitives.register tbl "package-bindings"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr pkg ] ->
          let names = Agent_tools.package_binding_names t pkg in
          (Pricklypear_lisp.Ast.VList
               (List.map (fun n -> Pricklypear_lisp.Ast.VStr n) names))
      | _ ->
          (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("package-bindings", "need package-str", "bad-args")))));

  (* (list-packages) -> VList of top-level package prefix strings *)
  Primitives.register tbl "list-packages"
    (fun args ->
      match args with
      | [] | [ Pricklypear_lisp.Ast.VNil ] ->
          let names = Agent_tools.list_package_names t in
          (Pricklypear_lisp.Ast.VList
               (List.map (fun n -> Pricklypear_lisp.Ast.VStr n) names))
      | _ ->
          ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("list-packages", "takes no args", "bad-args"))))));

  (* (packages) -> VList of (name enabled exports-count) rows from registry +
     prefix-discovered packages (enabled=true, exports=0 if unregistered). *)
  Primitives.register tbl "packages"
    (fun args ->
      match args with
      | [] | [ Pricklypear_lisp.Ast.VNil ] ->
          let reg = Store.list_packages t.store in
          let prefixes = Agent_tools.list_package_names t in
          let seen = Hashtbl.create 32 in
          let rows = ref [] in
          List.iter
            (fun (r : Store.package_row) ->
              Hashtbl.replace seen r.name ();
              rows :=
                Pricklypear_lisp.Ast.VList
                  [ Pricklypear_lisp.Ast.VStr r.name
                  ; Pricklypear_lisp.Ast.VBool r.enabled
                  ; Pricklypear_lisp.Ast.VInt
                      (Int64.of_int (List.length r.exports))
                  ]
                :: !rows)
            reg;
          List.iter
            (fun p ->
              if not (Hashtbl.mem seen p) then
                rows :=
                  Pricklypear_lisp.Ast.VList
                    [ Pricklypear_lisp.Ast.VStr p
                    ; Pricklypear_lisp.Ast.VBool true
                    ; Pricklypear_lisp.Ast.VInt 0L
                    ]
                  :: !rows)
            prefixes;
          let sorted =
            List.sort
              (fun a b ->
                match a, b with
                | Pricklypear_lisp.Ast.VList (Pricklypear_lisp.Ast.VStr na :: _),
                  Pricklypear_lisp.Ast.VList (Pricklypear_lisp.Ast.VStr nb :: _) ->
                    String.compare na nb
                | _ -> 0)
              !rows
          in
          (Pricklypear_lisp.Ast.VList sorted)
      | _ ->
          ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("packages", "takes no args", "bad-args"))))));

  let protected_pkg name =
    let n = String.lowercase_ascii (String.trim name) in
    n = "std" || n = "kernel" || n = "(kernel)"
  in

  (* (package-enable name) — flip enabled=true on current branch + rehydrate *)
  Primitives.register tbl "package-enable"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr name ]
      | [ Pricklypear_lisp.Ast.VSymbol name ] ->
          let name = String.trim name in
          if name = "" then
            ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("package-enable", "empty name", "bad-args")))))
          else if protected_pkg name then
            ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("package-enable", name ^ " is always enabled", "denied")))))
          else if t.manifest.username = "anonymous" then
            ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("package-enable", "auth required", "denied")))))
          else begin
            let () =
              Store.set_package_enabled t.store ~name ~enabled:true
                ~author:t.manifest.username
            in
            let () = rehydrate_fn t in
            (Pricklypear_lisp.Ast.VStr ("enabled " ^ name))
          end
      | _ ->
          (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("package-enable", "need package-str", "bad-args")))));

  (* (package-disable name) — hide package bindings on current branch *)
  Primitives.register tbl "package-disable"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr name ]
      | [ Pricklypear_lisp.Ast.VSymbol name ] ->
          let name = String.trim name in
          if name = "" then
            ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("package-disable", "empty name", "bad-args")))))
          else if protected_pkg name then
            ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("package-disable", "cannot disable " ^ name, "denied")))))
          else if t.manifest.username = "anonymous" then
            ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("package-disable", "auth required", "denied")))))
          else begin
            let () =
              Store.set_package_enabled t.store ~name ~enabled:false
                ~author:t.manifest.username
            in
            let () = rehydrate_fn t in
            (Pricklypear_lisp.Ast.VStr ("disabled " ^ name))
          end
      | _ ->
          (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("package-disable", "need package-str", "bad-args")))));

  (* (apropos-docs pat) -> VList of (name . doc) pairs *)
  Primitives.register tbl "apropos-docs"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr pat ] ->
          let pairs = Agent_tools.apropos_docs t pat in
          let rows =
            List.map
              (fun (n, d) ->
                Pricklypear_lisp.Ast.VList
                  [ Pricklypear_lisp.Ast.VStr n
                  ; Pricklypear_lisp.Ast.VStr d
                  ])
              pairs
          in
          (Pricklypear_lisp.Ast.VList rows)
      | _ ->
          (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("apropos-docs", "need pattern-str", "bad-args")))));

  (* (Agent_tools.describe name-or-pkg) -> VStr selective reveal text *)
  Primitives.register tbl "describe"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr needle ] ->
          let s = Agent_tools.describe t needle in
          (Pricklypear_lisp.Ast.VStr s)
      | _ ->
          (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("prim", "Agent_tools.describe: need name-or-package-str", "bad-args")))));

  (* (export-package pkg) -> VStr lib.pp text *)
  Primitives.register tbl "export-package"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr pkg ] ->
          let text = Agent_tools.export_package t pkg in
          (Pricklypear_lisp.Ast.VStr text)
      | _ ->
          (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("export-package", "need package-str", "bad-args")))));

  (* SSH keys (auth.user-ssh-repl phase 1) *)
  Primitives.register tbl "add-ssh-key"
    (fun args ->
      if t.manifest.username = "anonymous" then
        (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("add-ssh-key", "denied for anonymous", "denied"))))
      else
        match args with
        | [ Pricklypear_lisp.Ast.VStr openssh ] ->
            let r =
              Auth_store.add_ssh_key t.store ~user_id:t.manifest.user_id
                ~openssh ()
            in
            (match r with
             | Ok m ->
                 (Pricklypear_lisp.Ast.VStr m.Auth_store.fingerprint)
             | Error e ->
                 ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("add-ssh-key", e, "failed"))))))
        | [ Pricklypear_lisp.Ast.VStr openssh; Pricklypear_lisp.Ast.VStr label ] ->
            let r =
              Auth_store.add_ssh_key t.store ~user_id:t.manifest.user_id
                ~openssh ~label:(Some label) ()
            in
            (match r with
             | Ok m ->
                 (Pricklypear_lisp.Ast.VStr m.Auth_store.fingerprint)
             | Error e ->
                 ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("add-ssh-key", e, "failed"))))))
        | _ ->
            (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("add-ssh-key", "need openssh-str [, label]", "bad-args")))));

  Primitives.register tbl "list-ssh-keys"
    (fun _args ->
      if t.manifest.username = "anonymous" then
        (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("list-ssh-keys", "denied for anonymous", "denied"))))
      else
        let keys =
          Auth_store.list_ssh_keys t.store ~user_id:t.manifest.user_id
        in
        let vs =
          List.map
            (fun (k : Auth_store.ssh_key_meta) ->
              Pricklypear_lisp.Ast.VStr k.fingerprint)
            keys
        in
        (Pricklypear_lisp.Ast.VList vs));

  Primitives.register tbl "revoke-ssh-key"
    (fun args ->
      if t.manifest.username = "anonymous" then
        (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("revoke-ssh-key", "denied for anonymous", "denied"))))
      else
        match args with
        | [ Pricklypear_lisp.Ast.VStr id_or_fp ] ->
            let r =
              Auth_store.revoke_ssh_key t.store ~user_id:t.manifest.user_id
                ~id_or_fingerprint:id_or_fp
            in
            (match r with
             | Ok () -> Pricklypear_lisp.Ast.VNil
             | Error e ->
                 ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("revoke-ssh-key", e, "failed"))))))
        | _ ->
            (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("revoke-ssh-key", "need fingerprint-or-id", "bad-args")))));

  (* Smoke/admin: resolve openssh pubkey -> username | () *)
  Primitives.register tbl "verify-ssh-key"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr openssh ] ->
          let u = Auth_store.verify_ssh_key t.store ~openssh_or_blob:openssh () in
          (match u with
           | Some user ->
               (Pricklypear_lisp.Ast.VStr user.Auth_store.username)
           | None -> Pricklypear_lisp.Ast.VNil)
      | _ ->
          (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("verify-ssh-key", "need openssh-pubkey-str", "bad-args")))));

  (* Per-user LLM keys (auth.user-llm-keys) *)
  Primitives.register tbl "set-llm-key"
    (fun args ->
      if t.manifest.username = "anonymous" then
        (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("set-llm-key", "denied for anonymous", "denied"))))
      else
        match args with
        | [ Pricklypear_lisp.Ast.VStr key ] ->
            let r =
              Auth_store.set_llm_key t.store ~user_id:t.manifest.user_id
                ~api_key:key ()
            in
            (match r with
             | Ok m ->
                 (Pricklypear_lisp.Ast.VStr ("ok last4=" ^ m.Auth_store.last4))
             | Error e ->
                 ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("set-llm-key", e, "failed"))))))
        | [ Pricklypear_lisp.Ast.VStr key; Pricklypear_lisp.Ast.VStr base ] ->
            let r =
              Auth_store.set_llm_key t.store ~user_id:t.manifest.user_id
                ~api_key:key ~base:(Some base) ()
            in
            (match r with
             | Ok m ->
                 (Pricklypear_lisp.Ast.VStr ("ok last4=" ^ m.Auth_store.last4))
             | Error e ->
                 ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("set-llm-key", e, "failed"))))))
        | [ Pricklypear_lisp.Ast.VStr key; Pricklypear_lisp.Ast.VStr base;
            Pricklypear_lisp.Ast.VStr model ] ->
            let r =
              Auth_store.set_llm_key t.store ~user_id:t.manifest.user_id
                ~api_key:key ~base:(Some base) ~model:(Some model) ()
            in
            (match r with
             | Ok m ->
                 (Pricklypear_lisp.Ast.VStr ("ok last4=" ^ m.Auth_store.last4))
             | Error e ->
                 ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("set-llm-key", e, "failed"))))))
        | _ ->
            (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("set-llm-key", "need key-str [, base] [, model]", "bad-args")))));

  Primitives.register tbl "clear-llm-key"
    (fun _args ->
      if t.manifest.username = "anonymous" then
        (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("clear-llm-key", "denied for anonymous", "denied"))))
      else
        let r =
          Auth_store.clear_llm_key t.store ~user_id:t.manifest.user_id
        in
        match r with
        | Ok () -> Pricklypear_lisp.Ast.VNil
        | Error e ->
            ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("clear-llm-key", e, "failed"))))));

  Primitives.register tbl "llm-key-status"
    (fun _args ->
      if t.manifest.username = "anonymous" then
        (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("llm-key-status", "denied for anonymous", "denied"))))
      else
        let meta =
          Auth_store.get_llm_key_meta t.store ~user_id:t.manifest.user_id
        in
        match meta with
        | None ->
            let env_set = Llm_client.api_key () <> "" in
            (Pricklypear_lisp.Ast.VStr
                 (if env_set then
                    "configured=false env_fallback=true"
                  else "configured=false env_fallback=false"))
        | Some m ->
            let base =
              match m.base with Some b -> " base=" ^ b | None -> ""
            in
            let model =
              match m.model with Some m -> " model=" ^ m | None -> ""
            in
            (Pricklypear_lisp.Ast.VStr
                 (Printf.sprintf "configured=true last4=%s%s%s"
                    m.last4 base model)));

  (* (llm-chat system user [temperature] [max-tokens]) -> VStr content.
     Raw one-shot chat completion — NOT the agentic install loop. Returns
     the model's content string. Used by libs/forge drafters and any
     caller that wants a plain completion. See llm_client.call. *)
  Primitives.register tbl "llm-chat"
    (fun args ->
      let temperature, max_tokens, system, user =
        match args with
        | [ Pricklypear_lisp.Ast.VStr system;
            Pricklypear_lisp.Ast.VStr user ] ->
            (0.2, 2048, system, user)
        | [ Pricklypear_lisp.Ast.VStr system;
            Pricklypear_lisp.Ast.VStr user;
            Pricklypear_lisp.Ast.VFloat t ] ->
            (t, 2048, system, user)
        | [ Pricklypear_lisp.Ast.VStr system;
            Pricklypear_lisp.Ast.VStr user;
            Pricklypear_lisp.Ast.VInt n ] ->
            (0.2, Int64.to_int n, system, user)
        | [ Pricklypear_lisp.Ast.VStr system;
            Pricklypear_lisp.Ast.VStr user;
            Pricklypear_lisp.Ast.VFloat t;
            Pricklypear_lisp.Ast.VInt n ] ->
            (t, Int64.to_int n, system, user)
        | _ ->
            (0.2, 2048, "", "need system-str, user-str")
      in
      let r =
        Llm_client.call ~temperature ~max_tokens ~system ~user ()
      in
      (match r with
       | Ok s -> (Pricklypear_lisp.Ast.VStr s)
       | Error e -> (Pricklypear_lisp.Ast.VStr ("llm-chat: " ^ e))));
        (* lint-exempt-prim-error: forge/llm-error? pattern-matches this error
           prefix (libs/forge lib.pp); raising needs the paired Nopales guard
           change on the live package. Tracked on the habitat issue register. *)

  (* Coding tools: freeze AST rewrites into store *)
  Primitives.register tbl "llm-inner"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr name;
          Pricklypear_lisp.Ast.VStr concept ] ->
          Agent_tools.llm_inner t ~name ~concept
      | _ ->
          (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("llm-inner", "need name-str, concept-str", "bad-args")))));

  Primitives.register tbl "llm-outer"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr name;
          Pricklypear_lisp.Ast.VStr concept ] ->
          Agent_tools.llm_outer t ~name ~concept
      | _ ->
          (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("llm-outer", "need name-str, concept-str", "bad-args")))));

  Primitives.register tbl "llm-rewrite"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr name;
          Pricklypear_lisp.Ast.VStr concept ] ->
          Agent_tools.llm_rewrite t ~mode:"replace" ~name ~concept
      | [ Pricklypear_lisp.Ast.VStr name;
          Pricklypear_lisp.Ast.VStr concept;
          Pricklypear_lisp.Ast.VStr mode ] ->
          Agent_tools.llm_rewrite t ~mode ~name ~concept
      | _ ->
          ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("llm-rewrite", "need name, concept [, mode replace|wrap]", "bad-args"))))));

  Primitives.register tbl "llm-seed"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr requirement ] ->
          Agent_tools.llm_seed t ~requirement ()
      | [ Pricklypear_lisp.Ast.VStr requirement;
          Pricklypear_lisp.Ast.VInt n ] ->
          Agent_tools.llm_seed t ~requirement ~max_steps:(Int64.to_int n) ()
      | _ ->
          ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("llm-seed", "need requirement-str [, max-steps int]", "bad-args"))))));

  (* Ops: structured log tail + analytics summary (product-experience). *)
  Primitives.register tbl "log-tail"
    (fun args ->
      let n, level =
        match args with
        | [] -> 50, None
        | [ Pricklypear_lisp.Ast.VInt n ] -> Int64.to_int n, None
        | [ Pricklypear_lisp.Ast.VStr lv ] -> 50, Ops_log.level_of_string lv
        | [ Pricklypear_lisp.Ast.VInt n; Pricklypear_lisp.Ast.VStr lv ] ->
            Int64.to_int n, Ops_log.level_of_string lv
        | [ Pricklypear_lisp.Ast.VStr lv; Pricklypear_lisp.Ast.VInt n ] ->
            Int64.to_int n, Ops_log.level_of_string lv
        | _ -> 50, None
      in
      let evs = Ops_log.tail ~n ?level () in
      (Pricklypear_lisp.Ast.VList
           (List.map Ops_log.event_to_value evs)));

  Primitives.register tbl "ops/summary"
    (fun _args ->
      let ok, fail = Ops_incidents.count_logins t.store in
      let j = Ops_stats.summary_yojson ~logins_ok:ok ~logins_fail:fail in
      (Pricklypear_lisp.Ast.VStr (Yojson.Safe.to_string j)));

  Primitives.register tbl "ops/incidents"
    (fun _args ->
      let j = Ops_incidents.list_open_incidents t.store in
      (Pricklypear_lisp.Ast.VStr (Yojson.Safe.to_string j)));

  (* (code-history "name") / (code-history "name" "owner/branch")
     → VStr JSON list newest-first. Spec: branching.code-history. *)
  Primitives.register tbl "code-history"
    (fun args ->
      let resolve_branch_id ref_opt =
        match ref_opt with
        | None -> Store.current_branch_id t.store
        | Some branch_ref ->
            let owner, name =
              match String.split_on_char '/' branch_ref with
              | [ o; n ] when o <> "" && n <> "" -> (o, n)
              | _ -> ("", branch_ref)
            in
            (match
               Store.lookup_branch_id_by_name t.store ~owner ~name
             with
             | Some id -> id
             | None -> "")
      in
      match args with
      | [ Pricklypear_lisp.Ast.VStr name ] ->
          let bid = resolve_branch_id None in
          if bid = "" then
            (Pricklypear_lisp.Ast.VStr "[]")
          else
            (Pricklypear_lisp.Ast.VStr
               (Store.list_code_history_json t.store ~branch_id:bid ~name))
      | [ Pricklypear_lisp.Ast.VStr name
        ; Pricklypear_lisp.Ast.VStr branch_ref ] ->
          let bid = resolve_branch_id (Some branch_ref) in
          if bid = "" then
            (Pricklypear_lisp.Ast.VStr
               (Printf.sprintf
                  "code-history: no such branch: %s" branch_ref))
          else
            (Pricklypear_lisp.Ast.VStr
               (Store.list_code_history_json t.store ~branch_id:bid ~name))
      | _ ->
          ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("code-history", "need name [owner/branch]", "bad-args"))))));

  (* (code-diff "name" n) → VStr text diff of current vs nth-newest history. *)
  Primitives.register tbl "code-diff"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr name; Pricklypear_lisp.Ast.VInt n64 ] ->
          let n = Int64.to_int n64 in
          let bid = Store.current_branch_id t.store in
          (match Store.code_diff t.store ~branch_id:bid ~name ~n with
           | Ok s -> (Pricklypear_lisp.Ast.VStr s)
           | Error e -> (Pricklypear_lisp.Ast.VStr e))
      | [ Pricklypear_lisp.Ast.VStr name
        ; Pricklypear_lisp.Ast.VStr n_s ] ->
          (match int_of_string_opt n_s with
           | None ->
               ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("code-diff", "n must be int", "bad-args")))))
           | Some n ->
               let bid = Store.current_branch_id t.store in
               (match Store.code_diff t.store ~branch_id:bid ~name ~n with
                | Ok s -> (Pricklypear_lisp.Ast.VStr s)
                | Error e -> (Pricklypear_lisp.Ast.VStr e)))
      | _ ->
          (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("code-diff", "need name n", "bad-args")))));

  (* (diff source target) -> VList of VStr change summaries.
     Spec: branching.diff.  Args: two branch specs (owner/name strings, same
     parsing as checkout/merge/promote).  Returns one summary string per
     divergent row across function_defs + type_defs + view_bindings:
       "<kind>\t<status>\t<name>\t<from-updated>\t<to-updated>"
     Summary level ONLY — the structured AST delta is consumed in-process by
     the /diff page renderer, not surfaced here (a programmatic (diff-ast …)
     prim is explicit v2). *)
  Primitives.register tbl "diff"
    (fun args ->
      let resolve ref_ =
        let owner, name =
          match String.split_on_char '/' ref_ with
          | [ o; n ] when o <> "" && n <> "" -> (o, n)
          | _ -> ("", ref_)
        in
        Store.lookup_branch_id_by_name t.store ~owner ~name
      in
      let to_ref = function
        | Pricklypear_lisp.Ast.VStr s -> Some s
        | Pricklypear_lisp.Ast.VSymbol s -> Some s
        | _ -> None
      in
      match args with
      | [ a; b ] ->
          (match to_ref a, to_ref b with
           | Some from_ref, Some to_ref ->
               (match resolve from_ref, resolve to_ref with
                | Some from_id, Some to_id ->
                    let rows = Store.diff_branches t.store ~from_id ~to_id in
                    let kind_s = function
                      | Store.DiffFn -> "fn"
                      | Store.DiffType -> "type"
                      | Store.DiffRoute -> "route"
                    in
                    let status_s = function
                      | Store.DiffAdded -> "added"
                      | Store.DiffRemoved -> "removed"
                      | Store.DiffModified -> "modified"
                    in
                    let line (r : Store.branch_diff_row) =
                      let fu = Option.value ~default:"-" r.Store.from_updated in
                      let tu = Option.value ~default:"-" r.Store.to_updated in
                      Printf.sprintf "%s\t%s\t%s\t%s\t%s"
                        (kind_s r.Store.diff_kind) (status_s r.Store.diff_status)
                        r.Store.diff_name fu tu
                    in
                    (Pricklypear_lisp.Ast.VList
                       (List.map (fun r -> Pricklypear_lisp.Ast.VStr (line r)) rows))
                | _ ->
                    (Pricklypear_lisp.Ast.VStr
                       (Printf.sprintf "diff: no such branch pair (%s, %s)"
                          from_ref to_ref)))
           | _ ->
               (raise
                  (Pricklypear_lisp.Ast.Interp_error
                     (Pricklypear_lisp.Ast.Prim_error
                        ("diff", "need source target", "bad-args")))))
      | _ ->
          (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("diff", "need source target", "bad-args")))));
