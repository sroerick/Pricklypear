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
    Primitives.register tbl name (fun args -> fn (coerce_sym_args args))
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
            (name ^ ": denied for anonymous")))
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
      | _ -> (Pricklypear_lisp.Ast.VStr "get: bad args (need type, id)"));

  (* (create 'type "{fields-json}") -> VStr new-id | VStr "create failed" *)
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
            (Pricklypear_lisp.Ast.VStr "create: bad args (need type, fields)"));

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
            (Pricklypear_lisp.Ast.VStr
                 "update: bad args (need type, id, fields)"));

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
            (Pricklypear_lisp.Ast.VStr
                 "update-field: bad args (need type, id, field, value)"));

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
            (Pricklypear_lisp.Ast.VStr "delete: bad args (need type, id)"));

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
      | _ -> (Pricklypear_lisp.Ast.VStr "select-for-update: bad args (need type, id)"));

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
      | _ -> (Pricklypear_lisp.Ast.VStr "rows: bad args (need type[, limit])"));

  (* (find-row 'type "field" "value") -> fields JSON | () *)
  register_crud "find-row"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr type_name;
          Pricklypear_lisp.Ast.VStr field;
          Pricklypear_lisp.Ast.VStr value ] ->
          Crud.find_row t.store ~type_name ~field ~value
      | _ ->
          (Pricklypear_lisp.Ast.VStr
               "find-row: need type, field, value"));

  (* (find-rows 'type "f" "v" … [limit]) -> list of (id . fields) *)
  register_crud "find-rows"
    (fun args ->
      let rec parse_filters acc = function
        | [] -> Ok (List.rev acc, 500)
        | [ Pricklypear_lisp.Ast.VInt n ] ->
            Ok (List.rev acc, Int64.to_int n)
        | Pricklypear_lisp.Ast.VStr f :: Pricklypear_lisp.Ast.VStr v :: rest ->
            parse_filters ((f, v) :: acc) rest
        | _ -> Error "find-rows: bad filter args"
      in
      match args with
      | Pricklypear_lisp.Ast.VStr type_name :: rest ->
          (match parse_filters [] rest with
           | Error e -> (Pricklypear_lisp.Ast.VStr e)
           | Ok (filters, limit) ->
               Crud.find_rows t.store ~type_name ~filters ~limit ())
      | _ ->
          (Pricklypear_lisp.Ast.VStr
               "find-rows: need type and field/value pairs"));

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
          (Pricklypear_lisp.Ast.VStr
               "find-text: need type, field, needle [, limit]"));

  (* (find-random 'type) -> (id . fields) | () *)
  register_crud "find-random"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr type_name ] ->
          Crud.find_random t.store ~type_name
      | _ ->
          (Pricklypear_lisp.Ast.VStr "find-random: need type"));

  (* one-tree P6: typed nodes + path addressing + backlinks *)
  register_crud "node-path"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr type_name;
          Pricklypear_lisp.Ast.VStr id ] ->
          (Crud.node_path ~type_name ~id)
      | _ ->
          (Pricklypear_lisp.Ast.VStr "node-path: need type, id"));

  register_crud "type-fields"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr type_name ] ->
          Crud.type_fields t.store ~type_name
      | _ ->
          (Pricklypear_lisp.Ast.VStr "type-fields: need type"));

  register_crud "backlinks"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr needle ] ->
          Crud.backlinks t.store ~needle ()
      | [ Pricklypear_lisp.Ast.VStr needle;
          Pricklypear_lisp.Ast.VInt n ] ->
          Crud.backlinks t.store ~needle ~limit:(Int64.to_int n) ()
      | _ ->
          (Pricklypear_lisp.Ast.VStr "backlinks: need id-or-path [, limit]"));

  (* Hierarchical tree prims (notes.tree-cutover K0). Direct tree_nodes
     access for folder/note/heading — not kind=datum CRUD. *)
  register_crud "tree/get"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr path ] -> Crud.tree_get t.store ~path
      | _ ->
          (Pricklypear_lisp.Ast.VStr "tree/get: need path"));

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
          (Pricklypear_lisp.Ast.VStr
               "tree/list: need prefix [, depth [, limit]]"));

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
            (Pricklypear_lisp.Ast.VStr
                 "tree/upsert: need path kind type body-json"));

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
            (Pricklypear_lisp.Ast.VStr
                 "tree/delete: need path [, recursive]"));

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
            (Pricklypear_lisp.Ast.VStr "tree/move: need old-path new-path"));

  register_crud "tree/get-uuid"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr uuid ] ->
          Crud.tree_get_uuid t.store ~uuid
      | _ ->
          (Pricklypear_lisp.Ast.VStr "tree/get-uuid: need uuid"));

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
          (Pricklypear_lisp.Ast.VStr
               "tree/find-by: need kind field value [, limit]"));

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
          (Pricklypear_lisp.Ast.VStr
               "tree/list-kind: need prefix kind [, limit]"));

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
          (Pricklypear_lisp.Ast.VStr
               "tree/note-index: [] | prefix [, limit]"));

  (* Pure path helpers (no DB). *)
  register_crud "tree/path-label"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr s ] ->
          (Crud.tree_path_label s)
      | _ ->
          (Pricklypear_lisp.Ast.VStr "tree/path-label: need string"));

  register_crud "tree/garden-root"
    (fun args ->
      match args with
      | [] ->
          (Crud.tree_garden_root ~user:(actor ()))
      | [ Pricklypear_lisp.Ast.VStr user ] ->
          (Crud.tree_garden_root ~user)
      | _ ->
          (Pricklypear_lisp.Ast.VStr "tree/garden-root: [] or user"));

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

  (* storage.media Lisp surface — meta/list/delete + tiny put.
     Bulk Agent_tools.agent pushes use POST /api/media (5 MiB), not eval. *)
  Prims_media.register t tbl;
  Prims_net.register t tbl;

  (* (list-usernames) -> VList of VStr; any authenticated user.
     Usernames are public identifiers on the box (multi-user.chat DMs). *)
  Primitives.register tbl "list-usernames"
    (fun _args ->
      if t.manifest.username = "anonymous" then
        (Pricklypear_lisp.Ast.VStr "list-usernames: denied for anonymous")
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
  Primitives.register tbl "is-admin?"
    (fun _args ->
      let uname = caller_username () in
      if uname = "anonymous" then
        (Pricklypear_lisp.Ast.VBool false)
      else
        let u = Auth_store.get_user_by_username t.store uname in
        match u with
        | Some u -> (Pricklypear_lisp.Ast.VBool u.Auth_store.is_admin)
        | None -> (Pricklypear_lisp.Ast.VBool false));

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
  Primitives.register tbl "create-user"
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
            (Pricklypear_lisp.Ast.VStr
                 "create-user: username and password required")
          else
            let created =
              Auth_store.create_user t.store ~username ~is_admin
            in
            match created with
            | Error e ->
                (Pricklypear_lisp.Ast.VStr ("create-user: " ^ e))
            | Ok user ->
                let pw =
                  Auth_store.set_password t.store
                    ~user_id:user.Auth_store.id ~password
                in
                (match pw with
                 | Error e ->
                     (Pricklypear_lisp.Ast.VStr
                          ("create-user: password: " ^ e))
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
            (Pricklypear_lisp.Ast.VStr
                 "create-user: need username password [is-admin]")));

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
            (Pricklypear_lisp.Ast.VStr "create-Agent_tools.agent: need parent-username")));

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
            | None -> (Pricklypear_lisp.Ast.VStr "issue-token: no such user")
            | Some user ->
                let r = Auth_store.issue_token t.store ~user_id:user.id () in
                (match r with
                 | Error e -> (Pricklypear_lisp.Ast.VStr ("issue-token: " ^ e))
                 | Ok raw -> (Pricklypear_lisp.Ast.VStr raw)))
       | [ label ] ->
           (* issue for the caller themselves, with label *)
           let u = Auth_store.get_user_by_username t.store caller in
           (match u with
            | None -> (Pricklypear_lisp.Ast.VStr "issue-token: no such user")
            | Some user ->
                let r = Auth_store.issue_token t.store ~user_id:user.id ~label:(parse_label label) () in
                (match r with
                 | Error e -> (Pricklypear_lisp.Ast.VStr ("issue-token: " ^ e))
                 | Ok raw -> (Pricklypear_lisp.Ast.VStr raw)))
       | [ Pricklypear_lisp.Ast.VStr uid; label ] ->
           require_admin "issue-token" (fun () ->
             let r = Auth_store.issue_token t.store ~user_id:uid ~label:(parse_label label) () in
             (match r with
              | Error e -> (Pricklypear_lisp.Ast.VStr ("issue-token: " ^ e))
              | Ok raw -> (Pricklypear_lisp.Ast.VStr raw)))
       | _ -> (Pricklypear_lisp.Ast.VStr "issue-token: need [label] (own) or user-id label (admin)")));

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
        (Pricklypear_lisp.Ast.VStr "list-tokens: denied: admin only")
      else if target_uid = "" then
        (Pricklypear_lisp.Ast.VStr "list-tokens: no such user")
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
           | Error e -> (Pricklypear_lisp.Ast.VStr ("revoke-token: " ^ e))
           | Ok () -> (Pricklypear_lisp.Ast.VStr "ok"))
      | _ -> (Pricklypear_lisp.Ast.VStr "revoke-token: need credential-id"));

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
             (Pricklypear_lisp.Ast.VStr "rotate-token: denied: not your token")
           else
             let r = Auth_store.rotate_token t.store ~credential_id:cid ~user_id:caller_id () in
             (match r with
              | Error e -> (Pricklypear_lisp.Ast.VStr ("rotate-token: " ^ e))
              | Ok raw -> (Pricklypear_lisp.Ast.VStr raw))
       | _ -> (Pricklypear_lisp.Ast.VStr "rotate-token: need credential-id")));

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
        (Pricklypear_lisp.Ast.VStr "set-allow-Agent_tools.agent-promote: denied for anonymous")
      else
        (match args with
         | [ flag ] ->
             let u = Auth_store.get_user_by_username t.store caller in
             (match u with
              | None -> (Pricklypear_lisp.Ast.VStr "set-allow-Agent_tools.agent-promote: no such user")
              | Some user ->
                  let v = parse_bool flag in
                  let r = Auth_store.set_allow_agent_promote t.store ~user_id:user.id ~value:v in
                  (match r with
                   | Error e -> (Pricklypear_lisp.Ast.VStr ("set-allow-Agent_tools.agent-promote: " ^ e))
                   | Ok () -> (Pricklypear_lisp.Ast.VStr "ok")))
         | _ -> (Pricklypear_lisp.Ast.VStr "set-allow-Agent_tools.agent-promote: need bool")));

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
       | Error e -> (Pricklypear_lisp.Ast.VStr ("promote: " ^ e))
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
          (Pricklypear_lisp.Ast.VStr
             "promote-to-main: need source-ref and name(s)")
      | Pricklypear_lisp.Ast.VStr source_ref :: rest ->
          let names = List.concat_map names_of_val rest in
          if names = [] then
            (Pricklypear_lisp.Ast.VStr "promote-to-main: no names selected")
          else if
            (not t.is_admin)
            && t.manifest.username <> "admin"
            && t.manifest.username <> "system"
          then
            (Pricklypear_lisp.Ast.VStr
               "promote-to-main: requires admin")
          else begin
            let owner, name =
              match String.split_on_char '/' source_ref with
              | [ o; n ] when o <> "" && n <> "" -> (o, n)
              | _ -> ("", source_ref)
            in
            match Store.lookup_branch_id_by_name t.store ~owner ~name with
            | None ->
                (Pricklypear_lisp.Ast.VStr
                   ("promote-to-main: no such source branch: " ^ source_ref))
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
          (Pricklypear_lisp.Ast.VStr
             "promote-to-main: need source-ref string first"));

  (* (current-branch) -> VStr *)
  Primitives.register_sync tbl "current-branch"
    (fun _ ->
      let branch =
        match Request_context.get () with
        | Some c -> (Request_context.manifest c).branch_name
        | None -> t.manifest.branch_name
      in
      Pricklypear_lisp.Ast.VStr branch);

  (* Durable Agent_tools.agent sessions (ai-authoring.Agent_tools.agent-portal). *)
  Primitives.register tbl "Agent_tools.agent-session-new"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr seed ] ->
          Agent_tools.agent_session_new t ~seed ~tools:true ()
      | [ Pricklypear_lisp.Ast.VStr seed; Pricklypear_lisp.Ast.VBool tools ] ->
          Agent_tools.agent_session_new t ~seed ~tools ()
      | _ ->
          (Pricklypear_lisp.Ast.VStr
               "Agent_tools.agent-session-new: need seed-str [, tools-bool]"));

  Primitives.register tbl "Agent_tools.agent-say"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr sid; Pricklypear_lisp.Ast.VStr prompt ] ->
          Agent_tools.agent_say t ~session_id:sid ~prompt ()
      | [ Pricklypear_lisp.Ast.VStr sid; Pricklypear_lisp.Ast.VStr prompt;
          Pricklypear_lisp.Ast.VInt n ] ->
          Agent_tools.agent_say t ~session_id:sid ~prompt ~max_steps:(Int64.to_int n) ()
      | _ ->
          (Pricklypear_lisp.Ast.VStr
               "Agent_tools.agent-say: need session-id, prompt [, max-steps]"));

  Primitives.register tbl "Agent_tools.agent-history"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr sid ] -> Agent_tools.agent_history t ~session_id:sid
      | _ ->
          (Pricklypear_lisp.Ast.VStr "Agent_tools.agent-history: need session-id"));

  Primitives.register tbl "Agent_tools.agent-sessions"
    (fun args ->
      match args with
      | [] -> Agent_tools.agent_sessions t ()
      | [ Pricklypear_lisp.Ast.VInt n ] ->
          Agent_tools.agent_sessions t ~limit:(Int64.to_int n) ()
      | _ ->
          (Pricklypear_lisp.Ast.VStr "Agent_tools.agent-sessions: optional limit int"));

  Primitives.register tbl "Agent_tools.agent-session-delete"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr sid ] ->
          Agent_tools.agent_session_delete t ~session_id:sid
      | _ ->
          (Pricklypear_lisp.Ast.VStr "Agent_tools.agent-session-delete: need session-id"));

  (* Temporary aliases: new session + one say. Prefer Agent_tools.agent-session-new/say. *)
  Primitives.register tbl "Agent_tools.agent"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr seed;
          Pricklypear_lisp.Ast.VStr prompt ] ->
          Agent_tools.agent t ~seed ~prompt ()
      | _ ->
          (Pricklypear_lisp.Ast.VStr
               "Agent_tools.agent: need 2 strings (seed, prompt)"));

  Primitives.register tbl "Agent_tools.agent-task"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr prompt ] ->
          Agent_tools.agent_task t ~prompt ()
      | [ Pricklypear_lisp.Ast.VStr prompt; Pricklypear_lisp.Ast.VInt n ] ->
          Agent_tools.agent_task t ~max_steps:(Int64.to_int n) ~prompt ()
      | _ ->
          (Pricklypear_lisp.Ast.VStr
               "Agent_tools.agent-task: need prompt-str [, max-steps int]"));

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
          (Pricklypear_lisp.Ast.VStr
               "propose-function: bad args (need name-str, spec-str)"));

  (* (install-function name body-src) -> install AI-authored body (effect gate). *)
  Primitives.register tbl "install-function"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr name;
          Pricklypear_lisp.Ast.VStr body ] ->
          Agent_tools.install_ai_function t ~name ~body_src:body
      | _ ->
          (Pricklypear_lisp.Ast.VStr
               "install-function: need name-str, body-str"));

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
          (Pricklypear_lisp.Ast.VStr "function-source: need name-str"));

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
          (Pricklypear_lisp.Ast.VStr "function-doc: need name-str"));

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
          (Pricklypear_lisp.Ast.VStr "function-ast: need name-str"));

  (* (package-exports pkg) -> VList of public names (package-surface).
     Resolution: declared registry exports -> kernel_std_exports for
     std/kernel -> shallow heuristic. *)
  let kernel_std_exports =
    [ "list/map"; "list/filter"; "list/foldl"; "reverse"; "append"; "nth"
    ; "null?"; "pair?"; "car"; "cdr"; "cons"; "list"; "count"
    ; "string-append"; "string-length"; "substring"; "string-eq"
    ; "string-contains?"; "string-trim"; "html-escape"; "as-str"; "str"
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
          (Pricklypear_lisp.Ast.VStr "package-exports: need package-str"));

  (* (package-bindings pkg) -> VList of name strings (prefix pkg/ any depth) *)
  Primitives.register tbl "package-bindings"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr pkg ] ->
          let names = Agent_tools.package_binding_names t pkg in
          (Pricklypear_lisp.Ast.VList
               (List.map (fun n -> Pricklypear_lisp.Ast.VStr n) names))
      | _ ->
          (Pricklypear_lisp.Ast.VStr "package-bindings: need package-str"));

  (* (list-packages) -> VList of top-level package prefix strings *)
  Primitives.register tbl "list-packages"
    (fun args ->
      match args with
      | [] | [ Pricklypear_lisp.Ast.VNil ] ->
          let names = Agent_tools.list_package_names t in
          (Pricklypear_lisp.Ast.VList
               (List.map (fun n -> Pricklypear_lisp.Ast.VStr n) names))
      | _ ->
          (Pricklypear_lisp.Ast.VStr "list-packages: takes no args"));

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
          (Pricklypear_lisp.Ast.VStr "packages: takes no args"));

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
            (Pricklypear_lisp.Ast.VStr "package-enable: empty name")
          else if protected_pkg name then
            (Pricklypear_lisp.Ast.VStr
                 ("package-enable: " ^ name ^ " is always enabled"))
          else if t.manifest.username = "anonymous" then
            (Pricklypear_lisp.Ast.VStr "package-enable: auth required")
          else begin
            let () =
              Store.set_package_enabled t.store ~name ~enabled:true
                ~author:t.manifest.username
            in
            let () = rehydrate_fn t in
            (Pricklypear_lisp.Ast.VStr ("enabled " ^ name))
          end
      | _ ->
          (Pricklypear_lisp.Ast.VStr "package-enable: need package-str"));

  (* (package-disable name) — hide package bindings on current branch *)
  Primitives.register tbl "package-disable"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr name ]
      | [ Pricklypear_lisp.Ast.VSymbol name ] ->
          let name = String.trim name in
          if name = "" then
            (Pricklypear_lisp.Ast.VStr "package-disable: empty name")
          else if protected_pkg name then
            (Pricklypear_lisp.Ast.VStr
                 ("package-disable: cannot disable " ^ name))
          else if t.manifest.username = "anonymous" then
            (Pricklypear_lisp.Ast.VStr "package-disable: auth required")
          else begin
            let () =
              Store.set_package_enabled t.store ~name ~enabled:false
                ~author:t.manifest.username
            in
            let () = rehydrate_fn t in
            (Pricklypear_lisp.Ast.VStr ("disabled " ^ name))
          end
      | _ ->
          (Pricklypear_lisp.Ast.VStr "package-disable: need package-str"));

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
          (Pricklypear_lisp.Ast.VStr "apropos-docs: need pattern-str"));

  (* (Agent_tools.describe name-or-pkg) -> VStr selective reveal text *)
  Primitives.register tbl "Agent_tools.describe"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr needle ] ->
          let s = Agent_tools.describe t needle in
          (Pricklypear_lisp.Ast.VStr s)
      | _ ->
          (Pricklypear_lisp.Ast.VStr "Agent_tools.describe: need name-or-package-str"));

  (* (export-package pkg) -> VStr lib.pp text *)
  Primitives.register tbl "export-package"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr pkg ] ->
          let text = Agent_tools.export_package t pkg in
          (Pricklypear_lisp.Ast.VStr text)
      | _ ->
          (Pricklypear_lisp.Ast.VStr "export-package: need package-str"));

  (* SSH keys (auth.user-ssh-repl phase 1) *)
  Primitives.register tbl "add-ssh-key"
    (fun args ->
      if t.manifest.username = "anonymous" then
        (Pricklypear_lisp.Ast.VStr "add-ssh-key: denied for anonymous")
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
                 (Pricklypear_lisp.Ast.VStr ("add-ssh-key: " ^ e)))
        | [ Pricklypear_lisp.Ast.VStr openssh; Pricklypear_lisp.Ast.VStr label ] ->
            let r =
              Auth_store.add_ssh_key t.store ~user_id:t.manifest.user_id
                ~openssh ~label:(Some label) ()
            in
            (match r with
             | Ok m ->
                 (Pricklypear_lisp.Ast.VStr m.Auth_store.fingerprint)
             | Error e ->
                 (Pricklypear_lisp.Ast.VStr ("add-ssh-key: " ^ e)))
        | _ ->
            (Pricklypear_lisp.Ast.VStr "add-ssh-key: need openssh-str [, label]"));

  Primitives.register tbl "list-ssh-keys"
    (fun _args ->
      if t.manifest.username = "anonymous" then
        (Pricklypear_lisp.Ast.VStr "list-ssh-keys: denied for anonymous")
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
        (Pricklypear_lisp.Ast.VStr "revoke-ssh-key: denied for anonymous")
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
                 (Pricklypear_lisp.Ast.VStr ("revoke-ssh-key: " ^ e)))
        | _ ->
            (Pricklypear_lisp.Ast.VStr "revoke-ssh-key: need fingerprint-or-id"));

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
          (Pricklypear_lisp.Ast.VStr "verify-ssh-key: need openssh-pubkey-str"));

  (* Per-user LLM keys (auth.user-llm-keys) *)
  Primitives.register tbl "set-llm-key"
    (fun args ->
      if t.manifest.username = "anonymous" then
        (Pricklypear_lisp.Ast.VStr "set-llm-key: denied for anonymous")
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
                 (Pricklypear_lisp.Ast.VStr ("set-llm-key: " ^ e)))
        | [ Pricklypear_lisp.Ast.VStr key; Pricklypear_lisp.Ast.VStr base ] ->
            let r =
              Auth_store.set_llm_key t.store ~user_id:t.manifest.user_id
                ~api_key:key ~base:(Some base) ()
            in
            (match r with
             | Ok m ->
                 (Pricklypear_lisp.Ast.VStr ("ok last4=" ^ m.Auth_store.last4))
             | Error e ->
                 (Pricklypear_lisp.Ast.VStr ("set-llm-key: " ^ e)))
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
                 (Pricklypear_lisp.Ast.VStr ("set-llm-key: " ^ e)))
        | _ ->
            (Pricklypear_lisp.Ast.VStr
                 "set-llm-key: need key-str [, base] [, model]"));

  Primitives.register tbl "clear-llm-key"
    (fun _args ->
      if t.manifest.username = "anonymous" then
        (Pricklypear_lisp.Ast.VStr "clear-llm-key: denied for anonymous")
      else
        let r =
          Auth_store.clear_llm_key t.store ~user_id:t.manifest.user_id
        in
        match r with
        | Ok () -> Pricklypear_lisp.Ast.VNil
        | Error e ->
            (Pricklypear_lisp.Ast.VStr ("clear-llm-key: " ^ e)));

  Primitives.register tbl "llm-key-status"
    (fun _args ->
      if t.manifest.username = "anonymous" then
        (Pricklypear_lisp.Ast.VStr "llm-key-status: denied for anonymous")
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

  (* Coding tools: freeze AST rewrites into store *)
  Primitives.register tbl "llm-inner"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr name;
          Pricklypear_lisp.Ast.VStr concept ] ->
          Agent_tools.llm_inner t ~name ~concept
      | _ ->
          (Pricklypear_lisp.Ast.VStr "llm-inner: need name-str, concept-str"));

  Primitives.register tbl "llm-outer"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr name;
          Pricklypear_lisp.Ast.VStr concept ] ->
          Agent_tools.llm_outer t ~name ~concept
      | _ ->
          (Pricklypear_lisp.Ast.VStr "llm-outer: need name-str, concept-str"));

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
          (Pricklypear_lisp.Ast.VStr
               "llm-rewrite: need name, concept [, mode replace|wrap]"));

  Primitives.register tbl "llm-seed"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr requirement ] ->
          Agent_tools.llm_seed t ~requirement ()
      | [ Pricklypear_lisp.Ast.VStr requirement;
          Pricklypear_lisp.Ast.VInt n ] ->
          Agent_tools.llm_seed t ~requirement ~max_steps:(Int64.to_int n) ()
      | _ ->
          (Pricklypear_lisp.Ast.VStr
               "llm-seed: need requirement-str [, max-steps int]"));

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
          (Pricklypear_lisp.Ast.VStr
             "code-history: need name [owner/branch]"));

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
               (Pricklypear_lisp.Ast.VStr "code-diff: n must be int")
           | Some n ->
               let bid = Store.current_branch_id t.store in
               (match Store.code_diff t.store ~branch_id:bid ~name ~n with
                | Ok s -> (Pricklypear_lisp.Ast.VStr s)
                | Error e -> (Pricklypear_lisp.Ast.VStr e)))
      | _ ->
          (Pricklypear_lisp.Ast.VStr "code-diff: need name n"));
