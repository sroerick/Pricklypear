(* Auth (users, tokens, ssh, admin, promote) prims - split out of prims_db.ml (i-1788671035-8 fix pass).

   Bodies are verbatim relocations from the old single register closure;
   shared closure helpers come from Prims_db_common, re-bound below so the
   moved code stays byte-identical. *)
open Session_types

let promote_to_personal (t : t) ~(names : string list)
  : (int, string) result =
  let me_opt = Auth_store.get_user_by_username t.store t.manifest.username in
  match me_opt with
  | None -> (Error "no such user")
  | Some me when me.user_type <> "agent" ->
      (Error "promote-to-personal is for agent users")
  | Some me ->
      (match me.parent_user_id with
       | None -> (Error "agent has no parent")
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
                  (Error "agent promote disabled by parent")
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
  let caller_username () = Prims_db_common.caller_username ~t () in
  let require_admin op k = Prims_db_common.require_admin ~t op k in

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

  (* (create-agent parent-username) -> VStr agent-user-id. Admin only. *)
  Primitives.register tbl "create-agent"
    (fun args ->
      require_admin "create-agent" (fun () ->
        match args with
        | [ Pricklypear_lisp.Ast.VStr parent ] ->
            let r = Auth_store.create_agent t.store ~parent_username:parent in
            (match r with
             | Error e ->
                 (Pricklypear_lisp.Ast.VStr ("create-agent: " ^ e))
             | Ok u ->
                 (Pricklypear_lisp.Ast.VStr u.Auth_store.id))
        | _ ->
            (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("prim", "create-agent: need parent-username", "bad-args"))))));

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

  (* (set-allow-agent-promote bool) -> VStr ok|error. Sets the CALLER's own toggle. *)
  Primitives.register tbl "set-allow-agent-promote"
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
        (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("prim", "set-allow-agent-promote: denied for anonymous", "denied"))))
      else
        (match args with
         | [ flag ] ->
             let u = Auth_store.get_user_by_username t.store caller in
             (match u with
              | None ->
                  raise (Pricklypear_lisp.Ast.Interp_error
                           (Pricklypear_lisp.Ast.Prim_error
                              ("prim", "set-allow-agent-promote: no such user", "not-found")))
              | Some user ->
                  let v = parse_bool flag in
                  let r = Auth_store.set_allow_agent_promote t.store ~user_id:user.id ~value:v in
                  (match r with
                   | Error e ->
                       raise (Pricklypear_lisp.Ast.Interp_error
                                (Pricklypear_lisp.Ast.Prim_error
                                   ("prim", "set-allow-agent-promote: " ^ e, "failed")))
                   | Ok () -> (Pricklypear_lisp.Ast.VStr "ok")))
         | _ -> (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("prim", "set-allow-agent-promote: need bool", "bad-args"))))));

  (* (set-user-type username "agent"|"human") -> VStr "ok". Admin only;
     server-only like create-user (account administration, never a
     remote-proxy op). Mirrors POST /admin/users action=set_user_type
     (auth.user-management; the type flip the agent-native chapter
     recorded as pending). Flipping to 'agent' sets parent_user_id to
     the CALLING admin; 'human' clears it. An is_admin target is never
     flipped to agent (admin agents are a foot-gun). *)
  Primitives.register_server_only tbl "set-user-type"
    (fun args ->
      require_admin "set-user-type" (fun () ->
        let arg_str = function
          | Pricklypear_lisp.Ast.VStr s -> Some s
          | Pricklypear_lisp.Ast.VSymbol s -> Some s
          | _ -> None
        in
        let bad_args () =
          (raise (Pricklypear_lisp.Ast.Interp_error
                    (Pricklypear_lisp.Ast.Prim_error
                       ("set-user-type",
                        "need username and user-type (human|agent)",
                        "bad-args"))))
        in
        match args with
        | [ username_arg; utype_arg ] ->
            (match arg_str username_arg, arg_str utype_arg with
             | Some username, Some utype ->
                 let username =
                   String.lowercase_ascii (String.trim username) in
                 let utype = String.lowercase_ascii (String.trim utype) in
                 if username = "" then bad_args ()
                 else
                 let target =
                   Auth_store.get_user_by_username t.store username in
                 (match target with
                  | None ->
                      (raise (Pricklypear_lisp.Ast.Interp_error
                                (Pricklypear_lisp.Ast.Prim_error
                                   ("set-user-type", "no such user: " ^ username, "not-found"))))
                  | Some u when u.Auth_store.is_admin && utype = "agent" ->
                      (raise (Pricklypear_lisp.Ast.Interp_error
                                (Pricklypear_lisp.Ast.Prim_error
                                   ("set-user-type", "cannot make an admin user an agent", "denied"))))
                  | Some u ->
                      let parent_user_id =
                        if utype = "agent" then
                          (match
                             Auth_store.get_user_by_username t.store
                               (caller_username ())
                           with
                           | Some p -> Some p.Auth_store.id
                           | None -> None)
                        else None
                      in
                      let r =
                        Auth_store.set_user_type t.store ~user_id:u.id
                          ~user_type:utype ~parent_user_id
                      in
                      (match r with
                       | Error e ->
                           (raise (Pricklypear_lisp.Ast.Interp_error
                                     (Pricklypear_lisp.Ast.Prim_error
                                        ("set-user-type", e, "failed"))))
                       | Ok () ->
                           let () =
                             Auth_store.log_auth t.store ~user_id:u.id
                               ~kind:"admin_set_user_type" ~success:true ()
                           in
                           (Pricklypear_lisp.Ast.VStr "ok")))
             | _ -> bad_args ())
        | _ -> bad_args ()));

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

  (* Smoke/admin: resolve openssh pubkey -> username | ().
     ORACLE ONLY: this prim must never be reachable by anonymous callers
     (it is a key-equality oracle). The future SSH login door must verify a
     challenge SIGNATURE made by the private key — never test key equality
     against a stored pubkey directly. *)
  Primitives.register tbl "verify-ssh-key"
    (fun args ->
       if t.manifest.username = "anonymous" then
         (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("verify-ssh-key", "denied for anonymous", "denied"))))
       else
         match args with
      | [ Pricklypear_lisp.Ast.VStr openssh ] ->
          let u = Auth_store.verify_ssh_key t.store ~openssh_or_blob:openssh () in
          (match u with
           | Some user ->
               (Pricklypear_lisp.Ast.VStr user.Auth_store.username)
           | None -> Pricklypear_lisp.Ast.VNil)
      | _ ->
          (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("verify-ssh-key", "need openssh-pubkey-str", "bad-args")))));
