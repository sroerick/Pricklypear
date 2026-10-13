(* Data (CRUD + tree + shared data spaces) prims - split out of prims_db.ml (i-1788671035-8 fix pass).

   Bodies are verbatim relocations from the old single register closure;
   shared closure helpers come from Prims_db_common, re-bound below so the
   moved code stays byte-identical. *)
open Session_types

let register (t : t) (tbl : Primitives.t) : unit =
  let actor () = Prims_db_common.actor ~t () in
  let deny_anon name = Prims_db_common.deny_anon name in
  let register_crud name fn = Prims_db_common.register_crud ~tbl name fn in
  let caller_username () = Prims_db_common.caller_username ~t () in
  (* spec_delegation: delegated sessions carry data grants only; schema
     authority (type_defs) is code-adjacent and stays with the owner's
     own admin session. *)
  let deny_delegated op =
    raise (Pricklypear_lisp.Ast.Interp_error
             (Pricklypear_lisp.Ast.Prim_error (op, "denied for delegated sessions and tenants (data, never for code)", "denied")))
  in

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
        | [ type_v; fields_v ] ->
            (* json-values-alist: a structured fields value (alist from
               json-parse/dict-set*, or any VList body) serializes at the
               boundary exactly like tree/upsert's body — the documented
               dict-set* mutation idiom must not be a string-only trap. *)
            let type_name =
              Prims_common.coerce_str ~ctx:"create type" type_v
            in
            let fields_json = Prims_db_common.json_body ~ctx:"create" fields_v in
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
        | [ type_v; id_v; fields_v ] ->
            (* See create: structured fields serialize at the boundary. *)
            let type_name =
              Prims_common.coerce_str ~ctx:"update type" type_v
            in
            let id = Prims_common.coerce_str ~ctx:"update id" id_v in
            let fields_json = Prims_db_common.json_body ~ctx:"update" fields_v in
            Crud.update_guarded t.store ~type_name ~id ~fields_json ~caller:uname
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
            Crud.update_field_guarded t.store ~type_name ~id ~field ~value ~caller:uname
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
            Crud.delete_guarded t.store ~type_name ~id ~caller:uname
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

  (* (rows-author 'type [limit] [sort]) and
     (find-rows-author 'type "f" "v" ... [limit] [sort]): author-visible
     variants returning [id; author; fields] triples, so callers can bind
     policy to the real data_rows.author instead of caller-written fields. *)
  register_crud "rows-author"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr type_name ] ->
          Crud.list_with_author t.store ~type_name ()
      | [ Pricklypear_lisp.Ast.VStr type_name;
          Pricklypear_lisp.Ast.VInt n ] ->
          Crud.list_with_author t.store ~type_name ~limit:(Int64.to_int n) ()
      | [ Pricklypear_lisp.Ast.VStr type_name;
          Pricklypear_lisp.Ast.VStr sort ] ->
          Crud.list_with_author t.store ~type_name ~sort:(Some sort) ()
      | [ Pricklypear_lisp.Ast.VStr type_name;
          Pricklypear_lisp.Ast.VInt n;
          Pricklypear_lisp.Ast.VStr sort ] ->
          Crud.list_with_author t.store ~type_name ~limit:(Int64.to_int n) ~sort:(Some sort) ()
      | _ ->
          (raise (Pricklypear_lisp.Ast.Interp_error
                    (Pricklypear_lisp.Ast.Prim_error
                       ("rows-author", "bad args (need type[, limit][, sort])", "bad-args")))));

  register_crud "find-rows-author"
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
        | _ -> Error "find-rows-author: bad filter args"
      in
      match args with
      | Pricklypear_lisp.Ast.VStr type_name :: rest ->
          (match parse_filters [] rest with
           | Error e ->
               raise (Pricklypear_lisp.Ast.Interp_error
                        (Pricklypear_lisp.Ast.Prim_error
                           ("find-rows-author", e, "bad-args")))
           | Ok (filters, limit, sort) ->
               Crud.find_rows_with_author t.store ~type_name ~filters ~limit ~sort ())
      | _ ->
          ((raise (Pricklypear_lisp.Ast.Interp_error
                     (Pricklypear_lisp.Ast.Prim_error
                        ("find-rows-author", "need type and field/value pairs", "bad-args"))))));

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
          (Crud.node_path t.store ~type_name ~id)
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
          Crud.tree_get t.store ~path ~viewer:(Some (actor ())) ()
      | [ Pricklypear_lisp.Ast.VStr path
        ; Pricklypear_lisp.Ast.VInt g
        ] ->
            (* reads-as-of-gen: optional :gen N -> versioned read at that gen
               (tree-time.reads-as-of-gen). Omitted means head.
               note-vis-p1: both arms carry the caller as viewer so a read
               into another user's note garden clears the body vis floor. *)
            Crud.tree_get t.store ~path ~at_gen:g ~viewer:(Some (actor ())) ()
      | _ ->
          (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("tree/get", "need path [, :gen]/", "bad-args")))));

  register_crud "tree/get-many"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VList paths ] ->
          let ps =
            List.filter_map
              (function Pricklypear_lisp.Ast.VStr p -> Some p | _ -> None)
              paths
          in
          Crud.tree_get_many t.store ~paths:ps ~viewer:(Some (actor ())) ()
      | _ ->
          (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("tree/get-many", "need [path, …]", "bad-args")))));

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

  (* Shared data spaces (branching.shared-data-spaces S2): member-gated
     collaboration compartments on the data plane. Gates: any non-anon
     identity creates; owner (or role='owner' member, or kernel admin)
     manages members and admits types; read prims are membership-scoped.
     The non-member WRITE guard lives in Crud.create/update/update_field/
     delete (guard_space_write), so every data prim honors membership. *)
  let kernel_admin_of uname =
    match Auth_store.get_user_by_username t.store uname with
    | Some u -> u.Auth_store.is_admin
    | None -> false
  in
  register_crud "space-create"
    (fun args ->
      let uname = actor () in
      if uname = "anonymous" then deny_anon "space-create"
      else
        match args with
        | [ Pricklypear_lisp.Ast.VStr name ] ->
            Crud.space_create t.store ~caller:uname ~name ()
        | [ Pricklypear_lisp.Ast.VStr name; Pricklypear_lisp.Ast.VStr flag ]
          when flag = "open" ->
            Crud.space_create t.store ~caller:uname ~name ~open_:true ()
        | _ ->
            ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("space-create", "need space-name [open]", "bad-args"))))));

  register_crud "space-exists?"
    (fun args ->
      let uname = actor () in
      if uname = "anonymous" then deny_anon "space-exists?"
      else
        match args with
        | [ Pricklypear_lisp.Ast.VStr name ] ->
            Crud.space_exists_prim t.store ~name
        | _ ->
            ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("space-exists?", "need space-name", "bad-args"))))));

  register_crud "space-add-member"
    (fun args ->
      let uname = actor () in
      if uname = "anonymous" then deny_anon "space-add-member"
      else
        match args with
        | [ Pricklypear_lisp.Ast.VStr space;
            Pricklypear_lisp.Ast.VStr username ] ->
            Crud.space_add_member t.store ~caller:uname
              ~kernel_admin:(kernel_admin_of uname) ~space ~username
        | _ ->
            ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("space-add-member", "need space username", "bad-args"))))));

  register_crud "space-remove-member"
    (fun args ->
      let uname = actor () in
      if uname = "anonymous" then deny_anon "space-remove-member"
      else
        match args with
        | [ Pricklypear_lisp.Ast.VStr space;
            Pricklypear_lisp.Ast.VStr username ] ->
            Crud.space_remove_member t.store ~caller:uname
              ~kernel_admin:(kernel_admin_of uname) ~space ~username
        | _ ->
            ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("space-remove-member", "need space username", "bad-args"))))));

  register_crud "space-admit"
    (fun args ->
      let uname = actor () in
      if uname = "anonymous" then deny_anon "space-admit"
      else
        match args with
        | [ Pricklypear_lisp.Ast.VStr type_name;
            Pricklypear_lisp.Ast.VStr space ] ->
            Crud.space_admit t.store ~caller:uname
              ~kernel_admin:(kernel_admin_of uname) ~space ~type_name
        | _ ->
            ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("space-admit", "need type space", "bad-args"))))));

  register_crud "space-list"
    (fun _args ->
      let uname = actor () in
      if uname = "anonymous" then deny_anon "space-list"
      else
        Crud.space_list t.store ~caller:uname
          ~kernel_admin:(kernel_admin_of uname));

  (* (data-scope) -> VStr JSON describing the caller's EFFECTIVE data
     scope: the path prefixes this identity reads and writes, the shared
     seed corpus, the member spaces, and the caller's data lenses. This is
     the read-only verification companion the docs point at — an agent can
     establish its isolation boundary before running an experiment instead
     of inferring it from stale branch prose. Branches version CODE; they
     do not version data (borge/one-tree.borg branching-interaction). *)
  register_crud "data-scope"
    (fun _args ->
      let uname = actor () in
      if uname = "anonymous" then deny_anon "data-scope"
      else
        let strings (v : Pricklypear_lisp.Ast.value) : string list =
          match v with
          | Pricklypear_lisp.Ast.VList vs ->
              List.filter_map
                (function
                  | Pricklypear_lisp.Ast.VStr s -> Some s
                  | _ -> None)
                vs
          | _ -> []
        in
        let spaces =
          strings
            (Crud.space_list t.store ~caller:uname
               ~kernel_admin:(kernel_admin_of uname))
        in
        let lenses = strings (Crud.list_data_lenses t.store ()) in
        let json =
          `Assoc
            [ ("user", `String uname)
            ; ("user_root",
               `String ("data.u." ^ Tree_store.enc_label uname))
            ; ("shared_root", `String "data.shared")
            ; ("note_root", `String (Tree_store.garden_root ~user:uname))
            ; ("spaces", `List (List.map (fun s -> `String s) spaces))
            ; ("data_lenses", `List (List.map (fun s -> `String s) lenses))
            ]
        in
        Pricklypear_lisp.Ast.VStr (Yojson.Safe.to_string json));

  register_crud "space-members"
    (fun args ->
      let uname = actor () in
      if uname = "anonymous" then deny_anon "space-members"
      else
        match args with
        | [ Pricklypear_lisp.Ast.VStr space ] ->
            Crud.space_members t.store ~caller:uname
              ~kernel_admin:(kernel_admin_of uname) ~space
        | _ ->
            ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("space-members", "need space", "bad-args"))))));

  (* (data-history 'type id) -> VStr JSON list newest-first
     [{gen, author, created_at, delete, body}]. branching.shared-data-
     spaces S3: the LOG CONSUMER over tree_versions datum rows (any
     compartment). Confined like get: the caller's identity must see the
     row (own rows + shared + member spaces). Live rows only in v1. *)
  register_crud "data-history"
    (fun args ->
      let uname = actor () in
      (match args with
       | [ Pricklypear_lisp.Ast.VStr type_name;
           Pricklypear_lisp.Ast.VStr id ] ->
           Crud.data_history t.store ~viewer:uname ~type_name ~id
       | _ ->
           ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("data-history", "need type id", "bad-args")))))));
  register_crud "tree/upsert"
    (fun args ->
      let uname = actor () in
      if uname = "anonymous" then deny_anon "tree/upsert"
      else
        (* json-values-alist: the documented mutation idiom is
           tree/get + dict-set* + tree/upsert, and dict-set* over a
           parsed body returns an ALIST, not a JSON string. Accept a
           structured body (alist/VList/VPair) and serialize it here —
           the same polymorphism dict-get/set/keys already have. A VStr
           body keeps the legacy path (it may be JSON or a plain
           string). Path/kind/type ride coerce_str so symbols work.
           (Live bug 2026-09-27: notes/tree/save-outline passed
           dict-set output straight through, tree/upsert raised
           bad-args AFTER clear-headings had deleted every heading —
           the note was emptied on every save.) *)
        match args with
        | [ path_v; kind_v; type_v; body_v ] ->
            let path = Prims_common.coerce_str ~ctx:"tree/upsert path" path_v in
            let kind = Prims_common.coerce_str ~ctx:"tree/upsert kind" kind_v in
            let type_name =
              Prims_common.coerce_str ~ctx:"tree/upsert type" type_v
            in
            let body_json =
              Prims_db_common.json_body ~ctx:"tree/upsert" body_v
            in
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
          Crud.tree_get_uuid t.store ~uuid ~viewer:(Some (actor ())) ()
      | _ ->
          (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("tree/get-uuid", "need uuid", "bad-args")))));

  register_crud "tree/find-by"
    (fun args ->
      (* i-1788348645: reads are garden-scoped by default (writes always
         were). (tree/find-by kind field value [limit] [prefix]); an
         explicit prefix opts out to a wider subtree, "" = whole store.
         note-vis-p1: every Lisp call carries the caller as viewer, so
         rows outside the caller's garden must clear the body vis floor
         (private | users | public); the lib never decides visibility. *)
      let garden () = Tree_store.garden_root ~user:(actor ()) in
      let viewer = actor () in
      match args with
      | [ Pricklypear_lisp.Ast.VStr kind
        ; Pricklypear_lisp.Ast.VStr field
        ; Pricklypear_lisp.Ast.VStr value
        ] ->
            Crud.tree_find_by t.store ~kind:(Some kind) ~field ~value
              ~prefix:(garden ()) ~viewer:(Some viewer) ()
      | [ Pricklypear_lisp.Ast.VStr kind
        ; Pricklypear_lisp.Ast.VStr field
        ; Pricklypear_lisp.Ast.VStr value
        ; Pricklypear_lisp.Ast.VInt n
        ] ->
            Crud.tree_find_by t.store ~kind:(Some kind) ~field ~value
              ~prefix:(garden ()) ~viewer:(Some viewer)
              ~limit:(Int64.to_int n) ()
      | [ Pricklypear_lisp.Ast.VStr kind
        ; Pricklypear_lisp.Ast.VStr field
        ; Pricklypear_lisp.Ast.VStr value
        ; Pricklypear_lisp.Ast.VStr p
        ] ->
            Crud.tree_find_by t.store ~kind:(Some kind) ~field ~value
              ~prefix:p ~viewer:(Some viewer) ()
      | [ Pricklypear_lisp.Ast.VStr kind
        ; Pricklypear_lisp.Ast.VStr field
        ; Pricklypear_lisp.Ast.VStr value
        ; Pricklypear_lisp.Ast.VInt n
        ; Pricklypear_lisp.Ast.VStr p
        ] ->
            Crud.tree_find_by t.store ~kind:(Some kind) ~field ~value
              ~prefix:p ~viewer:(Some viewer) ~limit:(Int64.to_int n) ()
      | _ ->
          ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("tree/find-by", "need kind field value [, limit] [, prefix]", "bad-args"))))));

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

  (* Full variant: index rows carry the REAL outline (log/issues
     "note-index omits outline → From-notes scrape silently dead").
     The plain tree/note-index stays light (the /notes UI folds those
     rows on the request path); scrapes that need outlines call this. *)
  register_crud "tree/note-index-full"
    (fun args ->
      match args with
      | [] ->
          Crud.tree_note_index t.store
            ~prefix:(Tree_store.garden_root ~user:(actor ()))
            ~with_outline:true ()
      | [ Pricklypear_lisp.Ast.VStr prefix ] ->
          Crud.tree_note_index t.store ~prefix ~with_outline:true ()
      | [ Pricklypear_lisp.Ast.VStr prefix; Pricklypear_lisp.Ast.VInt n ] ->
          Crud.tree_note_index t.store ~prefix ~limit:(Int64.to_int n)
            ~with_outline:true ()
      | _ ->
          ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("tree/note-index-full", "[] | prefix [, limit]", "bad-args"))))));

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

    (* (deftype 'name 'field1 'field2 ...) -> VStr "defined type <name>".
       All args are quoted symbols (now VSymbol; coerced above). Persists
       the declaration into type_defs on the session's branch.

       Garden-branch ownership gate: deftype is a code/schema write and RLS
       exempts non-datum kinds (migration 013), so it is gated like the rest
       of the code write surface, not by require_admin. The write lands on the
       session's CURRENT branch, so the gate is branch_owner_authorized on
       that branch's owner (admin for system/*, own tenant otherwise).
       Replaces the require_admin added for BORGE-1785213343: schema
       authority lives on the branch you own, not only with site admins.
       Anonymous still raises (deny_anon); delegated stays denied
       (spec_delegation: schema is code-adjacent). *)
    register_crud "deftype"
      (fun args ->
        let uname = caller_username () in
        if uname = "anonymous" then deny_anon "deftype"
        else if Session_types.restricted t then deny_delegated "deftype"
        else
          let cur_owner, _ =
            Session_control_prims.split_branch_ref t.manifest.branch_name
          in
          if not (Session_control_prims.branch_owner_authorized t cur_owner)
          then
            (Pricklypear_lisp.Ast.VStr
               (Printf.sprintf "deftype: denied: branch %s is owned by %s"
                  t.manifest.branch_name cur_owner))
          else Crud.deftype t.store ~args ~author:uname);
