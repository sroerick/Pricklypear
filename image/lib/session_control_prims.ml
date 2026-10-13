(** Session CONTROL PRIMS — the DB-touching control surface (fork/merge/
    route/checkout/unbind/prune-synced/bench + private with-transaction/
    step rims + with-public-write), extracted verbatim from session.ml
    (i-1788671109-1). Also owns the branch-ref helpers (split_branch_ref /
    branch_owner_authorized) the prims and Session's checkout funnel share.

    Seam: this module cannot depend on Session (Session calls register),
    so the two env-driver operations the prims need — [rehydrate] (full
    env reload for the branch) and [checkout_to] (branch switch + shell
    bust + rehydrate) — arrive as a [hooks] record supplied by Session
    at registration time. Same pattern as Prims_db.~rehydrate_fn.

    Behavior contract: byte-identical relocation; the prim table, arg
    shapes, error strings, and owner-authorization rules are unchanged. *)

open Session_types

(** The env-driver seam (see module comment). *)
type hooks = {
  rehydrate : t -> unit;
  checkout_to : t -> branch_id:string -> branch_ref:string -> unit;
}

let split_branch_ref (s : string) : string * string =
  match String.split_on_char '/' s with
  | [ o; n ] -> o, n
  | [ n ] -> "system", n  (* bare name = system/<name> *)
  | _ -> "system", s

(** T5 ast-merge (branching ast-merge): the eval-boundary side of
    [Store.merge_branch]'s merge3 hook. The store hands us three OPAQUE
    jsonb fn bodies — base (fork point), ours (source pin), theirs
    (parent live) — and takes back either the merged jsonb body or the
    conflict lines. The AST codec ([Ast.expr_of_body_json] /
    [Ast.expr_to_body_json]) and the pure merge3 ([Sexp_merge]) live
    HERE, never in store_*.ml (store-split opaque-body contract,
    scripts/verify_store_split.sh). Conflicts are DATA: each line names
    the exact subtree path + both sides' values; nothing is silently
    resolved (the merged body keeps BASE at conflicted subtrees). The
    merged body is re-validated by canonical round-trip (encode, decode,
    same [Ast_diff.ast_hash]) before it is returned; a body that does
    not parse (or fails re-validation) is an Error, never a guess. *)
let merge3_hook ~(base : string) ~(ours : string) ~(theirs : string) :
  (string, string list) result =
  let open Pricklypear_lisp in
  match
    ( Ast.expr_of_body_json base,
      Ast.expr_of_body_json ours,
      Ast.expr_of_body_json theirs )
  with
  | Some b, Some o, Some t ->
      let r = Sexp_merge.merge b o t in
      if r.Sexp_merge.conflicts = [] then
        let merged = Sexp_merge.merged_body r in
        let json = Ast.expr_to_body_json merged in
        (match Ast.expr_of_body_json json with
         | Some back when Ast_diff.ast_hash back = Ast_diff.ast_hash merged ->
             Ok json
         | _ ->
             Error [ "merged body failed canonical re-validation" ])
      else
        Error
          (List.map Sexp_merge.conflict_to_string r.Sexp_merge.conflicts)
  | _ ->
      Error [ "body is not a parseable AST; three-way merge unavailable" ]

(** Branch-owner authorization (i-1787970640, spec branching.personal-
    workspace): a session with username U (admin = t.is_admin || U =
    "admin") may target a branch owner O iff O = U or the session is
    admin. Agent users are named "<parent>/agent" and work in the
    parent-owned <parent>/agent workshop branch, so their tenant is the
    parent username.

    ADMIN RECHECK (2026-10 security pass): the admin arm reads
    Prims_db_common.is_verified_admin — the raw flag plus a live
    users.is_admin re-check — because the jobs runner / loopback REPL
    evaluate on the SHARED BOOT SESSION whose t.is_admin is permanently
    boot-derived (true on prod) while manifest.username flips to the
    submitter. With the raw flag, any submitted job could checkout ANY
    tenant's garden. The U="admin" textual fallback is KEPT: it is the
    documented rule and the bootstrap valve (users row exists from
    bootstrap_admin_if_empty on real boots; no-DB tests exercise it). *)
let branch_owner_authorized (t : t) (owner : string) : bool =
  let u = t.manifest.username in
  if Prims_db_common.is_verified_admin ~t () || u = "admin" then true
  else
    let tenant =
      let suffix = "/agent" in
      let n = String.length u and m = String.length suffix in
      if n > m && String.sub u (n - m) m = suffix then
        String.sub u 0 (n - m)
      else u
    in
    owner = u || owner = tenant

(** Read-side rule (i-1787970640 residue, 09-10): system/* is the shared
    defaults tree every session inherits from, so its code is public to
    the image's users; personal gardens are not. Fork's parent and
    merge's source COPY code into the caller's space, so a garden owned
    by another tenant would exfiltrate it. *)
let branch_readable (t : t) (owner : string) : bool =
  owner = "system" || branch_owner_authorized t owner

let register ~(hooks : hooks) (t : t) (tbl : Primitives.t) : unit =
  (* (fork 'parent 'child) -> VStr new-branch-id | VStr "no such parent" *)
  Primitives.register_server_only tbl "fork"
    (fun args ->
      if t.manifest.username = "anonymous" then
        (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("fork", "denied for anonymous", "denied"))))
      else if t.agent_branch_lock then
        (Pricklypear_lisp.Ast.VStr
             "fork blocked: agent is locked to the current branch")
      else
      match args with
      | [ Pricklypear_lisp.Ast.VStr parent_name;
          Pricklypear_lisp.Ast.VStr child_name ] ->
        let parent_owner, parent_short = split_branch_ref parent_name in
        let child_owner, child_short = split_branch_ref child_name in
        (* i-1787970640 (residue 09-10): BOTH sides gate, BEFORE the
           lookup so a denial never leaks whether the ref exists.
           child_owner passes the write rule (blocks pre-claiming
           another user's garden); parent_owner passes the read rule —
           system/* is the shared defaults tree, other tenants' gardens
           are not, and fork would copy their code into the caller's
           readable branch. *)
        if not (branch_owner_authorized t child_owner) then
          (Pricklypear_lisp.Ast.VStr
             (Printf.sprintf "denied: branch %s is owned by %s"
                child_name child_owner))
        else if not (branch_readable t parent_owner) then
          (Pricklypear_lisp.Ast.VStr
             (Printf.sprintf "denied: branch %s is owned by %s"
                parent_name parent_owner))
        else
        let parent_id =
          Store.lookup_branch_id_by_name
            t.store ~owner:parent_owner ~name:parent_short
        in
        (match parent_id with
         | None ->
             (Pricklypear_lisp.Ast.VStr
                           (Printf.sprintf "no such parent: %s" parent_name))
         | Some pid ->
             let new_id =
               Store.fork_branch t.store ~parent_id:pid
                 ~owner:child_owner ~child_name:child_short
             in
             (match new_id with
              | Some id ->
                  (Pricklypear_lisp.Ast.VStr id)
              | None ->
                  (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("prim", "fork failed", "failed"))))))
      | _ ->
          (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("fork", "bad args (need 2 strings)", "bad-args")))));

  (* (merge 'source 'target) -> VStr "merged N rows" | raise (denied).
     POLICY (per spec [branching.merge]): merging into `system/*`
     requires the session username to be "admin"; merges into the
     caller's own `owner/*` branches are free. *)
  Primitives.register_server_only tbl "merge"
    (fun args ->
      if t.manifest.username = "anonymous" then
        (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("merge", "denied for anonymous", "denied"))))
      else if t.agent_branch_lock then
        (Pricklypear_lisp.Ast.VStr
             "merge blocked: agent is locked to the current branch")
      else
      match args with
      | [ Pricklypear_lisp.Ast.VStr source_ref;
          Pricklypear_lisp.Ast.VStr target_ref ] ->
          let src_owner, src_short = split_branch_ref source_ref in
          let tgt_owner, tgt_short = split_branch_ref target_ref in
          (* i-1787970640: BOTH src_owner and tgt_owner gate. Target
             system/* stays admin-only (spec branching.merge). The
             SOURCE is the read side: system/* is the shared defaults
             tree (merging main into your own garden is the spec's
             "per-user can merge into their own branches freely" sync
             path), but another tenant's garden is denied — it would
             copy their code into readable merge results. *)
          if (tgt_owner = "system")
             && not (Prims_db_common.is_verified_admin ~t ()
                     || t.manifest.username = "admin")
          then
            (Pricklypear_lisp.Ast.VStr
                          "denied: admin-only merge to system branch")
          else if not (branch_readable t src_owner) then
            (Pricklypear_lisp.Ast.VStr
               (Printf.sprintf "denied: branch %s is owned by %s"
                  source_ref src_owner))
          else if not (branch_owner_authorized t tgt_owner) then
            (Pricklypear_lisp.Ast.VStr
               (Printf.sprintf "denied: branch %s is owned by %s"
                  target_ref tgt_owner))
          else
            let src_id =
              Store.lookup_branch_id_by_name
                t.store ~owner:src_owner ~name:src_short
            in
            let tgt_id =
              Store.lookup_branch_id_by_name
                t.store ~owner:tgt_owner ~name:tgt_short
            in
            (match src_id, tgt_id with
             | Some s, Some tg ->
                 let r = Store.merge_branch t.store
                            ~source_id:s ~target_id:tg
                            ~merge3:merge3_hook in
                 (match r with
                  | Some res ->
                      (Pricklypear_lisp.Ast.VStr
                                    (Store.format_merge_result res))
                  | None ->
                      (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("prim", "merge failed", "failed")))))
             | _ ->
                 (Pricklypear_lisp.Ast.VStr
                               "source or target branch not found"))
      | _ ->
          (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("merge", "bad args (need 2 strings)", "bad-args")))));

  (* (route "/path" handler [visibility]) -> VStr "/path".
     Optional third arg 'public | "public" | 'private | "private".
     Handler-only re-register keeps prior visibility (store ON CONFLICT).
     Anonymous sessions may not register routes.
     H0.3: public targets must pass the public-route lattice (same as AI
     allow minus elevate): pure|read-db|write-db|html|txn. Branch/Ai/
     Publish on a public handler is denied. Missing handler body is OK
     (define-after-route still works). *)
  let parse_vis = function
    | Pricklypear_lisp.Ast.VStr s ->
        (match String.lowercase_ascii (String.trim s) with
         | "public" -> Some "public"
         | "private" -> Some "private"
         | _ -> None)
    | _ -> None
  in
  let check_public_handler (handler : string) : (unit, string) result =
    (* Fail-closed public-route gate (BORGE-1785212463 / 1785212603).
       Infer effects from the in-memory env (synchronous, fresh) via the
       pure [Route_gate] module. The previous store read was stale after
       the define persist race and DEFAULTED TO ALLOW on unknown handlers;
       now unknown / non-function handlers are DENIED. *)
    (Pricklypear_lisp.Route_gate.check_public_handler ~env:t.eval_state.env handler)
  in
  (* bumpers M3 route-notices (borge/bumpers.borg notices-envelope /
     fragment-by-default): on a successful route publish, generate +
     buffer ONE notice — {kind:route, name:path, effects:badge, text:
     remark} — with the htmx-harness pointer (docs/agent-ui-harness.md
     tier ladder), the full-shell remark for a full-shell page, and the
     standalone silence. All of this is pure AST work — handler body read
     via [Route_gate.handler_body], effects via [Effect.infer], shape via
     [Effect.classify_route_shape] — plus session state; no tree/notes
     reads, so nothing here touches the notes-tree eval path before
     /health serves (2026-09-14 promote incident invariant). Gated on the
     same bulk load/seed window as define-notices. The ≤2-per-response
     budget is enforced at the api_eval drain; kind "route" is not a tip,
     so the session taper does not apply. *)
  let notice_route ~(path : string) ~(handler : string)
      ~(standalone : bool) : unit =
    if t.ui_shell.suppress_refresh_on_define then ()
    else
      let body =
        Pricklypear_lisp.Route_gate.handler_body t.eval_state.env handler
      in
      let effects, shape =
        match body with
        | Some b ->
            ( (try Some (Pricklypear_lisp.Effect.infer b)
               with _ -> Some Pricklypear_lisp.Effect.EffSet.empty)
            , Pricklypear_lisp.Effect.classify_route_shape b )
        | None -> (None, Pricklypear_lisp.Effect.NotHtml)
      in
      (* A standalone declaration is monotone within the session: once a
         path is declared standalone, re-publishing it without the
         annotation keeps the fragment family silenced (the declaration
         states the route IS standalone, not per-publish intent). *)
      let standalone = standalone || List.mem path t.standalone_routes in
      let candidates =
        Pricklypear_lisp.Effect.route_notice
          { Pricklypear_lisp.Effect.path; effects; shape; standalone }
      in
      let to_wire (n : Pricklypear_lisp.Effect.define_notice)
          : Wire_codec.notice =
        { Wire_codec.kind = n.kind; name = Some n.name;
          effects = n.effects; text = n.text }
      in
      t.pending_notices <- t.pending_notices @ List.map to_wire candidates
  in
  (* Route annotation (bumpers M3 fragment-by-default): a publish with
     the 'standalone annotation records the path in the session's
     standalone set (monotone — first declaration wins for the session)
     so the fragment-family remark stays silent on later re-publishes
     of the same path. Only the ANNOTATED publish records; a plain
     publish is not a declaration. *)
  let declare_standalone ~(standalone : bool) ~(path : string) : unit =
    if standalone && not (List.mem path t.standalone_routes) then
      t.standalone_routes <- path :: t.standalone_routes
  in
  (* [true] when an extra route-prim arg is the standalone annotation
     ('standalone / "standalone"). Accepted as either a string (the same
     form route visibility uses) or a quoted symbol, mirroring the route
     prim's docstring. *)
  let arg_is_standalone = function
    | Pricklypear_lisp.Ast.VStr s ->
        String.lowercase_ascii (String.trim s) = "standalone"
    | Pricklypear_lisp.Ast.VSymbol s ->
        String.lowercase_ascii (String.trim s) = "standalone"
    | _ -> false
  in
  let register_route_checked ~route_path ~handler ~visibility
      ~(pass_vis_to_store : bool) ~(standalone : bool)
    : Pricklypear_lisp.Ast.value =
    (* Publish-time gate. SKIPPED during additive boot seed: at seed the
       env holds only the kernel prim catalog (fingerprint-skipped
       defines never bind), so an env-based gate would deny every fresh
       package's (public-route …) and permanently prevent the route row
       from ever storing. The seed file set is the trusted distribution
       channel (same trust as the (define)s it carries), and the gate
       still ENFORCES at every dispatch: Session.call_handler re-runs
       check_public_handler against the FULL session env before any
       public handler runs (BORGE-1785212463 dispatch gate). *)
    let gate =
      if visibility = "public" && not t.seed_additive
      then check_public_handler handler
      else (Ok ())
    in
    (match gate with
     | Error msg -> (Pricklypear_lisp.Ast.VStr msg)
    | Ok () ->
          (* spec_delegation: delegated sessions register in-memory only -
             the view_bindings row is a code write, and delegation is for
             data, never for code. Seeded package loads keep their routes
             working in-session; nothing outlives the session. *)
          Hashtbl.replace t.routes route_path (handler, visibility);
          let _ =
            if Session_types.restricted t then ()
            else if t.seed_additive then
            if pass_vis_to_store then
              Store.register_route_if_absent t.store ~route:route_path
                ~handler ~visibility ()
            else
              Store.register_route_if_absent t.store ~route:route_path
                ~handler ()
          else if pass_vis_to_store then
            Store.register_route t.store ~route:route_path ~handler
              ~visibility ()
          else
            Store.register_route t.store ~route:route_path ~handler ()
        in
        let () = declare_standalone ~standalone ~path:route_path in
        let () = notice_route ~path:route_path ~handler ~standalone in
        (Pricklypear_lisp.Ast.VStr route_path))
  in
  Primitives.register_server_only tbl "route"
    (fun args ->
      if t.manifest.username = "anonymous" then
        (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("route", "denied for anonymous", "denied"))))
      else
        match args with
        | [ Pricklypear_lisp.Ast.VStr route_path;
            Pricklypear_lisp.Ast.VStr handler ] ->
            let vis =
              match Hashtbl.find_opt t.routes route_path with
              | Some (_, v) -> v
              | None -> "private"
            in
            (* handler-only: pass_vis_to_store=false so DB keeps visibility
               on UPDATE; memory keeps prior or private for new. *)
            register_route_checked ~route_path ~handler ~visibility:vis
              ~pass_vis_to_store:false ~standalone:false
        | [ Pricklypear_lisp.Ast.VStr route_path;
            Pricklypear_lisp.Ast.VStr handler;
            e3 ] ->
            if arg_is_standalone e3 then
              (* (route path handler 'standalone): keeps prior/private
                 visibility (pass_vis_to_store=false), silences the
                 fragment family (bumpers M3 fragment-by-default). *)
              let vis =
                match Hashtbl.find_opt t.routes route_path with
                | Some (_, v) -> v
                | None -> "private"
              in
              register_route_checked ~route_path ~handler ~visibility:vis
                ~pass_vis_to_store:false ~standalone:true
            else
              (match parse_vis e3 with
               | None ->
                   (Pricklypear_lisp.Ast.VStr
                        "route: visibility must be public, private, or standalone")
               | Some vis ->
                   register_route_checked ~route_path ~handler
                     ~visibility:vis ~pass_vis_to_store:true
                     ~standalone:false)
        | [ Pricklypear_lisp.Ast.VStr route_path;
            Pricklypear_lisp.Ast.VStr handler;
            vis_v;
            e4 ] ->
            (* (route path handler visibility 'standalone) *)
            (match parse_vis vis_v with
             | Some vis when arg_is_standalone e4 ->
                 register_route_checked ~route_path ~handler ~visibility:vis
                   ~pass_vis_to_store:true ~standalone:true
             | _ ->
                 (Pricklypear_lisp.Ast.VStr
                      "route: bad args (path handler [public|private] ['standalone])"))
        | _ ->
            (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("route", "bad args", "bad-args")))));

  (* (public-route path handler ['standalone]) sugar *)
  Primitives.register tbl "public-route"
    (fun args ->
      if t.manifest.username = "anonymous" then
        (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("public-route", "denied for anonymous", "denied"))))
      else
        match args with
        | [ Pricklypear_lisp.Ast.VStr route_path;
            Pricklypear_lisp.Ast.VStr handler ] ->
            register_route_checked ~route_path ~handler ~visibility:"public"
              ~pass_vis_to_store:true ~standalone:false
        | [ Pricklypear_lisp.Ast.VStr route_path;
            Pricklypear_lisp.Ast.VStr handler;
            e3 ] ->
            if arg_is_standalone e3 then
              register_route_checked ~route_path ~handler ~visibility:"public"
                ~pass_vis_to_store:true ~standalone:true
            else
              (Pricklypear_lisp.Ast.VStr
                   "public-route: bad args (path handler ['standalone])")
        | _ ->
            (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("public-route", "bad args", "bad-args")))));

  (* (route-visibility path) -> "public" | "private" | () *)
  Primitives.register tbl "route-visibility"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr route_path ] ->
          (match Hashtbl.find_opt t.routes route_path with
           | Some (_, vis) ->
               (Pricklypear_lisp.Ast.VStr vis)
           | None -> Pricklypear_lisp.Ast.VNil)
      | _ -> Pricklypear_lisp.Ast.VNil);

  (* (checkout 'branch-ref) -> VStr "checked out" | "no such branch".
     Mutates t.store.branch_id + t.manifest, then re-hydrates the env
     from the new branch's function_defs (sync — so the next form sees
     the right bindings). Blocked while agent_branch_lock is set. *)
  Primitives.register_server_only tbl "checkout"
    (fun args ->
      if t.manifest.username = "anonymous" then
        (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("checkout", "denied for anonymous", "denied"))))
      else if t.agent_branch_lock then
        (Pricklypear_lisp.Ast.VStr
             "checkout blocked: agent is locked to the current branch")
      else
        match args with
        | [ Pricklypear_lisp.Ast.VStr branch_ref ] ->
            let owner, name = split_branch_ref branch_ref in
            (* i-1787970640: owner of the target ref must pass the
               branch-owner rule (system/* included — non-admin users
               never need main; personal branches inherit it). *)
            if not (branch_owner_authorized t owner) then
              (Pricklypear_lisp.Ast.VStr
                 (Printf.sprintf "denied: branch %s is owned by %s"
                    branch_ref owner))
            else
            let found =
              Store.lookup_branch_id_by_name t.store ~owner ~name
            in
            (match found with
             | None -> raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("checkout", "no such branch", "not-found")))
             | Some new_id ->
                 let () = (hooks.checkout_to) t ~branch_id:new_id ~branch_ref in
                 (Pricklypear_lisp.Ast.VStr "checked out"))
        | _ ->
            (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("checkout", "bad args", "bad-args")))));

  (* (unbind name) -> VStr "unbound" | "nothing to unbind" | error.
     Deletes ONLY the current branch's function_defs row for name;
     the name then resolves via inheritance to the shadowed parent
     binding (or becomes unbound). Refuses on system/main and on
     anonymous sessions. See branching.unbind spec. *)
  Primitives.register tbl "unbind"
    (fun args ->
      if t.manifest.username = "anonymous" then
        (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("unbind", "denied for anonymous", "denied"))))
      else
        (* i-1787970640: the current branch's owner must pass the
           branch-owner rule (delete_function already refuses system/main
           and the prim refuses anonymous). *)
        let cur_owner, _cur_name =
          split_branch_ref t.manifest.branch_name in
        if not (branch_owner_authorized t cur_owner) then
          (Pricklypear_lisp.Ast.VStr
             (Printf.sprintf "denied: branch %s is owned by %s"
                t.manifest.branch_name cur_owner))
        else
        match args with
        | [ Pricklypear_lisp.Ast.VStr name ] ->
            (try
              let deleted = Store.delete_function t.store name in
              if deleted then begin
                (* Rehydrate so the env sees the revealed parent binding *)
                (hooks.rehydrate) t;
                (Pricklypear_lisp.Ast.VStr "unbound")
              end else
                (Pricklypear_lisp.Ast.VStr "nothing to unbind")
            with Failure msg ->
              (Pricklypear_lisp.Ast.VStr msg))
        | _ ->
            (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("unbind", "need a name", "bad-args")))));

  (* (prune-synced) -> VStr JSON {"pruned": N, "names": [...]}.
     branching.pin-hygiene: unbinds every synced FN pin on the current
     branch (body canonically identical to the parent chain — a
     contentless pin). ahead/ahead-new/diverged pins untouched.
     Denied for anonymous; system/main is a no-op (a root pin can never
     be synced). Same branch-owner rule as unbind. *)
  Primitives.register tbl "prune-synced"
    (fun args ->
      if t.manifest.username = "anonymous" then
        (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("prune-synced", "denied for anonymous", "denied"))))
      else
        (match args with
         | [] ->
             let cur_owner, _cur_name =
               split_branch_ref t.manifest.branch_name in
             if not (branch_owner_authorized t cur_owner) then
               (Pricklypear_lisp.Ast.VStr
                  (Printf.sprintf "denied: branch %s is owned by %s"
                     t.manifest.branch_name cur_owner))
             else if t.store.Store_db.branch_id = Store.system_main_id then
               (Pricklypear_lisp.Ast.VStr
                  (Yojson.Safe.to_string
                     (`Assoc [ "pruned", `Int 0;
                               "note", `String "system/main: nothing can be synced" ])))
             else
               let names = Store.prune_synced_fn_pins t.store in
               let () = if names <> [] then (hooks.rehydrate) t in
               Pricklypear_lisp.Ast.VStr
                 (Yojson.Safe.to_string
                    (`Assoc
                      [ "pruned", `Int (List.length names);
                        "names",
                          `List
                            (List.map (fun n -> `String n)
                               (List.rev names)) ]))
         | _ ->
             (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("prune-synced", "no args", "bad-args"))))));

  (* (branch/status ref) -> VStr JSON {branch, counts{6 statuses},
     diverged[{kind,name}], pins[{kind,name,status,own}]}, or a plain
     VStr denial / "no such branch: ref".

     branching-ast-ergonomics branch-ops status row: per-pin classify +
     counts + the divergent worklist, rendered FROM THE STORE
     (Store.branch_status → Store.status_report) — the ONE classifier,
     never the session snapshot (2026-10-05/06 stale-counter incidents;
     /branches debt). Read-side authorization (branch_readable): system/*
     is the shared defaults tree; another tenant's garden metadata is
     denied. Denied for anonymous, like every branch prim. Server-only
     (ref-addressed, fork/merge/checkout family). *)
  Primitives.register_server_only tbl "branch/status"
    (fun args ->
      if t.manifest.username = "anonymous" then
        (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("branch/status", "denied for anonymous", "denied"))))
      else
        match args with
        | [ Pricklypear_lisp.Ast.VStr branch_ref ] ->
            let owner, short = split_branch_ref branch_ref in
            if not (branch_readable t owner) then
              (Pricklypear_lisp.Ast.VStr
                 (Printf.sprintf "denied: branch %s is owned by %s"
                    branch_ref owner))
            else
              (match
                 Store.lookup_branch_id_by_name t.store ~owner ~name:short
               with
               | None ->
                   (Pricklypear_lisp.Ast.VStr
                      (Printf.sprintf "no such branch: %s" branch_ref))
               | Some branch_id ->
                   let rows = Store.branch_status t.store ~branch_id in
                   let rep = Store.status_report rows in
                   let row_json (r : Store.branch_status_row) :
                       Yojson.Safe.t =
                     `Assoc
                       [ ("kind",
                          `String
                            (match r.Store.st_kind with
                             | Store.DiffFn -> "fn"
                             | Store.DiffType -> "type"
                             | Store.DiffRoute -> "route"))
                       ; ("name", `String r.Store.st_name)
                       ; ("status", `String (Store.st_status_s r.Store.st_status))
                       ; ("own", `Bool r.Store.st_own)
                       ]
                   in
                   Pricklypear_lisp.Ast.VStr
                     (Yojson.Safe.to_string
                        (`Assoc
                          [ ( "branch",
                              `String (owner ^ "/" ^ short) )
                          ; ( "counts",
                              `Assoc
                                (List.map
                                   (fun (s, n) -> (s, `Int n))
                                   rep.Store.counts) )
                          ; ( "diverged",
                              `List (List.map row_json rep.Store.worklist) )
                          ; ( "pins",
                              `List (List.map row_json rep.Store.pins) )
                          ])))
        | _ ->
            (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("branch/status", "bad args (need 1 branch ref)", "bad-args")))));

  (* (branch/pull ref) -> VStr JSON {pulled:[{kind,name}],
     skipped:[{kind,name}]} (skipped = diverged pins left alone), or a
     plain VStr denial / "no such branch: ref"; system/main answers
     {pulled:[],skipped:[],note:"system/main: no parent"}.

     branching-ast-ergonomics branch-ops pull row: every BEHIND pin on
     the ref adopts the parent's version via Store.pull_behind_pins —
     the UNBIND path (per-kind delete), never a body copy (pin hygiene);
     diverged pins untouched (obituary rule). Write-side authorization
     (branch_owner_authorized — pull mutates the ref's rows, same rule
     as unbind/prune-synced on the current branch). If the ref IS the
     session's current branch, rehydrate so the env sees the revealed
     parent bindings. Denied for anonymous; not gated on
     agent_branch_lock (same class as unbind/prune-synced: syncing a
     branch with its parent is not a branch-escape). *)
  Primitives.register_server_only tbl "branch/pull"
    (fun args ->
      if t.manifest.username = "anonymous" then
        (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("branch/pull", "denied for anonymous", "denied"))))
      else
        match args with
        | [ Pricklypear_lisp.Ast.VStr branch_ref ] ->
            let owner, short = split_branch_ref branch_ref in
            if not (branch_owner_authorized t owner) then
              (Pricklypear_lisp.Ast.VStr
                 (Printf.sprintf "denied: branch %s is owned by %s"
                    branch_ref owner))
            else
              (match
                 Store.lookup_branch_id_by_name t.store ~owner ~name:short
               with
               | None ->
                   (Pricklypear_lisp.Ast.VStr
                      (Printf.sprintf "no such branch: %s" branch_ref))
               | Some branch_id ->
                   if branch_id = Store.system_main_id then
                     (Pricklypear_lisp.Ast.VStr
                        (Yojson.Safe.to_string
                           (`Assoc
                             [ ("pulled", `List [])
                             ; ("skipped", `List [])
                             ; ( "note",
                                 `String "system/main: no parent" )
                             ]))
                        )
                   else
                     let pulled, skipped =
                       Store.pull_behind_pins t.store ~branch_id
                     in
                     let pair_json (kind, name) : Yojson.Safe.t =
                       `Assoc [ ("kind", `String kind);
                                ("name", `String name) ]
                     in
                     let () =
                       if pulled <> []
                          && t.store.Store_db.branch_id = branch_id
                       then (hooks.rehydrate) t
                     in
                     Pricklypear_lisp.Ast.VStr
                       (Yojson.Safe.to_string
                          (`Assoc
                            [ ( "pulled",
                                `List (List.map pair_json pulled) )
                            ; ( "skipped",
                                `List (List.map pair_json skipped) )
                            ])))
        | _ ->
            (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("branch/pull", "bad args (need 1 branch ref)", "bad-args")))));

    (* (branch/push ref [name...]) -> VStr JSON {branch, pushed:[{kind,name}],
       auto-included:[{kind,name}]}, or raise. branch-ops push row — the
       promote_to_personal generalization: the ref's OWN code (or the named
       subset) promotes onto <ref-owner>/personal, DEPENDENCY CLOSURE FIRST
       (Branch_push over Call_graph.push_closure): a pin referencing a name
       unbound on the target is auto-included from the source's own rows, or
       the push is REFUSED with the exact hole list — never a partial
       landing (2026-10-05 incident: pulse-row-from promoted without
       workstreams/stale-node? -> system/main 500).

       Callers: the ref's owner human (their garden — the branch-faces
       decision flow), an agent user whose PARENT owns the ref workshop
       (gated by the parent's allow_agent_promote, same gate as promote),
       or admin. system/* is refused: promote-to-main stays the
       admin-gated path. Not gated on agent_branch_lock (same class as
       promote: the branch escape is authorized by the target owner's
       toggle, not the delegated-session checkout lock). Server-only
       (ref-addressed, fork/merge/checkout family). *)
    Primitives.register_server_only tbl "branch/push"
      (fun args ->
        if t.manifest.username = "anonymous" then
          (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("branch/push", "denied for anonymous", "denied"))))
        else
          let names_of_val = function
            | Pricklypear_lisp.Ast.VStr s -> [ s ]
            | Pricklypear_lisp.Ast.VSymbol s -> [ s ]
            | Pricklypear_lisp.Ast.VList xs ->
                List.filter_map
                  (function
                    | Pricklypear_lisp.Ast.VStr s -> Some s
                    | Pricklypear_lisp.Ast.VSymbol s -> Some s
                    | _ -> None)
                  xs
            | _ -> []
          in
          match args with
          | Pricklypear_lisp.Ast.VStr branch_ref :: rest ->
              let owner, short = split_branch_ref branch_ref in
              let u = Prims_db_common.caller_username ~t () in
              let ledger ~ok =
                ignore
                  (Activity_ledger.push ~actor:u ~cls:"promote.attempt"
                     ~target:
                       (Activity_ledger.path_target
                          (Printf.sprintf "%s->%s/personal" branch_ref owner))
                     ~outcome:(if ok then "ok" else "denied")
                     ())
              in
                let deny (why : string) =
                ledger ~ok:false;
                (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("branch/push", why, "denied"))))
              in
                let failed (why : string) =
                ledger ~ok:false;
                (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("branch/push", why, "failed"))))
              in
              (if owner = "system" then
                 deny "system/* branches land via promote-to-main (admin)"
               else
                 (* authorization BEFORE any branch lookup (fork rule: a
                    denial never leaks whether the ref exists) *)
                 match Auth_store.get_user_by_username t.store u with
                 | Some caller when caller.Auth_store.user_type = "agent" ->
                     (match caller.Auth_store.parent_user_id with
                      | None -> deny "agent has no parent"
                      | Some pid ->
                          (match Auth_store.get_user_by_id t.store pid with
                           | None -> deny "parent user not found"
                           | Some parent ->
                               if parent.Auth_store.username <> owner then
                                 deny
                                   (Printf.sprintf
                                      "denied: branch %s is owned by %s"
                                      branch_ref owner)
                               else if
                                 not
                                   (Auth_store.get_allow_agent_promote
                                      t.store
                                      ~user_id:parent.Auth_store.id)
                               then
                                 deny "agent promote disabled by parent"
                               else ()))
                 | Some _ ->
                     (* human caller: own garden or verified admin —
                        branch_owner_authorized covers both plus the
                        documented admin bootstrap valve *)
                     if not (branch_owner_authorized t owner) then
                       deny
                         (Printf.sprintf
                            "denied: branch %s is owned by %s"
                            branch_ref owner)
                     else ()
                 | None ->
                     (* no users row (bootstrap image): the admin textual
                        valve only, the same fallback the family keeps *)
                     if
                       u <> "admin"
                       && not (Prims_db_common.is_verified_admin ~t ())
                     then
                       deny
                         (Printf.sprintf
                            "denied: branch %s is owned by %s"
                            branch_ref owner)
                     else ());
              let source_id =
                Store.lookup_branch_id_by_name t.store ~owner ~name:short
              in
              let target_id =
                Store.lookup_branch_id_by_name t.store ~owner ~name:"personal"
              in
              (match (source_id, target_id) with
               | None, _ -> failed ("no such branch: " ^ branch_ref)
               | _, None ->
                   failed
                     (Printf.sprintf "no such branch: %s/personal" owner)
               | Some source_id, Some target_id ->
                   let names = List.concat_map names_of_val rest in
                   let kernel = List.map fst t.kernel_catalog in
                   (match
                      Branch_push.push t.store ~source_id ~target_id ~names
                        ~kernel
                    with
                    | Branch_push.Push_ok { pushed; auto } ->
                        ledger ~ok:true;
                        let () =
                          if Store.current_branch_id t.store = target_id
                          then (hooks.rehydrate) t
                          else ()
                        in
                        let row_json (n : string) : Yojson.Safe.t =
                          `Assoc
                            [ ("kind", `String "fn")
                            ; ("name", `String n) ]
                        in
                        (Pricklypear_lisp.Ast.VStr
                           (Yojson.Safe.to_string
                              (`Assoc
                                 [ ("branch", `String (owner ^ "/personal"))
                                 ; ("pushed", `List (List.map row_json pushed))
                                 ; ( "auto-included",
                                     `List (List.map row_json auto) )
                                 ])))
                    | Branch_push.Push_holes holes ->
                        failed
                          ("refused, unresolved deps: "
                          ^ Branch_push.holes_msg holes)
                    | Branch_push.Push_refused why ->
                        failed why))
          | _ ->
              (raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("branch/push", "need 1 branch ref (+ optional name strings)", "bad-args")))));

  (* (bench expr-string [n]) -> VFloat average microseconds per iteration.
     Parses once, warms up once, runs n times (default 1000) under a max
     budget so the bench loop itself never hits Budget_exceeded. Uses the
     current session env. Prefer pure expressions (side-effecting bodies
     will run for real). *)
  let bench_run (expr_str : string) (n : int) : Pricklypear_lisp.Ast.value =
    let n = max 1 n in
    let e =
      try Pricklypear_lisp.Reader.read_one expr_str
      with exn ->
        raise
          (Pricklypear_lisp.Ast.Interp_error
             (Pricklypear_lisp.Ast.User_error
                ("bench: parse failed: " ^ Printexc.to_string exn, "bad-args")))
    in
    let run_once () =
      let budget =
        Pricklypear_lisp.Eval.make_budget ~limit:max_int ()
      in
      Control_rim.run ~prims:t.eval_state.prims (fun () ->
          Pricklypear_lisp.Eval.eval
            ~budget ~prims:Pricklypear_lisp.Eval.empty_prims t.eval_state.env e)
    in
    (* warmup (discard) *)
    let _ = run_once () in
    let t0 = Unix.gettimeofday () in
    let rec loop i =
      if i >= n then ()
      else
        let _ = run_once () in
        loop (i + 1)
    in
    let () = loop 0 in
    let t1 = Unix.gettimeofday () in
    let avg_us = ((t1 -. t0) *. 1_000_000.) /. float_of_int n in
    (Pricklypear_lisp.Ast.VFloat avg_us)
  in
  Primitives.register tbl "bench"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr s ] -> bench_run s 1000
      | [ Pricklypear_lisp.Ast.VStr s; Pricklypear_lisp.Ast.VInt n ] ->
          bench_run s (Int64.to_int n)
      | _ ->
          (Pricklypear_lisp.Ast.VStr
               "bench: need (bench expr-string [iterations])"));

  (* Private prim used by the with-transaction special form in eval.ml.
     NOT USER-VISIBLE: it's looked up under name __with_transaction__ by
     the special form handler, never `(with-transaction-call ...)` from Lisp.
     Also bound into the env as a VPrim name by the auto-bind loop, but
     a `(with-transaction-call ...)` call would just route through here.
     The prim receives one arg: a VLambda value. It wraps Eval.apply
     inside Store.with_dynamic_transaction, so prims called from the
     body of the lambda see the in-transaction pgx conn via
     Store.current_conn. Lands it29, option A from BORGE-1783438027.

     Returns the lambda body's result. Nested transactions RIDE the
     outer transaction: with_dynamic_transaction joins an active tx
     instead of raising (i-job-tx-wedge), so the inner body commits
     (or rolls back) with the outer. *)
  (* Apply a VLambda under the effect rim (H1.7). *)
  let apply_lambda_auto (lam : Pricklypear_lisp.Ast.value) : Pricklypear_lisp.Ast.value =
    let budget = Pricklypear_lisp.Eval.make_budget () in
    Control_rim.run ~prims:t.eval_state.prims (fun () ->
        Pricklypear_lisp.Eval.apply
          ~budget ~prims:Pricklypear_lisp.Eval.empty_prims lam [])
  in
  Primitives.register tbl "__with_transaction__"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VLambda (params, body, clo_env) as lam ] ->
          (* Enforce zero-arg lambda for clarity: with-transaction bodies
             shouldn't take args. *)
          let _ = params in
          (* i-1788669886: a with-transaction body that blocks (sleep /
             http-* / tcp streams / llm calls) holds the pool conn + the
             SERIALIZABLE txn open for that duration — one request can
             stall every other tenant behind the single per-connection
             Eio mutex. Infer the body's effects (resolving helper fns
             through the closure env) and refuse Net/Ai bodies up front. *)
          let known_fn =
            Pricklypear_lisp.Effect.known_fn_of_bindings
              (Pricklypear_lisp.Route_gate.env_lambda_bindings clo_env)
          in
          let fx = Pricklypear_lisp.Effect.infer ~known_fn body in
          if Pricklypear_lisp.Effect.EffSet.mem Pricklypear_lisp.Effect.Net fx
             || Pricklypear_lisp.Effect.EffSet.mem Pricklypear_lisp.Effect.Ai fx then
            (raise (Pricklypear_lisp.Ast.Interp_error
                      (Pricklypear_lisp.Ast.User_error
                         (Printf.sprintf
                            "with-transaction body must not block or call the model (effects: %s) — it holds the pool connection + transaction open for every other tenant (i-1788669886)"
                            (Pricklypear_lisp.Effect.EffSet.pp fx),
                          "denied"))));
          (* H1.3: body re-enters apply_eff under Control.run when
             PP_EVAL_CONTROL=eff so nested prims Prim_call while seeing
             Store.current_conn from with_dynamic_transaction. *)
          Io_db.Db.with_dynamic_transaction t.store (fun () ->
              apply_lambda_auto lam)
      | _ ->
          (Pricklypear_lisp.Ast.VStr
                        "__with_transaction__: bad args (need a lambda)"));

  (* Private prim used by the with-step special form in eval.ml. NOT
     USER-VISIBLE: routed to under name __step__ by eval.ml, never
     `(with-step "name" (lambda ...))` from a plain prim call.

     (__step__ "name" <lambda>) is a checkpoint boundary (borge/jobs.borg
     step-checkpoints):
       - Outside a job (Jobs.current_job_id_opt () = None, e.g. an HTTP
         request or REPL) it is a PLAIN thunk call — no checkpoint.
       - Inside a job (the runner binds the job id around eval), the
         thunk's result is memoized under (job_id, name): first execution
         runs the thunk and writes a job_steps row; re-execution (a retry)
         returns the stored value WITHOUT re-running the thunk.

     Exactly-once with (with-transaction (step …)): the checkpoint INSERT
     rides Store.current_conn — when the caller is inside
     with_transaction, the insert lands in the SAME transaction as the
     thunk's writes, so it commits or rolls back atomically with them (a
     failed/later-raised step leaves no checkpoint; a retry re-runs). If
     the THUNK ITSELF raises, no checkpoint is written (the exception
     propagates, rolling back the surrounding transaction if any). *)
  Primitives.register tbl "__step__"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr name;
          Pricklypear_lisp.Ast.VLambda (_params, _body, _clo) as lam ] ->
          (match Jobs.current_job_id_opt () with
           | None ->
               (* Outside a job: a plain thunk call with no checkpoint. *)
               apply_lambda_auto lam
           | Some job_id ->
               (match
                  Jobs.checkpoint_lookup ~store:t.store ~job_id ~name
                with
                | Some value_json ->
                    (* Memoized: return the stored value without re-running. *)
                    (try
                       Prims_pure.json_to_value
                         (Yojson.Safe.from_string value_json)
                     with _ ->
                       Pricklypear_lisp.Ast.VStr value_json)
                | None ->
                    (* First execution in this job: run the thunk, then write
                       the checkpoint. If the thunk raises, the exception
                       propagates (no checkpoint, surrounding tx rolls back). *)
                    let result = apply_lambda_auto lam in
                    let value_json =
                      Yojson.Safe.to_string (Prims_pure.value_to_json result)
                    in
                    Jobs.checkpoint_write ~store:t.store ~job_id ~name
                      ~value:value_json;
                    result))
      | _ ->
          (Pricklypear_lisp.Ast.VStr
                        "__step__: bad args (need name-str, lambda)"));

  (* (with-public-write (lambda () body...)) — run body with write privilege
     so public route handlers can create data_rows / notes captures.
     Elevates anonymous only (logged-in sessions already write). Restores
     identity afterward. Prefer for narrow public forms (signup), not general
     unauth APIs.

     Elevates BOTH the live session manifest AND the per-request context
     manifest. CRUD prims (create/deftype) read their anon guard from
     Request_context.manifest (the gate-time snapshot, per BORGE-1784434193
     cotenant fix); without elevating the snapshot, the guard sees the stale
     "anonymous" and silently denies (create returns a VStr, seq discards it,
     the handler renders its thanks page with no row written). *)
  Primitives.register tbl "with-public-write"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VLambda (_params, _body, _clo) as lam ] ->
          let prev = t.manifest in
          let need_elevate = prev.username = "anonymous" in
          if need_elevate then
            t.manifest <-
              { prev with user_id = "public"; username = "public" };
          let run_body =
            match Request_context.get () with
            | Some ctx when need_elevate ->
                let elevated_manifest =
                  { (Request_context.manifest ctx) with
                    user_id = "public"; username = "public" }
                in
                let elevated_ctx =
                  Request_context.make
                    ~manifest:elevated_manifest
                    ~env:(Request_context.env ctx)
                    ~is_admin:(Request_context.is_admin ctx)
                    ~routes:(Request_context.routes ctx)
                in
                Request_context.with_current elevated_ctx
                  (fun () -> apply_lambda_auto lam)
            | _ -> apply_lambda_auto lam
          in
          (Fun.protect ~finally:(fun () -> if need_elevate then t.manifest <- prev) (fun () -> run_body))
      | _ ->
          (Pricklypear_lisp.Ast.VStr
               "with-public-write: bad args (need a zero-arg lambda)"))


