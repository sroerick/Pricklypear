(* Local-first sync / data-lens prims - split out of prims_db.ml (i-1788671035-8 fix pass).

   Bodies are verbatim relocations from the old single register closure;
   shared closure helpers come from Prims_db_common, re-bound below so the
   moved code stays byte-identical. *)
open Session_types

let register (t : t) (tbl : Primitives.t) : unit =
  let actor () = Prims_db_common.actor ~t () in
  let deny_anon name = Prims_db_common.deny_anon name in
  let register_crud name fn = Prims_db_common.register_crud ~tbl name fn in

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
         LWW-ARBITRATED per sync-conflicts v0: the client envelope wins iff
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
  (* auth.user-llm-keys 5b — the sync round exports the CALLER's OWN
     llm credential as round metadata (never a datum, never another
     user's). API key + base only: model is a per-surface dial
     (provider ABI), deliberately not replicated. Agent users inherit
     the parent human's key via get_llm_key_secret's fallback. *)
  let llm_config_field () : (string * Yojson.Safe.t) list =
    let uname = actor () in
    match Auth_store.get_user_by_username t.store uname with
    | None -> []
    | Some u ->
        (match Auth_store.get_llm_key_secret t.store ~user_id:u.Auth_store.id with
         | None -> []
         | Some (key, meta) ->
             let base =
               match meta.Auth_store.base with
               | Some b when b <> "" -> b
               | _ -> Llm_client.api_base ()
             in
             [ ( "llm_config"
               , `Assoc
                   [ ("api_key", `String key)
                   ; ("base", `String base)
                   ])
             ])
  in
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
        (* sync-conflicts v0: LWW-arbitrate diverge pairs. Client strictly
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
                    (* 2. Diverge pairs LWW-arbitrate (sync-conflicts v0). *)
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
                        (`Assoc (
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
                           ]
                          @ (llm_config_field ())))
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
                        (`Assoc (
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
                           ]
                          @ (llm_config_field ())))
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
