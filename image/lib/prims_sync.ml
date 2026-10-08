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
  (* replication-unit prefixes (borge/local-first.borg replication-units,
     data.s.<space>.* PLANNED component): the caller's data.u subtree plus
     every member-space data.s.<space> subtree under the data.shared PULL +
     PROPOSE policy — spaces are never in the lens's write scope, so these
     prefixes drive the PULL side; client-driven space writes are rejected
     in Sync.commit. *)
  let sync_unit_prefixes () =
    let uname = actor () in
    ("data.u." ^ Tree_store.enc_label uname)
    :: Sync.member_space_prefixes t.store ~user:uname
  in
  register_crud "sync/plan"
    (fun args ->
      match args with
      | [ cm; b ] ->
          (* three-way: client manifest + base (last-synced) manifest *)
          (match parse_manifest cm, parse_manifest b with
           | Some c, Some base ->
               let prefixes = sync_unit_prefixes () in
               (match Sync.server_manifest t.store ~prefixes with
                | None -> ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("sync/plan", "no DB", "failed")))))
                | Some server ->
                    let plans = Sync.plan_of base server c in
                    (Pricklypear_lisp.Ast.VStr (Sync.json_of_plan plans)))
           | _ -> ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("sync/plan", "manifests must be [[path, hash], …]", "bad-args"))))))
      | [ cm ] ->
          (* two-way classify: client manifest vs server, no base *)
          (match parse_manifest cm with
           | Some c ->
               let prefixes = sync_unit_prefixes () in
               (match Sync.server_manifest t.store ~prefixes with
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
            (fun (won, kept, fails) (p, _) ->
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
                    | Ok () -> (p :: won, kept, fails)
                    | Error msg ->
                        (* the client won LWW but the apply was REJECTED — a
                           failed push, not a server win; surface it so the
                           client keeps + re-pushes instead of converging *)
                        (won, kept, (p, msg) :: fails))
                | None -> (won, p :: kept, fails)
              else (won, p :: kept, fails))
            ([], [], []) diverge
        in
      match args with
      | [ cm; bodies ] ->
          (match parse_manifest cm, bodies with
           | Some c, _ ->
               let prefixes = sync_unit_prefixes () in
               (match Sync.server_manifest t.store ~prefixes with
                | None ->
                    ((raise (Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Prim_error ("sync/push", "no DB", "failed")))))
                | Some server ->
                    let cb = parse_bodies bodies in
                    let r = Sync.classify server c in
                    (* 1. ClientOnly pushes (server missing the node) —
                       no arbitration needed, the server has no copy. *)
                      let applied_co, co_fails =
                        List.fold_left
                          (fun (n, fails) (p, _) ->
                             match List.assoc_opt p cb with
                             | Some env ->
                                 (match Sync.apply_client_node t.store ~env with
                                  | Ok () -> (n + 1, fails)
                                  | Error msg -> (n, (p, msg) :: fails))
                             | None ->
                                 (n, (p, "no client body for planned path") :: fails))
                          (0, []) r.client_only
                      in
                    (* 2. Diverge pairs LWW-arbitrate (sync-conflicts v0). *)
                      let lww_won, lww_kept, lww_fails =
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
                             ; ("failed_paths",
                                `List (List.map
                                         (fun (p, _) -> `String p)
                                         (List.rev (co_fails @ lww_fails))))
                             ; ("failed_reasons",
                                `Assoc (List.map
                                          (fun (p, m) -> (p, `String m))
                                          (List.rev (co_fails @ lww_fails))))
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
               let prefixes = sync_unit_prefixes () in
               (match Sync.server_manifest t.store ~prefixes with
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
                      match Sync.server_manifest t.store ~prefixes with
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
                             ; ("failed_paths",
                                `List (List.map (fun p -> `String p)
                                         (List.rev report.failed)))
                             ; ("failed_reasons",
                                `Assoc (List.map
                                          (fun (p, m) -> (p, `String m))
                                          (List.rev report.failed_reasons)))
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


  (* ---- delta exchange (borge/local-first.borg sync, chunked protocol,
     2026-09-11; issue i-1789178161-87d0742f) ----

     The v1 sync/push carries the client's FULL (path,hash) manifest (plus
     bodies) every round — O(subtree) on the wire, 413-bound past the
     edge proxy's ~1MB request cap, and unchunkable (a subset manifest
     would classify its absent paths as ServerOnly and inflate the pull
     list). The delta exchange stores the per-client last-synced merged
     manifest SERVER-SIDE — the merge index the spec already assigns to
     the server (sync_bases, migration 032; git's remote-tracking ref in
     miniature) — and negotiates deltas against it:

       sync/plan2  (lens delta-manifest deleted-paths)
           read-only negotiate: reconstruct the client's full manifest
           from the stored base + delta (merge_delta), run the SAME
           plan_of three-way as v1, and return pull_paths (ours_add /
           ours_mod), client_delete_paths (ours_del), diverge_paths
           (conflict previews — bodies + LWW happen at push2), and the
           generation. O(changes) request.
       sync/push2  (lens changes-bodies deleted-paths)
           apply ONE chunk of the delta: the same plan_of over the same
           stored base (chunks compose — every call re-derives theirs
           from the stored base, per-path apply is idempotent), commit
           the chunk-scoped outcomes (Theirs_add/Mod/Del + Conflict LWW
           with the chunk's bodies). Repeatable until the client's
           bodies are all sent. O(chunk) request.
       sync/commit2 (lens)
           end-of-round: store the CURRENT merged server manifest +
           generation as this (user, lens)'s next base. The client
           calls it once, after all pushes and pulls succeed; a crashed
           round never commits, so the next round re-plans against the
           old base (client deltas are idempotent on retry).

     A client with no stored base (fresh lens, or pre-032 server) gets a
     structured "no stored base" error and falls back to the v1 full
     exchange, then commits — so old clients keep working and new
     clients bootstrap themselves onto the delta protocol.

     THE BASE IS NEGOTIATION STATE, NOT CONTENT — one row per (user,
     lens) in its own table (032), never a tree datum, never in a
     manifest, never a snapshot. Multi-device is the star topology per
     lens-model: each device lens keys its OWN base row, so two devices
     of one user each negotiate against what THEY last had, and the
     server tree stays the sole content authority. *)

  (* the caller's uuid — the sync_bases key is per-identity so a user can
     only ever read/write their own base rows *)
  let sync_user_id () =
    let uname = actor () in
    match Auth_store.get_user_by_username t.store uname with
    | Some u -> u.Auth_store.id
    | None ->
        ((raise
            (Pricklypear_lisp.Ast.Interp_error
               (Pricklypear_lisp.Ast.Prim_error
                  ("sync", "no user row for caller", "failed")))))
  in
  (* same body-pair shape as the v1 sync/push prim: [[path, envelope-json], …] *)
  let parse_delta_bodies (body_pairs : Pricklypear_lisp.Ast.value)
      : (string * Yojson.Safe.t) list =
    match body_pairs with
    | Pricklypear_lisp.Ast.VList pairs ->
        List.filter_map
          (function
            | Pricklypear_lisp.Ast.VList
                [ Pricklypear_lisp.Ast.VStr p; Pricklypear_lisp.Ast.VStr env ] ->
                (try Some (p, Yojson.Safe.from_string env) with _ -> None)
            | _ -> None)
          pairs
    | _ -> []
  in
  let parse_path_list (v : Pricklypear_lisp.Ast.value) : string list =
    match v with
    | Pricklypear_lisp.Ast.VList items ->
        List.filter_map
          (function Pricklypear_lisp.Ast.VStr p -> Some p | _ -> None)
          items
    | _ -> []
  in
  let base_or_raise ~(prim : string) ~(lens : string) =
    let user_id = sync_user_id () in
    match Sync.load_base t.store ~user_id ~lens with
    | Some (base, branch, gen) -> (user_id, base, branch, gen)
    | None ->
        ((raise
            (Pricklypear_lisp.Ast.Interp_error
               (Pricklypear_lisp.Ast.Prim_error
                  ( prim
                  , "no stored base for this lens — run the v1 full "
                    ^ "exchange (sync/push) then sync/commit2"
                  , "no-stored-base" )))))
  in
  (* the shared server half of a delta round: stored base + current
     server manifest + the client's manifest reconstructed from its
     delta, ready for plan_of *)
  let delta_theirs ~(lens : string) ~(delta : Sync.manifest)
      ~(deleted : string list) =
    let _, base, _, _ = base_or_raise ~prim:"sync" ~lens in
    let prefixes = sync_unit_prefixes () in
    match Sync.server_manifest t.store ~prefixes with
    | None ->
        ((raise
            (Pricklypear_lisp.Ast.Interp_error
               (Pricklypear_lisp.Ast.Prim_error ("sync", "no DB", "failed")))))
    | Some server ->
        let theirs = Sync.merge_delta ~base ~changes:delta ~deleted in
        (base, server, theirs)
  in

  (* sync/plan2 — read-only negotiate (no apply, no base advance).
     (sync/plan2 lens delta-manifest deleted-paths) -> JSON
       {mode:"delta", generation, pull_paths, client_delete_paths,
        diverge_paths, theirs_add, theirs_mod, theirs_del} *)
  register_crud "sync/plan2"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr lens; delta; deleted ] ->
          let delta_m =
            match parse_manifest delta with
            | Some m -> m
            | None ->
                ((raise
                    (Pricklypear_lisp.Ast.Interp_error
                       (Pricklypear_lisp.Ast.Prim_error
                          ( "sync/plan2"
                          , "delta must be [[path, hash], …]"
                                              , "bad-args" )))))
          in
          let deleted = parse_path_list deleted in
          let _, base, _, base_gen =
            base_or_raise ~prim:"sync/plan2" ~lens in
          let prefixes = sync_unit_prefixes () in
          let server =
            match Sync.server_manifest t.store ~prefixes with
            | Some sv -> sv
            | None ->
                ((raise
                    (Pricklypear_lisp.Ast.Interp_error
                       (Pricklypear_lisp.Ast.Prim_error
                          ("sync/plan2", "no DB", "failed")))))
          in
          let theirs = Sync.merge_delta ~base ~changes:delta_m ~deleted in
          let plans = Sync.plan_of base server theirs in
          let actionable = Sync.actionable plans in
          let paths_of_outcome ~(over : Sync.plan list)
              (tag : Sync.outcome) : string list =
            List.filter_map
              (fun { Sync.path; outcome } ->
                if outcome = tag then Some path else None)
              over
          in
          (* the pull list is OURS_KEEP over the FULL plan — actionable
             deliberately excludes Ours_keep (nothing to transfer
             server-side), but ours_add/ours_mod are exactly the rows
             the client must pull; deriving them from the actionable
             subset silently emptied the pull list and stranded
             server-side rows (the missing-pull bug, 2026-09-11) *)
          let pull_paths =
            paths_of_outcome ~over:plans Sync.Ours_keep in
          let client_deletes =
            paths_of_outcome ~over:actionable Sync.Ours_del in
          let diverge =
            List.filter_map
              (fun { Sync.path; outcome } ->
                match outcome with Sync.Conflict _ -> Some path | _ -> None)
              actionable
          in
          let theirs_add =
            List.length (paths_of_outcome ~over:actionable Sync.Theirs_add) in
          let theirs_mod =
            List.length (paths_of_outcome ~over:actionable Sync.Theirs_mod) in
          let theirs_del =
            List.length (paths_of_outcome ~over:actionable Sync.Theirs_del) in
          let bid = Store.current_branch_id t.store in
          let gen =
            match Store.current_generation t.store ~branch_id:bid with
            | Some n -> n
            | None -> 0L
          in
          (Pricklypear_lisp.Ast.VStr
             (Yojson.Safe.to_string
                (`Assoc
                   [ ("mode", `String "delta")
                   ; ("generation", `String (Int64.to_string gen))
                   ; ( "base_generation"
                     , `String (Int64.to_string base_gen) )
                   ; ("pull_paths", `List (List.map (fun p -> `String p) pull_paths))
                   ; ( "client_delete_paths"
                     , `List (List.map (fun p -> `String p) client_deletes) )
                   ; ("diverge_paths", `List (List.map (fun p -> `String p) diverge))
                   ; ("theirs_add", `Int theirs_add)
                   ; ("theirs_mod", `Int theirs_mod)
                   ; ("theirs_del", `Int theirs_del)
                   ])))
      | _ ->
          ((raise
              (Pricklypear_lisp.Ast.Interp_error
                 (Pricklypear_lisp.Ast.Prim_error
                    ( "sync/plan2"
                    , "need (lens delta-manifest deleted-paths)"
                                              , "bad-args" ))))));

  (* sync/push2 — apply ONE delta chunk (repeatable; chunks compose over
     the stored base). Deleted paths travel every chunk: the apply is
     idempotent (a second Theirs_del on a gone node plans Unchanged).
     (sync/push2 lens changes-bodies deleted-paths) -> JSON
       {mode:"delta", applied, lww_client_won, lww_server_kept, generation} *)
  register_crud "sync/push2"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr lens; bodies; deleted ] ->
          let cb = parse_delta_bodies bodies in
          let deleted = parse_path_list deleted in
          (* the chunk's delta: paths only (marker hashes — the client's
             new hash for a locally-written node is "" until the
             convergence pull anyway, and plan_of's Theirs_* outcomes
             apply the BODY, whose updated_at is what LWW reads). The
             filter keeps apply/LWW scoped to what this chunk carries. *)
          let chunk_delta = List.map (fun (p, _) -> (p, "CHUNK")) cb in
          let chunk_paths = List.map fst cb in
          let base, server, theirs = delta_theirs ~lens ~delta:chunk_delta ~deleted in
          let plans =
            Sync.plan_of base server theirs
            |> List.filter (fun { Sync.path; outcome } ->
                   (List.mem path chunk_paths || List.mem path deleted)
                   &&
                   match outcome with
                   | Sync.Ours_del -> false (* plan2 owns the mirror list *)
                   | _ -> true)
          in
          let report = Sync.commit t.store ~client_bodies:cb plans in
          let bid = Store.current_branch_id t.store in
          let gen =
            match Store.current_generation t.store ~branch_id:bid with
            | Some n -> n
            | None -> 0L
          in
          (Pricklypear_lisp.Ast.VStr
             (Yojson.Safe.to_string
                (`Assoc
                   [ ("mode", `String "delta")
                   ; ("applied", `Int report.Sync.applied)
                   ; ( "lww_client_won"
                     , `List (List.map (fun p -> `String p) (List.rev report.Sync.lww_client_won)) )
                   ; ( "lww_server_kept"
                     , `List (List.map (fun p -> `String p) (List.rev report.Sync.lww_server_kept)) )
                     ; ( "failed_paths"
                       , `List (List.map (fun p -> `String p) (List.rev report.Sync.failed)) )
                     ; ( "failed_reasons"
                       , `Assoc (List.map
                                   (fun (p, m) -> (p, `String m))
                                   (List.rev report.Sync.failed_reasons)) )
                   ; ("generation", `String (Int64.to_string gen))
                   ])))
      | _ ->
          ((raise
              (Pricklypear_lisp.Ast.Interp_error
                 (Pricklypear_lisp.Ast.Prim_error
                    ( "sync/push2"
                    , "need (lens changes-bodies deleted-paths)"
                                              , "bad-args" ))))));

  (* sync/commit2 — end-of-round base advance (the ONLY base writer).
     Stores the CURRENT merged server manifest + generation as this
     (user, lens)'s next-round base. A crashed round never reaches it,
     so the next round re-plans against the old base.
     (sync/commit2 lens) -> JSON {mode:"delta", committed:true, paths, generation} *)
  register_crud "sync/commit2"
    (fun args ->
      match args with
      | [ Pricklypear_lisp.Ast.VStr lens ] ->
          let user_id = sync_user_id () in
          let prefixes = sync_unit_prefixes () in
          let merged =
            match Sync.server_manifest t.store ~prefixes with
            | Some m -> m
            | None ->
                ((raise
                    (Pricklypear_lisp.Ast.Interp_error
                       (Pricklypear_lisp.Ast.Prim_error
                          ("sync/commit2", "no DB", "failed")))))
          in
          let branch_name =
            match Request_context.get () with
            | Some c -> (Request_context.manifest c).branch_name
            | None -> t.manifest.branch_name
          in
          let bid = Store.current_branch_id t.store in
          let gen =
            match Store.current_generation t.store ~branch_id:bid with
            | Some n -> n
            | None -> 0L
          in
          Sync.save_base t.store ~user_id ~lens ~branch:branch_name
            ~generation:gen merged;
          (Pricklypear_lisp.Ast.VStr
             (Yojson.Safe.to_string
                (`Assoc
                   [ ("mode", `String "delta")
                   ; ("committed", `Bool true)
                   ; ("paths", `Int (List.length merged))
                   ; ("generation", `String (Int64.to_string gen))
                   ])))
      | _ ->
          ((raise
              (Pricklypear_lisp.Ast.Interp_error
                 (Pricklypear_lisp.Ast.Prim_error
                    ( "sync/commit2"
                    , "need (lens)"
                    , "bad-args" ))))));