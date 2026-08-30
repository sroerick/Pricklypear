(* remote/lib/sync_client.ml — borge/local-first.borg sync, the CLIENT
   half of the exchange (the git protocol in miniature, one round).

   A round: build the client's data (path, hash) manifest from the lens
   substrate, send it to the image's sync/push prim (with the recorded
   base manifest from the last successful round — three-way), apply the
   response (pull server copies, mirror server deletions, print the
   surfaced LWW resolutions), and pin the merged state as the NEXT
   round's base. The server is the merge authority; the client is a
   governed branch rendezvousing with it.

   Invisibility is the common case: a quiet round (nothing to transfer,
   nothing diverged) prints nothing. Every divergence resolution —
   client-won or server-kept — is a printed line (the obituary rule
   applies to sync losses). This module is the machinery --sync runs
   once and --sync-loop runs on a timer (the "background" in
   "sync invisibly in the background" is client-side scheduling of these
   rounds; no new server surface is needed). *)

open Pricklypear_lisp
open Ast

(** The transport shape (Proxy_rim.transport): a prim call over the wire
    returning a value or a printable error. *)
type transport = string -> value list -> (value, exn) Result.t

type round_report = {
  ok : bool;                     (* false when the round failed (offline, bad shape) *)
  error : string option;
  pushed : int;                  (* client-only nodes applied server-side *)
  pulled : int;                  (* server copies fetched into the lens *)
  deleted_local : int;           (* stale local copies removed (server deleted) *)
  diverge : int;                 (* diverge pairs seen (detection count) *)
  lww_client_won : string list;  (* diverge/conflict resolved TO the client copy *)
  lww_server_kept : string list; (* diverge/conflict resolved TO the server copy (loss surfaced) *)
  branch : string;               (* pinned snapshot branch *)
  generation : Int64.t;          (* pinned snapshot generation (next merge-base coordinate) *)
}

let empty_report () =
  { ok = false;
    error = None;
    pushed = 0;
    pulled = 0;
    deleted_local = 0;
    diverge = 0;
    lww_client_won = [];
    lww_server_kept = [];
    branch = "";
    generation = 0L }

(** The data replication unit this client syncs: data.u.<owner>.* (the
    user's own rows). Code-lens sync is deliberately NOT here — it is
    the branch-pointer move (promote/merge), server-mediated. *)
let data_manifest ~(lens : Lens_store.lens) : (string * string) list =
  let user_prefix = "data.u." ^ Lens_store.enc_label lens.owner in
  let plen = String.length user_prefix in
  Lens_store.node_manifest ~lens
  |> List.filter (fun (p, _) ->
         String.length p >= plen && String.sub p 0 plen = user_prefix)

(** Run ONE sync round. [quiet] = background mode: print only activity
    (pushes, pulls, deletions, surfaced LWW resolutions) and errors —
    a clean round is silent. Never raises: transport errors (including
    exceptions — Eio raises, it does not return Error) come back as
    {ok = false; error}. *)
let run_round ~(transport : transport) ~(lens : Lens_store.lens)
    ~(quiet : bool) () : round_report =
  let log fmt =
    Printf.ksprintf (fun s -> prerr_endline ("[sync] " ^ s)) fmt
  in
  let report = ref (empty_report ()) in
  (try
     let manifest = data_manifest ~lens in
     let bodies =
       List.filter_map
         (fun (p, _) ->
           match Lens_store.get ~lens p with
           | Some env -> Some (Ast.VList [ Ast.VStr p; Ast.VStr env ])
           | None -> None)
         manifest
     in
     let manifest_v =
       Ast.VList
         (List.map
            (fun (p, h) -> Ast.VList [ Ast.VStr p; Ast.VStr h ])
            manifest)
     in
     let base = Lens_store.base_manifest ~lens in
     let resp =
       if base = [] then
         (* no recorded base yet (fresh lens): two-way round — additions
            + LWW only. Deletions need the three-way, which exists once
            the first successful round pins the base. *)
         transport "sync/push" [ manifest_v; Ast.VList bodies ]
       else
         let base_v =
           Ast.VList
             (List.map
                (fun (p, h) -> Ast.VList [ Ast.VStr p; Ast.VStr h ])
                base)
         in
         transport "sync/push" [ manifest_v; Ast.VList bodies; base_v ]
     in
     match resp with
     | Error e ->
         report :=
           { !report with
             ok = false;
             error = Some (Printexc.to_string e) };
         log "failed: %s" (Printexc.to_string e)
     | Ok (Ast.VStr json) -> (
         match Yojson.Safe.from_string json with
         | `Assoc kv ->
             let int k =
               match List.assoc_opt k kv with Some (`Int n) -> n | _ -> 0
             in
             let strs k =
               match List.assoc_opt k kv with
               | Some (`List xs) ->
                   List.filter_map (function `String s -> Some s | _ -> None) xs
               | _ -> []
             in
             let str k =
               match List.assoc_opt k kv with
               | Some (`String s) -> s | _ -> ""
             in
             let server_only = strs "server_only_paths" in
             let pull_paths = strs "pull_paths" in
             let pulls =
               (* three-way rounds give an explicit pull list (server-only
                  + LWW-server-kept convergence); two-way rounds just
                  server_only_paths. *)
               if pull_paths = [] then server_only else pull_paths
             in
             let client_deletes = strs "client_delete_paths" in
             let diverge = strs "diverge_paths" in
             let won = strs "lww_client_won" in
             let kept = strs "lww_server_kept" in
             let gen_str = str "generation" in
             let gen = try Int64.of_string gen_str with _ -> 0L in
             (* pull server copies (tree/get bypasses the rim's lens-local
                reads on purpose: the pull must come from the server) *)
             let pulled =
               List.fold_left
                 (fun n p ->
                   match transport "tree/get" [ Ast.VStr p ] with
                   | Ok (Ast.VStr env) ->
                       ignore (Lens_store.materialize ~lens env);
                       if not quiet then log "pulled %s" p;
                       n + 1
                   | Ok _ -> n
                   | Error e ->
                       log "pull %s: %s" p (Printexc.to_string e);
                       n)
                 0 pulls
             in
             (* mirror server deletions (Ours_del): the stale local copy
                of a node the server deleted goes too *)
             let deleted =
               List.fold_left
                 (fun n p ->
                   if Lens_store.delete_node ~lens p then begin
                     if not quiet then log "deleted local %s (server deleted)" p;
                     n + 1
                   end
                   else n)
                 0 client_deletes
             in
             (* the obituary rule, applied to sync losses: every LWW
                resolution is surfaced with its winner *)
             List.iter (fun p -> log "LWW %s: client edit wins" p) won;
             List.iter (fun p -> log "LWW %s: server copy kept (client edit lost)" p) kept;
             let branch =
               match
                 (try transport "current-branch" [] with _ -> Error (Failure "offline"))
               with
               | Ok (Ast.VStr b) -> b
               | _ -> ""
             in
             (* pin the merged state as the NEXT round's base + the
                (branch, generation) merge-base coordinate *)
             (match List.assoc_opt "merged_manifest" kv with
              | Some (`List rows) ->
                  let m =
                    List.filter_map
                      (function
                        | `List [ `String p; `String h ] -> Some (p, h)
                        | _ -> None)
                      rows
                  in
                  if m <> [] then Lens_store.record_base_manifest ~lens m
              | _ -> ());
             Lens_store.record_snapshot ~lens ~branch ~generation:gen;
             let activity =
               int "applied" + pulled + deleted + List.length kept
             in
             if not quiet then
               log "applied=%d pulled=%d deleted=%d diverge=%d gen=%s"
                 (int "applied") pulled deleted (List.length diverge) gen_str
             else if activity > 0 || List.length diverge > 0 then
               log "pushed=%d pulled=%d deleted=%d diverge=%d (won=%d lost=%d) gen=%s"
                 (int "applied") pulled deleted (List.length diverge)
                 (List.length won) (List.length kept) gen_str;
             report :=
               { ok = true;
                 error = None;
                 pushed = int "applied";
                 pulled;
                 deleted_local = deleted;
                 diverge = List.length diverge;
                 lww_client_won = won;
                 lww_server_kept = kept;
                 branch;
                 generation = gen }
         | _ ->
             report := { !report with ok = false; error = Some "bad response shape" };
             log "bad response shape")
     | Ok _ ->
         report := { !report with ok = false; error = Some "unexpected return value" };
         log "unexpected return value"
   with exn ->
     report :=
       { !report with
         ok = false;
         error = Some (Printexc.to_string exn) };
     log "failed: %s" (Printexc.to_string exn));
  !report
