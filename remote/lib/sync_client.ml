(* remote/lib/sync_client.ml — borge/local-first.borg sync, the CLIENT
   half of the exchange (the git protocol in miniature, one round).

   A round: build the client's data (path, hash) manifest from the lens
   substrate, send it to the image's sync/push prim (with the recorded
   base manifest from the last successful round — three-way), apply the
   response (pull server copies, mirror server deletions, print the
   surfaced LWW resolutions), and pin the merged state as the NEXT
   round's base. The server is the merge authority; the client is a
   governed branch rendezvousing with it.

   Hash authority is server-side too: the client never computes hashes
   (the pre-027 OCaml-side hash domain is deleted — one-tree.hash-domain
   digests PG-canonical jsonb DB-side). Envelope [hash] fields on the
   client are server hashes carried verbatim from pulled envelopes; a
   locally-built envelope carries "" and is convergence-pulled at the
   end of the round so it inherits the server's canonical stamp —
   without that pull an identical body would re-diverge every round
   (the client side of the dead hash domain could never match).

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

(** [push_paths ~base manifest] — the SUBSET of the client manifest whose
    envelopes must travel this round. The server consults client-bodies
    only to apply ClientOnly / theirs_mod pushes and to LWW-arbitrate
    diverge pairs (it reads updated_at off the client envelope); a node
    whose manifest hash equals its base hash needs no envelope. Shipping
    every envelope every round made the sync/push POST grow with the
    WHOLE data subtree (2124 rows ≈ 8.6MB → nginx 413 on prod) even on a
    quiet round. No base (fresh lens, no recorded snapshot): everything
    travels, as before. Pure — unit-tested in sync_client_test.ml. *)
let push_paths ~(base : (string * string) list)
    (manifest : (string * string) list) : (string * string) list =
  match base with
  | [] -> manifest
  | _ ->
    let t = Hashtbl.create (List.length base + 1) in
    List.iter (fun (p, h) -> Hashtbl.replace t p h) base;
    List.filter
      (fun (p, h) ->
        match Hashtbl.find_opt t p with
        | None -> true
        | Some bh -> bh <> h)
      manifest

let is_space_path (p : string) : bool =
  let pre = "data.s." in
  String.length p >= String.length pre && String.sub p 0 (String.length pre) = pre

(** The data replication unit this client syncs: data.u.<owner>.* (the
    user's own rows) plus member-space data.s.<space>.* rows living in the
    lens substrate (borge/local-first.borg replication-units — the PLANNED
    data.s.<space> component lands here). Space paths join the manifest and
    the recorded base so pulls and mirror-deletions flow, but they NEVER
    travel in a client push: spaces are PULL + PROPOSE, server-authoritative
    (the server rejects any client-driven space write in Sync.commit), and
    a locally-changed space row is force-pulled each round, superseding the
    local copy with the surfacing below. Code-lens sync is deliberately NOT
    here — it is the branch-pointer move (promote/merge), server-mediated.
    Exported: the pull path (remote/bin main) records the same filter as the
    first round's BASE — a batched --lens-pull-data seed IS a
    last-synced state (server hashes verbatim), so the first round
    must not ship every envelope again (nginx 413 at ~1MB). *)
let data_manifest ~(lens : Lens_store.lens) : (string * string) list =
  let user_prefix = "data.u." ^ Lens_store.enc_label lens.owner in
  let plen = String.length user_prefix in
  Lens_store.node_manifest ~lens
  |> List.filter (fun (p, _) ->
         (String.length p >= plen && String.sub p 0 plen = user_prefix)
         || is_space_path p)

(** The pushable half of the manifest: everything except server-
    authoritative space paths (the data.s prefix family). The server rejects
    those regardless (Sync.commit space guard); filtering them client-side
    keeps quiet rounds quiet. Pure — unit-tested in sync_client_test.ml. *)
let pushable_paths (m : (string * string) list) : (string * string) list =
  List.filter (fun (p, _) -> not (is_space_path p)) m

(** The delta exchange raises these when the image can't run it —
    the caller falls back to the v1 full exchange. *)
exception No_stored_base
exception Other_prim_error

(** [delta_of ~base manifest] — the client's delta against its recorded
    base: changes (manifest entries whose hash differs from base, or
    which base lacks) and deletions (base paths the manifest no longer
    holds). Pure — unit-tested. *)
let delta_of ~(base : (string * string) list)
    (manifest : (string * string) list) :
    (string * string) list * string list =
  let base_tbl = Hashtbl.create (List.length base + 1) in
  List.iter (fun (p, h) -> Hashtbl.replace base_tbl p h) base;
  let man_tbl = Hashtbl.create (List.length manifest + 1) in
  List.iter (fun (p, h) -> Hashtbl.replace man_tbl p h) manifest;
  let changes =
    List.filter
      (fun (p, h) ->
        match Hashtbl.find_opt base_tbl p with
        | None -> true
        | Some bh -> bh <> h)
      manifest
  in
  let deletes =
    List.filter_map
      (fun (p, _) -> if Hashtbl.mem man_tbl p then None else Some p)
      base
  in
  (changes, deletes)

(** [chunk_bodies ~max_bytes bodies] — split (path, envelope-json) pairs
    into request-sized chunks. The edge proxy caps request bodies at
    ~1MB (nginx client_max_body_size); ~400KB chunks leave headroom for
    the manifest/path lists and JSON encoding overhead. Pure — tested. *)
let chunk_bodies ~(max_bytes : int)
    (bodies : (string * string) list) : (string * string) list list =
  let max_bytes = max max_bytes 1 in
  let chunks = ref [] in
  let cur = ref [] and cur_bytes = ref 0 in
  List.iter
    (fun ((p, env) as pair) ->
      let sz = String.length p + String.length env + 8 in
      if !cur_bytes > 0 && !cur_bytes + sz > max_bytes then begin
        chunks := List.rev !cur :: !chunks;
        cur := [];
        cur_bytes := 0
      end;
      cur := pair :: !cur;
      cur_bytes := !cur_bytes + sz)
    bodies;
  if !cur <> [] then chunks := List.rev !cur :: !chunks;
  List.rev !chunks

(** True when the transport error is a SERVER-SIDE prim failure (as
    opposed to a transport/offline failure, kind "transport"): the delta
    exchange should fall back to the v1 full exchange for those — an
    old image without the delta prims, a wiped base row, a transient
    prim error — while an unreachable image must surface (the v1 round
    would just fail too). *)
let is_transport_error (e : exn) : bool =
  match e with
  | Ast.Interp_error (Ast.Prim_error (_, _, kind)) -> kind = "transport"
  | _ -> false

(** True when the transport error is the delta exchange's structured
    "no stored base" refusal (pre-032 server, or the base row was wiped
    for this lens) — the caller falls back to the v1 full exchange. *)
let is_no_stored_base (e : exn) : bool =
  match e with
  | Ast.Interp_error (Ast.Prim_error (_, msg, _)) ->
      let sub = "no stored base" in
      let n = String.length msg and m = String.length sub in
      let rec go i = i + m <= n && (String.sub msg i m = sub || go (i + 1)) in
      go 0
  | _ -> false

(** The image serializes prim results with Prims_pure.value_to_json. Since
    f83827c (2026-09-27, json-values-alist) a JSON object crosses /api/prim
    as an ALIST value (VList of VPair (VStr k, v)) instead of a VStr
    carrying the JSON text; pre-f83827c servers still send the text form.
    The sync client accepts BOTH: parse a VStr as JSON, pass anything else
    through value_to_json (the exact inverse — an alist becomes the
    object). Callers read only `Assoc payloads. *)
let prim_json_of_value (v : value) : (Yojson.Safe.t, string) result =
  match v with
  | Ast.VStr j ->
      (try Ok (Yojson.Safe.from_string j) with Yojson.Json_error e -> Error e)
  | v -> Ok (Prims_pure.value_to_json v)

(** Materialize one pulled node into the lens: a VStr carries the envelope
    JSON text (pre-f83827c servers); a decoded envelope (alist since
    f83827c) is re-serialized to JSON text first. True when the shape was
    usable. *)
let materialize_node ~(lens : Lens_store.lens) (v : value) : bool =
  match v with
  | Ast.VStr s -> ignore (Lens_store.materialize ~lens s); true
  | Ast.VList _ ->
      ignore
        (Lens_store.materialize ~lens
           (Yojson.Safe.to_string (Prims_pure.value_to_json v)));
      true
  | _ -> false

(** A pulled envelope's path, tolerating both wire shapes (pre/post-f83827c:
    VStr JSON text or decoded alist) — the convergence check needs to know
    WHICH requested paths the server actually returned, because a push the
    server rejected never comes back. *)
let envelope_path (v : value) : string option =
  match prim_json_of_value v with
  | Ok (`Assoc kv) -> (
      match List.assoc_opt "path" kv with
      | Some (`String p) -> Some p
      | _ -> None)
  | _ -> None

(** One sync round over the DELTA exchange (the chunked protocol;
     issue i-1789178161-87d0742f). Requires a recorded base (the caller
     checks). The round:

       1. compute the delta vs the base (delta_of — O(changes));
       2. sync/plan2 → pull list (server changes) + mirror-deletions;
       3. sync/push2 per body chunk (~400KB, edge-proxy safe);
       4. batched pulls via tree/get-many (~100 paths per call) for the
          server-only list AND every pushed path (convergence: the
          server envelope stamps the canonical hash so the next round's
          manifest matches its base — v1's per-node unstamped pull, but
          batched and total);
       5. sync/commit2 pins the stored base + generation (the ONLY base
          advance — a crashed round never commits, so retries re-plan
          against the old base; applies are idempotent);
       6. the client records its OWN base from the post-round substrate
          (they coincide on a fully-successful round — no O(N) merged
          manifest crosses the wire in either direction).

     A fully-quiet round whose generation matches the recorded snapshot
     skips steps 3-5 entirely (the common case in steady state). *)
let run_delta_round ~(transport : transport) ~(lens : Lens_store.lens)
    ~(base : (string * string) list) ~(quiet : bool) () :
    (round_report, [ `No_stored_base | `Other_prim_error | `Failed of string ])
    result =
  let log fmt =
    Printf.ksprintf (fun s -> prerr_endline ("[sync] " ^ s)) fmt
  in
  let manifest = data_manifest ~lens in
  let all_changes, all_deletes = delta_of ~base manifest in
  (* space paths never travel in a client push (PULL + PROPOSE,
     server-authoritative — the server rejects them regardless); a
     locally-changed or locally-deleted space row is force-pulled below
     instead, which supersedes the local copy and surfaces the loss. *)
  let changes = pushable_paths all_changes in
  let deletes =
    List.filter (fun p -> not (is_space_path p)) all_deletes in
  let space_changes =
    List.filter (fun (p, _) -> is_space_path p) all_changes
    @ List.map (fun p -> (p, "DELETED")) (List.filter is_space_path all_deletes)
  in
  let vstr_list ps = Ast.VList (List.map (fun p -> Ast.VStr p) ps) in
  let manifest_v m =
    Ast.VList
      (List.map (fun (p, h) -> Ast.VList [ Ast.VStr p; Ast.VStr h ]) m)
  in
  let ok = ref true and err = ref None in
  let outcome = ref `Ok in
  let pushed_total = ref 0 and pulled_total = ref 0
    and deleted_total = ref 0 and diverge_total = ref 0 in
  let won_all = ref [] and kept_all = ref [] in
  let branch = ref (match Lens_store.snapshot ~lens with
      | Some (b, _) -> b | None -> "")
  in
  let gen = ref 0L in
  (try
     (* 1. plan — read-only negotiate *)
     let plan_resp =
       transport "sync/plan2"
         [ Ast.VStr lens.Lens_store.branch; manifest_v changes; vstr_list deletes ]
     in
     let kv =
       match plan_resp with
       | Ok v -> (
           match prim_json_of_value v with
           | Ok (`Assoc kv) -> kv
           | Ok _ -> failwith "sync/plan2: bad shape"
           | Error e -> failwith ("sync/plan2: bad json: " ^ e))
       | Error e ->
           if is_transport_error e then raise e
           else if is_no_stored_base e then raise No_stored_base
           else raise Other_prim_error
     in
     let strs k =
       match List.assoc_opt k kv with
       | Some (`List xs) ->
           List.filter_map (function `String s -> Some s | _ -> None) xs
       | _ -> []
     in
     let plan_gen =
       match List.assoc_opt "generation" kv with
       | Some (`String g) -> (try Int64.of_string g with _ -> 0L)
       | _ -> 0L
     in
     let stored_base_gen =
       match List.assoc_opt "base_generation" kv with
       | Some (`String g) -> (try Int64.of_string g with _ -> 0L)
       | _ -> 0L
     in
     let pull_paths = strs "pull_paths" in
     let client_deletes = strs "client_delete_paths" in
     let diverge = strs "diverge_paths" in
     (* STALE-BASE GUARD. The stored base and the client's base must be
        the SAME state — they advance together (server at commit2, client
        on the commit2 response). A disagreement means a round died
        between the two advances: the plan above was computed against a
        base the client doesn't hold, so its pull list can silently omit
        rows (the crash-window hole). Discard and fall back to the v1
        full exchange, which re-syncs against the client's real base and
        re-commits. *)
     let snap_gen =
       match Lens_store.snapshot ~lens with Some (_, g) -> g | None -> 0L
     in
     if snap_gen <> 0L && stored_base_gen <> 0L
        && stored_base_gen <> snap_gen then begin
       if not quiet then
         log "stored base gen %Ld ≠ client gen %Ld — v1 re-sync"
           stored_base_gen snap_gen;
       raise Other_prim_error
     end;
     (* fully-quiet round: nothing on either side, and the server
        generation matches the recorded snapshot → skip to done *)
     if changes = [] && deletes = [] && pull_paths = []
        && client_deletes = [] && diverge = [] && plan_gen = snap_gen
     then begin
       gen := plan_gen;
       if not quiet then log "quiet (delta): nothing to transfer"
     end
     else begin
       (* 2. mirror server deletions locally *)
       List.iter
         (fun p ->
           if Lens_store.delete_node ~lens p then begin
             incr deleted_total;
             if not quiet then log "deleted local %s (server deleted)" p
           end)
         client_deletes;
       (* 3. push the client delta in body chunks *)
        (* 3. push the client delta in body chunks *)
        let bodies =
          List.filter_map
            (fun (p, _) ->
              match Lens_store.get ~lens p with
              | Some env -> Some (p, env)
              | None -> None)
            changes
        in
        let pushed_paths = List.map fst bodies in
        let rejected = ref [] in
        let returned = Hashtbl.create (List.length changes + 1) in
        if changes <> [] then begin
          let chunks = chunk_bodies ~max_bytes:400_000 bodies in
          List.iter
            (fun chunk ->
              let chunk_paths = List.map fst chunk in
              let bodies_v =
                Ast.VList
                  (List.map
                     (fun (p, env) ->
                       Ast.VList [ Ast.VStr p; Ast.VStr env ])
                     chunk)
              in
              (match
                 transport "sync/push2"
                   [ Ast.VStr lens.Lens_store.branch; bodies_v; vstr_list deletes ]
               with
               | Ok v -> (
                   match prim_json_of_value v with
                   | Ok (`Assoc kv) ->
                       (* helpers bound to THIS response: the plan-bound
                          strs/int above close over sync/plan2's kv and
                          would silently report zero pushes here *)
                       let int k =
                         match List.assoc_opt k kv with
                         | Some (`Int n) -> n | _ -> 0
                       in
                       let strs k =
                         match List.assoc_opt k kv with
                         | Some (`List xs) ->
                             List.filter_map
                               (function `String s -> Some s | _ -> None) xs
                         | _ -> []
                       in
                       pushed_total := !pushed_total + (int "applied");
                       won_all := !won_all @ strs "lww_client_won";
                       kept_all := !kept_all @ strs "lww_server_kept";
                       (* SURFACE rejections: the typed-boundary gate reports
                          what it refused (path + reason on current servers).
                          The round must NOT advance either base (gated below),
                          so a rejected node re-pushes next round instead of
                          being absorbed by the base (the 10-07 outage). *)
                       let reasons =
                         match List.assoc_opt "failed_reasons" kv with
                         | Some (`Assoc rows) ->
                             List.filter_map
                               (function p, `String m -> Some (p, m) | _ -> None)
                               rows
                         | _ -> []
                       in
                       (match List.assoc_opt "failed_paths" kv with
                        | Some (`List xs) ->
                            List.iter
                              (function
                                | `String p ->
                                    let why =
                                      match List.assoc_opt p reasons with
                                      | Some m -> m
                                      | None -> "reason not reported"
                                    in
                                    rejected := p :: !rejected;
                                    log "push %s: server REJECTED: %s (kept locally; re-pushes next round)"
                                      p why
                                | _ -> ())
                              xs
                        | _ -> ())
                   | _ -> log "push2: bad json")
               | Error e ->
                   log "push2 failed: %s" (Printexc.to_string e);
                   ok := false);
              ignore chunk_paths)
            chunks
        end;
       (* 4. pulls — server-only list + every pushed path (convergence:
          the server envelope stamps the canonical hash; for an
          LWW-kept path it also replaces the stale local copy). *)
        let pull_set =
          let t = Hashtbl.create (List.length pull_paths + List.length changes + 1) in
          List.iter (fun p -> Hashtbl.replace t p ()) pull_paths;
          List.iter (fun p -> Hashtbl.replace t p ()) !kept_all;
          List.iter (fun (p, _) -> Hashtbl.replace t p ()) changes;
          (* force-pull locally-changed space rows: the server copy wins *)
          List.iter (fun p -> Hashtbl.replace t p ()) (List.map fst space_changes);
          Hashtbl.fold (fun p () acc -> p :: acc) t []
        in
       let pull_paths = List.sort String.compare pull_set in
       let batch_n = 100 in
       let rec batches l =
         if l = [] then []
         else
           let b, rest =
             if List.length l <= batch_n then (l, [])
             else (List.filteri (fun i _ -> i < batch_n) l,
                   List.filteri (fun i _ -> i >= batch_n) l)
           in
           b :: batches rest
       in
        List.iter
          (fun batch ->
            match transport "tree/get-many" [ vstr_list batch ] with
            | Ok (Ast.VList envs) ->
                let n = ref 0 in
                List.iter
                  (fun node ->
                    (match envelope_path node with
                     | Some p -> Hashtbl.replace returned p ()
                     | None -> ());
                    if materialize_node ~lens node then incr n)
                  envs;
                pulled_total := !pulled_total + !n
            | Ok _ ->
                log "pull batch: bad shape";
                ok := false
            | Error e ->
                log "pull batch failed: %s" (Printexc.to_string e);
                ok := false)
          (batches pull_paths);
        (* CONVERGENCE GATE: every pushed path must have come back from the
           server. A push the server REJECTED never returns an envelope —
           older servers report it only by omission (no failed_paths). A
           round that fails convergence never advances either base (commit2
           + the own-base stamp below are gated on ok), so the node re-pushes
           next round instead of being silently absorbed by the base. *)
        let missing =
          List.filter (fun p -> not (Hashtbl.mem returned p)) pushed_paths
        in
        List.iter
          (fun p ->
            if not (List.mem p !rejected) then
              log "push %s: not on server after round (kept locally; re-pushes next round)" p)
          missing;
        if !rejected <> [] || missing <> [] then begin
          ok := false;
          if !err = None then
            err := Some
              (if !rejected <> [] then
                 "push rejected: " ^ String.concat ", " (List.rev !rejected)
               else
                 "push not applied: " ^ String.concat ", " missing)
        end;
        diverge_total := List.length diverge;
        (* space paths surface locally: the server copy is authority. A
           locally-changed space row was force-pulled above (the edit is
           superseded — the obituary line says so); a local-only space row
           the server does not hold just sits, never replicated outward. *)
        List.iter
          (fun (p, h) ->
            let env_hash =
              match Lens_store.get ~lens p with
              | None -> ""
              | Some env -> (
                  match Yojson.Safe.from_string env with
                  | `Assoc kv -> (
                      match List.assoc_opt "hash" kv with
                      | Some (`String s) -> s
                      | _ -> "")
                  | _ -> "")
            in
            if env_hash <> "" && env_hash <> h then
              log "space %s: local edit superseded — server copy kept \
(land agent writes on the image, not in the lens)" p
            else if env_hash = "" then
              log "space %s: local-only copy kept — server does not hold it \
(lens space rows are pull-only)" p)
          space_changes;
       (* 5. commit — the ONLY base advance, and ONLY on a fully-successful
          round: commit2 after a failed pull would advance the stored base
          past rows the client never received (the crash-window hole the
          stale-base guard above closes on recovery). *)
        if not !ok then
          raise
            (Failure
               (Option.value !err ~default:"round had failures — skipping commit"));
       (match transport "sync/commit2" [ Ast.VStr lens.Lens_store.branch ] with
        | Ok v -> (
            match prim_json_of_value v with
            | Ok (`Assoc kv) -> (
                match List.assoc_opt "generation" kv with
                | Some (`String g) -> gen := (try Int64.of_string g with _ -> 0L)
                | _ -> ())
            | _ -> ())
        | Error e ->
            log "commit2 failed: %s" (Printexc.to_string e);
            ok := false);
       (* 6. record the client base from the post-round substrate (they
          coincide on a fully-successful round) *)
       if !ok then begin
         Lens_store.record_base_manifest ~lens (data_manifest ~lens);
         if !gen > 0L then
           Lens_store.record_snapshot ~lens ~branch:!branch ~generation:!gen
       end
     end
   with
   | No_stored_base -> outcome := `No_stored_base
   | Other_prim_error -> outcome := `Other_prim_error
   | exn ->
       ok := false;
       err := Some (Printexc.to_string exn);
       log "failed: %s" (Printexc.to_string exn));
  if not !ok && !err = None then err := Some "delta round failed";
  if !outcome = `No_stored_base then Error `No_stored_base
  else if !outcome = `Other_prim_error then Error `Other_prim_error
  else if not !ok then Error (`Failed (Option.value !err ~default:"delta round failed"))
  else
    Ok
      { (empty_report ()) with
        ok = true;
        pushed = !pushed_total;
        pulled = !pulled_total;
        deleted_local = !deleted_total;
        diverge = !diverge_total;
        lww_client_won = !won_all;
        lww_server_kept = !kept_all;
        branch = !branch;
        generation = !gen }

(** Run ONE sync round. [quiet] = background mode: print only activity
    (pushes, pulls, deletions, surfaced LWW resolutions) and errors —
    a clean round is silent. Never raises: transport errors (including
    exceptions — Eio raises, it does not return Error) come back as
    {ok = false; error}. *)
let run_v1_round_inner ~(transport : transport) ~(lens : Lens_store.lens)
    ~(quiet : bool) () : round_report =
  let log fmt =
    Printf.ksprintf (fun s -> prerr_endline ("[sync] " ^ s)) fmt
  in
  let report = ref (empty_report ()) in
  (try
     let manifest = data_manifest ~lens in
     (* locally-written envelopes carry no server hash ("") yet — the
        convergence pull below fetches the server envelope for each
        after the round's merge, so the next round's manifest compares
        real hashes. *)
      let unstamped =
        List.filter (fun (p, h) -> h = "" && not (is_space_path p)) manifest in
     let base = Lens_store.base_manifest ~lens in
     (* bodies only where the base says something changed (push_paths):
        a quiet round POSTs the manifest (~300KB for 2k rows), not every
        envelope (8.6MB → nginx 413). *)
      let bodies =
        push_paths ~base manifest
        |> pushable_paths (* space rows never travel: server-authoritative *)
        |> List.filter_map (fun (p, _) ->
            match Lens_store.get ~lens p with
            | Some env -> Some (Ast.VList [ Ast.VStr p; Ast.VStr env ])
            | None -> None)
      in
     let manifest_v =
       Ast.VList
         (List.map
            (fun (p, h) -> Ast.VList [ Ast.VStr p; Ast.VStr h ])
            manifest)
     in
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
       | Ok v -> (
           match prim_json_of_value v with
           | Ok (`Assoc kv) ->
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
               let client_deletes = strs "client_delete_paths" in
               let diverge = strs "diverge_paths" in
               let pulls =
                 (* three-way rounds give an explicit pull list (server-only
                    + LWW-server-kept convergence); two-way rounds just
                    server_only_paths. *)
                 if pull_paths = [] then server_only else pull_paths
               in
               (* space rows: the server copy is authority — every locally-held
                  space path re-pulls (supersedes local edits / refreshes stale
                  copies; the manifest carries their canonical hashes into the
                  recorded base). Never pushed: bodies are pushable-paths only. *)
               let pulls =
                 pulls
                 @ (manifest
                    |> List.filter (fun (p, _) -> is_space_path p)
                    |> List.map fst
                    |> List.filter (fun p -> not (List.mem p client_deletes)))
                 in
             let won = strs "lww_client_won" in
             let kept = strs "lww_server_kept" in
             let gen_str = str "generation" in
             let gen = try Int64.of_string gen_str with _ -> 0L in
             (* auth.user-llm-keys 5b: the round metadata export — the
                caller's OWN llm credential. Record to __config.json
                (preserving any locally chosen model); key last4 only
                in logs — never the raw secret. *)
             (match List.assoc_opt "llm_config" kv with
              | Some (`Assoc lc) ->
                  let lc_str k =
                    match List.assoc_opt k lc with
                    | Some (`String s) -> s | _ -> ""
                  in
                  let ak, b = lc_str "api_key", lc_str "base" in
                  if ak <> "" || b <> "" then begin
                    Lens_store.record_sync_config ~lens ~api_key:ak ~base:b;
                    if not quiet then
                      log "llm config updated (key last4=%s base=%s)"
                        (if String.length ak >= 4
                         then String.sub ak (String.length ak - 4) 4
                         else "????")
                        b
                  end
              | _ -> ());
             (* pull server copies (tree/get bypasses the rim's lens-local
                reads on purpose: the pull must come from the server) *)
               let pull_failed = ref [] in
               let pulled =
                 List.fold_left
                   (fun n p ->
                      match transport "tree/get" [ Ast.VStr p ] with
                      | Ok v ->
                          if materialize_node ~lens v then begin
                            if not quiet then log "pulled %s" p;
                            n + 1
                          end else (pull_failed := p :: !pull_failed; n)
                      | Error e ->
                          log "pull %s: %s" p (Printexc.to_string e);
                          pull_failed := p :: !pull_failed;
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
             (* convergence pull: a locally-written envelope (hash "")
                that the server now holds — the push was applied (clean
                edit or client-won LWW) — fetches the canonical server
                envelope so the local copy inherits the server-computed
                hash. Without this the next round would re-diverge on an
                identical body (the client side has no hash domain to
                match with) and churn LWW forever. Skips paths already
                pulled above and server-deleted paths (the mirror delete
                already removed the local copy — re-pulling would
                resurrect it). A server miss (push rejected/not applied)
                keeps the local envelope; the next round re-pushes it. *)
             let unstamped_pulls =
               List.filter
                 (fun (p, _) ->
                   not (List.mem p pulls) && not (List.mem p client_deletes))
                 unstamped
             in
               let v1_missing = ref [] in
               let converged =
                 List.fold_left
                   (fun n (p, _) ->
                      match transport "tree/get" [ Ast.VStr p ] with
                      | Ok v ->
                          if materialize_node ~lens v then begin
                            if not quiet then log "converged %s (server hash)" p;
                            n + 1
                          end else (v1_missing := p :: !v1_missing; n)
                      | Error e ->
                          log "converge %s: %s" p (Printexc.to_string e);
                          v1_missing := p :: !v1_missing;
                          n)
                   0 unstamped_pulls
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
                  (branch, generation) merge-base coordinate — ONLY on a
                  fully-converged round: a push the server rejected (or a
                  pull that failed) must NOT be absorbed by the base, or
                  the node never re-pushes (the 10-07 push outage). *)
               let missing = List.rev !v1_missing in
               List.iter
                 (fun p ->
                   log "push %s: not on server after round (kept locally; re-pushes next round)" p)
                 missing;
               List.iter
                 (fun p -> log "pull %s: not delivered — base not advanced" p)
                 (List.rev !pull_failed);
               if missing = [] && !pull_failed = [] then begin
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
                 Lens_store.record_snapshot ~lens ~branch ~generation:gen
               end;
             let activity =
               int "applied" + pulled + converged + deleted + List.length kept
             in
             if not quiet then
               log "applied=%d pulled=%d deleted=%d diverge=%d gen=%s"
                 (int "applied") (pulled + converged) deleted (List.length diverge) gen_str
             else if activity > 0 || List.length diverge > 0 then
               log "pushed=%d pulled=%d deleted=%d diverge=%d (won=%d lost=%d) gen=%s"
                 (int "applied") (pulled + converged) deleted (List.length diverge)
                 (List.length won) (List.length kept) gen_str;
               let failed = missing <> [] || !pull_failed <> [] in
               report :=
                 { ok = not failed;
                   error =
                     (if failed then
                        Some
                          ("not converged: "
                           ^ String.concat ", " (missing @ List.rev !pull_failed))
                      else None);
                   pushed = int "applied";
                   pulled = pulled + converged;
                   deleted_local = deleted;
                   diverge = List.length diverge;
                   lww_client_won = won;
                   lww_server_kept = kept;
                   branch;
                   generation = gen }
         | _ ->
             report := { !report with ok = false; error = Some "bad response shape" };
             log "bad response shape")
   with exn ->
     report :=
       { !report with
         ok = false;
         error = Some (Printexc.to_string exn) };
     log "failed: %s" (Printexc.to_string exn));
  !report

(* The v1 full-manifest exchange — unchanged behavior, plus an optional
   end-of-round sync/commit2 so a v1 bootstrap registers the stored base
   and the NEXT round can go delta (ignored silently on a pre-032
   server: the client just keeps running v1). *)
let run_v1_round ~(transport : transport) ~(lens : Lens_store.lens)
    ~(quiet : bool) ~(register_base : bool) () : round_report =
  let report = run_v1_round_inner ~transport ~lens ~quiet () in
  if report.ok && register_base then
    (try
         match transport "sync/commit2" [ Ast.VStr lens.Lens_store.branch ] with
         | Ok v -> (
             match prim_json_of_value v with
             | Ok (`Assoc kv) -> (
                 match List.assoc_opt "generation" kv with
                 | Some (`String g) ->
                     let g = try Int64.of_string g with _ -> 0L in
                     if g > 0L then
                       Lens_store.record_snapshot ~lens
                         ~branch:lens.Lens_store.branch ~generation:g
                 | _ -> ())
             | _ -> ())
         | Error _ -> ()
     with _ -> ());
  report

(** Run ONE sync round — the dispatcher. A lens with a recorded base
    negotiates the delta exchange (O(changes) requests, chunked bodies,
    batched pulls); everything else — fresh lens, pre-032 server, wiped
    base row — falls back to the v1 full-manifest exchange (which then
    registers a base via sync/commit2 so the next round goes delta).
    [quiet] = background mode (activity only; a clean round is silent). *)
let run_round ~(transport : transport) ~(lens : Lens_store.lens)
    ~(quiet : bool) () : round_report =
  let base = Lens_store.base_manifest ~lens in
  if base = [] then
    (* fresh lens: the v1 full exchange, then commit the stored base *)
    run_v1_round ~transport ~lens ~quiet ~register_base:true ()
  else
    match run_delta_round ~transport ~lens ~base ~quiet () with
    | Ok r -> r
    | Error `No_stored_base ->
        if not quiet then
          prerr_endline
            "[sync] no stored base on image — falling back to v1 full round";
        run_v1_round ~transport ~lens ~quiet ~register_base:true ()
    | Error `Other_prim_error ->
        if not quiet then
          prerr_endline
            "[sync] image rejected the delta exchange — falling back to v1 full round";
        run_v1_round ~transport ~lens ~quiet ~register_base:true ()
    | Error (`Failed msg) ->
        if not quiet then prerr_endline ("[sync] failed: " ^ msg);
        { (empty_report ()) with ok = false; error = Some msg }
