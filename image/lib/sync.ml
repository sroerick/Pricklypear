(** image/lib/sync.ml — borge/local-first.borg sync (campaign 8.3).

    The SERVER-SIDE sync merge authority. Sync = three-way tree-diff
    (branching.borg / one-tree.branching-interaction), keyed on subtree
    node hashes (the git protocol in miniature):

       1. Client sends its (path, hash) manifest for the lens subtree.
       2. Server compares against its own subtree hashes; reports which
          paths differ or are missing on each side.
       3. Each side sends the node BODIES the other is missing or has
          stale.
       4. Server applies the three-way merge (common ancestor = the
          last-synced generation) and returns the merged subtree state.

    The envelope hash is the negotiation; the file is the transfer. The
    hash domain is the unified one-tree.hash-domain digest: sha256 over
    PG's CANONICAL jsonb text of the body, computed DB-side in every
    writer (the pre-027 OCaml-side hash_body is deleted — it was a second
    domain for the same body). The client does NOT compute hashes: its
    manifest carries server hashes verbatim from pulled envelopes, and a
    locally-written envelope carries "" until the round's convergence
    pull (sync_client) fetches the server envelope — one hash scheme,
    both sides, so a Same/Diverge classification is meaningful across
    the wire.

    This module owns the pure reconciliation + three-way merge planner
    (the heart of the exchange — classification of a manifest union and
    the conflict-free / LWW-v0 plan), plus the server-side harness:
    reading its own subtree manifest from tree_nodes and applying a
    validated reconcile plan back to the store.

    The client-facing transport/exchange (an HTTP rendezvous that ships
    the manifest + bodies) is owned by the client lens / sync client;
    this module is the server half: given a client's (path, hash)
    manifest for the current lens, it decides what the server must send
    (ServerOnly = pull), what the client must send (ClientOnly = push),
    and where the three-way merge (base = last-synced) conflicts.
*)

(* ==================================================================
   HASH MANIFESTS
   ================================================================== *)

(** A (path, hash) manifest: path -> content hash. This is the exact
    shape [Lens_store.node_manifest] produces on the client and
    [server_manifest] below reads from tree_nodes. The hash is the
    one-tree.hash-domain digest (sha256 over PG's canonical jsonb text
    of the body, computed DB-side); the client carries it verbatim, or
    "" for a locally-written envelope not yet converged. *)
type manifest = (string * string) list

(** Association-list lookup. *)
let assoc_find (m : manifest) (path : string) : string option =
  List.assoc_opt path m

(** Convert a manifest to a sorted association list (path, hash), deduped
    (later wins, matching assoc semantics). *)
let normalize (m : manifest) : manifest =
  let tbl = Hashtbl.create (List.length m) in
  List.iter (fun (p, h) -> Hashtbl.replace tbl p h) m;
  Hashtbl.fold (fun p h acc -> (p, h) :: acc) tbl []
  |> List.sort (fun (a, _) (b, _) -> String.compare a b)

let to_assoc (m : manifest) : (string * string) list = normalize m

let of_assoc (a : (string * string) list) : manifest = a

(* ==================================================================
   CLASSIFICATION (pure)
   ================================================================== *)

(** Per-path delta between a server manifest and a client manifest.
    - [Same]        — both sides hold the node with the same hash.
    - [ClientOnly]  — only the client holds the node (server missing it);
                      the client must PUSH the body to the server.
    - [ServerOnly]  — only the server holds the node (client missing it);
                      the server must PULL (send the body to the client).
    - [Diverge]     — both sides hold the node but with DIFFERENT hashes.
                      Requires the three-way merge (last-synced base) to
                      decide push/pull/conflict; the raw two-way view can
                      only report it.
*)
type delta = Same | ClientOnly | ServerOnly | Diverge

let classify_delta (server : manifest) (client : manifest) (path : string)
    : delta =
  match assoc_find server path, assoc_find client path with
  | Some sh, Some ch when sh = ch -> Same
  | Some _, Some _ -> Diverge
  | Some _, None -> ServerOnly
  | None, Some _ -> ClientOnly
  | None, None -> Same  (* absent on both: nothing to do *)

(** Reconcile a client manifest against the server manifest. Returns the
    (path, hash) lists per delta class. Pure. *)
type classify_report = {
  same_len : int;
  client_only : manifest;   (* client -> server pushes *)
  server_only : manifest;   (* server -> client pulls *)
  diverge : manifest;       (* both changed; needs three-way *)
}

let classify (server : manifest) (client : manifest) : classify_report =
  let all =
    List.fold_left
      (fun acc (p, _) -> if List.mem_assoc p acc then acc else (p, 0) :: acc)
      [] (to_assoc server @ to_assoc client)
  in
  List.fold_left
    (fun r (p, _) ->
      match classify_delta server client p with
      | Same -> { r with same_len = r.same_len + 1 }
      | ClientOnly ->
          { r with client_only = (p, Option.get (assoc_find client p)) :: r.client_only }
      | ServerOnly ->
          { r with server_only = (p, Option.get (assoc_find server p)) :: r.server_only }
      | Diverge ->
          { r with diverge = (p, Option.get (assoc_find client p)) :: r.diverge })
    { same_len = 0; client_only = []; server_only = []; diverge = [] }
    all

(* ==================================================================
   THREE-WAY MERGE PLANNER (pure)
   ================================================================== *)

(** Three-way merge over (base = last-synced, ours = server, theirs =
    client), per the campaign-3 diff philosophy (branching.ast-nodes):
    disjoint changes compose; a same-path change kept identical by both
    sides folds to one; a same-path DIFFERENT change is a CONFLICT —
    report-only (LWW v0 keeps ours=server and surfaces the conflict; the
    obituary rule applies to sync losses too, so the conflict is never
    silently dropped).

    Per-path outcome:
      unchanged  — base = ours = theirs (or absent on all): nothing.
      theirs_add — added on client only since the base (base absent, ours
                   absent, theirs present): apply the client node.
      ours_add   — added on server only since the base (base absent,
                   theirs absent, ours present): keep, nothing to send.
      theirs_mod — ours stayed at base, theirs changed (base = ours ≠
                   theirs): apply the client body (a clean client edit).
      ours_mod   — theirs stayed at base, ours changed (base = theirs ≠
                   ours): keep server, nothing to send.
      identical  — both moved to the same new hash: keep either.
      conflict   — base present, both changed and to different hashes:
                   report; LWW v0 keeps ours (server) and surfaces.
      theirs_del — deleted on client only (base present, ours present,
                   theirs absent): delete the server node (a client
                   deletion applied).
      ours_del   — deleted on server only: nothing to send (server node
                   gone; client still holds a stale copy → client pulls
                   the deletion = the server-only tombstone/pull).
*)
type outcome =
  | Unchanged
  | Theirs_add                 (* client push (server applies) *)
  | Ours_keep                  (* server keeps; no transfer *)
  | Theirs_mod                 (* client push (server applies) *)
  | Identical
  | Conflict of string * string * string * string
                               (* path, base_hash, ours_hash, theirs_hash *)
  | Theirs_del                 (* client deletion applied server-side *)
  | Ours_del

type plan = {
  path : string;
  outcome : outcome;
}

let plan_of (base : manifest) (ours : manifest) (theirs : manifest)
    : plan list =
  let bh = assoc_find base in
  let oh = assoc_find ours in
  let th = assoc_find theirs in
  let all =
    List.fold_left
      (fun acc (p, _) ->
        if List.mem_assoc p acc then acc else (p, ()) :: acc)
      []
      (to_assoc base @ to_assoc ours @ to_assoc theirs)
  in
  List.map
    (fun (p, ()) ->
      let b, o, t = bh p, oh p, th p in
      let outcome =
        match b, o, t with
        (* all agree / all absent *)
        | bo, oo, to_ when bo = oo && oo = to_ -> Unchanged
        (* client added a node both the base and the server lack *)
        | None, None, Some _ -> Theirs_add
        (* server added a node; client never had it (pull provides it) *)
        | None, Some _, None -> Ours_keep
        (* client edited a node the server never moved from base *)
        | Some x, Some oo, Some t_ when oo = x && t_ <> x -> Theirs_mod
        (* server edited a node the client never moved from base *)
        | Some x, Some oo, Some t_ when t_ = x && oo <> x -> Ours_keep
        (* both moved to the SAME new hash: one change, not two *)
        | Some _, Some oo, Some t_ when oo = t_ -> Identical
        (* base present, both moved and to different hashes: conflict *)
        | Some x, Some oo, Some t_ -> Conflict (p, x, oo, t_)
        (* client deleted a node the server still holds *)
        | Some _, Some _, None -> Theirs_del
        (* server deleted a node the client still holds (client pulls) *)
        | Some _, None, Some _ -> Ours_del
        (* contradictory (some/None combos not covered): keep ours *)
        | _ -> Unchanged
      in
      { path = p; outcome })
    all

(** The non-trivial (transfer-or-conflict) subset of a three-way plan:
    the rows a sync round must act on. *)
let actionable (plans : plan list) : plan list =
  List.filter
    (fun { outcome; _ } ->
      match outcome with
      | Unchanged | Ours_keep | Identical -> false
      | Theirs_add | Theirs_mod | Conflict _ | Theirs_del | Ours_del -> true)
    plans

(* ==================================================================
   DELTA EXCHANGE (the chunked protocol's server half, 2026-09-11)

   The v1 exchange carries the client's FULL manifest every round —
   O(subtree) on the wire, 413-bound past nginx's ~1MB request cap and
   unchunkable (a subset manifest classifies its absent paths as
   ServerOnly and inflates the pull list). The delta exchange stores
   the per-client last-synced merged manifest SERVER-SIDE (the merge
   index — sync_bases, migration 032; git's remote-tracking ref in
   miniature) and negotiates deltas against it: the client sends only
   what changed vs its base (changes + deletions); a path ABSENT from
   the delta means "client unchanged = base", so the server can
   reconstruct the client's full manifest locally and feed the SAME
   plan_of / commit machinery as v1. Requests become O(changes);
   chunks compose because every push2 re-derives theirs from the same
   stored base and applies per-path (idempotent).
   ================================================================== *)

(** [merge_delta base changes deleted] — the client's FULL manifest as
    implied by its delta against [base]: start from the base, apply the
    client's changed (path, hash) entries, drop the deleted paths. A
    path absent from both delta lists means the client never touched it
    (its hash is the base hash). Pure — the heart of the delta
    exchange; unit-tested in sync_test.ml. *)
let merge_delta ~(base : manifest) ~(changes : manifest)
    ~(deleted : string list) : manifest =
  let deleted_set =
    let t = Hashtbl.create (List.length deleted + 1) in
    List.iter (fun p -> Hashtbl.replace t p ()) deleted;
    t
  in
  let ch = Hashtbl.create (List.length changes + 1) in
  List.iter (fun (p, h) -> Hashtbl.replace ch p h) changes;
  (* base paths: dropped when deleted, overridden when changed *)
  let kept =
    List.filter
      (fun (p, _) ->
        not (Hashtbl.mem deleted_set p) && not (Hashtbl.mem ch p))
      (normalize base)
  in
  normalize (kept @ normalize changes)

(* ---- the merge index (sync_bases, migration 032) ---- *)

(** Load the stored base manifest for one client lens. Returns None when
    no base was ever committed for (user, lens) — the caller falls back
    to the v1 full-manifest exchange. *)
let load_base (store : Store.t) ~(user_id : string) ~(lens : string)
    : (manifest * string * Int64.t) option =
  match Store.conn_of store with
  | None -> None
  | Some conn -> (
      match
        Store_db.exec conn
          ~params:
            [ Pgx.Value.of_string user_id; Pgx.Value.of_string lens ]
          "SELECT manifest::text, branch, generation FROM sync_bases \
           WHERE user_id = $1::uuid AND lens = $2"
      with
      | [ [ m; b; g ] ] -> (
          let manifest =
            match Yojson.Safe.from_string (Pgx.Value.to_string m |> Option.value ~default:"[]") with
            | `List rows ->
                List.filter_map
                  (function
                    | `List [ `String p; `String h ] -> Some (p, h)
                    | _ -> None)
                  rows
            | _ -> []
          in
          let branch = Pgx.Value.to_string b |> Option.value ~default:"" in
          let gen =
            match Pgx.Value.to_string g with
            | Some s -> (try Int64.of_string s with _ -> 0L)
            | None -> 0L
          in
          Some (manifest, branch, gen))
      | _ -> None)

(** Store the merged manifest this client lens last synced at (upsert —
    one row per (user, lens), the merge index the next round's delta
    negotiates against). *)
let save_base (store : Store.t) ~(user_id : string) ~(lens : string)
    ~(branch : string) ~(generation : Int64.t) (manifest : manifest) : unit =
  match Store.conn_of store with
  | None -> ()
  | Some conn ->
      let rows =
        `List (List.map (fun (p, h) -> `List [ `String p; `String h ]) (normalize manifest))
      in
      ignore
        (Store_db.exec conn
           ~params:
             [ Pgx.Value.of_string user_id
             ; Pgx.Value.of_string lens
             ; Pgx.Value.of_string branch
             ; Pgx.Value.of_string (Int64.to_string generation)
             ; Pgx.Value.of_string (Yojson.Safe.to_string rows)
             ]
           "INSERT INTO sync_bases (user_id, lens, branch, generation, manifest, updated_at) \
            VALUES ($1::uuid, $2, $3, $4::bigint, $5::jsonb, now()) \
            ON CONFLICT (user_id, lens) \
            DO UPDATE SET branch = EXCLUDED.branch, generation = EXCLUDED.generation, \
            manifest = EXCLUDED.manifest, updated_at = now()")

(* ==================================================================
   SERVER-SIDE HARNESS (DB-backed)
   ================================================================== *)

(** Member-space replication prefixes for one identity (borge/local-first
    .borg replication-units, data.s.<space>.* — branching.shared-data-spaces
    membership from migration 029): encoded data.s.<space> prefixes for every
    space the user is a member of, per the same authorized-reads argument as
    a member space's read rights. Empty without a live connection. *)
let member_space_prefixes (store : Store.t) ~(user : string) : string list =
  match Store.conn_of store with
  | None -> []
  | Some conn ->
      let rows =
        Store_db.exec conn
          ~params:[ Pgx.Value.of_string user ]
          "SELECT space FROM data_space_members WHERE username = $1 ORDER BY space"
      in
      List.filter_map
        (function
          | [ s ] -> (
              match Pgx.Value.to_string s with
              | Some sp when sp <> "" ->
                  Some ("data.s." ^ Tree_store.enc_label sp)
              | _ -> None)
          | _ -> None)
        rows

(** Read the server's (path, hash) manifest for the subtrees under
    [prefixes] (one-tree path-prefix RLS confinement = the replication
    boundary; the caller passes the replication-unit prefixes: the user's
    data.u.<owner> subtree plus their member-space data.s.<space> subtrees —
    for spaces, PULL + PROPOSE per the data.shared policy, never lens-writes).
    None when there is no live connection. *)
let server_manifest (store : Store.t) ~(prefixes : string list) :
    manifest option =
  match Store.conn_of store with
  | None -> None
  | Some conn ->
      let rows =
        List.concat_map
          (fun prefix ->
            Store_db.exec conn
              ~params:[ Pgx.Value.of_string prefix ]
              "SELECT path::text, hash FROM tree_nodes \
               WHERE path <@ $1::ltree ORDER BY path::text ASC")
          prefixes
      in
      Some
        (List.filter_map
           (function
             | [ p; h ] -> (
                 match Pgx.Value.to_string p, Pgx.Value.to_string h with
                 | Some pp, Some hh -> Some (pp, hh)
                 | _ -> None)
             | _ -> None)
           rows)

(** Apply a client push body for [path] to the store. The client sends
    its node envelope {path, kind, type, body, author, …}; the server
    re-writes it through the typed-boundary-validated tree write surface
    so a datum lands exactly as (tree/upsert) would (cross-checks the
    declared type fields). Kind-aware:

      - fn / type / route / pkg code nodes resolve the branch-lensed path
        under the current lens branch and write via the code write
        surface (upsert_fn / upsert_type / route via upsert_hier).
      - datum nodes write via [Tree_store.upsert_datum] (path-explicit,
        typed-boundary validation preserved).

    Returns [Ok ()] or [Error message]. *)
let apply_client_node (store : Store.t) ~(env : Yojson.Safe.t) : (unit, string) result =
  let path, kind, type_name, body, author =
    match env with
    | `Assoc kv ->
        let str k = List.assoc_opt k kv |> function Some (`String s) -> Some s | _ -> None in
        let get_body =
          List.assoc_opt "body" kv |> function Some b -> Some b | _ -> None
        in
        ( str "path"
        , str "kind"
        , str "type"
        , get_body
        , (match str "author" with Some a -> a | None -> "sync") )
    | _ -> (None, None, None, None, "sync")
  in
  match path, kind with
  | Some p, Some "datum" ->
      (match type_name, body with
       | Some ty, Some b ->
             (* typed-boundary validation: validate the client body against
                the type the SERVER declares for it (the server is the
                merge/index authority, not the client) so a body missing a
                required field is rejected the same way a local create
                would be — the client is a peer, not a bypass. An
                UNDECLARED type is a clean rejection (one-tree
                typed-nodes declared-type gate); a declared type with zero
                fields stays free-form. *)
             (let reject =
                match Store.lookup_type_fields store ~name:ty with
                | None ->
                    Some
                      (Printf.sprintf
                         "sync: undeclared type %s (declare it with (deftype '%s ...))"
                         ty ty)
                | Some fields ->
                    (match
                       Tree_store.validate_fields ~declared:fields
                         (Yojson.Safe.to_string b)
                     with
                    | Ok () -> None
                    | Error msg -> Some msg)
              in
              match reject with
             | Some msg -> Error msg
           | None ->
               (match Store.conn_of store with
                | None -> Error "sync: no DB"
                | Some conn ->
                    ignore
                      (Tree_store.upsert_datum conn ~path:p ~type_name:ty
                         ~body_json:(Yojson.Safe.to_string b) ~author ~rank:None);
                    Ok ()))
       | _ -> Error "sync: datum node missing type/body")
  | Some p, Some ("fn" | "type" | "route" | "pkg" as k) ->
      (* code-lens nodes carry {body:{src,…}}; the server re-writes the
         source through the code write surface so the reverse mirror
         repopulates the scaffolding tables. For v0 the code sync path is
         the branch-pointer move (promote/merge) owned by merge_branch;
         a direct code-node write here is the fallback for a code lens the
         server chose to reconcile in place. *)
      (match Store.conn_of store with
       | None -> Error "sync: no DB"
       | Some conn ->
           let body_json = match body with Some b -> Yojson.Safe.to_string b | None -> "{}" in
           (match k with
            | "fn" ->
                let name =
                  try List.nth (String.split_on_char '.' p) 3 with _ -> p
                in
                ignore
                  (Tree_store.upsert_fn conn
                     ~branch_id:(Store.current_branch_id store)
                     ~name ~body_json ~author ~doc:(Some ("code:" ^ name)));
                Ok ()
            | _ -> Error "sync: code kind not yet reconciled in place"))
  | Some p, Some k ->
      (* hierarchical / other kinds: write via the generic path-explicit
         upsert_hier so headings/folders/notes replicate too. *)
      (match Store.conn_of store with
       | None -> Error "sync: no DB"
       | Some conn ->
           let body_json = match body with Some b -> Yojson.Safe.to_string b | None -> "{}" in
           ignore
             (Tree_store.upsert_hier conn ~path:p ~kind:k
                ~type_name:(match type_name with Some t -> t | None -> k)
                ~body_json ~author ~rank:None);
           Ok ())
  | _ -> Error "sync: node envelope missing path/kind"

(** Apply a whole reconcile/merge plan to the server: for every actionable
    row the plan directs the server to take (Theirs_add / Theirs_mod /
    Theirs_del are applied from the client bodies; a Conflict is LWW-
    arbitrated per sync-conflicts v0 — client envelope strictly newer wins
    and is applied, otherwise the server copy stays), look up the client
    node envelope. Since this server harness applies the PLAN (not the
    bodies it must first receive), the client bodies are passed as an
    assoc path -> envelope JSON. Every resolution is SURFACED: the report
    lists the conflicts with their hashes, which side LWW picked per
    path, and the Ours_del paths the client must mirror-delete locally. *)

(* ---- sync-conflicts v0: LWW-per-node resolution (applied + surfaced) ----

    Detection was 8.3 (hashes classify; conflicts REPORT-only). The v0
    RESOLUTION policy sync-conflicts names is LAST-WRITER-WINS per node,
    updated_at arbitrating, with every loss SURFACED (the obituary rule
    applies to sync losses — no silent drop). One clock, both sides:
    the client lens substrate stamps its envelope updated_at as an epoch
    float (string_of_float Unix.gettimeofday); the server reads its node
    updated_at as the same epoch float (extract(epoch from updated_at)).
    The client wins a diverge/conflict pair iff its stamp is STRICTLY
    newer — ties and missing/unparseable stamps keep the server copy
    (deterministic, conservative; a materialized-from-server envelope
    carries the server's PG-format stamp, which is intentionally not
    parseable as an epoch here, so an unedited pull can never wrongly
    out-vote a newer server edit). Cross-machine clock skew is the
    classic LWW tax; the spec accepts it for v0 and surfaces every
    resolution. `commit` and the sync/push prim both apply this policy. *)

(** The server's updated_at epochs (float seconds since the epoch) for
    the given paths — the LWW arbitration clock. Paths absent from
    tree_nodes are absent from the result. *)
let server_updated_at (store : Store.t) (paths : string list)
    : (string * float) list =
  match Store.conn_of store with
  | None -> []
  | Some conn ->
      List.filter_map
        (fun p ->
          match
            Store_db.exec conn
              ~params:[ Pgx.Value.of_string p ]
              "SELECT extract(epoch from updated_at)::float8 FROM tree_nodes \
               WHERE path = $1::ltree"
          with
          | [ [ v ] ] -> (
              match Pgx.Value.to_float v with
              | Some f -> Some (p, f)
              | None -> None)
          | _ -> None)
        paths

(** A client node envelope's updated_at as an epoch float, or None when
    absent/unparseable. *)
let envelope_updated_at (env : Yojson.Safe.t) : float option =
  match env with
  | `Assoc kv -> (
      match List.assoc_opt "updated_at" kv with
      | Some (`String s) -> (try Some (float_of_string s) with _ -> None)
      | _ -> None)
  | _ -> None

(** LWW v0 for one diverge/conflict pair: the client wins iff its stamp
    is STRICTLY newer than the server node's. *)
let lww_client_wins (client_updated : float option) (server_updated : float)
    : bool =
  match client_updated with Some c -> c > server_updated | None -> false

type commit_report = {
  applied : int;
  conflicts : (string * string * string) list; (* path, ours_hash, theirs_hash *)
  lww_client_won : string list;  (* conflicts arbitrated TO the client copy (applied) *)
  lww_server_kept : string list; (* conflicts arbitrated TO the server copy (client edit surfaced as lost) *)
  client_deletes : string list;  (* Ours_del: server deleted; client's stale copy must go too *)
  failed : string list;          (* pushes the server REJECTED (typed-boundary validation etc.) —
                                    surfaced so the client keeps + re-pushes them instead of
                                    converging to the server copy (the obituary rule) *)
}

(** True for a shared-space data path (data.s.<space>.<type>.<id>,
    migration 029). Space subtrees are NEVER in a lens's write scope
    (borge/local-first.borg sync, lines "SPACE SUBTREES ... INHERIT THE
    data.shared POLICY VERBATIM"): the lens pulls them and the server
    stays sole write authority, so any client-driven apply/delete of a
    space path is rejected here and surfaced via the failed list. *)
let is_space_path (p : string) : bool =
  let pre = "data.s." in
  String.length p >= String.length pre && String.sub p 0 (String.length pre) = pre

let commit (store : Store.t)
    ~(client_bodies : (string * Yojson.Safe.t) list)
    (plans : plan list) : commit_report =
  let epochs =
    server_updated_at store
      (List.filter_map
         (fun { path; outcome } ->
           match outcome with Conflict (p, _, _, _) -> Some p | _ -> None)
         plans)
  in
  (* space guard: reject every client-driven write outcome on a space
     path (Theirs_add/Theirs_mod/Theirs_del + LWW-apply on Conflict) —
     they land in [failed] so the client is told to drop the local copy
     and keep the server's. Ours_del passes through: a server-side
     space deletion is the authority speaking, and must mirror. *)
  List.fold_left
    (fun r { path; outcome } ->
      match outcome with
      | Theirs_add | Theirs_mod when is_space_path path ->
          { r with failed = path :: r.failed }
      | Theirs_del when is_space_path path ->
          { r with failed = path :: r.failed }
      | Conflict (p, _, _, _) when is_space_path p ->
          { r with failed = p :: r.failed }
      | Theirs_add | Theirs_mod ->
          (match List.assoc_opt path client_bodies with
           | Some env ->
               (match apply_client_node store ~env with
                | Ok () -> { r with applied = r.applied + 1 }
                | Error _ -> { r with failed = path :: r.failed })
           | None -> { r with failed = path :: r.failed })
      | Theirs_del ->
          (match Store.conn_of store with
           | Some conn ->
               let n = Tree_store.delete_path conn ~path ~recursive:false in
               if n > 0 then { r with applied = r.applied + 1 } else r
           | None -> r)
      | Conflict (p, _b, o, t) ->
          (* LWW v0 (sync-conflicts): arbitrate by updated_at, then SURFACE
             the resolution either way — the obituary rule applies to the
             losing side, never a silent drop. *)
          let r = { r with conflicts = (p, o, t) :: r.conflicts } in
          let client_won =
            match List.assoc_opt p epochs with
            | None -> false (* server node unreadable: keep the server *)
            | Some sv ->
                let cl =
                  match List.assoc_opt p client_bodies with
                  | Some env -> envelope_updated_at env
                  | None -> None
                in
                lww_client_wins cl sv
          in
          if client_won then
            match List.assoc_opt p client_bodies with
            | Some env -> (
                match apply_client_node store ~env with
                | Ok () ->
                    { r with
                      applied = r.applied + 1;
                      lww_client_won = p :: r.lww_client_won }
                | Error _ ->
                    { r with lww_server_kept = p :: r.lww_server_kept })
            | None -> { r with lww_server_kept = p :: r.lww_server_kept }
          else { r with lww_server_kept = p :: r.lww_server_kept }
      | Ours_del -> { r with client_deletes = path :: r.client_deletes }
      | Unchanged | Ours_keep | Identical -> r)
    {
      applied = 0;
      conflicts = [];
      lww_client_won = [];
      lww_server_kept = [];
      client_deletes = [];
      failed = [];
    }
    plans

(* ---- report rendering (for the sync/plan prim + UI) ---- *)

let json_of_report (r : classify_report) : string =
  Yojson.Safe.to_string
    (`Assoc
       [ ("same", `Int r.same_len)
       ; ("client_only", `Int (List.length r.client_only))
       ; ("server_only", `Int (List.length r.server_only))
       ; ("diverge", `Int (List.length r.diverge))
       ; ("client_only_paths", `List (List.map (fun (p, _) -> `String p) (normalize r.client_only)))
       ; ("server_only_paths", `List (List.map (fun (p, _) -> `String p) (normalize r.server_only)))
       ; ("diverge_paths", `List (List.map (fun (p, _) -> `String p) (normalize r.diverge)))
       ])

let json_of_plan (plans : plan list) : string =
  Yojson.Safe.to_string
    (`Assoc
       [ ( "rows"
         , `List
             (List.map
                (fun { path; outcome } ->
                  let tag, detail =
                    match outcome with
                    | Unchanged -> ("unchanged", `Null)
                    | Theirs_add -> ("theirs_add", `String path)
                    | Ours_keep -> ("ours_keep", `String path)
                    | Theirs_mod -> ("theirs_mod", `String path)
                    | Identical -> ("identical", `Null)
                    | Conflict (_, b, o, t) ->
                        ("conflict",
                         `Assoc [ ("base", `String b); ("ours", `String o); ("theirs", `String t) ])
                    | Theirs_del -> ("theirs_del", `String path)
                    | Ours_del -> ("ours_del", `String path)
                  in
                  `Assoc [ ("path", `String path); ("outcome", `String tag); ("detail", detail) ])
                plans)
         ) ])

