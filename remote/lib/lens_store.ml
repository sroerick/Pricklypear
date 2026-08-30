(** remote/lib/lens_store.ml — borge/local-first.borg local-substrate.

    A local lens is a branch of the one tree whose root lives on a client.
    This module is the FILESPUN SUBSTRATE (local-substrate): a directory
    tree of node-envelope JSON files, one file per node, at its path. It
    also pins the two slices the 8.1 make of lens-model + replication-units
    is concretely responsible for:

      - LENS FORK / NAMING (lens-model). A clone forks the user's current
        branch into a client-owned lens via the EXISTING (fork ...) /
        Store.fork_branch (an empty child branch, parent pointer only).
        [fork_lens_name] builds the <owner>/lens-<device>[-<date>] branch
        name that fork is given (e.g. admin/personal -> admin/lens-laptop-
        2026-08-14). The lens is single-writer; the server never writes into
        it except via sync/merge; local writes are commits whose atomicity
        is the substrate's (rename(2)).

      - REPLICATION UNIT (replication-units). [in_replication_unit] decides
        which node paths travel to a client — exactly the authorized-read
        subtree: data.u.<user>.* (the user's own rows, full), data.shared.*
        (read-mostly seed), b.<lens>.* (the forked code lens within the
        subtree). Inheritance (defs resolving through branch ancestry) is
        READ-TIME resolution -- the client stores only the lens's own
        overrides, not a per-ancestor copy. Nothing outside the path-prefix
        confinement (the one-tree RLS floor) is offered for replication: the
        client cannot ask for, and the server cannot send, a path the
        identity cannot read in a normal session.

    Substrate interface (the spec pins: path-addressed get/put/subtree-list
    + envelope + hash). [materialize] writes a single node JSON under the
    lens root at its path (one file per node); [get]/[subtree_list] are the
    path-addressed reads; [node_hash] is the content hash (the git-style
    "hash over the canonical body" that sync's three-way-diff keys on).
    Every write is atomic (temp file + rename(2)) — the single-writer
    atomicity the lens gets WITHOUT a second engine (no SQLite, no second
    query engine; the server stays the index).

    This module is pure filesystem + JSON: no Postgres, no Lwt, no network.
    It lives in the client package (remote/) so the offline lens works even
    when the server is unreachable. *)

(** Content hash over a canonical JSON string — the node-envelope [hash]
    field, the git-style "hash over the canonical body" that sync's
    three-way-diff keys on. Pure digestif (the same digest family the lisp
    package uses); no image-side dependency, so the client computes hashes
    offline. *) 
let content_hash (s : string) : string =
  Digestif.SHA256.to_hex (Digestif.SHA256.digest_string s)

(** Whole-file read (standard library; no external File_io). *) 
let read_file (file : string) : string =
  let ic = open_in_bin file in
  let n = in_channel_length ic in
  let buf = Bytes.create n in
  really_input ic buf 0 n;
  close_in ic;
  Bytes.unsafe_to_string buf

(** UUID -> ltree-safe label (strip dashes). Mirrors
    image/lib/tree_store.ml [uuid_label] so a client can build a datum
    node path [data.u.<user>.<type>.<id>] from a raw UUID. *)
let uuid_label (u : string) : string =
  String.concat "" (String.split_on_char '-' u)

let enc_label (s : string) : string =
  (* Filesystem- and ltree-safe label encoding, mirrored from
     image/lib/tree_store.ml so a client can reproduce the replication
     path prefix for its own name without round-tripping the server. *)
  if s = "" then "x"
  else begin
    let b64 = Base64.encode_string ~pad:true s in
    let len = String.length b64 in
    let pad =
      if len >= 2 && String.sub b64 (len - 2) 2 = "==" then 2
      else if len >= 1 && b64.[len - 1] = '=' then 1
      else 0
    in
    let core = if pad > 0 then String.sub b64 0 (len - pad) else b64 in
    let buf = Bytes.of_string core in
    for i = 0 to Bytes.length buf - 1 do
      match Bytes.get buf i with
      | '+' -> Bytes.set buf i '_'
      | '/' -> Bytes.set buf i '-'
      | c -> ignore c
    done;
    Bytes.unsafe_to_string buf ^ string_of_int pad
  end

(** A client materialized lens. [root] is the filesystem directory holding
    the node-envelope tree (local-substrate); [owner]/[device]/[branch]/
    [generation] identify the (branch, generation) snapshot the lens is
    pinned at — the same migration-017 / gen-now coordinate sync negotiates
    on (last-synced = the merge base). *)
type lens = {
  root : string;
  owner : string;
  device : string;
  branch : string;      (* the forked lens branch id, e.g. the child of (fork) *)
  generation : int64;   (* branch_generations / gen-now snapshot *)
}

(** lens-model: the <owner>/lens-<device>[-<date>] fork name a clone mints
    for its lens branch. The existing (fork ...) / Store.fork_branch creates
    the empty child branch; this is the naming that keeps one device's lens
    distinct from another's (star topology, all rendezvousing at the owner's
    server branch). *)
let fork_lens_name ?date ~owner ~device () : string =
  match date with
  | None -> Printf.sprintf "%s/lens-%s" owner device
  | Some d -> Printf.sprintf "%s/lens-%s-%s" owner device d

(** Split a tree node path into its dot-separated segments. *)
let path_segments (path : string) : string list =
  String.split_on_char '.' path

(** replication-units: is [path] inside the replication unit for [user] with
    code lens [lens_prefix]? Exactly the authorized-read subtree:
      data.u.<user>.*   (the user's own rows, full)
      data.shared.*     (read-mostly seed)
      b.<lens_prefix>.* (the forked code lens)
    NOTHING outside it — another user's data.u.<other>.* never replicates,
    by the same path-prefix confinement that governs reads. *)
let datum_path ~(user : string) ~type_name ~id () : string =
  Printf.sprintf "data.u.%s.%s.%s" (enc_label user) (enc_label type_name)
    (uuid_label id)

let in_replication_unit ?(lens_prefix : string = "") ~user (path : string)
    : bool =
  match path_segments path with
  | "data" :: "u" :: u_seg :: _ -> u_seg = enc_label user
  | "data" :: "shared" :: _ -> true
  | "b" :: branch_uu :: _ ->
      String.length branch_uu = 32 && lens_prefix <> ""
      && branch_uu = lens_prefix
  | _ -> false

(** Map a tree node path to its file path under the lens root: one file per
    node, at its path, as a nested directory tree (the pp-sync / filetree
    format, inspectable with cat / jq / grep / diff / git). E.g.
      b.<uuid>.fn.<name>       -> b/<uuid>/fn/<name>.json
      data.u.admin.todo.<id>   -> data/u/admin/todo/<id>.json *)
let path_to_file ~(root : string) (path : string) : string =
  match path_segments path with
  | [] -> Filename.concat root "__root.json"
  | segs ->
      let dirs, leaf =
        match List.rev segs with
        | last :: rest -> (List.rev rest, last)
        | [] -> ([], "")
      in
      let dir = Filename.concat root (String.concat Filename.dir_sep dirs) in
      Filename.concat dir (leaf ^ ".json")

(** mkdir_p for the parent directory of [file]. *)
let mkdir_p_parent (file : string) : unit =
  let dir = Filename.dirname file in
  let rec mkdir d =
    if d = "" || d = "." || d = Filename.dir_sep then ()
    else begin
      if not (Sys.file_exists d) then begin
        mkdir (Filename.dirname d);
        (try Unix.mkdir d 0o755 with
         | Unix.Unix_error (Unix.EEXIST, _, _) -> ())
      end
    end
  in
  mkdir dir

(** Atomic write: write [contents] to a temp file in the same directory then
    rename(2) over [file]. This is the single-writer atomicity the lens
    substrate provides WITHOUT a concurrency engine. *)
let atomic_write (file : string) (contents : string) : unit =
  mkdir_p_parent file;
  let tmp = Filename.concat (Filename.dirname file) (".tmp-" ^ string_of_int (Unix.getpid ())) in
  let fd = Unix.openfile tmp [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] 0o600 in
  (try
     let buf = Bytes.of_string contents in
     let len = Bytes.length buf in
     let written = ref 0 in
     while !written < len do
       let n = Unix.write fd buf !written (len - !written) in
       written := !written + n
     done;
     (* fsync before rename so a crash never leaves a truncated envelope
        at the leaf path *)
     Unix.fsync fd;
     Unix.close fd
   with exn ->
     (try Unix.close fd with _ -> ());
     (try Sys.remove tmp with _ -> ());
     raise exn);
  Unix.rename tmp file

(** Extract the [path] field from a node-envelope JSON, or None. *)
let node_path_of_json (node_json : string) : string option =
  match Yojson.Safe.from_string node_json with
  | `Assoc kv -> (match List.assoc_opt "path" kv with
      | Some (`String p) -> Some p | _ -> None)
  | _ -> None

(** materialize: write one node envelope under the lens root at its path.
    Returns the written file path. The node JSON is the server-side envelope
    ({v,kind,path,type,body,author,doc,hash,created_at,updated_at} — the
    tree_store.node_to_json surface); the file IS that envelope verbatim. *)
let materialize ~(lens : lens) (node_json : string) : string option =
  match node_path_of_json node_json with
  | None -> None
  | Some path ->
      let file = path_to_file ~root:lens.root path in
      atomic_write file node_json;
      Some file

(** put_body: build a node envelope from a canonical body and write it at
    [path]. This is the substrate write path a local edit uses while the
    lens is offline (single-writer; no server round trip). The hash is the
    content hash over the canonical body (the sync/three-way-diff key). *)
let put_body ~(lens : lens) ~path ~kind ~type_ ~author ~(body : Yojson.Safe.t)
    : unit =
  let hash = content_hash (Yojson.Safe.to_string body) in
  let env =
    Yojson.Safe.to_string
      (`Assoc
         [ ("v", `Int 1)
         ; ("kind", `String kind)
         ; ("path", `String path)
         ; ("type", `String type_)
         ; ("body", body)
         ; ("author", `String author)
         ; ("doc", `Null)
         ; ("hash", `String hash)
         ; ("created_at", `String (string_of_float (Unix.gettimeofday ())))
         ; ("updated_at", `String (string_of_float (Unix.gettimeofday ())))
         ])
  in
  let file = path_to_file ~root:lens.root path in
  atomic_write file env

(** get: read the node envelope at [path] (None if absent). *)
let get ~(lens : lens) (path : string) : string option =
  let file = path_to_file ~root:lens.root path in
  if Sys.file_exists file then
    (try Some (read_file file) with _ -> None)
  else None

(** subtree_list: all node envelope file paths under the lens root whose
    tree path begins with [prefix] (readdir/find — the filesystem IS the
    path index; no second query engine). Returns tree paths. *)
let subtree_list ~(lens : lens) (prefix : string) : string list =
  let prefix_file = path_to_file ~root:lens.root prefix in
  let prefix_dir = Filename.dirname prefix_file in
  let rec walk dir acc =
    if not (Sys.file_exists dir) then acc
    else begin
      try
        Sys.readdir dir
        |> Array.fold_left (fun acc name ->
             let full = Filename.concat dir name in
             if Sys.is_directory full then walk full acc
             else if Filename.check_suffix name ".json" then full :: acc
             else acc)
             acc
      with _ -> acc
    end
  in
  let files = walk prefix_dir [] in
  List.map
    (fun f ->
      match node_path_of_json (read_file f) with
      | Some p -> p
      | None -> "")
    files
  |> List.filter (fun p -> p <> "" && String.length p >= String.length prefix
                          && String.sub p 0 (String.length prefix) = prefix)

(** node_hash: the content hash for the node envelope at [path] — the
    {v,kind,path,type,body,author,doc,hash} envelope hash field, which
    sync's three-way-diff keys on. *)
let node_hash ~(lens : lens) (path : string) : string option =
  match get ~lens path with
  | None -> None
  | Some json ->
      (match Yojson.Safe.from_string json with
       | `Assoc kv ->
           (match List.assoc_opt "hash" kv with
            | Some (`String h) -> Some h | _ -> None)
       | _ -> None)

(* ====================================================================
   CODE-LENS + ANCESTRY MATERIALIZATION (local-substrate, 8.2)

   The replication-unit manifest (replication-units subsection) names the
   forked code lens b.<lens>.{fn,type,route,pkg}.* PLUS the branch
   ancestry the lens resolves through (defs resolve through inheritance
   per one-tree tree-model read-time resolution). These helpers complete
   the substrate's coverage of that manifest: the client can materialize
   the forked code lens + record the ancestry chain it resolves through,
   then resolve ANY definition (offline) by walking the chain
   self-first with the same closest-ancestor-wins rule a server-side
   eval of the lens would use (store.branch_chain self-first).

   One file per code node at its b.<branch>.<kind>.<name> tree path,
   exactly like a datum node — the filesystem stays the only index, and
   sync (3-way tree-diff) keys on the same envelope [hash].
   ==================================================================== *)

(** The code-node kinds the replication unit manifests (fn/type/route/pkg).
    Any other kind is not a code-lens subtree member. *)
let code_kind_path ~branch (kind : string) ~(name : string) : string option =
  match kind with
  | "fn" | "type" | "route" | "pkg" ->
      Some (Printf.sprintf "b.%s.%s.%s" branch kind name)
  | _ -> None

(** Materialize one code-node envelope at its b.<branch>.<kind>.<name> tree
    path. [src] is the evaluable definition source (a toplevel (define …)
    form — the same string a server-side eval would build); [doc] the
    docstring. The envelope follows the one-tree tree-model shape; [hash]
    is the content hash over the canonical body.

    Returns the written file path, or None when [kind] is not a code kind. *)
let materialize_code_node ~(lens : lens) ~branch ~kind ~(name : string)
    ~(src : string) ~(doc : string) ~(author : string)
    : string option =
  match code_kind_path ~branch kind ~name with
  | None -> None
  | Some path ->
      let body =
        `Assoc [ ("src", `String src); ("doc", `String doc) ]
      in
      let hash = content_hash (Yojson.Safe.to_string body) in
      let env =
        Yojson.Safe.to_string
          (`Assoc
             [ ("v", `Int 1)
             ; ("kind", `String kind)
             ; ("path", `String path)
             ; ("type", `String "code")
             ; ("body", body)
             ; ("author", `String author)
             ; ("doc", `String doc)
             ; ("hash", `String hash)
             ; ("created_at", `String (string_of_float (Unix.gettimeofday ())))
             ; ("updated_at", `String (string_of_float (Unix.gettimeofday ())))
             ])
      in
      let file = path_to_file ~root:lens.root path in
      atomic_write file env;
      Some file

(** Extract the evaluable definition [src] from a code-node envelope which
    carries {body:{src,…}} (the [materialize_code_node] shape). Returns
    None when the envelope is absent or not a code node with a src body. *)
let code_src_of_envelope (envelope_json : string) : string option =
  match Yojson.Safe.from_string envelope_json with
  | `Assoc kv -> (
      match List.assoc_opt "body" kv with
      | Some (`Assoc bkv) -> (
          match List.assoc_opt "src" bkv with
          | Some (`String src) -> Some src | _ -> None)
      | _ -> None)
  | _ -> None

(** Extract the [body] of a DATUM node envelope as a JSON string — the
    shape [(get 'type id)] returns (the row's fields). Returns None when
    the envelope is absent or has no body. This is the lens-local
    equivalent of the server's [Crud.get]: the datum node's [body] IS the
    row's fields JSON. *)
let datum_body_of_envelope (envelope_json : string) : string option =
  match Yojson.Safe.from_string envelope_json with
  | `Assoc kv ->
      (match List.assoc_opt "body" kv with
       | Some body -> Some (Yojson.Safe.to_string body)
       | None -> None)
  | _ -> None

(** The branch ancestry chain metadata file under the lens root. The chain
    is ordered SELF-FIRST (closest-wins): [lens, parent, …, system/main],
    the same ordering store.branch_chain / function_defs override use. *)
let ancestry_file (root : string) : string =
  Filename.concat root "__ancestry.json"

(** Record the ancestry chain a lens resolves defs through. Self-first
    (closest-wins), oldest ancestor last. Atomic (rename(2)), like every
    other substrate write. *)
let record_ancestry ~(lens : lens) (chain : string list) : unit =
  let json =
    Yojson.Safe.to_string
      (`Assoc
         [ ("chain", `List (List.map (fun b -> `String b) chain))
         ; ("recorded_at", `String (string_of_float (Unix.gettimeofday ())))
         ])
  in
  atomic_write (ancestry_file lens.root) json

(** Read back the recorded ancestry chain (self-first). Empty when none. *)
let ancestry ~(lens : lens) : string list =
  let f = ancestry_file lens.root in
  if not (Sys.file_exists f) then []
  else
    (try
       match Yojson.Safe.from_string (read_file f) with
       | `Assoc kv -> (
           match List.assoc_opt "chain" kv with
           | Some (`List l) ->
               List.filter_map (function `String b -> Some b | _ -> None) l
           | _ -> [])
       | _ -> []
     with _ -> [])

(** resolve_def: find the def (kind, name) in the substrate by walking the
    ancestry chain SELF-FIRST (closest-ancestor-wins — the same override
    rule a server-side eval of the lens would use), returning the envelope
    JSON of the closest b.<branch>.<kind>.<name> that exists in the
    substrate, or None. Pure / offline: reads the filesystem only; it is
    the read-time inheritance resolution of one-tree tree-model, without a
    server round trip. ([chain] defaults to the lens's recorded ancestry.) *)
(** resolve_def over an EXPLICIT chain (self-first). The caller controls
    the chain (e.g. a freshly forked lens whose ancestry it just recorded,
    to avoid re-touching the metadata file). *)
let resolve_def_chain ~(lens : lens) (chain : string list) ~kind
    ~(name : string) : string option =
  List.find_map
    (fun branch ->
      match code_kind_path ~branch kind ~name with
      | None -> None
      | Some path -> get ~lens path)
    chain

(** resolve_def: find the def (kind, name) in the substrate by walking the
    lens's RECORDED ancestry chain SELF-FIRST (closest-ancestor-wins — the
    same override rule a server-side eval of the lens would use), returning
    the envelope JSON of the closest b.<branch>.<kind>.<name> that exists in
    the substrate, or None. Pure / offline: reads the filesystem only; it is
    the read-time inheritance resolution of one-tree tree-model, without a
    server round trip. *)
let resolve_def ~(lens : lens) ~kind ~(name : string) : string option =
  resolve_def_chain ~lens (ancestry ~lens) ~kind ~name

(** The subtree paths the replication unit names, materialized under a
    lens root, in a form the server manifest can diff against: every code
    node present in the substrate (its b.<branch>.<kind>.<name> tree
    paths) plus every datum node, each with its envelope hash. This is the
    (path, hash) manifest a sync round negotiates on (see the sync
    stanza). Returns [(path, hash)] pairs for every .json node envelope
    under the lens root (excluding the ancestry metadata file). *)
let node_manifest ~(lens : lens) : (string * string) list =
  let prefix = "" in
  let prefix_file = path_to_file ~root:lens.root prefix in
  let prefix_dir = Filename.dirname prefix_file in
  let rec walk dir acc =
    if not (Sys.file_exists dir) then acc
    else begin
      try
        Sys.readdir dir
        |> Array.fold_left (fun acc name ->
             let full = Filename.concat dir name in
             if Sys.is_directory full then walk full acc
             else if Filename.check_suffix name ".json" then full :: acc
             else acc)
             acc
      with _ -> acc
    end
  in
  (* hash of an arbitrary node-envelope FILE (extract path + hash) *)
  let path_hash_of_file f : (string * string) option =
    match node_path_of_json (read_file f) with
    | None -> None
    | Some p ->
        let h =
          match Yojson.Safe.from_string (read_file f) with
          | `Assoc kv -> (
              match List.assoc_opt "hash" kv with
              | Some (`String h) -> h | _ -> "")
          | _ -> ""
        in
        Some (p, h)
  in
  let files =
    walk prefix_dir []
    |> List.filter (fun f ->
         let b = Filename.basename f in
         b <> "__root.json" && b <> "__ancestry.json" && b <> "__snapshot.json"
         && b <> "__base.json")
  in
  List.filter_map path_hash_of_file files

(** delete_node: remove the node envelope at [path] (the Ours_del mirror:
    the server deleted the node, the client's stale copy goes too).
    Best-effort empty-parent cleanup; returns true when a file was
    removed. *)
let delete_node ~(lens : lens) (path : string) : bool =
  let file = path_to_file ~root:lens.root path in
  if Sys.file_exists file then begin
    (try Sys.remove file with _ -> ());
    (* prune now-empty parent dirs up to the lens root (cosmetic; the
       filesystem index regenerates on demand) *)
    let rec prune d =
      let root = Filename.concat lens.root "" in
      if d <> lens.root && String.length d > String.length root then begin
        (try if Sys.readdir d |> Array.length = 0 then Sys.rmdir d with _ -> ());
        prune (Filename.dirname d)
      end
    in
    prune (Filename.dirname file);
    true
  end
  else false

(* ====================================================================
   BASE MANIFEST (sync's common ancestor, client side)

   The (path, hash) manifest the server returned at the client's last
   successful sync round (merged_manifest). Recorded to __base.json and
   offered as the OPTIONAL third argument of the next sync/push so the
   server runs the full three-way plan: clean client edits apply,
   client DELETIONS propagate (theirs_del), server deletions come back
   as client_delete_paths (Ours_del), conflicts LWW-arbitrate. Without
   a recorded base the round degrades to two-way (additions + LWW only)
   — the exact behavior before a first sync ever ran.
   ==================================================================== *)

let base_file (root : string) : string =
  Filename.concat root "__base.json"

(** Record the last-synced (merged) server manifest as the next round's
    base. Atomic (rename(2)) like every substrate write. *)
let record_base_manifest ~(lens : lens) (m : (string * string) list) : unit =
  let json =
    Yojson.Safe.to_string
      (`Assoc
         [ ( "paths"
           , `List
               (List.map
                  (fun (p, h) -> `List [ `String p; `String h ])
                  m)
           )
         ; ("recorded_at", `String (string_of_float (Unix.gettimeofday ())))
         ])
  in
  atomic_write (base_file lens.root) json

(** Read back the recorded base manifest (empty when none was ever
    recorded — a fresh lens syncs two-way first). *)
let base_manifest ~(lens : lens) : (string * string) list =
  let f = base_file lens.root in
  if not (Sys.file_exists f) then []
  else
    (try
       match Yojson.Safe.from_string (read_file f) with
       | `Assoc kv -> (
           match List.assoc_opt "paths" kv with
           | Some (`List rows) ->
               List.filter_map
                 (function
                   | `List [ `String p; `String h ] -> Some (p, h)
                   | _ -> None)
                 rows
           | _ -> [])
       | _ -> []
     with _ -> [])

(* ====================================================================
   SNAPSHOT COORDINATE (sync's common ancestor)

   The (branch, generation) the lens was last synced at — the merge
   base a sync round's three-way diff keys on (local-first.borg sync:
   "last-synced as the common ancestor"). Stamped at pull time; read back
   to offer as the base manifest coordinate. One file under the lens
   root; atomic (rename(2)) like every substrate write. *)

let snapshot_file (root : string) : string =
  Filename.concat root "__snapshot.json"

(** Record the (branch, generation) snapshot the lens was materialized
    at. [generation] is the migration-017 / gen-now coordinate; [branch]
    is the checked-out branch name. Both come from the server at pull
    time (proxied read prims), so a later sync can offer this snapshot
    as the three-way merge base. *)
let record_snapshot ~(lens : lens) ~branch ~(generation : int64) : unit =
  let json =
    Yojson.Safe.to_string
      (`Assoc
         [ ("branch", `String branch)
         ; ("generation", `String (Int64.to_string generation))
         ; ("pulled_at", `String (string_of_float (Unix.gettimeofday ())))
         ])
  in
  atomic_write (snapshot_file lens.root) json

(** Read back the recorded (branch, generation) snapshot, or None. *)
let snapshot ~(lens : lens) : (string * int64) option =
  let f = snapshot_file lens.root in
  if not (Sys.file_exists f) then None
  else
    (try
       match Yojson.Safe.from_string (read_file f) with
       | `Assoc kv ->
           (match List.assoc_opt "branch" kv, List.assoc_opt "generation" kv with
            | Some (`String b), Some g ->
                let gen =
                  match g with
                  | `String s -> (try Int64.of_string s with _ -> 0L)
                  | `Int i -> Int64.of_int i
                  | _ -> 0L
                in
                Some (b, gen)
            | _ -> None)
       | _ -> None
     with _ -> None)
