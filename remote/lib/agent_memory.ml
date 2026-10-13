(* remote/lib/agent_memory.ml — the [memory-put] / [memory-delete] agent
   verbs (borge/agent.borg run protocol; card 1ce0929f, spec sboot (e)).

   Stage 1 of the authority flip: an agent's memory writes go through the
   CLIENT into the one tree instead of a harness-private mirror. Both
   verbs are pure local-lens operations and work OFFLINE:

     - memory-put writes an UNSTAMPED datum envelope (hash "") at
       data.u.<owner>.agent-memory.<node-id> — the shape an online
       (create) produces minus the server hash. The next [pp-local sync]
       pushes it (borge/local-first.borg sync: local writes travel) and
       the server stamps the canonical hash on apply. Re-putting a mid
       that already exists UPDATES that node in place (uat bumped,
       ver+1, creation time preserved): last-writer-wins at the source,
       the same rule cross-device ingest applies on import, without
       duplicating tree nodes.

     - memory-delete removes the local node file (when present) AND
       enqueues a tombstone <type>.<dashed-node-id>.tombstone — the
       established deletion carrier, because the sync round's three-way
       diff pushes writes only and cannot see a locally-missing file
       (card 4aff9d93). The drain (today the */10 sbcl cron, later folded
       into the sync round) turns each tombstone into an idempotent wire
       delete; deleting an already-gone address returns (), so retries
       converge. [type] may be agent-memory or agent-agenda (the agenda
       mirror shares the tombstone queue).

   Identity: the TREE node gets a fresh uuid per NEW memory; the body
   carries [mid], the caller's stable memory id.

   Timestamps: cat/uat are CL universal time (unix + 2208988800) as
   strings, matching the corpus the harness mirror already writes, so
   uat ordering stays comparable across writers (LWW by uat). *)

let memory_types = [ "agent-memory"; "agent-agenda" ]

let is_memory_type (t : string) : bool = List.mem t memory_types

(* CL universal time = unix epoch + seconds from 1900-01-01. *)
let cl_universal_time () : string =
  string_of_int (int_of_float (Unix.gettimeofday ()) + 2_208_988_800)

let hex = "0123456789abcdef"

(** A random uuid4-shaped identifier (dashed). *)
let uuid4 () : string =
  let buf = Buffer.create 32 in
  for i = 0 to 31 do
    if i = 12 then Buffer.add_char buf '4'
    else if i = 16 then Buffer.add_char buf "89ab".[Random.int 4]
    else Buffer.add_char buf hex.[Random.int 16]
  done;
  let h = Buffer.contents buf in
  Printf.sprintf "%s-%s-%s-%s-%s" (String.sub h 0 8) (String.sub h 8 4)
    (String.sub h 12 4) (String.sub h 16 4) (String.sub h 20 12)

let is_hex32 (s : string) : bool =
  String.length s = 32
  && String.for_all (fun c -> (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f')) s

(** Accept a dashed or dashless node address; return the DASHED lowercase
    form the tombstone name (and the wire delete) expect. *)
let normalize_address (s : string) : string =
  let core =
    String.concat "" (String.split_on_char '-' (String.lowercase_ascii s))
  in
  if not (is_hex32 core) then invalid_arg ("not a node address: " ^ s);
  Printf.sprintf "%s-%s-%s-%s-%s" (String.sub core 0 8) (String.sub core 8 4)
    (String.sub core 12 4) (String.sub core 16 4) (String.sub core 20 12)

let data_path ~(lens : Lens_store.lens) ~(type_ : string) ~(node_id : string) :
    string =
  Printf.sprintf "data.u.%s.%s.%s" (Lens_store.enc_label lens.owner)
    (Lens_store.enc_label type_) node_id

let type_dir ~(lens : Lens_store.lens) (type_ : string) : string =
  (* dirname of a FULL node path: path_to_file treats the last segment as
     the leaf, so the dirname of data.u.<owner>.<type> alone is the OWNER
     dir, not the type dir (caught by the re-put test). *)
  Filename.dirname
    (Lens_store.path_to_file ~root:lens.root
       (data_path ~lens ~type_ ~node_id:"x"))

(** (mid, ver, cat) of a node-envelope JSON, when it is one of ours. *)
let body_fields_of_json (json : string) :
    (string * int * string option) option =
  match Yojson.Safe.from_string json with
  | `Assoc kv -> (
      match List.assoc_opt "body" kv with
      | Some (`Assoc b) ->
          let ver =
            match List.assoc_opt "ver" b with
            | Some (`String v) -> Option.value (int_of_string_opt v) ~default:1
            | _ -> 1
          in
          let cat =
            match List.assoc_opt "cat" b with
            | Some (`String c) -> Some c
            | _ -> None
          in
          (match List.assoc_opt "mid" b with
           | Some (`String m) -> Some (m, ver, cat)
           | _ -> None)
      | _ -> None)
  | _ -> None

(** The existing node carrying [mid] in its body, if any:
    (dashless node id, ver, cat). *)
let find_by_mid ~(lens : Lens_store.lens) (type_ : string) (mid : string) :
    (string * int * string option) option =
  let dir = type_dir ~lens type_ in
  match Sys.readdir dir with
  | exception _ -> None
  | entries ->
      let out = ref None in
      Array.iter
        (fun e ->
          if !out = None && Filename.check_suffix e ".json" then
            let node_id = Filename.remove_extension e in
            match Lens_store.get ~lens (data_path ~lens ~type_ ~node_id) with
            | Some json -> (
                match body_fields_of_json json with
                | Some (m, v, c) when m = mid -> out := Some (node_id, v, c)
                | _ -> ())
            | None -> ())
        entries;
      !out

type put_result = { tree_path : string; node_id : string; created : bool }

(** Write (or update) one agent-memory node. Unstamped: hash "". *)
let put ~(lens : Lens_store.lens) ~(mid : string) ~(title : string)
    ~(content : string) ~(tags : string list) ~(scope : string) ~(ws : string)
    ~(src : string) : put_result =
  let type_ = "agent-memory" in
  let ut = cl_universal_time () in
  let node_id, ver, created, cat =
    match find_by_mid ~lens type_ mid with
    | Some (id, v, c) -> (id, v + 1, false, Option.value c ~default:ut)
    | None -> (Lens_store.uuid_label (uuid4 ()), 1, true, ut)
  in
  let body =
    `Assoc
      [ ("title", `String title)
      ; ("content", `String content)
      ; ( "tags"
        , `String
            (Yojson.Safe.to_string (`List (List.map (fun t -> `String t) tags)))
        )
      ; ("scope", `String scope)
      ; ("mid", `String mid)
      ; ("cat", `String cat)
      ; ("uat", `String ut)
      ; ("ver", `String (string_of_int ver))
      ; ("src", `String src)
      ; ("ws", `String ws) ]
  in
  Lens_store.put_body ~lens
    ~path:(data_path ~lens ~type_ ~node_id)
    ~kind:"datum" ~type_ ~author:lens.owner ~body;
  { tree_path = data_path ~lens ~type_ ~node_id; node_id; created }

type delete_result = { removed_file : string option; tombstone : string }

(** The drain's tombstone queue: $PP_LENS_TOMBSTONES, else the harness
    default the */10 cron already consumes. *)
let default_tombstone_dir () : string =
  match Sys.getenv_opt "PP_LENS_TOMBSTONES" with
  | Some d when String.trim d <> "" -> d
  | _ ->
      Filename.concat (Sys.getenv "HOME")
        ".local/share/autolith/pp-lens-tombstones"

(** Delete one node: remove the local lens file when present, always
    enqueue the tombstone (the only deletion carrier — sync never
    propagates deletions). *)
let delete ~(lens : Lens_store.lens) ~(type_ : string) ~(address : string)
    ?(tombstone_dir = "") () : delete_result =
  if not (is_memory_type type_) then invalid_arg ("unknown memory type: " ^ type_);
  let dashed = normalize_address address in
  let dashless = String.concat "" (String.split_on_char '-' dashed) in
  let file =
    Lens_store.path_to_file ~root:lens.root
      (data_path ~lens ~type_ ~node_id:dashless)
  in
  let removed =
    if Sys.file_exists file then (Sys.remove file; Some file) else None
  in
  let dir =
    if tombstone_dir = "" then default_tombstone_dir () else tombstone_dir
  in
  let tomb =
    Filename.concat dir (Printf.sprintf "%s.%s.tombstone" type_ dashed)
  in
  Lens_store.atomic_write tomb
    (Printf.sprintf "deleted %s at unix %.0f\n" dashed (Unix.gettimeofday ()));
  { removed_file = removed; tombstone = tomb }

(** Body content for --memory-put: a file path, or "-" for stdin. *)
let read_content (file : string) : string =
  if file = "-" then
    let buf = Buffer.create 4096 in
    (try
       while true do
         Buffer.add_channel buf stdin 4096
       done
     with End_of_file -> ());
    Buffer.contents buf
  else
    let ic = open_in_bin file in
    let n = in_channel_length ic in
    let s = really_input_string ic n in
    close_in ic;
    s
type node_record = {
  node_id : string;
  tree_path : string;
  type_ : string;
  mid : string;
  title : string;
  content : string;
  tags : string list;   (* decoded from the stored JSON-encoded array *)
  scope : string;
  cat : string;         (* creation CL universal time *)
  uat : string;         (* last-update CL universal time *)
  ver : int;
}

let core_of_key (s : string) : string =
  String.concat "" (String.split_on_char '-' (String.lowercase_ascii s))

let body_assoc_of_json (json : string) :
    (string * Yojson.Safe.t) list option =
  let trimmed = String.trim json in
  if trimmed = "" then None
  else
    (try
       (match Yojson.Safe.from_string trimmed with
        | `Assoc kv -> (
            match List.assoc_opt "body" kv with
            | Some (`Assoc b) -> Some b
            | _ -> None)
        | _ -> None)
     with _ -> None)

let body_str (b : (string * Yojson.Safe.t) list) (k : string) : string =
  match List.assoc_opt k b with Some (`String s) -> s | _ -> ""

let tags_of_body (b : (string * Yojson.Safe.t) list) : string list =
  (try
     (match String.trim (body_str b "tags") with
      | "" -> []
      | raw -> (
          match Yojson.Safe.from_string raw with
          | `List xs ->
              List.filter_map (function `String s -> Some s | _ -> None) xs
          | _ -> []))
   with _ -> [])

let rec_of_body ~(lens : Lens_store.lens) ~(type_ : string) ~(node_id : string)
    (b : (string * Yojson.Safe.t) list) : node_record =
  { node_id;
    tree_path = data_path ~lens ~type_ ~node_id;
    type_;
    mid = body_str b "mid";
    title = body_str b "title";
    content = body_str b "content";
    tags = tags_of_body b;
    scope = body_str b "scope";
    cat = body_str b "cat";
    uat = body_str b "uat";
    ver =
      (match int_of_string_opt (body_str b "ver") with Some v -> v | None -> 1) }

let read_node ~(lens : Lens_store.lens) ~(type_ : string) ~(node_id : string) :
    node_record option =
  match Lens_store.get ~lens (data_path ~lens ~type_ ~node_id) with
  | None -> None
  | Some json -> (
      match body_assoc_of_json json with
      | Some b -> Some (rec_of_body ~lens ~type_ ~node_id b)
      | None -> None)

(** Read one memory node by NODE ADDRESS (dashed/dashless hex32) or by MID.
    Stage-2 read verb (borge/agent.borg): reads resolve from the tree; the
    caller's own log is the cache. Pure lens operation, works offline. *)
let get ~(lens : Lens_store.lens) ~(type_ : string) (key : string) :
    node_record option =
    let core = core_of_key key in
    if is_hex32 core then read_node ~lens ~type_ ~node_id:core
    else
      (* MID search keeps the ORIGINAL key: the stable memory id carries
         dashes and case, so core_of_key must not feed find_by_mid. *)
      (match find_by_mid ~lens type_ key with
       | Some (node_id, _, _) -> read_node ~lens ~type_ ~node_id
       | None -> None)

(** All memory nodes in the lens for the owner across the memory types,
    newest-first (uat desc, creation cat as tie-break). *)
let list ~(lens : Lens_store.lens) : node_record list =
  let rows =
    List.flatten
      (List.map
         (fun type_ ->
           let dir = type_dir ~lens type_ in
           match Sys.readdir dir with
           | exception _ -> []
           | entries ->
               List.filter_map
                 (fun e ->
                   if Filename.check_suffix e ".json" then
                     read_node ~lens ~type_
                       ~node_id:(Filename.remove_extension e)
                   else None)
                 (Array.to_list entries))
         memory_types)
  in
  let int_of_opt (s : string) : int =
    match int_of_string_opt s with Some v -> v | None -> 0
  in
  List.sort
    (fun a b ->
      let c = compare (int_of_opt b.uat) (int_of_opt a.uat) in
      if c <> 0 then c else compare (int_of_opt b.cat) (int_of_opt a.cat))
    rows

let node_to_json (r : node_record) : Yojson.Safe.t =
  `Assoc
    [ ("node_id", `String r.node_id);
      ("tree_path", `String r.tree_path);
      ("type", `String r.type_);
      ("mid", `String r.mid);
      ("title", `String r.title);
      ("content", `String r.content);
      ("tags", `List (List.map (fun t -> `String t) r.tags));
      ("scope", `String r.scope);
      ("cat", `String r.cat);
      ("uat", `String r.uat);
      ("ver", `Int r.ver) ]
