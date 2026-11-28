(* Tree algebra — the pure, executable spec of the tree store
   (borge/trees.borg: model, contracts, core-ops, op-list, visibility,
   concurrency).

   The chapter asks for the node forest as a TOTAL algebra over an
   immutable map (process step B: "pure tree_algebra.ml: the model +
   core ops as pure functions over an immutable map, property-tested
   against a naive reference (the branch_algebra pattern)"). This module
   is that algebra; it holds no I/O, no SQL and no interpreter types, and
   image/test/tree_algebra_test.ml checks it against a hand-written naive
   reference plus the chapter's own contract statements.

   What lives here:
     - resolve_path: the ONE zone/scope resolution function
       (data.note.u.<user> | data.u.<user> | data.shared | data.s.<space> |
       data.l.<lens>.<user>) — the visibility chokepoint every op calls
       exactly once per operation.
     - vis: VIS(zone, caller) -> {read; write}.
     - the 15-op core: get/put/delete/exists/children/subtree/find/count/
       move/copy/ancestors/search-text/history/validate (+ resolve-path).
     - the contract shape: every op is total and returns
       Ok value | Error (Missing|Denied|Conflict|Too_large|Bad_path);
       listings carry {items; next; truncated; total_hint} and are shallow
       refs unless with_body is set; depth N = N levels below the path.
     - optimistic CAS: put ?expected rejects a mismatched current hash
       with Conflict <current-hash>.

   The ops are pure: mutating ops return (value, updated store). The rim
   (prims over the live tree_nodes table) and the SEXP binding are not
   here; see borge/trees.borg core-ops / ocaml-sexp. *)

module SMap = Map.Make (String)
module SSet = Set.Make (String)

(* ------------------------------------------------------------------ *)
(* Paths and zones                                                     *)
(* ------------------------------------------------------------------ *)

type zone =
  | Garden_note of string  (* data.note.u.<owner>            (raw label) *)
  | Garden of string       (* data.u.<owner>                 (decoded)   *)
  | Shared                 (* data.shared                               *)
  | Space of string        (* data.s.<space>                            *)
  | Lens of string         (* data.l.<lens>.<owner>          (decoded)   *)

type error =
  | Missing of string
  | Denied of string
  | Conflict of string
  | Too_large of string
  | Bad_path of string

type 'a result = ('a, error) Stdlib.result

let error_kind = function
  | Missing _ -> "missing"
  | Denied _ -> "denied"
  | Conflict _ -> "conflict"
  | Too_large _ -> "too-large"
  | Bad_path _ -> "bad-path"

let error_reason = function
  | Missing r | Denied r | Conflict r | Too_large r | Bad_path r -> r

let error_kinds =
  [ "missing"; "denied"; "conflict"; "too-large"; "bad-path" ]
type resolved = {
  zone : zone;
  owner : string option;
  space : string option;
  type_name : string option;
  id : string option;
}

let labels (p : string) : string list = String.split_on_char '.' p

let is_label (s : string) : bool =
  s <> ""
  && String.for_all
       (fun c ->
         (c >= 'a' && c <= 'z')
         || (c >= 'A' && c <= 'Z')
         || (c >= '0' && c <= '9')
         || c = '_')
       s

(* data.u / data.l owners are stored label-ENCODED (tree_store datum_path);
   the note garden stores its owner RAW (tree_store.note_garden_owner does
   the same split, so the two classifiers agree). *)
let decode_owner (label : string) : string =
  match Tree_store.dec_label label with Some s when s <> "" -> s | _ -> label

(** The chapter's resolution chokepoint. Total over the data grammar: a
    zone root and every address inside it resolve; anything outside the
    data grammar (b.<branch> code paths, lib/, pkg/, junk) is bad-path.
    Node-shaped addresses also report their type/id. *)
let resolve_path (p : string) : resolved result =
  let ls = labels p in
  if ls = [] || not (List.for_all is_label ls) then Error (Bad_path p)
  else
    match ls with
    | "data" :: "note" :: "u" :: o :: _ when o <> "" ->
        Ok { zone = Garden_note o; owner = Some o; space = None;
             type_name = None; id = None }
    | [ "data"; "u"; o; ty; id ] ->
        let owner = decode_owner o in
        Ok { zone = Garden owner; owner = Some owner; space = None;
             type_name = Some ty; id = Some id }
    | "data" :: "u" :: o :: _ when o <> "" ->
        let owner = decode_owner o in
        Ok { zone = Garden owner; owner = Some owner; space = None;
             type_name = None; id = None }
    | [ "data"; "shared"; ty; id ] ->
        Ok { zone = Shared; owner = None; space = None;
             type_name = Some ty; id = Some id }
    | "data" :: "shared" :: _ ->
        Ok { zone = Shared; owner = None; space = None;
             type_name = None; id = None }
    | [ "data"; "s"; sp; ty; id ] when sp <> "" ->
        Ok { zone = Space sp; owner = None; space = Some sp;
             type_name = Some ty; id = Some id }
    | "data" :: "s" :: sp :: _ when sp <> "" ->
        Ok { zone = Space sp; owner = None; space = Some sp;
             type_name = None; id = None }
    | [ "data"; "l"; l; o; ty; id ] when l <> "" ->
        let owner = decode_owner o in
        Ok { zone = Lens l; owner = Some owner; space = None;
             type_name = Some ty; id = Some id }
    | "data" :: "l" :: l :: o :: _ when l <> "" && o <> "" ->
        let owner = decode_owner o in
        Ok { zone = Lens l; owner = Some owner; space = None;
             type_name = None; id = None }
    | _ -> Error (Bad_path p)

(** Stable key per zone (labels are ltree-safe, so '/' never collides). *)
let zone_key = function
  | Garden_note o -> "note/" ^ o
  | Garden o -> "u/" ^ o
  | Shared -> "shared/"
  | Space s -> "s/" ^ s
  | Lens l -> "l/" ^ l

let zone_name = function
  | Garden_note _ -> "garden-note"
  | Garden _ -> "garden"
  | Shared -> "shared"
  | Space _ -> "space"
  | Lens _ -> "lens"

(* Number of labels in the zone ROOT itself (data.note.u.<owner> = 4, ...). *)
let zone_depth = function
  | Garden_note _ -> 4
  | Garden _ -> 3
  | Shared -> 2
  | Space _ -> 3
  | Lens _ -> 3

let parent_path (p : string) : string option =
  match List.rev (labels p) with
  | _ :: rev -> Some (String.concat "." (List.rev rev))
  | [] -> None

let leaf (p : string) : string =
  match List.rev (labels p) with x :: _ -> x | [] -> ""

(* ------------------------------------------------------------------ *)
(* Effects, nodes, versions, store                                    *)
(* ------------------------------------------------------------------ *)

type caller = {
  name : string;                  (* "" = anonymous *)
  admin : bool;                   (* kernel admin (pp.user unset) *)
  cross_zone : bool;              (* the cross-zone-read capability *)
  member : string -> bool;        (* space membership *)
}

let make_caller ?(admin = false) ?(cross_zone = false)
    ?(member = fun (_ : string) -> false) (name : string) : caller =
  { name; admin; cross_zone; member }

type access = { read : bool; write : bool }

let no_access = { read = false; write = false }
let full_access = { read = true; write = true }

(** VIS(zone, caller), evaluated once per operation by each op below.
    Garden zones are private to the owner (admins pass with attribution);
    data.shared is the read-mostly seed corpus every authenticated
    identity reads and writes (migration 014/guestbook precedent);
    data.s.<space> is membership-gated (branching.shared-data-spaces,
    which supersedes the chapter's older "data.s readable by
    authenticated" line — a space row's readers are its members). *)
let vis (zone : zone) ~(caller : caller) : access =
  let owner_only o =
    if caller.admin || (caller.name <> "" && caller.name = o) then
      full_access
    else no_access
  in
  match zone with
  | Garden_note o -> owner_only o
  | Garden o -> owner_only o
  | Lens o -> owner_only o
  | Shared ->
      if caller.admin || caller.name <> "" then full_access else no_access
  | Space sp ->
      if caller.admin || caller.member sp then full_access else no_access

type node = {
  path : string;
  kind : string;
  type_name : string option;
  body : string;
  author : string;
  created_at : float;
  updated_at : float;
  hash : string;
}

type ref_ = {
  r_path : string;
  r_kind : string;
  r_type : string option;
  r_hash : string;
  r_body : string option;   (* None = shallow ref (the default) *)
}

type version = {
  v_hash : string;
  v_body : string;
  v_author : string;
  v_updated_at : float;
}

type t = {
  nodes : node SMap.t;
  kids : SSet.t SMap.t;            (* parent path -> children *)
  classes : SSet.t SMap.t;         (* (zone,kind,type) -> paths *)
  hist : version list SMap.t;      (* path -> previous versions, newest first *)
}

let empty : t =
  { nodes = SMap.empty; kids = SMap.empty; classes = SMap.empty;
    hist = SMap.empty }

let length (t : t) : int = SMap.cardinal t.nodes

let all_nodes (t : t) : node list = SMap.bindings t.nodes |> List.map snd

let hash_body (body : string) : string = Digest.to_hex (Digest.string body)

(* Body cap: the chapter's error set has too-large but no number; 1 MiB is
   the store's own jsonb comfort zone for one node body. *)
let max_body_bytes = 1_048_576

let ref_of_node ?(with_body = false) (n : node) : ref_ =
  { r_path = n.path; r_kind = n.kind; r_type = n.type_name; r_hash = n.hash;
    r_body = (if with_body then Some n.body else None) }

type 'a listing = {
  items : 'a list;
  next : string option;
  truncated : bool;
  total_hint : int;      (* items remaining at/after the cursor *)
}

(* Pagination over a path-sorted list; the cursor is the last path emitted,
   so a walk that follows [next] yields every item exactly once. *)
let paginate ?(limit = 100) ?cursor (xs : 'a list) ~(key : 'a -> string) :
    'a listing =
  let limit = max 1 limit in
  let rest =
    match cursor with
    | None -> xs
    | Some c -> List.filter (fun x -> String.compare (key x) c > 0) xs
  in
  let rec split n acc = function
    | [] -> (List.rev acc, [])
    | x :: tl ->
        if n <= 0 then (List.rev acc, x :: tl)
        else split (n - 1) (x :: acc) tl
  in
  let page, more = split limit [] rest in
  let next =
    match (page, more) with
    | [], _ -> None
    | _, [] -> None
    | _, _ -> Some (key (List.nth page (List.length page - 1)))
  in
  { items = page; next; truncated = more <> [];
    total_hint = List.length rest }

(* Pagination over a path list, so callers materialize only the page. *)
let paginate_paths ?(limit = 100) ?cursor (paths : string list) :
    string list * string option * bool * int =
  let l = paginate ~limit ?cursor paths ~key:(fun p -> p) in
  (l.items, l.next, l.truncated, l.total_hint)

(* Pagination over the indexed path SETS (children/find): O(page + log n)
   element work, never O(class size) materialization. *)
let paginate_set ?(limit = 100) ?cursor (set : SSet.t) :
    string list * string option * bool * int =
  let limit = max 1 limit in
  let above =
    match cursor with
    | None -> set
    | Some c ->
        let _, _, hi = SSet.split c set in
        hi
  in
  let total = SSet.cardinal above in
  let rec take n acc s =
    if n <= 0 then acc
    else
      match SSet.min_elt_opt s with
      | None -> acc
      | Some x -> take (n - 1) (x :: acc) (SSet.remove x s)
  in
  let page : string list = List.rev (take limit [] above) in
  let next =
    if total > List.length page then Some (List.nth page (List.length page - 1))
    else None
  in
  (page, next, total > List.length page, total)

let class_key (zone : zone) (kind : string) (type_name : string option) :
    string =
  zone_key zone ^ "\000" ^ kind ^ "\000"
  ^ (match type_name with Some t -> t | None -> "")

let add_index (idx : SSet.t SMap.t) key v : SSet.t SMap.t =
  SMap.update key
    (function Some s -> Some (SSet.add v s) | None -> Some (SSet.singleton v))
    idx

let del_index (idx : SSet.t SMap.t) key v : SSet.t SMap.t =
  SMap.update key
    (function
      | Some s ->
          let s' = SSet.remove v s in
          if SSet.is_empty s' then None else Some s'
      | None -> None)
    idx

let zone_of_path_opt (p : string) : zone option =
  match resolve_path p with Ok r -> Some r.zone | Error _ -> None

let with_node (t : t) ~(zone : zone) (n : node) : t =
  let nodes = SMap.add n.path n t.nodes in
  let kids =
    match parent_path n.path with
    | Some pp -> add_index t.kids pp n.path
    | None -> t.kids
  in
  let classes = add_index t.classes (class_key zone n.kind n.type_name) n.path in
  { t with nodes; kids; classes }

let remove_node (t : t) (path : string) : t =
  let n = SMap.find_opt path t.nodes in
  let nodes = SMap.remove path t.nodes in
  let kids =
    match parent_path path with
    | Some pp -> del_index t.kids pp path
    | None -> t.kids
  in
  let classes =
    match n with
    | Some node -> (
        match zone_of_path_opt path with
        | Some z -> del_index t.classes (class_key z node.kind node.type_name) path
        | None -> t.classes)
    | None -> t.classes
  in
  { t with nodes; kids; classes }

(* ------------------------------------------------------------------ *)
(* Reading                                                             *)
(* ------------------------------------------------------------------ *)

(** node-get path -> node | missing | denied.  Denied is decided BEFORE the
    lookup so an unreadable path never reports whether it exists. *)
let get (t : t) ~(caller : caller) ~(path : string) : node result =
  match resolve_path path with
  | Error e -> Error e
  | Ok r ->
      if not (vis r.zone ~caller).read then Error (Denied path)
      else (
        match SMap.find_opt path t.nodes with
        | Some n -> Ok n
        | None -> Error (Missing path))

(** node-exists path -> bool | denied ; cheap, indexed. *)
let exists (t : t) ~(caller : caller) ~(path : string) : bool result =
  match resolve_path path with
  | Error e -> Error e
  | Ok r ->
      if not (vis r.zone ~caller).read then Error (Denied path)
      else Ok (SMap.mem path t.nodes)

let children_set (t : t) (path : string) : SSet.t =
  match SMap.find_opt path t.kids with Some s -> s | None -> SSet.empty

(** children path [cursor limit] -> depth 1 listing; shallow by default. *)
let children (t : t) ~(caller : caller) ~(path : string) ?(limit = 100)
    ?cursor ?(with_body = false) () : ref_ listing result =
  match resolve_path path with
  | Error e -> Error e
  | Ok r ->
      if not (vis r.zone ~caller).read then Error (Denied path)
      else if not (SMap.mem path t.nodes) then Error (Missing path)
      else
          let page, next, truncated, total =
            paginate_set ~limit ?cursor (children_set t path)
          in
          let refs =
            List.filter_map (fun p -> SMap.find_opt p t.nodes) page
            |> List.map (ref_of_node ~with_body)
          in
          Ok { items = refs; next; truncated; total_hint = total }

(** subtree path depth [cursor] -> the nodes 1..depth levels below path.
    Depth N is N levels below the path, uniformly (contracts). *)
let subtree (t : t) ~(caller : caller) ~(path : string) ~(depth : int)
    ?(limit = 100) ?cursor ?(with_body = false) () : ref_ listing result =
  match resolve_path path with
  | Error e -> Error e
  | Ok r ->
      if not (vis r.zone ~caller).read then Error (Denied path)
      else if not (SMap.mem path t.nodes) then Error (Missing path)
      else
        let rec walk lvl frontier acc =
          if lvl > depth || frontier = [] then List.rev acc
          else
            let next =
              List.concat_map (fun p -> SSet.elements (children_set t p)) frontier
            in
            walk (lvl + 1) next (List.rev_append next acc)
        in
          let paths = List.sort String.compare (walk 1 [ path ] []) in
          let page, next, truncated, total =
            paginate_paths ~limit ?cursor paths
          in
          let refs =
            List.filter_map (fun p -> SMap.find_opt p t.nodes) page
            |> List.map (ref_of_node ~with_body)
          in
          Ok { items = refs; next; truncated; total_hint = total }

(** ancestors path -> [node], root-first, excluding the path itself. *)
let ancestors (t : t) ~(caller : caller) ~(path : string) : node list result =
  match resolve_path path with
  | Error e -> Error e
  | Ok r ->
      if not (vis r.zone ~caller).read then Error (Denied path)
      else if not (SMap.mem path t.nodes) then Error (Missing path)
      else
          let rec up p acc =
            match parent_path p with
            | None -> acc
            | Some pp -> (
                match resolve_path pp with
                | Error _ -> acc
                | Ok rr ->
                    if zone_key rr.zone <> zone_key r.zone then acc
                    else
                      match SMap.find_opt pp t.nodes with
                      | Some n -> up pp (n :: acc)
                      | None -> acc)
          in
          Ok (up path [])

(* ------------------------------------------------------------------ *)
(* Writing                                                            *)
(* ------------------------------------------------------------------ *)

(** node-put path kind type body [cas-hash] -> hash + store.
    CAS is optimistic: a supplied expected hash that differs from the
    current one (or from "absent") is Conflict <current-hash>. *)
let put (t : t) ~(caller : caller) ~(now : float) ~(path : string)
    ~(kind : string) ?(type_name : string option) ~(body : string)
    ?(expected : string option) () : (string * t) result =
  match resolve_path path with
  | Error e -> Error e
  | Ok r ->
      let acc = vis r.zone ~caller in
      if not acc.write then Error (Denied path)
      else if String.length body > max_body_bytes then Error (Too_large path)
      else
        let prev = SMap.find_opt path t.nodes in
        let current = match prev with Some n -> n.hash | None -> "absent" in
        match expected with
        | Some want when want <> current -> Error (Conflict current)
        | _ ->
            let created_at = match prev with Some n -> n.created_at | None -> now in
            let node =
              { path; kind; type_name; body; author = caller.name; created_at;
                updated_at = now; hash = hash_body body }
            in
            let nodes = SMap.add path node t.nodes in
            let kids =
              match prev with
              | Some _ -> t.kids
              | None -> (
                  match parent_path path with
                  | Some pp -> add_index t.kids pp path
                  | None -> t.kids)
            in
            let classes =
              match prev with
              | Some old ->
                  let old_key = class_key r.zone old.kind old.type_name in
                  let new_key = class_key r.zone kind type_name in
                  if old_key = new_key then t.classes
                  else add_index (del_index t.classes old_key path) new_key path
              | None -> add_index t.classes (class_key r.zone kind type_name) path
            in
            let hist =
              match prev with
              | Some old ->
                  let v =
                    { v_hash = old.hash; v_body = old.body;
                      v_author = old.author; v_updated_at = old.updated_at }
                  in
                  SMap.update path
                    (function Some l -> Some (v :: l) | None -> Some [ v ])
                    t.hist
              | None -> t.hist
            in
            Ok (node.hash, { nodes; kids; classes; hist })

(** The subtree set of [path]: every stored descendant (prefix + '.'). *)
let subtree_set (t : t) (path : string) : SSet.t =
  let prefix = path ^ "." in
  let plen = String.length prefix in
  SMap.fold
    (fun p _ acc ->
      if String.length p > plen && String.sub p 0 plen = prefix then SSet.add p acc
      else acc)
    t.nodes SSet.empty

(** node-delete path [recursive] -> ok | missing.  Non-recursive deletes
    the addressed row only (tree_store.delete_path semantics: the SQL is
    `path = $1`); recursive removes the subtree. *)
let delete (t : t) ~(caller : caller) ~(path : string)
    ~(recursive : bool) : (unit * t) result =
  match resolve_path path with
  | Error e -> Error e
  | Ok r ->
      if not (vis r.zone ~caller).write then Error (Denied path)
      else if not (SMap.mem path t.nodes) then Error (Missing path)
      else
        let victims =
          if recursive then path :: SSet.elements (subtree_set t path)
          else [ path ]
        in
        Ok ((), List.fold_left remove_node t victims)

(** move path new-parent -> new-path ; re-parent, cycle-checked.
    A move stays inside its own compartment: crossing zones is a routing
    change (space admission), not a move. *)
let move (t : t) ~(caller : caller) ~(now : float) ~(path : string)
    ~(new_parent : string) : (string * t) result =
  match (resolve_path path, resolve_path new_parent) with
  | Error e, _ -> Error e
  | _, Error e -> Error e
  | Ok r, Ok rp ->
      let acc = vis r.zone ~caller in
      let accp = vis rp.zone ~caller in
      if not (acc.write && accp.write) then Error (Denied new_parent)
      else if not (SMap.mem path t.nodes) then Error (Missing path)
      else if zone_key r.zone <> zone_key rp.zone then
        Error (Bad_path "move across zones")
      else
        let subtree = subtree_set t path in
        let new_path = new_parent ^ "." ^ leaf path in
        if new_parent = path || SSet.mem new_parent subtree then
          Error (Bad_path "cycle")
        else if new_path = path then Ok (path, t)
        else if SMap.mem new_path t.nodes then Error (Conflict "exists")
        else
          let victims = path :: SSet.elements subtree in
          let moved =
            List.filter_map
              (fun p ->
                match SMap.find_opt p t.nodes with
                | Some n -> Some (p, n)
                | None -> None)
              victims
          in
          let renamed p =
            if p = path then new_path
            else
              new_path
              ^ String.sub p (String.length path)
                  (String.length p - String.length path)
          in
          let cleared = List.fold_left (fun t p -> remove_node t p) t victims in
          let t' =
            List.fold_left
              (fun t (p, n) ->
                with_node t ~zone:r.zone { n with path = renamed p; updated_at = now })
              cleared moved
          in
          Ok (new_path, t')

(** copy path new-parent -> new-path ; deep.  Copies carry the same
    bodies (hence hashes) as new rows; no history travels with a copy. *)
let copy (t : t) ~(caller : caller) ~(now : float) ~(path : string)
    ~(new_parent : string) : (string * t) result =
  match (resolve_path path, resolve_path new_parent) with
  | Error e, _ -> Error e
  | _, Error e -> Error e
  | Ok r, Ok rp ->
      let acc = vis r.zone ~caller in
      let accp = vis rp.zone ~caller in
      if not (acc.read && accp.write) then Error (Denied new_parent)
      else if not (SMap.mem path t.nodes) then Error (Missing path)
      else if zone_key r.zone <> zone_key rp.zone then
        Error (Bad_path "copy across zones")
      else
        let new_path = new_parent ^ "." ^ leaf path in
        if new_path = path then Error (Conflict "exists")
        else if SMap.mem new_path t.nodes then Error (Conflict "exists")
        else
          let victims = path :: SSet.elements (subtree_set t path) in
          let sources =
            List.filter_map
              (fun p ->
                match SMap.find_opt p t.nodes with
                | Some n -> Some (p, n)
                | None -> None)
              victims
          in
          let renamed p =
            if p = path then new_path
            else
              new_path
              ^ String.sub p (String.length path)
                  (String.length p - String.length path)
          in
          let t' =
            List.fold_left
              (fun t (p, n) ->
                with_node t ~zone:r.zone
                  { n with path = renamed p; created_at = now; updated_at = now })
              t sources
          in
          Ok (new_path, t')

(* ------------------------------------------------------------------ *)
(* Queries                                                            *)
(* ------------------------------------------------------------------ *)

type zone_filter = Any | Only of zone

let key_parts (k : string) : (string * string * string) option =
  match String.split_on_char '\000' k with
  | [ zk; kd; ty ] -> Some (zk, kd, ty)
  | _ -> None

let class_selected (zf : zone_filter) ~(kind : string option)
    ~(type_name : string option) (k : string) : bool =
  match key_parts k with
  | None -> false
  | Some (zk, kd, ty) ->
      let zone_ok = match zf with Any -> true | Only z -> zk = zone_key z in
      let kind_ok = match kind with None -> true | Some k -> kd = k in
      let ty_ok = match type_name with None -> true | Some t -> ty = t in
      zone_ok && kind_ok && ty_ok

let candidate_set (t : t) ~(zone : zone_filter) ?kind ?type_name
    ?(prefix = "") () : SSet.t =
  let set =
    SMap.fold
      (fun k s acc ->
        if class_selected zone ~kind ~type_name k then SSet.union acc s else acc)
      t.classes SSet.empty
  in
  if prefix = "" then set
  else
    let plen = String.length prefix + 1 in
    SSet.filter
      (fun p ->
        p = prefix
        || (String.length p > plen && String.sub p (String.length prefix) 1 = "."
           && String.sub p 0 (String.length prefix) = prefix))
      set

let body_field_equals (body : string) ~(field : string) ~(value : string) :
    bool =
  match Yojson.Safe.from_string body with
  | exception _ -> false
  | `Assoc fields -> (
      match List.assoc_opt field fields with
      | Some (`String s) -> s = value
      | Some (`Int i) -> string_of_int i = value
      | Some (`Bool b) -> string_of_bool b = value
      | _ -> false)
  | _ -> false

let where_all (body : string) (where : (string * string) list) : bool =
  List.for_all (fun (f, v) -> body_field_equals body ~field:f ~value:v) where

let readable (caller : caller) (p : string) : bool =
  match resolve_path p with
  | Error _ -> false
  | Ok r -> (vis r.zone ~caller).read

let zone_gate (t : t) ~(caller : caller) (zone : zone_filter) : unit result =
  ignore t;
  match zone with
  | Only z -> if (vis z ~caller).read then Ok () else Error (Denied (zone_name z))
  | Any ->
      if caller.cross_zone then Ok ()
      else Error (Denied "cross-zone-read")

(** find {zone kind type where cursor} -> paginated refs.  The index is
    consulted per (zone,kind,type) class; the forest is never scanned. *)
let find (t : t) ~(caller : caller) ~(zone : zone_filter) ?kind ?type_name
    ?(prefix = "") ?(where = []) ?(limit = 100) ?cursor
    ?(with_body = false) () : ref_ listing result =
  match zone_gate t ~caller zone with
  | Error e -> Error e
  | Ok () ->
      let set = candidate_set t ~zone ?kind ?type_name ~prefix () in
      let filtered =
        where <> [] || match zone with Any -> true | Only _ -> false
      in
      let page, next, truncated, total =
        if not filtered then paginate_set ~limit ?cursor set
        else
          let paths =
            List.filter
              (fun p ->
                (match zone with Any -> readable caller p | Only _ -> true)
                && match SMap.find_opt p t.nodes with
                   | Some n -> where_all n.body where
                   | None -> false)
              (SSet.elements set)
          in
          paginate_paths ~limit ?cursor paths
      in
      let refs =
        List.filter_map (fun p -> SMap.find_opt p t.nodes) page
        |> List.map (ref_of_node ~with_body)
      in
      Ok { items = refs; next; truncated; total_hint = total }

(** count {zone kind type} -> n ; indexed count, no scan. *)
let count (t : t) ~(caller : caller) ~(zone : zone_filter) ?kind ?type_name
    ?(prefix = "") () : int result =
  match zone_gate t ~caller zone with
  | Error e -> Error e
  | Ok () ->
      let set = candidate_set t ~zone ?kind ?type_name ~prefix () in
      let n =
        match zone with
        | Only _ -> SSet.cardinal set
        | Any -> SSet.fold (fun p n -> if readable caller p then n + 1 else n) set 0
      in
      Ok n

(** search-text scope query [cursor] -> hits ; visibility-enforced (a
    cross-zone search needs the cross-zone-read capability). *)
let search_text (t : t) ~(caller : caller) ~(zone : zone_filter)
    ~(query : string) ?(limit = 100) ?cursor ?(with_body = false) () :
    ref_ listing result =
  match zone_gate t ~caller zone with
  | Error e -> Error e
  | Ok () ->
      let needle = String.lowercase_ascii query in
      let paths =
        SMap.fold
          (fun p n acc ->
            let in_zone =
              match zone with
              | Any -> true
              | Only z -> (
                  match resolve_path p with
                  | Ok r -> zone_key r.zone = zone_key z
                  | Error _ -> false)
            in
            if
              in_zone
              && readable caller p
              && (needle = ""
                 || String.length needle > 0
                    && (let body = String.lowercase_ascii n.body in
                        let blen = String.length body and nlen = String.length needle in
                        let rec scan i =
                          i + nlen <= blen
                          && (String.sub body i nlen = needle || scan (i + 1))
                        in
                        nlen <= blen && scan 0))
            then p :: acc
            else acc)
          t.nodes []
      in
        let paths = List.sort String.compare paths in
        let page, next, truncated, total =
          paginate_paths ~limit ?cursor paths
        in
        let refs =
          List.filter_map (fun p -> SMap.find_opt p t.nodes) page
          |> List.map (ref_of_node ~with_body)
        in
        Ok { items = refs; next; truncated; total_hint = total }

(** history path [cursor] -> versions, newest first (data-history). *)
let history (t : t) ~(caller : caller) ~(path : string) ?(limit = 100)
    ?cursor () : version listing result =
  match resolve_path path with
  | Error e -> Error e
  | Ok r ->
      if not (vis r.zone ~caller).read then Error (Denied path)
      else if not (SMap.mem path t.nodes) then Error (Missing path)
      else
        let vs = match SMap.find_opt path t.hist with Some l -> l | None -> [] in
        let off =
          match cursor with
          | None -> Ok 0
          | Some c -> (
              match int_of_string_opt c with
              | Some i when i >= 0 -> Ok i
              | _ -> Error (Bad_path ("cursor:" ^ c)))
        in
        (match off with
        | Error e -> Error e
        | Ok off ->
            let rec drop n = function
              | [] -> []
              | _ :: tl as l -> if n <= 0 then l else drop (n - 1) tl
            in
            let rest = drop off vs in
            let rec split n acc = function
              | [] -> (List.rev acc, [])
              | x :: tl ->
                  if n <= 0 then (List.rev acc, x :: tl)
                  else split (n - 1) (x :: acc) tl
            in
            let page, more = split (max 1 limit) [] rest in
            let next =
              if more = [] then None
              else Some (string_of_int (off + List.length page))
            in
            Ok
              { items = page; next; truncated = more <> [];
                total_hint = List.length rest })

(* ------------------------------------------------------------------ *)
(* Integrity                                                          *)
(* ------------------------------------------------------------------ *)

type diagnostic = { d_path : string; d_problem : string }

(** validate -> diagnostics: bad paths, hash/body mismatches, orphans and
    index drift. Empty list = clean. *)
let validate (t : t) : diagnostic list =
  let check p n acc =
    match resolve_path p with
    | Error _ -> { d_path = p; d_problem = "bad-path" } :: acc
    | Ok r ->
        let acc =
          if hash_body n.body <> n.hash then
            { d_path = p; d_problem = "hash-mismatch" } :: acc
          else acc
        in
        let acc =
          match SMap.find_opt (class_key r.zone n.kind n.type_name) t.classes with
          | Some s when SSet.mem p s -> acc
          | _ -> { d_path = p; d_problem = "index" } :: acc
        in
        let acc =
          match parent_path p with
          | Some pp when List.length (labels pp) >= zone_depth r.zone ->
              if SMap.mem pp t.nodes then acc
              else { d_path = p; d_problem = "orphan" } :: acc
          | _ -> acc
        in
        acc
  in
  SMap.fold check t.nodes [] |> List.rev
