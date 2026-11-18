(** Subtask cards (parent / children / rollup) — card 10695f13, parent
    c257735e.

    A card may be filed UNDER another card: libs/todo/lib.pp carries the
    relation as a parent field on the child row (todo/item/set-parent) and
    the loop reads it back through todo/item/parent + todo/item/children.
    This module is the pure OCaml leg of that same contract, so the loop's
    rules are testable without Postgres:

      1. [insert] files a child under a parent and the relation round-trips
         BOTH ways — parent.children holds the child, child.parent is
         [Some parent];
      2. [progress] / [rollup] report landed children over total children,
         so a parent is never invisible work;
      3. [blocked] stays true while ANY child is unlanded;
      4. [loop_rows] is the work queue — LEAF cards only. A parent is
         filtered out there and reported by [rollup_lines] instead of
         vanishing, so the loop never implements a parent directly. *)

type status = Landed | Unlanded

type node = {
  id : string;
  parent : string option;  (** who this card is filed under, if anyone *)
  status : status;
  children : string list;  (** direct children, sibling (insert) order *)
}

type t = node list

let find (t : t) (id : string) : node option =
  List.find_opt (fun (n : node) -> n.id = id) t

let children (t : t) (id : string) : node list =
  match find t id with
  | None -> []
  | Some n -> List.filter_map (fun c -> find t c) n.children

let is_leaf (t : t) (id : string) : bool = children t id = []

let set_status (t : t) ~(id : string) (status : status) : t =
  List.map (fun (n : node) -> if n.id = id then { n with status } else n) t

(** Mark one card landed (the /land transition). *)
let mark_landed (t : t) ~(id : string) : t = set_status t ~id Landed

(** File a card. Nobody under it ([parent] = None) keeps it top-level;
    filing an existing card again MOVES it (the old parent's children list
    drops it), which is what todo/item/set-parent's empty parent does. *)
let insert (t : t) ~(id : string) ?(parent : string option)
    ?(status = Unlanded) () : t =
  let detached =
    List.map
      (fun (n : node) ->
        { n with children = List.filter (fun c -> c <> id) n.children })
      t
  in
  let base = List.filter (fun (n : node) -> n.id <> id) detached in
  let base =
    match parent with
    | None -> base
    | Some pid ->
        List.map
          (fun (n : node) ->
            if n.id = pid then { n with children = n.children @ [ id ] } else n)
          base
  in
  base @ [ { id; parent; status; children = [] } ]

let done_children (t : t) (id : string) : int =
  List.length
    (List.filter (fun (n : node) -> n.status = Landed) (children t id))

let progress (t : t) (id : string) : string =
  let kids = children t id in
  if kids = [] then ""
  else Printf.sprintf "%d/%d" (done_children t id) (List.length kids)

(** The rollup a parent reports instead of being implemented:
    "<landed>/<total> done" (empty for a leaf). *)
let rollup (t : t) (id : string) : string =
  match progress t id with "" -> "" | p -> p ^ " done"

(** The loop's parent gate: true while ANY child is unlanded. *)
let blocked (t : t) (id : string) : bool =
  List.exists (fun (n : node) -> n.status = Unlanded) (children t id)

(** The loop's work queue: LEAF cards only. *)
let loop_rows (t : t) : string list =
  List.filter_map
    (fun (n : node) -> if is_leaf t n.id then Some n.id else None)
    t

(** One line per PARENT card: its rollup plus the blocked-by-children gate
    while any child is unlanded. Leaves drop out. *)
let rollup_lines (t : t) : string list =
  List.filter_map
    (fun (n : node) ->
      if is_leaf t n.id then None
      else
        let gate = if blocked t n.id then " blocked-by-children" else "" in
        Some (Printf.sprintf "rollup %s: %s%s" n.id (rollup t n.id) gate))
    t
