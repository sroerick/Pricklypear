(** Pricklypear Image — hypermedia harness kernel prims.

    A headless htmx client substrate: lambdasoup-based HTML tree prims +
    an in-process route-dispatch prim that calls routed handlers without
    an HTTP round-trip. Used by [libs/hyper] to simulate htmx swaps
    server-side for debug agents (the fixi-five contract reference impl).

    Spec: borge/hypermedia-harness.borg (kernel-prims + request-runner).

    Page/node values are opaque VStr handles into module-global tables
    (same handle-as-id pattern as net_streams). Low-level html/* prims
    mutate in place; the Lisp layer [libs/hyper] deep-copies (html/page-copy)
    before each deriving interaction, so pages are immutable at the
    hyper/* surface (free history + diff). *)
open Pricklypear_lisp.Ast
module S = Soup

type soup_node = S.soup S.node
type element_node = S.element S.node
type general_node = S.general S.node
type element_nodes = S.element S.nodes

(* ------------------------------------------------------------------ *
   Handle tables: VStr id <-> lambdasoup node. *)

type page = { soup : soup_node }
type node = { elt : element_node; pid : string }

let pages : (string, page) Hashtbl.t = Hashtbl.create 16
let nodes : (string, node) Hashtbl.t = Hashtbl.create 64

let counter = ref 0
let fresh prefix =
  incr counter;
  Printf.sprintf "%s-%d" prefix !counter

let store_page (s : soup_node) : string =
  let id = fresh "page" in
  Hashtbl.replace pages id { soup = s };
  id

let store_node (e : element_node) (pid : string) : string =
  let id = fresh "node" in
  Hashtbl.replace nodes id { elt = e; pid };
  id

let page_of (v : value) : soup_node option =
  match v with
  | VStr id -> (match Hashtbl.find_opt pages id with
                | Some p -> Some p.soup | None -> None)
  | _ -> None

let node_of (v : value) : element_node option =
  match v with
  | VStr id -> (match Hashtbl.find_opt nodes id with
                | Some n -> Some n.elt | None -> None)
  | _ -> None

let page_id_of (v : value) : string option =
  match v with VStr s -> Some s | _ -> None

(* ------------------------------------------------------------------ *
   Deep-copy a page (serialize + reparse) so a deriving operation
   never mutates its source — the hyper/* immutability invariant. *)
let copy_soup (s : soup_node) : soup_node =
  S.parse (S.to_string s)

(* Parse an HTML fragment into a list of top-level general nodes. *)
let fragment_nodes (html : string) : general_node list =
  S.to_list (S.children (S.parse html))

(* Apply one htmx swap strategy to [target], grafting [fragment]'s
   top-level nodes. Matches the 8 htmx 4 strategies. *)
let apply_swap ~target (style : string) (fragment : string) : unit =
  let new_nodes = fragment_nodes fragment in
  match String.lowercase_ascii style with
  | "innerhtml" ->
    S.clear target;
    List.iter (S.append_child target) new_nodes
  | "outerhtml" ->
    (* Insert new nodes before target, then unlink target itself. *)
    List.iter (fun n -> S.insert_before target n) new_nodes;
    S.delete target
  | "beforebegin" ->
    List.iter (fun n -> S.insert_before target n) new_nodes
  | "afterend" ->
    List.iter (fun n -> S.insert_after target n) new_nodes
  | "beforeend" ->
    List.iter (S.append_child target) new_nodes
  | "afterbegin" ->
    (* prepend_child reverses order; reverse first to preserve it. *)
    List.iter (S.prepend_child target) (List.rev new_nodes)
  | "delete" -> S.delete target
  | "none" -> ()
  | _ -> ()  (* unknown style: no-op *)

(* ------------------------------------------------------------------ *
   Handler value -> HTML string (mirrors http_core.value_to_html_string:
   raw-HTML-tagged VStrs unwrap, plain VStrs are text, others pp'd). *)
let render_handler_value (v : value) : string =
  match Primitives.is_raw_html v with
  | Some s -> s
  | None ->
    (match v with
     | VStr s -> s
     | _ -> Pricklypear_lisp.Reader.pp_value v)

(* Alist-of-pairs value -> (string,string) list. Accepts VList of
   Pair(VStr k, VStr v). Used for params / headers in route/dispatch. *)
let rec assoc_of (v : value) : (string * string) list =
  match v with
  | VList items -> List.filter_map pair_of items
  | VPair (head, tail) ->
    (match pair_of head with
     | Some kv -> kv :: assoc_of tail
     | None -> assoc_of tail)
  | _ -> []

and pair_of (p : value) : (string * string) option =
  match p with
  | VPair (VStr k, VStr x) -> Some (k, x)
  | _ -> None

(* ------------------------------------------------------------------ *
   Pure prims: html/parse, html/select, html/text, html/attr,
   html/serialize, html/swap!, html/create, html/replace, html/diff,
   html/page-copy. *)
let err s = VStr ("hyper: " ^ s)

let register_pure_prims (tbl : Primitives.t) : unit =
  Primitives.register tbl "soup/parse"
    (fun args ->
       match args with
       | [VStr s] -> VStr (store_page (S.parse s))
       | _ -> err "html/parse needs one string");

  Primitives.register tbl "soup/page-copy"
    (fun args ->
       match args with
       | [pid] ->
         (match page_of pid with
          | Some s -> VStr (store_page (copy_soup s))
          | None -> err "html/page-copy: not a page")
       | _ -> err "html/page-copy needs one page");

  Primitives.register tbl "soup/select"
    (fun args ->
       match args with
       | [page_v; VStr sel] ->
         (match page_of page_v with
          | None -> err "html/select: not a page"
          | Some s ->
            let matches = S.to_list (S.select sel s) in
            let pid = match page_id_of page_v with Some p -> p | None -> "?" in
            VList (List.map (fun e -> VStr (store_node e pid)) matches))
       | _ -> err "html/select needs page + selector");

  Primitives.register tbl "soup/select-one"
    (fun args ->
       match args with
       | [page_v; VStr sel] ->
         (match page_of page_v with
          | None -> err "html/select-one: not a page"
          | Some s ->
            match S.select_one sel s with
            | None -> VNil
            | Some e ->
              let pid = match page_id_of page_v with Some p -> p | None -> "?" in
              VStr (store_node e pid))
       | _ -> err "html/select-one needs page + selector");

  Primitives.register tbl "soup/serialize"
    (fun args ->
       match args with
       | [v] ->
         (match page_of v with
          | Some s -> VStr (S.to_string s)
          | None ->
            match node_of v with
            | Some e -> VStr (S.to_string e)
            | None -> err "html/serialize: not a page or node")
       | _ -> err "html/serialize needs one handle");

  Primitives.register tbl "soup/text"
    (fun args ->
       match args with
       | [v] ->
         (match page_of v with
          | Some s -> VStr (String.concat "" (S.texts s))
          | None ->
            match node_of v with
            | Some e -> VStr (String.concat "" (S.texts e))
            | None -> err "html/text: not a page or node")
       | _ -> err "html/text needs one handle");

  Primitives.register tbl "soup/attr"
    (fun args ->
       match args with
       | [node_v; VStr name] ->
         (match node_of node_v with
          | None -> err "html/attr: not a node"
          | Some e ->
            match S.attribute name e with
            | Some s -> VStr s
            | None -> VNil)
       | _ -> err "html/attr needs node + name");

  Primitives.register tbl "soup/has-attr"
    (fun args ->
       match args with
       | [node_v; VStr name] ->
         (match node_of node_v with
          | None -> err "html/has-attr: not a node"
          | Some e ->
            if S.has_attribute name e then VStr "true" else VNil)
       | _ -> err "html/has-attr needs node + name");

  Primitives.register tbl "soup/swap!"
    (fun args ->
       (* (html/swap! node style fragment) — mutate in place, return nil. *)
       match args with
       | [node_v; VStr style; VStr fragment] ->
         (match node_of node_v with
          | None -> err "html/swap!: not a node"
          | Some e -> apply_swap ~target:e style fragment; VNil)
       | _ -> err "html/swap! needs node + style + fragment");

  Primitives.register tbl "soup/create"
    (fun args ->
       match args with
       | [VStr tag] ->
         let e = S.create_element tag in
         let id = store_node e "orphan" in
         VStr id
       | _ -> err "html/create needs tag name");

  Primitives.register tbl "soup/replace"
    (fun args ->
       (* (html/replace node fragment) — replace target with fragment's
          top-level nodes, preserving order. *)
       match args with
       | [node_v; VStr fragment] ->
         (match node_of node_v with
          | None -> err "html/replace: not a node"
          | Some e ->
            let news = fragment_nodes fragment in
            List.iter (fun n -> S.insert_before e n) news;
            S.delete e;
            VNil)
       | _ -> err "html/replace needs node + fragment");

  Primitives.register tbl "soup/closest-attr"
    (fun args ->
       (* (soup/closest-attr node name) -> walk from node up through ancestors,
          return the value of `name` from the nearest element that HAS it.
          Mirrors htmx inherited-target resolution (compat implicit
          inheritance). Returns VNil if none found. *)
       match args with
       | [node_v; VStr name] ->
         (match node_of node_v with
          | None -> err "soup/closest-attr: not a node"
          | Some e ->
            (* check the element itself, then ancestors *)
            let rec find n =
              if Soup.has_attribute name n then Soup.attribute name n
              else
                match Soup.parent n with
                | None -> None
                | Some p -> find p
            in
            (match find e with
             | Some v -> VStr v
             | None -> VNil))
       | _ -> err "soup/closest-attr needs node + name");

  Primitives.register tbl "soup/parent"
    (fun args ->
       match args with
       | [node_v] ->
         (match node_of node_v with
          | None -> err "soup/parent: not a node"
          | Some e ->
            match Soup.parent e with
            | None -> VNil
            | Some p -> VStr (store_node p "?"))
       | _ -> err "soup/parent needs one node");

  Primitives.register tbl "soup/diff"
    (fun args ->
       (* v0: structural equality + coarse report. Walks both pages'
          element descendants by id attribute, reports first
          changed / added / removed. *)
       match args with
       | [a; b] ->
         (match page_of a, page_of b with
          | Some sa, Some sb ->
            if S.equal_modulo_whitespace sa sb then VStr "no changes"
            else
              let ea = S.to_list (S.select "*" sa) in
              let eb = S.to_list (S.select "*" sb) in
              let idset lst =
                List.filter_map
                  (fun e -> S.attribute "id" e) lst
              in
              let ia, ib = idset ea, idset eb in
              let added = List.filter (fun x -> not (List.mem x ia)) ib in
              let removed = List.filter (fun x -> not (List.mem x ib)) ia in
              let parts =
                (if added = [] then []
                 else [Printf.sprintf "added: %s" (String.concat "," added)])
                @ (if removed = [] then []
                   else [Printf.sprintf "removed: %s" (String.concat "," removed)])
              in
              VStr
                (if parts = [] then "changed (no id diff)"
                 else String.concat "; " parts)
          | _ -> err "html/diff needs two pages")
       | _ -> err "html/diff needs two pages")

(* Session-capturing prim route/dispatch is registered INLINE in
   session.ml (register_hyper_route_dispatch, right after this module's
   register_pure_prims call) so it can close over Session.t without a
   dependency cycle. This mirrors how register_crud_prims /
   register_net_prims live in session.ml.) *)
