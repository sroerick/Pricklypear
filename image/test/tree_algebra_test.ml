(* Tree algebra tests — borge/trees.borg model / contracts / core-ops /
   op-list / visibility / concurrency / large-tree.

   Each test is a statement the chapter makes:
     1. model: every data path resolves to exactly ONE zone under the
        grammar the store writes (data.note.u.<owner>, data.u.<owner>,
        data.shared, data.s.<space>, data.l.<lens>).
     2. contracts: every op is total, the five error kinds are distinct,
        listing cursors round-trip, depth N is N levels below the path.
     3. core-ops / op-list: the 15-op core exists with the stated
        semantics; listings are shallow refs unless with_body.
     4. visibility: garden zones deny non-owners (admins pass), data.shared
        reads for authenticated identities, cross-zone find/search need
        the capability.
     5. concurrency: CAS node-put (expected hash) -> Conflict current.
     6. large-tree: deterministic 100k fixture; children/find/count are
        indexed (p95 < 50ms); move/copy correct under iteration.
   Plus generative properties against a naive full-forest reference
   (the branch_algebra pattern). *)

module A = Pricklypear_image_lib.Tree_algebra
module TS = Pricklypear_image_lib.Tree_store
module G = QCheck.Gen

let enc = TS.enc_label
let alice = A.make_caller "alice"
let bob = A.make_caller "bob"
let admin = A.make_caller ~admin:true "alice"
let anon = A.make_caller ""
let bob_member_of sp = A.make_caller ~member:(fun s -> s = sp) "bob"
let now0 = 1.0e9
let p1 = "data.u." ^ enc "alice" ^ ".todo.i1"

let put_ok t ~caller ~path ~kind ~(type_name : string option) ~body ~now =
  match A.put t ~caller ~now ~path ~kind ?type_name:type_name ~body () with
  | Ok (_, t') -> t'
  | Error e ->
      failwith
        (Printf.sprintf "put %s: %s (%s)" path (A.error_kind e)
           (A.error_reason e))

let body p = "{\"seq\":\"" ^ p ^ "\"}"

(* Build a forest through the write path: every path as a row (note paths as
   folders), admin caller, deterministic timestamps. *)
let build paths =
  List.fold_left
    (fun t p ->
      let kind, ty =
        match A.resolve_path p with
        | Ok r -> (
            match r.A.zone with
            | A.Garden_note _ -> ("folder", None)
            | _ -> ("datum", Some "todo"))
        | Error _ -> ("datum", Some "todo")
      in
      put_ok t ~caller:admin ~path:p ~kind ~type_name:ty ~body:(body p) ~now:now0)
    A.empty paths

(* ---- naive reference: full-forest scans ---- *)

let node_paths (t : A.t) = List.map (fun n -> n.A.path) (A.all_nodes t)

let rec labels_prefix xs ys =
  match (xs, ys) with
  | [], _ -> true
  | x :: xt, y :: yt -> String.equal x y && labels_prefix xt yt
  | _ -> false

let is_under parent p =
  let lp = A.labels parent and lq = A.labels p in
  List.length lp < List.length lq && labels_prefix lp lq

let naive_children t p =
  List.filter (fun n -> A.parent_path n.A.path = Some p) (A.all_nodes t)

let naive_subtree t p depth =
  List.filter
    (fun n ->
      let l = List.length (A.labels n.A.path) in
      let pl = List.length (A.labels p) in
      l > pl && l <= pl + depth && is_under p n.A.path)
    (A.all_nodes t)

let naive_find t ~zone ~kind ~type_name ~prefix =
  List.filter
    (fun n ->
      let zone_ok =
        match (zone, A.resolve_path n.A.path) with
        | A.Any, _ -> true
        | _, Error _ -> false
        | A.Only z, Ok r -> A.zone_key z = A.zone_key r.A.zone
      in
      let kind_ok = match kind with None -> true | Some k -> n.A.kind = k in
      let ty_ok =
        match type_name with None -> true | Some ty -> n.A.type_name = Some ty
      in
      let prefix_ok = prefix = "" || is_under prefix n.A.path in
      zone_ok && kind_ok && ty_ok && prefix_ok)
    (A.all_nodes t)

(* ---- generators ---- *)

let gen_owner = G.oneof [ G.return "alice"; G.return "bob" ]
let gen_type = G.oneof [ G.return "todo"; G.return "note" ]
let gen_id = G.map (fun i -> "i" ^ string_of_int i) (G.int_range 0 4)
let gen_space = G.oneof [ G.return "board"; G.return "lab" ]

let gen_path =
  G.oneof
    [ G.map3
        (fun o t i -> "data.u." ^ enc o ^ "." ^ t ^ "." ^ i)
        gen_owner gen_type gen_id;
      G.map2 (fun t i -> "data.shared." ^ t ^ "." ^ i) gen_type gen_id;
      G.map3
        (fun s t i -> "data.s." ^ s ^ "." ^ t ^ "." ^ i)
        gen_space gen_type gen_id;
      G.map (fun o -> "data.note.u." ^ o) gen_owner;
      G.map2 (fun o i -> "data.note.u." ^ o ^ ".f" ^ i) gen_owner gen_id;
      G.map2
        (fun o i -> "data.note.u." ^ o ^ ".f" ^ i ^ ".n" ^ i)
        gen_owner gen_id ]

let gen_caller =
  G.oneof
    [ G.return alice; G.return bob; G.return admin; G.return anon;
      G.return (bob_member_of "board") ]

let unit_test name f =
  QCheck.Test.make ~count:1 ~name (QCheck.make G.(return ())) f

(* ---- 1. model ---- *)

let t_resolve_grammar =
  unit_test "model: resolve_path classifies the zone grammar" (fun () ->
      let ok p want ty =
        match A.resolve_path p with
        | Ok r ->
            A.zone_key r.A.zone = A.zone_key want
            && (match ty with None -> true | Some t -> r.A.type_name = Some t)
        | Error _ -> false
      in
      ok "data.note.u.alice" (A.Garden_note "alice") None
      && ok "data.note.u.alice.f1.n1" (A.Garden_note "alice") None
      && ok ("data.u." ^ enc "alice" ^ ".todo.i1")
           (A.Garden "alice") (Some "todo")
      && ok "data.shared.todo.i1" A.Shared (Some "todo")
      && ok "data.s.board.todo.i1" (A.Space "board") (Some "todo")
      && ok ("data.l.l1." ^ enc "alice" ^ ".todo.i1")
           (A.Lens "l1") (Some "todo")
      && (match A.resolve_path ("data.u." ^ enc "alice" ^ ".todo.i1") with
         | Ok r -> r.A.owner = Some "alice" && r.A.id = Some "i1"
         | Error _ -> false)
      && (match A.resolve_path "b.main.fn.foo" with
         | Error e -> A.error_kind e = "bad-path"
         | Ok _ -> false)
      && (match A.resolve_path "data.u" with Error _ -> true | Ok _ -> false)
      && (match A.resolve_path "data..x.y.z" with Error _ -> true | Ok _ -> false)
      && (match A.resolve_path "" with Error _ -> true | Ok _ -> false))

let t_no_two_zones =
  QCheck.Test.make ~count:200
    ~name:"model: no path matches two zones (one zone root per path)"
    (QCheck.make gen_path) (fun p ->
      match A.resolve_path p with
      | Error _ -> false
      | Ok r ->
          let roots =
            [ "data.note.u.alice"; "data.note.u.bob";
              "data.u." ^ enc "alice"; "data.u." ^ enc "bob";
              "data.shared"; "data.s.board"; "data.s.lab"; "data.l.l1" ]
          in
          let matches =
            List.filter
              (fun root ->
                (root = p || is_under root p)
                && (match A.resolve_path root with
                   | Ok rr -> A.zone_key rr.A.zone = A.zone_key r.A.zone
                   | Error _ -> false))
              roots
          in
          List.length matches = 1)

let t_constructors =
  unit_test
    "model: every data path the store constructs parses; code paths do not"
    (fun () ->
      let uuid = "11111111-2222-3333-4444-555555555555" in
      let parses p = match A.resolve_path p with Ok _ -> true | Error _ -> false in
      parses (TS.datum_path ~user:"alice" ~type_name:"todo" ~id:uuid ())
      && parses
           (TS.datum_path ~user:"alice" ~lens:(Some "l1") ~type_name:"todo"
              ~id:uuid ())
      && parses
           (TS.datum_path ~space:(Some "board") ~type_name:"todo" ~id:uuid ())
      && parses
           (TS.datum_path ~user:"alice" ~type_name:"guestbook_entry" ~id:uuid ())
      && parses (TS.garden_root ~user:"alice")
      && not (parses (TS.fn_path ~branch_id:"b1" ~name:"foo"))
      && not (parses (TS.type_path ~branch_id:"b1" ~name:"todo"))
      && not (parses (TS.route_path ~branch_id:"b1" ~route:"/x"))
      && not (parses (TS.pkg_path ~branch_id:"b1" ~name:"pkg")))

(* ---- 2. contracts ---- *)

let t_error_kinds =
  unit_test "contracts: five distinct error kinds, each produced by a real op"
    (fun () ->
      let s =
        build [ p1; "data.u." ^ enc "alice" ^ ".todo.i2"; "data.note.u.alice" ]
      in
      let kind f = match f () with Error e -> Some (A.error_kind e) | Ok _ -> None in
      let missing =
        kind (fun () ->
            A.get s ~caller:alice ~path:("data.u." ^ enc "alice" ^ ".todo.i9"))
      in
      let denied = kind (fun () -> A.get s ~caller:bob ~path:"data.note.u.alice") in
      let conflict =
        kind (fun () ->
            A.put s ~caller:alice ~now:now0 ~path:p1 ~kind:"datum"
              ~type_name:"todo" ~body:"x" ~expected:"nope" ())
      in
      let too_large =
        kind (fun () ->
            A.put s ~caller:alice ~now:now0
              ~path:("data.u." ^ enc "alice" ^ ".todo.i3") ~kind:"datum"
              ~type_name:"todo"
              ~body:(String.make (A.max_body_bytes + 1) 'x') ())
      in
      let bad_path =
        kind (fun () -> A.get s ~caller:alice ~path:"b.main.fn.foo")
      in
      missing = Some "missing"
      && denied = Some "denied"
      && conflict = Some "conflict"
      && too_large = Some "too-large"
      && bad_path = Some "bad-path"
      && List.sort compare A.error_kinds
           = List.sort compare
               [ "missing"; "denied"; "conflict"; "too-large"; "bad-path" ])

let t_denied_before_missing =
  unit_test
    "contracts: an unreadable path reports denied, never missing (no existence leak)"
    (fun () ->
      let s = build [ "data.note.u.alice.n1" ] in
      match
        ( A.get s ~caller:bob ~path:"data.note.u.alice.n1",
          A.get s ~caller:bob ~path:"data.note.u.alice.n2" )
      with
      | Error e1, Error e2 ->
          A.error_kind e1 = "denied" && A.error_kind e2 = "denied"
      | _ -> false)

let t_cursor_children =
  unit_test "contracts: child cursor round-trips (every page, no gap, no repeat)"
    (fun () ->
      let root = "data.note.u.alice" in
      let paths = List.init 23 (fun i -> root ^ ".k" ^ string_of_int i) in
      let s = build (root :: paths) in
      let rec walk cursor acc pages =
        match A.children s ~caller:alice ~path:root ~limit:5 ?cursor () with
        | Error _ -> None
        | Ok l ->
            let acc =
              List.rev_append (List.map (fun r -> r.A.r_path) l.A.items) acc
            in
            if pages > 50 then None
            else (
              match l.A.next with
              | None -> Some (List.rev acc, pages + 1, l)
              | Some c -> walk (Some c) acc (pages + 1))
      in
      match walk None [] 0 with
      | None -> false
      | Some (got, pages, last) ->
          List.sort compare got = List.sort compare paths
          && pages = 5
          && (not last.A.truncated)
          && last.A.total_hint = 3)

let t_cursor_find_history =
  unit_test "contracts: find and history cursors round-trip" (fun () ->
      let z = A.Garden "alice" in
      let paths =
        List.init 12 (fun i ->
            "data.u." ^ enc "alice" ^ ".todo.i" ^ string_of_int i)
      in
      let s =
        List.fold_left
          (fun s i ->
            put_ok s ~caller:alice ~now:(now0 +. float_of_int i)
              ~path:("data.u." ^ enc "alice" ^ ".todo.i" ^ string_of_int i)
              ~kind:"datum" ~type_name:(Some "todo")
              ~body:("{\"n\":\"" ^ string_of_int i ^ "\"}"))
          A.empty (List.init 12 (fun i -> i))
      in
      let rec walk cursor acc =
        match
          A.find s ~caller:alice ~zone:(A.Only z) ~kind:"datum"
            ~type_name:"todo" ~limit:5 ?cursor ()
        with
        | Error _ -> None
        | Ok l ->
            let acc =
              List.rev_append (List.map (fun r -> r.A.r_path) l.A.items) acc
            in
            (match l.A.next with
            | None -> Some (List.rev acc)
            | Some c -> walk (Some c) acc)
      in
      let p = "data.u." ^ enc "alice" ^ ".todo.i0" in
      let s =
        List.fold_left
          (fun s k ->
            put_ok s ~caller:alice ~now:(now0 +. float_of_int k) ~path:p
              ~kind:"datum" ~type_name:(Some "todo")
              ~body:("{\"v\":\"" ^ string_of_int k ^ "\"}"))
          s [ 1; 2; 3 ]
      in
      let rec hwalk cursor acc =
        match A.history s ~caller:alice ~path:p ~limit:2 ?cursor () with
        | Error _ -> None
        | Ok l ->
            let acc =
              List.rev_append (List.map (fun v -> v.A.v_body) l.A.items) acc
            in
            (match l.A.next with
            | None -> Some (List.rev acc)
            | Some c -> hwalk (Some c) acc)
      in
      (match walk None [] with
      | None -> false
      | Some got -> List.sort compare got = List.sort compare paths)
      && (match hwalk None [] with
         | None -> false
         | Some bodies ->
             List.length bodies = 3
             && List.length (List.sort_uniq compare bodies) = 3))

let t_depth_uniform =
  unit_test "contracts: depth N is N levels below the path, in every listing op"
    (fun () ->
      let root = "data.note.u.alice" in
      let paths =
        [ root; root ^ ".a"; root ^ ".b"; root ^ ".a.a1"; root ^ ".a.a2";
          root ^ ".a.a1.deep" ]
      in
      let s = build paths in
      let depth_n n =
        match A.subtree s ~caller:alice ~path:root ~depth:n ~limit:100 () with
        | Error _ -> []
        | Ok l -> List.map (fun r -> r.A.r_path) l.A.items
      in
      List.sort compare (depth_n 1)
        = List.sort compare [ root ^ ".a"; root ^ ".b" ]
      && List.sort compare (depth_n 2)
           = List.sort compare
               [ root ^ ".a"; root ^ ".b"; root ^ ".a.a1"; root ^ ".a.a2" ]
      && List.length (depth_n 3) = 5
      && List.sort compare (depth_n 99)
           = List.sort compare (List.filter (fun p -> p <> root) paths))

let t_shallow_default =
  unit_test "core-ops: listings are shallow refs unless with_body is set"
    (fun () ->
      let root = "data.note.u.alice" in
      let child = root ^ ".a" in
      let s = build [ root; child ] in
      let find_ref with_body =
        match
          A.children s ~caller:alice ~path:root ~with_body:(with_body = true) ()
        with
        | Error _ -> None
        | Ok l -> List.find_opt (fun r -> r.A.r_path = child) l.A.items
      in
      (match find_ref false with
      | Some r ->
          r.A.r_body = None && r.A.r_hash = A.hash_body (body child)
          && r.A.r_kind = "folder"
      | None -> false)
      && (match find_ref true with
         | Some r -> r.A.r_body = Some (body child)
         | None -> false))

(* ---- 5. concurrency ---- *)

let t_cas =
  unit_test
    "concurrency: CAS put accepts the current hash and reports the live one on mismatch"
    (fun () ->
      let s = build [ p1 ] in
      let h =
        match A.get s ~caller:alice ~path:p1 with
        | Ok n -> n.A.hash
        | Error _ -> ""
      in
      let wrong =
        match
          A.put s ~caller:alice ~now:now0 ~path:p1 ~kind:"datum"
            ~type_name:"todo" ~body:"second" ~expected:"deadbeef" ()
        with
        | Error (A.Conflict cur) -> cur = h
        | _ -> false
      in
      let right =
        match
          A.put s ~caller:alice ~now:now0 ~path:p1 ~kind:"datum"
            ~type_name:"todo" ~body:"second" ~expected:h ()
        with
        | Ok (h2, _) -> h2 = A.hash_body "second" && h2 <> h
        | Error _ -> false
      in
      let absent =
        match
          A.put s ~caller:alice ~now:now0
            ~path:("data.u." ^ enc "alice" ^ ".todo.i2") ~kind:"datum"
            ~type_name:"todo" ~body:"new" ~expected:"nothing" ()
        with
        | Error (A.Conflict cur) -> cur = "absent"
        | _ -> false
      in
      wrong && right && absent)

(* ---- 4. visibility ---- *)

let t_vis_matrix =
  unit_test
    "visibility: garden private to owner (admin passes), shared reads, space needs membership"
    (fun () ->
      let g = A.Garden "alice" and gn = A.Garden_note "alice" in
      let sp = A.Space "board" in
      let own c z = (A.vis z ~caller:c).A.read && (A.vis z ~caller:c).A.write in
      own alice g && own alice gn
      && (not (A.vis g ~caller:bob).A.read)
      && (not (A.vis gn ~caller:bob).A.write)
      && (not (A.vis g ~caller:anon).A.read)
      && own admin g
      && (A.vis A.Shared ~caller:bob).A.read
      && (not (A.vis A.Shared ~caller:anon).A.read)
      && own (bob_member_of "board") sp
      && (not (A.vis sp ~caller:alice).A.read)
      && own admin sp
      && not (A.vis (A.Lens "l1") ~caller:bob).A.read)

let t_cross_zone_find =
  unit_test "visibility: cross-zone find/search deny without the capability"
    (fun () ->
      let s =
        build
          [ "data.u." ^ enc "alice" ^ ".todo.i1";
            "data.u." ^ enc "bob" ^ ".todo.i1";
            "data.shared.todo.i1" ]
      in
      let denied f =
        match f () with Error e -> A.error_kind e = "denied" | Ok _ -> false
      in
      denied (fun () -> A.find s ~caller:alice ~zone:A.Any ())
      && denied (fun () -> A.count s ~caller:alice ~zone:A.Any ())
      && denied (fun () ->
             A.search_text s ~caller:alice ~zone:A.Any ~query:"i1" ())
      && (match
            A.count s ~caller:(A.make_caller ~cross_zone:true "alice")
              ~zone:A.Any ()
          with
         | Ok n -> n = 2
         | Error _ -> false)
      && denied (fun () ->
             A.find s ~caller:alice ~zone:(A.Only (A.Garden "bob")) ()))

let t_search_visibility =
  unit_test "visibility: search never surfaces another identity's row"
    (fun () ->
      let s =
        put_ok A.empty ~caller:alice ~now:now0 ~path:p1 ~kind:"datum"
          ~type_name:(Some "todo") ~body:"{\"secret\":\"xyzzy\"}"
      in
      let hits caller =
        match
          A.search_text s ~caller ~zone:(A.Only (A.Garden "alice"))
            ~query:"xyzzy" ()
        with
        | Ok l -> List.length l.A.items
        | Error _ -> -1
      in
      hits alice = 1
      && hits bob = -1
      && (match
            A.search_text s ~caller:bob ~zone:(A.Only A.Shared) ~query:"xyzzy" ()
          with
         | Ok l -> l.A.items = []
         | Error _ -> false))

(* ---- 3. op-list ---- *)

let t_move_copy_ancestors =
  unit_test
    "op-list: move is cycle-checked and renames the subtree; copy is deep; ancestors root-first"
    (fun () ->
      let root = "data.note.u.alice" in
      let s = build [ root; root ^ ".a"; root ^ ".a.inner"; root ^ ".b" ] in
      let cycle =
        match
          A.move s ~caller:alice ~now:now0 ~path:(root ^ ".a")
            ~new_parent:(root ^ ".a.inner")
        with
        | Error e -> A.error_kind e = "bad-path"
        | Ok _ -> false
      in
      let cross =
        match
          A.move s ~caller:alice ~now:now0 ~path:(root ^ ".a")
            ~new_parent:"data.shared"
        with
        | Error e -> A.error_kind e = "bad-path"
        | Ok _ -> false
      in
      let moved =
        match
          A.move s ~caller:alice ~now:now0 ~path:(root ^ ".a")
            ~new_parent:(root ^ ".b")
        with
        | Ok (p, s') ->
            p = root ^ ".b.a"
            && List.mem (root ^ ".b.a.inner") (node_paths s')
            && not (List.mem (root ^ ".a") (node_paths s'))
        | Error _ -> false
      in
      let copied =
        match
          A.copy s ~caller:alice ~now:now0 ~path:(root ^ ".a")
            ~new_parent:(root ^ ".b")
        with
        | Ok (p, s') ->
            p = root ^ ".b.a"
            && List.mem (root ^ ".a") (node_paths s')
            && List.mem (root ^ ".b.a.inner") (node_paths s')
        | Error _ -> false
      in
      let anc =
        match A.ancestors s ~caller:alice ~path:(root ^ ".a.inner") with
        | Ok l -> List.map (fun n -> n.A.path) l = [ root; root ^ ".a" ]
        | Error _ -> false
      in
      cycle && cross && moved && copied && anc)

let t_find_where =
  unit_test
    "op-list: find honours kind/type/prefix/where without crossing zones"
    (fun () ->
      let z = A.Garden "alice" in
      let base = "data.u." ^ enc "alice" in
      let s =
        put_ok A.empty ~caller:alice ~now:now0 ~path:(base ^ ".todo.i1")
          ~kind:"datum" ~type_name:(Some "todo") ~body:"{\"status\":\"open\"}"
      in
      let s =
        put_ok s ~caller:alice ~now:now0 ~path:(base ^ ".todo.i2")
          ~kind:"datum" ~type_name:(Some "todo") ~body:"{\"status\":\"done\"}"
      in
      let s =
        put_ok s ~caller:alice ~now:now0 ~path:(base ^ ".note.i3")
          ~kind:"datum" ~type_name:(Some "note") ~body:"{\"status\":\"open\"}"
      in
      let got ?where ?type_name () =
        match
          A.find s ~caller:alice ~zone:(A.Only z) ~kind:"datum" ?type_name
            ?where ~limit:100 ()
        with
        | Error _ -> []
        | Ok l -> List.map (fun r -> r.A.r_path) l.A.items
      in
      List.length (got ()) = 3
      && List.length (got ~type_name:"todo" ()) = 2
      && List.length (got ~where:[ ("status", "open") ] ()) = 2
      && (match A.count s ~caller:alice ~zone:(A.Only z) ~kind:"datum" ~type_name:"todo" () with
         | Ok n -> n = 2
         | Error _ -> false))

let t_delete_and_validate =
  unit_test
    "op-list: delete is row-scoped unless recursive; validate reports drift"
    (fun () ->
      let root = "data.note.u.alice" in
      let s = build [ root; root ^ ".a"; root ^ ".a.b" ] in
      let clean = A.validate s = [] in
      let row_only =
        match A.delete s ~caller:alice ~path:(root ^ ".a") ~recursive:false with
        | Ok ((), s') ->
            List.mem (root ^ ".a.b") (node_paths s')
            && not (List.mem (root ^ ".a") (node_paths s'))
            && List.exists (fun d -> d.A.d_problem = "orphan") (A.validate s')
        | Error _ -> false
      in
      let deep =
        match A.delete s ~caller:alice ~path:root ~recursive:true with
        | Ok ((), s') -> node_paths s' = []
        | Error _ -> false
      in
      let missing =
        match A.delete s ~caller:alice ~path:(root ^ ".zz") ~recursive:false with
        | Error e -> A.error_kind e = "missing"
        | Ok _ -> false
      in
      let denied =
        match A.delete s ~caller:bob ~path:(root ^ ".a") ~recursive:false with
        | Error e -> A.error_kind e = "denied"
        | Ok _ -> false
      in
      let tampered =
        match A.get s ~caller:alice ~path:(root ^ ".a") with
        | Error _ -> false
        | Ok n ->
            let nodes =
              A.SMap.add n.A.path { n with A.hash = "bogus" } s.A.nodes
            in
            let s' = { s with A.nodes } in
            List.exists (fun d -> d.A.d_problem = "hash-mismatch") (A.validate s')
      in
      clean && row_only && deep && missing && denied && tampered)

(* ---- properties against the naive reference ---- *)

let prop_children_matches_naive =
  QCheck.Test.make ~count:40
    ~name:"children matches a naive parent filter over random forests"
    (QCheck.make QCheck.Gen.(list_size (int_range 0 14) gen_path))
    (fun paths ->
      let t = build paths in
      let probes = node_paths t in
      List.for_all
        (fun p ->
          match A.children t ~caller:admin ~path:p ~limit:1000 () with
          | Error _ -> false
          | Ok l ->
              let got = List.map (fun r -> r.A.r_path) l.A.items in
              let want = List.map (fun n -> n.A.path) (naive_children t p) in
              List.sort compare got = List.sort compare want)
        probes)

let prop_subtree_matches_naive =
  QCheck.Test.make ~count:40
    ~name:"subtree matches a naive depth filter"
    (QCheck.make QCheck.Gen.(pair gen_path (int_range 1 4)))
    (fun (p, depth) ->
      let t = build [ p; p ^ ".a"; p ^ ".a.b"; p ^ ".a.b.c" ] in
      match A.subtree t ~caller:admin ~path:p ~depth ~limit:1000 () with
      | Error _ -> false
      | Ok l ->
          let got = List.map (fun r -> r.A.r_path) l.A.items in
          let want = List.map (fun n -> n.A.path) (naive_subtree t p depth) in
          List.sort compare got = List.sort compare want)

let prop_find_count =
  QCheck.Test.make ~count:40
    ~name:"find/count match a naive scan (zone, kind, type, prefix)"
    (QCheck.make QCheck.Gen.(triple gen_path gen_type (option gen_space)))
    (fun (p, ty, sp) ->
      let t = build [ p; p ^ ".a"; "data.shared." ^ ty ^ ".i0" ] in
      let zone =
        match sp with
        | Some s -> A.Only (A.Space s)
        | None -> (
            match A.resolve_path p with
            | Ok r -> A.Only r.A.zone
            | Error _ -> A.Only A.Shared)
      in
      let want =
        List.map (fun n -> n.A.path) (naive_find t ~zone ~kind:None ~type_name:None ~prefix:"")
      in
      let got =
        match A.find t ~caller:admin ~zone ~limit:1000 () with
        | Error _ -> []
        | Ok l -> List.map (fun r -> r.A.r_path) l.A.items
      in
      let cnt =
        match A.count t ~caller:admin ~zone () with Ok n -> n | Error _ -> -1
      in
      List.sort compare got = List.sort compare want && cnt = List.length want)

let prop_move_matches_naive =
  QCheck.Test.make ~count:30
    ~name:"move rewrites exactly the subtree paths (naive mapping)"
    (QCheck.make gen_path)
    (fun p ->
      let root = "data.note.u.alice" in
      let dst = root ^ ".dst" in
      let t = build [ root; dst; p; p ^ ".x"; p ^ ".x.y" ] in
      if not (List.mem p (node_paths t)) || p = root || p = dst then true
      else
        match A.move t ~caller:admin ~now:now0 ~path:p ~new_parent:dst with
        | Error _ -> true
        | Ok (np, t') ->
            let want =
              List.map
                (fun path ->
                  if path = p then np
                  else if is_under p path then
                    np
                    ^ String.sub path (String.length p)
                        (String.length path - String.length p)
                  else path)
                (node_paths t)
            in
            List.sort compare (node_paths t') = List.sort compare want)

let prop_total =
  QCheck.Test.make ~count:100
    ~name:"every op is total: random callers and paths never raise"
    (QCheck.make QCheck.Gen.(pair gen_path gen_caller))
    (fun (p, caller) ->
      let t = build [ p; p ^ ".a" ] in
      let safe f = try ignore (f ()); true with _ -> false in
      safe (fun () -> A.get t ~caller ~path:p)
      && safe (fun () -> A.exists t ~caller ~path:(p ^ ".zz"))
      && safe (fun () -> A.children t ~caller ~path:p ~limit:3 ())
      && safe (fun () -> A.subtree t ~caller ~path:p ~depth:3 ())
      && safe (fun () -> A.ancestors t ~caller ~path:(p ^ ".a"))
      && safe (fun () ->
             A.put t ~caller ~now:now0 ~path:(p ^ ".n") ~kind:"k" ~body:"b" ())
      && safe (fun () -> A.delete t ~caller ~path:(p ^ ".n") ~recursive:true)
      && safe (fun () -> A.find t ~caller ~zone:A.Any ())
      && safe (fun () -> A.count t ~caller ~zone:(A.Only A.Shared) ())
      && safe (fun () ->
             A.search_text t ~caller ~zone:(A.Only A.Shared) ~query:"x" ())
      && safe (fun () -> A.history t ~caller ~path:p ())
      && safe (fun () -> A.move t ~caller ~now:now0 ~path:(p ^ ".a") ~new_parent:p)
      && safe (fun () -> A.copy t ~caller ~now:now0 ~path:(p ^ ".a") ~new_parent:p)
      && safe (fun () -> A.validate t))

(* ---- 6. large-tree ---- *)

let percentile (xs : float list) (p : float) : float =
  let xs = List.sort compare xs in
  let n = List.length xs in
  if n = 0 then 0.0
  else
    let i = int_of_float (Float.round (p *. float_of_int (n - 1))) in
    List.nth xs i

let time_once f =
  let t0 = Sys.time () in
  f ();
  Sys.time () -. t0

let p95_of samples f =
  let rec loop k acc =
    if k = 0 then acc else loop (k - 1) (time_once f :: acc)
  in
  percentile (loop samples []) 0.95

let t_large_tree =
  unit_test
    "large-tree: 100k deterministic fixture; children/find/count p95 < 50ms; move/copy correct under iteration"
    (fun () ->
      let n = 100_000 in
      let troot = "data.u." ^ enc "alice" ^ ".todo" in
      let s = ref (put_ok A.empty ~caller:alice ~now:now0 ~path:troot
                     ~kind:"folder" ~type_name:None ~body:"r")
      in
      for i = 0 to n - 1 do
        s :=
          put_ok !s ~caller:alice ~now:now0
            ~path:(troot ^ ".n" ^ string_of_int i)
            ~kind:"datum" ~type_name:(Some "todo")
            ~body:("{\"i\":\"" ^ string_of_int (i mod 97) ^ "\"}")
      done;
      let s = !s in
      let zone = A.Only (A.Garden "alice") in
      let ok_children =
        match A.children s ~caller:alice ~path:troot ~limit:100 () with
        | Ok l -> l.A.total_hint = n && List.length l.A.items = 100 && l.A.truncated
        | Error _ -> false
      in
      let ok_count =
        match A.count s ~caller:alice ~zone ~kind:"datum" ~type_name:"todo" () with
        | Ok c -> c = n
        | Error _ -> false
      in
      let p95_children =
        p95_of 60 (fun () ->
            match A.children s ~caller:alice ~path:troot ~limit:100 () with
            | Ok _ -> ()
            | Error _ -> failwith "children")
      in
      let p95_find =
        p95_of 60 (fun () ->
            match
              A.find s ~caller:alice ~zone ~kind:"datum" ~type_name:"todo"
                ~limit:100 ()
            with
            | Ok _ -> ()
            | Error _ -> failwith "find")
      in
      let p95_count =
        p95_of 60 (fun () ->
            match A.count s ~caller:alice ~zone ~kind:"datum" ~type_name:"todo" () with
            | Ok _ -> ()
            | Error _ -> failwith "count")
      in
      (* move/copy stay correct under iteration: a move renames the subtree,
         so the walk tracks the moved path rather than the original. *)
      let p0 = troot ^ ".n0" and p1 = troot ^ ".n1" and dst = troot ^ ".n2" in
      let s =
        put_ok s ~caller:alice ~now:now0 ~path:(p0 ^ ".c") ~kind:"datum"
          ~type_name:(Some "todo") ~body:"c"
      in
      let rec iterate k s cur ok =
        if k = 0 then (s, cur, ok)
        else
          let target = if k mod 2 = 0 then p1 else dst in
          match A.move s ~caller:alice ~now:now0 ~path:cur ~new_parent:target with
          | Error _ -> (s, cur, false)
          | Ok (np, s') ->
              iterate (k - 1) s' np
                (ok && List.mem np (node_paths s')
                && List.mem (np ^ ".c") (node_paths s'))
      in
      let s, cur, iter_ok = iterate 8 s p0 true in
      let copy_ok =
        match A.copy s ~caller:alice ~now:now0 ~path:cur ~new_parent:p1 with
        | Ok (np, s') ->
            List.mem np (node_paths s') && List.mem (np ^ ".c") (node_paths s')
            && List.length (node_paths s') = List.length (node_paths s) + 2
        | Error _ -> false
      in
      if p95_children >= 0.05 || p95_find >= 0.05 || p95_count >= 0.05 then
        failwith
          (Printf.sprintf "p95 over budget: children=%.4fs find=%.4fs count=%.4fs"
             p95_children p95_find p95_count);
      ok_children && ok_count && iter_ok && copy_ok)

let () =
  QCheck_base_runner.run_tests_main
    [ t_resolve_grammar;
      t_no_two_zones;
      t_constructors;
      t_error_kinds;
      t_denied_before_missing;
      t_cursor_children;
      t_cursor_find_history;
      t_depth_uniform;
      t_shallow_default;
      t_cas;
      t_vis_matrix;
      t_cross_zone_find;
      t_search_visibility;
      t_move_copy_ancestors;
      t_find_where;
      t_delete_and_validate;
      prop_children_matches_naive;
      prop_subtree_matches_naive;
      prop_find_count;
      prop_move_matches_naive;
      prop_total;
      t_large_tree ]
