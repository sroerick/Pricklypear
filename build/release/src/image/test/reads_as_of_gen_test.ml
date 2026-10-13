(** tree-time reads-as-of-gen prim surface tests (tree-time.reads-as-of-gen).

    Exercises the USER-FACING gen coordinate built on the versioned read
    foundation (versioned_head/versioned_history + the `version` row type,
    landed in the 5.1 close-out):

      - tree/get <path> <gen>  : optional :gen N resolves the node at that
        gen (versioned), omitted = head (live tree_nodes).
      - gen-read <path> <gen>  : node body at gen as version JSON; supports
        absolute b.*/data.* paths AND relative logical suffixes crossed
        through the branch lens chain (closest-wins).
      - gen-diff <path> ga gb  : node-granular diff — hashes first, walk on
        mismatch; returns "same" when both leaves hash-identical.
      - undo <path> [n]        : restore the value at head-n as a NEW write at
        head+1 (restore-by-write) for datum paths; the restore is itself a
        versioned append, so undo is lossless + redoable.

    Needs Postgres (scripts/dev.sh). Skips cleanly when unreachable.
*)

module S = Pricklypear_image_lib.Store
module T = Pricklypear_image_lib.Tree_store
module C = Pricklypear_image_lib.Crud
open Pricklypear_lisp.Ast

let main_id = "00000000-0000-0000-0000-000000000001"

let with_store f =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let ctx : Pricklypear_image_lib.Io_eio.Ctx.t =
    Pricklypear_image_lib.Io_eio.Ctx.of_stdenv ~sw env
  in
  Pricklypear_image_lib.Io_eio.with_ctx ctx (fun () ->
      let cfg = S.config_from_env () in
      let store = S.make ~cfg ~branch_id:main_id () in
      S.connect store;
      match S.conn_of store with
      | None ->
          Printf.printf
            "SKIP: no postgres (set PP_DB_* / run scripts/dev.sh)\n%!"
      | Some _ ->
          S.with_branch store ~branch_id:main_id (fun () -> f store))

let uniq p = Printf.sprintf "rg_%s_%d" p (Random.int 1_000_000)

let declare_type (store : S.t) ~name ~fields =
  S.persist_type store ~name ~fields ~author:"tester"

let path_id (path : string) : string =
  match List.rev (String.split_on_char '.' path) with
  | l :: _ -> l
  | [] -> path

(** The last write's gen for a datum path. *)
let head_gen (store : S.t) (path : string) : int64 =
  match S.conn_of store with
  | Some conn ->
      (match T.versioned_head conn ~path () with
       | Some v -> (match v.T.v_gen with Some g -> g | None -> 0L)
       | None -> 0L)
  | None -> 0L

(** Create a datum row and return its tree path. *)
let make_datum (store : S.t) ~type_name ~title : string =
  match S.conn_of store with
  | Some conn ->
      (match
         T.upsert_datum conn ~path:"data.u.tester.new" ~type_name
           ~body_json:
             (Printf.sprintf "{\"title\":\"%s\",\"body\":\"b\"}" title)
           ~author:"tester" ~rank:None
       with
       | Ok p -> p
       | Error e -> Alcotest.fail ("create failed: " ^ e))
  | None -> Alcotest.fail "no conn"

(** Extract a string field from gen-read JSON (the version_to_json surface
    has {"body":{...}} and {"gen":...}). *)
let version_json_title (store : S.t) (path : string) (gen : int64)
    : string option =
  match C.tree_gen_read store ~path ~gen with
  | VStr s ->
      (try
         (match Yojson.Safe.from_string s with
          | `Assoc kv ->
              (match List.assoc_opt "body" kv with
               | Some (`Assoc bkv) ->
                   (match List.assoc_opt "title" bkv with
                    | Some (`String t) -> Some t
                    | _ -> None)
               | _ -> None)
          | _ -> None)
       with _ -> None)
  | _ -> None

(** gen-read on a RELATIVE logical suffix resolves through the branch lens
    chain when the store's current branch differs from main and main has the
    datum-like fn node; that path is exercised separately (fn code-path
    reads) — here we just assert the absolute-path surface. *)

(* ---- tree/get with optional :gen -> versioned read; omitted = head ---- *)

let body_title (kv : (string * Yojson.Safe.t) list) : string option =
  match List.assoc_opt "body" kv with
  | Some (`Assoc bkv) ->
      (match List.assoc_opt "title" bkv with
       | Some (`String t) -> Some t
       | _ -> None)
  | _ -> None

let test_tree_get_at_gen () =
  with_store (fun store ->
      let type_name = uniq "t" in
      declare_type store ~name:type_name ~fields:[ "title"; "body" ];
      let path = make_datum store ~type_name ~title:"a" in
      let g0 = head_gen store path in
      (* head, no :gen -> live body title "a" *)
      (match C.tree_get store ~path () with
       | VStr s ->
           (match Yojson.Safe.from_string s with
            | `Assoc kv ->
                Alcotest.(check (option string)) "head title a"
                  (Some "a") (body_title kv)
            | _ -> Alcotest.fail "tree/get head not object")
       | _ -> Alcotest.fail "tree/get head missing");
      (* update -> title "a2" *)
      (match S.conn_of store with
       | Some conn ->
           ignore
             (T.upsert_datum conn ~path ~type_name
                ~body_json:"{\"title\":\"a2\",\"body\":\"b2\"}"
                ~author:"tester" ~rank:None)
       | None -> ());
      (* head now "a2" *)
      (match C.tree_get store ~path () with
       | VStr s ->
           (match Yojson.Safe.from_string s with
            | `Assoc kv ->
                Alcotest.(check (option string)) "head title becomes a2"
                  (Some "a2") (body_title kv)
            | _ -> Alcotest.fail "tree/get head2 not object")
       | _ -> Alcotest.fail "tree/get head2 missing");
      (* :gen g0 -> old body "a" *)
      (match C.tree_get store ~path ~at_gen:g0 () with
       | VStr s ->
           (match Yojson.Safe.from_string s with
            | `Assoc kv ->
                Alcotest.(check (option string)) ":gen g0 restores title a"
                  (Some "a") (body_title kv)
            | _ -> Alcotest.fail "tree/get :gen not object")
       | _ -> Alcotest.fail "tree/get :gen missing");
      (* cleanup *)
      (match S.conn_of store with
       | Some conn -> ignore (T.delete_datum conn ~type_name ~id:(path_id path))
       | None -> ()))

(* ---- gen-read absolute path at head and as-of ---- *)
let test_gen_read () =
  with_store (fun store ->
      let type_name = uniq "t" in
      declare_type store ~name:type_name ~fields:[ "title"; "body" ];
      let path = make_datum store ~type_name ~title:"a" in
      let g0 = head_gen store path in
      (match S.conn_of store with
       | Some conn ->
           ignore
             (T.upsert_datum conn ~path ~type_name
                ~body_json:"{\"title\":\"a2\",\"body\":\"b2\"}"
                ~author:"tester" ~rank:None)
       | None -> ());
      (* gen-read at g0 -> old body *)
      Alcotest.(check (option string)) "gen-read g0 -> a"
        (Some "a") (version_json_title store path g0);
      (* gen-read at head gen -> new body *)
      let g1 = head_gen store path in
      Alcotest.(check (option string)) "gen-read head -> a2"
        (Some "a2") (version_json_title store path g1);
      Alcotest.(check bool) "gens differ" true (Int64.compare g0 g1 <> 0);
      (* cleanup *)
      (match S.conn_of store with
       | Some conn -> ignore (T.delete_datum conn ~type_name ~id:(path_id path))
       | None -> ()))

(* ---- gen-diff node-granular diff between two gens ---- *)
let test_gen_diff () =
  with_store (fun store ->
      let type_name = uniq "t" in
      declare_type store ~name:type_name ~fields:[ "title"; "body" ];
      let path = make_datum store ~type_name ~title:"a" in
      let g0 = head_gen store path in
      (match S.conn_of store with
       | Some conn ->
           ignore
             (T.upsert_datum conn ~path ~type_name
                ~body_json:"{\"title\":\"a2\",\"body\":\"b2\"}"
                ~author:"tester" ~rank:None)
       | None -> ());
      let g1 = head_gen store path in
      (match C.tree_gen_diff store ~path ~gen_a:g0 ~gen_b:g1 with
       | VStr "same" -> Alcotest.fail "different bodies reported same"
       | VStr s ->
           (match Yojson.Safe.from_string s with
            | `Assoc kv ->
                Alcotest.(check bool) "diff.same false" false
                  (match List.assoc_opt "same" kv with
                   | Some (`Bool b) -> b | _ -> true);
                (* delta has title changed a->a2 *)
                (match List.assoc_opt "delta" kv with
                 | Some (`Assoc dkv) ->
                     Alcotest.(check bool) "delta has title key" true
                       (List.mem_assoc "title" dkv)
                 | _ -> Alcotest.fail "diff.delta missing")
            | _ -> Alcotest.fail "gen-diff not object")
       | _ -> Alcotest.fail "gen-diff missing");
      (* diff of a gen against itself -> hash-equal -> "same" *)
      (match C.tree_gen_diff store ~path ~gen_a:g1 ~gen_b:g1 with
       | VStr "same" -> ()
       | _ -> Alcotest.fail "gen-diff of same gen should be 'same'");
      (* cleanup *)
      (match S.conn_of store with
       | Some conn -> ignore (T.delete_datum conn ~type_name ~id:(path_id path))
       | None -> ()))

(* ---- undo: restore value at head-n as a NEW write at head+1 ---- *)
let test_undo () =
  with_store (fun store ->
      let type_name = uniq "t" in
      declare_type store ~name:type_name ~fields:[ "title"; "body" ];
      let path = make_datum store ~type_name ~title:"v0" in
      let g0 = head_gen store path in
      (* two more appends: v1, v2 *)
      (match S.conn_of store with
       | Some conn ->
           ignore
             (T.upsert_datum conn ~path ~type_name
                ~body_json:"{\"title\":\"v1\",\"body\":\"b\"}"
                ~author:"tester" ~rank:None);
           ignore
             (T.upsert_datum conn ~path ~type_name
                ~body_json:"{\"title\":\"v2\",\"body\":\"b\"}"
                ~author:"tester" ~rank:None)
       | None -> ());
      let g2 = head_gen store path in
      Alcotest.(check bool) "history grew" true (Int64.compare g2 g0 > 0);
      (* undo 1 -> restore v1 (head-1) as a NEW write at head+1 *)
      (match C.tree_undo store ~path () with
       | VStr _ -> ()
       | _ -> Alcotest.fail "undo returned non-string");
      let g_after1 = head_gen store path in
      Alcotest.(check bool) "undo appended a new gen" true
        (Int64.compare g_after1 g2 > 0);
      Alcotest.(check (option string)) "undo 1 restores v1"
        (Some "v1")
        (version_json_title store path g_after1);
      (* undo again on the restored state = redo (climbs back to v2 via the
         gen chain, restore-by-write — spec: redo = undo again) *)
      (match C.tree_undo store ~path () with
       | VStr _ -> ()
       | _ -> Alcotest.fail "undo2 returned non-string");
      let g_after2 = head_gen store path in
      Alcotest.(check (option string)) "undo again redoes to v2"
        (Some "v2")
        (version_json_title store path g_after2));
  (* independent chain: undo ~n:2 reaches two steps deep (v0 from v2) *)
  with_store (fun store ->
      let type_name = uniq "t" in
      declare_type store ~name:type_name ~fields:[ "title"; "body" ];
      let path = make_datum store ~type_name ~title:"v0" in
      (match S.conn_of store with
       | Some conn ->
           ignore
             (T.upsert_datum conn ~path ~type_name
                ~body_json:"{\"title\":\"v1\",\"body\":\"b\"}"
                ~author:"tester" ~rank:None);
           ignore
             (T.upsert_datum conn ~path ~type_name
                ~body_json:"{\"title\":\"v2\",\"body\":\"b\"}"
                ~author:"tester" ~rank:None)
       | None -> ());
      (match C.tree_undo store ~path ~n:2L () with
       | VStr _ -> ()
       | _ -> Alcotest.fail "undo n=2 returned non-string");
      let g_new = head_gen store path in
      Alcotest.(check (option string)) "undo n=2 restores v0"
        (Some "v0")
        (version_json_title store path g_new);
      (* live node (tree/get head) reads the restored value too *)
      (match C.tree_get store ~path () with
       | VStr s ->
           (match Yojson.Safe.from_string s with
            | `Assoc kv ->
                Alcotest.(check (option string)) "live head after undo n=2"
                  (Some "v0") (body_title kv)
            | _ -> Alcotest.fail "undo live head not object")
       | _ -> Alcotest.fail "undo live head missing");
      (* cleanup *)
      (match S.conn_of store with
       | Some conn -> ignore (T.delete_datum conn ~type_name ~id:(path_id path))
       | None -> ()))

(* tree-time.consumers (notes-undo): undo must be KIND-AWARE. A note is a
   hierarchical node (kind=note written through upsert_hier); a naive
   restore-by-write through upsert_datum rewrites the payload envelope marker
   to "datum" (kind column stays note, payload->>'kind' flips to datum) —
   the corruption the notes-undo consumer exposes. Restore must go back
   through upsert_hier preserving the original kind. *)

(** Read the live tree_nodes payload envelope marker for a path. *)
let live_payload_kind (conn : _) (path : string) : string option =
  try
    let rows =
      Pricklypear_image_lib.Store_db.exec conn
        ~params:[ Pgx.Value.of_string path ]
        "SELECT payload->>'kind' FROM tree_nodes WHERE path = $1::ltree"
    in
    (match rows with
     | [ [ c ] ] -> Pgx.Value.to_string c
     | _ -> None)
  with _ -> None

let test_undo_note_kind () =
  with_store (fun store ->
      let slug = uniq "note" in
      let path = Printf.sprintf "data.note.u.tester.%s" slug in
      (* two note versions through the hier write surface *)
      (match S.conn_of store with
       | Some conn ->
           ignore
             (T.upsert_hier conn ~path ~kind:"note" ~type_name:"note"
                ~body_json:
                  (Printf.sprintf
                     "{\"uuid\":\"%s\",\"slug\":\"%s\",\"title\":\"v1\",\"tags\":\"\",\"kind\":\"note\",\"day\":\"\"}"
                     slug slug)
                ~author:"tester" ~rank:None);
           ignore
             (T.upsert_hier conn ~path ~kind:"note" ~type_name:"note"
                ~body_json:
                  (Printf.sprintf
                     "{\"uuid\":\"%s\",\"slug\":\"%s\",\"title\":\"v2\",\"tags\":\"\",\"kind\":\"note\",\"day\":\"\"}"
                     slug slug)
                ~author:"tester" ~rank:None)
       | None -> ());
      (match C.tree_undo store ~path () with
       | VStr _ -> ()
       | _ -> Alcotest.fail "note undo returned non-string");
      (* restored title, node kind, AND payload marker kind all intact *)
      (match C.tree_get store ~path () with
       | VStr s ->
           Printf.eprintf "[DBG] noteundo get: %s\n%!" s;
           (match Yojson.Safe.from_string s with
            | `Assoc kv ->
                Alcotest.(check (option string)) "undo restores note title v1"
                  (Some "v1") (body_title kv);
                let node_kind =
                  match List.assoc_opt "kind" kv with
                  | Some (`String k) -> Some k
                  | _ -> None
                in
                Alcotest.(check (option string)) "node kind stays note"
                  (Some "note") node_kind
            | _ -> Alcotest.fail "note live head not object")
       | _ -> Alcotest.fail "note live head missing");
      (match S.conn_of store with
       | Some conn ->
           Alcotest.(check (option string))
             "payload marker kind stays note (not datum)"
             (Some "note") (live_payload_kind conn path)
       | None -> ());
      (* cleanup: delete the whole note subtree *)
      (match S.conn_of store with
       | Some conn -> ignore (T.delete_path conn ~path ~recursive:true)
       | None -> ()))

let () =
  Random.self_init ();
  Alcotest.run "reads_as_of_gen"
    [
      ( "reads-as-of-gen",
        [
          Alcotest.test_case "tree/get optional :gen reads-as-of" `Quick
            test_tree_get_at_gen;
          Alcotest.test_case "gen-read absolute path at head and as-of"
            `Quick test_gen_read;
          Alcotest.test_case "gen-diff node-granular" `Quick test_gen_diff;
          Alcotest.test_case "undo restore-by-write" `Quick test_undo;
          Alcotest.test_case "undo kind-aware (note payload not datumed)"
            `Quick test_undo_note_kind;
        ] );
    ]
