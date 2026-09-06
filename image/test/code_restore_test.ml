(** tree-time.consumers: code-restore — the code-path analogue of (undo)
    (which is datum-path only). Reads the versioned state at head-n and
    re-writes it back through the code write surface (upsert_fn / upsert_type
    / upsert_route), so the reverse mirror re-populates function_defs /
    type_defs / view_bindings and the restore itself is versioned + redoable.
    "code-restore = gen-read + define" (the 012 lossless-redefine pattern).

    Below-head semantic matches code-history's "read the version row at
    gen-1": n=1 restores the state just BEFORE the latest change; an
    insert-only code node (nothing below head) has no state to restore.

    These tests exercise Store.code_restore directly. Needs Postgres
    (scripts/dev.sh); skips cleanly when unreachable.
*)

module S = Pricklypear_image_lib.Store
module T = Pricklypear_image_lib.Tree_store

let main_id = "00000000-0000-0000-0000-000000000001"

let with_store f =
  Eio_main.run @@ fun env ->
  Lwt_eio.with_event_loop ~clock:(Eio.Stdenv.clock env) @@ fun _ ->
  Eio.Switch.run @@ fun sw ->
  let ctx : Pricklypear_image_lib.Io_eio.Ctx.t =
    {
      sw;
      net = (Eio.Stdenv.net env :> [ `Generic ] Eio.Net.ty Eio.Std.r);
    }
  in
  Pricklypear_image_lib.Io_eio.with_ctx ctx (fun () ->
      let cfg = S.config_from_env () in
      let store = S.make ~cfg ~branch_id:main_id () in
      S.connect store;
      match S.conn_of store with
      | None ->
          Printf.printf
            "SKIP: no postgres (set PP_DB_* / run scripts/dev.sh)\n%!"
      | Some _ -> f store)

let uniq p = Printf.sprintf "crr_%s_%d_%d" p (Unix.getpid ()) (Random.int 1_000_000_000)

let contains_sub s sub =
  let ls = String.length s and lb = String.length sub in
  let rec go i = if i + lb > ls then false
    else if String.sub s i lb = sub then true
    else go (i + 1)
  in
  lb <= ls && (if lb = 0 then true else go 0)

let assert_body store ~branch_id ~name ~needle ~label =
  (* live_code_body_text reads the scaffolding table (function_defs / type_defs
     / view_bindings) — the authoritative post-reverse-mirror read. *) 
  let live =
    Option.value (S.live_code_body_text store ~branch_id ~name) ~default:""
  in
  Alcotest.(check bool) label true (contains_sub live needle)

(* Parse a top-level string field out of a JSON object text (robust to JSON
   reordering/relocating from canonicalization). *) 
let json_field (s : string) (key : string) : string option =
  match Yojson.Safe.from_string s with
  | `Assoc kv -> (
      match List.assoc_opt key kv with
      | Some (`String x) -> Some x
      | Some (`Int x) -> Some (string_of_int x)
      | Some (`Intlit x) -> Some x
      | Some (`Float x) -> Some (Printf.sprintf "%g" x)
      | _ -> None )
  | _ -> None

(* fn: 3 writes -> restore n=1 rewinds to the state just before the latest
   change (body 2); n=2 to body 1; out-of-range n is a clean error, not a
   crash; the restore itself becomes the new live + scaffolding head. *) 
let test_fn_restore () =
  with_store (fun store ->
      match S.conn_of store with
      | None -> ()
      | Some conn ->
          let name = uniq "fn" in
          ignore
            (T.upsert_fn conn ~branch_id:main_id ~name
               ~body_json:"{\"params\":[],\"body\":1}" ~author:"tester"
               ~doc:None);
          ignore
            (T.upsert_fn conn ~branch_id:main_id ~name
               ~body_json:"{\"params\":[],\"body\":2}" ~author:"tester"
               ~doc:None);
          ignore
            (T.upsert_fn conn ~branch_id:main_id ~name
               ~body_json:"{\"params\":[],\"body\":3}" ~author:"tester"
               ~doc:None);
          let r = S.code_restore store ~branch_id:main_id ~name ~author:"tester" () in
          Alcotest.(check bool) "n=1 returns restored-fn" true
            (contains_sub r "restored fn");
          let live1 =
            Option.value
              (S.live_code_body_text store ~branch_id:main_id ~name)
              ~default:""
          in
          Alcotest.(check (option string)) "n=1 rewinds body to 2"
            (Some "2") (json_field live1 "body");
          (* n=2 on a FRESH 3-write chain (no intervening restore): restores the
             state 2 back = body 1. Restore-by-write semantics: each restore is a
             NEW write, so n counts back from the live head through any prior
             restores — reuse of [name] after the n=1 restore would re-target the
             restored head. *) 
          let name3 = uniq "fn3" in
          ignore
            (T.upsert_fn conn ~branch_id:main_id ~name:name3
               ~body_json:"{\"params\":[],\"body\":1}" ~author:"tester"
               ~doc:None);
          ignore
            (T.upsert_fn conn ~branch_id:main_id ~name:name3
               ~body_json:"{\"params\":[],\"body\":2}" ~author:"tester"
               ~doc:None);
          ignore
            (T.upsert_fn conn ~branch_id:main_id ~name:name3
               ~body_json:"{\"params\":[],\"body\":3}" ~author:"tester"
               ~doc:None);
          let r2 =
            S.code_restore store ~branch_id:main_id ~name:name3 ~author:"tester" ~n:2 ()
          in
          Alcotest.(check bool) "n=2 returns restored-fn" true
            (contains_sub r2 "restored fn");
          let live2 =
            Option.value
              (S.live_code_body_text store ~branch_id:main_id ~name:name3)
              ~default:""
          in
          Alcotest.(check (option string)) "n=2 rewinds body to 1"
            (Some "1") (json_field live2 "body");
          let r3 =
            S.code_restore store ~branch_id:main_id ~name ~author:"tester" ~n:999 ()
          in
          Alcotest.(check bool) "out-of-range n is clean error" true
            (contains_sub r3 "no state");
          (* insert-only fn (nothing below head) has no state to restore *)
          let name2 = uniq "fn2" in
          ignore
            (T.upsert_fn conn ~branch_id:main_id ~name:name2
               ~body_json:"{\"params\":[],\"body\":1}" ~author:"tester"
               ~doc:None);
          let r4 =
            S.code_restore store ~branch_id:main_id ~name:name2 ~author:"tester" ()
          in
          Alcotest.(check bool) "insert-only has no state" true
            (contains_sub r4 "no state");
          (* unknown name errors cleanly *)
          let r5 =
            S.code_restore store ~branch_id:main_id
              ~name:(Printf.sprintf "%s_%d" name 999999) ~author:"tester" ()
          in
          Alcotest.(check bool) "unknown name errors cleanly" true
            (contains_sub r5 "no such code path");
          ignore (T.delete_fn conn ~branch_id:main_id ~name);
          ignore (T.delete_path conn ~path:(T.fn_path ~branch_id:main_id ~name:name2)
                    ~recursive:false);
          ignore (T.delete_path conn ~path:(T.fn_path ~branch_id:main_id ~name:name3)
                    ~recursive:false))

(* type: 3 writes -> restore n=1 rewinds the fields to the state before the
   latest change. *)
let test_type_restore () =
  with_store (fun store ->
      match S.conn_of store with
      | None -> ()
      | Some conn ->
          let name = uniq "ty" in
          let f i = Printf.sprintf "{\"fields\":[\"a%d\"]}" i in
          ignore
            (T.upsert_type conn ~branch_id:main_id ~name ~fields_json:(f 1)
               ~author:"tester");
          ignore
            (T.upsert_type conn ~branch_id:main_id ~name ~fields_json:(f 2)
               ~author:"tester");
          ignore
            (T.upsert_type conn ~branch_id:main_id ~name ~fields_json:(f 3)
               ~author:"tester");
          let r = S.code_restore store ~branch_id:main_id ~name ~author:"tester" () in
          Alcotest.(check bool) "type n=1 returns restored-type" true
            (contains_sub r "restored type");
          assert_body store ~branch_id:main_id ~name ~needle:"a2"
            ~label:"type n=1 rewinds fields to a2";
          ignore
            (T.delete_path conn
               ~path:(T.type_path ~branch_id:main_id ~name) ~recursive:false))

(* route: 3 writes -> restore n=1 rewinds handler + visibility to the state
   before the latest change. *)
let test_route_restore () =
  with_store (fun store ->
      match S.conn_of store with
      | None -> ()
      | Some conn ->
          let name = uniq "rt" in
          let h i = Printf.sprintf "handler%d" i in
          ignore
            (T.upsert_route conn ~branch_id:main_id ~route:name ~handler:(h 1)
               ~visibility:"public");
          ignore
            (T.upsert_route conn ~branch_id:main_id ~route:name ~handler:(h 2)
               ~visibility:"public");
          ignore
            (T.upsert_route conn ~branch_id:main_id ~route:name ~handler:(h 3)
               ~visibility:"private");
          let r = S.code_restore store ~branch_id:main_id ~name ~author:"tester" () in
          Alcotest.(check bool) "route n=1 returns restored-route" true
            (contains_sub r "restored route");
          let live =
            Option.value
              (S.live_code_body_text store ~branch_id:main_id ~name)
              ~default:""
          in
          Alcotest.(check (option string)) "route n=1 rewinds handler to handler2"
            (Some "handler2") (json_field live "handler");
          Alcotest.(check (option string)) "route n=1 rewinds visibility to public"
            (Some "public") (json_field live "visibility");
          ignore
            (T.delete_path conn
               ~path:(T.route_path ~branch_id:main_id ~route:name)
               ~recursive:false))

let () =
  Alcotest.run "code_restore"
    [
      ("fn", [ Alcotest.test_case "fn restore-by-write n=1/n=2" `Quick test_fn_restore ]);
      ("type", [ Alcotest.test_case "type restore-by-write n=1" `Quick test_type_restore ]);
      ("route", [ Alcotest.test_case "route restore-by-write n=1" `Quick test_route_restore ]);
    ]
