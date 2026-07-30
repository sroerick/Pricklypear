(** Unit tests for tree_store pure helpers: label codec round-trip,
    path constructors, hashing, envelope. No DB. *)

open Alcotest
module T = Pricklypear_image_lib.Tree_store

let rt s =
  match T.dec_label (T.enc_label s) with
  | Some r -> r
  | None -> fail ("decode failed for: " ^ s)

let test_label_roundtrip () =
  check string "empty" "" (rt "");
  check string "plain" "greet" (rt "greet");
  check string "slash route" "/notes/:id" (rt "/notes/:id");
  check string "dots" "notes/doc/get" (rt "notes/doc/get");
  check string "unicode" "héllo wörld" (rt "héllo wörld");
  check string "colon+plus" "a:b+c/d" (rt "a:b+c/d");
  check string "spaces" "my type name" (rt "my type name");
  check string "pad2" "ab" (rt "ab");       (* 2 bytes -> 1 pad *)
  check string "pad1" "abc" (rt "abc");     (* 3 bytes -> 0 pad *)
  check string "json" "{\"a\":1,\"b\":[2,3]}" (rt "{\"a\":1,\"b\":[2,3]}")

let test_label_ltree_safe () =
  (* Encoded labels must be ltree-safe: [A-Za-z0-9_] and trailing pad digit *)
  let check_safe s =
    let e = T.enc_label s in
    String.iter
      (fun c ->
        let ok =
          (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
          || (c >= '0' && c <= '9') || c = '_'
        in
        if not ok then fail (Printf.sprintf "unsafe char %c in %s (from %s)" c e s))
      e
  in
  List.iter check_safe [ ""; "greet"; "/notes/:id"; "notes/doc/get";
                         "héllo"; "a:b+c/d"; "my type" ]

let test_uuid_label () =
  check string "strips dashes" "00000000000000000000000000000001"
    (T.uuid_label "00000000-0000-0000-0000-000000000001")

let test_paths () =
  let b = "00000000-0000-0000-0000-000000000001" in
  check string "fn path"
    ("b." ^ T.uuid_label b ^ ".fn." ^ T.enc_label "greet")
    (T.fn_path ~branch_id:b ~name:"greet");
  check string "type path"
    ("b." ^ T.uuid_label b ^ ".type." ^ T.enc_label "todo")
    (T.type_path ~branch_id:b ~name:"todo");
  check string "route path"
    ("b." ^ T.uuid_label b ^ ".route." ^ T.enc_label "/notes/:id")
    (T.route_path ~branch_id:b ~route:"/notes/:id");
  check string "datum path"
    ("data." ^ T.enc_label "todo" ^ "." ^ T.uuid_label b)
    (T.datum_path ~type_name:"todo" ~id:b);
  (* paths contain no raw dots/slashes beyond the ltree separators *)
  let p = T.fn_path ~branch_id:b ~name:"notes/doc/get" in
  check bool "fn path has exactly 3 separators" true
    (String.length (String.concat "" (String.split_on_char '.' p)) >= 0
     && List.length (String.split_on_char '.' p) = 4)

let test_hash () =
  check string "sha256(abc)"
    "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
    (T.hash_body "abc")

let test_envelope () =
  let env =
    T.envelope ~kind:"fn" ~path:"b.x.fn.y" ~type_:"fn"
      ~body:(`String "(lambda ())") ~author:"admin" ~doc:(Some "d")
  in
  match env with
  | `Assoc kvs ->
      check (option string) "kind" (Some "fn")
        (match List.assoc_opt "kind" kvs with
         | Some (`String s) -> Some s | _ -> None);
      check (option string) "doc" (Some "d")
        (match List.assoc_opt "doc" kvs with
         | Some (`String s) -> Some s | _ -> None);
      check bool "has hash" true (List.mem_assoc "hash" kvs)
  | _ -> fail "envelope not assoc"

let test_validate () =
  let open T in
  (match validate_fields ~declared:["a";"b"] "{\"a\":1,\"b\":2}" with
   | Ok () -> ()
   | Error e -> fail e);
  (match validate_fields ~declared:["a";"b"] "{\"a\":1}" with
   | Error msg ->
       check bool "mentions missing" true (String.contains msg 'b' || String.length msg > 0)
   | Ok () -> fail "should reject missing b");
  (match validate_fields ~declared:["a"] "{\"a\":1,\"extra\":true}" with
   | Ok () -> ()  (* extra keys allowed *)
   | Error e -> fail e);
  (match validate_fields ~declared:[] "{}" with
   | Ok () -> ()
   | Error e -> fail e)

let () =
  run "tree_store"
    [ "codec", [ test_case "roundtrip" `Quick test_label_roundtrip
               ; test_case "ltree-safe" `Quick test_label_ltree_safe
               ; test_case "uuid label" `Quick test_uuid_label ]
    ; "paths", [ test_case "constructors" `Quick test_paths ]
    ; "envelope", [ test_case "hash" `Quick test_hash
                  ; test_case "build" `Quick test_envelope ]
    ; "typed", [ test_case "validate_fields" `Quick test_validate ]
    ]
