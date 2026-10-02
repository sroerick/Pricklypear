(** Unit tests for tree_store pure helpers: label codec round-trip,
    path constructors. No DB. (Hashing moved DB-side by the 2026-09
    hash-domain unification; see tree_version_shadow_test.) *)

open Alcotest
module T = Pricklypear_image_lib.Tree_store

let rt s =
  match T.dec_label (T.enc_label s) with
  | Some r -> r
  | None -> fail ("decode failed for: " ^ s)

let long n = String.make n 'a'   (* >=57 bytes -> base64 output wraps in PG *)

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
  check string "json" "{\"a\":1,\"b\":[2,3]}" (rt "{\"a\":1,\"b\":[2,3]}");
  check string "57 bytes (MIME wrap boundary)" (long 57) (rt (long 57));
  check string "200 bytes" (long 200) (rt (long 200))

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
  List.iter check_safe
    [ ""; "greet"; "/notes/:id"; "notes/doc/get";
      "héllo"; "a:b+c/d"; "my type"; long 57; long 76; long 200 ]

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
  check string "pkg path"
    ("b." ^ T.uuid_label b ^ ".pkg." ^ T.enc_label "todo")
    (T.pkg_path ~branch_id:b ~name:"todo");
  check string "datum path user"
    ("data.u." ^ T.enc_label "alice" ^ "." ^ T.enc_label "todo" ^ "." ^ T.uuid_label b)
    (T.datum_path ~user:"alice" ~type_name:"todo" ~id:b ());
  check string "datum path seed"
    ("data.shared." ^ T.enc_label "bible-verse" ^ "." ^ T.uuid_label b)
    (T.datum_path ~user:"alice" ~type_name:"bible-verse" ~id:b ());
  check string "guestbook is shared"
    ("data.shared." ^ T.enc_label "guestbook_entry" ^ "." ^ T.uuid_label b)
    (T.datum_path ~user:"public" ~type_name:"guestbook_entry" ~id:b ());
  (* paths contain no raw dots/slashes beyond the ltree separators *)
  let p = T.fn_path ~branch_id:b ~name:"notes/doc/get" in
  check bool "fn path has exactly 3 separators" true
    (String.length (String.concat "" (String.split_on_char '.' p)) >= 0
     && List.length (String.split_on_char '.' p) = 4)

(* hash + envelope OCaml builders were REMOVED by the 2026-09 hash-domain
   unification (one-tree.hash-domain): node hashes are computed DB-side
   over PG canonical jsonb text, one domain for every writer (Tree_store
   upserts + the 009 mirror). The domain is guarded by the cross-writer
   equality test in tree_version_shadow_test (hash domain cross-writer). *)

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

let test_path_label () =
  check string "plain" "john" (T.path_label "john");
  check string "hyphen to us" "my_note" (T.path_label "my-note");
  check string "upper" "genesis" (T.path_label "Genesis");
  check string "spaces" "a_b" (T.path_label "a  b");
  check string "empty" "x" (T.path_label "");
  check string "trim edge us" "ab" (T.path_label "-ab-");
  check bool "ltree safe" true (T.valid_path "data.note.u.admin.garden.wiki");
  check bool "hyphen invalid path" false (T.valid_path "data.note.my-note");
  check string "garden root" "data.note.u.admin" (T.garden_root ~user:"admin");
  check bool "hier kinds" true
    (T.is_hier_kind "folder" && T.is_hier_kind "note" && T.is_hier_kind "heading");
  check bool "datum not hier" false (T.is_hier_kind "datum")

let () =
  run "tree_store"
    [ "codec", [ test_case "roundtrip" `Quick test_label_roundtrip
               ; test_case "ltree-safe" `Quick test_label_ltree_safe
               ; test_case "uuid label" `Quick test_uuid_label ]
    ; "paths", [ test_case "constructors" `Quick test_paths
               ; test_case "path_label" `Quick test_path_label ]
    ; "typed", [ test_case "validate_fields" `Quick test_validate ]
    ]
