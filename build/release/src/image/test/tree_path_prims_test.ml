(** Unit tests for the kernel path algebra (one-tree.tree-path-prims).

    Pure — no DB. The five prims (tree/parent, tree/leaf, tree/segments,
    tree/child, tree/ancestors) are backed by Tree_store.path_*; the prim
    glue (Crud.tree_parent &co) is a thin VStr/VList/VNil wrap plus the
    Invalid_argument -> Prim_error conversion, both exercised here. *)

open Alcotest
module T = Pricklypear_image_lib.Tree_store
module C = Pricklypear_image_lib.Crud
open Pricklypear_lisp.Ast

let uuid = "00000000-0000-0000-0000-000000000001"

(* Strips a value down for equality checks. *)
let show_value = function
  | VStr s -> Printf.sprintf "(VStr %S)" s
  | VList _ -> "(VList ...)"
  | VNil -> "VNil"
  | VInt i -> Printf.sprintf "(VInt %Ld)" i
  | VSymbol s -> Printf.sprintf "(VSymbol %S)" s
  | VBool b -> Printf.sprintf "(VBool %b)" b
  | _ -> "(other value)"
let as_str = function VStr s -> s | v -> fail ("not a VStr: " ^ show_value v)
let as_strs = function
  | VList vs -> List.map as_str vs
  | _ -> fail "not a VList"
let is_nil = function VNil -> () | v -> fail ("not nil: " ^ show_value v)

(* ---- kernel-level algebra (Tree_store path helpers) ---- *)

let test_parent_leaf_segments () =
  check (option string) "parent of deep" (Some "data.u.alice.todo")
    (T.path_parent "data.u.alice.todo.00000000000000000000000000000001");
  check (option string) "parent of two-label" (Some "data")
    (T.path_parent "data.u");
  check (option string) "parent of root is none" None (T.path_parent "data");
  check string "leaf" "00000000000000000000000000000001"
    (T.path_leaf "data.u.alice.todo.00000000000000000000000000000001");
  check string "leaf of root" "data" (T.path_leaf "data");
  check (list string) "segments"
    [ "data"; "u"; "alice"; "todo"; "00000000000000000000000000000001" ]
    (T.path_segments "data.u.alice.todo.00000000000000000000000000000001");
  check (list string) "segments of root" [ "data" ] (T.path_segments "data");
  (* uppercase base64url label (a fn name's encoded form) passes through
     leaf/parent UNCHANGED — the kernel never re-sanitizes stored labels *)
  let p = "b." ^ T.uuid_label uuid ^ ".fn.Z3JlZXQ1" in
  check string "leaf uppercase label" "Z3JlZXQ1" (T.path_leaf p);
  check (option string) "parent uppercase label"
    (Some ("b." ^ T.uuid_label uuid ^ ".fn")) (T.path_parent p)

let test_ancestors () =
  check (list string) "strict prefixes root-first"
    [ "data"; "data.u"; "data.u.alice" ]
    (T.path_ancestors "data.u.alice.todo");
  check (list string) "root has none" [] (T.path_ancestors "data");
  check (list string) "two labels" [ "data" ] (T.path_ancestors "data.u")

let test_child_roundtrip () =
  (* spec smoke: (tree/child (tree/parent p) (tree/leaf p)) = p for data/b
     paths — INCLUDING uppercase base64url labels, which is why an already-
     ltree-safe label must pass through path_child verbatim. *)
  let roundtrip p =
    match T.path_parent p with
    | None -> fail ("root has no parent: " ^ p)
    | Some parent ->
        check string ("roundtrip " ^ p) p (T.path_child parent (T.path_leaf p))
  in
  List.iter roundtrip
    [ "data.u.alice.todo.00000000000000000000000000000001"
    ; "data.shared.bible.verse.00000000000000000000000000000001"
    ; "data.note.u.admin.garden"
    ; "b." ^ T.uuid_label uuid ^ ".fn.Z3JlZXQ1"
    ; "b." ^ T.uuid_label uuid ^ ".route._2Fnotes"
    ; "data.u.alice.todo.my_row_2Fid" ]

let test_child_encoding () =
  (* labels that need sanitizing go through path_label — identical
     semantics to tree/path-label (dashes, dots, uuid labels, empty) *)
  check string "dash+dot label" "data.u.alice.my_note_md"
    (T.path_child "data.u.alice" "my-note.md");
  check string "spaces collapse" "data.u.alice.a_b"
    (T.path_child "data.u.alice" "a  b");
  check string "empty label -> x" "data.u.alice.x"
    (T.path_child "data.u.alice" "");
  check string "edge underscores trimmed" "data.u.alice.ab"
    (T.path_child "data.u.alice" "-ab-");
  check string "uuid-shaped label verbatim"
    ("data.u.alice." ^ T.uuid_label uuid)
    (T.path_child "data.u.alice" (T.uuid_label uuid));
  check string "safe label verbatim (no re-lowercase)"
    "data.u.alice.MyLabel_2"
    (T.path_child "data.u.alice" "MyLabel_2")

let test_invalid_raise () =
  let raises name f =
    (try
       ignore (f ());
       fail (name ^ ": expected Invalid_argument")
     with Invalid_argument _ -> ())
  in
  raises "empty path" (fun () -> T.path_parent "");
  raises "empty segments" (fun () -> T.path_segments "");
  raises "double dot" (fun () -> T.path_leaf "a..b");
  raises "bad char" (fun () -> T.path_segments "data.u.al-ice");
  raises "trailing dot" (fun () -> T.path_ancestors "data.u.");
  raises "bad label child" (fun () -> T.path_child "bad..path" "x")

(* ---- prim glue (Crud tree wrappers) ---- *)

let test_prim_glue () =
  check string "tree_parent" "data.u.alice"
    (as_str (C.tree_parent "data.u.alice.todo"));
  is_nil (C.tree_parent "data");
  check string "tree_leaf" "todo" (as_str (C.tree_leaf "data.u.alice.todo"));
  check (list string) "tree_segments" [ "data"; "u" ]
    (as_strs (C.tree_segments "data.u"));
  check string "tree_child" "data.u.alice.my_note"
    (as_str (C.tree_child ~path:"data.u.alice" ~label:"my-note"));
  check (list string) "tree_ancestors" [ "data" ]
    (as_strs (C.tree_ancestors "data.u"));
  (* invalid path -> Prim_error raise (not a guess) *)
  (try
     ignore (C.tree_parent "bad..path");
     fail "expected Prim_error"
   with Pricklypear_lisp.Ast.Interp_error
          (Pricklypear_lisp.Ast.Prim_error ("tree/parent", _, _)) -> ())

let () =
  run "tree_path_prims"
    [ "algebra", [ test_case "parent-leaf-segments" `Quick test_parent_leaf_segments
                 ; test_case "ancestors" `Quick test_ancestors
                 ; test_case "child roundtrip" `Quick test_child_roundtrip
                 ; test_case "child encoding" `Quick test_child_encoding
                 ; test_case "invalid raises" `Quick test_invalid_raise ]
    ; "prims", [ test_case "glue" `Quick test_prim_glue ]
    ]
