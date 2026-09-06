(** Write-prim no-DB contract (i-1788670475).

    Before this fix the crud module signaled "no DB" three different ways
    on its WRITE prims: create raised Interp_error, update/update-field/
    delete returned VNil (indistinguishable from "no row matched"), and
    tree/upsert returned a VStr error — worst of all, tree/delete returned
    the fabricated success shape {"deleted":0,"inbound_edges":0}, so a
    destructive flow looked idempotent with the DB down.

    Contract pinned here: every WRITE prim raises
    Interp_error (Prim_error (<prim>, "failed: no DB", "failed")) when the
    store has no connection; READ prims keep their empty defaults (VNil /
    VList [] — DB-down-as-empty is the separate swallow-and-default class,
    i-1788671060). Companion shape fix: Tree_store write helpers all return
    (string, string) result (one convention).

    No DB needed: Store.make without connect gives conn=None, and the
    no-DB arms run before any I/O. *)

module S = Pricklypear_image_lib.Store
module C = Pricklypear_image_lib.Crud
module Ast = Pricklypear_lisp.Ast

let main_id = "00000000-0000-0000-0000-000000000001"

let no_db_store () =
  S.make ~cfg:(S.config_from_env ()) ~branch_id:main_id ()

(** True when [f ()] raises exactly an Interp_error. *)
let raises_interp (f : unit -> Ast.value) : bool =
  match f () with
  | _ -> false
  | exception Ast.Interp_error _ -> true
  | exception _ -> false

(** Show a value compactly for failure messages. *)
let show (v : Ast.value) : string =
  match v with
  | Ast.VStr s -> Printf.sprintf "VStr %S" s
  | Ast.VNil -> "VNil"
  | Ast.VList _ -> "VList"
  | Ast.VBool b -> Printf.sprintf "VBool %b" b
  | Ast.VInt i -> Printf.sprintf "VInt %Ld" i
  | Ast.VFloat f -> Printf.sprintf "VFloat %g" f
  | Ast.VSymbol s -> Printf.sprintf "VSymbol %s" s
  | Ast.VPair _ -> "VPair"
  | _ -> "other"

let check_raises_no_db (name : string) (f : unit -> Ast.value) : unit =
  if raises_interp f then ()
  else
    let shown =
      match f () with
      | v -> show v
      | exception e -> Printexc.to_string e
    in
    Alcotest.fail
      (Printf.sprintf
         "%s: expected Interp_error (failed: no DB), got %s" name shown)

let test_write_prims_raise () =
  let store = no_db_store () in
  (* CRUD datum writes (create already raised pre-fix; the rest were VNil) *)
  check_raises_no_db "create"
    (fun () -> C.create store ~type_name:"t" ~fields_json:"{}" ~author:"a");
  check_raises_no_db "update"
    (fun () ->
      C.update store ~type_name:"t" ~id:"00000000-0000-0000-0000-00000000000f"
        ~fields_json:"{}");
  check_raises_no_db "update-field"
    (fun () ->
      C.update_field store ~type_name:"t"
        ~id:"00000000-0000-0000-0000-00000000000f" ~field:"f" ~value:"1");
  check_raises_no_db "delete"
    (fun () ->
      C.delete store ~type_name:"t"
        ~id:"00000000-0000-0000-0000-00000000000f");
  (* tree writes (tree/delete used to return the fabricated
     {"deleted":0,"inbound_edges":0} success shape) *)
  check_raises_no_db "tree/upsert (hier)"
    (fun () ->
      C.tree_upsert store ~path:"b.x.note.p" ~kind:"note" ~type_name:"note"
        ~body_json:"{\"title\":\"t\"}" ~author:"a");
  check_raises_no_db "tree/upsert (datum)"
    (fun () ->
      C.tree_upsert store ~path:"data.u.a.t.new" ~kind:"datum" ~type_name:"t"
        ~body_json:"{}" ~author:"a");
  check_raises_no_db "tree/delete"
    (fun () -> C.tree_delete store ~path:"b.x.fn.f" ~recursive:false);
  check_raises_no_db "tree/move"
    (fun () ->
      C.tree_move store ~old_path:"b.x.fn.f" ~new_path:"b.x.fn.g");
  check_raises_no_db "link"
    (fun () ->
      C.link_edge store ~from_path:"b.x.fn.f" ~to_path:"data.u.a.t.abc"
        ~edge_kind:"ref" ~label:None ~author:"a");
  check_raises_no_db "unlink"
    (fun () -> C.unlink_edge store ~edge_path:"b.x.fn.f.lnk.abc");
  check_raises_no_db "undo"
    (fun () -> C.tree_undo store ~path:"b.x.fn.f" ());
  check_raises_no_db "tree/move-before"
    (fun () ->
      C.tree_move_before store ~path:"b.x.note.a" ~anchor:"b.x.note.b");
  check_raises_no_db "tree/move-after"
    (fun () ->
      C.tree_move_after store ~path:"b.x.note.a" ~anchor:"b.x.note.b");
  check_raises_no_db "tree/reorder"
    (fun () -> C.tree_reorder store ~path:"b.x.note.a" ~rank:"1");
  check_raises_no_db "tree/insert-child"
    (fun () ->
      C.tree_insert_child store ~parent:"b.x.note.parent" ~kind:"note"
        ~type_name:"note" ~body_json:"{\"title\":\"t\"}" ~anchor:None
        ~author:"a")

(** Reads keep their empty defaults — the write/read split is deliberate
    (reads are harmless when degraded; writes must be loud). *)
let test_reads_keep_empty_defaults () =
  let store = no_db_store () in
  Alcotest.(check bool) "get is VNil" true
    (match
       C.get store ~type_name:"t" ~id:"00000000-0000-0000-0000-00000000000f"
     with
     | Ast.VNil -> true
     | v -> Alcotest.fail ("expected VNil, got " ^ show v));
  Alcotest.(check bool) "tree_get is VNil" true
    (match C.tree_get store ~path:"b.x.fn.f" () with
     | Ast.VNil -> true
     | v -> Alcotest.fail ("expected VNil, got " ^ show v))

let () =
  let open Alcotest in
  run "crud_no_db"
    [
      ( "write-prims-raise",
        [ test_case "every write prim raises Interp_error on no DB" `Quick
            test_write_prims_raise
        ; test_case "read prims keep empty defaults" `Quick
            test_reads_keep_empty_defaults
        ] )
    ]
