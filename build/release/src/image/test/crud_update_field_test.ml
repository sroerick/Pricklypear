(* Crud update-field regression tests (i-1789074821).

    update-field on a DECLARED type used to route a throwaway single-field
    JSON through Tree_store.validate_fields, which requires every declared
    field to be present, so the prim always rejected on types with 2+
    fields ("missing fields") and callers fell back to full-row updates.
    The fix validates the ONE written field against the declared list
    (membership); undeclared types and empty declarations stay free-form,
    and the VALUE is never interpolated into validation JSON (a value with
    a double quote used to reject as "invalid JSON" before the write).

    Needs Postgres (scripts/dev.sh). Skips cleanly when unreachable. *)

module S = Pricklypear_image_lib.Store
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
      | Some _ -> f store)

let uniq p = Printf.sprintf "uf_%s_%d" p (Random.int 1_000_000)

(** True when [sub] occurs in [s] (empty needle: true). *)
let contains_sub s sub =
  let n = String.length sub and m = String.length s in
  let rec go i = i + n > m || String.sub s i n = sub || go (i + 1) in
  go 0

let expect_vstr what = function
  | VStr s -> s
  | _ -> Alcotest.fail (Printf.sprintf "%s: expected VStr" what)

let fields_of = function
  | VStr s ->
      (match Yojson.Safe.from_string s with
       | `Assoc kvs -> kvs
       | _ -> Alcotest.fail (Printf.sprintf "get returned a non-object: %s" s))
  | _ -> Alcotest.fail "get returned VNil (row missing or invisible)"

(* Declare the type and seed one row. Author "anonymous" matches the
   prim-path session user, so the RLS floor lets the test read it back. *)
let seed store type_name =
  S.persist_type store ~name:type_name ~fields:[ "title"; "body" ]
    ~author:"tester";
  expect_vstr "create"
    (C.create store ~type_name
       ~fields_json:"{\"title\":\"t0\",\"body\":\"b0\"}"
       ~author:"anonymous")

(* THE regression: a single-field update on a declared 2+-field type used
   to reject with "missing fields". *)
let test_update_one_field () =
  with_store (fun store ->
      let type_name = uniq "t" in
      let id = seed store type_name in
      let updated =
        C.update_field store ~type_name ~id ~field:"title" ~value:"t1" in
      Alcotest.(check string) "update-field echoes the id" id
        (expect_vstr "update-field" updated);
      let kvs = fields_of (C.get store ~type_name ~id) in
      Alcotest.(check bool) "title updated" true
        (List.assoc_opt "title" kvs = Some (`String "t1"));
      Alcotest.(check bool) "other field preserved" true
        (List.assoc_opt "body" kvs = Some (`String "b0")))

(* Unknown field still rejects cleanly, naming the field; row untouched. *)
let test_unknown_field_rejects () =
  with_store (fun store ->
      let type_name = uniq "t" in
      let id = seed store type_name in
      let before = fields_of (C.get store ~type_name ~id) in
      (match
         C.update_field store ~type_name ~id ~field:"nope" ~value:"x"
       with
       | VStr msg ->
           Alcotest.(check bool) "rejection names the field" true
             (contains_sub msg "nope")
       | _ -> Alcotest.fail "expected a rejection string");
      Alcotest.(check bool) "row untouched by rejection" true
        (before = fields_of (C.get store ~type_name ~id)))

(* A value containing a double quote used to reject as "invalid JSON"
   because it was interpolated into the validation sample. *)
let test_value_with_quotes () =
  with_store (fun store ->
      let type_name = uniq "t" in
      let id = seed store type_name in
      let v = "he said \"hi\"" in
      ignore
        (expect_vstr "update-field quoted value"
           (C.update_field store ~type_name ~id ~field:"body" ~value:v));
      let kvs = fields_of (C.get store ~type_name ~id) in
      Alcotest.(check bool) "quoted value stored verbatim" true
        (List.assoc_opt "body" kvs = Some (`String v)))

(* An UNDECLARED type_name is a clean rejection naming the type and the
   (deftype) surface (one-tree declared-type write gate); the row is not
   touched. *)
let test_undeclared_rejects () =
  with_store (fun store ->
      let type_name = uniq "ghost" in
      (match
         C.update_field store ~type_name
           ~id:"00000000-0000-0000-0000-000000000001" ~field:"title"
           ~value:"x"
       with
       | VStr msg ->
           Alcotest.(check bool) "rejection names undeclared" true
             (contains_sub msg "undeclared");
           Alcotest.(check bool) "rejection names (deftype)" true
             (contains_sub msg "deftype")
       | _ -> Alcotest.fail "update-field on undeclared type should reject"))

let () =
  Random.self_init ();
  Alcotest.run "crud_update_field"
    [
      ( "update-field",
        [
          ("update-one-field", `Quick, test_update_one_field);
          ("unknown-field-rejects", `Quick, test_unknown_field_rejects);
            ("value-with-quotes", `Quick, test_value_with_quotes);
            ("undeclared-rejects", `Quick, test_undeclared_rejects);
        ] );
    ]
