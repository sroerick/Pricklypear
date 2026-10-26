(** Pricklypear Image — ops_incidents filter tests (no DB).

    [Pricklypear_image_lib.Ops_incidents.is_expected_runtime_error] classifies an exception
    as an expected user/runtime error (log, don't file as a reliability
    incident) vs a genuine fault (file). This is a pure function over
    the [Interp_error] variants, so it can be unit-tested without a
    session or store. *)
open Pricklypear_lisp.Ast

(* A Pgx error carrying [code] — the client-fault SQLSTATEs (22P02 bad id
   cast, 42601 unparsable cast target) are the card fb492910 class: the
   request is malformed, not the image, so no ops incident is filed. *)
let pgx_err code =
  let fields =
    { Pgx.Error_response.code; severity = "ERROR"; message = "x"; custom = [] }
  in
  Pgx.PostgreSQL_Error ("msg", fields)

let expected = [
  ("budget exceeded is expected", `Quick, fun () ->
    Alcotest.(check bool) "budget" true
      (Pricklypear_image_lib.Ops_incidents.is_expected_runtime_error
         (Interp_error Budget_exceeded)));
  ("user error is expected", `Quick, fun () ->
    Alcotest.(check bool) "user_error" true
      (Pricklypear_image_lib.Ops_incidents.is_expected_runtime_error
         (Interp_error (User_error ("car needs a list or pair", "bad-args")))));
  ("arity mismatch is expected", `Quick, fun () ->
    Alcotest.(check bool) "arity" true
      (Pricklypear_image_lib.Ops_incidents.is_expected_runtime_error
         (Interp_error (Arity_mismatch ("lambda", 1, 2)))));
  ("wrong arg count is expected", `Quick, fun () ->
    Alcotest.(check bool) "wrong_arg" true
      (Pricklypear_image_lib.Ops_incidents.is_expected_runtime_error
         (Interp_error (Wrong_arg_count ("foo", 1, 2)))));
  ("type mismatch is expected", `Quick, fun () ->
    Alcotest.(check bool) "type_mismatch" true
      (Pricklypear_image_lib.Ops_incidents.is_expected_runtime_error
         (Interp_error (Type_mismatch ("list", VNil)))));
  ("22P02 invalid id cast is a client fault, not an incident", `Quick, fun () ->
    Alcotest.(check bool) "22P02" true
      (Pricklypear_image_lib.Ops_incidents.is_expected_runtime_error
         (pgx_err "22P02")));
  ("42601 unparsable cast target is a client fault, not an incident", `Quick, fun () ->
    Alcotest.(check bool) "42601" true
      (Pricklypear_image_lib.Ops_incidents.is_expected_runtime_error
         (pgx_err "42601")));
]

let still_file = [
  ("unbound var is filed", `Quick, fun () ->
    Alcotest.(check bool) "unbound" false
      (Pricklypear_image_lib.Ops_incidents.is_expected_runtime_error
         (Interp_error (Unbound_var "nope"))));
  ("not a function is filed", `Quick, fun () ->
    Alcotest.(check bool) "not_a_fn" false
      (Pricklypear_image_lib.Ops_incidents.is_expected_runtime_error
         (Interp_error (Not_a_function VNil))));
  ("prim error is filed", `Quick, fun () ->
    Alcotest.(check bool) "prim_error" false
      (Pricklypear_image_lib.Ops_incidents.is_expected_runtime_error
         (Interp_error (Prim_error ("get", "boom", "prim_error")))));
  ("parse error is filed (route shouldn't parse-fail at runtime)", `Quick, fun () ->
    Alcotest.(check bool) "parse" false
      (Pricklypear_image_lib.Ops_incidents.is_expected_runtime_error
         (Interp_error (Parse_error ("oops", 1, 1)))));
  ("non-Interp_error is filed", `Quick, fun () ->
    Alcotest.(check bool) "other" false
      (Pricklypear_image_lib.Ops_incidents.is_expected_runtime_error (Failure "boom")));
  ("unique violation is still filed", `Quick, fun () ->
    Alcotest.(check bool) "23505" false
      (Pricklypear_image_lib.Ops_incidents.is_expected_runtime_error
         (pgx_err "23505")));
  ("serialization failure is still filed", `Quick, fun () ->
    Alcotest.(check bool) "40001" false
      (Pricklypear_image_lib.Ops_incidents.is_expected_runtime_error
         (pgx_err "40001")));
]

let () =
  Alcotest.run "ops_incidents" [
    ("expected", expected);
    ("still_file", still_file);
  ]
