(** Pricklypear Image — ops_incidents filter tests (no DB).

    [Pricklypear_image_lib.Ops_incidents.is_expected_runtime_error] classifies an exception
    as an expected user/runtime error (log, don't file as a reliability
    incident) vs a genuine fault (file). This is a pure function over
    the [Interp_error] variants, so it can be unit-tested without a
    session or store. *)
open Pricklypear_lisp.Ast

let expected = [
  ("budget exceeded is expected", `Quick, fun () ->
    Alcotest.(check bool) "budget" true
      (Pricklypear_image_lib.Ops_incidents.is_expected_runtime_error
         (Interp_error Budget_exceeded)));
  ("user error is expected", `Quick, fun () ->
    Alcotest.(check bool) "user_error" true
      (Pricklypear_image_lib.Ops_incidents.is_expected_runtime_error
         (Interp_error (User_error "car needs a list or pair"))));
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
         (Interp_error (Prim_error ("get", "boom")))));
  ("parse error is filed (route shouldn't parse-fail at runtime)", `Quick, fun () ->
    Alcotest.(check bool) "parse" false
      (Pricklypear_image_lib.Ops_incidents.is_expected_runtime_error
         (Interp_error (Parse_error ("oops", 1, 1)))));
  ("non-Interp_error is filed", `Quick, fun () ->
    Alcotest.(check bool) "other" false
      (Pricklypear_image_lib.Ops_incidents.is_expected_runtime_error (Failure "boom")));
]

let () =
  Alcotest.run "ops_incidents" [
    ("expected", expected);
    ("still_file", still_file);
  ]
