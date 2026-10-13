(** safe_sort_sql contract (i-1788669685 — ORDER BY SQLi fix).

    (rows …)/(find-rows …) took the [sort] argument and appended it to the
    SQL verbatim; PostgreSQL accepts arbitrary expressions in ORDER BY, so
    that was an error-based read oracle over every table the image role can
    read (credentials, auth_sessions, …). The fix parses sort into a strict
    grammar and builds the fragment from validated tokens only.

    Pinned here: accepted forms and their exact SQL; every injection shape
    from the security review rejected with a clean Prim_error. *)

module C = Pricklypear_image_lib.Crud
module Ast = Pricklypear_lisp.Ast

let ok (label : string) (sort : string) (expect : string) : unit =
  match C.safe_sort_sql ~prim:"test" ~sort with
  | sql -> Alcotest.(check string) label expect sql
  | exception Ast.Interp_error (Ast.Prim_error (p, m, _)) ->
      Alcotest.failf "%s: rejected %S (%s: %s)" label sort p m
  | exception e -> Alcotest.failf "%s: unexpected %s" label
                     (Printexc.to_string e)

let bad (label : string) (sort : string) : unit =
  match C.safe_sort_sql ~prim:"test" ~sort with
  | sql -> Alcotest.failf "%s: accepted %S as %S" label sort sql
  | exception Ast.Interp_error (Ast.Prim_error ("test", _, "bad-args")) -> ()
  | exception e -> Alcotest.failf "%s: wrong error %s" label
                     (Printexc.to_string e)

let test_accepted () =
  ok "default column" "created_at" " ORDER BY created_at ASC";
  ok "updated_at desc" "updated_at DESC" " ORDER BY updated_at DESC";
  ok "desc lowercase" "id desc" " ORDER BY id DESC";
  ok "field accessor" "fields->>'verse' ASC" " ORDER BY fields->>'verse' ASC";
  ok "field no dir" "fields->>'score'" " ORDER BY fields->>'score' ASC";
  ok "whitespace tolerated" "  created_at   DESC  "
    " ORDER BY created_at DESC";
  ok "name with dash/underscore" "fields->>'the-num_1'"
    " ORDER BY fields->>'the-num_1' ASC";
  ok "numeric field cast" "fields->>'verse'::int ASC"
    " ORDER BY (fields->>'verse')::int ASC";
  ok "numeric field cast no dir" "fields->>'verse'::int"
    " ORDER BY (fields->>'verse')::int ASC"

let test_rejected () =
  (* Error-based exfiltration shapes from docs/security-review-2026-09.md. *)
  bad "case-oracle"
    "(CASE WHEN (SELECT 1 FROM credentials WHERE secret_data->>'api_key' LIKE 'sk-a%') THEN 1 ELSE 1/0 END)";
  bad "subquery" "(SELECT 1)";
  bad "two columns" "created_at, id";
  bad "unknown column" "author";
  bad "bare field name" "verse";
  bad "empty string" "";
  bad "only spaces" "   ";
  bad "quote escape" "fields->>'a'' OR 1=1 --'";
  bad "double dash comment" "created_at --";
  bad "semicolon" "created_at; DROP TABLE users";
  bad "three tokens" "created_at ASC DESC";
  bad "bad direction" "created_at UP";
  bad "empty field name" "fields->>''";
  bad "quote without close" "fields->>'verse";
  bad "double-quote field" "fields->>\"verse\"";
  bad "bad cast type" "fields->>'verse'::text";
  bad "cast on column" "created_at::int";
  bad "injection via cast" "fields->>'a'::int OR 1=1 --"

let () =
  let open Alcotest in
  run "crud_sort"
    [
      ( "safe_sort_sql"
      , [ test_case "accepted grammar" `Quick test_accepted
        ; test_case "injection shapes rejected" `Quick test_rejected
        ] )
    ]
