(** Pure unit tests for storage.media helpers (no DB). *)

open Alcotest

let test_normalize_and_whitelist () =
  check bool "png ok" true (Pricklypear_image_lib.Media.content_type_allowed "image/png");
  check bool "jpeg ok" true
    (Pricklypear_image_lib.Media.content_type_allowed "image/jpeg; charset=binary");
  check bool "pdf ok" true
    (Pricklypear_image_lib.Media.content_type_allowed "application/pdf");
  check bool "octet denied" false
    (Pricklypear_image_lib.Media.content_type_allowed "application/octet-stream");
  check bool "html denied" false
    (Pricklypear_image_lib.Media.content_type_allowed "text/html");
  check string "normalize strips params" "image/png"
    (Pricklypear_image_lib.Media.normalize_content_type "Image/PNG; charset=x")

let test_sanitize_filename () =
  check (option string) "plain"
    (Some "shot.png")
    (Pricklypear_image_lib.Media.sanitize_filename (Some "shot.png"));
  check (option string) "strips path"
    (Some "shot.png")
    (Pricklypear_image_lib.Media.sanitize_filename (Some "/tmp/evil/shot.png"));
  check (option string) "empty -> none"
    None
    (Pricklypear_image_lib.Media.sanitize_filename (Some "  "));
  check (option string) "dotdot -> none"
    None
    (Pricklypear_image_lib.Media.sanitize_filename (Some ".."))

let test_url_and_b64 () =
  check string "url" "/media/abc"
    (Pricklypear_image_lib.Media.url_of_id "abc");
  let raw = "hello-media" in
  let b64 = Base64.encode_string raw in
  match Pricklypear_image_lib.Media.b64_decode_opt b64 with
  | Some s -> check string "roundtrip" raw s
  | None -> fail "b64 decode failed"

let test_sha256 () =
  let h = Pricklypear_image_lib.Media.sha256_hex "abc" in
  check string "sha256(abc)"
    "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad" h

let test_limits () =
  check int "http 5MiB" (5 * 1024 * 1024)
    Pricklypear_image_lib.Media.max_http_bytes;
  check int "eval 256KiB" (256 * 1024)
    Pricklypear_image_lib.Media.max_eval_put_bytes

(** BORGE-1785695911: visibility confinement SQL — non-admin actor is
    confined to own blobs; admin (or kernel caller, visible_to=None) sees
    all. The SQL fragment is the single predicate list/get/delete share. *)
let test_visible_sql () =
  check string "non-admin confines" "author = $2"
    (Pricklypear_image_lib.Media.visible_sql ~is_admin:false ~actor_param:2);
  check string "admin unconstrained" "TRUE"
    (Pricklypear_image_lib.Media.visible_sql ~is_admin:true ~actor_param:2)

let test_multipart_boundary () =
  let open Pricklypear_image_lib.Http_media in
  check (option string) "quoted"
    (Some "----WebKitFormBoundary7MA4YWxkTrZu0gW")
    (multipart_boundary_for_test
       "multipart/form-data; boundary=\"----WebKitFormBoundary7MA4YWxkTrZu0gW\"");
  check (option string) "plain"
    (Some "abc123")
    (multipart_boundary_for_test "multipart/form-data; boundary=abc123");
  check (option string) "not multipart" None
    (multipart_boundary_for_test "image/png")

let test_multipart_parse () =
  let open Pricklypear_image_lib.Http_media in
  let boundary = "----ppbound" in
  let body =
    String.concat "\r\n"
      [ "--" ^ boundary
      ; "Content-Disposition: form-data; name=\"file\"; filename=\"x.png\""
      ; "Content-Type: image/png"
      ; ""
      ; "PNGDATA"
      ; "--" ^ boundary ^ "--"
      ; ""
      ]
  in
  match parse_multipart_for_test ~boundary body with
  | Error e -> fail e
  | Ok parts ->
      (match pick_upload_file_for_test parts with
       | None -> fail "no file part"
       | Some p ->
           check string "field" "file" p.field_name;
           check (option string) "filename" (Some "x.png") p.filename;
           check (option string) "ct" (Some "image/png") p.content_type;
           check string "data" "PNGDATA" p.data)

let () =
  run "media"
    [ ( "whitelist"
      , [ test_case "content types" `Quick test_normalize_and_whitelist ] )
    ; ( "filename"
      , [ test_case "sanitize" `Quick test_sanitize_filename ] )
    ; ( "codec"
      , [ test_case "url + b64" `Quick test_url_and_b64
        ; test_case "sha256" `Quick test_sha256
        ] )
    ; ( "limits"
      , [ test_case "byte caps" `Quick test_limits ] )
    ; ( "visibility"
      , [ test_case "sql confinement" `Quick test_visible_sql ] )
    ; ( "multipart"
      , [ test_case "boundary" `Quick test_multipart_boundary
        ; test_case "parse file part" `Quick test_multipart_parse
        ] )
    ]
