(** Tier limits tests (spec_town_profile P1): pure decision math only.
    Env-reading wrappers and Profile parsing are covered by profile_test
    and boot behavior; these pin the window counter, the quota decision,
     and directive folding. *)
module Tier_limits = Pricklypear_image_lib.Tier_limits

let test_allow_write () =
  let open Tier_limits in
  Hashtbl.reset write_counters;
  let limit = 3 in
  Alcotest.(check bool) "first admitted"
    true (allow_write ~username:"u" ~limit ~now:100.0);
  Alcotest.(check bool) "second admitted"
    true (allow_write ~username:"u" ~limit ~now:101.0);
  Alcotest.(check bool) "third admitted"
    true (allow_write ~username:"u" ~limit ~now:102.0);
  Alcotest.(check bool) "fourth denied same window"
    false (allow_write ~username:"u" ~limit ~now:110.0);
  Alcotest.(check bool) "new window resets"
    true (allow_write ~username:"u" ~limit ~now:3600.0);
  (* independent users do not share windows *)
  Alcotest.(check bool) "other user admitted"
    true (allow_write ~username:"v" ~limit ~now:3600.5);
  (* limit <= 0 disables *)
  Alcotest.(check bool) "disabled admits"
    true (allow_write ~username:"w" ~limit:0 ~now:0.0)

let test_media_ok () =
  let open Tier_limits in
  Alcotest.(check (result unit (triple int int int)))
    "under cap ok" (Ok ())
    (media_ok ~used:10 ~incoming:5 ~limit:100);
  Alcotest.(check (result unit (triple int int int)))
    "zero-byte incoming ok" (Ok ())
    (media_ok ~used:100 ~incoming:0 ~limit:100);
  Alcotest.(check (result unit (triple int int int)))
    "exactly at cap ok" (Ok ())
    (media_ok ~used:95 ~incoming:5 ~limit:100);
    Alcotest.(check (result unit (triple int int int)))
      "one byte over denied" (Error (95, 6, 100))
      (media_ok ~used:95 ~incoming:6 ~limit:100)

let test_apply_directives () =
  let open Tier_limits in
  let base = default_tenant () in
  let l =
    apply_directives base
      [ ("tenant", "writes-per-min", 60)
      ; ("tenant", "media-mb", 32)
      ; ("instance", "max-users", 50) ]
  in
  Alcotest.(check int) "writes folded" 60 l.writes_per_min;
  Alcotest.(check int) "media folded" (32 * 1024 * 1024) l.media_bytes;
  (* last directive wins *)
  let l2 =
    apply_directives base
      [ ("tenant", "writes-per-min", 60); ("tenant", "writes-per-min", 240) ]
  in
  Alcotest.(check int) "later wins" 240 l2.writes_per_min

let tests =
  [ ("allow_write window", `Quick, test_allow_write)
  ; ("media_ok quota", `Quick, test_media_ok)
  ; ("apply_directives", `Quick, test_apply_directives) ]

let () = Alcotest.run "tier_limits" [ "limits", tests ]
