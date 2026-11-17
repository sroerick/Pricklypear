(** PP smoke: the tier matrix, driven entirely by the harness fixtures.

    Per tier: act as a real identity, call a real prim through the image's
    dispatch, assert on the wire contract.  There is no test-local tier table
    here on purpose — the policy under test is reached through the fixture
    (admin/agent/guest, the same dispatch the image serves), which is the
    surface the borge drift card consumes. *)

let h = Pp_smoke_fixtures.local_harness ()

(* The prims the matrix crosses with {!Pp_smoke_fixtures.all_tiers}, and the
   tier each one needs.  The EXPECTED status is never written down here: it is
   asked of the fixture, so a drift in the real policy fails this test. *)
let prims = [ "admin.revoke"; "eval"; "tree.write"; "read" ]

let check tier prim =
  let id = Pp_smoke_fixtures.act_as h ~actor:"smoke" tier in
  let r = Pp_smoke_fixtures.call h id ~prim ~arg:"" in
  let expected = if Pp_smoke_fixtures.tier_allows ~tier prim then 200 else 403 in
  Pp_smoke_fixtures.assert_status expected r;
  if expected = 200 then
    Pp_smoke_fixtures.assert_body
      (fun b -> String.length b > 0 && b = Pp_smoke_fixtures.body r)
      r
  else
    Pp_smoke_fixtures.assert_body (fun b -> String.length b > 0) r

let () =
  List.iter
    (fun tier ->
      List.iter (fun prim -> check tier prim) prims)
    Pp_smoke_fixtures.all_tiers
