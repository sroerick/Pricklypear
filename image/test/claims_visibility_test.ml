(** Claims visibility: private unless marked public.

    Owner decision 2026-11-23, and the book's rule (notes.borg
    note-visibility: "Absent reads private"). This replaces card
    5a30a045's public-by-default contract.

    Pinned against the pure read-floor decision ([Crud.note_vis_allows]),
    which [Crud.gate_note_vis] uses and which [Tree_store.find_by_body_field]
    and pp_vis_readable (migration 050) mirror:

      - an unflagged write in another identity's namespace is denied (403),
        including the town namespaces (u.hollis, u.gregor);
      - public is an explicit flag and reads for anyone, anonymous included;
      - users reads for any signed-in identity, never anonymous;
      - private (and absent) honors the grant lookup: a granted space member
        reads, an ungranted identity is denied;
      - no viewer name, root included, bypasses the decision.
*)

module C = Pricklypear_image_lib.Crud

(* The read floor's decision as an HTTP status: allow -> 200, deny -> 403.
   [vis = None] is an unflagged write; gate_note_vis maps it to "private". *)
let status ~(vis : string option) ~(viewer : string) ~(granted : bool) : int =
  let vis = match vis with Some v -> v | None -> "private" in
  if C.note_vis_allows ~vis ~viewer ~granted then 200 else 403

let test_unflagged_is_private () =
  Alcotest.(check int)
    "an unflagged write in another identity's namespace is denied" 403
    (status ~vis:None ~viewer:"u.gregor" ~granted:false);
  Alcotest.(check int) "an unflagged write is denied to anonymous" 403
    (status ~vis:None ~viewer:"anonymous" ~granted:false);
  Alcotest.(check int) "an unrecognized vis value is treated as private" 403
    (status ~vis:(Some "everyone") ~viewer:"u.hollis" ~granted:false)

let test_town_namespace_private () =
  List.iter
    (fun (owner, viewer) ->
      Alcotest.(check int)
        (Printf.sprintf "%s's unflagged town write is denied to %s" owner
           viewer)
        403 (status ~vis:None ~viewer ~granted:false))
    [ ("u.hollis", "u.gregor"); ("u.gregor", "u.hollis");
      ("u.broland", "u.cholla"); ("u.cholla", "u.broland") ]

let test_public_is_explicit () =
  Alcotest.(check int) "explicitly public reads 200 for any other id" 200
    (status ~vis:(Some "public") ~viewer:"u.hollis" ~granted:false);
  Alcotest.(check int) "explicitly public reads 200 for anonymous" 200
    (status ~vis:(Some "public") ~viewer:"anonymous" ~granted:false)

let test_users_scope () =
  Alcotest.(check int) "users reads for a signed-in identity" 200
    (status ~vis:(Some "users") ~viewer:"u.gregor" ~granted:false);
  Alcotest.(check int) "users excludes anonymous" 403
    (status ~vis:(Some "users") ~viewer:"anonymous" ~granted:false)

let test_private_grant () =
  Alcotest.(check int) "granted space member reads a private scope" 200
    (status ~vis:(Some "private") ~viewer:"u.jonny" ~granted:true);
  Alcotest.(check int) "granted space member reads an unflagged write" 200
    (status ~vis:None ~viewer:"u.jonny" ~granted:true);
  Alcotest.(check int) "ungranted identity is denied a private scope" 403
    (status ~vis:(Some "private") ~viewer:"u.gregor" ~granted:false)

let test_no_root_bypass () =
  Alcotest.(check int) "root without a grant is denied a private scope" 403
    (status ~vis:(Some "private") ~viewer:"root" ~granted:false);
  Alcotest.(check int) "root without a grant is denied an unflagged write" 403
    (status ~vis:None ~viewer:"root" ~granted:false)

let () =
  Alcotest.run "claims-visibility"
    [ ( "claims-visibility"
      , [ Alcotest.test_case "unflagged-is-private" `Quick
            test_unflagged_is_private
        ; Alcotest.test_case "town-namespace-private" `Quick
            test_town_namespace_private
        ; Alcotest.test_case "public-is-explicit" `Quick test_public_is_explicit
        ; Alcotest.test_case "users-scope" `Quick test_users_scope
        ; Alcotest.test_case "private-grant" `Quick test_private_grant
        ; Alcotest.test_case "no-root-bypass" `Quick test_no_root_bypass
        ] )
    ]
