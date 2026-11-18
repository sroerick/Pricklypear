(** Claims visibility: public by default, private opt-in per scope
    (card 5a30a045, operator policy 11-12 "everything should be public by
    default").

    The write-flag / read-floor disagreement this pins: the write side has
    always let an identity stamp a body [vis] field, but the read floor
    IGNORED it and hardcoded "unflagged = private" — so gregor's broadcast
    of u.you.claim...observed.wall.slug read 403/404 to every non-root
    identity, and a legitimately granted reader of a private client scope
    was denied too.

    Pinned here against the pure read-floor decision
    ([Crud.note_vis_allows]), which [Crud.gate_note_vis] and the
    tree/find-by vis clause both use, so a change in one place cannot drift
    from the other:

      - unflagged / public writes read (200) for ANY other identity,
        including the town namespaces (u.hollis, u.gregor) — the default;
      - private is opt-in per scope and consults a GRANT lookup, so the
        owner and a granted space member read (200) while an ungranted
        identity is denied (403);
      - a sovereign reader (root/kernel viewer=None stays ungated; an admin
        rides the grant) reads everything (200).
*)

module C = Pricklypear_image_lib.Crud

(* The read floor's decision as an HTTP status: allow -> 200, deny -> 403. *)
let status ~(vis : string option) ~(viewer : string) ~(granted : bool) : int =
  let vis = match vis with Some v -> v | None -> "public" in
  if C.note_vis_allows ~vis ~viewer ~granted then 200 else 403

(* ---- C7/C2: an unflagged write is public, across identities ---- *)

let test_public_write_cross_identity () =
  Alcotest.(check int)
    "an unflagged write in another identity's namespace reads 200" 200
    (status ~vis:None ~viewer:"u.gregor" ~granted:false);
  Alcotest.(check int) "explicitly public reads 200 for any other id" 200
    (status ~vis:(Some "public") ~viewer:"u.hollis" ~granted:false);
  Alcotest.(check int) "public reads 200 even for anonymous" 200
    (status ~vis:(Some "public") ~viewer:"anonymous" ~granted:false)

(* ---- C3: town namespaces carry no flag and read cross-id ---- *)

let test_town_namespace_public () =
  List.iter
    (fun (owner, viewer) ->
      Alcotest.(check int)
        (Printf.sprintf "%s's unflagged town write reads 200 for %s" owner
           viewer)
        200 (status ~vis:None ~viewer ~granted:false))
    [ ("u.hollis", "u.gregor"); ("u.gregor", "u.hollis");
      ("u.broland", "u.cholla"); ("u.cholla", "u.broland") ]

(* ---- C4: a guest's private scope, owner + grantee + ungranted ---- *)

let test_guest_scope_private () =
  (* owner: the garden gate does not fire for the owner at all; the floor
     admits it either way (a grant is trivially held). *)
  Alcotest.(check int) "owner reads its own private scope" 200
    (status ~vis:(Some "private") ~viewer:"u.cos" ~granted:true);
  Alcotest.(check int) "granted space member reads the private scope" 200
    (status ~vis:(Some "private") ~viewer:"u.jonny" ~granted:true);
  Alcotest.(check int) "ungranted u.gregor is denied the private scope" 403
    (status ~vis:(Some "private") ~viewer:"u.gregor" ~granted:false);
  (* the town's unflagged writes next door are untouched by a neighbour's
     private opt-in. *)
  Alcotest.(check int) "ungranted u.hollis still reads an unflagged write" 200
    (status ~vis:None ~viewer:"u.hollis" ~granted:false)

(* ---- C5: a sovereign reader reads everything ---- *)

let test_root_reads_all () =
  (* root/kernel reads pass viewer=None and stay ungated by the gate; at the
     decision level that is a sovereign grant on the private client scope. *)
  Alcotest.(check int) "root reads u.cos's private client scope (no grant)" 200
    (status ~vis:(Some "private") ~viewer:"root" ~granted:false);
  (* "users" widens to every signed-in identity. *)
  Alcotest.(check int) "an unflagged-signed-in read of a users scope" 200
    (status ~vis:(Some "users") ~viewer:"u.gregor" ~granted:false);
  Alcotest.(check int) "a users scope still excludes anonymous" 403
    (status ~vis:(Some "users") ~viewer:"anonymous" ~granted:false)

let () =
  Alcotest.run "claims-visibility"
    [ ( "claims-visibility"
      , [ Alcotest.test_case "public-write-cross-identity" `Quick
            test_public_write_cross_identity
        ; Alcotest.test_case "town-namespace-public" `Quick
            test_town_namespace_public
        ; Alcotest.test_case "guest-scope-private" `Quick test_guest_scope_private
        ; Alcotest.test_case "root-reads-all" `Quick test_root_reads_all
        ] )
    ]
