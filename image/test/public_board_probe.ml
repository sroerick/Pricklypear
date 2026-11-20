(* Probe binary for libs/filmstrip/test/vectors.ksh (card a6ca7850): prints
   the public board pane body the image renders for a seeded board.

   The seed is three cards in three states, one of them carrying internal
   notes text. The probe calls the same Public_board.render_body the
   (public-board-tiles) prim serves, so the vectors script asserts the real
   projection: titles + states present, notes text absent, and no
   interactive surface (form / admin href / POST target). *)

module B = Pricklypear_image_lib.Public_board

let card title state notes = { B.title; state; notes }

let () =
  let cards =
    [ card "fix the leak" "TODO" "internal: roerick line, do not publish";
      card "ship the pane" "CODE" "";
      card "review the diff" "DONE" "" ]
  in
  print_string (B.render_body cards)
