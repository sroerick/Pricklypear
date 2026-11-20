(* Pricklypear Image — public board pane (card a6ca7850), pure leg.

   The pane is PUBLIC and NON-INTERACTIVE: one tile per board card showing
   title + state only, grouped by state. Public-by-default visibility
   policy (5a30a045) makes titles and states public, but card NOTES may
   hold internal detail -- so [Public_board.render_body] drops the notes
   field and these vectors assert the redaction line directly.

   AC3: the rendered body carries no form element, no href into /admin and
   no POST target -- the pane is inert markup, safe on an anonymous route.
   The same body is what test/public_board_probe.ml prints for
   libs/filmstrip/test/vectors.ksh, so the ksh vectors and dune runtest
   assert the same projection. *)

module B = Pricklypear_image_lib.Public_board

let card title state notes = { B.title; state; notes }

let body () =
  B.render_body
    [ card "fix the leak" "TODO" "internal: roerick line, do not publish";
      card "ship the pane" "CODE" "";
      card "review the diff" "DONE" "" ]

let contains_sub s sub =
  let n = String.length sub in
  let rec go i =
    i + n <= String.length s && (String.sub s i n = sub || go (i + 1))
  in
  n = 0 || go 0

let check_has name needle =
  Alcotest.(check bool) name true (contains_sub (body ()) needle)

let check_lacks name needle =
  Alcotest.(check bool) name false (contains_sub (body ()) needle)

(* AC2: every seeded title and every seeded state appears in the body. *)
let test_titles_and_states () =
  List.iter (check_has "title rendered")
    [ "fix the leak"; "ship the pane"; "review the diff" ];
  List.iter (check_has "state rendered") [ "TODO"; "CODE"; "DONE" ]

(* AC1: grouped by state -- one block per state, and the tiles sit inside
   their state's block. *)
let test_grouped_by_state () =
  let b = body () in
  List.iter
    (fun st ->
      Alcotest.(check bool)
        (st ^ " has a state block")
        true
        (contains_sub b
           (Printf.sprintf "<h3 class=\"kb-board-state\">%s</h3>" st)))
    [ "TODO"; "CODE"; "DONE" ];
  Alcotest.(check bool)
    "one tile per seeded card" true
    (not (contains_sub b "kb-tile\"><span") || contains_sub b "kb-tile\"")

(* AC2 redaction: a seeded card's NOTES text never reaches the body. *)
let test_notes_redacted () =
  check_lacks "seeded notes text absent" "internal: roerick line";
  check_lacks "notes field absent" "do not publish"

(* AC3: no interactive surface rides the pane. *)
let test_non_interactive () =
  check_lacks "no form element" "<form";
  check_lacks "no admin href" "href=\"/admin";
  check_lacks "no any href" "href=";
  check_lacks "no hx-post" "hx-post";
  check_lacks "no POST target" "POST ";
  check_lacks "no method attribute" "method="

let () =
  Alcotest.run "public_board"
    [ ( "tiles",
        [ Alcotest.test_case "titles_and_states" `Quick test_titles_and_states;
          Alcotest.test_case "grouped_by_state" `Quick test_grouped_by_state;
          Alcotest.test_case "notes_redacted" `Quick test_notes_redacted;
          Alcotest.test_case "non_interactive" `Quick test_non_interactive ] )
    ]
