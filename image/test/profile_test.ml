(** Profile tests (spec_town_profile): parse + seed-list application.
    Pure — no env reads, no filesystem. The env-reading wrappers
    (Profile.name / Profile.packages) fail closed in Boot_seed, which
    the boot logs surface loudly; the pure order contract is what these
    tests pin. *)

let parse_cases =
  [ ( "comments blanks and whitespace",
      fun () ->
        Alcotest.(check (list string)) "parse"
          [ "std"; "notes"; "kanban" ]
          (Pricklypear_image_lib.Profile.parse
             "# header comment\nstd\n\n  notes   # inline comment\n\nkanban\n# \n") );
    ( "empty text parses to empty list",
      fun () ->
        Alcotest.(check (list string)) "empty" []
          (Pricklypear_image_lib.Profile.parse "\n# only comments\n") );
  ]

(* The canonical priority list prefix shared with Boot_seed (std, theme,
   ui, layout, ...). apply_profile keeps canonical order for preferred
   names and appends non-preferred in FILE order. *)
let priority_prefix = [ "std"; "theme"; "ui"; "layout"; "css"; "html" ]

let full =
  priority_prefix
  @ [ "outline"; "structed"; "notes"; "todo"; "chat"; "calendar"; "weather"
    ; "agent"; "kanban" ]

let apply_cases =
  [ ( "preferred keep canonical order regardless of file order",
      fun () ->
        (* file lists them scrambled; output must be priority order *)
        let profile = [ "html"; "std"; "css"; "ui"; "theme" ] in
        Alcotest.(check (list string)) "canonical"
          [ "std"; "theme"; "ui"; "css"; "html" ]
          (Pricklypear_image_lib.Boot_seed.apply_profile ~profile) );
    ( "extras follow in file order after preferred",
      fun () ->
        let profile = [ "kanban"; "notes"; "std"; "todo" ] in
        (* notes + std are preferred; kanban + todo... todo is preferred
           too, so extras = kanban only *)
        Alcotest.(check (list string)) "extras-last"
          ([ "std"; "notes"; "todo" ] @ [ "kanban" ])
          (Pricklypear_image_lib.Boot_seed.apply_profile ~profile) );
    ( "profile may drop preferred packages",
      fun () ->
        let profile = [ "std"; "notes" ] in
        Alcotest.(check (list string)) "subset" [ "std"; "notes" ]
          (Pricklypear_image_lib.Boot_seed.apply_profile ~profile) );
    ( "profile name not in libs still appears (load-library target)",
      fun () ->
        (* the profile is the allowlist too: a package the box has not
           discovered (e.g. tree on mainframe priority) must survive so
           tenants can load it once shipped *)
        let profile = [ "std"; "tree" ] in
        Alcotest.(check (list string)) "unseeded-name" [ "std"; "tree" ]
          (Pricklypear_image_lib.Boot_seed.apply_profile ~profile) );
    ( "empty profile list applies to empty",
      fun () ->
        Alcotest.(check (list string)) "empty" []
          (Pricklypear_image_lib.Boot_seed.apply_profile ~profile:[]) );
  ]

let () =
  Alcotest.run "profile"
    [ ("parse", List.map (fun (n, f) -> n, `Quick, f) parse_cases)
    ; ("apply", List.map (fun (n, f) -> n, `Quick, f) apply_cases) ]
