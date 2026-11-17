(** erde — the one driver command surface that lives beside the image.

      erde export --journal journal.jsonl --out trajectories/

    Reads the turn journal and writes one JSONL training trajectory per
    session that holds a user/assistant pair (see Erde_export). Prints
    [trajectories written: N] and exits 0 even when the journal yields none. *)

let usage = "erde export --journal <journal.jsonl> --out <dir>"

let journal = ref ""
let out = ref ""

let spec =
  [
    ( "--journal",
      Arg.Set_string journal,
      "turn journal to read (newline-delimited JSON)" );
    ("--out", Arg.Set_string out, "directory to write trajectory files into");
  ]

let () =
  let anon = ref [] in
  Arg.parse spec (fun a -> anon := a :: !anon) usage;
  match List.rev !anon with
  | "export" :: _ ->
      if !journal = "" || !out = "" then begin
        prerr_endline usage;
        exit 2
      end;
      exit (Erde_export.export_cli ~journal:!journal ~out:!out)
  | _ ->
      prerr_endline usage;
      exit 2
