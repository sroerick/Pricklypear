(* The garden welcome note (card 2c440c53): the filmstrip's FIRST pane is a
   note the gardener owns, not code. GARDEN_DIR names a directory on disk and
   welcome.md inside it is read AT RENDER TIME -- roerick edits the file and
   reloads, no redeploy and no definition edit. An unset/empty GARDEN_DIR, a
   missing file, or a blank file falls back to the one hardcoded string the
   card allows. *)

let default = "Welcome to Pricklypear"

let default_dir () =
  Filename.concat
    (match Sys.getenv_opt "HOME" with Some h when h <> "" -> h | _ -> ".")
    ".pricklypear/garden"

let dir () =
  match Sys.getenv_opt "GARDEN_DIR" with
  | Some d when String.trim d <> "" -> String.trim d
  | _ -> default_dir ()

let read path =
  try
    let ic = open_in_bin path in
    let n = in_channel_length ic in
    let s = really_input_string ic n in
    close_in ic;
    if String.trim s <> "" then Some s else None
  with _ -> None

let welcome_text () =
  match read (Filename.concat (dir ()) "welcome.md") with
  | Some text -> text
  | None -> default
