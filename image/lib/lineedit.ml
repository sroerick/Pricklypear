(** Pricklypear Image — Minimal line editor.

    For the minimal image: read a single line of input from a channel and
    return it. No cursor movement, no history, no reverse-search.

    Per spec [image.repl-experience]: real line editing should use
    ocaml-linenoise (small, Mirage-compatible-ish). The minimal stub is
    acceptable for the boot-checkpoint smoke test. TODO: linenoise. *)

(* Read a line, returning [None] at EOF. Blocks the Eio fiber via Lwt_eio. *)
let read_line (ic : Lwt_io.input_channel) : string option =
  Lwt_eio.run_lwt (fun () -> Lwt_io.read_line_opt ic)
