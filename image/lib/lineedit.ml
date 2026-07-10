(** Pricklypear Image — Minimal line editor.

    For the minimal image: read a single line of input from a channel and
    return it. No cursor movement, no history, no reverse-search.

    Per spec [image.repl-experience]: real line editing should use
    ocaml-linenoise (small, Mirage-compatible-ish). The minimal stub is
    acceptable for the boot-checkpoint smoke test. TODO: linenoise. *)
open Lwt.Syntax

(* Read a line, returning [None] at EOF. *)
let read_line (ic : Lwt_io.input_channel) : string option Lwt.t =
  let* line = Lwt_io.read_line_opt ic in
  Lwt.return line
