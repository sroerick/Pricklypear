open Pricklypear_lisp
let () =
  Eio_main.run @@ fun _env ->
  let rt = Runtime.make () in
  (match Runtime.load_file ~rt "libs/std/lib.pp" with
   | Ok () -> print_endline "std OK"
   | Error e -> print_endline ("std ERR: " ^ e))
