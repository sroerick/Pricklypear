open Pricklypear_lisp
let () =
  let transport _ _ = Ok Ast.VNil in
  let t = Remote.make ~transport in
  (try let v = Remote.eval t "'user" in Printf.printf "quoted = %s\n" (Reader.pp_value v)
   with e -> Printf.printf "err: %s\n" (Printexc.to_string e))
