(** Crypto primitives backed by Digestif (C stubs).

    Split out of [Prims_pure] so the pure-prims library stays free of
    C-stub deps and can be shared with the js_of_ocaml client target.
    The browser client does not need sha1 for structural editing. *)
open Pricklypear_lisp.Ast

let prim_sha1 args =
  match args with
  | [ v ] ->
      let s =
        match v with
        | VStr s -> s
        | VSymbol s -> s
        | VList [VStr "__str__"; VStr s] -> s
        | VList [VSymbol "__str__"; VStr s] -> s
        | VInt n -> Int64.to_string n
        | VFloat f -> Printf.sprintf "%.12g" f
        | VBool true -> "true"
        | VBool false -> "false"
        | VNil -> ""
        | _ -> "<nonstr>"
      in
      VStr (Digestif.SHA1.to_hex (Digestif.SHA1.digest_string s))
  | _ -> raise (Interp_error (User_error "sha1 needs 1 string"))
