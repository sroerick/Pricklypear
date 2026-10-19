(** Crypto primitives backed by Digestif (C stubs).

    Split out of [Prims_pure] so the pure-prims library stays free of
    C-stub deps and can be shared with the js_of_ocaml client target.
    The browser client does not need sha1/sha256 for structural editing.
    sha256 exists for the tuna FED surface (libs/tuna/lib.pp): the
    rehash-verify-and-reject client laws need a verifiable digest prim. *)
open Pricklypear_lisp.Ast

(* Display-string coercion shared by the digest prims: symbols read as
   their name, the __str__ tag unwraps, numbers/bools render, and
   anything else hashes its display form rather than erroring (same
   policy sha1 shipped with). *)
let str_of_value = function
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

let prim_sha1 args =
  match args with
  | [ v ] -> VStr (Digestif.SHA1.to_hex (Digestif.SHA1.digest_string (str_of_value v)))
  | _ -> raise (Interp_error (User_error ("sha1 needs 1 string", "bad-args")))

let prim_sha256 args =
  match args with
  | [ v ] -> VStr (Digestif.SHA256.to_hex (Digestif.SHA256.digest_string (str_of_value v)))
  | _ -> raise (Interp_error (User_error ("sha256 needs 1 string", "bad-args")))
