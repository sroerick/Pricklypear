(** Conformance gate against the JS target.

    Runs the shared [Conformance.cases] through the same evaluator
    compiled with js_of_ocaml. Spec [structural-editing.client-eval]:
    CONFORMANCE IS THE GATE — client/server agreement is a tested
    invariant. Exits nonzero on any mismatch. *)
open Pricklypear_lisp

open Pricklypear_lisp
open Pricklypear_conformance
open Conformance

let () =
  let prims, env = Conformance.setup_arith () in
  let pass = ref 0 in
  let fail = ref 0 in
  List.iter (fun c ->
      List.iter (fun s -> ignore (Eval.eval_string ~prims env s)) c.setups;
      (try
         let got = Conformance.eval_pp ~prims env c.src in
         if got <> c.expected then begin
           incr fail;
           Printf.printf "FAIL [%s] %s: expected %S got %S\n"
             c.group c.name c.expected got
         end else incr pass
       with e ->
         incr fail;
         Printf.printf "FAIL [%s] %s: raised %s\n"
           c.group c.name (Printexc.to_string e)))
    Conformance.cases;
  Printf.printf "conformance (js target): %d pass, %d fail\n" !pass !fail;
  if !fail > 0 then exit 1
