(** Pricklypear browser evaluator runtime.

    A js_of_ocaml build of the pure Nopales interpreter (lisp/lib) plus
    the shared pure-prims table (pure/prims_pure.ml). The same .pp
    package source the server serves is evaluated verbatim here — see
    borge/structural-editing.borg client-eval.

    Built with `js_of_ocaml --effects=cps` (OCaml 5 effects require the
    CPS backend; the default backend fatally aborts). Exposes a small
    JSON-bound surface to JavaScript via [Js.export]:

      boot()                 -> initializes the prim table + env
      load_source(string)    -> evaluates a multi-form .pp source string
      apply_op(string, json) -> runs a pure op fn over a JSON tree,
                                returns the result tree as JSON
      eval(string)           -> evaluates one Nopales expression,
                                returns its printed value (for REPL/debug)

    The pure prim table EXCLUDES every DB / IO / net prim — the
    capability boundary IS the prim table. Wall-clock prims (now,
    date-today, date-weekday, etc.) are absent too: they live in
    image-only prims_time.ml because they read Unix. *)
open Pricklypear_lisp

(* --- the runtime state, initialized by boot --- *)
let prims : Eval.prim_table option ref = ref None
let env : Ast.env option ref = ref None

let boot () =
  (* ONE shared catalog (pure/prims_pure.ml [Prims_pure.install], built
     in client/lib/pp_client_table.ml) instead of a hand enumeration
     that had drifted 8 prims behind the image (i-1788670799-f). The
     pure table EXCLUDES every DB / IO / net / wall-clock prim — the
     capability boundary IS the prim table. *)
  let t = Pp_client_table.build () in
  (* Bind each prim name as VPrim in env, mirroring session.ml. *)
  let e = Env.create () in
  Hashtbl.iter
    (fun name _ -> Env.define e name (Ast.VPrim name))
    t;
  (* Route prim dispatch through the direct table, bypassing the
     Effect.Deep handler. Under js_of_ocaml's CPS backend the
     one-shot deep-match continuations resume twice on nested prim
     calls (Continuation_already_resumed); direct_prims avoids
     performing Prim_call effects entirely. Spec: structural-editing.client-eval. *)
  Pricklypear_lisp.Control.direct_prims := Some t;
  prims := Some t;
  env := Some e

let get_prims () =
  match !prims with Some t -> t | None -> failwith "pp: boot() not called"
let get_env () =
  match !env with Some e -> e | None -> failwith "pp: boot() not called"

(* Evaluate a multi-form .pp source string (load-library shape). Mirrors
   image/lib/loader.ml: read_all + eval each form under the effect
   handler. The reader handles ;; line comments natively. *)
let load_source (src : Js_of_ocaml.Js.js_string Js_of_ocaml.Js.t) : Js_of_ocaml.Js.js_string Js_of_ocaml.Js.t =
  let s = Js_of_ocaml.Js.to_string src in
  let t = get_prims () in
  let e = get_env () in
  (try
     Control.run_sync ~prims:t (fun () ->
         let forms = Reader.read_all s in
         List.iter (fun form ->
             let budget = Eval.make_budget () in
             ignore (Eval.eval ~budget ~prims:t e form))
           forms)
   with _ -> ());
  Js_of_ocaml.Js.string "ok"

(* Evaluate one expression, return its printed value (REPL/debug). *)
let eval (src : Js_of_ocaml.Js.js_string Js_of_ocaml.Js.t) : Js_of_ocaml.Js.js_string Js_of_ocaml.Js.t =
  let s = Js_of_ocaml.Js.to_string src in
  let t = get_prims () in
  let e = get_env () in
  try
    let v = Eval.eval_string ~prims:t e s in
    Js_of_ocaml.Js.string (Reader.pp_value v)
  with
  | Ast.Interp_error err ->
      Js_of_ocaml.Js.string
        ("ERROR: " ^ Ast.pp_interp_error err)
  | Reader.Reader_error (m, l, c) ->
      Js_of_ocaml.Js.string (Printf.sprintf "ERROR: %s (%d:%d)" m l c)
  | e -> Js_of_ocaml.Js.string ("ERROR: " ^ Printexc.to_string e)

(* set_tree(json): parse the outline tree JSON into a Nopales value and
   bind it as `pp-tree` in the env. json-parse of a JSON array yields a
   VList, matching the server's tree shape. Spec: structural-editing.
   client-eval OPTIMISTIC EXECUTION MODEL step 1. *)
let set_tree (json : Js_of_ocaml.Js.js_string Js_of_ocaml.Js.t) : unit =
  let s = Js_of_ocaml.Js.to_string json in
  let t = get_prims () in
  let e = get_env () in
  let v = Eval.eval_string ~prims:t e ("(json-parse " ^ Printf.sprintf "%S" s ^ ")") in
  Env.define e "pp-tree" v

(* apply_op(op, nid, slug): run the optimistic preview in the browser —
   dispatch the pure op over pp-tree (same outline/structed fns the
   server runs) and render via outline/html-lite. Returns the preview
   HTML, or "" if the op is not previewable (set-*, eval, no-op). *)
let apply_op
    (op : Js_of_ocaml.Js.js_string Js_of_ocaml.Js.t)
    (nid : Js_of_ocaml.Js.js_string Js_of_ocaml.Js.t)
    (slug : Js_of_ocaml.Js.js_string Js_of_ocaml.Js.t)
  : Js_of_ocaml.Js.js_string Js_of_ocaml.Js.t =
  let op_s = Js_of_ocaml.Js.to_string op in
  let nid_s = Js_of_ocaml.Js.to_string nid in
  let slug_s = Js_of_ocaml.Js.to_string slug in
  let t = get_prims () in
  let e = get_env () in
  let expr =
    Printf.sprintf "(pp/preview %S pp-tree %S %S)" slug_s op_s nid_s
  in
  try
    let v = Eval.eval_string ~prims:t e expr in
    (match v with
     | Ast.VStr s -> Js_of_ocaml.Js.string s
     | _ -> Js_of_ocaml.Js.string "")
  with _ -> Js_of_ocaml.Js.string ""

let () =
  let open Js_of_ocaml.Js in
  let obj =
    Unsafe.obj
      [| "boot", Unsafe.inject (wrap_callback boot)
       ; "load_source", Unsafe.inject (wrap_callback load_source)
       ; "eval", Unsafe.inject (wrap_callback eval)
       ; "set_tree", Unsafe.inject (wrap_callback set_tree)
       ; "apply_op", Unsafe.inject (wrap_callback apply_op)
      |]
  in
  export "pp" obj
