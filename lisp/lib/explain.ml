(** Mechanical inside-out (innermost-first) peel of a Nopales AST.

    Pure structure + light annotations. LLM glosses are image-layer. *)
open Ast
open Recursion

(** One peel layer: depth, a human kind label, an annotation string,
    the subexpression, and its path relative to the root. *)
type layer = {
  depth : int;
  kind : string;
  annotation : string;
  expr : expr;
  path : int list;
}

(** Short label for an expression shape (e.g. ["var"], ["call:map"]). *)
let kind_of = function
  | Var _ -> "var"
  | Lit _ -> "lit"
  | App (Var n, _) -> "call:" ^ n
  | App _ -> "app"
  | Let _ -> "let"
  | Letrec _ -> "letrec"
  | If _ -> "if"
  | Lambda _ -> "lambda"
  | Prim (n, _) -> "prim:" ^ n
  | Seq _ -> "seq"
  | Quote _ -> "quote"
  | Define _ -> "define"

(** Build a human annotation for [e] describing recursion kind (tail /
    non-tail), primitives, calls and literals, per [lisp-interpreter.eval].*)
let annotate ~(fn_name : string) ~(info : recursion_info) (e : expr)
    ~(path : int list) : string =
  let bits = ref [] in
  (match e with
   | App (Var n, _) when n = fn_name ->
       bits := "recursive call" :: !bits;
       if is_tail_self_path info path then bits := "tail" :: !bits
       else if is_nontail_self_path info path then bits := "non-tail" :: !bits
   | Prim (n, _) -> bits := ("prim " ^ n) :: !bits
   | App (Var n, _) -> bits := ("calls " ^ n) :: !bits
   | Lit _ -> bits := "literal" :: !bits
   | Var n -> bits := ("var " ^ n) :: !bits
   | If _ -> bits := "conditional" :: !bits
   | Lambda _ -> bits := "closure" :: !bits
   | _ -> ());
  match !bits with
  | [] -> kind_of e
  | xs -> String.concat "; " (List.rev xs)

(** Depth of an expression (1 = leaf). *)
let rec depth_of = function
  | Var _ | Lit _ | Quote _ -> 1
  | App (f, args) ->
      1 + List.fold_left max (depth_of f) (List.map depth_of args)
  | Let (_, a, b) | Seq (a, b) -> 1 + max (depth_of a) (depth_of b)
  | Letrec (bs, body) ->
      let d =
        List.fold_left max (depth_of body)
          (List.map (fun (_, e) -> depth_of e) bs)
      in
      1 + d
  | If (a, b, c) ->
      1 + max (depth_of a) (max (depth_of b) (depth_of c))
  | Lambda (_, body) | Define (_, body) -> 1 + depth_of body
  | Prim (_, args) ->
      1
      + List.fold_left max 0 (List.map depth_of args)

(** All subexpressions with path and depth, leaves first (stable). *)
let collect_nodes (e : expr) : (int * int list * expr) list =
  let acc = ref [] in
  let rec go path e =
    acc := (depth_of e, List.rev path, e) :: !acc;
    match e with
    | Var _ | Lit _ | Quote _ -> ()
    | App (f, args) ->
        go (0 :: path) f;
        List.iteri (fun i a -> go ((i + 1) :: path) a) args
    | Let (_, v, body) ->
        go (0 :: path) v;
        go (1 :: path) body
    | Letrec (bs, body) ->
        List.iteri (fun i (_, e') -> go (i :: path) e') bs;
        go (List.length bs :: path) body
    | If (t, th, el) ->
        go (0 :: path) t;
        go (1 :: path) th;
        go (2 :: path) el
    | Lambda (_, body) | Define (_, body) -> go (0 :: path) body
    | Prim (_, args) -> List.iteri (fun i a -> go (i :: path) a) args
    | Seq (a, b) ->
        go (0 :: path) a;
        go (1 :: path) b
  in
  go [] e;
  (* Innermost first: higher depth first; then left-to-right by path. *)
  !acc
  |> List.sort (fun (d1, p1, _) (d2, p2, _) ->
         let c = compare d2 d1 in
         if c <> 0 then c else compare p1 p2)

(** Peel [e] (assumed to be [fn_name]'s body) into an innermost-first
    list of layers, each annotated with recursion info. *)
let peel ~(fn_name : string) (e : expr) : layer list =
  let body = unwrap_fn_body e in
  let info = analyze_self ~fn_name body in
  collect_nodes body
  |> List.mapi (fun i (_d, path, expr) ->
         {
           depth = i;
           kind = kind_of expr;
           annotation = annotate ~fn_name ~info expr ~path;
           expr;
           path;
         })

(** Render peel [layers] as human-readable lines (depth/kind/annotation). *)
let layers_to_strings (layers : layer list) : string list =
  List.map
    (fun (l : layer) ->
      Printf.sprintf "[%d] %s — %s\n  %s" l.depth l.kind l.annotation
        (Reader.pp_expr l.expr))
    layers

(** ---- Stepping / trace debugger (borge/remote-eval.borg stepping) ----

    The LLM-debugger substrate. eval runs in a client process; stepping
    is PURELY a client concern — the server never learns a breakpoint
    fired. The data plane still crosses the wire exactly as a fast eval
    would: pure compute stays local, every prim call that is not a local
    pure prim is forwarded via [transport], and its value/error returns
    to the paused continuation.

    [trace] is a ONE-PASS trace: it runs [src] once under a proxy-style
    rim and records an ordered, interleaved event list whose [Local]
    entries count the local pure eval steps since the last wire crossing
    and whose [Remote_ok]/[Remote_err] entries are the remote prim round
    trips (distinguished from local steps). The [Breakpoint]/[Step]/
    [Continue] driver walks that already-produced trace (presentation
    stepping): it NEVER re-evals, so side effects are never repeated and
    the wire is never re-crossed — stepping is a client concern by
    construction (remote-eval.borg hard-constraints). *)

(** One trusted event in a [trace] run. [Local n] = n local pure eval
    steps since the last wire crossing (0 between two adjacent round
    trips); [Remote_ok (name, value)] = a remote prim round-trip
    returned [value]; [Remote_err (name, err)] = it relayed an error. *)
type trace_event =
  | Local of int
  | Remote_ok of string * string
  | Remote_err of string * string

(** A full one-pass trace of one expression. [events] is in execution
    order (innermost-first stepping would revisit, but the trace records
    the actual crossed wire + local-step sequence). *)
type trace_result = {
  result : string;         (* final value, or "error: …" on a raised exn *)
  events : trace_event list;
  total_steps : int;       (* budget steps consumed by the whole eval *)
}

let pp_trace_event = function
  | Local n -> Printf.sprintf "local %d step(s)" n
  | Remote_ok (name, v) -> Printf.sprintf "remote %s -> %s" name v
  | Remote_err (name, e) -> Printf.sprintf "remote %s !! %s" name e

(** Render a [trace_result] as display lines (one per event, plus the
    outcome). A trace of an expression shows local pure steps
    interleaved with remote prim round trips, distinguished. *)
let trace_to_strings (tr : trace_result) : string list =
  List.map pp_trace_event tr.events
  @ [ Printf.sprintf "=> %s (%d steps)" tr.result tr.total_steps ]

(* Stepping uses OCaml-5 effect handlers (a proxy-style rim intercepting
   [Control.Prim_call]): alias Stdlib.Effect here so [Effect.Deep] in the
   trace handler below resolves. *)
module Effect = Stdlib.Effect

(** Run [src] once under a proxy-style rim and record the interleaved
    local/remote event list. [local_prims] is the local pure prim table
    (serviced locally — counts as local steps, never crosses the wire);
    [transport] is the remote round trip (name -> args -> (value, exn),
    the same boundary [Proxy_rim.transport] uses); [init_env] is the
    starting env (defaults to a fresh top-level env). Pure and Lwt-free. *)
let trace
    ~(local_prims : Eval.prim_table)
    ~(transport : string -> Ast.value list -> (Ast.value, exn) result)
    ~(init_env : Ast.env)
    ~(src : string)
    : trace_result =
  let env = init_env in
  let events = ref ([] : trace_event list) in
  let last_steps = ref 0 in
  (* sample current-budget absolute steps; attribute a [Local n] span
     since the last sample (the running counter is published by
     [Eval.eval_step] while we are inside the eval that performed the
     [Prim_call]). *)
  let add_local () =
    let n = Eval.current_steps () - !last_steps in
    last_steps := Eval.current_steps ();
    if n > 0 then events := Local n :: !events
  in
  let budget = Eval.make_budget () in
  Eval.last_budget := Some budget;
  let outcome =
    try
      let forms = Reader.read_all src in
      let open Effect.Deep in
      match_with
        (fun () ->
          List.fold_left
            (fun _ f -> Eval.eval ~budget ~prims:local_prims env f)
            VNil forms)
        ()
        { retc = (fun v -> `Ok v)
        ; exnc = (fun e -> `Exn e)
        ; effc =
            (fun (type a) (eff : a Effect.t) ->
              match eff with
              | Control.Prim_call (name, args) ->
                  Some
                    (fun (k : (a, _) continuation) ->
                      match Hashtbl.find_opt local_prims name with
                      | Some fn ->
                          (* local pure prim: serviced locally, a local
                             step, no wire crossing *)
                          (try continue k (fn args)
                           with exn -> discontinue k exn)
                      | None ->
                          add_local ();
                          (match transport name args with
                           | Ok v ->
                               events :=
                                 Remote_ok (name, Reader.pp_value v) :: !events;
                               continue k v
                           | Error e ->
                               events :=
                                 Remote_err (name, Printexc.to_string e)
                                 :: !events;
                               discontinue k e))
              | _ -> None)
        }
    with exn -> `Exn exn
  in
  add_local ();
  {
    result =
      (match outcome with
       | `Ok v -> Reader.pp_value v
       | `Exn e -> "error: " ^ Printexc.to_string e);
    events = List.rev !events;
    total_steps = budget.steps;
  }

(** A drivable stepper over an already-produced [trace_result]. [Step]
    advances one event, [Continue] advances to the next breakpoint, and
    [Frames] inspects the source-level AST at the current position using
    the peel above. It never re-evals the source, so nothing is repeated
    and the wire is never re-crossed. *)
type stepper = {
  tr : trace_result;
  mutable cursor : int;          (* index of the next event to step to *)
  mutable breakpoints : (int -> trace_event -> bool) list;
}

let stepper_of_trace (tr : trace_result) : stepper = { tr; cursor = 0; breakpoints = [] }

(** The index of the current event (past the last event when done). *)
let current_index (s : stepper) : int = s.cursor

(** True when the stepper has consumed every event. *)
let done_p (s : stepper) : bool = s.cursor >= List.length s.tr.events

(** Advance one event; raise [End_of_file] when already done. Returns
    the event stepped onto. *)
let step (s : stepper) : trace_event =
  if done_p s then raise End_of_file;
  let e = List.nth s.tr.events s.cursor in
  s.cursor <- s.cursor + 1;
  e

(** Register a breakpoint predicate; [continue] stops at the first
    future event satisfying any registered predicate. *)
let add_breakpoint (s : stepper) (pred : int -> trace_event -> bool) : unit =
  s.breakpoints <- pred :: s.breakpoints

(** Advance (step) until the next breakpoint fires or the trace ends.
    Returns [true] if a breakpoint fired, [false] if the trace ran to
    completion. *)
let continue (s : stepper) : bool =
  let fired = ref false in
  while (not !fired) && not (done_p s) do
    let _ = step s in
    let idx = s.cursor - 1 in
    let e = List.nth s.tr.events idx in
    if List.exists (fun p -> p idx e) s.breakpoints then fired := true
  done;
  !fired

(** A convenient breakpoint: pause on a remote prim round trip whose
    name matches [name] (the natural "break on this server prim"). *)
let break_on_remote (name : string) : int -> trace_event -> bool =
  fun _idx -> function
    | Remote_ok (n, _) | Remote_err (n, _) -> n = name
    | Local _ -> false

(** Inspect the source-level frame stack at the current position: the
    peeked AST layers of [src] (the peel machinery above). [src] may
    hold several top-level forms; each is peeled independently. *)
let frames (src : string) : string list =
  let forms = try Reader.read_all src with _ -> [] in
  List.flatten
    (List.map (fun f -> layers_to_strings (peel ~fn_name:"<expr>" f)) forms)
