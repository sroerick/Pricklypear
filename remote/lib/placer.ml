(** remote/lib/placer.ml (borge/remote-eval.borg placement).

    Hybrid placement: decide, per expression, whether to evaluate
    LOCALLY (under the proxy rim — pure compute stays local, prims
    proxy individually) or to POST the expression REMOTE (/api/eval
    wholesale — one round trip for the whole thing).

    Rules (correctness first, then latency):
      1. (with-transaction …) → REMOTE, hard. A transaction holds
         (select-for-update …) locks across prims in its body; locks
         cannot span HTTP calls. Relaxing this corrupts the store.
      2. prim-call count above a configurable threshold → REMOTE, to
         avoid N prim round trips where one eval round trip suffices.
      3. zero prim calls → LOCAL (pure compute, no wire).
      4. otherwise → LOCAL (proxy prims individually; the common case
         for definitional/interactive work).

    The decision is visible: [placement] carries a human-readable
    reason so the user can see WHY an expr went remote (no silent
    boundary). The walker is conservative: it counts apparent prim
    calls by AST shape (App of Var name where name is not a known
    special form / local pure prim definition site). It does NOT eval,
    so a name that resolves to a lambda is still counted — that's the
    right bias for a latency heuristic (over-counting pushes remote,
    which is the safe direction). *)

open Pricklypear_lisp
open Ast

type placement = Local | Remote of string * string (* (where, reason) *)

let pp_placement = function
  | Local -> "local"
  | Remote (where, why) -> Printf.sprintf "remote(%s): %s" where why

(** Names that are NOT prim calls even though they appear as App head:
    special forms and local pure-primitive definitions. The placer
    receives the local pure prim names + the special-form set so it
    doesn't miscount a (define …) or a (lambda …) as a prim call. *)
type cfg = {
  threshold : int;           (* prim-call count above which → REMOTE *)
  local_prims : string list; (* names handled locally (pure prims) *)
  special_forms : string list;
}

let default_threshold = 3

let default_cfg ~local_prims =
  { threshold = default_threshold; local_prims;
    special_forms =
      [ "lambda"; "quote"; "if"; "define"; "set!"; "begin"; "let";
        "let*"; "letrec"; "cond"; "when"; "unless"; "and"; "or";
        "case"; "match"; "guard"; "with-transaction"; "delay"; "force" ] }

let is_special cfg name = List.mem name cfg.special_forms
let is_local_prim cfg name = List.mem name cfg.local_prims

(** Walk [e], returning (has_transaction, prim_call_count). A prim call
    is an [App (Var name, _)] where name is not a special form and not a
    known local pure prim — i.e. a name that would proxy. Local pure
    prims stay local by definition and don't count toward the
    proxy-round-trip axis. *)
let rec walk cfg (e : expr) : bool * int =
  match e with
  | Var _ | Lit _ -> (false, 0)
  | Quote _ -> (false, 0)
  | Define (_, body) -> walk cfg body
  | Lambda (_, body) -> walk cfg body
  | Let (_, bound, body) ->
      let hb, cb = walk cfg bound in
      let hb2, cb2 = walk cfg body in
      (hb || hb2, cb + cb2)
  | Letrec (binds, body) ->
      let hb, cb =
        List.fold_left
          (fun (h, c) (_, e) ->
            let h2, c2 = walk cfg e in (h || h2, c + c2))
          (false, 0) binds
      in
      let hb2, cb2 = walk cfg body in
      (hb || hb2, cb + cb2)
  | If (a, b, c) ->
      let h1, c1 = walk cfg a in
      let h2, c2 = walk cfg b in
      let h3, c3 = walk cfg c in
      (h1 || h2 || h3, c1 + c2 + c3)
  | Prim (name, args) ->
      (* Prim node: a prim call the reader already resolved. Treat
         like an App of Var name. with-transaction is usually an App
         (special form), but be safe. *)
      let body_ht, body_count =
        List.fold_left
          (fun (h, c) a ->
            let h2, c2 = walk cfg a in (h || h2, c + c2))
          (false, 0) args
      in
      let self_count =
        if is_special cfg name || is_local_prim cfg name then 0 else 1
      in
      let is_tx = (name = "with-transaction") in
      (is_tx || body_ht, body_count + self_count)
  | Seq (a, b) ->
      let ht, ca = walk cfg a in
      let ht2, cb = walk cfg b in
      (ht || ht2, ca + cb)
  | App (Var name, args) when name = "with-transaction" ->
      let ht, cb =
        List.fold_left
          (fun (h, c) a ->
            let h2, c2 = walk cfg a in (h || h2, c + c2))
          (true, 0) args
      in
      (ht, cb)
  | App (Var name, args) ->
      let body_ht, body_count =
        List.fold_left
          (fun (h, c) a ->
            let h2, c2 = walk cfg a in (h || h2, c + c2))
          (false, 0) args
      in
      let self_count =
        if is_special cfg name || is_local_prim cfg name then 0 else 1
      in
      (body_ht, body_count + self_count)
  | App (f, args) ->
      let htf, cf = walk cfg f in
      let ht, ca =
        List.fold_left
          (fun (h, c) a ->
            let h2, c2 = walk cfg a in (h || h2, c + c2))
          (false, 0) args
      in
      (htf || ht, cf + ca)

(** Decide placement for [src]. Parses with the shared reader; a parse
    error is REMOTE (let the server report it consistently with
    /api/eval). *)
let place ~(cfg : cfg) (src : string) : placement =
  match Reader.read_all src with
  | exception _ ->
      (* parse error: send to server; it reports the error shape *)
      Remote ("/api/eval", "parse error; let server report")
  | forms ->
      let has_tx, prim_count =
        List.fold_left
          (fun (h, c) f ->
            let h2, c2 = walk cfg f in (h || h2, c + c2))
          (false, 0) forms
      in
      if has_tx then
        Remote ("/api/eval", "with-transaction: locks cannot span HTTP")
      else if prim_count = 0 then Local
      else if prim_count > cfg.threshold then
        Remote
          ( "/api/eval",
            Printf.sprintf
              "prim-dense (%d > threshold %d): one eval round trip beats %d prim round trips"
              prim_count cfg.threshold prim_count )
      else Local
