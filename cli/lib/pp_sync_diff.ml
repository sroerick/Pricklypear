(** pp-sync vault drift: canonical BODY-level comparison (card 8dff7c9e,
    borge/branching.borg vault-authority Option 1).

    The vault (libs/<pkg>/lib.pp) is a generated mirror of system/main,
    not an authoring source. But `pp-sync status` historically compared
    only NAME SETS, so a name present both places looked healthy even
    when the live body differed from the vault body — the exact
    "fix committed in git but never deployed" case (the additive boot
    seed never overwrites an existing name). These pure helpers parse
    both sides with the real reader and compare canonical AST bodies.
*)

(* Parse top-level defines into (name, canonical-body-json). The
   canonical form is [Ast.expr_to_body_json] — the same codec the store
   persists — so equality is AST-level: whitespace, comments, and the
   docstring (metadata, not body) do not affect it. [None] when the
   source does not parse, so a caller can be loud rather than report a
   confidently wrong diff. *)
let parse_bodies (text : string) : (string * string) list option =
  let body_of e =
    match Pricklypear_lisp.Eval.parse_top_define e with
    | Some { Pricklypear_lisp.Eval.name; body; _ } ->
        Some (name, Pricklypear_lisp.Ast.expr_to_body_json body)
    | None -> None
  in
  try
    let forms = Pricklypear_lisp.Reader.read_all text in
    Some (List.filter_map body_of forms)
  with
  | Pricklypear_lisp.Reader.Reader_error _ -> None
  | Pricklypear_lisp.Ast.Interp_error _ -> None
  | _ -> None

(* Bodies from a live (export-package …) text; same codec as the local
   side. [] on parse failure (the caller still has the name-set diff). *)
let bodies_of_export (text : string) : (string * string) list =
  match parse_bodies text with Some bs -> bs | None -> []

(* (changed, only_local, only_live). [changed] is (name, local_body,
   live_body) for names present on BOTH sides whose canonical bodies
   differ. Names sorted for stable output. *)
let divergence ~(local : (string * string) list)
    ~(live : (string * string) list) =
  let lmap = List.to_seq local |> Hashtbl.of_seq in
  let vmap = List.to_seq live |> Hashtbl.of_seq in
  let all_names =
    List.sort_uniq String.compare
      (List.map fst local @ List.map fst live)
  in
  let changed = ref [] and only_local = ref [] and only_live = ref [] in
  List.iter
    (fun n ->
      match (Hashtbl.find_opt lmap n, Hashtbl.find_opt vmap n) with
      | Some lb, Some vb -> if lb <> vb then changed := (n, lb, vb) :: !changed
      | Some _, None -> only_local := n :: !only_local
      | None, Some _ -> only_live := n :: !only_live
      | None, None -> ())
    all_names;
  (List.rev !changed, List.rev !only_local, List.rev !only_live)

(* Pretty-print a canonical body JSON back to one line of source for the
   human diff; the raw JSON when the body cannot be decoded. *)
let pp_body (body : string) : string =
  let one_line s =
    let b = Buffer.create (String.length s) in
    let pending_space = ref false in
    String.iter
      (fun c ->
        match c with
        | '\n' | '\r' | '\t' | ' ' ->
            if Buffer.length b > 0 then pending_space := true
        | _ ->
            if !pending_space then begin
              Buffer.add_char b ' ';
              pending_space := false
            end;
            Buffer.add_char b c)
      s;
    Buffer.contents b
  in
  match Pricklypear_lisp.Ast.expr_of_body_json body with
  | Some e -> one_line (Pricklypear_lisp.Reader.pp_expr e)
  | None -> one_line body
