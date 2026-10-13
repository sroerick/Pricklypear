(** Static App-Var call graph for inspector / impact / DNU.
    Spec: image.inspector phases I1+; shared by system-browser + dnu-stubs. *)


module Ast = Pricklypear_lisp.Ast

let rec collect_var_apps (acc : string list) (e : Ast.expr) : string list =
  match e with
  | Ast.Var _ | Ast.Lit _ -> acc
  | Ast.App (Ast.Var f, args) ->
      List.fold_left collect_var_apps (f :: acc) args
  | Ast.App (f, args) ->
      List.fold_left collect_var_apps (collect_var_apps acc f) args
  | Ast.Let (_, e1, e2) -> collect_var_apps (collect_var_apps acc e1) e2
  | Ast.Letrec (binds, body) ->
      let acc =
        List.fold_left (fun a (_, e) -> collect_var_apps a e) acc binds
      in
      collect_var_apps acc body
  | Ast.If (c, t, e) ->
      collect_var_apps (collect_var_apps (collect_var_apps acc c) t) e
  | Ast.Lambda (_, body) -> collect_var_apps acc body
  | Ast.Prim (_, args) -> List.fold_left collect_var_apps acc args
  | Ast.Seq (a, b) -> collect_var_apps (collect_var_apps acc a) b
  | Ast.Quote _ -> acc
  | Ast.Define (_, body) -> collect_var_apps acc body

let uniq_stable (xs : string list) : string list =
  let seen = Hashtbl.create 16 in
  List.fold_left
    (fun acc x ->
      if Hashtbl.mem seen x then acc
      else begin
        Hashtbl.add seen x ();
        x :: acc
      end)
    [] xs
  |> List.rev

type snapshot = {
  (** name → body expr (visible function_defs) *)
  bodies : (string, Ast.expr) Hashtbl.t;
  (** callee → direct callers (App-Var heads; includes missing callees) *)
  callers_of : (string, string list) Hashtbl.t;
  (** name → direct callees that are stored function_defs *)
  callees_of : (string, string list) Hashtbl.t;
  (** route path → handler name *)
  routes : (string * string) list;
  (** prim + special-form names for holes() *)
  bound_kernel : (string, unit) Hashtbl.t;
}

(** Bound-kernel names for [bound_kernel] (holes()): the interpreter's
    single special-form list (i-1788671219). Previously a hand-copied
    18-name subset that had already drifted (missing delay/force/
    with-step/apply, macroexpand-1/macroexpand, gensym, the list HOFs);
    consuming Eval's list makes drift impossible. *)
let special_forms : string list = Pricklypear_lisp.Eval.special_form_names

let build_snapshot
    ~(defs : (string * Ast.expr) list)
    ~(routes : (string * string) list)
    ~(prim_names : string list)
    : snapshot =
  let bodies = Hashtbl.create (List.length defs + 8) in
  List.iter (fun (n, e) -> Hashtbl.replace bodies n e) defs;
  let callers_of : (string, string list) Hashtbl.t = Hashtbl.create 64 in
  let callees_of : (string, string list) Hashtbl.t = Hashtbl.create 64 in
  let add_caller ~callee ~caller =
    let prev = try Hashtbl.find callers_of callee with Not_found -> [] in
    if List.mem caller prev then ()
    else Hashtbl.replace callers_of callee (caller :: prev)
  in
  List.iter
    (fun (name, expr) ->
      let apps = uniq_stable (collect_var_apps [] expr) in
      let fn_callees =
        List.filter
          (fun c -> c <> name && Hashtbl.mem bodies c)
          apps
      in
      Hashtbl.replace callees_of name fn_callees;
      List.iter (fun c -> add_caller ~callee:c ~caller:name) apps)
    defs;
  let bound_kernel = Hashtbl.create 64 in
  List.iter (fun n -> Hashtbl.replace bound_kernel n ()) special_forms;
  List.iter (fun n -> Hashtbl.replace bound_kernel n ()) prim_names;
  { bodies; callers_of; callees_of; routes; bound_kernel }

let direct_callees (snap : snapshot) (name : string) : string list =
  try Hashtbl.find snap.callees_of name with Not_found -> []

let direct_callers (snap : snapshot) (name : string) : string list =
  try Hashtbl.find snap.callers_of name with Not_found -> []

(** Strongly connected components of size > 1 (mutual recursion groups),
    plus trivial self-loops already covered by recursion.analyze_self. *)
let mutual_cycles (snap : snapshot) : string list list =
  let index = ref 0 in
  let stack : string list ref = ref [] in
  let on_stack = Hashtbl.create 32 in
  let indices = Hashtbl.create 32 in
  let lowlink = Hashtbl.create 32 in
  let comps : string list list ref = ref [] in
  let rec strongconnect v =
    Hashtbl.replace indices v !index;
    Hashtbl.replace lowlink v !index;
    incr index;
    stack := v :: !stack;
    Hashtbl.replace on_stack v true;
    List.iter
      (fun w ->
        if not (Hashtbl.mem indices w) then begin
          strongconnect w;
          Hashtbl.replace lowlink v
            (min (Hashtbl.find lowlink v) (Hashtbl.find lowlink w))
        end else if Hashtbl.mem on_stack w && Hashtbl.find on_stack w then
          Hashtbl.replace lowlink v
            (min (Hashtbl.find lowlink v) (Hashtbl.find indices w)))
      (direct_callees snap v);
    if Hashtbl.find lowlink v = Hashtbl.find indices v then begin
      let rec pop acc =
        match !stack with
        | [] -> acc
        | x :: xs ->
            stack := xs;
            Hashtbl.replace on_stack x false;
            if x = v then x :: acc else pop (x :: acc)
      in
      let comp = pop [] in
      if List.length comp > 1 then comps := comp :: !comps
    end
  in
  Hashtbl.iter
    (fun name _ -> if not (Hashtbl.mem indices name) then strongconnect name)
    snap.bodies;
  !comps

let mutual_of (snap : snapshot) (name : string) : string list =
  match
    List.find_opt (fun comp -> List.mem name comp) (mutual_cycles snap)
  with
  | None -> []
  | Some comp -> List.filter (( <> ) name) comp

(** Forward BFS over stored-function edges. [include_root] keeps [name]. *)
let walk_forward (snap : snapshot) ~(name : string) ~(depth : int)
    ~(include_root : bool) : string list =
  let max_d = max 0 depth in
  let seen = Hashtbl.create 32 in
  let rec bfs q acc =
    match q with
    | [] -> List.rev acc
    | (n, d) :: rest ->
        if d > max_d || Hashtbl.mem seen n then bfs rest acc
        else begin
          Hashtbl.add seen n ();
          let acc =
            if (not include_root) && n = name then acc else n :: acc
          in
          if d >= max_d then bfs rest acc
          else
            let next =
              List.filter_map
                (fun c ->
                  if Hashtbl.mem seen c then None else Some (c, d + 1))
                (direct_callees snap n)
            in
            bfs (rest @ next) acc
        end
  in
  bfs [ (name, 0) ] []

(** Reverse BFS (dependents). Excludes [name] itself. *)
let dependents (snap : snapshot) ~(name : string) ~(depth : int)
    : string list =
  let max_d = max 0 depth in
  let seen = Hashtbl.create 32 in
  Hashtbl.add seen name ();
  let rec bfs q acc =
    match q with
    | [] -> List.rev acc
    | (n, d) :: rest ->
        if d >= max_d then bfs rest acc
        else
          let callers = direct_callers snap n in
          let rec fold cs q acc =
            match cs with
            | [] -> bfs q acc
            | c :: tl ->
                if Hashtbl.mem seen c then fold tl q acc
                else begin
                  Hashtbl.add seen c ();
                  fold tl (q @ [ (c, d + 1) ]) (c :: acc)
                end
          in
          fold callers rest acc
  in
  bfs [ (name, 0) ] []

let callees (snap : snapshot) ~(name : string) ~(depth : int) : string list =
  if depth <= 1 then direct_callees snap name
  else
    (* depth>1: walk but drop root *)
    List.filter (( <> ) name) (walk_forward snap ~name ~depth ~include_root:true)

let callers = direct_callers

let used_by_routes (snap : snapshot) (name : string) : string list =
  let deps = dependents snap ~name ~depth:8 in
  let is_hit h = h = name || List.mem h deps in
  snap.routes
  |> List.filter_map (fun (path, handler) ->
         if is_hit handler then Some path else None)
  |> uniq_stable

let holes (snap : snapshot) (name : string) : string list =
  match Hashtbl.find_opt snap.bodies name with
  | None -> []
  | Some expr ->
      let apps = uniq_stable (collect_var_apps [] expr) in
      List.filter
        (fun c ->
          (not (Hashtbl.mem snap.bodies c))
          && not (Hashtbl.mem snap.bound_kernel c))
        apps
(* branch-ops push (branching.borg branch-ops table): the PUSH dependency
   closure — the holes() analytic (App-Var heads) pointed at a push set over
   two branch views. [src_defs] = effective (chain-walked, closest-wins)
   defs of the SOURCE; [tgt_visible] = names the TARGET chain already
   binds; [kernel] = the caller's prim catalog. For each requested name, a
   referenced name the target does not bind is AUTO-INCLUDED when the
   source defines it (recursively; mutual recursion safe), recorded as a
   HOLE when nothing defines it — all-or-nothing: any hole refuses the
   whole push, with a complete report (every holed name × its exact
   missing refs) so one fix can land it (2026-10-05 incident:
   pulse-row-from promoted without workstreams/stale-node? ->
   system/main 500). Analytic limits are holes(): a function passed as a
   VALUE is not visible to the scan; the closure travels the same
   aperture, so what lands is fully covered by what it scans. Returns
   Ok (requested names, auto-included names), each in discovery order. *)
let push_closure ~(requested : string list)
    ~(src_defs : (string * Ast.expr) list)
    ~(tgt_visible : string list)
    ~(kernel : string list)
    : (string list * string list, (string * string list) list) result =
  let src_bodies : (string, Ast.expr) Hashtbl.t = Hashtbl.create 64 in
  List.iter (fun (n, e) -> Hashtbl.replace src_bodies n e) src_defs;
  let bound : (string, unit) Hashtbl.t = Hashtbl.create 128 in
  List.iter (fun n -> Hashtbl.replace bound n ()) kernel;
  List.iter (fun n -> Hashtbl.replace bound n ()) special_forms;
  List.iter (fun n -> Hashtbl.replace bound n ()) tgt_visible;
  let seen : (string, unit) Hashtbl.t = Hashtbl.create 64 in
  let holes : (string * string list) list ref = ref [] in
  let auto : string list ref = ref [] in
  let rec add (name : string) ~(is_auto : bool) : unit =
    if not (Hashtbl.mem seen name) then
      match Hashtbl.find_opt src_bodies name with
      | None ->
          (* requested, but nothing on the source chain defines it *)
          holes := (name, []) :: !holes
      | Some body ->
          Hashtbl.replace seen name ();
          if is_auto then auto := name :: !auto;
          let refs = uniq_stable (collect_var_apps [] body) in
          let miss = ref [] in
          List.iter
            (fun r ->
               if Hashtbl.mem bound r || Hashtbl.mem seen r then ()
               else if Hashtbl.mem src_bodies r then add r ~is_auto:true
               else miss := r :: !miss)
            refs;
          if !miss <> [] then holes := (name, List.rev !miss) :: !holes
  in
  let requested_names = uniq_stable requested in
  List.iter (fun n -> add n ~is_auto:false) requested_names;
  if !holes <> [] then Error (List.rev !holes)
  else
    Ok
      ( requested_names
      , (* a dep also requested by name stays in [pushed], never auto *)
        List.filter
          (fun n -> not (List.mem n requested_names))
          (List.rev !auto) )

(** Load defs + routes from session store and build snapshot. *)
let snapshot_of_session (session : Session_types.t) : snapshot =
  let defs = Store.list_function_names session.store in
  (* Opaque-body contract (i-1788671083-c): parse at the eval boundary;
     a malformed row is skipped, as the store's old decode guard did. *)
  let bodies =
    List.filter_map
      (fun (n, body, _) ->
        match Pricklypear_lisp.Ast.expr_of_body_json body with
        | Some e -> Some (n, e)
        | None -> None)
      defs
  in
  let routes =
    Hashtbl.fold
      (fun path (handler, _) acc -> (path, handler) :: acc)
      session.routes []
  in
  let prim_names = List.map fst session.kernel_catalog in
  (build_snapshot ~defs:bodies ~routes ~prim_names)

let take n xs =
  let rec go i = function
    | [] -> []
    | _ when i <= 0 -> []
    | x :: rest -> x :: go (i - 1) rest
  in
  go n xs

(** After successful function_def write: impact queue + change transcript. *)
let record_define_side_effects (t : Session_types.t) ~(name : string)
    : unit =
  if t.ui_shell.suppress_refresh_on_define || t.manifest.username = "anonymous" then
    ()
  else
    let snap = snapshot_of_session t in
    let deps = dependents snap ~name ~depth:8 in
    let routes = used_by_routes snap name in
    let now = Unix.gettimeofday () in
    let branch = t.manifest.branch_name in
    let author = t.manifest.username in
    let ch : Session_types.change_event =
      { op = "define"; name; at = now; branch; author }
    in
    t.change_transcript <- take 64 (ch :: t.change_transcript);
    if deps = [] && routes = [] then ()
    else begin
      let ev : Session_types.impact_event =
        { name; at = now; branch; author; dependents = deps;
          routes; status = "pending" }
      in
      let rest =
        List.filter
          (fun (e : Session_types.impact_event) -> e.name <> name)
          t.impact_queue
      in
      t.impact_queue <- take 32 (ev :: rest);
      ()
    end

let impact_summary_text (t : Session_types.t) (name : string) : string =
  match
    List.find_opt
      (fun (e : Session_types.impact_event) -> e.name = name)
      t.impact_queue
  with
  | None -> "impact: no routes; eval dependents or smoke parents manually"
  | Some e ->
      let deps =
        if e.dependents = [] then "(none)"
        else String.concat "," e.dependents
      in
      let rts =
        if e.routes = [] then "(none)"
        else String.concat " " e.routes
      in
      Printf.sprintf "impact: dependents=%s; smoke routes: %s" deps rts

let page_function_graph (session : Session_types.t) ~(handler : string)
    : (string * string) list =
  let snap = snapshot_of_session session in
  let max_depth = 8 in
  let rec walk depth seen name role acc =
    if depth > max_depth || List.mem name seen then acc
    else
      match Hashtbl.find_opt snap.bodies name with
      | None -> ((name, role ^ " (missing)") :: acc)
      | Some _ ->
          let acc = (name, role) :: acc in
          let seen = name :: seen in
          let cs = direct_callees snap name in
          let rec fold_c cs acc =
            match cs with
            | [] -> acc
            | c :: rest ->
                let acc = walk (depth + 1) seen c "callee" acc in
                fold_c rest acc
          in
          fold_c cs acc
  in
  let rows = walk 0 [] handler "handler" [] in
  (List.rev rows)
