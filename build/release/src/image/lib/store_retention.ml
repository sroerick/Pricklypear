(** Pricklypear Image — Store: retention worker (i-1788671083-c split).
    tree-time.gc policy rows + the partition-retention runner. *)
(* ---- tree-time.gc (5.3): retention worker -------------------------------
   Retention is habitat data + the jobs runner, composed on the existing
   gen-window partitions:
     - policy rows (tree_retention_policy, migration 022) declare per-path-
       class retention (code keeps all; data per-type, default keep-everything
       until a policy exists).
     - tree_versions_retain(path_class, keep_windows) (SQL, 022) computes the
       retention floor from the live head gen, ROLLS UP the surviving state of
       fully-retired paths to CHECKPOINT rows at the floor boundary (so as-of
       reads of ancient gens still resolve), then DROP PARTITIONs every lazy
       gen-window entirely below the floor (never the base window). Idempotent
       + conservative; on the real habitat it is gated by a scheduled job on
       the jobs runner (migration 015), not ad-hoc deletion.
   The first long pole is code paths (tiny); retention is deferred until a
   path class outgrows its welcome — measured via /timing-style storage
   accounting, not vibes. *) 

let retention_set_policy (t : Store_db.t) ~(path_class : string)
    ~(type_name : string option) ~(keep_windows : int64) : string =
  if path_class <> "code" && path_class <> "data" then
    "retention-policy-set: path_class must be code or data"
  else
    match Store_db.current_conn t with
    | None -> "retention-policy-set: no DB"
    | Some conn -> (
        try
          let tn =
            match type_name with
            | None -> Pgx.Value.null
            | Some s -> Pgx.Value.of_string s
          in
          ignore
            (Store_db.exec conn
               ~params:
                 [ Pgx.Value.of_string path_class; tn;
                   Pgx.Value.of_int64 keep_windows ]
               "INSERT INTO tree_retention_policy (path_class, type_name, keep_windows)
                VALUES ($1, $2, $3)
                ON CONFLICT (path_class, type_name)
                  DO UPDATE SET keep_windows = EXCLUDED.keep_windows"
              : Pgx.row list);
          match type_name with
          | None -> Printf.sprintf "retention policy %s: keep %Ld windows" path_class keep_windows
          | Some tnm ->
              Printf.sprintf "retention policy %s/%s: keep %Ld windows" path_class tnm keep_windows
        with ex ->
          (* Honest error-string result contract (i-1788671060-2 keep):
             the failure text IS the return value the prim displays. *)
          Printf.sprintf "retention-policy-set: %s" (Printexc.to_string ex))

let retention_delete_policy (t : Store_db.t) ~(path_class : string)
    ~(type_name : string option) : string =
  match Store_db.current_conn t with
  | None -> "retention-delete-policy: no DB"
  | Some conn -> (
      try
        let tn =
          match type_name with
          | None -> Pgx.Value.null
          | Some s -> Pgx.Value.of_string s
        in
        ignore
          (Store_db.exec conn
             ~params:[ Pgx.Value.of_string path_class; tn ]
             "DELETE FROM tree_retention_policy
              WHERE path_class = $1 AND type_name IS NOT DISTINCT FROM $2"
            : Pgx.row list);
        "retention policy deleted"
      with ex ->
        (* Honest error-string result contract (same class as above). *)
        Printf.sprintf "retention-delete-policy: %s" (Printexc.to_string ex))

let retention_list_policies (t : Store_db.t)
    : (string * string option * int64 * string) list =
  match Store_db.current_conn t with
  | None -> []
  | Some conn -> (
      try
        Store_db.exec conn
          "SELECT path_class, type_name, keep_windows,
                  created_at::text FROM tree_retention_policy
           ORDER BY path_class, type_name NULLS FIRST"
        |> List.map (fun row ->
               let cell i = List.nth row i in
               let pc =
                 match Pgx.Value.to_string (cell 0) with
                 | Some s -> s
                 | None -> ""
               in
               let tn =
                 match Pgx.Value.to_string (cell 1) with
                 | Some s -> Some s
                 | None -> None
               in
               let kw =
                 match Pgx.Value.to_int64 (cell 2) with
                 | Some k -> k
                 | None -> 0L
               in
               let ca =
                 match Pgx.Value.to_string (cell 3) with
                 | Some s -> s
                 | None -> ""
               in
               (pc, tn, kw, ca))
      with exn -> Store_db.reraise ~op:"retention_list_policies" exn)

(* Run the retention worker for each path class that has an explicit
   whole-class policy (type_name IS NULL). Whole-class policies are the v1
   gate: retention is deferred until a path class outgrows its welcome, and
   the global gen-window partitions mean the floor is whole-class regardless
   of any per-type rows (per-type rows are recorded for future finer-grained
   work but do not change the shared floor). keep_everything until a
   whole-class policy exists — the default — is a no-op, never ad-hoc
   deletion. Returns per-class summary rows (dropped, checkpointed). *) 
let retention_run (t : Store_db.t) ~(path_class : string option) ()
    : (string * int64 * int64) list =
  let all =
    match path_class with
    | Some pc -> [ pc ]
    | None -> [ "code"; "data" ]
  in
  match Store_db.current_conn t with
  | None -> []
  | Some conn -> (
      try
        List.concat_map
          (fun pc ->
            (* whole-class policy for this path class? *) 
            let pol =
              Store_db.exec conn
                ~params:[ Pgx.Value.of_string pc ]
                "SELECT keep_windows FROM tree_retention_policy
                 WHERE path_class = $1 AND type_name IS NULL"
            in
            match pol with
            | [] -> [] (* keep everything: no whole-class policy exists *) 
            | [ row ] -> (
                match Pgx.Value.to_int64 (List.nth row 0) with
                | None -> []
                | Some kw ->
                    let rows =
                      Store_db.exec conn
                        ~params:
                          [ Pgx.Value.of_string pc;
                            Pgx.Value.of_int64 kw ]
                        "SELECT dropped_windows, checkpointed_paths
                         FROM tree_versions_retain($1::text, $2::bigint)"
                    in
                    (match rows with
                     | [ r ] ->
                         let d =
                           match Pgx.Value.to_int64 (List.nth r 0) with
                           | Some x -> x
                           | None -> 0L
                         in
                         let c =
                           match Pgx.Value.to_int64 (List.nth r 1) with
                           | Some x -> x
                           | None -> 0L
                         in
                         [ (pc, d, c) ]
                     | _ -> []))
            | _ -> [])
          all
      with exn -> Store_db.reraise ~op:"retention_run" exn)

