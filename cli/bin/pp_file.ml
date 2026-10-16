(** pp-file — the file-bridge transport (branching.file-bridge P2 client
    half, spec commit 3f4d6d1). A local projection file is a manifest of
    defs + raw spans; materialize writes a branch's own defs into it, push
    lands edits back through (branch/ingest ...) in ONE eval call.

      pp-file materialize <ref> <file> [--names a,b,c]
      pp-file push <ref> <file> [--force] [--no-prune] [--dry-run]

    Sidecar <file>.fb.json records the materialize coordinate — head_gen,
    per-name pin hashes, and the managed def set — so push's ops carry the
    base_hash the ingest conflict rule needs (diverged = the target moved
    off the recorded base since the file was authored). After a successful
    push the sidecar re-bases to the branch's current hashes, so the next
    clean push is zero-ops. *)

open Cmdliner
module FP = Pricklypear_lisp.File_projection

let env_url = Pp_client.getenv_default "PP_URL" "http://127.0.0.1:18080"

let env_user = Pp_client.getenv_default "PP_USER" "admin"

let env_pass = Option.value (Pp_client.getenv "PP_PASS") ~default:""

let eval (expr : string) : (string, string) result =
  let r =
    Pp_client.eval_expr ~base_url:env_url ~user:env_user ~password:env_pass
      ~expr ~timeout_s:120. ~quiet:true
  in
  match r.ok with
  | false -> Error (Option.value r.error ~default:"eval failed")
  | true -> Ok (Pp_client.unwrap_lisp_string (Option.value r.value ~default:""))

let read_all (path : string) : (string, string) result =
  try Ok (In_channel.with_open_text path In_channel.input_all)
  with Sys_error e -> Error e

(* ---- sidecar: the materialize coordinate ---- *)

let sidecar_path (file : string) : string = file ^ ".fb.json"

type sidecar = {
  s_branch : string;
  s_head_gen : Int64.t;
  s_hashes : (string * string) list; (* name -> pin hash at materialize *)
  s_defs : string list;              (* the managed def set *)
}

let sidecar_to_json (s : sidecar) : Yojson.Safe.t =
  `Assoc
    [ ("branch", `String s.s_branch)
    ; ("head_gen", `Int (Int64.to_int s.s_head_gen))
    ; ("hashes", `Assoc (List.map (fun (n, h) -> (n, `String h)) s.s_hashes))
    ; ("defs", `List (List.map (fun n -> `String n) s.s_defs)) ]

let write_sidecar (file : string) (s : sidecar) : unit =
  let oc = open_out (sidecar_path file) in
  output_string oc (Yojson.Safe.to_string (sidecar_to_json s) ^ "\n");
  close_out oc

let read_sidecar (file : string) : (sidecar option, string) result =
  match read_all (sidecar_path file) with
  | Error _ -> Ok None
  | Ok text -> (
      try
        let j = Yojson.Safe.from_string text in
        let open Yojson.Safe.Util in
        let branch = j |> member "branch" |> to_string in
        let head_gen =
          Int64.of_int (j |> member "head_gen" |> to_int) in
        let hashes =
          (match j |> member "hashes" with `Assoc kvs -> kvs | _ -> [])
          |> List.filter_map (fun kv ->
                 match kv with
                 | (n, `String h) -> Some (n, h)
                 | _ -> None)
        in
        let defs =
          j |> member "defs" |> to_list
          |> List.filter_map (function `String s -> Some s | _ -> None)
        in
        Ok
          (Some
             {
               s_branch = branch;
               s_head_gen = head_gen;
               s_hashes = hashes;
               s_defs = defs;
             })
      with e -> Error (Printexc.to_string e))

(* ---- the branch's own defs (the branch/defs read prim) ---- *)

type live_def = { l_name : string; l_body : string; l_hash : string }

let fetch_defs (ref : string) : ((Int64.t * live_def list), string) result =
  match eval (Printf.sprintf "(branch/defs %S)" ref) with
  | Error e -> Error e
  | Ok raw -> (
      try
        let j = Yojson.Safe.from_string raw in
        let open Yojson.Safe.Util in
        let head_gen =
          Int64.of_int (j |> member "head_gen" |> to_int) in
        let defs =
          j |> member "defs" |> to_list
          |> List.filter_map (fun d ->
                 Some
                   {
                     l_name = d |> member "name" |> to_string;
                     l_body = d |> member "body" |> to_string;
                     l_hash = d |> member "hash" |> to_string;
                   })
        in
        Ok (head_gen, defs)
      with e -> Error (Printexc.to_string e))

(* body jsonb -> define source: the canonical round-trip through
   parse_top_define. Lambda bodies (define-sugar fns) rebuild as
   (define (name params...) body); plain values as (define name body). *)
let define_source (name : string) (body_json : string) : string =
  match Pricklypear_lisp.Ast.expr_of_body_json body_json with
  | Some (Pricklypear_lisp.Ast.Lambda (params, body)) ->
      Printf.sprintf "(define (%s %s)\n  %s)\n" name
        (String.concat " " params)
        (Pricklypear_lisp.Reader.pp_expr body)
  | Some e ->
      Printf.sprintf "(define %s\n  %s)\n" name
        (Pricklypear_lisp.Reader.pp_expr e)
  | None -> Printf.sprintf ";; UNPARSABLE BODY for %s\n" name

(* ---- materialize ---- *)

let materialize (ref : string) (file : string) (names_opt : string option) :
    int =
  match fetch_defs ref with
  | Error e -> prerr_endline ("error: " ^ e); 1
  | Ok (head_gen, all_defs) -> (
      let entries =
        match names_opt with
        | None -> all_defs
        | Some csv ->
            let ns = List.map String.trim (String.split_on_char ',' csv) in
            List.filter (fun (d : live_def) -> List.mem d.l_name ns) all_defs
      in
      match entries with
      | [] -> prerr_endline "no defs matched"; 1
      | _ ->
          let defs =
            List.map
              (fun (d : live_def) -> (d.l_name, define_source d.l_name d.l_body))
              entries
          in
          let p = FP.of_defs ~branch:ref ~base_gen:head_gen defs in
          let oc = open_out file in
          output_string oc (FP.materialize p);
          close_out oc;
          write_sidecar file
            {
              s_branch = ref;
              s_head_gen = head_gen;
              s_hashes =
                List.map (fun (d : live_def) -> (d.l_name, d.l_hash)) entries;
              s_defs = List.map (fun (d : live_def) -> d.l_name) entries;
            };
          Printf.printf "materialized %d defs from %s -> %s (head gen %Ld)\n"
            (List.length entries) ref file head_gen;
          0)

(* ---- push ---- *)

let push (ref : string) (file : string) (force : bool) (no_prune : bool)
    (dry_run : bool) : int =
  match read_all file with
  | Error e -> prerr_endline ("error: " ^ e); 1
  | Ok text -> (
      match FP.parse text with
      | Error e -> prerr_endline ("parse failed: " ^ e); 1
      | Ok proj -> (
          match read_sidecar file with
          | Error e -> prerr_endline ("sidecar: " ^ e); 1
          | Ok sc_opt -> (
              let sc = sc_opt in
              (match sc with
               | Some s when s.s_branch <> "" && not (String.equal s.s_branch ref) ->
                   Printf.printf "warning: sidecar materialized from %s, pushing to %s\n"
                     s.s_branch ref
               | _ -> ());
              let base_gen = match sc with Some s -> s.s_head_gen | None -> 0L in
              let hash_of (name : string) : string option =
                match sc with
                | Some s -> (match List.assoc_opt name s.s_hashes with Some h -> Some h | None -> None)
                | None -> None
              in
              let file_defs = proj.FP.p_defs in
              let file_names =
                List.map
                  (fun (d : FP.def_slot) -> d.FP.d_name)
                  file_defs
              in
              let define_ops =
                List.filter_map
                  (fun (d : FP.def_slot) ->
                     match Pp_sync_diff.parse_bodies d.FP.d_body with
                     | Some [ (name, body_json) ]
                       when String.equal name d.FP.d_name ->
                         Some
                           (`Assoc
                             ( [ ("op", `String "define")
                               ; ("name", `String name)
                               ; ("body", `String body_json) ]
                             @ (match hash_of name with
                                | Some h -> [ ("base_hash", `String h) ]
                                | None -> []) ))
                     | _ -> None)
                  file_defs
              in
              let pruned_names =
                if no_prune then []
                else
                  match sc with
                  | None -> []
                  | Some s ->
                      List.filter (fun n -> not (List.mem n file_names)) s.s_defs
              in
              let unbind_ops =
                List.map
                  (fun n ->
                     `Assoc
                       ( [ ("op", `String "unbind"); ("name", `String n) ]
                       @ (match hash_of n with
                          | Some h -> [ ("base_hash", `String h) ]
                          | None -> []) ))
                  pruned_names
              in
              let all_ops = define_ops @ unbind_ops in
              let ops_json = Yojson.Safe.to_string (`List all_ops) in
              let n_def = List.length define_ops in
              let n_unbind = List.length unbind_ops in
              if dry_run then begin
                Printf.printf
                  "would push %d defines, %d unbinds to %s (base gen %Ld)\n%s\n"
                  n_def n_unbind ref base_gen ops_json;
                0
              end
              else if all_ops = [] then begin
                Printf.printf "nothing to push (%s matches %s)\n" file ref;
                0
              end
              else begin
                match
                  eval
                    (Printf.sprintf "(branch/ingest %S %S %Ld %S%s)" ref file
                       base_gen ops_json
                       (if force then " 'force" else ""))
                with
                | Error e -> prerr_endline ("error: " ^ e); 1
                | Ok raw -> (
                    try
                      let j = Yojson.Safe.from_string raw in
                      let open Yojson.Safe.Util in
                      let landed =
                        j |> member "landed" |> to_list
                        |> List.filter_map (function
                             | `String s -> Some s
                             | _ -> None) in
                      let skipped = j |> member "skipped" |> to_list in
                      List.iter (fun n -> Printf.printf "landed: %s\n" n) landed;
                      List.iter
                        (fun s ->
                           match s with
                           | `Assoc kv -> (
                               let name =
                                 match List.assoc_opt "name" kv with
                                 | Some (`String n) -> n
                                 | _ -> "?" in
                               let reason =
                                 match List.assoc_opt "reason" kv with
                                 | Some (`String r) -> r
                                 | _ -> "" in
                               Printf.printf "skipped: %s (%s)\n" name reason)
                           | _ -> ())
                        skipped;
                      (* re-base the sidecar so the next push is zero-ops:
                         managed set = the file's defs, hashes re-read *)
                      (match fetch_defs ref with
                       | Ok (head_gen, all_defs) ->
                           write_sidecar file
                             {
                               s_branch = ref;
                               s_head_gen = head_gen;
                               s_hashes =
                                 List.map
                                   (fun (d : live_def) -> (d.l_name, d.l_hash))
                                   all_defs;
                               s_defs = file_names;
                             }
                       | Error e ->
                           Printf.printf "note: sidecar refresh failed: %s\n" e);
                      0
                    with e -> prerr_endline (Printexc.to_string e); 1) end)))
(* ---- cmdliner surface ---- *)

let ref_arg =
  Arg.(
    value & pos 0 (some string) None & info [] ~docv:"REF"
    ~doc:"branch ref, e.g. admin/dev")

let file_arg =
  Arg.(
    value & pos 1 (some string) None & info [] ~docv:"FILE"
    ~doc:"projection file")

let materialize_cmd =
  let doc = "write a branch's own defs into a projection file" in
  let names_arg =
    Arg.(
      value & opt (some string) None & info [ "names" ] ~docv:"NAMES"
      ~doc:"comma-separated def names to include")
  in
  let run r f n =
    match (r, f) with
    | Some r, Some f -> materialize r f n
    | _ ->
        prerr_endline "usage: pp-file materialize <ref> <file>";
        2
  in
  Cmd.v (Cmd.info "materialize" ~doc)
    Term.(const run $ ref_arg $ file_arg $ names_arg)

let push_cmd =
  let doc = "land projection-file edits through branch/ingest" in
  let force =
    Arg.(
      value & flag & info [ "force" ]
      ~doc:"land even over diverged target pins")
  in
  let no_prune =
    Arg.(
      value & flag & info [ "no-prune" ]
      ~doc:"do not unbind defs removed from the file")
  in
  let dry_run =
    Arg.(
      value & flag & info [ "dry-run" ]
      ~doc:"print the op batch without evaling")
  in
  let run r f force no_prune dry_run =
    match (r, f) with
    | Some r, Some f -> push r f force no_prune dry_run
    | _ ->
        prerr_endline "usage: pp-file push <ref> <file>";
        2
  in
  Cmd.v (Cmd.info "push" ~doc)
    Term.(const run $ ref_arg $ file_arg $ force $ no_prune $ dry_run)

let default_cmd =
  Cmd.group (Cmd.info "pp-file" ~version:"0.1") [ materialize_cmd; push_cmd ]

let () = exit (Cmd.eval' default_cmd)
