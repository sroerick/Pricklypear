(** Shared HTTP client for live-image /api/eval.

    Uses [curl] for HTTP and HTTPS (OpenBSD + laptop). No Python. *)

type eval_result = {
  ok : bool;
  value : string option;
  error : string option;
  raw : string;
}

let getenv name = Sys.getenv_opt name

let getenv_default name default =
  match getenv name with Some v when v <> "" -> v | _ -> default

let read_password_file path =
  let ic = open_in path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () -> String.trim (input_line ic))

let resolve_password ~password ~password_file =
  if password <> "" then password
  else if password_file <> "" then read_password_file password_file
  else
    match getenv "PP_PASS_FILE" with
    | Some p when p <> "" -> read_password_file p
    | _ -> getenv_default "PP_PASS" ""

let eval_url base =
  let base = String.trim base in
  let base =
    if base = "" then "http://127.0.0.1:18080"
    else if base.[String.length base - 1] = '/' then
      String.sub base 0 (String.length base - 1)
    else base
  in
  base ^ "/api/eval"

(** Run curl; return stdout body (and stderr on failure message).

    Auth: Basic user:password by default; a non-empty PP_TOKEN takes
    over when no password is configured (the delegated-token identity
    — the agent — has no Basic password). An explicit password always
    wins, so habitat scripts that set both keep their behavior. *)
let curl_post ~url ~user ~password ~body ~timeout_s : (string, string) Result.t =
  let timeout = max 1 (int_of_float (Float.ceil timeout_s)) in
  let auth_args =
    match getenv "PP_TOKEN" with
    | Some t when password = "" && String.trim t <> "" ->
        [| "-H"; "Authorization: Bearer " ^ String.trim t |]
    | _ -> [| "-u"; user ^ ":" ^ password |]
  in
  let args =
    Array.append
      [| "curl"; "-sS" |]
      (Array.append auth_args
         [| "-H";
            "Content-Type: application/json";
            "-H";
            "Accept: application/json";
            "--max-time";
            string_of_int timeout;
            "-d";
            body;
            url;
         |])
  in
  (* Prefer open_process_args when available (no shell). *)
  let stdout_path, stderr_path =
    let tmp = Filename.temp_file "pp-eval" "" in
    (tmp ^ ".out", tmp ^ ".err")
  in
  let cleanup () =
    (try Sys.remove stdout_path with _ -> ());
    (try Sys.remove stderr_path with _ -> ())
  in
  Fun.protect
    ~finally:cleanup
    (fun () ->
      let oc_out = open_out_bin stdout_path in
      let oc_err = open_out_bin stderr_path in
      let pid =
        Unix.create_process "curl" args Unix.stdin
          (Unix.descr_of_out_channel oc_out)
          (Unix.descr_of_out_channel oc_err)
      in
      close_out oc_out;
      close_out oc_err;
      let _, status = Unix.waitpid [] pid in
      let read_file p =
        if not (Sys.file_exists p) then ""
        else
          let ic = open_in_bin p in
          Fun.protect
            ~finally:(fun () -> close_in_noerr ic)
            (fun () -> really_input_string ic (in_channel_length ic))
      in
      let stdout = read_file stdout_path in
      let stderr = read_file stderr_path in
      match status with
      | Unix.WEXITED 0 -> Ok stdout
      | Unix.WEXITED n ->
          Error
            (Printf.sprintf "curl exit %d: %s%s" n
               (if stderr = "" then "" else stderr ^ " ")
               (if String.length stdout > 200 then
                  String.sub stdout 0 200
                else stdout))
      | Unix.WSIGNALED s -> Error (Printf.sprintf "curl signaled %d" s)
      | Unix.WSTOPPED s -> Error (Printf.sprintf "curl stopped %d" s))

let parse_eval_json (raw : string) : eval_result =
  try
    let j = Yojson.Safe.from_string raw in
    let open Yojson.Safe.Util in
    let ok =
      match j |> member "ok" with
      | `Bool b -> b
      | _ -> false
    in
    let value =
      match j |> member "value" with
      | `Null | `Assoc [] -> None
      | `String s -> Some s
      | other -> Some (Yojson.Safe.to_string other)
    in
    let error =
      match j |> member "error" with
      | `String s -> Some s
      | `Null -> None
      | other -> Some (Yojson.Safe.to_string other)
    in
    { ok; value; error; raw }
  with exn ->
    {
      ok = false;
      value = None;
      error = Some (Printf.sprintf "bad JSON: %s (%s)" (Printexc.to_string exn) raw);
      raw;
    }

let eval_expr ~base_url ~user ~password ~expr ~timeout_s : eval_result =
  let url = eval_url base_url in
  let body =
    Yojson.Safe.to_string
      (`Assoc [ ("expr", `String expr) ])
  in
  match curl_post ~url ~user ~password ~body ~timeout_s with
  | Error e -> { ok = false; value = None; error = Some e; raw = "" }
  | Ok raw -> parse_eval_json raw

(** Strip outer Lisp/pp_value quotes from a VStr print. *)
let unwrap_lisp_string (v : string) : string =
  let v = String.trim v in
  if String.length v >= 2 && v.[0] = '"' && v.[String.length v - 1] = '"' then
    try Yojson.Safe.Util.to_string (Yojson.Safe.from_string v)
    with _ -> String.sub v 1 (String.length v - 2)
  else v
