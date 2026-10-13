(** Pricklypear Common — Boot manifest.

    Built from env (or defaults) at image boot — see [image.boot] and
    [scripts/dev.sh]. Tells the image which branch to load, which user
    it is running as, boot mode, and budgets/flags.

    In single-image v1, identity for HTTP comes from in-process auth
    (session cookie or Basic). The manifest is the process-level default
    claim used before a request attaches a user (and for the optional
    TCP REPL). *)
open Yojson.Safe

type boot_mode =
  | Boot_on_connect    (* image dies when session ends; default *)
  | Boot_always_on     (* image pinned running; survives SSH disconnects *)

let boot_mode_to_string = function
  | Boot_on_connect -> "on-connect"
  | Boot_always_on -> "always-on"

let boot_mode_of_string = function
  | "on-connect" -> Some Boot_on_connect
  | "always-on" -> Some Boot_always_on
  | _ -> None

type config = {
  step_budget_limit : int;       (* eval step budget per call, default 100000 *)
  memory_budget_mb : int;        (* deferred; recorded here *)
  feature_flags : (string * bool) list;
}

let default_config : config = {
  step_budget_limit = 100_000;
  memory_budget_mb = 64;
  feature_flags = [];
}

let config_to_json (c : config) : Yojson.Safe.t =
  `Assoc [
    ("step_budget_limit", `Int c.step_budget_limit);
    ("memory_budget_mb", `Int c.memory_budget_mb);
    ("feature_flags",
     `Assoc (List.map (fun (k, v) -> (k, `Bool v)) c.feature_flags));
  ]

let config_of_json (j : Yojson.Safe.t) : (config, string) result =
  try
    let assoc = match j with `Assoc a -> a | _ -> failwith "not assoc" in
    let step = match List.assoc_opt "step_budget_limit" assoc with
               | Some (`Int n) -> n | _ -> 100_000 in
    let mem = match List.assoc_opt "memory_budget_mb" assoc with
              | Some (`Int n) -> n | _ -> 64 in
    let flags = match List.assoc_opt "feature_flags" assoc with
                | Some (`Assoc items) ->
                    List.filter_map (fun (k, v) ->
                      match v with `Bool b -> Some (k, b) | _ -> None) items
                | _ -> [] in
    Ok { step_budget_limit = step; memory_budget_mb = mem; feature_flags = flags }
  with Failure msg -> Error msg

type t = {
  user_id : string;        (* process default identity; HTTP may re-attach per request *)
  username : string;       (* for the REPL prompt and author tracking *)
  (* spec_delegation (auth.delegation-tokens): when the session was carried
     by a delegated on-behalf-of token, [username] is the OWNER the session
     acts for and [actor] names the carrying agent. None = self (every
     non-delegated session). Provenance only — attribution/authorship stays
     the owner. *)
  actor : string option;
  branch_id : string;      (* default branch UUID, usually user/main *)
  branch_name : string;    (* qualified: "alice/main" *)
  boot_mode : boot_mode;
  image_version : string;  (* for A/B tracking *)
  config : config;
}

let make
    ~user_id ~username ~branch_id ~branch_name ~boot_mode ~image_version
    ?(actor : string option = None)
    ?(config = default_config) () : t =
  { user_id; username; actor; branch_id; branch_name; boot_mode; image_version; config }

(* Accessors for cross-package callers that don't have the record type. *)
let user_id (m : t) : string = m.user_id
let username (m : t) : string = m.username
let actor (m : t) : string option = m.actor
let branch_id (m : t) : string = m.branch_id
let branch_name (m : t) : string = m.branch_name

let to_json (m : t) : Yojson.Safe.t =
  `Assoc ([
    ("user_id", `String m.user_id);
    ("username", `String m.username);
  ] @ (match m.actor with
       | Some a -> [ ("actor", `String a) ]
       | None -> [])
    @ [
    ("branch_id", `String m.branch_id);
    ("branch_name", `String m.branch_name);
    ("boot_mode", `String (boot_mode_to_string m.boot_mode));
    ("image_version", `String m.image_version);
    ("config", config_to_json m.config);
  ])

let of_json (j : Yojson.Safe.t) : (t, string) result =
  try
    let assoc = match j with `Assoc a -> a | _ -> failwith "not assoc" in
    let user_id = match List.assoc "user_id" assoc with `String s -> s | _ -> failwith "user_id" in
    let username = match List.assoc "username" assoc with `String s -> s | _ -> failwith "username" in
    let branch_id = match List.assoc "branch_id" assoc with `String s -> s | _ -> failwith "branch_id" in
    let branch_name = match List.assoc "branch_name" assoc with `String s -> s | _ -> failwith "branch_name" in
    let boot_mode_str = match List.assoc "boot_mode" assoc with `String s -> s | _ -> failwith "boot_mode" in
    let boot_mode = match boot_mode_of_string boot_mode_str with
                    | Some m -> m | None -> failwith "bad boot_mode" in
    let actor =
      match List.assoc_opt "actor" assoc with
      | Some (`String a) -> Some a
      | _ -> None
    in
    let image_version = match List.assoc "image_version" assoc with `String s -> s | _ -> failwith "image_version" in
    let config = match List.assoc_opt "config" assoc with
                 | Some cj -> (match config_of_json cj with Ok c -> c | Error _ -> default_config)
                 | None -> default_config in
    Ok { user_id; username; actor; branch_id; branch_name; boot_mode; image_version; config }
  with Failure msg -> Error msg
