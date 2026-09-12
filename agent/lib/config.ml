(** Client-side LLM config for glochid (borge/agent.borg llm-config).

    One logical record ({api_key, base_url, model}, all optional), two
    physical locations:

      - [~/.config/agent/config.json] — the agent's own config home
        (XDG_CONFIG_HOME honored); the /settings surface writes here.

      - [<lens>/__config.json] — written by pp-remote --sync from the
        image's sync round metadata (auth.user-llm-keys 5b). Booting
        with --lens DIR picks up the key + base the habitat handed down
        invisibly through the sync exchange; a locally chosen model in
        the same file survives sync rounds (model is a per-surface
        dial, never replicated).

    Resolution ORDER at boot (first non-empty wins, per field):
    --flag > env (AGENT_API_KEY/AGENT_BASE_URL/AGENT_MODEL) > lens
    __config.json > ~/.config/agent/config.json. *)

type t = {
  api_key : string option;
  base_url : string option;
  model : string option;
}

let empty = { api_key = None; base_url = None; model = None }

let config_home () : string =
  match Sys.getenv_opt "XDG_CONFIG_HOME" with
  | Some d when d <> "" -> Filename.concat d "agent"
  | _ ->
      (match Sys.getenv_opt "HOME" with
       | Some h
         when h <> "" ->
           Filename.concat (Filename.concat h ".config") "agent"
       | _ -> ".config/agent")

let config_path () : string = Filename.concat (config_home ()) "config.json"

let lens_config_path (dir : string) : string =
  Filename.concat dir "__config.json"

(** Parse a config file's JSON. Tolerant: unknown/extra keys ignored;
    both "base_url" and "base" accepted (the lens file is written by
    remote/lib/lens_store.ml as "base"). *)
let of_json_string (s : string) : t =
  try
    match Yojson.Safe.from_string s with
    | `Assoc kv ->
        let opt_str ks =
          List.fold_left
            (fun acc k ->
              match acc with
              | Some _ -> acc
              | None ->
                  (match List.assoc_opt k kv with
                   | Some (`String v) when v <> "" -> Some v
                   | _ -> None))
            None ks
        in
        { api_key = opt_str [ "api_key" ];
          base_url = opt_str [ "base_url"; "base" ];
          model = opt_str [ "model" ] }
    | _ -> empty
  with _ -> empty

let load_file (path : string) : t =
  if not (Sys.file_exists path) then empty
  else
    (try
       let ic = open_in_bin path in
       let s = In_channel.input_all ic in
       close_in ic;
       of_json_string s
     with _ -> empty)

(** The agent's own config home file. *)
let load () : t = load_file (config_path ())

(** A lens's __config.json (sync round metadata landing spot). *)
let load_lens (dir : string) : t = load_file (lens_config_path dir)

let rec mkdir_p (dir : string) : unit =
  if dir <> "" && not (Sys.file_exists dir && Sys.is_directory dir) then begin
    mkdir_p (Filename.dirname dir);
    (try Unix.mkdir dir 0o700 with Unix.Unix_error (Unix.EEXIST, _, _) -> ())
  end

(** Field-wise merge: a set field of [primary] wins, else [fallback]. *)
let merge ~(primary : t) ~(fallback : t) : t =
  { api_key = (match primary.api_key with Some _ -> primary.api_key | None -> fallback.api_key);
    base_url = (match primary.base_url with Some _ -> primary.base_url | None -> fallback.base_url);
    model = (match primary.model with Some _ -> primary.model | None -> fallback.model) }

(** Write config (0600 — holds a provider secret; same footprint as
    ~/.config/pricklypear/env). Creates the config home on first save.
    Atomic via rename(2), like the lens substrate. *)
let save ?(path : string option) (c : t) : unit =
  let path = match path with Some p -> p | None -> config_path () in
  mkdir_p (Filename.dirname path);
  let opt k = function Some v -> [ (k, `String v) ] | None -> [] in
  let json =
    Yojson.Safe.to_string
      (`Assoc
         (opt "api_key" c.api_key
          @ opt "base_url" c.base_url
          @ opt "model" c.model))
  in
  let tmp = path ^ ".tmp" in
  let oc = open_out_bin tmp in
  output_string oc json;
  close_out oc;
  (try Unix.chmod tmp 0o600 with _ -> ());
  Unix.rename tmp path

let last4 (k : string) : string =
  if String.length k >= 4 then String.sub k (String.length k - 4) 4
  else "????"

(** One-turn provider round-trip cap (Loop.run max_turns). Default is
    the engine's max_turns_default; AGENT_MAX_TURNS overrides. *)
let max_turns () : int =
  match Sys.getenv_opt "AGENT_MAX_TURNS" with
  | Some s when s <> "" ->
    (try max (int_of_string (String.trim s)) 1 with _ -> Loop.max_turns_default)
  | _ -> Loop.max_turns_default
