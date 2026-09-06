(* Shared process-env config plumbing for the standalone agent binaries
   (glochid agent + pp-remote). Both carried verbatim copies of
   [getenv] / [load_dotenv]; this is the one env-config helper
   (i-1788671587-9). Pure Unix, no Eio — call [load_dotenv] before any
   env read at boot. *)

(** getenv with a default that also wins on an empty string (an
   exported-but-empty var must not shadow the default). *)
let getenv name ~default =
  match Sys.getenv_opt name with
  | Some s when s <> "" -> s
  | Some _ | None -> default

(* Auto-load ./.env if present (KEY=VALUE or `export KEY=VALUE` lines).
   Runs before any cfg/env read so AGENT_API_KEY etc. land without the
   user sourcing the file. Quoted values are unquoted; blank lines and
   # comments are skipped. Failures are silent (file optional). *)
let load_dotenv () : unit =
  let path = ".env" in
  match open_in path with
  | exception _ -> ()
  | ic ->
    (try
       while true do
         let line = String.trim (input_line ic) in
         if line = "" || line.[0] = '#' then ()
         else begin
           let line =
             if String.length line > 7 && String.sub line 0 7 = "export "
             then String.trim (String.sub line 7 (String.length line - 7))
             else line
           in
           match String.index_opt line '=' with
           | Some i ->
               let k = String.trim (String.sub line 0 i) in
               let v_raw = String.trim (String.sub line (i + 1) (String.length line - i - 1)) in
               let v =
                 let n = String.length v_raw in
                 if n >= 2
                    && (v_raw.[0] = '"' && v_raw.[n - 1] = '"'
                        || v_raw.[0] = '\'' && v_raw.[n - 1] = '\'')
                 then String.sub v_raw 1 (n - 2)
                 else v_raw
               in
               Unix.putenv k v
           | None -> ()
         end
       done
     with End_of_file -> ());
    close_in ic
