(* Harness identity for the standalone agent binaries (glochid agent +
   pp-remote): the one-shot system-prompt preamble that tells the model
   its harness facts up front so it does not probe the OS (or leak
   secrets like AGENT_API_KEY doing so). The computed facts, the
   one-shot capture helper, and the shared skeleton (facts block) live
   here — each binary supplies its own identity/conduct text segments
   (they are deliberately different narratives, not copies)
   (i-1788671587-9). *)

(* One-shot capture for boot-time facts (uname). Empty output (a failed
   or missing command) reads as "". *)
let run_captured (cmd : string) : string =
  try
    let ic = Unix.open_process_in cmd in
    let out = String.trim (In_channel.input_all ic) in
    match Unix.close_process_in ic with
    | Unix.WEXITED 0 -> out
    | _ -> ""
  with _ -> ""

type facts = {
  host : string;   (* hostname, "?" on failure *)
  uname : string;  (* uname -srm, "" when unavailable *)
  cwd : string;    (* process working directory (project root) *)
  date : string;   (* operator-local "YYYY-MM-DD HH:MM" *)
}

(* Computed once at boot. *)
let facts () : facts =
  let host = try Unix.gethostname () with _ -> "?" in
  let uname = run_captured "uname -srm" in
  let cwd = Sys.getcwd () in
  let tm = Unix.localtime (Unix.gettimeofday ()) in
  let date =
    Printf.sprintf "%04d-%02d-%02d %02d:%02d"
      (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1) tm.Unix.tm_mday
      tm.Unix.tm_hour tm.Unix.tm_min
  in
  { host; uname; cwd; date }

(** Assemble the system prompt from the shared skeleton:

    {v
      pre_facts            (binary: identity + "one unix process" stance)
      <blank>
      Harness facts (...): - host / - cwd / - date / - model: M via U
      fact_lines           (binary: tools / image / surface lines)
      <blank>
      post_facts           (binary: Conduct block)
    v}

    Byte-identical to the pre-refactor agent/bin + remote/bin
    [harness_context] outputs. *)
let context ~(pre_facts : string) ~(fact_lines : string)
    ~(post_facts : string) ~(model : string) ~(base_url : string) : string =
  let f = facts () in
  Printf.sprintf
    "%s\n\nHarness facts (told to you here; do NOT probe the OS to \
     discover them):\n- host: %s (%s)\n- cwd: %s -- the operator's \
     working directory; treat as project root\n- date: %s (operator \
     local)\n- model: %s via %s\n%s\n%s"
    pre_facts f.host
    (if f.uname = "" then "?" else f.uname)
    f.cwd f.date model base_url fact_lines post_facts
