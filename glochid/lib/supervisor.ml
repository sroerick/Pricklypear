(* PP run-primitive supervisor — borge/run-primitive.borg supervisor-tick,
   PHASE 2 (the glochid app contract).

   One run = one attempt to advance one card one gate, executed by a
   SUPERVISED SIDECAR RUNNER outside this process:
   scripts/pp-flow.sh --run <card-id> --supervised (which runs the autolith
   adapter). The supervisor owns claims, budgets, transitions, and evidence;
   PP rows (data.s.run.* via the workstreams run-rows prim set) are the
   queue; the LLM loop NEVER runs inside the PP runtime process.

   The tick (every tick_s, default 30s — the chapter's PROPOSED default):
     1. HEARTBEAT: bump claim_heartbeat_at + claim_deadline for every run
        row we hold with a live sidecar. Under the ratified
        single-supervisor design the run-row claim IS the supervisor lease;
        no separate supervisor datum exists in phase 2.
     2. REAP: rows whose claim expired are TAKEN OVER from the step pointer
        (never restarted from scratch, resume-semantics 3). The phase-2 step
        list is a single sidecar step, so takeover re-dispatches THE SAME
        step without burning attempts. A row that expires twice in one boot
        is finished dead instead of respawned forever.
     3. FILL THE QUEUE: cards from todo/item/queue (ORDER owned by the prim)
        minus the eligibility filter (actionable states, excluded kinds, no
        live card lease, no open run row) -> run-new (queued).
     4. DISPATCH: advisory budget check -> CAS claim
        (workstreams/run-claim, queued->claimed, the ONE compare-and-set)
        -> ONE claim-time eval (state + brief + columns + assignee, the
        state_brief_cols discipline) -> brief_hash patch -> card lease
        mirror (assignee run:<run-id>, the session-lease amendment shape)
        -> run-start -> spawn the sidecar under the concurrency cap
        (default 2, POLICY-4). Due retries (POLICY-2 backoff, 2^n capped
        60s) re-dispatch the SAME step from the stored brief.
     5. PUBLISH: one stderr count line when the shape changes.

   Sidecar completion is harvested by the child's own fiber (lower latency
   than waiting for the next tick): parse the PPFLOW-* verdict/usage lines
   from the driver log -> run-usage-set + run-result (THE USAGE
   PASS-THROUGH: the adapter's :INPUT-TOKENS / :OUTPUT-TOKENS /
   :PROVIDER-REQUESTS finally land on the row instead of being dropped) ->
   write-back (ONE batched eval, pp-flow's advance_batch/dest_batch
   discipline; POLICY-3 no-verdict fail-safe VERBATIM) or re-dispatch with
   backoff, or dead/failed/timed_out with a card comment.

   USAGE IS A SELF-REPORT (RATIFIED): the pre-dispatch budget gate is
   ADVISORY until provider-structured usage lands; a crashed or lying
   executor reports zero. THE EXECUTOR NEVER TOUCHES THE BOARD in
   supervised mode: pp-flow --supervised skips claim/brief-render/write-back
   entirely; every card write flows through here. Transcripts stay local
   (.ralph/supervisor/<run-id>/), never in rows. *)

open Eio_unix.Stdenv

let log fmt =
  Printf.ksprintf (fun s -> Printf.eprintf "[supervisor] %s\n%!" s) fmt

(* ------------------------------------------------------------------ *)
(* Configuration (env knobs via the GLOCHID_ > AGENT_ compat window)   *)
(* ------------------------------------------------------------------ *)

type t = {
  tick_s : int;                       (* tick interval (PROPOSED 30s) *)
  concurrency : int;                  (* POLICY-4: 2 until the soak *)
  step_wall_s : int;                  (* POLICY-1 proposed: 3600s *)
  run_wall_s : int;                   (* POLICY-1 proposed: 14400s *)
  attempts_transient : int;           (* ratified proposed: 2 *)
  attempts_deterministic : int;       (* ratified proposed: 1 *)
  states : string;                    (* actionable columns, csv *)
  exclude_kinds : string;             (* excluded kinds, csv *)
  list_id : string;                   (* "" = all lists *)
  flow_script : string;               (* scripts/pp-flow.sh path *)
  cwd : string;                       (* child working directory *)
  log_root : string;                  (* per-run artifact dirs *)
  identity : string;                  (* runner_identity recorded on rows *)
  supervisor_id : string;             (* stable claim owner per host *)
  instance_id : string;               (* pid@boot-ts sidecar instance *)
  max_tokens : int;                   (* advisory; 0 = unlimited *)
  max_requests : int;                 (* advisory; 0 = unlimited *)
  runner : string;                    (* when set: child PP_FLOW_PI *)
}

let env_nonneg (default : int) (name : string) : int =
  match Config.getenv_compat name with
  | Some v -> (
      match int_of_string_opt (String.trim v) with
      | Some i when i >= 0 -> i
      | _ -> default)
  | None -> default

let env_pos (default : int) (name : string) : int =
  match Config.getenv_compat name with
  | Some v -> (
      match int_of_string_opt (String.trim v) with
      | Some i when i > 0 -> i
      | _ -> default)
  | None -> default

let env_str (default : string) (name : string) : string =
  match Config.getenv_compat name with
  | Some v when String.trim v <> "" -> String.trim v
  | _ -> default

let config () : t =
  let host = try Unix.gethostname () with _ -> "localhost" in
  {
    tick_s = env_pos 30 "SUPERVISOR_TICK_S";
    concurrency = env_pos 2 "SUPERVISOR_CONCURRENCY";
    step_wall_s = env_pos 3600 "SUPERVISOR_STEP_WALL_S";
    run_wall_s = env_pos 14400 "SUPERVISOR_RUN_WALL_S";
    attempts_transient = env_pos 2 "SUPERVISOR_ATTEMPTS_TRANSIENT";
    attempts_deterministic = env_pos 1 "SUPERVISOR_ATTEMPTS_DETERMINISTIC";
    states = env_str "TODO,CODE" "SUPERVISOR_STATES";
    exclude_kinds = env_str "" "SUPERVISOR_EXCLUDE_KINDS";
    list_id = env_str "" "SUPERVISOR_LIST";
    flow_script = env_str "scripts/pp-flow.sh" "SUPERVISOR_FLOW_SCRIPT";
    cwd = env_str (Sys.getcwd ()) "SUPERVISOR_CWD";
    log_root =
      env_str (Filename.concat (Sys.getcwd ()) ".ralph/supervisor")
        "SUPERVISOR_LOG_ROOT";
    identity = env_str "pp-supervisor" "SUPERVISOR_IDENTITY";
    supervisor_id = "glochid-supervisor@" ^ host;
    instance_id =
      Printf.sprintf "%d@%d" (Unix.getpid ())
        (int_of_float (Unix.gettimeofday ()));
    max_tokens = env_nonneg 0 "SUPERVISOR_MAX_TOKENS";
    max_requests = env_nonneg 0 "SUPERVISOR_MAX_REQUESTS";
    runner = env_str "" "SUPERVISOR_RUNNER";
  }

(* ------------------------------------------------------------------ *)
(* Time + small helpers                                                *)
(* ------------------------------------------------------------------ *)

(* ISO 8601 UTC, byte-identical to the habitat's (now-iso):
   "YYYY-MM-DDTHH:MM:SSZ". ISO strings sort lexicographically — the same
   trick the run-rows prims use (run-expired? has no seconds arithmetic;
   neither does this side). *)
let iso_at (t : float) : string =
  let tm = Unix.gmtime t in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ"
    (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1) tm.Unix.tm_mday
    tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec

let iso_now () : string = iso_at (Unix.gettimeofday ())

let iso_after (secs : int) : string =
  iso_at (Unix.gettimeofday () +. float_of_int secs)

let sh_quote (s : string) : string =
  "'" ^ String.concat "'\\''" (String.split_on_char '\'' s) ^ "'"

let rec mkdir_p (path : string) : unit =
  if path = "" || path = "/" || path = "." then ()
  else begin
    (try mkdir_p (Filename.dirname path) with _ -> ());
    (try Unix.mkdir path 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ())
  end

let read_file (path : string) : string =
  try
    let ic = open_in_bin path in
    let n = in_channel_length ic in
    let s = really_input_string ic n in
    close_in ic;
    s
  with _ -> ""

let write_file (path : string) (content : string) : unit =
  let oc = open_out_bin path in
  output_string oc content;
  close_out oc

(* A bounded single-line tail of a driver log for card comments: control
   chars (ANSI escapes, CR/LF) become spaces — enough to see WHY, never a
   dump. *)
let log_tail (text : string) (n : int) : string =
  let flat =
    String.map
      (fun c -> if c < ' ' || c = '\n' || c = '\r' || c = '\027' then ' ' else c)
      text
  in
  let len = String.length flat in
  if len <= n then String.trim flat
  else String.trim (String.sub flat (len - n) n)

(* A JSON string literal (for building (json-parse "...") arguments). *)
let json_lit (s : string) : string =
  let b = Buffer.create (String.length s + 2) in
  Buffer.add_char b '"';
  String.iter
    (fun c ->
      match c with
      | '\\' -> Buffer.add_string b "\\\\"
      | '"' -> Buffer.add_string b "\\\""
      | c -> Buffer.add_char b c)
    s;
  Buffer.add_char b '"';
  Buffer.contents b

(* pp-flow's csv_to_json, ported: "TODO,CODE" -> ["TODO","CODE"]; a value
   that is already a JSON array passes through; empty -> []. *)
let csv_to_json (s : string) : string =
  let s = String.trim s in
  if s = "" then "[]"
  else if s.[0] = '[' then s
  else
    let parts =
      String.split_on_char ',' s |> List.map String.trim
      |> List.filter (fun x -> x <> "")
    in
    "[" ^ String.concat "," (List.map json_lit parts) ^ "]"

let contains_sub (needle : string) (hay : string) : bool =
  let nl = String.length needle and hl = String.length hay in
  let rec go i =
    if i + nl > hl then false
    else if String.sub hay i nl = needle then true
    else go (i + 1)
  in
  nl = 0 || go 0

(* ------------------------------------------------------------------ *)
(* Habitat session: ONE eval client, serialized behind a mutex (the    *)
(* tick fiber and the per-child harvest fibers share it).              *)
(* ------------------------------------------------------------------ *)

type session = { env : Eio_unix.Stdenv.base; mutex : Eio.Mutex.t }

let eval_s (sess : session) (expr : string) :
    (Yojson.Safe.t, string) Result.t =
  Eio.Mutex.lock sess.mutex;
  Fun.protect
    ~finally:(fun () -> Eio.Mutex.unlock sess.mutex)
    (fun () -> Habitat.eval sess.env expr)

let jmember (name : string) (j : Yojson.Safe.t) : Yojson.Safe.t =
  match j with
  | `Assoc fields -> ( try List.assoc name fields with Not_found -> `Null)
  | _ -> `Null

let jstr ?(default = "") (j : Yojson.Safe.t) : string =
  match j with
  | `String s -> s
  | `Int i -> string_of_int i
  | `Intlit s -> s
  | `Float f ->
      if Float.is_integer f then Printf.sprintf "%d" (int_of_float f)
      else string_of_float f
  | `Bool true -> "true"
  | `Bool false -> "false"
  | _ -> default

let jint (j : Yojson.Safe.t) : int =
  match int_of_string_opt (jstr j) with Some i -> i | None -> 0

(* An eval whose reply VALUE is a JSON TEXT (json-emit) -> parsed JSON. *)
let eval_json (sess : session) (expr : string) :
    (Yojson.Safe.t, string) Result.t =
  match eval_s sess expr with
  | Error e -> Error e
  | Ok (`String s) -> (
      try Ok (Yojson.Safe.from_string s)
      with Yojson.Json_error m -> Error ("unparsable json-emit: " ^ m))
  | Ok v -> Ok v

(* ------------------------------------------------------------------ *)
(* Pure policy (unit-tested)                                           *)
(* ------------------------------------------------------------------ *)

(* POLICY-2 (SET, gregor): backoff = the jobs shape, 2^n capped 60s. *)
let backoff_s (attempts : int) : int =
  min (1 lsl min (max 1 attempts) 8) 60

(* The adapter's classification, supervisor-side (pp-flow-autolith.sh retry
   policy + run-primitive resume-semantics 2/6): transient = :PROVIDER-
   FAILURE naming a self-clearing HTTP status (402/408/409/425/429/5xx), a
   crash with no job result, or :TIMED-OUT (terminal for THAT adapter
   attempt, but the supervisor MAY take a fresh attempt with a fresh wall).
   Deterministic classes (:PROCESS-FAILURE, :INVALID-*, 400/401/403/404/422,
   ...) attempt EXACTLY ONCE. *)
let transient_status_in_message (message : string) : bool =
  let rec find_from (from : int) : bool =
    let len = String.length message in
    let rec go i =
      if i + 5 > len then false
      else if String.sub message i 5 <> "HTTP " then go (i + 1)
      else if i + 8 > len then find_from (i + 5)
      else
        match int_of_string_opt (String.sub message (i + 5) 3) with
        | Some code ->
            if code = 402 || code = 408 || code = 409 || code = 425
            || code = 429 || code >= 500
            then true
            else find_from (i + 5)
        | None -> find_from (i + 5)
    in
    go from
  in
  find_from 0

let transient_failure ~(status : string) ~(category : string)
    ~(message : string) : bool =
  (* Normalize before comparing: the adapter's keywords arrive as
     ":PROVIDER-FAILURE" / "PROVIDER-FAILURE" / ":TIMED-OUT" depending on
     which caller passes them (parse_verdict keeps them verbatim; the
     harvest path pre-strips the colon). Case + colon must not change the
     class, or a transient 402 dies on the first attempt. *)
  let norm s =
    let s = String.trim s in
    let s =
      if s <> "" && s.[0] = ':' then String.sub s 1 (String.length s - 1)
      else s
    in
    String.lowercase_ascii s
  in
  let status = norm status in
  let category = norm category in
  if status = "crashed" then true
  else if status = "timed-out" then true
  else if category = "provider-failure" then transient_status_in_message message
  else false

(* pp-flow's *REVIEW* column glob: a REVIEW column's whole contract is a
   VERDICT; POLICY-3 (SET, gregor) keeps the NO-VERDICT fail-safe verbatim. *)
let noverdict_column (claimed_state : string) : bool =
  contains_sub "REVIEW" claimed_state

(* pp-flow's terminal guard: todo/cycle WRAPS at the end of the column set,
   so these states are never advanced. *)
let terminal_card_state (s : string) : bool =
  List.mem s [ "DONE"; "PARKED"; "CANCELLED"; "ARCHIVED" ]

let terminal_run_state (s : string) : bool =
  List.mem s [ "succeeded"; "failed"; "timed_out"; "canceled"; "dead" ]

type verdict = {
  v_dest : string;
  v_evid : string;
  v_status : string;
  v_category : string;
  v_trace : string;
  v_message : string;
  v_requests : int;
  v_input : int;
  v_cached : int;
  v_output : int;
  v_wall : int;
}

let empty_verdict =
  { v_dest = ""; v_evid = ""; v_status = ""; v_category = ""; v_trace = "";
    v_message = ""; v_requests = 0; v_input = 0; v_cached = 0; v_output = 0;
    v_wall = 0 }

let starts_with (p : string) (s : string) : bool =
  String.length s >= String.length p
  && String.sub s 0 (String.length p) = p

let parse_kv_pairs (s : string) : (string * string) list =
  String.split_on_char ' ' s
  |> List.filter_map (fun tok ->
         match String.index_opt tok '=' with
         | Some i when i > 0 ->
             Some
               ( String.sub tok 0 i,
                 String.sub tok (i + 1) (String.length tok - i - 1) )
         | _ -> None)

let rindex (needle : string) (hay : string) : int option =
  let nl = String.length needle and hl = String.length hay in
  if nl = 0 || nl > hl then None
  else begin
    let rec go i best =
      if i + nl > hl then best
      else go (i + 1) (if String.sub hay i nl = needle then Some i else best)
    in
    go 0 None
  end

(* PPFLOW-STATUS line: key=value tokens, then message= as the rest of the
   line (the failure text may contain spaces and '='). *)
let parse_status_rest (rest : string) : (string * string) list * string =
  match rindex " message=" rest with
  | Some i ->
      ( parse_kv_pairs (String.sub rest 0 i),
        String.sub rest (i + 9) (String.length rest - i - 9) )
  | None -> (parse_kv_pairs rest, "")

let kv_int (kvs : (string * string) list) (k : string) : int =
  match List.assoc_opt k kvs with
  | Some v -> ( match int_of_string_opt (String.trim v) with Some i -> i | None -> 0)
  | None -> 0

let kv_str (kvs : (string * string) list) (k : string) : string =
  match List.assoc_opt k kvs with Some v -> String.trim v | None -> ""

(* The supervisor/sidecar contract: the supervised pp-flow prints
     pp-flow: PPFLOW-DEST <col>
     pp-flow: PPFLOW-EVIDENCE <single line>
     pp-flow: PPFLOW-USAGE requests=N input_tokens=N cached_tokens=N
                             output_tokens=N wall_s=N trace=T
     pp-flow: PPFLOW-STATUS status=S category=C trace=T message=rest
   on its stdout (captured in driver.log). *)
let parse_verdict (text : string) : verdict =
  let v = ref empty_verdict in
  String.split_on_char '\n' text
  |> List.iter (fun raw ->
         let line = String.trim raw in
         let pfx = "pp-flow: PPFLOW-" in
         if starts_with pfx line then begin
           let rest =
             String.sub line (String.length pfx)
               (String.length line - String.length pfx)
           in
           if starts_with "DEST " rest then
             v := { !v with v_dest = String.trim (String.sub rest 5 (String.length rest - 5)) }
           else if starts_with "EVIDENCE " rest then
             v := { !v with v_evid = String.trim (String.sub rest 9 (String.length rest - 9)) }
           else if starts_with "USAGE " rest then begin
             let kvs = parse_kv_pairs (String.sub rest 6 (String.length rest - 6)) in
             v :=
               { !v with
                 v_requests = kv_int kvs "requests";
                 v_input = kv_int kvs "input_tokens";
                 v_cached = kv_int kvs "cached_tokens";
                 v_output = kv_int kvs "output_tokens";
                 v_wall = kv_int kvs "wall_s";
                 v_trace = kv_str kvs "trace" }
           end
           else if starts_with "STATUS " rest then begin
             let kvs, msg =
               parse_status_rest (String.sub rest 7 (String.length rest - 7))
             in
             v :=
               { !v with
                 v_status = kv_str kvs "status";
                 v_category = kv_str kvs "category";
                 v_trace = kv_str kvs "trace";
                 v_message = msg }
           end
         end);
  !v

let norm_colon (s : string) : string =
  let s = String.trim s in
  if s = "" then ""
  else if s.[0] = ':' then String.sub s 1 (String.length s - 1)
  else s

let norm_status (s : string) : string = String.lowercase_ascii (norm_colon s)

(* ------------------------------------------------------------------ *)
(* steps_json (the supervisor's serialized step list; phase 2 = one    *)
(* sidecar step per gate run)                                          *)
(* ------------------------------------------------------------------ *)

let steps_doc (cfg : t) ~(state : string) ~(attempts : int) ~(error : string) :
    string =
  let step =
    `Assoc
      [ ("name", `String "sidecar");
        ("kind", `String "provider");
        ("state", `String state);
        ("attempts", `Int attempts);
        ("max_attempts", `Int cfg.attempts_transient);
        ("wall_budget_s", `Int cfg.step_wall_s);
        ("idempotent", `Bool true);
        (* a fresh LLM turn is naturally retryable (the idempotency rule) *)
        ("usage",
         `Assoc
           [ ("requests", `Int 0); ("input_tokens", `Int 0);
             ("cached_tokens", `Int 0); ("output_tokens", `Int 0);
             ("wall_s", `Int 0) ]);
        ("evidence_ref", `String "");
        ("error", `String error) ]
  in
  Yojson.Safe.to_string (`List [ step ])

let steps_initial (cfg : t) : string =
  steps_doc cfg ~state:"pending" ~attempts:0 ~error:""

let steps_rebuild (cfg : t) ~(state : string) ~(attempts : int)
    ~(error : string) : string =
  steps_doc cfg ~state ~attempts ~error

(* ------------------------------------------------------------------ *)
(* Remote expr builders (the workstreams run-rows prim set)            *)
(* ------------------------------------------------------------------ *)

let q = Habitat.q

(* CLAIM TTL (F5, ratified): ttl_s >= step wall + 2 ticks + slack, floor
   600s; the supervisor computes the deadline ISO (the prim set has no
   seconds math). *)
let ttl_s (cfg : t) : int = max 600 (cfg.step_wall_s + (2 * cfg.tick_s) + 60)

let run_new_expr (cfg : t) ~(card : string) ~(list_id : string)
    ~(from_state : string) : string =
  Printf.sprintf
    "(workstreams/run-new %s %s %s \"\" \"\" %d %d %d %s)"
    (q card) (q list_id) (q from_state) cfg.max_tokens cfg.max_requests
    cfg.run_wall_s (q (steps_initial cfg))

let run_patch_expr (run_id : string) (updates : (string * string) list) :
    string =
  let flat =
    updates |> List.map (fun (k, v) -> q k ^ " " ^ q v) |> String.concat " "
  in
  Printf.sprintf "(workstreams/run-patch %s (list %s))" (q run_id) flat

(* THE CAS: queued -> claimed, guarded (only a still-queued row flips). *)
let run_claim_expr (cfg : t) (run_id : string) : string =
  Printf.sprintf "(workstreams/run-claim %s %s %s %s %d %s)"
    (q run_id) (q cfg.supervisor_id) (q cfg.instance_id) (q cfg.identity)
    (ttl_s cfg) (q (iso_after (ttl_s cfg)))

let run_heartbeat_expr (cfg : t) (run_id : string) : string =
  Printf.sprintf "(workstreams/run-heartbeat %s %s)" (q run_id)
    (q (iso_after (ttl_s cfg)))

let run_start_expr (run_id : string) : string =
  Printf.sprintf "(workstreams/run-start %s)" (q run_id)

let run_usage_expr (run_id : string) (v : verdict) : string =
  Printf.sprintf "(workstreams/run-usage-set %s %d %d %d %d %d)"
    (q run_id) v.v_requests v.v_input v.v_cached v.v_output v.v_wall

let run_result_expr (run_id : string) (v : verdict) (fallback_msg : string) :
    string =
  let status = if v.v_status = "" then "crashed" else norm_status v.v_status in
  let category =
    if v.v_category = "" then
      (if v.v_status = "" then "sidecar-failure" else "")
    else norm_colon v.v_category
  in
  let msg = if v.v_message = "" then fallback_msg else v.v_message in
  Printf.sprintf "(workstreams/run-result %s %s %s %s %s)" (q run_id)
    (q status) (q category) (q msg) (q v.v_trace)

let run_finish_expr (run_id : string) (final_state : string)
    (error_class : string) (error_message : string) : string =
  Printf.sprintf "(workstreams/run-finish %s %s %s %s)" (q run_id)
    (q final_state) (q error_class) (q error_message)

let run_step_expr (cfg : t) (run_id : string) ~(state : string)
    ~(attempts : int) ~(error : string) : string =
  Printf.sprintf "(workstreams/run-step-set %s %s)" (q run_id)
    (q (steps_rebuild cfg ~state ~attempts ~error))

(* ONE scan of the shared run space (workstreams/run-all: the proven
   all-shared depth, filtered to the run type), projected to the fields the
   supervisor decides from. ACTIVE ROWS ONLY (queued/claimed/running/parked):
   the run table grows without bound and the full projection over ~620 rows
   blew the eval budget (10-18 tick scan budget-exceeded). Terminal counts
   for the standup summary come from runs_tally_expr instead. *)
let runs_scan_expr () : string =
  let fields =
    [
      "card_id";
      "list_id";
      "from_state";
      "state";
      "claim_supervisor_id";
      "claim_deadline";
      "budget_max_tokens";
      "budget_max_requests";
      "usage_requests";
      "usage_input_tokens";
      "steps_json";
      "created_at";
      "started_at";
    ]
  in
  let gets =
    List.map
      (fun k ->
        Printf.sprintf "%s (as-str (dict-get b %s))" (q k) (q k))
      fields
  in
  Printf.sprintf
      "(begin (load-library \"todo\") (let ((cid (lambda (p) (let ((s (as-str p))) (let ((n (string-length s))) (if (< n 33) (todo/tree/path-segment s) (if (string-eq (substring s (- n 33) 1) \".\") (todo/tree/redash (substring s (- n 32) 32)) (if (and (< 36 n) (string-eq (substring s (- n 37) 1) \".\")) (substring s (- n 36) 36)(todo/tree/path-segment s))))))))) (json-emit (list/map (lambda (nj) (let ((b (dict-get nj \"body\"))) (dict-set* \"{}\" (list \"id\" (cid (dict-get nj \"path\")) %s)))) (list/filter (lambda (nj) (let ((st (as-str (dict-get (dict-get nj \"body\") \"state\")))) (and (string-eq (as-str (dict-get nj \"type\")) \"run\") (or (string-eq st \"queued\") (string-eq st \"claimed\") (string-eq st \"running\") (string-eq st \"parked\"))))) (tree/list \"data.s\" 3 2000))))))"
      (String.concat " " gets)

let queue_eligible_expr (cfg : t) : string =
  (* The PP-side prio-sorted moved OUT of this expression (10-18): the
     insertion sort's per-comparison rank+position recomputation blew the
     eval budget at ~60 eligible rows (the todo-lib decorate-once sort,
     eadf3e0, is committed but not yet seeded onto system/main). The
     canonical work order (priority rank, then position, then id) is
     mirrored OCaml-side in the tick - POLICY-6: the same key, the
     supervisor never invents an order. *)
  Printf.sprintf
      {|(begin (load-library "todo") (let ((states (json-parse %s)) (excl (json-parse %s)) (want-list %s)) (let ((in? (lambda (x xs) (list/foldl (lambda (a c) (if (string-eq (as-str c) (as-str x)) #t a)) #f xs)))) (json-emit (list/map (lambda (row) (let ((b (cdr row))) (dict-set* "{}" (list "id" (as-str (car row)) "state" (as-str (todo/item/state b)) "title" (as-str (todo/item/title b)) "list_id" (as-str (todo/item/list-id b)) "kind" (as-str (dict-get b "kind")) "priority" (as-str (dict-get b "priority")) "position" (as-str (dict-get b "position")))))) (list/filter (lambda (row) (let ((b (cdr row))) (and (string-eq (todo/item/assignee b) "") (or (string-eq want-list "") (string-eq (todo/item/list-id b) want-list)) (in? (todo/item/state b) states) (or (null? excl) (not (in? (as-str (dict-get b "kind")) excl)))))) (todo/item/active (todo/item/all))))))))|}
    (q (csv_to_json cfg.states)) (q (csv_to_json cfg.exclude_kinds))
    (q cfg.list_id)

(* Cheap per-state count over ALL run rows: one dict-get + one dict-set*
   per row. The tick's scan rows are active-only now, so terminal counts
   for the standup summary come from here. *)
let runs_tally_expr () : string =
  {|(begin (load-library "todo") (json-emit (list/map (lambda (nj) (as-str (dict-get (dict-get nj "body") "state"))) (list/filter (lambda (nj) (string-eq (as-str (dict-get nj "type")) "run")) (tree/list "data.s" 3 2000)))))|}

(* ---- card writes (pp-flow's batched-eval discipline, ONE round trip) ---- *)

let probe_expr (card : string) : string =
  Printf.sprintf
    {|(begin (load-library "todo") (if (null? (todo/item/get %s)) "missing" (string-append "ok|" (as-str (todo/item/state (todo/item/get %s))))))|}
    (q card) (q card)

let advance_expr (card : string) (msg : string) : string =
  Printf.sprintf
    {|(begin (load-library "todo") (string-append (as-str (todo/cycle %s)) "|" (as-str (todo/comment/add %s %s)) "|" (as-str (todo/item/state (todo/item/get %s))) "|" (as-str (todo/item/unassign %s))))|}
    (q card) (q card) (q msg) (q card) (q card)

let dest_expr (card : string) (msg : string) (dest : string) : string =
  Printf.sprintf
    {|(begin (load-library "todo") (string-append (as-str (todo/set-state %s %s)) "|" (as-str (todo/comment/add %s %s)) "|" (as-str %s) "|" (as-str (todo/item/unassign %s))))|}
    (q card) (q dest) (q card) (q msg) (q dest) (q card)

let comment_only_expr (card : string) (msg : string) : string =
  Printf.sprintf
    {|(begin (load-library "todo") (string-append (as-str (todo/comment/add %s %s)) "|" (as-str (todo/item/unassign %s))))|}
    (q card) (q msg) (q card)

let unassign_expr (card : string) : string =
  Printf.sprintf {|(begin (load-library "todo") (todo/item/unassign %s))|} (q card)

let assign_expr (card : string) (lease : string) : string =
  Printf.sprintf {|(begin (load-library "todo") (todo/item/assign %s %s))|} (q card)
    (q lease)

let card_comment_expr (card : string) (msg : string) : string =
  Printf.sprintf
    {|(begin (load-library "todo") (todo/comment/add %s %s) (todo/item/unassign %s))|}
    (q card) (q msg) (q card)

let restore_state_expr (card : string) (state : string) : string =
  Printf.sprintf {|(begin (load-library "todo") (todo/set-state %s %s))|} (q card)
    (q state)

(* ONE claim-time eval: claimed state + brief + columns + assignee (the
   state_brief_cols layout, plus the assignee the double-claim check
   needs). Layout: state\nMARK\nbrief\nCOLSEP\ncols\nMARK2\nassignee. *)
let claim_facts_expr (card : string) (mark : string) (colsep : string)
    (mark2 : string) : string =
  Printf.sprintf
    {|(begin (load-library "todo") (if (null? (todo/item/get %s)) "missing" (let ((it (todo/item/get %s))) (string-append (as-str (todo/item/state it)) "\n%s\n" (as-str (todo/flow-brief %s)) "\n%s\n" (list/foldl (lambda (a c) (string-append a "," (as-str c))) "" (todo/state/columns-for-list (as-str (todo/item/list-id it)))) "\n%s\n" (as-str (todo/item/assignee it))))))|}
    (q card) (q card) mark (q card) colsep mark2

(* ------------------------------------------------------------------ *)
(* Scanned rows + supervisor state                                     *)
(* ------------------------------------------------------------------ *)

type row = {
  r_id : string;
  r_card : string;
  r_list : string;
  r_from : string;
  r_state : string;
  r_sup : string;
  r_deadline : string;
  r_budget_tok : int;
  r_budget_req : int;
  r_usage_req : int;
  r_usage_in : int;
  r_steps : string;
  r_created : string;
  r_started : string;
}

let row_of_json (j : Yojson.Safe.t) : row option =
  let id = jstr (jmember "id" j) in
  if id = "" then None
  else
    Some
      { r_id = id;
        r_card = jstr (jmember "card_id" j);
        r_list = jstr (jmember "list_id" j);
        r_from = jstr (jmember "from_state" j);
        r_state = jstr (jmember "state" j);
        r_sup = jstr (jmember "claim_supervisor_id" j);
        r_deadline = jstr (jmember "claim_deadline" j);
        r_budget_tok = jint (jmember "budget_max_tokens" j);
        r_budget_req = jint (jmember "budget_max_requests" j);
        r_usage_req = jint (jmember "usage_requests" j);
        r_usage_in = jint (jmember "usage_input_tokens" j);
        r_steps = jstr (jmember "steps_json" j);
        r_created = jstr (jmember "created_at" j);
        r_started = jstr (jmember "started_at" j) }

type facts = Facts_ok of string * string * string * string | Facts_missing

type ctx = {
  x_run : string;
  x_card : string;
  x_claimed_state : string;
  x_cols : string;
  x_brief : string;  (* path of the stored raw claim-time brief *)
  x_dir : string;
  mutable x_attempts : int;
  mutable x_wall_exceeded : bool;
}

type child = { c_run : string; c_card : string; c_started : float }

type st = {
  cfg : t;
  sess : session;
  sw : Eio.Switch.t;
  live : (string, child) Hashtbl.t;       (* run_id -> live sidecar *)
  ctxs : (string, ctx) Hashtbl.t;         (* run_id -> dispatch context *)
  retry_at : (string, float) Hashtbl.t;   (* run_id -> earliest dispatch *)
  taken : (string, unit) Hashtbl.t;       (* rows adopted this boot *)
  mutable last_summary : string;
}

let eval_ok (st : st) (expr : string) : Yojson.Safe.t option =
  match eval_s st.sess expr with
  | Ok v -> Some v
  | Error e ->
      log "eval failed: %s" e;
      None

(* USAGE IS A SELF-REPORT (ratified): the gate is ADVISORY until
   provider-structured usage lands. The row's own caps win when set; 0 =
   unlimited. *)
let budget_exceeded (st : st) (r : row) : bool =
  let tok = if r.r_budget_tok > 0 then r.r_budget_tok else st.cfg.max_tokens in
  let req =
    if r.r_budget_req > 0 then r.r_budget_req else st.cfg.max_requests
  in
  (tok > 0 && r.r_usage_in >= tok) || (req > 0 && r.r_usage_req >= req)

(* ------------------------------------------------------------------ *)
(* Write-back: the ONE batched card write (pp-flow's discipline)       *)
(* ------------------------------------------------------------------ *)

let write_back (st : st) ~(card : string) ~(claimed_state : string)
    ~(dest : string) ~(msg : string) : (string, string) Result.t =
  match eval_ok st (probe_expr card) with
  | None -> Error "write-back-probe"
  | Some j -> (
      let probe = jstr j in
      if probe = "missing" then Error "card-missing"
      else
        let cur =
          match String.index_opt probe '|' with
          | Some i -> String.sub probe (i + 1) (String.length probe - i - 1)
          | None -> probe
        in
        if claimed_state <> "" && cur <> "" && cur <> claimed_state then begin
          (* double-write defense: the card already left its claimed column
             (the executor never touches the board, but a human might) *)
          ignore (eval_ok st (comment_only_expr card msg));
          Ok cur
        end
        else if terminal_card_state cur then begin
          (* a closed card is never advanced (todo/cycle would wrap it) *)
          ignore (eval_ok st (unassign_expr card));
          Error "card-terminal"
        end
        else
          let res =
            if dest <> "" then eval_ok st (dest_expr card msg dest)
            else eval_ok st (advance_expr card msg)
          in
          match res with
          | None ->
              (* batch failed mid-flight: defensive release, so the lease
                 is never left behind (the stale-lease lesson) *)
              ignore (eval_ok st (unassign_expr card));
              Error "write-back-batch"
          | Some j -> (
              let res_s = jstr j in
              match String.split_on_char '|' res_s with
              | _ :: _ :: newstate :: _ when newstate <> "" -> Ok newstate
              | _ ->
                  (* 09-27 (gregor, run 3): a list with no columns erases
                     the state — restore it and refuse the advance *)
                  if claimed_state <> "" then
                    ignore (eval_ok st (restore_state_expr card claimed_state));
                  Ok claimed_state)
      )

(* ------------------------------------------------------------------ *)
(* Claim-time facts (ONE eval)                                         *)
(* ------------------------------------------------------------------ *)

let claim_facts (st : st) ~(card : string) : facts option =
  let mark = Printf.sprintf "PPCUT%x" (Random.int 0x7ffffff) in
  let colsep = Printf.sprintf "PPCUTC%x" (Random.int 0x7ffffff) in
  let mark2 = Printf.sprintf "PPCUTA%x" (Random.int 0x7ffffff) in
  match eval_ok st (claim_facts_expr card mark colsep mark2) with
  | None -> None
  | Some j -> (
      let raw = jstr j in
      if raw = "missing" || raw = "" then Some Facts_missing
      else
        let lines = String.split_on_char '\n' raw in
        let find needle =
          let rec go i = function
            | [] -> None
            | l :: tl -> if l = needle then Some i else go (i + 1) tl
          in
          go 0 lines
        in
        match (find mark, find colsep, find mark2) with
        | Some mi, Some ci, Some ai when mi < ci && ci < ai ->
            let slice a b =
              lines
              |> List.filteri (fun i _ -> i >= a && i < b)
              |> String.concat "\n"
            in
            let state =
              lines
              |> List.filteri (fun i _ -> i < mi)
              |> String.concat "\n" |> String.trim
            in
            Some
              (Facts_ok
                 ( state,
                   slice (mi + 1) ci,
                   slice (ci + 1) ai,
                   slice (ai + 1) (List.length lines) ))
        | _ ->
            log "claim facts for %s: unparseable layout" card;
            None)

(* ------------------------------------------------------------------ *)
(* Sidecar harvest (per-child fiber; the tick never waits on children) *)
(* ------------------------------------------------------------------ *)

let harvest (st : st) ~(child : child) ~(ctx : ctx) ~(rc : int) : unit =
  let cfg = st.cfg in
  let run_id = child.c_run in
  let driver = read_file (Filename.concat ctx.x_dir "driver.log") in
  let v = parse_verdict driver in
  (* THE USAGE PASS-THROUGH: run-usage-set + run-result land the adapter's
     terminal counts + verdict fields on the row (pp-flow-autolith.sh used
     to parse :INPUT-TOKENS/:OUTPUT-TOKENS/:PROVIDER-REQUESTS and drop
     them at the boundary). *)
  ignore (eval_ok st (run_usage_expr run_id v));
  ignore (eval_ok st
            (run_result_expr run_id v
               "no PPFLOW-STATUS line (sidecar setup failure)"));
  let attempts = ctx.x_attempts + 1 in
  ctx.x_attempts <- attempts;
  if rc = 0 then begin
    let noverdict = noverdict_column ctx.x_claimed_state && v.v_dest = "" in
    let evid =
      if noverdict then
        (* POLICY-3 (SET, gregor), VERBATIM: the no-verdict fail-safe toward
           the human survives; the advance stays the conservative
           one-column step. *)
        "pp-flow: [NO VERDICT RENDERED — the reviewer did not name a DESTINATION. Advanced one column conservatively; read the summary below and re-run if a decision is needed.] "
        ^ v.v_evid
      else v.v_evid
    in
    match
      write_back st ~card:ctx.x_card ~claimed_state:ctx.x_claimed_state
        ~dest:v.v_dest ~msg:evid
    with
    | Ok newstate ->
        ignore (eval_ok st
                  (run_step_expr cfg run_id ~state:"done" ~attempts ~error:""));
        ignore (eval_ok st (run_finish_expr run_id "succeeded" "" ""));
        log "run %s: write-back ok, card %s -> %s%s" run_id ctx.x_card
          newstate
          (if noverdict then " (NO VERDICT: conservative advance)" else "")
    | Error eclass ->
        ignore (eval_ok st
                  (run_step_expr cfg run_id ~state:"failed" ~attempts
                     ~error:eclass));
        (match eclass with
         | "card-terminal" ->
             ignore (eval_ok st
                       (run_finish_expr run_id "failed" "card-terminal"
                          "card already closed; not advanced"));
             log "run %s: card %s already terminal; not advanced" run_id
               ctx.x_card
         | "card-missing" ->
             ignore (eval_ok st
                       (run_finish_expr run_id "failed" "card-missing"
                          "card vanished before write-back"))
         | _ ->
             ignore (eval_ok st
                       (run_finish_expr run_id "failed" eclass
                          "write-back failed; evidence preserved in the run row + log"));
             ignore (eval_ok st
                       (card_comment_expr ctx.x_card
                          (Printf.sprintf
                             "pp-flow(supervisor): write-back failed for run %s (%s); evidence preserved below: %s"
                             run_id eclass evid))));
        Hashtbl.remove st.ctxs run_id
  end
  else begin
    let status = if v.v_status = "" then "crashed" else norm_status v.v_status in
    let category =
      if v.v_status = "" then "sidecar-failure" else norm_colon v.v_category
    in
    let transient =
      transient_failure ~status ~category ~message:v.v_message
    in
    ignore (eval_ok st
              (run_step_expr cfg run_id ~state:"failed" ~attempts
                 ~error:(category ^ " " ^ v.v_message)));
    if ctx.x_wall_exceeded then begin
      ignore (eval_ok st
                (card_comment_expr ctx.x_card
                   (Printf.sprintf
                      "pp-flow(supervisor): run %s TIMED OUT at the run wall (%ds); card NOT advanced. Log: %s"
                      run_id cfg.run_wall_s ctx.x_dir)));
      ignore (eval_ok st
                (run_finish_expr run_id "timed_out" "run-wall"
                   (Printf.sprintf "run wall %ds exceeded" cfg.run_wall_s)));
      Hashtbl.remove st.ctxs run_id
    end
    else if transient && attempts < cfg.attempts_transient then begin
      (* POLICY-2: re-dispatch THE SAME step (never the whole run) after
         the 2^n capped backoff; the row stays live. *)
      Hashtbl.replace st.retry_at run_id
        (Unix.gettimeofday () +. float_of_int (backoff_s attempts));
      log "run %s attempt %d/%d failed transiently (%s %s); retry in %ds"
        run_id attempts cfg.attempts_transient category v.v_message
        (backoff_s attempts)
    end
    else begin
      (* deterministic classes fail ONCE; transient attempts exhausted ->
         dead. Terminal runs never auto-retry. *)
      ignore (eval_ok st
                (card_comment_expr ctx.x_card
                   (Printf.sprintf
                      "pp-flow(supervisor): run %s FAILED (rc=%d, %s %s) after %d attempt(s); card NOT advanced. Log: %s. Tail: %s"
                      run_id rc category v.v_message attempts ctx.x_dir
                      (log_tail driver 300))));
      ignore (eval_ok st
                (run_finish_expr run_id "dead" category
                   (Printf.sprintf "rc=%d %s" rc v.v_message)));
      log "run %s: dead (%s) after %d attempt(s)" run_id category attempts;
      Hashtbl.remove st.ctxs run_id
    end
  end

(* ------------------------------------------------------------------ *)
(* Sidecar spawn (ONE pp-flow --run --supervised child per attempt)    *)
(* ------------------------------------------------------------------ *)

let spawn_sync (env : Eio_unix.Stdenv.base) (cmd : string) : int =
  let proc_mgr = Eio.Stdenv.process_mgr env in
  Eio.Switch.run (fun sw ->
      let proc = Eio.Process.spawn ~sw proc_mgr [ "/bin/sh"; "-c"; cmd ] in
      match Eio.Process.await proc with
      | `Exited c -> c
      | `Signaled _ -> 144)

let spawn_run (st : st) ~(run_id : string) ~(ctx : ctx) : unit =
  let cfg = st.cfg in
  let driver_log = Filename.concat ctx.x_dir "driver.log" in
  let runner =
    if cfg.runner = "" then ""
    else Printf.sprintf "PP_FLOW_PI=%s " (sh_quote cfg.runner)
  in
  let cmd =
    Printf.sprintf
      "cd %s && exec %s sh %s --run %s --supervised --brief-file %s --claimed-state %s --columns %s --log-dir %s >%s 2>&1"
      (sh_quote cfg.cwd) runner (sh_quote cfg.flow_script)
      (sh_quote ctx.x_card) (sh_quote ctx.x_brief)
      (sh_quote ctx.x_claimed_state) (sh_quote ctx.x_cols)
      (sh_quote ctx.x_dir) (sh_quote driver_log)
  in
  let child =
    { c_run = run_id; c_card = ctx.x_card; c_started = Unix.gettimeofday () }
  in
  Hashtbl.replace st.live run_id child;
  log "dispatch run %s card %s [%s] (attempt %d)" run_id ctx.x_card
    ctx.x_claimed_state ctx.x_attempts;
  Eio.Fiber.fork ~sw:st.sw (fun () ->
      let rc = spawn_sync st.sess.env cmd in
      (try harvest st ~child ~ctx ~rc
       with e -> log "harvest run %s failed: %s" run_id (Printexc.to_string e));
      Hashtbl.remove st.live run_id)

(* ------------------------------------------------------------------ *)
(* Queue fill + dispatch + reap                                        *)
(* ------------------------------------------------------------------ *)

let create_run (st : st) (card : Yojson.Safe.t) : unit =
  let id = jstr (jmember "id" card) in
  if id <> "" then begin
    let list_id = jstr (jmember "list_id" card) in
    let from_state = jstr (jmember "state" card) in
    let title = jstr (jmember "title" card) in
    match eval_ok st (run_new_expr st.cfg ~card:id ~list_id ~from_state) with
    | Some j ->
        let run_id = jstr j in
        if run_id <> "" then
          log "queued run %s for card %s [%s] %s" run_id id from_state title
        else log "run-new returned empty for card %s" id
    | None ->
        log "run-new failed for card %s — if the run datum type is missing, seed (workstreams/runs-init!) as admin on main once" id
  end

let park_run (st : st) (r : row) : unit =
  match eval_ok st (run_claim_expr st.cfg r.r_id) with
  | Some j when jstr (jmember "state" j) = "claimed" ->
      ignore (eval_ok st (unassign_expr r.r_card));
      ignore (eval_ok st
                (card_comment_expr r.r_card
                   (Printf.sprintf
                      "pp-flow(supervisor): run %s PARKED pre-dispatch — advisory budget exhausted (usage: requests=%d, input_tokens=%d; caps: %d/%d; usage is a self-report until provider-structured usage lands)"
                      r.r_id r.r_usage_req r.r_usage_in st.cfg.max_tokens
                      st.cfg.max_requests)));
      ignore (eval_ok st
                (run_finish_expr r.r_id "parked" "budget"
                   "advisory budget exhausted pre-dispatch"));
      log "parked run %s (advisory budget)" r.r_id
  | _ -> ()

let dispatch_queued (st : st) (r : row) : unit =
  let run_id = r.r_id in
  match eval_ok st (run_claim_expr st.cfg run_id) with
  | Some j when jstr (jmember "state" j) = "claimed" -> (
      match claim_facts st ~card:r.r_card with
      | None ->
          ignore (eval_ok st
                    (run_finish_expr run_id "failed" "claim-facts"
                       "claim-time eval failed"))
      | Some Facts_missing ->
          ignore (eval_ok st
                    (run_finish_expr run_id "canceled" "card-missing"
                       "card vanished before dispatch"))
      | Some (Facts_ok (cs, brief, cols, assignee)) ->
          if assignee <> "" && assignee <> "run:" ^ run_id then
            (* another driver holds the card lease: park for a human *)
            ignore (eval_ok st
                      (run_finish_expr run_id "parked" "card-leased"
                         (Printf.sprintf "assignee %s held the card at dispatch"
                            assignee)))
          else begin
            ignore (eval_ok st
                      (run_patch_expr run_id
                         [ ("brief_hash",
                            "md5:" ^ Digest.to_hex (Digest.string brief)) ]));
            (* mirror the claim onto the card (run:<run-id> shape, the
               session-lease amendment) so humans see it on the board *)
            match eval_ok st (assign_expr r.r_card ("run:" ^ run_id)) with
            | Some a when jstr a <> "" ->
                ignore (eval_ok st (run_start_expr run_id));
                ignore (eval_ok st
                          (run_step_expr st.cfg run_id ~state:"running"
                             ~attempts:1 ~error:""));
                let dir = Filename.concat st.cfg.log_root run_id in
                mkdir_p dir;
                let brief_src = Filename.concat dir "brief.md" in
                write_file brief_src brief;
                let ctx =
                  { x_run = run_id; x_card = r.r_card;
                    x_claimed_state = cs; x_cols = cols; x_brief = brief_src;
                    x_dir = dir; x_attempts = 1; x_wall_exceeded = false }
                in
                Hashtbl.replace st.ctxs run_id ctx;
                spawn_run st ~run_id ~ctx
            | Some _ ->
                ignore (eval_ok st
                          (run_finish_expr run_id "failed" "card-lease"
                             "claim lease write denied"))
            | None ->
                ignore (eval_ok st
                          (run_finish_expr run_id "failed" "card-lease"
                             "claim lease eval failed"))
          end)
  | Some _ -> () (* CAS lost: another supervisor claimed it, or it is gone *)
  | None -> () (* eval failed; the row is retried next tick *)

let take_over (st : st) (r : row) : unit =
  if Hashtbl.mem st.taken r.r_id then begin
    (* expired twice this boot: the re-dispatch died again — stop looping *)
    ignore (eval_ok st
              (run_finish_expr r.r_id "dead" "reap"
                 "claim expired twice after takeover"));
    log "run %s expired twice after takeover; finished dead" r.r_id
  end
  else begin
    Hashtbl.replace st.taken r.r_id ();
    (* THE two-supervisor moment the claim CAS design anticipates: adopt the
       row with a fresh lease. A live claim is NEVER stolen — this only runs
       for rows whose heartbeat deadline passed. *)
    ignore (eval_ok st
              (run_patch_expr r.r_id
                 [ ("claim_supervisor_id", st.cfg.supervisor_id);
                   ("claim_lease_ts", iso_now ());
                   ("claim_heartbeat_at", iso_now ());
                   ("claim_deadline", iso_after (ttl_s st.cfg));
                   ("runner_instance", st.cfg.instance_id) ]));
    if r.r_state = "claimed" then ignore (eval_ok st (run_start_expr r.r_id));
    let ctx =
      match Hashtbl.find_opt st.ctxs r.r_id with
      | Some ctx -> Some ctx
      | None -> (
          (* previous boot or foreign owner: re-render the claim facts and
             resume from the step pointer — the SAME step, no attempt
             burned (cheap lease, expensive state). *)
          match claim_facts st ~card:r.r_card with
          | Some (Facts_ok (cs, brief, cols, _assignee)) ->
              let dir = Filename.concat st.cfg.log_root r.r_id in
              mkdir_p dir;
              let brief_src = Filename.concat dir "brief.md" in
              write_file brief_src brief;
              ignore (eval_ok st
                        (run_patch_expr r.r_id
                           [ ("brief_hash",
                              "md5:" ^ Digest.to_hex (Digest.string brief)) ]));
              Some
                { x_run = r.r_id; x_card = r.r_card; x_claimed_state = cs;
                  x_cols = cols; x_brief = brief_src; x_dir = dir;
                  x_attempts = 1; x_wall_exceeded = false }
          | _ -> None)
    in
    match ctx with
    | Some ctx ->
        Hashtbl.replace st.ctxs r.r_id ctx;
        log "reap: taking over run %s (card %s, was %s, owner %s)" r.r_id
          r.r_card r.r_state r.r_sup;
        spawn_run st ~run_id:r.r_id ~ctx
    | None ->
        ignore (eval_ok st
                  (run_finish_expr r.r_id "failed" "takeover"
                     "could not re-render the claim facts"));
        log "reap: run %s takeover failed (claim facts unavailable)" r.r_id
  end

(* ------------------------------------------------------------------ *)
(* The tick (supervisor-tick 1-6, one scan per tick)                   *)
(* ------------------------------------------------------------------ *)

let tick (st : st) : unit =
  (* 1. HEARTBEAT every row we hold with a live sidecar; flag run-wall
        breaches (harvest decides timed_out — the sidecar's own adapter
        timeout bounds the process; no kill in phase 2). *)
  let now = Unix.gettimeofday () in
  Hashtbl.iter
    (fun run_id child ->
      ignore (eval_ok st (run_heartbeat_expr st.cfg run_id));
      if now -. child.c_started > float_of_int st.cfg.run_wall_s then
        match Hashtbl.find_opt st.ctxs run_id with
        | Some ctx -> ctx.x_wall_exceeded <- true
        | None -> ())
    st.live;
  match eval_json st.sess (runs_scan_expr ()) with
  | Error e -> log "scan failed: %s" e
  | Ok j -> (
      let rows =
        match j with `List items -> List.filter_map row_of_json items | _ -> []
      in
      (* 2. REAP expired claims -> takeover (never live ones). *)
      List.iter
        (fun r ->
          if
            (r.r_state = "claimed" || r.r_state = "running")
            && r.r_deadline <> ""
            && r.r_deadline < iso_now ()
            (* lexicographic, the run-expired? semantics *)
            && not (Hashtbl.mem st.live r.r_id)
          then take_over st r)
        rows;
      (* 3. FILL THE QUEUE: eligible cards minus cards with open runs. *)
      (match eval_json st.sess (queue_eligible_expr st.cfg) with
       | Error e -> log "queue scan failed: %s" e
       | Ok (`List cards) ->
           let open_cards = Hashtbl.create 16 in
           List.iter
             (fun r ->
               if not (terminal_run_state r.r_state) then
                 Hashtbl.replace open_cards r.r_card ())
             rows;
           (* POLICY-6: create runs in the canonical work order - priority
              rank (P0..P3, else 4), then position, then id - mirroring
              todo/item/prio-le? exactly. The PP-side prio-sorted moved out
              of the scan expression for eval budget (part 2d not yet
              seeded); the supervisor mirrors, never invents, the order. *)
           let prio_rank (s : string) : int =
             match s with "P0" -> 0 | "P1" -> 1 | "P2" -> 2 | "P3" -> 3 | _ -> 4
           in
           let card_key (c : Yojson.Safe.t) : int * int * string =
             let pr = jstr (jmember "priority" c) in
             let pos =
               match int_of_string_opt (jstr (jmember "position" c)) with
               | Some i -> i
               | None -> 0
             in
             (prio_rank pr, pos, jstr (jmember "id" c))
           in
             List.iter
               (fun c ->
                 let id = jstr (jmember "id" c) in
                 if id <> "" && not (Hashtbl.mem open_cards id) then begin
                   create_run st c;
                   (* the eligible list can carry the same card twice
                      (todo/item/active duplicates, 10-18) - register the
                      card as open so a second copy does not queue again *)
                   Hashtbl.replace open_cards id ()
                 end)
               (List.sort (fun a b -> compare (card_key a) (card_key b)) cards)
       | Ok _ -> ());
      (* 4. DISPATCH: due retries first, then queued rows by creation order
            (todo/item/queue owns the ORDER; creation order mirrors it —
            POLICY-6: the supervisor NEVER reorders), under the cap. *)
      let now = Unix.gettimeofday () in
      let due =
        Hashtbl.fold
          (fun run_id at acc -> if at <= now then run_id :: acc else acc)
          st.retry_at []
      in
      List.iter
        (fun run_id ->
          Hashtbl.remove st.retry_at run_id;
          match Hashtbl.find_opt st.ctxs run_id with
          | Some ctx when not (Hashtbl.mem st.live run_id) ->
              (match List.find_opt (fun r -> r.r_id = run_id) rows with
               | Some r when budget_exceeded st r -> park_run st r
               | _ -> spawn_run st ~run_id ~ctx)
          | _ -> ())
        due;
      let cap = st.cfg.concurrency - Hashtbl.length st.live in
      if cap > 0 then begin
        let queued =
          rows
          |> List.filter (fun r -> r.r_state = "queued")
          |> List.sort (fun a b -> compare a.r_created b.r_created)
        in
        let rec go n = function
          | _ when n <= 0 -> ()
          | [] -> ()
          | r :: tl ->
              if Hashtbl.mem st.live r.r_id then go n tl
              else if budget_exceeded st r then begin
                park_run st r;
                go n tl
              end
              else begin
                dispatch_queued st r;
                go (n - 1) tl
              end
        in
        go cap queued
      end;
        (* 6. PUBLISH: counts for the standup surface (stderr in phase 2).
              Active counts from the scan rows (active-only now); terminal
              counts OCaml-side from the cheap state-list eval over ALL run
              rows (PP-side dict arithmetic broke on missing keys). *)
        let active_count s =
          List.length (List.filter (fun r -> r.r_state = s) rows)
        in
        let tally =
          match eval_json st.sess (runs_tally_expr ()) with
          | Ok j -> (
              match j with
              | `List items -> Some (List.map jstr items)
              | _ -> None)
          | Error _ -> None
        in
        let count s =
          match List.find_opt (fun r -> r.r_state = s) rows with
          | Some _ -> active_count s
          | None -> (
              match tally with
              | Some states -> List.length (List.filter (fun t -> t = s) states)
              | None -> 0)
        in
      let summary =
        Printf.sprintf
          "runs: queued=%d claimed=%d running=%d succeeded=%d failed=%d timed_out=%d canceled=%d dead=%d parked=%d live-sidecars=%d"
          (count "queued") (count "claimed") (count "running")
          (count "succeeded") (count "failed") (count "timed_out")
          (count "canceled") (count "dead") (count "parked")
          (Hashtbl.length st.live)
      in
      if summary <> st.last_summary then begin
        st.last_summary <- summary;
        log "%s" summary
      end)

(* Boot adoption: rows our previous boot left claimed/running (the stable
   supervisor_id owns them) are taken over immediately — a supervisor
   restart must not strand a live gate for a full TTL. Expired foreign rows
   are adopted on the first regular tick. *)
let boot_pass (st : st) : unit =
  match eval_json st.sess (runs_scan_expr ()) with
  | Error e -> log "boot scan failed: %s" e
  | Ok j ->
      let rows =
        match j with `List items -> List.filter_map row_of_json items | _ -> []
      in
      List.iter
        (fun r ->
          if
            (r.r_state = "claimed" || r.r_state = "running")
            && r.r_sup = st.cfg.supervisor_id
            && not (Hashtbl.mem st.live r.r_id)
          then take_over st r)
        rows;
      let count s = List.length (List.filter (fun r -> r.r_state = s) rows) in
      log "boot: %d open runs (queued=%d claimed=%d running=%d)"
        (List.length (List.filter (fun r -> not (terminal_run_state r.r_state)) rows))
        (count "queued") (count "claimed") (count "running")

(* ------------------------------------------------------------------ *)
(* Entry point (Q6: rides wyo's existing glochid service; no filmstrip *)
(* dependency — register-app! is the UI half and stays phase 3)        *)
(* ------------------------------------------------------------------ *)

let run ~(env : Eio_unix.Stdenv.base) ~(sw : Eio.Switch.t) : unit =
  match Habitat.creds ~env with
  | None ->
      log "disabled: no habitat credentials (set GLOCHID_HABITAT_URL/USER/PASS or PP_URL/PP_USER/PP_PASS, or a PP_TOKEN Bearer)"
  | Some _ ->
      let cfg = config () in
      let sess = { env; mutex = Eio.Mutex.create () } in
      let st =
        { cfg; sess; sw; live = Hashtbl.create 4; ctxs = Hashtbl.create 8;
          retry_at = Hashtbl.create 4; taken = Hashtbl.create 4;
          last_summary = "" }
      in
      log
        "starting: tick=%ds concurrency=%d states=%s exclude=%s list=%s script=%s cwd=%s identity=%s instance=%s"
        cfg.tick_s cfg.concurrency cfg.states cfg.exclude_kinds cfg.list_id
        cfg.flow_script cfg.cwd cfg.identity cfg.instance_id;
      (try boot_pass st with e -> log "boot pass failed: %s" (Printexc.to_string e));
      let clock = Eio.Stdenv.clock env in
      while true do
        (try tick st with e -> log "tick failed: %s" (Printexc.to_string e));
        Eio.Time.sleep clock (float_of_int cfg.tick_s)
      done
