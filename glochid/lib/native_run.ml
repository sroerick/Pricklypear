(* The NATIVE RUNNER's contract (borge/run-primitive.borg, PHASE 3).

   Phase 2 runs one executor brief through a SIDECAR: the supervisor spawns
   scripts/pp-flow.sh --run <card> --supervised, which spawns a PP_FLOW_PI
   process (today the Autolith adapter), and the supervisor scrapes that
   process's stdout for PPFLOW-* verdict/usage lines. Phase 3 replaces the
   body with the harness: glochid runs the executor turn itself and writes ONE
   structured result file, so

     - usage is PROVIDER-STRUCTURED (the provider's own counts arrive through
       Loop.run's on_usage) instead of a self-report scraped out of terminal
       text, and
     - the step's verdict / destination / evidence arrive as DATA.

   What stays phase-2 by design, not by omission:
     - THE EXECUTOR NEVER TOUCHES THE BOARD: this module produces a RESULT;
       the supervisor owns every card write (one gate = one run = one
       process).
     - A FAIL verdict is a SUCCESSFUL STEP: the runner's exit code reports
       whether the step ran, never whether the card passed. Only a crash that
       leaves no result file is a step failure.
     - The no-verdict fail-safe (POLICY-3) stays supervisor-side: an empty
       verdict is reported faithfully and the supervisor decides. *)

type usage = {
  requests : int;       (* provider requests this step made *)
  input_tokens : int;   (* sum of usage.prompt_tokens *)
  cached_tokens : int;  (* sum of usage.prompt_tokens_details.cached_tokens *)
  output_tokens : int;  (* sum of usage.completion_tokens *)
  wall_s : float;
}

let zero_usage =
  { requests = 0; input_tokens = 0; cached_tokens = 0; output_tokens = 0;
    wall_s = 0. }

(** Fold ONE provider reply's usage into the step accumulator. One call per
    provider request, so [requests] is a count of real provider calls - never
    inferred, never scraped. *)
let add_usage (acc : usage) (u : Provider.usage) : usage =
  { requests = acc.requests + 1;
    input_tokens = acc.input_tokens + u.input_tokens;
    cached_tokens = acc.cached_tokens + u.cached_tokens;
    output_tokens = acc.output_tokens + u.output_tokens;
    wall_s = acc.wall_s }

type verdict = {
  v_verdict : string;      (* PASS | FAIL | ESCALATE | "" (none reported) *)
  v_destination : string;  (* target column; "" = one-column advance *)
  v_evidence : string;     (* the evidence line, verbatim *)
}

let no_verdict = { v_verdict = ""; v_destination = ""; v_evidence = "" }

(** Build one provider-usage record for [add_usage]. The wire type lives in the
    shared codec, so hosts construct it HERE instead of reaching across
    libraries for a record whose fields are not theirs. *)
let provider_usage ~(input_tokens : int) ~(cached_tokens : int)
    ~(output_tokens : int) : Provider.usage =
  { Pricklypear_common.Llm_codec.input_tokens; cached_tokens; output_tokens }
let string_member (j : Yojson.Safe.t) (k : string) : string =
  let open Yojson.Safe.Util in
  match j |> member k with
  | `String s -> String.trim s
  | _ -> ""

let int_of_json (j : Yojson.Safe.t) : int =
  match j with
  | `Int i -> i
  | `Intlit s -> (try int_of_string s with _ -> 0)
  | `Float f -> int_of_float f
  | _ -> 0

let float_of_json (j : Yojson.Safe.t) : float =
  match j with
  | `Float f -> f
  | `Int i -> float_of_int i
  | `Intlit s -> (try float_of_string s with _ -> 0.)
  | _ -> 0.

let int_member (j : Yojson.Safe.t) (k : string) : int =
  let open Yojson.Safe.Util in
  int_of_json (j |> member k)

let float_member (j : Yojson.Safe.t) (k : string) : float =
  let open Yojson.Safe.Util in
  float_of_json (j |> member k)

(** Parse the executor's answer. Two accepted shapes, in order:
    1. a JSON object carrying "verdict" (the executor role's output contract),
       bare or wrapped in prose; and
    2. the line contract pp-flow's briefs already ask for (VERDICT: /
       DESTINATION: / EVIDENCE:).
    TOTAL: never raises. An unparseable answer yields [no_verdict], which the
    supervisor's POLICY-3 fail-safe is responsible for (it fails SAFE toward a
    human; the runner never decides that). *)
let parse_verdict (text : string) : verdict =
  let try_object s =
    match Yojson.Safe.from_string s with
    | `Assoc _ as j -> Some j
    | _ -> None
    | exception _ -> None
  in
  let json_object_of_text t =
    let t = String.trim t in
    match try_object t with
    | Some j -> Some j
    | None -> (
        match (String.index_opt t '{', String.rindex_opt t '}') with
        | Some i, Some j when j > i ->
            try_object (String.sub t i (j - i + 1))
        | _ -> None)
  in
  let line_value prefix =
    let n = String.length prefix in
    let rec scan = function
      | [] -> ""
      | l :: rest ->
          let l = String.trim l in
          if String.length l >= n && String.sub l 0 n = prefix then
            String.trim (String.sub l n (String.length l - n))
          else scan rest
    in
    scan (String.split_on_char '\n' text)
  in
  match json_object_of_text text with
  | Some j when string_member j "verdict" <> "" ->
      { v_verdict = String.uppercase_ascii (string_member j "verdict");
        v_destination = string_member j "destination";
        v_evidence = string_member j "evidence" }
  | _ ->
      { v_verdict = String.uppercase_ascii (line_value "VERDICT:");
        v_destination = line_value "DESTINATION:";
        v_evidence = line_value "EVIDENCE:" }

type result = {
  r_schema : string;
  r_step_name : string;
  r_step_state : string;   (* done | failed | timed_out *)
  r_attempts : int;
  r_outcome : string;      (* succeeded | failed | no-verdict *)
  r_verdict : verdict;
  r_usage : usage;
  r_tool_total : int;
  r_tools : (string * int) list;
  r_turns : int;
  r_model : string;
  r_error_class : string;
  r_error_message : string;
  r_transcript_ref : string;
}

let schema = "pp-native-run/1"

let to_json (r : result) : Yojson.Safe.t =
  let u : usage = r.r_usage in
  let v : verdict = r.r_verdict in
  `Assoc
    [ ("schema", `String r.r_schema);
      ( "step",
        `Assoc
          [ ("name", `String r.r_step_name);
            ("state", `String r.r_step_state);
            ("attempts", `Int r.r_attempts) ] );
      ("outcome", `String r.r_outcome);
      ( "verdict",
        `Assoc
          [ ("verdict", `String v.v_verdict);
            ("destination", `String v.v_destination);
            ("evidence", `String v.v_evidence) ] );
      ( "usage",
        `Assoc
          [ ("requests", `Int u.requests);
            ("input_tokens", `Int u.input_tokens);
            ("cached_tokens", `Int u.cached_tokens);
            ("output_tokens", `Int u.output_tokens);
            ("wall_s", `Float u.wall_s) ] );
      ( "tools",
        `Assoc
          [ ("total", `Int r.r_tool_total);
            ("by_name", `Assoc (List.map (fun (k, n) -> (k, `Int n)) r.r_tools)) ] );
      ("turns", `Int r.r_turns);
      ("model", `String r.r_model);
      ( "error",
        `Assoc
          [ ("class", `String r.r_error_class);
            ("message", `String r.r_error_message) ] );
      ("transcript_ref", `String r.r_transcript_ref) ]

(** Decode a result file. None when the body is not an object at all, so the
    supervisor can tell "the runner never wrote a result" (a step failure)
    from "the runner reported something" (a verdict to act on). *)
let result_of_json (j : Yojson.Safe.t) : result option =
  let open Yojson.Safe.Util in
  match j with
  | `Assoc _ ->
      let step = j |> member "step" in
      let v = j |> member "verdict" in
      let u = j |> member "usage" in
      let e = j |> member "error" in
      let tools = j |> member "tools" in
      Some
        { r_schema = string_member j "schema";
          r_step_name = string_member step "name";
          r_step_state = string_member step "state";
          r_attempts = int_member step "attempts";
          r_outcome = string_member j "outcome";
          r_verdict =
            { v_verdict = string_member v "verdict";
              v_destination = string_member v "destination";
              v_evidence = string_member v "evidence" };
          r_usage =
            { requests = int_member u "requests";
              input_tokens = int_member u "input_tokens";
              cached_tokens = int_member u "cached_tokens";
              output_tokens = int_member u "output_tokens";
              wall_s = float_member u "wall_s" };
          r_tool_total = int_member tools "total";
          r_tools =
            (match tools |> member "by_name" with
             | `Assoc kv -> List.map (fun (k, x) -> (k, int_of_json x)) kv
             | _ -> []);
          r_turns = int_member j "turns";
          r_model = string_member j "model";
          r_error_class = string_member e "class";
          r_error_message = string_member e "message";
          r_transcript_ref = string_member j "transcript_ref" }
  | _ -> None

(** Convenience: the result's own step-state -> top-level run outcome, so the
    supervisor's harvest reads one field. *)
let outcome_of_state (step_state : string) (v : verdict) : string =
  if step_state <> "done" then "failed"
  else if v.v_verdict = "" then "no-verdict"
  else String.lowercase_ascii v.v_verdict

(** Classify a harness/provider failure message into the ratified retry classes
    (borge/run-primitive.borg resume-semantics 6 + POLICY-2): a TRANSIENT class
    may be re-dispatched (provider statuses 402/408/409/425/429 and 5xx, plus a
    crash that produced no reply at all); a DETERMINISTIC class attempts exactly
    once, because retrying a guaranteed failure only burns the lease. The loop's
    error TEXT is all this seam has, so the match is on the shapes the harness
    itself produces - the same rule pp-flow-autolith.sh applies to the adapter. *)
let classify_error (msg : string) : string =
  let contains needle =
    let n = String.length needle and m = String.length msg in
    let rec scan i = i + n <= m && (String.sub msg i n = needle || scan (i + 1)) in
    n = 0 || scan 0
  in
  let any needles = List.exists contains needles in
  if any [ "timeout"; "timed out" ] then "timeout"
  else if contains "max turns exceeded" then "deterministic"
  else if
    any
      [ "HTTP 402"; "HTTP 408"; "HTTP 409"; "HTTP 425"; "HTTP 429";
        "HTTP 500"; "HTTP 502"; "HTTP 503"; "HTTP 504" ]
  then "transient"
  else if
    any [ "HTTP 400"; "HTTP 401"; "HTTP 403"; "HTTP 404"; "HTTP 422" ]
  then "deterministic"
  else (* a crash with no reply is the executor's own retry case *)
    "transient"
