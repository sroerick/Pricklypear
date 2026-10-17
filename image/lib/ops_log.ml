(** Structured ops log ring — product reliability stream.

    Always mirrors to stderr (image.log). Bounded in-memory ring for
    (log-tail) / agent / analytics. See product-experience.reliability-work-queue. *)

type level = Info | Warn | Error | Fatal

type event = {
  ts : float;
  level : level;
  component : string;
  msg : string;
  context : (string * string) list;
  fingerprint : string;
}

let level_to_string = function
  | Info -> "info"
  | Warn -> "warn"
  | Error -> "error"
  | Fatal -> "fatal"

let level_of_string s =
  match String.lowercase_ascii (String.trim s) with
  | "info" -> Some Info
  | "warn" | "warning" -> Some Warn
  | "error" -> Some Error
  | "fatal" -> Some Fatal
  | _ -> None

let max_events = 5000
let ring : event Queue.t = Queue.create ()
let mutex = Mutex.create ()
let seq = ref 0

(** Normalize msg for fingerprinting: collapse runs of hex/uuid-ish tokens. *)
let normalize_msg (msg : string) : string =
  let buf = Buffer.create (String.length msg) in
  let n = String.length msg in
  let i = ref 0 in
  let is_hex c =
    (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')
  in
  while !i < n do
    let c = msg.[!i] in
    if is_hex c then begin
      let j = ref !i in
      while !j < n && (is_hex msg.[!j] || msg.[!j] = '-') do
        incr j
      done;
      let len = !j - !i in
      if len >= 8 then Buffer.add_string buf "<id>"
      else Buffer.add_substring buf msg !i len;
      i := !j
    end else begin
      Buffer.add_char buf c;
      incr i
    end
  done;
  let s = Buffer.contents buf in
  if String.length s > 160 then String.sub s 0 160 else s

let fingerprint_of ~component ~msg =
  component ^ "|" ^ normalize_msg msg

let push (ev : event) : unit =
  Mutex.lock mutex;
  (try
     Queue.add ev ring;
     incr seq;
     while Queue.length ring > max_events do
       ignore (Queue.take ring)
     done
   with ex -> Mutex.unlock mutex; raise ex);
  Mutex.unlock mutex

(** UTC ISO-8601 stamp for the stderr mirror line. The 2026-10-16 tuna
    forensics suffered from timestamp-less request logs (only boot lines
    carried any time), so every ops-log line now stamps itself. *)
let iso_utc (t : float) : string =
  let tm = Unix.gmtime t in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ"
    (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1) tm.Unix.tm_mday
    tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec
let emit ?(context = []) ~(level : level) ~(component : string)
    (msg : string) : event =
  let fp = fingerprint_of ~component ~msg in
  let ev =
    { ts = Unix.gettimeofday ()
    ; level
    ; component
    ; msg
    ; context
    ; fingerprint = fp
    }
  in
  push ev;
  let ctx =
    if context = [] then ""
    else
      " "
      ^ String.concat " "
          (List.map (fun (k, v) -> k ^ "=" ^ v) context)
  in
  Printf.eprintf "%s [%s] %s: %s%s\n%!"
    (iso_utc ev.ts) (level_to_string level) component msg ctx;
  ev

let info ?(context = []) ~component msg = emit ~context ~level:Info ~component msg
let warn ?(context = []) ~component msg = emit ~context ~level:Warn ~component msg
let error ?(context = []) ~component msg = emit ~context ~level:Error ~component msg
let fatal ?(context = []) ~component msg = emit ~context ~level:Fatal ~component msg

let tail ?(n = 50) ?level () : event list =
  Mutex.lock mutex;
  let all =
    try Queue.fold (fun acc e -> e :: acc) [] ring |> List.rev
    with ex -> Mutex.unlock mutex; raise ex
  in
  Mutex.unlock mutex;
  let filtered =
    match level with
    | None -> all
    | Some lv ->
        List.filter
          (fun e ->
            match lv, e.level with
            | Fatal, Fatal -> true
            | Error, (Error | Fatal) -> true
            | Warn, (Warn | Error | Fatal) -> true
            | Info, _ -> true
            | _ -> false)
          all
  in
  let len = List.length filtered in
  if len <= n then filtered
  else
    let rec drop k xs =
      if k <= 0 then xs else match xs with [] -> [] | _ :: t -> drop (k - 1) t
    in
    drop (len - n) filtered

let event_to_yojson (e : event) : Yojson.Safe.t =
  `Assoc
    [ ("ts", `Float e.ts)
    ; ("level", `String (level_to_string e.level))
    ; ("component", `String e.component)
    ; ("msg", `String e.msg)
    ; ("fingerprint", `String e.fingerprint)
    ; ( "context"
      , `Assoc (List.map (fun (k, v) -> (k, `String v)) e.context) )
    ]

let event_to_value (e : event) : Pricklypear_lisp.Ast.value =
  (* JSON string — dict-get works on VStr JSON in this image. *)
  Pricklypear_lisp.Ast.VStr (Yojson.Safe.to_string (event_to_yojson e))
