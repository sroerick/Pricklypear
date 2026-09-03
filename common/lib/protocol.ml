(** Pricklypear Common — Health-check protocol.

    JSON shape for GET /health on the image (branch, uptime, status).
    Used by reverse proxies, watchdogs, and smoke scripts — not a
    multi-process supervisor fleet. *)
open Yojson.Safe

type health_status =
  | H_unknown
  | H_healthy
  | H_unhealthy
  | H_dead

let health_status_to_string = function
  | H_unknown -> "unknown"
  | H_healthy -> "healthy"
  | H_unhealthy -> "unhealthy"
  | H_dead -> "dead"

let health_status_of_string = function
  | "unknown" -> H_unknown
  | "healthy" -> H_healthy
  | "unhealthy" -> H_unhealthy
  | "dead" -> H_dead
  | _ -> H_unknown

type health_response = {
  ok : bool;
  branch : string;          (* qualified name like "alice/main" *)
  uptime_seconds : int;     (* seconds since image boot *)
  (** Unix seconds at image boot (promote task 4, 2026-09-03). Proves WHICH
      release answers: the promote gate requires boot_epoch >= its restart
      stamp, so a pre-restart orphan still holding :18080 (healthy, old
      release — the 2026-09-03 prod incident) can no longer false-OK the
      health gate. 0 = unknown (old image without the field). *)
  boot_epoch : int;
  error : string option;    (* present if not ok *)
}

let health_response_to_json (h : health_response) : Yojson.Safe.t =
  `Assoc [
    ("ok", `Bool h.ok);
    ("branch", `String h.branch);
    ("uptime_seconds", `Int h.uptime_seconds);
    ("boot_epoch", `Int h.boot_epoch);
    ("error", match h.error with Some e -> `String e | None -> `Null);
  ]

let health_response_of_json (j : Yojson.Safe.t) : (health_response, string) result =
  try
    let assoc = match j with `Assoc a -> a | _ -> failwith "not assoc" in
    let ok = match List.assoc "ok" assoc with `Bool b -> b | _ -> failwith "ok" in
    let branch = match List.assoc "branch" assoc with `String s -> s | _ -> failwith "branch" in
    let uptime = match List.assoc "uptime_seconds" assoc with `Int n -> n | _ -> failwith "uptime" in
    (* Additive evolution: images older than 2026-09-03 omit boot_epoch;
       parse it as 0 (unknown) instead of failing, so new consumers can
       still read an old image's health JSON. *)
    let boot_epoch = match List.assoc_opt "boot_epoch" assoc with
      | Some (`Int n) -> n
      | _ -> 0 in
    let error = match List.assoc "error" assoc with
                | `String s -> Some s | `Null -> None | _ -> failwith "error" in
    Ok { ok; branch; uptime_seconds = uptime; boot_epoch; error }
  with Failure msg -> Error msg
