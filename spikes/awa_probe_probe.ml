(* Inspect the Awa API surface (compile-time; no run). *)
module SSH = Awa_mirage.Make (struct
  type error = [ `Msg of string ]
  let pp_error fmt (`Msg s) = Fmt.string fmt s
  type write_error = [ `Closed | error ]
  let pp_write_error fmt = function
    | `Closed -> Fmt.string fmt "closed"
    | #error as e -> pp_error fmt e
  type flow = unit
  let read _ = assert false
  let write _ _ = assert false
  let writev _ _ = assert false
  let close _ = assert false
  let shutdown _ _ = assert false
end) (struct let sleep_ns _ = assert false end)
(struct
  let elapsed_ns () = 0L
  let period_ns () = None
end)

(* Trigger type errors that print the actual API signature. *)
let _ = (fun (s : Awa.Server.t) -> s) ()
let _ = (fun (m : Awa.Ssh.message list) -> m) ()
let _ = (fun (k : Awa.Hostkey.priv) -> k) ()
let _ = (fun (a : Awa.Keys.authenticator) -> a) ()
let _ = (fun (u : Awa.Server.user) -> u) ()
