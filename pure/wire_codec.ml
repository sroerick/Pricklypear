(* Shared wire codecs: the /api/eval error JSON shape (error_to_json /
   error_of_json — image and proxy rims link the SAME codec) and the
   curl -D http response header blob parser. Split from prims_pure.ml
   (i-1788671384). *)
open Pricklypear_lisp.Ast


(* ---- bumpers notices codec (borge/bumpers.borg notices-envelope) ----

   Notices travel OUT OF BAND on the /api/eval envelope: an additive
   "notices" field, {"ok","value"[, "error"]} + "notices":[...]. The
   bumpers plan milestone M1 (docs/plans/bumpers-m1-notices.md) lands
   ONLY the wire shape + codec + per-session buffer + pp_eval -q proof
   of the additive contract — NO notices are generated until M2+, so
   every "value" consumer is untouched by construction. Invariants kept
   here: (1) define's return value NEVER changes (notices are strictly
   out-of-band); (2) the field is additive-only — never rename/re-shape
   ok/value/error; (3) the request flag {"quiet": true} suppresses the
   field entirely, so a quiet request is wire-identical to an old
   client that never heard of notices. The same codec is linked by the
   image, the proxy rims (remote/), the lens, and pp-eval — one shape,
   the same reason the error codec is factored here. The notice
   CONCEPT (budget, kinds, taper) is owned by bumpers.borg; this file
   owns only the shape.

   A notice is a small dict: {kind, name?, effects?, text}. kind is one
   of define|route|tip|warn; name?/effects? carry the define/route name
   and the effect badge when relevant; text is the payload. *)

type notice = {
  kind : string;
  name : string option;
  effects : string option;
  text : string;
}

let notice_to_json (n : notice) : Yojson.Safe.t =
  let fields =
    [ ("kind", `String n.kind) ]
    @ (match n.name with Some s -> [ ("name", `String s) ] | None -> [])
    @ (match n.effects with
      | Some s -> [ ("effects", `String s) ]
      | None -> [])
    @ [ ("text", `String n.text) ]
  in
  `Assoc fields

let notices_to_json (ns : notice list) : Yojson.Safe.t =
  `List (List.map notice_to_json ns)

(** Inverse of [notice_to_json] (used by pp-eval / lens to render notices
    a peer sent us; also the round-trip witness in the pure test). A
    notice missing kind or text is malformed and rejected (None). *)
let notice_of_json (j : Yojson.Safe.t) : notice option =
  match j with
  | `Assoc fields ->
      let str k =
        match List.assoc_opt k fields with
        | Some (`String s) -> Some s
        | _ -> None
      in
      (match (str "kind", str "text") with
      | Some kind, Some text ->
          Some { kind; name = str "name"; effects = str "effects"; text }
      | _ -> None)
  | _ -> None

(** Compact one-line rendering, shared by every rim that prints notices
    to a person or agent (pp-eval stderr, the line REPL, the browser
    /repl muted line). Product-experience owns pretty HTML rendering;
    this is the minimal form so pp_eval / lens / line REPL emit
    IDENTICAL text for the same notice. *)
let notice_to_line (n : notice) : string =
  let head =
    match n.name with
    | Some nm -> (
        match n.effects with
        | Some e -> Printf.sprintf "%s %s [%s]" n.kind nm e
        | None -> Printf.sprintf "%s %s" n.kind nm)
    | None -> (
        match n.effects with
        | Some e -> Printf.sprintf "%s [%s]" n.kind e
        | None -> n.kind)
  in
  Printf.sprintf "%s: %s" head n.text


(* ---- http response header parsing (helper for http-get prim) ----

   curl -D writes headers like:
     HTTP/2 200\r\n  (or "HTTP/1.1 200 OK")
     content-type: text/html\r\n
   Returns a JSON object string with lowercased keys. *)

let http_parse_headers (blob : string) : string =
  let lines = String.split_on_char '\n' blob in
  let acc = ref [] in
  List.iter
    (fun line ->
      let line = String.trim line in
      if line = "" then ()
      else
        (match String.index_opt line ':' with
         | Some i ->
             let k = String.lowercase_ascii (String.trim (String.sub line 0 i)) in
             let v = String.trim (String.sub line (i + 1) (String.length line - i - 1)) in
             acc := (k, v) :: !acc
         | None ->
             (* status line; ignore — we capture status separately *)
             ()))
    lines;
  Yojson.Safe.to_string (`Assoc (List.map (fun (k, v) -> (k, `String v)) (List.rev !acc)))


(* Shared ERROR codec (borge/remote-eval.borg substrate).
   Factored out of image/lib/http_pages.ml api_eval so that the image
   and a proxy rim link the SAME codec: [error_to_json] serializes any
   [exn] to the /api/eval error JSON shape; [error_of_json] is the
   inverse — it re-raises the SAME [exn] the local rim would, so a
   remote client observes identical exceptions to an in-process eval.
   The shape is {ok:false, error, kind, name?, line?, col?} plus two
   additive round-trip fields ([raw_error], and [name] on Prim_error)
   that let the inverse reconstruct Prim_error / Reader_error exactly.
   An exn matching none of the known classes serializes with
   kind:"exn" and the Printexc text as [error] — the inverse maps it
   back as User_error(msg,"exn"), NOT Reader_error, so an arbitrary
   server failure (DB outage, Sys_error, …) is never misattributed as
   a parse failure (i-1788714788).
   NB: [json_to_value] maps JSON strings to VStr, so a deserialized
   value is never a VSymbol on its own — symbols are a read-side
   concern. *)
(** bumpers M6 (borge/bumpers.borg discovery-surfaces / sequencing): an
    optional "hint" field on the error envelope — discovery arrives at
    the moment it is wanted. KNOWN classes only; unknown classes carry
    none (a hint must be actionable or absent). Generated here in pure
    so the image and every proxy rim emit the SAME hint text:
      Unbound_var "a/b"                      -> (apropos "a/")      (find the namespace)
      Unbound_var "frobnic"                  -> (apropos "frobnic")  (spelling help)
      Prim_error (p, _, "bad-args")          -> (function-doc "p")    (prim docs)
    The hint is advisory — [error_of_json] does NOT replay it (it is not
    part of the exn being reconstructed; a peer re-reading our error
    keeps the hint text only if it inspects the raw JSON). The stem for
    an unbound var is its namespace prefix when namespaced (todo/add ->
    "todo/"), else the full name: apropos matches by substring, so
    (apropos "todo/") lists the whole namespace and (apropos "frobnic")
    surfaces the correct spelling. *) 
let error_hint (exn : exn) : string option =
  match exn with
  | Pricklypear_lisp.Ast.Interp_error (Pricklypear_lisp.Ast.Unbound_var name) ->
      let stem =
        match String.rindex_opt name '/' with
        | Some i -> String.sub name 0 (i + 1)
        | None -> name
      in
      Some (Printf.sprintf "(apropos \"%s\")" stem)
  | Pricklypear_lisp.Ast.Interp_error
      (Pricklypear_lisp.Ast.Prim_error (p, _, "bad-args")) ->
      Some (Printf.sprintf "(function-doc \"%s\")" p)
  | _ -> None

let error_to_json (exn : exn) : Yojson.Safe.t =
  match exn with
  | Pricklypear_lisp.Ast.Interp_error e ->
      let base =
        ref
          [ ("ok", `Bool false)
          ; ("error", `String (Pricklypear_lisp.Ast.pp_interp_error e))
          ; ("kind", `String (Pricklypear_lisp.Ast.interp_error_kind e)) ]
      in
      (match e with
       | Unbound_var name -> base := !base @ [ ("name", `String name) ]
       | Prim_error (name, rmsg, _) ->
           (* carry name + raw message so a proxy can re-raise the SAME exn *)
           base := !base @ [ ("name", `String name); ("raw_error", `String rmsg) ]
       | _ -> ());
      (match Pricklypear_lisp.Ast.interp_error_loc e with
       | Some (line, col) ->
           base := !base @ [ ("line", `Int line); ("col", `Int col) ]
       | None -> ());
      (* bumpers M6: optional "hint" field for known error classes;
         absent (not null) for unknown classes — additive-only. *)
      (match error_hint exn with
       | Some h -> base := !base @ [ ("hint", `String h) ]
       | None -> ());
      `Assoc !base
  | Pricklypear_lisp.Reader.Reader_error (m, line, col) ->
      `Assoc
        [ ("ok", `Bool false)
        ; ("error", `String (Printf.sprintf "%s (%d:%d)" m line col))
        ; ("kind", `String "parse_error")
        ; ("raw_error", `String m)
        ; ("line", `Int line)
        ; ("col", `Int col) ]
  | _ ->
      (* Generic server exception: the original constructor cannot
         cross the wire, but the kind tag must — without it the inverse
         degraded this shape to Reader_error(msg,0,0). *)
      `Assoc
        [ ("ok", `Bool false)
        ; ("error", `String (Printexc.to_string exn))
        ; ("kind", `String "exn") ]

(** Inverse of [error_to_json]. Returns an [exn] that, when raised by a
    proxy rim, is the same exception the local rim would raise. *)
let error_of_json (j : Yojson.Safe.t) : exn =
  let field l k = List.assoc_opt k l in
  let str l k =
    match field l k with Some (`String s) -> Some s | _ -> None
  in
  let int l k =
    match field l k with
    | Some (`Int n) -> Some n
    | Some (`Intlit s) -> (try Some (int_of_string s) with _ -> None)
    | _ -> None
  in
  match j with
  | `Assoc fields ->
      let kind = str fields "kind" in
      (match kind with
       | Some k when k <> "parse_error" ->
           let name = str fields "name" in
           let raw = str fields "raw_error" in
           let err = str fields "error" in
           let interp e = Pricklypear_lisp.Ast.Interp_error e in
           (match name, raw with
            | Some n, Some r ->
                interp (Prim_error (n, r, k))
            | Some n, None ->
                (match k with
                 | "unbound_var" -> interp (Unbound_var n)
                 | _ ->
                     (* Prim_error without raw_error (e.g. a legacy peer);
                        degrade to User_error carrying the name+kind so the
                        message still names the prim. *)
                     interp (User_error ("prim " ^ n ^ ": " ^ n, k)))
            | None, _ ->
                (match k with
                 | "budget_exceeded" -> interp Budget_exceeded
                 (* kind:"exn" (error_to_json's generic-exn shape): the
                    message is the Printexc text — the original exn
                    constructor is unreconstructable, but the failure is
                    NOT a parse failure, so re-raise as User_error with
                    the "exn" kind preserved. *)
                 | _ ->
                     interp
                       (User_error
                          (Option.value err ~default:"", k))))
       | _ ->
           (* parse_error: re-raise as Reader_error. A Parse_error also
              serializes to this shape; reconstruct as Reader_error (same
              parse failure, same wire). *)
           let m =
             match str fields "raw_error" with
             | Some m -> m
             | None -> Option.value (str fields "error") ~default:""
           in
           let line = Option.value (int fields "line") ~default:0 in
           let col = Option.value (int fields "col") ~default:0 in
           Pricklypear_lisp.Reader.Reader_error (m, line, col))
  | _ ->
      Pricklypear_lisp.Ast.Interp_error
        (Pricklypear_lisp.Ast.User_error
           (Yojson.Safe.to_string j, "unknown"))
