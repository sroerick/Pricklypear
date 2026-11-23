(** Pure parser for the [(package …)] manifest form.

    The form is a deploy/registry directive, not an expression:

      (package notes
        (version "0.3.0")
        (requires std html css outline tree structed roam)
        (provides notes/doc notes/page)
        (export notes/doc/get notes/doc/upsert))

    [version], [requires], [provides] and [contributes] are optional. A
    [requires] item may be a bare name or a versioned pair
    [(pkg ">=1.2")]; this slice records the package name and leaves the
    constraint for a later resolver. Unknown clauses are ignored so a
    newer manifest can load on an older host. *)

open Ast

(** Shell contribution. [Css]/[Nav]/[Home] name a zero-arg function
    (except a CSS value like notes/tree-css, which is used as-is). [App]
    is a launcher tile for the filmstrip. *)
type contribution =
  | Css of string
  | Nav of string
  | Home of string
  | App_tile of {
      glyph : string;
      name : string;
      summary : string;
      href : string;
    }

type t = {
  name : string;
  version : string option;
  requires : string list;
  provides : string list;
  exports : string list;
  contributions : contribution list;
}

let empty name =
  { name; version = None; requires = []; provides = []; exports = [];
    contributions = [] }

(** Stable identity for a contribution: a re-load replaces the same slot
    instead of appending a duplicate; a different package contributing the
    same function wins (last load). *)
let contribution_key = function
  | Css fn -> "css:" ^ fn
  | Nav fn -> "nav:" ^ fn
  | Home fn -> "home:" ^ fn
  | App_tile { name; href; _ } -> "app:" ^ name ^ ":" ^ href

(** Register a package's contributions. A re-load replaces same-key
    entries in place and drops contributions the new manifest no longer
    declares (a package can remove its own nav entry by reloading). *)
let register_contributions ~(package : string)
    (existing : (string * contribution) list)
    (cs : contribution list) : (string * contribution) list =
  let replace_or_append key item items =
    let rec go acc = function
      | [] -> List.rev (item :: acc)
      | (_p, c) :: rest when contribution_key c = key ->
          List.rev_append acc (item :: rest)
      | x :: rest -> go (x :: acc) rest
    in
    go [] items
  in
  let new_keys = List.map contribution_key cs in
  let kept =
    List.filter
      (fun (p, c) ->
         p <> package || List.mem (contribution_key c) new_keys)
      existing
  in
  List.fold_left
    (fun items c ->
       replace_or_append (contribution_key c) (package, c) items)
    kept cs

(** Function names for a css/nav/home kind, in registration order. *)
let functions_of_kind (kind : string)
    (items : (string * contribution) list) : string list =
  List.filter_map
    (fun (_pkg, c) ->
       match (c, kind) with
       | Css fn, "css" -> Some fn
       | Nav fn, "nav" -> Some fn
       | Home fn, "home" -> Some fn
       | _ -> None)
    items

(** Host-wide contribution registry.

    Contributions are manifest metadata, not per-session definitions: the
    boot seed evaluates each [(package …)] form in the BOOT session, while
    the browser/REPL sessions are separate [Session.t] values built later
    (per identity). Without a shared registry those sessions would see an
    empty list. The registry is idempotent and keyed by contribution, so a
    later load-library or reseed replaces its own slots instead of piling
    up duplicates. *)
let global_contributions : (string * contribution) list ref = ref []

let register_global ~(package : string) (cs : contribution list) : unit =
  global_contributions :=
    register_contributions ~package !global_contributions cs

let global_contribution_list () : (string * contribution) list =
  !global_contributions

(** Test/standalone seam: drop every registered contribution. Boot does
    not call this; tests that exercise the registry start from empty. *)
let reset_global_contributions () : unit = global_contributions := []

let valid_name (s : string) : bool =
  let n = String.length s in
  n > 0 && n <= 64
  && String.for_all
       (fun c ->
          (c >= 'a' && c <= 'z')
          || (c >= '0' && c <= '9')
          || c = '-' || c = '_')
       s

let rec name_of = function
  | Var s -> Some s
  | Lit (VStr s) -> Some s
  | Lit (VSymbol s) -> Some s
  | App (Var "quote", [ x ]) -> name_of x
  | _ -> None

(** A clause's item list: bare symbols/strings, a quoted symbol, or a
    versioned pair [(pkg ">=1")]. Returns the package component. *)
let rec item_name = function
  | Var s -> Some s
  | Lit (VStr s) -> Some s
  | Lit (VSymbol s) -> Some s
  | App (Var "quote", [ x ]) -> item_name x
  | App (Var s, _) -> Some s
  | _ -> None

let dedupe xs =
  let seen = Hashtbl.create (List.length xs) in
  List.filter
    (fun x ->
       if Hashtbl.mem seen x then false
       else (Hashtbl.replace seen x (); true))
    xs

let clause_items what xs =
  let rec go acc = function
    | [] -> Ok (List.rev acc)
    | x :: rest ->
        (match item_name x with
         | Some s -> go (s :: acc) rest
         | None ->
             Error
               (Printf.sprintf
                  "%s: expected a package/name symbol or string" what))
  in
  match go [] xs with
  | Ok xs -> Ok (dedupe xs)
  | Error e -> Error e

(** [(contributes (css f …) (nav f) (home f) (app "glyph" "name"
    "summary" "/href"))]. Unknown kinds are ignored so a newer manifest
    can load on an older host. *)
let parse_contributes (clauses : expr list)
    : (contribution list, string) result =
  let rec go acc = function
    | [] -> Ok (List.rev acc)
    | App (Var "css", xs) :: rest ->
        (match clause_items "contributes css" xs with
         | Ok names ->
             go (List.rev_append (List.map (fun n -> Css n) names) acc) rest
         | Error e -> Error e)
    | App (Var "nav", xs) :: rest ->
        (match clause_items "contributes nav" xs with
         | Ok names ->
             go (List.rev_append (List.map (fun n -> Nav n) names) acc) rest
         | Error e -> Error e)
    | App (Var "home", xs) :: rest ->
        (match clause_items "contributes home" xs with
         | Ok names ->
             go (List.rev_append (List.map (fun n -> Home n) names) acc) rest
         | Error e -> Error e)
    | App (Var "app", [ glyph; name; summary; href ]) :: rest ->
        (match (name_of glyph, name_of name, name_of summary, name_of href) with
         | Some glyph, Some name, Some summary, Some href ->
             go (App_tile { glyph; name; summary; href } :: acc) rest
         | _ ->
             Error
               "contributes app: expected four strings (glyph name summary href)")
    | _ :: rest -> go acc rest
  in
  go [] clauses

(** Every clause that is manifest metadata. [session.ml] and the standalone
    host skip these instead of evaluating them as calls. *)
let is_clause = function
  | App (Var ("export" | "version" | "requires" | "provides" | "contributes"
             | "doc"), _) -> true
  | _ -> false

let parse_form (e : expr) : (t, string) result =
  match e with
  | App (Var "package", []) -> Error "package: need a name"
  | App (Var "package", name_e :: clauses) ->
      (match name_of name_e with
       | None -> Error "package: name must be a symbol or string"
       | Some name ->
           let name = String.trim name in
           if name = "" then Error "package: empty name"
           else if name = "kernel" then Error "package: reserved name: kernel"
           else if not (valid_name name) then
             Error
               (Printf.sprintf
                  "package: invalid name %S (want [a-z0-9_-]{1,64})" name)
           else begin
             let rec loop (acc : t) = function
               | [] -> Ok acc
               | App (Var "version", [ v ]) :: rest ->
                   (match name_of v with
                    | Some version -> loop { acc with version = Some version } rest
                    | None -> Error "package: (version ...) needs a string")
               | App (Var "version", _) :: _ ->
                   Error "package: (version ...) takes one string"
               | App (Var "requires", xs) :: rest ->
                   (match clause_items "requires" xs with
                    | Ok requires ->
                        loop { acc with requires = dedupe (acc.requires @ requires) } rest
                    | Error e -> Error ("package: " ^ e))
               | App (Var "provides", xs) :: rest ->
                   (match clause_items "provides" xs with
                    | Ok provides ->
                        loop { acc with provides = dedupe (acc.provides @ provides) } rest
                    | Error e -> Error ("package: " ^ e))
               | App (Var "export", xs) :: rest ->
                   (match clause_items "export" xs with
                    | Ok exports ->
                        loop { acc with exports = dedupe (acc.exports @ exports) } rest
                    | Error e -> Error ("package: " ^ e))
               | App (Var "contributes", body) :: rest ->
                   (match parse_contributes body with
                    | Ok cs ->
                        loop
                          { acc with
                            contributions = acc.contributions @ cs }
                          rest
                    | Error e -> Error ("package: " ^ e))
               (* doc and unknown/future clauses are metadata, not calls *)
               | _ :: rest -> loop acc rest
             in
             loop (empty name) clauses
           end)
  | _ -> Error "not a (package ...) form"

(** Cheap guard: is there a [(package …)] form anywhere in the text?
    Allows whitespace between [(] and [package]; a comment or string match
    only costs a full parse, never correctness. *)
let has_package_form (src : string) : bool =
  let n = String.length src and m = String.length "package" in
  let is_space = function ' ' | '\t' | '\n' | '\r' -> true | _ -> false in
  let rec skip j = if j < n && is_space src.[j] then skip (j + 1) else j in
  let rec go i =
    if i >= n then false
    else if src.[i] = '(' then
      let j = skip (i + 1) in
      if j + m <= n && String.sub src j m = "package" then true else go (i + 1)
    else go (i + 1)
  in
  go 0

(** Find the first package form in a source file. [Ok None] means the file
    has no manifest (the common case for older packages). The cheap
    substring guard keeps the 60+ manifest-less libs from being re-parsed
    in full at every boot. *)
let parse_source (src : string) : (t option, string) result =
  if not (has_package_form src) then Ok None
  else
    try
      let forms = Reader.read_all src in
      match
        List.find_opt
          (function App (Var "package", _) -> true | _ -> false)
          forms
      with
      | None -> Ok None
      | Some form ->
          (match parse_form form with
           | Ok m -> Ok (Some m)
           | Error e -> Error e)
    with
    | Reader.Reader_error (m, l, c) ->
        Error (Printf.sprintf "reader: %s (%d:%d)" m l c)
