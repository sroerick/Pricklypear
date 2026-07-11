(** In-branch prefix packages: organize function names with `/`.

    Orthogonal to branch ownership. Full binding key remains the
    entire name string (e.g. "todo/list"). See image.packages. *)

let package_of_name (name : string) : string =
  match String.rindex_opt name '/' with
  | None -> ""
  | Some i -> String.sub name 0 i

let local_name (name : string) : string =
  match String.rindex_opt name '/' with
  | None -> name
  | Some i -> String.sub name (i + 1) (String.length name - i - 1)

let join_name ~(package : string) ~(local : string) : string =
  let package = String.trim package in
  let local = String.trim local in
  if package = "" then local
  else if local = "" then package
  else
    let pkg =
      if String.length package > 0 && package.[String.length package - 1] = '/'
      then String.sub package 0 (String.length package - 1)
      else package
    in
    pkg ^ "/" ^ local

(** Group (name, body, author) defs by package. Sorted package keys;
    root "" first. Within package, sort by full name. *)
let group_by_package
    (defs : (string * 'a * string) list)
    : (string * (string * 'a * string) list) list =
  let tbl : (string, (string * 'a * string) list) Hashtbl.t =
    Hashtbl.create 16
  in
  List.iter
    (fun ((name, _, _) as row) ->
      let pkg = package_of_name name in
      let prev = try Hashtbl.find tbl pkg with Not_found -> [] in
      Hashtbl.replace tbl pkg (row :: prev))
    defs;
  let pkgs =
    Hashtbl.fold (fun k _ acc -> k :: acc) tbl []
    |> List.sort (fun a b ->
           match a, b with
           | "", _ -> -1
           | _, "" -> 1
           | _ -> String.compare a b)
  in
  List.map
    (fun pkg ->
      let rows =
        Hashtbl.find tbl pkg
        |> List.sort (fun (a, _, _) (b, _, _) -> String.compare a b)
      in
      (pkg, rows))
    pkgs

let package_label (pkg : string) : string =
  if pkg = "" then "(root)" else pkg

(** URL-encode a function name for path use (encode / as %2F). *)
let name_encode (name : string) : string =
  let buf = Buffer.create (String.length name + 8) in
  String.iter
    (function
      | '/' -> Buffer.add_string buf "%2F"
      | ' ' -> Buffer.add_string buf "%20"
      | '(' -> Buffer.add_string buf "%28"
      | ')' -> Buffer.add_string buf "%29"
      | '&' -> Buffer.add_string buf "%26"
      | '=' -> Buffer.add_string buf "%3D"
      | c -> Buffer.add_char buf c)
    name;
  Buffer.contents buf

(** Decode path segment(s) after /edit/ back to a function name. *)
let name_decode (s : string) : string =
  (* reuse minimal decode for %2F and + *)
  let buf = Buffer.create (String.length s) in
  let len = String.length s in
  let rec loop i =
    if i >= len then Buffer.contents buf
    else
      match s.[i] with
      | '+' ->
          Buffer.add_char buf ' ';
          loop (i + 1)
      | '%' when i + 2 < len ->
          let hex = String.sub s (i + 1) 2 in
          (try
             let code = int_of_string ("0x" ^ hex) in
             Buffer.add_char buf (Char.chr code)
           with _ -> Buffer.add_char buf '%');
          loop (i + 3)
      | c ->
          Buffer.add_char buf c;
          loop (i + 1)
  in
  loop 0

let edit_href (name : string) : string = "/edit/" ^ name_encode name
let binding_href (name : string) : string = "/binding/" ^ name_encode name
let inspect_href (name : string) : string =
  "/inspect/binding/" ^ name_encode name
