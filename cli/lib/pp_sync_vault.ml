(* Vault-shape rules for pp-sync (i-1788671331-d).

   The vault layout convention (borge/branching.borg LAYOUT CONVENTION,
   borge/notes.borg PACKAGE FILE LAYOUT) is ONE file per package family:
   libs/<pkg>/lib.pp is the bootstrap + main vault (seed input); non-main
   dumps belong in gitignored backups/<ref>/.

   Sub-prefix "packages" (notes/capture, todo/item, structed/list, …) are
   NOT packages: the package registry has no rows for them and boot seeds
   only top-level libs/<pkg>/lib.pp. They were pp-sync artifacts — package
   discovery inferred them from binding-name prefixes and pull-all
   exported them, splitting one family's defines across many stale files
   (~230 double-bookkept names across notes/todo/structed; already drifted:
   the notes/capture/add docstring lived only in the nested copy, and
   todo/item/assign + notes/outline/op-seq-* existed only in dead nested
   dumps). One family, one file, one diff. *)

(* The family (top-level package segment) of a binding or package name.
   "notes/capture/add" -> "notes"; "std" -> "std". *)
let package_family (name : string) : string =
  match String.index_opt name '/' with
  | Some i -> String.sub name 0 i
  | None -> name

(* The package list over one vault: every local package dir plus every
   live binding name's FAMILY (first path segment). Bare binding names
   (kernel prims, unprefixed REPL defines) carry no package evidence and
   are ignored — matching the old prefix-inference, which only fired on
   slashed names. Sorted, deduplicated. This is the duplicate-name guard
   per family: a binding name can only ever land in its ONE family file,
   so no name is bookkept twice. *)
let packages_of ~(dirs : string list) ~(bindings : string list) : string list =
  let pkgs = Hashtbl.create 32 in
  List.iter (fun d -> if d <> "" then Hashtbl.replace pkgs d ()) dirs;
  List.iter
    (fun n ->
      match String.index_opt n '/' with
      | Some _ -> Hashtbl.replace pkgs (package_family n) ()
      | None -> ())
    bindings;
  Hashtbl.fold (fun k () acc -> k :: acc) pkgs []
  |> List.sort_uniq String.compare

(* Operations that would re-create double bookkeeping are refused before
   any network call. Returns Some refusal-message for:
   - the (root) namespace (i-1788671623-1 precedent), and
   - sub-prefix names (notes/capture, …): not packages; their family file
     is the vault home, personal-branch dumps belong in backups/. *)
let vault_refusal (pkg : string) : string option =
  if pkg = "(root)" || pkg = "root" then
    Some
      "the (root) namespace is not a package; refused. Root-branch dumps \
       belong in backups/ via pp-sync snapshot"
  else if String.contains pkg '/' then
    let fam = package_family pkg in
    Some
      (Printf.sprintf
         "%S is a sub-prefix of package %S, not a package; refused. The \
          vault keeps ONE file per family (libs/%s/lib.pp — the seeded, \
          canonical home); personal-branch dumps belong in backups/ via \
          pp-sync snapshot --branch"
         pkg fam fam)
  else None
