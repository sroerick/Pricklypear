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
   slashed names. A name whose family is EMPTY (main binds a stray
   define literally named "/" — REPL debris) is ignored too, else
   discovery invents family "" → libs/_root (2026-10-02 dry-run).
   Sorted, deduplicated. This is the duplicate-name guard per family:
   a binding name can only ever land in its ONE family file, so no
   name is bookkept twice. *)
let packages_of ~(dirs : string list) ~(bindings : string list) : string list =
  let pkgs = Hashtbl.create 32 in
  List.iter (fun d -> if d <> "" then Hashtbl.replace pkgs d ()) dirs;
  List.iter
    (fun n ->
      match String.index_opt n '/' with
      | Some _ ->
          (match package_family n with
           | "" -> ()  (* stray "/" define: family would be _root *)
           | fam -> Hashtbl.replace pkgs fam ())
      | None -> ())
    bindings;
  Hashtbl.fold (fun k () acc -> k :: acc) pkgs []
  |> List.sort_uniq String.compare

(* Live rows that must NEVER materialize a vault file, even though the
   live side still binds them. Deletion of a define row from
   system/main has no supported primitive yet (unbind refuses on the
   inheritance root), so these stay live-but-unvaulted by policy:
     - "list": std's OLD shape. list/map|filter|foldl|range exist on main
       as duplicates of std's (which declares the same names); libs/list
       was deleted by hand twice (6ed8656 — kernel-form defines aborted
       seeding; 628416f — regressed duplicate restored by a pull-all).
       A snapshot/pull-all would resurrect it a third time. Delete the
       live rows when a guarded main-delete primitive exists. *)
let vault_banned (pkg : string) : string option =
  match pkg with
  | "list" ->
      Some
        "the 'list' package is std's old shape — its live names \
         (list/map, list/filter, ...) are duplicates of std's \
         (i-1788670156); libs/list is deliberately gone (deleted 6ed8656, \
         restored by a pull-all, re-deleted 628416f). It will stay \
         unvaulted until the live duplicate rows can be deleted from \
         system/main."
  | _ -> None

(* Operations that would re-create double bookkeeping are refused before
   any network call. Returns Some refusal-message for:
   - banned families (vault_banned above),
   - the (root) namespace (i-1788671623-1 precedent), and
   - sub-prefix names (notes/capture, ...): not packages; their family file
     is the vault home, personal-branch dumps belong in backups/. *)
let vault_refusal (pkg : string) : string option =
  match vault_banned pkg with
  | Some m -> Some m
  | None ->
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
