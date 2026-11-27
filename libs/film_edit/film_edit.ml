(* borge/film-edit.borg, subsection "model" — the studio data rows.

   asset / project / shot / shot-cast / shot-ref / take / edl-entry /
   render-job. Timecodes are integer frames; the project row carries fps.

   Media references are a content hash plus a path RELATIVE to a media
   root: motion masters never enter Postgres as bytes and never get an
   absolute path (the media boundary in borge/film-edit.borg). A render is
   a memoized pure function of the EDL rows: the canonical print of the
   cut hashes to the render-job cache key, and a take's input_snapshot is
   the canonical JSON of its expanded gen inputs, immutable once landed. *)

(* ---- type checks a row must pass, called by the writers ---- *)

let is_lower_hex c = (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f')

(* A content hash is 64 lowercase hex chars (sha256 of the file bytes). *)
let is_content_hash s =
  let n = String.length s in
  let rec go i = i >= n || (is_lower_hex s.[i] && go (i + 1)) in
  n = 64 && go 0

(* Masters and takes live on disk under a media root: the row holds the
   tail of the path, never an absolute path and never the bytes. *)
let is_relative_media_path p =
  let ok_seg s = s <> "" && s <> "." && s <> ".." in
  String.length p > 0
  && p.[0] <> '/'
  && (not (String.contains p ':'))
  && List.for_all ok_seg (String.split_on_char '/' p)

let media_pointer_ok ~hash ~path = is_content_hash hash && is_relative_media_path path

(* ---- Studio library (stable identity) ---- *)

type asset = {
  asset_project : string;  (* project slug, or "" studio-wide on this branch *)
  asset_slug : string;  (* "mara", "diner-ext", "theme-A" *)
  asset_kind : string;
      (* character | location | prop | style | voice | sfx | score |
         plate | still | other *)
  asset_summary : string;
  asset_bible : string;  (* long notes / backstory / art direction *)
  asset_appearance : string;  (* locked positive prompt fragment *)
  asset_negative : string;  (* locked negative fragment *)
  asset_primary_hash : string;  (* canonical still/sheet/sample content hash *)
  asset_primary_path : string;  (* relative to a media root *)
  asset_ref_hashes : string list;
      (* extra content hashes: turnarounds, costume variants *)
  asset_voice_ref : string;  (* voice id or kind=voice asset slug *)
  asset_lora_ref : string;  (* backend adapter id, or "" *)
  asset_media_id : string;
      (* optional habitat media_blobs UUID for UI thumbs only *)
}

(* ---- Project graph ---- *)

type project = {
  pr_slug : string;  (* "short-01" *)
  pr_title : string;
  pr_fps : int;
  pr_resolution : string;  (* "1920x1080" *)
  pr_budget_cents : int;  (* spend cap for API gen *)
  pr_notes : string;
}

type shot = {
  sh_project : string;
  sh_ord : int;  (* position in the cut *)
  sh_slug : string;
  sh_summary : string;  (* script beat *)
  sh_prompt : string;  (* ACTION/dialogue only; appearance comes from cast *)
  sh_dur_target : int;  (* frames, planned *)
}

(* who appears in the shot — a typed row, not a JSON bag *)
type shot_cast = {
  sc_shot : string;
  sc_asset : string;  (* asset id (usually kind=character) *)
  sc_wardrobe : string;  (* optional asset id, or "" *)
  sc_role : string;  (* hero | featured | background *)
}

(* non-cast reference bindings (typed, not a JSON hash list) *)
type shot_ref = {
  sr_shot : string;
  sr_asset : string;  (* location plate, style, prop hero, … *)
  sr_purpose : string;
      (* ip-adapter | style | depth | loc | prop | plate | other *)
  sr_weight : string;  (* backend hint, "" ok *)
}

(* ---- takes: one candidate clip (or still) for a shot ---- *)

type take_state = Pending | Done | Failed | Rejected

let string_of_take_state = function
  | Pending -> "pending"
  | Done -> "done"
  | Failed -> "failed"
  | Rejected -> "rejected"

type take = {
  take_shot : string;
  take_n : int;  (* take number *)
  take_backend : string;
      (* "fal-wan" | "replicate-ltx" | "local-comfy" | "camera" | "mic" | … *)
  take_model : string;  (* model/version string *)
  take_seed : string;
  take_cost_cents : int;  (* 0 for local/camera *)
  take_media_hash : string;  (* content hash of the file on disk *)
  take_media_path : string;  (* path RELATIVE to a media root *)
  take_state : take_state;
  take_rating : string;  (* "" | human note on review *)
  take_input_snapshot : string;
      (* canonical JSON of the expanded gen inputs at submit (prompt, asset
         ids, appearance fragments, ref hashes, model). Immutable after land:
         that is what makes a take reproducible and cacheable. *)
  take_source_assets : string list;  (* asset ids used *)
}

(* Land a take: attach the produced media pointer. Only a pending take may
   land, because landing freezes input_snapshot. *)
let land_take (t : take) ~media_hash ~media_path =
  if not (media_pointer_ok ~hash:media_hash ~path:media_path) then
    invalid_arg
      "land: a master is a content hash + relative path — no bytes, no \
       absolute paths";
  match t.take_state with
  | Pending ->
      {
        t with
        take_state = Done;
        take_media_hash = media_hash;
        take_media_path = media_path;
      }
  | _ -> invalid_arg "land: take already landed; input_snapshot is immutable"

(* Cost queries are closed questions over take rows: spend per project /
   cut / backend. The ledger is the take list, not a second table. *)
let spend_cents (ts : take list) =
  List.fold_left (fun acc t -> acc + t.take_cost_cents) 0 ts

let spend_by_backend (ts : take list) =
  let tbl = Hashtbl.create 8 in
  List.iter
    (fun t ->
      let prev = try Hashtbl.find tbl t.take_backend with Not_found -> 0 in
      Hashtbl.replace tbl t.take_backend (prev + t.take_cost_cents))
    ts;
  List.sort
    (fun (a, _) (b, _) -> compare a b)
    (Hashtbl.fold (fun k v acc -> (k, v) :: acc) tbl [])

(* ---- expansion: what input_snapshot stores ---- *)

let union_ref_hashes (assets : asset list) =
  let hs =
    List.concat_map (fun a -> a.asset_primary_hash :: a.asset_ref_hashes) assets
  in
  List.sort_uniq compare (List.filter (fun h -> h <> "") hs)

(* Canonical JSON: object keys sorted, so the same expanded bundle always
   prints the same bytes (the snapshot is a cache key, not prose). *)
let canonical_json (j : Yojson.Safe.t) =
  let rec go = function
    | `Assoc kvs ->
        let kvs = List.sort (fun (a, _) (b, _) -> compare a b) kvs in
        "{"
        ^ String.concat ","
            (List.map
               (fun (k, v) -> Yojson.Safe.to_string (`String k) ^ ":" ^ go v)
               kvs)
        ^ "}"
    | `List xs -> "[" ^ String.concat "," (List.map go xs) ^ "]"
    | j -> Yojson.Safe.to_string j
  in
  go j

(* Expand a shot's bindings: action prompt + each cast appearance (role
   kept) + ref asset hashes (primary + extras, unioned) + voice_ref. *)
let input_snapshot ~prompt ~model ~cast ~refs ~voice_ref =
  let cast =
    List.sort
      (fun (a, r) (b, s) -> compare (a.asset_slug, r) (b.asset_slug, s))
      cast
  in
  let refs = List.sort (fun a b -> compare a.asset_slug b.asset_slug) refs in
  canonical_json
    (`Assoc
      [
        ("prompt", `String prompt);
        ("model", `String model);
        ( "cast",
          `List
            (List.map
               (fun (a, role) ->
                 `Assoc
                   [
                     ("asset", `String a.asset_slug);
                     ("role", `String role);
                     ("appearance", `String a.asset_appearance);
                   ])
               cast) );
        ("ref_hashes", `List (List.map (fun h -> `String h) (union_ref_hashes refs)));
        ("voice_ref", `String voice_ref);
      ])

(* ---- edl-entry: the cut itself, one row per timeline segment ---- *)

type edl_entry = {
  edl_project : string;
  edl_cut : string;  (* cut label (branch-correlated) *)
  edl_ord : int;
  edl_take : string;
  edl_tc_in : int;  (* source in-point, frames *)
  edl_dur : int;
  edl_transition : string;  (* "cut" | "dissolve:N" | "fade:N" *)
  edl_audio : string;  (* take id of TTS/dialogue, or "" *)
}

(* The canonical print: rows in ord order, one line each. Hashing this is
   the render cache key, so re-cutting one shot changes one line. *)
let canonical_edl (rows : edl_entry list) =
  let sorted = List.stable_sort (fun a b -> compare a.edl_ord b.edl_ord) rows in
  String.concat "\n"
    (List.map
       (fun e ->
         Printf.sprintf "%s|%s|%d|%s|%d|%d|%s|%s" e.edl_project e.edl_cut
           e.edl_ord e.edl_take e.edl_tc_in e.edl_dur e.edl_transition
           e.edl_audio)
       sorted)

let edl_hash (rows : edl_entry list) =
  Digestif.SHA256.(to_hex (digest_string (canonical_edl rows)))

(* ---- render-job: memoized pure function of the cut ---- *)

type render_job = {
  rj_project : string;
  rj_cut : string;
  rj_edl_hash : string;  (* cache key: hash of the canonical EDL *)
  rj_state : string;  (* queued | running | done | failed *)
  rj_out_hash : string;  (* result media hash *)
  rj_out_path : string;  (* result relative to a media root *)
  rj_log : string;
}

let render_job_for ~project ~cut rows =
  {
    rj_project = project;
    rj_cut = cut;
    rj_edl_hash = edl_hash rows;
    rj_state = "queued";
    rj_out_hash = "";
    rj_out_path = "";
    rj_log = "";
  }

(* ---- studio library: asset rows + shot bindings, pure over rows ---- *)

(* The library is a set of rows, not a second schema: asset rows carry the
   stable identity (project + slug), shot-cast/shot-ref carry the bindings.
   Every operation below is a pure rewrite of those lists. *)
type library = {
  lib_assets : asset list;  (* most recently upserted first *)
  lib_cast : shot_cast list;
  lib_refs : shot_ref list;
}

let empty_library = { lib_assets = []; lib_cast = []; lib_refs = [] }

(* the asset id handed around by edit/shot-cast and edit/promote-take *)
let asset_id (a : asset) = a.asset_slug

let same_asset project slug (a : asset) =
  a.asset_project = project && a.asset_slug = slug

(* extra content hashes are not a media pointer of their own: they are
   hashes of files on the same media root, so checked the same way. *)
let asset_upsert lib ~project ~slug ~kind ?(summary = "") ?(bible = "")
    ?(appearance = "") ?(negative = "") ?(primary_hash = "")
    ?(primary_path = "") ?(ref_hashes = []) ?(voice_ref = "")
    ?(lora_ref = "") ?(media_id = "") () =
  if slug = "" then
    invalid_arg "asset-upsert: slug is the asset id (edit/asset-get slug)";
  if kind = "" then invalid_arg "asset-upsert: kind is required";
  if primary_hash <> "" || primary_path <> "" then
    if not (media_pointer_ok ~hash:primary_hash ~path:primary_path) then
      invalid_arg
        "asset-upsert: primary is a content hash + relative path — no bytes, \
         no absolute paths";
  if not (List.for_all is_content_hash ref_hashes) then
    invalid_arg "asset-upsert: ref_hashes are content hashes";
  let a =
    {
      asset_project = project;
      asset_slug = slug;
      asset_kind = kind;
      asset_summary = summary;
      asset_bible = bible;
      asset_appearance = appearance;
      asset_negative = negative;
      asset_primary_hash = primary_hash;
      asset_primary_path = primary_path;
      asset_ref_hashes = ref_hashes;
      asset_voice_ref = voice_ref;
      asset_lora_ref = lora_ref;
      asset_media_id = media_id;
    }
  in
  {
    lib with
    lib_assets =
      a :: List.filter (fun b -> not (same_asset project slug b)) lib.lib_assets;
  }

(* Default lookup: the caller's project row first, then the studio-wide ("")
   pack on this branch (a personal garden pack available to every project). *)
let asset_get ?(project = "") lib slug =
  match List.find_opt (same_asset project slug) lib.lib_assets with
  | Some a -> Some a
  | None ->
      if project = "" then None
      else List.find_opt (same_asset "" slug) lib.lib_assets

let assets ?project ?kind lib =
  List.filter
    (fun a ->
      (match project with None -> true | Some p -> a.asset_project = p)
      && match kind with None -> true | Some k -> a.asset_kind = k)
    lib.lib_assets

(* media_id is a UI-thumbnail pointer only; generation always reads the
   primary content hash + path from the media root. *)
let asset_gen_pointer (a : asset) = (a.asset_primary_hash, a.asset_primary_path)
let asset_thumb_media_id (a : asset) = a.asset_media_id

(* ---- shot bindings: typed rows, never a shot.refs JSON column ---- *)

let shot_cast lib ~shot ~asset ?(role = "featured") ?(wardrobe = "") () =
  let row =
    { sc_shot = shot; sc_asset = asset; sc_wardrobe = wardrobe; sc_role = role }
  in
  {
    lib with
    lib_cast =
      row
      :: List.filter
           (fun r -> not (r.sc_shot = shot && r.sc_asset = asset))
           lib.lib_cast;
  }

let shot_cast_clear lib shot =
  { lib with lib_cast = List.filter (fun r -> r.sc_shot <> shot) lib.lib_cast }

let shot_ref lib ~shot ~asset ?(purpose = "other") ?(weight = "") () =
  let row =
    { sr_shot = shot; sr_asset = asset; sr_purpose = purpose; sr_weight = weight }
  in
  {
    lib with
    lib_refs =
      row
      :: List.filter
           (fun r -> not (r.sr_shot = shot && r.sr_asset = asset))
           lib.lib_refs;
  }

let shot_ref_clear lib shot =
  { lib with lib_refs = List.filter (fun r -> r.sr_shot <> shot) lib.lib_refs }

(* "which shots must be re-generated if this asset changes" is a query over
   the binding rows, not a stored back-reference. *)
let which_shots_use_asset lib slug =
  let from_cast =
    List.filter_map
      (fun r ->
        if r.sc_asset = slug || r.sc_wardrobe = slug then Some r.sc_shot
        else None)
      lib.lib_cast
  in
  let from_refs =
    List.filter_map
      (fun r -> if r.sr_asset = slug then Some r.sr_shot else None)
      lib.lib_refs
  in
  List.sort_uniq compare (from_cast @ from_refs)

(* ---- expand: the shot's own action prompt + bound appearances/hashes ---- *)

type expanded = {
  exp_shot : string;
  exp_prompt : string;  (* action/dialogue only; appearance comes from cast *)
  exp_cast : (asset * string) list;  (* asset (appearance + wardrobe folded) * role *)
  exp_ref_hashes : string list;  (* union of the ref assets' hashes, sorted *)
  exp_voice_ref : string;
  exp_json : string;  (* canonical bundle: exactly what a take snapshots *)
}

let resolve_asset lib slug = List.find_opt (fun a -> a.asset_slug = slug) lib.lib_assets

(* a wardrobe row is an override fragment on the character's appearance,
   never a restatement of it in shot.prompt *)
let cast_appearance lib (r : shot_cast) =
  let part id =
    match resolve_asset lib id with Some a -> a.asset_appearance | None -> ""
  in
  match (part r.sc_asset, part r.sc_wardrobe) with
  | "", w -> w
  | a, "" -> a
  | a, w -> a ^ ", " ^ w

let expand ?(prompt = "") ?(model = "") lib shot =
  let cast_rows =
    List.sort (fun a b -> compare a.sc_asset b.sc_asset)
      (List.filter (fun r -> r.sc_shot = shot) lib.lib_cast)
  in
  let cast =
    List.filter_map
      (fun r ->
        match resolve_asset lib r.sc_asset with
        | None -> None
        | Some a ->
            Some ({ a with asset_appearance = cast_appearance lib r }, r.sc_role))
      cast_rows
  in
  let refs =
    List.filter_map
      (fun r ->
        if r.sr_shot = shot then resolve_asset lib r.sr_asset else None)
      lib.lib_refs
  in
  let voice_ref =
    List.fold_left
      (fun acc (a, _) -> if acc = "" then a.asset_voice_ref else acc)
      "" cast
  in
  {
    exp_shot = shot;
    exp_prompt = prompt;
    exp_cast = cast;
    exp_ref_hashes = union_ref_hashes refs;
    exp_voice_ref = voice_ref;
    exp_json = input_snapshot ~prompt ~model ~cast ~refs ~voice_ref;
  }

(* ---- promote-take: a landed take's media becomes a library asset ---- *)

(* A character sheet, a plate, a voice sample: the take already holds the
   content hash + relative path, so promoting mints an asset that points at
   the same file. Only a landed take has media to point at. *)
let promote_take ?(project = "") lib (t : take) ~kind ~slug () =
  if t.take_state <> Done then
    invalid_arg "promote-take: only a landed take has a media hash";
  if not (media_pointer_ok ~hash:t.take_media_hash ~path:t.take_media_path) then
    invalid_arg "promote-take: take media is not a content hash + relative path";
  let lib =
    asset_upsert lib ~project ~slug ~kind
      ~summary:(Printf.sprintf "from take %s-%d" t.take_shot t.take_n)
      ~primary_hash:t.take_media_hash ~primary_path:t.take_media_path ()
  in
  (lib, slug)
