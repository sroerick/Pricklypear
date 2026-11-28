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

(* ---- sound: line rows, cue rows, and the memoized mix job ---- *)

(* A take id is derived from its row (shot + take number), so a line can name
   the read it uses without a second identity column. *)
let take_id (t : take) = Printf.sprintf "%s-%d" t.take_shot t.take_n

(* Audio takes are ordinary take rows: a mic recording, or a local/premium
   TTS backend (backend is "tts-<engine>"). Picture backends are not audio. *)
let is_audio_take (t : take) =
  let p = "tts-" in
  let n = String.length p in
  let b = t.take_backend in
  b = "mic" || (String.length b >= n && String.sub b 0 n = p)

(* A line binds dialogue text to a character on a shot; ln_take names the
   chosen audio take and "" means the line is still silent. The old read is
   never destroyed: choosing another take edits this one field. *)
type line = {
  ln_project : string;
  ln_shot : string;
  ln_ord : int;
  ln_character : string;
  ln_text : string;
  ln_notes : string;
  ln_take : string;
}

let line_exists (lns : line list) shot ord =
  List.exists (fun l -> l.ln_shot = shot && l.ln_ord = ord) lns

let line_upsert (lns : line list) ~project ~shot ~ord ~character ~text
    ?(notes = "") () =
  if line_exists lns shot ord then
    List.map
      (fun l ->
        if l.ln_shot = shot && l.ln_ord = ord then
          { l with ln_character = character; ln_text = text; ln_notes = notes }
        else l)
      lns
  else
    {
      ln_project = project;
      ln_shot = shot;
      ln_ord = ord;
      ln_character = character;
      ln_text = text;
      ln_notes = notes;
      ln_take = "";
    }
    :: lns

let lines_of_shot (lns : line list) shot =
  List.sort
    (fun a b -> compare a.ln_ord b.ln_ord)
    (List.filter (fun l -> l.ln_shot = shot) lns)

let lines_of_project (lns : line list) project =
  List.sort
    (fun a b -> compare (a.ln_shot, a.ln_ord) (b.ln_shot, b.ln_ord))
    (List.filter (fun l -> l.ln_project = project) lns)

(* The ADR loop: a line reads wrong -> a new take lands on the shot ->
   line-choose points the line at it -> the next bounce picks it up. *)
let line_choose (lns : line list) ~shot ~ord ~take =
  if not (line_exists lns shot ord) then
    invalid_arg "line-choose: no such line (shot + ord)";
  List.map
    (fun l ->
      if l.ln_shot = shot && l.ln_ord = ord then { l with ln_take = take }
      else l)
    lns

(* A cue is a bed or a spot hit placed on the cut's timeline in frames, with
   a gain in dB (negative to duck under dialogue). *)
type cue_mode = Bed | Hit

let string_of_cue_mode = function Bed -> "bed" | Hit -> "hit"

type cue = {
  cq_project : string;
  cq_cut : string;
  cq_ord : int;
  cq_mode : cue_mode;
  cq_asset : string;
  cq_tc_start : int;  (* timeline position, frames *)
  cq_dur : int;  (* frames *)
  cq_gain_db : int;
  cq_loop : bool;
}

let cue_add (cs : cue list) ~project ~cut ~ord ~mode ~asset ~tc_start ~dur
    ?(gain_db = 0) ?(loop = false) () =
  {
    cq_project = project;
    cq_cut = cut;
    cq_ord = ord;
    cq_mode = mode;
    cq_asset = asset;
    cq_tc_start = tc_start;
    cq_dur = dur;
    cq_gain_db = gain_db;
    cq_loop = loop;
  }
  :: cs

let cues_of_cut (cs : cue list) ~project ~cut =
  List.sort
    (fun a b -> compare a.cq_ord b.cq_ord)
    (List.filter
       (fun c -> c.cq_project = project && c.cq_cut = cut)
       cs)

(* The audio graph of a cut: line rows (dialogue) + cue rows (beds, hits).
   Mixing is a pure function of it, so its canonical print is the cache key. *)
type audio_graph = { ag_lines : line list; ag_cues : cue list }

let empty_audio_graph = { ag_lines = []; ag_cues = [] }

let canonical_cues (cs : cue list) =
  String.concat "\n"
    (List.map
       (fun c ->
         Printf.sprintf "%d|%s|%s|%d|%d|%d|%b" c.cq_ord
           (string_of_cue_mode c.cq_mode) c.cq_asset c.cq_tc_start c.cq_dur
           c.cq_gain_db c.cq_loop)
       cs)

let canonical_lines (lns : line list) =
  String.concat "\n"
    (List.map
       (fun l -> Printf.sprintf "%s|%d|%s" l.ln_shot l.ln_ord l.ln_take)
       lns)

let cue_canonical (g : audio_graph) =
  canonical_cues
    (List.stable_sort (fun a b -> compare a.cq_ord b.cq_ord) g.ag_cues)
  ^ "\n--\n"
  ^ canonical_lines
      (List.stable_sort
         (fun a b -> compare (a.ln_shot, a.ln_ord) (b.ln_shot, b.ln_ord))
         g.ag_lines)

let cue_hash ~profile (g : audio_graph) =
  Digestif.SHA256.(
    to_hex (digest_string (cue_canonical g ^ "\nprofile:" ^ profile)))

(* Presets ship with the package (code, not rows). *)
type mix_preset = {
  mp_name : string;
  mp_normalize_dialogue : bool;
  mp_bed_gain_db : int;
  mp_bus_compress : bool;
  mp_limiter : bool;
  mp_encode : string;
}

let mix_preset = function
  | "flat" ->
      {
        mp_name = "flat";
        mp_normalize_dialogue = false;
        mp_bed_gain_db = 0;
        mp_bus_compress = false;
        mp_limiter = false;
        mp_encode = "medium";
      }
  | "film" ->
      {
        mp_name = "film";
        mp_normalize_dialogue = true;
        mp_bed_gain_db = -12;
        mp_bus_compress = true;
        mp_limiter = true;
        mp_encode = "medium";
      }
  | _ ->
      {
        mp_name = "dailies";
        mp_normalize_dialogue = true;
        mp_bed_gain_db = -20;
        mp_bus_compress = false;
        mp_limiter = true;
        mp_encode = "veryfast";
      }

(* ---- mix-job: memoized bounce keyed on cue_hash ---- *)

type mix_job = {
  mj_project : string;
  mj_cut : string;
  mj_profile : string;
  mj_cue_hash : string;  (* cache key: hash of the canonical audio graph *)
  mj_state : string;  (* queued | running | done | failed *)
  mj_out_hash : string;
  mj_out_path : string;  (* relative to a media root *)
  mj_stems : string list;  (* stem paths, flat preset wants all of them *)
  mj_log : string;
}

(* An unchanged audio graph returns the finished job (cache hit); a chosen
   line take or a moved/regained cue changes cue_hash and mints a new job. *)
let mix_job_for ~project ~cut ~profile (g : audio_graph) (jobs : mix_job list) =
  let h = cue_hash ~profile g in
  match
    List.find_opt (fun j -> j.mj_cue_hash = h && j.mj_state = "done") jobs
  with
  | Some j -> (j, true)
  | None ->
      ( {
          mj_project = project;
          mj_cut = cut;
          mj_profile = profile;
          mj_cue_hash = h;
          mj_state = "queued";
          mj_out_hash = "";
          mj_out_path = "";
          mj_stems = [];
          mj_log = "";
        },
        false )

(* edit/dailies v2: a proxy mp4 of the chosen picture plus the lines and cues
   already in the graph — same take semantics as a mix, dailies preset. *)
let dailies ~project ~cut (g : audio_graph) (jobs : mix_job list) =
  let j, hit = mix_job_for ~project ~cut ~profile:"dailies" g jobs in
  let path =
    if j.mj_out_path <> "" then j.mj_out_path
    else Printf.sprintf "dailies/%s-%s.mp4" project cut
  in
  ({ j with mj_out_path = path }, hit)

let take_media_path (takes : take list) id =
  match List.find_opt (fun t -> take_id t = id) takes with
  | Some t -> t.take_media_path
  | None -> ""

(* ---- exports (v1): the cue sheet and the ffmpeg bounce script ---- *)

(* Human/agent readable sheet: cues in timeline order, then the lines and the
   media path of the read each one uses. *)
let cue_sheet ~project ~cut ~takes (g : audio_graph) =
  let cue_rows =
    List.map
      (fun c ->
        Printf.sprintf "%d,%s,%s,%d,%d,%d,%b" c.cq_ord
          (string_of_cue_mode c.cq_mode) c.cq_asset c.cq_tc_start c.cq_dur
          c.cq_gain_db c.cq_loop)
      (cues_of_cut g.ag_cues ~project ~cut)
  in
  let line_rows =
    List.map
      (fun l ->
        Printf.sprintf "%s,%d,%s,%s,%s,%s" l.ln_shot l.ln_ord l.ln_character
          l.ln_text l.ln_take (take_media_path takes l.ln_take))
      (lines_of_project g.ag_lines project)
  in
  String.concat "\n"
    ([
       Printf.sprintf "# pricklypear cue sheet project=%s cut=%s" project cut;
       Printf.sprintf "# cue_hash=%s" (cue_hash ~profile:"dailies" g);
       "ord,mode,asset,tc_start,dur,gain_db,loop";
     ]
    @ cue_rows
    @ [ "# lines"; "shot,ord,character,text,take,path" ]
    @ line_rows)
  ^ "\n"

(* A shell script the operator reviews and runs: chosen picture (MEDIA root)
   plus the dialogue takes and the cue bed/hits, dailies preset encode. *)
let bounce_script ~project ~cut ~takes (g : audio_graph) =
  let p = mix_preset "dailies" in
  let out = Printf.sprintf "dailies-%s-%s.mp4" project cut in
  let dialogue_inputs =
    List.filter_map
      (fun l ->
        let path = take_media_path takes l.ln_take in
        if path = "" then None
        else Some (Printf.sprintf "  -i \"$MEDIA/%s\" \\" path))
      (lines_of_project g.ag_lines project)
  in
  let cue_comments =
    List.map
      (fun c ->
        Printf.sprintf "# cue %s asset=%s tc=%d dur=%d gain=%ddB loop=%b"
          (string_of_cue_mode c.cq_mode) c.cq_asset c.cq_tc_start c.cq_dur
          c.cq_gain_db c.cq_loop)
      (cues_of_cut g.ag_cues ~project ~cut)
  in
  String.concat "\n"
    ([
       "#!/bin/sh";
       Printf.sprintf "# pricklypear edit/export-ffmpeg project=%s cut=%s"
         project cut;
       Printf.sprintf "# cue_hash=%s" (cue_hash ~profile:"dailies" g);
       Printf.sprintf "# profile=%s beds=%ddB limiter=%b encode=%s" p.mp_name
         p.mp_bed_gain_db p.mp_limiter p.mp_encode;
       "set -e";
       ": \"${MEDIA:=.}\"";
       Printf.sprintf "OUT=\"%s\"" out;
     ]
    @ cue_comments
    @ dialogue_inputs
    @ [
        "ffmpeg -y -i \"$MEDIA/picture.mov\" \\";
        "  -filter_complex \"[0:a]volume=1.0[aout]\" \\";
        "  -map 0:v -map \"[aout]\" \\";
        "  -c:v libx264 -preset " ^ p.mp_encode ^ " -crf 23 \\";
        "  -c:a aac -b:a 160k \"$OUT\"";
        "echo wrote \"$OUT\"";
      ])
  ^ "\n"

(* Write both exports for a cut under dir; returns the two paths. *)
let write_exports ~dir ~project ~cut ~takes (g : audio_graph) =
  let put name body =
    let path = Filename.concat dir name in
    let oc = open_out_bin path in
    output_string oc body;
    close_out oc;
    path
  in
  let sheet =
    put
      (Printf.sprintf "dailies-%s-%s.cue.csv" project cut)
      (cue_sheet ~project ~cut ~takes g)
  in
  let script =
    put
      (Printf.sprintf "dailies-%s-%s.ffmpeg.sh" project cut)
      (bounce_script ~project ~cut ~takes g)
  in
  (sheet, script)

(* ---- edl-and-export (v1): MLT XML + Blender VSE script, both on disk ----

   The EDL rows ARE the cut; every export is a derived artifact. Each row
   prints as one MLT producer carrying the row's own fields as
   pricklypear:* properties, so a GUI edit that comes back through
   reimport_mlt rebuilds the same edl_entry rows — and therefore the same
   edl_hash — unless a trim, a take or a transition really moved. *)

let find_sub hay needle =
  let nh = String.length hay and nn = String.length needle in
  let rec go i =
    if i + nn > nh then None
    else if String.sub hay i nn = needle then Some i
    else go (i + 1)
  in
  if nn = 0 then Some 0 else go 0

(* The exports are XML: project slugs, take ids and transitions go through
   the escaper, and come back through its inverse on re-import. *)
let xml_escape s =
  let b = Buffer.create (String.length s) in
  String.iter
    (fun c ->
      match c with
      | '&' -> Buffer.add_string b "&amp;"
      | '<' -> Buffer.add_string b "&lt;"
      | '>' -> Buffer.add_string b "&gt;"
      | '"' -> Buffer.add_string b "&quot;"
      | '\'' -> Buffer.add_string b "&apos;"
      | c -> Buffer.add_char b c)
    s;
  Buffer.contents b

let xml_unescape s =
  let b = Buffer.create (String.length s) in
  let n = String.length s in
  let rec go i =
    if i >= n then ()
    else if s.[i] <> '&' then (
      Buffer.add_char b s.[i];
      go (i + 1))
    else
      match find_sub (String.sub s i (n - i)) ";" with
      | Some j -> (
          let ent = String.sub s (i + 1) (j - 1) in
          let decoded =
            match ent with
            | "amp" -> Some '&'
            | "lt" -> Some '<'
            | "gt" -> Some '>'
            | "quot" -> Some '"'
            | "apos" -> Some '\''
            | _ -> None
          in
          match decoded with
          | Some c ->
              Buffer.add_char b c;
              go (i + j + 1)
          | None ->
              Buffer.add_char b '&';
              go (i + 1))
      | None ->
          Buffer.add_char b '&';
          go (i + 1)
  in
  go 0;
  Buffer.contents b

let xml_attr line name =
  match find_sub line (name ^ "=\"") with
  | None -> None
  | Some i -> (
      let start = i + String.length name + 2 in
      match String.index_from_opt line start '"' with
      | None -> None
      | Some j -> Some (String.sub line start (j - start)))

let xml_int_attr line name =
  match xml_attr line name with
  | None -> 0
  | Some v -> ( match int_of_string_opt v with Some n -> n | None -> 0)

(* An MLT producer is one EDL row: resource (content-hash media path),
   source in-point, duration and the row's own binding fields. *)
let mlt_xml ~project ~cut ?(fps = 24) ?(resolution = "1920x1080")
    ~(takes : take list) (rows : edl_entry list) =
  let rows = List.stable_sort (fun a b -> compare a.edl_ord b.edl_ord) rows in
  let width, height =
    match String.index_opt resolution 'x' with
    | Some i -> (
        let w = String.sub resolution 0 i in
        let h = String.sub resolution (i + 1) (String.length resolution - i - 1) in
        match (int_of_string_opt w, int_of_string_opt h) with
        | Some w, Some h when w > 0 && h > 0 -> (w, h)
        | _ -> (1920, 1080))
    | None -> (1920, 1080)
  in
  let row_dur e = max 1 e.edl_dur in
  (* a producer's `out` is inclusive: in + dur - 1 *)
  let out_of e = e.edl_tc_in + row_dur e - 1 in
  let producers =
    List.mapi
      (fun i e ->
        let path =
          match take_media_path takes e.edl_take with
          | "" -> "missing.mp4"
          | p -> p
        in
        String.concat "\n"
          [
            Printf.sprintf "  <producer id=\"producer%d\" in=\"%d\" out=\"%d\">"
              i e.edl_tc_in (out_of e);
            Printf.sprintf "    <property name=\"resource\">%s</property>"
              (xml_escape path);
            "    <property name=\"mlt_service\">avformat-novalidate</property>";
            Printf.sprintf
              "    <property name=\"pricklypear:project\">%s</property>"
              (xml_escape e.edl_project);
            Printf.sprintf "    <property name=\"pricklypear:cut\">%s</property>"
              (xml_escape e.edl_cut);
            Printf.sprintf
              "    <property name=\"pricklypear:ord\">%d</property>" e.edl_ord;
            Printf.sprintf
              "    <property name=\"pricklypear:take\">%s</property>"
              (xml_escape e.edl_take);
            Printf.sprintf
              "    <property name=\"pricklypear:dur\">%d</property>"
              (row_dur e);
            Printf.sprintf
              "    <property name=\"pricklypear:transition\">%s</property>"
              (xml_escape e.edl_transition);
            Printf.sprintf
              "    <property name=\"pricklypear:audio\">%s</property>"
              (xml_escape e.edl_audio);
            "  </producer>";
          ])
      rows
  in
  let entries =
    List.concat
      (List.mapi
         (fun i e ->
           let entry =
             Printf.sprintf "    <entry producer=\"producer%d\" in=\"%d\" out=\"%d\"/>"
               i e.edl_tc_in (out_of e)
           in
           if e.edl_transition = "cut" then [ entry ]
           else
             [
               entry;
               Printf.sprintf "    <!-- transition %s -->"
                 (xml_escape e.edl_transition);
             ])
         rows)
  in
  let total = List.fold_left (fun acc e -> acc + row_dur e) 0 rows in
  String.concat "\n"
    ([
       "<?xml version=\"1.0\" standalone=\"no\"?>";
       Printf.sprintf
         "<mlt LC_NUMERIC=\"C\" version=\"7.0.0\" title=\"pricklypear %s %s\" \
          producer=\"tractor0\">"
         (xml_escape project) (xml_escape cut);
       Printf.sprintf
         "  <profile description=\"pricklypear %s\" width=\"%d\" height=\"%d\" \
          progressive=\"1\" sample_aspect_num=\"1\" sample_aspect_den=\"1\" \
          display_aspect_num=\"%d\" display_aspect_den=\"%d\" \
          frame_rate_num=\"%d\" frame_rate_den=\"1\" colorspace=\"709\"/>"
         (xml_escape resolution) width height width height fps;
     ]
    @ producers
    @ [ "  <playlist id=\"playlist0\">" ]
    @ entries
    @ [
        "  </playlist>";
        Printf.sprintf "  <tractor id=\"tractor0\" in=\"0\" out=\"%d\">"
          (max 0 (total - 1));
        "    <track producer=\"playlist0\"/>";
        "  </tractor>";
        "</mlt>";
        "";
      ])

(* The re-import path: a GUI edit (trim, reorder, retake) lands in the MLT
   file, and the rows it carries are read back into edl_entry rows. *)
type mlt_row_builder = {
  mutable mb_project : string;
  mutable mb_cut : string;
  mutable mb_ord : int;
  mutable mb_take : string;
  mutable mb_tc_in : int;
  mutable mb_dur : int;
  mutable mb_transition : string;
  mutable mb_audio : string;
}

let xml_property line =
  match find_sub line "name=\"" with
  | None -> None
  | Some i -> (
      let start = i + 6 in
      match find_sub (String.sub line start (String.length line - start)) "\">" with
      | None -> None
      | Some j -> (
          let name = String.sub line start j in
          let vstart = start + j + 2 in
          let rest = String.sub line vstart (String.length line - vstart) in
          let vlen =
            match find_sub rest "</property>" with
            | Some k -> k
            | None -> String.length rest
          in
          Some (name, xml_unescape (String.sub rest 0 vlen))))

let reimport_mlt (xml : string) : edl_entry list =
  let rows = ref [] in
  let cur = ref None in
  let sub_eq t n p = String.length t >= n && String.sub t 0 n = p in
  List.iter
    (fun line ->
      let t = String.trim line in
      if sub_eq t 10 "<producer " then
        cur :=
          Some
            {
              mb_project = "";
              mb_cut = "";
              mb_ord = 0;
              mb_take = "";
              mb_tc_in = xml_int_attr t "in";
              mb_dur = 0;
              mb_transition = "";
              mb_audio = "";
            }
      else
        match !cur with
        | None -> ()
        | Some b ->
            if t = "</producer>" then (
              if b.mb_take <> "" then
                rows :=
                  {
                    edl_project = b.mb_project;
                    edl_cut = b.mb_cut;
                    edl_ord = b.mb_ord;
                    edl_take = b.mb_take;
                    edl_tc_in = b.mb_tc_in;
                    edl_dur = b.mb_dur;
                    edl_transition =
                      (if b.mb_transition = "" then "cut" else b.mb_transition);
                    edl_audio = b.mb_audio;
                  }
                  :: !rows;
              cur := None)
            else if sub_eq t 10 "<property " then
              match xml_property t with
              | None -> ()
              | Some (name, v) -> (
                  match name with
                  | "pricklypear:project" -> b.mb_project <- v
                  | "pricklypear:cut" -> b.mb_cut <- v
                  | "pricklypear:take" -> b.mb_take <- v
                  | "pricklypear:transition" -> b.mb_transition <- v
                  | "pricklypear:audio" -> b.mb_audio <- v
                  | "pricklypear:ord" -> (
                      match int_of_string_opt v with
                      | Some n -> b.mb_ord <- n
                      | None -> ())
                  | "pricklypear:dur" -> (
                      match int_of_string_opt v with
                      | Some n -> b.mb_dur <- n
                      | None -> ())
                  | _ -> ()))
    (String.split_on_char '\n' xml);
  List.stable_sort (fun a b -> compare a.edl_ord b.edl_ord) (List.rev !rows)

(* The Blender VSE build script for the same cut: one strip per row, in ord
   order, offset into the source by tc_in and held for dur frames. *)
let blender_script ~project ~cut ?(fps = 24) ~(takes : take list)
    (rows : edl_entry list) =
  let rows = List.stable_sort (fun a b -> compare a.edl_ord b.edl_ord) rows in
  let strip e =
    let path =
      match take_media_path takes e.edl_take with
      | "" -> "/missing.mp4"
      | p -> p
    in
    let dur = max 1 e.edl_dur in
    String.concat "\n"
      [
        Printf.sprintf
          "se.sequences.new_movie(name='t%d', filepath='%s', channel=1, \
           frame_start=frame)"
          e.edl_ord path;
        Printf.sprintf "strip = se.sequences_all['t%d']" e.edl_ord;
        Printf.sprintf "strip.frame_offset_start = %d" e.edl_tc_in;
        Printf.sprintf "strip.frame_final_duration = %d" dur;
        Printf.sprintf "# transition: %s" e.edl_transition;
        Printf.sprintf "frame += %d" dur;
      ]
  in
  String.concat "\n"
    ([
       "# pricklypear edit/export-blender (edl-and-export)";
       Printf.sprintf "# project=%s cut=%s" project cut;
       Printf.sprintf "# edl_hash=%s" (edl_hash rows);
       "# Load in Blender Text Editor and Run Script, or:";
       "#   blender -b -P this_script.py";
       "import bpy";
       "scene = bpy.context.scene";
       Printf.sprintf "scene.render.fps = %d" fps;
       "scene.sequence_editor_clear()";
       "se = scene.sequence_editor_create()";
       "frame = 1";
     ]
    @ List.map strip rows
    @ [ "print('pricklypear EDL loaded')"; "" ])

(* Write both cut exports under dir; returns the two paths. *)
let write_cut_exports ~dir ~project ~cut ?(fps = 24)
    ?(resolution = "1920x1080") ~(takes : take list) (rows : edl_entry list) =
  let put name body =
    let path = Filename.concat dir name in
    let oc = open_out_bin path in
    output_string oc body;
    close_out oc;
    path
  in
  let mlt =
    put
      (Printf.sprintf "%s-%s.mlt" project cut)
      (mlt_xml ~project ~cut ~fps ~resolution ~takes rows)
  in
  let script =
    put
      (Printf.sprintf "%s-%s.blender.py" project cut)
      (blender_script ~project ~cut ~fps ~takes rows)
  in
  (mlt, script)
