(* film_edit — alcotest suite for borge/film-edit.borg "model".

   The load-bearing checks: every row carries its documented fields, take
   rows carry backend/model/seed/cost/media_hash/input_snapshot, edl-entry
   rows order a cut by tc_in/dur/transition (and hash to the render cache
   key), and a motion master is a content hash + relative path — never
   bytes, never an absolute path. *)

open Film_edit

let h64 c = String.make 64 c

let contains hay needle =
  let nh = String.length hay and nn = String.length needle in
  let rec go i =
    i + nn <= nh && (String.sub hay i nn = needle || go (i + 1))
  in
  nn = 0 || go 0

let asset ?(kind = "character") ?(project = "short-01") slug appearance hash =
  {
    asset_project = project;
    asset_slug = slug;
    asset_kind = kind;
    asset_summary = "";
    asset_bible = "";
    asset_appearance = appearance;
    asset_negative = "";
    asset_primary_hash = hash;
    asset_primary_path = "assets/" ^ slug ^ ".png";
    asset_ref_hashes = [];
    asset_voice_ref = "";
    asset_lora_ref = "";
    asset_media_id = "";
  }

let take ?(backend = "fal-wan") ?(cost = 0) ?(n = 1) ?(state = Pending) shot =
  {
    take_shot = shot;
    take_n = n;
    take_backend = backend;
    take_model = "wan-2.1";
    take_seed = "42";
    take_cost_cents = cost;
    take_media_hash = h64 'a';
    take_media_path = "takes/" ^ shot ^ "-" ^ string_of_int n ^ ".mp4";
    take_state = state;
    take_rating = "";
    take_input_snapshot = "{}";
    take_source_assets = [];
  }

let edl ?(dur = 24) ?(ord = 1) ?(transition = "cut") take =
  {
    edl_project = "short-01";
    edl_cut = "cut-a";
    edl_ord = ord;
    edl_take = take;
    edl_tc_in = 0;
    edl_dur = dur;
    edl_transition = transition;
    edl_audio = "";
  }

(* 1. library + graph rows: typed fields, cast/ref are rows not JSON. *)
let test_rows () =
  let mara = asset "mara" "a woman in her 40s, weathered" (h64 'a') in
  Alcotest.(check string) "character is an asset with kind=character"
    "character" mara.asset_kind;
  Alcotest.(check string) "primary is a content hash" (h64 'a')
    mara.asset_primary_hash;
  let proj =
    {
      pr_slug = "short-01";
      pr_title = "Short 01";
      pr_fps = 24;
      pr_resolution = "1920x1080";
      pr_budget_cents = 50_000;
      pr_notes = "";
    }
  in
  Alcotest.(check int) "project row carries fps" 24 proj.pr_fps;
  let shot =
    {
      sh_project = "short-01";
      sh_ord = 1;
      sh_slug = "mara-at-window";
      sh_summary = "she sees the car";
      sh_prompt = "turns from the window";
      sh_dur_target = 48;
    }
  in
  Alcotest.(check int) "shot ord is a row field" 1 shot.sh_ord;
  let cast =
    {
      sc_shot = "mara-at-window";
      sc_asset = "mara";
      sc_wardrobe = "";
      sc_role = "hero";
    }
  in
  let ref =
    {
      sr_shot = "mara-at-window";
      sr_asset = "diner-ext";
      sr_purpose = "loc";
      sr_weight = "0.6";
    }
  in
  Alcotest.(check string) "shot-cast binds an asset id" "mara" cast.sc_asset;
  Alcotest.(check string) "shot-ref is a typed row, not a JSON bag" "loc"
    ref.sr_purpose

(* 2. take rows: provenance fields + immutable expanded input_snapshot. *)
let test_take () =
  let mara = asset "mara" "weathered, 40s, grey coat" (h64 'a') in
  let snap =
    input_snapshot ~prompt:"turns from the window" ~model:"wan-2.1"
      ~cast:[ (mara, "hero") ] ~refs:[ mara ] ~voice_ref:""
  in
  let t = { (take "mara-at-window") with take_input_snapshot = snap } in
  Alcotest.(check string) "backend" "fal-wan" t.take_backend;
  Alcotest.(check string) "model" "wan-2.1" t.take_model;
  Alcotest.(check string) "seed" "42" t.take_seed;
  Alcotest.(check int) "cost_cents" 0 t.take_cost_cents;
  Alcotest.(check bool) "media_hash is a content hash" true
    (is_content_hash t.take_media_hash);
  Alcotest.(check bool) "input_snapshot carries the appearance fragment" true
    (contains snap "weathered, 40s, grey coat");
  Alcotest.(check bool) "input_snapshot carries the ref hash" true
    (contains snap (h64 'a'));
  Alcotest.(check string) "same bundle prints the same snapshot bytes" snap
    (input_snapshot ~prompt:"turns from the window" ~model:"wan-2.1"
       ~cast:[ (mara, "hero") ] ~refs:[ mara ] ~voice_ref:"");
  let other = { mara with asset_appearance = "different coat" } in
  Alcotest.(check bool) "a changed bundle changes the snapshot" false
    (snap
    = input_snapshot ~prompt:"turns from the window" ~model:"wan-2.1"
        ~cast:[ (other, "hero") ] ~refs:[ other ] ~voice_ref:"");
  let landed =
    land_take t ~media_hash:(h64 'b')
      ~media_path:"takes/mara-at-window-1.mp4"
  in
  Alcotest.(check string) "land sets the landed state" "done"
    (string_of_take_state landed.take_state);
  Alcotest.(check bool) "re-land is refused (snapshot freezes at land)" true
    (try
       ignore
         (land_take landed ~media_hash:(h64 'c')
            ~media_path:"takes/x.mp4");
       false
     with Invalid_argument _ -> true)

(* 3. edl-entry rows order a cut; the canonical print is the cache key. *)
let test_edl () =
  let e1 = edl ~ord:1 "take-a" in
  let e2 = edl ~ord:2 ~dur:36 ~transition:"dissolve:12" "take-b" in
  let key = edl_hash [ e1; e2 ] in
  Alcotest.(check string) "row order does not change the key" key
    (edl_hash [ e2; e1 ]);
  Alcotest.(check string) "canonical print orders by ord"
    "short-01|cut-a|1|take-a|0|24|cut|\nshort-01|cut-a|2|take-b|0|36|dissolve:12|"
    (canonical_edl [ e2; e1 ]);
  Alcotest.(check bool) "duration is part of the key" false
    (key = edl_hash [ e1; { e2 with edl_dur = 37 } ]);
  Alcotest.(check bool) "transition is part of the key" false
    (key = edl_hash [ e1; { e2 with edl_transition = "fade:12" } ]);
  let job = render_job_for ~project:"short-01" ~cut:"cut-a" [ e2; e1 ] in
  Alcotest.(check string) "render-job.edl_hash is the cut's canonical hash" key
    job.rj_edl_hash;
  Alcotest.(check string) "a fresh render job is queued" "queued"
    job.rj_state

(* 4. media boundary: masters are hashes + relative paths only. *)
let test_motion_masters () =
  let t = take "mara-at-window" in
  Alcotest.(check bool) "hash + relative path is a valid pointer" true
    (media_pointer_ok ~hash:t.take_media_hash ~path:t.take_media_path);
  Alcotest.(check bool) "an absolute path is refused" false
    (media_pointer_ok ~hash:(h64 'a') ~path:"/srv/media/takes/1.mp4");
  Alcotest.(check bool) "a parent-relative path is refused" false
    (media_pointer_ok ~hash:(h64 'a') ~path:"../escape/1.mp4");
  Alcotest.(check bool) "clip bytes are not a hash" false
    (media_pointer_ok ~hash:"RIFF____WAVEfmt " ~path:"takes/1.wav");
  Alcotest.(check bool) "a URL is not a media-root path" false
    (media_pointer_ok ~hash:(h64 'a') ~path:"s3://bucket/1.mp4")

(* 5. cost ledger: closed questions over take rows. *)
let test_costs () =
  let ts =
    [
      take ~backend:"fal-wan" ~cost:120 "s1";
      take ~backend:"fal-wan" ~cost:180 "s2";
      take ~backend:"camera" ~cost:0 "s3";
    ]
  in
  Alcotest.(check int) "total spend over the takes" 300 (spend_cents ts);
  Alcotest.(check (option int)) "spend per backend (fal-wan)" (Some 300)
    (List.assoc_opt "fal-wan" (spend_by_backend ts));
  Alcotest.(check (option int)) "spend per backend (camera)" (Some 0)
    (List.assoc_opt "camera" (spend_by_backend ts))

(* 6. studio library: asset rows, typed cast/ref bindings, expand. *)
let test_library () =
  let lib =
    asset_upsert empty_library ~project:"short-01" ~slug:"mara"
      ~kind:"character" ~summary:"hero" ~bible:"grew up on the coast"
      ~appearance:"weathered, 40s" ~negative:"no hat"
      ~primary_hash:(h64 'a') ~primary_path:"assets/mara.png"
      ~ref_hashes:[ h64 'b' ] ~voice_ref:"mara-voice" ()
  in
  let lib =
    asset_upsert lib ~project:"" ~slug:"diner-ext" ~kind:"location"
      ~appearance:"neon diner exterior" ~primary_hash:(h64 'c')
      ~primary_path:"assets/diner-ext.png" ()
  in
  let lib =
    asset_upsert lib ~project:"" ~slug:"theme-A" ~kind:"style"
      ~appearance:"high-contrast neon" ~primary_hash:(h64 'e')
      ~primary_path:"assets/theme-A.png" ~ref_hashes:[ h64 'a' ] ()
  in
  (match asset_get ~project:"short-01" lib "mara" with
  | Some a ->
      Alcotest.(check string) "asset-upsert keeps kind=character" "character"
        a.asset_kind;
      Alcotest.(check string) "asset-upsert keeps the appearance" "weathered, 40s"
        a.asset_appearance;
      Alcotest.(check (list string)) "asset-upsert keeps ref_hashes" [ h64 'b' ]
        a.asset_ref_hashes;
      Alcotest.(check string) "media_id is optional (UI thumbs only)" ""
        a.asset_media_id;
      Alcotest.(check string) "gen reads primary_hash on disk" (h64 'a')
        (fst (asset_gen_pointer a))
  | None -> Alcotest.fail "asset-get did not find the project asset");
  Alcotest.(check bool) "default lookup falls back to studio-wide" true
    (asset_get ~project:"short-01" lib "diner-ext" <> None);
  Alcotest.(check int) "assets ?kind filters rows" 1
    (List.length (assets ~project:"short-01" ~kind:"character" lib));
  let lib = shot_cast lib ~shot:"mara-at-window" ~asset:"mara" ~role:"hero" () in
  let lib =
    shot_ref lib ~shot:"mara-at-window" ~asset:"diner-ext" ~purpose:"loc"
      ~weight:"0.6" ()
  in
  let lib =
    shot_ref lib ~shot:"mara-at-window" ~asset:"theme-A" ~purpose:"style" ()
  in
  Alcotest.(check (list string))
    "which-shots-use-asset is a rows query over cast and ref" [ "mara-at-window" ]
    (which_shots_use_asset lib "diner-ext");
  let bundle =
    expand ~prompt:"turns from the window" ~model:"wan-2.1" lib "mara-at-window"
  in
  Alcotest.(check bool) "expand carries the action prompt" true
    (contains bundle.exp_json "turns from the window");
  Alcotest.(check bool) "expand carries the cast appearance" true
    (contains bundle.exp_json "weathered, 40s");
  Alcotest.(check (list string)) "expand unions the ref hashes"
    [ h64 'a'; h64 'c'; h64 'e' ] bundle.exp_ref_hashes;
  Alcotest.(check string) "expand carries the cast voice_ref" "mara-voice"
    bundle.exp_voice_ref;
  let lib = shot_cast_clear lib "mara-at-window" in
  Alcotest.(check (list string)) "shot-cast-clear drops the bindings" []
    (which_shots_use_asset lib "mara")

(* 7. promote-take mints an asset from a landed take's media_hash. *)
let test_promote () =
  let t =
    land_take (take "mara-at-window") ~media_hash:(h64 'd')
      ~media_path:"takes/mara-at-window-1.mp4"
  in
  let lib, id = promote_take empty_library t ~kind:"still" ~slug:"mara-sheet" () in
  Alcotest.(check string) "promote-take returns the asset id" "mara-sheet" id;
  (match asset_get lib "mara-sheet" with
  | Some a ->
      Alcotest.(check string) "promoted kind" "still" a.asset_kind;
      Alcotest.(check string) "primary_hash is the take media hash" (h64 'd')
        a.asset_primary_hash;
      Alcotest.(check string) "primary_path is the take media path"
        "takes/mara-at-window-1.mp4" a.asset_primary_path
  | None -> Alcotest.fail "promote-take did not mint an asset");
  Alcotest.(check bool) "a pending take cannot be promoted" true
    (try
       ignore (promote_take empty_library (take "s1") ~kind:"still" ~slug:"x" ());
       false
     with Invalid_argument _ -> true)

(* 8. sound: line rows, cue rows, exports and the memoized mix job. *)

let audio_take ?(backend = "tts-piper") ?(n = 1) shot =
  land_take
    { (take ~backend ~n shot) with take_model = "piper-1" }
    ~media_hash:(h64 'e')
    ~media_path:("takes/" ^ shot ^ "-" ^ string_of_int n ^ ".wav")

let read_file p =
  let ic = open_in_bin p in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let test_sound () =
  let tts = audio_take "mara-at-window" in
  let mic = audio_take ~backend:"mic" ~n:2 "mara-at-window" in
  Alcotest.(check bool) "a tts-* take is an audio take" true
    (is_audio_take tts);
  Alcotest.(check bool) "a mic take is an audio take" true (is_audio_take mic);
  Alcotest.(check bool) "a picture take is not an audio take" false
    (is_audio_take (take "mara-at-window"));
  Alcotest.(check string) "a take id is derived from its row" "mara-at-window-2"
    (take_id mic);
  let g = empty_audio_graph in
  let g =
    {
      g with
      ag_lines =
        line_upsert g.ag_lines ~project:"short-01" ~shot:"mara-at-window"
          ~ord:1 ~character:"mara" ~text:"you came back" ();
    }
  in
  let g =
    {
      g with
      ag_lines =
        line_upsert g.ag_lines ~project:"short-01" ~shot:"mara-at-window"
          ~ord:2 ~character:"mara" ~text:"i never left" ~notes:"soft" ();
    }
  in
  Alcotest.(check int) "line rows bind text to a shot" 2
    (List.length (lines_of_shot g.ag_lines "mara-at-window"));
  let l1 = List.hd (lines_of_shot g.ag_lines "mara-at-window") in
  Alcotest.(check string) "a line keeps its character" "mara" l1.ln_character;
  Alcotest.(check string) "a fresh line is silent (no chosen take)" "" l1.ln_take;
  let rewritten =
    line_upsert g.ag_lines ~project:"short-01" ~shot:"mara-at-window" ~ord:1
      ~character:"mara" ~text:"you came back?" ()
  in
  Alcotest.(check int) "line-upsert on shot+ord rewrites, never duplicates" 2
    (List.length rewritten);
  let g =
    {
      g with
      ag_lines =
        line_choose g.ag_lines ~shot:"mara-at-window" ~ord:1
          ~take:(take_id tts);
    }
  in
  Alcotest.(check string) "line-choose picks the audio take" (take_id tts)
    (List.hd (lines_of_shot g.ag_lines "mara-at-window")).ln_take;
  Alcotest.(check bool) "choosing never destroys the other read" true
    (List.exists (fun t -> take_id t = take_id mic) [ tts; mic ]);
  let g =
    {
      g with
      ag_cues =
        cue_add g.ag_cues ~project:"short-01" ~cut:"cut-a" ~ord:1 ~mode:Bed
          ~asset:"theme-A" ~tc_start:0 ~dur:240 ~gain_db:(-20) ~loop:true ();
    }
  in
  let g =
    {
      g with
      ag_cues =
        cue_add g.ag_cues ~project:"short-01" ~cut:"cut-a" ~ord:2 ~mode:Hit
          ~asset:"door-slam" ~tc_start:48 ~dur:6 ~gain_db:(-3) ();
    }
  in
  let cs = cues_of_cut g.ag_cues ~project:"short-01" ~cut:"cut-a" in
  Alcotest.(check int) "cue rows place beds and hits on the timeline" 2
    (List.length cs);
  let hit_cue = List.nth cs 1 in
  Alcotest.(check int) "a cue carries gain_db" (-3) hit_cue.cq_gain_db;
  Alcotest.(check string) "cue mode prints as the row mode" "hit"
    (string_of_cue_mode hit_cue.cq_mode);
  let job, fresh_hit =
    mix_job_for ~project:"short-01" ~cut:"cut-a" ~profile:"dailies" g []
  in
  Alcotest.(check bool) "a fresh mix job is not a cache hit" false fresh_hit;
  Alcotest.(check string) "a fresh mix job is queued" "queued" job.mj_state;
  let done_job =
    {
      job with
      mj_state = "done";
      mj_out_hash = h64 'f';
      mj_out_path = "mixes/short-01-cut-a.wav";
      mj_stems = [ "mixes/dialogue.wav" ];
    }
  in
  let _, same_hit =
    mix_job_for ~project:"short-01" ~cut:"cut-a" ~profile:"dailies" g
      [ done_job ]
  in
  Alcotest.(check bool) "unchanged audio graph returns a cache hit" true
    same_hit;
  let g_mic =
    {
      g with
      ag_lines =
        line_choose g.ag_lines ~shot:"mara-at-window" ~ord:1
          ~take:(take_id mic);
    }
  in
  Alcotest.(check bool) "changing a line take changes cue_hash" false
    (cue_hash ~profile:"dailies" g = cue_hash ~profile:"dailies" g_mic);
  let _, mic_hit =
    mix_job_for ~project:"short-01" ~cut:"cut-a" ~profile:"dailies" g_mic
      [ done_job ]
  in
  Alcotest.(check bool) "a changed line take misses the cache" false mic_hit;
  let g_cue =
    {
      g with
      ag_cues =
        cue_add g.ag_cues ~project:"short-01" ~cut:"cut-a" ~ord:2 ~mode:Hit
          ~asset:"door-slam" ~tc_start:96 ~dur:6 ~gain_db:(-3) ();
    }
  in
  Alcotest.(check bool) "moving a cue changes cue_hash" false
    (cue_hash ~profile:"dailies" g = cue_hash ~profile:"dailies" g_cue);
  Alcotest.(check bool) "the profile is part of the cache key" false
    (cue_hash ~profile:"dailies" g = cue_hash ~profile:"flat" g);
  let dj, dailies_hit =
    dailies ~project:"short-01" ~cut:"cut-a" g []
  in
  Alcotest.(check bool) "dailies v2 is a mix job on the dailies preset" false
    dailies_hit;
  Alcotest.(check string) "dailies bounces to a proxy mp4"
    "dailies/short-01-cut-a.mp4" dj.mj_out_path;
  let dir =
    let d = Filename.concat (Filename.get_temp_dir_name ()) "film-edit-exports" in
    (try Sys.mkdir d 0o755 with Sys_error _ -> ());
    d
  in
  let sheet, script =
    write_exports ~dir ~project:"short-01" ~cut:"cut-a" ~takes:[ tts; mic ] g
  in
  Alcotest.(check bool) "export writes the cue sheet on disk" true
    (Sys.file_exists sheet);
  Alcotest.(check bool) "export writes the ffmpeg bounce script on disk" true
    (Sys.file_exists script);
  let sheet_txt = read_file sheet in
  Alcotest.(check bool) "the sheet carries the cue_hash" true
    (contains sheet_txt ("# cue_hash=" ^ cue_hash ~profile:"dailies" g));
  Alcotest.(check bool) "the sheet carries cue rows with gain_db" true
    (contains sheet_txt "1,bed,theme-A,0,240,-20,true");
  Alcotest.(check bool) "the sheet binds the chosen take's media path" true
    (contains sheet_txt "takes/mara-at-window-1.wav");
  let script_txt = read_file script in
  Alcotest.(check bool) "the script is an ffmpeg bounce of the cut" true
    (contains script_txt "ffmpeg"
    && contains script_txt "dailies-short-01-cut-a.mp4")

(* 9. edl-and-export: identical rows hash identically, the MLT XML and the
   Blender VSE script land on disk, and re-importing GUI edits rebuilds the
   same rows. *)
let test_edl_export () =
  let media_take shot =
    land_take (take shot) ~media_hash:(h64 'b')
      ~media_path:("takes/" ^ shot ^ ".mp4")
  in
  let takes = [ media_take "take-a"; media_take "take-b" ] in
  let e1 = edl ~ord:1 "take-a-1" in
  let e2 = edl ~ord:2 ~dur:36 ~transition:"dissolve:12" "take-b-1" in
  let e3 =
    { (edl ~ord:3 ~dur:12 "take-a-1") with edl_tc_in = 6; edl_audio = "line-1" }
  in
  let rows = [ e1; e2; e3 ] in
  Alcotest.(check string) "identical EDL rows produce identical edl_hash"
    (edl_hash rows) (edl_hash [ e3; e2; e1 ]);
  let xml =
    mlt_xml ~project:"short-01" ~cut:"cut-a" ~fps:24 ~resolution:"1920x1080"
      ~takes rows
  in
  Alcotest.(check string) "the MLT export is a pure function of the rows" xml
    (mlt_xml ~project:"short-01" ~cut:"cut-a" ~fps:24
       ~resolution:"1920x1080" ~takes [ e3; e2; e1 ]);
  Alcotest.(check bool) "the MLT export is an XML document" true
    (contains xml "<?xml version=\"1.0\"" && contains xml "<mlt "
    && contains xml "</mlt>");
  Alcotest.(check bool) "each EDL row becomes an MLT producer" true
    (contains xml "<producer id=\"producer2\""
    && contains xml "takes/take-b.mp4"
    && contains xml "<property name=\"pricklypear:take\">take-b-1</property>");
  Alcotest.(check bool) "the playlist places rows in ord order" true
    (contains xml "<entry producer=\"producer1\" in=\"0\" out=\"35\"/>");
  Alcotest.(check bool) "the row audio binding survives into the MLT" true
    (contains xml "<property name=\"pricklypear:audio\">line-1</property>");
  let back = reimport_mlt xml in
  Alcotest.(check int) "re-import rebuilds every row" 3 (List.length back);
  Alcotest.(check string) "re-import round-trips the edl_hash" (edl_hash rows)
    (edl_hash back);
  Alcotest.(check string) "re-import keeps the transition" "dissolve:12"
    (List.nth back 1).edl_transition;
  let script =
    blender_script ~project:"short-01" ~cut:"cut-a" ~fps:24 ~takes rows
  in
  Alcotest.(check bool) "the Blender script builds a VSE scene" true
    (contains script "import bpy"
    && contains script "scene.sequence_editor_create()"
    && contains script "takes/take-a.mp4");
  Alcotest.(check bool) "the Blender script carries the edl_hash" true
    (contains script ("# edl_hash=" ^ edl_hash rows));
  let dir =
    let d =
      Filename.concat (Filename.get_temp_dir_name ()) "film-edit-cut-exports"
    in
    (try Sys.mkdir d 0o755 with Sys_error _ -> ());
    d
  in
  let mlt_path, py_path =
    write_cut_exports ~dir ~project:"short-01" ~cut:"cut-a" ~takes rows
  in
  Alcotest.(check bool) "export writes the MLT XML on disk" true
    (Sys.file_exists mlt_path);
  Alcotest.(check bool) "export writes the Blender scene script on disk" true
    (Sys.file_exists py_path);
  Alcotest.(check bool) "the MLT file on disk re-imports to the same cut" true
    (edl_hash (reimport_mlt (read_file mlt_path)) = edl_hash rows)

(* 10. render-cache: render is a memoized pure function of the EDL rows —
   unchanged cut is a hit and queues nothing; one changed edl-entry mints a
   new job; a done job records out_hash/out_path. *)
let test_render_cache () =
  let e1 = edl ~ord:1 "take-a-1" in
  let e2 = edl ~ord:2 ~dur:36 ~transition:"dissolve:12" "take-b-1" in
  let rows = [ e1; e2 ] in
  let job, hit = render ~project:"short-01" ~cut:"cut-a" rows [] in
  Alcotest.(check bool) "a first render is not a cache hit" false hit;
  Alcotest.(check string) "a first render is queued" "queued" job.rj_state;
  Alcotest.(check string) "the job carries the cut's edl_hash" (edl_hash rows)
    job.rj_edl_hash;
  let done_job =
    render_job_done job ~out_hash:(h64 'f')
      ~out_path:"renders/short-01-cut-a.mp4"
  in
  Alcotest.(check string) "done records out_hash" (h64 'f')
    done_job.rj_out_hash;
  Alcotest.(check string) "done records out_path"
    "renders/short-01-cut-a.mp4" done_job.rj_out_path;
  let cached, same_hit =
    render ~project:"short-01" ~cut:"cut-a" rows [ done_job ]
  in
  Alcotest.(check bool) "an unchanged cut is a cache hit (no job)" true
    same_hit;
  Alcotest.(check string) "the cache hit returns the finished row" "done"
    cached.rj_state;
  Alcotest.(check string) "the cached row names the rendered file"
    "renders/short-01-cut-a.mp4" cached.rj_out_path;
  let changed = [ e1; { e2 with edl_dur = 37 } ] in
  Alcotest.(check bool) "one changed edl-entry changes edl_hash" false
    (edl_hash rows = edl_hash changed);
  let _, miss = render ~project:"short-01" ~cut:"cut-a" changed [ done_job ] in
  Alcotest.(check bool) "a changed cut misses the cache (queues a job)" false
    miss;
  Alcotest.(check bool) "render-done refuses an absolute out path" true
    (try
       ignore
         (render_job_done job ~out_hash:(h64 'f') ~out_path:"/srv/x.mp4");
       false
     with Invalid_argument _ -> true)

(* 11. blender-bridge: typed command set, one line per command, strip-add
   from the EDL rows, a render command that writes a playable file, and a
   queue that runs one render at a time (the second waits). *)
let test_bridge () =
  let media_take shot =
    land_take (take shot) ~media_hash:(h64 'b')
      ~media_path:("takes/" ^ shot ^ ".mp4")
  in
  let takes = [ media_take "take-a"; media_take "take-b" ] in
  let rows = [ edl ~ord:1 "take-a-1"; edl ~ord:2 ~dur:36 "take-b-1" ] in
  let status = bridge_command_line Status in
  Alcotest.(check string) "a command prints as one canonical JSON line" status
    (bridge_command_line Status);
  Alcotest.(check bool) "the status command round-trips" true
    (match bridge_parse status with Ok Status -> true | _ -> false);
  let cmds = bridge_strip_commands ~takes rows in
  Alcotest.(check int) "strip-add is one command per EDL row" 2
    (List.length cmds);
  Alcotest.(check bool) "strip-add names the take's media path" true
    (List.exists
       (fun c ->
         contains (bridge_command_line c) "takes/take-b.mp4")
       cmds);
  Alcotest.(check bool) "the timeline is offset by the previous durations" true
    (match List.rev cmds with
    | Strip_add s :: _ -> s.bs_frame_start = 25 && s.bs_dur = 36
    | _ -> false);
  Alcotest.(check bool) "strip-add survives the socket as typed JSON" true
    (match bridge_parse (bridge_command_line (List.hd cmds)) with
    | Ok (Strip_add s) -> s.bs_name = "t1" && s.bs_offset = 0
    | _ -> false);
  Alcotest.(check bool) "strip-add refuses an absolute media path" true
    (match
       bridge_parse
         "{\"cmd\":\"strip-add\",\"name\":\"t1\",\"path\":\"/srv/x.mp4\",\
          \"channel\":1,\"frame_start\":1,\"offset\":0,\"dur\":24}"
     with
    | Error _ -> true
    | Ok _ -> false);
  let batch = bridge_cut_commands ~out:"renders/cut-a.mp4" ~takes rows in
  Alcotest.(check bool) "a batch run builds the timeline then renders" true
    (match List.rev batch with
    | Render r :: _ -> r.br_out = "renders/cut-a.mp4" && r.br_format = "FFMPEG"
    | _ -> false);
  let addon = bridge_addon ~port:8765 () in
  Alcotest.(check bool) "the addon is a bpy socket server" true
    (contains addon "import bpy" && contains addon "strip-add"
    && contains addon "bpy.ops.render.render(animation=True)");
  let r = bridge_render_command ~out:"renders/cut-b.mp4" () in
  let q = bridge_submit empty_bridge_queue (bridge_job "r1" r) in
  Alcotest.(check bool) "the first render runs" true (bridge_busy q);
  let q = bridge_submit q (bridge_job "r2" r) in
  Alcotest.(check (list string)) "the second render waits" [ "r2" ]
    (bridge_waiting_ids q);
  Alcotest.(check (option string)) "the second render did not replace the first"
    (Some "r1") (bridge_running_id q);
  let q = bridge_finish q in
  Alcotest.(check (option string)) "finishing starts the waiting render"
    (Some "r2") (bridge_running_id q);
  Alcotest.(check (list string)) "the queue is drained" []
    (bridge_waiting_ids q)

let () =
  Alcotest.run "film_edit"
    [
      ( "model",
        [
          Alcotest.test_case "library + graph rows" `Quick test_rows;
          Alcotest.test_case "take rows" `Quick test_take;
          Alcotest.test_case "edl-entry rows" `Quick test_edl;
          Alcotest.test_case "motion masters are pointers" `Quick
            test_motion_masters;
          Alcotest.test_case "cost ledger" `Quick test_costs;
        ] );
      ( "library",
        [
          Alcotest.test_case "asset rows + bindings + expand" `Quick
            test_library;
          Alcotest.test_case "promote-take mints an asset" `Quick test_promote;
        ] );
      ("sound", [ Alcotest.test_case "lines, cues, exports, mix job" `Quick test_sound ]);
      ( "edl-and-export",
        [
          Alcotest.test_case "canonical EDL + MLT XML + Blender script" `Quick
            test_edl_export;
        ] );
      ( "render-cache",
        [
          Alcotest.test_case "memoized render of the EDL" `Quick
            test_render_cache;
        ] );
      ( "blender-bridge",
        [
          Alcotest.test_case "typed commands + one-at-a-time queue" `Quick
            test_bridge;
        ] );
    ]
