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
    ]
