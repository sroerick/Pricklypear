(* MR4: the full merge -- acceptance checks that a checkout really holds the one
   repo with per-product path-filtered deploys.

   These are the static half of the card's criteria; the history-count half
   (non-merge commits under pp/ == pricklypear.git's count, etc.) needs the
   source repos and lives in scripts/verify_monorepo_merge.sh.

   The records are read through the scripts/ copies of record (deps of this
   test, so they are always visible in the build tree); the committed copies at
   .ci/deploy.yml, .ci/cutover.log and configs/repos.yml are byte-identical to
   them because dune promotes one from the other. *)

(* The test may run from _build/default/tests (dune) or from the checkout
   root; find the directory that holds the scripts/ copies of record. *)
let rec find_root dir =
  if Sys.file_exists (Filename.concat dir "scripts/monorepo-deploy.yml") then dir
  else
    let up = Filename.dirname dir in
    if up = dir then failwith "pp_monorepo_merge: no checkout root"
    else find_root up

let root = find_root (Sys.getcwd ())
let path rel = Filename.concat root rel
let exists rel = Sys.file_exists (path rel)

let read rel =
  let ic = open_in_bin (path rel) in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let contains hay needle =
  let nh = String.length hay and nn = String.length needle in
  let rec go i =
    if i + nn > nh then false
    else if String.sub hay i nn = needle then true
    else go (i + 1)
  in
  go 0

let first hay needle =
  let nh = String.length hay and nn = String.length needle in
  let rec go i =
    if i + nn > nh then -1
    else if String.sub hay i nn = needle then i
    else go (i + 1)
  in
  go 0

let check name cond = if not cond then failwith ("pp_monorepo_merge: " ^ name)

(* Criterion 1: the merge places every tree under one root with full history --
   one filter-repo --to-subdirectory-filter per source repo, filmstrip folding
   in at libs/filmstrip.  No squash, no reset. *)
let () =
  let m = read "scripts/monorepo-merge.sh" in
  check "merge uses git filter-repo" (contains m "filter-repo");
  check "merge rewrites under a prefix" (contains m "--to-subdirectory-filter");
  check "merge keeps lineage (no squash flag)" (contains m "--squash" = false);
  List.iter
    (fun p -> check ("merge places " ^ p) (contains m p))
    [ "pp:"; "tuna:"; "sundial:"; "libs/filmstrip:" ];
  check "filmstrip tree is present" (Sys.is_directory (path "libs/filmstrip"));
  check "filmstrip lib.pp is present" (exists "libs/filmstrip/lib.pp");
  (* the other three trees are real directories under the one root too. *)
  List.iter
    (fun d -> check (d ^ "/ is a directory") (Sys.is_directory (path d)))
    [ "pp"; "tuna"; "sundial" ];
  let v = read "scripts/verify_monorepo_merge.sh" in
  check "verify counts per prefix"
    (contains v "rev-list --no-merges --count HEAD");
  check "verify checks libs/filmstrip is a directory"
    (contains v "libs/filmstrip is a directory")

(* Criterion 2: .ci/deploy.yml carries the four path filters and the four jobs,
   each running its own product's build. *)
let () =
  let d = read "scripts/monorepo-deploy.yml" in
  List.iter
    (fun p -> check ("deploy path " ^ p) (contains d p))
    [ "pp/**"; "tuna/**"; "sundial/**"; "libs/filmstrip/**" ];
  List.iter
    (fun j -> check ("deploy job " ^ j) (contains d j))
    [ "pp-deploy"; "tuna-deploy"; "sundial-deploy"; "filmstrip-test" ];
  check "pp job builds pp" (contains d "make -C pp");
  check "tuna job builds tuna" (contains d "make -C tuna");
  check "filmstrip job runs dune build" (contains d "dune build")

(* Criterion 2 (continued): the filmstrip job runs BOTH consumers before the
   shared package's dune build, so one gate covers per-product targets. *)
let () =
  let d = read "scripts/monorepo-deploy.yml" in
  check "filmstrip job runs both product builds"
    (contains d "make -C pp && make -C tuna && dune build")

(* Criterion 3: the factory rewires to one repo; cards resolve to it. *)
let () =
  let r = read "scripts/monorepo-repos.yml" in
  check "repos.yml names the monorepo" (contains r "monorepo");
  check "repos.yml card_repo" (contains r "card_repo: monorepo");
  check "repos.yml merge gates"
    (contains r "make -C pp" && contains r "make -C tuna"
     && contains r "dune build")

(* Criterion 3 (continued): a card's repo field resolves to the monorepo, so
   repos.yml must carry a checkout path, and the published copy must be the
   promote of the scripts/ copy of record -- a stale or missing promote would
   silently send cards at an old per-product repo. *)
let () =
  let rec_ = read "scripts/monorepo-repos.yml" in
  check "repos.yml names a monorepo checkout" (contains rec_ "path:");
  check "repos.yml is the promoted copy" (read "repos.yml" = rec_);
  check "deploy manifest is the promoted copy"
    (read "deploy.yml" = read "scripts/monorepo-deploy.yml");
  check "cutover log is the promoted copy"
    (read "cutover.log" = read "scripts/monorepo-cutover.log")

(* Criterion 4: the cutover log is bracketed CUTOVER-START .. CUTOVER-END, and
   the old repos stay armed for rollback (executable pre-receive hooks). *)
let () =
  let log = read "scripts/monorepo-cutover.log" in
  let start = "CUTOVER-START" and stop = "CUTOVER-END" in
  let i = first log start and j = first log stop in
  check "cutover start present" (i >= 0);
  check "cutover end present" (j >= 0);
  check "cutover ordered" (i < j);
  let v = read "scripts/verify_monorepo_merge.sh" in
  check "verify checks pre-receive hooks" (contains v "hooks/pre-receive")
