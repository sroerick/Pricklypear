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

(* The product trees are excluded from the root dune project, so they may be
   absent from a dune sandbox copy even though they are whole checkouts in the
   repository: a directory test must not raise when the sandbox lacks them. *)
let is_dir rel = try Sys.is_directory (path rel) with Sys_error _ -> false

(* Same for the checkout itself: a sandbox copy has no .git entry, so the
   product-tree assertions only run against a real checkout. *)
let in_checkout = Sys.file_exists (path ".git")

let read rel =
  let ic = open_in_bin (path rel) in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

(* A promoted record is addressed by its dune target name (deploy.yml,
   repos.yml, cutover.log) inside the build sandbox, and by its committed
   destination (.ci/deploy.yml, configs/repos.yml, .ci/cutover.log) in a plain
   checkout; accept either so the check reads the same record both ways. *)
let promoted target committed = if exists target then read target else read committed

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
  (* filter-repo is a pip package, not a git builtin: a host without it must
     still complete the cutover, or the trees never land and criteria 1 and 2
     stay unmet.  The fallback is a rewrite too, never a copy. *)
  check "merge chooses a rewrite backend" (contains m "REWRITE=filter-repo");
  check "merge falls back without filter-repo"
    (contains m "REWRITE=fast-import");
  check "fallback rewrites the export stream (no copy)"
    (contains m "fast-export" && contains m "fast-import");
  List.iter
    (fun p -> check ("merge places " ^ p) (contains m p))
    [ "pp:"; "tuna:"; "sundial:"; "libs/filmstrip:" ];
  check "filmstrip tree is present" (Sys.is_directory (path "libs/filmstrip"));
  check "filmstrip lib.pp is present" (exists "libs/filmstrip/lib.pp");
  (* The other three trees are real directories under the one root too.  They
     are excluded from this dune project (root `dirs`), so they are absent from
     a _build sandbox: assert them against the checkout itself, and let the
     sandbox run assert the same thing through
     scripts/verify_monorepo_merge.sh ("$prefix/ carries files").
     Sys.is_directory raises on a missing path, so probe defensively. *)
  if in_checkout then
    List.iter
      (fun d -> check (d ^ "/ is a directory") (is_dir d))
      [ "pp"; "tuna"; "sundial" ];
  (* the merge itself must refuse to finish when a tree loses commits, not only
     the checker afterwards: history is the memory at cutover time too. *)
  (* the count is taken on the parked rewritten tip, not on the prefix's
     footprints: an empty source commit leaves nothing under <prefix>/ and would
     undercount a merge that carried every commit. *)
  check "merge enforces the per-prefix count"
    (contains m "rev-list --no-merges --count \"refs/monorepo/$prefix\"");
  check "merge aborts on lost history" (contains m "history lost");
  check "merge is resumable without re-merging" (contains m "already merged");
  (* a tree can land its lineage and still be short of the source's paths: the
     prefix is then refreshed from the incoming tip instead of being frozen as
     "already merged", because criterion 1 is stated in FILES. *)
  check "merge refreshes a short prefix from the incoming tip"
    (contains m "git -C \"$MR\" checkout \"$incoming\" -- \"$prefix\"");
  check "merge decides refresh on path counts, not only commit counts"
    (contains m "have_files_now");
  (* the promoted anchors sit exactly where the incoming trees write, so the
     merge has to clear them or git stops on "untracked working tree files
     would be overwritten" and the lineage never lands. *)
  check "merge clears the promoted anchors before merging"
    (contains m "would be overwritten" && contains m "rm -f \"$MR/$prefix/$anchor\"");
  (* the merge rewrites HEAD in the worktree, so it must run against the
     checkout's git dir and not against a _build sandbox -- a sandbox pass would
     leave the anchors in place and the lineage nowhere.  The script refuses
     that case, and `dune build @monorepo-merge` hands it the workspace root. *)
  check "merge runs against a git checkout"
    (contains m "[ -e \"$MR/.git\" ]");
  (* a factory worktree carries .git as a file (gitdir: ...), not a directory,
     and %{workspace_root} expands to "." inside the _build sandbox: the script
     walks back to the toplevel that owns HEAD instead of merging a copy. *)
  check "merge walks back to the worktree root"
    (contains m "rev-parse --show-toplevel");
  check "merge refuses a non-checkout instead of silently doing nothing"
    (contains m "is not a git checkout");
  let d = read "dune" in
  check "the monorepo-merge alias passes the workspace root"
    (contains d "%{dep:scripts/monorepo-merge.sh} %{workspace_root}");
  (* the alias runs the merge from inside _build, so the script has to walk back
     to the checkout that owns HEAD instead of merging the sandbox copy. *)
  check "merge finds the worktree that owns HEAD"
    (contains m "rev-parse --show-toplevel");
  (* sources are resolved through SRC_ROOTS: bare repos on the production host,
     local checkouts on a factory host, so the cutover is not /srv/git-only. *)
  check "merge resolves sources through SRC_ROOTS"
    (contains m "SRC_ROOTS" && contains m "find_src");
  let v = read "scripts/verify_monorepo_merge.sh" in
  check "merge writes a lineage marker per tree"
    (contains m "MR4: merge $prefix history from $name");
  check "verify counts per prefix"
    (contains v "rev-list --no-merges --count HEAD");
  (* criterion 1 is stated in files as well as commits: the check must be the
     card's own `git ls-tree -r --name-only HEAD -- <prefix> | wc -l` against
     the source's `git ls-files | wc -l`, or a merge that keeps commits and
     drops paths passes the gate and still fails the card. *)
  check "verify counts files per prefix (git ls-tree vs git ls-files)"
    (contains v "ls-tree -r --name-only HEAD" && contains v "ls-files | wc -l");
  check "verify tests the three product trees' file counts"
    (contains v "pp/ files == pricklypear.git"
     && contains v "tuna/ files == tuna-work.git"
     && contains v "sundial/ files == sundial.git");
  check "verify exempts filmstrip only while its source carries no files"
    (contains v "source carries no files");
  (* the count alone cannot say WHICH tree failed to land, so the checker also
     looks for the per-tree merge marker the merge script writes. *)
  check "verify checks the pp lineage marker"
    (contains v "MR4: merge $prefix history from $src");
  check "verify checks every merged tree's marker"
    (contains v "'libs/filmstrip:filmstrip.git'");
  check "verify checks libs/filmstrip is a directory"
    (contains v "libs/filmstrip is a directory");
  (* a missing source repo would make ls-files and ls-tree both 0, so a bare
     count equality passes on an anchors-only checkout: the checker must refuse
     that state instead of printing ok for a merge that never happened. *)
  check "verify refuses a missing source repo"
    (contains v "source repo $src is present");
  check "verify demands files under each product prefix"
    (contains v "carries files (not anchors only)");
  (* a reviewed FAIL on this card was a branch that promoted every record and
     still had no pp/, tuna/, sundial/ in the checkout: the gate has to assert
     the trees are there, not only that the manifest names them. *)
  check "verify demands the merged trees in the checkout"
    (contains v "is a directory in the checkout");
  (* criterion 1 counts FILES per prefix (ls-tree -r --name-only), not only
     commits: filter-repo can keep the whole lineage and still drop paths. *)
  check "verify counts files per prefix"
    (contains v "ls-tree -r --name-only HEAD --");
  check "verify counts source files"
    (contains v "ls-files | wc -l");
  check "verify checks pp file count"
    (contains v "pp/ files == pricklypear.git");
  (* the path-filtered deploy jobs run `make -C <tree>`, so each merged tree is
     addressable from the root; libs/filmstrip is a real package.  The product
     trees are excluded from this dune project, so they are only visible in a
     real checkout, not in a _build sandbox. *)
  check "libs/filmstrip/dune is present" (exists "libs/filmstrip/dune");
  (* The merged trees are whole checkouts of their source repos, so their files
     sit at <prefix>/<source path>: sundial's own dune project is
     sundial/sundial/dune, not a root-level anchor.  Assert one real file per
     product tree, plus that each prefix carries more than the promoted
     Makefile/README anchor -- the reviewed FAIL was a green build over a
     checkout with no merged trees in it at all. *)
  if in_checkout then begin
    List.iter
      (fun f -> check (f ^ " is present") (exists f))
      [ "pp/README.md"; "tuna/README.md"; "sundial/sundial/dune" ];
    List.iter
      (fun d ->
        let n = try Array.length (Sys.readdir (path d)) with Sys_error _ -> 0 in
        check (d ^ "/ carries files (not anchors only)") (n > 2))
      [ "pp"; "tuna"; "sundial" ]
  end

(* Criterion 1 is stated in PATHS, so the merge must not lose a path to this
   repository's own ignore rules: the product trees are whole checkouts and
   their histories carry files whose patterns the root .gitignore ignores
   (tuna's .vscode/, package.json/package-lock.json, one .wat.bak; pp's and
   tuna's .ralph/, pp's .borge-bugs/).  A swallowed path makes the committed
   tree short of its source, which is exactly the reviewed failure of this card
   in file-count form -- so the re-includes are checked, not assumed. *)
let () =
  let gi = read ".gitignore" in
  List.iter
    (fun p -> check ("gitignore re-includes " ^ p) (contains gi p))
    [ "!tuna/.ralph/"; "!pp/.ralph/"; "!sundial/.ralph/"; "!pp/.borge-bugs/";
      "!tuna/reference/tree-calculus/.vscode/";
      "!tuna/reference/tree-calculus/implementation/typescript/package.json" ]

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
  check "repos.yml is the promoted copy"
    (promoted "repos.yml" "configs/repos.yml" = rec_);
  check "deploy manifest is the promoted copy"
    (promoted "deploy.yml" ".ci/deploy.yml"
    = read "scripts/monorepo-deploy.yml");
  check "cutover log is the promoted copy"
    (promoted "cutover.log" ".ci/cutover.log"
    = read "scripts/monorepo-cutover.log");
  (* the loop's acceptance script must check the same resolution the factory
     uses, or a green merge could still route cards at an old repo. *)
  let v = read "scripts/verify_monorepo_merge.sh" in
  check "verify names the monorepo checkout path"
    (contains v "grep -qxF '    path: $MR'");
  check "verify checks card_repo" (contains v "card_repo: monorepo");
  check "verify checks the promoted copies" (contains v "cmp -s")

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
