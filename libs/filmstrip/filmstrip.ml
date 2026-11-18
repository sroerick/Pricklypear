(* Pricklypear's filmstrip registry: PP-specific parts stay here (app borg
   routes, act-glyphs, ws-fields, native pane state). The registry is a
   tile provider -- the strip rendering itself lives in filmstrip-shared
   (libs/filmstrip/shared), so a refinement there hits tuna too. *)

type app = {
  glyph : string;
  name : string;
  summary : string;
  href : string;
}

let apps =
  [
    {
      glyph = "\226\150\164";
      name = "outline";
      summary = "your notes, in the tree";
      href = "/outline";
    };
    {
      glyph = "\226\152\176";
      name = "todo";
      summary = "open items, baked";
      href = "/todo";
    };
    {
      glyph = "\226\156\137";
      name = "notes";
      summary = "write and search";
      href = "/notes";
    };
    {
      glyph = "\226\154\153";
      name = "calendar";
      summary = "today's agenda, baked";
      href = "/calendar";
    };
    {
      glyph = "\226\134\187";
      name = "workstreams";
      summary = "what is in flight";
      href = "/workstreams";
    };
  ]

(* Every tile carries its stamp: a summary that reads a live number is
   stamped "live", a baked one says so. *)
let tile_of_app ?(stamp = "baked") (a : app) : Filmstrip_shared.tile =
  {
    Filmstrip_shared.glyph = a.glyph;
    name = a.name;
    summary = a.summary;
    href = a.href;
    stamp;
  }

let tiles () = List.map tile_of_app apps

(* ---- recent commits: the monorepo commit-history pane ----
   The pane lists rev/author/date/subject for the last N commits, read
   from the bare repo on disk -- the same repo git.wyo.town's stagit
   output is regenerated from, so `git log` there tracks pushes with no
   second generator to run. Both the path and N are CONSTANTS (the pane
   must survive MR4: nothing here depends on a cwd or a session), and
   both a missing repo and a git failure degrade to an EMPTY strip --
   the shell renders either way. *)

(* Pinned data source: the bare wyo-preview repo. *)
let commit_repo = "/home/git/srv/git/wyo-preview.git"

(* Pinned N: the pane lists the last 20 commits, never the history. *)
let commit_count = 20

let commit_of_line line =
  match String.split_on_char '\t' line with
  | rev :: author :: date :: (_ :: _ as rest) ->
      Some
        {
          Filmstrip_shared.rev;
          author;
          date;
          subject = String.concat "\t" rest;
        }
  | _ -> None

(* Last [n] commits of the pinned repo, newest first. Read-only: git log
   in the bare dir, one tab-separated record per line. *)
let recent_commits ?(n = commit_count) () =
  let cmd =
    Printf.sprintf
      "git --git-dir=%s log -n %d --date=short \
       --pretty=format:%%h%%x09%%an%%x09%%ad%%x09%%s 2>/dev/null"
      (Filename.quote commit_repo) n
  in
  match Unix.open_process_in cmd with
  | exception _ -> []
  | ic ->
      let rec loop acc =
        match input_line ic with
        | line -> loop (line :: acc)
        | exception End_of_file -> List.rev acc
      in
      let lines = loop [] in
      (try ignore (Unix.close_process_in ic) with _ -> ());
      List.filter_map commit_of_line lines
