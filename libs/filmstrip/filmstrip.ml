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
