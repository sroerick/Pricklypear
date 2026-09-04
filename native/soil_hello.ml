(* soil-hello -- pads hello dual-render spike, V2 (plan:
   docs/plans/pads-hello-dualrender.md W5; spec: borge/soil.borg
   native-rendering).

   Takes a scene-dialect-v0 JSON document (pads/render-scene output)
   from one of two sources (decision D2, thin shim):

     --scene FILE   imageless file input (the local-first seed)
     --eval EXPR    fetch via scripts/pp-eval.sh from a running dev image

   then: parse -> two-pass soil layout (owned OCaml per D1) -> software
   raster (rect fill + bitmap font, D4) -> wl_shm ARGB buffer ->
   xdg-shell toplevel on the running Wayland compositor. Full-surface
   repaint per commit (dumb v1, soil/soil-contract). No Nopales eval on
   any frame path (soil contract).

   Flags: --frames N exits 0 after N committed frames (smoke mode);
   default runs until the window is closed. No input handling beyond
   window-close (D5). *)

open Eio.Std

(* ---------- scene model (dialect v0, pads/render-scene docstring) ---------- *)

type size =
  | Fit
  | Grow of int
  | Fixed of int

type kind =
  | Row
  | Column
  | Text

type node = {
  kind : kind;
  text : string; (* Text only *)
  attrs : (string * string) list;
  kids : node array; (* Row/Column only *)
}

module Json = struct
  open Yojson.Safe
  open Yojson.Safe.Util

  let str ?(default = "") k j =
    match member k j with
    | `String s -> s
    | `Null -> default
    | j -> to_string j

  let attrs j =
    match j with
    | `Assoc _ -> List.map (fun (k, v) -> (k, to_string v)) (to_assoc j)
    | _ -> []

  let kids j =
    match member "kids" j with
    | `List l -> l
    | _ -> []
end

let rec node_of (j : Yojson.Safe.t) : node =
  let kind =
    match Json.str "kind" j with
    | "row" -> Row
    | "column" -> Column
    | _ -> Text
  in
  {
    kind;
    text = Json.str "text" j;
    attrs = Json.attrs (Yojson.Safe.Util.member "attrs" j);
    kids = Array.of_list (List.map node_of (Json.kids j));
  }

let of_json (j : Yojson.Safe.t) : node =
  match Yojson.Safe.Util.member "root" j with
  | `Null -> failwith "scene: no \"root\" member"
  | root -> node_of root

(* ---------- attrs helpers ---------- *)

let attr_int ?(default = 0) k attrs =
  match List.assoc_opt k attrs with
  | Some s -> ( try int_of_string (String.trim s) with _ -> default )
  | None -> default

let level attrs = attr_int "level" attrs

(* Headings (level >= 1) render at 2x; body text at 1x. *)
let scale_of attrs = if level attrs >= 1 then 2 else 1

let size_of attrs =
  match List.assoc_opt "size" attrs with
  | None | Some "" -> Fit
  | Some s -> (
      match String.index_opt s ':' with
      | Some i -> (
          let tag = String.sub s 0 i in
          let n =
            try int_of_string (String.sub s (i + 1) (String.length s - i - 1))
            with _ -> 0
          in
          match tag with
          | "grow" -> Grow (max 0 n)
          | "fixed" -> Fixed (max 0 n)
          | _ -> Fit )
      | None -> Fit )

(* ---------- layout (owned, D1; soil/native-layout) ----------

   Two passes: fit measures bottom-up (text via the font advance
   table), then assign distributes main-axis space top-down (fixed /
   fit take their measure, grow splits the remainder by weight) and
   stretches the cross axis. gap between kids; padding insets content.
   Direction is well under the ~200 LOC gold-plating bar (W5). *)

type rect = { x : int; y : int; w : int; h : int }

(* 8x8 glyphs drawn at 8x16: every source row doubled. *)
let glyph_w = 8
let glyph_h = 16

let cell_w scale = glyph_w * scale
let cell_h scale = glyph_h * scale

let text_fit text attrs =
  let scale = scale_of attrs in
  let lines = String.split_on_char '\n' text in
  let wmax = List.fold_left (fun a l -> max a (String.length l)) 0 lines in
  (wmax * cell_w scale, List.length lines * cell_h scale)

let rec fit (n : node) : int * int =
  match n.kind with
  | Text -> text_fit n.text n.attrs
  | Row | Column ->
      let gap = attr_int "gap" n.attrs and pad = attr_int "padding" n.attrs in
      let fits = Array.map fit n.kids in
      let nk = Array.length n.kids in
      if nk = 0 then (2 * pad, 2 * pad)
      else if
        (match n.kind with Row -> true | _ -> false)
      then
        ( Array.fold_left (fun a (w, _) -> a + w) 0 fits + (gap * (nk - 1))
            + (2 * pad),
          Array.fold_left (fun a (_, h) -> max a h) 0 fits + (2 * pad) )
      else
        ( Array.fold_left (fun a (w, _) -> max a w) 0 fits + (2 * pad),
          Array.fold_left (fun a (_, h) -> a + h) 0 fits + (gap * (nk - 1))
            + (2 * pad) )

type placed = { node : node; rect : rect }

let rec assign (n : node) (r : rect) (acc : placed list ref) : unit =
  acc := { node = n; rect = r } :: !acc;
  match n.kind with
  | Text -> ()
  | Row | Column when Array.length n.kids = 0 -> ()
  | Row | Column ->
      let row = (match n.kind with Row -> true | _ -> false) in
      let gap = attr_int "gap" n.attrs and pad = attr_int "padding" n.attrs in
      let content =
        {
          x = r.x + pad;
          y = r.y + pad;
          w = max 0 (r.w - (2 * pad));
          h = max 0 (r.h - (2 * pad));
        }
      in
      let main_avail = if row then content.w else content.h in
      let cross = if row then content.h else content.w in
      let fits = Array.map fit n.kids in
      let sizes = Array.map (fun k -> size_of k.attrs) n.kids in
      let nk = Array.length n.kids in
      let gaps = gap * (nk - 1) in
      let main_fit i = if row then fst fits.(i) else snd fits.(i) in
      let fixed_total = ref 0 and grow_total = ref 0 in
      Array.iteri
        (fun i s ->
          match s with
          | Fixed m -> fixed_total := !fixed_total + m
          | Grow k -> grow_total := !grow_total + k
          | Fit -> fixed_total := !fixed_total + main_fit i)
        sizes;
      let free = max 0 (main_avail - !fixed_total - gaps) in
      let main_size i =
        match sizes.(i) with
        | Fixed m -> m
        | Fit -> main_fit i
        | Grow k -> if !grow_total > 0 then (free * k) / !grow_total else 0
      in
      let pos = ref (if row then content.x else content.y) in
      Array.iteri
        (fun i k ->
          let m = main_size i in
          let kr =
            if row then { x = !pos; y = content.y; w = m; h = cross }
            else { x = content.x; y = !pos; w = cross; h = m }
          in
          assign k kr acc;
          pos := !pos + m + gap)
        n.kids

(* Root ignores its own size and fills the window. *)
let layout (root : node) w h : placed list =
  let acc = ref [] in
  assign root { x = 0; y = 0; w; h } acc;
  List.rev !acc

(* ---------- raster (ARGB32; rect fill + font8x8 glyphs) ---------- *)

module Raster = struct
  type buf = (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array2.t

  let fill (buf : buf) (r : rect) color =
    let h = Bigarray.Array2.dim1 buf and w = Bigarray.Array2.dim2 buf in
    let x0 = max 0 r.x and y0 = max 0 r.y in
    let x1 = min w (r.x + r.w) and y1 = min h (r.y + r.h) in
    for y = y0 to y1 - 1 do
      for x = x0 to x1 - 1 do
        buf.{y, x} <- color
      done
    done

  (* One glyph: 8 source rows, each doubled to fill the 8x16 cell;
     scale >= 2 also doubles horizontally. Bit gx (LSB) = leftmost
     pixel. Clipped to [clip]. *)
  let glyph (buf : buf) (clip : rect) ~(x : int) ~(y : int) ~(scale : int)
      (rows : int array) color =
    for gy = 0 to 7 do
      let bits = rows.(gy) in
      if bits <> 0 then
        for sy = 0 to (2 * scale) - 1 do
          let py = y + (gy * 2 * scale) + sy in
          for gx = 0 to 7 do
            if bits land (1 lsl gx) <> 0 then
              for sx = 0 to scale - 1 do
                let px = x + (gx * scale) + sx in
                if
                  px >= clip.x && px < clip.x + clip.w && py >= clip.y
                  && py < clip.y + clip.h
                then buf.{py, px} <- color
              done
          done
        done
    done

  let text (buf : buf) (clip : rect) ~(x : int) ~(y : int) ~(scale : int)
      (s : string) color =
    String.split_on_char '\n' s
    |> List.iteri (fun li line ->
           let ly = y + (li * cell_h scale) in
           ignore
             (String.fold_left
                (fun lx ch ->
                  if ch >= Char.chr Font8x8.first_char && ch <= '~' then
                    glyph buf clip ~x:lx ~y:ly ~scale
                      Font8x8.glyphs.(Char.code ch - Font8x8.first_char)
                      color;
                  lx + cell_w scale)
                x line))
end

(* v0 presentational palette (no bg attrs in dialect v0; nested boxes
   get a faint panel fill so layout is visible). *)
let col_bg = 0xFF16181Dl
let col_box = 0xFF1E222Al
let col_text = 0xFFE6E6E6l
let col_heading = 0xFF82AAFFl

let paint (buf : Raster.buf) (placed : placed list) : unit =
  let h = Bigarray.Array2.dim1 buf and w = Bigarray.Array2.dim2 buf in
  Raster.fill buf { x = 0; y = 0; w; h } col_bg;
  List.iteri
    (fun i p ->
      match p.node.kind with
      | Row | Column ->
          if i > 0 then Raster.fill buf p.rect col_box
          (* the root keeps the page background *)
      | Text ->
          let scale = scale_of p.node.attrs in
          let color =
            if level p.node.attrs >= 1 then col_heading else col_text
          in
          Raster.text buf p.rect ~x:p.rect.x ~y:p.rect.y ~scale p.node.text
            color)
    placed

(* ---------- shm pool (temp file in /dev/shm; adapted from
   ocaml-wayland example/shm.ml, MIT) ---------- *)

let rand = Random.State.make_self_init ()

let with_memory_fd ~size f =
  let rec aux = function
    | 0 -> failwith "Failed to find an unused temporary file name!"
    | i ->
        let rnd = Random.State.bits rand land 0xFFFFFF in
        let path = Printf.sprintf "/dev/shm/wl_shm-%06x" rnd in
        (match
           Unix.openfile path Unix.[ O_CREAT; O_RDWR; O_CLOEXEC; O_EXCL ] 0o600
         with
        | exception Unix.Unix_error (Unix.EEXIST, _, _) -> aux (i - 1)
        | fd ->
            Fun.protect
              ~finally:(fun () -> Unix.close fd)
              (fun () ->
                 Unix.unlink path;
                 Unix.ftruncate fd size;
                 f fd))
  in
  aux 100

(* ---------- wayland client ---------- *)

open Wayland.Wayland_client
open Wayland_protocols.Xdg_shell_client

module Registry = Wayland.Registry

type st = {
  shm : [ `V1 ] Wl_shm.t;
  surface : [ `V4 ] Wl_surface.t;
  display : Wayland.Client.t;
  root : node;
  max_frames : int; (* 0 = run until close *)
  mutable width : int;
  mutable height : int;
  mutable committed : int;
}

(* One wl_shm pool + wl_buffer per commit. The pool is destroyed right
   after the buffer is created (buffers created through a pool remain
   valid per wayland.xml), and the fd is closed once create_pool has
   dup'd it (ocaml-wayland Msg.add_fd dups at marshal). Full-surface
   repaint per commit. *)
let draw st =
  let w, h = (st.width, st.height) in
  if w > 0 && h > 0 then begin
    let stride = w * 4 in
    let size = h * stride in
    let pool, data =
      with_memory_fd ~size (fun fd ->
          let ba =
            Unix.map_file fd Bigarray.Int32 Bigarray.c_layout true [| h; w |]
          in
          let pool =
            Wl_shm.create_pool st.shm (new Wl_shm_pool.v1) ~fd
              ~size:(Int32.of_int size)
          in
          (* fd lifetime is owned by with_memory_fd's finally; add_fd dups
             before queuing, so closing here (or not) is safe either way. *)
          (pool, Bigarray.array2_of_genarray ba))
    in
    let buffer =
      Wl_shm_pool.create_buffer pool ~offset:0l ~width:(Int32.of_int w)
        ~height:(Int32.of_int h) ~stride:(Int32.of_int stride)
        ~format:Wl_shm.Format.Xrgb8888
      @@ object
           inherit [_] Wl_buffer.v1
           (* Compositors may send release more than once per buffer
               (Hyprland does); only the first destroy is legal. *)
           val mutable released = false
           method on_release self =
             if not released then begin
               released <- true;
               Wl_buffer.destroy self
             end
         end
    in
    Wl_shm_pool.destroy pool;
    paint data (layout st.root w h);
    Wl_surface.attach st.surface ~buffer:(Some buffer) ~x:0l ~y:0l;
    Wl_surface.damage st.surface ~x:0l ~y:0l ~width:Int32.max_int
      ~height:Int32.max_int;
    Wl_surface.commit st.surface
  end

let run ~(root : node) ~(max_frames : int) ~clock ~net ~sw : unit =
  let transport = Wayland.Unix_transport.connect ~sw ~net () in
  let display = Wayland.Client.connect ~sw transport in
  let reg = Registry.of_display display in
  let compositor = Registry.bind reg (new Wl_compositor.v4) in
  let shm = Registry.bind reg @@ object
      inherit [_] Wl_shm.v1
      method on_format _ ~format:_ = ()
    end
  in
  let xdg_wm_base = Registry.bind reg @@ object
      inherit [_] Xdg_wm_base.v1
      method on_ping = Xdg_wm_base.pong
    end
  in
  let surface = Wl_compositor.create_surface compositor @@ object
      inherit [_] Wl_surface.v1
      method on_enter _ ~output:_ = ()
      method on_leave _ ~output:_ = ()
      method on_preferred_buffer_scale _ ~factor:_ = ()
      method on_preferred_buffer_transform _ ~transform:_ = ()
    end
  in
  let st =
    {
      shm;
      surface;
      display;
      root;
      max_frames;
      width = 640;
      height = 480;
      committed = 0;
    }
  in
  let configured, set_configured = Promise.create () in
  let xdg_surface =
    Xdg_wm_base.get_xdg_surface xdg_wm_base ~surface @@ object
        inherit [_] Xdg_surface.v1
        method on_configure proxy ~serial =
          Xdg_surface.ack_configure proxy ~serial;
          if not (Promise.is_resolved configured) then
            Promise.resolve set_configured ()
      end
  in
  let toplevel = Xdg_surface.get_toplevel xdg_surface @@ object
      inherit [_] Xdg_toplevel.v1
      method on_configure_bounds _ ~width:_ ~height:_ = ()
      method on_configure _ ~width ~height ~states:_ =
        if width > 0l then st.width <- Int32.to_int width;
        if height > 0l then st.height <- Int32.to_int height
      method on_close _ =
        Logs.info (fun f -> f "window closed - exiting!");
        Wayland.Client.stop display
      method on_wm_capabilities _ ~capabilities:_ = ()
    end
  in
  Xdg_toplevel.set_title toplevel ~title:"soil-hello";
  Wl_surface.commit surface;
  Promise.await configured;
  (* One commit per tick. Pacing waits for the compositor's frame
     callback (vsync when the window is visible), but a short timer
     fallback keeps making progress when the window is throttled or
     suspended (e.g. parked on an inactive workspace) and frame
     callbacks never fire -- otherwise --frames N would hang forever.
     Hyprland also double-sends release; handled in draw's on_release. *)
  let rec animate () =
    draw st;
    st.committed <- st.committed + 1;
    if st.max_frames > 0 && st.committed >= st.max_frames then
      Wayland.Client.stop st.display
    else begin
      let frame_done, fire_frame = Promise.create () in
      let _cb =
        Wl_surface.frame surface
          (Wayland.callback (fun _ -> Promise.resolve fire_frame ()))
      in
      Fiber.first
        (fun () -> Promise.await frame_done)
        (fun () -> Eio.Time.sleep clock 0.1)
      |> ignore;
      animate ()
    end
  in
  animate ()

(* ---------- scene sources (D2) ---------- *)

let read_all ic =
  let buf = Buffer.create 4096 in
  (try
     while true do
       Buffer.add_channel buf ic 4096
     done
   with End_of_file -> ());
  Buffer.contents buf

let read_file path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () -> read_all ic)

(* Live pull: shell out to scripts/pp-eval.sh -j EXPR (loopback eval on
   the dev image) and unwrap {"ok":true,"value":"<scene-json>"}. Run
   from the repo root (or set SOIL_PP_EVAL to the script's path);
   needs PP_PASS like pp-eval itself. *)
let eval_scene (expr : string) : Yojson.Safe.t =
  let script =
    match Sys.getenv_opt "SOIL_PP_EVAL" with
    | Some p -> p
    | None -> "scripts/pp-eval.sh"
  in
  let cmd = Filename.quote_command script [ "-j"; expr ] in
  let ic = Unix.open_process_in cmd in
  let out = read_all ic in
  let status = Unix.close_process_in ic in
  (match status with
  | Unix.WEXITED 0 -> ()
  | Unix.WEXITED n | Unix.WSIGNALED n ->
      failwith (Printf.sprintf "pp-eval failed (%s):\n%s" (string_of_int n) out)
  | Unix.WSTOPPED _ -> failwith "pp-eval stopped");
  let j = Yojson.Safe.from_string out in
  let open Yojson.Safe.Util in
  (match member "ok" j with
  | `Bool true -> ()
  | _ -> failwith ("pp-eval error: " ^ Yojson.Safe.to_string j));
  match member "value" j with
  | `String s -> Yojson.Safe.from_string s (* scene JSON wrapped as a string *)
  | other -> other

(* ---------- args + main ---------- *)

let usage =
  "usage: soil-hello [--scene FILE | --eval EXPR] [--frames N]\n\
  \  --scene FILE   scene-dialect-v0 JSON file (pads/render-scene output)\n\
  \  --eval EXPR    lisp expr fetched via scripts/pp-eval.sh (default\n\
  \                 \"(hello/scene)\"; needs a running dev image + PP_PASS)\n\
  \  --frames N     exit 0 after N committed frames (smoke mode)\n\
  \  -h, --help     this text"

let main () =
  let source = ref (`Eval "(hello/scene)") in
  let frames = ref 0 in
  let rec go = function
    | [] -> ()
    | "--scene" :: f :: rest -> source := `Scene f; go rest
    | "--eval" :: e :: rest -> source := `Eval e; go rest
    | "--frames" :: n :: rest ->
        frames :=
          (try int_of_string n with _ -> failwith ("bad --frames: " ^ n));
        go rest
    | ("-h" | "--help") :: _ ->
        print_endline usage;
        exit 0
    | x :: _ ->
        prerr_endline ("soil-hello: unknown arg " ^ x);
        prerr_endline usage;
        exit 2
  in
  (match Array.to_list Sys.argv with
  | _ :: rest -> go rest (* drop argv[0] *)
  | [] -> ());
  let scene_json =
    match !source with
    | `Scene f -> Yojson.Safe.from_string (read_file f)
    | `Eval e -> eval_scene e
  in
  let root = of_json scene_json in
  Logs.info (fun f ->
      f "scene: root %s, %d top-level kids"
        (match root.kind with Row -> "row" | Column -> "column" | Text -> "text")
        (Array.length root.kids));
  Eio_main.run @@ fun env ->
  Switch.run @@ fun sw ->
  run ~root ~max_frames:!frames ~clock:env#clock ~net:env#net ~sw

let () =
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.(set_level (Some Info));
  Printexc.record_backtrace true;
  try main () with
  | Failure msg ->
      prerr_endline ("soil-hello: " ^ msg);
      exit 1
  | Yojson.Json_error msg ->
      prerr_endline ("soil-hello: bad scene JSON: " ^ msg);
      exit 1
  | Yojson.Safe.Util.Type_error (msg, _) ->
      prerr_endline ("soil-hello: bad scene JSON: " ^ msg);
      exit 1
  | Unix.Unix_error (e, fn, arg) ->
      prerr_endline
        (Printf.sprintf "soil-hello: %s: %s%s" fn (Unix.error_message e)
           (if arg = "" then "" else ": " ^ arg));
      exit 1
