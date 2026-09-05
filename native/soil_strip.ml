(* soil-strip — soil M-B window policy client (plan:
   docs/plans/soil-m-b.md; spec: borge/soil.borg river-split).

   Attaches to a (nested) river 0.4 compositor as its window manager:
   every mapped window becomes a filmstrip pane — a row record carrying
   a `view` key (its river window identity) — and each manage sequence
   lays the view-carrying panes out as the niri strip (same 2/3/6
   weights as filmstrip/scene), focuses the focused pane's REAL window,
   and paints the focus ring with set_borders. Keybindings (alt+arrows
   focus, alt+shift+arrows move) are WM policy via river-xkb-bindings-v1;
   pressing one evals the policy and manage_dirty's a fresh sequence.
   A poller re-syncs browser-driven row changes.

   Policy authority: the workspace row (D2) — the shim is a dumb
   actuator + reporter speaking loopback eval (thin shim, D3): one
   login, then POST /api/eval with the session cookie (per-request
   Basic auth re-pays argon2id — measured 100x over budget). river
   owns frames; Nopales evals happen only on river events / poll ticks
   (soil contract). No shell rendering this pass (D5): set_borders is
   the only paint. Manager global bound at v1 (D1).

   Flags:
     --until N   exit 0 after N foreign-open AND N foreign-close policy
                 cycles (smoke mode); latency p50/p95 printed at exit
     --poll MS   row re-sync interval (default 500)
*)

open Eio.Std

module Registry = Wayland.Registry
module Rwm = River_window_management_v1_client
module Rxb = River_xkb_bindings_v1_client

let ( // ) = Filename.concat
(* fiber spawn: Eio.Std's `spawn` is shadowed by nothing here; alias for clarity *)

(* ---------- policy eval (thin shim; soil/binding-path (a)) ---------- *)

(* ---------- policy transport (thin shim; soil/binding-path (a)) ------

   Loopback HTTP to the image's /api/eval. The shim logs in ONCE (POST
   /login; the argon2id verify is paid once) and reuses the pp_session
   cookie: per-request Basic auth re-paid argon2 on EVERY policy eval
   (~0.25-1s, the AGENTS.md floor) which blew the event budget 100x
   (measured p95 740ms event->applied). With the cookie, policy evals
   run at loopback speed. Loopback only (soil security rule). *)

type http_cfg = {
  mutable host : string;
  mutable port : int;
  mutable cookie : string; (* pp_session token after login *)
  user : string;
  pass : string;
}

let uri_escape s =
  let buf = Buffer.create (String.length s) in
  String.iter
    (fun c ->
      if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')
         || c = '-' || c = '_' || c = '.' || c = '~'
      then Buffer.add_char buf c
      else Buffer.add_string buf (Printf.sprintf "%%%02X" (Char.code c)))
    s;
  Buffer.contents buf

let cfg_of_env () =
  let url = match Sys.getenv_opt "PP_URL" with Some u -> u | None -> "http://127.0.0.1:18080" in
  let rest =
    if String.starts_with ~prefix:"http://" url then
      String.sub url 7 (String.length url - 7)
    else url
  in
  let host, port =
    match String.index_opt rest ':' with
    | Some i ->
        (String.sub rest 0 i,
         try int_of_string (String.sub rest (i + 1) (String.length rest - i - 1)) with _ -> 18080)
    | None -> (rest, 18080)
  in
  { host; port; cookie = "";
    user = (match Sys.getenv_opt "PP_USER" with Some u -> u | None -> "admin");
    pass = (match Sys.getenv_opt "PP_PASS" with Some p -> p | None -> "admin") }

(* set in main before any eval *)
let http_env : (Eio_unix.Stdenv.base * Eio.Switch.t * http_cfg) option ref = ref None

(* One HTTP/1.1 request over a fresh loopback connection (Connection:
   close). Returns (status, headers, body). *)
let http_call ~method_ ~path ?body () =
  let env, sw, cfg =
    match !http_env with Some (e, s, c) -> (e, s, c) | None -> failwith "http_env unset"
  in
  let conn =
    Eio.Net.connect ~sw env#net (`Tcp (Eio.Net.Ipaddr.V4.loopback, cfg.port))
  in
  let hdr = Buffer.create 256 in
  Buffer.add_string hdr
    (Printf.sprintf "%s %s HTTP/1.1\r\nHost: %s:%d\r\n" method_ path cfg.host cfg.port);
  if cfg.cookie <> "" then
    Buffer.add_string hdr (Printf.sprintf "Cookie: pp_session=%s\r\n" cfg.cookie);
  (match body with
   | Some (ctype, b) ->
       Buffer.add_string hdr
         (Printf.sprintf "Content-Type: %s\r\nContent-Length: %d\r\n" ctype (String.length b))
   | None -> ());
  Buffer.add_string hdr "Connection: close\r\n\r\n";
  (match body with Some (_, b) -> Buffer.add_string hdr b | None -> ());
  Eio.Flow.write conn [ Cstruct.of_string (Buffer.contents hdr) ];
  (* Connection: close -> read to EOF *)
  let buf = Buffer.create 4096 in
  let tmp = Cstruct.create 8192 in
  let rec drain () =
    match Eio.Flow.single_read conn tmp with
    | 0 -> ()
    | n -> Buffer.add_string buf (Cstruct.to_string (Cstruct.sub tmp 0 n)); drain ()
    | exception End_of_file -> ()
  in
  (try drain () with _ -> ());
  Eio.Flow.close conn;
  let all = Buffer.contents buf in
  let split_pos () =
    let pat = "\r\n\r\n" in
    let pl = String.length pat and n = String.length all in
    let rec go i = if i + pl > n then None else if String.sub all i pl = pat then Some i else go (i + 1) in
    go 0
  in
  (match split_pos () with
  | None ->
      failwith
        ("soil-strip: bad HTTP response: "
        ^ String.escaped (String.sub all 0 (min 200 (String.length all))))
  | Some i ->
      let head = String.sub all 0 i in
      let body = String.sub all (i + 4) (String.length all - i - 4) in
      let lines = String.split_on_char '\n' head in
      let status =
        match lines with
        | sl :: _ -> (
            match String.split_on_char ' ' sl with
            | _ :: code :: _ -> ( try int_of_string code with _ -> 0 )
            | _ -> 0)
        | _ -> 0
      in
      let headers =
        List.filter_map
          (fun l ->
            match String.index_opt l ':' with
            | Some i ->
                Some
                  ( String.lowercase_ascii (String.trim (String.sub l 0 i)),
                    String.trim (String.sub l (i + 1) (String.length l - i - 1)) )
            | None -> None)
          (List.tl lines)
      in
      (status, headers, body))

let login cfg =
  let body = Printf.sprintf "username=%s&password=%s" (uri_escape cfg.user) (uri_escape cfg.pass) in
  let status, headers, _body =
    http_call ~method_:"POST" ~path:"/login"
      ~body:("application/x-www-form-urlencoded", body) ()
  in
  if status <> 303 && status <> 200 && status <> 302 then
    failwith (Printf.sprintf "soil-strip: login failed (HTTP %d)" status);
  let cookies =
    List.filter_map
      (fun (k, v) ->
        if k = "set-cookie" && String.starts_with ~prefix:"pp_session=" v then
          let rest = String.sub v 11 (String.length v - 11) in
          let token =
            match String.index_opt rest ';' with
            | Some i -> String.sub rest 0 i
            | None -> rest
          in
          Some token
        else None)
      headers
  in
  (match cookies with
   | c :: _ -> cfg.cookie <- c
   | [] -> failwith "soil-strip: login did not mint a pp_session cookie")

type st = {
  mutable manager : [`V1] Rwm.River_window_manager_v1.t option;
  mutable bindings_global : [`V1] Rxb.River_xkb_bindings_v1.t option;
  mutable seat : [`V1] Rwm.River_seat_v1.t option;
  mutable bindings : (string * [`V1] Rxb.River_xkb_binding_v1.t) list;
  mutable bindings_pending : bool;
  mutable ox : int;
  mutable oy : int;
  mutable ow : int;
  mutable oh : int;
  windows : (int, win) Hashtbl.t;
  mutable until : int;
  mutable opens : int;
  mutable closes : int;
  lat_apply : float list ref;
  timings : (string * float) list ref;
  mutable last_hash : string;
  (* manage computes it; the render sequence applies positions/borders *)
  mutable layout : ([`V1] Rwm.River_window_v1.t * [`V1] Rwm.River_node_v1.t * int * int * bool) list;
  exit_p : unit Promise.t * unit Promise.u;
}

and win = {
  wproxy : [`V1] Rwm.River_window_v1.t;
  key : string;
  mutable app_id : string;
  mutable opened : bool;
  mutable laid_out : bool;
  mutable t_event : float;
  mutable node : [`V1] Rwm.River_node_v1.t option; (* for set_position *)
}

let now () = Unix.gettimeofday ()

let json_of_value (v : Yojson.Safe.t) : Yojson.Safe.t =
  match v with
  | `String s -> ( try Yojson.Safe.from_string s with _ -> `String s )
  | v -> v

(* One policy eval: (fn args) via POST /api/eval with the session
   cookie. Returns the unwrapped value; times the call. *)
let eval_native st fn args =
  let t0 = now () in
  let expr = Printf.sprintf "(%s %s)" fn args in
  let body = Yojson.Safe.to_string (`Assoc [ ("expr", `String expr) ]) in
  let status, _headers, out =
    http_call ~method_:"POST" ~path:"/api/eval"
      ~body:("application/json", body) ()
  in
  if status <> 200 then
    failwith (Printf.sprintf "soil-strip: /api/eval HTTP %d: %s" status (String.escaped out));
  let j = Yojson.Safe.from_string out in
  let open Yojson.Safe.Util in
  (match member "ok" j with
  | `Bool true -> ()
  | _ -> failwith ("pp-eval error: " ^ Yojson.Safe.to_string j));
  st.timings := (fn, now () -. t0) :: !(st.timings);
  (* the lisp value may come back as a JSON string or an embedded JSON
     value -- unwrap the string form *)
  json_of_value (member "value" j)

(* ---------- row state -> layout ---------- *)

(* niri width preset -> grow weight (denominator 6; matches
   filmstrip/scene-w). *)
let weight_of (width : string) = match width with "half" -> 3 | "full" -> 6 | _ -> 2

type pane = { view : string; width : string; focused : bool }

let parse_state (v : Yojson.Safe.t) : pane list =
  let open Yojson.Safe.Util in
  match member "panes" v with
  | `List xs ->
      List.filter_map
        (fun p ->
          match p with
          | `Assoc _ ->
              (* dict-set* JSON-parses values, so "1" comes back as Int 1;
                 coerce any scalar to its string form. *)
              let s k =
                match member k p with
                | `String s -> s
                | `Null -> ""
                | j -> Yojson.Safe.to_string j
              in
              let view = s "view" in
              if view = "" then None
              else
                Some
                  {
                    view;
                    width = s "width";
                    focused = (s "focused") = "1";
                  }
          | _ -> None)
        xs
  | _ -> []

let amber = (224l, 175l, 104l, 255l)

(* The manage sequence: enable pending bindings, register unopened
   windows with policy, read the row, propose dimensions + focus, cache
   the layout, finish. The render sequence (apply_render) then places
   windows via their nodes. *)
let rec sync st =
  let manager = match st.manager with Some m -> m | None -> assert false in
  (* keybindings are enabled as manage-state, inside the sequence *)
  if st.bindings_pending then begin
    List.iter (fun (_, b) -> Rxb.River_xkb_binding_v1.enable b) st.bindings;
    st.bindings_pending <- false
  end;
  (* register unopened windows (policy eval; open-or-focus by view key).
     native-open RETURNS native-state — reuse it instead of a second eval. *)
  let state_from_opens = ref None in
  Hashtbl.iter
    (fun _ w ->
      if (not w.opened) && w.app_id <> "" then begin
        state_from_opens :=
          Some (eval_native st "filmstrip/native-open"
                  (Printf.sprintf "%S %S" w.app_id w.key));
        w.opened <- true;
        st.opens <- st.opens + 1
      end)
    st.windows;
  let state = match !state_from_opens with Some s -> s | None -> eval_native st "filmstrip/native-state" "" in
  let panes = parse_state state in
  let hash = Yojson.Safe.to_string state in
  (* window key -> win record *)
  let lookup key =
    let found = ref None in
    Hashtbl.iter
      (fun _ w -> if w.key = key then found := Some w)
      st.windows;
    !found
  in
  (* niri strip across the first output; weights over denominator
     max(6, total) so full+others shrinks proportionally (no scroll
     in v1). *)
  let view_panes = List.filter (fun (p : pane) -> lookup p.view <> None) panes in
  let total = List.fold_left (fun a (p : pane) -> a + weight_of p.width) 0 view_panes in
  let denom = max 6 total in
  let x = ref st.ox in
  let layout = ref [] in
  List.iter
    (fun (p : pane) ->
      match lookup p.view with
      | None -> ()
      | Some w ->
          let wpx = if denom = 0 then 0 else (st.ow * weight_of p.width) / denom in
          Rwm.River_window_v1.propose_dimensions w.wproxy
            ~width:(Int32.of_int (max 0 wpx)) ~height:(Int32.of_int (max 0 st.oh));
          let node =
            match w.node with
            | Some n -> n
            | None ->
                let n =
                  Rwm.River_window_v1.get_node w.wproxy
                    (object
                       inherit [_] Rwm.River_node_v1.v1
                    end)
                in
                w.node <- Some n;
                n
          in
          layout := (w.wproxy, node, !x, st.oy, p.focused) :: !layout;
          if p.focused then begin
            (match st.seat with
             | Some seat -> Rwm.River_seat_v1.focus_window seat ~window:w.wproxy
             | None -> ())
          end;
          x := !x + wpx)
    view_panes;
  st.layout <- List.rev !layout;
  Rwm.River_window_manager_v1.manage_finish manager;
  st.last_hash <- hash

(* The render sequence: place windows (set_position is rendering state
   on the node) and paint the focus ring. *)
and apply_render st =
  List.iter
    (fun (wproxy, node, x, y, focused) ->
      Rwm.River_node_v1.set_position node ~x:(Int32.of_int x) ~y:(Int32.of_int y);
      let (r, g, b, a) = amber in
      if focused then
        Rwm.River_window_v1.set_borders wproxy ~edges:15l ~width:2l ~r ~g ~b ~a
      else
        Rwm.River_window_v1.set_borders wproxy ~edges:0l ~width:0l ~r ~g ~b ~a;
      Hashtbl.iter
        (fun _ w ->
          if w.wproxy == wproxy && not w.laid_out then begin
            st.lat_apply := (now () -. w.t_event) :: !(st.lat_apply);
            w.laid_out <- true
          end)
        st.windows)
    st.layout;
  match st.manager with
  | Some m -> Rwm.River_window_manager_v1.render_finish m
  | None -> ()

let dirty st =
  match st.manager with
  | Some m -> Rwm.River_window_manager_v1.manage_dirty m
  | None -> ()

(* ---------- handler construction ---------- *)

(* Holder indirection: handler objects need st, st needs the manager
   proxy which is created by binding the handler. *)
let st_slot : st option ref = ref None

let with_st k = match !st_slot with Some st -> k st | None -> ()

let keysym_left = 0xff51l (* XK_Left *)
let keysym_right = 0xff53l (* XK_Right *)

let make_binding st ~dir ~keysym ~modifiers =
  match (st.bindings_global, st.seat) with
  | Some g, Some seat ->
      let handler =
        object
          inherit [_] Rxb.River_xkb_binding_v1.v1
          method on_pressed _ =
            with_st (fun st ->
                let dir_arg = if dir = "focus-left" then "left"
                  else if dir = "focus-right" then "right"
                  else if dir = "move-left" then "left"
                  else "right" in
                let fn = if String.starts_with ~prefix:"focus" dir then
                  "filmstrip/native-focus"
                else "filmstrip/native-move" in
                ignore (eval_native st fn (Printf.sprintf "%S" dir_arg));
                dirty st);
          method on_released _ = ();
          method on_stop_repeat _ = ()
        end
      in
      let b =
        Rxb.River_xkb_bindings_v1.get_xkb_binding g ~seat handler ~keysym
          ~modifiers
      in
      st.bindings <- (dir, b) :: st.bindings;
      st.bindings_pending <- true
  | _ -> ()

let rec manager_handler () =
  object
    inherit [_] Rwm.River_window_manager_v1.v1
    method on_unavailable _ =
      failwith "river: another window manager is already active"
    method on_finished _ = ()
    method on_manage_start _ = with_st sync
    method on_render_start _ = with_st apply_render
    method on_session_locked _ = ()
    method on_session_unlocked _ = ()
    method on_window _ wproxy =
      let key = "w" ^ Int32.to_string (Wayland.Proxy.id wproxy) in
      let w =
        {
          wproxy;
          key;
          app_id = "";
          opened = false;
          laid_out = false;
          t_event = now ();
          node = None;
        }
      in
      with_st (fun st ->
          Hashtbl.replace st.windows (Int32.to_int (Wayland.Proxy.id wproxy)) w;
          Wayland.Proxy.Handler.attach wproxy
            (window_handler st w));
    method on_output _ oproxy =
      with_st (fun st ->
          Wayland.Proxy.Handler.attach oproxy
            (object
               inherit [_] Rwm.River_output_v1.v1
               method on_removed _ = ()
               method on_wl_output _ ~name:_ = ()
               method on_position _ ~x ~y =
                 st.ox <- Int32.to_int x;
                 st.oy <- Int32.to_int y
               method on_dimensions _ ~width ~height =
                 st.ow <- Int32.to_int width;
                 st.oh <- Int32.to_int height
               method on_capture_sessions _ ~count:_ = ()
            end))
    method on_seat _ sproxy =
      with_st (fun st ->
          st.seat <- Some sproxy;
          Wayland.Proxy.Handler.attach sproxy
            (object
               inherit [_] Rwm.River_seat_v1.v1
               method on_removed _ = ()
               method on_wl_seat _ ~name:_ = ()
               method on_pointer_enter _ ~window:_ = ()
               method on_pointer_leave _ = ()
               method on_window_interaction _ ~window:_ = ()
               method on_shell_surface_interaction _ ~shell_surface:_ = ()
               method on_op_delta _ ~dx:_ ~dy:_ = ()
               method on_op_release _ = ()
               method on_pointer_position _ ~x:_ ~y:_ = ()
            end);
          (* alt+arrows: focus; alt+shift+arrows: move (D6: the physical
             keypress path shares the browser-leg code). *)
          let alt = Rwm.River_seat_v1.Modifiers.mod1 in
          let alt_shift = Int32.logor alt Rwm.River_seat_v1.Modifiers.shift in
          make_binding st ~dir:"focus-left" ~keysym:keysym_left ~modifiers:alt;
          make_binding st ~dir:"focus-right" ~keysym:keysym_right ~modifiers:alt;
          make_binding st ~dir:"move-left" ~keysym:keysym_left ~modifiers:alt_shift;
          make_binding st ~dir:"move-right" ~keysym:keysym_right ~modifiers:alt_shift)
  end

and window_handler st (w : win) =
  object
    inherit [_] Rwm.River_window_v1.v1
    method on_closed _ =
      if w.opened then (
        ignore (eval_native st "filmstrip/native-close" (Printf.sprintf "%S" w.key));
        st.closes <- st.closes + 1;
        Hashtbl.remove st.windows (Int32.to_int (Wayland.Proxy.id w.wproxy));
        if st.until > 0 && st.opens >= st.until && st.closes >= st.until then
          Promise.resolve (snd st.exit_p) ())
      else Hashtbl.remove st.windows (Int32.to_int (Wayland.Proxy.id w.wproxy))
    method on_dimensions_hint _ ~min_width:_ ~min_height:_ ~max_width:_ ~max_height:_ = ()
    method on_dimensions _ ~width:_ ~height:_ = ()
    method on_app_id _ ~app_id =
      w.app_id <- match app_id with Some s -> s | None -> "app"
    method on_title _ ~title:_ = ()
    method on_parent _ ~parent:_ = ()
    method on_decoration_hint _ ~hint:_ = ()
    method on_pointer_move_requested _ ~seat:_ = ()
    method on_pointer_resize_requested _ ~seat:_ ~edges:_ = ()
    method on_show_window_menu_requested _ ~x:_ ~y:_ = ()
    method on_maximize_requested _ = ()
    method on_unmaximize_requested _ = ()
    method on_fullscreen_requested _ ~output:_ = ()
    method on_exit_fullscreen_requested _ = ()
    method on_minimize_requested _ = ()
    method on_unreliable_pid _ ~unreliable_pid:_ = ()
    method on_presentation_hint _ ~hint:_ = ()
    method on_identifier _ ~identifier:_ = ()
    method on_capture_sessions _ ~count:_ = ()
  end

(* ---------- args + main ---------- *)

let usage =
  "usage: soil-strip [--until N] [--poll MS]\n\
  \  --until N   exit 0 after N foreign-open AND N foreign-close cycles\n\
  \  --poll MS   row re-sync interval (default 500)"

let rec main () =
  let until = ref 0 and poll_ms = ref 500 in
  let rec go = function
    | [] -> ()
    | "--until" :: n :: rest ->
        until := (try int_of_string n with _ -> failwith ("bad --until: " ^ n));
        go rest
    | "--poll" :: n :: rest ->
        poll_ms := (try int_of_string n with _ -> failwith ("bad --poll: " ^ n));
        go rest
    | ("-h" | "--help") :: _ ->
        print_endline usage;
        exit 0
    | x :: _ ->
        prerr_endline ("soil-strip: unknown arg " ^ x);
        prerr_endline usage;
        exit 2
  in
  (match Array.to_list Sys.argv with _ :: rest -> go rest | [] -> ());
  Eio_main.run @@ fun env ->
  Switch.run @@ fun sw ->
  let http_cfg = cfg_of_env () in
  http_env := Some (env, sw, http_cfg);
  login http_cfg;
  Logs.info (fun f -> f "policy channel: logged in (session cookie)");
  let transport = Wayland.Unix_transport.connect ~sw ~net:env#net () in
  let display = Wayland.Client.connect ~sw transport in
  let reg = Registry.of_display display in
  let bindings_global = Registry.bind reg (new Rxb.River_xkb_bindings_v1.v1) in
  let manager = Registry.bind reg (manager_handler ()) in
  let exit_p = Promise.create () in
  let st =
    {
      manager = Some manager;
      bindings_global = Some bindings_global;
      seat = None;
      bindings = [];
      bindings_pending = false;
      ox = 0;
      oy = 0;
      ow = 1920;
      oh = 1080;
      windows = Hashtbl.create 8;
      until = !until;
      opens = 0;
      closes = 0;
      lat_apply = ref [];
      timings = ref [];
      last_hash = "";
      layout = [];
      exit_p;
    }
  in
  st_slot := Some st;
  Logs.info (fun f ->
      f "soil-strip attached (manager v1); poll=%dms until=%d" !poll_ms !until);
  (* poller: browser-driven row changes reach river via manage_dirty *)
  let _poller =
    Fiber.fork ~sw (fun () ->
        let s = Float.of_int !poll_ms /. 1000. in
        while true do
          Eio.Time.sleep env#clock s;
          with_st (fun st ->
              match st.manager with
              | Some m when not (Promise.is_resolved (fst st.exit_p)) ->
                  let state = eval_native st "filmstrip/native-state" "" in
                  let hash = Yojson.Safe.to_string state in
                  if hash <> st.last_hash then
                    Rwm.River_window_manager_v1.manage_dirty m
              | _ -> ())
        done)
  in
  Promise.await (fst exit_p);
  percentile_report st;
  (try Wayland.Client.stop display with _ -> ());
  (* the wayland dispatch thread may outlive the switch teardown; exit
     explicitly now that the report is out (documented v1 shortcut) *)
  exit 0

and percentile_report st =
  let evals =
    List.filter_map
      (fun (fn, dt) ->
        if fn = "filmstrip/native-state" then None else Some dt)
      !(st.timings)
  in
  let p50, p95 = p50_p95 !(st.lat_apply) in
  let ep50, ep95 = p50_p95 evals in
  Printf.printf "soil-strip latency: event->applied p50=%.1fms p95=%.1fms (n=%d) | policy-eval p50=%.1fms p95=%.1fms (n=%d)\n"
    (p50 *. 1000.) (p95 *. 1000.) (List.length !(st.lat_apply))
    (ep50 *. 1000.) (ep95 *. 1000.) (List.length evals);
  flush stdout

and p50_p95 xs =
  match List.sort compare xs with
  | [] -> (0.0, 0.0)
  | xs ->
      let n = List.length xs in
      let nth k = List.nth xs (min (n - 1) k) in
      (nth (n / 2), nth ((n * 95) / 100))

let () =
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.(set_level (Some Info));
  Printexc.record_backtrace true;
  try main () with
  | Failure msg ->
      prerr_endline ("soil-strip: " ^ msg);
      exit 1
  | Yojson.Json_error msg ->
      prerr_endline ("soil-strip: bad policy JSON: " ^ msg);
      exit 1
  | Unix.Unix_error (e, fn, arg) ->
      prerr_endline
        (Printf.sprintf "soil-strip: %s: %s%s" fn (Unix.error_message e)
           (if arg = "" then "" else ": " ^ arg));
      exit 1
