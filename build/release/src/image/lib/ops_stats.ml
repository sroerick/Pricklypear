(** In-process product analytics counters (today).

    Not per-hit Postgres. See product-experience.analytics. *)

module SSet = Set.Make (String)
module SMap = Map.Make (String)

type day_stats = {
  mutable day : string;
  mutable page_views : int;
  mutable authed_requests : int;
  mutable public_uniques : SSet.t;
  mutable active_users : SSet.t;
  mutable path_counts : int SMap.t;
  mutable errors : int;
  mutable fatals : int;
  mutable noise_views : int;
}

let mutex = Mutex.create ()

let today_str () =
  let tm = Unix.gmtime (Unix.gettimeofday ()) in
  Printf.sprintf "%04d-%02d-%02d"
    (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1) tm.Unix.tm_mday

let empty_day day =
  { day
  ; page_views = 0
  ; authed_requests = 0
  ; public_uniques = SSet.empty
  ; active_users = SSet.empty
  ; path_counts = SMap.empty
  ; errors = 0
  ; fatals = 0
  ; noise_views = 0
  }

let state = ref (empty_day (today_str ()))

let ensure_today () =
  let d = today_str () in
  if (!state).day <> d then state := empty_day d

let hash_viewer ~(ip : string) ~(day : string) : string =
  (* Stable opaque id for the day; not reversible in the UI. *)
  let open Digestif.SHA256 in
  let dig = digest_string (day ^ "|" ^ ip) in
  let hex = to_hex dig in
  String.sub hex 0 (min 16 (String.length hex))

let path_class (path : string) : string =
  let p =
    match String.index_opt path '?' with
    | Some i -> String.sub path 0 i
    | None -> path
  in
  if p = "" then "/"
  else if String.length p >= 8 && String.sub p 0 8 = "/assets/" then "/assets/*"
  else if String.length p >= 8 && String.sub p 0 8 = "/static/" then "/static/*"
  else if String.length p >= 5 && String.sub p 0 5 = "/code" then "/code/*"
  else if String.length p >= 5 && String.sub p 0 5 = "/data" then "/data/*"
  else if String.length p >= 8 && String.sub p 0 8 = "/binding" then "/bindings/*"
  else if String.length p >= 5 && String.sub p 0 5 = "/edit" then "/edit/*"
  else if String.length p >= 8 && String.sub p 0 8 = "/partial" then "/partials/*"
  else if String.length p >= 4 && String.sub p 0 4 = "/api" then
    (match String.index_opt p '/' with
     | None -> "/api"
     | Some i when i = 0 ->
         (match String.index_from_opt p 1 '/' with
          | None -> p
          | Some j -> String.sub p 0 j)
     | Some _ -> p)
  else p

let is_noise_path (path : string) : bool =
  let p = String.lowercase_ascii path in
  let has sub =
    let n = String.length sub in
    let rec find i =
      if i + n > String.length p then false
      else if String.sub p i n = sub then true
      else find (i + 1)
    in
    find 0
  in
  has "wp-admin" || has "wp-login" || has ".env" || has "phpmyadmin"
  || has "xmlrpc" || has "/.git" || has "cgi-bin"

let is_excluded_path (path : string) : bool =
  path = "/health"
  || (String.length path >= 8 && String.sub path 0 8 = "/assets/")
  || (String.length path >= 8 && String.sub path 0 8 = "/static/")

let client_ip ~(headers : (string * string) list) ~(sockaddr : Unix.sockaddr)
    : string =
  let lower = List.map (fun (k, v) -> (String.lowercase_ascii k, v)) headers in
  match List.assoc_opt "x-forwarded-for" lower with
  | Some v when String.trim v <> "" ->
      (match String.split_on_char ',' v with
       | h :: _ -> String.trim h
       | [] -> String.trim v)
  | _ ->
      (match List.assoc_opt "x-real-ip" lower with
       | Some v when String.trim v <> "" -> String.trim v
       | _ ->
           (match sockaddr with
            | Unix.ADDR_INET (a, _) -> Unix.string_of_inet_addr a
            | Unix.ADDR_UNIX _ -> "unix"))

let record_request
    ~(path : string)
    ~(is_authed : bool)
    ~(username : string option)
    ~(headers : (string * string) list)
    ~(sockaddr : Unix.sockaddr)
  : unit =
  if is_excluded_path path then ()
  else begin
    Mutex.lock mutex;
    (try
       ensure_today ();
       let s = !state in
       let cls = path_class path in
       if is_noise_path path then s.noise_views <- s.noise_views + 1
       else begin
         s.page_views <- s.page_views + 1;
         s.path_counts <-
           SMap.update cls
             (function None -> Some 1 | Some n -> Some (n + 1))
             s.path_counts;
         let day = s.day in
         let ip = client_ip ~headers ~sockaddr in
         let vk = hash_viewer ~ip ~day in
         if not is_authed then
           s.public_uniques <- SSet.add vk s.public_uniques;
         if is_authed then begin
           s.authed_requests <- s.authed_requests + 1;
           (match username with
            | Some u when u <> "" && u <> "anonymous" ->
                s.active_users <- SSet.add u s.active_users
            | _ -> ())
         end
       end
     with ex -> Mutex.unlock mutex; raise ex);
    Mutex.unlock mutex
  end

let note_error ~(fatal : bool) : unit =
  Mutex.lock mutex;
  (try
     ensure_today ();
     let s = !state in
     s.errors <- s.errors + 1;
     if fatal then s.fatals <- s.fatals + 1
   with ex -> Mutex.unlock mutex; raise ex);
  Mutex.unlock mutex

let snapshot () =
  Mutex.lock mutex;
  let s =
    try
      ensure_today ();
      let cur = !state in
      let top =
        SMap.bindings cur.path_counts
        |> List.sort (fun (_, a) (_, b) -> compare b a)
        |> fun xs ->
           let rec take n xs =
             if n <= 0 then [] else match xs with [] -> [] | h :: t -> h :: take (n - 1) t
           in
           take 15 xs
      in
      ( cur.day
      , cur.page_views
      , SSet.cardinal cur.public_uniques
      , cur.authed_requests
      , SSet.cardinal cur.active_users
      , cur.errors
      , cur.fatals
      , cur.noise_views
      , top
      , SSet.elements cur.active_users )
    with ex -> Mutex.unlock mutex; raise ex
  in
  Mutex.unlock mutex;
  s

let summary_yojson ~(logins_ok : int) ~(logins_fail : int) : Yojson.Safe.t =
  let day, views, uniques, authed, active_n, errors, fatals, noise, top, users =
    snapshot ()
  in
  `Assoc
    [ ("day", `String day)
    ; ("page_views", `Int views)
    ; ("public_uniques", `Int uniques)
    ; ("authed_requests", `Int authed)
    ; ("active_users", `Int active_n)
    ; ("active_user_names", `List (List.map (fun u -> `String u) users))
    ; ("logins_ok", `Int logins_ok)
    ; ("logins_fail", `Int logins_fail)
    ; ("errors", `Int errors)
    ; ("fatals", `Int fatals)
    ; ("noise_views", `Int noise)
    ; ( "top_paths"
      , `List
          (List.map
             (fun (p, n) -> `Assoc [ ("path", `String p); ("n", `Int n) ])
             top) )
    ]
