(** On-demand LLM complexity analysis + cache in data_rows. *)
module Ast = Pricklypear_lisp.Ast
module Reader = Pricklypear_lisp.Reader
module Comp = Pricklypear_lisp.Complexity
module Expl = Pricklypear_lisp.Explain

let sha1_hex (s : string) : string =
  let d = Digestif.SHA1.digest_string s in
  Digestif.SHA1.to_hex d

let ast_hash (e : Ast.expr) : string = sha1_hex (Reader.pp_expr e)

let heuristic_json ~(fn_name : string) (e : Ast.expr) : Yojson.Safe.t =
  let cx = Comp.classify ~fn_name e in
  `Assoc
    [
      ("complexity", `String (Comp.badge cx));
      ("confidence", `String "heuristic");
      ("rationale", `String "static pattern match (no LLM)");
      ("source", `String "heuristic");
    ]

let parse_llm_json (raw : string) : (Yojson.Safe.t, string) result =
  let strip_fence s =
    let s = String.trim s in
    if String.length s > 8 && String.starts_with ~prefix:"```" s then
      let s =
        match String.index_opt s '\n' with
        | None -> s
        | Some i -> String.sub s (i + 1) (String.length s - i - 1)
      in
      if String.ends_with ~suffix:"```" s then
        String.sub s 0 (String.length s - 3) |> String.trim
      else s
    else s
  in
  try Ok (Yojson.Safe.from_string (strip_fence raw))
  with _ -> Error ("analyze: LLM response not JSON: " ^ String.sub raw 0 (min 200 (String.length raw)))

let cache_get (store : Store.t) ~(hash : string) : Yojson.Safe.t option =
  match Crud.find_row store ~type_name:"function_analysis" ~field:"cache_hash" ~value:hash with
  | Ast.VNil -> None
  | Ast.VPair (Ast.VStr _id, Ast.VStr fields_json) ->
      (try
         let j = Yojson.Safe.from_string fields_json in
         Some (Yojson.Safe.Util.member "result" j)
       with _ -> None)
  | Ast.VStr fields_json ->
      (* some paths return fields only *)
      (try
         let j = Yojson.Safe.from_string fields_json in
         match j with
         | `Assoc _ ->
             (match Yojson.Safe.Util.member "result" j with
              | `Null -> Some j
              | r -> Some r)
         | _ -> None
       with _ -> None)
  | _ -> None

let cache_put (store : Store.t) ~(author : string) ~(fn_name : string)
    ~(hash : string) ~(result : Yojson.Safe.t) : unit =
  let fields =
    `Assoc
      [
        ("fn", `String fn_name);
        ("cache_hash", `String hash);
        ("result", result);
        ("when", `String (string_of_float (Unix.gettimeofday ())));
      ]
  in
  let fields_json = Yojson.Safe.to_string fields in
  ignore
    (Crud.create store ~type_name:"function_analysis" ~fields_json ~author)

let resolve_llm_creds (store : Store.t) ~(user_id : string option) :
    Llm_client.creds option =
  match user_id with
  | Some uid ->
      (match Auth_store.get_llm_key_secret store ~user_id:uid with
       | Some (key, meta) ->
           Some
             {
               Llm_client.api_key = key;
               api_base =
                 (match meta.Auth_store.base with
                  | Some b when b <> "" -> b
                  | _ -> Llm_client.api_base ());
               model =
                 (match meta.Auth_store.model with
                  | Some m when m <> "" -> m
                  | _ -> Llm_client.model ());
             }
       | None ->
           let k = Llm_client.api_key () in
           if k = "" then None
           else
             Some
               {
                 api_key = k;
                 api_base = Llm_client.api_base ();
                 model = Llm_client.model ();
               })
  | None ->
      let k = Llm_client.api_key () in
      if k = "" then None
      else
        Some
          {
            api_key = k;
            api_base = Llm_client.api_base ();
            model = Llm_client.model ();
          }

let llm_analyze ~(fn_name : string) (e : Ast.expr) ~(creds : Llm_client.creds)
    : (Yojson.Safe.t, string) result =
  let pp = Reader.pp_expr e in
  let system =
    "You are a code complexity analyzer for a Scheme-family Lisp. \
     Respond ONLY with JSON: {\"complexity\":\"O(...)\",\"confidence\":\"low|medium|high\",\"rationale\":\"...\",\"notes\":\"...\"}. \
     Prefer honest uncertainty over false precision."
  in
  let user =
    Printf.sprintf "Function name: %s\n\nSource:\n%s\n" fn_name pp
  in
  match
    Llm_client.with_creds creds (fun () ->
        Llm_client.call ~temperature:0.1 ~max_tokens:800
          ~system ~user ())
  with
  | Error e -> Error e
  | Ok raw ->
      (match parse_llm_json raw with
       | Ok j ->
           let open Yojson.Safe.Util in
           Ok
             (`Assoc
                [
                  ("complexity", member "complexity" j);
                  ("confidence", member "confidence" j);
                  ("rationale", member "rationale" j);
                  ("notes", member "notes" j);
                  ("source", `String "llm");
                  ("model", `String creds.Llm_client.model);
                  ("label", `String "LLM estimate");
                ])
       | Error e -> Error e)

let analyze_function (store : Store.t) ~(author : string)
    ~(user_id : string option) ~(fn_name : string) ~(force_llm : bool)
    : (Yojson.Safe.t, string) result =
  match Store.lookup_function store fn_name with
  | None -> Error ("analyze: no function " ^ fn_name)
  | Some (expr, _, _) ->
      let hash = ast_hash expr in
      (match cache_get store ~hash with
       | Some j when not force_llm -> Ok j
       | _ ->
           let heur = heuristic_json ~fn_name expr in
           if not force_llm then Ok heur
           else
             (match resolve_llm_creds store ~user_id with
              | None ->
                  Error
                    "analyze: no LLM key configured (set user key at \
                     /settings/keys or LLM_API_KEY)"
              | Some creds ->
                  (match llm_analyze ~fn_name expr ~creds with
                   | Error e -> Error e
                   | Ok j ->
                       cache_put store ~author ~fn_name ~hash ~result:j;
                       Ok j)))

let explain_function (store : Store.t) ~(fn_name : string)
    : (string, string) result =
  match Store.lookup_function store fn_name with
  | None -> Error ("explain: no function " ^ fn_name)
  | Some (expr, _, _) ->
      let layers = Expl.peel ~fn_name expr in
      Ok (String.concat "\n\n" (Expl.layers_to_strings layers))

let result_to_vstr = function
  | Ok (`String s) -> Ast.VStr s
  | Ok j -> Ast.VStr (Yojson.Safe.to_string j)
  | Error e -> Ast.VStr e
