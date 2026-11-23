(** PP forward-gate seam (P1).

    The optional adapter installs the matcher + responder; the base binary
    installs nothing, so no path is forwarded and the base library has no
    dependency on glochid_pp. [register] in glochid/pp/habitat.ml calls
    [install] only under explicit activation (/--pp/ or GLOCHID_PP=1). *)

type response = (int * (string * string) list * string, string) result

let matcher : (string -> string option) ref = ref (fun _ -> None)

let responder :
    (env:Eio_unix.Stdenv.base -> meth:string -> path:string -> query:string ->
     body:string -> hx:bool -> response) ref =
  ref (fun ~env:_ ~meth:_ ~path:_ ~query:_ ~body:_ ~hx:_ ->
      Error "no Pricklypear adapter installed")

let matches (path : string) : string option = !matcher path

let respond ~env ~meth ~path ~query ~body ~hx : response =
  !responder ~env ~meth ~path ~query ~body ~hx

let install ~(match_path : string -> string option)
    ~(respond_with :
       env:Eio_unix.Stdenv.base -> meth:string -> path:string -> query:string
       -> body:string -> hx:bool -> response) : unit =
  matcher := match_path;
  responder := respond_with
