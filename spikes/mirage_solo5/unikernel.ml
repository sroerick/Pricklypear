open Lwt.Infix

module Hello
    (P : Mirage_ptime.S)
    (M : Mirage_mtime.S) = struct

  let start _pc _mc =
    Logs.info (fun m -> m "hello from mirage solo5 spike (phase 1 probe OK)");
    Lwt.return_unit

end
