(* Public, NON-INTERACTIVE board pane (card a6ca7850): the factory's card
   board as a read-only filmstrip pane.

   Public-by-default visibility policy (5a30a045): card TITLES and STATES
   are public; card NOTES may hold internal detail, so this projection
   carries title+state ONLY. The [notes] field rides the record exactly so
   the renderer can DROP it -- the redaction line is enforced here, not by
   the caller.

   The renderer emits no form, no hx-* attribute, no link and no POST
   target: the pane is inert markup, safe on an anonymous route. The lisp
   side (libs/filmstrip/lib.pp filmstrip/board-pane-html) feeds it
   (public-board-tiles), which the prim of the same name registers over
   this function -- and test/public_board_probe.ml (driven by
   libs/filmstrip/test/vectors.ksh) calls that same function. *)

type card = { title : string; state : string; notes : string }

let esc s =
  let b = Buffer.create (String.length s) in
  String.iter
    (fun c ->
      match c with
      | '&' -> Buffer.add_string b "&amp;"
      | '<' -> Buffer.add_string b "&lt;"
      | '>' -> Buffer.add_string b "&gt;"
      | '"' -> Buffer.add_string b "&quot;"
      | '\'' -> Buffer.add_string b "&#39;"
      | c -> Buffer.add_char b c)
    s;
  Buffer.contents b

(* One tile: the card title and its state, and nothing else. *)
let tile c =
  Printf.sprintf
    "<li class=\"kb-tile\"><span class=\"kb-tile-title\">%s</span>\
     <span class=\"kb-tile-state\">%s</span></li>"
    (esc c.title) (esc c.state)

(* States in first-seen order, so the pane's grouping follows the board's
   own read order instead of inventing a private column order. *)
let groups cards =
  List.fold_left
    (fun acc c -> if List.mem c.state acc then acc else acc @ [ c.state ])
    [] cards

let render_body cards =
  let block st =
    let tiles =
      List.filter_map (fun c -> if c.state = st then Some (tile c) else None) cards
    in
    Printf.sprintf
      "<div class=\"kb-board-state-block\"><h3 class=\"kb-board-state\">%s</h3>\
       <ul class=\"kb-board-tiles\">%s</ul></div>"
      (esc st) (String.concat "" tiles)
  in
  String.concat "" (List.map block (groups cards))
  ^ "<p class=\"muted kb-board-live\">Live: read from the board API on each \
     request. Read-only: titles and states only.</p>"
