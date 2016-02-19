open Game
open Config
open Carte
open Utils

type view_env  = {
  (* (pos_x X pos_y) X (width_x X width_y) *)
  cards_position : ((int*int)*(int*int)) list
  ; card_raised : int option
  ; should_redraw : bool
}

(* anchor => point central où l'on va afficher la main  *)
let anchor_p1 = tapis_w / 2, tapis_h - card_h

let init_view_env env =
  let cards = env.mains.(0) in
  let len = List.length cards in
  let ratio = 0.6 +. (float (len - 1)) *. (0.2 /. 7.)  in
  let positions = 
    let len' = len / 2 in
    (* Printf.printf "%f\n" ratio; *)
    if len mod 2 = 0 then
      let l1,l2 = split_n_prems cards len' in
      List.mapi (fun i _ -> 
	let x = int_of_float (float ((i - len') * card_w) *. ratio) + tapis_w / 2 in
	x, snd anchor_p1
      ) l1 
      @
	List.mapi (fun i _ -> 
	  let x = int_of_float (float (i * card_w) *. ratio) + tapis_w / 2 in
	  x, snd anchor_p1
	) l2

    else
      let l1,l2 = split_n_prems cards len' in
      List.mapi (fun i _ -> 
	let x = int_of_float (float ((i - len') * card_w) *. ratio) + tapis_w / 2 
	  - card_w / 2
	in
	x, snd anchor_p1
      ) l1 
      @ [tapis_w / 2 - card_w / 2, snd anchor_p1]
      @ List.mapi (fun i _ -> 
	let x = int_of_float (float (i * card_w) *. ratio) + tapis_w / 2 
	  + (int_of_float (float (card_w / 2) *. (1. -. ratio))) in
	x, snd anchor_p1
      ) (List.tl l2)
  in
  { cards_position = List.map 
      (fun ((x,y) as pos) -> 
	let width =  int_of_float (float card_w *. ratio) in
	let height = card_h in
	pos, (width, height))
      positions
  ; card_raised = None
  ; should_redraw = false}
    


let show_string screen (x,y) size s =  
  let font = Sdlttf.open_font font_file size in
  let text = Sdlttf.render_text_blended font s ~fg:Sdlvideo.black in
  Sdlvideo.blit_surface ~src:text ~dst_rect:(Sdlvideo.rect x y 100 50) ~dst:screen ();
  Sdlvideo.flip screen

let tapis = Sdlloader.load_image tapis_file
let tapis_rect = Sdlvideo.rect 0 0 tapis_w tapis_h

module CardImgMap = Map.Make(struct type t = carte let compare = compare end)

let get_card_filename ((v,c) : carte) = 
  let v = 
    match v with 
    | Sept -> "7"
    | Huit -> "8"
    | Neuf -> "9"
    | Dix -> "10"
    | Valet -> "v"
    | Dame -> "d"
    | Roi -> "r"
    | As -> "as"
  in
  let c = 
    match c with 
    | Trefle -> "tr"
    | Coeur -> "co"
    | Pique -> "pi"
    | Carreau -> "ca"
  in
  share_path ^ v ^ "_" ^ c ^ ".png"

let map_img = 
  let open CardImgMap in
  paquet
  |> List.fold_left 
      (fun acc c -> 
	let img = Sdlloader.load_image (get_card_filename c) in
	CardImgMap.add c img acc)
      CardImgMap.empty

(* Draws player 1 cards *)
let draw_visible_hand screen env view_env =
  let cards = env.mains.(0) in
  let taille = List.length cards in
  let positions = view_env.cards_position in
  assert (taille > 0);
  (* DON'T LOOK BACK IN ANGEEER *)
  List.iteri
    (fun i c -> 
      let ((x,y), (w,h)) = List.nth positions i in
      let y = match view_env.card_raised, i with
	| Some i, v when i = v -> y - 10
	| _ -> y
      in
      let dst_rect = Sdlvideo.rect x y w h in
      let src = CardImgMap.find c map_img in
      Sdlvideo.blit_surface ~dst_rect ~src ~dst:screen ()      
    )
    cards

exception Found of int

let raise_p1_card env view_env (x,y) = 
  let positions = view_env.cards_position in
  let cards = env.mains.(0) in
  let card_raised = 
    try 
      List.iteri
	(fun i pos -> 
	  if is_rect_intersect (x,y) pos then
	    (* TODO : filtrer les cartes jouables *)
	    (* let card = List.nth cards i in *)
	    (* let playable = valides  *)
	    raise (Found i)
	) positions;
      None
    with Found i -> Some i
  in
  {view_env with card_raised ; should_redraw =  view_env.card_raised <> card_raised}  

let draw_cards screen env view_env =
  match env.mains with
  | [| p1; p2; p3; p4 |] ->
    begin
      draw_visible_hand screen env view_env;
      ()	
    end
  | _ -> assert false
  
let draw screen
    ({ state
     ; mains
     ; plis
     ; contrat
     ; est_coinche
     ; qui_part
     ; scores
     } as env) 
    view_env
    =

  (* draw table *)
  Sdlvideo.blit_surface ~dst_rect:tapis_rect ~src:tapis ~dst:screen ();

  (* draw cards *)
  draw_cards screen env view_env;
  
  show_string screen (500,300) 55 "LA BAGARRE";
  Sdlvideo.flip screen;
  ()
