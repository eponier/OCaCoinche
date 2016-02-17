open Game
open Config
open Carte

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
    | Dix -> "9"
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

(* anchor => point central où l'on va afficher la main  *)
let anchor_p1 = tapis_w / 2, tapis_h - card_h


let draw_visible_hand screen (cards : carte list) =
  let taille = List.length cards in
  let range_p1 = card_w * taille in
  let positions = 
    List.mapi (fun i _ -> 
      
      (* TODO *)
      let y = snd anchor_p1 in
      let x = (taille / 2 - i) * (range_p1 / taille) in
      x,y
    ) cards
  in
  List.iteri
    (fun i c -> 
      let pos = List.nth positions i in
      let dst_rect = 
	Sdlvideo.rect
	  (fst pos) (snd pos)
	  card_w card_h
      in
      let src = CardImgMap.find c map_img in
      Sdlvideo.blit_surface ~dst_rect ~src ~dst:screen ()      
    )
    cards


let draw_cards screen = function
  | [| p1; p2; p3; p4 |] ->
    begin
      draw_visible_hand screen p1;
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
    =

  (* draw table *)
  Sdlvideo.blit_surface ~dst_rect:tapis_rect ~src:tapis ~dst:screen ();

  (* draw cards *)
  draw_cards screen mains;
  
  show_string screen (500,300) 55 "LA BAGARRE";
  Sdlvideo.flip screen;
  ()
