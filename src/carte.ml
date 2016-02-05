(****** Cartes *********)
type couleur = Trefle | Coeur | Pique | Carreau

type valeur = Sept | Huit | Neuf | Valet | Dame | Roi | Dix | As

type carte = valeur * couleur

(******** operation sur un paquet de carte *******)
let paquet = 
  let vals = [Sept ; Huit ; Neuf ; Valet ; Dame ; Roi ; Dix ; As] in
  let trefles = List.map (fun e -> e,Trefle) vals 
  and coeurs = List.map (fun e -> e,Coeur) vals
  and piques = List.map (fun e -> e,Pique) vals 
  and carreaux = List.map (fun e -> e,Carreau) vals 
  in List.concat [trefles;coeurs;piques;carreaux]

let melange : carte list -> carte list = Utils.shuffle

let distribue paquet =
  let joueurs = [|[];[];[];[]|] in
  let push c i = joueurs.(i) <- c::joueurs.(i) in
  let rec aux liste nb =
    match liste with
    | [] -> ()
    | h::tl when nb < 12 -> begin push h (nb/3); aux tl (nb+1) end
    | h::tl when nb < 20 -> begin push h ((nb-12)/2); aux tl (nb+1) end
    | h::tl ->  begin push h ((nb-20)/3); aux tl (nb+1) end
  in
  aux paquet 0;
  joueurs

let coupe paquet =
  let taille = List.length paquet in
  let coupe = ((Random.int (taille-2)) + (Random.int (taille-2))) / 2 + 1 in
  let (l1,l2) = Utils.split_n_prems paquet coupe in
  l2@l1

(******** Operations sur les cartes *********)
let nb_point_normal = function
  | Sept | Huit | Neuf -> 0
  | Valet -> 2
  | Dame -> 3
  | Roi -> 4
  | Dix -> 10
  | As -> 11

let nb_point_atout = function
  | Sept | Huit -> 0
  | Dame -> 3
  | Roi -> 4
  | Dix -> 10
  | As -> 11
  | Neuf -> 14
  | Valet -> 20

let nb_point atout (v,c) = 
  if atout=c then nb_point_atout v 
  else nb_point_normal v

(* compare deux cartes de la meme couleur. ne peut pas etre les memes *)
let compare_meme v1 v2 = 
  match v1,v2 with
  | Sept,_ -> -1
  | _,Sept -> 1
  | Huit,_ -> -1
  | _,Huit -> 1
  | Neuf,_ -> -1
  | _,Neuf -> 1
  | Valet,_ -> -1
  | _,Valet -> 1 
  | Dame,_ -> -1 
  | _,Dame -> 1
  | Roi,_ -> -1
  | _,Roi -> 1
  | Dix,_ -> -1
  | _,Dix -> 1
  | _ -> assert false

(* compare deux atouts. ne peut pas etre les memes *)
let compare_atout v1 v2 = 
  match v1,v2 with
  | Sept,_ -> -1
  | _,Sept -> 1
  | Huit,_ -> -1
  | _,Huit -> 1
  | Dame,_ -> -1 
  | _,Dame -> 1
  | Roi,_ -> -1
  | _,Roi -> 1
  | Dix,_ -> -1
  | _,Dix -> 1
  | As,_ -> -1
  | _,As -> 1
  | Neuf,_ -> -1
  | _,Neuf -> 1
  | _ -> assert false

(* compare deux cartes, selon l'atout et la couleur demande *)
let compare atout demande (v1,c1) (v2,c2) =
  match c1,c2 with
  | a,b when a = b && a = atout -> compare_atout v1 v2
  | a,b when a = atout -> 1
  | a,b when b = atout -> -1
  | a,b when a = b && a = demande -> compare_meme v1 v2
  | a,b when a = demande -> 1
  | a,b when b = demande -> -1
  | a,b -> (* pas besoin de comparer deux cartes pissées.*) assert false



(* ***

type partie = {e1:equipe; e2:equipe; manches: manche list; paquet: carte list}

and manche = {atout:couleur; a:joueur; b:joueur; c:joueur; d:joueur}

and equipe = {j1:joueur; j2:joueur; score:int}

and joueur = string

let init = {
  e1 = {j1="bli"; j2="bla"; score = 0};
  e2 = {j1="blou"; j2="blbl"; score = 0};
  manches = [];
  paquet = Utils.shuffle paquet
}
***)
