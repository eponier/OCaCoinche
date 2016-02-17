(****** Cartes *********)
type couleur = Trefle | Coeur | Pique | Carreau

type valeur = Sept | Huit | Neuf | Valet | Dame | Roi | Dix | As

type carte = valeur * couleur

(******** operation sur un paquet de carte *******)
let paquet = 
  let vals = [Sept ; Huit ; Neuf ; Valet ; Dame ; Roi ; Dix ; As] in
  List.fold_left  
    (fun acc c -> List.map (fun v -> v,c) vals @ acc) []
    [Trefle ; Coeur; Pique; Carreau ]
    
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
  let coupe =
    ((Random.int (taille-2)) + (Random.int (taille-2))) / 2 + 1 in
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
let compare_carte atout demande (v1,c1) (v2,c2) =
  match c1,c2 with
  | a,b when a = b && a = atout -> compare_atout v1 v2
  | a,b when a = atout -> 1
  | a,b when b = atout -> -1
  | a,b when a = b && a = demande -> compare_meme v1 v2
  | a,b when a = demande -> 1
  | a,b when b = demande -> -1
  | a,b -> (* pas besoin de comparer deux cartes pissées.*) assert false

(* 
   retourne la liste des cartes jouables
   selon un atout, une couleur demandée, 
   et si le coéquipier est maitre
*)
let valides 
    (cartes:carte list) ((valeur,atout):carte) 
    (demande:couleur) (maitre:bool) =
  let atouts,normales = List.partition (fun (_,c) -> c=atout) cartes in
  let demandes,pisse = List.partition (fun (_,c) -> c=demande) normales in
  let monte,sous = 
    List.partition 
      (fun (v,_) -> compare_atout v valeur > 0)
      atouts
  in
  if atout = demande then
    if monte <> [] then monte 
    else if sous <> [] then sous
    else normales
    else if demandes <> [] then demandes
    else if maitre then cartes 
    else if atouts <> [] then atouts else pisse
	
let trie_main cartes = 
  let coeurs = List.filter (fun (_,c) -> c = Coeur) cartes in
  let trefles = List.filter (fun (_,c) -> c = Trefle) cartes in
  let carreaux = List.filter (fun (_,c) -> c = Carreau) cartes in
  let piques = List.filter (fun (_,c) -> c = Pique) cartes in
  let f (v,_) (v',_) = compare_meme v v' in
  let coeurs = List.sort f coeurs in
  let trefles = List.sort f trefles in
  let carreaux = List.sort f carreaux in
  let piques = List.sort f piques in
  coeurs @ trefles @ carreaux @ piques

let trie_main_atout cartes atout = 
  let coeurs = List.filter (fun (_,c) -> c = Coeur) cartes in
  let trefles = List.filter (fun (_,c) -> c = Trefle) cartes in
  let carreaux = List.filter (fun (_,c) -> c = Carreau) cartes in
  let piques = List.filter (fun (_,c) -> c = Pique) cartes in
  let f = compare_carte atout in
  let coeurs = List.sort (f Coeur) coeurs in
  let trefles = List.sort (f Trefle) trefles in
  let carreaux = List.sort (f Carreau) carreaux in
  let piques = List.sort (f Pique) piques in
  coeurs @ trefles @ carreaux @ piques
