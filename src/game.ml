open Carte 
open Utils

(* [1..4] *)
type id = int

type annonce = | Capot of couleur
	       | Normal of int * couleur
	       | Rien

type state = Annonce | Joue | Fin

type env = 
  { state : state
  ; mains : carte list array
  ; plis : (id * (carte  array)) list
  ; contrat : annonce
  ; est_coinche : bool * bool (* coinche / surcoinche *)
  ; qui_part : id
  ; scores : int * int (* nord/sud X ouest/est *)
  ;
  }

(* Joueurs *)

(* 0 => sud, 1 => est, 2 => nord, 3 => ouest *)
let joueurs = iota 4

let id_sud = 1
let id_est = 2
let id_nord = 3
let id_ouest = 4
let next_player id = id mod 4 + 1

let init_env () =
  let state = Annonce in
  let paquet = paquet |> melange in
  let mains = distribue paquet |> Array.map trie_main in
  let contrat = Rien in
  let plis = [] in
  let est_coinche = false, false in
  let qui_part = Random.int 4 + 1 in
  let scores = 0,0 in
  { state
  ; mains
  ; plis
  ; contrat
  ; est_coinche
  ; qui_part
  ; scores
  }
