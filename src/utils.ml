let shuffle_array a =
  Array.sort (fun _ _ -> (Random.int 3) - 1) a

let shuffle l =
  let a = Array.of_list l in
  shuffle_array a;
  Array.to_list a

let split_n_prems l n = 
  let rec aux res l i =
    match l with
    | h::tl when i < n -> aux (h::res) tl (i+1) 
    | x -> res,x  
  in
  aux [] l 0
