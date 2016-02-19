open Config 

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
    | x -> List.rev res,x  
  in
  aux [] l 0

let iota n = 
  let rec loop acc = function
    | 0 -> acc
    | n -> loop (n::acc) (n-1)
  in loop [] n 

let is_rect_intersect (a,b) ((x,y),(w,h)) =
  a > x && b > y && a < x + w && b < y + h
