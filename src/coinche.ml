open GMain

let share_path = "share/"

let create_cards_pixbuf_array share_path =
  Utils.iota 33
  |> List.map (
    fun i ->
      let str = 
	if i = 33 then 
	  "cover"
	else
	  let v = match i mod 8 with
	    | 0 -> "as"
	    | 1 -> "7"
	    | 2 -> "8"
	    | 3 -> "9"
	    | 4 -> "10"
	    | 5 -> "v"
	    | 6 -> "d" 
	    | 7 -> "r"
	    | _ -> assert false
	  in 
	  let col =
	    if i < 8 then "tr" 
	    else if i < 16 then "ca"
	    else if i < 24 then "co"
	    else "pi"
	  in
	  Printf.sprintf "%s_%s" v col
      in

      GdkPixbuf.from_file (share_path ^ str ^ ".png")
  )
  |> Array.of_list

let arr = create_cards_pixbuf_array share_path

(* Main *)
let () =
  let w = GWindow.window () in
  w#connect#destroy ~callback:Main.quit |> ignore;
  let b = GPack.fixed 
    ~has_window:true
    ~border_width:2
    ~width:800
    ~height:800
    ~show:true
    ~packing:w#add () 
  in
  let event_box = GBin.event_box ~packing:b#add () in
  let img = GMisc.image ~pixbuf:arr.(0) ~packing:event_box#add () in
  
  w#show ();
  Main.main ()
