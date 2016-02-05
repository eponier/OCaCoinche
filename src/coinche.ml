open GMain


(* Main *)
let () =
  let w = GWindow.window () in
  w#connect#destroy ~callback:Main.quit |> ignore;
  let im = GdkPixbuf.from_file "/home/vincent/coinche/share/cards.png" in
  let b = GPack.box `VERTICAL ~width:800 ~height:800 ~packing:w#add () in

  
  GMisc.image ~pixbuf:im ~pixel_size:10 ~xpad:79 ~ypad:123 ~width:79 ~height:123 ~packing:b#add () |> ignore;
  (* y:123 * x:79 *)
  
  w#show ();
  Main.main ()
