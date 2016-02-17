open Carte 
open Config 
open Sdl
open Sdlevent

let init_sdl () =
  Sdl.init [`VIDEO];
  Sdlttf.init ();
  at_exit Sdl.quit;  
  at_exit Sdlttf.quit

let rec start_event_loop env =
    wait_event () |> function 
    | QUIT ->
      (* exit *)
      print_endline "On remballe !"

    | event ->
      start_event_loop env

let main () =
  Random.self_init ();
  init_sdl ();
  let screen = Sdlvideo.set_video_mode window_w window_h [`DOUBLEBUF] in

  enable_events all_events_mask;

  let env = Game.init_env () in
  View.draw screen env;
  start_event_loop env

let () = main ()
