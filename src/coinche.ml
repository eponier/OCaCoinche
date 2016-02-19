open Carte 
open Config 
open Sdl
open Sdlevent
open Game
open View

let init_sdl () =
  Sdl.init [`VIDEO];
  Sdlttf.init ();
  at_exit Sdl.quit;  
  at_exit Sdlttf.quit

let rec start_event_loop screen env view_env =
  wait_event () |> function 
  | QUIT ->
      (* exit *)
    print_endline "On remballe !"
      
  | event ->
    let env,view_env =
      begin
	match env.state with
	(* | Annonce -> env, view_env *)
	(* | Joue -> begin *)
	| _ -> begin
	  match event with 
	  | MOUSEMOTION mousemotion_event -> 
	      (* Hover on a card should raise it *)
	    begin
	      let view_env' = 
		View.raise_p1_card 
		  env view_env 
		  (mousemotion_event.mme_x, mousemotion_event.mme_y) 
	      in 
	      env, view_env'
	    end
	  | _ -> env,view_env
	end	    
      end
    in
    if view_env.should_redraw then
      View.draw screen env view_env;
    start_event_loop screen env {view_env with should_redraw = false}

let main () =
  Random.self_init ();
  init_sdl ();
  let screen = Sdlvideo.set_video_mode window_w window_h [`DOUBLEBUF] in

  enable_events all_events_mask;

  let env = Game.init_env () in
  let view_env = View.init_view_env env in
  View.draw screen env view_env;
  start_event_loop screen env view_env

let () = main ()
