(* Copyright (C) 2014 Daniel Wilkins  *)
open Sdl2;;

let die str =
  Printf.eprintf "FATAL: %s\n" str;
  raise Exit;;

let main () = 
  ignore (Init.init Init.init_everything);
  let win = match (Window.make "foobar?" 640 480 640 480 0) with
    |Ok w -> w
    |Error e -> die e in
  let renderer = match (Render.make win 2) with
    | Ok r -> r
    |Error e -> die e in
  let img = match Image.load renderer "resources/images/bg.png" with
    | Ok i -> i
    |Error e -> die e
  in

  let rec iloop () =
    ignore (Render.copy {x=0;y=0;w=3000;h=3000} {x=0;y=0;w=640;h=480} renderer img);
    Render.present renderer;
    match (Event.poll_event ()) with
    |Quit -> ()
    |Key k -> Printf.printf "key event: %d %B %B\n" k.timestamp (k.state = Event.KeyboardEvent.Pressed) k.repeat; flush stdout; iloop ()
    |_ -> iloop ()
  in
  iloop ();
  Etc.quit ();; 

let () =
  main ();;
