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
  let img = match Image.load renderer "bg.png" with
    | Ok i -> i
    |Error e -> die e
  in

  let (imgw, imgh) = Image.query_texture img in
  let render = Render.copy renderer in
  let rec iloop rect = begin
    match (render img  ~dest:rect ()) with
    |Error e -> die e
    |Ok () -> ();
    Render.present renderer;
    match (Event.poll_event ()) with
    |Quit -> ()
    |Key k -> Printf.printf "key event: %d %B %B\n" k.timestamp (k.state = Event.KeyboardEvent.Pressed) k.repeat; flush stdout; iloop rect
    |_ -> iloop rect
    end
  in
  iloop {Rect.x=50;Rect.y=0;Rect.w=640;Rect.h=480;};
  Etc.quit ();; 

let () =
  main ();;
