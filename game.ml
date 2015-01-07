(* Copyright (C) 2014 Daniel Wilkins  *)
open Sdl2;;

let die str =
  Printf.eprintf "FATAL: %s\n" str;
  raise Exit;;

let rec main () = 
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
    let ev = Ctypes.make Event.t in
    ignore (Event.poll_event (Ctypes.addr ev));

    if (Event.etype ev) <> Event.quit_event
    then
      iloop ()
  in
  iloop ();
  Etc.quit ();; 

let () =
  main ();;
