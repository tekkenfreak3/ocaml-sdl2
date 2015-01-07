(* Copyright (C) 2014 Daniel Wilkins  *)
open Ctypes;;

let rec main () = 
  ignore (Sdl2.Init.init Sdl2.Init.init_everything);
  let win = match (Sdl2.Window.make "foobar?" 640 480 640 480 0) with
    |Some w -> w in
  let renderer = match (Sdl2.Render.make win 2) with
    | Some r -> r in
  let img = match Sdl2.Image.load renderer "resources/images/bg.png" with
    | Some i -> i
  in

  let rec iloop () =
    let res = Sdl2.Render.copy {x=0;y=0;w=3000;h=3000} {x=0;y=0;w=640;h=480} renderer img in
    Sdl2.Render.present renderer;
    let ev = Ctypes.make Sdl2.Event.t in
    ignore (Sdl2.Event.poll_event (Ctypes.addr ev));

    if (getf ev Sdl2.Event.etype) <> Sdl2.Event.quit_event
    then
      iloop () in
  iloop ();
  Sdl2.quit ();;

let () =
  main ();;
