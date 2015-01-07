(* Copyright (C) 2014 Daniel Wilkins  *)
open Ctypes;;
  
let rec main () = 
  ignore (Sdl2.init Sdl2.init_everything);
  let win = match (Sdl2.Window.make "foobar?" 640 480 640 480 0) with
    |Some w -> w in
  let renderer = match (Sdl2.create_renderer win 2) with
    | Some r -> r in
  Printf.eprintf "%B\n%!" (renderer = Ctypes.null);
  let img = match Sdl2.Image.load renderer "resources/images/bg.png" with
    | Some i -> i
  in

  let rec iloop () =
    let res = Sdl2.render_copy_f renderer img {x=0;y=0;w=3000;h=3000} {x=0;y=0;w=640;h=480}  in
    Sdl2.render_present renderer;
    let ev = Ctypes.make Sdl2.event in
    ignore (Sdl2.poll_event (Ctypes.addr ev));

    if (getf ev Sdl2.event_type) <> Sdl2.quit_event
    then
      iloop () in
  iloop ();
  Sdl2.quit ();;

let () =
  main ();;
