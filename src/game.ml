(* Copyright (C) 2014 Daniel Wilkins  *)
open Sdl2;;

let () =
  let window = match (Window.make "A test game." Window.pos_undefined Window.pos_undefined 800 600 0) with
    |Ok w -> w in
  let renderer = match (Render.make window 2) with
    |Ok r-> r in
  let pixmap = match Image.load renderer "avatar.png" with
    |Ok image -> image in
  let module Ke = Event.KeyboardEvent in
  Highlevel.Input.register_key {Ke.scancode=Ke.ScancodeZ;Ke.modkey=Ke.ModNone}
			       (fun k ->
				begin
				  Printf.printf "Z ";
				  ignore (match k.Ke.state with
				  |Released -> Printf.printf "Released!\n"
				  |Pressed -> Printf.printf "Pressed!\n");
				    flush stdout; end);

  let rec loop () =
    let nothing = match (Event.poll_event ()) with
    |Quit -> Etc.quit ()
    |Key k -> (Highlevel.Input.handle_key k)
    |_ -> () in
    ignore (Render.copy renderer pixmap ());
    Render.present renderer;
    loop () in
  loop ()


