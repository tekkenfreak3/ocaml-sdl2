(* Copyright (C) 2014 Daniel Wilkins  *)
open Sdl2;;

let () =
  match Init.init Init.init_everything with
  | Ok () -> () in
  let window = match (Window.make "A test game." Window.pos_undefined Window.pos_undefined 800 600 0) with
    |Ok w -> w in
  let renderer = match (Render.make window 2) with
    |Ok r-> r in
  let background = match Image.load renderer "bg.png" with
    |Ok image -> image in
  let coin = match Image.load renderer "coin.png" with
    |Ok image -> image in
  let player = match Image.load renderer "avatar.png" with
    |Ok image -> image in
  let font = match Ttf.load "monofur.ttf" 20 with
    |Ok font -> font
    |Error e -> failwith e in
  
  
  let module Ke = Event.KeyboardEvent in
  Highlevel.Input.register_key {Ke.scancode=Ke.ScancodeZ;Ke.modkey=Ke.ModNone}
			       (fun k ->
				begin
				  Printf.printf "Z ";
				  ignore (match k.Ke.state with
				  |Released -> Printf.printf "Released!\n"
				  |Pressed -> Printf.printf "Pressed!\n");
				    flush stdout; end);

  let random_color () =
    {Color.r=(Random.int 256);Color.g=(Random.int 256);Color.b=(Random.int 256);Color.a=(Random.int 256)} in
  let texts = Array.of_list ["This is some text I think"; "yeah"; "There was once a bug which looked amazing; luckily I recreated it"; "this all really doesn't matter but it's nice to test text rendering with a bunch of different sizes. Sadly it doesn't wrap"] in
  let rec loop () =
    let nothing = match (Event.poll_event ()) with
    |Quit -> Etc.quit ()
    |Key k -> (Highlevel.Input.handle_key k)
    |_ -> () in
    let textnum = Random.int (Array.length texts) in (* This works because it's an exclusive parameter *)
    ignore (Render.copy renderer background ());
    let text = Ttf.render renderer font texts.(textnum) (random_color ()) in
    ignore (Render.copy renderer text ~dest:{Rect.x=16;Rect.y=16;Rect.w=0;Rect.h=0} ());
    Etc.delay 100;

    Render.present renderer;
    loop () in
  loop ()


