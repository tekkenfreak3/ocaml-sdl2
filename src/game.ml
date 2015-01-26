(* Copyright (C) 2014 Daniel Wilkins  *)
open Sdl2;;
open Highlevel;;

let ordie a = match a with 
  |Core.Result.Ok a -> a
  |Core.Result.Error e -> failwith e

let () =
      
  ordie (Init.init Init.init_everything);
  let window = ordie(Window.make "A test game." Window.pos_undefined Window.pos_undefined 800 600 0) in
  let renderer = ordie (Render.make window 2) in
  let background = ordie (Image.load renderer "bg.png") in
  let coin = ordie (Image.load renderer "coin.png") in
  let player = ordie (Image.load renderer "avatar.png") in
  let font = ordie (Ttf.load "monofur.ttf" 20) in
  
  let module Ke = Event.KeyboardEvent in
  Input.register_key {Ke.scancode=Ke.ScancodeZ;Ke.modkey=Ke.ModNone}
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
    ignore (match (Event.poll_event ()) with
    |Quit -> Etc.quit ()
    |Key k -> (Input.handle_key k)
    |_ -> ());
    let textnum = Random.int (Array.length texts) in (* This works because it's an exclusive parameter *)
    ordie (Draw.draw renderer background ());
    let text = Ttf.render renderer font texts.(textnum) (random_color ()) in
    ordie (Draw.draw renderer text ~pos:(Position {Position.x = 10; Position.y = 32}) ());

    Render.present renderer;
    Etc.delay 100;

    Texture.free text;
    loop () in
  loop ()
