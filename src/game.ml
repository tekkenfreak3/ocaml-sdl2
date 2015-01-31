(* Copyright (C) 2014 Daniel Wilkins  *)
open Sdl2;;
open Highlevel;;
open Core.Std;;
  
let () =
      
  Highlevel.ordie (Init.init Init.init_everything);
  let window = Highlevel.ordie(Window.make "A test game." Window.pos_undefined Window.pos_undefined 800 600 0) in
  let renderer = Highlevel.ordie (Render.make window 2) in
  let background = Highlevel.ordie (Image.load renderer "bg.png") in
  let coin = Highlevel.ordie (Image.load renderer "coin.png") in
  let font = Highlevel.ordie (Ttf.load "monofur.ttf" 20) in
  
  let module Ke = Event.KeyboardEvent in

  let random_color () =
    {Color.r=(Random.int 256);Color.g=(Random.int 256);Color.b=(Random.int 256);Color.a=(Random.int 256)} in
  let texts = Array.of_list ["This is some text I think"; "yeah"; "There was once a bug which looked amazing; luckily I recreated it"; "this all really doesn't matter but it's nice to test text rendering with a bunch of different sizes. Sadly it doesn't wrap"] in

  (* Input.register_key {Ke.scancode=Ke.ScancodeUp;Ke.modkey=Ke.ModNone} *)
  (* 		     (fun k es -> *)
  (* 		      let p = match (List.Assoc.find es "player") with *)
  (* 			|Some pl -> match pl  with *)
  (* 				    |Player p -> p in *)
  (* 		      let (xs, _) = Player.get_speed p in *)
  (* 		      ("player", Player (Player.set_speed p (0.0, -5.0))) ::(List.Assoc.remove es "player")); *)

  (* Input.register_key {Ke.scancode=Ke.ScancodeDown;Ke.modkey=Ke.ModNone} *)
  (* 		     (fun k es -> *)
  (* 		      let p = matc h(List.Assoc.find es "player") with *)
  (* 			|Some pl -> match pl  with *)
  (* 				    |Player p -> p in		       *)
  (* 		      let (xs, _) = Player.get_speed p in *)
  (* 		      ("player", Player (Player.set_speed p (xs, 5.0))) ::(List.Assoc.remove es "player")); *)

  (* Input.register_key {Ke.scancode=Ke.ScancodeLeft;Ke.modkey=Ke.ModNone} *)
  (* 		     (fun k es -> *)
  (* 		      let p = match (List.Assoc.find es "player") with *)
  (* 			|Some pl -> match pl  with *)
  (* 				    |Player p -> p in *)
  (* 		      let (_, ys) = Player.get_speed p in *)
  (* 		      ("player", Player (Player.set_speed p (-5.0,ys))) ::(List.Assoc.remove es "player")); *)
  (* Input.register_key {Ke.scancode=Ke.ScancodeRight;Ke.modkey=Ke.ModNone} *)
  (* 		     (fun k es -> *)
  (* 		      let p = match (List.Assoc.find es "player") with *)
  (* 			|Some pl -> match pl  with *)
  (* 				    |Player p -> p in *)
  (* 		      let (_, ys) = Player.get_speed p in *)
  (* 		      ("player", Player (Player.set_speed p (5.0,ys))) ::(List.Assoc.remove es "player")); *)

  let rec loop entities =
    let player = match (List.Assoc.find entities "player") with
      |Some pl -> match pl with
		  |Player p -> p in
    ignore (match (Event.poll_event ()) with
    |Quit -> Etc.quit ()
    |Key k -> loop ["player", (Player (Player.update (Player.react player (Key k))))]
    |None -> begin (*Check if we have a pressed key *)
	     end
    |_ -> ()
	   );
    let textnum = Random.int (Array.length texts) in (* This works because it's an exclusive parameter *)
    Highlevel.ordie (Draw.draw renderer background ());
    let text = Ttf.render renderer font texts.(textnum) (random_color ()) in
    Highlevel.ordie (Draw.draw renderer text ~pos:(Position {Position.x = 10; Position.y = 32}) ());

    Highlevel.ordie (Draw.draw renderer (Player.texture player) ~pos:(Position (Player.position player)) ());
    Render.present renderer;
    Etc.delay (1000 / 30);

    Texture.free text;
    loop ["player", Player (Player.update player)] in
  loop ["player", Player (Player.make renderer)]
