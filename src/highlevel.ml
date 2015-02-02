open Sdl2;;
open Core.Std;;
  
let ordie a = match a with 
  |Ok a -> a
  |Error e -> failwith e



module Position : sig
  type t = {x: int; y:int}
  val of_rect : Rect.rect -> t
  val to_rect : t -> Texture.t -> Rect.rect
end = struct
  type t = {x:int; y:int}
  let of_rect rect = {x=rect.Rect.x;y=rect.Rect.y}
  let to_rect self reference = let (w,h) = Texture.query reference in
    {Rect.x=self.x;Rect.y=self.y;Rect.w=w;Rect.h=h}
end

module Draw : sig
  type position =
    |Rect of Rect.rect
    |Position of Position.t
  val draw : Render.t -> Texture.t -> ?pos:position -> unit -> (unit, string) Core.Result.t
end = struct
    type position =
    |Rect of Rect.rect
    |Position of Position.t
  let draw renderer texture ?(pos=Rect {Rect.x=0;Rect.y=0;Rect.w=0;Rect.h=0}) () =
    let srcrect = {Rect.x=0;Rect.y=0;Rect.w=0;Rect.h=0} in
    match pos with
    |Rect r -> Render.copy renderer texture srcrect r
    |Position p -> Render.copy renderer texture srcrect (Position.to_rect p texture)
end

(* module type Entity = sig *)
(*     type t *)
(*     val position : t -> Position.t *)
(*     val update : t -> t *)
(*     val texture : t -> Texture.t *)
(*     val set_speed : t -> (int * int) -> t *)
(*     val make : Render.t -> t *)
(*   end *)

let steptowards num target step_size =
  if num < target
  then begin
      if (num +. step_size) > target then target else num +. step_size
    end
  else if num > target
  then begin
      if (num -. step_size) < target then target else num -. step_size
    end
  else
    target
      
module Player = struct
  type t = {pos: Position.t; tex: Texture.t; x_speed: float; y_speed: float}
  let texture self = self.tex;;
  let position self = self.pos;;
  let update self = {pos= {x=self.pos.x + (Float.to_int self.x_speed);y= self.pos.y + (Float.to_int self.y_speed)}; tex= self.tex; x_speed = steptowards self.x_speed 0. 5.; y_speed = steptowards self.y_speed 0. 5.}
  let set_speed self target =
    let (xs, ys) = target in
    {pos=self.pos;tex=self.tex;x_speed=xs;y_speed=ys}
  let get_speed self = (self.x_speed, self.y_speed)
  let react self event =
    let module Ke = Event.KeyboardEvent in
    let rec print_keys l =
      match l with
      |[] -> Printf.printf"\n\n\n"
      | e :: l -> Printf.printf "%d " (Unsigned.UInt8.to_int e); flush stdout; print_keys l in
    let keys = Ke.state () in
    print_keys keys;
    match event with
    |Event.Key ke -> set_speed self (match ke.Ke.keysym.scancode with
				     |Ke.ScancodeUp -> (0., -5.)
				     |Ke.ScancodeDown -> (0., 5.)
				     |Ke.ScancodeLeft -> (-5., 0.)
				     |Ke.ScancodeRight -> (5., 0.)
				     |_ -> get_speed self)
    |_ -> self

  let make renderer = {pos={Position.x=0;Position.y=0};tex= ordie (Image.load renderer "avatar.png");x_speed=0.0;y_speed=3.0}
end

module Coin = struct
  type t = {pos: Rect.t; tex: Texture.t;}
end
type entity = |Player of Player.t
type entities = (string * entity) list

		
module Input : sig
  val register_key : Event.KeyboardEvent.keysym -> (Event.KeyboardEvent.t -> entities ->entities) -> unit
  val handle_key : Event.KeyboardEvent.t -> entities -> entities
  (* val wants_key : Event.KeyboardEvent.t -> entities -> (string * Event.t) list *)
end = struct
  let keybinds = Hashtbl.Poly.create ()
  
  let register_key : Event.KeyboardEvent.keysym -> (Event.KeyboardEvent.t -> entities -> entities) -> unit = fun key callback -> ignore (Hashtbl.add keybinds ~key ~data:callback)
											      
  let handle_key key entities = match (Hashtbl.find keybinds key.Event.KeyboardEvent.keysym) with
    |Some f -> f key entities
    |None -> entities

  (* let wants_key key entities  = *)
  (*   let module Ke = Event.KeyboardEvent in *)
  (*   Hashtbl.find_all key entities *)
end

   
