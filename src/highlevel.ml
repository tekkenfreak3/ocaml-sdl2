open Sdl2;;
let ordie a = match a with 
  |Core.Result.Ok a -> a
  |Core.Result.Error e -> failwith e



module Position : sig
  type t = {x: int; y:int}
end = struct
   type t = {x:int; y:int}
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
    |Position p -> begin
		   let (w, h) = Texture.query texture in
		   Render.copy renderer texture srcrect {Rect.x=p.x;Rect.y=p.y;Rect.w=w;Rect.h=h}
		 end
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
  let update self = Printf.printf "%f %f\n" self.x_speed self.y_speed; flush stdout; {pos= {x=self.pos.x + (int_of_float self.x_speed);y= self.pos.y + (int_of_float self.y_speed)}; tex= self.tex; x_speed = steptowards self.x_speed 0.0 0.1; y_speed = steptowards self.y_speed 0.0 0.1}
  let set_speed self target =
    let (xs, ys) = target in
    {pos=self.pos;tex=self.tex;x_speed=xs;y_speed=ys}
  let get_speed self = (self.x_speed, self.y_speed)
  let make renderer = {pos={Position.x=0;Position.y=0};tex= ordie (Image.load renderer "avatar.png");x_speed=0.0;y_speed=3.0}
end

module Coin = struct
  type t = {pos: Rect.t; tex: Texture.t;}
end
type entity = |Player of Player.t
type entities = (string * entity) list

		
module Input : sig
  val register_key : Event.KeyboardEvent.keysym -> (Event.KeyboardEvent.t -> entities -> entities) -> unit
  val handle_key : Event.KeyboardEvent.t -> entities -> entities
end = struct
  let keybinds = Hashtbl.create 128
	     
  let register_key : Event.KeyboardEvent.keysym -> (Event.KeyboardEvent.t -> entities -> entities) -> unit = fun key callback -> Hashtbl.add keybinds key callback

  let handle_key key ents =
    let module Ke = Event.KeyboardEvent in

    let k = key.Ke.keysym in
    if Hashtbl.mem keybinds k then
      ((Hashtbl.find keybinds k) key ents)
    else
      ents
    
end
