open Sdl2;;


module Position : sig
  type t = {x: int; y:int};;
end = struct
  type t = {x:int; y:int};;
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

type entities = (string * Position.t) list
				      
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

