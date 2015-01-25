open Sdl2;;
module Input : sig
  val register_key : Event.KeyboardEvent.keysym -> (Event.KeyboardEvent.t -> unit) -> unit
  val handle_key : Event.KeyboardEvent.t -> unit
end = struct
  let keybinds = Hashtbl.create 128
	     
  let register_key : Event.KeyboardEvent.keysym -> (Event.KeyboardEvent.t -> unit) -> unit = fun key callback -> Hashtbl.add keybinds key callback

  let handle_key key =
    let module Ke = Event.KeyboardEvent in

    let k = key.Ke.keysym in
    if Hashtbl.mem keybinds k then
      (Hashtbl.find keybinds k) key
    
end
