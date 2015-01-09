(* Copyright (C) 2014 Daniel Wilkins  *)
open Ctypes;;
open Foreign;;
open Unsigned;;
open Core;;
module Rect : sig
  type rect = {x: int; y: int; w: int; h: int};;
  type t;;
  val t : rect typ;;
end = struct
  type rect = {x: int; y: int; w: int; h: int}

  type sdl_rect;;
  let sdl_rect : sdl_rect structure typ = structure "SDL_Rect";;
  let x = field sdl_rect "x" int;;
  let y = field sdl_rect "y" int;;
  let w = field sdl_rect "w" int;;
  let h = field sdl_rect "h" int;;
  seal sdl_rect;;

  let sdl_rect_of_rect rect =
    if rect.w <> 0
    then
      begin
	let ret = make sdl_rect in
	setf ret x rect.x;
	setf ret y rect.y;
	setf ret w rect.w;
	setf ret h rect.h;
	(addr ret)
      end	  
    else
      (from_voidp sdl_rect null);;
	  
  let rect_of_sdl_rect r =
    let rect = !@ r in
    {x = (getf rect x);y = (getf rect y);w = (getf rect w);h = (getf rect h)};;
  type t;;
  let t = view ~read:rect_of_sdl_rect ~write:sdl_rect_of_rect (ptr sdl_rect);;
end

module Error = struct
  let get_error = foreign "SDL_GetError" (void @-> returning string);;
end

module Init : sig
  val init_timer : int;;
  val init_audio : int;;
  val init_video : int;;
  val init_joystick : int;;
  val init_events : int;;
  val init_everything : int;;

  val init : int -> (unit, string) Result.t;;
end = struct
  let init_timer = 1;;
  let init_audio = 16;;
  let init_video = 32;;
  let init_joystick = 512;;
  let init_events = 0x00004000;;
  let init_everything = init_timer lor init_audio lor init_video lor init_joystick lor init_events;;

  let init_f = foreign "SDL_Init" (int @-> returning int);;


  let init flags =
    if init_f flags = 1
    then
      Result.Error (Error.get_error ())
    else
      Result.Ok ()
end
		
module Window : sig
  type t
  val t : t typ;;
  val make : string -> int -> int -> int -> int -> int -> (t, string) Result.t
end = struct
  type t = unit ptr;;
  let t : t typ = ptr void;;
  let create_window_f = foreign "SDL_CreateWindow" (string @-> int @-> int @-> int @-> int @-> int @-> returning t);;
  let make title x y w h flags =
    let win = create_window_f title x y w h flags in
    if  win <> null
    then
      Result.Ok win
    else
      Result.Error (Error.get_error ());;
end
	



module Render : sig
  type t
  val t : t typ
  type texture
  val texture : texture typ
  val texture_exists : texture -> bool
  val make : Window.t -> int -> (t, string) Result.t
  val copy : Rect.rect -> Rect.rect -> t -> texture -> (unit, string) Result.t
  val present : t -> unit
end = struct
  type t = unit ptr;;
  let t : t typ = ptr void;;
  type texture = unit ptr;;
  let texture : texture typ = ptr void;;

  let texture_exists tex =
    tex <> null;;
  let create_renderer_f = foreign "SDL_CreateRenderer" (Window.t @-> int @-> int @-> returning t);;
  
  let make window flags =
    let rend = create_renderer_f window ~-1 flags in (* ~-1 is to avoid confusing the parser, it just means -1*)
    if rend <> null
    then
      Result.Ok rend
    else
      Result.Error (Error.get_error ());;

  let copy_f = foreign "SDL_RenderCopy" (t @-> texture @-> Rect.t @-> Rect.t @-> returning int);;
  let copy src dest renderer texture =
    let ret = copy_f renderer texture src dest in
    if ret = 0 then
      Result.Ok ()
    else
      Result.Error (Error.get_error ());;
    
  let present = foreign "SDL_RenderPresent" (t @-> returning void);;
end



module Event = struct

  type window_event_f;;
  let window_event_f : window_event_f structure typ = structure "SDL_WindowEvent";;
    let wtype = field window_event_f "type" int;;
    let timestamp = field window_event_f "timestamp" int;;
    let windowID = field window_event_f "type" int;;
    let win = field window_event_f "type" int;;
    let data1 = field window_event_f "type" int;;
    let data2 = field window_event_f "type" int;;
    let window_data3 = field window_event_f "type" int;;
    let window_data4 = field window_event_f "type" int;;
    let window_data5 = field window_event_f "type" int;;
    let window_data6 = field window_event_f "type" int;;
    let window_data7 = field window_event_f "type" int;;
    let window_data8 = field window_event_f "type" int;;
    let window_data9 = field window_event_f "type" int;;
    let window_data10 = field window_event_f "type" int;;
    let window_data11 = field window_event_f "type" int;;
    let window_data12 = field window_event_f "type" int;;
    let window_data13 = field window_event_f "type" int;;
    let window_data14 = field window_event_f "type" int;;
  seal window_event_f;;

  type sdl_event;;
  let sdl_event: sdl_event union typ = union "SDL_Event";;
  let etype = field sdl_event "type" int;;
  let window = field sdl_event "window" window_event_f;;
    seal sdl_event;;

  type window_event = {timestamp: int; window_id: int; data1: int; data2: int};;

      
  type t =
    | Quit
    | Window of window_event
    | None;;

    (* | KeyDown of keydown_event *)
  (* | KeyUp of keyup_event;; *)
  let event_of_sdl_event sevent =
    let ty = getf sevent etype in
    match ty with
    |0x100 -> Quit
    |0x200 -> begin
			    let wevent = (getf sevent window) in
			    Window {timestamp= (getf wevent timestamp); window_id= (getf wevent windowID);data1= (getf wevent data1);data2= (getf wevent data2)} end
    |_ -> None;;
  let poll_event_f = foreign "SDL_PollEvent" (ptr sdl_event @-> returning int);;
  let poll_event () =
    let e = make sdl_event in
    ignore (poll_event_f (addr e));
    event_of_sdl_event e;;
end
		 
module Image : sig
  val quit : unit -> unit -> unit;;
  val load_f : Render.t -> string -> Render.texture;;
  val load : Render.t -> string -> (Render.texture, string) Result.t;;
end = struct
  let quit () = foreign "IMG_Quit" (void @-> returning void);;
  let load_f = foreign "IMG_LoadTexture" (Render.t @-> string @-> returning Render.texture);;
  let load renderer texname =
    let tex = load_f renderer texname in
    if Render.texture_exists tex
    then
      Result.Ok tex
    else
      Result.Error (Error.get_error ());;
end
  
module Etc : sig
  val delay : int -> unit
  val quit : unit -> unit
end = struct
  let delay = foreign "SDL_Delay" (int @-> returning void);;
  let quit = foreign "SDL_Quit" (void @-> returning void);;
end
