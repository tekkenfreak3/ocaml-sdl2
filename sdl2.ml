(* Copyright (C) 2014 Daniel Wilkins  *)
open Ctypes;;
open Foreign;;
open Unsigned;;
  

type texture = unit ptr;;
let texture : texture typ = ptr void;;

type renderer = unit ptr;;
let renderer : renderer typ = ptr void;;

module Rect : sig
  type ocaml_rect = {x: int; y: int; w: int; h: int};;
  type t;;
  val t : ocaml_rect typ;;
end = struct
  type ocaml_rect = {x: int; y: int; w: int; h: int}

  type sdl_rect;;
  let sdl_rect : sdl_rect structure typ = structure "SDL_Rect";;
  let x = field sdl_rect "x" int;;
  let y = field sdl_rect "y" int;;
  let w = field sdl_rect "w" int;;
  let h = field sdl_rect "h" int;;
  seal sdl_rect;;

  let sdl_rect_of_ocaml_rect rect =
    let ret = make sdl_rect in
    setf ret x rect.x;
    setf ret y rect.y;
    setf ret w rect.w;
    setf ret h rect.h;
    (addr ret);;
  let ocaml_rect_of_sdl_rect r =
    let rect = !@ r in
    {x = (getf rect x);y = (getf rect y);w = (getf rect w);h = (getf rect h)};;
  type t;;
  let t = view ~read:ocaml_rect_of_sdl_rect ~write:sdl_rect_of_ocaml_rect (ptr sdl_rect);;
end

let init_timer = 1;;
let init_audio = 16;;
let init_video = 32;;
let init_joystick = 512;;
let init_events = 0x00004000;;
let init_everything = init_timer lor init_audio lor init_video lor init_joystick lor init_events;;

let init_f = foreign "SDL_Init" (int @-> returning int);;
let get_error = foreign "SDL_GetError" (void @-> returning string);;

let init flags =
  if init_f flags = 1
  then begin
      Printf.eprintf "SDL failed to initialize, this may help: %s\n" (get_error ());
      raise Exit;
    end;;

module Window : sig
  type t
  val t : t typ;;
  val make : string -> int -> int -> int -> int -> int -> t option
end = struct
  type t = unit ptr;;
  let t : t typ = ptr void;;
  let create_window_f = foreign "SDL_CreateWindow" (string @-> int @-> int @-> int @-> int @-> int @-> returning t);;
  let make title x y w h flags =
    let win = create_window_f title x y w h flags in
    if  win <> null
    then
      Some win
    else
      None;;
end
	
let delay = foreign "SDL_Delay" (int @-> returning void);;
  
let create_renderer_f = foreign "SDL_CreateRenderer" (Window.t @-> int @-> int @-> returning renderer);;

  
let create_renderer window flags =
  let rend = create_renderer_f window ~-1 flags in (* ~-1 is to avoid confusing the parser, it just means -1*)
  if rend <> null
  then
    Some rend
  else
    None;;

let render_copy_f = foreign "SDL_RenderCopy" (renderer @-> texture @-> Rect.t @-> Rect.t @-> returning int);;
let render_present = foreign "SDL_RenderPresent" (renderer @-> returning void);;
let quit = foreign "SDL_Quit" (void @-> returning void);;

let quit_event = 0x100;;
let keydown_event = 0x300;;
let keyup_event = 0x301;;
type window_event;;
let window_event : window_event structure typ = structure "SDL_WindowEvent";;
let window_event_type = field window_event "type" int;;
let window_event_timestamp = field window_event "timestamp" int;;
let window_event_windowID = field window_event "type" int;;
let window_event_event = field window_event "type" int;;
let window_event_pad1 = field window_event "type" uint8_t;;
let window_event_pad2 = field window_event "type" uint8_t;;
let window_event_pad3 = field window_event "type" uint8_t;;
let window_event_data1 = field window_event "type" int;;
let window_event_data2 = field window_event "type" int;;
let window_event_data3 = field window_event "type" int;;
let window_event_data4 = field window_event "type" int;;
let window_event_data5 = field window_event "type" int;;
let window_event_data6 = field window_event "type" int;;
let window_event_data7 = field window_event "type" int;;
let window_event_data8 = field window_event "type" int;;
let window_event_data9 = field window_event "type" int;;
let window_event_data10 = field window_event "type" int;;
let window_event_data11 = field window_event "type" int;;
let window_event_data12 = field window_event "type" int;;
let window_event_data13 = field window_event "type" int;;
let window_event_data14 = field window_event "type" int;;
  
  seal window_event;;
    
type event;;
let event : event union typ = union "SDL_Event";;
let event_type = field event "type" int;;
let event_1 = field event "window" window_event;;
seal event;;

let poll_event = foreign "SDL_PollEvent" (ptr event @-> returning int);;
module Image : sig
  val quit : unit -> unit -> unit;;
  val load_f : renderer -> string -> texture;;
  val load : renderer -> string -> texture option;;
end = struct
  let quit () = foreign "IMG_Quit" (void @-> returning void);;
  let load_f = foreign "IMG_LoadTexture" (renderer @-> string @-> returning texture);;
  let load renderer texname =
    let tex = load_f renderer texname in
    if tex <> null
    then
      Some tex
    else
      None;;
end
  
