(* Copyright (C) 2014 Daniel Wilkins  *)
(* This code contains many magic numbers. If you see a plain number then that's almost certainly why. *)

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
  let etype = field window_event_f "type" int;;
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


    

  type window_event = {timestamp: int; window_id: int; data1: int; data2: int};;

  let window_event_of_window_event_f wef = {timestamp= (getf wef timestamp); window_id= (getf wef windowID);data1= (getf wef data1);data2= (getf wef data2)};;

  module KeyboardEvent = struct
    let bool_of_int i = match i with
      |0 -> false
      |_ -> true;;

    type keysym_f;;
    let keysym_f : keysym_f structure typ =  structure "SDL_Keysym";;
    let scancode = field keysym_f "scancode" int;; (*not to be "fixed", these are defined as machine int types*)
    let sym = field keysym_f "sym" int;;
    let modkey = field keysym_f "mod" uint16_t;;
    let unused = field keysym_f "unused" uint32_t;;
    seal keysym_f;;
  

    type key_event_f;;
    let key_event_f : key_event_f structure typ = structure "SDL_KeyboardEvent";;
    let etype = field key_event_f "type" uint32_t;;
    let timestamp = field key_event_f "timestamp" uint32_t;;
    let windowID = field key_event_f "windowID" uint32_t;;
    let state = field key_event_f "state" uint8_t;;
    let repeat = field key_event_f "repeat" uint8_t;;
    let padding = field key_event_f "padding2" uint16_t;; (*WARNING this is 2 u8s in the original source. it shouldn't mak ea difference but if a bug happens...*)
    let keysym = field key_event_f "keysym" keysym_f;;
    seal key_event_f;;


    type scancode = ScancodeUnknown | ScancodeZ | ScancodeUp | ScancodeDown | ScancodeLeft | ScancodeRight | ScancodeEsc;;
    let scancode_of_sdl_scancode sk = match sk with
      |29 -> ScancodeZ
      |41 -> ScancodeEsc
      |79 -> ScancodeLeft
      |80 -> ScancodeRight
      |81 -> ScancodeDown
      |82 -> ScancodeUp
      |_ -> ScancodeUnknown;;


    (* sym isn't implemented for now. it's just a mapping of a key to its unicode representation*)
    type modkey = ModNone | ModLShift | ModRShift | ModLCtrl | ModRCtrl | ModLAlt | ModRAlt | ModLGui | ModRGui | ModNumLk | ModCaps | ModMode;;
    let modkey_of_sdl_modkey mk = match Unsigned.UInt16.to_int mk with
      |0x0 -> ModNone
      |0x1 -> ModLShift
      |0x2 -> ModRShift
      |0x40 -> ModLCtrl
      |0x80 -> ModRCtrl
      |0x100 -> ModLAlt
      |0x200 -> ModRAlt
      |0x400 -> ModLGui
      |0x800 -> ModRGui
      |0x1000 -> ModNumLk
      |0x2000 -> ModCaps
      |0x4000 -> ModMode;;
      
    type keysym = {scancode: scancode; modkey: modkey};;

    type key_state = Released | Pressed;;
    let key_state_of_sdl_keystate ks = if Unsigned.UInt8.to_int ks = 1 then Pressed else Released;;
    type t = {timestamp: int; window_id: int; state: key_state; repeat: bool; keysym: keysym};;

    let of_key_event_f ke = {timestamp= (Unsigned.UInt32.to_int (getf ke timestamp)); window_id= (Unsigned.UInt32.to_int (getf ke windowID)); state= (key_state_of_sdl_keystate (getf ke state)); repeat= (bool_of_int (Unsigned.UInt8.to_int (getf ke repeat)));keysym = {scancode= (scancode_of_sdl_scancode (((getf ke keysym) |> getf) scancode)) ;modkey = (modkey_of_sdl_modkey (((getf ke keysym) |> getf) modkey))}}
  end

  type t =
    | Quit
    | Window of window_event
    | Key of KeyboardEvent.t
    | None;;

  type sdl_event;;
  let sdl_event: sdl_event union typ = union "SDL_Event";;
  let etype = field sdl_event "type" int;;
  let window = field sdl_event "window" window_event_f;;
  let keyboard = field sdl_event "key" KeyboardEvent.key_event_f;;
    seal sdl_event;;
      




  let event_of_sdl_event sevent =
    let ty = getf sevent etype in
    match ty with
    |0x100 -> Quit
    |0x200 ->  Window (window_event_of_window_event_f (getf sevent window))

    |0x300 -> begin
	      let kevent = (getf sevent keyboard) in
	      Key (KeyboardEvent.of_key_event_f kevent)
	    end
    |0x301 -> begin
	      let kevent = (getf sevent keyboard) in
	      Key (KeyboardEvent.of_key_event_f kevent)
	    end
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
    let tex = load_f renderer ("resources/images/" ^ texname) in
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
