(* Copyright (C) 2014 Daniel Wilkins  *)
(* This code contains many magic numbers. If you see a plain number then that's almost certainly why. *)

open Ctypes;;
open Foreign;;
open Unsigned;;
open Core.Std;;

module Position : sig
  type t = {x: int; y:int};;
end = struct
  type t = {x:int; y:int};;
end
	
module Rect : sig
  type rect = {x: int; y: int; w: int; h: int};;
  type t;;
  val t : rect typ;;
end = struct
  type rect = {x: int; y: int; w: int; h: int}

  type sdl_rect;;
  let sdl_rect : sdl_rect structure typ = structure "SDL_Rect";;
  let x = field sdl_rect "y" int;;
  let y = field sdl_rect "x" int;;
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

  let init_ttf_f = foreign "TTF_Init" (void @-> returning int);;

  let init flags =
    if init_f flags = 1
    then
      Result.Error (Error.get_error ())
    else
      if (init_ttf_f ()) = -1
      then
	Result.Error (Error.get_error ())
      else
	Result.Ok ()
end
		
module Window : sig
  type t
  val t : t typ;;
  val pos_undefined : int;;
  val make : string -> int -> int -> int -> int -> int -> (t, string) Result.t
end = struct
  type t = unit ptr;;
  let t : t typ = ptr void;;
  let pos_undefined = 0x1FFF0000;; (*magic*)
  let create_window_f = foreign "SDL_CreateWindow" (string @-> int @-> int @-> int @-> int @-> int @-> returning t);;
  let make title x y w h flags =
    let win = create_window_f title x y w h flags in
    if  win <> null
    then
      Result.Ok win
    else
      Result.Error (Error.get_error ());;
end
	
module Texture : sig
  type t
  val t : t typ
  val exists : t -> bool
  val query : t -> int * int
  val free : t -> unit
end = struct
  type t = unit ptr
  let t : t typ = ptr void
  let exists tex = tex <> null
  let query_f = foreign "SDL_QueryTexture" (t @-> ptr uint32_t @-> ptr int @-> ptr int @-> ptr int @-> returning void)

  let query tex =
    let w = allocate int 0 in
    let h = allocate int 0 in
    query_f tex (from_voidp uint32_t null) (from_voidp int null) w h;
    ((!@ w), (!@ h))
  let free = foreign "SDL_DestroyTexture" (t @-> returning void)
end

module Render : sig
  type t
  val t : t typ

  val make : Window.t -> int -> (t, string) Result.t
  val copy :  t -> Texture.t -> Rect.rect -> Rect.rect -> (unit, string) Result.t
  val present : t -> unit

end = struct
  type t = unit ptr;;
  let t : t typ = ptr void;;
    
  let create_renderer_f = foreign "SDL_CreateRenderer" (Window.t @-> int @-> int @-> returning t);;
  
  let make window flags =
    let rend = create_renderer_f window ~-1 flags in (* ~-1 is to avoid confusing the parser, it just means -1*)
    if rend <> null
    then
      Result.Ok rend
    else
      Result.Error (Error.get_error ());;

  let copy_f = foreign "SDL_RenderCopy" (t @-> Texture.t @-> Rect.t @-> Rect.t @-> returning int);;

  let copy renderer tex src dest =
    if (copy_f renderer tex src dest) = 0 then
      Result.Ok ()
    else
      Result.Error (Error.get_error ())

    
  let present = foreign "SDL_RenderPresent" (t @-> returning void);;

end

module Image : sig
  val quit : unit -> unit -> unit;;
  val load_f : Render.t -> string -> Texture.t;;
  val load : Render.t -> string -> (Texture.t, string) Result.t;;
end = struct
  let quit () = foreign "IMG_Quit" (void @-> returning void);;
  let load_f = foreign "IMG_LoadTexture" (Render.t @-> string @-> returning Texture.t);;
  let load renderer texname =
    let tex = load_f renderer ("resources/images/" ^ texname) in
    if Texture.exists tex
    then
      Result.Ok tex
    else
      Result.Error (Error.get_error ());;

end

module Event = struct

  module WindowEvent = struct
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


	

    type t = {timestamp: int; window_id: int; data1: int; data2: int};;

    let of_window_event_f wef = {timestamp= (getf wef timestamp); window_id= (getf wef windowID);data1= (getf wef data1);data2= (getf wef data2)};;
  end
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
    let padding = field key_event_f "padding2" uint16_t;; (*WARNING this is 2 u8s in the original source. it shouldn't make a difference but if a bug happens...*)
    let keysym = field key_event_f "keysym" keysym_f;;
    seal key_event_f;;


    type scancode = ScancodeUnknown | ScancodeZ | ScancodeUp | ScancodeDown | ScancodeLeft | ScancodeRight | ScancodeEsc;;
    let scancode_of_sdl_scancode sk = match sk with
      |29 -> ScancodeZ
      |41 -> ScancodeEsc
      |79 -> ScancodeRight
      |80 -> ScancodeLeft
      |81 -> ScancodeDown
      |82 -> ScancodeUp
      |_ -> ScancodeUnknown;;

      let int_of_scancode sk = match sk with
	|ScancodeZ -> 29
	|ScancodeEsc -> 41
	|ScancodeRight -> 79
	|ScancodeLeft -> 80
	|ScancodeDown -> 81
	|ScancodeUp -> 82
	|ScancodeUnknown -> 0;;


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
      |0x4000 -> ModMode
      |_ -> ModNone;;
      
    type keysym = {scancode: scancode; modkey: modkey};;

    type key_state = Released | Pressed;;
    let key_state_of_sdl_keystate ks = if Unsigned.UInt8.to_int ks = 1 then Pressed else Released;;
    type t = {timestamp: int; window_id: int; state: key_state; repeat: bool; keysym: keysym};;

    let of_key_event_f ke = {timestamp= (Unsigned.UInt32.to_int (getf ke timestamp)); window_id= (Unsigned.UInt32.to_int (getf ke windowID)); state= (key_state_of_sdl_keystate (getf ke state)); repeat= (bool_of_int (Unsigned.UInt8.to_int (getf ke repeat)));keysym = {scancode= (scancode_of_sdl_scancode (((getf ke keysym) |> getf) scancode)) ;modkey = (modkey_of_sdl_modkey (((getf ke keysym) |> getf) modkey))}}

    let state_f = foreign "SDL_GetKeyboardState" (ptr int @-> returning (ptr uint8_t));;
    let state () =
      let i = allocate int 0 in
      let pt = state_f i in
      let bool_of_int i = match Unsigned.UInt8.to_int i with
	|0 -> false
	|_ -> true in
      List.to_array (List.map ~f:bool_of_int (CArray.to_list (CArray.from_ptr pt (!@ i))));;
  end

  type t =
    | Quit
    | Window of WindowEvent.t
    | Key of KeyboardEvent.t
    | None;;

  type sdl_event;;
  let sdl_event: sdl_event union typ = union "SDL_Event";;
  let etype = field sdl_event "type" int;;
  let window = field sdl_event "window" WindowEvent.window_event_f;;
  let keyboard = field sdl_event "key" KeyboardEvent.key_event_f;;
    seal sdl_event;;
      




  let event_of_sdl_event sevent =
    let ty = getf sevent etype in
    match ty with
    |0x100 -> Quit
    |0x200 -> Window (WindowEvent.of_window_event_f (getf sevent window))

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
		 

  
module Etc : sig
  val delay : int -> unit
  val quit : unit -> unit
end = struct
  let delay = foreign "SDL_Delay" (int @-> returning void);;
  let quit_f = foreign "SDL_Quit" (void @-> returning void);;
  let quit () = quit_f (); exit 0;;
end

module Surface : sig
  type t
  val t : t typ
  val convert : Render.t -> t -> Texture.t
end = struct
  type t = unit ptr
  let t: t typ = ptr void
  let convert_t = foreign "SDL_CreateTextureFromSurface" (Render.t @-> t @-> returning Texture.t)
			        
  let free = foreign "SDL_FreeSurface" (t @-> returning void)
  let convert renderer surface =
    let ret = convert_t renderer surface in
    free surface;
    ret

end
	
module Color : sig
  type color = {r: int; g: int; b: int; a: int}
  type t
  val t : color typ
  val red : color
  val green : color
  val blue : color
  val yellow : color
  val purple : color
  val cyan : color
end = struct
  type color = {r: int; g: int; b: int; a: int}

  type sdl_color
  let sdl_color : sdl_color structure typ = structure "SDL_Color";;
  let r = field sdl_color "r" uint8_t;;
  let g = field sdl_color "g" uint8_t;;
  let b = field sdl_color "b" uint8_t;;
  let a = field sdl_color "a" uint8_t;;
  seal sdl_color;;
  let sdl_color_of_color color =
    let toint = Unsigned.UInt8.to_int in
    let ret = make sdl_color in
    setf ret r (Unsigned.UInt8.of_int color.r);
    setf ret g (Unsigned.UInt8.of_int color.g);
    setf ret b (Unsigned.UInt8.of_int color.b);
    setf ret a (Unsigned.UInt8.of_int color.a);
    ret;;
  let color_of_sdl_color col =
    {r = (Unsigned.UInt8.to_int (getf col r));g = (Unsigned.UInt8.to_int (getf col g));b = (Unsigned.UInt8.to_int (getf col b));a = (Unsigned.UInt8.to_int (getf col a))};;

  let red = {r=255;g=0;b=0;a=255}
  let green = {r=0;g=255;b=0;a=255}
  let blue = {r=255;g=0;b=255;a=255}
  let purple = {r=255;g=0;b=255;a=255}
  let yellow = {r=255;g=255;b=0;a=255}
  let cyan = {r=0;g=255;b=255;a=255}
  let white = {r=255;g=255;b=255;a=255}
  let black = {r=0;g=0;b=0;a=255}
		
  type t;;
  let t = view ~read:color_of_sdl_color ~write:sdl_color_of_color sdl_color
end
	
module Ttf : sig
  type t
  val t : t typ
  val load : string -> int -> (t, string) Result.t
  val render : Render.t -> t -> string -> Color.color -> Texture.t
end = struct
  type t = unit ptr
  let t : t typ = ptr void
  let load_f = foreign "TTF_OpenFont" (string @-> int @-> returning t)
  let load filename ptsize =
    let result = load_f ("resources/fonts/" ^ filename) ptsize in
    if result <> Ctypes.null then
      Result.Ok result
    else
      Result.Error (Error.get_error ())
  let render_f = foreign "TTF_RenderText_Blended" (t @-> string @-> Color.t @-> returning Surface.t)
  let render renderer font text color = Surface.convert renderer (render_f font text color)
end
