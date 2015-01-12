module M : Entity.M = struct
  open Sdl2;;
  type t = {position: Rect.t; image: Image.t;x_speed: int; y_speed: int; module Self: M};;
  let make () = {position={Rect.x=0;Rect.y=0;Rect.w=32;Rect.h=32};image=Image.load "avatar.png";x_speed=0;y_speed=0;Self=M};;
  let update self () = {position={x=self.position.x + self.x_speed; y=self.position.y + self.y_speed; w=self.position.w; h=self.position.h}};;
  let draw self draw_func = ignore (draw_func image ~dest=position); true;
  let beat () = [Entity.M.t]
end

