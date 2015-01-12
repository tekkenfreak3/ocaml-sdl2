open Sdl2;;
module type M = sig
  type t
  (* val draw: texture -> ?src:Rect.rect -> ?dest:Rect.rect -> unit -> (unit, string) Result.t;; *)
  val draw: t -> (texture -> ?src:Rect.rect -> ?dest:Rect.rect -> unit -> (unit, string) Result.t) -> bool
  val update: t -> t
  val beat: t -> t list
  val make: () -> t
end

let draw (type s) (module E : M with type t = s, stru) draw_fun = ignore (E.draw stru draw_fun)
