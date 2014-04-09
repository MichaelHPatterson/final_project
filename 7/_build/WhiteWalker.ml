open Core.Std
open Helpers
open WorldObject
open WorldObjectI

(* ### Part 2 Movement ### *)
let walker_inverse_speed = Some 1

(* ### Part 6 Custom Events ### *)
let max_destroyed_objects = 100

(** A White Walker will roam the world until it has destroyed a satisfactory
    number of towns *)
class white_walker p kings_landing : Movable.movable_t =
object (self)
  inherit Movable.movable p walker_inverse_speed

  (******************************)
  (***** Instance Variables *****)
  (******************************)

  (* ### TODO: Part 3 Actions ### *)

  (***********************)
  (***** Initializer *****)
  (***********************)

  (* ### TODO: Part 3 Actions ### *)

  (**************************)
  (***** Event Handlers *****)
  (**************************)

  (* ### TODO: Part 3 Actions ### *)

  (* ### TODO: Part 6 Custom Events ### *)

  (********************************)
  (***** WorldObjectI Methods *****)
  (********************************)

  (* ### TODO: Part 1 Basic ### *)

  method! get_name = "white_walker"

  method! draw = self#draw_circle (Graphics.rgb 0x89 0xCF 0xF0) Graphics.black ""

  method! draw_z_axis = 4

  (* ### TODO: Part 3 Actions ### *)

  (***************************)
  (***** Movable Methods *****)
  (***************************)

  (* ### TODO: Part 2 Movement ### *)

  method! next_direction =
    if Random.float (float World.size) < 2. then
      Direction.natural self#get_pos kings_landing#get_pos
    else Some (Direction.ord (Random.int 8))


  (* ### TODO: Part 6 Custom Events ### *)

end
