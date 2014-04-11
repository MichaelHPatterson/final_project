open Core.Std
open WorldObject
open WorldObjectI

(* Baratheons should travel in a random direction. *)
class baratheon p city : world_object_i =
object (self)
  inherit Human.human p city as super

  (******************************)
  (***** Instance Variables *****)
  (******************************)

  (* ### TODO: Part 5 Smart Humans *)

  (********************************)
  (***** WorldObjectI Methods *****)
  (********************************)

  (* ### TODO: Part 5 Smart Humans *)

  (***********************)
  (***** Human Methods *****)
  (***********************)

  (* ### TODO: Part 5 Smart Humans *)
  method! get_name = "baratheon"

  method! next_direction_default = Some (Direction.random (World.rand))

end


