open Core.Std
open Human
open WorldObject
open WorldObjectI

(* Baratheons should travel in a random direction. *)
class baratheon p city : human_t =
object
  inherit human p city

  (******************************)
  (***** Instance Variables *****)
  (******************************)

  (* ### TODO: Part 5 Smart Humans *)

  (* no instance variables necessary for baratheons *)

  (********************************)
  (***** WorldObjectI Methods *****)
  (********************************)

  (* ### TODO: Part 5 Smart Humans *)
  method! get_name = "baratheon"

  (***********************)
  (***** Human Methods *****)
  (***********************)

  (* ### TODO: Part 5 Smart Humans *)

  (* baratheons move in a random direction by default *)
  method! private next_direction_default = Some (Direction.random World.rand)

end
