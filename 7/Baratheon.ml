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

  (********************************)
  (***** WorldObjectI Methods *****)
  (********************************)

  (* ### TODO: Part 5 Smart Humans *)

  (***********************)
  (***** Human Methods *****)
  (***********************)

  (* ### TODO: Part 5 Smart Humans *)
  method! get_name = "baratheon"

  method! private next_direction_default = Some (Direction.random World.rand)

end


