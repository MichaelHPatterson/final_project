open Core.Std
open WorldObject
open WorldObjectI

(** Ponds serve as obstruction for other world objects. *)
class pond p : world_object_i =
object (self)
  inherit world_object p

  (********************************)
  (***** WorldObjectI Methods *****)
  (********************************)

  (* ### TODO: Part 1 Basic ### *)

  method! get_name = "pond"

  (* Draws a blue circle with no text *)
  method! draw = self#draw_circle Graphics.blue Graphics.black ""

  (* Overrides is_obstacle to make the pond an obstacle*)
  method! is_obstacle = true
end
