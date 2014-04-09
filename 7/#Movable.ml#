open Core.Std
open Event51
open Helpers
open WorldObject
open WorldObjectI

(** Class type for objects which constantly try to move in a calculated next
    direction. *)
class type movable_t =
object
  inherit world_object_i

  (** The next direction for which this object should move. *)
  method next_direction : Direction.direction option
end

class movable p (inv_speed:int option) : movable_t =
object (self)
  inherit world_object p


  (***********************)
  (***** Initializer *****)
  (***********************)

  (* ### TODO: Part 2 Movement ### *)
  initializer
    match inv_speed with
    | None -> ()
    | Some s ->
      let buffered_event = Event51.buffer s World.move_event in
      self#register_handler buffered_event self#do_move


  (**************************)
  (***** Event Handlers *****)
  (**************************)

  (* ### TODO: Part 2 Movement ### *)
  method private do_move (_ : unit list) = self#move self#next_direction

  (***************************)
  (***** Movable Methods *****)
  (***************************)

  (* ### TODO: Part 2 Movement ### *)
  (* NOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOTE: Is "Some North" a sufficiently "sensible default"? *)
  method next_direction = Some North

end
