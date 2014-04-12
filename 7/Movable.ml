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

(* A class for any movable objects, taking a point p and the inverse of
 * the speed inv_speed as parameters. *)
class movable p (inv_speed:int option) : movable_t =
object (self)
  (* inherits world_object using the point p *)
  inherit world_object p


  (***********************)
  (***** Initializer *****)
  (***********************)

  (* ### TODO: Part 2 Movement ### *)

  (* If inv_speed is not None, then use Event51.buffer to generate an event that
   * fires on every s fires of World.move_event. Adds a listener to that event
   * that calls do_move. *)
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

  (* Moves by calling the move from WorldObject.ml and next_direction *)
  method private do_move (_ : unit list) = self#move self#next_direction


  (***************************)
  (***** Movable Methods *****)
  (***************************)

  (* ### TODO: Part 2 Movement ### *)

  (* Arbitrary default for next_direction, keeping the object stationery *)
  method next_direction = None

end
