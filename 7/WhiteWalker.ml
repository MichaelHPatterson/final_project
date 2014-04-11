open Core.Std
open Helpers
open Movable
open WorldObject
open WorldObjectI

(* ### Part 2 Movement ### *)
let walker_inverse_speed = Some 1

(* ### Part 6 Custom Events ### *)
let max_destroyed_objects = 100

(** A White Walker will roam the world until it has destroyed a satisfactory
    number of towns *)
class white_walker p kings_landing : movable_t =
object (self)
  inherit movable p walker_inverse_speed

  (******************************)
  (***** Instance Variables *****)
  (******************************)

  (* ### TODO: Part 3 Actions ### *)
  val mutable objs_destroyed : int = 0

  (***********************)
  (***** Initializer *****)
  (***********************)

  (* ### TODO: Part 3 Actions ### *)
  initializer
    self#register_handler World.action_event self#do_action

  (**************************)
  (***** Event Handlers *****)
  (**************************)

  (* ### TODO: Part 3 Actions ### *)
  method private do_action () : unit =
    let destroy (obj : world_object_i) : unit =
      obj#die; objs_destroyed <- objs_destroyed + 1 in
    let gold_check = fun obj -> obj#smells_like_gold <> None in
    let gold_neighbors = List.filter ~f:gold_check (World.get self#get_pos) in
    List.iter ~f:destroy gold_neighbors

  (* ### TODO: Part 6 Custom Events ### *)

  (********************************)
  (***** WorldObjectI Methods *****)
  (********************************)

  (* ### TODO: Part 1 Basic ### *)

  method! get_name = "white_walker"

  method! draw =
    let display_string = string_of_int objs_destroyed in
    self#draw_circle (Graphics.rgb 0x89 0xCF 0xF0) Graphics.black display_string

  method! draw_z_axis = 4

  (* ### TODO: Part 3 Actions ### *)

  (***************************)
  (***** Movable Methods *****)
  (***************************)

  (* ### TODO: Part 2 Movement ### *)

  method! next_direction =
    if World.rand World.size < 2 then
      World.direction_from_to self#get_pos kings_landing#get_pos
    else Some (Direction.random World.rand)


  (* ### TODO: Part 6 Custom Events ### *)

end
