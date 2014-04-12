open Core.Std
open Helpers
open Movable
open WorldObjectI

(* ### Part 2 Movement ### *)
let walker_inverse_speed = Some 1

(* ### Part 6 Custom Events ### *)
let max_destroyed_objects = 100

(** A White Walker will roam the world until it has destroyed a satisfactory
    number of towns *)
class white_walker p kings_landing wall : movable_t =
object (self)
  (* inherit the movable class using current position and speed *)
  inherit movable p walker_inverse_speed


  (******************************)
  (***** Instance Variables *****)
  (******************************)

  (* ### TODO: Part 3 Actions ### *)

  (* Mutable variable to track the number of objects destroyed *)
  val mutable objs_destroyed : int = 0


  (***********************)
  (***** Initializer *****)
  (***********************)

  (* ### TODO: Part 3 Actions ### *)

  (* Adds a listener to World.action_event that calls do_action *)
  initializer
    self#register_handler World.action_event self#do_action


  (**************************)
  (***** Event Handlers *****)
  (**************************)

  (* ### TODO: Part 3 Actions ### *)

  (* Responds to an action event. If dangerous, destroys all neighbors that
   * smells like gold. If neighboring Dany and not dangerous, then dies. *)
  method private do_action () : unit =
    if self#is_dangerous then
      let destroy (obj : world_object_i) : unit =
        obj#die; objs_destroyed <- objs_destroyed + 1 in
      let gold_check = fun obj -> obj#smells_like_gold <> None in
      let gold_neighbors = List.filter ~f:gold_check (World.get self#get_pos) in
      List.iter ~f:destroy gold_neighbors
    else if self#get_pos = wall#get_pos then self#die

  (* ### TODO: Part 6 Custom Events ### *)

  (* Helper that evaluates whether the White Walker should stay dangerous.
   * Placed under "Event Handlers" because there's no better location, and it's
   * closely associated with do_action. *)
  method private is_dangerous : bool = objs_destroyed < max_destroyed_objects


  (********************************)
  (***** WorldObjectI Methods *****)
  (********************************)

  (* ### TODO: Part 1 Basic ### *)

  method! get_name = "white_walker"

  (* Places the white_walker above all conflicting objects *)
  method! draw_z_axis = 4

  (* ### TODO: Part 3 Actions ### *)

  (* displays a circle showing the number of objects destroyed, in black *)
  method! draw =
    let display_string = string_of_int objs_destroyed in
    self#draw_circle (Graphics.rgb 0x89 0xCF 0xF0) Graphics.black display_string


  (***************************)
  (***** Movable Methods *****)
  (***************************)

  (* ### TODO: Part 2 Movement ### *)

  (* Determines the next direction. If not dangerous, go to the wall; else
   * there's a 2/World.size chance of going toward King's Landing; else move
   * randomly. *)
  method! next_direction =
    if not self#is_dangerous then
      World.direction_from_to self#get_pos wall#get_pos
    else if World.rand World.size < 2 then
      World.direction_from_to self#get_pos kings_landing#get_pos
    else Some (Direction.random World.rand)

  (* ### TODO: Part 6 Custom Events ### *)

end
