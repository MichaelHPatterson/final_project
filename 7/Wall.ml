open Core.Std
open Helpers
open WorldObject
open WorldObjectI

(* ### Part 6 Custom Events ### *)
let town_limit = 200

(** The Wall will spawn a white walker when there are enough towns
    in the world. *)
class wall p kings_landing : world_object_i =
object (self)
  (* inherits from world_object using the location p *)
  inherit world_object p


  (******************************)
  (***** Instance Variables *****)
  (******************************)

  (* ### TODO Part 6 Custom Events ### *)


  (***********************)
  (***** Initializer *****)
  (***********************)

  (* ### TODO Part 6 Custom Events ### *)

  (* Adds a listener to World.action_event that calls do_action *)
  initializer
    self#register_handler World.action_event self#do_action


  (**************************)
  (***** Event Handlers *****)
  (**************************)

  (* ### TODO Part 6 Custom Events ### *)

  (* Responds to an action event. Spawns a new White Walker if there is no
   * other walker and the number of objects that smell like gold exceeds
   * town_limit. *)
  method private do_action () : unit =
    let smell_counter o n : int =
      if o#smells_like_gold <> None then n + 1 else n in
    let smelly_num : int = World.fold smell_counter 0 in
    let walker_checker o b : bool = b && o#get_name <> "white_walker" in
    let no_walkers = World.fold walker_checker true in
    if smelly_num > town_limit && no_walkers then
      (ignore(new WhiteWalker.white_walker self#get_pos kings_landing self);
      print_string "white walkers! ";
      flush_all ())


  (********************************)
  (***** WorldObjectI Methods *****)
  (********************************)

  (* ### TODO Part 1 Basic ### *)

  method! get_name = "wall"

  (* draws a circle with a white W in it *)
  method! draw = self#draw_circle (Graphics.rgb 70 100 130) Graphics.white "W"

end

