open Core.Std
open Helpers
open Movable
open WorldObject
open WorldObjectI

(* ### Part 3 Actions ### *)
let gold_theft_amount = 1000

(* ### Part 4 Aging ### *)
let dragon_starting_life = 20

(* ### Part 2 Movement ### *)
let dragon_inverse_speed = Some 10

class dragon p kings_landing : movable_t =
object (self)
  inherit movable p dragon_inverse_speed

  (******************************)
  (***** Instance Variables *****)
  (******************************)

  (* ### TODO: Part 3 Actions ### *)
  val mutable stolen_gold : int = 0

  (* ### TODO: Part 6 Events ### *)

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
  method private do_action : unit -> unit = fun _ ->
    if self#get_pos = kings_landing#get_pos then
      let cast = self :> world_object_i in
      let loot = kings_landing#forfeit_treasury gold_theft_amount cast in
      stolen_gold <- stolen_gold + loot
    

  (* ### TODO: Part 6 Custom Events ### *)

  (********************************)
  (***** WorldObjectI Methods *****)
  (********************************)

  (* ### TODO: Part 1 Basic ### *)

  method! get_name = "dragon"

  method! draw =
    let stolen_gold_string = string_of_int stolen_gold in
    self#draw_circle Graphics.red Graphics.black stolen_gold_string

  method! draw_z_axis = 3

  (* ### TODO: Part 3 Actions ### *)

  (* ### TODO: Part 6 Custom Events ### *)

  (***************************)
  (***** Movable Methods *****)
  (***************************)

  (* ### TODO: Part 2 Movement ### *)

  method! next_direction = Direction.natural self#get_pos kings_landing#get_pos


  (* ### TODO: Part 6 Custom Events ### *)

end
