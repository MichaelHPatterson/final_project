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

class dragon p kings_landing dany : movable_t =
object (self)
  inherit movable p dragon_inverse_speed

  (******************************)
  (***** Instance Variables *****)
  (******************************)

  (* ### TODO: Part 3 Actions ### *)
  val mutable stolen_gold : int = 0

  (* ### TODO: Part 6 Events ### *)
  val mutable life : int = dragon_starting_life

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
    if self#get_pos = kings_landing#get_pos then
      (let cast = (self :> world_object_i) in
      let loot = kings_landing#forfeit_treasury gold_theft_amount cast in
      stolen_gold <- stolen_gold + loot);
    (* The 2nd condition (stolen_gold > 0) makes sure that the dragon doesn't
     * just spawn, move around randomly for a bit because of ponds, land back
     * on Dany on accident, and then die. *)
    if self#get_pos = dany#get_pos && stolen_gold > 0 then
      (stolen_gold <- 0;
      if kings_landing#get_gold < gold_theft_amount / 2 then self#die)

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
  (* NOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOTE: Is this the correct location? *)
  method! receive_damage =
    life <- life - 1;
    if life <= 0 then self#die

  (***************************)
  (***** Movable Methods *****)
  (***************************)

  (* ### TODO: Part 2 Movement ### *)

  (* ### TODO: Part 6 Custom Events ### *)

  method! next_direction =
    let dst = if stolen_gold = 0 then kings_landing#get_pos else dany#get_pos in
    World.direction_from_to self#get_pos dst

end
