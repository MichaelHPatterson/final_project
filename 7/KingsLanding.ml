open Core.Std
open Event51
open Helpers
open WorldObject
open WorldObjectI

(* ### Part 3 Actions ### *)
let starting_gold = 500
let cost_of_human = 10
let spawn_probability = 20
let gold_probability = 50
let max_gold_deposit = 3

(** King's Landing will spawn humans and serve as a deposit point for the gold
    that humans gather. It is possible to steal gold from King's Landing;
    however the city will signal that it is in danger and its loyal humans
    will become angry. *)
class kings_landing p :
object
  inherit world_object_i 

  method forfeit_treasury : int -> world_object_i -> int

  method get_gold_event : int Event51.event

  method get_gold : int
end =
object (self)
  (* inherits from world_object using the point p *)
  inherit world_object p

  (******************************)
  (***** Instance Variables *****)
  (******************************)

  (* ### TODO: Part 3 Actions ### *)

  (* Contains the gold amount, initialized as starting_gold *)
  val mutable gold : int = starting_gold

  (* ### TODO: Part 6 Custom Events ### *)

  (* Event that fires when the city receives gold *)
  val gold_event : int Event51.event = Event51.new_event ()


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

  (* Responds to an action event. Spawns human if there's enough gold, with
   * probability 1/spawn_probability. Increments gold with probability
   * 1/gold_probability. *)
  method private do_action () : unit =
    if World.rand gold_probability = 0 then
      gold <- gold + 1;
    if gold >= cost_of_human && World.rand spawn_probability = 0 then
      (gold <- gold - cost_of_human; self#generate_human)

  (* ### TODO: Part 4 Aging ### *)


  (**************************)
  (***** Helper Methods *****)
  (**************************)

  (* ### TODO: Part 4 Aging ### *)

  (* Helper that spawns a Baratheon or a Lannister, with equal probability. *)
  method private generate_human =
    if World.rand 2 = 0 then
      ignore(new Baratheon.baratheon self#get_pos (self :> world_object_i))
    else ignore(new Lannister.lannister self#get_pos (self :> world_object_i))
    

  (****************************)
  (*** WorldObjectI Methods ***)
  (****************************)

  (* ### TODO: Part 1 Basic ### *)

  method! get_name = "kings_landing"

  (* Draws a yellow circle displaying the amount of gold in black *)
  method! draw =
    let gold_string = string_of_int gold in
    self#draw_circle (Graphics.rgb 0xFF 0xD7 0x00) Graphics.black gold_string

  (* ### TODO: Part 3 Actions ### *)

  (* Receives gold. Collects the minimum of max_gold_deposit and List.length
   * gold_offer, and allows the depositor to keep nothing. Fires gold_event with
   * the current amount of gold. *)
  method! receive_gold (gold_offer : int list) =
    gold <- gold + Int.min (List.length gold_offer) max_gold_deposit;
    Event51.fire_event gold_event self#get_gold;
    []

  (* ### TODO: Part 6 Custom Events ### *)


  (**********************************)
  (***** King's Landing Methods *****)
  (**********************************)

  (* ### TODO: Part 3 Actions ### *)

  (* Forfeits money to a dragon. Gives up the min of current gold and the amount
   * requested. Updates gold and fires a danger event. *)
  method forfeit_treasury (n : int) (stealer : world_object_i) : int =
    let stolen = Int.min gold n in
    gold <- gold - stolen;
    self#danger stealer;
    stolen

  (* ### TODO: Part 6 Custom Events ### *)

  (* exposes the gold_event outside the class *)
  method get_gold_event : int Event51.event = gold_event

  (* exposes the amount of gold *)
  method get_gold : int = gold

end
