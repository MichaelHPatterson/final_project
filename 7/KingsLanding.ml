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
  inherit world_object p

  (******************************)
  (***** Instance Variables *****)
  (******************************)

  (* ### TODO: Part 3 Actions ### *)
  val mutable gold : int = starting_gold

  (* ### TODO: Part 6 Custom Events ### *)
  (* NOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOTE: Should this be mutable? And is it the right way to do it? *)
  val mutable gold_event : int Event51.event = Event51.new_event ()

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
    if World.rand gold_probability = 0 then
      gold <- gold + 1;
    if gold >= cost_of_human && World.rand spawn_probability = 0 then
      (gold <- gold - cost_of_human; self#generate_human)

  (* ### TODO: Part 4 Aging ### *)

  (**************************)
  (***** Helper Methods *****)
  (**************************)

  (* ### TODO: Part 4 Aging ### *)
  method private generate_human =
    if World.rand 2 = 0 then
      ignore(new Baratheon.baratheon self#get_pos (self :> world_object_i))
    else ignore(new Lannister.lannister self#get_pos (self :> world_object_i))
    

  (****************************)
  (*** WorldObjectI Methods ***)
  (****************************)

  (* ### TODO: Part 1 Basic ### *)

  method! get_name = "kings_landing"

  method! draw =
    let gold_string = string_of_int gold in
    self#draw_circle (Graphics.rgb 0xFF 0xD7 0x00) Graphics.black gold_string

  (* ### TODO: Part 3 Actions ### *)
  method! receive_gold (gold_offer : int list) =
    gold <- gold + Int.max (List.length gold_offer) max_gold_deposit;
    (* NOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOTE: Is the 2nd argument correct? The specs don't explicitly say what exactly it should be. *)
    Event51.fire_event gold_event gold;
    []

  (* ### TODO: Part 6 Custom Events ### *)

  (**********************************)
  (***** King's Landing Methods *****)
  (**********************************)

  (* ### TODO: Part 3 Actions ### *)
  method forfeit_treasury (n : int) (stealer : world_object_i) : int =
    let stolen = Int.min gold n in
    gold <- gold - stolen;
    self#danger stealer;
    stolen

  (* ### TODO: Part 6 Custom Events ### *)

  method get_gold_event : int Event51.event = gold_event

  (* NOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOTE: What is the purpose of this method? It never gets referenced again *)
  method get_gold : int = gold

end
