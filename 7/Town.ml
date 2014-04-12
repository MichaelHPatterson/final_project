open Core.Std
open Helpers
open Ageable
open CarbonBased

(* ### Part 3 Actions ### *)
let next_gold_id = ref 0
let get_next_gold_id () =
  let p = !next_gold_id in incr next_gold_id ; p

(* ### Part 3 Actions ### *)
let max_gold = 5
let produce_gold_probability = 50
let expand_probability = 4000
let forfeit_gold_probability = 3

(* ### Part 4 Aging ### *)
let town_lifetime = 2000

(** Towns produce gold.  They will also eventually die if they are not cross
    pollenated. *)
class town p gold_id : ageable_t =
object (self)
  (* Inherits from the carbon_based class using the location p, a speed of None,
   * a maximum lifetime of town_lifetime, and a random starting lifetime between
   * 0 (inclusive) and town_lifetime (exclusive). *)
  inherit carbon_based p None (World.rand town_lifetime) town_lifetime


  (******************************)
  (***** Instance Variables *****)
  (******************************)

  (* ### TODO: Part 3 Actions ### *)

  (* Keeps track of the amount of gold. Starts at a random value between 0
   * (inclusive) and max_gold (exclusive). *)
  val mutable gold = World.rand max_gold


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

  (* Responds to an action event. Increments gold with probability
   * 1/produce_gold_probability if gold<max_gold. Also spawns a new town
   * nearby with probability 1/expand_probability. *)
  method private do_action () : unit =
    if gold < max_gold && World.rand produce_gold_probability = 0 then
      gold <- gold + 1;
    if World.rand expand_probability = 0 then
      World.spawn 1 self#get_pos (fun p -> ignore(new town p gold_id))


  (********************************)
  (***** WorldObjectI Methods *****)
  (********************************)

  (* ### TODO: Part 1 Basic ### *)

  method! get_name = "town"

  (* displays a circle showing the amount of gold, in black *)
  method! draw_picture =
    let gold_string = string_of_int gold in
    self#draw_circle (Graphics.rgb 0x96 0x4B 0x00) Graphics.black gold_string

  (* ### TODO: Part 4 Aging ### *)

  (* ### TODO: Part 3 Actions ### *)

  (* smells like gold if the amount of gold is nonzero *)
  method! smells_like_gold = if gold <= 0 then None else Some gold_id

  (* Forfeits a unit of gold with probability 1/forfeit_gold_probability,
   * if current gold is nonzero. *)
  method! forfeit_gold =
    if gold <= 0 || World.rand forfeit_gold_probability > 0 then None
    else
      let _ = gold <- gold - 1 in
      Some gold_id

  (* ### TODO: Part 4 Aging ### *)

  (* Calls reset_life if the received gold list contains any id different
   * from the towns own gold_id. *)
  method! receive_gold (gold_list : int list) : int list =
    if List.exists ~f:((<>) gold_id) gold_list then self#reset_life;
    gold_list
end
