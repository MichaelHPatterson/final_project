open Core.Std
open Ageable
open CarbonBased
open Event51
open Helpers
open Movable
open WorldObject
open WorldObjectI


(* ### Part 2 Movement ### *)
let human_inverse_speed = Some 1

(* ### Part 3 Actions ### *)
let max_gold_types = 5

(* ### Part 4 Aging ### *)
let human_lifetime = 1000

(* ### Part 5 Smart Humans ### *)
let max_sensing_range = 5

class type human_t =
object
  inherit ageable_t
  
  method private next_direction_default : Direction.direction option

  method private gold_length : int
end

(** Humans travel the world searching for towns to trade for gold.
    They are able to sense towns within close range, and they will return
    to King's Landing once they have traded with enough towns. *)

class human p (home : world_object_i): human_t =
object(self)
  inherit carbon_based p human_inverse_speed (World.rand human_lifetime) human_lifetime

  (******************************)
  (***** Instance Variables *****)
  (******************************)

  (* ### TODO: Part 3 Actions ### *)
  val mutable gold : int list = []

  (* ### TODO: Part 5 Smart Humans ### *)

  val sensing_range = World.rand max_sensing_range

  val gold_types = World.rand max_gold_types + 1

  (* ### TODO: Part 6 Custom Events ### *)

  (***********************)
  (***** Initializer *****)
  (***********************)

  (* ### TODO: Part 3 Actions ### *)
  initializer
    self#register_handler World.action_event self#do_action

  (* ### TODO: Part 6 Custom Events ### *)

  (**************************)
  (***** Event Handlers *****)
  (**************************)

  (* ### TODO: Part 6 Custom Events ### *)

  (**************************)
  (***** Helper Methods *****)
  (**************************)

  (* ### TODO: Part 3 Actions ### *)
  method private deposit_gold (neighbor : world_object_i) : unit =
    gold <- neighbor#receive_gold gold

  method private extract_gold (neighbor : world_object_i) : unit =
    match neighbor#forfeit_gold with
    | None -> ()
    | Some g -> gold <- g :: gold

  method private magnet_gold : world_object_i option =
    let in_range = World.objects_within_range self#get_pos sensing_range in
    let check_gold (obj : world_object_i) : bool =
      match obj#smells_like_gold with
      | None -> false
      | Some x -> not (List.mem gold x) in

    let gold_list =
      List.filter in_range (fun x -> x#get_name="town" && check_gold x) in
    let dist_to pt = Direction.distance self#get_pos pt in
    let find_closest x acc =
      match acc with
      | None -> Some x
      | Some x' -> if (dist_to x#get_pos) < (dist_to x'#get_pos) then Some x
		   else acc in
    List.fold_right gold_list ~f:find_closest ~init: None
				
								  
  (* ### TODO: Part 5 Smart Humans ### *)

  (********************************)
  (***** WorldObjectI Methods *****)
  (********************************)

  (* ### TODO: Part 1 Basic ### *)

  method! get_name = "human"

  method! draw_picture =
    let gold_string = string_of_int (List.length gold) in
    self#draw_circle (Graphics.rgb 0xC9 0xC0 0xBB) Graphics.black gold_string

  method! draw_z_axis = 2

  (* ### TODO: Part 3 Actions ### *)
  method private do_action () : unit =
    let exchange (neighbor : world_object_i) : unit =
      self#deposit_gold neighbor; self#extract_gold neighbor in
    List.iter ~f:exchange (World.get self#get_pos)

  (***************************)
  (***** Ageable Methods *****)
  (***************************)

  (* ### TODO: Part 4 Aging ### *)

  (***************************)
  (***** Movable Methods *****)
  (***************************)

  (* ### TODO: Part 2 Movement ### *)

  method! next_direction =
    if List.length (unique gold) >= gold_types
    then World.direction_from_to self#get_pos home#get_pos
    else
      if self#magnet_gold <> None 
      then let magnet_town = let Some obj = self#magnet_gold in obj in
	World.direction_from_to self#get_pos magnet_town#get_pos
      else self#next_direction_default

  (* ### TODO: Part 5 Smart Humans ### *)

  (* ### TODO: Part 6 Custom Events ### *)

  (***********************)
  (**** Human Methods ****)
  (***********************)

  (* ### TODO: Part 5 Smart Humans ### *)

  method private next_direction_default = None

  method private gold_length = List.length gold
end
