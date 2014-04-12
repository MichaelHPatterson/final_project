open Core.Std
open Ageable
open CarbonBased
open Event51
open Helpers
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
  inherit carbon_based p human_inverse_speed (World.rand human_lifetime)
          human_lifetime

  (******************************)
  (***** Instance Variables *****)
  (******************************)

  (* ### TODO: Part 3 Actions ### *)
  
  (* initializes starting gold to empty list *)
  val mutable gold : int list = []

  (* ### TODO: Part 5 Smart Humans ### *)

  val sensing_range = World.rand max_sensing_range

  val gold_types = World.rand max_gold_types + 1

  (* ### TODO: Part 6 Custom Events ### *)

  (* keeps track of dangerous object *)
  val mutable danger_object : world_object_i option = None

  (***********************)
  (***** Initializer *****)
  (***********************)

  (* ### TODO: Part 3 Actions ### *)

  (* adds do_action listener to action event, and adds react_danger listener
   * to the get_danger event *)
  initializer
    self#register_handler World.action_event self#do_action;
    self#register_handler home#get_danger_event self#react_danger

  (* ### TODO: Part 6 Custom Events ### *)

  (**************************)
  (***** Event Handlers *****)
  (**************************)

  (* ### TODO: Part 6 Custom Events ### *)
  
  (* exchanges gold, if possible, with all neighbor objects; if a dangerous 
   * object is detected, it will do damage to the object and the human dies *)
  method private do_action () : unit =
    let exchange (neighbor : world_object_i) : unit =
      self#deposit_gold neighbor; self#extract_gold neighbor in
    List.iter ~f:exchange (World.get self#get_pos);
    match danger_object with
    | None -> ()
    | Some o ->
      if self#get_pos = o#get_pos then
        (o#receive_damage;
        self#die)

  (* every time a get_danger_event is fired, the dangerous object is stored in
   * danger_object; listener for enemy's death is added to die_event of enemy *)
  method private react_danger (enemy : world_object_i) : unit =
    danger_object <- Some enemy;
    self#register_handler enemy#get_die_event self#enemy_dead

  (* upon enemy's death, removes it from danger_object *)
  method private enemy_dead () : unit = danger_object <- None

  (**************************)
  (***** Helper Methods *****)
  (**************************)

  (* ### TODO: Part 3 Actions ### *)
  (* calls the receive_gold method on neighbor, the return value of which is the
   * gold that may be kept by the human itself; its gold is updated to this *)
  method private deposit_gold (neighbor : world_object_i) : unit =
    gold <- neighbor#receive_gold gold

  (* calls forfeit_gold on neighbor, which returns the gold that can be added to
   * the human's list of gold *)
  method private extract_gold (neighbor : world_object_i) : unit =
    match neighbor#forfeit_gold with
    | None -> ()
    | Some g -> gold <- g :: gold

  (* a two-step process for finding the closest town within a certain range with
   * an uncollected gold type: first, all towns within range and w/ uncollected
   * gold type are found, and then  with a gold type*)
  method private magnet_gold : world_object_i option =
    let in_range = World.objects_within_range self#get_pos sensing_range in
    let check_gold (obj : world_object_i) : bool =
      match obj#smells_like_gold with
      | None -> false
      | Some x -> not (List.mem gold x) in
    let gold_list =
      List.filter in_range ~f:(fun x -> x#get_name="town" && check_gold x) in
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
  
  (* draws the default human circle with a string of number of gold *)
  method! draw_picture =
    let gold_string = string_of_int (List.length gold) in
    self#draw_circle (Graphics.rgb 0xC9 0xC0 0xBB) Graphics.black gold_string

  method! draw_z_axis = 2

  (* ### TODO: Part 3 Actions ### *)

  (***************************)
  (***** Ageable Methods *****)
  (***************************)

  (* ### TODO: Part 4 Aging ### *)

  (***************************)
  (***** Movable Methods *****)
  (***************************)

  (* ### TODO: Part 2 Movement ### *)
  
  (* if danger_object detected, will go toward that object; if max gold types
   * are met/exceeded, then go home (King's Landing); else, travel with default
   * direction *)
  method! next_direction =
    match danger_object with
    | Some o -> World.direction_from_to self#get_pos o#get_pos
    | None ->
      if List.length (unique gold) >= gold_types
      then World.direction_from_to self#get_pos home#get_pos
      else
        match self#magnet_gold with
        | None -> self#next_direction_default
        | Some obj -> World.direction_from_to self#get_pos obj#get_pos

  (* ### TODO: Part 5 Smart Humans ### *)

  (* ### TODO: Part 6 Custom Events ### *)

  (***********************)
  (**** Human Methods ****)
  (***********************)

  (* ### TODO: Part 5 Smart Humans ### *)

  (* default next direction of a normal human is None *)
  method private next_direction_default = None

  (* allows the amount of gold to be accessed outside object *)
  method private gold_length = List.length gold
end
