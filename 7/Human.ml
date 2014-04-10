open Core.Std
open Event51
open Helpers
open WorldObject
open WorldObjectI

(* ### Part 2 Movement ### *)
let human_inverse_speed = Some 1

(* ### Part 3 Actions ### *)
(* NOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOTE: What are we supposed to do here??? *)
let max_gold_types = 5

(* ### Part 4 Aging ### *)
let human_lifetime = 1000

(* ### Part 5 Smart Humans ### *)
let max_sensing_range = 5

(** Humans travel the world searching for towns to trade for gold.
    They are able to sense towns within close range, and they will return
    to King's Landing once they have traded with enough towns. *)
(* NOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOTE: Would it be better to include "open Movable" at the top and type "movable_t" and "movable", or to keep it as is (typing "Movable.movable_t" and "Movable.movable")? *)
class human p : Movable.movable_t =
object(self)
  inherit Movable.movable p human_inverse_speed

  (******************************)
  (***** Instance Variables *****)
  (******************************)

  (* ### TODO: Part 3 Actions ### *)
  (* NOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOTE: What are we supposed to name the gold variable??? *)
  (* NOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOTE: Have I done this in the correct place? Should it be elsewhere? *)
  (* NOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOTE: Am I correct to initialize this value to an empty list? *)
  val mutable gold : int list = []

  (* ### TODO: Part 5 Smart Humans ### *)

  (* ### TODO: Part 6 Custom Events ### *)

  (***********************)
  (***** Initializer *****)
  (***********************)

  (* ### TODO: Part 3 Actions ### *)
  (* NOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOTE: Is this where/how we're supposed to add the listener to action_event? This is how I've done it in a bunch of different files (Dragon.ml, Town.ml, etc.). *)
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

  (* ### TODO: Part 5 Smart Humans ### *)

  (********************************)
  (***** WorldObjectI Methods *****)
  (********************************)

  (* ### TODO: Part 1 Basic ### *)

  method! get_name = "human"

  method! draw =
    let gold_string = string_of_int (List.length gold) in
    self#draw_circle (Graphics.rgb 0xC9 0xC0 0xBB) Graphics.black gold_string

  method! draw_z_axis = 2

  (* ### TODO: Part 3 Actions ### *)
  (* NOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOTE: Is this the right place in the file to put the do_move method? This is where I've put it in a bunch of different files (Dragon.ml, Town.ml, etc.). *)
  method private do_action : unit -> unit = fun _ ->
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

  method! next_direction = Some (Direction.random (World.rand))

  (* ### TODO: Part 5 Smart Humans ### *)

  (* ### TODO: Part 6 Custom Events ### *)

  (***********************)
  (**** Human Methods ****)
  (***********************)

  (* ### TODO: Part 5 Smart Humans ### *)

end
