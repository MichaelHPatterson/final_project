open Core.Std
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

  (* ### TODO: Part 5 Smart Humans ### *)

  (* ### TODO: Part 6 Custom Events ### *)

  (***********************)
  (***** Initializer *****)
  (***********************)

  (* ### TODO: Part 3 Actions ### *)

  (* ### TODO: Part 6 Custom Events ### *)

  (**************************)
  (***** Event Handlers *****)
  (**************************)

  (* ### TODO: Part 6 Custom Events ### *)

  (**************************)
  (***** Helper Methods *****)
  (**************************)

  (* ### TODO: Part 3 Actions ### *)

  (* ### TODO: Part 5 Smart Humans ### *)

  (********************************)
  (***** WorldObjectI Methods *****)
  (********************************)

  (* ### TODO: Part 1 Basic ### *)

  method! get_name = "human"

  method! draw = self#draw_circle (Graphics.rgb 0xC9 0xC0 0xBB) Graphics.black ""

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

  method! next_direction = Some (Direction.ord (Random.int 8))


  (* ### TODO: Part 5 Smart Humans ### *)

  (* ### TODO: Part 6 Custom Events ### *)

  (***********************)
  (**** Human Methods ****)
  (***********************)

  (* ### TODO: Part 5 Smart Humans ### *)

end
