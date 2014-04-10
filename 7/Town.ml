open Core.Std
open Helpers
open WorldObject
open WorldObjectI

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
class town p gold_id : world_object_i =
object (self)
  inherit world_object p

  (******************************)
  (***** Instance Variables *****)
  (******************************)

  (* ### TODO: Part 3 Actions ### *)
  val mutable gold = World.rand max_gold

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
    if gold < max_gold && World.rand produce_gold_probability = 0 then
      gold <- gold + 1;
    if World.rand expand_probability = 0 then
      World.spawn 1 self#get_pos (fun p -> ignore(new town p gold_id))


  (********************************)
  (***** WorldObjectI Methods *****)
  (********************************)

  (* ### TODO: Part 1 Basic ### *)

  method! get_name = "town"

  method! draw =
    let gold_string = string_of_int gold in
    self#draw_circle (Graphics.rgb 0x96 0x4B 0x00) Graphics.black gold_string

  (* ### TODO: Part 4 Aging ### *)

  (* ### TODO: Part 3 Actions ### *)
  method! smells_like_gold = if gold = 0 then None else Some gold_id


  method! forfeit_gold =
    if gold = 0 || World.rand forfeit_gold_probability > 0 then None
    else
      let _ = gold <- gold - 1 in
      Some gold_id

  (* ### TODO: Part 4 Aging ### *)

end
