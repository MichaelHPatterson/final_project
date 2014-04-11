open Core.Std
open Human
open WorldObject
open WorldObjectI

(* Baratheons should travel in a random direction. *)
class lannister p city : human_t =
object (self)
  inherit human p city as super

  (******************************)
  (***** Instance Variables *****)
  (******************************)

  (* ### TODO: Part 5 Smart Humans *)

  (********************************)
  (***** WorldObjectI Methods *****)
  (********************************)

  (* ### TODO: Part 5 Smart Humans *)

  (***********************)
  (***** Human Methods *****)
  (***********************)

  (* ### TODO: Part 5 Smart Humans *)
  method! get_name = "lannister"

  method! draw =
    let gold_string = string_of_int (super#gold_length) in
    self#draw_circle (Graphics.rgb 0xFF 0xEF 0x00) Graphics.black gold_string

  (* NOOOOOOOOOOOOOTE: this will infinite loop if we ever have a situation 
   * where our lannister is completely surrounded by shit. although this is true
   * for everything that moves randomly, I guess *)
  method! private next_direction_default =
    let dir_ref = ref (super#next_direction_default) in
    let move_attempt = Direction.move_point self#get_pos !dir_ref in
    let no_move = (!dir_ref = None) || World.can_move move_attempt in
    while no_move do
      dir_ref := Some (Direction.random (World.rand))
    done;
    !dir_ref

end


