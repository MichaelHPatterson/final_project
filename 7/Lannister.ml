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
  
  val mutable traj : Direction.direction option  = super#next_direction_default

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
    let no_move (dir: Direction.direction option) : bool =
      let move_attempt x = Direction.move_point self#get_pos x in
      (traj = None) || not (World.can_move (move_attempt traj)) in
    while (no_move traj) do
      traj <- Some (Direction.random (World.rand))
    done;
    traj

end


