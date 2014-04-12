open Core.Std
open Human

(* Lannisters are a type of human that move in a straight line unless new gold
 * is detected nearby or there's an obstruction. *)
class lannister p city : human_t =
object (self)
  inherit human p city as super

  (******************************)
  (***** Instance Variables *****)
  (******************************)

  (* ### TODO: Part 5 Smart Humans *)

  (* Variable for the direction. Uses the same initial default value (None) as
   * Human. (The compiler didn't let us use super#next_direction_default to do
   * this.) *)
  val mutable traj : Direction.direction option = None


  (********************************)
  (***** WorldObjectI Methods *****)
  (********************************)

  (* ### TODO: Part 5 Smart Humans *)
  method! get_name = "lannister"


  (***********************)
  (***** Human Methods *****)
  (***********************)

  (* ### TODO: Part 5 Smart Humans *)
  (* display a gold circle to represent a lannister, and prints the amount of
   * gold the lannister has on the circle (by calling from super) *)
  method! draw_picture =
    let gold_string = string_of_int super#gold_length in
    self#draw_circle (Graphics.rgb 0xFF 0xEF 0x00) Graphics.black gold_string

  (* Defines default movement (if there's no danger, there's no nearby town with
   * new gold, and not all gold types have been found), as described above. *)
  (* Note: this will infinitely loop if our lannister is completely surrounded
   * by ponds and/or map boundaries. The lannister would have no unobstructed
   * direction to move in. *)
  method! private next_direction_default =
    let rec new_dir (d : Direction.direction) =
      let next_pt = Direction.move_point self#get_pos (Some d) in
      if World.can_move next_pt then
        let _ = traj <- Some d in Some d
      else new_dir (Direction.random World.rand)
    in
    match traj with
    | None -> new_dir (Direction.random World.rand)
    | Some d -> new_dir d

end
