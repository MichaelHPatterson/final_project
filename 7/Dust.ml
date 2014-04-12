open Core.Std
open Ageable

(* ### Part 4 Aging ### *)
let dust_lifetime = 50

(** Dust is what remains when carbon-based objects die. *)
class dust p (name:string) : ageable_t =
object (self)
  inherit ageable p None dust_lifetime dust_lifetime

  (***************************)
  (***** Ageable Methods *****)
  (***************************)

  (* ### TODO: Part 4 Aging ### *)
  
  (* draws the dust itself with a truncated string *)
  method! draw_picture : unit =
    let name_sub : string = String.sub name ~pos:0 ~len:2 in
    self#draw_circle (Graphics.rgb 150 150 150) Graphics.black name_sub
end
